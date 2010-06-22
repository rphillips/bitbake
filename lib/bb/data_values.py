#!/usr/bin/env python2.6
"""BitBake variable reference tracking and signature generation"""

import re
import codegen
import ast
import hashlib
from fnmatch import fnmatchcase
from itertools import chain
from collections import deque
from pysh import pyshyacc, pyshlex
from sys import exc_info
from bb import msg, utils

BLACKLIST = (
    "__*",
    "_task_deps",
    "BB_*",
    "BBFILE_*",
    "BBFILES",
    "BBLAYERS",
    "BBPATH",
    "BBMASK",
    "BBPKGS",
    "BBVERSIONS",
    "BBINCLUDELOGS",
    "BBINCLUDELOGS_LINES",
    "BUILDNAME",
    "DATE",
    "TIME",
    "*DIR",
    "PATH",
    "CWD",
    "FILE",
)

WHITELIST = (
    "PE",
    "BPN",
    "BP",
    "PN",
    "PV",
    "PR",
    "P",
    "PF",
    "DEPENDS",
    "RPROVIDES",
    "RPROVIDES_*",
    "RDEPENDS",
    "RDEPENDS_*",
    "RRECOMMENDS",
    "RRECOMMENDS_*",
    "OVERRIDES",
    "T",
    "B",
    "S",
    "STAMP",
)

from pysh.sherrors import ShellSyntaxError

class RecursionError(RuntimeError):
    def __init__(self, cycle = None):
        self.path = cycle

    def __str__(self):
        return str(self.path)

class PythonExpansionError(Exception):
    def __init__(self, exception, node, path, traceback=None):
        self.exception = exception
        self.node = node
        self.path = path
        self.traceback = traceback

    def __str__(self):
        string = "'%s: %s' while resolving '%s'" % (self.exception.__class__.__name__,
                                                    self.exception.args[0],
                                                    stable_repr(self.node))
        if self.path:
            string += " via:\n%s" % self.path
        if self.traceback:
            from traceback import format_tb
            string += "\n" + "".join(format_tb(self.traceback, 8))
        return string


class Visitor(object):
    def __init__(self, crossref = False):
        self.crossref = crossref
        self.path = Path()

    def visit(self, node):
        if node in self.path:
            raise RecursionError(self.path)
        self.path.append(node)
        self.generic_visit(node)
        classname = node.__class__.__name__
        if hasattr(self, "visit_" + classname):
            getattr(self, "visit_" + classname)(node)
        self.path.pop()

    def generic_visit(self, node):
        if self.crossref and isinstance(node, VariableRef):
            self.visit(node.components)
            name = node.components.resolve()
            value = new_value(name, node.metadata)
            self.visit(value)

        elif isinstance(node, PythonValue):
            for component in node.components:
                self.visit(component)

            if self.crossref:
                for var in chain(node.calls, node.visitor.var_execs, node.visitor.var_references):
                    strvalue = node.metadata.getVar(var, False)
                    if strvalue:
                        self.visit(new_value(strvalue, node.metadata))

        elif isinstance(node, ShellValue):
            for component in node.components:
                self.visit(component)

            if self.crossref:
                for var in node.execs:
                    strvalue = node.metadata.getVar(var, False)
                    if strvalue:
                        self.visit(new_value(strvalue, node.metadata))

        elif isinstance(node, Value):
            for component in node.components:
                self.visit(component)

        elif isinstance(node, Components):
            for component in node:
                self.visit(component)


class References(Visitor):
    def __init__(self, crossref=False):
        self.references = set()
        Visitor.__init__(self, crossref)

    def visit_VariableRef(self, node):
        name = node.components.resolve()
        self.references.add(name)

    def visit_PythonValue(self, node):
        self.references.update(node.visitor.var_references)
        self.references.update(node.visitor.var_execs)
        for var in node.calls:
            if node.metadata.getVar(var, False):
                self.references.add(var)

    visit_PythonSnippet = visit_PythonValue

    def visit_ShellValue(self, node):
        for var in node.execs:
            if node.metadata.getVar(var, False):
                self.references.add(var)


class Transformer(Visitor):
    def visit(self, node):
        if node in self.path:
            raise RecursionError(self.path)
        self.path.append(node)
        node = self.generic_visit(node)
        classname = node.__class__.__name__
        if hasattr(self, "visit_" + classname):
            result = getattr(self, "visit_" + classname)(node)
        else:
            result = node
        self.path.pop()
        return result

    def generic_visit(self, node):
        if self.crossref and isinstance(node, VariableRef):
            name = node.components.resolve()
            value = new_value(name, node.metadata)
            return self.visit(value)
        elif isinstance(node, Value):
            newcomponents = Components(self.visit(component)
                                       for component in node.components)
            if node.components != newcomponents:
                newvalue = node.__class__(newcomponents, node.metadata)
                newvalue.references.update(node.references)
                return newvalue
        return node


class Blacklister(Transformer):
    def __init__(self, is_blacklisted):
        self.is_blacklisted = is_blacklisted
        Transformer.__init__(self, False)

    def visit_VariableRef(self, node):
        name = node.components.resolve()
        if self.is_blacklisted(name):
            return "${%s}" % name
        else:
            return node


class Resolver(Transformer):
    def visit_Value(self, node):
        return "".join(node.components)

    visit_PythonValue = visit_Value
    visit_ShellValue = visit_Value

    def visit_PythonSnippet(self, node):
        code = self.visit_PythonValue(node)
        codeobj = compile(code.strip(), "<expansion>", "eval")
        try:
            value = str(utils.better_eval(codeobj, {"d": node.metadata}))
        except Exception, exc:
            raise PythonExpansionError(exc, node, None, exc_info()[2])
        return self.visit(Value(value, node.metadata))

    def visit_VariableRef(self, node):
        name = node.components.resolve()
        value = new_value(name, node.metadata)
        return self.visit(value)


class Path(deque):
    def __repr__(self):
        return "Path(%s)" % list(self)

    def __str__(self):
        return " ->\n".join("%s%s" % (indent * "  ", stable_repr(element))
                            for indent, element in enumerate(self))


class Components(list):
    """A list of components, which concatenates itself upon str(), and runs
    str() on each component.  A given component is defined as being a
    string, python snippet, or variable reference"""

    def __str__(self):
        return self.resolve()

    def _resolve(self, path = None):
        for v in self:
            if hasattr(v, "resolve"):
                yield v.resolve(path)
            else:
                yield v

    def resolve(self, path = None):
        if path is None:
            path = Path()
        return "".join(self._resolve(path))

    def __hash__(self):
        return hash("Components(%s)" % ", ".join(repr(c) for c in self))


class VariableRef(object):
    """Reference to a variable.  The variable name is supplied as a Components
    object, as we allow nested variable references, so the inside of a
    reference can be any number of components"""

    def __init__(self, components, metadata):
        self.components = components
        self.metadata = metadata

    def __repr__(self):
        return "VariableRef(%s, %s)" % (repr(self.components),
                                        repr(self.metadata))

    def __str__(self):
        return self.resolve()

    def resolve(self, path = None):
        if path is None:
            path = Path()

        name = self.components.resolve(path)
        value = new_value(name, self.metadata)
        if value in path:
            raise RecursionError(path)

        if hasattr(value, "resolve"):
            return value.resolve(path)
        else:
            return value


class Value(object):
    """Parse a value from the OE metadata into a Components object, held
    internally.  Running str() on this is equivalent to doing the same to its
    internal Components."""

    variable_ref = re.compile(r"(\$\{|\})")
    memory = {}

    def __new__(cls, value, metadata):
        key = (cls, value, id(metadata))
        if key in cls.memory:
            return cls.memory[key]
        else:
            cls.memory[key] = value = object.__new__(cls)
            return value

    def __init__(self, value, metadata):
        if not isinstance(value, basestring):
            self.components = Components(value)
            self.value = None
        else:
            self.value = value
            self.components = Components()
        self.metadata = metadata
        self.references = set()
        self.parse()
        self.update_references(self)

    def update_references(self, value):
        for item in value.components:
            if isinstance(item, VariableRef):
                if all(isinstance(x, basestring) for x in item.components):
                    self.references.add(item.components.resolve())
                else:
                    self.update_references(item)
            elif isinstance(item, Value):
                self.references.update(item.references)

    def __eq__(self, other):
        return isinstance(other, type(self)) and \
               self.components == other.components and \
               self.metadata == other.metadata

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash((self.components, id(self.metadata)))

    def __repr__(self):
        return "%s(%s, %s)" % (self.__class__.__name__, repr(self.components),
                               repr(self.metadata))

    def __str__(self):
        return self.resolve()

    def resolve(self, path = None):
        if path is None:
            path = Path()
        path.append(self)
        resolved = self.components.resolve(path)
        path.pop()
        return resolved

    def parse(self):
        """Parse a value from the OE metadata into a Components object"""

        if self.value is None:
            return

        if not isinstance(self.value, basestring) or \
           "${" not in self.value:
            self.components.append(self.value)
            return

        tokens = (var for var in self.variable_ref.split(self.value) if var)
        result = Components()
        current = None
        stack = deque()
        for token in tokens:
            if token == "${":
                stack.append(current)
                current = Components()
            elif current is not None:
                if token == "}":
                    if hasattr(current[0], "startswith") and \
                       current[0].startswith("@"):
                        current[0] = current[0][1:]
                        value = PythonSnippet(current, self.metadata)
                    else:
                        value = VariableRef(current, self.metadata)

                    current = stack.pop()
                    if current is None:
                        result.append(value)
                    else:
                        current.append(value)
                else:
                    current.append(token)
            else:
                result.append(token)
        if current:
            result += current
        self.components = result


class ShellValue(Value):
    """Represents a block of shell code, initialized from a string.  First
    parses the string into a components object to gather information about
    regular variable references, then parses the resulting expanded shell code
    to extract calls to other shell functions in the metadata.

    May raise: NotImplementedError, ShellSyntaxError
    """

    def __init__(self, value, metadata):
        self.funcdefs = set()
        self.execs = set()
        Value.__init__(self, value, metadata)

    def parse(self):
        Value.parse(self)
        try:
            strvalue = Value.resolve(self)
        except (RecursionError, PythonExpansionError):
            if self.value:
                strvalue = self.value
            else:
                raise

        self.execs = self.parse_shell(strvalue)
        for var in self.metadata.keys():
            flags = self.metadata.getVarFlags(var)
            if flags:
                if "export" in flags:
                    self.references.add(var)
                elif var in self.execs and \
                     "func" in flags and "python" not in flags:
                    self.references.add(var)

    def parse_shell(self, value):
        """Parse the supplied shell code in a string, returning the external
        commands it executes.
        """

        try:
            tokens, _ = pyshyacc.parse(value, eof=True, debug=False)
        except pyshlex.NeedMore:
            raise ShellSyntaxError("Unexpected EOF")

        for token in tokens:
            self.process_tokens(token)
        cmds = set(cmd for cmd in self.execs
                       if cmd not in self.funcdefs)
        return cmds

    def process_tokens(self, tokens):
        """Process a supplied portion of the syntax tree as returned by
        pyshyacc.parse.
        """

        def function_definition(value):
            self.funcdefs.add(value.name)
            return [value.body], None

        def case_clause(value):
            # Element 0 of each item in the case is the list of patterns, and
            # Element 1 of each item in the case is the list of commands to be
            # executed when that pattern matches.
            words = chain(*[item[0] for item in value.items])
            cmds  = chain(*[item[1] for item in value.items])
            return cmds, words

        def if_clause(value):
            main = chain(value.cond, value.if_cmds)
            rest = value.else_cmds
            if isinstance(rest, tuple) and rest[0] == "elif":
                return chain(main, if_clause(rest[1]))
            else:
                return chain(main, rest)

        def simple_command(value):
            return None, chain(value.words, (assign[1] for assign in value.assigns))

        token_handlers = {
            "and_or": lambda x: ((x.left, x.right), None),
            "async": lambda x: ([x], None),
            "brace_group": lambda x: (x.cmds, None),
            "for_clause": lambda x: (x.cmds, x.items),
            "function_definition": function_definition,
            "if_clause": lambda x: (if_clause(x), None),
            "pipeline": lambda x: (x.commands, None),
            "redirect_list": lambda x: ([x.cmd], None),
            "subshell": lambda x: (x.cmds, None),
            "while_clause": lambda x: (chain(x.condition, x.cmds), None),
            "until_clause": lambda x: (chain(x.condition, x.cmds), None),
            "simple_command": simple_command,
            "case_clause": case_clause,
        }

        for token in tokens:
            name, value = token
            try:
                more_tokens, words = token_handlers[name](value)
            except KeyError:
                raise NotImplementedError("Unsupported token type " + name)

            if more_tokens:
                self.process_tokens(more_tokens)

            if words:
                self.process_words(words)

    def process_words(self, words):
        """Process a set of 'words' in pyshyacc parlance, which includes
        extraction of executed commands from $() blocks, as well as grabbing
        the command name argument.
        """

        words = list(words)
        for word in list(words):
            wtree = pyshlex.make_wordtree(word[1])
            for part in wtree:
                if not isinstance(part, list):
                    continue

                if part[0] in ('`', '$('):
                    command = pyshlex.wordtree_as_string(part[1:-1])
                    self.parse_shell(command)

                    if word[0] in ("cmd_name", "cmd_word"):
                        if word in words:
                            words.remove(word)

        usetoken = False
        for word in words:
            if word[0] in ("cmd_name", "cmd_word") or \
               (usetoken and word[0] == "TOKEN"):
                if "=" in word[1]:
                    usetoken = True
                    continue

                cmd = word[1]
                if cmd.startswith("$"):
                    msg.debug(1, None, "Warning: execution of non-literal command '%s'" % cmd)
                elif cmd == "eval":
                    command = " ".join(word for _, word in words[1:])
                    self.parse_shell(command)
                else:
                    self.execs.add(cmd)
                break


class PythonValue(Value):
    """Represents a block of python code, initialized from a string.  First
    determines the variables referenced via normal variable expansion, then
    traverses the python syntax tree to extract variables references accessed
    via the usual bitbake metadata APIs, as well as the external functions
    called (to track usage of functions in the methodpool).
    """

    class ValueVisitor(ast.NodeVisitor):
        """Visitor to traverse a python abstract syntax tree and obtain
        the variables referenced via bitbake metadata APIs, and the external
        functions called.
        """

        getvars = ("d.getVar", "bb.data.getVar", "data.getVar")
        expands = ("d.expand", "bb.data.expand", "data.expand")
        execs = ("bb.build.exec_func", "bb.build.exec_task")

        @classmethod
        def _compare_name(cls, strparts, node):
            """Given a sequence of strings representing a python name,
            where the last component is the actual Name and the prior
            elements are Attribute nodes, determine if the supplied node
            matches.
            """

            if not strparts:
                return True

            current, rest = strparts[0], strparts[1:]
            if isinstance(node, ast.Attribute):
                if current == node.attr:
                    return cls._compare_name(rest, node.value)
            elif isinstance(node, ast.Name):
                if current == node.id:
                    return True
            return False

        @classmethod
        def compare_name(cls, value, node):
            """Convenience function for the _compare_node method, which
            can accept a string (which is split by '.' for you), or an
            iterable of strings, in which case it checks to see if any of
            them match, similar to isinstance.
            """

            if isinstance(value, basestring):
                return cls._compare_name(tuple(reversed(value.split("."))),
                                         node)
            else:
                return any(cls.compare_name(item, node) for item in value)

        def __init__(self, value):
            self.var_references = set()
            self.var_execs = set()
            self.direct_func_calls = set()
            self.value = value
            ast.NodeVisitor.__init__(self)

        @classmethod
        def warn(cls, func, arg):
            """Warn about calls of bitbake APIs which pass a non-literal
            argument for the variable name, as we're not able to track such
            a reference.
            """

            try:
                funcstr = codegen.to_source(func)
                argstr = codegen.to_source(arg)
            except TypeError:
                msg.debug(2, None, "Failed to convert function and argument to source form")
            else:
                msg.debug(1, None, "Warning: in call to '%s', argument '%s' is not a literal" %
                                     (funcstr, argstr))

        def visit_Call(self, node):
            ast.NodeVisitor.generic_visit(self, node)
            if self.compare_name(self.getvars, node.func):
                if isinstance(node.args[0], ast.Str):
                    self.var_references.add(node.args[0].s)
                else:
                    self.warn(node.func, node.args[0])
            elif self.compare_name(self.expands, node.func):
                if isinstance(node.args[0], ast.Str):
                    value = Value(node.args[0].s, self.value.metadata)
                    self.var_references.update(value.references)
                elif isinstance(node.args[0], ast.Call) and \
                     self.compare_name(self.getvars, node.args[0].func):
                    pass
                else:
                    self.warn(node.func, node.args[0])
            elif self.compare_name(self.execs, node.func):
                if isinstance(node.args[0], ast.Str):
                    self.var_execs.add(node.args[0].s)
                else:
                    self.warn(node.func, node.args[0])
            elif isinstance(node.func, ast.Name):
                self.direct_func_calls.add(node.func.id)
            elif isinstance(node.func, ast.Attribute):
                # We must have a qualified name.  Therefore we need
                # to walk the chain of 'Attribute' nodes to determine
                # the qualification.
                attr_node = node.func.value
                identifier = node.func.attr
                while isinstance(attr_node, ast.Attribute):
                    identifier = attr_node.attr + "." + identifier
                    attr_node = attr_node.value
                if isinstance(attr_node, ast.Name):
                    identifier = attr_node.id + "." + identifier
                self.direct_func_calls.add(identifier)

    def __init__(self, value, metadata):
        self.visitor = self.ValueVisitor(self)
        self.function_references = set()
        self.calls = None

        Value.__init__(self, value, metadata)

    def parse(self):
        Value.parse(self)
        try:
            value = Value.resolve(self)
        except (RecursionError, PythonExpansionError):
            if self.value:
                value = self.value
            else:
                raise

        code = compile(value, "<string>", "exec", ast.PyCF_ONLY_AST)
        self.visitor.visit(code)

        self.references.update(self.visitor.var_references)
        self.references.update(self.visitor.var_execs)
        self.calls = self.visitor.direct_func_calls
        env = {}
        for var in self.calls:
            try:
                func_obj = utils.better_eval(var, env)
                if self.metadata.getVar(var, False) is not None:
                    self.references.add(var)
                self.function_references.add((var, func_obj))
            except (NameError, AttributeError):
                pass

class PythonSnippet(PythonValue):
    """Lazy evaluation of a python snippet"""

    def resolve(self, path = None):
        code = PythonValue.resolve(self, path)
        codeobj = compile(code.strip(), "<expansion>", "eval")
        try:
            value = str(utils.better_eval(codeobj, {"d": self.metadata}))
        except RuntimeError as exc:
            if exc.args[0] == "maximum recursion depth exceeded":
                raise RecursionError(path)
            else:
                raise PythonExpansionError(exc, self, path, exc_info()[2])
        except Exception, exc:
            raise PythonExpansionError(exc, self, path, exc_info()[2])
        return Value(value, self.metadata).resolve(path)


from tokenize import generate_tokens, untokenize, INDENT, DEDENT
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

def dedent_python(codestr):
    """Remove the first level of indentation from a block of python code"""

    indent = None
    level = 0
    tokens = []
    for toknum, tokval, _, _, _ in generate_tokens(StringIO(codestr).readline):
        if toknum == INDENT:
            level += 1
            if level == 1:
                indent = tokval
                continue
            elif indent:
                tokval = tokval[len(indent):]
        elif toknum == DEDENT:
            level -= 1
            if level == 0:
                indent = None
                continue
        tokens.append((toknum, tokval))
    return untokenize(tokens)

def new_value(variable, metadata, path = None):
    """Value creation factory for a variable in the metadata"""

    if path is None:
        path = Path()
    if variable in path:
        raise RecursionError(path)
    path.append(variable)

    try:
        strvalue = metadata.getVar(variable, False)
        if strvalue is None:
            path.pop()
            return "${%s}" % variable
    except Exception as exc:
        msg.error(None, "Exception raised while handling '%s', path is %s" % (variable, path))
        raise

    if metadata.getVarFlag(variable, "func"):
        if metadata.getVarFlag(variable, "python"):
            value = PythonValue(dedent_python(strvalue.expandtabs()), metadata)
        else:
            try:
                value = ShellValue(strvalue, metadata)
            except pyshlex.NeedMore:
                raise RuntimeError("Ran out of input while parsing shell for %s" % variable)
            except ShellSyntaxError, exc:
                raise RuntimeError("Syntax error parsing shell for %s: %s" % (variable, exc))
    else:
        value = Value(strvalue, metadata)

    for flag in ("dirs", "cleandirs", "lockfiles"):
        flagvalue = metadata.getVarFlag(variable, flag)
        if flagvalue:
            value.references.update(Value(flagvalue, metadata).references)

    varrefs = metadata.getVarFlag(variable, "varrefs")
    if varrefs:
        refs = Value(varrefs, metadata)
        value.references.update(refs.references)
        patterns = str(refs).split()
        for key in metadata.keys():
            if any(fnmatchcase(key, pat) for pat in patterns):
                value.references.add(key)

    path.pop()
    return value

def stable_repr(value):
    """Produce a more stable 'repr' string for a value"""
    if isinstance(value, dict):
        return "{%s}" % ", ".join("%s: %s" % (stable_repr(key), stable_repr(value))
                                  for key, value in sorted(value.iteritems()))
    elif isinstance(value, (set, frozenset)):
        return "%s(%s)" % (value.__class__.__name__, stable_repr(sorted(value)))
    elif isinstance(value, list):
        return "[%s]" % ", ".join(stable_repr(value) for value in value)
    elif isinstance(value, tuple):
        return "(%s)" % ", ".join(stable_repr(value) for value in value)
    elif isinstance(value, (VariableRef, Value)):
        return "%s(%s)" % (value.__class__.__name__, stable_repr(value.components))
    return repr(value)

class Signature(object):
    """A signature is produced uniquely identifying part of the BitBake metadata.

    keys is the list of variable names to include in the signature (default is
    all current tasks).  blacklist is a list of globs which identify variables
    which should not be included at all, even when referenced by other
    variables.
    """

    def __init__(self, metadata, keys = None, blacklist = None):
        self._md5 = None
        self._data = None
        self._data_string = None
        self.metadata = metadata

        if keys:
            self.keys = keys
        else:
            self.keys = [key for key in self.metadata.keys()
                         if metadata.getVarFlag(key, "task")]

        if blacklist:
            self.blacklist = blacklist
        else:
            blacklist = metadata.getVar("BB_HASH_BLACKLIST", True)
            if blacklist:
                self.blacklist = set(blacklist.split())
            else:
                self.blacklist = set()

        self.blacklist.update(BLACKLIST)

    def __repr__(self):
        return "Signature(%s, %s, %s)" % (self.metadata, self.keys, self.blacklist)

    def __hash__(self):
        return hash((id(self.metadata), self.keys, self.blacklist))

    def __str__(self):
        from base64 import urlsafe_b64encode

        return urlsafe_b64encode(self.md5.digest()).rstrip("=")

    def hash(self):
        """Return an integer version of the signature"""
        return int(self.md5.hexdigest(), 16)

    @property
    def md5(self):
        """The underlying python 'md5' object"""

        value = self._md5
        if value is None:
            value = self._md5 = hashlib.md5(self.data_string)
        return value

    @property
    def data_string(self):
        """Stabilized string representation of the data to be hashed"""
        string = self._data_string
        if string is None:
            string = self._data_string = stable_repr(self.data)
        return string

    def is_blacklisted(self, name):
        for bl in self.blacklist:
            if fnmatchcase(name, bl):
                return True

    @property
    def data(self):
        """The object containing the data which will be converted to a string and then hashed"""

        if self._data:
            return self._data

        blacklister = Blacklister(self.is_blacklisted)
        seen = set()
        def data_for_hash(key):
            """Returns an iterator over the variable names and their values, including references"""

            if key in seen:
                return
            seen.add(key)
            if self.is_blacklisted(key):
                return

            valstr = self.metadata.getVar(key, False)
            if valstr is None:
                yield key, valstr
            else:
                try:
                    value = blacklister.visit(new_value(key, self.metadata))
                except (SyntaxError, ShellSyntaxError, NotImplementedError,
                        PythonExpansionError, RecursionError), exc:
                    import traceback
                    msg.error(msg.domain.Data, "Excluding '%s' from signature: %s" %
                                               (key, exc.__class__.__name__))
                    msg.error(msg.domain.Data, traceback.format_exc(limit=8))
                else:
                    yield key, value

                    for ref in value.references:
                        for other in data_for_hash(ref):
                            yield other

        whitelisted = set()
        for key in self.metadata.keys():
            if any(fnmatchcase(key, pattern) for pattern in WHITELIST):
                whitelisted.add(key)
        dictdata = chain.from_iterable(data_for_hash(key) for key in self.keys)
        dictdata = chain(dictdata, chain.from_iterable(data_for_hash(key) for key in whitelisted))
        data = self._data = dict(dictdata)
        return data
