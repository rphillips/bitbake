#!/usr/bin/env python

import unittest
import sys
import os

basedir = os.path.dirname(os.path.abspath(os.path.dirname(__file__)))
oedir = os.path.dirname(basedir)
searchpath = [os.path.join(basedir, "lib"),
              os.path.join(oedir, "openembedded", "lib"),
              os.path.join(oedir, "bitbake", "lib")]
sys.path[0:0] = searchpath

import bb.data
import bb.data_values

class TestExpansions(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()
        self.d["foo"] = "value of foo"
        self.d["bar"] = "value of bar"
        self.d["value of foo"] = "value of 'value of foo'"

    def test_one_var(self):
        val = bb.data_values.Value.from_string("${foo}", self.d)
        self.assertEqual(str(val), "value of foo")
        self.assertEqual(val.references, set(["foo"]))

    def test_indirect_one_var(self):
        val = bb.data_values.Value.from_string("${${foo}}", self.d)
        self.assertEqual(str(val), "value of 'value of foo'")
        self.assertEqual(val.references, set(["foo"]))

    def test_indirect_and_another(self):
        val = bb.data_values.Value.from_string("${${foo}} ${bar}", self.d)
        self.assertEqual(str(val), "value of 'value of foo' value of bar")
        self.assertEqual(val.references, set(["foo", "bar"]))

    def test_python_snippet(self):
        val = bb.data_values.Value.from_string("${@5*12}", self.d)
        self.assertEqual(str(val), "60")
        self.assertFalse(val.references)

    def test_expand_in_python_snippet(self):
        val = bb.data_values.Value.from_string("${@'boo ' + '${foo}'}", self.d)
        self.assertEqual(str(val), "boo value of foo")
        self.assertEqual(val.references, set(["foo"]))

    def test_python_snippet_getvar(self):
        val = bb.data_values.Value.from_string("${@d.getVar('foo', True) + ' ${bar}'}", self.d)
        self.assertEqual(str(val), "value of foo value of bar")
        self.assertEqual(val.references, set(["foo", "bar"]))

    def test_python_snippet_syntax_error(self):
        self.d.setVar("FOO", "${@foo = 5}")
        val = bb.data_values.new_value("FOO", self.d)
        self.assertRaises(SyntaxError, val.resolve)

    def test_python_snippet_runtime_error(self):
        self.d.setVar("FOO", "${@int('test')}")
        val = bb.data_values.new_value("FOO", self.d)
        self.assertRaises(bb.data_values.PythonExpansionError, val.resolve)

    def test_python_snippet_error_path(self):
        self.d.setVar("FOO", "foo value ${BAR}")
        self.d.setVar("BAR", "bar value ${@int('test')}")
        val = bb.data_values.new_value("FOO", self.d)
        try:
            val.resolve()
        except bb.data_values.PythonExpansionError, exc:
            self.assertEqual(len(exc.path), 2)
        else:
            self.fail("Did not raise expected PythonExpansionError")

    def test_value_containing_value(self):
        otherval = bb.data_values.Value.from_string("${@d.getVar('foo', True) + ' ${bar}'}", self.d)
        val = bb.data_values.Value(bb.data_values.Components([otherval, " test"]), self.d)
        self.assertEqual(str(val), "value of foo value of bar test")
        self.assertEqual(val.references, set(["foo", "bar"]))

    def test_reference_undefined_var(self):
        val = bb.data_values.Value.from_string("${undefinedvar} meh", self.d)
        self.assertEqual(str(val), "${undefinedvar} meh")
        self.assertEqual(val.references, set(["undefinedvar"]))

    def test_double_reference(self):
        self.d.setVar("BAR", "bar value")
        self.d.setVar("FOO", "${BAR} foo ${BAR}")
        val = bb.data_values.new_value("FOO", self.d)
        val.resolve()

    def test_direct_recursion(self):
        self.d.setVar("FOO", "${FOO}")
        value = bb.data_values.new_value("FOO", self.d)
        self.assertRaises(bb.data_values.RecursionError, str, value)

    def test_indirect_recursion(self):
        self.d.setVar("FOO", "${BAR}")
        self.d.setVar("BAR", "${BAZ}")
        self.d.setVar("BAZ", "${FOO}")
        value = bb.data_values.new_value("FOO", self.d)
        self.assertRaises(bb.data_values.RecursionError, str, value)

    def test_recursion_exception(self):
        self.d.setVar("FOO", "${BAR}")
        self.d.setVar("BAR", "${${@'FOO'}}")
        value = bb.data_values.new_value("FOO", self.d)
        try:
            str(value)
        except bb.data_values.RecursionError, exc:
            self.assertTrue(value in exc.path)
            self.assertTrue(bb.data_values.new_value("BAR", self.d) in exc.path)
        else:
            self.fail("RecursionError not raised")

    def test_incomplete_python_ref(self):
        value = bb.data_values.Value.from_string("${@5*3", self.d)
        self.assertEqual(value.components, bb.data_values.Components(["${", "@5*3"]))

    def test_incomplete_ref(self):
        value = bb.data_values.Value.from_string("${FOO", self.d)
        self.assertEqual(value.components, bb.data_values.Components(["${", "FOO"]))

    def test_nested_incomplete_ref(self):
        value = bb.data_values.Value.from_string("${FOO${BAR", self.d)
        self.assertEqual(value.components, bb.data_values.Components(["${", "FOO", "${", "BAR"]))

    def test_nested_partial_python_incomplete_ref(self):
        value = bb.data_values.Value.from_string("${${@5*3}bar", self.d)
        self.assertEqual(bb.data_values.stable_repr(value), "Value(['${', PythonSnippet(['5*3']), 'bar'])")

    def test_nested_partial_incomplete_ref(self):
        value = bb.data_values.Value.from_string("${${FOO}bar", self.d)
        self.assertEqual(bb.data_values.stable_repr(value), "Value(['${', VariableRef(['FOO']), 'bar'])")


class TestMemoize(unittest.TestCase):
    def test_memoized(self):
        d = bb.data.init()
        d.setVar("FOO", "bar")
        self.assertEqual(bb.data_values.new_value("FOO", d),
                         bb.data_values.new_value("FOO", d))

    def test_not_memoized(self):
        d1 = bb.data.init()
        d2 = bb.data.init()
        d1.setVar("FOO", "bar")
        d2.setVar("FOO", "bar")
        self.assertNotEqual(bb.data_values.new_value("FOO", d1),
                            bb.data_values.new_value("FOO", d2))


class TestShell(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()

    def test_quotes_inside_assign(self):
        value = bb.data_values.ShellValue.from_string('foo=foo"bar"baz', self.d)

    def test_quotes_inside_arg(self):
        value = bb.data_values.ShellValue.from_string('sed s#"bar baz"#"alpha beta"#g', self.d)
        self.assertEqual(value.execs, set(["sed"]))

    def test_arg_continuation(self):
        value = bb.data_values.ShellValue.from_string("sed -i -e s,foo,bar,g \\\n *.pc", self.d)
        self.assertEqual(value.execs, set(["sed"]))

    def test_dollar_in_quoted(self):
        value = bb.data_values.ShellValue.from_string('sed -i -e "foo$" *.pc', self.d)
        self.assertEqual(value.execs, set(["sed"]))

    def test_quotes_inside_arg_continuation(self):
        value = bb.data_values.ShellValue.from_string("""
        sed -i -e s#"moc_location=.*$"#"moc_location=${bindir}/moc4"# \\
               -e s#"uic_location=.*$"#"uic_location=${bindir}/uic4"# \\
               ${D}${libdir}/pkgconfig/*.pc
        """, self.d)
        self.assertEqual(value.references, set(["bindir", "D", "libdir"]))

    def test_assign_subshell_expansion(self):
        value = bb.data_values.ShellValue.from_string("foo=$(echo bar)", self.d)
        self.assertEqual(value.execs, set(["echo"]))

    def test_shell_unexpanded(self):
        value = bb.data_values.ShellValue.from_string('echo "${QT_BASE_NAME}"', self.d)
        self.assertEqual(value.execs, set(["echo"]))
        self.assertEqual(value.references, set(["QT_BASE_NAME"]))

    def test_incomplete_varexp_single_quotes(self):
        value = bb.data_values.ShellValue.from_string("sed -i -e 's:IP{:I${:g' $pc", self.d)
        self.assertEqual(value.execs, set(["sed"]))

    def test_until(self):
        shellval = bb.data_values.ShellValue.from_string("until false; do echo true; done", self.d)
        self.assertEquals(shellval.execs, set(["false", "echo"]))
        self.assertEquals(shellval.references, set())

    def test_case(self):
        script = """
case $foo in
    *)
        bar
        ;;
esac
        """
        shellval = bb.data_values.ShellValue.from_string(script, self.d)
        self.assertEquals(shellval.execs, set(["bar"]))
        self.assertEquals(shellval.references, set())

    def test_assign_exec(self):
        value = bb.data_values.ShellValue.from_string("a=b c='foo bar' alpha 1 2 3", self.d)
        self.assertEquals(value.execs, set(["alpha"]))

    def test_redirect_to_file(self):
        value = bb.data_values.ShellValue.from_string("echo foo >${foo}/bar", self.d)
        self.assertEquals(value.execs, set(["echo"]))
        self.assertEquals(value.references, set(["foo"]))

    def test_heredoc(self):
        script = """
        cat <<END
alpha
beta
theta
END
        """
        value = bb.data_values.ShellValue.from_string(script, self.d)

    def test_redirect_from_heredoc(self):
        script = """
    cat <<END >${B}/cachedpaths
shadow_cv_maildir=${SHADOW_MAILDIR}
shadow_cv_mailfile=${SHADOW_MAILFILE}
shadow_cv_utmpdir=${SHADOW_UTMPDIR}
shadow_cv_logdir=${SHADOW_LOGDIR}
shadow_cv_passwd_dir=${bindir}
END
        """
        value = bb.data_values.ShellValue.from_string(script, self.d)
        self.assertEquals(value.references, set(["B", "SHADOW_MAILDIR",
                                                 "SHADOW_MAILFILE", "SHADOW_UTMPDIR",
                                                 "SHADOW_LOGDIR", "bindir"]))
        self.assertEquals(value.execs, set(["cat"]))

    def test_incomplete_command_expansion(self):
        self.assertRaises(bb.data_values.ShellSyntaxError, bb.data_values.ShellValue.from_string,
                          "cp foo`", self.d)

    def test_rogue_dollarsign(self):
        self.d.setVar("D", "/tmp")
        value = bb.data_values.ShellValue.from_string("install -d ${D}$", self.d)
        self.assertEqual(value.references, set(["D"]))
        self.assertEqual(value.execs, set(["install"]))

class TestContentsTracking(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()

    pydata = """
        bb.data.getVar('somevar', d, True)
        def test():
            foo = 'bar %s' % 'foo'
            def test2():
                d.getVar(foo, True)
            d.getVar('bar', False)
            test2()

        def a():
            \"\"\"some
    stuff
            \"\"\"
            return "heh"

        bb.data.expand(bb.data.getVar("something", False, d), d)
        bb.data.expand("${inexpand} somethingelse", d)
        bb.data.getVar(a(), d, False)
    """

    def test_python(self):
        self.d.setVar("FOO", self.pydata)
        self.d.setVarFlags("FOO", {"func": True, "python": True})

        value = bb.data_values.new_value("FOO", self.d)
        self.assertEquals(value.references, set(["somevar", "bar", "something", "inexpand"]))
        self.assertEquals(value.calls, set(["test2", "a"]))

    shelldata = """
        foo () {
            bar
        }
        {
            echo baz
            $(heh)
            eval `moo`
        }
        a=b
        c=d
        (
            true && false
            test -f foo
            testval=something
            $testval
        ) || aiee
        ! inverted
        echo ${somevar}

        case foo in
            bar)
                echo bar
                ;;
            baz)
                echo baz
                ;;
            foo*)
                echo foo
                ;;
        esac
    """

    def test_shell(self):
        self.d.setVar("somevar", "heh")
        self.d.setVar("inverted", "echo inverted...")
        self.d.setVarFlag("inverted", "func", True)

        shellval = bb.data_values.ShellValue.from_string(self.shelldata, self.d)
        self.assertEquals(shellval.references, set(["somevar", "inverted"]))
        self.assertEquals(shellval.execs, set(["bar", "echo", "heh", "moo",
                                               "true", "false", "test", "aiee",
                                               "inverted"]))

    def test_varrefs(self):
        self.d.setVar("oe_libinstall", "echo test")
        self.d.setVar("FOO", "foo=oe_libinstall; eval $foo")
        self.d.setVarFlag("FOO", "varrefs", "oe_libinstall")
        value = bb.data_values.new_value("FOO", self.d)
        self.assertEqual(set(["oe_libinstall"]), value.references)

    def test_varrefs_expand(self):
        self.d.setVar("oe_libinstall", "echo test")
        self.d.setVar("FOO", "foo=oe_libinstall; eval $foo")
        self.d.setVarFlag("FOO", "varrefs", "${@'oe_libinstall'}")
        value = bb.data_values.new_value("FOO", self.d)
        self.assertEqual(set(["oe_libinstall"]), value.references)

    def test_varrefs_wildcards(self):
        self.d.setVar("oe_libinstall", "echo test")
        self.d.setVar("FOO", "foo=oe_libinstall; eval $foo")
        self.d.setVarFlag("FOO", "varrefs", "oe_*")
        value = bb.data_values.new_value("FOO", self.d)
        self.assertEqual(set(["oe_libinstall"]), value.references)

class TestPython(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()
        if hasattr(bb.utils, "_context"):
            self.context = bb.utils._context
        else:
            import __builtin__
            self.context = __builtin__.__dict__

    def test_getvar_reference(self):
        value = bb.data_values.PythonValue.from_string("bb.data.getVar('foo', d, True)", self.d)
        self.assertEqual(value.references, set(["foo"]))
        self.assertEqual(value.calls, set())

    def test_var_reference(self):
        value = bb.data_values.PythonValue.from_string("foo('${FOO}')", self.d)
        self.assertEqual(value.references, set(["FOO"]))
        self.assertEqual(value.calls, set(["foo"]))

    def test_var_exec(self):
        for etype in ("func", "task"):
            self.d.setVar("do_something", "echo 'hi mom! ${FOO}'")
            self.d.setVarFlag("do_something", etype, True)
            value = bb.data_values.PythonValue.from_string("bb.build.exec_func('do_something', d)",
                                        self.d)
            self.assertEqual(value.references, set(["do_something"]))

    def test_function_reference(self):
        self.context["testfunc"] = lambda msg: bb.msg.note(1, None, msg)
        self.d.setVar("FOO", "Hello, World!")
        value = bb.data_values.PythonValue.from_string("testfunc('${FOO}')", self.d)
        self.assertEqual(value.references, set(["FOO"]))
        self.assertEqual(value.function_references,
                         set([("testfunc", self.context["testfunc"])]))
        del self.context["testfunc"]

    def test_qualified_function_reference(self):
        value = bb.data_values.PythonValue.from_string("time.time()", self.d)
        self.assertEqual(value.function_references,
                         set([("time.time", self.context["time"].time)]))

    def test_qualified_function_reference_2(self):
        value = bb.data_values.PythonValue.from_string("os.path.dirname('/foo/bar')", self.d)
        self.assertEqual(value.function_references,
                         set([("os.path.dirname", self.context["os"].path.dirname)]))

    def test_qualified_function_reference_nested(self):
        value = bb.data_values.PythonValue.from_string("time.strftime('%Y%m%d',time.gmtime())",
                                     self.d)
        self.assertEqual(value.function_references,
                         set([("time.strftime", self.context["time"].strftime),
                              ("time.gmtime", self.context["time"].gmtime)]))

    def test_function_reference_chained(self):
        self.context["testget"] = lambda: "\tstrip me     "
        value = bb.data_values.PythonSnippet.from_string("testget().strip()", self.d)
        value.resolve()
        self.assertEqual(value.function_references,
                         set([("testget", self.context["testget"])]))
        del self.context["testget"]

    def test_string_format(self):
        self.d.setVar("SOMEVAR", "${@'%s.*' % bb.data.expand(bb.data.getVar('STAMP', d), d)}")
        value = bb.data_values.new_value("SOMEVAR", self.d)
        self.assertEqual(value.references, set(["STAMP"]))

class TestSignatureGeneration(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()
        self.d["BB_HASH_BLACKLIST"] = "blacklisted*"

    def test_basic_signature(self):
        self.d.setVar("alpha", "echo ${TOPDIR}/foo \"$@\"")
        self.d.setVarFlags("alpha", {"func": True, "task": True})
        self.d.setVar("beta", "test -f bar")
        self.d.setVarFlags("beta", {"func": True, "task": True})
        self.d.setVar("theta", "alpha baz")
        self.d.setVarFlags("theta", {"func": True, "task": True})
        signature = bb.data_values.Signature(self.d)
        self.assertEquals(signature.data_string, "{'alpha': ShellValue(['echo ', '${TOPDIR}', '/foo \"$@\"']), 'beta': ShellValue(['test -f bar']), 'theta': ShellValue(['alpha baz'])}")

    def test_blacklist_nested_varref(self):
        self.d["blacklistedvar"] = "avalue"
        self.d["avalue"] = "value"
        self.d["foo"] = "${${blacklistedvar}}"
        signature = bb.data_values.Signature(self.d, keys=["foo"])
        self.assertEqual(signature.data_string, "{'foo': Value(['${${blacklistedvar}}'])}")

    def test_signature_blacklisted(self):
        self.d["blacklistedvar"] = "blacklistedvalue"
        self.d["testbl"] = "${@5} foo ${blacklistedvar} bar"
        signature = bb.data_values.Signature(self.d, keys=["testbl"])
        self.assertEqual(signature.data_string, "{'testbl': Value([PythonSnippet(['5']), ' foo ', '${blacklistedvar}', ' bar'])}")

    def test_signature_only_blacklisted(self):
        self.d["anotherval"] = "${blacklistedvar}"
        signature = bb.data_values.Signature(self.d, keys=["anotherval"])
        self.assertEquals(signature.data_string, "{'anotherval': Value(['${blacklistedvar}'])}")

    def test_signature_undefined(self):
        self.d["someval"] = "${undefinedvar} ${blacklistedvar} meh"
        signature = bb.data_values.Signature(self.d, keys=["someval"])
        self.assertEquals(signature.data_string, "{'someval': Value([VariableRef(['undefinedvar']), ' ', '${blacklistedvar}', ' meh']), 'undefinedvar': None}")

    def test_signature_oe_devshell(self):
        self.d.setVar("do_devshell", "devshell_do_devshell")
        self.d.setVarFlag("do_devshell", "func", True)
        devshell = """
                export TERMWINDOWTITLE="Bitbake Developer Shell"
                ${TERMCMD}
                if [ $? -ne 0 ]; then
                    echo "Fatal: '${TERMCMD}' not found. Check TERMCMD variable."
                    exit 1
                fi
        """
        self.d.setVar("devshell_do_devshell", devshell)
        self.d.setVarFlag("devshell_do_devshell", "func", True)
        self.d.setVar("GNOME_TERMCMD", "gnome-terminal --disable-factory -t \"$TERMWINDOWTITLE\"")
        self.d.setVar("TERMCMD", "${GNOME_TERMCMD}")
        signature = bb.data_values.Signature(self.d, keys=["do_devshell"])
        self.assertEquals(signature.md5.digest(),
                          'h HM\xea1\x90\xdeB[iV\xc7\xd9@3')

    def test_reference_to_reference(self):
        self.d.setVar("FOO", "-${BAR}-")
        self.d.setVar("BAR", "+${BAZ}+")
        self.d.setVar("BAZ", "alpha")
        signature = bb.data_values.Signature(self.d, keys=["FOO"])
        self.assertEquals(set(signature.data.keys()), set(["FOO", "BAR", "BAZ"]))

    def test_reference_to_reference_shell(self):
        self.d.setVar("alpha", "echo; beta")
        self.d.setVarFlag("alpha", "func", True)
        self.d.setVar("beta", "theta; echo")
        self.d.setVarFlag("beta", "func", True)
        self.d.setVar("theta", "echo foo")
        self.d.setVarFlag("theta", "func", True)
        signature = bb.data_values.Signature(self.d, keys=["alpha"])
        self.assertEquals(set(signature.data.keys()), set(["alpha", "beta", "theta"]))

    def test_oe_madness(self):
        prune = """
def base_prune_suffix(var, suffixes, d):
    # See if var ends with any of the suffixes listed and
    # remove it if found
    for suffix in suffixes:
        if var.endswith(suffix):
            return var.replace(suffix, "")
    return var
globals()['base_prune_suffix'] = base_prune_suffix
"""
        bb.utils.simple_exec(prune, {})
        self.d.setVar("BPN", "${@base_prune_suffix('${PN}', '${SPECIAL_PKGSUFFIX}'.split(), d)}")
        self.d.setVar("BP", "${BPN}-${PV}")
        self.d.setVar("S", "${WORKDIR}/${BP}")
        self.d.setVar("WORKDIR", "/tmp")

        self.assertEquals(bb.data_values.new_value("BPN", self.d).references,
                          set(["PN", "SPECIAL_PKGSUFFIX"]))
        self.assertEquals(bb.data_values.new_value("BP", self.d).references,
                          set(["BPN", "PV"]))
        self.assertEquals(bb.data_values.new_value("S", self.d).references,
                          set(["WORKDIR", "BP"]))

        signature = bb.data_values.Signature(self.d, keys=["S"])
        self.assertEquals(set(signature.data.keys()), set(["S", "BP", "BPN", "PV", "PN", "SPECIAL_PKGSUFFIX"]))

    def test_methodpool_function(self):
        self.d.setVar("SOMEVAR", "${@foo(d)}")
        foostr = "bb.data.getVar('LIBC', True, d)"
        self.d.setVar("foo", foostr)
        self.d.setVarFlag("foo", "func", True)
        self.d.setVarFlag("foo", "python", True)
        bb.utils.simple_exec("def foo(d):\n    %s\nglobals()['foo'] = foo" % foostr, {})

        signature = bb.data_values.Signature(self.d, ("SOMEVAR",))
        self.assertEquals(set(signature.data.iterkeys()), set(["SOMEVAR", "foo", "LIBC"]))

    def test_exported_vars(self):
        self.d.setVar("do_something", "./configure")
        self.d.setVarFlag("do_something", "func", True)
        self.d.setVar("CFLAGS", "-O2")
        self.d.setVarFlag("CFLAGS", "export", True)

        signature = bb.data_values.Signature(self.d, ("do_something",))
        self.assertEquals(set(signature.data.iterkeys()), set(["do_something", "CFLAGS"]))

class TestVisiting(unittest.TestCase):
    def setUp(self):
        self.d = bb.data.init()
        self.d.setVar("FOO", "-${BAR}- ${BAZ}")
        self.d.setVar("BAR", "bar value")
        self.d.setVar("BAZ", "baz value")
        self.d.setVar("REC", "${${BAR}}")
        self.d.setVar("bar value", "bar value value")

    def test_visitor(self):
        value = bb.data_values.new_value("FOO", self.d)
        visitor = bb.data_values.References(True)
        visitor.visit(value)
        self.assertEqual(visitor.references, set(["BAR", "BAZ"]))

        value = bb.data_values.new_value("REC", self.d)
        visitor = bb.data_values.References(True)
        visitor.visit(value)
        self.assertEqual(visitor.references, set(["BAR", "bar value"]))


        self.d.setVar("alpha", "foo ${FOO}")
        value = bb.data_values.new_value("alpha", self.d)

        visitor = bb.data_values.References(True)
        visitor.visit(value)
        self.assertEqual(visitor.references, set(["FOO", "BAR", "BAZ"]))

        visitor = bb.data_values.References(False)
        visitor.visit(value)
        self.assertEqual(visitor.references, set(["FOO"]))

    def test_transformer(self):
        resolver = bb.data_values.Resolver()
        value = bb.data_values.new_value("FOO", self.d)
        self.assertEqual(resolver.visit(value), "-bar value- baz value")
        blacklisted = bb.data_values.Blacklister(lambda name: name in ["BAZ"]).visit(value)
        self.assertEqual(bb.data_values.stable_repr(blacklisted),
                         "Value(['-', VariableRef(['BAR']), '- ', '${BAZ}'])")
        self.assertEqual(resolver.visit(blacklisted), "-bar value- ${BAZ}")
        self.assertEqual(resolver.visit(bb.data_values.PythonSnippet.from_string("5 * 3", self.d)), "15")
        self.assertEqual(resolver.visit(bb.data_values.Value.from_string("${@5 * 3}", self.d)), "15")
        self.d.setVar("bar value", "bar value value")
        self.assertEqual(resolver.visit(bb.data_values.Value.from_string("${${BAR}}!", self.d)), "bar value value!")

    def test_path(self):
        foovalue = bb.data_values.new_value("FOO", self.d)
        barvalue = bb.data_values.new_value("BAR", self.d)
        bazvalue = bb.data_values.new_value("BAZ", self.d)
        testobj = self

        class PathCheck(bb.data_values.Visitor):
            def visit_Value(self, node):
                if node == foovalue:
                    testobj.assertEqual(list(self.path), [foovalue])
                elif node == barvalue:
                    testobj.assertEqual(list(self.path),
                                        [foovalue, foovalue.components[1], barvalue])
                elif node == bazvalue:
                    testobj.assertEqual(list(self.path),
                                        [foovalue, foovalue.components[3], bazvalue])
        PathCheck(crossref=True).visit(foovalue)

    def test_noncrossref_resolver(self):
        value = bb.data_values.new_value("FOO", self.d)
        resolver = bb.data_values.Resolver(False)
        self.assertEqual(resolver.visit(value), "-${BAR}- ${BAZ}")

    def test_noncrossref_resolver_python(self):
        self.d.setVar("alpha", "${@5 * 3}")
        value = bb.data_values.new_value("alpha", self.d)
        resolver = bb.data_values.Resolver(False)
        self.assertEqual(resolver.visit(value), "${@5 * 3}")


if __name__ == "__main__":
    unittest.main()
