# ex:ts=4:sw=4:sts=4:et
# -*- tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
"""
BitBake Utility Functions
"""

# Copyright (C) 2004 Michael Lauer
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

import fcntl, os, stat, shutil, time
import sys
import bb
import errno
import bb.msg
from commands import getstatusoutput
from distutils.version import LooseVersion as Version

# Context used in better_exec, eval
_context = {
    "os": os,
    "bb": bb,
    "time": time,
}

def vercmp(alpha, beta):
    return cmp(tuple(Version(i) for i in alpha),
               tuple(Version(i) for i in beta))

def vercmp_string(version1, version2):
    return cmp(Version(version1), Version(version2))

def explode_deps(s):
    """
    Take an RDEPENDS style string of format:
    "DEPEND1 (optional version) DEPEND2 (optional version) ..."
    and return a list of dependencies.
    Version information is ignored.
    """
    r = []
    l = s.split()
    flag = False
    for i in l:
        if i[0] == '(':
            flag = True
            #j = []
        if not flag:
            r.append(i)
        #else:
        #    j.append(i)
        if flag and i.endswith(')'):
            flag = False
            # Ignore version
            #r[-1] += ' ' + ' '.join(j)
    return r

def explode_dep_versions(s):
    """
    Take an RDEPENDS style string of format:
    "DEPEND1 (optional version) DEPEND2 (optional version) ..."
    and return a dictionary of dependencies and versions.
    """
    r = {}
    l = s.split()
    lastdep = None
    lastver = ""
    inversion = False
    for i in l:
        if i[0] == '(':
            inversion = True
            lastver = i[1:] or ""
            #j = []
        elif inversion and i.endswith(')'):
            inversion = False
            lastver = lastver + " " + (i[:-1] or "")
            r[lastdep] = lastver
        elif not inversion:
            r[i] = None
            lastdep = i
            lastver = ""
        elif inversion:
            lastver = lastver + " " + i

    return r

def join_deps(deps):
    """
    Take the result from explode_dep_versions and generate a dependency string
    """
    result = []
    for dep in deps:
        if deps[dep]:
            result.append(dep + " (" + deps[dep] + ")")
        else:
            result.append(dep)
    return ", ".join(result)

def _print_trace(body, line):
    """
    Print the Environment of a Text Body
    """
    # print the environment of the method
    bb.msg.error(bb.msg.domain.Util, "Printing the environment of the function")
    min_line = max(1, line-4)
    max_line = min(line + 4, len(body)-1)
    for i in range(min_line, max_line + 1):
        bb.msg.error(bb.msg.domain.Util, "\t%.4d:%s" % (i, body[i-1]) )


def better_compile(text, file, realfile, mode = "exec"):
    """
    A better compile method. This method
    will print  the offending lines.
    """
    try:
        return compile(text, file, mode)
    except Exception as e:
        # split the text into lines again
        body = text.split('\n')
        bb.msg.error(bb.msg.domain.Util, "Error in compiling python function in: %s" % (realfile))
        bb.msg.error(bb.msg.domain.Util, "The lines leading to this error were:")
        bb.msg.error(bb.msg.domain.Util, "\t%d:%s:'%s'" % (e.lineno, e.__class__.__name__, body[e.lineno-1]))

        _print_trace(body, e.lineno)

        # exit now
        sys.exit(1)

def better_exec(code, context, text, realfile):
    """
    Similiar to better_compile, better_exec will
    print the lines that are responsible for the
    error.
    """
    import bb.parse
    try:
        exec(code, _context, context)
    except:
        (t, value, tb) = sys.exc_info()

        if t in [bb.parse.SkipPackage, bb.build.FuncFailed]:
            raise

        # print the Header of the Error Message
        bb.msg.error(bb.msg.domain.Util, "Error in executing python function in: %s" % realfile)
        bb.msg.error(bb.msg.domain.Util, "Exception:%s Message:%s" % (t, value))

        # let us find the line number now
        while tb.tb_next:
            tb = tb.tb_next

        import traceback
        line = traceback.tb_lineno(tb)

        _print_trace( text.split('\n'), line )

        raise

def simple_exec(code, context):
    exec(code, _context, context)

def better_eval(source, locals):
    return eval(source, _context, locals)

def lockfile(name):
    """
    Use the file fn as a lock file, return when the lock has been acquired.
    Returns a variable to pass to unlockfile().
    """
    path = os.path.dirname(name)
    if not os.path.isdir(path):
        bb.msg.error(bb.msg.domain.Util, "Error, lockfile path does not exist!: %s" % path)
        sys.exit(1)

    while True:
        # If we leave the lockfiles lying around there is no problem
        # but we should clean up after ourselves. This gives potential
        # for races though. To work around this, when we acquire the lock
        # we check the file we locked was still the lock file on disk.
        # by comparing inode numbers. If they don't match or the lockfile
        # no longer exists, we start again.

        # This implementation is unfair since the last person to request the
        # lock is the most likely to win it.

        try:
            lf = open(name, "a + ")
            fcntl.flock(lf.fileno(), fcntl.LOCK_EX)
            statinfo = os.fstat(lf.fileno())
            if os.path.exists(lf.name):
                statinfo2 = os.stat(lf.name)
                if statinfo.st_ino == statinfo2.st_ino:
                    return lf
            # File no longer exists or changed, retry
            lf.close
        except Exception as e:
            continue

def unlockfile(lf):
    """
    Unlock a file locked using lockfile()
    """
    os.unlink(lf.name)
    fcntl.flock(lf.fileno(), fcntl.LOCK_UN)
    lf.close

def md5_file(filename):
    """
    Return the hex string representation of the MD5 checksum of filename.
    """
    try:
        import hashlib
        m = hashlib.md5()
    except ImportError:
        import md5
        m = md5.new()

    for line in open(filename):
        m.update(line)
    return m.hexdigest()

def sha256_file(filename):
    """
    Return the hex string representation of the 256-bit SHA checksum of
    filename.  On Python 2.4 this will return None, so callers will need to
    handle that by either skipping SHA checks, or running a standalone sha256sum
    binary.
    """
    try:
        import hashlib
    except ImportError:
        return None

    s = hashlib.sha256()
    for line in open(filename):
        s.update(line)
    return s.hexdigest()

def preserved_envvars_list():
    return [
        'BBPATH',
        'BB_PRESERVE_ENV',
        'BB_ENV_WHITELIST',
        'BB_ENV_EXTRAWHITE',
        'COLORTERM',
        'DBUS_SESSION_BUS_ADDRESS',
        'DESKTOP_SESSION',
        'DESKTOP_STARTUP_ID',
        'DISPLAY',
        'GNOME_KEYRING_PID',
        'GNOME_KEYRING_SOCKET',
        'GPG_AGENT_INFO',
        'GTK_RC_FILES',
        'HOME',
        'LANG',
        'LOGNAME',
        'PATH',
        'PWD',
        'SESSION_MANAGER',
        'SHELL',
        'SSH_AUTH_SOCK',
        'TERM',
        'USER',
        'USERNAME',
        '_',
        'XAUTHORITY',
        'XDG_DATA_DIRS',
        'XDG_SESSION_COOKIE',
    ]

def filter_environment(good_vars):
    """
    Create a pristine environment for bitbake. This will remove variables that
    are not known and may influence the build in a negative way.
    """

    removed_vars = []
    for key in os.environ.keys():
        if key in good_vars:
            continue

        removed_vars.append(key)
        os.unsetenv(key)
        del os.environ[key]

    if len(removed_vars):
        bb.msg.debug(1, bb.msg.domain.Util, "Removed the following variables from the environment: %s" % (", ".join(removed_vars)))

    return removed_vars

def clean_environment():
    """
    Clean up any spurious environment variables. This will remove any
    variables the user hasn't chose to preserve.
    """
    if 'BB_PRESERVE_ENV' not in os.environ:
        if 'BB_ENV_WHITELIST' in os.environ:
            good_vars = os.environ['BB_ENV_WHITELIST'].split()
        else:
            good_vars = preserved_envvars_list()
        if 'BB_ENV_EXTRAWHITE' in os.environ:
            good_vars.extend(os.environ['BB_ENV_EXTRAWHITE'].split())
        filter_environment(good_vars)

def empty_environment():
    """
    Remove all variables from the environment.
    """
    for s in os.environ.keys():
        os.unsetenv(s)
        del os.environ[s]

def build_environment(d):
    """
    Build an environment from all exported variables.
    """
    import bb.data
    for var in bb.data.keys(d):
        export = bb.data.getVarFlag(var, "export", d)
        if export:
            os.environ[var] = bb.data.getVar(var, d, True) or ""

def prunedir(topdir):
    # Delete everything reachable from the directory named in 'topdir'.
    # CAUTION:  This is dangerous!
    for root, dirs, files in os.walk(topdir, topdown = False):
        for name in files:
            os.remove(os.path.join(root, name))
        for name in dirs:
            if os.path.islink(os.path.join(root, name)):
                os.remove(os.path.join(root, name))
            else:
                os.rmdir(os.path.join(root, name))
    os.rmdir(topdir)

#
# Could also use return re.compile("(%s)" % "|".join(map(re.escape, suffixes))).sub(lambda mo: "", var)
# but thats possibly insane and suffixes is probably going to be small
#
def prune_suffix(var, suffixes, d):
    # See if var ends with any of the suffixes listed and
    # remove it if found
    for suffix in suffixes:
        if var.endswith(suffix):
            return var.replace(suffix, "")
    return var

def mkdirhier(dir):
    """Create a directory like 'mkdir -p', but does not complain if
    directory already exists like os.makedirs
    """

    bb.msg.debug(3, bb.msg.domain.Util, "mkdirhier(%s)" % dir)
    try:
        os.makedirs(dir)
        bb.msg.debug(2, bb.msg.domain.Util, "created " + dir)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise e

def movefile(src, dest, newmtime = None, sstat = None):
    """Moves a file from src to dest, preserving all permissions and
    attributes; mtime will be preserved even when moving across
    filesystems.  Returns true on success and false on failure. Move is
    atomic.
    """

    #print "movefile(" + src + "," + dest + "," + str(newmtime) + "," + str(sstat) + ")"
    try:
        if not sstat:
            sstat = os.lstat(src)
    except Exception as e:
        print("movefile: Stating source file failed...", e)
        return None

    destexists = 1
    try:
        dstat = os.lstat(dest)
    except:
        dstat = os.lstat(os.path.dirname(dest))
        destexists = 0

    if destexists:
        if stat.S_ISLNK(dstat[stat.ST_MODE]):
            try:
                os.unlink(dest)
                destexists = 0
            except Exception as e:
                pass

    if stat.S_ISLNK(sstat[stat.ST_MODE]):
        try:
            target = os.readlink(src)
            if destexists and not stat.S_ISDIR(dstat[stat.ST_MODE]):
                os.unlink(dest)
            os.symlink(target, dest)
            #os.lchown(dest,sstat[stat.ST_UID],sstat[stat.ST_GID])
            os.unlink(src)
            return os.lstat(dest)
        except Exception as e:
            print("movefile: failed to properly create symlink:", dest, "->", target, e)
            return None

    renamefailed = 1
    if sstat[stat.ST_DEV] == dstat[stat.ST_DEV]:
        try:
            os.rename(src, dest)
            renamefailed = 0
        except Exception as e:
            if e[0] != errno.EXDEV:
                # Some random error.
                print("movefile: Failed to move", src, "to", dest, e)
                return None
            # Invalid cross-device-link 'bind' mounted or actually Cross-Device

    if renamefailed:
        didcopy = 0
        if stat.S_ISREG(sstat[stat.ST_MODE]):
            try: # For safety copy then move it over.
                shutil.copyfile(src, dest + "#new")
                os.rename(dest + "#new", dest)
                didcopy = 1
            except Exception as e:
                print('movefile: copy', src, '->', dest, 'failed.', e)
                return None
        else:
            #we don't yet handle special, so we need to fall back to /bin/mv
            a = getstatusoutput("/bin/mv -f " + "'" + src + "' '" + dest + "'")
            if a[0] != 0:
                print("movefile: Failed to move special file:" + src + "' to '" + dest + "'", a)
                return None # failure
        try:
            if didcopy:
                os.lchown(dest, sstat[stat.ST_UID], sstat[stat.ST_GID])
                os.chmod(dest, stat.S_IMODE(sstat[stat.ST_MODE])) # Sticky is reset on chown
                os.unlink(src)
        except Exception as e:
            print("movefile: Failed to chown/chmod/unlink", dest, e)
            return None

    if newmtime:
        os.utime(dest, (newmtime, newmtime))
    else:
        os.utime(dest, (sstat[stat.ST_ATIME], sstat[stat.ST_MTIME]))
        newmtime = sstat[stat.ST_MTIME]
    return newmtime

def copyfile(src, dest, newmtime = None, sstat = None):
    """
    Copies a file from src to dest, preserving all permissions and
    attributes; mtime will be preserved even when moving across
    filesystems.  Returns true on success and false on failure.
    """
    #print "copyfile(" + src + "," + dest + "," + str(newmtime) + "," + str(sstat) + ")"
    try:
        if not sstat:
            sstat = os.lstat(src)
    except Exception as e:
        print("copyfile: Stating source file failed...", e)
        return False

    destexists = 1
    try:
        dstat = os.lstat(dest)
    except:
        dstat = os.lstat(os.path.dirname(dest))
        destexists = 0

    if destexists:
        if stat.S_ISLNK(dstat[stat.ST_MODE]):
            try:
                os.unlink(dest)
                destexists = 0
            except Exception as e:
                pass

    if stat.S_ISLNK(sstat[stat.ST_MODE]):
        try:
            target = os.readlink(src)
            if destexists and not stat.S_ISDIR(dstat[stat.ST_MODE]):
                os.unlink(dest)
            os.symlink(target, dest)
            #os.lchown(dest,sstat[stat.ST_UID],sstat[stat.ST_GID])
            return os.lstat(dest)
        except Exception as e:
            print("copyfile: failed to properly create symlink:", dest, "->", target, e)
            return False

    if stat.S_ISREG(sstat[stat.ST_MODE]):
        try: # For safety copy then move it over.
            shutil.copyfile(src, dest + "#new")
            os.rename(dest + "#new", dest)
        except Exception as e:
            print('copyfile: copy', src, '->', dest, 'failed.', e)
            return False
    else:
        #we don't yet handle special, so we need to fall back to /bin/mv
        a = getstatusoutput("/bin/cp -f " + "'" + src + "' '" + dest + "'")
        if a[0] != 0:
            print("copyfile: Failed to copy special file:" + src + "' to '" + dest + "'", a)
            return False # failure
    try:
        os.lchown(dest, sstat[stat.ST_UID], sstat[stat.ST_GID])
        os.chmod(dest, stat.S_IMODE(sstat[stat.ST_MODE])) # Sticky is reset on chown
    except Exception as e:
        print("copyfile: Failed to chown/chmod/unlink", dest, e)
        return False

    if newmtime:
        os.utime(dest, (newmtime, newmtime))
    else:
        os.utime(dest, (sstat[stat.ST_ATIME], sstat[stat.ST_MTIME]))
        newmtime = sstat[stat.ST_MTIME]
    return newmtime

def which(path, item, direction = 0):
    """
    Locate a file in a PATH
    """

    paths = (path or "").split(':')
    if direction != 0:
        paths.reverse()

    for p in paths:
        next = os.path.join(p, item)
        if os.path.exists(next):
            return next

    return ""

def init_logger(logger, verbose, debug, debug_domains):
    """
    Set verbosity and debug levels in the logger
    """

    if verbose:
        logger.set_verbose(True)

    if debug:
        logger.set_debug_level(debug)
    else:
        logger.set_debug_level(0)

    if debug_domains:
        logger.set_debug_domains(debug_domains)
