# BitBake Persistent Data Store
#
# Copyright (C) 2007        Richard Purdie
# Copyright (C) 2010        Chris Larson <chris_larson@mentor.com>
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

import bb.msg, bb.data, bb.utils
import collections
import os.path
import logging
import warnings

try:
    import sqlite3
except ImportError:
    from pysqlite2 import dbapi2 as sqlite3

sqlversion = sqlite3.sqlite_version_info
if sqlversion[0] < 3 or (sqlversion[0] == 3 and sqlversion[1] < 3):
    raise Exception("sqlite3 version 3.3.0 or later is required.")

logger = logging.getLogger("BitBake.PersistData")

class SQLTable(collections.MutableMapping):
    def __init__(self, cursor, table):
        self.cursor = cursor
        self.table = table

        cursor.execute("CREATE TABLE IF NOT EXISTS %s(key TEXT, value TEXT);" % table)

    def _execute(self, *query):
        while True:
            try:
                self.cursor.execute(*query)
                return
            except sqlite3.OperationalError as e:
                if 'database is locked' in str(e):
                    continue
                raise

    def __getitem__(self, key):
        data = self.cursor.execute("SELECT * from %s where key=?;" % self.table, [key])
        for row in data:
            return row[1]

    def __delitem__(self, key):
        self._execute("DELETE from %s where key=?;" % self.table, [key])

    def __setitem__(self, key, value):
        data = self.cursor.execute("SELECT * from %s where key=?;" % self.table, [key])
        rows = 0
        for row in data:
            rows = rows + 1
        if rows:
            self._execute("UPDATE %s SET value=? WHERE key=?;" % self.table, [value, key])
        else:
            self._execute("INSERT into %s(key, value) values (?, ?);" % self.table, [key, value])

    def __contains__(self, key):
        return key in set(self)

    def __len__(self):
        data = self.cursor.execute("SELECT COUNT(key) FROM %s;" % self.table)
        for row in data:
            return row[0]

    def __iter__(self):
        data = self.cursor.execute("SELECT key FROM %s;" % self.table)
        for row in data:
            yield row[0]

    def iteritems(self):
        data = self.cursor.execute("SELECT * FROM %s;" % self.table)
        for row in data:
            yield row[0], row[1]

    def itervalues(self):
        data = self.cursor.execute("SELECT value FROM %s;" % self.table)
        for row in data:
            yield row[0]

class SQLData(object):
    def __init__(self, filename, connection = None):
        if not connection:
            bb.utils.mkdirhier(os.path.dirname(filename))
            connection = sqlite3.connect(filename, timeout=5, isolation_level=None)

        self.filename = filename
        self.connection = connection
        self.cursor = connection.cursor()
        self._tables = {}

    def __getitem__(self, table):
        if not isinstance(table, basestring):
            raise TypeError("table argument must be a string, not '%s'" % type(table))

        if table in self._tables:
            return self._tables[table]
        else:
            tableobj = self._tables[table] = SQLTable(self.cursor, table)
            return tableobj

    def __delitem__(self, table):
        if table in self._tables:
            del self._tables[table]
        self.cursor.execute("DROP TABLE IF EXISTS %s;" % table)

def persist(d, conncache=None):
    cachedir = (bb.data.getVar("PERSISTENT_DIR", d, True) or
                bb.data.getVar("CACHE", d, True))
    if not cachedir:
        bb.msg.fatal(bb.msg.domain.PersistData, "Please set the 'PERSISTENT_DIR' or 'CACHE' variable.")

    if conncache and "connection" in conncache:
        connection = conncache["connection"]
    else:
        connection = None

    cachefile = os.path.join(cachedir, "bb_persist_data.sqlite3")
    data = SQLData(cachefile, connection)

    if conncache is not None:
        conncache["connection"] = data.connection
    return data

class PersistData(object):
    """
    BitBake Persistent Data Store

    Used to store data in a central location such that other threads/tasks can
    access them at some future date.

    The "domain" is used as a key to isolate each data pool and in this
    implementation corresponds to an SQL table. The SQL table consists of a
    simple key and value pair.

    Why sqlite? It handles all the locking issues for us.
    """
    def __init__(self, d, persistent_database_connection):
        warnings.warn("Use of PersistData will be deprecated in the future",
                      category=PendingDeprecationWarning,
                      stacklevel=2)
        self.cachedir = bb.data.getVar("PERSISTENT_DIR", d, True) or bb.data.getVar("CACHE", d, True)
        if not self.cachedir:
            bb.msg.fatal(bb.msg.domain.PersistData, "Please set the 'PERSISTENT_DIR' or 'CACHE' variable.")

        self.cachefile = os.path.join(self.cachedir, "bb_persist_data.sqlite3")
        logger.debug(1, "Using '%s' as the persistent data cache", self.cachefile)

        if "connection" in persistent_database_connection:
            connection = persistent_database_connection["connection"]
        else:
            connection = None
        self.data = SQLData(self.cachefile, connection)

    def addDomain(self, domain):
        """
        Add a domain (pending deprecation)
        """
        self.data[domain]

    def delDomain(self, domain):
        """
        Removes a domain and all the data it contains
        """
        del self.data[domain]

    def getKeyValues(self, domain):
        """
        Return a list of key + value pairs for a domain
        """
        return self.data[domain].items()

    def getValue(self, domain, key):
        """
        Return the value of a key for a domain
        """
        return self.data[domain][key]

    def setValue(self, domain, key, value):
        """
        Sets the value of a key for a domain
        """
        self.data[domain][key] = value

    def delValue(self, domain, key):
        """
        Deletes a key/value pair
        """
        del self.data[domain][key]
