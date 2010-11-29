#
# BitBake Process based server.
#
# Copyright (C) 2010 Bob Foerster <robert@erafx.com>
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

"""
    This module implements a multiprocessing.Process based server for bitbake.
"""

from bb.cooker import BBCooker
import time
import bb
from multiprocessing import Process, Event
import signal


class EventAdapter():
    """
    Adapter to wrap our event queue since the caller (bb.event) expects to
    call a send() method, but our actual queue only has put()
    """
    def __init__(self, queue):
        self.queue = queue

    def send(self, event):
        try:
            self.queue.put(event)
        except Exception, err:
            print("EventAdapter puked: %s" % str(err))


class ProcessServer(Process):

    def __init__(self, command_channel, event_queue, configuration):
        Process.__init__(self)
        self.command_channel = command_channel
        self.event_queue = event_queue
        self.event = EventAdapter(event_queue)
        self.configuration = configuration
        self.cooker = BBCooker(configuration, self.register_idle_function)
        self._idlefunctions = {}
        self.event_handle = bb.event.register_UIHhandler(self)
        self.quit = False

        self.keep_running = Event()
        self.keep_running.set()

        for event in bb.event.ui_queue:
            self.event_queue.put(event)

    
    def register_idle_function(self, function, data):
        """Register a function to be called while the server is idle"""
        assert hasattr(function, '__call__')
        self._idlefunctions[function] = data

                
    def run(self):
        # Ignore SIGINT within the server, as all SIGINT handling is done by
        # the UI and communicated to us
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        while self.keep_running.is_set():
            if self.command_channel.poll():
                command = self.command_channel.recv()
                self.runCommand(command)

            self.idle_commands(.1)

        bb.event.unregister_UIHhandler(self.event_handle)
        self.command_channel.close()
        self.cooker.finalize()
        return


    def idle_commands(self, delay):
        #print ("Idle queue length %s, delay" % len(self._idlefunctions), delay)
        nextsleep = delay

        for function, data in self._idlefunctions.items():
            try:
                retval = function(self, data, False)
                #print( "Idle function %s returned %s" % ( function, retval))
                if retval is False:
                    del self._idlefunctions[function]
                elif retval is True:
                    nextsleep = None
                elif nextsleep is None:
                    continue
                elif retval < nextsleep:
                    nextsleep = retval
            except SystemExit:
                raise
            except:
                import traceback
                traceback.print_exc()
                pass
        if nextsleep is not None:
            #print "Sleeping for %s (%s)" % (nextsleep, delay)
            time.sleep(nextsleep)

    def runCommand(self, command):
        """
        Run a cooker command on the server
        """
        self.command_channel.send(self.cooker.command.runCommand(command))

    def stop(self):
        self.keep_running.clear()
