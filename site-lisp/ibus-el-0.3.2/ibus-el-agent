#! /usr/bin/env python
# -*- coding: utf-8 -*-

# ibus-el-agent --- helper program of IBus client for GNU Emacs
# Copyright (c) 2010-2012 and onwards, S. Irie

# Author: S. Irie
# Maintainer: S. Irie
# Version: 0.3.2

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# IBus is a new input method framework under active development
# which is designed to overcome the limitations of SCIM.

# IBus uses D-Bus protocol for communication between the ibus-daemon
# and clients (engines, panel, config tools). Since the components
# run in separate processes there is enhanced modularity and stability.
# Client processes can be loaded, started and stopped independently.
# IBus supports Gtk2 and XIM, and has input method engines for anthy,
# chewing, hangul, m17n, pinyin, rawcode, and large tables. Engines
# and clients can be written in any language with a dbus binding.

# This program is IBus client for GNU Emacs. It is, however,
# not part of official IBus project.

# ChangeLog:

# 2012-02-29  S. Irie
#         * Version 0.3.2
#         * Change command line options for ibus-daemon ("-d" -> "-dx")
#         * Bug fix
#
# 2012-02-14  S. Irie
#         * Version 0.3.1
#         * Bug fix
#
# 2011-12-24  S. Irie
#         * Version 0.3.0
#         * Add support for surrounding text
#         * Implement list_active_engines() for `ibus-enable-specified-engine'
#         * Improve input focus detection
#         * Bug fixes
#
# 2010-11-03  S. Irie
#         * Version 0.2.1
#         * Bug fixes
#
# 2010-08-19  S. Irie
#         * Version 0.2.0
#         * Added Frame class in order to:
#           - observe input focus
#           - obtain frame absolute coordinates
#         * Added -q option to quit if ibus-daemon is not running
#         * Fixed shift modifier bug
#         * Bug fix
#
# 2010-06-11  S. Irie
#         * Version 0.1.1
#         * Changed to reduce inter-process communication
#         * Bug fix
#
# 2010-05-29  S. Irie
#         * Version 0.1.0
#         * Initial release
#
# 2010-05-09  S. Irie
#         * Version 0.0.2
#
# 2010-04-12  S. Irie
#         * Version 0.0.1
#         * Initial experimental version

# Code:

import sys
import glib
import re

import ibus
from ibus import modifier

########################################################################
# Process command line option
########################################################################

start_ibus_daemon = True
use_surrounding_text = False

if __name__ == "__main__":

    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-q", "--quit",
                      action="store_true", dest="quit", default=False,
                      help="quit if ibus-daemon is not running")
    parser.add_option("-s", "--surrounding-text",
                      action="store_true", dest="surrounding_text", default=True,
                      help="enable surrounding text support")
    options, args = parser.parse_args()
    if options.quit:
        start_ibus_daemon = False
    if options.surrounding_text \
            and map(int, ibus.get_version().split(".")) >= [1, 4, 0]:
        use_surrounding_text = True
        print "Surrounding text support enabled"

########################################################################
# Setup Xlib
########################################################################

try:
    import Xlib.display
    import Xlib.X
    import Xlib.Xatom
except ImportError:
    print "python-xlib library is required"
    exit(1)

class Frame(object):

    def __init__(self, display):
        self.display = display
        self.window_id = None
        self.window = None
        self.root_window = None
        self.left = 0
        self.top = 0
        self.focus = None
        self.focus_cb_id = None
        display.set_error_handler(self.__error_handler)

    # When the active window vanishes, focus.query_tree() etc. may cause
    # BadWIndow error unwantedly.  This handler quiets all Xlib's errors,
    # but we still can check them in "*ibus-mode log*" buffer if necessary.
    def __error_handler(self, error, request):
        print '("%s")'%("%s"%error).replace('"', '\\"')

    def __update_focus_cb(self, form, sync=False):
        # Check FocusIn/FocusOut events
        if self.focus:
            focus_in = None
            focus_out = None
            for i in range(display.pending_events()):
                event = display.next_event()
                if event.type == Xlib.X.FocusIn:
                    focus_in = event.window
                elif event.type == Xlib.X.FocusOut:
                    focus_out = event.window
            if focus_in == self.focus:
                if focus_out != self.focus:
                    # This is ugly workaround, but necessary to avoid a
                    # problem that the language bar doesn't appear after
                    # moving to other workspace in Ubuntu Unity desktop.
                    # If old version of ibus.el which doesn't define
                    # `ibus-redo-focus-in-cb' is running on Emacs, the
                    # following message will just be ignored and take no effect.
                    print '(ibus-redo-focus-in-cb)'
                if sync:
                    print form % focus_in.id
                return True
        # Main part
        focus = self.display.get_input_focus().focus
        try:
            # get_input_focus() may return an integer 0 that query_tree()
            # causes AttributeError when X session is going to logout.
            tree = focus.query_tree()
            # In Ubuntu's Unity desktop, get_input_focus() often returns root
            # window incorrectly after changing workspace.
            if focus != tree.root:
                if not (focus.get_wm_class() or focus.get_wm_name()):
                    focus = tree.parent
                if focus != self.focus or sync:
                    print form % focus.id
                    focus.change_attributes(event_mask=Xlib.X.FocusChangeMask)
                    self.focus = focus
                return True
        except AttributeError:
            if sync:
                print form % 0
            return True
        # Fallback
        atom = display.get_atom("_NET_ACTIVE_WINDOW", True)
        focus_id = tree.root.get_property(atom, Xlib.Xatom.WINDOW, 0, 1).value[0]
        focus = display.create_resource_object("window", focus_id)
        if focus != self.focus or sync:
            print form % focus_id
            focus.change_attributes(event_mask=Xlib.X.FocusChangeMask)
            self.focus = focus
        return True

    def stop_focus_observation(self):
        self.focus = None
        if self.focus_cb_id:
            glib.source_remove(self.focus_cb_id)
            self.focus_cb_id = None

    def start_focus_observation(self, interval):
        self.stop_focus_observation()
        self.__update_focus_cb("(ibus-start-focus-observation-cb %d)", True)
        self.focus_cb_id = glib.timeout_add(interval, self.__update_focus_cb,
                                            "(ibus-focus-changed-cb %d)")

    def update_coordinates(self):
        if self.window:
            absolute = self.root_window.translate_coords(self.window, 0, 0)
            self.left = absolute.x
            self.top = absolute.y

    def set_window_id(self, window_id):
        if window_id != self.window_id:
            self.window = self.display.create_resource_object('window', window_id)
            self.root_window = self.window.get_geometry().root
            self.window_id = window_id

try:
    display = Xlib.display.Display()
except:
    import time
    time.sleep(0.5)
    display = Xlib.display.Display()

frame = Frame(display)

########################################################################
# Connect to IBus daemon
########################################################################

try:
    bus = ibus.Bus()
except:
    if not start_ibus_daemon:
        print '(error "ibus-daemon is not running")'
        exit(1)
    print "Launch IBus daemon..."
    import os
    import time
    if os.spawnlp(os.P_WAIT, "ibus-daemon", "ibus-daemon", "-dx") == 0:
        for i in range(10):
            time.sleep(0.5)
            try:
                bus = ibus.Bus()
                break
            except:
                pass
        else:
            print '(error "Failed to connect with ibus-daemon")'
            exit(1)
    else:
        print '(error "Failed to launch ibus-daemon")'
        exit(1)

########################################################################
# Miscellaneous functions
########################################################################

def lisp_boolean(boolean):
    return "t" if boolean else "nil"

escape_regexp = re.compile(ur'(["\\])')

def escape_string(string):
    return escape_regexp.sub(ur'\\\1', string)

########################################################################
# Input Context
########################################################################

class IBusELInputContext(ibus.InputContext):

    def __init__(self, bus):
        self.__bus = bus
        self.__path = bus.create_input_context("IBusELInputContext")
        super(IBusELInputContext, self).__init__(bus, self.__path, True)

        self.id_no = 0
        self.preediting = False
        self.lookup_table = None
        self.surrouding_text_received = False

        self.connect('commit-text', commit_text_cb)
        self.connect('update-preedit-text', update_preedit_text_cb)
        self.connect('show-preedit-text', show_preedit_text_cb)
        self.connect('hide-preedit-text', hide_preedit_text_cb)
        self.connect('update-auxiliary-text', update_auxiliary_text_cb)
        self.connect('show-auxiliary-text', show_auxiliary_text_cb)
        self.connect('hide-auxiliary-text', hide_auxiliary_text_cb)
        self.connect('update-lookup-table', update_lookup_table_cb)
        self.connect('show-lookup-table', show_lookup_table_cb)
        self.connect('hide-lookup-table', hide_lookup_table_cb)
        self.connect('page-up-lookup-table', page_up_lookup_table_cb)
        self.connect('page-down-lookup-table', page_down_lookup_table_cb)
        self.connect('cursor-up-lookup-table', cursor_up_lookup_table_cb)
        self.connect('cursor-down-lookup-table', cursor_down_lookup_table_cb)
        self.connect('enabled', enabled_cb)
        self.connect('disabled', disabled_cb)
        try:
            self.connect('forward-key-event', forward_key_event_cb)
        except TypeError:
            pass
        try:
            self.connect('delete-surrounding-text', delete_surrounding_text_cb)
        except TypeError:
            pass

########################################################################
# Callbacks
########################################################################

def commit_text_cb(ic, text):
    print '(ibus-commit-text-cb %d "%s")'% \
        (ic.id_no, escape_string(text.text).encode("utf-8"))

def update_preedit_text_cb(ic, text, cursor_pos, visible):
    preediting = len(text.text) > 0
    if preediting or ic.preediting:
        attrs = ['%s %d %d %d'%
                 (["nil", "'underline", "'foreground", "'background"][attr.type],
                  attr.value & 0xffffff, attr.start_index, attr.end_index)
                 for attr in text.attributes]
        print '(ibus-update-preedit-text-cb %d "%s" %d %s %s)'% \
            (ic.id_no, escape_string(text.text).encode("utf-8"),
             cursor_pos, lisp_boolean(visible), ' '.join(attrs))
    ic.preediting = preediting

def show_preedit_text_cb(ic):
    print '(ibus-show-preedit-text-cb %d)'%(ic.id_no)

def hide_preedit_text_cb(ic):
    print '(ibus-hide-preedit-text-cb %d)'%(ic.id_no)

def update_auxiliary_text_cb(ic, text, visible):
    print '(ibus-update-auxiliary-text-cb %d "%s" %s)'% \
        (ic.id_no, escape_string(text.text).encode("utf-8"),
         lisp_boolean(visible))

def show_auxiliary_text_cb(ic):
    print '(ibus-show-auxiliary-text-cb %d)'%(ic.id_no)

def hide_auxiliary_text_cb(ic):
    print '(ibus-hide-auxiliary-text-cb %d)'%(ic.id_no)

def update_lookup_table_cb(ic, lookup_table, visible):
    ic.lookup_table = lookup_table
    if visible:
        self.__show_lookup_table_cb(ic)
    else:
        self.__hide_lookup_table_cb(ic)

def show_lookup_table_cb(ic):
    print "(ibus-show-lookup-table-cb %d '(%s) %s)"% \
        (ic.id_no, escape_string(
            " ".join(map(lambda item : '"%s"'%item.text,
                         ic.lookup_table.get_candidates_in_current_page())
                     )).encode("utf-8"),
         ic.lookup_table.get_cursor_pos_in_current_page())

def hide_lookup_table_cb(ic):
    print '(ibus-hide-lookup-table-cb %d)'%(ic.id_no)

def page_up_lookup_table_cb(ic):
    print '(ibus-log "page up lookup table")'

def page_down_lookup_table_cb(ic):
    print '(ibus-log "page down lookup table")'

def cursor_up_lookup_table_cb(ic):
    print '(ibus-log "cursor up lookup table")'

def cursor_down_lookup_table_cb(ic):
    print '(ibus-log "cursor down lookup table")'

def enabled_cb(ic):
    print '(ibus-status-changed-cb %d "%s")'%(ic.id_no, ic.get_engine().name)
    ic.surrouding_text_received = False

def disabled_cb(ic):
    print '(ibus-status-changed-cb %d nil)'%ic.id_no
    ic.surrouding_text_received = False

def forward_key_event_cb(ic, keyval, keycode, modifiers):
    print '(ibus-forward-key-event-cb %d %d %d %s)'% \
        (ic.id_no, keyval, modifiers & ~modifier.RELEASE_MASK,
         lisp_boolean(modifiers & modifier.RELEASE_MASK == 0))

def delete_surrounding_text_cb(ic, offset, n_chars):
    print '(ibus-delete-surrounding-text-cb %d %d %d)'% \
        (ic.id_no, offset, n_chars)

########################################################################
# Process methods from client
########################################################################

imcontexts = []

def create_imcontext():
    ic = IBusELInputContext(bus)
    try:
        ic.id_no = imcontexts.index(None)
        imcontexts[ic.id_no] = ic
    except ValueError:
        ic.id_no = len(imcontexts)
        imcontexts.append(ic)
    ic.set_capabilities(int('101001' if use_surrounding_text else '001001',2))
    print '(ibus-create-imcontext-cb %d)'%ic.id_no

def destroy_imcontext(id_no):
    imcontexts[id_no].destroy()
    if id_no == len(imcontexts) - 1:
        imcontexts.pop()
    else:
        imcontexts[id_no] = None

def process_key_event(id_no, keyval, modmask, backslash, pressed = None):
    def update_keymap():
        display._update_keymap(display.display.info.min_keycode,
                               (display.display.info.max_keycode
                                - display.display.info.min_keycode + 1))

    def reply(id_no, handled):
        print '(ibus-process-key-event-cb %d %s)'%(id_no, lisp_boolean(handled))
        return False

    ic = imcontexts[id_no]
    if use_surrounding_text and pressed != False \
            and ic.needs_surrounding_text() and not ic.surrouding_text_received:
        print '(ibus-query-surrounding-text-cb %d %d ?\\x%x "%s" "%s")'% \
            (id_no, keyval, modmask, backslash, pressed)
        return

    if backslash:
        keycode = display.keysym_to_keycode(backslash) - 8
        if keycode < 0:
            update_keymap()
            keycode = display.keysym_to_keycode(backslash) - 8
    else:
        keycodes = display.keysym_to_keycodes(keyval)
        if not keycodes:
            update_keymap()
            keycodes = display.keysym_to_keycodes(keyval) or [(0, 0)]
        if modmask & modifier.SHIFT_MASK:
            for keycode_tuple in keycodes:
                if keycode_tuple[1] & 1:
                    break
            else:
                keycode_tuple = keycodes[0]
        elif keyval < 0x100:
            for keycode_tuple in keycodes:
                if keycode_tuple[1]:
                    if keycode_tuple[1] > 1:
                        keycode_tuple = keycodes[0]
                    break
            else:
                keycode_tuple = keycodes[0]
            if keycode_tuple[1] & 1:
                modmask |= modifier.SHIFT_MASK
        else:
            keycode_tuple = keycodes[0]
        keycode = keycode_tuple[0] - 8
    if pressed != None:
        if not pressed:
            modmask |= modifier.RELEASE_MASK
        handled = ic.process_key_event(keyval, keycode, modmask)
    else:
        handled_p = ic.process_key_event(keyval, keycode, modmask)
        handled_r = ic.process_key_event(keyval, keycode,
                                         modmask | modifier.RELEASE_MASK)
        handled = handled_p or handled_r
    glib.idle_add(reply, id_no, handled)

def set_cursor_location(id_no, x, y, w, h):
    imcontexts[id_no].set_cursor_location(max(0, frame.left + x),
                                          frame.top + y, w, h)

def focus_in(id_no):
    imcontexts[id_no].focus_in()

def focus_out(id_no):
    imcontexts[id_no].focus_out()
    print '()' # Dummy response

def reset(id_no):
    imcontexts[id_no].reset()

def enable(id_no):
    imcontexts[id_no].enable()

def disable(id_no):
    imcontexts[id_no].disable()

def set_engine(id_no, name):
    for engine in bus.list_active_engines():
        if name == '%s'%engine.name:
            imcontexts[id_no].set_engine(engine)
            break
    else:
        enable(id_no)

def set_surrounding_text(id_no, text, cursor_pos, anchor_pos):
    ic = imcontexts[id_no]
    ic.set_surrounding_text(text.decode("utf-8"), cursor_pos, anchor_pos)
    ic.surrouding_text_received = True

def update_frame_coordinates(window_id = None):
    if window_id:
        frame.set_window_id(window_id)
    frame.update_coordinates()

def start_focus_observation(interval):
    frame.start_focus_observation(interval)

def stop_focus_observation():
    frame.stop_focus_observation()

def list_active_engines():
    print "(ibus-list-active-engines-cb '(%s))"% \
        ' '.join('"%s"'%i.name for i in bus.list_active_engines())

########################################################################
# Main loop
########################################################################

class IBusModeMainLoop(glib.MainLoop):

    def __init__(self, bus):
        super(IBusModeMainLoop, self).__init__()
        bus.connect("disconnected", self.__disconnected_cb)

    def __disconnected_cb(self, *args):
        print '(ibus-log "disconnected")'
        exit()

    def __start_cb(self):
        print '(setq ibus-version "%s")'%ibus.get_version()
        print '(setq started t)'
        print 'Agent successfully started for display "%s"'% \
            display.get_display_name()
        return False

    def __stdin_cb(self, fd, condition):
        try:
            exec sys.stdin.readline()
        except:
            import traceback
            print '(error "%s")'%traceback.format_exc().replace('"', '\\"')
        return True

    def run(self):
        glib.idle_add(self.__start_cb)
        glib.io_add_watch(0, glib.IO_IN, self.__stdin_cb)
        while True:
            try:
                super(IBusModeMainLoop, self).run()
            except:
                import traceback
                print '(error "%s")'%traceback.format_exc().replace('"', '\\"')
            else:
                break
        for ic in imcontexts:
            if ic:
                ic.destroy()


if __name__ == "__main__":

    mainloop = IBusModeMainLoop(bus)
    mainloop.run()
