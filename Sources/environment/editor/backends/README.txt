Synopsis:  Notes on the editor backends
Author:    Hugh Greene
Copyright: 1997 Functional Objects, Inc. All rights reserved.

CRiSP
-----

Not yet implemented.


DDE
---

Not yet implemented.


Emacsserver
-----------

You must have "emacs19" and "emacsclient19" in your PATH for this
backend to work.  Also, your ".emacs" file should contain ''(require
'server)'' (or ''(load-library "server")'') and then
''(server-start)'' (to start up the emacsserver process
automatically).

This backend will launch emacs19 if the emacsserver process is not
already running, or use emacsclient to access a running Emacs if it
is.

[The gnuserv backend is similar but requires the "gnuserv" package,
which is not part of the standard Emacs distribution.  That backend
works less well on Unix but as well on WinNT (where emacsserver is
unavailable).]


Exe
---

This is a "abstract" backend for editors accessed in a command-line
based way (e.g., Emacs and Vi).


Gnuserv
-------

You must have "gnudoit" in your PATH for this backend to work.  Also,
your ".emacs" (called "_emacs" on WinNT) file should contain
''(require 'gnuserv)'' (or ''(load-library "gnuserv")'') and then
''(start-gnuserv)'' (to start up the gnuserv process automatically).

[NOTE for WinNT: Emacs looks for the "_emacs" file in the directory
specified by the "HOME" environment variable.  However, this is not
set by default under WinNT, so it may need to be set manually.  If it
is unset and Emacs consequently fails to find the "_emacs" file, the
gnuserv process will not be started.  The result will be that each
editor command will launch a separate instance of Emacs and fail to
get it to do anything.]

Under WinNT, this backend will launch emacs19 if it is not already
running, or use gnudoit to access a running Emacs if it is.  Under
Unix, emacs19 (and the gnuserv process) must already be running.
(This is a limitation of the Unix version of the gnuserv package.)


LispWorks
---------

This is a backend which uses the LispWorks editor directly.  Therefore
it can only be used in the emulator-environment.


Vi
--

You must have "xterm" and "vi" in your PATH for this backend to work.


Win32 Shell
-----------

This backend performs the Win32 Shell's "open" action on the
appropriate file; i.e., acts as if you had right-clicked on the file
and selected "Open".  I'm not yet sure what it will do for blank
strings, file extensions with no "Open" action (such as ".dylan" and
".lid", normally) and other odd cases.
