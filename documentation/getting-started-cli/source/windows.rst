Notes for Windows Users
=======================

For users trying out the command line tools under Windows, there are
some important differences.

dylan-compiler
--------------

On Windows, the ``dylan-compiler`` executable is called
``dylan-compiler-with-tools.exe``.

The IDE is ``win32-environment.exe``.

Both are located in ``C:\Program Files\Open Dylan\bin\`` on
a 32 bit Windows version. On 64 bit Windows, they can be found
within ``C:\Program Files (x86)\Open Dylan\bin\``.

Build Products Location
-----------------------

Instead of placing build products within a ``_build`` directory,
they are stored in a single location by default::

    %APPDATA%\Open-Dylan\

The ``APPDATA`` defaults to ``C:\Users\...\AppData\Roaming\``.

This can be modified by setting the ``OPEN_DYLAN_USER_ROOT``
environment variable.

Setting Environment Variables
-----------------------------

Environment variables are managed differently on Windows.
For modifying environment variables permanently, see
`this guide <http://www.computerhope.com/issues/ch000549.htm>`_.

For modifying an environment variable from the command line,
use this syntax::

    set variable=value

