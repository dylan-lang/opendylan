******************
Install Open Dylan
******************

The current version is **Open Dylan 2025.1**.

Unix Platforms
==============

➟ `Download binaries from GitHub <https://github.com/dylan-lang/opendylan/releases/tag/v2025.1.0>`_

Note that Unix versions only have a command-line compiler and not the full IDE that is
available on Windows, although there is some integration with VS Code and Emacs.

All required dependencies (llvm, libgc, libunwind) are included; just unpack
the tarball and add the ``bin`` directory to your ``PATH``::

  $ export PATH=/path/to/opendylan/bin:$PATH

You may wish to ``sudo apt install lldb-15``, for debugging.


Windows
=======

Open Dylan for Windows is currently a 32-bit application that runs under the WOW64 x86
emulator on Windows 10 64-bit machines.

➟ `Download binaries from GitHub <https://github.com/dylan-lang/opendylan/releases/tag/v2025.1.0>`_

For installation, double-click on the installer, and follow instructions.  You
need to have either the PellesC linker or the linker of VC++ 6.0, 7.0 or the
current .NET platform SDK installed. `PellesC 8.00
<https://web.archive.org/web/20191224014825/https://www.pellesc.de/index.php?page=download&lang=en&version=8.00>`_ is the
best option.  (Note that PellesC 9.00 does not work.)

.. warning:: When running the installer you **must** select a build option (Pelles, VC6,
             etc.) rather than just taking the default, or the build script will be set
             incorrectly in the IDE and you will not be able to open or create a project.
             This can also be fixed after installation by navigating to Tools >
             Environment Options ... > Build > Build Script and selecting the appropriate
             build script for your setup.

Your environment variables must be set such that the external build system
(linker, resource compiler, etc.) can be found.  For example, for Pelles C set
these environment variables in the System control panel (assuming installation
in ``C:\Program Files\PellesC``)::

  INCLUDE=C:\Program Files\PellesC\include;C:\Program Files\PellesC\include\win
  LIB=C:\Program Files\PellesC\lib;C:\Program Files\PellesC\lib\win
  PATH=C:\Program Files\PellesC\bin;...more...

You may instead start a Pelles C interactive shell and run
``C:\Program Files\Open Dylan\bin\win32-environment.exe``, but this
won't help if you want to run Open Dylan via the Start menu.


Installing Older Versions
=========================

Older builds can be found `on GitHub
<https://github.com/dylan-lang/opendylan/releases>`_ or in the `download
directories <https://opendylan.org/downloads/opendylan/>`_.

For the 2020.1 or newer releases just untar the downloaded file and add the
``bin`` directory to your :envvar:`PATH`.  Optionally, ``sudo apt install
lldb`` for debugging.

For 2019.1 and earlier releases:

* All Unix platforms must have the Boehm GC and ``libunwind`` installed.
  For example, ``apt-get install libgc-dev libunwind-dev`` on Ubuntu.

* The README file inside the tarball describes installation and basic
  usage. The easiest way is extracting the tarball in /opt.

* The Linux and FreeBSD platforms should have gcc installed, in order to allow
  linking.

* On Arch Linux you may use the following recipe instead::

    git clone https://aur.archlinux.org/opendylan.git
    cd opendylan
    makepkg -si

* On macOS you may use this recipe instead::

    brew tap dylan-lang/dylan
    brew install opendylan       # or brew upgrade opendylan
