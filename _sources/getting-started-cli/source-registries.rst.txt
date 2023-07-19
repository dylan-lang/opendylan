Using Source Registries
=======================

.. note:: When using the :program:`dylan` tool source registries are created
          for you via the ``dylan update`` command and you should not have to
          create them explicitly. You may want to skip this section for now.

Passing the name of a ".lid" file to :program:`dylan-compiler` works great when
you have a single library that only uses other libraries that are part of Open
Dylan, but what if you want to use a second library that you wrote yourself or
that you installed from GitHub?  How will :program:`dylan-compiler` find the
sources for that library?  The answer is registries.  For each Dylan library
that isn't part of Open Dylan itself, you create a file in the registry that
points to the ".lid" file for the library.  For example, here's the registry
file for hello-world, created in the previous section. Note that this assumes
you are still in the directory created by :command:`dylan new application`::

  $ cat ./registry/{platform}/hello-world   # substitute your platform
  abstract://dylan/hello-world.lid

What's going on here?  First of all, the registry mechanism makes it possible
to have platform-specific libraries.  Anything platform-independent goes in the
:file:`registry/generic` directory. Other supported platform names are
``x86_64-freebsd``, ``x86_64-linux``, ``x86-win32``, etc. For a full list see
`the Open Dylan registry
<https://github.com/dylan-lang/opendylan/tree/master/sources/registry>`_.

Platform-specific registry directories are searched before
:file:`registry/generic`, so if you have a library that includes an extra file
for Windows-only definitions, you can use two registry entries: one in the
"x86-win32" directory and one in the "generic" directory.

Now let's look at the actual content of our hello-world registry file::

  abstract://dylan/hello-world.lid

What this is doing is locating a file *relative to the directory containing the
registry directory itself*.  If the :file:`registry` directory is
``/home/you/xyz/registry`` then ``abstract://dylan/hello-world.lid`` says the
hello-world ".lid" file is in ``/home/you/xyz/hello-world.lid``.
"abstract://dylan/" is just boilerplate.

When you invoke :program:`dylan-compiler` in the directory containing the
"registry" directory it automatically uses that registry, in addition to (and
taking precedence over) the registry in the Open Dylan installation directory.

If you prefer to invoke the compiler from elsewhere, you can set the
:envvar:`OPEN_DYLAN_USER_REGISTRIES` environment variable to point your
registry directory. For example::

  $ export OPEN_DYLAN_USER_REGISTRIES=/home/you/xyz/registry

Once you've set :envvar:`OPEN_DYLAN_USER_REGISTRIES` to your new registry,
:program:`dylan-compiler` can find the hello-world library source no matter
what directory you're currently working in. You only need to specify the
library name::

  $ cd /tmp
  $ dylan-compiler -build hello-world

You can add more than one registry to :envvar:`OPEN_DYLAN_USER_REGISTRIES` by
separating them with colons (Unix) or semicolons (Windows)::

  $ export OPEN_DYLAN_USER_REGISTRIES=/my/registry:/their/registry

.. note:: If :envvar:`OPEN_DYLAN_USER_REGISTRIES` is set, the "registry"
          directory in the current working directory is ignored.
