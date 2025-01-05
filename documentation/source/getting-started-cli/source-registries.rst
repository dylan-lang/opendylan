Using Source Registries
=======================

.. note:: When using :program:`deft`, source registries are created for you via
          the `deft update
          <https://package.opendylan.org/deft/index.html#deft-update>`_ command
          and you should not have to create them explicitly. You may want to
          skip this section for now.

Passing the name of a ".lid" file to :program:`dylan-compiler` works great when
you have a single library that only uses other libraries that are part of Open
Dylan, but what if you want to use a second library that you wrote yourself or
that you installed from GitHub?  How will :program:`dylan-compiler` find the
sources for that library?  The answer is registries.

For each Dylan library that isn't part of Open Dylan itself, you create a file
in the registry which is named the same as the library and contains a pointer
to the ".lid" file for the library.  For example, here's the registry file for
hello-world, created in the previous section. Note that this assumes you are
still in the directory created by `deft new application
<https://package.opendylan.org/deft/index.html#deft-new-application>`_.

::

  $ ls registry/*/hello-world
  registry/x86_64-darwin/hello-world

What's going on here?  First of all, the registry mechanism makes it possible
to have platform-specific libraries.  `deft
<https://package.opendylan.org/deft/index.html>`_ currently always writes
registry entries to a platform-specific directory, in this case
``x86_64-darwin``, but anything platform-independent can actually go in the
:file:`registry/generic` subdirectory.

Platform-specific registry directories are searched before
:file:`registry/generic`, so if you have a library that includes an extra file
for Windows-only definitions, you can use two registry entries: one in
:file:`registry/x86-win32/hello-world` pointing to the Windows-specific LID
file and one in :file:`registry/generic/hello-world` pointing to the LID file
that works on other platforms.

Now let's look at the actual content of our hello-world registry file::

  $ cat registry/x86_64-darwin/hello-world
  abstract://dylan/hello-world.lid

What this is doing is locating a file *relative to the directory containing the
registry directory itself*.  If the :file:`registry` directory is
``/tmp/hello-world/registry`` then ``abstract://dylan/hello-world.lid`` says
the hello-world LID file is in ``/tmp/hello-world/hello-world.lid``.
"abstract://dylan/" is just boilerplate.

When you invoke :program:`dylan-compiler` in the directory containing a
"registry" directory it automatically uses that registry, in addition to (and
taking precedence over) the registry in the Open Dylan installation directory.

.. _open-dylan-user-registries:

OPEN_DYLAN_USER_REGISTRIES
--------------------------

If you prefer to invoke the compiler from outside your workspace or if you want
to include multiple workspace registries when searching for libraries, you can
set the :envvar:`OPEN_DYLAN_USER_REGISTRIES` environment variable. For
example::

  $ export OPEN_DYLAN_USER_REGISTRIES=/tmp/workspace1/registry

Once you've set :envvar:`OPEN_DYLAN_USER_REGISTRIES`, :program:`dylan-compiler`
can find library source code no matter what directory you're currently working
in. You only need to specify the library name::

  $ cd /tmp
  $ dylan-compiler -build hello-world

You can add more than one registry to :envvar:`OPEN_DYLAN_USER_REGISTRIES` by
separating them with colons (Unix) or semicolons (Windows)::

  $ export OPEN_DYLAN_USER_REGISTRIES=/tmp/workspace1/registry:/tmp/workspace2/registry


Registry Search Order
---------------------

For each step in the following list, the directory named for the current
platform is searched **before** the "generic" directory.

1. If :envvar:`OPEN_DYLAN_USER_REGISTRIES` is set then its list of registries
   is searched in order.

2. If a directory named :file:`registry` exists in the current working
   directory where the Dylan compiler is started, it is searched.

3. The internal Open Dylan registry in the :file:`sources/registry` directory
   in your Open Dylan installation is searched last.

To see the list of registries in the order they will be searched, use the
``show registries`` command in the interactive compiler.  Example::

  $ echo $OPEN_DYLAN_USER_REGISTRIES
  OPEN_DYLAN_USER_REGISTRIES=/tmp/a:/tmp/b
  $ cd /tmp/x
  $ mkdir registry
  $ dylan-compiler
  > show registries
    {personal registry in /tmp/a/x86_64-linux/}
    {personal registry in /tmp/b/x86_64-linux/}
    {personal registry in /tmp/x/registry/x86_64-linux/}
    {personal registry in /opt/opendylan-2025.1/sources/registry/x86_64-linux/}
    {personal registry in /tmp/a/generic/}
    {personal registry in /tmp/b/generic/}
    {personal registry in /tmp/x/registry/generic/}
    {personal registry in /opt/opendylan-2025.1/sources/registry/generic/}
