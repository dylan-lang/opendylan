Environment Variables
=====================

There are a number of environment variables which control
where the Open Dylan compiler looks for or places files.

``OPEN_DYLAN_RELEASE_INSTALL``:
    Unknown purpose.

``OPEN_DYLAN_RELEASE_REGISTRIES``:
    Unknown purpose.

``OPEN_DYLAN_RELEASE``:
    Unknown purpose.

    Defaults to the path to the compiler.

``OPEN_DYLAN_RELEASE_BUILD``:
    Unknown purpose.

``OPEN_DYLAN_USER_REGISTRIES``:
    Controls where to look for registry entries to
    find libraries. See :doc:`source-registries`.

    Defaults to looking for a directory named
    ``registry`` in the current directory.

``OPEN_DYLAN_USER_INSTALL``:
    Unknown purpose.

``OPEN_DYLAN_USER_PROJECTS``:
    Unknown purpose. Appears to be largely unused.

``OPEN_DYLAN_USER_ROOT``:
    The directory where build output is placed.
    This is the 'user root'.

    On Windows, see :doc:`windows`. On all other
    platforms, this defaults to the directory ``_build``
    in the current directory.

``OPEN_DYLAN_USER_BUILD``:
    The directory within the 'user root' where
    things are built.

    Defaults to ``build``.
