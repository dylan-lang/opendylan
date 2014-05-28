:Author: Chris Double
:Date: 2014-05-28 00:00:00

Nix Packages Available
======================

Open Dylan builds have been added to the `Nix package manager`_
This makes Open Dylan installable from systems using Nix, or the
`NixOS Linux distribution`_, using a command like::

    nix -i opendylan

This will download the Open Dylan binaries and use that to bootstrap a
build from source.

Nix is a purely functional package manager. Different versions of
packages can be installed at the same time and packages can depend on
different versions of other packages. The management of these versions
is handled behind the scenes by the package manager using symbolic
links. This enables doing things like installing multiple Open Dylan
versions at the same time for testing. It's possible to set up custom
Nix definitions for different Dylan versions and test bootstrapping
with each version with some changes to the user Nix configuration
files.

Another nice feature of Nix is that it avoids polluting the user
environment with packages that are used by Open Dylan but not required
for the end user. For example, ``gcc``, ``mps`` and ``boehmgc`` are used by
Open Dylan but they are not visible to the user after installing the
Open Dylan package via Nix. The ``PATH`` for ``dylan-compiler`` has these
available for internal usage but the ``PATH`` for the users profile does
not.

Chris Double's `pull request to add Open Dylan`_ was merged and
should be available now in the ``unstable`` Nix channel.

Many thanks to Chris Double for his work on this!


.. _Nix package manager: http://nixos.org/nix/
.. _NixOS Linux distribution: http://nixos.org/nixos/
.. _pull request to add Open Dylan: https://github.com/NixOS/nixpkgs/pull/2770
