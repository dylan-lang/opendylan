:orphan:
:Author: Carl Gay
:Date: 2022-11-28 00:00:00

Open Dylan 2022.1 Released
==========================

Posted on 28 November 2022

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

Some highlights in this release:

* This release comes with "batteries included". The binary distributions
  contain all necessary dependent libraries, including clang, libunwind, and
  libgc, making installation much easier.

* Support has been added for linking executable Dylan libraries and all of
  their dependencies into a single unified executable using the ``-unify``
  build option.

* A new `dylan <https://github.com/dylan-lang/dylan-tool>`_ tool manages Dylan
  workspaces. It provides a centralized catalog of Dylan packages and a way to
  manage dependencies without resorting to Git submodules or needing to
  manually create registry files.

* The 64-bit RISC-V architecture (rv64gc ISA) is now supported on Linux.

* Updates to various libraries included with Open Dylan: collections, io,
  system, ssl, testworks.

For full details on changes in this release see the :doc:`release notes
</release-notes/2022.1>`, or go
directly to the :doc:`download page </download/index>`.

For more information on Open Dylan, see https://opendylan.org/.

Good luck and happy Dylan hacking!
