Rethinking the Project Manager
******************************

Problem
=======

The current project manager has a variety of problems that we would
like to solve.  These include:

* It combines install with build.
* Similarly, it doesn't distinguish between a library that I'm using
  and one that I'm developing. Not all dependencies should be treated
  in the same manner.  While this is seen by some to be an advantage,
  I don't believe that it is in any larger scale setting with multiple
  endeavors happening at once.
* It provides no versioning of libraries.
* Everything is always built into a shared location. This is pretty
  confusing for new users.
* It provides no way of locating, downloading or otherwise managing
  dependencies. You still have to download everything yourself, set
  up registries, and get things to work.

Example Application
===================

Given a software application like Network Night Vision which consists of
an executable and multiple libraries, this is an ideal demonstration of
how the new system could work.

Network Night Vision depends on externally provided (and maintained)
libraries like Monday and the Dylan runtime.

It is also consists of a collection of libraries which are part of the
application and linked into the final executable.

Current Situation
-----------------

Currently, there is a registry for Network Night Vision which lists
all of these libraries, both internal and external, and requires that
they be on disk in a particular location (relatively).  When building
NNV, all of these libraries are built and installed into a common build
directory (``~/Open-Dylan``).  Any changes to any of the sources for
these libraries results in everything getting appropriately rebuilt.
This rebuild happens even for a change in the Dylan runtime or the
Monday libraries even though they are external dependencies and aren't
being developed as part of NNV.

When talking to the compiler, you give the compiler a single project
or LID file which is used to automatically get all dependencies, look
them up in the registry, and build them as needed.

Better World?
-------------

In a better world, external dependencies would be available from the
system as a whole.  These external dependencies would have a version
number or identifier associated with them so that changes in the
dependency wouldn't require that everything using it be rebuilt or
updated. The internal (component) libraries would be handled roughly
as they are now: changes in them would cause rebuilds.

It would be great if one didn't have to manually go and download,
build, and install external dependencies that didn't exist or if the
process were at least easier than it is now.

Proposal
========

Workspaces
----------

Workspaces are a new concept to Open Dylan and are a layer above the
project.  (A project corresponds to a single library or executable.)

Taking Network Night Vision as an example, you would have a top level
``nnv.project`` file which contained (among other things) a list of the
projects that were part of the Network Night Vision application, using
relative paths from the workspace file:

* ``flow-printer/flow-printer.lid``
* ...
* ``gui-sniffer/gui-sniffer.lid``
* ...
* ``vector-table/vector-table.lid``

Upon opening this workspace, the projects would also be loaded.
Libraries referenced from these projects would be resolved by:

#. First, look for a project in the workspace that provides the
   specified library.
#. If not found, look to the system-installed libraries.
#. See `Package Manager`_.

Libraries that are provided by the workspace are treated as they are
now and changes to them trigger rebuilds.  However, libraries that
are provided by the system are entirely an external dependency, just
like system-provided libraries in any other language or environment.

Package Manager
---------------

The second big piece of this proposal is the creation of a package
management system for Open Dylan. This is something along the lines of
what many other programming language communities provide:

* Clojure: `Leiningen <https://github.com/technomancy/leiningen>`_
* Common Lisp: `QuickLisp <http://www.quicklisp.org/beta/>`_
* Emacs Lisp: `Marmalade <http://marmalade-repo.org/>`_ / `ELPA  <http://tromey.com/elpa/>`_
* Erlang: `EPM <http://www.jkvor.com/erlang-package-manager>`_
* haXe: `haxelib <http://haxe.org/haxelib>`_
* Lua: `LuaRocks <http://luarocks.org/>`_
* NodeJS: `NPM <http://npmjs.org/>`_
* Haskell: `Hackage <http://hackage.haskell.org/packages/hackage.html>`_
* Ocaml: `GODI <http://godi.camlcity.org/>`_
* PHP: `PEAR <http://pear.php.net/>`_
* Perl: CPAN
* Python: PIP / CheeseShop / easy_install
* Racket: `Raco <http://docs.racket-lang.org/raco/index.html>`_
* Ruby: `RubyGems <http://docs.rubygems.org/>`_

Our package manager would feature a global catalog of packages, the
ability to download, build and install those packages, and integration
of these features in the build system and IDE.

Catalog
~~~~~~~

The catalog is a mapping of library names to metadata about those
libraries.

This metadata should be able to include:

* Author
* Version
* Synopsis
* Description
* Keywords / Tags
* Source repository location
* Software license

The catalog may be maintained independently of the various libraries
or it might be the result of a mechanical process which extracts the
metadata from the workspace and project files of the libraries. There
are advantages and disadvantages to both, so this should be discussed.

Software Distribution
~~~~~~~~~~~~~~~~~~~~~

I propose that we initially rely upon Git for retrieving the source code
for a library.  We can always add additional methods later (HTTP, Mercurial,
Subversion, etc), but for now, it would be easiest if we could avoid having
to write our own HTTP interfaces, deal with unzipping and unpacking tarballs
and all of the other associated details.

Upon locating a package that we wish to install, the package manager
would look at the metadata that it received from the catalog for the
Git repository and run a git client to retrieve the source code prior
to building and installing the package.

User Interface
~~~~~~~~~~~~~~

The package manager should support at least these operations:

* Search / Apropos. Given some text, find all packages that
  reference that text. This can have features added over time
  to search by author, version, etc.
* Install. Retrieve the
* Uninstall. This would be nice to have, but is probably going
  to come later. When installing a package, a manifest of all
  installed files should be generated. This can then be used
  to uninstall the package.
* Bundle. Take an executable with the libraries that it depends
  upon and put them in a directory structure independent of
  the typical install location. This is probably mainly required
  on Mac OS X where we have to deal with ``install_name_tool``
  and other OS X specific concerns.

Over time, the package metadata should be accessible by a variety
of means:

* Packages website which allows browsing all metadata as well
  as perhaps hosting the generated documentation for the package
  if it is using the standard Sphinx-based structure.
* Command line tools, including within ``dylan-compiler``.
* A GUI browser tool within the IDE.

Versioning
----------

I haven't thought much yet about what will be involved here.

Development Process
===================

A big (and reasonable) fear is that some of this work is fairly large
and extensive and that may make it more difficult.  To help mitigate
this, I propose that we:

* Work iteratively. Small steps that eventually get us somewhere
  awesome are better than getting nowhere at all.
* Accept that this isn't ever going to be perfect. But it will surely
  be better than what we have now.
* Make technology decisions to simplify things and add more features
  later.  An example of this is the proposed reliance upon Git via
  ``run-application()``.

Glossary
========

Project:

    Each project represents a single target. It contains:

    * A collection of Dylan source files.
    * A set of dependencies which are expected to be satisfied by the
      system.
    * A set of dependencies which are expected to be resolved within
      the current workspace.

Registry:

    In Open Dylan, the registry provides a way to map from a library
    name to the associated LID file (with sources) for that library.

Target:

    A target is the final build product that results from a project, be
    that a library or an executable.

Workspace:

    A workspace is a new concept to Open Dylan.  It is a collection of
    projects which, together, represent a software project.

    A given project within the workspace can be specified as the default
    project which is the one which is built first, is the target of a
    'Run' or 'Debug' command and so on. The user can change the current
    active project within the workspace so that the subsequent 'Run' or
    'Debug' commands are directed at the new active project.
