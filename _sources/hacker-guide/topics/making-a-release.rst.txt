******************
Release Check-list
******************

There are a number of manual steps involved in making an Open Dylan release and
it's easy to forget one or two. Hopefully we can automate some of these but for
now here is a manual check-list.

#. Tell `the Matrix channel
   <https://app.element.io/#/room/#dylan-lang_general:gitter.im>`_ that you plan to build
   a release.

#. Update submodules to the latest stable version tags and document the version
   number changes in the release notes. This command may help::

     git submodule foreach --quiet 'echo -n "$name "; git describe --always --tags $sha1'

   What "stable" means isn't well-defined; use your discretion. The important
   point is that libraries are bundled with the release so people may use them
   without explicitly requesting a specific version. Therefore they shouldn't
   be allowed to get too stale.

#. Test on supported platforms.

   * To ensure that you catch any bootstrapping dependency issues, make sure
     your active ``dylan-compiler`` is set to the previous release binary,
     **not** to a more recent local build.

   * Do a 3-stage bootstrap: make distclean, autogen.sh, configure, make, make
     install, make dist.

   * Run ``make check`` and if anything fails that is not marked ``EXPECTED TO
     FAIL``, fix the problem or discuss with others how to proceed.

   * As a smoke test, verify that the :doc:`"Hello World" instructions
     </getting-started-cli/hello-world>` work on each platform.

   **TODO:** This should be done automatically by GitHub CI. See
   https://github.com/dylan-lang/opendylan/blob/master/.github/workflows/bootstrap.yml
   which currently only runs on Ubuntu.

#. Update the version number in the sources

   * In the release-info library
   * In the configure.ac file
   * Do a ``git grep 2019.\\d`` to see if anything else needs to be updated.

#. Update version numbers in ``build/unix/release-with-batteries.sh``
   to the latest stable versions of the relevant software (Clang+LLVM,
   the Boehm-Demers-Weiser garbage collector, and libunwind).

#. Update the release notes if necessary. *Don't actually create the release
   yet, until the release notes have been committed.*

   To determine what to put in the Contributors section of the notes, this
   command is useful (but modify the tag argument)::

     git log --format=short --no-merges v2019.1.0..origin/master | grep '^Author: ' | sort | uniq -c | sort -n

#. Create a draft release on GitHub

   This step is primarily to create the release tag in the git repo.

   On https://github.com/dylan-lang/opendylan/releases click the "Draft a
   release" button and create a release with name similar to "Open Dylan
   2019.1", tag similar to "v2019.1.0", any description you like, and make sure
   the "This is a pre-release" box is checked.

   The major version is always the current year, the minor version is a number
   starting at 1 for the current year, and the patch version starts at 0.

#. Build the binaries for supported platforms

   It's best to build the release from a clean checkout to be sure that no
   uncommitted files become part of the release tarball. In particular, the
   entire "sources" directory is copied into the release, so any uncommitted
   files or a "_build" directory could be copied.

   On unix platforms::

     $ git clone --recursive https://github.com/dylan-lang/opendylan
     $ cd opendylan
     $ git co v2019.1.0      # the tag you created above
     $ ./build/unix/release-with-batteries.sh

   Use the previous release as the bootstrap compiler so that we can be sure
   that works.  If it doesn't work, then opendylan/BUILDING.rst must be updated
   to indicate which version **can** be used to bootstrap the compiler.

   Ask Peter Housel to build the Windows release. :-)

#. Upload the binaries to GitHub

   Edit the release created previously and upload the binaries. Do **not**
   uncheck the "This is a pre-release" checkbox yet.

#. Test the tarballs

   In your own branch, modify the `libraries-test-suite.yml workflow
   <https://github.com/dylan-lang/opendylan/blob/ee21ac1e65f8aa921a2d76e197fb4ba652f3b8a1/.github/workflows/libraries-test-suite.yml#L28>`_
   to explicitly specify the new release version and tag so that it will be
   installed and tested on a clean machine on multiple platforms. You'll have
   to go to your fork in the GitHub UI, click on Actions, and find the workflow
   runs.

#. Upload binaries to opendylan.org

   The binaries go in ``/var/www/opendylan.org/downloads/opendylan/YYYY.n/``.
   abeaumont, cgay, and housel have access currently. (It's nice to have a
   backup on a server that we own.)

#. Update the `Downloads
   <https://github.com/dylan-lang/opendylan/blob/master/documentation/source/download/index.rst>`_
   page.

#. On GitHub, move the release from Draft to Final.

#. Publish the new release in pacman-catalog with `deft publish
   <https://package.opendylan.org/deft/index.html#deft-publish>`_.

#. Announce the release. Check previous announcements for ideas, but no need to
   slavishly copy the format.

   * dylan-lang@googlegroups.com
   * @DylanLanguage on Twitter (housel)
   * @DylanLang on fosstodon.org (cgay)
   * https://www.reddit.com/r/dylanlang/

Post-release Tasks
==================

These items aren't urgent but should be done after each release.

#. Bump the OD version to something plausible, like 2023.1pre. `Example pull
   request <https://github.com/dylan-lang/opendylan/pull/1465>`_.

#. Create a file in which to put the release notes for the next version. See
   the above example pull request.

#. Update other packages

   * archlinux -- https://aur.archlinux.org/packages/opendylan/
   * debian
   * homebrew

   .. TODO: Add detail on how to make each package, either here or in a
      separate document.

#. Update play.opendylan.org to the new version. Requires cgay for now, but
   basically change the opendylan link to point to the new release, restart
   the playground, and compile an example so the next build goes fast.

#. Update the `install-opendylan GitHub Action
   <https://github.com/dylan-lang/install-opendylan/>`_ to use the new
   release by default. Normally this just involves changing the default
   values for the "version" and "tag" inputs.

   **Setting the new version as the default too quickly may be a bad idea.
   People can explicitly upgrade to it whenever they want by changing their
   CI to explicitly specify the new release.**

#. Update `the Wikipedia page
   <https://en.wikipedia.org/wiki/Dylan_(programming_language)>`_ with the
   latest release version and date.
