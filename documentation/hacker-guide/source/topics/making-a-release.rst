******************
Release Check-list
******************

There are a number of manual steps involved in making an Open Dylan release and
it's easy to forget one or two. Hopefully we can automate some of these but for
now here is a manual check-list.

#. Let people know you plan to make a release. Use the Gitter channel for this.
   Because the Dylan community is small there's no need to make a release
   branch; people can just be careful what they commit until the release is
   out.

#. Update submodules to the latest stable version tags and document the version
   number changes in the release notes.

   What "stable" means isn't well-defined; use your discretion. The important
   point is that libraries are bundled with the release so people may use them
   without explicitly requesting a specific version. Therefore they shouldn't
   be allowed to get too stale.

#. Test on supported platforms.

   * Do a 3-stage boostrap: make distclean, autogen.sh, configure, make, make
     install.

   * Run ``make check`` and if anything fails that is not marked ``EXPECTED TO
     FAIL``, fix the problem or discuss with others how to proceed.

   * As a smoke test, verify that the "hello world" instructions at the top of
     `BUILDING.rst
     <https://github.com/dylan-lang/opendylan/blob/master/BUILDING.rst>`_ work
     on each platform.

#. Update the version number in the sources

   * In the release-info library
   * In the build/packages/unix/README file
   * In the configure.ac file
   * Do a ``git grep`` for the previous release number, e.g., "2019.1" and see
     if anything else needs to be updated.

#. Update version numbers in ``build/unix/release-with-batteries.sh``
   to the latest stable versions of the relevant software (Clang+LLVM,
   the Boehm-Demers-Weiser garbage collector, and libunwind).
   Download the release tarballs.

#. Update the release notes. Hopefully these have been maintained as changes
   were made.  It may be worth scanning the commit logs or pull requests.

   To determine what to put in the Contributors section of the notes, this
   command is useful (with obvious modifications)::

     git log --format=short --no-merges v2019.1.0..origin/master | grep '^Author: ' | sort | uniq -c | sort -n

#. Create a draft release

   This step is primarily to create the release tag in the git repo.

   On https://github.com/dylan-lang/opendylan/releases click the "Draft a
   release" button and create a release with name similar to "Open Dylan
   2019.1", tag similar to "v2019.1.0", any description you like, and make sure
   the "This is a pre-release" box is checked.

   The major version is always the current year, the minor version is a number
   starting at 1 for the current year, and the patch version starts at 0.

#. Build the binaries for supported platforms

   On un\*x platforms::

     $ cd opendylan
     $ git co v2019.1.0      # the tag you created above
     $ ./build/unix/release-with-batteries.sh

   Use the previous release as the bootstrap compiler so that we can be sure
   that works.  If it doesn't work, then opendylan/BUILDING.rst must be updated
   to indicate which version **can** be used to bootstrap the compiler.

   Ask Peter Housel to build the Windows release. :-)

#. Upload the binaries to GitHub

   Edit the release created previously and upload the binaries.  After the last
   binary has been uploaded, uncheck the "This is a pre-release" checkbox and
   save/publish the release.

#. Upload binaries to opendylan.org

   The binaries go in ``/var/www/opendylan.org/downloads/opendylan/YYYY.n/``.
   abeaumont, cgay, and housel have access currently.

#. Update other packages

   * archlinux -- https://aur.archlinux.org/packages/opendylan/
   * debian
   * homebrew

   .. TODO: Add detail on how to make each package, either here or in a
      separate document.

#. Update the `Downloads
   <https://github.com/dylan-lang/website/blob/master/source/download/index.rst>`_
   page.

#. Write a news item and publish it on opendylan.org

   Copy from the previous one but don't feel the need to follow the exact same
   template. **Do** include the ``:Date:`` header as this is how articles get
   included in the RSS feed.
   https://github.com/dylan-lang/website/tree/master/source/news

#. Announce the release

   * dylan-lang@googlegroups.com
   * Tweet from @DylanLanguage (cgay)

#. Update `the Wikipedia page <https://en.wikipedia.org/wiki/Dylan_(programming_language)>`_
   with the latest release version and date.

