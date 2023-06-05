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
   number changes in the release notes. This command may help::

     git submodule foreach --quiet 'echo -n "$name "; git describe --always --tags $sha1'

   What "stable" means isn't well-defined; use your discretion. The important
   point is that libraries are bundled with the release so people may use them
   without explicitly requesting a specific version. Therefore they shouldn't
   be allowed to get too stale.

#. Test on supported platforms.

   * Do a 3-stage boostrap: make distclean, autogen.sh, configure, make, make
     install, make dist.

   * Run ``make check`` and if anything fails that is not marked ``EXPECTED TO
     FAIL``, fix the problem or discuss with others how to proceed.

   * As a smoke test, verify that the "hello world" instructions in `README.md
     <https://github.com/dylan-lang/opendylan/blob/master/README.md>`_ work on
     each platform.

#. Update the version number in the sources

   * In the release-info library
   * In the configure.ac file
   * Do a ``git grep`` for the previous release number, e.g., "2019.1" and see
     if anything else needs to be updated.

#. Update version numbers in ``build/unix/release-with-batteries.sh``
   to the latest stable versions of the relevant software (Clang+LLVM,
   the Boehm-Demers-Weiser garbage collector, and libunwind).
   Download the release tarballs.

#. Update the release notes. Hopefully these have been maintained as changes
   were made.  GitHub's "Draft a New Release" UI provides a succinct list of
   contributions based on pull requests since the previous release; just click
   on "Generate release notes" button. *But don't actually create the release
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

#. Test the tarballs

   Before uploading to GitHub or opendylan.org, at least install and build
   hello-world to make sure the release isn't obviously broken.

#. Upload the binaries to GitHub

   Edit the release created previously and upload the binaries. Do **not**
   uncheck the "This is a pre-release" checkbox yet.

   **Before finishing the steps below, make sure some core Dylan hackers use
   the new release for a few days or a week.** With a small user base it's not
   hard to miss critical problems.

#. Upload binaries to opendylan.org

   The binaries go in ``/var/www/opendylan.org/downloads/opendylan/YYYY.n/``.
   abeaumont, cgay, and housel have access currently.

#. Update the `Downloads
   <https://github.com/dylan-lang/website/blob/master/source/download/index.rst>`_
   page.

#. Write a news item and publish it on opendylan.org

   Copy from the previous one but don't feel the need to follow the exact same
   template. **Do** include the ``:Date:`` header as this is how articles get
   included in the RSS feed.
   https://github.com/dylan-lang/website/tree/master/source/news

#. Announce the release. Probably a good idea to link to the above news item on
   the website and just say a few brief words about the release in these
   postings....

   * dylan-lang@googlegroups.com
   * Tweet from @DylanLanguage
   * https://www.reddit.com/r/dylanlang/

#. Some post-release tasks that aren't urgent but should be done soon...

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
      release by default. Normally this just involves changed the default
      values for the "version" and "tag" inputs.

      **Setting the new version as the default too quickly may be a bad idea.
      People can explicitly upgrade to it whenever they want by changing their
      CI to explicitly specify the new release.**

   #. Update `the Wikipedia page
      <https://en.wikipedia.org/wiki/Dylan_(programming_language)>`_ with the
      latest release version and date.

