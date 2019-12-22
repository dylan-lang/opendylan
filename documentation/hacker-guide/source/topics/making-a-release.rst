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

#. Test on supported platforms.

   * Do a 3-stage boostrap.
   * Run test suites and try to figure out if anything serious is broken.
     (This is an art, currently, and we should make all our tests pass.)
   * Do some ad-hoc testing.  (Anything specific to mention here?)


#. Update the version number in the sources

   * In the release-info library
   * In the build/packages/unix/README file
   * In the configure.ac file
   * Do a ``git grep`` for the previous release number, e.g., "2019.1" and see
     if anything else needs to be updated.


#. Update the release notes. Hopefully these have been maintained as changes
   were made.  It may be worth scanning the commit logs.

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
     $ make release

   Use the previous release as the bootstrap compiler so that we can be sure
   that works.  If it doesn't work, then opendylan/README.rst must be updated
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

   * archlinux -- https://aur.archlinux.org/opendylan.git
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
