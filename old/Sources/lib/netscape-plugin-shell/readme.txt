Building A Netscape Plugin In Vmtether

contents of the directories

netscape-plugin-shell/
  ;; This has the netscape-plugin library and a bunch of setup and
  ;; templates used to make a dll.
  netscape-plugin.mak
    ;; the makefile that gets copied to the build directory for the dll
  netscape-plugin.lid
  *.dylan
    ;; dylan code.  If we actually use this as an example for users
    ;; then we'll have to explain more here
  npwin.cpp
    ;; A hacked version of the C++ code provided by netscape that
    ;; every plugin uses.  I fixed a couple of bugs in it. I have
    ;; added a couple of hacks to work around the ffi losing high bits
    ;; in pointers.
    ;; I added a call to hacks__main and ConnectionProtocol in
    ;; NP_Initialize init the dylan world, and the tether connection,
    ;; respectively.
    ;; This gets copied to the directory where the dll is to be built.
  wsock.c
    ;; Special version of the tether setup code that doesn't have a
    ;; winmain.
    ;; This gets copied to the directory where the dll is to be built.
  fix-libs.sh
    ;; script that makes the libs.lnk file more digestible for a
    ;; makefile
    ;; This gets copied to the directory where the dll is to be built.
  dll-setup.sh
    ;; This is the script that copies everything interesting to the
    ;; dll build directory.  do:
    ;; dll-setup.sh dll-build-directory library-target-name
    ;; so for the othello plugin you would do:
    ;; dll-setup.sh plugin-othello/application oww-othello-application
    ;; 
  plugin.rc
    ;; the resource file for the plugin.
    ;; You will need to edit this to indicate the file extention(s)
    ;; and mimetype(s) your
    ;; plugin accepts.  This was part of the example that netscape provided
    ;; if it's not already in the dll build directory this file gets
    ;; copied there.
    ;; I have provided one for othello in plugin-othello/application
    ;; which uses the .oth extension
  plugin.rc2 and resource.h
    ;; These get copied to the build directory.  They are referred to
    ;; by plugin.rc and I didn't want to perturb that at all.
  include/
    ;; include directory from netscape
    ;; This directory gets copied into the build directory by dll-setup.sh
  plugin-othello/
    ;; This is the directory with the code to actually use othello with
    ;; the plugin
    plugin-othello.dylan
    plugin-othello.lid
    plugin-othello-library.dylan
    plugin-othello-old-ffi.dylan
      ;; interesting bit is in NPP-SetWindow where we do attach-othello
    names/
      ;; directory with names for idvm
      ;; All the foo-names files are in hope, but they really must
      ;; be generated from the corresponding libraries.
      ;; the name of the library is oww-othello-names
    application/
      ;; toplevel directory where the dll gets built
      ;; the name of the library is oww-othello-application
      ;; run the dll-setup.sh script to put all the stuff to make it work
      ;; do:
      ;; nmake -f oww-othello-application.mak
      ;; to build the dll.


To build the othello dll

On unix:

  Compile all the libraries natively in a vmtether image.
  The libraries here are netscape-plugin, plugin-othello,
  oww-othello-names, and oww-othello-application.  

After oww-othello-application has been compiled
in the directory netscape-plugin-shell:
  
dll-setup.sh plugin-othello/application oww-othello-application

On a pc:

In each of the library directories (netscape-plugin-shell,
netscape-plugin-shell/plugin-othello, and
netscape-plugin-shell/plugin-othello/names) do: 

  nmake install-library -f makefile 

to install the libraries.

In the netscape-plugin-shell/plugin-othello/application directory

  nmake -f oww-othello-application.mak 

This will create the file oww-othello-application.dll.  There will be
zillions of warnings from the linker about symbols defined more than
once.  That's ok. This is the dll with all of dylan built into it.  It
will actually go into the netscape program directory (the same
directory as the netscape exe file).  It must be called dylplugn.dll
there.  We use 2 dll's to prevent netscape from unloading dylan when
you move to a different page.

  copy oww-othello-application.dll <netscape-dir>/program/dylplugn.dll

Then in the netscape-plugin-shell directory you will need to build the
little dll that netscape loads and unloads as you move to and from a
page with othello on it.

  nmake -f mini-plugin.mak

This will create mini-plugin.dll.  It must be copied to netscape's
plugins directory.  It must have an 8dot3 filename there.

  copy mini-plugin.dll <netscape-dir>/program/plugins/npoth.dll

Start up netscape.

open a url with a .oth extension, or open a url that has a .oth file
embedded in it.  There are examples in hope in D-lib-netscape-plugin-shell.
See oth.html, and junk.oth.

