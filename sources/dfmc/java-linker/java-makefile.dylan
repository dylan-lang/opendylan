Module: dfmc-java-linker
Author: keith and jonathan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Platform-specific makefile configuration.

// We need to generate different makefiles for Unix and Windows since
// they're incompatible for various reasons. In particular, we want
// to use !include with nmake, and there's not always a way to get
// that ignored in Unix makes.

//define abstract class <makefile-target> (<object>) end;

define class <java-makefile-target> (<makefile-target>) end;


//define constant *java-makefile-target* = make(<java-makefile-target>);

// this should be specialized on backend.
//define method makefile-target-using-os-name (name)
//  *java-makefile-target*
//end method;

define method emit-target-makefile (back-end :: <java-back-end>,
				    t :: <java-makefile-target>,
				    makefile-name,
				    library-description, 
				    units,
				    #key executable, base-address, 
				    linker-options, c-source-files,
				    c-header-files, c-object-files,
				    rc-files, c-libraries)
//  format-out ("-- No makefile needed for Java!\n") ;
  #f
end method;

// eof

