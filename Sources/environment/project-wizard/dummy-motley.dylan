Module:    environment-project-wizard
Author:    Hugh Greene, Seth LaForge
Synopsis:  Dummy stubs for motley, so basic-wizard can be built!
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define constant <registry-type-library-info> = <pair>;
define constant registry-type-library-name = head;
define constant registry-type-library-path = tail;

// Reads the list of type libraries from the registry and returns information
// about type library names and locations.
//	r: sequence of type library descriptions.
//	r[*].registry-type-library-name: a <string> describing a type library.
//	r[*].registry-type-library-path: a <string> with the path of a type
//					 library.
//
// DUMMY version returns an empty <sequence>.

define function get-registry-type-libraries () => (type-libraries :: <vector>)
  #[]
end function get-registry-type-libraries;


// Extract the short name of a type library.  If there are any problems 
// opening the type library, an <error> will be signalled.
//
// DUMMY version always returns a dummy-name

define function get-type-library-information
    (path :: type-union(<string>, <file-locator>)) 
 => (short-name :: <string>, interfaces :: <sequence>)
  values("no-name", #())
end function get-type-library-information;
