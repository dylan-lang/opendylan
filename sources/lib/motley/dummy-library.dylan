Module:    dylan-user
Synopsis:  Definition of the library and module
Author:    Seth LaForge
Synopsis:  OLE typeinfo walker and Dylan client stub generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library motley
  use functional-dylan;
  //use tools-interface;
  export motley;
end library motley;

define module motley
  use functional-dylan;
  //use tools-interface;

  //export get-registry-type-libraries, <registry-type-library-info>, 
  //	 registry-type-library-name, registry-type-library-path;
  //export get-type-library-short-name;	// DEPRECATED
  //export get-type-library-information;
end module motley;
