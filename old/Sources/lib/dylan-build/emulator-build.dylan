Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *build* = #f;

define function indentation(o) #f end;

define class <build-error>(<condition>)
end class;

define generic build() => ();

define generic build-system(build-targets :: <sequence>,
			   #key toplevel?, directory) => ();
