Module:       Dylan-User
Synopsis:     XML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library xml
  use functional-dylan;
  use io;
  use expat;
  use dom;

  export xml,
	 xml-internals;
end library xml;
