Module:       Dylan-User
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dom
  use functional-dylan;
  use collections;

  export xml-dom,
	 html-dom,
	 dom,
	 dom-internals;
end library dom;
