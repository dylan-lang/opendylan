Module:       Dylan-User
Synopsis:     HTML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library html
  use functional-dylan;
  use streams;
  use dom;

  export html,
	 html-internals;
end library html;
