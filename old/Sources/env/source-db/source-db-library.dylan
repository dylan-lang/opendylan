Module: dylan-user
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-db
  use dylan;
  use functional-extensions;
  use infix-reader;
  use streams;
  use streams-format;
  use doss;
  use locators;
//  use front-end;
  export source-db
//    ,dylan-source
end;
