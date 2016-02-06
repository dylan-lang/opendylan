Module:       common-dylan-internals
Author:       Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Table definition macro

define macro table-definer
  { define table ?table-name:name = { ?entries } }
    => { define constant ?table-name :: <table> = make(<table>);
         begin let the-table = ?table-name; ?entries end; }
  { define table ?table-name:name :: ?table-type:name = { ?entries } }
    => { define constant ?table-name :: ?table-type = make(?table-type);
         begin let the-table = ?table-name; ?entries end; }
 entries:
  { } => { }
  { ?key:expression => ?value:expression, ... }
    => { the-table[ ?key ] := ?value; ... }
end macro table-definer;
