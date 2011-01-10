Module:       table-extensions
Synopsis:     The table constructor macro, et al.
Author:       Dustin Voss
Copyright:    Copyright (c) 2006 Dustin Voss.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Table constructor. Syntax:
// let my-table = table("red"=>"stop", "green"=>"go");
// let my-table = table(<string-table>, "red"=>"stop", "green"=>"go");
//
define macro table 

  // Matches when optional class included.
  { table(?table-class:expression, ?table-contents) }
    => { let ht = make(?table-class); ?table-contents; ht; }

  // Matches without optional class.
  { table(?rest:*) } => { table(<table>, ?rest); }

  table-contents:
  { } => { }
  { ?key:expression => ?value:expression, ... }
    => { ht[?key] := ?value; ... }
end macro table;
