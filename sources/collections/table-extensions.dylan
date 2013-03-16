Module:       table-extensions
Synopsis:     The table constructor macro and <case-insensitive-string-table>
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


// Code is taken from GD, using a superclass of <table> instead of <value-table>.
define sealed class <case-insensitive-string-table> (<table>)
end class <case-insensitive-string-table>;

define sealed domain make(singleton(<case-insensitive-string-table>));
define sealed domain initialize(<case-insensitive-string-table>);

define sealed inline method table-protocol (ht :: <case-insensitive-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(case-insensitive-equal, case-insensitive-string-hash);
end method table-protocol;
