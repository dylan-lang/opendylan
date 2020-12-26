Module:       table-extensions
Synopsis:     The table constructor macro and <case-insensitive-string-table>
Author:       Dustin Voss
Copyright:    Copyright (c) 2006 Dustin Voss.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Table constructor. Syntax:
//   tabling(#"red"=>"stop", #"green"=>"go");
//   tabling(<string-table>, "red"=>"stop", "green"=>"go");
//
define macro tabling

  // Matches when optional class included.
  { tabling(?class:expression, ?contents) }
    => { let t :: ?class = make(?class); ?contents; t; }

  // Matches without optional class.
  { tabling(?more:*) } => { tabling(<table>, ?more); }

  contents:
  { } => { }
  { ?key:expression => ?value:expression, ... }
    => { t[?key] := ?value; ... }
end macro;


// Code is taken from GD, using a superclass of <table> instead of <value-table>.
define sealed class <case-insensitive-string-table> (<table>)
end class;

define sealed domain make(singleton(<case-insensitive-string-table>));
define sealed domain initialize(<case-insensitive-string-table>);

define sealed inline method table-protocol
    (table :: <case-insensitive-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(case-insensitive-equal, case-insensitive-string-hash)
end method;
