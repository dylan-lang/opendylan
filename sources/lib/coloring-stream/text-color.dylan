Module:    coloring-stream-internals
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <text-color> (<object>)
  constant slot color-name :: <string>,
    required-init-keyword: name:;
  constant slot color-ansi-code :: <integer>,
    required-init-keyword: ansi:;
end class;

ignore(color-name);

define macro color-definer
  { define color ?name:name = ansi ?ansi-code:expression }
    => { define constant "$color-" ## ?name :: <text-color>
           = make(<text-color>,
                  name: ?"name",
                  ansi: ?ansi-code); }
end macro color-definer;

define color black = ansi 0;
define color red = ansi 1;
define color green = ansi 2;
define color yellow = ansi 3;
define color blue = ansi 4;
define color magenta = ansi 5;
define color cyan = ansi 6;
define color white = ansi 7;
define color default = ansi 9;
