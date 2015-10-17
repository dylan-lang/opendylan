Module:    dylan-user
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library coloring-stream
  use common-dylan;
  use io;
  use system;

  export coloring-stream,
         coloring-stream-internals;
end library;

define module coloring-stream
  create <coloring-stream>,
         colorize-stream;

  create stream-supports-color?;

  create $color-black,
         $color-blue,
         $color-cyan,
         $color-default,
         $color-green,
         $color-magenta,
         $color-red,
         $color-white,
         $color-yellow;

  create $normal-intensity,
         $bright-intensity,
         $dim-intensity;

  create $reset-attributes;

  create <text-attributes>, text-attributes;
end module;

define module coloring-stream-internals
  use common-dylan;
  use streams;
  use streams-internals, import: { <file-stream>, stream-console? };
  use operating-system, import: { environment-variable, $os-name };
  use print, import: { print-object };
  use coloring-stream, export: all;

  export <ansi-coloring-stream>,
         <null-coloring-stream>;

  export attributes-to-ansi;

  export <text-color>,
         <text-intensity>;
end module;
