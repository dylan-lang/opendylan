Module:    coloring-stream-internals
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define constant $normal-intensity = #"normal";
define constant $bright-intensity = #"bright";
define constant $dim-intensity = #"dim";
define constant <text-intensity>
  = one-of($normal-intensity,
           $bright-intensity,
           $dim-intensity);

define sealed class <text-attributes> (<object>)
  constant slot text-foreground-color :: false-or(<text-color>) = #f,
    init-keyword: foreground:;
  constant slot text-background-color :: false-or(<text-color>) = #f,
    init-keyword: background:;
  constant slot text-intensity :: false-or(<text-intensity>) = #f,
    init-keyword: intensity:;
end class <text-attributes>;

define function text-attributes
    (#key foreground :: false-or(<text-color>) = #f,
          background :: false-or(<text-color>) = #f,
          intensity :: false-or(<text-intensity>) = #f)
 => (attributes :: <text-attributes>)
  make(<text-attributes>,
       foreground: foreground,
       background: background,
       intensity: intensity)
end function text-attributes;

define constant $reset-attributes
  = text-attributes(intensity: $normal-intensity);
