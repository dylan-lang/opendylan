Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <unicode-integer> = <double-byte>;
define character <unicode-character>;

//// CONVERSIONS BETWEEN

define sealed inline method as 
    (class == <unicode-character>, character :: <byte-character>)
 => (code :: <unicode-character>)
  as(<unicode-character>, as(<integer>, character))
end method as;

define sealed inline method as 
    (class == <byte-character>, character :: <unicode-character>)
 => (code :: <byte-character>)
  as(<byte-character>, as(<integer>, character))
end method as;

