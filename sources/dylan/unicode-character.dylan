Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// XXX this should become something like:
//       limited(<integer>, min: 0, max: 17 * 2 ^ 16)
define constant <unicode-integer> = <integer>;
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

