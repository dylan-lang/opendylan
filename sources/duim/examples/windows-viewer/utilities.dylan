Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Number printing

define constant $hex-prefix = "0x";

define method number-to-hex-string
    (integer :: <integer>) => (string :: <string>)
  concatenate($hex-prefix, integer-to-string(integer, base: 16, size: 8))
end method number-to-hex-string;

define method number-to-hex-string
    (machine-word :: <machine-word>) => (string :: <string>)
  machine-word-to-string(machine-word, prefix: $hex-prefix)
end method number-to-hex-string;


/// Flag handling

define inline method flag-set?
    (mask, value) => (set? :: <boolean>)
  %logand(mask, value) = mask
end method flag-set?;
