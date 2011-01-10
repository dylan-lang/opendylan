Module: common-dylan-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2003-2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Integers

define generic integer-length
    (integer :: <abstract-integer>) => (length :: <integer>);

define method integer-length
    (integer :: <integer>) => (length :: <integer>);
  let word :: <machine-word>
    = if (negative?(integer))
        %lognot(as(<machine-word>, integer))
      else
        as(<machine-word>, integer)
      end;
  $machine-word-size - %count-high-zeros(word)
end method;
