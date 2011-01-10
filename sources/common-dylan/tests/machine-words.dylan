Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <machine-word>) => (instance :: <machine-word>);
  make(<machine-word>, value: 1729)
end method make-test-instance;

define machine-words class-test <machine-word> ()
  //---*** Fill this in...
end class-test <machine-word>;


/// Variables

define machine-words constant-test $machine-word-size ()
  //---** What can we do here?
end constant-test $machine-word-size;

define machine-words constant-test $maximum-signed-machine-word ()
  //---** What can we do here?
end constant-test $maximum-signed-machine-word;

define machine-words constant-test $minimum-signed-machine-word ()
  //---** What can we do here?
end constant-test $minimum-signed-machine-word;

define machine-words constant-test $maximum-unsigned-machine-word ()
  //---** What can we do here?
end constant-test $maximum-unsigned-machine-word;

define machine-words constant-test $minimum-unsigned-machine-word ()
  //---** What can we do here?
end constant-test $minimum-unsigned-machine-word;

define machine-words constant-test as-unsigned ()
  //---** What can we do here?
end constant-test as-unsigned;


/// Basic and signed single word operations

define machine-words function-test %logior ()
  //---*** Fill this in...
end function-test %logior;

define machine-words function-test %logxor ()
  //---*** Fill this in...
end function-test %logxor;

define machine-words function-test %logand ()
  //---*** Fill this in...
end function-test %logand;

define machine-words function-test %lognot ()
  //---*** Fill this in...
end function-test %lognot;

define machine-words function-test %logbit? ()
  //---*** Fill this in...
end function-test %logbit?;

define machine-words function-test %count-low-zeros ()
  //---*** Fill this in...
end function-test %count-low-zeros;

define machine-words function-test %count-high-zeros ()
  //---*** Fill this in...
end function-test %count-high-zeros;

define machine-words function-test \%+ ()
  //---*** Fill this in...
end function-test \%+;

define machine-words function-test \%- ()
  //---*** Fill this in...
end function-test \%-;

define machine-words function-test \%* ()
  //---*** Fill this in...
end function-test \%*;

define machine-words function-test %floor/ ()
  //---*** Fill this in...
end function-test %floor/;

define machine-words function-test %ceiling/ ()
  //---*** Fill this in...
end function-test %ceiling/;

define machine-words function-test %round/ ()
  //---*** Fill this in...
end function-test %round/;

define machine-words function-test %truncate/ ()
  //---*** Fill this in...
end function-test %truncate/;

define machine-words function-test %divide ()
  //---*** Fill this in...
end function-test %divide;

define machine-words function-test %negative ()
  //---*** Fill this in...
end function-test %negative;

define machine-words function-test %abs ()
  //---*** Fill this in...
end function-test %abs;

define machine-words function-test %shift-left ()
  //---*** Fill this in...
end function-test %shift-left;

define machine-words function-test %shift-right ()
  //---*** Fill this in...
end function-test %shift-right;


/// Overflow signalling operations

define machine-words function-test so%+ ()
  //---*** Fill this in...
end function-test so%+;

define machine-words function-test so%- ()
  //---*** Fill this in...
end function-test so%-;

define machine-words function-test so%* ()
  //---*** Fill this in...
end function-test so%*;

define machine-words function-test so%negative ()
  //---*** Fill this in...
end function-test so%negative;

define machine-words function-test so%abs ()
  //---*** Fill this in...
end function-test so%abs;

define machine-words function-test so%shift-left ()
  //---*** Fill this in...
end function-test so%shift-left;


/// Signed double word operations

define machine-words function-test d%floor/ ()
  //---*** Fill this in...
end function-test d%floor/;

define machine-words function-test d%ceiling/ ()
  //---*** Fill this in...
end function-test d%ceiling/;

define machine-words function-test d%round/ ()
  //---*** Fill this in...
end function-test d%round/;

define machine-words function-test d%truncate/ ()
  //---*** Fill this in...
end function-test d%truncate/;

define machine-words function-test d%divide ()
  //---*** Fill this in...
end function-test d%divide;


/// Unsigned single word operations

define machine-words function-test u%+ ()
  //---*** Fill this in...
end function-test u%+;

define machine-words function-test u%- ()
  //---*** Fill this in...
end function-test u%-;

define machine-words function-test u%* ()
  //---*** Fill this in...
end function-test u%*;

define machine-words function-test u%divide ()
  //---*** Fill this in...
end function-test u%divide;

define machine-words function-test u%rotate-left ()
  //---*** Fill this in...
end function-test u%rotate-left;

define machine-words function-test u%rotate-right ()
  //---*** Fill this in...
end function-test u%rotate-right;

define machine-words function-test u%shift-left ()
  //---*** Fill this in...
end function-test u%shift-left;

define machine-words function-test u%shift-right ()
  //---*** Fill this in...
end function-test u%shift-right;

define machine-words function-test u%< ()
  //---*** Fill this in...
end function-test u%<;


/// Unsigned double word operations

define machine-words function-test ud%divide ()
  //---*** Fill this in...
end function-test ud%divide;

define machine-words function-test ud%shift-left ()
  //---*** Fill this in...
end function-test ud%shift-left;

define machine-words function-test ud%shift-right ()
  //---*** Fill this in...
end function-test ud%shift-right;

