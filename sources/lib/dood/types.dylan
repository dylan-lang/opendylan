Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// DISK-ADDRESS

define constant <address> = <integer>;
define constant <pointer> = <address>;

define constant <dood-disk-address> = <address>;
define constant <dood-disk-pointer> = <pointer>;

//// TAGGING

define constant $tag-pairs?         = #t;

define constant $number-tag-bits    = 2;
define constant $tagged-word-size   = $word-size - $number-tag-bits;
define constant $dood-word-size     = $tagged-word-size - $number-tag-bits;
define constant $tag-mask           = 3;
define constant $address-mask       = 2;
// define constant $mark-mask          = 1;
// define constant $mark-tag           = 1;
define constant $address-tag        = 0;
define constant $pair-tag           = 1;
define constant $integer-tag        = 2;
define constant $byte-character-tag = 3;

define constant $max-dood-integer :: <integer>
  =   (2 ^ ($dood-word-size - 1)) - 1;
define constant $min-dood-integer :: <integer>
  = - (2 ^ ($dood-word-size - 1));

/*
define method tagged? (object :: <integer>) #T end;
define method tagged? (object :: <byte-character>) #T end;
define method tagged? (object :: <pair>) $tag-pairs? end;
define method tagged? (object) #F end;
*/

define inline function untag (pointer :: <pointer>)
  ash(pointer, - $number-tag-bits)
end function;

// define inline function marked? 
//     (pointer :: <pointer>) => (res :: <boolean>)
//   logand(pointer, $mark-mask) == $mark-tag
// end function;

define inline function address? 
    (pointer :: <pointer>) => (res :: <boolean>)
  logand(pointer, $address-mask) == $address-tag
end function;

define inline function integer? 
    (pointer :: <pointer>) => (res :: <boolean>)
  logand(pointer, $tag-mask) == $integer-tag
end function;

define inline function pair? 
    (pointer :: <pointer>) => (res :: <boolean>)
  logand(pointer, $tag-mask) == $pair-tag
end function;

define inline function byte-character? 
    (pointer :: <pointer>) => (res :: <boolean>)
  logand(pointer, $tag-mask) == $byte-character-tag
end function;

define inline function tag-as-integer 
    (address :: <address>) => (res :: <pointer>)
  logior(ash(address, $number-tag-bits), $integer-tag)
end function;

define inline function tag-as-pair 
    (address :: <address>) => (res :: <pointer>)
  logior(ash(address, $number-tag-bits), $pair-tag)
end function;

define inline function tag-as-byte-character 
    (address :: <address>) => (res :: <pointer>)
  logior(ash(address, $number-tag-bits), $byte-character-tag)
end function;

// define inline function mark
//     (pointer :: <pointer>) => (res :: <pointer>)
//   logior(pointer, $mark-tag)
// end function;

// define inline function unmark
//     (pointer :: <pointer>) => (res :: <pointer>)
//   logand(pointer, lognot($mark-tag))
// end function;

define inline function tag-as-address 
    (object, address :: <address>) => (res :: <pointer>)
  if ($tag-pairs? & instance?(object, <pair>))
    tag-as-pair(address)
  else
    logior(ash(address, $number-tag-bits), $address-tag)
  end if
end function;

define inline function bytes-to-words (bytes :: <integer>)
  truncate/(bytes + $bytes-per-word - 1, $bytes-per-word)
end function;

// eof
