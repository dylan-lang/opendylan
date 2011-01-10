Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <character> ... end;

define sealed inline method make (class == <character>, #key code)
 => (character :: <byte-character>);
  make(<byte-character>, code: code)
end method make;

define open generic as-uppercase (object :: <object>) => (result :: <object>);
define open generic as-lowercase (object :: <object>) => (result :: <object>);

define sealed inline method as (class == <character>, integer :: <abstract-integer>)
 => (result)
  as(<byte-character>, integer)
end method as;

define sealed inline method \=
    (character-1 :: <character>, character-2 :: <character>) => (well? :: <boolean>)
  as(<integer>, character-1) = as(<integer>, character-2)
end method \=;

define sealed inline method \< 
    (character-1 :: <character>, character-2 :: <character>) => (well? :: <boolean>)
  as(<integer>, character-1) < as(<integer>, character-2)
end method \<;

define sealed method as-uppercase (character :: <character>) 
 => (uppercase-character :: <character>)
  if (character.lowercase?)
    as(<character>,
       as(<integer>, character) + (as(<integer>, 'A') - as(<integer>, 'a')))
  else
    character
  end if
end method as-uppercase;

define inline function as-lowercase-guts (character :: <character>)
 => (lowercase-character :: <character>)
  if (character.uppercase?)
    as(<character>,
       as(<integer>, character) + (as(<integer>, 'a') - as(<integer>, 'A')))
  else
    character
  end if
end function as-lowercase-guts;

define sealed method as-lowercase (character :: <character>)
 => (lowercase-character :: <character>)
  as-lowercase-guts(character);
end method as-lowercase;

///// EXTRAS FROM COMMON LISP

// TODO: OBSOLETE?

/*
define function alpha? (character :: <character>) => (result :: <boolean>)
  let code :: <integer> = as(<integer>, character);
  (code >= as(<integer>, 'a') & code <= as(<integer>, 'z'))
  | (code >= as(<integer>, 'A') & code <= as(<integer>, 'Z'))
end function alpha?;
*/

define inline function lowercase? (character :: <character>) => (result :: <boolean>)
  let code :: <integer> = as(<integer>, character);
  code >= as(<integer>, 'a') & code <= as(<integer>, 'z')
end function lowercase?;

define inline function uppercase? (character :: <character>) => (result :: <boolean>)
  let code :: <integer> = as(<integer>, character);
  code >= as(<integer>, 'A') & code <= as(<integer>, 'Z')
end function uppercase?;

////
//// <BYTE-CHARACTER>
////

// BOOTED: define ... class <byte-character> ... end;

//  (code init-keyword: code: type: <integer>)

define macro character-definer
  { define character "<" ## ?:name ## "-character>" }
    => { define sealed inline method make
	     (class == "<" ## ?name ## "-character>", 
              #key code :: "<" ## ?name ## "-integer>")
	  => (character :: "<" ## ?name ## "-character>")
	   as("<" ## ?name ## "-character>", code)
	 end method make;

	 define sealed inline method as 
	     (class == <abstract-integer>, character :: "<" ## ?name ## "-character>")
	  => (code :: "<" ## ?name ## "-integer>");
	   as(<integer>, character)
	 end method as;

	 define sealed inline method as 
	     (type :: <limited-integer>, character :: "<" ## ?name ## "-character>")
	  => (code :: "<" ## ?name ## "-integer>");
	   as(<integer>, character)
	 end method as;

	 define sealed inline method as
	     (class == <integer>, character :: "<" ## ?name ## "-character>")
	  // => (code :: "<" ## ?name ## "-integer>");
	  //  let code :: "<" ## ?name ## "-integer>"
          //    = raw-as-integer("primitive-" ## ?name ## "-character-as-raw"(character));
          //  code
	  => (code :: <integer>)
	   raw-as-integer("primitive-" ## ?name ## "-character-as-raw"(character))
	 end method as;

	 define sealed inline method as
	     (class == "<" ## ?name ## "-character>", 
              // integer :: "<" ## ?name ## "-integer>")
              integer :: <integer>)
	  => (result :: "<" ## ?name ## "-character>")
	   // (element *byte-characters* integer)
	   "primitive-raw-as-" ## ?name ## "-character"(integer-as-raw(integer))
	 end method as;
         }
end macro;

define constant <byte-integer> = <byte>;
define character <byte-character>;

define constant $number-ascii-characters = 256;

define constant $lowercase-ascii :: <byte-string>
  = make(<byte-string>, size: $number-ascii-characters);

for (i from 0 below size($lowercase-ascii))
  let c = as(<byte-character>, i);
  $lowercase-ascii[i] := as-lowercase-guts(c);
end for;

/// THIS NEEDS TO BE FAST FOR SYMBOLS ETC

define sealed inline method as-lowercase (character :: <byte-character>)
 => (lowercase-character :: <byte-character>)
  // without-bounds-checks
  element-no-bounds-check($lowercase-ascii, as(<integer>, character))
  // end without-bounds-checks;
end method as-lowercase;

// ALREADY BOOTED
// (define *byte-characters* (make <vector> size: 256))

/// INITIALIZE *BYTE-CHARACTERS*

// (for ((index from 0 below 256))
//  (set! (element *byte-characters* index) (as <byte-character> index)))
