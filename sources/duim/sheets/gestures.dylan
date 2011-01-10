Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic gesture support

define constant %button_base    :: <integer> = 8;
define constant $left-button    :: <integer> = ash(1, %button_base + 0);
define constant $middle-button  :: <integer> = ash(1, %button_base + 1);
define constant $right-button   :: <integer> = ash(1, %button_base + 2);

// The order of this must match the values above
define constant $pointer-buttons :: <simple-object-vector>
    = #[#"left", #"middle", #"right"];

// Button indices are 0, 1, or 2
define inline function button-index
    (name) => (index :: false-or(<integer>))
  position($pointer-buttons, name)
end function button-index;

define inline function button-index-name
    (index :: <integer>) => (name)
  $pointer-buttons[index]
end function button-index-name;


define constant %modifier_base :: <integer> = 0;
define constant $shift-key     :: <integer> = ash(1, %modifier_base + 0);
define constant $control-key   :: <integer> = ash(1, %modifier_base + 1);
define constant $meta-key      :: <integer> = ash(1, %modifier_base + 2);
define constant $super-key     :: <integer> = ash(1, %modifier_base + 3);
define constant $hyper-key     :: <integer> = ash(1, %modifier_base + 4);

define constant $bucky-keys :: <integer>
    = logior($control-key, $meta-key, $super-key, $hyper-key);

define constant $alt-key    :: <integer> = $meta-key;
define constant $option-key :: <integer> = $super-key;

// One more magic key for Windows, which shifts to an alternate set
// of characters on the keyboard
define constant $altgr-key    :: <integer> = ash(1, %modifier_base + 5);
define constant $capslock-key :: <integer> = ash(1, %modifier_base + 6);

// The order of this must match the values above
// #"alt" and #"option" are handled as special cases below
define constant $modifier-keys :: <simple-object-vector>
    = #[#"shift", #"control", #"meta", #"super", #"hyper"];

// Modifier key indices are 0, 1, 2, 3, or 4
define inline function modifier-key-index
    (name) => (index :: false-or(<integer>))
  position($modifier-keys, name)
  | (name == #"alt"     & position($modifier-keys, #"meta"))
  | (name == #"option"  & position($modifier-keys, #"super"))
end function modifier-key-index;

define inline function modifier-key-index-name
    (index :: <integer>) => (name)
  $modifier-keys[index]
end function modifier-key-index-name;

// Modifier states can be compared with =
define function make-modifier-state
    (#rest modifiers) => (state :: <integer>)
  dynamic-extent(modifiers);
  let state = 0;
  for (name in modifiers)
    if (member?(name, #[1, 2, 4, 8, 16]))
      state := logior(state, name)
    else
      let bit = modifier-key-index(name);
      if (bit)
	state := logior(state, ash(1, %modifier_base + bit))
      else
	error("%= is not a valid modifier key name", name)
      end
    end
  end;
  state
end function make-modifier-state;


/// Higher level gesture support

// The idea here is to provide a "gesture" abstraction that can be matched
// against lower level events.  For example the gesture $left-button + $shift-key
// will match a pointer button event on the left-hand button while the shift
// key is held down.
define protocol-class gesture (<object>) end;


/// Keyboard gestures

define sealed class <keyboard-gesture> (<gesture>)
  sealed constant slot gesture-keysym :: <symbol>,
    required-init-keyword: keysym:;
  sealed constant slot gesture-modifier-state :: <integer>,
    required-init-keyword: modifier-state:;
end class <keyboard-gesture>;

define sealed domain make (singleton(<keyboard-gesture>));
define sealed domain initialize (<keyboard-gesture>);

// Mapping between standard characters and keysyms
define constant $standard-char-keysyms :: <simple-object-vector>
    = make(<vector>, size: 128);

begin
  for (ch :: <integer> from 33 below 127)
    // Each string must be unique, because 'as(<symbol>, ...)'
    // doesn't copy the string as it creates the symbol
    let string = make(<byte-string>, size: 1, fill: as(<character>, ch));
    $standard-char-keysyms[ch]  := as(<symbol>, string)
  end;
  $standard-char-keysyms[as(<integer>, ' ')]   := #"space";
  $standard-char-keysyms[as(<integer>, '\t')]  := #"tab";
  $standard-char-keysyms[as(<integer>, '\b')]  := #"backspace";
  $standard-char-keysyms[as(<integer>, '\n')]  := #"newline";
  $standard-char-keysyms[as(<integer>, '\r')]  := #"return";
  $standard-char-keysyms[as(<integer>, '\f')]  := #"form";
  $standard-char-keysyms[as(<integer>, '\e')]  := #"escape";
  $standard-char-keysyms[as(<integer>, '\0')]  := #"null";
  $standard-char-keysyms[127] := #"delete"	// formerly #"rubout"
end;

define inline function standard-char->keysym
    (char :: <character>) => (keysym :: false-or(<symbol>))
  let code = as(<integer>, char);
  0 <= code & code <= 127 & $standard-char-keysyms[code]
end function standard-char->keysym;

define method gesture-character
    (gesture :: <keyboard-gesture>) => (char :: false-or(<character>))
  let code = position($standard-char-keysyms, gesture-keysym(gesture));
  when (code)
    let char = as(<character>, code);
    if (alpha-char?(char))
      if (zero?(logand(gesture-modifier-state(gesture), $shift-key)))
	as-lowercase(char)
      else
	as-uppercase(char)
      end
    else
      char
    end
  end
end method gesture-character;


/// Useful methods on characters

define method gesture-character
    (char :: <character>) => (char :: false-or(<character>))
  char
end method gesture-character;

define method gesture-keysym
    (char :: <character>) => (keysym :: false-or(<symbol>))
  standard-char->keysym(char)
end method gesture-keysym;

define method gesture-modifier-state
    (char :: <character>) => (modifier-state :: <integer>)
  if (upper-case?(char)) $shift-key else 0 end
end method gesture-modifier-state;


/// Pointer gestures

define sealed class <pointer-gesture> (<gesture>)
  sealed constant slot gesture-button :: <integer>,
    required-init-keyword: button:;
  sealed constant slot gesture-modifier-state :: <integer>,
    required-init-keyword: modifier-state:;
end class <pointer-gesture>;

define sealed domain make (singleton(<pointer-gesture>));
define sealed domain initialize (<pointer-gesture>);


/// Constructors

define variable *gesture-table* :: <simple-object-vector>
    = make(<simple-vector>, size: 32);

// This interns gestures so that we can use == dispatch on them...
define sealed method make
    (class == <gesture>, #key keysym, button, modifiers, modifier-state = 0)
 => (gesture :: <gesture>)
  case
    keysym =>
      make(<keyboard-gesture>,
	   keysym: keysym,
	   modifiers: modifiers, modifier-state: modifier-state);
    button =>
      make(<pointer-gesture>,
	   button: button,
	   modifiers: modifiers, modifier-state: modifier-state);
    otherwise =>
      error("You must supply one of 'keysym:' or 'button:'");
  end
end method make;

define sealed method make
    (class == <keyboard-gesture>, #key keysym, modifiers, modifier-state = 0)
 => (gesture :: <keyboard-gesture>)
  let modifier-state
    = if (modifiers) apply(make-modifier-state, modifiers)
      else modifier-state end;
  when (instance?(keysym, <character>))
    assert(standard-char?(keysym),
	   "The character %s is not a standard character", keysym);
    when (upper-case?(keysym))
      modifier-state := logior(modifier-state, $shift-key)
    end;
    keysym := standard-char->keysym(keysym)
  end;
  let bucket :: <object-table>
    = begin
        let bucket = *gesture-table*[modifier-state];
        unless (bucket)
	  bucket := make(<table>);
	  *gesture-table*[modifier-state] := bucket
	end;
	bucket
      end;
  gethash(bucket, keysym)
  | begin
      let gesture = next-method(class,
				keysym: keysym, modifier-state: modifier-state);
      gethash(bucket, keysym) := gesture;
      gesture
    end
end method make;

define sealed method make
    (class == <pointer-gesture>, #key button, modifiers, modifier-state = 0)
 => (gesture :: <pointer-gesture>)
  let modifier-state
    = if (modifiers) apply(make-modifier-state, modifiers)
      else modifier-state end;
  when (member?(button, $pointer-buttons))
    button := ash(1, %button_base + button-index(button))
  end;
  let bucket :: <object-table>
    = begin
        let bucket = *gesture-table*[modifier-state];
        unless (bucket)
	  bucket := make(<table>);
	  *gesture-table*[modifier-state] := bucket
	end;
	bucket
      end;
  gethash(bucket, button)
  | begin
      let gesture = next-method(class,
				button: button, modifier-state: modifier-state);
      gethash(bucket, button) := gesture;
      gesture
    end
end method make;


// Gesture comparisons
define generic gesture-equal
    (gesture1, gesture2) => (true? :: <boolean>);

define method \=
    (g1 :: <gesture>, g2 :: <gesture>) => (true? :: <boolean>)
  g1 == g2
  | gesture-equal(g1, g2)
end method \=;

define method gesture-equal
    (gesture1 :: <gesture>, gesture2 :: <gesture>) => (true? :: <boolean>)
  #f
end method gesture-equal;

define sealed method gesture-equal
    (gesture1 :: <pointer-gesture>, gesture2 :: <pointer-gesture>)
 => (true? :: <boolean>)
  gesture-button(gesture1) == gesture-button(gesture2)
  & gesture-modifier-state(gesture1) == gesture-modifier-state(gesture2)
end method gesture-equal;

define sealed method gesture-equal
    (gesture1 :: <keyboard-gesture>, gesture2 :: <keyboard-gesture>)
 => (true? :: <boolean>)
  gesture-keysym(gesture1) == gesture-keysym(gesture2)
  & gesture-modifier-state(gesture1) == gesture-modifier-state(gesture2)
end method gesture-equal;


// Gesture matching
define generic event-matches-gesture?
    (event :: <event>, gesture :: type-union(<gesture>, <character>))
 => (true? :: <boolean>);

define method event-matches-gesture?
    (event :: <event>, gesture :: <gesture>) => (true? :: <boolean>)
  #f
end method event-matches-gesture?;

define sealed method event-matches-gesture?
    (event :: <pointer-button-event>, gesture :: <pointer-gesture>)
 => (true? :: <boolean>)
  event-button(event) == gesture-button(gesture)
  & event-modifier-state(event) == gesture-modifier-state(gesture)
end method event-matches-gesture?;

define sealed method event-matches-gesture?
    (event :: <keyboard-event>, gesture :: <keyboard-gesture>)
 => (true? :: <boolean>)
  event-key-name(event) == gesture-keysym(gesture)
  //---*** This isn't quite right -- if the shift key is held down to
  //---*** get a shifted keysym, the two modifier states won't match
  & event-modifier-state(event) == gesture-modifier-state(gesture)
end method event-matches-gesture?;

define sealed method event-matches-gesture?
    (event :: <keyboard-event>, char :: <character>)
 => (true? :: <boolean>)
  event-character(event) = char
  & logand(event-modifier-state(event), $bucky-keys) == 0
end method event-matches-gesture?;


define generic modifier-state-matches-gesture?
    (modifier-state :: <integer>, gesture :: <gesture>)
 => (true? :: <boolean>);

define sealed method modifier-state-matches-gesture?
    (modifier-state :: <integer>, gesture :: <pointer-gesture>)
 => (true? :: <boolean>)
  modifier-state == gesture-modifier-state(gesture)
end method modifier-state-matches-gesture?;
