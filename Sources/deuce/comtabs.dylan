Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command tables

define constant <command-oid> = false-or(type-union(<function>, <command-table>));

define protocol <<command-table>> ()
  getter command-table-name
    (command-table :: <command-table>) => (name);
  function add-command!
    (command-table :: <command-table>, gesture, command :: <command-oid>)
 => (old-command :: <command-oid>);
  function remove-command!
    (command-table :: <command-table>, gesture)
 => (command :: <command-oid>);
  function find-command
    (command-table :: <command-table>, gesture)
 => (command :: <command-oid>);
end protocol <<command-table>>;

// A command table maps from "gestures" (that is, a keysym plus a modifier
// state) to either a function that implements the command, or to a nested
// command table in the case of "control-X"-style gestures.
define sealed class <standard-command-table> (<command-table>)
  sealed slot command-table-name = #f,
    init-keyword: name:;
  // A vector that's just big enough to the usual keycodes (including the
  // ASCII (ISO Latin-1) characters and the Windows characters as well)
  // Each entry in the vector is a tuple that encodes the modifier key state, too
  sealed slot %key-table :: <simple-object-vector>
    = make(<simple-object-vector>, size: 256, fill: #[]);
  // The keysym table is for named keysyms
  // Each entry in the table is a tuple that encodes the modifier key state, too
  sealed slot %keysym-table :: <object-table> = make(<table>);
end class <standard-command-table>;

define sealed domain make (singleton(<standard-command-table>));
define sealed domain initialize (<standard-command-table>);

define sealed inline method make
    (class == <command-table>, #rest initargs, #key, #all-keys)
 => (command-table :: <standard-command-table>)
  apply(make, <standard-command-table>, initargs)
end method make;


define sealed method copy-command-table
    (command-table :: <standard-command-table>)
 => (new-command-table :: <standard-command-table>)
  copy-command-table-into!(command-table, make(<standard-command-table>))
end method copy-command-table;

define sealed method copy-command-table-into!
    (command-table :: <standard-command-table>, into :: <standard-command-table>)
 => (into :: <standard-command-table>)
  command-table-name(into) := command-table-name(command-table);
  for (i :: <integer> from 0 below size(command-table.%key-table))
    into.%key-table[i] := copy-sequence(command-table.%key-table[i])
  end;
  for (entry keyed-by key in command-table.%keysym-table)
    gethash(into.%keysym-table, key) := copy-sequence(entry)
  end;
  into
end method copy-command-table-into!;


// Note that, in gestures, the character is case sensitive!
define sealed method add-command!
    (command-table :: <standard-command-table>, gesture, command :: <command-oid>)
 => (old-command :: <command-oid>)
  let (bucket, modifier-state) = decode-gesture(gesture);
  let old = #f;
  if (instance?(bucket, <integer>))
    let entries = command-table.%key-table[bucket];
    when (empty?(entries))
      entries := make(<simple-object-vector>, size: 16); // XXX: hardcoded number of modifier bits!
      command-table.%key-table[bucket] := entries
    end;
    old := entries[modifier-state];
    entries[modifier-state] := command
  else
    let entries = gethash(command-table.%keysym-table, bucket);
    unless (entries)
      entries := make(<simple-object-vector>, size: 16); // XXX: hardcoded number of modifier bits!
      gethash(command-table.%keysym-table, bucket) := entries
    end;
    old := entries[modifier-state];
    entries[modifier-state] := command
  end;
  old
end method add-command!;

define sealed method add-commands!
    (command-table :: <standard-command-table>, #rest commands)
 => (old-commands :: <stretchy-object-vector>)
  let old-commands :: <stretchy-object-vector> = make(<stretchy-vector>);
  let gesture = vector(#f, #f);
  for (c in commands)
    gesture[0] := c[0];
    gesture[1] := c[1];
    let command = c[2];
    let old-command = add-command!(command-table, gesture, command);
    add!(old-commands, list(c[0], c[1], old-command))
  end;
  old-commands
end method add-commands!;


define sealed method remove-command!
    (command-table :: <standard-command-table>, gesture)
 => (command :: <command-oid>)
  let (bucket, modifier-state) = decode-gesture(gesture);
  let old = #f;
  if (instance?(bucket, <integer>))
    let entries = command-table.%key-table[bucket];
    unless (empty?(entries))
      old := entries[modifier-state];
      entries[modifier-state] := #f
    end
  else
    let entries = gethash(command-table.%keysym-table, bucket);
    when (entries)
      old := entries[modifier-state];
      entries[modifier-state] := #f
    end
  end;
  old
end method remove-command!;

define sealed method remove-commands!
    (command-table :: <standard-command-table>, #rest commands) => ()
  let gesture = vector(#f, #f);
  for (c in commands)
    gesture[0] := c[0];
    gesture[1] := c[1];
    remove-command!(command-table, gesture)
  end
end method remove-commands!;


define sealed method find-command
    (command-table :: <standard-command-table>, gesture)
 => (command :: <command-oid>)
  let (bucket, modifier-state) = decode-gesture(gesture);
  if (instance?(bucket, <integer>))
    let entries = command-table.%key-table[bucket];
    ~empty?(entries)
    & entries[modifier-state]
  else
    let entries = gethash(command-table.%keysym-table, bucket);
    entries & entries[modifier-state]
  end
end method find-command;


/// Gestures

define constant $control-key :: <integer> = #o01;
define constant $meta-key    :: <integer> = #o02;
define constant $super-key   :: <integer> = #o04;
define constant $shift-key   :: <integer> = #o10;

define constant $modifier-key-names :: <simple-object-vector>
    = #["",    "c-",    "m-",    "c-m-",    "s-",    "s-c-",    "s-m-",    "s-c-m-",
        "sh-", "c-sh-", "m-sh-", "c-m-sh-", "s-sh-", "s-c-sh-", "s-m-sh-", "s-c-m-sh-"];

define constant $left-button   :: <integer> = #o0400;
define constant $middle-button :: <integer> = #o1000;
define constant $right-button  :: <integer> = #o2000;

// The kinds of mouse events...
define constant <event-type> = one-of(#"press", #"release", #"double-click", #"drag");


/// Simple mouse gestures

define sealed class <gesture> (<object>)
  sealed constant slot %button :: <integer>,
    required-init-keyword: button:;
  sealed constant slot %modifiers :: <integer>,
    required-init-keyword: modifiers:;
  sealed constant slot %event-type :: false-or(<event-type>) = #f,
    init-keyword: event-type:;
end class <gesture>;

define sealed domain make (singleton(<gesture>));
define sealed domain initialize (<gesture>);

define sealed method \=
    (gesture1 :: <gesture>, gesture2 :: <gesture>) => (equal? :: <boolean>)
  gesture1 == gesture2
  | (  gesture1.%button    = gesture2.%button
     & gesture1.%modifiers = gesture2.%modifiers)
end method \=;

define method gesture-matches?
    (gesture :: <gesture>, button, modifiers, #key event-type = $unsupplied)
 => (matches? :: <boolean>)
    gesture.%button    == button
  & gesture.%modifiers == modifiers
  & (unsupplied?(event-type)
     | gesture.%event-type == event-type
     | gesture.%event-type == #f)
end method gesture-matches?;


define variable $menu-gesture :: <gesture>
    = make(<gesture>, 
	   button: $right-button, modifiers: 0, event-type: #"press");

define variable $move-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button, modifiers: 0, event-type: #"press");
define variable $copy-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button, modifiers: $control-key, event-type: #"press");

define variable $edit-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button, modifiers: $control-key + $shift-key, event-type: #"press");


// The gesture is a <gesture>, a character, or a two-element sequence of
// a character or keysym followed by a modifier state.
// NB: the case of characters is significant!
define method decode-gesture
    (gesture)
 => (bucket :: false-or(type-union(<integer>, <symbol>)), modifier-state :: <integer>)
  select (gesture by instance?)
    <character> =>
      values(as(<integer>, gesture), 0);
    <symbol> =>
      values(gesture, 0);
    <sequence> =>
      let keysym    = gesture[0];
      let modifiers = gesture[1];
      let modifier-state = 0;
      if (instance?(modifiers, <integer>))
	modifier-state := modifiers
      else
        if (instance?(keysym, <character>))
	  when (member?(#"shift", modifiers))
	    keysym := as-uppercase(keysym)
	  end
        else
          when (member?(#"shift", modifiers))
            modifier-state := logior(modifier-state, $shift-key)
          end
        end;
	when (member?(#"control", modifiers))
	  modifier-state := logior(modifier-state, $control-key)
	end;
	when (member?(#"meta", modifiers) | member?(#"alt", modifiers))
	  modifier-state := logior(modifier-state, $meta-key)
	end;
	when (member?(#"super", modifiers) | member?(#"option", modifiers))
	  modifier-state := logior(modifier-state, $super-key)
	end
      end;
      if (instance?(keysym, <character>))
	values(as(<integer>, keysym), modifier-state)
      else
	values(keysym, modifier-state)
      end;
  end
end method decode-gesture;

define method decode-gesture
    (gesture :: <gesture>)
 => (bucket :: type-union(<integer>, <symbol>), modifier-state :: <integer>)
  error("<gesture> objects are not supported in command tables")
end method decode-gesture;


/// Command sets

define sealed class <standard-command-set> (<command-set>)
  sealed slot command-set-name = #f,
    init-keyword: name:;
  sealed slot standard-command-table  :: <standard-command-table>,
    required-init-keyword: standard-command-table:;
  sealed slot control-X-command-table :: <standard-command-table>,
    required-init-keyword: control-X-command-table:;
  sealed slot control-C-command-table :: <standard-command-table>,
    required-init-keyword: control-C-command-table:;
  sealed slot escape-command-table    :: <standard-command-table>,
    required-init-keyword: escape-command-table:;
end class <standard-command-set>;

define sealed domain make (singleton(<standard-command-set>));
define sealed domain initialize (<standard-command-set>);

define sealed inline method make
    (class == <command-set>, #rest initargs, #key, #all-keys)
 => (command-set :: <standard-command-set>)
  apply(make, <standard-command-set>, initargs)
end method make;

define sealed method copy-command-set
    (command-set :: <standard-command-set>)
 => (new-command-set :: <standard-command-set>)
  copy-command-set-into!(command-set,
			 make(<command-set>,
			      standard-command-table:  make(<command-table>),
			      control-X-command-table: make(<command-table>),
			      control-C-command-table: make(<command-table>),
			      escape-command-table:    make(<command-table>)))
end method copy-command-set;

define sealed method copy-command-set-into!
    (command-set :: <standard-command-set>, into :: <standard-command-set>)
 => (into :: <standard-command-set>)
  command-set-name(into) := command-set-name(command-set);
  copy-command-table-into!(standard-command-table(command-set),
			   standard-command-table(into));
  copy-command-table-into!(control-X-command-table(command-set),
			   control-X-command-table(into));
  copy-command-table-into!(control-C-command-table(command-set),
			   control-C-command-table(into));
  copy-command-table-into!(escape-command-table(command-set),
			   escape-command-table(into));
  //--- Kludgy way to fix up the prefix bindings...
  select (command-set-name(into))
    #"emacs" =>
      let command-table = standard-command-table(into);
      add-command!(command-table, vector('x', $control-key),
		   control-X-command-table(into));
      add-command!(command-table, vector('c', $control-key),
		   control-C-command-table(into));
      add-command!(command-table, vector(#"escape", 0),
		   escape-command-table(into));
    #"windows" =>
      #f;
    otherwise =>
      #f;
  end;
  into
end method copy-command-set-into!;
