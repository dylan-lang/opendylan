Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dood-unbound = unbound();

// version

define constant $dood-version-id                           = 0;
define constant $dood-user-version-id                      = 1;

// book keeping

define constant $dood-corruption-id                        = 2;
define constant $dood-free-address-id                      = 3;
define constant $dood-root-id                              = 4;
define constant $dood-proxies-id                           = 5;

// predefines

define constant $dood-predefines-begin                     = 10;

define constant $dood-true-id                              = 10;
define constant $dood-false-id                             = 11;
define constant $dood-unbound-id                           = 12;
define constant $dood-empty-list-id                        = 13;
define constant $dood-empty-vector-id                      = 14;
define constant $dood-class-program-binding-proxy-class-id = 15;
define constant $dood-program-module-proxy-class-id        = 16;
define constant $dood-symbol-class-id                      = 17;
define constant $dood-string-class-id                      = 18;
define constant $dood-pair-class-id                        = 19;

define constant $dood-predefines-end                       = 20;

//define constant $dood-predefines-count
//  = $dood-predefines-end - $dood-predefines-begin;

// header finish

define constant $dood-header-size                    = $dood-predefines-end;

define constant $dood-version    =  4;
define constant $dood-no-version = -1;

define constant $dood-empty-vector = #[];

define constant $dood-empty-stretchy-vector = make(<stretchy-vector>, size: 0);

// TODO: PLUG IN REAL THANG

define constant $default-dood-table-size = 128;

define inline function make-weak-key-table
   (#rest all-keys, #key size, #all-keys)
  apply(make, <dood-table>, /* weak: #"key", */ all-keys)
end function;

define inline function make-weak-value-table
   (#rest all-keys, #key size, #all-keys)
  apply(make, <dood-table>, /* weak: #"value", */ all-keys)
end function;

define inline function make-object->address-table (#rest all-keys, #key size)
  apply(make-weak-key-table, all-keys)
  // apply(make-two-level-table, all-keys)
end function;

define inline function make-address->object-table 
    (#key size = $default-dood-table-size)
  // make-two-level-table()
  make(<dood-table>, size: size)
end function;

// define function two-level-table-sizes (x :: <table>)
//   let sizes = collecting () for (e in x) collect(size(e)) end end;
//   list(reduce(\+, 0, sizes), sizes)
// end function;

// define function dood-object->address-table-sizes (x :: <table>)
//   two-level-table-sizes(x)
// end function;

// define function dood-address->object-table-sizes (x :: <table>)
//   // two-level-table-sizes(x)
//   size(x)
// end function;

define inline function make-weak-table 
    (kind :: false-or(<symbol>), #rest all-keys, #key size, #all-keys)
  if (kind == #"key")
    apply(make-weak-key-table, all-keys)
  elseif (kind == #"value")
    apply(make-weak-value-table, all-keys)
  else // false
    apply(make, <dood-table>, all-keys)
  end if
end function;

define macro dood-state-definer
  { define ?adjectives:* dood-state ?:name
	of ?class:name using ?trampoline:name (?supers:*)
      ?slots:*
    end }
    => { define dood-state-class (?adjectives) ?name (?supers) ?slots end;
         define dood-state-accessors using ?trampoline (?class) ?slots end }
end macro;

define macro dood-state-class-definer
  { define dood-state-class (?adjectives:*) ?:name (?supers:*) ?cslots end }
    => { define ?adjectives class ?name (?supers) ?cslots end }
cslots:
  { } => { }
  { ?cslot:*; ...} => { ?cslot; ...}
cslot:
  { ?mods:* slot ?slot-variable, ?rest:* }
    => { ?mods slot ?slot-variable, ?rest }
slot-variable:
  { ?slot-name :: ?:expression ?maybe-init-expression } 
    => { ?slot-name :: ?expression ?maybe-init-expression }
slot-name:
  { ?:name } => { ?name ## "-state" }
maybe-init-expression:
  { = ?:expression } => { = ?expression }
  { }                => { }
end macro;

define macro dood-state-accessors-definer
  { define dood-state-accessors using ?trampoline:name (?class:name) end } 
    => { }

  { define dood-state-accessors using ?trampoline:name (?class:name)
      constant slot ?accessor:name :: ?type:expression ?maybe-init-expression,
          ?rest:*;
      ?more:*
    end }
    => { define inline method ?accessor (object :: ?class) => (val :: ?type)
	   ?accessor ## "-state"(?trampoline(object))
	 end method;
         define dood-state-accessors using ?trampoline (?class) ?more end }

  { define dood-state-accessors using ?trampoline:name (?class:name)
      slot ?accessor:name :: ?type:expression ?maybe-init-expression,
          ?rest:*;
      ?more:*
    end }
    => { define inline method ?accessor (object :: ?class) => (val :: ?type)
	   ?accessor ## "-state"(?trampoline(object))
	 end method;
         define inline method ?accessor ## "-setter" 
	     (value :: ?type, object :: ?class)
	   ?accessor ## "-state"(?trampoline(object)) := value
         end method;
         define dood-state-accessors using ?trampoline (?class) ?more end }
maybe-init-expression:
  { = ?:expression } => { = ?expression }
  { }                => { }
end macro;

// (gts,98sep20) moved from object.dylan to avoid emulator forward ref. issue. 

define constant <dood-segment-id> = <integer>;

define class <dood-segment> (<object>)
  slot dood-segment-name = #"default",
    init-keyword: name:;
  slot dood-segment-id :: <dood-segment-id> = 0,
    init-keyword: id:;
end class;

ignore(dood-segment-name);

define generic dood-segment-instance? 
    (segment :: <dood-segment>, x) => (well? :: <boolean>);


define method dood-segment-instance? 
    (segment :: <dood-segment>, x) => (well? :: <boolean>)
  #t
end method;

define class <dood-typed-segment> (<dood-segment>)
  constant slot dood-segment-type :: <type>,
    required-init-keyword: type:;
end class;

define sealed method initialize 
    (segment :: <dood-typed-segment>, #key name, type, #all-keys)
  next-method();
  unless (name)
    dood-segment-name(segment) := type;
  end unless;
end method;

define method dood-segment-instance? 
    (segment :: <dood-typed-segment>, x) => (well? :: <boolean>)
  subtype?(x, dood-segment-type(segment))
end method;

define class <dood-functional-segment> (<dood-segment>)
  constant slot dood-segment-test :: <function>,
    required-init-keyword: test:;
end class;

define method dood-segment-instance? 
    (segment :: <dood-functional-segment>, x) => (well? :: <boolean>)
  dood-segment-test(segment)(x)
end method;

define class <dood-segment-state> (<object>)
  constant slot dood-segment-state-segment :: <dood-segment>,
    required-init-keyword: segment:;
  slot dood-segment-free-address :: <address> = 0,
    init-keyword: free-address:;
end class;

ignore(dood-segment-state-segment);

// ok, done with stuff moved from object.dylan

define open primary class <dood> (<object>)
  slot dood-name = #f,
    init-keyword: name:;
  slot dood-given-stream :: false-or(<stream>) = #f, 
    init-keyword: stream:;
  slot dood-backups? :: <boolean> = #t, 
    init-keyword: backups?:;
  slot dood-root = #f;
  slot dood-state  :: <dood-state> = make(<dood-state>);
  slot dood-backup :: <dood-state> = make(<dood-state>);
  constant slot dood-world :: <dood-world> = dood-world-default(), 
    init-keyword: world:;
  constant slot dood-read-only? :: <boolean> = #f, 
    init-keyword: read-only?:;
  constant slot dood-batch-mode? :: <boolean> = #t, 
    init-keyword: batch-mode?:;
  constant slot dood-specified-user-version :: <integer> = $dood-version,
    init-keyword: version:;
  slot dood-forwarding-address :: false-or(<address>) = #f;
  // should be locked
  slot dood-walked-addresses :: <dood-table> = make-object->address-table();
  /*
  // used for walking
  slot dood-current-mark :: <integer> = 0;
  slot dood-walked-mark-addresses :: <table> 
     = make-weak-key-table(size: $default-dood-table-size);
  slot dood-walked-objects :: <stretchy-vector> 
    = make(<stretchy-vector>);
  */
  constant slot dood-back-pointers :: <dood-table> 
    = make(<dood-table>);
  slot dood-walked-count :: <integer> = 0;
  slot dood-locator, init-keyword: locator:;
  slot dood-init-keys :: <simple-object-vector>;
  slot dood-walk-queue :: <dood-queue> 
    = make(<dood-queue>);
  constant slot dood-work :: <dood-queue> 
    = make(<dood-queue>);
  constant slot dood-default-segment :: <dood-segment> = make(<dood-segment>),
   init-keyword: default-segment:;
  slot dood-segments :: <simple-object-vector> = make-default-segments(),
   init-keyword: segments:;
end class;

// This slot is not really needed...
ignore(dood-given-stream);

define inline method dood-classes (dood :: <dood>) => (res :: <dood-table>)
  dood-world-classes(dood-world(dood))
end method;

define function make-default-segments () => (res :: <simple-object-vector>)
  // #[]
  vector(make(<dood-typed-segment>, 
  	      name: "symbol", 
  	      type: type-union(<symbol>, <string>)))
end function;

define dood-state <dood-state> of <dood> using dood-state (<object>)
  slot dood-dood :: <dood>, init-keyword: dood:;
  slot dood-stream :: <dood-stream>, init-keyword: stream:;
  slot dood-free-address :: <address> = 0;
  slot dood-addresses :: <dood-table> = make-object->address-table();
  constant slot dood-objects :: <dood-table>   = make-address->object-table();
  // HACK: SHOULD BE SET
  constant slot dood-predefines :: <dood-table> = make(<dood-table>);
  constant slot dood-predefine-addresses :: <dood-table> = make(<dood-table>);
  constant slot dood-disk-objects :: <dood-table> = make(<dood-table>);
  constant slot dood-module-proxies :: <dood-table> = make(<dood-table>);
  // same as dood-disk-objects.key-sequence..
  slot dood-proxies :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot dood-lock :: <recursive-lock> = make(<recursive-lock>);
  slot dood-segment-states :: <simple-object-vector> = #[];
  slot dood-current-segment :: <dood-segment>;
end dood-state;

ignore(dood-lock);                           // HACK: SHOULD AVOID CONSTRUCTING THIS
ignore(dood-dood); ignore(dood-dood-setter); // HACK: SHOULD USE CONSTANT ADJECTIVE

// define method dood-reinitialize-state
//     (state :: <dood-state>, #rest all-keys, #key, #all-keys) => ()
//   // rerun init expressions
//   apply(reinitialize, state, all-keys);
// end method;

define method dood-flush-state (x :: <dood-state>) => (res :: <dood-state>)
  if (slot-initialized?(x, dood-stream-state))
    make(<dood-state>, 
	 dood:   dood-dood-state(x),
	 stream: dood-stream-state(x));
  else
    x
  end if
end method;

define method dood-flush-state (x :: <dood>) => (res :: <dood>)
  dood-state(x)  := dood-flush-state(dood-state(x));
  dood-backup(x) := dood-flush-state(dood-backup(x));
  x
end method;

define method dood-exchange-states (dood :: <dood>)
  let tmp-state      = dood-backup(dood);
  dood-backup(dood) := dood-state(dood);
  dood-state(dood)  := tmp-state;
end method;

define open class <dood-proxy-error> (<error>)
end class;

define inline method do-with-dood-state
    (dood :: <dood>, state :: <dood-state>, f1 :: <function>, f2 :: <function>)
  with-lock (dood-lock-state(state))
    if (dood-state(dood) == state)
      f1()
    else 
      block ()
	dood-exchange-states(dood);
	if (dood-state(dood) == state)
	  f2();
	else 
	  signal(make(<dood-proxy-error>));
	end if;
      cleanup
	dood-exchange-states(dood);
      end block;
    end if;
  end with-lock;
end method;

define macro with-dood-state
  { with-dood-state (?dood:expression, ?state:expression) ?:body end }
      => { do-with-dood-state(?dood, ?state, method () ?body end, method () ?body end) }
end macro;

define method dood-open-stream
    (dood :: <dood>, #rest extra-keys, #key, #all-keys) 
 => (stream :: <dood-stream>)
  let all-keys = concatenate(extra-keys, dood-init-keys(dood));
  apply(make, <dood-stream>, 
        buffer-vector: dood-world-buffer-pool(dood-world(dood)),
        // number-of-buffers: $dood-default-number-of-buffers, 
        // buffer-size:       $dood-default-buffer-size, 
        // direction:         #"input-output", 
        all-keys);
end method;

define method dood-new-locator (dood :: <dood>) => (locator)
  let locator = dood-locator(dood);
  make(<file-locator>,
       directory: locator-directory(locator),
       base: locator-base(locator),
       extension: "new")
end method;

define method dood-open-new-stream
    (dood :: <dood>) => (stream :: <dood-stream>)
  dood-open-stream
    (dood, locator: dood-new-locator(dood), if-exists: #"replace");
end method;

define method dood-save-state (dood :: <dood>) => ()
  when (dood-backups?(dood))
    dood-exchange-states(dood);
    dood-state(dood) := dood-flush-state(dood-state(dood));
    dood-stream-state(dood-state(dood)) := dood-open-new-stream(dood);
    dood-boot(dood);
  end when;
end method;

define method dood-close-state-stream
    (dood :: <dood>, state :: <dood-state>, 
     #rest all-keys, #key abort? = #t, #all-keys) => ()
  if (slot-initialized?(state, dood-stream-state))
    apply(close, dood-stream-state(state), abort?: abort?, all-keys)
  end if;
end method;

define method dood-size (dood :: <dood>) => (res :: <integer>)
  let state = dood-state(dood);
  if (slot-initialized?(state, dood-stream-state))
    stream-size(dood-stream(dood))
  else
    0
  end if
end method;

define method dood-restore-state (dood :: <dood>) => ()
  when (dood-backups?(dood))
    dood-exchange-states(dood);
    dood-close-state-stream(dood, dood-backup(dood));
    delete-file(dood-new-locator(dood));
    dood-backup(dood) := dood-flush-state(dood-backup(dood));
  end when;
end method;

define method dood-close-streams (dood :: <dood>) => ()
  dood-close-state-stream(dood, dood-state(dood));
  dood-close-state-stream(dood, dood-backup(dood));
end method;

define method dood-exchange-stream-names (dood :: <dood>)
  dood-close-streams(dood);
  rename-file
    (dood-new-locator(dood), dood-locator(dood), if-exists: #"replace");
  dood-stream(dood)   // REOPEN
    := dood-open-stream(dood, if-exists: #"overwrite");
end method;

define method dood-flush-backup (dood :: <dood>) => ()
  when (dood-backups?(dood))
    dood-exchange-stream-names(dood);
    dood-backup(dood) := dood-flush-state(dood-backup(dood));
  end when;
end method;

// define method dood-read-only? (dood :: <dood>) => (res :: <boolean>)
//   stream-direction(dood) == #"input"
// end method;

define method boot-predefines (dood :: <dood>) => ()
  // TODO: EVERY DATABASE HAS COPY OF THESE!
  local method register-predefine (object, address)
          dood-register-object(dood, object, address);
	  dood-predefines(dood)[object] := object;
	  dood-predefine-addresses(dood)[object] := address;
        end method;
  register-predefine(#t,                  $dood-true-id);
  register-predefine(#f,                  $dood-false-id);
  register-predefine($dood-unbound,       $dood-unbound-id);
  register-predefine(#(),                 $dood-empty-list-id);
  register-predefine($dood-empty-vector,  $dood-empty-vector-id);
  register-predefine(<byte-string>,       $dood-string-class-id);
  register-predefine(<pair>,              $dood-pair-class-id);
  register-predefine(<symbol>,            $dood-symbol-class-id);
  register-predefine
    (<dood-class-program-binding-proxy>, $dood-class-program-binding-proxy-class-id);
  register-predefine
    (<dood-program-module-proxy>, $dood-program-module-proxy-class-id);
end method;  

/*
define method dood-reset-live-objects 
    (dood :: <dood>, live-objects :: <set>) => ()
  let objects = make-address->object-table();
  format-out("NUMBER LIVE OBJECTS %=\n", size(live-objects));
  for (object keyed-by address in dood-objects(dood))
    when (member?(object, live-objects))
      format-out("  FOUND %= @ %=\n", object, address);
      objects[address] := object;
    end when;
  end for;
  format-out("NUMBER FOUND LIVE OBJECTS %=\n", size(objects));
  dood-objects(dood) := objects;
  let addresses = make-object->address-table();
  for (address keyed-by object in dood-addresses(dood))
    when (member?(object, live-objects))
      format-out("  FOUND %= @ %=\n", object, address);
      addresses[object] := address;
    end when;
  end for;
  format-out("NUMBER FOUND LIVE ADDRESSES %=\n", size(addresses));
  dood-addresses(dood) := addresses;
  for (object keyed-by address in dood-objects(dood))
    unless (dood-address(dood, object))
      // INVERT MAPPING TABLE ENTRY
      format-out("INVERTING %=\n", object);
      dood-register-address(dood, object, address);
      mark-lazy-slots(dood, object);
    end unless;
  end for;
  boot-predefines(dood);
end method;
*/

define sealed method dood-initial-segment-states 
    (dood :: <dood>) => (res :: <simple-object-vector>)
  let states
    = collecting (as <simple-object-vector>)
        for (segment in dood-segments(dood))
	  collect(make(<dood-segment-state>, segment: segment));
	end for;
      end collecting;
  states;
end method;

define method dood-boot (dood :: <dood>) => ()
  dood-segment-states(dood)
    := dood-initial-segment-states(dood);
  let segment   
    = dood-default-segment(dood);
  let segment-state
    = dood-lookup-segment-state-by-id(dood, dood-segment-id(segment));
  dood-segment-free-address(segment-state)
    := $dood-header-size;
  dood-current-segment(dood)
    := segment;
  dood-free-address(dood)
    := dood-number-pages(dood, $dood-header-size) * dood-page-size(dood);
  dood-world-register-dood(dood-world(dood), dood);
  boot-predefines(dood);
end method;

define method dood-flush (dood :: <dood>) => (dood :: <dood>)
  dood-flush-state(dood);
  boot-predefines(dood);
  dood-free-address(dood) := untag(dood-read-at(dood, $dood-free-address-id));
  dood-root(dood) := #f; // INITIAL VALUE FOR THOSE THAT CARE
  dood-root(dood) := read-object-at(dood, $dood-root-id);
  dood-proxies(dood)
    := as(<stretchy-vector>, read-object-at(dood, $dood-proxies-id));
  dood-world-register-dood(dood-world(dood), dood);
  dood
end method;

define class <dood-opening-warning> (<dood-warning>)
  constant slot dood-failed-dood :: <dood>, required-init-keyword: dood:;
end class;

define class <dood-corruption-warning> (<dood-opening-warning>)
end class;

define class <dood-version-warning> (<dood-opening-warning>)
end class;

define class <dood-user-version-warning> (<dood-version-warning>)
end class;

define method make-dood-stream (#rest all-keys, #key locator, if-exists, direction)
  apply(make, <dood-stream>, all-keys)
end method;

define method initialize
    (dood :: <dood>, #rest all-keys, 
     #key name, locator, if-exists, stream, backups?, segments, #all-keys)
  next-method();
  dood-init-keys(dood) := copy-sequence(all-keys);
  unless (segments)
    dood-segments(dood) 
      := add(dood-segments(dood), dood-default-segment(dood));
  end unless;
  for (segment in dood-segments(dood), i :: <integer> from 0)
    dood-segment-id(segment) := i;
  end for;
  when (locator)
    dood-locator(dood) := as(<file-locator>, locator);
    unless (name)
      dood-name(dood) := as(<symbol>, as(<string>, locator));
    end unless;
  end when;
  dood-dood-state(dood-state(dood))   := dood;
  dood-dood-state(dood-backup(dood))  := dood;
  let replaceable? = /* stream | */ if-exists == #"replace";
  if (stream)
    dood-stream(dood)       := dood-open-stream(dood, locator: stream);
    dood-given-stream(dood) := stream;
    dood-backups?(dood)     := #f;
  elseif (~(replaceable? & ~file-exists?(dood-locator(dood))))
    // dont create file until after first commit
    dood-stream(dood) := dood-open-stream(dood);
  end if;
  if (replaceable?
        | ~dood-booted?(dood)
        |  dood-corrupted?(dood) 
        |  dood-outdated?(dood) 
        |  dood-user-outdated?(dood))
    unless (replaceable? | ~dood-booted?(dood))
      case
	dood-outdated?(dood)
	  => signal(make(<dood-version-warning>,
			 dood:             dood,
			 format-string:    "DOOD %= OUTDATED VERSION %= EXPECTED %=", 
			 format-arguments: vector(dood-name(dood) | "",
						  dood-version(dood), 
						  $dood-version)));
	dood-user-outdated?(dood)
	  => signal(make(<dood-user-version-warning>,
			 dood:             dood,
			 format-string:    "DOOD %= OUTDATED USER-VERSION %= EXPECTED %=", 
			 format-arguments: vector(dood-name(dood) | "",
						  dood-user-version(dood), 
						  dood-specified-user-version(dood))));
	dood-corrupted?(dood)
	  => signal(make(<dood-corruption-warning>, 
			 dood:             dood,
			 format-string:    "DOOD %= CORRUPTED", 
			 format-arguments: vector(dood-name(dood) | "")));
      end case;
    end unless;
    dood-boot(dood);
  else
    dood-flush(dood);
  end if;
end method;

define method dood-close (dood :: <dood>, #rest all-keys, #key abort?) => ()
  // unless (abort?)
  //   dood-commit(dood);
  // end unless; 
  apply(dood-close-state-stream, dood, dood-state(dood), all-keys);
  dood-close-state-stream(dood, dood-backup(dood), abort?: #f);
  dood-flush-state(dood);
  dood-clean-proxies(dood);
  dood-world-unregister-dood(dood-world(dood), dood);
end method;

/// BOOKKEEPING

define abstract open primary dood-class <dood-mapped-object> (<object>)
  // weak slot dood-pointer :: false-or(<address>) = #f, 
  //   // HACK: WONT WORK FOR NON-BATCH-MODE CAUSE IT WILL UNDO ATTACHMENT
  //   // reinit-expression: #f,
  //   init-keyword: pointer:;
end dood-class;

// define method dood-address 
//     (dood :: <dood>, object :: <dood-mapped-object>) 
//  => (address :: false-or(<address>))
//   let pointer = dood-pointer(object);
//   pointer & untag(pointer)
// end method;
// 
// define method dood-address-setter
//     (address :: <address>, dood :: <dood>, object :: <dood-mapped-object>) 
//  => (address :: <address>)
//   dood-pointer(object) := tag-as-address(address);
// end method;
// 
// define method dood-unregister-address 
//     (dood :: <dood>, object :: <dood-mapped-object>)
//   dood-pointer(object) := #f
// end method;

define abstract open primary dood-class
    <dood-mapped-and-owned-object> (<dood-mapped-object>)
  // keyword slot dood:;
  weak slot object-dood-state :: false-or(<dood-state>) = #f, 
    // HACK: WONT WORK FOR NON-BATCH-MODE CAUSE IT WILL UNDO ATTACHMENT
    // reinit-expression: #f,
    init-keyword: dood-state:;
end dood-class;

define method initialize
    (x :: <dood-mapped-and-owned-object>, #key dood, #all-keys)
  next-method();
  when (dood)
    object-dood-state(x) := dood-state(dood);
  end when;
end method;

define inline method object-dood 
    (x :: <dood-mapped-and-owned-object>) => (res :: false-or(<dood>))
  let state = object-dood-state(x);
  state & dood-dood-state(state)
end method;

define inline method object-dood-setter 
    (nv :: false-or(<dood>), x :: <dood-mapped-and-owned-object>) 
 => (res :: false-or(<dood>))
  object-dood-state(x) := nv & dood-state(nv);
  nv
end method;

define method dood-register-object-dood 
    (dood :: <dood>, object :: <dood-mapped-and-owned-object>) 
  object-dood(object) := dood;
end method;

/*
define method dood-unregister-object-dood
    (dood :: <dood>, object :: <dood-mapped-and-owned-object>)
  object-dood(object) := #f;
end method;
*/

define open generic object-dood
    (object /*, #key world */) => (dood :: false-or(<dood>));

/*
define method dood-object-dood
    (dood :: <dood>, object) => (dood :: false-or(<dood>))
  object-dood(object /*, world: dood-world(dood)*/)
end method;
*/

define method object-dood 
    (object /*, #key world = dood-world-default() */)
 => (dood :: false-or(<dood>))
  #f // dood-world-object-dood(world, object)
end method;

define method dood-register-object-dood (dood :: <dood>, object)
  // dood-world-register-object-dood(dood-world(dood), dood, object)
end method;

/*
define method dood-unregister-object-dood (dood :: <dood>, object)
  // dood-world-unregister-object-dood(dood-world(dood), dood, object);
end method;
*/

// define constant $default-two-level-table-size = 10;

// define inline function make-two-level-table
//     (#key size = $default-two-level-table-size) => (res :: <dood-table>)
//   make(<dood-table>, size: size)
// end function;

define inline function first-level-table
    (weak-kind :: false-or(<symbol>), tables :: <dood-table>, key) 
 => (table :: <dood-table>)
  element(tables, key, default: #f)
    | (element(tables, key) := make-weak-table(weak-kind));
end function;

define inline function two-level-table-element
    (weak-kind :: false-or(<symbol>), tables :: <dood-table>, key, 
     first-key :: <function>, second-key :: <function>, default)
 => (value)
  let table :: <dood-table>
    = first-level-table(weak-kind, tables, first-key(key));
  element(table, second-key(key), default: default)
end function;

define inline function two-level-table-element-setter
    (value, weak-kind :: false-or(<symbol>), tables :: <dood-table>, key,
     first-key :: <function>, second-key :: <function>)
 => (value)
  let table :: <dood-table>
    = first-level-table(weak-kind, tables, first-key(key));
  element(table, second-key(key)) := value;
end function;

/*
define inline function two-level-table-remove-key!
    (tables :: <dood-table>, key, 
     first-key :: <function>, second-key :: <function>)
 => ()
  let key-1 = first-key(key);
  let table :: false-or(<dood-table>)
    = element(tables, key-1, default: #f);
  if (table)
    remove-key!(table, second-key(key));
    if (empty?(table))
      remove-key!(tables, key-1);
    end if;
  end if;
end function;
*/

define method dood-address 
    (dood :: <dood>, object) => (address :: false-or(<address>))
  // element(dood-addresses(dood), object, default: #f)
  two-level-table-element
    (#"key", dood-addresses(dood), object, object-class, identity, #f);
end method;

define method dood-address-setter
    (address :: <address>, dood :: <dood>, object) => (address :: <address>)
  // element(dood-addresses(dood), object) := address
  two-level-table-element
    (#"key", dood-addresses(dood), object, object-class, identity) := address;
end method;

define method dood-register-address 
    (dood :: <dood>, object, address :: <address>)
  dood-address(dood, object) := address
end method;

/*
define method dood-unregister-address (dood :: <dood>, object)
  // remove-key!(dood-addresses(dood), object)
  two-level-table-remove-key!
    (dood-addresses(dood), object, object-class, identity)
end method;
*/

// define constant $dood-address-key-1-size
//   = 10;
// define constant $dood-address-key-1-mask 
//   = ash(1, $dood-address-key-1-size) - 1;
// 
// define function dood-address-key-1 
//     (address :: <address>) => (key :: <address>)
//   ash(address, -$dood-address-key-1-size)
// end function;
// 
// define function dood-address-key-2 
//     (address :: <address>) => (key :: <address>)
//   logand(address, $dood-address-key-1-mask)
// end function;

define inline function dood-object
    (dood :: <dood>, address :: <address>, #rest optionals, #key default) 
 => (object)
  apply(element, dood-objects(dood), address, optionals)
  // two-level-table-element
  //   (#"value", dood-objects(dood), 
  //    address, dood-address-key-1, dood-address-key-2, default)
end function;

define inline function dood-object-setter 
    (object, dood :: <dood>, address :: <address>)
  element(dood-objects(dood), address) := object
  // two-level-table-element
  //     (#"value", dood-objects(dood), 
  //      address, dood-address-key-1, dood-address-key-2)
  //   := object;
end function;

define function dood-register-object-maybe-read
    (dood :: <dood>, object, address :: <address>, read? :: <boolean>)
  if (read?)
    // used for wrapper proxies to avoid circularities
    let forwarding-address = dood-forwarding-address(dood);
    if (forwarding-address)
      // format-out("FORWARDING %= TO %= FOR %=\n", 
      //            address, forwarding-address, object-class(object));
      dood-object(dood, forwarding-address) := object;
      dood-forwarding-address(dood) := #f; // ONE SHOT ONLY
    end if;
  end if;
  // unless (read? & dood-batch-mode?(dood))
    dood-register-object-dood(dood, object);
  // COMMENT THIS OUT FOR FLUSHABILITY
  unless (read? & dood-batch-mode?(dood))
    dood-register-address(dood, object, address);
  end unless;
  dood-object(dood, address) := object;
  dood-format("REGISTERING %= @ %d IN %s\n", 
              object-class(object), address, dood-name(dood));
end function;

define method dood-register-object
    (dood :: <dood>, object, address :: <address>)
  dood-register-object-maybe-read(dood, object, address, #f);
end method;

define method dood-register-read-object
    (dood :: <dood>, object, address :: <address>)
  dood-register-object-maybe-read(dood, object, address, #t);
end method;

/*
define method dood-unregister-object (dood :: <dood>, object)
  if (dood == dood-object-dood(dood, object))
    dood-unregister-object-dood(dood, object)
  end if;
  let address = dood-address(dood, object);
  remove-key!(dood-objects(dood), address);
  // two-level-table-remove-key!
  //   (dood-objects(dood), address, dood-address-key-1, dood-address-key-2);
  // unless (dood-read-only?(dood))
    dood-unregister-address(dood, object);
  // end unless;
end method;
*/

define method dood-booted? (dood :: <dood>) => (well? :: <boolean>)
  let well? = stream-size(dood-stream(dood)) > $dood-header-size;
  dood-format("BOOTED? %=\n", well?);
  well?
end method;

define method dood-corrupted? (dood :: <dood>) => (res :: <boolean>)
  let well? =
  if (dood-booted?(dood))
    untag(dood-read-at(dood, $dood-corruption-id)) ~== $dood-false-id
  else
    #f
  end if;
  dood-format("CORRUPTED? %=\n", well?);
  well?
end method;

define method dood-corrupted?-setter 
    (x :: <boolean>, dood :: <dood>) => (res :: <boolean>)
  dood-write-at
    (dood, 
     tag-as-address(x, if (x) $dood-true-id else $dood-false-id end), 
     $dood-corruption-id);
  x
end method;

define method dood-version (dood :: <dood>) => (res :: <integer>)
  if (dood-booted?(dood))
    untag(dood-read-at(dood, $dood-version-id))
  else
    $dood-no-version
  end if
end method;

define method dood-outdated? (dood :: <dood>) => (res :: <boolean>)
  let version = dood-version(dood);
  let well? =
  version ~= $dood-no-version   // valid version
    & version ~= $dood-version;  // but wrong 
  dood-format("OUTDATED? %=\n", well?);
  well?
end method;

define method dood-user-version (dood :: <dood>) => (res :: <integer>)
  if (dood-booted?(dood))
    let version = untag(dood-read-at(dood, $dood-user-version-id));
    version
  else
    $dood-no-version
  end if
end method;

define method dood-user-outdated? (dood :: <dood>) => (res :: <boolean>)
  let version = dood-user-version(dood);
  let well? =
  version ~= $dood-no-version   // valid version
    & version ~= dood-specified-user-version(dood);  // but wrong 
  dood-format("USER OUTDATED? %=\n", well?);
  well?
end method;

/// FORWARD DECLARED FOR EMULATOR

define class <walk-info> (<object>)
  constant slot walk-info-function :: <function> = identity, 
    init-keyword: function:;
  constant slot walk-info-flush? :: <boolean> = #f, 
    init-keyword: flush?:;
  constant slot walk-info-force? :: <boolean> = #t, 
    init-keyword: force?:;
  constant slot walk-info-parents? :: <boolean> = #f, 
    init-keyword: parents?:;
  constant slot walk-info-commit? :: <boolean> = #f, 
    init-keyword: commit?:;
  constant slot walk-info-batch? :: <boolean> = #t, 
    init-keyword: batch?:;
end class;

define sealed domain initialize (<walk-info>);
define sealed domain make (subclass(<walk-info>));
