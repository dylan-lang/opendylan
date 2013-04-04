Module:   dfmc-conditions-implementation
Author:   haahr, jonathan, keith, swm
Synopsis: The program conditions protocol.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// signaling conditions

// The primary program condition signaling interface is note, which calls
// make on the condition class and signals it, possibly returning.  It can
// be used for any program condition, but is mainly oriented towards
// <program-note>s.
//
// Raise is analogous to the Dylan module's error function and is guaranteed
// not to return.

define open generic note
    (class :: subclass(<program-condition>), #key, #all-keys) => ();

define open generic raise
    (class :: subclass(<program-condition>), #key, #all-keys) => ();

define open generic restart
    (class :: subclass(<program-restart>), #key, #all-keys) => ();


// default methods

define method note
    (class :: subclass(<program-condition>), #rest inits, #key, #all-keys)
 => ()
  signal(apply(make, class, inits));
  values()
end method note;

define method raise
    (class :: subclass(<program-error>), #rest inits, #key, #all-keys) => ()
  error(apply(make, class, inits));
end method raise;

define method restart
    (class :: subclass(<program-restart>), #rest inits, #key, #all-keys) => ()
  let condition = apply(make, class, inits);
  signal(condition);
  error("restart %= returned", condition);
end method restart;


// We define simplified versions of the above functions for those cases where
// we just want to set the format string and arguments.  Hopefully most notes
// will eventually want to provide more information than this, and these 
// functions will become redundant.

define method simple-note
    (class :: subclass(<program-note>), 
     format-string :: <string>,  #rest args) => ()
  signal(make(class, format-string: format-string, format-arguments: args));
  values()
end method simple-note;

define method simple-raise
    (class :: subclass(<program-error>), 
     format-string :: <string>,  #rest args) => ()
  error(make(class, format-string: format-string, format-arguments: args));
end method simple-raise;


/// properties of program conditions

// serious-note?
//
// True if this note is serious -- that is, requires terminating the
// current processing and picking a restart.  The default behavior
// is that notes are not serious, but the policy object should allow
// upgrading them, with options like ``all warnings are errors'' for
// making <program-warning>s serious, or ``strict Dylan'' for making
// <portability-note>s serious.
//
// Errors are always serious, by definition, because the compiler
// can't just skip them.  Restarts are always serious, as much as such
// a definition make sense for them.

define generic serious-note? (note :: <program-note>)
 => (serious? :: <boolean>);

define method serious-note? (note :: <program-note>) => (serious? :: <boolean>)
  #f // until we have a policy object
end method serious-note?;

define method serious-note? (note :: <program-error>)
 => (serious? :: <boolean>)
  #t // until we have a policy object
end method serious-note?;

define method serious-note? (note :: <serious-program-warning>)
 => (serious? :: <boolean>)
  #t // until we have a policy object
end method serious-note?;


// interesting-note?
//
// True if the note is interesting to the user, according to the
// yet-to-be-defined compiler policy object.  Uninteresting conditions
// are suppressed, either by not printing messages for them or not
// logging them at all.  Because all errors and restarts are ``serious,''
// they are also interesting.

define generic interesting-note? (note :: <program-note>)
 => (interesting? :: <boolean>);

define method interesting-note? (note :: <program-note>)
 => (interesting? :: <boolean>)
  #t // until we have a policy object
end method interesting-note?;

define method interesting-note? (note :: <performance-note>)
 => (interesting? :: <boolean>)
  #f // until we have a policy object
end method interesting-note?;

// obsolete-condition?
//
// True if this condition is obsolete, which can happen because the
// conditions which were checked when it was created have changed.
//
// Conditions have to explicitly handle making themselves obsolete.
// The default behavior is that conditions are never obsolete, but
// redefining the obsolete-condition? method is possible.
//
// Most conditions should probably have obsolete-condition? methods
// if program conditions persist longer than a single compilation.

define open generic obsolete-condition? (condition :: <program-condition>)
 => (obsolete? :: <boolean>);

define method obsolete-condition? (condition :: <program-condition>)
 => (obsolete? :: singleton(#f));
  #f
end method obsolete-condition?;


/// preserving conditions
 
// All signaled program errors and notes are stored in a condition
// table held in the associated compilation record.  It is the 
// responsibility of the condition presentation functions to add them.
//
// Restarts are not entered into the table.  It is assumed that all
// conditions in the table have already been signaled.

define compiler-open generic library-conditions-table
  (library) => (table :: <table>);

define thread variable *subnotes-queue* = #f;

// Don't store runtime errors in the database, since don't have enough
// information to manage them under incremental compilation.
// TODO: could wrap them in a subclass of <program-condition> of some kind
// and store that...
define method add-program-condition (condition :: <condition>) => ()
end method;

define method add-program-condition (condition :: <program-condition>) => ()
  convert-condition-slots-to-ppml(condition);
  if ( *subnotes-queue* )
    push-last(*subnotes-queue*, condition)
  else
    let lib = current-library-description();
    if (lib)
      // TODO: should use what's stored in condition-program-note-creator, not
      // the current dependent, in case out of context now.
      let current-dependent = *current-dependent*;
      debug-assert(current-dependent); // dependent always set up if lib ~== #f
      let creator = (current-dependent ~== $no-dependent) & current-dependent;
      // Make sure this matches, for now.  See TODO above.
      condition-program-note-creator(condition) := creator;
      let table = lib.library-conditions-table;
      let q = element(table, creator, default: $unfound);
      if (found?(q))
        push-last(q, condition)
      else
        let q = make(<deque>);
        push-last(q, condition);
        table[creator] := q;
      end
    end
  end;
end method add-program-condition;


define method remove-program-conditions-from!(table, key, stages) => ()
  local method matching-condition?(cond, stages)
	  ~instance?(cond, <program-condition>) |
	    dependency-stage-match?(condition-compilation-stage(cond), stages)
	end;

  let q = element(table, key, default: $unfound);

  if (found?(q))
    remove!(q, stages, test: matching-condition?);
    if (empty?(q)) remove-key!(table, key) end;
  end;
  values()
end;





