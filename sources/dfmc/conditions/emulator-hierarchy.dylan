Synopsis: The predefined program condition classes.
Author:   haahr, jonathan, keith, swm
Module:   dfmc-conditions-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic convert-condition-slots-to-ppml(condition :: <condition>) => ();

define method convert-condition-slots-to-ppml(condition :: <condition>) => ();
end;

define method convert-condition-slots-to-ppml
    (condition :: <simple-error>) => ();
  for (i from 0 below condition.condition-format-arguments.size)
    condition.condition-format-arguments[i] :=
      as(<ppml>, condition.condition-format-arguments[i])
  end;
end;

define method convert-condition-slots-to-ppml
    (condition :: <simple-warning>) => ();
  for (i from 0 below condition.condition-format-arguments.size)
    condition.condition-format-arguments[i] :=
      as(<ppml>, condition.condition-format-arguments[i])
  end;
end;


/// <program-condition>

// The root of the hierarchy is <program-condition>.  All errors,
// warnings, etc, about code in a program being compiled should be
// reported as instances of this class.
//
//  This class should only be used for type declarations and as the
// superclass for mixin properties.  For instantiable classes, it's
// best to subclass one of <program-error>, <program-note>, or
// <program-restart> instead.

define open abstract class <program-condition> (<condition>)

  // TODO: Inherit from <format-string-condition> when that exists.
  // TODO: Make this class primary.

  // The source-location is used for indicating to the user where the
  // error occured.  It might be sensible for this information to be
  // obtained from a combination of the source-record and this slot,
  // if that could condense the size of source locations.
  slot condition-source-location = #f,
    init-keyword: source-location:;

  slot condition-program-note-creator = *current-dependent*,
    init-keyword: program-note-creator:;
  slot condition-compilation-stage = *current-stage*,
    init-keyword: compilation-stage:;

end class <program-condition>;


/// The independently subclassable roots.

// A <program-note> is any message that the compiler wants to report,
// including but not limited to errors and warnings.

define constant <program-note-filter> = false-or(<function>);

define open primary abstract program-condition <program-note>
    (<simple-warning>, <program-condition>)

  // TODO: inherit from <warning>, when <format-string-condition> exists
  
  // This slot can be initialized with some indication of the logical
  // context of the source the note is about, typically to give a concise
  // textual hint. Allowing for example:
  // 
  //   foo.dylan:180:Warning in process-foo: Bogus call to bar.
  //
  // where "process-foo" is the context.
  slot condition-context-id = #f,
    init-keyword: context-id:;

  // Notes may have subnotes, allowing hierarchical explanations to be
  // constructed.
  slot subnotes :: limited(<sequence>, of: <program-note>) = #[],
    init-keyword: subnotes:;
  filter #f;
end program-condition <program-note>;

define program-condition-definer program-note;


// Some common program note predicates

define function program-note-in(form :: <string>) => (pred :: <function>)
  method (condition :: <condition>) => (b :: <boolean>)
    instance?(condition, <program-note>) 
  & form = condition.condition-context-id
  end
end;

define function program-note-location-between
    (from :: <integer>, to :: <integer>) => (pred :: <function>)
  method (condition :: <condition>) => (b :: <boolean>) 
    if (instance?(condition, <program-note>))
      let loc = condition.condition-source-location; 
      if (loc) 
        let sr = loc.source-location-source-record;
        let (file-name, header-offset) = source-line-location(sr, 0);
        ignore(file-name);
        let start-minus-header = source-offset-line(
                                   source-location-start-offset(loc));
        let start-line = start-minus-header + header-offset;

        start-line >= from & start-line <= to
      else 
        #f
      end
    else
      #f
    end
  end
end;

define function program-note-class-=
    (class :: subclass(<condition>)) => (pred :: <function>)
  method (condition :: <condition>) => (b :: <boolean>)
    instance?(condition, class)
  end
end;

define function program-note-file-name-=
    (file-name :: <string>) => (pred :: <function>)
  method (condition :: <condition>) => (b :: <boolean>)
    let loc = condition.condition-source-location;
    if (loc)
      let sr = loc.source-location-source-record;
      file-name = source-line-location(sr, 0)
    end
  end
end;


// Some common program note filters

define generic add-program-condition (condition :: <condition>) => ();
define generic terminate-and-restart (condition :: <condition>) => ();
define open generic report-condition (condition :: <condition>) => ();

define method make-program-note-filter
  (#key file-name :: <string> = "",
        from :: <integer> = 0, to :: <integer> = $maximum-integer,
        in :: <string> = "",
        class :: subclass(<condition>) = <condition>,
        action :: <function> = add-program-condition) 
    => (filter :: <program-note-filter>);
  let predicates = make(<deque>);
  if (file-name ~= "") 
    add!(predicates, program-note-file-name-=(file-name))
  end if;
  if (in ~= "") 
    add!(predicates, program-note-in(in))
  end if;
  if (from ~= 0 | to ~= $maximum-integer)
    add!(predicates, program-note-location-between(from, to))
  end if;
  if (class ~== <condition>)
    add!(predicates, program-note-class-=(class))
  end if;
  
  if (empty?(predicates)) action
  else
    method (c :: <condition>) => () 
      if (apply(conjoin, predicates)(c)) action(c) end; 
      values () 
    end
  end
end;

define constant $record-program-note = add-program-condition;

define function $signal-program-error(c :: <condition>) => ()
  add-program-condition(c);
  terminate-and-restart(c) 
end;

define function $signal-program-note(c :: <condition>) => ()
  add-program-condition(c);
  report-condition(c) 
end;


// Each program note class has a filter associated with it.

define open generic program-note-filter
  (class :: subclass(<condition>)) 
    => (filter :: <program-note-filter>);

define open generic program-note-filter-setter
  (filter :: <program-note-filter>, 
   class :: subclass(<program-condition>))
    => (filter :: <program-note-filter>);

define method program-note-filter (class :: subclass(<condition>))
    => (filter :: <program-note-filter>);
  ignore(class);
  #f
end;

define method program-note-filter-setter
    (filter :: <program-note-filter>, 
     c :: subclass(<program-condition>))
        => (filter :: <program-note-filter>);
  for (sc in c.direct-subclasses) 
    program-note-filter(sc) := filter 
  end;
  filter
end;


// A <program-restart> is a <restart> meant to be used as part of the
// recovery protocol for some <program-condition>.

define open primary abstract program-condition <program-restart>
    (<program-condition>, <simple-restart>)
  // TODO: inherit from <restart>, when <format-string-condition> exists
  slot condition-format-string, init-keyword: format-string:;
  slot condition-format-arguments, init-keyword: format-arguments:;
  keyword default?: = #f;
end program-condition <program-restart>;

define program-condition-definer program-restart;

/// a bit more of a hierarchy

// A <program-warning> is a note about something that might be a
// mistake in program, but the compiler is able to compile it without
// intervention.

define open abstract program-note <program-warning>
  filter $signal-program-note;
end program-condition <program-warning>;

define program-condition-definer program-warning;

define open abstract program-warning <serious-program-warning>
  filter $signal-program-note;
end program-warning <serious-program-warning>;

define program-condition-definer serious-program-warning;

// A <program-error> is a language error.  Examples would be (most)
// syntax errors, inconsistent direct superclasses, or a reference to
// an undefined name.
// gts,98apr06 -- short term hack -- instead of having <program-error> be 
//   interesting in its own right, just stick it under serious-program-warning 
//   for now.

// define open abstract program-note <program-error>
define open abstract serious-program-warning <program-error>
  // filter $signal-program-error;
end program-note <program-error>;

define program-condition-definer program-error;


// Run-time-error warnings are given when the compiler can prove that
// executing the code will lead definitely lead to a run-time error,
// whether or not that error is handled.  These warnings should be
// hard for the user to suppress.  It should be possible for a user to
// treat these warnings as errors;  that is, stop the compilation
// process because of one.

define open abstract program-warning <run-time-error-warning>
  filter $signal-program-note;
end program-warning <run-time-error-warning>;

define program-condition-definer run-time-error-warning;


// Style warnings are given when the compiler detects code in a style
// that is legal (strictly speaking), but not desirable.  The display
// of style warnings can be inhibited globally, or on a class-by-class
// basis.

define open abstract program-warning <style-warning>
  filter #f;
end program-warning <style-warning>;

define program-condition-definer style-warning;


// Performance notes are given when the compiler is prevented from
// doing an optimization that should be reasonable or expected in the
// current context.  Typical reasons would be that it has insufficient
// type, sealing, or program flow information.

define open abstract program-note <performance-note>
  filter #f;
end program-note <performance-note>;

define program-condition-definer performance-note;


// Portability notes are given when the compiler detects something
// that is valid in the DylanWorks compiler, but is not part of
// portable Dylan or could have undefined effects in Dylan.
// 
// It should be possible to turn these warnings into errors, to
// support a standards-conforming version of the compiler.

define open abstract program-note <portability-note>
  filter #f;
end program-note <portability-note>;

define program-condition-definer portability-note;

//// EMULATOR HACK: Override the default warning handler for program warnings
//// since we can't stop it being called in the emulator, and we handle 
//// and report them ourselves.

define method default-handler (c :: <program-warning>) end;

// eof
