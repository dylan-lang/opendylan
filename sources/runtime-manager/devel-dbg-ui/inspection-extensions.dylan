module:     devel-dbg-ui
synopsis:   Methods for producing visual representations of objects in the
            debugger, deferring to default DM functionality where
            necessary.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// **********************************************************************
//    **             Console Debugger Printing Directives                 **
//    ** ---------------------------------------------------------------- **
//    ** Use this to modify the way in which objects of specific classes  **
//    ** are printed by the debugger.                                     **
//    **********************************************************************

/* Use this when the blasted macro works properly

define debugger-print-directives 
  for-objects
    of-class "<application>" from-module "devel-dbg-ui" printing-as
      string "{console debugger target ";
      indirect-slot "debug-target-access-path", "access-path-application";
      string " allocating with ";
      slot-called "runtime-allocator-name";
      string "}"
end debugger-print-directives;
*/

// Temporary hand-expansion of the debugger-print-directives macro:

define constant $debugger-print-directives =
  vector(
    make(<debugger-print-directive>,
         print-class-name: "<application>", 
         print-module-name: "devel-dbg-ui",
         print-instructions:
           vector(
             pair(#"string", "{console debugger target "),
             pair(#"indirect-slot", 
               #["debug-target-access-path", "access-path-application"]),
             pair(#"string", " allocating with "),
             pair(#"slot", "runtime-allocator-name"),
             pair(#"string", "}"))));

///// *CLASS-LOOKUP-TABLE*

define variable *class-lookup-table* :: false-or(<string-table>) = #f;


///// FIND-NAMED-SLOT-IN-OBJECT
//    Given an object and a slot name, try to get the slot value.
//    Returns #f if there is no slot with the given name.

define method find-named-slot-in-object
    (application :: <application>, instance :: <remote-value>,
     slot-name :: <string>)
 => (maybe-value :: false-or(<remote-value>), position :: <integer>)
  let (class-name, slots, slot-values, repeats, repeat-name, repeat-vals)
    = describe-dylan-object(application, instance);
  let maybe-value = #f;
  let position = 1;
  block (exit)
    for (i from 0 below slots.size)
      if (as-lowercase(slots[i]) = as-lowercase(slot-name))
        maybe-value := slot-values[i];
        position := i + 1;
        exit()
      end if
    end for
  end block;
  values(maybe-value, position);
end method;

define method find-named-slot-in-object
    (application :: <application>, instance :: <remote-value>,
     slot-name :: <integer>)
 => (maybe-value :: false-or(<remote-value>), position :: <integer>)
  let (value, ok) =
    read-dylan-value(application,
                     indexed-remote-value(instance, slot-name));
  if (ok)
    values(value, slot-name);
  else
    values(#f, slot-name);
  end if;
end method;


///// FOLLOW-SLOT-NAME-INDIRECTIONS
//    This time a sequence of slot names is supplied, and we use
//    FIND-NAMED-SLOT-IN-OBJECT multiple times, descending the data
//    structure.

define method follow-slot-name-indirections
    (application :: <application>, instance :: <remote-value>,
     slot-names :: <sequence>)
 => (maybe-value :: false-or(<remote-value>), failed-on :: false-or(<string>))
  let current-object = instance;
  let failed-on = #f;
  block (exit)
    let i = 0;
    for (slot-name in slot-names)
      let (new-object, position) =
        find-named-slot-in-object(application, current-object, slot-name);
      unless (new-object)
        failed-on := slot-name;
        exit()
      end unless;
      current-object := new-object;
      slot-names[i] := position;
      i := i + 1;
    end for
  end block;
  values(current-object, failed-on); // Should be impossible for both to be #f
end method;


///// BUILD-CLASS-LOOKUP-TABLE
//    The macro DEFINE DEBUGGER-PRINT-DIRECTIVES produces a data structure
//    that we cannot efficiently do lookups on. This function transforms
//    it into a more appropriate tabular structure.

define method add-inspection-extension
    (class-descriptor :: <debugger-print-directive>) => ()
  let class-name = class-descriptor.print-directive-class;
  let module-name = class-descriptor.print-directive-module;
  let instructions = class-descriptor.print-directive-instructions;
  let subtable = element(*class-lookup-table*, module-name, default: #f);
  if (subtable)
    subtable[class-name] := instructions;
  else
    subtable := make(<string-table>);
    *class-lookup-table*[module-name] := subtable;
    subtable[class-name] := instructions;
  end if;
end method;

define method build-class-lookup-table () => ()
  *class-lookup-table* := make(<string-table>);
  for (class-descriptor in $debugger-print-directives)
    add-inspection-extension(class-descriptor);
  end for;
end method;


///// DEBUGGER-PRINT-GENERAL-DYLAN-OBJECT
//    The function used by the console debugger to print dylan objects that
//    have not been treated specially by the DM.

define method debugger-print-general-dylan-object
    (application :: <application>, instance :: <remote-value>,
     class-name :: <string>, class-module :: <string>,
     #key length = 5, level = 3, decorate? = #t)
 => (representation :: <string>)

  local method apply-directive (directive :: <pair>) => (result :: <string>)
          let result = "";
          select(directive.head)
	    #"string" =>
	      result := directive.tail;      // Simplest case!

	    #"slot" =>
	      let (subval, position) = 
		find-named-slot-in-object
                  (application, instance, directive.tail);
              if (subval)
                directive.tail := position;  // Optimize to speed up future
                                             // lookups.
                result := debugger-print-object
                            (application, 
                             subval,
                             length: length,
                             level: level,
                             decorate?: decorate?);
              else
                result := 
                  format-to-string(" {no such slot %s} ", directive.tail);
              end if;

	    #"indirect-slot" =>
	      let (subval, noslot) = 
		follow-slot-name-indirections
                  (application, instance, directive.tail);
              if (subval)
                result := debugger-print-object
                            (application, 
                             subval,
                             length: length,
                             level: level,
                             decorate?: decorate?);
              else
                result := 
                  format-to-string(" {no such slot %s} ", noslot);
              end if;

	  end select;
          result
	end method;

  // Build the lookup table if we haven't yet done so.
  unless (*class-lookup-table*)
    build-class-lookup-table()
  end unless;

  let subtable = element(*class-lookup-table*, class-module, default: #f);
  if (subtable)
    let instructions = element(subtable, class-name, default: #f);
    if (instructions)
      let components = map(apply-directive, instructions);
      apply(concatenate, "", components);
    else
      print-dylan-object(application, instance,
                         length: length, level: level, decorate?: #t);
    end if;
  else
    print-dylan-object(application, instance,
                       length: length, level: level, decorate?: #t);
  end if;
end method;


///// DEBUGGER-PRINT-OBJECT
//    The function used by the console debugger to generate the printable
//    representation of an object.

define method debugger-print-object
    (application :: <application>, instance :: <remote-value>,
     #key length = 5, level = 3, decorate? = #t)
  => (representation :: <string>)

  let representation = "";
  let path = application.debug-target-access-path;

  block ()
    let classification = classify-runtime-value(application, instance);
    select (classification)
      #"dylan-general-object" =>
	let (class-instance, immediate?) =
	  dylan-object-class(application, instance);
	let (class-name, class-context, precise?, constant?) =
	  find-dylan-name(application, class-instance);
	let class-module = class-context.context-module;
	class-name := as-lowercase(class-name);
	class-module := as-lowercase(class-module);
	representation :=
	  debugger-print-general-dylan-object
	    (application, instance, class-name, class-module,
             length: length, level: level,
             decorate?: #t);

      #"foreign-object" =>
	representation :=
	  format-to-string("0x%s",
            remote-value-as-string(path, instance, 16));

      otherwise =>
	representation := 
	  print-dylan-object(application, instance, length: length,
                             level: level, decorate?: decorate?);
    end select;
  exception (<remote-access-violation-error>)
    representation := "{Failed to print object: memory violation occurred}"
  exception (<error>)
    representation := "{Failed to print object: internal error}"
  end block;

  representation;
end method;
