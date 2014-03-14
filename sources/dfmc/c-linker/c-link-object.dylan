Module: dfmc-c-linker
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// OBJECT EMISSION PROTOCOL

define generic emit-forward    // binding declaration
  (back-end, stream, object) => (); 
define generic emit-definition // binding
  (back-end, stream, object) => (); 

/// VARIABLES

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <module-binding>) => ()
  format-emit*(back-end, stream, "extern ~ @;\n", $dylan-type-string, o);
end method;

define method emit-definition 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <module-binding>) => ()
  format-emit*(back-end, stream, "~ %;\n", $dylan-type-string, o);
end method;

// CODE

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&kernel-ep>) => ()
end method;

define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&kernel-ep>) => ()
end method;

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&mep>) => ()
end method;

define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&mep>) => ()
end method;

//// GENERIC OBJECT EMISSION

// FORWARD DECLARATIONS

define method emit-forward  // !@#$ NEED UNIFYING TYPE
    (back-end :: <c-back-end>, stream :: <stream>, o) => ()
  unless (o.direct-object?)
    if (o.model-definition 
        | instance?(o, <&mm-wrapper>)
        | instance?(o, <&singular-terminal-engine-node>))
      write(stream, "extern ");
    else
      write(stream, "static ");
    end;
    emit-type-name(back-end, stream, o);
    format-emit*(back-end, stream, " ^;\n", o);
  end unless;
end method;

// DEFINITIONS

define method emit-definition // !@#$ need unifying type
    (back-end :: <c-back-end>, stream :: <stream>, o :: <object>) => ()
  // Direct objects are always emitted in full at point of reference and
  // are never referred to by name, hence no need for a forward declaration.
  unless (o.direct-object?)
    unless (o.model-definition 
            | instance?(o, <&mm-wrapper>)
            | instance?(o, <&singular-terminal-engine-node>)) 
      write(stream, "static ");
    end;
    emit-type-name(back-end, stream, o);
    format-emit*(back-end, stream, " ^ = %;\n\n", o, o);
  end unless;
end method;

// INDIRECTION DEFINITIONS

define method emit-indirection-definition
    (back-end :: <c-back-end>, stream :: <stream>, o :: <object>) => ()
  format-emit*
    (back-end, stream, "static ~ ~^ = @;\n", 
     $dylan-type-string, $indirection-prefix, o, o);
end method;

// STRUCTURE

// Indirect objects are just dumped slot-by-slot directed my MOP 
// information. Packed representations like characters in strings
// obviously cause wrinkles. 

// Issue: Although this is sweet, it may also be slow if we use the 
// generic code for common objects like methods and slot descriptors.
// We will have to profile, but there may be a case for the model 
// class macros generating custom emitters for common classes.

define generic emit-object-slot
    (back-end :: <c-back-end>, stream, class, slotd, object) => ();

define sideways method emit-object // !@#$ NEED UNIFYING TYPE
    (back-end :: <c-back-end>, stream :: <stream>, o :: <object>) => ()
  let class = &object-class(o);
  let wrapper = ^class-mm-wrapper(class);
  write(stream, "{\n  ");
  emit-reference(back-end, stream, wrapper);
  write(stream, " /* wrapper */");
  for (slotd in ^instance-slot-descriptors(class))
    emit-object-slot(back-end, stream, class, slotd, o);
  end;
  let rpt = ^repeated-slot-descriptor(class);
  when (rpt)
    emit-object-slot(back-end, stream, class, rpt, o);
  end when;
  write(stream, "\n}");
end method;


define method emit-object-slot
    (back-end :: <c-back-end>, stream :: <stream>, 
     class, slotd :: <&any-instance-slot-descriptor>, o) => ()
  write(stream, ",\n  ");
  emit-reference(back-end, stream, ^slot-value(o, slotd));
  // write(stream, " /* ");
  // emit-struct-field-name(back-end, stream, class, slotd, 0);
  // write(stream, " */");
end method;

define constant $delete-character = as(<character>, 127);

define method graphic? (character :: <character>)
  let code :: <integer> = as(<integer>, character);
  code >= as(<integer>, ' ') & code < as(<integer>, $delete-character)
end method graphic?;

define method emit-raw-character-data
    (back-end :: <c-back-end>, stream :: <stream>, c :: <byte-character>)
 => ()
  select (c)
    '\\' => write(stream, "\\\\");
    '\"' => write(stream, "\\\"");
    '\'' => write(stream, "\\'");
    '\n' => write(stream, "\\n");
    '\f' => write(stream, "\\f");
    '\t' => write(stream, "\\t");
    '\r' => write(stream, "\\r");
    otherwise =>
      if (c.graphic?)
        write-element(stream, c);
      else
        format(stream, "\\x%x", as(<integer>, c));
      end if;
  end select
end method;

define method emit-object-slot
    (back-end :: <c-back-end>, stream :: <stream>, 
     class, slotd :: <&repeated-slot-descriptor>, o) => ()
  let size = ^slot-value(o, ^size-slot-descriptor(slotd));
  /*  ---- This is no longer needed.  The size descriptor slot is represented
           in the instnace-slot-descriptors explicitly now.
  emit-object-slot(back-end, stream, class, ^size-slot-descriptor(slotd), o);
  */
  if (slotd.^slot-type == dylan-value(#"<byte-character>"))
    write(stream, ",\n  \"");
    for (i from 0 below size)
      emit-raw-character-data
        (back-end, stream, ^repeated-slot-value(o, slotd, i));
    end;
    write(stream, "\"");
  else
    for (i from 0 below size)
      write(stream, ",\n  ");
      emit-reference(back-end, stream, ^repeated-slot-value(o, slotd, i));
      // write(stream, " /* ");
      // emit-struct-field-name(back-end, stream, class, slotd, i);
      // format(stream, "[%d] */", i);
    end;
  end if;
end method;

// Any other kind of slot descriptor has no presence in an instance.
// This covers virtual, class, and each-subclass slots.

/*
--- This shouldn't be needed anymore, we shouldn't be getting called
--- on non-instance slots.

define method emit-object-slot
    (back-end :: <c-back-end>, stream :: <stream>, 
     class, slotd :: <&slot-descriptor>, o) => ()
end method;
*/


// CLASSES

// If the class has a repeated slot, rather than dumping the struct 
// itself we dump a struct constructor macro. 

define method emit-slot-definition-using-type-name 
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     type-name :: <string>, 
     o :: <&class>, slotd :: <&slot-descriptor>, offset :: <integer>) 
 => ()
  write(stream, prefix-string);
  print-message(type-name, stream);
  write-element(stream, ' ');
  emit-struct-field-name(current-back-end(), stream, o, slotd, offset);
  write(stream, suffix-string);
end method;

define method emit-slot-definition
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     o :: <&class>, slotd :: <&repeated-slot-descriptor>, offset :: <integer>) 
 => ()
  emit-slot-definition-using-type-name
    (stream, prefix-string, suffix-string, 
     slotd.^slot-type.c-repeated-type-name, o, slotd, offset);
end method;

define method emit-slot-definition
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     o :: <&class>, slotd :: <&slot-descriptor>, offset :: <integer>) 
 => ()
  emit-slot-definition-using-type-name
    (stream, prefix-string, suffix-string, 
     slotd.^slot-type.c-type-name, o, slotd, offset);
end method;

define method emit-typedef
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&class>) => ()
  ^ensure-slots-initialized(o);
  let rslotd = o.^repeated-slot-descriptor; 
  let islots = o.^instance-slot-descriptors;
  if (~rslotd)
    write(stream, "typedef struct {\n");
    format(stream, "  %s wrapper;\n", model-<mm-wrapper>().c-type-name);
    for (slotd in islots, i from 0)
      emit-slot-definition(stream, "  ", ";\n", o, slotd, i);
    end;
    write(stream, "} ");
    emit-struct-name(back-end, stream, o);
    write(stream, ";\n");
  else
    write(stream, "#define  "); 
    emit-struct-definer-name(back-end, stream, o);
    write(stream, "(nrepeated) \\\n");
    write(stream, "  typedef struct { \\\n");
    format(stream, "    %s wrapper; \\\n", model-<mm-wrapper>().c-type-name);
    for (slotd in islots, i from 0)
      emit-slot-definition(stream, "    ", "; \\\n", o, slotd, i);
    end;
    emit-slot-definition
      (stream, "    ", "[nrepeated+1]; \\\n", o, rslotd, islots.size);
    write(stream, "  } ");
    emit-struct-name(back-end, stream, o);
    write(stream, "_##nrepeated;\n");
  end;
end method;

// VARIABLES

define sideways method emit-object 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <module-binding>)
 => ()
  format-emit*(back-end, stream, "@", o);
  let maybe-value = binding-model-or-hollow-object(o, default: unfound());
  let value       = if (found?(maybe-value)) maybe-value else &unbound end;
  format-emit*(back-end, stream, " = @", value);
end method;

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>) => ()
  emit-lambda-interface(back-end, stream, o);
  write(stream, ";\n");
end method;

define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>) => ()
  format(stream, "%s\n", o.code);
end method;
