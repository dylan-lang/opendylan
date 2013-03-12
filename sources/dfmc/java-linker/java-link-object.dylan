Module: dfmc-java-linker
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// OBJECT EMISSION PROTOCOL
/*
define generic emit-forward    // binding declaration
  (back-end, stream, object) => (); 
define generic emit-definition // binding
  (back-end, stream, object) => (); 

/// VARIABLES

define method emit-forward
    (back-end :: <java-back-end>, stream :: <stream>, o :: <module-binding>) => ()
break(); //MT
  format-emit*(back-end, stream, "extern ~ @;\n", $dylan-type-string, o);
end method;

define method emit-definition 
    (back-end :: <java-back-end>, stream :: <stream>, o :: <module-binding>) => ()
break(); //MT
  format-emit*(back-end, stream, "~ %;\n", $dylan-type-string, o);
end method;

// CODE

define method emit-forward
    (back-end :: <java-back-end>, stream :: <stream>, o :: <&xep>) => ()
break(); //MT
end method;

define method emit-definition
    (back-end :: <java-back-end>, stream :: <stream>, o :: <&xep>) => ()
break(); //MT
end method;

//// GENERIC OBJECT EMISSION

// FORWARD DECLARATIONS

define method emit-forward  // !@#$ NEED UNIFYING TYPE
    (back-end :: <java-back-end>, stream :: <stream>, o) => ()
break(); //MT
  unless (o.direct-object?)
    write(stream, "extern ");
    emit-type-name(back-end, stream, o);
    format-emit*(back-end, stream, " ^;\n", o);
  end unless;
end method;

// DEFINITIONS

//define method ^debug-name (object) #f end; // !@#$ PATCH

define method emit-definition // !@#$ need unifying type
    (back-end :: <java-back-end>, stream :: <stream>, o :: <object>) => ()
break(); //MT
  // Direct objects are always emitted in full at point of reference and
  // are never referred to by name, hence no need for a forward declaration.
  unless (o.direct-object?)
    unless (o.model-definition) 
      write(stream, "static ");
    end;
    emit-type-name(back-end, stream, o);
    format-emit*(back-end, stream, " ^ = %;\n\n", o, o);
  end unless;
end method;

// INDIRECTION DEFINITIONS

define method emit-indirection-definition
    (back-end :: <java-back-end>, stream :: <stream>, o :: <object>) => ()
break(); //MT
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
    (back-end :: <java-back-end>, stream, class, slotd, object) => ();

define method emit-object // !@#$ NEED UNIFYING TYPE
    (back-end :: <java-back-end>, stream :: <stream>, o :: <object>) => ()
break(); //MT
  let class = &object-class(o);
  write(stream, "{\n  ");
  emit-reference(back-end, stream, class);
  write(stream, " /* object-class */");
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
    (back-end :: <java-back-end>, stream :: <stream>, 
     class, slotd :: <&any-instance-slot-descriptor>, o) => ()
break(); //MT
  format(stream, ",\n  ");
  emit-reference(back-end, stream, ^slot-value(o, slotd));
  write(stream, " /* ");
  emit-struct-field-name(back-end, stream, class, slotd, 0);
  write(stream, " */");
end method;


define method emit-object-slot
    (back-end :: <java-back-end>, stream :: <stream>, 
     class, slotd :: <&repeated-slot-descriptor>, o) => ()
break(); //MT
  let size = ^slot-value(o, ^size-slot-descriptor(slotd));
  //  ---- This is no longer needed.  The size descriptor slot is represented
  //       in the instnace-slot-descriptors explicitly now.
  //emit-object-slot(back-end, stream, class, ^size-slot-descriptor(slotd), o);
  //
  if (slotd.^slot-type == dylan-value(#"<byte-character>"))
    format(stream, ",\n  \"");
    for (i from 0 below size)
      emit-raw-character-data
        (back-end, stream, ^repeated-slot-value(o, slotd, i));
    end;
    format(stream, "\"");
  else
    for (i from 0 below size)
      format-emit*(back-end, stream, ",\n  ");
      emit-reference(back-end, stream, ^repeated-slot-value(o, slotd, i));
      write(stream, " /* ");
      emit-struct-field-name(back-end, stream, class, slotd, i);
      format(stream, "[%d] */", i);
    end;
  end if;
end method;
*/
// Any other kind of slot descriptor has no presence in an instance.
// This covers virtual, class, and each-subclass slots.

/*
--- This shouldn't be needed anymore, we shouldn't be getting called
--- on non-instance slots.

define method emit-object-slot
    (back-end :: <java-back-end>, stream :: <stream>, 
     class, slotd :: <&slot-descriptor>, o) => ()
break(); //MT
end method;
*/


// CLASSES

// If the class has a repeated slot, rather than dumping the struct 
// itself we dump a struct constructor macro. 
/*
define method emit-slot-definition-using-type-name 
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     type-name :: <string>, 
     o :: <&class>, slotd :: <&slot-descriptor>, offset :: <integer>) 
 => ()
break(); //MT
//  write(stream, prefix-string);
//  format(stream, "%s", type-name);
//  write-element(stream, ' ');
//  emit-struct-field-name(*java-back-end*, stream, o, slotd, offset);
//  write(stream, suffix-string);
end method;

define method emit-slot-definition
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     o :: <&class>, slotd :: <&repeated-slot-descriptor>, offset :: <integer>) 
 => ()
break(); //MT
  emit-slot-definition-using-type-name
    (stream, prefix-string, suffix-string, 
     slotd.^slot-type.java-repeated-type-name, o, slotd, offset);
end method;

define method emit-slot-definition
    (stream :: <stream>, prefix-string :: <string>, suffix-string :: <string>,
     o :: <&class>, slotd :: <&slot-descriptor>, offset :: <integer>) 
 => ()
break(); //MT
  emit-slot-definition-using-type-name
    (stream, prefix-string, suffix-string, 
     slotd.^slot-type.java-type-name, o, slotd, offset);
end method;

define method emit-typedef
    (back-end :: <java-back-end>, stream :: <stream>, o :: <&class>) => ()
break(); //MT
  ^ensure-slots-initialized(o);
  let rslotd = o.^repeated-slot-descriptor; 
  let islots = o.^instance-slot-descriptors;
  if (~rslotd)
    format(stream, "typedef struct {\n");
    format(stream, "  %s class;\n", <&object>.java-type-name);
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
    format(stream, "  typedef struct { \\\n");
    format(stream, "    %s class; \\\n", <&object>.java-type-name);
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

define method emit-struct-name (back-end :: <java-back-end>, stream :: <stream>, o :: <&class>)
break(); //MT
end;

// VARIABLES

define method emit-object 
    (back-end :: <java-back-end>, stream :: <stream>, o :: <module-binding>) => ()
break(); //MT
  format-emit*(back-end, stream, "@", o);
  let value = binding-model-object(o);
  unless (instance?(value, <unknown>))
    format-emit*(back-end, stream, " = @", value);
  end;
end method;

define method emit-forward
    (back-end :: <java-back-end>, stream :: <stream>, o :: <&iep>) => ()
break(); //MT
  emit-lambda-interface(back-end, stream, o);
  write(stream, ";\n");
end method;

define method emit-definition
    (back-end :: <java-back-end>, stream :: <stream>, o :: <&iep>) => ()
break(); //MT
  format(stream, "%s\n", o.code);
end method;
*/
