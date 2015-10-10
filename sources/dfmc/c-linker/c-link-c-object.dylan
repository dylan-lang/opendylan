Module: dfmc-c-linker
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method emit-parameter-types
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&c-function>) => ()
  format(stream, "(");
  let required = o.c-signature.^signature-required;
  if (empty?(required))
    format(stream, "void");
  else
    for (type in required,
         first? = #t then #f)
      unless (first?)
        format(stream, ", ");
      end unless;
      format-emit*(back-end, stream, "^", type);
    end for;
  end if;
  format(stream, ")");
end method;

///--- Emitting extern declarations for these functions will produce parameter
///--- lists that conflict with their declarations in the system header files.
define constant $generic-names-not-to-emit = #["pseudo_primitive_command_name",
                                               "pseudo_primitive_command_arguments"];

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&c-function>) => ();
  unless(member?(o.c-function-name, $generic-names-not-to-emit, test: \=))
    let sig-values = o.primitive-signature.^signature-values;
    let return-type = first(sig-values, default: dylan-value(#"<raw-c-void>"));
    if (target-os-name() == #"win32")
      format-emit*(back-end, stream, "~ ^ ~ ^ ",
                   if (o.c-function-name) "extern" else "typedef" end,
                   return-type,
                   o.c-modifiers,
                   o);
    else
      format-emit*(back-end, stream, "~ ^ ^ ",
                   if (o.c-function-name) "extern" else "typedef" end,
                   return-type,
                   o);
    end if;
    emit-parameter-types(back-end, stream, o);
    format-emit*(back-end, stream, ";\n");
  end;
end method;

define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>, v :: <&c-callable-function>)
 => ()
end method;

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, v :: <&c-callable-function>)
 => ()
end method;

define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>, v :: <&c-variable>)
 => ()
end method;

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, v :: <&c-variable>)
 => ()
  format(stream, "extern void *%s;\n", v.name);
end method;

define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&objc-msgsend>) => ();
  format(stream, "extern void %s(void);\n", o.c-function-name);
end;


define method emit-raw-struct-member
    (back-end :: <c-back-end>, stream,
     member :: <raw-aggregate-ordinary-member>,
     object, options, number)
  format(stream, "  ");
  format-emit*(back-end, stream, "^", member-raw-type(member));
  format(stream, " member_%d;\n", number);
end;

define method emit-raw-struct-member
    (back-end :: <c-back-end>, stream,
     member :: <raw-struct-bitfield-member>,
     object, options, number)
  format(stream, "  ");
  format-emit*(back-end, stream, "^", member-raw-type(member));
  format(stream, " member_%d:%d;\n", number, member-bitfield-width(member));
end;

define method emit-raw-struct-member
    (back-end :: <c-back-end>, stream,
     member :: <raw-aggregate-array-member>,
     object, options, number)
  format(stream, "  ");
  format-emit*(back-end, stream, "^", member-raw-type(member));
  format(stream, " member_%d[%d];\n", number, member-array-length(member));
end;

// define method emit-c-parameter-typedefs (back-end :: <c-back-end>,
//                                          stream, o :: <&c-function>)
//   for (type in o.c-signature.^signature-required)
//     emit-c-parameter-typedef(back-end, stream, type);
//   end;
// end;
//
// define method emit-c-parameter-typedef (back-end :: <c-back-end>, stream,
//                                         object)
// end;
//
// define method emit-c-parameter-typedef (back-end :: <c-back-end>, stream,
//                                         object :: <&raw-aggregate-type>)
//   let options = raw-aggregate-options(object);
//   emit-pre-raw-struct-options(back-end, stream, object, options);
//   format(stream, "typedef struct {\n");
//   for (member in raw-aggregate-members(object),
//        i from 0)
//     emit-raw-struct-member(back-end, stream, member, object, options, i);
//   end;
//   format(stream, "} ");
//   format-emit*(back-end, stream, "^", object);
//   format(stream, ";\n");
// end;


define method emit-pre-raw-struct-options
    (back-end :: <c-back-end>, stream, object, options)
  // deal with pragma pack on windows, other packing options
end;


// Is this necessary?
define method emit-definition
    (back-end :: <c-back-end>, stream :: <stream>,
     o :: <&raw-aggregate-type>) => ()
  // do nothing
end;



define method emit-forward
    (back-end :: <c-back-end>, stream :: <stream>,
     object :: <&raw-aggregate-type>) => ()
end;


define method emit-heap-typedefs
    (back-end :: <c-back-end>, stream :: <stream>, heap,
     object :: <&raw-aggregate-type>) => ()
  let options = raw-aggregate-options(object);
  emit-pre-raw-struct-options(back-end, stream, object, options);
  format(stream, "typedef struct {\n");
  for (member in raw-aggregate-members(object),
       i from 0)
    emit-raw-struct-member(back-end, stream, member, object, options, i);
  end;
  format(stream, "} ");
  format-emit*(back-end, stream, "^", object);
  format(stream, ";\n\n");
end;
