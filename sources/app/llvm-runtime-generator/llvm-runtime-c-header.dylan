Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010-2014 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print-raw-struct-c-declaration
    (be :: <llvm-back-end>, type :: <&raw-struct-type>, stream :: <stream>)
 => ();
  format(stream, "struct %s {\n", raw-mangle(be, type.^debug-name));
  for (member in type.raw-aggregate-members)
    let member-type = member.member-raw-type;
    format(stream, "  %s %s",
           member-type.raw-type-c-name,
           raw-mangle(be, as(<string>, member.member-name)));
    if (instance?(member, <raw-aggregate-array-member>))
      format(stream, "[%d]", member.member-array-length);
    end if;
    format(stream, ";\n");
  end for;

  format(stream, "};\n\n");
end method;

define method print-class-c-struct-declaration
    (be :: <llvm-back-end>, type :: <&class>, stream :: <stream>)
 => ();
  let islots = type.^instance-slot-descriptors;
  format(stream, "struct %s {\n", emit-name-internal(be, #f, type));

  format(stream, "  ");
  print-primitive-c-type(be, model-<mm-wrapper>(), stream);
  format(stream, " wrapper;\n");

  for (slotd in islots, i from 0)
    format(stream, "  ");
    print-primitive-c-type(be, slotd.^slot-type, stream);
    if (slotd.^debug-name)
      format(stream, " %s;\n",
             raw-mangle(be, as(<string>, slotd.^debug-name)));
    else
      format(stream, " anon_slot_%d;\n", i);
    end;
  end;

  let rslotd = type.^repeated-slot-descriptor;
  if (rslotd)
    format(stream, "  ");
    print-primitive-c-type(be, rslotd.^slot-type, stream);
    if (rslotd.^debug-name)
      format(stream, " %s[];\n",
             raw-mangle(be, as(<string>, rslotd.^debug-name)));
    else
      format(stream, " repeated[];\n");
    end;
  end if;

  write(stream, "};\n\n");
end method;

define method print-primitive-c-function-declaration
    (be :: <llvm-back-end>, name :: <symbol>, signature :: <&signature>,
     stream :: <stream>)
 => ();
  if (zero?(^signature-number-values(signature)))
    format(stream, "void");
  else
    print-primitive-c-type(be, ^signature-values(signature).first, stream);
  end;

  format(stream, " %s(", raw-mangle(be, name));

  if (zero?(^signature-number-required(signature)))
    if (^signature-rest?(signature))
      format(stream, "...");
    else
      format(stream, "void");
    end if;
  else
    for (required in ^signature-required(signature),
         index from 0 below ^signature-number-required(signature))
      unless (zero?(index)) format(stream, ", "); end;
      print-primitive-c-type(be, required, stream);
    end for;
    if (^signature-rest?(signature))
      format(stream, ", ...");
    end if;
  end if;

  format(stream, ");\n");
end method;

define method print-runtime-variable-declaration
    (be :: <llvm-back-end>, name :: <symbol>,
     descriptor :: <llvm-runtime-variable-descriptor>,
     stream :: <stream>)
 => ()
  format(stream, "extern ");

  let thread-local?
    = member?(#"thread-local", descriptor.runtime-variable-attributes)
    & llvm-thread-local-support?(be);
  if (thread-local?)
    format(stream, "__thread ");
  end if;

  let type-name = descriptor.runtime-variable-type-name;
  select (type-name)
    #"<teb>" =>
      format(stream, "struct dylan_teb");
    otherwise =>
      print-primitive-c-type(be, dylan-value(type-name), stream);
  end select;

  format(stream, " %s;\n", raw-mangle(be, name));
end method;

define method print-primitive-c-type
    (be :: <llvm-back-end>, type :: <&raw-type>, stream :: <stream>)
 => ();
  print-message(raw-type-c-name(type), stream);
end method;

define method print-primitive-c-type
    (be :: <llvm-back-end>, type, stream :: <stream>)
 => ();
  print-message("dylan_value", stream);
end method;

define method print-runtime-object-declaration
    (be :: <llvm-back-end>, name :: <symbol>,
     stream :: <stream>)
 => ();
  let o = dylan-value(name);
  let type = o.^object-class;
  format(stream, "extern struct %s %s;\n",
         emit-name-internal(be, #f, type),
         emit-name-internal(be, #f, o));
end method;

