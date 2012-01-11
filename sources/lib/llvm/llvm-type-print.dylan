Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print-object (object :: <llvm-type>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    let obj-class = object.object-class;
    let name = obj-class.debug-name;
    if (name)
      write(stream, as-lowercase(as(<byte-string>, name)));
    else
      print(obj-class, stream);
    end if;
    write(stream, " ");
    write(stream, machine-word-to-string(address-of(object)));

    write(stream, " ");
    print-message(object, stream);
  end;
end method;

define sealed method print-message
    (type :: <llvm-primitive-type>, stream :: <stream>)
 => ();
  let text
    = select (type.llvm-primitive-type-kind)
        #"VOID"      => "void";
        #"FLOAT"     => "float";
        #"DOUBLE"    => "double";
        #"LABEL"     => "label";
        #"X86_FP80"  => "x86_fp80";
        #"FP128"     => "fp128";
        #"PPC_FP128" => "ppc_fp128";
        #"METADATA"  => "metadata";
        #"X86_MMX"   => "x86_mmx";
      end select;
  write-text(stream, text);
end method;

define sealed method print-message
    (type :: <llvm-integer-type>, stream :: <stream>)
 => ();
  format(stream, "i%d", type.llvm-integer-type-width);
end method;

define sealed method print-message
    (type :: <llvm-pointer-type>, stream :: <stream>)
 => ();
  print-message(type.llvm-pointer-type-pointee, stream);
  let space = type.llvm-pointer-type-address-space;
  unless (zero?(space))
    format(stream, " addrspace(%d)", space);
  end unless;
  write-element(stream, '*');
end method;

define sealed method print-message
    (type :: <llvm-function-type>, stream :: <stream>)
 => ();
  print-message(type.llvm-function-type-return-type, stream);
  printing-logical-block (stream, prefix: " (", suffix: ")")
    for (parameter-type in type.llvm-function-type-parameter-types,
         first? = #t then #f)
      unless (first?) write(stream, ", "); end unless;
      print-message(parameter-type, stream);
    finally
      if (type.llvm-function-type-varargs?)
        unless (first?) write(stream, ", "); end unless;
        write(stream, "...");
      end if;
    end for;
  end;
end method;

define sealed method print-message
    (type :: <llvm-struct-type>, stream :: <stream>)
 => ();
  if (type.llvm-struct-type-name)
    format(stream, "%%%s", type.llvm-struct-type-name);
  else
    // FIXME packed?
    printing-logical-block (stream, prefix: "{", suffix: "}")
      for (type-element in type.llvm-struct-type-elements,
           first? = #t then #f)
        unless (first?) write(stream, ", "); end unless;
        print-message(type-element, stream);
      end for;
    end;
  end if;
end method;

define sealed method print-message
    (type :: <llvm-array-type>, stream :: <stream>)
 => ();
  format(stream, "[%d x %s]",
         type.llvm-array-type-size,
         type.llvm-array-type-element-type);
end method;

define sealed method print-message
    (type :: <llvm-vector-type>, stream :: <stream>)
 => ();
  format(stream, "[%d x %s]",
         type.llvm-vector-type-size,
         type.llvm-vector-type-element-type);
end method;

define sealed method print-message
    (type :: <llvm-symbolic-type>, stream :: <stream>)
 => ();
  if (slot-initialized?(type, llvm-placeholder-type-forward))
    print-message(type.llvm-placeholder-type-forward, stream);
  else
    format(stream, "%%%s", type.llvm-symbolic-type-name);
  end if;
end method;

define sealed method print-message
    (type :: <llvm-opaque-type>, stream :: <stream>)
 => ();
  if (type.llvm-opaque-type-name)
    format(stream, "%%%s", type.llvm-opaque-type-name);
  else
    write(stream, "opaque");
  end if;
end method;
