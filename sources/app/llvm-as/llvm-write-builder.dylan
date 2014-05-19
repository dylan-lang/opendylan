Module: llvm-write-builder
Copyright:    Original Code is Copyright 2014-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function llvm-write-builder
    (module :: <llvm-module>, stream :: <stream>)
  => ();
  let type-names = make(<object-table>);
  type-names[$llvm-label-type] := "$llvm-label-type";
  type-names[$llvm-void-type] := "$llvm-void-type";
  type-names[$llvm-metadata-type] := "$llvm-metadata-type";
  type-names[$llvm-float-type] := "$llvm-float-type";
  type-names[$llvm-double-type] := "$llvm-double-type";
  type-names[$llvm-i1-type] := "$llvm-i1-type";
  type-names[$llvm-i8-type] := "$llvm-i8-type";
  type-names[$llvm-i8*-type] := "$llvm-i8*-type";
  type-names[$llvm-i16-type] := "$llvm-i16-type";
  type-names[$llvm-i32-type] := "$llvm-i32-type";
  type-names[$llvm-i64-type] := "$llvm-i64-type";

  let global-value-names = make(<object-table>);
  global-value-names[$llvm-false] := "$llvm-false";
  global-value-names[$llvm-true] := "$llvm-true";
  for (global :: <llvm-global-value> keyed-by name in module.llvm-global-table)
    global-value-names[global] := name;
  end for;

  for (func :: <llvm-function> in module.llvm-module-functions)
    let local-value-names = shallow-copy(global-value-names);
    for (value :: <llvm-value> keyed-by name in func.llvm-function-value-table)
      local-value-names[value] := name;
      if (instance?(value, <llvm-placeholder-value>))
        local-value-names[value.llvm-value-forward] := name;
      end if;
    end for;

    format(stream, "// %s\n", func.llvm-global-name);
    format(stream, "begin\n");

    let v = 0;
    for (bb :: <llvm-basic-block> in func.llvm-function-basic-blocks,
         index from 1)
      local-value-names[bb] := format-to-string("bb%d", index);

      format(stream, "  let b%d = make(<llvm-basic-block>);\n", index);


      for (instruction :: <llvm-instruction>
             in bb.llvm-basic-block-instructions)
        let instruction-name
          = element(local-value-names, instruction, default: #f);
        unless (instruction-name
                  | llvm-void-type?(instruction.llvm-value-type))
          local-value-names[instruction] := format-to-string("v%d", v);
          v := v + 1;
        end unless;
      end for;
    end for;
    format(stream, "\n");

    for (bb :: <llvm-basic-block> in func.llvm-function-basic-blocks)
      format(stream, "  ins--block(be, %s);\n", local-value-names[bb]);

      for (instruction :: <llvm-instruction>
             in bb.llvm-basic-block-instructions)
        let instruction-name
          = element(local-value-names, instruction, default: #f);
        if (instruction-name)
          format(stream, "  let %s = ", instruction-name)
        else
          format(stream, "  ");
        end if;
        print-builder-instruction(instruction,
                                  local-value-names, type-names,
                                  stream);
        format(stream, ";\n");
      end for;

      format(stream, "\n");
    end for;
    format(stream, "end;\n\n");
  end for;
end function;

define method print-builder-instruction
    (instruction :: <llvm-binop-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--%s(be", instruction.llvm-binop-instruction-operator);
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-cast-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--%s(be",
         instruction.llvm-cast-instruction-operator);
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ", ");
  print-builder-type(instruction.llvm-value-type, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-icmp-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--icmp-%s(be",
         instruction.llvm-cmp-predicate);
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-gep-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--gep%s(be",
         if (instruction.llvm-gep-instruction-in-bounds?)
           "-inbounds"
         else
           ""
         end);
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-load-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--load(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-store-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--store(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-phi-node>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--phi*(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-select-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--select(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-call-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--call(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-branch-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--br(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-return-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--return(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-instruction
    (instruction :: <llvm-instruction>, value-names, type-names,
     stream)
  => ();
  format(stream, "ins--?(be");
  print-builder-operands(instruction.llvm-instruction-operands,
                         value-names, type-names, stream);
  format(stream, ") /* %s */", instruction);
end method;

define function print-builder-operands
    (operands :: <sequence>, value-names, type-names,
     stream :: <stream>)
  for (operand in operands)
    format(stream, ", ");
    print-builder-operand(operand, value-names, type-names, stream);
  end for;
end function;

define method print-builder-operand
    (operand :: <llvm-value>, value-names, type-names,
     stream :: <stream>)
 => ()
  let name = element(value-names, operand, default: #f);
  if (name)
    print-message(name, stream);
  else
    print-message(operand, stream);
  end if;
end method;

define method print-builder-operand
    (operand :: <llvm-placeholder-value>, value-names, type-names,
     stream :: <stream>)
 => ()
  print-builder-operand(operand.llvm-value-forward,
                        value-names, type-names, stream);
end method;

define method print-builder-operand
    (operand :: <llvm-argument>, value-names, type-names,
     stream :: <stream>)
 => ()
  print-message(operand.llvm-argument-name, stream);
end method;

define method print-builder-operand
    (operand :: <llvm-integer-constant>, value-names, type-names,
     stream :: <stream>)
 => ()
  let type = operand.llvm-value-type.llvm-type-forward;
  if (type.llvm-integer-type-width = 32)
    format(stream, "%d", operand.llvm-integer-constant-integer);
  else
    next-method();
  end;
end method;

define method print-builder-operand
    (operand :: <llvm-null-constant>, value-names, type-names,
     stream :: <stream>)
 => ()
  format(stream, "make(<llvm-null-constant>, type: ");
  print-builder-type(operand.llvm-value-type, type-names, stream);
  format(stream, ")");
end method;

define method print-builder-type
    (type :: <llvm-type>, type-names, stream :: <stream>)
 => ()
  let name = element(type-names, type, default: #f);
  if (name)
    print-message(name, stream);
  else
    print-object(type, stream);
  end if;
end method;

define method print-builder-type
    (type :: <llvm-placeholder-type>, type-names, stream :: <stream>)
 => ()
  print-builder-type(type.llvm-type-forward, type-names, stream);
end method;

define method print-builder-type
    (type :: <llvm-integer-type>, type-names, stream :: <stream>)
  => ()
  format(stream, "$llvm-i%d-type", type.llvm-integer-type-width);
end method;
