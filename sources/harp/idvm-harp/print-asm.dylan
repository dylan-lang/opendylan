module:    idvm-harp
Synopsis:  Printing IDVM assembler
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Assembler printing support for the IDVM.
//// A specialization of the print-harp outputter protocol.



// print-harp outputter support

define method file-extension-for-outputter-type
       (backend :: <idvm-back-end>, type == #"print-harp") 
       => (extension :: <byte-string>)
  "vm-harp";
end method;



define method output-lambda-preamble 
    (be :: <idvm-back-end>, outputter :: <harp-print-outputter>) => ()
  output-lambda-preamble(be, outputter.outputter-stream);
end method;


define method output-lambda-preamble 
    (be :: <idvm-back-end>, asm-stream :: <stream>) => ()
  let vars = be.variables;
  let locals-size = be.locals-vector-size;
  format(asm-stream, "\n\n... Locals vector size: %=\n", locals-size);
  format(asm-stream, "... Code vector: ...\n");
end method;


define method output-lambda-postamble 
    (be :: <idvm-back-end>, outputter :: <harp-print-outputter>) => ()
  output-lambda-postamble(be, outputter.outputter-stream);
end method;


define method output-lambda-postamble 
    (be :: <idvm-back-end>, asm-stream :: <stream>) => ()
  format(asm-stream, "\n\n... done ...\n\n");
end method;



define method output-code-item
    (be :: <idvm-back-end>, outputter :: <harp-print-outputter>, item) => ()
  output-code-item(be, outputter.outputter-stream, item);
end method;


define method output-code-item 
    (be :: <idvm-back-end>, asm-stream :: <stream>, opcode :: <idvm-opcode>)
     => ()
  let str = make(<string>, size: 40, fill: ' ');
  let name = as(<string>, opcode.opcode-name);
  replace-subsequence!(str, name, end: name.size);
  format(asm-stream, "\n  %s", str);
end method;

define method output-code-item 
    (be :: <idvm-back-end>, asm-stream :: <stream>, operand :: <object>) 
     => ()
  format(asm-stream, "	%=", operand);
end method;

define method output-code-item 
    (be :: <idvm-back-end>, asm-stream :: <stream>, operand :: <idvm-hilo>)
     => ()
  format(asm-stream, "	{%=,%=}", operand.idvm-hi, operand.idvm-lo);
end method;

define method output-code-item 
    (be :: <idvm-back-end>, asm-stream :: <stream>, operand :: <new-sdi>)
     => ()
  for (elt in operand.new-sdi-code-holder)
    output-code-item(be, asm-stream, elt);
  end for;
end method;


