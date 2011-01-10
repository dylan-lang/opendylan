module: mnemonic-assembler
Synopsis:  mnemonic assembler output from harp for i486
Author:    Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method convert-label(lambda-name :: <byte-string>, label :: <labelled-absolute-constant>) => (ext :: <some-external>)
  make(<labelled-external>, ext-code-pos: label.labelled-constant-index, init-label-name: cr-refers-to(label.labelled-constant-reference))
end method;

define method convert-label(lambda-name :: <byte-string>, label :: <labelled-relative-constant>) => (ext :: <some-external>)
  make(<labelled-external>, ext-code-pos: label.labelled-constant-index, init-label-name: cr-refers-to(label.labelled-constant-reference))
end method;

define method convert-label(lambda-name :: <byte-string>, label :: <labelled-constant-with-opcode>) => (ext :: <some-external>)
  make(<labelled-external>, ext-code-pos: label.labelled-constant-index + 1, init-label-name: cr-refers-to(label.labelled-constant-reference))
end method;

define method convert-label(lambda-name :: <byte-string>, label :: <relative-address-constant>) => (ext :: <some-external>)
  make(<relative-external>, ext-code-pos: label.labelled-constant-index, init-label-name: lambda-name, init-offset: label.relative-offset)
end method;

define method table-lookup(addr :: <object>) => (obj :: <object>)
  #f
end method table-lookup;

define sideways method output-compiled-lambda
    (be :: <harp-back-end>, outputter :: <harp-mnemonic-assembler-outputter>, lambda :: <fully-compiled-lambda>,
     #key)
    => ()
// This produces assemby output for one function.

  let name = lambda.lambda-name;
  let code = lambda.lambda-code;
  let labels = lambda.lambda-labels;
  let total-len = code.size;
  let asm-stream = outputter.outputter-stream;

// To start with, we just output the code as disassembled from the
// vector, and ignore relocations.
// Now we transform and pass in the relocations

  let relocs = map(curry(convert-label, name), labels);
  let i = 0;
  let (opcode-list, new-index) = decode-opcodes(code, 0, total-len, external-table: relocs);
  let mnemonics = opcodes-to-string(opcode-list, 0, table-lookup);
  format(asm-stream, "%s:\n", name);
  for (y in mnemonics)
    format(asm-stream, "%s\n", y)
  end for;
end method;
