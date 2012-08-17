module:    x86-harp
Synopsis:  Pentium assembly code outputting
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// This generates just COFF output

define constant $coff-only-output$ = list(#"coff");


// This generates both assembler and COFF output

// define constant $coff-and-assembler-output$ = list(#"assembler", #"coff");


// By default, we generate COFF files for the Pentium

define constant $default-harp-output-type$ = $coff-only-output$;


define method default-harp-output-type
    (backend :: <harp-x86-back-end>) => (type)
   $default-harp-output-type$;
end method;



/// COFF outputter support
/// (mostly this is implemented in the harp-coff library)


define method file-extension-for-outputter-type
       (backend :: <harp-x86-back-end>, type == $coff-type$)
       => (extension :: <byte-string>)
  "obj";
end method;

define method stream-type-for-outputter-type
       (backend :: <harp-x86-back-end>, type == $coff-type$)
       => (stream-type :: <class>)
  <byte-file-stream>
end method;


define method coff-machine-type
    (backend :: <harp-x86-back-end>)
    => (machine :: <integer>, big-endian? :: <boolean>)
  values(#x14c, #f);
end method;


define method output-code-start
      (backend :: <harp-x86-back-end>, outputter :: <harp-coff-builder>) => ()
  next-method();
  output-implicit-externals(backend, outputter);
end method;


define method real-register-debug-info-enumeration
    (backend :: <harp-x86-back-end>, register :: <pentium-integer-register>)
    => (enumeration :: <integer>)
  register.real-register-number + 17; // see CodeView spec for this
end method;


define method real-register-debug-info-enumeration
    (backend :: <harp-x86-back-end>, register :: <pentium-float-register>)
    => (enumeration :: <integer>)
  register.real-register-number + 128; // see CodeView spec for this
end method;


define method real-register-from-debug-info-enumeration
    (backend :: <harp-x86-back-end>, enumeration :: <integer>)
    => (reg :: <pentium-register>)

  local method report-unknown ()
          harp-error("Unknown register enumeration %d", enumeration);
        end method;

  if (enumeration < 128)
    // must be an general register
    let reg-num = enumeration - 17; // see CodeView spec for this
    let key = find-key(pentium-real-registers,
                       method (x) x.real-register-number == reg-num end);
    if (key)
      pentium-real-registers[key];
    else report-unknown();
    end if;
  else
    // must be a FP register
    if (enumeration == 128)
      f-st;
    else
      // we don't expect to ever see these
      report-unknown();
    end if;
  end if;
end method;


/// Assembler outputter support


define method output-compiled-lambda
    (be :: <harp-x86-back-end>, outputter :: <harp-binary-assembler-outputter>, lambda :: <fully-compiled-lambda>,
     #key)
    => ()
  let name = lambda.lambda-name;
  let code = lambda.lambda-code;
  let labels = lambda.lambda-labels;
  let total-len = code.size;
  let labels-size = labels.size;

  let asm-stream = outputter.outputter-stream;
  reset-asm-line-pos(outputter);

  // output any external references
  for (ext :: <constant-reference> in lambda.lambda-externals)
    format(asm-stream, "EXTRN\t%s:NEAR\n", ext.cr-refers-to);
  end for;

  if (lambda.lambda-is-public?)
    format(asm-stream, "\n%s\tPROC\tPUBLIC\n", name);
  else
    format(asm-stream, "\n%s\tPROC\n", name);
  end if;


  let code-index = 0;
  let lab-index = 0;

  local method find-next-label-and-code-index ()
                   => (label, index :: <integer>)
          if (lab-index < labels-size)
            // There's a forthcoming label - so return its index into the code
            let label :: <labelled-constant> = labels[lab-index];
            lab-index := lab-index + 1;
            values(label, label.labelled-constant-index);
          else // There are no more labels - so return the code size
            values(#f, total-len);
          end if;
        end;

  // iterate over the entire code vector
  while (code-index < total-len)
    // Find the next label and corresponding code index
    let (label, label-pos) = find-next-label-and-code-index();

    // Emit all the normal code up to the label
    for (index from code-index below label-pos)
      output-integer-code-item(outputter, code[index]);
    end for;

    // Emit code for any label
    if (label)
      output-code-label(outputter, label);
      code-index := label-pos + label.labelled-constant-size;
    else
      code-index := label-pos;
    end if;
  end while;

  maybe-reset-asm-line-pos(outputter);
  format(asm-stream, "%s\tENDP\n\n", name);
end method;


define method output-integer-code-item
    (outputter :: <harp-assembler-outputter>, item :: <integer>) => ()
  let asm-stream = outputter.outputter-stream;
  if (at-asm-line-start?(outputter))
    format(asm-stream, "\tDB\t");
  else
    format(asm-stream, ",");
  end;
  format(asm-stream, "%d", item);
  increment-asm-line-pos(outputter);
end method;


define method output-code-label
    (outputter :: <harp-assembler-outputter>,
     item :: <relative-address-constant>) => ()

  let offset = item.relative-offset;
  let asm-stream = outputter.outputter-stream;

  maybe-reset-asm-line-pos(outputter);
  format(asm-stream, "\tDD\t$");

  if (offset = 0)
    format(asm-stream, "\n");
  elseif (offset < 0)
    format(asm-stream, " - %d\n", - offset);
  else
    format(asm-stream, " + %d\n", offset);
  end if;
end method;


define method output-code-label
    (outputter :: <harp-assembler-outputter>,
     item :: <labelled-absolute-constant>) => ()
  output-code-label-internal(outputter, item);
end method;


define method output-code-label
    (outputter :: <harp-assembler-outputter>,
     item :: <labelled-relative-constant>) => ()
  let adjust = 4; // allow for the 4 bytes of the constant itself
  output-code-label-internal(outputter, item,
                             attr: " - $", adjust: adjust);
end method;


define method output-code-label
    (outputter :: <harp-assembler-outputter>,
     item :: <labelled-constant-with-opcode>)
  output-code-label-internal(outputter, item,
                             intervene: "near ptr", directive: item.opcode);
end method;


define method output-code-label-internal
    (outputter :: <harp-assembler-outputter>,
     item :: <labelled-constant>,
     #key attr :: <byte-string> = "",
          adjust :: <integer> = 0,
          directive = "DD",
          intervene = #f) => ()

  let asm-stream = outputter.outputter-stream;
  let cr = item.labelled-constant-reference;
  let ref = cr.cr-refers-to;
  let offset = cr.cr-const-offset - adjust;

  maybe-reset-asm-line-pos(outputter);
  format(asm-stream, "\t%s\t", directive);

  if (intervene)
    format(asm-stream, "%s ", intervene);
  end if;

  if (offset == 0)
    format(asm-stream, "%s%s\n", ref, attr);
  elseif (offset < 0)
    format(asm-stream, "%s - %d%s\n", ref, - offset, attr);
  else
    format(asm-stream, "%s + %d%s\n", ref, offset, attr);
  end if;
end method;



define method output-data-item
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     item :: <abstract-integer>,
     #key import?) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n\tDD %d", item);
  unflag-asm-line-start(outputter);
end method;


define method output-data-item
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     item :: <byte-string>,
     #key import?,
          model-object = unsupplied()) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n\tDD %s",
         canonical-data-object(item, model-object));
  unflag-asm-line-start(outputter);
end method;


define method output-data-item
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     item :: <constant-reference>,
     #key import?) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n\tDD %s", item.cr-refers-to);
  unflag-asm-line-start(outputter);
end method;


define method unreadable-character? (ch :: <character>) => (r :: <boolean>)
  // Returns true for characters which are invalid in MASM's string syntax
  (ch < ' ') | (ch > '~') | (ch == '"')
end method;

define method print-string
    (stream :: <stream>, string :: <byte-string>) => ()
  // Print a string in quoted MASM syntax, assuming all characters are valid
  format(stream, "\"%s\"", string);
end method;

define method output-readable-string
    (stream :: <stream>, string :: <byte-string>) => ()
  let badch = find-key(string, unreadable-character?);
  if (badch)
    unless (badch == 0)
      print-string(stream, copy-sequence(string, end: badch));
      format(stream, ", ");
    end unless;
    format(stream, "%d", as(<integer>, string[badch]));
    if (badch ~= (string.size - 1))
      format(stream, ", ");
      output-readable-string(stream, copy-sequence(string, start: badch + 1));
    end if;
  else
    // string is readable, so write it directly
    print-string(stream, string);
  end if;
end method;


define method output-data-byte
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     item :: <byte-string>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n\tDB ");
  output-readable-string(asm-stream, item);
  unflag-asm-line-start(outputter);
end method;


define method output-data-byte
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     byte :: <integer>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n\tDB %d", byte);
  unflag-asm-line-start(outputter);
end method;



define method output-header
      (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n.486\n");
  format(asm-stream, ".MODEL\tFLAT\n");
  format(asm-stream, "OPTION\tCASEMAP:NONE\n");
  flag-asm-line-start(outputter);
end method;


define method output-footer
      (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\nEND\n");
end method;

define method output-code-start
      (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n.CODE\n");
  flag-asm-line-start(outputter);
  output-implicit-externals(be, outputter);
end method;

define method output-data-start
      (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>) => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n.DATA\n");
  flag-asm-line-start(outputter);
end method;


define method output-glue-symbols
     (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     #key data-start, data-end,
          variables-start, variables-end,
          objects-start, objects-end,
          fixup-start, fixup-end) => ()
  harp-warning(be, "Output-glue-symbols NYI for assembler outputter.");
end method;


define method output-external
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>, name :: <byte-string>,
     #key import?)
     => ()
  let asm-stream = outputter.outputter-stream;
  ensure-asm-line-start(outputter);
  format(asm-stream, "EXTRN\t%s:NEAR\n", name);
end method;

define method output-external
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     name :: <constant-reference>, #key import?) => ()
  output-external(be, outputter, name.cr-refers-to, import?: import?);
end method;


define method output-public
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>, name :: <byte-string>,
     #key)
     => ()
  let asm-stream = outputter.outputter-stream;
  ensure-asm-line-start(outputter);
  format(asm-stream, "PUBLIC\t%s\n", name);
end method;

define method output-public
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     name :: <constant-reference>,
     #key) => ()
  output-public(be, outputter, name.cr-refers-to);
end method;


define method output-export
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>, name :: <byte-string>)
     => ()
  // Do nothing. This is not implemented for the assembler
end method;

define method output-export
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     name :: <constant-reference>) => ()
  output-export(be, outputter, name.cr-refers-to);
end method;


define method output-definition
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     name :: <byte-string>,
     #key section, public?, export?,
          model-object = unsupplied()) => ()
  let asm-stream = outputter.outputter-stream;
  let name = canonical-data-object(name, model-object);
  ensure-asm-line-start(outputter);
  format(asm-stream, "\nALIGN 4\n%s \\", name);
  unflag-asm-line-start(outputter);
  if (public?) output-public(be, outputter, name) end;
  if (export?) output-export(be, outputter, name) end;
end method;

define method output-definition
    (be :: <harp-x86-back-end>,
     outputter :: <harp-assembler-outputter>,
     name :: <constant-reference>,
     #rest all-keys, #key section, public?, export?) => ()
  apply(output-definition, be, outputter, name.cr-refers-to, all-keys);
end method;



define method output-variable
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     name :: <byte-string>, initial-value,
     #key repeat, section, import-value?, public?, export?,
          model-object = unsupplied()) => ()
  let asm-stream = outputter.outputter-stream;
  let name = canonical-data-object(name, model-object);
  output-definition(be, outputter, name, section: section);
  if (repeat)
    format(asm-stream, "\n\tDD %= DUP(%=)", repeat, initial-value);
  else
    output-data-item(be, outputter, initial-value, import?: import-value?);
  end if;
  format(asm-stream, "\n");
  flag-asm-line-start(outputter);
  if (public?) output-public(be, outputter, name) end;
  if (export?) output-export(be, outputter, name) end;
end method;

define method output-variable
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>,
     name :: <constant-reference>, initial-value,
     #rest all-keys, #key repeat, section, import-value?, public?, export?) => ()
  apply(output-variable,
        be, outputter, name.cr-refers-to, initial-value,
        all-keys);
end method;


define method output-comment
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>, comment :: <string>)
     => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\n;;;  %s   ;;;\n", comment);
  flag-asm-line-start(outputter);
end method;

define method output-line-comment
    (be :: <harp-x86-back-end>, outputter :: <harp-assembler-outputter>, comment :: <string>)
     => ()
  let asm-stream = outputter.outputter-stream;
  format(asm-stream, "\t;;;  %s   ;;;", comment);
end method;





// Functions to update the current position within the line


define constant max-ints-per-line = 15;


define method at-asm-line-start?
      (outputter :: <harp-assembler-outputter>) => (res :: <boolean>)
  outputter.outputter-line-pos == 0
end method;


define method maybe-reset-asm-line-pos
      (outputter :: <harp-assembler-outputter>) => ()
  if (outputter.outputter-line-pos ~= 0)
    outputter.outputter-line-pos := 0;
    format(outputter.outputter-stream, "\n");
  end if;
end method;

define method reset-asm-line-pos
      (outputter :: <harp-assembler-outputter>) => ()
  outputter.outputter-line-pos := 0;
  format(outputter.outputter-stream, "\n");
end method;

define method increment-asm-line-pos
      (outputter :: <harp-assembler-outputter>) => ()
  let pos = outputter.outputter-line-pos;
  if (pos >= max-ints-per-line)
    reset-asm-line-pos(outputter);
  else
    outputter.outputter-line-pos := pos + 1;
  end if;
end method;



// Functions to flag whether or not we are at the start of a line

define method flag-asm-line-start
      (outputter :: <harp-assembler-outputter>) => ()
  outputter.outputter-line-pos := 0;
end method;

define method unflag-asm-line-start
      (outputter :: <harp-assembler-outputter>) => ()
  outputter.outputter-line-pos := 1;
end method;

define method ensure-asm-line-start
      (outputter :: <harp-assembler-outputter>) => ()
  if (outputter.outputter-line-pos ~= 0)
    outputter.outputter-line-pos := 0;
    format(outputter.outputter-stream, "\n");
  end if;
end method;
