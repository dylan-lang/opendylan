module:    gnu-as-outputter
Synopsis:  GNU assembler output from harp for i486
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define class <harp-gnu-as-outputter>(<harp-binary-builder>)
  slot outputter-line-position :: <integer> = 0;
  slot finished-outputting? :: <boolean> = #f;
end class;

define class <harp-elf-as-outputter>(<harp-gnu-as-outputter>)
end class;

define class <harp-macho-as-outputter> (<harp-gnu-as-outputter>)
end class;


define class <gnu-section>(<binary-section>)
  slot current-position :: <integer> = 0;
end class;

define constant $gnu-as-outputter-type$ = #"gnu-as-outputter";

define sideways method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $gnu-as-outputter-type$)
       => (extension :: <byte-string>)
  "s";
end method;

define sideways method stream-type-for-outputter-type
       (backend :: <harp-back-end>, type == $gnu-as-outputter-type$)
       => (stream-type :: <class>)
  <byte-file-stream>
end method;

define sideways method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $gnu-as-outputter-type$)
    => (outputter :: <harp-gnu-as-outputter>)
  let file-string = as(<string>, filename);
  let stream = open-output-stream(backend, file-string, type);
  let def-file = open-output-stream(backend, file-string, "def");
  let outputter
    = make-binary-builder(<harp-gnu-as-outputter>,
                          destination: stream,
                          def-file: def-file);
  outputter;
end method;


define constant $elf-as-outputter-type$ = #"elf-as-outputter";

define sideways method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $elf-as-outputter-type$)
       => (extension :: <byte-string>)
  file-extension-for-outputter-type(backend, $gnu-as-outputter-type$);
end method;

define sideways method stream-type-for-outputter-type
       (backend :: <harp-back-end>, type == $elf-as-outputter-type$)
       => (stream-type :: <class>)
  stream-type-for-outputter-type(backend, $gnu-as-outputter-type$);
end method;

define sideways method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $elf-as-outputter-type$)
    => (outputter :: <harp-elf-as-outputter>)
  let file-string = as(<string>, filename);
  let stream = open-output-stream(backend, file-string, type);
  let outputter
    = make-binary-builder(<harp-elf-as-outputter>,
                          destination: stream);
  outputter;
end method;



define constant $macho-as-outputter-type$ = #"macho-as-outputter";

define sideways method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $macho-as-outputter-type$)
       => (extension :: <byte-string>)
  file-extension-for-outputter-type(backend, $gnu-as-outputter-type$);
end method;

define sideways method stream-type-for-outputter-type
       (backend :: <harp-back-end>, type == $macho-as-outputter-type$)
       => (stream-type :: <class>)
  stream-type-for-outputter-type(backend, $gnu-as-outputter-type$);
end method;

define sideways method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $macho-as-outputter-type$)
    => (outputter :: <harp-macho-as-outputter>)
  let file-string = as(<string>, filename);
  let stream = open-output-stream(backend, file-string, type);
  let outputter
    = make-binary-builder(<harp-macho-as-outputter>,
                          destination: stream);
  outputter;
end method;



define constant $elf-as-assemble-command-line =
  "as -L --32 -o %s.o %s.s";

define method assemble-harp-outputter
    (outputter :: <harp-elf-as-outputter>, filename) => ()
  if (outputter.finished-outputting?)
    let file-string = as(<string>, filename);
    let command-line =
      format-to-string($elf-as-assemble-command-line,
                       file-string, file-string);
    run-application(command-line);
  end if;
end method;

define constant $x86-darwin-assemble-command-line =
  "as -L -arch i386 -o %s.o %s.s";

define method assemble-harp-outputter
    (outputter :: <harp-macho-as-outputter>, filename) => ()
  if (outputter.finished-outputting?)
    let file-string = as(<string>, filename);
    let command-line =
      format-to-string($x86-darwin-assemble-command-line,
                       file-string, file-string);
    run-application(command-line);
  end if;
end method;


/// GNU Assembler outputter support

define sideways method output-compiled-lambda
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, lambda :: <fully-compiled-lambda>,
     #key section = #"code", #all-keys)
    => ()
// This produces assembly output for one function.

  let name = lambda.lambda-name;
  let code = lambda.lambda-code;
  let labels = lambda.lambda-labels;
  let total-len = code.size;
  let labels-size = labels.size;
  let export? = lambda.lambda-is-export?.and-emit-dll?;
  let ref-data = lambda.lambda-referenced-data;
  let stream = outputter.destination;

  // Just toggle the required code sections; these can be interleaved

  let previous-section =
    slot-initialized?(outputter, current-section) & outputter.current-section;

  select-dylan-section(outputter, section, be.code-item-increment);
  let section = outputter.current-section;
  unless (previous-section == section)
    write-binary-section-header(stream, section);
  end unless;

  if (ref-data)
    // output any referenced data BEFORE the definition, because constant
    // references use a negative offset. NB we assume that there will never
    // be any padding when the name gets defined after this data, because
    // of a suitable choice of alignment.
    add-data-vector(outputter, ref-data);
  end if;

  reset-asm-line-pos(outputter);

  // output any external references
  for (ext :: <constant-reference> in lambda.lambda-externals)
    let import? = instance?(ext, <imported-constant-reference>);
    write(stream, "\t.global ");
    write(stream, imported-name(outputter,
                                ext.cr-refers-to,
                                import?));
    write-element(stream, byte-for-newline);
  end for;

  if (lambda.lambda-is-public?)
    write(stream, "\n\t.global "); write(stream, name);
  end if;

  output-function-type(outputter, name);
  write-element(stream, byte-for-newline);

  do-export(export?, outputter, name);

  write(stream, "\t.size "); write(stream, name); write(stream, ", ");
  write(stream, integer-to-string(total-len));
  write-element(stream, byte-for-newline);

  write(stream, name); write(stream, ":\n");

  let code-index = 0;
  let lab-index = 0;
  let code-item-increment = be.code-item-increment;

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
      output-integer-code-item(outputter, code[index], code-item-increment);
    end for;

    // Emit code for any label
    if (label)
      output-code-label(outputter, label, be.labelled-constant-increment);
      code-index :=
        label-pos + labelled-constant-code-increment(be, label);
    else
      code-index := label-pos;
    end if;
  end while;

  maybe-reset-asm-line-pos(outputter);

end method;


define method output-integer-code-item
    (outputter :: <harp-gnu-as-outputter>,
     item :: <integer>,
     increment :: <integer>) => ()
  let stream = outputter.destination;
  if (at-asm-line-start?(outputter))
    select (increment)
      1 => write(stream, "\t.byte\t");
      2 => write(stream, "\t.short\t");
    end;
  else
    write-element(stream, byte-for-comma);
  end;
  write-integer(stream, item);
  increment-asm-line-pos(outputter);
end method;


define method output-relative-address
    (outputter :: <harp-gnu-as-outputter>,
     item :: <relative-address-constant>,
     increment :: <integer>,
     #key attr :: <byte-string> = "") => ()

  let offset = item.relative-offset;
  let stream = outputter.destination;

  maybe-reset-asm-line-pos(outputter);
  select (increment)
    4 => write(stream, "\t.long\t.");
    2 => write(stream, "\t.short\t.");
  end;

  if (offset = 0) #f
  elseif (offset < 0)
    write(stream, " - "); write-integer(stream, - offset);
  else
    write(stream, " + "); write-integer(stream, offset);
  end if;
  unless (attr.empty?) write(stream, attr) end;
  write-element(stream, byte-for-newline);
end method;

define method output-code-label
    (outputter :: <harp-gnu-as-outputter>,
     item :: <relative-address-constant>,
     increment :: <integer>) => ()
  output-relative-address(outputter, item, increment);
end method;

define method output-code-label
    (outputter :: <harp-gnu-as-outputter>,
     item :: <labelled-absolute-constant>,
     increment :: <integer>) => ()
  output-code-label-internal(outputter, item, increment);
end method;

define method output-code-label
    (outputter :: <harp-gnu-as-outputter>,
     item :: <labelled-absolute-thread-constant>,
     increment :: <integer>) => ()
  output-code-label-internal(outputter, item, increment, attr: "@ntpoff");
end method;


define method output-code-label
    (outputter :: <harp-gnu-as-outputter>,
     item :: <labelled-relative-constant>,
     increment :: <integer>) => ()
  let adjust = 4; // allow for the 4 bytes of the constant itself
  output-code-label-internal(outputter, item, increment,
                             attr: " - .", adjust: adjust);
end method;


define method output-code-label
    (outputter :: <harp-gnu-as-outputter>,
     item :: <labelled-constant-with-opcode>,
     increment :: <integer>)
  output-code-label-internal(outputter, item, increment,
                             directive: as(<byte-string>, item.opcode));
end method;


define method output-code-label-internal
    (outputter :: <harp-gnu-as-outputter>,
     item :: <labelled-constant>,
     increment :: <integer>,
     #key attr :: <byte-string> = "",
          adjust :: <integer> = 0,
          directive = select (increment)
                        4 => ".long";
                        2 => ".short";
                      end,
          intervene = #f) => ()

  let stream = outputter.destination;
  let cr = item.labelled-constant-reference;
  let ref =
    imported-name(outputter, cr.cr-refers-to,
                  instance?(cr, <imported-constant-reference>));
  let offset = cr.cr-const-offset - adjust;

  maybe-reset-asm-line-pos(outputter);
  write-element(stream, byte-for-tab);
  write(stream, directive);
  write-element(stream, byte-for-tab);

  if (intervene)
    write(stream, intervene); write-element(stream, byte-for-space);
  end if;

  if (offset == 0)
    write(stream, ref); write(stream, attr);
  elseif (offset < 0)
    write(stream, ref); write(stream, " - ");
    write-integer(stream, - offset); write(stream, attr);
  else
    write(stream, ref); write(stream, " + ");
    write-integer(stream, offset); write(stream, attr);
  end if;
  write-element(stream, byte-for-newline);
end method;


define method output-glue-symbols
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>,
     #key data-start = $data-start-symbol,
          data-end = $data-end-symbol,
          variables-start = $vars-start-symbol,
          variables-end = $vars-end-symbol,
          objects-start = $objs-start-symbol,
          objects-end = $objs-end-symbol,
          fixup-start = $fixup-start-symbol,
          fixup-end = $fixup-end-symbol,
          import-start = $import-start-symbol,
          import-end = $import-end-symbol) => ()

  local method put-symbol-in-section
            (section, symbol, #key flags, alignment = 4)
          let flags = flags | dylan-data-flags(outputter);
          select-gnu-binary-section(outputter, section, flags: flags, alignment: alignment);
          add-binary-symbol-definition(outputter, symbol);
        end method;

  put-symbol-in-section($data-start-section, data-start);
  put-symbol-in-section($data-end-section, data-end);
  put-symbol-in-section($vars-start-section, variables-start);
  put-symbol-in-section($vars-end-section, variables-end);
  put-symbol-in-section($objs-start-section, objects-start);
  put-symbol-in-section($objs-end-section, objects-end);
  put-symbol-in-section($fixup-start-section, fixup-start,
                        alignment: 1, flags: fixup-flags(outputter));
  put-symbol-in-section($fixup-end-section, fixup-end,
                        alignment: 1, flags: fixup-flags(outputter));
  put-symbol-in-section($import-start-section, import-start,
                        alignment: 1, flags: fixup-flags(outputter));
  put-symbol-in-section($import-end-section, import-end,
                        alignment: 1, flags: fixup-flags(outputter));
end method;


define method output-header
      (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>) => ()
end method;


define method output-footer
      (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>) => ()
  let stream = outputter.destination;
  // Output directives section last because this contains code entries
  let section = outputter.directives-section;
  if (section)
    write-binary-section(stream, outputter.binary-file, section);
  end;
  // Set this flag to communicate completion to assembler
  outputter.finished-outputting? := #t;
end method;

define method output-code-start
      (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>) => ()
  // Fixup data
  add-imported-data-fixups(outputter);

  let stream = outputter.destination;

  // Sectioned data is all complete so can be emitted at this point
  write-binary-data(stream, outputter);

  flag-asm-line-start(outputter);
  output-implicit-externals(be, outputter);
end method;

define method output-data-start
      (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>) => ()
end method;


define method output-external
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, name :: <byte-string>,
     #key import?,
          model-object = unsupplied(),
     #all-keys)
     => ()
  let stream = outputter.destination;
  let name = canonical-data-object(name, model-object);
  write(stream, "\t.global ");
  write(stream, imported-name(outputter, name, import?));
  write-element(stream, byte-for-newline);
end method;

define method output-external
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>,
     name :: <constant-reference>, #rest all-keys, #key, #all-keys) => ()
  apply(output-external, be, outputter, name.cr-refers-to, all-keys);
end method;


define method output-public
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, name :: <byte-string>,
     #key model-object = unsupplied(),
          export? = and-force-dll-exports?(#t),
     #all-keys)
     => ()
  let stream = outputter.destination;
  let name = canonical-data-object(name, model-object);
  write(stream, "\t.global "); write(stream, name);
  write-element(stream, byte-for-newline);
  do-export(export?, outputter, name);
end method;

define method output-public
    (be :: <harp-back-end>,
     outputter :: <harp-gnu-as-outputter>,
     name :: <constant-reference>,
     #rest all-keys, #key, #all-keys) => ()
  apply(output-public, be, outputter, name.cr-refers-to, all-keys);
end method;


define method add-symbol-definition
    (outputter :: <harp-gnu-as-outputter>,
     name :: <byte-string>, model-object,
     #key section = outputter.current-section,
     #all-keys)
  // copy-to-section(section, "\n\n\t.align 4\n");
  copy-to-section(section, "\n\n");
  copy-to-section(section, name);
  copy-to-section(section, ":");
  align-section-data(section);
end method;

define method output-definition
    (be :: <harp-back-end>,
     outputter :: <harp-gnu-as-outputter>,
     name :: <byte-string>,
     #key section, public?,
          export? = public?.and-force-dll-exports?,
          model-object = unsupplied(),
     #all-keys) => ()
  let name = canonical-data-object(name, model-object);
  select-gnu-dylan-section(be, outputter, section);
  add-symbol-definition(outputter, name, model-object);
  if (public?) output-public(be, outputter, name) end;
  do-export(export?, outputter, name);
end method;

define method output-definition
    (be :: <harp-back-end>,
     outputter :: <harp-gnu-as-outputter>,
     name :: <constant-reference>,
     #rest all-keys, #key, #all-keys) => ()
  apply(output-definition, be, outputter, name.cr-refers-to, all-keys);
end method;


define method select-gnu-dylan-section
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, section)
 => ()
  select-dylan-section(outputter, section | #"data", be.code-item-increment);
end method;


define method select-gnu-binary-section
    (outputter :: <harp-gnu-as-outputter>, section,
     #rest keys)
 => ()
  apply(select-binary-section, outputter, section, keys);
end method;


define method output-comment
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, comment :: <string>)
     => ()
end method;

define method output-line-comment
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>, comment :: <string>)
     => ()
end method;


define method output-function-type
    (outputter :: <harp-gnu-as-outputter>, name :: <string>) => ()
end method;

define method output-function-type
    (outputter :: <harp-elf-as-outputter>, name :: <string>) => ()
  // ELF Outputter requires this to create PLT relocation type
  let stream = outputter.destination;
  write(stream, "\n\t.type "); write(stream, name); write(stream, ",@function");
end method;


define method output-data-footer
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>,
     name :: <constant-reference>,
     #rest keys,
     #key, #all-keys) => ()
  apply(output-data-footer, be, outputter, name.cr-refers-to, keys);
end method;

define method output-data-footer
    (be :: <harp-back-end>, outputter :: <harp-gnu-as-outputter>,
     name :: <byte-string>,
     #key model-object = unsupplied(),
     #all-keys) => ()
  let section = outputter.current-section;
  copy-to-section(section, "\n\t.align 4");
end method;

define method output-data-footer
    (be :: <harp-back-end>, outputter :: <harp-elf-as-outputter>,
     name :: <byte-string>,
     #key model-object = unsupplied(),
     #all-keys) => ()
  // ELF Outputter requires this to create PLT relocation type
  let section = outputter.current-section;
  let name :: <string> = canonical-data-object(name, model-object);

  copy-to-section(section, "\n\t.align 4");
  copy-to-section(section, "\n\t.type ");
  copy-to-section(section, name);
  copy-to-section(section, ",@object");

  copy-to-section(section, "\n\t.size ");
  copy-to-section(section, name);
  copy-to-section(section, ",. - ");
  copy-to-section(section, name);
end method;

define method do-export
    (export?, builder :: <harp-elf-as-outputter>, name :: <byte-string>) => ()
end method do-export;

// Functions to update the current position within the line


define constant max-ints-per-line = 15;


define method at-asm-line-start?
      (outputter :: <harp-gnu-as-outputter>) => (res :: <boolean>)
  outputter.outputter-line-position == 0
end method;


define method maybe-reset-asm-line-pos
      (outputter :: <harp-gnu-as-outputter>) => ()
  if (outputter.outputter-line-position ~= 0)
    outputter.outputter-line-position := 0;
    format(outputter.destination, "\n");
  end if;
end method;

define method reset-asm-line-pos
      (outputter :: <harp-gnu-as-outputter>) => ()
  outputter.outputter-line-position := 0;
  format(outputter.destination, "\n");
end method;

define method increment-asm-line-pos
      (outputter :: <harp-gnu-as-outputter>) => ()
  let pos = outputter.outputter-line-position;
  if (pos >= max-ints-per-line)
    reset-asm-line-pos(outputter);
  else
    outputter.outputter-line-position := pos + 1;
  end if;
end method;



// Functions to flag whether or not we are at the start of a line

define method flag-asm-line-start
      (outputter :: <harp-gnu-as-outputter>) => ()
  outputter.outputter-line-position := 0;
end method;


define inline method imported-name
    (outputter :: <harp-gnu-as-outputter>, name :: <byte-string>, import? :: <boolean>)
  => (imported-name :: <byte-string>)
  if (import?)
    $imported-name-mangler(name)
  else
    name
  end if
end method;

define inline method imported-name
    (outputter :: <harp-elf-as-outputter>, name :: <byte-string>, import? :: <boolean>)
  => (imported-name :: <byte-string>)
  name
end method;


// Extension of the sectioning protocol
//   "current-position" is the exact position in raw binary data terms
//   "raw-data-size" is the actual position in outputter stream terms

define method add-word-to-section
    (section :: <gnu-section>, data :: <integer>) => ()
  let pos = section.current-position;
  let new-size = pos + 4;
  section.current-position := new-size;
  copy-to-section(section, "\n\t.long ");
  copy-integer-to-section(section, data);
end method;

define method add-word-to-section
    (section :: <gnu-section>, data :: <double-integer>) => ()
  let pos = section.current-position;
  let new-size = pos + 4;
  section.current-position := new-size;
  copy-to-section(section, "\n\t.long ");
  copy-to-section(section,
                  machine-word-to-string(%double-integer-low(data),
                                         prefix: "0x"));
end method;

define method add-word-to-section
    (section :: <gnu-section>, data :: <byte-string>) => ()
  let pos = section.current-position;
  let new-size = pos + 4;
  section.current-position := new-size;
  copy-to-section(section, "\n\t.long ");
  copy-to-section(section, data);
end method;

define method add-short-to-section
    (section :: <gnu-section>, data :: <integer>) => ()
  let pos = section.current-position;
  let new-size = pos + 2;
  section.current-position := new-size;
  copy-to-section(section, "\n\t.short ");
  copy-integer-to-section(section, data);
end method;

define method add-byte-to-section
    (section :: <gnu-section>, data :: <integer>) => ()
  let pos = section.current-position;
  let new-size = pos + 1;
  section.current-position := new-size;
  copy-to-section(section, "\n\t.byte ");
  copy-integer-to-section(section, data);
end method;


define method add-string-to-section
    (section :: <gnu-section>, string :: <byte-string>) => ()
  let pos = section.current-position;
  let len = string.size;
  section.current-position := pos + len;
  add-assembler-string-to-section(section, string, 0);
end method;

define method unreadable-character? (ch :: <character>) => (r :: <boolean>)
  // Returns true for characters which are invalid in GNU's string syntax
  (ch < ' ') | (ch > '~') | (ch == '"') | (ch == '\\')
end method;

define inline method find-key-from-start
    (string :: <byte-string>, predicate? :: <function>, start :: <integer>)
 => (key :: false-or(<integer>))
  let size :: <integer> = string.size;
  let pos :: <integer> = start;
  while ((pos < size) & ~ predicate?(string[pos]))
    pos := pos + 1;
  end while;
  if (pos < size)
    pos
  end;
end method;

// Propagate string positions to avoid consing substrings

define method add-assembler-string-to-section
    (section :: <gnu-section>, string :: <byte-string>, start :: <integer>) => ()
  copy-to-section(section, "\n\t");
  let badch = find-key-from-start(string, unreadable-character?, start);
  if (badch)
    unless (badch == start)
      copy-to-section(section, ".ascii \"");
      copy-to-section(section, string, start: start, end: badch);
      copy-to-section(section, "\"\n\t");
    end unless;
    copy-to-section(section, ".byte ");
    copy-integer-to-section(section, as(<integer>, string[badch]));
    if (badch ~= (string.size - 1))
      add-assembler-string-to-section(section, string, badch + 1);
    end if;
  else
    // string is readable, so write it directly
    copy-to-section(section, ".ascii \"");
    copy-to-section(section, string, start: start);
    copy-to-section(section, "\"");
  end if;
end method;

define method add-data-vector
    (outputter :: <harp-gnu-as-outputter>, vector :: <byte-vector>,
     #key section = outputter.current-section)
    => ()
  let stream = outputter.destination;
  for (byte :: <integer> in vector)
    write(stream, "\n\t.byte "); write-integer(stream, byte);
  end for;
end method;

define method add-data-vector
    (outputter :: <harp-gnu-as-outputter>, vector :: <simple-integer-vector>,
     #key section = outputter.current-section)
    => ()
  let stream = outputter.destination;
  for (word :: <integer> in vector)
    write(stream, "\n\t.short "); write-integer(stream, word);
  end for;
end method;

define method copy-to-section
    (section :: <gnu-section>, string :: <byte-string>,
     #key start :: <integer> = 0, end: _end :: <integer> = string.size) => ()
  let pos = section.raw-data-size;
  let len = _end - start;
  let new-pos = pos + len;
  ensure-size-of-section-data(section, new-pos);
  section.raw-data-size := new-pos;
  copy-bytes(section.section-data, pos, string, start, len);
end method;

define method copy-integer-to-section
    (section :: <gnu-section>, integer :: <integer>) => ()

  let negative? = integer < 0;
  let integer = if (negative?) - integer else integer end;
  let pos :: <integer> = section.raw-data-size;

  iterate process-integer (integer :: <integer> = integer, index :: <integer> = 1)
    let (quotient :: <integer>, remainder :: <integer>) = truncate/(integer, 10);

    if (quotient = 0)
      let len =
        if (negative?) index + 1 else index end;
      let new-pos = pos + len;
      ensure-size-of-section-data(section, new-pos);
      section.raw-data-size := new-pos;
      without-bounds-checks
        if (negative?)
          section.section-data[pos] := byte-for-minus;
          pos := pos + 1;
        end if;
        section.section-data[pos] := byte-for-zero + remainder;
        pos := pos + 1;
      end;
    else
      process-integer(quotient, index + 1);
      without-bounds-checks
        section.section-data[pos] := byte-for-zero + remainder;
        pos := pos + 1;
      end;
    end if;

  end iterate;

end method;


define method add-data
    (outputter :: <harp-gnu-as-outputter>,
     name :: <byte-string>, model-object,
     #key section = outputter.current-section,
          type = #"absolute",
          relocation-class = #f, relocation-type = #f,
          position-relative? = #f, // when true, want offset relative to
                                   // current position, using name as base
          offset = 0)
    => ()
  let name = canonical-data-object(name, model-object);
  add-word-to-section(section, name);
end method;

define method add-fixup-data
    (outputter :: <harp-gnu-as-outputter>, name, model-object)
    => ()
  let name = canonical-data-object(name, model-object);
  add-word-to-section(outputter.current-section,
                      imported-name(outputter, name, #t));
end method;

// For ELF, add real data for imports; the Linker will automatically
// handle any fixups; nothing extra is required here

define method add-imported-data
    (outputter :: <harp-elf-as-outputter>,
     item :: <byte-string>,
     model-object, offset) => ()
  add-data(outputter, item, model-object);
end method;

define method fill-section-data
     (section :: <gnu-section>, fill :: <integer>, #key, #all-keys)
     => ()
  #f
end method;


define method write-binary-section
    (stream :: <stream>, binary-file :: <binary-file>, section :: <gnu-section>)
     => ()
  write-binary-section-header(stream, section);
  write(stream, section.section-data, end: section.raw-data-size);
  write(stream, "\n\n");
end method;

define method write-binary-section-header
     (stream :: <stream>, section :: <gnu-section>) => ()
  write(stream, "\n\t.section ");
  write(stream, section.section-name);
  unless (section.section-flags.empty?)
    let flags = choose(curry(\~=, 'p'), section.section-flags);
    write(stream, concatenate(", \"", flags, "\""));

    if (member?('p', section.section-flags))
      write(stream, ", @progbits");
    end if;
  end unless;
  write-element(stream, byte-for-newline);
  write(stream, "\t.align 4\n");
end method;


define method make-binary-section
    (builder :: <harp-gnu-as-outputter>, name :: <byte-string>,
     alignment :: <integer>, flags)
    => (new :: <gnu-section>)
  make(<gnu-section>,
       name: name,
       alignment: alignment,
       flags: flags);
end method;



// GNU Assembler Section flags

define constant $data-flags  = "aw";  // Allocatable, writable
define constant $fixup-flags = "a";   // Allocatable
define constant $code-flags  = "axp"; // Allocatable, executable, @progbits

define constant $directives-flags = "n";
define constant $null-flags = "";


// Standard GNU section flags are set by default, so only need specify
// non-empty flags for non-standard sections

define inline method directives-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $directives-flags
end method;

define inline method directives-flags(outputter :: <harp-elf-as-outputter>)
 => (flags)
  $null-flags
end method;

define inline method data-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $null-flags
end method;

define inline method dylan-data-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $data-flags
end method;

define inline method dylan-data-flags(outputter :: <harp-elf-as-outputter>)
 => (flags)
  $data-flags
end method;

define inline method code-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $null-flags
end method;

define inline method init-code-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $code-flags
end method;

define inline method init-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $null-flags
end method;

define inline method fixup-flags(outputter :: <harp-gnu-as-outputter>)
 => (flags)
  $fixup-flags
end method;

define inline method fixup-flags(outputter :: <harp-elf-as-outputter>)
 => (flags)
  $null-flags
end method;


define constant byte-for-newline = as(<integer>, '\n');
define constant byte-for-tab = as(<integer>, '\t');
define constant byte-for-comma = as(<integer>, ',');
define constant byte-for-space = as(<integer>, ' ');
define constant byte-for-minus = as(<integer>, '-');
define constant byte-for-zero = as(<integer>, '0');


// write constituent byte-chars of integer directly to outputter stream

define method write-integer(stream :: <stream>, integer :: <integer>)
  let negative? = integer < 0;
  let integer =
    if (negative?)
      write-element(stream, byte-for-minus);
      - integer
    else integer
    end;

  iterate process-integer (integer :: <integer> = integer)
    let (quotient :: <integer>, remainder :: <integer>) = truncate/(integer, 10);

    if (quotient = 0)
      write-element(stream, byte-for-zero + remainder);
    else
      process-integer(quotient);
      write-element(stream, byte-for-zero + remainder);
    end if;

  end iterate;

end method;
