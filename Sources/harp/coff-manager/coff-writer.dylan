module:    coff-writer
Synopsis:  Support for dumping COFF files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define generic write-coff-header
    (stream :: <stream>, coff-file :: <coff-file>) => ();

define generic write-section-table 
    (stream :: <stream>, coff-file :: <coff-file>) => ();

define generic write-relocations
    (stream :: <stream>, coff-file :: <coff-file>, section :: <coff-section>)
    => ();

define generic write-line-numbers 
    (stream :: <stream>, coff-file :: <coff-file>, section :: <coff-section>)
    => ();

define generic write-symbol-table 
    (stream :: <stream>, coff-file :: <coff-file>) => ();

define generic write-string-table 
    (stream :: <stream>, coff-file :: <coff-file>) => ();

define generic write-byte
    (stream :: <stream>, coff-file :: <coff-file>, byte :: <object>) => ();

define generic write-short
    (stream :: <stream>, coff-file :: <coff-file>, short :: <coff-short>) => ();

define generic write-word 
    (stream :: <stream>, coff-file :: <coff-file>, word :: <coff-word>) => ();




define method write-binary (stream :: <stream>, coff-file :: <coff-file>) => ()
  write-coff-header  (stream, coff-file);
  write-section-table(stream, coff-file);
  write-section-data (stream, coff-file);
  write-symbol-table (stream, coff-file);
  write-string-table (stream, coff-file);
  values();
end method;


define method write-coff-header
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  write-short(stream, coff-file, coff-file.machine);
  write-short(stream, coff-file, coff-file.sections.ordered-data.size);  
  write-word (stream, coff-file, coff-file.time-stamp);
  write-word (stream, coff-file, coff-file.symbol-table-offset);
  write-word (stream, coff-file, coff-file.symbols.ordered-data.size);  
  write-short(stream, coff-file, coff-file.header-size);
  write-short(stream, coff-file, coff-file.characteristics);
end method;


define method write-section-table
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  let data-offset = coff-file.first-section-offset;
  for (section :: <coff-section> in coff-file.sections.ordered-data)
    write-coff-section-header(stream, coff-file, section, data-offset);
    data-offset := data-offset + section.unit-size;
  end for;
end method;

define method write-coff-section-header
     (stream :: <stream>, coff-file :: <coff-file>, 
      section :: <coff-section>, base :: <integer>) => ()
  let base-of-raw-data = base;
  let reloc-size = section.relocations.size;
  let lines-size = section.line-numbers.size;
  let base-of-relocs = base-of-raw-data + section.section-data-size-in-file;
  let base-of-lines  = base-of-relocs + section.total-relocation-size;
  section.linenumbers-pointer := base-of-lines; // need this later
  write-section-name(stream, coff-file, section.section-name);
  write-word(stream, coff-file, section.virtual-size);
  write-word(stream, coff-file, section.rva-offset);
  write-word(stream, coff-file, section.section-data-size-in-image);
  write-word(stream, coff-file, base-of-raw-data);
  write-word(stream, coff-file, if (reloc-size > 0) base-of-relocs else 0 end); 
  write-word(stream, coff-file, if (lines-size > 0) base-of-lines  else 0 end); 
  write-short(stream, coff-file, reloc-size);
  write-short(stream, coff-file, lines-size);
  write-word(stream, coff-file, section.section-flags);
end method;


define method write-section-name
     (stream :: <stream>, coff-file :: <coff-file>, 
      name :: <coff-short-string>) => ()
  write-short-string(stream, coff-file, name);
end method;

define constant byte-for-slash = as(<coff-byte>, '/');
define constant byte-for-zero = as(<coff-byte>, '0');

define method write-section-name
     (stream :: <stream>, coff-file :: <coff-file>, 
      name :: <coff-long-string>) => ()
  let ascii-offset = format-to-string("%d", name.index);
  let padding-size = 7 - ascii-offset.size;
  write-element(stream, byte-for-slash);
  for (i from 0 below padding-size) write-element(stream, byte-for-zero) end;
  write(stream, ascii-offset);
end method;


define method write-section-data
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  for (section :: <coff-section> in coff-file.sections.ordered-data)
    write-binary-section(stream, coff-file, section);
  end for;
end method;

define method write-binary-section
    (stream :: <stream>, coff-file :: <coff-file>, section :: <coff-section>)
     => ()
  // raw-data-size is usually a fill-pointer into the section data
  // so don't write any more than that. But for bss sections, the
  // section-data will actually be empty - so we don't want to write anything.
  let amount = min(section.raw-data-size, section.section-data.size);
  write(stream, section.section-data, end: amount);
  write-relocations(stream, coff-file, section);
  write-line-numbers (stream, coff-file, section);
end method;

define method write-relocations
    (stream :: <stream>,  coff-file :: <coff-file>, section :: <coff-section>)
    => ()
  for (reloc :: <coff-relocation> in section.relocations)
    write-relocation(stream, coff-file, reloc);
  end for;
end method;

define method write-relocation
    (stream :: <stream>, coff-file :: <coff-file>, reloc :: <coff-relocation>)
     => ()
  let sym = reloc.relocation-symbol;
  let sym-index = sym.index;
  if (sym-index < 0)
    error("Internal linker error: relocation to undefined symbol %=",
	  sym.symbol-name.string-data);
  end if;
  write-word(stream, coff-file, reloc.index);
  write-word(stream, coff-file, sym-index);
  write-short(stream, coff-file, reloc.relocation-type);
end method;

define method write-relocation
    (stream :: <stream>, coff-file :: <coff-file>, reloc :: <coff-interactor-relocation>)
     => ()
  // Interactor relocations have no meaning in written coff files
  // But rather than crash writing them, try and write something
  // vaguely useful
  write-word(stream, coff-file, reloc.index);
  write-word(stream, coff-file, $sym-undefined);
  write-short(stream, coff-file, reloc.relocation-type);
end method;


define method write-line-numbers
    (stream :: <stream>, coff-file :: <coff-file>, section :: <coff-section>)
    => ()
  // While writing the line numbers, fill in their file offset
  let file-offset = section.linenumbers-pointer;
  for (line :: <coff-line-number> in section.line-numbers)
    line.line-file-offset := file-offset;
    write-line-number(stream, coff-file, line);
    file-offset := file-offset + line.unit-size;
  end for;
end method;

define method write-line-number
    (stream :: <stream>, 
     coff-file :: <coff-file>, 
     line :: <coff-line-number-symbol>)
     => ()
  write-word(stream, coff-file, line.line-number-symbol.index);
  write-short(stream, coff-file, 0);
end method;

define method write-line-number
    (stream :: <stream>, 
     coff-file :: <coff-file>, 
     line :: <coff-line-number-relative>)
     => ()
  write-word(stream, coff-file, line.line-number-rva);
  write-short(stream, coff-file, line.line-number);
end method;


define method write-symbol-table
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  for (symbol :: <coff-symbol-record> in coff-file.symbols.ordered-data)
    write-one-symbol(stream, coff-file, symbol);
  end for;
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-symbol>) => ()
  write-symbol-name(stream, coff-file, symbol.symbol-name);
  write-word (stream, coff-file, symbol.symbol-value);
  write-short(stream, coff-file, symbol.section.symbol-locator-section-number);
  write-short(stream, coff-file, symbol.symbol-type);
  write-byte (stream, coff-file, symbol.storage-class);
  write-byte (stream, coff-file, symbol.aux-symbols.size);
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-auxiliary-symbol>) => ()
  error("Attempt to write an unknown auxiliary symbol.");
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-plain-auxiliary-symbol>) => ()
  write(stream, symbol.auxiliary-data);
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-empty-auxiliary-symbol>) => ()
  // That's right - do nothing
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-string-auxiliary-symbol>) => ()
  let string = symbol.auxiliary-string; 
  let padding =  size-of-symbol - 1 - modulo(string.size - 1, size-of-symbol);
  write(stream, string);
  for (i from 1 to padding)
    write-byte (stream, coff-file, 0);
  end for;
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-section-auxiliary-symbol>) => ()
  let section = symbol.auxiliary-section;
  write-word (stream, coff-file, section.section-data-size-in-image);
  write-short(stream, coff-file, section.relocations.size);
  write-short(stream, coff-file, section.line-numbers.size);
  write-word (stream, coff-file, symbol.auxiliary-check-sum);
  write-short(stream, coff-file, symbol.auxiliary-number);
  write-byte (stream, coff-file, symbol.auxiliary-selection);
  write-byte (stream, coff-file, 0); // padding
  write-short(stream, coff-file, 0); // padding
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-function-definition-auxiliary-symbol>) => ()
  write-word (stream, coff-file, symbol.auxiliary-tag.index);
  write-word (stream, coff-file, symbol.auxiliary-total-size);
  write-word (stream, coff-file, 
              symbol.auxiliary-line-numbers.line-file-offset);
  write-word (stream, coff-file, symbol.next-function-index);
  write-short(stream, coff-file, 0); // padding
end method;


define method write-one-symbol
     (stream :: <stream>, 
      coff-file :: <coff-file>, 
      symbol :: <coff-function-lines-auxiliary-symbol>) => ()
  write-word (stream, coff-file, 0); // padding
  write-short(stream, coff-file, symbol.auxiliary-line-number);
  write-word (stream, coff-file, 0); // padding
  write-short(stream, coff-file, 0); // padding
  write-word (stream, coff-file, symbol.next-function-index);
  write-short(stream, coff-file, 0); // padding
end method;


define method symbol-locator-section-number 
    (section :: <coff-section>) => (num :: <integer>)
  section.index;
end method;


define method symbol-locator-section-number 
    (section :: <coff-undefined-locator>) => (num :: <integer>)
  section.index;
end method;


define method write-symbol-name
     (stream :: <stream>, coff-file :: <coff-file>, 
      name :: <coff-short-string>) => ()
  write-short-string(stream, coff-file, name);
end method;

define method write-symbol-name
     (stream :: <stream>, coff-file :: <coff-file>, 
      name :: <coff-long-string>) => ()
  write-word(stream, coff-file, 0);
  write-word(stream, coff-file, name.index);
end method;




define method write-string-table
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  write(stream, coff-file.strings.ordered-data);
end method;


define method write-short-string 
    (stream :: <stream>, coff-file :: <coff-file>, 
     string :: <coff-short-string>) => ()
  let name = string.string-data;
  let padding-size = 8 - name.size;
  write(stream, name);
  for (i from 0 below padding-size)
    write-byte(stream, coff-file, 0);
  end for;
end method;


define method write-byte 
    (stream :: <stream>, coff-file :: <coff-file>, byte :: <coff-byte>) => ()
  write-element(stream, byte);
end method;

define method write-byte 
    (stream :: <stream>, coff-file :: <coff-file>, byte :: <character>) => ()
  write-element(stream, as(<coff-byte>, byte));
end method;

define method write-short 
    (stream :: <stream>, coff-file :: <coff-file>, short :: <coff-short>) => ()
  let b0 = logand(short, #xff);
  let w0 = ash(short, -8);
  let b1 = logand(w0, #xff);
  if (coff-file.big-endian?)
    write-element(stream, b1); write-element(stream, b0); 
  else
    write-element(stream, b0); write-element(stream, b1); 
  end if;
end method;

define method write-word 
    (stream :: <stream>, coff-file :: <coff-file>, word :: <coff-word>) => ()
  if (word.zero?)
    write-element(stream, 0); write-element(stream, 0); 
    write-element(stream, 0); write-element(stream, 0); 
  else
    let b0 :: <integer> = generic-logand(word, #xff);
    let w0 :: <integer> = generic-ash(word, -8);
    let b1 = logand(w0, #xff);
    let w1 = ash(w0, -8);
    let b2 = logand(w1, #xff);
    let w2 = ash(w1, -8);
    let b3 = logand(w2, #xff);
    if (coff-file.big-endian?)
      write-element(stream, b3); write-element(stream, b2); 
      write-element(stream, b1); write-element(stream, b0); 
    else
      write-element(stream, b0); write-element(stream, b1); 
      write-element(stream, b2); write-element(stream, b3); 
    end if;
  end if;
end method;
