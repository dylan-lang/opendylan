module:    coff-reader
Synopsis:  Support for reading COFF files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define open generic read-coff-file (file) => (data :: <coff-file>);

define open generic make-coff-file
   (machine :: <coff-short>) => (coff-file :: <coff-file>);



define method read-coff-file (file :: <string>) =>  (data :: <coff-file>)
  read-coff-file(as(<file-locator>, file));
end method;

define method read-coff-file (file :: <locator>) =>  (data :: <coff-file>)
  let stream = make(<file-stream>, locator: file);
  block ()
    read-coff-file(stream);
  cleanup
    close(stream);
  end block;
end method;

define method read-coff-file (stream :: <stream>) =>  (data :: <coff-file>)
  let machine = read-byte(stream) + (#x100 * read-byte(stream));
  let coff-file = make-coff-file(machine);
  let number-of-sections     =  read-short(stream, coff-file);
  coff-file.time-stamp      :=  read-word (stream, coff-file);
  let symbol-table-offset    =  read-word (stream, coff-file);
  let number-of-symbols      =  read-word (stream, coff-file);
  coff-file.header-size     :=  read-short(stream, coff-file);
  coff-file.characteristics :=  read-short(stream, coff-file);
  let string-table-offset = 
     symbol-table-offset + (number-of-symbols * size-of-symbol);
  read-coff-strings(stream, coff-file, string-table-offset);
  read-coff-sections(stream, coff-file, number-of-sections);
  read-coff-symbols(stream, coff-file, symbol-table-offset, number-of-symbols);
  read-coff-section-data(stream, coff-file);
  read-coff-relocations-and-line-numbers(stream, coff-file);
  fixup-coff-symbols(coff-file);
  coff-file;
end method;


define method read-coff-strings
    (stream :: <stream>, coff-file :: <coff-file>, offset :: <integer>) => ()

  stream.stream-position := offset;     // get to the right place in the file
  let length = read-word(stream, coff-file); 
  let start = 4;   // skip the 4 bytes of length info
  let whole-string = make(<string>, size: length);

  stream.stream-position := offset;     // get back to the right place
  read-into!(stream, length, whole-string);

  let the-strings = coff-file.strings;
  the-strings.ordered-data := whole-string;
  let nul = as(<character>, 0);
  while (start < length)
    for (i from start, while: whole-string[i] ~= nul)
    finally
      let str = copy-sequence(whole-string, start: start, end: i);
      let coff-str = make(<coff-long-string>, string: str, index: start);
      coff-element(the-strings, str) := coff-str;
      start := i + 1;
    end for;
  end while;
end method;


define method read-coff-sections
    (stream :: <stream>, coff-file :: <coff-file>, number :: <integer>) => ()
  let section-start = coff-file.file-headers-size;
  let section-size = 40;
  for (file-pos from section-start by section-size, count from 1 to number)
    stream.stream-position := file-pos;
    let section = read-a-coff-section(stream, coff-file);
    let name = section.section-name.string-data;
    coff-element-add!(coff-file.sections, name, section);
  end for;
end method;


define constant $empty-byte-vector = make(<byte-vector>, size: 0);

define method read-a-coff-section
    (stream :: <stream>, coff-file :: <coff-file>) => (s :: <coff-section>)
  make-a-coff-section
      (name:               read-section-name(stream, coff-file),
       virtual-size:       read-word(stream, coff-file),
       rva-offset:         read-word(stream, coff-file),
       raw-data-size:      read-word(stream, coff-file),
       raw-data-pointer:   read-word(stream, coff-file),
       relocs-pointer:     read-word(stream, coff-file),
       lines-pointer:      read-word(stream, coff-file),
       relocs-number:      read-short(stream, coff-file),
       lines-number:       read-short(stream, coff-file),
       flags:              read-word(stream, coff-file),
       data:               $empty-byte-vector);
end method;


define method make-a-coff-section 
    (#rest keys, #key flags, #all-keys) => (s :: <coff-section>)
  let alignment = alignment-from-flags(flags);
  apply(make, <coff-section>, alignment: alignment, keys);
end method;


define method alignment-from-flags (flags :: <coff-word>) => (alignment :: <integer>)
  let masked-flags :: <integer> = generic-logand(flags, $align-64bytes);
  select (masked-flags)
    $align-1bytes  => 1;
    $align-2bytes  => 2;
    $align-4bytes  => 4;
    $align-8bytes  => 8;
    $align-16bytes => 16;
    $align-32bytes => 32;
    $align-64bytes => 64;
    otherwise => 4;
  end select;
end method;


define method read-section-name
    (stream :: <stream>, coff-file :: <coff-file>) => (name :: <coff-string>)
  let name-field-size = 8;
  let name-field = make(<byte-string>, size: name-field-size);
  read-into!(stream, name-field-size, name-field);
  if (name-field[0] == '/')
    error("Found a long section name. Format is ~s", name-field);
  else
    coff-string-from-name-field(name-field);
  end if;
end method;


define method read-coff-symbols
    (stream :: <stream>, coff-file :: <coff-file>, 
     offset :: <integer>, number :: <integer>) => ()
  stream.stream-position := offset;
  let to-be-read = number;
  while (to-be-read > 0)
    to-be-read := to-be-read - read-coff-symbol(stream, coff-file);
  end while;
end method;


define method read-coff-symbol
      (stream :: <stream>, coff-file :: <coff-file>) 
   => (num-read :: <integer>)
  let (symbol, aux-num, section) = read-a-normal-symbol(stream, coff-file);
  if (aux-num > 0)
    if (symbol.storage-class = $sym-file)
      // It's a .file symbol followed by the file name
      read-string-auxiliary-symbols(stream, coff-file, symbol, aux-num);
    elseif (aux-num = 1 & symbol.symbol-type = $sym-type-function)
      // It's a function definition symbol followed by line number info
      read-function-definition-auxiliary-symbol(stream, coff-file, symbol, section);
    elseif (symbol.storage-class = $sym-function & aux-num = 1)
      // It's a .bf or .ef symbol
      read-function-lines-auxiliary-symbol(stream, coff-file, symbol);
    elseif (symbol.storage-class = $sym-static & aux-num = 1)
      // It's a section definition
      read-section-auxiliary-symbol(stream, coff-file, symbol);
    else
      // auxiliary symbols of an unknown type
      read-plain-auxiliary-symbols(stream, coff-file, symbol, aux-num);
    end if;
  end if;
  1 + aux-num;
end method;


define method read-a-normal-symbol
      (stream :: <stream>, coff-file :: <coff-file>) 
   => (symbol :: <coff-symbol>, aux-num :: <integer>, section :: <coff-symbol-locator>)
  let name =    read-symbol-name(stream, coff-file);
  let value =   read-word(stream, coff-file);
  let section = read-symbol-section(stream, coff-file);
  let symbol = 
    make(<coff-symbol>, 
         name:          name,
         value:         value,
         section:       section,
         type:          read-short(stream, coff-file),
         storage-class: read-byte(stream),
         aux-symbols:   make(<stretchy-vector>));
  coff-element-add!(coff-file.symbols, name.string-data, symbol);
  let aux-num = read-byte(stream);
  values(symbol, aux-num, section);
end method;


define method read-section-auxiliary-symbol
      (stream :: <stream>, coff-file :: <coff-file>, symbol :: <coff-symbol>) 
   => ()
  read-word(stream, coff-file);  // size of section data (redundant)
  read-word(stream, coff-file);  // num of relocs and lines (redundant)
  let check-sum = read-word(stream, coff-file);
  let number    = read-short(stream, coff-file);
  let selection = read-byte(stream);
  let section = coff-element(coff-file.sections, symbol.symbol-name.string-data);
  let aux = make(<coff-section-auxiliary-symbol>, 
                 section: section,
                 check-sum: check-sum,
                 number: number,
                 selection: selection);
  read-byte(stream);             // read the padding
  read-short(stream, coff-file); // read the padding
  add-auxiliary-symbol(coff-file, symbol, aux);
end method;


define method read-function-definition-auxiliary-symbol
      (stream :: <stream>, coff-file :: <coff-file>, 
       symbol :: <coff-symbol>, section :: <coff-section>) 
   => ()
  let aux = 
    make(<coff-function-definition-auxiliary-symbol>, 
         tag: symbol, // dummy value for efficiency - it's a required parameter
         tag-index:           read-word(stream, coff-file),
         total-size:          read-word(stream, coff-file),
         line-numbers-offset: read-word(stream, coff-file),
         next-function-index: read-word(stream, coff-file));
  read-short(stream, coff-file); // read the padding
  add-auxiliary-symbol(coff-file, symbol, aux);
end method;


define method read-function-lines-auxiliary-symbol
      (stream :: <stream>, coff-file :: <coff-file>, symbol :: <coff-symbol>) 
   => ()
  read-word(stream, coff-file);  // unused data
  let line = read-short(stream, coff-file);
  read-word(stream, coff-file);  // unused data
  read-short(stream, coff-file); // unused data
  let next = read-word(stream, coff-file);
  read-short(stream, coff-file); // unused data
  let aux = 
    make(<coff-function-lines-auxiliary-symbol>, 
         line-number:         line,
         next-function-index: next);
  add-auxiliary-symbol(coff-file, symbol, aux);
end method;


define method read-string-auxiliary-symbols
      (stream :: <stream>, coff-file :: <coff-file>, 
       symbol :: <coff-symbol>, aux-num :: <integer>) 
   => ()
  let aux-size = aux-num * size-of-symbol;
  let data = make(<byte-string>, size: aux-size);
  read-into!(stream, aux-size, data);
  let base = remove(data, as(<character>, 0));
  let aux = make(<coff-string-auxiliary-symbol>, string: base);
  add-auxiliary-symbol(coff-file, symbol, aux);
  for (i from 1 below aux-num)
    add-auxiliary-symbol(coff-file, symbol, make(<coff-empty-auxiliary-symbol>));
  end for;
end method;


define method read-plain-auxiliary-symbols
      (stream :: <stream>, coff-file :: <coff-file>, 
       symbol :: <coff-symbol>, aux-num :: <integer>) 
   => ()
  for (i from 0 below aux-num)
    let data-field = make(<vector>, size: size-of-symbol);
    read-into!(stream, size-of-symbol, data-field);
    let aux = make(<coff-plain-auxiliary-symbol>, data: data-field);
    add-auxiliary-symbol(coff-file, symbol, aux);
  end for;
end method;


define method add-auxiliary-symbol
    (coff-file :: <coff-file>, symbol :: <coff-symbol>, 
     aux :: <coff-auxiliary-symbol>) 
 => ()
  coff-element-add!(coff-file.symbols, "dummy-name", aux);
  add!(symbol.aux-symbols, aux);
end method;


define method read-symbol-name
      (stream :: <stream>, coff-file :: <coff-file>) => (name :: <coff-string>)
  let start-pos = stream.stream-position;
  let zeros = read-word(stream, coff-file);
  if (zeros == 0)
    // We have found a long name - so look in strings table
    let offset = read-word(stream, coff-file);
    coff-string-at-index(coff-file, offset);
  else
    // we have found a short name - so reset the position and read it
    stream.stream-position := start-pos;
    let name-field-size = 8;
    let name-field = make(<byte-string>, size: name-field-size);
    read-into!(stream, name-field-size, name-field);
    coff-string-from-name-field(name-field);
  end if;
end method;


define method read-symbol-section
      (stream :: <stream>, coff-file :: <coff-file>) 
   => (s :: <coff-symbol-locator>)
  let section-number = read-signed-short(stream, coff-file);
  select (section-number)
     0        => coff-section-undefined;
    -1        => coff-section-sym-absolute;
    -2        => coff-section-sym-debug;
    otherwise => coff-file.sections.ordered-data[section-number - 1];
  end select;
end method;



define method fixup-coff-symbols (coff-file :: <coff-file>) => ()
  // Now that the line numbers have been read in, we can fix up the
  //  auxiliary symbols
  let last-section = #f;
  for (symbol in coff-file.symbols.ordered-data)
    if (instance?(symbol, <coff-symbol>))
      last-section := symbol.section;
    else
      fixup-coff-auxiliary-symbol(coff-file, last-section, symbol);
    end if;
  end for;
end method;
/// Symbols require an extra fixup stage after reading the line numbers.
/// This is done after the line numbers are read in.

define method fixup-coff-auxiliary-symbol
    (coff-file :: <coff-file>, 
     section,
     symbol :: <coff-auxiliary-symbol>) => ()
  // By default, there is no need to fixup
end method;

define method fixup-coff-auxiliary-symbol
    (coff-file :: <coff-file>, 
     section :: <coff-section>,
     symbol :: <coff-function-definition-auxiliary-symbol>) 
    => ()
  // Fixup the next-function reference, from the next-function-index
  symbol.auxiliary-next-function 
    := symbol-at-index(coff-file, symbol.auxiliary-next-function-index);
  // Fixup the tag reference, from the tag-index
  symbol.auxiliary-tag
    := symbol-at-index(coff-file, symbol.auxiliary-tag-index);
  // And fix up the line number reference
  symbol.auxiliary-line-numbers
    := line-at-offset(section, symbol.auxiliary-line-numbers-offset);
end method;

define method fixup-coff-auxiliary-symbol
    (coff-file :: <coff-file>, 
     section :: <coff-section>,
     symbol :: <coff-function-lines-auxiliary-symbol>) 
    => ()
  // Fixup the next-function reference, from the next-function-index
  symbol.auxiliary-next-function 
    := symbol-at-index(coff-file, symbol.auxiliary-next-function-index);
end method;


define method symbol-at-index (coff-file :: <coff-file>, index :: <integer>) => (res)
  if (index == 0)
    #f
  else
    coff-file.symbols.ordered-data[index];
  end if;
end method;





define method read-coff-section-data
    (stream :: <stream>, coff-file :: <coff-file>) => ()
  for (section in coff-file.sections.ordered-data)
    read-one-coff-section-data(stream, section);
  end for;
end method;


define method read-one-coff-section-data
    (stream :: <stream>, section :: <coff-section>) => ()
  let data-size = section.raw-data-size;
  let data = make(<byte-vector>, size: data-size);
  section.section-data := data;
  stream.stream-position := section.raw-data-pointer;
  read-into!(stream, data-size, data);
end method;


define method read-one-coff-section-data
    (stream :: <stream>, section :: <coff-bss-section>) => ()
  // Don't bother to read the section data for a bss section
end method;


define method read-coff-relocations-and-line-numbers
    (stream :: <stream>, coff-file :: <coff-file>) => ()
  for (section in coff-file.sections.ordered-data)
    read-relocations-for-section(stream, coff-file, section);
    read-line-numbers-for-section(stream, coff-file, section);
  end for;
end method;


define method read-line-numbers-for-section
    (stream :: <stream>, 
     coff-file :: <coff-file>, 
     section :: <coff-section>) => ()
  let lines = section.line-numbers;
  let symbols = coff-file.symbols.ordered-data;
  let file-pos = section.linenumbers-pointer;
  stream.stream-position := file-pos;
  for (count from 0 below section.linenumbers-number)
    let index-or-rva    = read-word(stream, coff-file);
    let number          = read-short(stream, coff-file);
    add!(lines, 
         if (number = 0)
           make(<coff-line-number-symbol>, 
                symbol: symbols[index-or-rva],
                offset: file-pos);
         else
           make(<coff-line-number-relative>, 
                rva: index-or-rva, 
                line-number: number,
                offset: file-pos);
         end if);
    file-pos := file-pos + 6;
  end for;
end method;


define method line-at-offset
    (section :: <coff-section>, file-offset :: <integer>) 
    => (line :: <coff-line-number>)
  block (return)
    for (line in section.line-numbers)
      if (line.line-file-offset == file-offset)
        return(line);
      end if;
    end for;
    error("Inconsistent COFF file - failed to find line number data.");
  end block;
end method;


define method read-relocations-for-section
    (stream :: <stream>, 
     coff-file :: <coff-file>, 
     section :: <coff-section>) => ()
  let relocs = section.relocations;
  let symbols = coff-file.symbols.ordered-data;
  stream.stream-position := section.relocs-pointer;
  for (count from 0 below section.relocs-number)
    let offset     = read-word(stream, coff-file);
    let sym-index  = read-word(stream, coff-file);
    let type       = read-short(stream, coff-file);
    let symbol     = symbols[sym-index];
    let class = if (type == #x14) 
                  <coff-relative-relocation> 
                else <coff-absolute-relocation>
                end;
    add!(relocs, make(class, 
                      index: offset, symbol: symbol, 
                      relocation-type: type));
  end for;
end method;



define method coff-string-at-index
       (coff-file :: <coff-file>, offset :: <integer>) 
    => (str :: <coff-long-string>)
  // start by finding the string from the ordered data, 
  // and then use this to key into the table
  let strings = coff-file.strings;
  let data = strings.ordered-data;
  let nul = as(<character>, 0);
  for (index from offset, while: data[index] ~== nul)
  finally
    let str = copy-sequence(data, start: offset, end: index);
    coff-element(strings, str);
  end for;
end method;



define method coff-string-from-name-field
       (name-field :: <byte-string>) 
    => (str :: <coff-short-string>)
  let nul = as(<character>, 0);
  let null-pos = find-key(name-field, curry(\==, nul));
  let name-string = if (null-pos) 
                      copy-sequence(name-field, end: null-pos);
                    else name-field;
                    end;
  make(<coff-short-string>, string: name-string);
end method;




define method make-coff-file
   (machine :: <coff-short>) => (coff-file :: <coff-file>)
  block ()
    error("Not a COFF file from a known machine: machine code is #X%x\n", 
          machine);
  exception (<simple-restart>, 
             init-arguments: vector(format-string: 
                                    "Process as big-endian."))
    make(<coff-file>, machine: machine, big-endian?: #t);
  exception (<simple-restart>, 
             init-arguments: vector(format-string: 
                                    "Process as little-endian."))
    make(<coff-file>, machine: machine, big-endian?: #f);
  end block;
end method;


define method make-coff-file
   (machine == #x14c) =>  (coff-file :: <coff-file>)
  // Intel Windows COFF file
  make(<coff-file>, machine: machine, big-endian?: #f);
end method;



define method read-char
    (stream :: <stream>, coff-file :: <coff-file>) => (char :: <character>)
  as(<character>, read-element(stream));
end method;


define method read-byte
    (stream :: <stream>) => (byte :: <coff-byte>)
  as(<integer>, read-element(stream));
end method;


define method read-signed-short
    (stream :: <stream>, coff-file :: <coff-file>) => (short :: <coff-short>)
  let num = read-short(stream, coff-file);
  if (num >= #x8000) num - #x10000 else num end if;
end method;


define method read-short
    (stream :: <stream>, coff-file :: <coff-file>) => (short :: <coff-short>)
  let w0 = read-byte(stream);
  let w1 = read-byte(stream);
  let (d0, d1) = if (coff-file.big-endian?)
                   values(w1, w0);
                 else
                   values(w0, w1);
                 end if;
  d0 + ash(d1, 8);
end method;



// TEMPORARY:  the Dylan reader is unable to create big-integer constants.
define constant $too-big  = generic-ash(#x8000, 16);
define constant $overflow = #x10000000;

define method read-signed-word
    (stream :: <stream>, coff-file :: <coff-file>) => (word :: <coff-word>)
  let num = read-word(stream, coff-file);
  if (num >= $too-big) generic--(num, $overflow) else num end if;
end method;


define method read-word 
    (stream :: <stream>, coff-file :: <coff-file>) => (word :: <coff-word>)
  let w0 = read-byte(stream);
  let w1 = read-byte(stream);
  let w2 = read-byte(stream);
  let w3 = read-byte(stream);
  let (d0, d1, d2, d3) = if (coff-file.big-endian?)
                           values(w3, w2, w1, w0);
                         else
                           values(w0, w1, w2, w3);
                         end if;
  generic-+(d0 + ash(d1, 8) + ash(d2, 16), generic-ash(d3, 24));
end method;
