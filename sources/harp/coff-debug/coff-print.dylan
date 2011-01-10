module:    coff-print
Synopsis:  Support for printing COFF objects and files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method print-object
   (object :: <coff-string>, stream :: <stream>) => ()
  let type = 
    select (object by instance?)
      <coff-long-string>  => "long-string";
      <coff-short-string> => "short-string";
    end select;
  format(stream, "{%s %=}", type, object.string-data);
end method;


define sideways method print-object
   (object :: <coff-symbol>, stream :: <stream>) => ()
  format(stream, "{coff-symbol %=}", object.symbol-name.string-data);
end method;

define sideways method print-object
   (object :: <coff-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-aux-symbol UNKOWN TYPE}");
end method;


define sideways method print-object
   (object :: <coff-plain-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-plain-auxiliary-symbol UNKOWN TYPE}");
end method;


define sideways method print-object
   (object :: <coff-empty-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-empty-auxiliary-symbol}");
end method;


define sideways method print-object
   (object :: <coff-string-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-string-aux-symbol %=}", object.auxiliary-string);
end method;


define sideways method print-object
   (object :: <coff-function-lines-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-function-lines-aux-symbol line %d}", 
         object.auxiliary-line-number);
end method;


define sideways method print-object
   (object :: <coff-section-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-section-aux-symbol %=}", 
         object.auxiliary-section.section-name.string-data);
end method;


define sideways method print-object
   (object :: <coff-function-definition-auxiliary-symbol>, 
    stream :: <stream>) => ()
  format(stream, 
         "{coff-function-definition-aux-symbol tag %d linenumbers %d}", 
         object.auxiliary-tag.index, 
         if (object.auxiliary-line-numbers)
           object.auxiliary-line-numbers.line-file-offset;
         else 0;
         end if);
end method;


define sideways method print-object
   (object :: <coff-section>, stream :: <stream>) => ()
  format(stream, "{coff-section %=}", object.section-name.string-data);
end method;


define sideways method print-object
   (object :: <coff-bss-section>, stream :: <stream>) => ()
  format(stream, "{coff-bss-section %=}", object.section-name.string-data);
end method;


define sideways method print-object
   (object :: <coff-undefined-locator>, stream :: <stream>) => ()
  format(stream, "{coff-undefined-section %d}", object.index);
end method;


define sideways method print-object
   (object :: <coff-relocation>, stream :: <stream>) => ()
  let (type, contents) = relocation-type-and-contents(object);
  format(stream, "{%s %= @ #X%x}", type, contents, object.index);
end method;


define method relocation-type-and-contents 
    (object :: <coff-absolute-relocation>)
     => (type :: <byte-string>, contents :: <byte-string>)
  values("coff-reloc", object.relocation-symbol.symbol-name.string-data);
end method;

define method relocation-type-and-contents 
    (object :: <coff-relative-relocation>)
     => (type :: <byte-string>, contents :: <byte-string>)
  values("coff-relative-reloc", object.relocation-symbol.symbol-name.string-data);
end method;

define method relocation-type-and-contents 
    (object :: <coff-interactor-relocation>)
     => (type :: <byte-string>, contents)
  values("coff-interactor-reloc", object.interactor-handle);
end method;


define sideways method print-object
   (object :: <coff-line-number-symbol>, 
    stream :: <stream>) => ()
  format(stream, "{coff-line-number-symbol %=}",
         object.line-number-symbol.symbol-name.string-data);
end method;


define sideways method print-object
   (object :: <coff-line-number-relative>, 
    stream :: <stream>) => ()
  format(stream, "{coff-line-number %d @ #X%x}",
         object.line-number, object.line-number-rva);
end method;


define method print-coff-file 
     (coff-file :: <coff-file>,
      #key 
       stream = *standard-output*, 
       verbose?        = #f,
       relocs?         = verbose?,
       lines?          = verbose?,
       symbols?        = verbose?,
       strings?        = verbose?,
       exports?        = verbose?,
       debug?          = verbose?,
       fixups-offsets? = verbose?,
       fixups?         = verbose?) 
      => ()
  format(stream, "\n++++ COFF file ++++\n\n");
  format(stream, "machine:            #X%x\n", coff-file.machine);
  format(stream, "time-stamp:         #X%x\n", coff-file.time-stamp);
  format(stream, "characteristics:    #X%x\n", coff-file.characteristics);
  format(stream, "number of sections: %d\n", coff-file.sections.ordered-data.size);
  format(stream, "number of symbols:  %d\n", coff-file.symbols.ordered-data.size);
  format(stream, "number of strings:  %d\n", coff-file.strings.table-data.size);
  print-coff-sections(stream, coff-file, relocs?: relocs?, lines?: lines?);
  if (symbols?) print-coff-symbols(stream, coff-file) end;
  if (strings?) print-coff-strings(stream, coff-file) end;
  if (exports?) print-exports(stream, coff-file) end;
  if (fixups? | fixups-offsets?)
    print-fixups(stream, coff-file, fixups-offsets?) 
  end;
  if (debug?)   print-debug-info(stream, coff-file) end;
  format(stream, "\n++++ end of COFF file ++++\n\n");
end method;


define method print-coff-sections 
     (stream :: <stream>, coff-file :: <coff-file>, #key relocs?, lines?) => ()
  for (section in coff-file.sections.ordered-data)
    let prefix = section.prefix-type;
    let name = section.section-name.string-data;
    format(stream, "\n%sSection %s:\n", prefix, name);
    format(stream, "   virtual size:       #X%x\n", section.virtual-size);
    format(stream, "   RVA/Offset:         #X%x\n", section.rva-offset);
    format(stream, "   raw data size:      #X%x\n", section.raw-data-size);
    format(stream, "   raw data pointer:   #X%x\n", section.raw-data-pointer);
    format(stream, "   number of relocs:   %d\n", section.relocations.size);
    format(stream, "   number of lines:    %d\n", section.line-numbers.size);
    format(stream, "   flags:              #X%x\n", section.section-flags);
    if (relocs?) print-section-relocations(stream, section)  end;
    if (lines?)  print-section-line-numbers(stream, section) end;
  end for;
end method;


define method prefix-type 
    (section :: <coff-section>) => (name :: <byte-string>)
  "";
end method;


define method prefix-type 
    (section :: <coff-bss-section>) => (name :: <byte-string>)
  "BSS ";
end method;


define method print-section-relocations
     (stream :: <stream>, section :: <coff-section>) => ()
  format(stream, "   Relocations:");
  if (section.relocations.empty?)
    format(stream, "        None\n");
  else
    format(stream, "\n");
    for (reloc in section.relocations)
      let the-type = 
        select (reloc by instance?)
          <coff-relative-relocation> => "Relative";
          <coff-absolute-relocation> => "Absolute";
        end select;
      format(stream, "       %s @ %s type: %s :: %=\n",
           the-type,
           pad-out(reloc.index,             6, hex: #t),
           pad-out(reloc.relocation-type,   4, hex: #t),
           reloc.relocation-symbol.symbol-name.string-data);
    end for;
  end if;
end method;


define method print-section-line-numbers
     (stream :: <stream>, section :: <coff-section>) => ()
  format(stream, "   Line Numbers:");
  if (section.line-numbers.empty?)
    format(stream, "       None\n");
  else
    format(stream, "\n");
    for (line in section.line-numbers)
      print-one-line-number(stream, line);
    end for;
  end if;
end method;


define method print-one-line-number 
    (stream :: <stream>, line :: <coff-line-number-symbol>) => ()
  format(stream, "       %=\n",
         line.line-number-symbol.symbol-name.string-data);
end method;


define method print-one-line-number 
    (stream :: <stream>, line :: <coff-line-number-relative>) => ()
  format(stream, "               Line %s @ %s\n",
         pad-out(line.line-number,  4),
         pad-out(line.line-number-rva, 6, hex: #t));
end method;


define method print-coff-symbols
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  format(stream, "\nSymbol table:\n");
  for (sym in coff-file.symbols.ordered-data)
    print-one-symbol(stream, sym);
  end for;
end method;


define method print-one-symbol
     (stream :: <stream>, symbol :: <coff-symbol>) => ()
  format(stream, "   value: %s section: %s type: %s class: %s aux: %d :: %=\n",
         pad-out(symbol.symbol-value,  6, hex: #t),
         pad-out(symbol.section.index, 2),
         pad-out(symbol.symbol-type,   4, hex: #t),
         pad-out(symbol.storage-class, 2, hex: #t),
         symbol.aux-symbols.size, 
         symbol.symbol-name.string-data);
end method;


define method print-one-symbol
     (stream :: <stream>, symbol :: <coff-auxiliary-symbol>) => ()
  format(stream, "        *   Unknown auxiliary symbol\n");
end method;


define method print-one-symbol
     (stream :: <stream>, symbol :: <coff-empty-auxiliary-symbol>) => ()
  // That's right. Do nothing!
end method;


define method print-one-symbol
     (stream :: <stream>, symbol :: <coff-string-auxiliary-symbol>) => ()
  format(stream, "        *   %=\n", symbol.auxiliary-string);
end method;


define method print-one-symbol
     (stream :: <stream>, symbol :: <coff-section-auxiliary-symbol>) => ()
  let section = symbol.auxiliary-section;
  format(stream, 
         "        *   len: %s relocs: %s lines-num: %s "
         "sum: %s num: %s selection: %s\n", 
         pad-out(section.raw-data-size,        6, hex: #t),
         pad-out(section.relocations.size,     4),
         pad-out(section.line-numbers.size,    4),
         pad-out(symbol.auxiliary-check-sum,   4),
         pad-out(section.index,                4),
         pad-out(symbol.auxiliary-selection,   2));
end method;


define method print-one-symbol
     (stream :: <stream>, 
      symbol :: <coff-function-definition-auxiliary-symbol>) 
  => ()
  let offset 
    = if (symbol.auxiliary-line-numbers)
        symbol.auxiliary-line-numbers.line-file-offset;
      else 0
      end if;
  format(stream, 
         "        *   tag: %s size: %s line-nums: %s next-fn: %s\n",
         pad-out(symbol.auxiliary-tag.index,     4),
         pad-out(symbol.auxiliary-total-size,    4),
         pad-out(offset,                         6, hex: #t),
         pad-out(symbol.next-function-index,     4));
end method;


define method print-one-symbol
     (stream :: <stream>,
      symbol :: <coff-function-lines-auxiliary-symbol>) 
  => ()
  format(stream, 
         "        *   line-number: %s next-fn: %s\n",
         pad-out(symbol.auxiliary-line-number,   4),
         pad-out(symbol.next-function-index,     4));
end method;


define method print-coff-strings
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  format(stream, "\nString table:\n");
  for (str in coff-file.strings.table-data)
    format(stream, "   %s\n", str.string-data);
  end for;
end method;




define method print-exports
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  let exports-section = binary-table-member?(coff-file.sections, ".drectve");
  if (exports-section)
    local method convert (x)
            let ch = as(<character>, x);
            if (ch = ' ') '\n' else ch end;
          end method;
    let exports-data = exports-section.section-data;
    let exports-string = map-as(<byte-string>, convert, exports-data);
    format(stream, "\nExport info: \n%s\n\n", exports-string);
  else
    format(stream, "\nNo export info.\n");
  end;
end method;



define method print-fixups
     (stream :: <stream>, coff-file :: <coff-file>, print-offsets? :: <boolean>) => ()
  let fixups-section = binary-table-member?(coff-file.sections, ".dyfix$m");
  if (fixups-section)
    format(stream, "\nFixup info:");
    let data = fixups-section.section-data;
    let relocs = fixups-section.relocations;
    let data-size = fixups-section.raw-data-size;
    let data-stream = make(<sequence-stream>, contents: data);
    let rindex = 0; // index into the vector of relocations
    block ()
      while (data-stream.stream-position < data-size)
        let stream-pos = data-stream.stream-position;
        let start = read-word(data-stream, coff-file); // don't need this
        let reloc = relocs[rindex];
        let base-name = reloc.relocation-symbol.symbol-name.string-data;
        format(stream, "\n   Relocations based at %s", base-name);
        rindex := rindex + 1;
        rindex := print-fixups-sequence(stream, coff-file, start, data-stream, 
                                        relocs, rindex, print-offsets?);
      end while;
      format(stream, "\n");
    cleanup
      close(data-stream);
    end;
  else
    format(stream, "\nNo fixup info.\n");
  end;
end method;


define method print-fixups-sequence
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      start :: <integer>,
      data-stream :: <stream>,
      relocs :: <vector>,
      reloc-index :: <integer>,
      print-offsets? :: <boolean>) 
     => (new-reloc-index :: <integer>)
  let num = read-encoded-fixup-number(data-stream, coff-file);
  if (num > 0)
    let import = read-word(data-stream, coff-file); // don't need this
    let reloc = relocs[reloc-index];
    let import-name = reloc.relocation-symbol.symbol-name.string-data;
    let next-index = reloc-index + 1;
    let last-position = 0;
    format(stream, "\n  %s relocations for %s", pad-out(num, 5), import-name);
    for (i from 0 below num)
      let (position, prefix) = 
        read-encoded-fixup-position(data-stream, coff-file, last-position);
      last-position := position;
      if (print-offsets?)
        format(stream, if (remainder(i, 10) = 0) "\n          " else ", " end);
        format(stream, "%s%d", prefix, position);
      end if;
    end for;
    print-fixups-sequence(stream, coff-file, start, data-stream, 
                          relocs, next-index, print-offsets?);
  else reloc-index;
  end if;
end method;


// Decode the ** encoding
define method read-encoded-fixup-position
    (data-stream :: <stream>, coff-file :: <coff-file>, last-position :: <integer>)
    => (position :: <integer>, prefix :: <string>)
  let offset = read-byte(data-stream);
  if (offset == #xff)
    let long-offset = read-short(data-stream, coff-file);
    if (long-offset = #xffff)
      values(read-word(data-stream, coff-file), "**");
    else values(last-position + ash(long-offset, 2), "*");
    end if;
  else values(last-position + ash(offset, 2), "");
  end if;
end method;


// Decode the * encoding
define method read-encoded-fixup-number
    (data-stream :: <stream>, coff-file :: <coff-file>)
    => (num :: <integer>)
  let num = read-byte(data-stream);
  if (num == #xff)
    let long-num = read-short(data-stream, coff-file);
    if (long-num = #xffff)
      read-word(data-stream, coff-file);
    else long-num;
    end if;
  else num;
  end if;
end method;


define method print-debug-info
     (stream :: <stream>, coff-file :: <coff-file>) => ()
  let debug-section = binary-table-member?(coff-file.sections, ".debug$S");
  if (debug-section)
    format(stream, "\nSymbol Debug info: ");
    let data = debug-section.section-data;
    let data-size = debug-section.raw-data-size;
    let data-stream = make(<sequence-stream>, contents: data);
    block ()
      let type = read-word(data-stream, coff-file);
      print-info-by-type(stream, coff-file, type, data-size, data-stream);
    cleanup
      close(data-stream);
    end;
  else
    format(stream, "\nNo symbol Debug info.\n");
  end;
end method;



define method print-info-by-type
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      type :: <integer>,
      limit :: <integer>,
      data-stream :: <stream>) 
     => ()
  format(stream, "Debug-info in unknown format of type %d\n", type);
end method;

define method print-info-by-type
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      type == 1,
      limit :: <integer>,
      data-stream :: <stream>) 
     => ()
  let position = 4;   // we've just read the type
  format(stream, "Debug info in Microsoft CodeView 4 format\n");
  while (position < limit)
    let length = read-short(data-stream, coff-file);
    let index  = read-short(data-stream, coff-file);
    let next-pos = position + length + 2;
    print-info-by-index(stream, coff-file, index, length, data-stream);
    position := next-pos;
    data-stream.stream-position := position;
  end while;
end method;



define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index :: <integer>,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let start = data-stream.stream-position - 4;
  print-info-header(stream, index, "UNKNOWN");
  format(stream, "length %d at offset %d\n", length, start);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == 1,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let flags = read-word(data-stream, coff-file);
  let version = read-sized-string(data-stream);
  print-info-header(stream, index, "Compile Flag");
  format(stream, "flags: %x version: %s\n", flags, version);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == 2,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let type = read-short(data-stream, coff-file);
  let register = read-short(data-stream, coff-file);
  let name = read-sized-string(data-stream);
  print-info-header(stream, index, "Register");
  format(stream, "type: %s register: %s name: %s\n", 
         pad-out(type, 4, hex: #t),
         pad-out(register, 2, hex: #t),
         name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == 4,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let type = read-short(data-stream, coff-file);
  let name = read-sized-string(data-stream);
  print-info-header(stream, index, "User-defined type");
  format(stream, "type: %s name: %s\n", pad-out(type,  4, hex: #t), name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == 6,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  print-info-header(stream, index, "End of Block", thats-all: #t);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == 9,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let signature = read-word(data-stream, coff-file);
  let name = read-sized-string(data-stream);
  print-info-header(stream, index, "Object File Name");
  format(stream, "signature: %x name: %s\n", signature, name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x200,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let offset = read-signed-word(data-stream, coff-file);
  let type   = read-short(data-stream, coff-file);
  let name   = read-sized-string(data-stream);
  print-info-header(stream, index, "BP Relative 16:32");
  format(stream, "[BP%s%s] type: %s name: %s\n",
         if (offset < 0) "" else "+" end,
         offset,
         pad-out(type,  4, hex: #t),
         name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x201,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let offset  = read-word(data-stream, coff-file);
  let segment = read-short(data-stream, coff-file);
  let type    = read-short(data-stream, coff-file);
  let name    = read-sized-string(data-stream);
  print-info-header(stream, index, "Local  Data 16:32");
  format(stream, "@ %x:%x type: %s name: %s\n",
         segment, offset,
         pad-out(type,  4, hex: #t),
         name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x202,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let offset  = read-word(data-stream, coff-file);
  let segment = read-short(data-stream, coff-file);
  let type    = read-short(data-stream, coff-file);
  let name    = read-sized-string(data-stream);
  print-info-header(stream, index, "Global Data 16:32");
  format(stream, "@ %x:%x type: %s name: %s\n",
         segment, offset,
         pad-out(type,  4, hex: #t),
         name);
end method;

define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x204,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()  
  print-info-header(stream, index, "Local  Proc Start");
  print-procedure-start-info(stream, coff-file, data-stream);
end method;


define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x205,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  print-info-header(stream, index, "Global Proc Start");
  print-procedure-start-info(stream, coff-file, data-stream);
end method;


define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x207,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let parent  = read-word(data-stream, coff-file);
  let p-end   = read-word(data-stream, coff-file);
  let length  = read-word(data-stream, coff-file);
  let offset  = read-word(data-stream, coff-file);
  let segment = read-short(data-stream, coff-file);
  let name    = read-sized-string(data-stream);
  print-info-header(stream, index, "Block Start 16:32");
  format(stream, 
         "pParent: %x pEnd: %x length: %x @ %x:%x name: %s\n",
         parent, p-end, length, segment, offset, name);
end method;


define method print-info-by-index
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      index == #x209,
      length :: <integer>,
      data-stream :: <stream>) 
     => ()
  let offset  = read-word(data-stream, coff-file);
  let segment = read-short(data-stream, coff-file);
  let flags   = read-byte(data-stream);
  let name    = read-sized-string(data-stream);
  print-info-header(stream, index, "Code Label 16:32");
  format(stream, "@ %x:%x flags: %x name: %s\n",
         segment, offset, flags, name);
end method;


define method print-procedure-start-info
     (stream :: <stream>, 
      coff-file :: <coff-file>,
      data-stream :: <stream>) 
     => ()
  let parent  = read-word(data-stream, coff-file);
  let p-end   = read-word(data-stream, coff-file);
  let next    = read-word(data-stream, coff-file);
  let length  = read-word(data-stream, coff-file);
  let d-start = read-word(data-stream, coff-file);
  let d-end   = read-word(data-stream, coff-file);
  let offset  = read-word(data-stream, coff-file);
  let segment = read-short(data-stream, coff-file);
  let type    = read-short(data-stream, coff-file);
  let flags   = read-byte(data-stream);
  let name    = read-sized-string(data-stream);
  format(stream, 
         "pParent: %x pEnd: %x pNext: %x length: %x FrameOn: %x FrameOff: %x ",
         parent, p-end, next, length, d-start, d-end);
  format(stream, 
         "@ %x:%x type: %x flags: %x name: %s\n",
         segment, offset, type, flags, 
         name);
end method;

define method print-info-header
     (stream :: <stream>, 
      index :: <integer>, 
      meaning :: <string>,
      #key thats-all = #f) => ()
  let past-end = meaning.size;
  let str1 = make(<byte-string>, size: 20, fill: ' ');
  let str2 = replace-subsequence!(str1, meaning, end: meaning.size);
  format(stream, "  Index: %s  %s", pad-out(index, 4, hex: #t), str2);
  if (thats-all) format(stream, "\n") end;
end method;



define method read-sized-string 
      (data-stream :: <stream>) => (str :: <string>)
  let len = read-byte(data-stream);
  let str = make(<string>, size: len);
  for (i from 0 below len)
    str[i] := as(<character>, read-byte(data-stream));
  end for;
  str;
end method;


define method pad-out
       (num :: <integer>, field :: <integer>, #key hex) => (str :: <string>)
  let str = make(<byte-string>, size: field, fill: ' ');
  let data = format-to-string(if (hex) "%x" else "%d" end, num);
  let data-size = data.size;
  if (data-size <= field)
    replace-subsequence!(str, data, start: (field - data-size));
  else data;
  end if;
end method;

