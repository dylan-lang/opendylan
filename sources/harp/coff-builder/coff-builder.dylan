module:    coff-builder
Synopsis:  Support for assembling and dumping COFF files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open class <coff-builder> (<binary-builder>)

  constant slot big-endian?     :: <boolean>,
    required-init-keyword: big-endian?:;
  slot last-function-definition-aux
     :: false-or(<coff-function-definition-auxiliary-symbol>) = #f;
    // This is used to make a chain of function definitions, as 
    // required by coff.
  slot last-begin-function-aux
    :: false-or(<coff-function-lines-auxiliary-symbol>) = #f;
    // This is used to make a chain of function definitions, as 
    // required by coff.

end class;


define method make-binary-builder 
    (coff-builder-class :: subclass(<coff-builder>),
     #key machine, big-endian?, destination, def-file, source-file) 
    => (new :: <coff-builder>)
  make(coff-builder-class,
       big-endian?: big-endian?,
       destination: destination,
       def-file: def-file,
       binary-file: make(<coff-file>, 
			 machine: machine,
			 big-endian?: big-endian?,
			 time-stamp: coff-time-stamp()));
end method;



define generic default-symbol-types
    (symbol :: <coff-symbol>, representation :: <symbol>) 
    => (storage-class, value, type);

define method default-symbol-types
    (symbol :: <coff-symbol>, representation :: <symbol>) 
    => (storage-class, value, type)
  select (representation)
    #"external"        => values($sym-external,        0,  $sym-type-null);
    #"function"        => values($sym-external,        #f, $sym-type-function);
    #"public-function" => values($sym-external,        #f, $sym-type-function);
    #"static-function" => values($sym-static,          #f, $sym-type-function);
    #"end-function"    => values($sym-end-of-function, #f, $sym-type-null);
    #"public-data"     => values($sym-external,        #f, $sym-type-null);
    #"static-literal"  => values($sym-static,          #f, $sym-type-null);
  end select;
end method;



define generic default-relocation-type
    (section :: <coff-section>, symbol :: <coff-symbol>, type :: <symbol>) 
    => (class :: <class>, relocation-type :: <integer>);

define method default-relocation-type
    (section :: <coff-section>, symbol :: <coff-symbol>, type :: <symbol>) 
    => (class :: <class>, relocation-type :: <integer>)
  select (type)
    #"relative"     => values(<coff-relative-relocation>, #x14);
    #"absolute"     => values(<coff-absolute-relocation>, #x6);
    #"offset"       => values(<coff-absolute-relocation>, #xB);
    #"segment"      => values(<coff-absolute-relocation>, #xA);
  end select;
end method;



define method coff-time-stamp () => (time :: <integer>)
  #xdefbabe;  /// #\!@#$ temporary value (Lizzie just failed a hearing test)
end method;



// add-source-file-definition generates a .file symbol, followed by a string marking 
// the source file name. Normally this would be called first. Generation of line 
// number information is disabled until this function is called.


define method add-source-file-definition
    (builder :: <coff-builder>, file-name :: <byte-string>) => ()
  // This symbol is not entered into the name dictionary - so we bypass
  // all the sharing support.
  let syms = builder.binary-file.symbols;
  let aux-number 
    = floor/(file-name.size + size-of-symbol - 1, size-of-symbol);
  let sym-name = ".file";
  let dummy = "dummy-name";
  let aux-symbols = make(<vector>, size: aux-number);
  let name-string = make-coff-string(builder, sym-name);
  let file-symbol
    = make(<coff-symbol>, 
           name:          name-string,
           value:         0,
           section:       coff-section-sym-debug,
           storage-class: $sym-file,
           aux-symbols:   aux-symbols);

  // First add the ".file" symbol
  binary-element-add!(syms, sym-name, file-symbol);

  // Next add an auxiliary symbol which contains the string
  let aux-symbol = make(<coff-string-auxiliary-symbol>, string: file-name);
  binary-element-add!(syms, dummy, aux-symbol);
  aux-symbols[0] := aux-symbol;

  // Now add a dummy "empty" aux symbol to pad the length to size-of-symbol
  for (i from 1 below aux-number)
    let empty-aux = make(<coff-empty-auxiliary-symbol>);
    aux-symbols[i] := empty-aux;
    binary-element-add!(syms, dummy, empty-aux);
  end for;
  
  // Finally, mark the builder to show that source file debug info is OK
  builder.source-file := file-name;
end method;



// add-function-line-number-definition
// This creates a complex function definition symbol, along with corresponding
// auxiliary symbols describing line number information. It should be used
// to define the symbol for the function provided that line number information 
// is available. The alternative, when line number information is not available
// is to use add-symbol-definition.
// NB num-lines is the number of specific line number entries for the function. 
// I.e. it does not include the line number symbol. 

define method add-function-line-number-definition
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     code-size :: <integer>, num-lines :: <integer>, 
     start-line :: <integer>, end-line :: <integer>,
     #key section = builder.current-section, 
          representation = #"public-function")
    => (cs :: <coff-symbol>)

  // First, define the actual function symbol
  align-section-data(section);
  let def = define-coff-symbol(builder, name, model-object,
			       value: section.current-position,
			       section: section,
			       representation: representation,
                               must-be-fresh?: #t);

  // Before generating any line numbers for a function, there
  // are two initializations to perform: (1) Create the .bf, .lf
  // and .ef symbols, and (2) Add the symbol to the section line numbers.

  // Here's (2) [this is the same as add-line-number-symbol ]
  let line = make(<coff-line-number-symbol>, symbol: def);
  add-line-number-to-section(section, line);

  // Here's (1)
  let bf-aux = make(<coff-function-lines-auxiliary-symbol>,
                    line-number: start-line);

  let ef-aux = make(<coff-function-lines-auxiliary-symbol>, 
                    line-number: end-line);

  let bf = make(<coff-symbol>, 
                name:          make-coff-string(builder, ".bf"),
                value:         0,
                section:       section,
                storage-class: $sym-function,
                aux-symbols:   vector(bf-aux));

  let lf = make(<coff-symbol>, 
                name:          make-coff-string(builder, ".lf"),
                value:         num-lines + 1, // +1 for the symbol
                section:       section,
                storage-class: $sym-function,
                aux-symbols:   vector());

  let ef = make(<coff-symbol>, 
                name:          make-coff-string(builder, ".ef"),
                value:         0,
                section:       section,
                storage-class: $sym-function,
                aux-symbols:   vector(bf-aux));

  let def-aux = make(<coff-function-definition-auxiliary-symbol>,
                     total-size: code-size,
                     line-numbers: line,
                     tag: bf);

  def.aux-symbols := vector(def-aux);

  let syms = builder.binary-file.symbols;
  let dummy = "dummy-name";
  binary-element-add!(syms, dummy, def-aux);
  binary-element-add!(syms, dummy, bf);
  binary-element-add!(syms, dummy, bf-aux);
  binary-element-add!(syms, dummy, lf);
  binary-element-add!(syms, dummy, ef);
  binary-element-add!(syms, dummy, ef-aux);

  if (builder.last-function-definition-aux)
    builder.last-function-definition-aux.auxiliary-next-function := def;
    builder.last-begin-function-aux.auxiliary-next-function := bf;
  end if;
  builder.last-function-definition-aux := def-aux;
  builder.last-begin-function-aux := bf-aux;
  def
end method;




// share-or-create-unordered is like share-or-create, except that it uses 
// coff-element-setter, rather than binary-element-add!. This means that 
// the addition only happens to unordered code.

define inline method share-or-create-unordered
      (table :: <coff-table>, key, model-object, creator :: <function>) => (entry)
  let entry = binary-table-member?(table, key, model-object: model-object);
  if (entry)
    entry
  else
    let new-entry = creator();
    coff-element-setter(new-entry, table, key, model-object: model-object);
    new-entry;
  end if;
end method;


define method make-coff-string 
     (builder :: <coff-builder>, string :: <byte-string>,
      #key model-object = unsupplied())
     => (cs :: <coff-string>)
  if (string.size <= 8)
    make(<coff-short-string>, string: string);
  else
    share-or-create(builder.binary-file.strings, string, model-object,
                    method ()
                      let new-entry = make(<coff-long-string>, string: string);
                      new-entry
                    end method);
  end if;
end method;


/// Making symbols.
//
// A symbol is always created or shared on demand. Checks for inappropriately
// defined symbols are delayed to as late as possible. A new symbol is created
// as a static literal. The protocol expects all other symbol types to be
// explicitly notified.
//
// The symbols are only inserted into the COFF symbol hashtable at creation
// time. They are assigned an ordered number at definition time. This is 
// necessary because some symbols are expected to be followed by auxiliary
// symbols, and only the definer can know this.

define method make-coff-symbol 
     (builder :: <coff-builder>,
      name :: <byte-string>,
      model-object,
      #key import?)
      => (cs :: <coff-symbol>)
  let import? = import? | identity;
  share-or-create-unordered
    (builder.binary-file.symbols, name, model-object,
     method ()
       // This is the only point in time that names are created
       let name =
	 import?(if (supplied?(model-object))
		   builder-model-object-name(model-object, name)
		 else
		   name
		 end if);
       let name-string =
	 make-coff-string(builder, name,
			  model-object: model-object);
       let sym = make(<coff-symbol>, name: name-string);
       initialize-coff-symbol(sym, representation: #"static-literal");
       sym;
     end method);
end method;

define open generic builder-model-object-name(model-object, name :: <string>)
 => (name :: <string>);


define method default-initialize-coff-symbol
    (symbol :: <coff-symbol>, representation) => ()
  let (storage-class, value, type) = default-symbol-types(symbol, representation);
  if (value)         symbol.symbol-value  := value         end;
  if (type)          symbol.symbol-type   := type          end;
  if (storage-class) symbol.storage-class := storage-class end;
end method;


define method initialize-coff-symbol
    (symbol :: <coff-symbol>, 
     #key representation, value, type, storage-class, section, aux-symbols,
     #all-keys) => ()
  if (representation)
    default-initialize-coff-symbol(symbol, representation);
  end if;
  if (value)         symbol.symbol-value  := value         end;
  if (type)          symbol.symbol-type   := type          end;
  if (storage-class) symbol.storage-class := storage-class end;
  if (section)       symbol.section       := section       end;
  if (aux-symbols)   symbol.aux-symbols   := aux-symbols   end;
end method;


define method coff-symbol-already-defined?
    (symbol :: <coff-symbol>) => (defined? :: <boolean>)
  symbol.index >= 0;
end method;



define method define-coff-symbol
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     #rest all-keys,  
     #key representation, section, value, aux-symbols,
          must-be-fresh? = #f, import?,
     #all-keys) 
     => (cs :: <coff-symbol>)
  let coff-symbol = 
    make-coff-symbol(builder, name, model-object, import?: import?);
  apply(initialize-coff-symbol, coff-symbol, all-keys);
  // Before adding the symbol to the ordered part of the table,
  // check for duplicates. It might have been defined twice for 
  // genuine different reasons (e.g. public & add)
  let symtab = builder.binary-file.symbols;
  if (coff-symbol.coff-symbol-already-defined?)
    if (must-be-fresh?)
      error("COFF generation error: illegal multiple definition of %=.", 
            coff-symbol.symbol-name.string-data);
    end if;
  else 
    binary-element-add!(symtab,
			name,
			coff-symbol,
			model-object: model-object);
  end if;
  coff-symbol;
end method;



define method define-external-symbol
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     #rest all-keys,  
     #key representation = #"external", section = coff-section-undefined,
          import?,
     #all-keys)  
     => (cs :: <coff-symbol>)
  apply(define-coff-symbol, builder, name, model-object,
        section: section, representation: representation,
	import?: import?,
        all-keys);
end method;


define method define-public-symbol
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     #rest all-keys,  
     #key representation = #"public-data",
     #all-keys)  
     => (cs :: <coff-symbol>)
  apply(define-coff-symbol, builder,
	name, model-object,
	representation: representation, 
        all-keys);
end method;



define inline method init-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $cnt-initialized-data
end method;


define method make-binary-section 
    (builder :: <coff-builder>, name :: <byte-string>, 
     alignment :: <integer>, flags :: <abstract-integer>)
    => (new :: <coff-section>)
  make(<coff-section>, name: make-coff-string(builder, name), 
                       alignment: alignment,
                       flags: flags,
                       big-endian?: builder.big-endian?);
end method;


define constant $directives-flags 
  = $lnk-info + $lnk-remove + $align-1bytes;


define inline method directives-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $directives-flags
end method;


/// Adding data (assume that all section data is represented as bytes)

define method add-symbol-definition
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     #rest all-keys,
     #key section = builder.current-section, 
          public? = #f,
          representation = if (public?) #"public-data" else #f end,
     #all-keys)
    => (coff-symbol :: <coff-symbol>)
  align-section-data(section);
  let pos = section.current-position;
  apply(define-coff-symbol, builder,
	name, model-object,
	value: pos, section: section,
	representation: representation,
        all-keys);
end method;

define method add-line-number-symbol
    (builder :: <coff-builder>, name :: <byte-string>, 
     #key section = builder.current-section)
    => ()
  let symbol = make-coff-symbol(builder, name, unsupplied());
  let line = make(<coff-line-number-symbol>, symbol: symbol);
  add-line-number-to-section(section, line);
end method;


define method add-line-number
    (builder :: <coff-builder>, line-number :: <integer>, 
     #key section = builder.current-section,
          pos = section.current-position)
    => ()
  let line = make(<coff-line-number-relative>,
                  line-number: line-number, rva: pos);
  add-line-number-to-section(section, line);
end method;


define method add-data-vector
    (builder :: <coff-builder>, vector :: <byte-vector>, 
     #key section = builder.current-section)
    => ()
  add-vector-to-section(section, vector);
end method;



define method add-data-short
    (builder :: <coff-builder>,
     data :: <integer>, model-object,
     #key section = builder.current-section)
    => ()
  add-short-to-section(section, data);
end method;

define method add-data-short 
    (builder :: <coff-builder>, 
     name :: <byte-string>, model-object,
     #key section = builder.current-section, 
          type = #"segment", 
          relocation-class = #f, relocation-type = #f,
          offset = 0)
    => ()
  let symbol = make-coff-symbol(builder, name, model-object);
  let pos = section.current-position;
  let (d-class, d-reloc-type) = default-relocation-type(section, symbol, type);
  let class =      relocation-class | d-class;
  let reloc-type = relocation-type  | d-reloc-type;
  let reloc = make(class, relocation-type: reloc-type, 
                          symbol: symbol, index: pos);
  add-reloc-to-section(section, reloc);
  add-short-to-section(section, offset);
end method;


define method add-data 
    (builder :: <coff-builder>, 
     name :: <byte-string>, model-object,
     #key section = builder.current-section, 
          type = #"absolute", 
          relocation-class = #f, relocation-type = #f,
          position-relative? = #f, // when true, want offset relative to
                                   // current position, using name as base
          offset :: <integer> = 0)
    => ()
  let symbol = make-coff-symbol(builder, name, model-object);
  let pos = section.current-position;
  let wanted-offset :: <integer> =
    if (position-relative?)
      offset + pos - symbol.symbol-value;
    else 
      offset;
    end if;
  let (d-class, d-reloc-type) = default-relocation-type(section, symbol, type);
  let class =      relocation-class | d-class;
  let reloc-type = relocation-type  | d-reloc-type;
  let reloc = make(class, relocation-type: reloc-type, 
                          symbol: symbol, index: pos);
  add-reloc-to-section(section, reloc);
  add-word-to-section(section, wanted-offset);
end method;


define method insert-relocation
    (builder :: <coff-builder>, 
     name :: <byte-string>, 
     model-object,
     #key section = builder.current-section, 
          type = #"absolute", 
          pos = section.current-position,
          relocation-class = #f, 
          relocation-type = #f,
          import?)
    => ()
  let symbol =
    make-coff-symbol(builder, name, model-object, import?: import?);
  let (d-class, d-reloc-type) = default-relocation-type(section, symbol, type);
  let class =      relocation-class | d-class;
  let reloc-type = relocation-type  | d-reloc-type;
  let reloc = make(class, relocation-type: reloc-type, 
                          symbol: symbol, index: pos);
  add-reloc-to-section(section, reloc);
end method;

define method insert-interactor-relocation
    (builder :: <coff-builder>, 
     handle,
     #key section = builder.current-section, 
          pos = section.current-position)
    => ()
  let reloc = make(<coff-interactor-relocation>, relocation-type: 0, 
		   index: pos, handle: handle);
  add-reloc-to-section(section, reloc);
end method;



define sideways method fill-section-data
     (section :: <coff-section>, fill :: <integer>,
      #key start, end: _end)
     => ()
  fill!(section.section-data, fill, start: start, end: _end);
end method;


define method add-4-bytes-to-section 
    (section :: <coff-section>, 
     b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>) => ()
  let pos = section.current-position;
  let new-size = pos + 4;
  section.current-position := new-size;
  let data :: <byte-vector> = section.section-data;
  without-bounds-checks
    data[pos]     := b0;
    data[pos + 1] := b1;
    data[pos + 2] := b2;
    data[pos + 3] := b3;
  end without-bounds-checks;
end method;


define method add-2-bytes-to-section 
    (section :: <coff-section>, b0 :: <byte>, b1 :: <byte>) => ()
  let pos = section.current-position;
  let new-size = pos + 2;
  section.current-position := new-size;
  let data :: <byte-vector> = section.section-data;
  data[pos]     := b0;
  data[pos + 1] := b1;
end method;


define sideways method add-byte-to-section 
    (section :: <coff-section>, byte :: <integer>) => ()
  let pos = section.current-position;
  let new-size = pos + 1;
  section.current-position := new-size;
  section.section-data[pos] := logand(byte, #xff);
end method;


define sideways method add-string-to-section 
    (section :: <coff-section>, string :: <byte-string>) => ()
  let pos = section.current-position;
  let len = string.size;
  section.current-position := pos + len;
  copy-bytes(section.section-data, pos, string, 0, len);
end method;


define method add-vector-to-section 
    (section :: <coff-section>, vector :: <byte-vector>) => ()
  let pos = section.current-position;
  let len = vector.size;
  section.current-position := pos + len;
  copy-bytes(section.section-data, pos, vector, 0, len);
end method;


define generic split-word-into-bytes 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>);

define method split-word-into-bytes 
    (word :: <integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>)
  if (word.zero?)
    values(0, 0, 0, 0)
  else
    let b0 :: <byte> = logand(word, #xff);
    let b1 :: <byte> = logand(ash(word, -8), #xff);
    let b2 :: <byte> = logand(ash(word, -16), #xff);
    let b3 :: <byte> = logand(ash(word, -24), #xff);
    if (big-endian?)
      values(b3, b2, b1, b0);
    else
      values(b0, b1, b2, b3);
    end if;
  end if;
end method;

define method split-word-into-bytes 
    (word :: <abstract-integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>)
  if (word.zero?)
    values(0, 0, 0, 0)
  else
    let b0 :: <byte> = generic-logand(word, #xff);
    let b1 :: <byte> = generic-logand(generic-ash(word, -8), #xff);
    let b2 :: <byte> = generic-logand(generic-ash(word, -16), #xff);
    let b3 :: <byte> = generic-logand(generic-ash(word, -24), #xff);
    if (big-endian?)
      values(b3, b2, b1, b0);
    else
      values(b0, b1, b2, b3);
    end if;
  end if;
end method;


define method split-short-into-bytes 
    (word :: <integer>, big-endian?)
    => (b0 :: <byte>, b1 :: <byte>)
  if (word.zero?)
    values(0, 0)
  else
    let b0 :: <byte> = logand(word, #xff);
    let b1 :: <byte> = logand(ash(word, -8), #xff);
    if (big-endian?)
      values(b1, b0);
    else
      values(b0, b1);
    end if;
  end if;
end method;


define method join-bytes-into-word
    (b0 :: <byte>, b1 :: <byte>, b2 :: <byte>, b3 :: <byte>, 
     big-endian?)
    => (word :: <abstract-integer>)
  if (big-endian?)
     generic-+(generic-*(b0, #x1000000), (b1 * #x10000) + (b2 * #x100) + b3);
  else
     generic-+(generic-*(b3, #x1000000), (b2 * #x10000) + (b1 * #x100) + b0);
  end if;
end method;


/*
define method join-bytes-into-short
    (b0 :: <byte>, b1 :: <byte>, big-endian?)
    => (word :: <integer>)
  if (big-endian?)
     (b0 * #x100) + b1;
  else
     (b1 * #x100) + b0;
  end if;
end method;
*/

define sideways method add-word-to-section 
    (section :: <coff-section>, word :: <abstract-integer>) => ()
  let (b0, b1, b2, b3) = split-word-into-bytes(word, section.big-endian?);
  add-4-bytes-to-section(section, b0, b1, b2, b3);
end method;


define sideways method add-short-to-section 
    (section :: <coff-section>, word :: <integer>) => ()
  let (b0, b1) = split-short-into-bytes(word, section.big-endian?);
  add-2-bytes-to-section(section, b0, b1);
end method;


define method add-line-number-to-section 
    (section :: <coff-section>, line :: <coff-line-number>) => ()
  add!(section.line-numbers, line);
end method;


define method add-reloc-to-section 
    (section :: <coff-section>, reloc :: <coff-relocation>) => ()
  add!(section.relocations, reloc);
end method;


define method word-in-section
    (section :: <coff-section>, pos :: <integer>) 
 => (word :: <abstract-integer>)
  let data = section.section-data;
  let b0 :: <byte> = data[pos];
  let b1 :: <byte> = data[pos + 1];
  let b2 :: <byte> = data[pos + 2];
  let b3 :: <byte> = data[pos + 3];
  join-bytes-into-word(b0, b1, b2, b3, section.big-endian?);
end method;


define method word-in-section-setter
    (new-word :: <abstract-integer>, section :: <coff-section>, pos :: <integer>) 
 => (word :: <abstract-integer>)
  let (b0, b1, b2, b3) = split-word-into-bytes(new-word, section.big-endian?);
  let data = section.section-data;
  data[pos + 0] := b0;
  data[pos + 1] := b1;
  data[pos + 2] := b2;
  data[pos + 3] := b3;
  new-word;
end method;


/// Finishing off ...
/// Fill in the housekeeping details, when the raw data is complete.

define method fixup-coff-builder (builder :: <coff-builder>) => ()
  fixup-string-table(builder);
  fixup-symbol-table(builder);
  fixup-sections(builder);
end method;


define method fixup-sections (builder :: <coff-builder>) => ()
  let file = builder.binary-file;
  for (section :: <coff-section> in file.sections.ordered-data)
    fixup-one-section(builder, section);
  end for;
end method;

// A <coff-relocation> with type local-relocation-type has been
// statically linked by being fixed up here.

define constant local-relocation-type = -1; 

define method external-relocation? 
    (reloc :: <coff-relocation>) => (r :: <boolean>)
  reloc.relocation-type ~= local-relocation-type;
end method;

define method do-local-relocation
    (section :: <coff-section>, reloc :: <coff-relative-relocation>) => ()
  let pos :: <integer> = reloc.index;       // position of the relocation
  let sym :: <coff-symbol> = reloc.relocation-symbol;
  let dest :: <integer> = sym.symbol-value; // position of destination symbol
  let rel :: <integer> = dest - pos - 4;    // allow for 4 bytes of word itself
  word-in-section(section, pos) := word-in-section(section, pos) + rel;
  reloc.relocation-type := local-relocation-type;
end method;

define method locally-linked-relocation
    (reloc :: <coff-relocation>, reloc-section :: <coff-section>)
   => (bool :: <boolean>)
  #f;
end method;

define method locally-linked-relocation 
    (reloc :: <coff-relative-relocation>, reloc-section :: <coff-section>)
   => (bool :: <boolean>)
  reloc.relocation-symbol.section == reloc-section;
end method;

define method fixup-one-section 
    (builder :: <coff-builder>, section :: <coff-section>) => ()
  // Fix up any relocations of the section which can be linked
  // with only information in this file. This applies to relative
  // relocations to symbols in the same section.
  let had-to-fixup? = #f;
  for (reloc :: <coff-relocation> in section.relocations)
    if (locally-linked-relocation(reloc, section))
      had-to-fixup? := #t;
      do-local-relocation(section, reloc);
    end if;
  end for;
  if (had-to-fixup?)
    section.relocations := choose(external-relocation?, section.relocations);
  end if;
end method;


define method fixup-symbol-table (builder :: <coff-builder>) => ()
  // This is all done on-the-fly nowadays
end method;

define method fixup-string-table (builder :: <coff-builder>) => ()
  let file = builder.binary-file;
  // Iterate around the strings finding an index for each.
  let curpos :: <integer> = 4;    // make room for the string table length
  for (string :: <coff-long-string> in file.strings.table-data)
    string.index := curpos;
    // allow for null termination
    curpos := curpos + string.string-data.size + 1; 
  end for;
  for (string :: <coff-long-string> in file.strings.id-table-data)
    string.index := curpos;
    // allow for null termination
    curpos := curpos + string.string-data.size + 1; 
  end for;
  // Then create the string table data as a sequence
  // let data-size = curpos - 4;  // the size field does not allow for itself
  let data-size = curpos;  // size field seems to allow for itself after all
  let string-section = make(<byte-vector>, size: curpos);
  let (b0, b1, b2, b3) = split-word-into-bytes(data-size, builder.big-endian?);
  string-section[0] := b0; string-section[1] := b1; 
  string-section[2] := b2; string-section[3] := b3;
  file.strings.ordered-data := string-section;
  for (string :: <coff-long-string> in file.strings.table-data)
    let inx = string.index;
    splice-string-into-vector!(string-section, string.string-data, inx);
  end for;
  for (string :: <coff-long-string> in file.strings.id-table-data)
    let inx = string.index;
    splice-string-into-vector!(string-section, string.string-data, inx);
  end for;
end method;

define method splice-string-into-vector! (data :: <byte-vector>, 
                                          string :: <byte-string>, 
                                          inx :: <integer>) => ()
  let len = string.size;
  copy-bytes(data, inx, string, 0, len);
  data[inx + len] := 0;  // the null termination
end method;


define constant $fixup-flags 
  = generic-+($mem-read, $align-1bytes + $cnt-initialized-data);

define inline method fixup-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $fixup-flags
end method;


define inline method data-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $data-flags
end method;

define inline method dylan-data-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $data-flags
end method;

define inline method code-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $code-flags
end method;

define inline method init-code-flags(builder :: <coff-builder>)
 => (flags :: <abstract-integer>)
  $code-flags
end method;
