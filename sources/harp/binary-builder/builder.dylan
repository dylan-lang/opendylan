module:    binary-builder
Synopsis:  Support for assembling and dumping BINARY files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Temporary support for controlling DLL generation

define variable *dll-support* = #t;

define variable *force-all-dll-exports* = #f;

define method and-emit-dll? (val) => (r)
  *dll-support* & val;
end method;

define method and-force-dll-exports? (val :: <boolean>) => (r :: <boolean>)
  val & *force-all-dll-exports* & *dll-support*;
end method;


// A variable to control whether to use the directives section
define variable *add-exports-to-directives-section* = #t;


define class <section-with-fixups> (<object>)
  constant slot actual-section :: <binary-section>,
    required-init-keyword: section:;

  constant slot import-fixups :: <ordered-string-table>
    = make(<ordered-string-table>);
  // this is ordered because dynamic linking requires symbol
  // dependencies to be ordered in .dyimp sections in object files;
  // this is an important requirement for derived import fixups

  constant slot id-import-fixups :: <table> = make(<table>);
    // A table of imported symbols, which may be used to store
    // data about how to indirect the values in statically-built data

end class;


define open class <binary-builder> (<object>)
  constant slot binary-file :: <binary-file>,
    required-init-keyword: binary-file:;
  slot current-section :: <binary-section>,
    init-keyword: section:;
  slot debug-section :: false-or(<binary-section>) = #f,
    init-keyword: debug-section:;
  slot code-section :: false-or(<binary-section>) = #f;
    // Contains normal code
  slot init-code-section :: false-or(<binary-section>) = #f;
    // Contains initialization code
  slot directives-section :: false-or(<binary-section>) = #f;
    // Contains directives for the linker, including DLL export info

  slot optional-data-section :: false-or(<section-with-fixups>) = #f;
    // Contains statically initialized data (ambiguously traced by GC)
  slot optional-vars-section :: false-or(<section-with-fixups>) = #f;
    // Contains statically initialized variables (traced as roots by GC)
  slot optional-objs-section :: false-or(<section-with-fixups>) = #f;
    // Contains statically initialized data (traced as heap by GC)
  slot optional-untraced-objs-section :: false-or(<section-with-fixups>) = #f;
    // Contains statically initialized data (as heap, but untraced by GC)
  slot optional-untraced-data-section :: false-or(<section-with-fixups>) = #f;
    // Contains statically initialized data (not traced by GC)

  constant slot destination = #f,      // May be used for a stream
    init-keyword: destination:;
  constant slot def-file = #f,         // May be used for a stream for a .def file
    init-keyword: def-file:;
  slot source-file = #f,
    init-keyword: source-file:;
    // Either #f (to indicate no source level debugging)
    // or a <byte-string> with the filename of the source

  slot current-fixups :: false-or(<ordered-string-table>) = #f;
  slot id-current-fixups :: false-or(<table>) = #f;
    // The fixups table for the current section.

  slot dynamic-linking? :: <boolean> = #f;
  // dynamic-linking protocol

end class;

define open generic make-binary-builder 
    (binary-builder-class :: <class>, #key destination, def-file)
 => (new :: <binary-builder>);


define method make-binary-builder 
    (binary-builder-class :: <class>,
     #key destination, def-file) 
    => (new :: <binary-builder>)
  make(binary-builder-class,
       destination: destination,
       def-file: def-file,
       binary-file: make(<binary-file>));
end method;

define method write-binary (stream :: <stream>, builder :: <binary-builder>) => ()
  let binary-file :: <binary-file> = builder.binary-file;
  write-binary (stream, binary-file);
end method;

define method write-binary-data (stream, builder :: <binary-builder>) => ()
  let binary-file :: <binary-file> = builder.binary-file;
  write-binary-data (stream, binary-file);
end method;

define sideways method binary-data-section?(section :: <binary-section>)
 => (data-section? :: <boolean>)
  select (section.section-name by \==)
    $code-section, $init-code-section, $directives-section => #f;
    otherwise => #t;
  end;
end method;

// Dynamic derived import fixups require a sequence of indirections
// to use when fixing up objects at enumerated positions at runtime

define class <dynamic-fixups>(<object>)
  slot dynamic-offsets, required-init-keyword: offsets:;
  constant slot dynamic-positions :: <stretchy-vector>,
    required-init-keyword: positions:;
end class;

define method fixups-element
    (builder :: <binary-builder>,
     item :: <byte-string>,
     model-object, offset) => (element :: false-or(<stretchy-vector>))
  if (supplied?(model-object))
    let result =
      element(builder.id-current-fixups, model-object, default: #f);
    if (result) result.tail end if;
  else
    if (builder.dynamic-linking?)
      let result =
	element(builder.current-fixups, item, default: #f);
      if (result)
	let result :: <dynamic-fixups> = result;
	let offsets = result.dynamic-offsets;
	if (instance?(offsets, <integer>))
	    let vec = make(<stretchy-vector>, size: 2);
	    vec[0] := offsets; vec[1] := offset; 
	    result.dynamic-offsets := vec;
	else
	    add!(offsets, offset);
	end;
	result.dynamic-positions;
      end if;
    else
      element(builder.current-fixups, item, default: #f)
    end;
  end if;
end method;

define method fixups-element-setter
    (new-value :: <stretchy-vector>,
     builder :: <binary-builder>,
     item :: <byte-string>,
     model-object, offset) => (new-value :: <stretchy-vector>)
  if (supplied?(model-object))
    builder.id-current-fixups[model-object] := pair(item, new-value);
  else
    if (builder.dynamic-linking?)
      let offset = if (offset) offset else -1 end;
      builder.current-fixups[item] :=
	make(<dynamic-fixups>, offsets: offset, positions: new-value);
    else
      builder.current-fixups[item] := new-value
    end;
  end if;
  new-value
end method;


// share-or-create looks to see if the key is already known, and adds a
// newly created object for the key if not.

define inline method share-or-create
      (table :: <binary-table>, key, model-object, creator :: <function>) => (entry)
  let entry =
    // we are guaranteed that the key is not known for model-object clients
    if (supplied?(model-object)) #f
    else
      binary-table-member?(table, key, model-object: model-object)
    end if;

  if (entry)
    entry
  else
    let new-entry = creator();
    binary-element-add!(table, key, new-entry, model-object: model-object);
    new-entry;
  end if;
end method;



/// Support for imported constants:
///
/// For WinTel, at least, imported constants are prefixed with the string "__imp_".
/// Here we implement that mapping as the default, by make the mapping function open
/// for future back ends.


define inline method $imported-name-mangler
    (name :: <byte-string>) => (mangled :: <byte-string>)
  if (*dll-support*)
    concatenate("__imp_", name)
  else
    name
  end if
end method;




/// Support for fixing up imported data in the initialized data section:
///
/// We use code in the runtime to perform the indirection through the 
/// DLL import table, and to store the value into the appropriate 
/// place on the heap. The BINARY dumper is responsible for recording
/// all those places that need to be fixed up. It does this by writing
/// data into the fixup-section.
///
/// The data in the fixup-section is organized as follows:
///
/// 
/// $fixup-start-symbol
///     obj file-1:     address of start of data for file-1       4 bytes
///           N1:       number of locations to update             * encoded
///           import1:  address of 1st object in import table     4 bytes
///                     1st data offset to fixup                  ** encoded
///                     ... more offsets ...                      ** encoded
///                     N1 data offset to fixup
///           N2:       number of locations to update
///           import2:  address of 2nd object in import table
///                     1st data offset to fixup
///                     ... more offsets ...
///                     N2 data offset to fixup
///           ... more imported objects to fixup ...
///           0                                                   1 byte
///     obj file-2:     address of start of data for file-2
///           ... imported objects to fixup ...
///           0                                                   1 byte
///     ... more object files ...
/// $fixup-end-symbol
///
/// * encoding is as follows:-
///   num          1 byte
///   long-num     2 bytes [optional - only present if num = #xff]
///   huge-num     4 bytes [optional - only present if long-num = #xffff]
///
/// The number of fixups for the import is calculated as follows:
///   It's huge-num if supplied, else it's long-num if supplied, else it's num
///
/// ** encoding is as follows:-
///   offset       1 byte
///   long-offset  2 bytes [optional - only present if offset = #xff]
///   position     4 bytes [optional - only present if long-offset = #xffff]
///
/// The address is calculated as follows:
///   if position is supplied, then it gives the address relative to start of data
///   if long-offset is supplied then it gives the address relative to the last
///     address in multiples of 4 bytes
///   otherwise, offset gives the address relative to the last address in 
///     multiples of 4 bytes

// Before the data has been fixed up, it will contain this dummy value
define constant $not-yet-relocated-data = #x3;  // Invalid tag value


// ADD-IMPORTED-DATA puts the dummy value in the data section, and 
// records the fixup information for the fixup section
//



define open generic add-imported-data
    (builder :: <binary-builder>, 
     item :: <byte-string>,
     model-object, offset) => ();

define method add-imported-data
    (builder :: <binary-builder>, 
     item :: <byte-string>,
     model-object, offset) => ()
  let this-item :: <stretchy-vector> =
    fixups-element(builder, item, model-object, offset) 
    | (fixups-element(builder, item, model-object, offset) := make(<stretchy-vector>));

  add!(this-item, builder.current-section.current-position);
  add-integer-data(builder, $not-yet-relocated-data);
end method;


// ADD-IMPORTED-DATA-FIXUPS dumps all fixup information to the the fixups 
// section
//
define method add-imported-data-fixups
    (builder :: <binary-builder>) => ()

  let dynamic-linking? :: <boolean> = builder.dynamic-linking?;

  local method add-fixups-for-one-section 
            (optional-section :: false-or(<section-with-fixups>),
	     start-symbol :: <byte-string>) => () 
          if (optional-section)
            let fixups = optional-section.import-fixups;
            let id-fixups = optional-section.id-import-fixups;
            unless (fixups.empty? & id-fixups.empty?)

	      let fixup-section =
		if (dynamic-linking?) $import-section
		else $fixup-section end;

	      select-binary-section(builder, fixup-section, 
				    alignment: 1, flags: fixup-flags(builder));

	      add-data(builder, start-symbol, unsupplied());

              for (positions keyed-by model-object in id-fixups)
                let name = positions.head;
                add-fixups-for-imported-symbol
                  (builder, name, model-object, positions.tail);
              end for;

	      if (dynamic-linking?)

              for (positions keyed-by name in fixups)
                add-dynamic-fixups-for-imported-symbol
                  (builder, name, unsupplied(), positions);
              end for;
	      add-integer-data(builder, 0);

	      else

              for (positions keyed-by name in fixups)
                add-fixups-for-imported-symbol
                  (builder, name, unsupplied(), positions);
              end for;

	      end if;

              add-data-byte(builder, 0);  // Mark end of data for this section
            end unless;
          end if;
        end method;

  add-fixups-for-one-section(builder.optional-data-section,
                             $obj-file-start-data-symbol);

  add-fixups-for-one-section(builder.optional-vars-section,
			     $obj-file-start-vars-symbol);

  add-fixups-for-one-section(builder.optional-objs-section,
			     $obj-file-start-objs-symbol);

  add-fixups-for-one-section(builder.optional-untraced-objs-section,
			     $obj-file-start-untraced-objs-symbol);

  add-fixups-for-one-section(builder.optional-untraced-data-section,
			     $obj-file-start-untraced-data-symbol);
end method;


define open generic add-fixup-data 
    (builder :: <binary-builder>, data, model-object)
    => ();

define method add-fixup-data 
    (builder :: <binary-builder>, data, model-object)
    => ()
  add-data(builder, data, model-object);
end method;

define method add-fixups-for-imported-symbol
    (builder :: <binary-builder>,  name, model-object, positions :: <stretchy-vector>)
     => ()
  let last-pos :: <integer> = 0;
  add-fixups-number(builder, positions.size);  // The number of fixups
  add-fixup-data(builder, name, model-object);       // The address to indirect
  for (pos :: <integer> in positions)
    add-fixup-address(builder, pos, last-pos); // Offsets into the data section
    last-pos := pos;
  end for;
end method;

// Dynamic derived import fixups require a sequence of indirections
// to use when fixing up objects at enumerated positions at runtime
// 
// The binary format for these differs slightly from .dyfix sections;
// currently, each import and its accompanying indirections mask and
// position are listed separately each time, room for improvement here.
// 

define method add-dynamic-fixups-for-imported-symbol
    (builder :: <binary-builder>,  name, model-object, fixups :: <dynamic-fixups>)
     => ()

  let offsets = fixups.dynamic-offsets;
  let positions = fixups.dynamic-positions;

  if (instance?(offsets, <integer>))
    add-integer-data(builder, offsets);
    add-data-byte(builder, 1);                         // The number of fixups
    add-fixup-data(builder, name, model-object);       // The address to indirect
    add-fixup-address(builder, positions.first, 0);    // Offsets into the data section
  else
    let offsets :: <stretchy-vector> = offsets;
    for (offset :: <integer> in offsets,
	 pos :: <integer> in positions)
      add-integer-data(builder, offset);
      add-data-byte(builder, 1);                      // The number of fixups
      add-fixup-data(builder, name, model-object);    // The address to indirect
      add-fixup-address(builder, pos, 0);             // Offsets into the data section
    end;
  end;
end method;


// Implement the "* encoding"
//
define method add-fixups-number (builder :: <binary-builder>, num :: <integer>) => ()
  if (num >= #xff)
    add-data-byte(builder, #xff);
    if (num >= #xffff)
       add-integer-data-short(builder, #xffff);
       add-integer-data(builder, num);
    else add-integer-data-short(builder, num);
    end if;
  else add-data-byte(builder, num);
  end if;
end method;


// Implement the "** encoding"
//
define method add-fixup-address
     (builder :: <binary-builder>, pos :: <integer>, last-pos :: <integer>) => ()
  let offset = ash(pos - last-pos, -2);
  if (offset >= #xff)
    add-data-byte(builder, #xff);
    if (offset >= #xffff)
       add-integer-data-short(builder, #xffff);
       add-integer-data(builder, pos);
    else add-integer-data-short(builder, offset);
    end if;
  else add-data-byte(builder, offset);
  end if;
end method;


/// Managing DLL exports
///
/// Symbols are exported by adding a "-export:<name>,data" linker directive 
/// in the .drectve section - unless there is genuinely a need to get
/// a stub indirection function into the client (via the .lib file),
/// in which case the ",data" prefix is not used.

// ensure-directives-section ensures that the directives section exists,
// and caches it in the directives-section slot in the builder


define method ensure-directives-section 
    (builder :: <binary-builder>, 
     #key section-name = $directives-section) 
    => (section :: <binary-section>)
  builder.directives-section
    | (builder.directives-section :=
         share-or-create(builder.binary-file.sections, section-name, unsupplied(),
                         method ()
                           make-binary-section(builder, section-name, 
					       1, directives-flags(builder));
                         end method));
end method;


define method undecorated-name 
    (name :: <byte-string>) => (export-name :: <byte-string>)
  // STDCALL Names of the form "_foo@4" should be exported as "foo"
  if (name[0] == '_')
    // If it's a STDCALL, some demangling is needed
    let at-pos = find-key(name, curry(\==, '@'), failure: #f);
    if (at-pos)
      copy-sequence(name, start: 1, end: at-pos);
    else name;
    end if;
  else name;
  end if;
end method;


define method add-symbol-export
    (builder :: <binary-builder>, name :: <byte-string>,
     #key section = builder.ensure-directives-section,
          code-stub? = #f)
    => ()
  if (*add-exports-to-directives-section*)
    local method add-export 
	      (export-name :: <byte-string>, suffix :: <byte-string>)
	    let prefix :: <byte-string> =
	      if (section.current-position = 0)
		"-export:"
	      else " -export:"
	      end;
	    let data-string :: <byte-string> =
	      if (code-stub?) "" else ",data" end;
	    let export-string =
	      concatenate(prefix, export-name, data-string, suffix);
	    add-string-to-section(section, export-string);
	  end method;

    let export-name = name.undecorated-name;
    add-export(name, "");
    unless (export-name == name)
      add-export(export-name, concatenate("=", name));
    end unless;
  end if;
end method;

define method add-symbol-def(builder :: <binary-builder>, name :: <byte-string>)
 => ()
  let stream = builder.def-file;
  if (stream) format(stream, "%s\n", name) end;
end method add-symbol-def;

