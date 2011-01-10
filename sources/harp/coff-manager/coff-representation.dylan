module:    coff-representation
Synopsis:  The classes to describe a COFF file
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <coff-byte> = <integer>;

define constant <coff-short> = <integer>;

define constant <coff-word> = <abstract-integer>;


/// Abstract class <coff-unit> is a superclass of all classes which
/// describe a portion of a COFF file. COFF manipulation libraries
/// are free to define their own protocols on this class.

define abstract class <coff-unit> (<object>)
end class;

define abstract class <coff-index> (<coff-unit>)
  slot index :: <integer> = -1,      // an index into a table
    init-keyword: index:;
end class;


/// Abstract class <coff-string> is a superclass of all strings classes
/// for a COFF file. There are 2 concrete representations of strings. 
/// Large strings are represented in the string table, whereas small strings 
/// are not.

define abstract class <coff-string> (<coff-unit>)
  constant slot string-data :: <byte-string>,
    required-init-keyword: string:;
end class;


/// Class <coff-long-string> contains the data for each string which is 
/// represented in a string table.


define class <coff-long-string> (<coff-string>, <coff-index>)
end class;



/// Class <coff-short-string> contains the data for each string which is 
/// represented in-line. This corresponds to strings of length 8 or less.


define class <coff-short-string> (<coff-string>)
end class;




/// Abstract class <coff-symbol-record> describes all those entities
/// which may appear in the COFF symbol table

define abstract class <coff-symbol-record> (<coff-index>)
end class;


/// Class <coff-symbol> describes a single standard symbol entry in a COFF
/// symbol table


define class <coff-symbol> (<coff-symbol-record>)
  constant slot symbol-name :: <coff-string>, 
    required-init-keyword: name:;
  slot symbol-value :: <integer> = 0,
    init-keyword: value:;
  slot section :: <coff-symbol-locator>,
    init-keyword: section:;
  slot symbol-type :: <coff-short> = 0,
    init-keyword: type:;
  slot storage-class :: <coff-byte> = 0,
    init-keyword: storage-class:;
  slot aux-symbols :: <sequence> = #[], // sequence of auxiliary symbol records
    init-keyword: aux-symbols:;
end class;


/// Class <coff-auxiliary-symbol> describes an auxiliary symbol 
/// entry in a COFF symbol table

define abstract class <coff-auxiliary-symbol> (<coff-symbol-record>)
end class;


/// A <coff-string-auxiliary-symbol> contains just a string, as used 
/// in .file declarations.

define class <coff-string-auxiliary-symbol> (<coff-auxiliary-symbol>)
  slot auxiliary-string :: <byte-string>,
    required-init-keyword: string:;
end class;


/// A <coff-empty-auxiliary-symbol> pads up the extra space taken by 
/// a long <coff-string-auxiliary-symbol>

define class <coff-empty-auxiliary-symbol> (<coff-auxiliary-symbol>)
end class;


// A <coff-section-auxiliary-symbol> describes an auxiliary symbol
// which follows a symbol which defines a section

define class <coff-section-auxiliary-symbol> (<coff-auxiliary-symbol>)
  slot auxiliary-section :: <coff-section>,
    required-init-keyword: section:;
  slot auxiliary-check-sum :: <integer> = 0,
    init-keyword: check-sum:;
  slot auxiliary-number :: <integer> = 0,
    init-keyword: number:;
  slot auxiliary-selection :: <integer> = 0,
    init-keyword: selection:;
end class;



// A <coff-chained-auxiliary-symbol> describes an auxiliary symbol
// which references another COFF symbol, to form a chain

define abstract class <coff-chained-auxiliary-symbol> (<coff-auxiliary-symbol>)
  // This slot is expected to be set in any canonical representation
  slot auxiliary-next-function :: false-or(<coff-symbol>) = #f,
    init-keyword: next-function:;
  // But this slot is an alternative representation of the same data
  // which may be used temporarily while reading a file
  constant slot auxiliary-next-function-index :: <integer> = 0,  
    init-keyword: next-function-index:;
end class;



define method next-function-index 
    (symbol :: <coff-chained-auxiliary-symbol>) => (index :: <integer>)
  let next-function = symbol.auxiliary-next-function;
  if (next-function)
    next-function.index;
  else 0;
  end if;
end method;


// A <coff-function-definition-auxiliary-symbol> describes an auxiliary symbol
// entry for function definition symbol record

define class <coff-function-definition-auxiliary-symbol> (<coff-chained-auxiliary-symbol>)
  // This slot is expected to be set in any canonical representation
  slot auxiliary-tag :: <coff-symbol>,
    required-init-keyword: tag:;
  // But this slot is an alternative representation of the same data
  // which may be used temporarily while reading a file
  constant slot auxiliary-tag-index :: <integer> = 0,  
    init-keyword: tag-index:;
  slot auxiliary-total-size :: <integer> = 0,
    init-keyword: total-size:;
  // This slot is expected to be set in any canonical representation
  slot auxiliary-line-numbers :: false-or(<coff-line-number-symbol>) = #f,
    init-keyword: line-numbers:;
  // But this slot is an alternative representation of the same data
  // which may be used temporarily while reading a file
  constant slot auxiliary-line-numbers-offset :: <integer> = 0,  
    init-keyword: line-numbers-offset:;
end class;


// A <coff-function-lines-auxiliary-symbol> describes an auxiliary symbol
// entry for a .bf or .ef function symbol record

define class <coff-function-lines-auxiliary-symbol> (<coff-chained-auxiliary-symbol>)
  slot auxiliary-line-number :: <integer> = 0,
    init-keyword: line-number:;
end class;


/// A <coff-plain-auxiliary-symbol> is one that we don't have any context
/// information about.

define class <coff-plain-auxiliary-symbol> (<coff-auxiliary-symbol>)
  slot auxiliary-data :: <vector>,
    init-keyword: data:;
end class;


define method make 
    (class == <coff-auxiliary-symbol>, #rest keys, #key, #all-keys) 
    => (r :: <coff-plain-auxiliary-symbol>)
  apply(make, <coff-plain-auxiliary-symbol>, keys);
end method;




/// Abstract class <coff-symbol-locator> is the object that describes the 
/// section for COFF symbols. This might be a real section (<coff-section>)
/// or it might be undefined (<coff-undefined-locator>).

define abstract class <coff-symbol-locator> (<coff-index>)
end class;


define open generic big-endian? (obj :: <object>) => (be? :: <boolean>);

/// Class <coff-section> contains the data in a COFF section.

define abstract class <coff-section> (<binary-section>, <coff-symbol-locator>)
  constant slot coff-data-units :: <integer> = 1, 
    init-keyword: units:;             // size of an element in bytes
  constant slot big-endian? :: <boolean> = #f,
    init-keyword: big-endian?:;
  slot virtual-size :: <integer> = 0,
    init-keyword: virtual-size:;      // virtual size when loaded into memory
  slot rva-offset :: <integer> = 0,
    init-keyword: rva-offset:;        // relative virtual address
  slot raw-data-pointer :: <integer> = 0,
    init-keyword: raw-data-pointer:;  // Pointer to raw data (need not be used)
  slot relocs-pointer :: <integer> = 0,
    init-keyword: relocs-pointer:;    // Pointer to relocs (need not be used)
  slot relocs-number :: <integer> = 0,
    init-keyword: relocs-number:;     // Number of relocs (need not be used)
  slot linenumbers-pointer :: <integer> = 0,
    init-keyword: lines-pointer:;     // Pointer to lines (need not be used)
  slot linenumbers-number :: <integer> = 0,
    init-keyword: lines-number:;      // Number of lines (need not be used)
  slot relocations :: <vector> = make(<stretchy-vector>),  
    init-keyword: relocations:;       // vector of <coff-relocation>s
  slot line-numbers :: <vector> = make(<stretchy-vector>),
    init-keyword: line-numbers:;
end class;

ignore(coff-data-units); // TODO: WHY IS THIS UNREFERENCED?


/// A <coff-bss-section> contains data which is not represented in the 
/// COFF file - so it requires special treatment. For these sections, the 
/// raw-data-size slot is always used to determine the size of the section data.

define class <coff-bss-section> (<coff-section>)
end class;


/// A <coff-normal-section>, on the other hand, contains data which is both
/// mapped and represented in the file

define class <coff-normal-section> (<coff-section>)
end class;


define method make 
    (class == <coff-section>, #rest keys, #key flags, #all-keys) 
    => (r :: <coff-section>)
  if (generic-logand(flags, $cnt-uninitialized-data) = 0)
    // This isn't an unitialized section - so it's normal
    apply(make, <coff-normal-section>, keys);
  else
    // This is an unitialized section - so it's bss
    apply(make, <coff-bss-section>, keys);
  end if;
end method;



// section-data-size-in-file is an accessor which determines how much space 
// the section data physically consumes in a coff file.

define method section-data-size-in-file 
    (section :: <coff-section>) => (r :: <integer>)
  section.raw-data-size;
end method;


define method section-data-size-in-file 
    (section :: <coff-bss-section>) => (r :: <integer>)
  0;
end method;


// section-data-size-in-image is an accessor which determines how much space 
// the section data physically consumes when loaded in an application

define method section-data-size-in-image
    (section :: <coff-section>) => (r :: <integer>)
  section.raw-data-size;
end method;


define method section-data-size-in-image
    (section :: <coff-bss-section>) => (r :: <integer>)
  section.raw-data-size;
end method;



/// COFF-SECTION-UNDEFINED
/// The canonical undefined sections for external COFF symbols.

define class <coff-undefined-locator> (<coff-symbol-locator>)
end class;

define constant coff-section-undefined
  = make(<coff-undefined-locator>, index: 0);

define constant coff-section-sym-absolute
  = make(<coff-undefined-locator>, index: -1);

define constant coff-section-sym-debug
  = make(<coff-undefined-locator>, index: -2);



/// <COFF-LINE-NUMBER>
/// An abstract class for all types of line number information.
/// Concrete classes correspond to either function identifiers
/// or line number locations relative to those functions

define abstract class <coff-line-number> (<coff-unit>)
  constant slot line-number :: <integer>,
    required-init-keyword: line-number:;
  slot line-file-offset :: <integer> = 0,
    init-keyword: offset:;  // Will be filled in while writing
end class;


define class <coff-line-number-symbol> (<coff-line-number>)
  slot line-number-symbol :: <coff-symbol>,
    init-keyword: symbol:;
  keyword line-number:, init-value: 0;
end class;


define class <coff-line-number-relative> (<coff-line-number>)
  slot line-number-rva :: <integer> = 0,
    init-keyword: rva:;
end class;



/// <COFF-RELOCATION>
/// An abstract class for all types of relocations
/// Concrete classes correspond to each specific type

define abstract class <coff-relocation> (<coff-index>)
  slot relocation-type :: <coff-short> = 0,
    init-keyword: relocation-type:;
end class;


/// A <coff-genuine-relocation> has a symbol, and may legally
/// be included in a physical coff file.

define abstract class <coff-genuine-relocation> (<coff-relocation>)
  slot relocation-symbol :: <coff-symbol>,
    required-init-keyword: symbol:;
end class;


/// A <coff-interactor-relocation>, however may not

define class <coff-interactor-relocation> (<coff-relocation>)
  constant slot interactor-handle :: <object>, 
    required-init-keyword: handle:;
end class;


define class <coff-absolute-relocation> (<coff-genuine-relocation>)
end class;


define class <coff-relative-relocation> (<coff-genuine-relocation>)
end class;



define sealed method make 
    (class == <coff-relocation>, #rest keys, #key type, #all-keys) 
    => (r :: <coff-relocation>)
  let wanted-class =
    select (type)
      #"relative"   => <coff-relative-relocation>;
      #"absolute"   => <coff-absolute-relocation>;
      #"interactor" => <coff-interactor-relocation>;
    end select;
  apply(make, wanted-class, keys);
end method;



/// Abstract COFF tables
/// These are containers for tables, but are not actually collections 
/// themselves. Data is represented as both tables and a sequence,
/// because each turns out to be useful for different stages of the 
/// coff manipulation process


define class <coff-table> (<binary-table>, <coff-unit>)
  constant slot id-table-data :: <table> = make(<table>),
    init-keyword: id-table-data:;
  slot ordered-data :: <vector>,
    init-keyword: ordered-data:;
end class;


/// A <coff-duplicated-table> has each entry appearing in the table
/// of data and the ordered-data sequence

define class <coff-duplicated-table> (<coff-table>)
  // symbol table for COFF
  // keyed by <byte-string>s
  // values are coff-symbols
  inherited slot ordered-data,
    init-function: curry(make, <stretchy-vector>);
end class;


define generic coff-element 
    (table :: <coff-table>, key, #key model-object) => elt;

define generic coff-element-setter 
    (val, table :: <coff-table>, key, #key model-object) => val;


/// The default methods for these generics are keyed by strings.


define method coff-element 
  (coff-table :: <coff-table>, key, #key model-object = unsupplied()) => (elt)
  if (supplied?(model-object))
    coff-table.id-table-data[model-object];
  else
    coff-table.table-data[key];
  end if;
end method;

define method coff-element-setter
  (newval, coff-table :: <coff-table>, key, #key model-object = unsupplied()) => (elt)
  if (supplied?(model-object))
    coff-table.id-table-data[model-object] := newval;
  else
    coff-table.table-data[key] := newval;
  end if;
end method;

define method binary-table-member? 
      (coff-table :: <coff-table>, key, #key model-object = unsupplied())
   => (boolean :: <object>)
   if (supplied?(model-object))
     element(coff-table.id-table-data, model-object, default: #f);
   else
     element(coff-table.table-data, key, default: #f);
   end if;
end method;


// BINARY-ELEMENT-ADD! is similar to COFF-ELEMENT-SETTER, but it adds
// the newval into both strands of a duplicated table


define method binary-element-add!
  (coff-table :: <coff-table>, key, newval, #key model-object = unsupplied()) => (newval)
  if (supplied?(model-object))
    coff-table.id-table-data[model-object] := newval;
  else
    coff-table.table-data[key] := newval;
  end if;
end method;

define method binary-element-add!
      (coff-table :: <coff-duplicated-table>, 
       key,
       newval, #key model-object = unsupplied()) => (newval)
  add!(coff-table.ordered-data, newval);
  if (supplied?(model-object))
    coff-table.id-table-data[model-object] := newval;
  else
    coff-table.table-data[key] := newval;
  end if;
end method;



/// Symbol tables

define class <coff-symbol-table> (<coff-duplicated-table>)
  // symbol table for COFF
  // keyed by <byte-string>s
  // values are coff-symbols
end class;


define method binary-element-add!
      (coff-table :: <coff-symbol-table>, 
       key :: <byte-string>, 
       newval :: <coff-symbol>,
       #key model-object = unsupplied()) => (newval)
  // fill in the symbol number (starting from 0)
  newval.index := coff-table.ordered-data.size; 
  next-method();
end method;


define method binary-element-add!
      (coff-table :: <coff-symbol-table>, 
       dummy-key :: <byte-string>,
       newval :: <coff-auxiliary-symbol>,
       #key model-object = unsupplied()) => (newval)
  // auxiliary symbols are not put in the table - only in the ordered data
  // fill in the symbol number (starting from 0)
  newval.index := coff-table.ordered-data.size; 
  add!(coff-table.ordered-data, newval);
  newval;
end method;



/// section tables

define class <coff-section-table> (<coff-duplicated-table>)
  // table of COFF sections
  // keyed by <byte-string>s
  // values are coff-sections
end class;


define method binary-element-add!
      (coff-table :: <coff-section-table>, 
       key :: <byte-string>, 
       newval :: <coff-section>,
       #key model-object = unsupplied()) => (newval)
  next-method();
  // fill in the section number (starting from 1)
  newval.index := coff-table.ordered-data.size; 
  newval;
end method;


/// string tables (these are not duplicated tables, because 
/// the ordered-data and the table hold different objects)

define class <coff-string-table> (<coff-table>)
  // table of COFF strings
  // keyed by <byte-string>s
  // values are coff-long-strings
end class;



/// The outermost class: the <coff-file> itself


define primary class <coff-file> (<binary-file>, <coff-unit>)
  /// Start with some magic values for the header
  constant slot big-endian? :: <boolean> = #f,
    init-keyword: big-endian?:;
  constant slot machine            :: <coff-short>,
    required-init-keyword: machine:;
  slot time-stamp         :: <coff-word> = 0,
    init-keyword: time-stamp:;
  slot header-size        :: <coff-short> = 0,
    init-keyword: header-size:;
  slot characteristics    :: <coff-short> = 0,
    init-keyword: characteristics:;
  /// Now the real data
  // sections represented as a table
  inherited slot sections = make(<coff-section-table>);
  // Symbols represented as a table
  constant slot symbols :: <coff-symbol-table> = make(<coff-symbol-table>),
    init-keyword: symbols:;
  // strings represented as a table
  constant slot strings :: <coff-string-table> = make(<coff-string-table>),
    init-keyword: strings:;
end class;

