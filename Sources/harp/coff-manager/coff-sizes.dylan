module:    coff-sizes
Synopsis:  Sizing utilities for COFF units
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define generic unit-size
    (coff-unit :: <coff-unit>) => (size :: <integer>);

define generic file-headers-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic section-headers-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic sections-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic symbol-table-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic string-table-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic first-section-offset
    (coff-file :: <coff-file>) => (offset :: <integer>);

define generic symbol-table-offset
    (coff-file :: <coff-file>) => (offset :: <integer>);

define generic string-table-offset
    (coff-file :: <coff-file>) => (offset :: <integer>);

define generic total-file-size
    (coff-file :: <coff-file>) => (size :: <integer>);

define generic total-relocation-size 
    (section :: <coff-section>) => (size :: <integer>);

define generic total-line-numbers-size 
    (section :: <coff-section>) => (size :: <integer>);



define constant size-of-symbol = 18;

define method unit-size (unit :: <coff-long-string>) => (size :: <integer>)
  unit.string-data.size + 1;  // allow for null termination
end method;

define method unit-size (unit :: <coff-short-string>) => (size :: <integer>)
  8;
end method;

define method unit-size (unit :: <coff-symbol-record>) => (size :: <integer>)
  size-of-symbol;
end method;

define method unit-size (unit :: <coff-section>) => (size :: <integer>)
  // The unit size of a COFF section is the size of the section data
  // plus any relocation or line number information. It does NOT include
  // the size of the section header (which is not located contiguously)
  unit.section-data-size-in-file 
  + unit.total-relocation-size 
  + unit.total-line-numbers-size;
end method;

define method unit-size (unit :: <coff-relocation>) => (size :: <integer>)
  10;
end method;

define method unit-size (unit :: <coff-line-number>) => (size :: <integer>)
  6;
end method;

define method unit-size (unit :: <coff-symbol-table>) => (size :: <integer>)
  reduce(method (val :: <integer>, symbol :: <coff-symbol-record>)
           val + symbol.unit-size
         end method,
         0,
         unit.table-data)
    +
    reduce(method (val :: <integer>, symbol :: <coff-symbol-record>)
	     val + symbol.unit-size
	   end method,
	   0,
	   unit.id-table-data)
end method;

define method unit-size (unit :: <coff-string-table>) => (size :: <integer>)
  4 + reduce(method (val :: <integer>, string :: <coff-long-string>)
               val + string.unit-size
             end method,
             0,
             unit.table-data)
    + reduce(method (val :: <integer>, string :: <coff-long-string>)
               val + string.unit-size
             end method,
             0,
             unit.id-table-data)
end method;


define method total-line-numbers-size 
    (section :: <coff-section>) => (size :: <integer>)
  reduce(method (val :: <integer>, reloc :: <coff-line-number>)
           val + reloc.unit-size
         end method,
         0,
         section.line-numbers);
end method;


define method total-relocation-size 
    (section :: <coff-section>) => (size :: <integer>)
  reduce(method (val :: <integer>, reloc :: <coff-relocation>)
           val + reloc.unit-size
         end method,
         0,
         section.relocations);
end method;


define method file-headers-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  20;
end method;


define method section-headers-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  coff-file.sections.ordered-data.size * coff-file.one-section-header-size;
end method;

define method one-section-header-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  40;
end method;


define method sections-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  reduce(method (val :: <integer>, section :: <coff-section>)
           val + section.unit-size
         end method,
         0,
         coff-file.sections.ordered-data);
end method;


define method symbol-table-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  size-of-symbol * coff-file.symbols.ordered-data.size;
end method;


define method string-table-size
    (coff-file :: <coff-file>) => (size :: <integer>)
  coff-file.strings.ordered-data.size;
end method;


define method first-section-offset
    (coff-file :: <coff-file>) => (offset :: <integer>)
 coff-file.file-headers-size + coff-file.section-headers-size;
end method;


define method symbol-table-offset
    (coff-file :: <coff-file>) => (offset :: <integer>)
 coff-file.first-section-offset + coff-file.sections-size;
end method;


define method string-table-offset
    (coff-file :: <coff-file>) => (offset :: <integer>)
 coff-file.symbol-table-offset + coff-file.symbol-table-size;
end method;


define method total-file-size
    (coff-file :: <coff-file>) => (size :: <integer>)
 coff-file.string-table-offset + coff-file.string-table-size;
end method;
