module:    cv4-builder
Synopsis:  Support for generating CodeView 4 debug info
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Basic CodeView Housekeeping


define constant $dylan-version 
  = "Functional DeveloperWorks (R) Dylan compiler Version 1.0";

define constant $dylan-flags = #x04098010;
  // 04 = Machine: Intel 80486
  // 09 = Language Dylan
  // 80 = Code present
  // 10 = Mode 32


// Initialize-debug-section puts in the appropriate headers for the CV4 
// debug section

define method initialize-debug-section 
    (builder :: <coff-builder>,
     object-file-name :: <byte-string>,
     #key version = $dylan-version,
          section-name = ".debug$S")
 => ()
  let section = select-debug-section(builder, section-name);
  add-integer-data(builder, 1, section: section); // CodeView format
  add-cv4-object-file-name(builder, object-file-name, section: section);
  add-cv4-compile-flag(builder, version, section: section, flags: $dylan-flags);
end method;


// Select-debug-section ensures that the debug section exists,
// and caches it in the debug-section slot in the builder

define method select-debug-section 
    (builder :: <coff-builder>, section-name :: <byte-string>,
     #key alignment = 4, flags = $debug-flags) 
    => (section :: <coff-section>)
  builder.debug-section :=
    share-or-create(builder.binary-file.sections, section-name, unsupplied(),
                    method ()
                      make-binary-section(builder, section-name, 
					  alignment, flags);
                    end method);
end method;


// add-cv4-string adds a string to the CodeView record in normal format - 
// i.e. preceeded by a length byte.

define method add-cv4-string 
    (builder :: <coff-builder>, str :: <string>, 
     #key section = builder.debug-section)
 => ()
  add-data-byte(builder, str.size, section: section);
  add-data-string(builder, str, section: section);
end method;


// coff-string-length calculates the size of a string in CodeView format 
// (i.e. allowing for the length byte).

define method coff-string-length (str :: <string>) => (len :: <integer>)
  str.size + 1; // allow for the length byte
end method;


// start-debug-field is used to start each CodeView record.
// Functions which use this must work out for themselves what
// length of data they directly add to the record, and pass
// that as the length field.

define method start-debug-field 
     (builder :: <coff-builder>, 
      length :: <integer>, index :: <integer>,
      section :: <coff-section>)
 => ()
  add-integer-data-short(builder, length + 2, section: section);
  // length allows for the index too
  add-integer-data-short(builder, index, section: section);
end method;




//// Functions for adding the specific types of CodeView records


define method add-cv4-compile-flag
    (builder :: <coff-builder>, version :: <byte-string>,
     #key flags = 0, section = builder.debug-section)
 => ()
  let len = 4 + version.coff-string-length;
  start-debug-field(builder, len, 1, section);
  add-integer-data(builder, flags, section: section);
  add-cv4-string(builder, version, section: section);
end method;


define method add-cv4-register
    (builder :: <coff-builder>, type :: <integer>, 
     register :: <integer>, name :: <byte-string>, 
     #key section = builder.debug-section)
 => ()
  let len = 4 + name.coff-string-length;
  start-debug-field(builder, len, 2, section);
  add-integer-data-short(builder, type, section: section);
  add-integer-data-short(builder, register, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-user-defined-type
    (builder :: <coff-builder>, type :: <integer>, name :: <string>, 
     #key section = builder.debug-section)
 => ()
  let len = 2 + name.coff-string-length;
  start-debug-field(builder, len, 4, section);
  add-integer-data-short(builder, type, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-end-of-block
    (builder :: <coff-builder>, #key section = builder.debug-section)
 => ()
  let len = 0;
  start-debug-field(builder, len, 6, section);
end method;


define method add-cv4-object-file-name 
    (builder :: <coff-builder>, name :: <string>, 
     #key signature = 0,
          section = builder.debug-section)
 => ()
  let len = 4 + name.coff-string-length;
  start-debug-field(builder, len, 9, section);
  add-integer-data(builder, signature, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-bp-relative
    (builder :: <coff-builder>, 
     offset :: <integer>, type :: <integer>, name :: <byte-string>,
     #key section = builder.debug-section)
 => ()
  let len = 6 + name.coff-string-length;
  start-debug-field(builder, len, #x200, section);
  add-integer-data(builder, offset, section: section);
  add-integer-data-short(builder, type, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-local-data
    (builder :: <coff-builder>, type :: <integer>, name :: <byte-string>,
     #key segment = 0, offset = 0, 
          section = builder.debug-section)
 => ()
  let len = 8 + name.coff-string-length;
  start-debug-field(builder, len, #x201, section);
  add-cv4-relocation(builder, name, unsupplied(), offset, segment, section: section);
  add-integer-data-short(builder, type, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-global-data
    (builder :: <coff-builder>, type :: <integer>, name :: <byte-string>,
     #key segment = 0, offset = 0, 
          section = builder.debug-section)
 => ()
  let len = 8 + name.coff-string-length;
  start-debug-field(builder, len, #x202, section);
  add-cv4-relocation(builder, name, unsupplied(), offset, segment, section: section);
  add-integer-data-short(builder, type, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-local-proc-start
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     length :: <integer>, frame-on :: <integer>, frame-off :: <integer>, 
     proctype :: <integer>,
     #key p-parent = 0, p-end = 0, p-next = 0, 
          segment = 0, offset = 0, flags = 0, 
          section = builder.debug-section)
 => ()
  add-cv4-procedure-start(builder, #x204, name, model-object,
			  length, frame-on, frame-off, 
                          proctype, p-parent, p-end, p-next, 
                          segment, offset, flags, section);
end method;


define method add-cv4-global-proc-start
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     length :: <integer>, frame-on :: <integer>, frame-off :: <integer>, 
     proctype :: <integer>,
     #key p-parent = 0, p-end = 0, p-next = 0, 
          segment = 0, offset = 0, flags = 0, 
          section = builder.debug-section)
 => ()
  add-cv4-procedure-start(builder, #x205, name, model-object,
			  length, frame-on, frame-off, 
                          proctype, p-parent, p-end, p-next, 
                          segment, offset, flags, section);
end method;



define method add-cv4-procedure-start
    (builder :: <coff-builder>, index :: <integer>,
     name :: <byte-string>, model-object,
     length :: <integer>, frame-on :: <integer>, frame-off :: <integer>, 
     proctype :: <integer>,
     p-parent :: <integer>, p-end :: <integer>, p-next :: <integer>, 
     segment :: <integer>, offset :: <integer>, flags :: <integer>, 
     section :: <coff-section>)
 => ()
  let len = 33 + name.coff-string-length;
  start-debug-field(builder, len, index, section);
  add-integer-data(builder, p-parent, section: section);
  add-integer-data(builder, p-end, section: section);
  add-integer-data(builder, p-next, section: section);
  add-integer-data(builder, length, section: section);
  add-integer-data(builder, frame-on, section: section);
  add-integer-data(builder, frame-off, section: section);
  add-cv4-relocation(builder, name, model-object,
		     offset, segment, section: section);
  add-integer-data-short(builder, proctype, section: section);
  add-data-byte(builder, flags, section: section);
  add-cv4-string(builder, name, section: section);
end method;


define method add-cv4-block-start
    (builder :: <coff-builder>, 
     reloc-name :: <byte-string>,
     model-object,
     block-name :: <byte-string>, 
     length :: <integer>, 
     #key p-parent = 0, p-end = 0, 
          segment = 0, offset = 0, 
          section = builder.debug-section)
 => ()
  let len = 18 + block-name.coff-string-length;
  start-debug-field(builder, len, #x207, section);
  add-integer-data(builder, p-parent, section: section);
  add-integer-data(builder, p-end, section: section);
  add-integer-data(builder, length, section: section);
  add-cv4-relocation(builder, reloc-name, model-object,
		     offset, segment, section: section);
  add-cv4-string(builder, block-name, section: section);
end method;


define method add-cv4-code-label
    (builder :: <coff-builder>, name :: <byte-string>, 
     #key segment = 0, offset = 0, flags = 0,
          section = builder.debug-section)
 => ()
  let len = 7 + name.coff-string-length;
  start-debug-field(builder, len, #x209, section);
  add-cv4-relocation(builder, name, unsupplied(), offset, segment, section: section);
  add-data-byte(builder, flags, section: section);
  add-cv4-string(builder, name, section: section);
end method;



define method add-cv4-relocation
    (builder :: <coff-builder>,
     name :: <byte-string>, model-object,
     offset :: <integer>, segment :: <integer>, 
     #key section = builder.debug-section)
 => ()
  add-data(builder, name, model-object,
	   offset: offset, type: #"offset", section: section);
  add-data-short
    (builder, name, model-object,
     offset: segment, type: #"segment", section: section);
end method;
