module:    binary-manager
Synopsis:  Support for assembling and dumping BINARY files
Author:    Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Class <binary-section> contains the data in a BINARY section.

define open abstract class <binary-section> (<object>)
  constant slot section-name,
    init-keyword: name:;
  constant slot section-alignment :: <integer> = 4,
    init-keyword: alignment:;         // default section alignment
  constant slot section-flags,
    init-keyword: flags:;
  slot section-data :: <byte-vector> = make(<byte-vector>),
    init-keyword: data:;
  slot raw-data-size :: <integer> = 0,
    init-keyword: raw-data-size:;     // Fill pointer for raw data
end class;


/// The outermost class: the <binary-file> itself


define open class <binary-file> (<object>)
  /// Now the real data
  // sections represented as a table
  constant slot sections :: <binary-table> = make(<binary-table>),
    init-keyword: sections:;
end class;

define open class <binary-table> (<object>)
  constant slot table-data :: <string-table> = make(<string-table>),
    init-keyword: table-data:;
end class;

define open generic binary-table-member? 
    (table :: <binary-table>, key, #key model-object) => (boolean);

define method binary-table-member? 
      (binary-table :: <binary-table>, key, #key model-object)
   => (boolean :: <object>)
  element(binary-table.table-data, key, default: #f);
end method;


define open generic binary-element-add!
    (table :: <binary-table>, key, newval, #key model-object) => (val);

define method binary-element-add!
  (binary-table :: <binary-table>, key, newval,
   #key model-object) => (newval)
  binary-table.table-data[key] := newval;
end method;


define open generic write-binary (stream :: <stream>, binary) => ();

define method write-binary (stream :: <stream>, binary-file :: <binary-file>) => ()
  write-section-data (stream, binary-file);
end method;

define open generic write-binary-data (stream, binary) => ();

define method write-binary-data (stream, binary-file :: <binary-file>) => ()
  write-data-section-data (stream, binary-file);
end method;


define open generic write-binary-section
    (stream :: <stream>, binary-file :: <binary-file>, section :: <binary-section>)
     => ();

define open generic write-section-data
     (stream :: <stream>, binary-file :: <binary-file>) => ();

define method write-section-data
     (stream :: <stream>, binary-file :: <binary-file>) => ()
  for (section :: <binary-section> in binary-file.sections.table-data)
    write-binary-section(stream, binary-file, section);
  end for;
end method;

define method write-data-section-data
     (stream :: <stream>, binary-file :: <binary-file>) => ()
  for (section :: <binary-section> in binary-file.sections.table-data)
    if (section.binary-data-section?)
      write-binary-section(stream, binary-file, section);
    end if;
  end for;
end method;

define open generic binary-data-section?(section :: <binary-section>)
 => (data-section? :: <boolean>);

