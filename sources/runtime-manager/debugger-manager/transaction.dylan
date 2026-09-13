module:      dm-internals
synopsis:    Debugger Transaction
author:      Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <PAGE-RELATIVE-OBJECT-TABLE>
//    A class used to store remote dylan objects in fast-lookup form.
//    The values in the table can be arbitrary information that needs to
//    be obtained from the key.
define open abstract class <page-relative-object-table> (<object>)
  constant slot lookup-table-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;
  slot objects-by-virtual-page :: <table> = make(<table>);
end class;

// Make it instantiable.

define class <dm-page-relative-object-table> (<page-relative-object-table>)
end class;

define method make
    (c == <page-relative-object-table>, #rest keys, #key, #all-keys)
 => (inst :: <page-relative-object-table>)
  apply(make, <dm-page-relative-object-table>, keys)
end method;


///// <PAGE-RELATIVE-OBJECT-TABLE-ENTRY>
//    Any object stored as a value in a <page-relative-object-table> must
//    be a general instance of this class.
define open abstract class <page-relative-object-table-entry> (<object>)
end class;


///// INVALIDATE-PAGE-RELATIVE-OBJECT-TABLE
//    Removes all entries.
define open generic invalidate-page-relative-object-table
    (table :: <page-relative-object-table>) => ();

define method invalidate-page-relative-object-table
    (table :: <page-relative-object-table>) => ()
  table.objects-by-virtual-page := make(<table>)
end method;


///// ENQUIRE-OBJECT
//    Checks to see whether a dylan object is present in a table. If so,
//    returns the description that was supplied to ADD-OBJECT when the
//    object was put into the table, otherwise returns #f.
define open generic enquire-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => (entry :: false-or(<page-relative-object-table-entry>));

define method enquire-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => (entry :: false-or(<page-relative-object-table-entry>))
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  if (subtable)
    element(subtable, offset, default: #f)
  else
    #f
  end if
end method;


///// ADD-OBJECT
//    Adds an object to the page-relative-object-table. If there is
//    already an entry for the object, it will be overwritten with this
//    entry.
//    (Generic function and its default method)
define open generic add-object
    (table :: <page-relative-object-table>, instance :: <remote-value>,
     entry :: <page-relative-object-table-entry>)
 => ();

define method add-object
    (table :: <page-relative-object-table>, instance :: <remote-value>,
     entry :: <page-relative-object-table-entry>)
 => ()
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  unless (subtable)
    subtable := make(<table>);
    page-table[page] := subtable;
  end unless;
  subtable[offset] := entry;
end method;


///// REMOVE-OBJECT
//    Removes an object from a page-relative table. Silently does nothing
//    if the object is not present in the table.
//    (Generic function and its default method).
define open generic remove-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => ();

define method remove-object
    (table :: <page-relative-object-table>, instance :: <remote-value>)
 => ()
  let application = table.lookup-table-debug-target;
  let page-table = table.objects-by-virtual-page;
  let path = application.debug-target-access-path;
  let (page, offset) = page-relative-address(path, instance);
  let subtable = element(page-table, page, default: #f);
  if (subtable)
    let entry-count = subtable.size;
    let entry = element(subtable, offset, default: #f);
    if (entry)
      remove-key!(subtable, offset);
      if (entry-count == 1)
        remove-key!(page-table, page);
      end if
    end if
  end if
end method;
