module:        ist-implementation
synopsis:      The interactive symbol table class and its interface
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <SUBTABLE-ADDRESS-BOUNDARY> (Internal implementation class)
//    A simple object to encapsulate the two delimiting addresses of
//    a region of memory.

define sealed class <subtable-address-boundary> (<object>)

  constant slot boundary-lower-address :: <remote-value>,
    required-init-keyword: from:;

  constant slot boundary-upper-address :: <remote-value>,
    required-init-keyword: to:;

end class;


///// <REMOTE-LIBRARY-SUBTABLE> (Internal Implementation class)
//    The interactive symbol table is split up into these units, indexed
//    on <remote-library> objects.

define sealed class <remote-library-subtable> (<object>)

  slot subtable-empty? :: <boolean>,
    init-value: #t;

  constant slot subtable-address-boundaries :: <stretchy-vector> =
    make(<stretchy-vector>);

  // (Not constant due to use of idiom x := sort!(x). Maybe could be
  // constant).

  slot symbols-by-address :: <stretchy-vector> = 
    make(<stretchy-vector>);

  constant slot symbols-by-name :: <string-table> = 
    make(<string-table>);

  // Maps <remote-object-file> to a <string-table> for the symbols static
  // object files:

  constant slot statics-by-object-file :: <table> = make(<table>);

end class;


///// <INTERACTIVE-SYMBOL-TABLE> (Exported class)
//    The class that provides an opaque interface to <remote-symbol> objects,
//    and allows new definitions to be created.

define sealed class <interactive-symbol-table> (<object>)

  ///// Exported Slots

  constant slot symbol-table-redefinition-allowed? :: <boolean>,
    init-value: #t,
    init-keyword: redefinition-allowed?:;

  constant slot symbol-table-access-path :: <access-path>,
    required-init-keyword: access-path:;

  ///// Internal Slots

  slot symbol-table-empty? :: <boolean>,
    init-value: #t;

  ///// SYMBOLS-BY-LIBRARY
  //    Maps <remote-library> to <remote-library-subtable>.

  constant slot symbols-by-library :: <table> = make(<table>);

  ///// KNOWN-SYMBOL-LIBRARIES
  //    A sequence of <remote-library>s known to this symbol table.
  //    Every member of this sequence must also be a key in the
  //    above table. Further, there must be no keys in the table that
  //    are not members of this sequence.

  constant slot known-symbol-libraries :: <sequence> = make(<stretchy-vector>);

  // This slot is set to TRUE when a client knows that a number of
  // definitions have to be made at once. The symbols-by-address field
  // must be maintained sorted, but it is inefficient to re-sort the field
  // after every individual addition to the table. When defining a symbol,
  // the sort will be deferred if this flag is set to #t.
  // The library provides a macro to set this flag during a period of
  // 'symbol-table-define-symbol' calls.

  slot performing-multiple-definitions? :: <boolean>,
    init-value: #f;

end class;



///// <INTERACTIVE-SYMBOL-TABLE-ERROR>
//    The superclass of all errors that get signalled from within the
//    implementation of this library.

define abstract class <interactive-symbol-table-error> (<error>)

  constant slot offending-symbol-table :: <interactive-symbol-table>,
    required-init-keyword: table:;

end class;


///// <SYMBOL-DOES-NOT-EXIST-ERROR>
//    An attempt was made to remove a symbolic definition from the table,
//    but there is no such symbol present in the table.

define class <symbol-does-not-exist-error>
                  (<interactive-symbol-table-error>)

  constant slot target-name :: <string>,
    required-init-keyword: name:;

end class;


///// <SYMBOL-NO-LIBRARY-ERROR>
//    An attempt was made to define a symbol, but there was no
//    instance of <remote-library> to associate with it.

define class <symbol-no-library-error>
                 (<interactive-symbol-table-error>)

  constant slot defined-name :: <string>,
    required-init-keyword: name:;

end class;

define class <symbol-static-no-file-error> (<symbol-no-library-error>)
end class;


///// <FILE-NO-LIBRARY-ERROR>
//    An attempt was made to define a file, but there was no
//    instance of <remote-library> to associate with it.

define class <file-no-library-error>
                 (<interactive-symbol-table-error>)

  constant slot defined-filename :: <string>,
    required-init-keyword: name:;

end class;


///// <SYMBOL-ATTEMPTED-DEFINITION-IN-UNDEFINED-FILE-ERROR>
//    An attempt was made to define a symbol within the scope of a
//    <remote-object-file>, but the file was not associated with the
//    <remote-library>.

define class <symbol-attempted-definition-in-undefined-file-error>
                 (<interactive-symbol-table-error>)

  constant slot defined-name :: <string>,
    required-init-keyword: name:;

  constant slot associated-library :: <remote-library>,
    required-init-keyword: library:;

  constant slot undefined-file :: <remote-object-file>,
    required-init-keyword: file:;

end class;


///// <SYMBOL-ILLEGAL-REDEFINITION-ERROR>
//    The error class that describes an attempt to redefine a symbol in
//    a table that does not allow redefinition.

define abstract class <symbol-illegal-redefinition-error> 
                       (<interactive-symbol-table-error>)

  constant slot redefined-name :: <string>,
    required-init-keyword: name:;

  constant slot existing-symbol :: <remote-symbol>,
    required-init-keyword: symbol:;

end class;


///// <SYMBOL-ILLEGAL-REDEFINITION-BY-NAME-ERROR>
//    Trying to redefine a symbol by its name (ie, making that name
//    point to a different address) is only legal in symbol tables
//    that have a "redefinition allowed" policy.

define class <symbol-illegal-redefinition-by-name-error>
            (<symbol-illegal-redefinition-error>)
end class;


///// <SYMBOL-ILLEGAL-REDEFINITION-BY-ADDRESS-ERROR>
//    Trying to redefine a symbol by its address (ie, making a completely
//    different name point to that address) is always illegal!

define class <symbol-illegal-redefinition-by-address-error>
            (<symbol-illegal-redefinition-error>)
end class;
