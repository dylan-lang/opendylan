module:     environment-protocols
synopsis:   Protocols describing memory/register locations, and for obtaining
            data from them in various forms.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <ADDRESS-DISPLAY-FORMAT>
//    Describes a number base suitable for the display of data that may be
//    addresses.

define constant <address-display-format> =
  one-of(#"octal", 
         #"decimal", 
         #"hexadecimal");


///// <NON-ADDRESS-DISPLAY-FORMAT>
//    Describes a number base suitable for the display of data that are not
//    addresses.

define constant <non-address-display-format> =
  one-of(#"byte-character",
         #"unicode-character",
         #"single-float",
         #"double-float");


///// <DATA-DISPLAY-FORMAT>
//    A union of the above types, therefore describing a number base for
//    the display of arbitrary data.

define constant <data-display-format> = 
  type-union(<address-display-format>, <non-address-display-format>);


///// <DATA-DISPLAY-SIZE>
//    Describes a size constraint on the importing and printing of data
//    read from an address.

define constant <data-display-size> =
  one-of(#"byte",        /* 8-bit value */
         #"short",       /* 16-bit value */
         #"long",        /* 32-bit value */
         #"hyper",       /* 64-bit value. Terminology comes from IDL. */
         #"float",       /* Single-precision floating-point value */
         #"double",      /* Double-precision floating-point value */
         #"word");       /* Platform-word-size value */


///// <ADDRESS-OBJECT>
//    Nothing interesting is held in the class itself, except for the
//    inherited application proxy and project slots.

define class <address-object> (<application-object>)
end class;


///// $INVALID-ADDRESS-OBJECT
//    This unique instance of <address-object> serves as a legal member of
//    the type, but without any valid interpretation. This is used in
//    preference to #f as a failing result or argument.

define constant $invalid-address-object = 
  make(<address-object>, application-object-proxy: #f);


///// ADDRESS-TO-STRING
//    Synopsis: Converts an abstract address into a printable string.
//    Inputs:
//       server      - The backend dispatching object.
//       address     - Instance of <address-object> to be printed.
//       format      - The format (number base) of the required string.
//                     (Note: The size of the data is dictated by the runtime
//                     platform, so is not supplied to this function).
//    Outputs:
//       A string of fixed size per runtime platform, padded with leading
//       zeros if necessary, and formatted as per the supplied number base.
//       The string contains no extra decoration. This must be added by the
//       UI where required.
//       If the supplied address is invalid, the server will return a
//       string of the correct size, but filled with question-mark ('?')
//       characters.

define open generic address-to-string
   (server :: <server>, address :: <address-object>, 
    #key format :: <address-display-format>)
 => (s :: <string>);

define method address-to-string
   (project :: <project-object>, address :: <address-object>,
    #key format :: <address-display-format> = #"hexadecimal")
 => (s :: <string>)
  let server = choose-server(project, address);
  address-to-string(server, address, format: format);
end method;


///// STRING-TO-ADDRESS
//    Synopsis: Converts a string into an abstract address.
//    Inputs:
//       server      - The backend dispatching object.
//       str         - A string representing a numeric value which needs to
//                     be converted to a runtime address.
//       format      - The format (number base) within which the string
//                     should be interpreted by the server.
//    Outputs:
//       addr        - An instance of <address-object> corresponding to
//                     the supplied string, assuming success. (See note).
//    Note:
//       This is not a parsing function. For a string that is not well-formed,
//       the address returned may be invalid, or otherwise nonsensical.
//       Parsing should be performed by the UI, which should also undertake
//       to strip away any extra decoration that it might require in order
//       to determine the number base (eg. "#x").

define open generic string-to-address
   (server :: <server>, str :: <string>,
    #key format :: <address-display-format>)
 => (addr :: <address-object>);

define method string-to-address
  (project :: <project-object>, str :: <string>,
   #key format :: <address-display-format> = #"hexadecimal")
 => (addr :: <address-object>)
  // Can't use choose-server here, since there is no incoming env object
  // to dispatch on?
  let server = project.project-application;
  (server & string-to-address(server, str, format: format))
    | $invalid-address-object
end method;


///// ADDRESS-APPLICATION-OBJECT
//    Synopsis: Converts an abstract address to a more specific application
//              object where possible. (Eg. if the address corresponds
//              exactly to a dylan object, then the dylan object will be
//              returned).
//    Inputs:
//       server      - The backend dispatching object.
//       addr        - The address to be converted.
//    Outputs:
//       obj         - An instance of <application-object>, possibly a
//                     <foreign-object>. If the address is not valid, then
//                     it may just be returned unchanged, which is to contract
//                     since <address-object> is itself a subclass of
//                     <application-object>.

define open generic address-application-object
  (server :: <server>, addr :: <address-object>) 
 => (obj :: <application-object>);

define method address-application-object
  (project :: <project-object>, addr :: <address-object>)
 => (obj :: <application-object>)
  let server = choose-server(project, addr);
  (server & address-application-object(server, addr)) | $invalid-address-object
end method;


///// APPLICATION-OBJECT-ADDRESS
//    Synopsis: Converts an application object to an address where possible.
//    Inputs:
//       server      - The backend dispatching object.
//       obj         - The application object to be converted.
//    Outputs:
//       addr        - An instance of <address-object> that most directly
//                     corresponds to the original application object.
//                     (Eg. direct conversion from a dylan pointer into an
//                     address).
//                     For application objects that have no meaningful
//                     address, this will return $invalid-address-object.

//    In most cases, it's clear what the "address" of an application
//    object is, since it's value is likely to be a pointer. However,
//    there are some special cases, and the behaviour of this protocol
//    for those cases is given below:

//    <component-object>     - Returns the "base address" of the component.
//                             ie. The address at which it is loaded into
//                             memory. (It is assumed that such an address is
//                             both meaningful and obtainable for all
//                             platforms).

//    <stack-frame-object>   - Returns the "frame pointer" of the stack
//                             frame, if it can be determined.

define open generic application-object-address
    (server :: <server>, object :: <application-object>)
 => (address :: false-or(<address-object>));

define method application-object-address
    (project :: <project-object>, object :: <application-object>)
 => (addr :: false-or(<address-object>))
  let server = project.project-application;
  server & application-object-address(server, object)
end method application-object-address;


///// INDIRECT-ADDRESS
//    Synopsis: Indirects through an address to generate a new address.
//    Inputs:
//       server      - The backend dispatching object.
//       addr        - The address through which to perform the indirection.
//    Outputs:
//       i-addr      - The address object obtained by indirecting through
//                     the original address.
//                     It is entirely possible for this address to be invalid,
//                     and this will certainly be the case if the original
//                     address is invalid.

define open generic indirect-address
  (server :: <server>, addr :: <address-object>) => (i-addr :: <address-object>);

define method indirect-address
  (project :: <project-object>, addr :: <address-object>) 
 => (i-addr :: <address-object>);
  let server = choose-server(project, addr);
  (server & indirect-address(server, addr)) | $invalid-address-object
end method;


///// INDEXED-ADDRESS
//    Synopsis: Adds an indexed-offset to a base address.
//    Inputs:
//      server   - The project dispatching context
//      addr     - The base address.
//      index    - An integer used as the index. 
//      size     - An instance of <data-display-size>. The implementation
//                 will multiply the index by the appropriate factor according
//                 to this. The default is #"word".
//    Outputs:
//      i-addr   - The indexed address.

define open generic indexed-address
  (server :: <server>, addr :: <address-object>, i :: <integer>,
   #key size :: <data-display-size>) 
 => (i-addr :: <address-object>);

define method indexed-address
  (project :: <project-object>, addr :: <address-object>, i :: <integer>,
   #key size :: <data-display-size> = #"word") 
 => (i-addr :: <address-object>);
  let server = choose-server(project, addr);
  (server & indexed-address(server, addr, i, size: size)) 
    | $invalid-address-object
end method;


///// ADDRESS-READ-MEMORY-CONTENTS
//    Synopsis: Import a block of memory contents starting at the supplied
//              address, and return the contents as formatted strings. Also
//              returns the address that immediately follows the block that
//              has been read.
//    Inputs:
//       server      - The backend dispatching object.
//       addr        - The address at which to base the import.
//       size        - The granularity at which to read data, defaults
//                     to #"word" (the runtime platform word-size).
//       format      - The format directive for the imported data.
//       from-index  - An array index, interpreted according to the 'size'
//                     parameter, from which to read the first object.
//                     Defaults to zero.
//       to-index    - The index at which to read the last object. This
//                     defaults to 7.

define open generic address-read-memory-contents
  (server :: <server>, addr :: <address-object>,
   #key size :: <data-display-size>,
        format :: <data-display-format>,
        from-index :: <integer>,
        to-index :: <integer>)
 => (printable-strings :: <sequence>, nxt :: <address-object>);

define method address-read-memory-contents
  (project :: <project-object>, addr :: <address-object>,
   #key size :: <data-display-size> = #"word",
        format :: <data-display-format> = #"hexadecimal",
        from-index :: <integer> = 0,
        to-index :: <integer> = 7)
 => (printable-strings :: <sequence>, nxt :: <address-object>)
  let server = choose-server(project, addr);
  if (server)
    address-read-memory-contents(server,
                                 addr,
                                 format: format,
                                 from-index: from-index, to-index: to-index,
                                 size: size)
  else
    values(#[], $invalid-address-object)
  end if
end method address-read-memory-contents;


///// ADDRESS-READ-APPLICATION-OBJECT
//    Synopsis: Import an application object from an address.
//    Inputs:
//       server      - The backend dispatching object.
//       addr        - The address at which to base the import.
//    Outputs:
//       obj         - Returns the imported application object, or #f if the
//                     import fails.

define open generic address-read-application-object
  (server :: <server>, addr :: <address-object>)
 => (obj :: false-or(<application-object>));

define method address-read-application-object
  (project :: <project-object>, addr :: <address-object>)
 => (obj :: false-or(<application-object>))
  let server = choose-server(project, addr);
  server & address-read-application-object(server, addr);
end method;
