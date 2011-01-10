module:    harp-outputter
Synopsis:  HARP assembler output generation - basic support
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The harp-outputter module provides minimal support for outputting
//// assembler. There are no methods provided to do the outputting,
//// and back ends must provide their own methods. However, methods
//// are provided for the stream management
////

//// This file defines the outputter type #"assembler"
//// and also the type #"mnemonic-assembler"

define constant $asm-type$ = #"assembler";
define constant $mnemonic-asm-type$ = #"mnemonic-assembler";

/// Back ends may specialize FILE-EXTENSION-FOR-OUTPUTTER-TYPE
/// to get different file extensions.
///

define method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $asm-type$) 
       => (extension :: <byte-string>)
  "asm";
end method;

define method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $mnemonic-asm-type$) 
       => (extension :: <byte-string>)
  "s";
end method;

/// This instantiable class is used as the outputter for HARP asm printing.
///
define abstract class <harp-assembler-outputter> (<stream-wrapper-outputter>)
end class;

/// There are two concrete subclasses - one which manages it's own stream
/// (for close requests) and one which does not.
/// These have now become abstract as we wish to subclass them
/// to allow for mnemonic and binary asm output

define abstract class <harp-streamed-assembler-outputter> (<harp-assembler-outputter>)
end class;

define abstract class <harp-unstreamed-assembler-outputter> (<harp-assembler-outputter>)
end class;

define abstract class <harp-mnemonic-assembler-outputter> (<harp-assembler-outputter>)
end class;

define abstract class <harp-binary-assembler-outputter> (<harp-assembler-outputter>)
end class;

define sealed class <harp-mnemonic-streamed-assembler-outputter> (<harp-streamed-assembler-outputter>,<harp-mnemonic-assembler-outputter>)
end class;

define sealed class <harp-mnemonic-unstreamed-assembler-outputter> (<harp-unstreamed-assembler-outputter>,<harp-mnemonic-assembler-outputter>)
end class;

define sealed class <harp-binary-streamed-assembler-outputter> (<harp-streamed-assembler-outputter>,<harp-binary-assembler-outputter>)
end class;

define sealed class <harp-binary-unstreamed-assembler-outputter> (<harp-unstreamed-assembler-outputter>,<harp-binary-assembler-outputter>)
end class;

define sealed method make (class == <harp-binary-assembler-outputter>, #rest keys, #key, #all-keys) 
    => (r :: <harp-binary-streamed-assembler-outputter>)
  apply(make, <harp-binary-streamed-assembler-outputter>, keys);
end method;

define sealed method make (class == <harp-assembler-outputter>, #rest keys, #key, #all-keys) 
    => (r :: <harp-binary-streamed-assembler-outputter>)
  apply(make, <harp-binary-streamed-assembler-outputter>, keys);
end method;

define sealed method make (class == <harp-mnemonic-assembler-outputter>, #rest keys, #key, #all-keys) 
    => (r :: <harp-mnemonic-streamed-assembler-outputter>)
  apply(make, <harp-mnemonic-streamed-assembler-outputter>, keys);
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $asm-type$)
    => (output-stream :: <harp-assembler-outputter>)
  make(<harp-binary-assembler-outputter>,
       stream: open-output-stream(backend, filename, type));
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $mnemonic-asm-type$)
    => (output-stream :: <harp-mnemonic-assembler-outputter>)
  make(<harp-mnemonic-assembler-outputter>,
       stream: open-output-stream(backend, filename, type));
end method;

// The close method for a streamed assembler outputter closes the inner stream

define method close-harp-outputter
    (backend :: <harp-back-end>, outputter :: <harp-streamed-assembler-outputter>,
     #key, #all-keys) => ()
  close-output-stream(outputter.outputter-stream);
end method;

// However, for an unstreamed assembler outputter the close method does nothing

define method close-harp-outputter
    (backend :: <harp-back-end>, outputter :: <harp-unstreamed-assembler-outputter>,
     #key, #all-keys) => ()
end method;
