module:    harp-outputter
Synopsis:  HARP output generation
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// This file defines outputters which output to *standard-output*
//// The following types are defined:
////
//// #"interactive-print-harp"       - a print-harp outputter
//// #"interactive-assembler"        - an assembler outputter
//// #"interactive"                  - a multiplex of the above


define constant $int-harp-type$ =    #"interactive-print-harp";

define constant $int-asm-type$ =     #"interactive-assembler";

define constant $interactive-type$ = #"interactive";

define constant $int-mnemonic-asm-type$ =     #"interactive-mnemonic-assembler";

/// The interactive print-harp ...

define sealed class <harp-interactive-print-outputter> (<harp-print-outputter>)
end class;

define method make-interactive-print-outputter ()
    => (output-stream :: <harp-interactive-print-outputter>)
  make(<harp-interactive-print-outputter>, stream: *standard-output*);
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $int-harp-type$)
    => (output-stream :: <harp-interactive-print-outputter>)
  make-interactive-print-outputter();
end method;

define method close-harp-outputter
    (backend :: <harp-back-end>, 
     outputter :: <harp-interactive-print-outputter>,
     #key, #all-keys) => ()
end method;

// All other methods in the protocol are inherited from <harp-print-outputter>


/// The interactive assembler outputter ... 

define method make-binary-interactive-assembler-outputter ()
    => (output-stream :: <harp-binary-unstreamed-assembler-outputter>)
  make(<harp-binary-unstreamed-assembler-outputter>, stream: *standard-output*);
end method;

define method make-mnemonic-interactive-assembler-outputter ()
    => (output-stream :: <harp-mnemonic-unstreamed-assembler-outputter>)
  make(<harp-mnemonic-unstreamed-assembler-outputter>, stream: *standard-output*);
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $int-asm-type$)
    => (output-stream :: <harp-binary-unstreamed-assembler-outputter>)
  make-binary-interactive-assembler-outputter();
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $int-mnemonic-asm-type$)
    => (output-stream :: <harp-mnemonic-unstreamed-assembler-outputter>)
  make-mnemonic-interactive-assembler-outputter();
end method;


// All methods in the protocol are inherited from 
// <harp-assembler-outputter>

/// The multiplexed interactive outputter ... 

define constant $int-mux-types$ = list($int-harp-type$, $int-asm-type$); 

define method make-interactive-outputter ()
    => (output-stream :: <harp-multiple-outputter>)
  make(<harp-multiple-outputter>, 
       outputters: vector(make-binary-interactive-assembler-outputter(),
                          make-interactive-print-outputter()));
end method;

define method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $interactive-type$)
    => (output-stream :: <harp-multiple-outputter>)
  make-harp-outputter-by-type(backend, filename, $int-mux-types$);
end method;
