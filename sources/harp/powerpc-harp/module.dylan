module:    dylan-user
Synopsis:  The module definition for the POWERPC-HARP module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module powerpc-harp
  use functional-dylan;
  use dylan-extensions,
    import: {<abstract-integer>, <simple-integer-vector>,
	     <machine-word>, <simple-machine-word-vector> };
  use machine-word-lowlevel;
  use big-integers, prefix: "generic-";
  use streams;
  use streams-internals, import: {<byte-file-stream>};
  use format;
  use format-out;
  use print;
  use dfmc-back-end-protocol;
  use native-harp, export: all;
  use native-harp-for-extenders;
  use gnu-outputter;

  export

    // Powerpc instructions

    <powerpc-back-end>,
    <powerpc-macos-back-end>,
    <powerpc-linux-back-end>,

    // ins--machine-specific,
    ins--get-control-register,
    ins--set-control-register,
    ins--bsr, 
    ins--save-return-address,
    ins--restore-return-address,

    read-return-address, write-return-address,

    // registers
    reg--tmp1, reg--tmp2, reg--tmp3, 
    reg--stack, reg--frame, reg--arg0, 
    reg--mlist, reg--function,
    reg--teb, reg--link
    ;

end module;


define module powerpc-harp-test
  use dylan;
  use streams;
  use format;
  use print;
  use native-harp-for-extenders;
  use powerpc-harp;
  use source-records, 
    exclude: { source-record-start-line, source-record-end-line };
end module;
    
