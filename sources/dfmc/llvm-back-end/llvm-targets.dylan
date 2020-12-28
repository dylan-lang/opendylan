Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Target calling convention support

define method llvm-back-end-calling-convention-fast
    (back-end :: <llvm-back-end>)
 => (calling-convention :: <integer>);
  $llvm-calling-convention-fast
end method;


/// OS platform subclasses

define abstract class <llvm-unix-back-end> (<llvm-back-end>)
end class;

define abstract class <llvm-darwin-back-end> (<llvm-back-end>)
end class;

define abstract class <llvm-windows-back-end> (<llvm-back-end>)
end class;


/// Processor subclasses

// x86

define abstract class <llvm-x86-back-end> (<llvm-back-end>)
end class;

define method back-end-word-size
    (back-end :: <llvm-x86-back-end>)
 => (number-bytes :: <integer>)
  4
end method back-end-word-size;

define method llvm-back-end-data-layout
    (back-end :: <llvm-x86-back-end>) => (layout :: <string>);
  "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
    "i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-"
    "a0:0:64-f80:32:32-n8:16:32"
end method;

define method llvm-back-end-unwind-exception-size
    (back-end :: <llvm-x86-back-end>)
 => (number-words :: <integer>)
  8
end method;

define method llvm-back-end-unwind-exception-alignment
    (back-end :: <llvm-x86-back-end>)
 => (alignment-bytes :: <integer>)
  16
end method;

// x86_64

define abstract class <llvm-x86_64-back-end> (<llvm-x86-back-end>)
end class;

define method back-end-word-size
    (back-end :: <llvm-x86_64-back-end>)
 => (number-bytes :: <integer>)
  8
end method back-end-word-size;

define method llvm-back-end-data-layout
    (back-end :: <llvm-x86_64-back-end>) => (layout :: <string>);
  "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
    "i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-"
    "a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
end method;

define method llvm-back-end-unwind-exception-size
    (back-end :: <llvm-x86_64-back-end>)
 => (number-words :: <integer>)
  4
end method;

// PowerPC (32-bit)

define abstract class <llvm-ppc-back-end> (<llvm-back-end>)
end class;

define method back-end-word-size
    (back-end :: <llvm-ppc-back-end>)
 => (number-bytes :: <integer>)
  4
end method back-end-word-size;

define method llvm-back-end-data-layout
    (back-end :: <llvm-ppc-back-end>) => (layout :: <string>);
  "E-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
    "i64:64:64-f32:32:32-f64:64:64-v128:128:128-n32"
end method;

// PowerPC (64-bit)

define abstract class <llvm-ppc64-back-end> (<llvm-ppc-back-end>)
end class;

define method back-end-word-size
    (back-end :: <llvm-ppc64-back-end>)
 => (number-bytes :: <integer>)
  8
end method back-end-word-size;

define method llvm-back-end-data-layout
    (back-end :: <llvm-ppc64-back-end>) => (layout :: <string>);
  "E-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
    "i64:64:64-f32:32:32-f64:64:64-v128:128:128-n32:64"
end method;

// ARM (32-bit)

define abstract class <llvm-arm-back-end> (<llvm-back-end>)
end class;

// aarch64

define abstract class <llvm-aarch64-back-end> (<llvm-arm-back-end>)
end class;

define method back-end-word-size
    (back-end :: <llvm-aarch64-back-end>)
 => (number-bytes :: <integer>)
  8
end method back-end-word-size;

define method llvm-back-end-data-layout
    (back-end :: <llvm-aarch64-back-end>) => (layout :: <string>);
  "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
end method;

define method llvm-back-end-unwind-exception-size
    (back-end :: <llvm-aarch64-back-end>)
 => (number-words :: <integer>)
  4
end method;

define method llvm-back-end-unwind-exception-alignment
    (back-end :: <llvm-aarch64-back-end>)
 => (alignment-bytes :: <integer>)
  16
end method;


/// Concrete LLVM back-end subclasses

// x86-windows

define class <llvm-x86-windows-back-end> (<llvm-x86-back-end>,
                                        <llvm-windows-back-end>)
end class;

register-back-end(<llvm-x86-windows-back-end>, #"llvm", #"x86-win32");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86-windows-back-end>) => (triple :: <string>);
  "i386-unknown-win32"
end method;

define method llvm-back-end-data-layout
    (back-end :: <llvm-x86-windows-back-end>) => (layout :: <string>);
  "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
    "i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-"
    "v128:128:128-a0:0:64-f80:32:32-n8:16:32"
end method;

// x86_64-darwin

define class <llvm-x86_64-darwin-back-end> (<llvm-x86_64-back-end>,
                                            <llvm-darwin-back-end>)
end class;

register-back-end(<llvm-x86_64-darwin-back-end>,
                  #"llvm", #"x86_64-darwin");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86_64-darwin-back-end>) => (triple :: <string>);
  "x86_64-apple-macosx10.14.0"
end method;

define method llvm-back-end-data-layout
    (back-end :: <llvm-x86_64-darwin-back-end>) => (layout :: <string>);
  "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
end method;

// x86-linux

define class <llvm-x86-linux-back-end> (<llvm-x86-back-end>,
                                        <llvm-unix-back-end>)
end class;

register-back-end(<llvm-x86-linux-back-end>, #"llvm", #"x86-linux");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86-linux-back-end>) => (triple :: <string>);
  "i386-unknown-linux"
end method;

// x86_64-linux

define class <llvm-x86_64-linux-back-end> (<llvm-x86_64-back-end>,
                                           <llvm-unix-back-end>)
end class;

register-back-end(<llvm-x86_64-linux-back-end>, #"llvm", #"x86_64-linux");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86_64-linux-back-end>) => (triple :: <string>);
  "x86_64-unknown-linux"
end method;

// x86-freebsd

define class <llvm-x86-freebsd-back-end> (<llvm-x86-back-end>,
                                          <llvm-unix-back-end>)
end class;

register-back-end(<llvm-x86-freebsd-back-end>, #"llvm", #"x86-freebsd");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86-freebsd-back-end>) => (triple :: <string>);
  "i386-unknown-freebsd"
end method;

// x86_64-freebsd

define class <llvm-x86_64-freebsd-back-end> (<llvm-x86_64-back-end>,
                                             <llvm-unix-back-end>)
end class;

register-back-end(<llvm-x86_64-freebsd-back-end>,
                  #"llvm", #"x86_64-freebsd");

define method llvm-back-end-target-triple
    (back-end :: <llvm-x86_64-freebsd-back-end>) => (triple :: <string>);
  "x86_64-unknown-freebsd"
end method;

// aarch64-linux

define class <llvm-aarch64-linux-back-end> (<llvm-aarch64-back-end>,
                                            <llvm-unix-back-end>)
end class;

register-back-end(<llvm-aarch64-linux-back-end>, #"llvm", #"aarch64-linux");

define method llvm-back-end-target-triple
    (back-end :: <llvm-aarch64-linux-back-end>) => (triple :: <string>);
  "aarch64-unknown-linux-gnu"
end method;


/// LLVM Thread-local storage platform support

define method llvm-thread-local-support?
    (back-end :: <llvm-back-end>)
 => (support? :: <boolean>)
  #t
end method;

define method llvm-thread-local-support?
    (back-end :: <llvm-windows-back-end>)
 => (support? :: <boolean>)
  #f
end method;
