module:    pentium-linux-rtg
Synopsis:  Managing the FFI barrier, allocation of TEBs & entry points
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define sideways method op--initialize-thread-instructions (be :: <pentium-linux-back-end>) => ()
  when-base
    op--call-c(be, check-runtime-thread-library-uses-segment-register);
  end when-base;

  let load-offset = 2 + 6 + 6 + 1 + 4 + 3 + 2;
  let store-offset = 2 + 6 + 6 + 3 + 3 + 2;
  let tmp = make-n-register(be);
  when-base
    ins--muls(be, tmp, thread-library-uses-segment-register0, load-offset);
	ins--move(be, segment-register-load-instruction-offset, tmp);
    ins--muls(be, tmp, thread-library-uses-segment-register0, store-offset);
	ins--move(be, segment-register-store-instruction-offset, tmp);
  end;
  when-client
    ins--muls(be, tmp, thread-library-uses-segment-register, load-offset);
	ins--move(be, segment-register-load-instruction-offset, tmp);
    ins--muls(be, tmp, thread-library-uses-segment-register, store-offset);
	ins--move(be, segment-register-store-instruction-offset, tmp);
  end;
end method;

define c-fun runtime-external check-runtime-thread-library-uses-segment-register
  = "check_runtime_thread_library_uses_segment_register";

define c-full-indirect runtime-variable thread-library-uses-segment-register0 = "%thread-library-uses-segment-register?",
   client?: #f, base?: #t;

define c-full-indirect runtime-external thread-library-uses-segment-register = "%thread-library-uses-segment-register?",
   client?: #t, base?: #f;

define runtime-variable segment-register-load-instruction-offset = "%segment-register-load-instruction-offset",
   data: 0, client?: #t, base?: #t;

define runtime-variable segment-register-store-instruction-offset = "%segment-register-store-instruction-offset",
   data: 0, client?: #t, base?: #t;
