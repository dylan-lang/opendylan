module:    pentium-rtg
Synopsis:  Apply entry point generation for the Dylan Pentium rtg
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




//// Apply entry points



define sideways method op--extend-stack-for-apply
    (be :: <pentium-back-end>, vec, size, req-index)
  with-harp (be)
    stack stack;
    function function;
    arg-count argc;
    nreg nsize, first-opt, hole-start, save-argc, save-argc2;

    let args-in-regs = be.registers.arguments-passed-in-registers;

    // Handle the extend-stack case
    ins--sub(be, size, size, 1);  // decrement the size to allow for vector arg
    let ntop-size = op--divide-by-4(be, req-index);
    let nntop-size = argc;
    ins--move(be, nntop-size, ntop-size);  // Colour carefully for Pentium
    op--shuffle-stack(be, first-opt, #f, nntop-size, 0, size);
    let nreq-index = op--duplicate(be, req-index);
    ins--ld(be, vec, first-opt, 0);  // reload the vector
    let nsize = argc;
    op--vector-size(be, nsize, vec);  // get back the vector size
    ins--add(be, vec, vec, 8);        // the start of the data
    // save the final arg count around the shuffle
    op--calculate-arg-count-for-apply(be, save-argc, nreq-index, nsize);
    ins--push(be, save-argc);  // relieve some serious register pressure
    let nnsize = op--duplicate(be, nsize);
    ins--add(be, hole-start, stack, nreq-index);
    ins--add(be, hole-start, hole-start, 4); // allowed for the pushed data
    ins--pop(be, save-argc2);  // save the argc around the shuffle
    op--copy-words-with-update(be, #f, hole-start, vec, nnsize);
    ins--move(be, argc, save-argc2);  // put back the argc after the shuffle
    ins--jmp-indirect(be, function, be.function-xep-offset, args-in-regs);
  end with-harp;
end method;

