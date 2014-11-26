module: dylan-user

define library dfmc-back-end-implementations
  use dylan;

  use dfmc-harp-x86-cg;          // Harp x86 backend
  use dfmc-harp-browser-support; // Harp browsing support
  use dfmc-c-back-end;           // C backend
  use dfmc-c-linker;             // C linker
  use dfmc-llvm-back-end;        // LLVM backend
  use dfmc-llvm-linker;          // LLVM linker
end library dfmc-back-end-implementations;

