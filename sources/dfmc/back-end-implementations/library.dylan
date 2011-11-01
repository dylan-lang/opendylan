module: dylan-user

define library dfmc-back-end-implementations
  use functional-dylan;

  use dfmc-pentium-harp-cg;      // Pentium backend
  use dfmc-harp-browser-support; // Harp browsing support
  use dfmc-c-back-end;           // C backend
  use dfmc-c-linker;             // C linker
end library dfmc-back-end-implementations;

