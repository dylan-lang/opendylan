module: dylan-user

define library dfmc-back-end-implementations
  use functional-dylan;

  use dfmc-c-back-end;           // C backend
  use dfmc-c-linker;             // C linker
end library dfmc-back-end-implementations;
