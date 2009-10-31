Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec llvm ()
  module llvm;

  suite llvm-asm-suite;
end library-spec llvm;

define module-spec llvm ()
  instantiable class <llvm-module> (<object>);
end module-spec llvm;

define sideways method make-test-instance
    (class :: subclass(<llvm-module>))
 => (module :: <llvm-module>)
  make(class, name: "test.ll")
end method make-test-instance;

define llvm class-test <llvm-module> ()
  //---*** Fill this in...
end class-test <llvm-module>;

