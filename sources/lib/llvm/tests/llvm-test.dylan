Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class :: subclass(<llvm-module>))
 => (module :: <llvm-module>)
  make(class, name: "test.ll")
end method make-test-instance;

define llvm class-test <llvm-module> ()
  //---*** Fill this in...
end class-test <llvm-module>;

define llvm constant-test $llvm-label-type ()
  check-no-errors("$llvm-label-type matches expected definition",
                  llvm-constrain-type($llvm-label-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"LABEL")));
end constant-test $llvm-label-type;

define llvm constant-test $llvm-void-type ()
  check-no-errors("$llvm-void-type matches expected definition",
                  llvm-constrain-type($llvm-void-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"VOID")));
end constant-test $llvm-void-type;

define llvm constant-test $llvm-metadata-type ()
  check-no-errors("$llvm-void-type matches expected definition",
                  llvm-constrain-type($llvm-metadata-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"METADATA")));
end constant-test $llvm-metadata-type;

define llvm constant-test $llvm-float-type ()
  check-no-errors("$llvm-float-type matches expected definition",
                  llvm-constrain-type($llvm-float-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"FLOAT")));
end constant-test $llvm-float-type;

define llvm constant-test $llvm-double-type ()
  check-no-errors("$llvm-double-type matches expected definition",
                  llvm-constrain-type($llvm-double-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"DOUBLE")));
end constant-test $llvm-double-type;

define llvm constant-test $llvm-i1-type ()
  check-equal("$llvm-i1-type width",
              1, llvm-integer-type-width($llvm-i1-type));
end constant-test $llvm-i1-type;

define llvm constant-test $llvm-i8-type ()
  check-equal("$llvm-i8-type width",
              8, llvm-integer-type-width($llvm-i8-type));
end constant-test $llvm-i8-type;

define llvm constant-test $llvm-i8*-type ()
  check-no-errors("$llvm-i8*-type pointee matches $llvm-i8-type",
                  llvm-constrain-type
                    (llvm-pointer-type-pointee($llvm-i8*-type),
                     $llvm-i8-type));
end constant-test $llvm-i8*-type;

define llvm constant-test $llvm-i16-type ()
  check-equal("$llvm-i16-type width",
              16, llvm-integer-type-width($llvm-i16-type));
end constant-test $llvm-i16-type;

define llvm constant-test $llvm-i32-type ()
  check-equal("$llvm-i32-type width",
              32, llvm-integer-type-width($llvm-i32-type));
end constant-test $llvm-i32-type;

define llvm constant-test $llvm-i64-type ()
  check-equal("$llvm-i64-type width",
              64, llvm-integer-type-width($llvm-i64-type));
end constant-test $llvm-i64-type;
