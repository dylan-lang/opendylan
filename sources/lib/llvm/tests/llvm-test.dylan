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

define test test-<llvm-module> ()
  //---*** Fill this in...
end test;

define test test-$llvm-label-type ()
  check-no-errors("$llvm-label-type matches expected definition",
                  llvm-constrain-type($llvm-label-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"LABEL")));
end test;

define test test-$llvm-void-type ()
  check-no-errors("$llvm-void-type matches expected definition",
                  llvm-constrain-type($llvm-void-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"VOID")));
end test;

define test test-$llvm-metadata-type ()
  check-no-errors("$llvm-void-type matches expected definition",
                  llvm-constrain-type($llvm-metadata-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"METADATA")));
end test;

define test test-$llvm-float-type ()
  check-no-errors("$llvm-float-type matches expected definition",
                  llvm-constrain-type($llvm-float-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"FLOAT")));
end test;

define test test-$llvm-double-type ()
  check-no-errors("$llvm-double-type matches expected definition",
                  llvm-constrain-type($llvm-double-type,
                                      make(<llvm-primitive-type>,
                                           kind: #"DOUBLE")));
end test;

define test test-$llvm-i1-type ()
  check-equal("$llvm-i1-type width",
              1, llvm-integer-type-width($llvm-i1-type));
end test;

define test test-$llvm-i8-type ()
  check-equal("$llvm-i8-type width",
              8, llvm-integer-type-width($llvm-i8-type));
end test;

define test test-$llvm-i8*-type ()
  check-no-errors("$llvm-i8*-type pointee matches $llvm-i8-type",
                  llvm-constrain-type
                    (llvm-pointer-type-pointee($llvm-i8*-type),
                     $llvm-i8-type));
end test;

define test test-$llvm-i16-type ()
  check-equal("$llvm-i16-type width",
              16, llvm-integer-type-width($llvm-i16-type));
end test;

define test test-$llvm-i32-type ()
  check-equal("$llvm-i32-type width",
              32, llvm-integer-type-width($llvm-i32-type));
end test;

define test test-$llvm-i64-type ()
  check-equal("$llvm-i64-type width",
              64, llvm-integer-type-width($llvm-i64-type));
end test;

define suite llvm-module-test-suite ()
  test test-<llvm-module>;
  test test-$llvm-label-type;
  test test-$llvm-void-type;
  test test-$llvm-metadata-type;
  test test-$llvm-float-type;
  test test-$llvm-double-type;
  test test-$llvm-i1-type;
  test test-$llvm-i8-type;
  test test-$llvm-i8*-type;
  test test-$llvm-i16-type;
  test test-$llvm-i32-type;
  test test-$llvm-i64-type;
end;
