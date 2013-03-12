module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module java-modeling
  use functional-dylan ;
  use streams ;
  use streams-internals ;
  use print ;
  use format ;
  use format-out ;

  export

    // unique strings

    <unique-string>,
    the-string, unique-tag,
    start-with-unique-strings,
    finish-with-unique-strings,
//    clobber-scoped-strings,
    uniq,
    ensure-uniq,
    valid-unique?,
    print-unique-strings,

    // java-packages
    <java-package>,  name-component, super-package,
    $java-default-package$,
    package-concatenate-with-name,
    java-package,
    
    // java-types

    <java-type>,
    <java-any>,
    <java-return-address-type>,
    $java-return-address$,
    <java-any-type>,
    <java-reference-type>,
    <java-null-type>,
    $java-any-type$,
    $java-null-type$,
    java-type-words,
//    $java-internal-package-separator$,
    java-class-name,
    signature-string,
    signature-string-string,
    <java-function-type>,  java-function-result-type, java-function-arg-types,
    <java-array-type>,     java-array-element-type,
    $java-array-type$,  // should be $java-any-array-type$

    j-int-code,
    j-long-code,
    j-float-code,
    j-double-code,
    j-ref-code,
    j-byte-code,
    j-char-code,
    j-short-code,
    j-void-code,

    j-code-for,
    <java-primitive-type>,  java-prim-reflected-class,
    <java-primitive-fragment-type>,
    <java-class-or-interface>, class-name, class-package, represents, represents-setter, 
    concrete-implementation, concrete-implementation-setter,
    <java-class>,              super, super-setter, interfaces, interfaces-setter, rcpl, rcpl-setter,
    <java-interface>,          super-interfaces, super-interfaces-setter,
    <java-concrete>,           class-or-interface,
    <java-stub-class-or-interface>,
    <java-stub-class>,
    <java-stub-interface>,
    java-lang-class,

    $java-pack$, $java-lang-pack$, $java-io-pack$, $java-util-pack$,

    $java/lang/Object$,

    $java/lang/Void$,
    $java/lang/Integer$,
    $java/lang/Character$,
    $java/lang/Byte$,
    $java/lang/Boolean$,
    $java/lang/Short$,
    $java/lang/Long$,
    $java/lang/Float$,
    $java/lang/Double$,

    $java-void-type$,
    $java-int-type$,
    $java-char-type$,
    $java-byte-type$,
    $java-bool-type$,
    $java-short-type$,
    $java-long-type$,
    $java-float-type$,
    $java-double-type$,

    $java-long-low-fragment$,   // should be in vm-code-generation
    $java-long-high-fragment$,
    $java-double-low-fragment$,
    $java-double-high-fragment$,
    double-type-halves,

    meth-type,
    array-type,
    java-type-equivalent?,
    java-type-merge,
    java-type-intersect,
    assignment-compatible? ;

end module;
