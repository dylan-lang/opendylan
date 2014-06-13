Module:    DFMC-Testing
Author:    Steve Rowley
Synopsis:  Tests for the typist's type algebra.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// Substrate for defining type algebra tests.
///

define variable *typist-algebra-tests* = #();

define function run-typist-algebra-tests(#key tests     = *typist-algebra-tests*,
                                              safely?   = #t,
                                              progress? = *standard-output*,
                                              report?   = *standard-output*) 
  => ()
  // Run just the typist algebra tests and print a short report.
  run-tests(tests: tests, safely?: safely?, progress?: progress?, 
            report?: report?)
end;

define macro typist-algebra-test-definer
  // Define manual compiler test & remember the name in typist algebra registry
  { define typist-algebra-test ?testname:name
      ?test-body:body
    end }
  => { *typist-algebra-tests* := add-new!(*typist-algebra-tests*, ?#"testname");
       *tests*[?#"testname"] := 
          method ()
            // Some things want to pretend a compilation is in progress.
            with-testing-context (#f)
	      ?test-body
            end
          end }
end;

///
/// Tests of the algebra.
///

// *** Should give normalize?: #f in makes here.
define typist-algebra-test typist-normalization
  // Test type-estimate-normalize on the various types.

  // First cons up a bazillion test objects!
  let a-bottom    = make(<type-estimate-bottom>);

  let a-raw       = make(<type-estimate-raw>, raw: dylan-value(#"<raw-integer>"));

  let a-lim-int   = make(<type-estimate-limited-integer>, min: 0);
  let a-lim-int-2 = make(<type-estimate-limited-integer>, max: 2);
  let a-lim-int-3 = make(<type-estimate-limited-integer>, min: 0, max: 2);
  let a-lim-int-4 = make(<type-estimate-limited-integer>, min: 1, max: 1);

  let a-lim-class = make(<type-estimate-limited-class>, 
                         subclass: dylan-value(#"<integer>"));

  let a-lim-inst  = make(<type-estimate-limited-instance>, 
                         singleton: 1, class: dylan-value(#"<integer>"));
  let a-lim-inst-2 = make(<type-estimate-limited-instance>, 
                          singleton: #(), class: dylan-value(#"<list>"));

  let a-lim-coll  = make(<type-estimate-limited-collection>, 
                         class: dylan-value(#"<list>"),
                         concrete-class: dylan-value(#"<list>"),
                         size: 0);

  let a-class     = make(<type-estimate-class>, 
                         class: dylan-value(#"<integer>"));
  let a-class-2   = make(<type-estimate-class>, 
                         class: dylan-value(#"<string>"));
  let a-class-3   = make(<type-estimate-class>, 
                         class: dylan-value(#"<pair>"));
  let a-class-4   = make(<type-estimate-class>, 
                         class: dylan-value(#"<list>"));
  let a-class-5   = make(<type-estimate-class>, 
                         class: dylan-value(#"<character>"));
  let a-class-6   = make(<type-estimate-class>, 
                         class: dylan-value(#"<symbol>"));
  let a-class-7   = make(<type-estimate-class>, 
                         class: dylan-value(#"<empty-list>"));
  let a-class-8   = make(<type-estimate-class>, 
                         class: dylan-value(#"<vector>"));

  let a-union   = make(<type-estimate-union>, unionees: #());
  let a-union-2 = make(<type-estimate-union>, unionees: list(a-class));
  let a-union-3 = make(<type-estimate-union>, unionees: list(a-class, 
                                                             a-class-2));
  let a-union-4 = make(<type-estimate-union>, 
                       unionees: list(a-class-3, a-class-4, a-class-2));
  let a-union-5 = make(<type-estimate-union>, 
                       unionees: list(a-class-4, a-class-3, a-class-2));
  let a-union-6 = make(<type-estimate-union>,
                       unionees: list(a-class-4, a-class-2, a-class-3));
  let a-union-7 = make(<type-estimate-union>,
                       unionees: list(a-class-4, a-class-2));

  let a-values = make(<type-estimate-values>, 
                      fixed: vector(a-class, a-class-2), 
                      rest: make(<type-variable>, contents: a-class-5));
  let a-values-2 = make(<type-estimate-values>, fixed: vector(a-union-2));
  let a-values-3 = make(<type-estimate-values>, 
                        fixed: vector(make(<type-estimate-union>,
                                           unionees: list(a-class, a-class-5)),
                                      make(<type-estimate-union>,
                                           unionees: list(a-class-2, a-class-6))),
                        rest: make(<type-variable>,
                                   contents: make(<type-estimate-union>, 
                                                  unionees: list(a-class-7, a-class-3))));
  let a-values-4 = make(<type-estimate-values>);

  let a-function = make(<type-estimate-limited-function>, 
                        class:     dylan-value(#"<function>"),
                        requireds: vector(a-class, a-class-5));
  let a-function-2 = make(<type-estimate-limited-function>, 
                          class: dylan-value(#"<function>"),
                          requireds: vector(make(<type-estimate-union>, 
                                                 unionees: list(a-class))));
  let a-function-3 = make(<type-estimate-limited-function>, 
                          class: dylan-value(#"<function>"),
                          requireds: vector(make(<type-estimate-union>,
                                                 unionees: list(a-class, 
                                                                a-class-5)),
                                            make(<type-estimate-union>,
                                                 unionees:
                                                 list(a-class-8, a-class-2))));

  // Normalization is a noop on bottoms.
  type-estimate-normalize(a-bottom) == a-bottom

  // Normalization is a noop on raws.
  & type-estimate-normalize(a-raw) == a-raw

  // Normalization is a noop on limited integers, usually.
  // Cases: min present, max present, min & max present.
  & (type-estimate-normalize(a-lim-int)   == a-lim-int
   & type-estimate-normalize(a-lim-int-2) == a-lim-int-2
   & type-estimate-normalize(a-lim-int-3) == a-lim-int-3
   & type-estimate-match?(type-estimate-normalize(a-lim-int-4), a-lim-inst))

  // Normalization is a noop on subclass types.
  & type-estimate-normalize(a-lim-class) == a-lim-class

  // Normalization is a noop on singleton types.
  & type-estimate-normalize(a-lim-inst) == a-lim-inst

  // *** Normalization on limited collections isn't in until collections are!
  & (type-estimate-match?(type-estimate-normalize(a-lim-coll), a-lim-inst-2)
     )

  // Normalization is a noop on class types.
  & type-estimate-normalize(a-class) == a-class

  // Normalization of unions often CPA-expands.
  // Cases: [1] A union of 0 things is bottom.
  //        [2] A union of 1 thing is that thing, not a union.
  //        [3] Normal union of classes, no interesting subtypeness -- no change
  //        [4] Union where subtyping introduces changes to unionees.
  //        [5] Like [4], but with subtypable classes in the opposite order.
  //        [6] Like [4], but with subtype guy at end.  Fixed a subtle bug!
  & (instance?(type-estimate-normalize(a-union), <type-estimate-bottom>)
   & type-estimate-normalize(a-union-2) == a-class
   & type-estimate-normalize(a-union-3) == a-union-3
   & type-estimate-match?(type-estimate-normalize(a-union-4), a-union-7)
   & type-estimate-match?(type-estimate-normalize(a-union-5), a-union-7)
   & type-estimate-match?(type-estimate-normalize(a-union-6), a-union-7))

  // Normalization of multiple values may CPA expand if a value is a union.
  // Cases: [1] No unions in args & no changes in them, so no change.
  //        [2] Change in an arg, but not enough to CPA expand.
  //        [3] CP expansion in the presence of unions.
  //        [4] values(), just because I'm suspicious.
  & (type-estimate-normalize(a-values) == a-values
   & type-estimate-match?(type-estimate-normalize(a-values-2), 
                          make(<type-estimate-values>, fixed: list(a-class)))
   & type-estimate-match?(type-estimate-normalize(a-values-3),
                          make(<type-estimate-union>,
                               // Glaah.  Order of answers hard to intuit here.
                               unionees: list(make(<type-estimate-values>, 
                                                   fixed: list(a-class, a-class-2), 
                                                   rest:  a-class-7),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class, a-class-6), 
                                                   rest:  a-class-7),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class-5, a-class-2), 
                                                   rest:  a-class-7),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class-5, a-class-6), 
                                                   rest:  a-class-7),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class, a-class-2), 
                                                   rest:  a-class-3),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class, a-class-6), 
                                                   rest:  a-class-3),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class-5, a-class-2), 
                                                   rest:  a-class-3),
                                              make(<type-estimate-values>, 
                                                   fixed: list(a-class-5, a-class-6), 
                                                   rest:  a-class-3))))
   & type-estimate-normalize(a-values-4) == a-values-4)

  // Normalization of contravariant function types.
  // Cases: [1] Args with no interesting normalization, so no change.
  //        [2] Args which normalization changes trivially (union canon.).
  //        [3] Args which are actually unions.
  //        [4] Trivial functions become class types.
  & (type-estimate-normalize(a-function) == a-function
   & type-estimate-match?(type-estimate-normalize(a-function-2),
                          make(<type-estimate-limited-function>, 
                               class: dylan-value(#"<function>"),
                               requireds: list(a-class)))
   & type-estimate-match?(
       type-estimate-normalize(a-function-3), 
       // Glaah.
       make(<type-estimate-union>, 
            unionees: list(make(<type-estimate-limited-function>, 
                                class: dylan-value(#"<function>"),
                                requireds: list(a-class, a-class-8)),
                           make(<type-estimate-limited-function>, 
                                class: dylan-value(#"<function>"),
                                requireds: list(a-class, a-class-2)),
                           make(<type-estimate-limited-function>, 
                                class: dylan-value(#"<function>"),
                                requireds: list(a-class-5, a-class-8)),
                           make(<type-estimate-limited-function>, 
                                class: dylan-value(#"<function>"),
                                requireds: list(a-class-5, a-class-2)))))
   & type-estimate-match?(make(<type-estimate-limited-function>), 
                          make(<type-estimate-class>, class: dylan-value(#"<function>"))))
end;

define typist-algebra-test typist-base
  // Test typist-base-type on the various types.
  let a-class    = make(<type-estimate-class>, 
                        class: dylan-value(#"<integer>"));
  let a-class-2  = make(<type-estimate-class>,
                        class: dylan-value(#"<class>"));
  let a-class-3  = make(<type-estimate-class>,
                        class: dylan-value(#"<function>"));
  let a-lim-int  = make(<type-estimate-limited-integer>, min: 0);
  let a-lim-inst = make(<type-estimate-limited-instance>, 
                         singleton: 1, class: dylan-value(#"<integer>"));
  let a-lim-cl   = make(<type-estimate-limited-class>, 
                        subclass: dylan-value(#"<integer>"));
  let a-union    = make(<type-estimate-union>, unionees: #());
  let a-union-2  = make(<type-estimate-union>, 
                        unionees: list(a-class, a-lim-inst));
  let a-union-3  = make(<type-estimate-union>, 
                        unionees: list(a-lim-int, a-lim-cl));
  let a-values   = make(<type-estimate-values>);
  let a-values-2 = make(<type-estimate-values>, fixed: vector(a-class));
  let a-values-3 = make(<type-estimate-values>, fixed: vector(a-lim-int));
  let a-values-4 = make(<type-estimate-values>, rest: make(<type-variable>, contents: a-class));
  let a-values-5 = make(<type-estimate-values>, rest: make(<type-variable>, contents: a-lim-int));

  let a-function   = make(<type-estimate-limited-function>, 
                          class:     dylan-value(#"<function>"),
                          requireds: vector(a-lim-int, a-lim-inst));
  let a-function-2 = make(<type-estimate-limited-function>, 
                          class:     dylan-value(#"<function>"),
                          requireds: vector(a-class, a-lim-inst));

  let a-bottom = make(<type-estimate-bottom>);
  let a-raw = make(<type-estimate-raw>, raw: dylan-value(#"<raw-integer>"));

  // Base type of a class type is itself.
  type-estimate-base(a-class) == a-class

  // Base type of a limited type is the class being limited.
  // *** Do limited collections here, too, when that gets done.
  & type-estimate-match?(type-estimate-base(a-lim-int), a-class)

  // Base type of a singleton type is itself.
  & type-estimate-base(a-lim-inst) == a-lim-inst

  // Base type of a subclass is <class>.
  & type-estimate-match?(type-estimate-base(a-lim-cl), a-class-2)

  // Base type of a union is the union of the base types of the unionees.
  & (instance?(type-estimate-base(a-union), <type-estimate-bottom>)
   & type-estimate-base(a-union-2) == a-union-2
   & type-estimate-match?(type-estimate-base(a-union-3), 
                          make(<type-estimate-union>, 
                               unionees: list(a-class, a-class-2))))

  // Base type of values is values of base-types.
  & (type-estimate-base(a-values) == a-values
   & type-estimate-base(a-values-2) == a-values-2
   & type-estimate-match?(type-estimate-base(a-values-3), a-values-2)
   & type-estimate-base(a-values-4) == a-values-4
   & type-estimate-match?(type-estimate-base(a-values-5), a-values-4))

  // Base type of function is function of base types.
  & (type-estimate-match?(type-estimate-base(a-function), a-class-3)
     & type-estimate-match?(type-estimate-base(a-function-2), a-class-3))

  // Base type of a bottom is itself.
  & type-estimate-base(a-bottom) == a-bottom 

  // Base type of a raw is itself.
  & type-estimate-base(a-raw) == a-raw
end;

// *** Under construction.
define typist-algebra-test typist-subtype?
  // Test of type-estimate-subtype?.
  let a-bottom    = make(<type-estimate-bottom>);
  let a-raw       = make(<type-estimate-raw>, raw: dylan-value(#"<raw-integer>"));
  let a-raw2      = make(<type-estimate-raw>, raw: dylan-value(#"<raw-pointer>"));
  let a-class     = make(<type-estimate-class>, 
                         class: dylan-value(#"<integer>"));
  let a-class-2   = make(<type-estimate-class>,
                         class: dylan-value(#"<string>"));
  let a-class-3   = make(<type-estimate-class>, 
                         class: dylan-value(#"<pair>"));
  let a-class-4   = make(<type-estimate-class>, 
                         class: dylan-value(#"<list>"));
  let a-class-7   = make(<type-estimate-class>, 
                         class: dylan-value(#"<empty-list>"));
  let a-class-8   = make(<type-estimate-class>, 
                         class: dylan-value(#"<byte-string>"));

  let a-union     = make(<type-estimate-union>, 
                         unionees: list(a-class-3, a-class-7)); // pair, empty
  let a-union-2   = make(<type-estimate-union>, 
                         unionees: list(a-class-4, a-class-2)); // list, string
  let a-union-3   = make(<type-estimate-union>, 
                         unionees: list(a-class-3, a-class-8)); // pair, byte-string
  let a-lim-int   = make(<type-estimate-limited-integer>, min: 0);
  let a-lim-int-2 = make(<type-estimate-limited-integer>, min: 2);
  let a-lim-int-3 = make(<type-estimate-limited-integer>, max: 3);
  let a-lim-int-4 = make(<type-estimate-limited-integer>, max: 4);
  let a-lim-int-5 = make(<type-estimate-limited-integer>, min: 1, max: 2);
  let a-lim-int-6 = make(<type-estimate-limited-integer>, min: 0, max: 3);
  let a-lim-int-7 = make(<type-estimate-limited-integer>, min: 1, max: 1, 
                         class: dylan-value(#"<integer>"));
  let a-singleton = make(<type-estimate-limited-instance>, 
                         singleton: 1, class: dylan-value(#"<integer>"));
  let a-values    = make(<type-estimate-values>, rest: #f);
  let a-values-1  = make(<type-estimate-values>, fixed: vector(a-class-3), rest: #f);
  let a-values-2  = make(<type-estimate-values>, fixed: vector(a-class-4), rest: #f);
  let a-values-3  = make(<type-estimate-values>, fixed: vector(a-class-3, a-class-3), rest: #f);
  let a-values-4  = make(<type-estimate-values>, fixed: vector(a-class-4, a-class-4), rest: #f);
  let a-values-5  = make(<type-estimate-values>, fixed: vector(a-class-3), rest: make(<type-variable>, contents: a-class-3));
  let a-values-6  = make(<type-estimate-values>, fixed: vector(a-class-4), rest: make(<type-variable>, contents: a-class-4));

  let a-fun-1     = make(<type-estimate-limited-function>, 
                         // <string> -> <pair>
                         requireds: vector(a-class-2),
                         rest?:     #f,
                         values:    make(<type-estimate-values>, 
                                         fixed: vector(a-class-3),
                                         rest:  #f),
                         class:     dylan-value(#"<function>"));
  let a-fun-2     = make(<type-estimate-limited-function>, 
                         // <byte-string> -> <list>
                         requireds: vector(a-class-8),
                         rest?:     #f,
                         values:    make(<type-estimate-values>, 
                                         fixed: vector(a-class-4),
                                         rest:  #f),
                         class:     dylan-value(#"<function>"));

  // Bottom
  (type-estimate-subtype?(a-bottom, a-bottom)
   & type-estimate-subtype?(a-bottom, a-class)
   & ~type-estimate-subtype?(a-class, a-bottom))
   
  // Raw types
  & (type-estimate-subtype?(a-raw, a-raw)
     & ~type-estimate-subtype?(a-raw, a-raw2)
     & ~type-estimate-subtype?(a-raw, a-class))

  // Unions
  & (type-estimate-subtype?(a-union, a-class-4)    // Rule 1
   & type-estimate-subtype?(a-class-3, a-union-2)  // Rule 2
   & type-estimate-subtype?(a-union-3, a-union-2)) // Rule 3

  // Multiple values: (required vals, rest val)
  & (type-estimate-subtype?(a-values, a-values)        // (0,0) vs (0,0)
   & type-estimate-subtype?(a-values-1, a-values-2)  // (1,0) vs (1,0)
   & ~type-estimate-subtype?(a-values-2, a-values-1)
   & type-estimate-subtype?(a-values-3, a-values-4)  // (2,0) vs (2,0)
   & ~type-estimate-subtype?(a-values-4, a-values-3)
   & type-estimate-subtype?(a-values-5, a-values-6)  // (1,1) vs (1,1)
   & ~type-estimate-subtype?(a-values-6, a-values-5)
   & type-estimate-subtype?(a-values-5, a-values-2)  // (1,0) vs (1,1)
   & ~type-estimate-subtype?(a-values-2, a-values-5)
   & type-estimate-subtype?(a-values-2, a-values-6)  // (1,0) vs (1,1)
   & type-estimate-subtype?(a-values-6, a-values-2)  // (1,1) vs (1,0)
   & ~type-estimate-subtype?(a-values, a-values-1))  // (0,0) vs (1,0)

  // Classes
  & (type-estimate-subtype?(a-class-3, a-class-4)
   & ~type-estimate-subtype?(a-class-4, a-class-3)
   & ~type-estimate-subtype?(a-class-4, a-class))

  // Functions
  & (type-estimate-subtype?(a-fun-1, a-fun-2)
     ) // *** Need more here.

  // *** Limited
  & (type-estimate-subtype?(a-lim-int, a-class)
   & ~type-estimate-subtype?(a-class, a-lim-int))

  // Limited integers
  & (type-estimate-subtype?(a-lim-int-2, a-lim-int)
   & type-estimate-subtype?(a-lim-int-3, a-lim-int-4)
   & type-estimate-subtype?(a-lim-int-5, a-lim-int-6)
   & type-estimate-subtype?(a-lim-int-7, a-singleton))

  // Singletons
  & (type-estimate-subtype?(a-singleton, a-lim-int-7)
     ) // *** Need more here.

  // *** Subclass

  // *** Limited collections
end;

// *** Under construction.
define typist-algebra-test typist-instance?
  // Tests of type-estimate-instance?.
  let one     = 1;
  let three   = 3;
  let foo     = "foo";
  let a-char  = 'a';
  let bottom  = make(<type-estimate-bottom>);
  let object  = make(<type-estimate-class>, class: dylan-value(#"<object>"));
  let charact = make(<type-estimate-class>, class: dylan-value(#"<character>"));
  let integer = make(<type-estimate-class>, class: dylan-value(#"<integer>"));
  let string  = make(<type-estimate-class>, class: dylan-value(#"<string>"));
  let struint = make(<type-estimate-union>, unionees: list(string, integer));
  let sing1   = make(<type-estimate-limited-instance>, singleton: one, class: dylan-value(#"<integer>"));
  let singfoo = make(<type-estimate-limited-instance>, singleton: foo, class: dylan-value(#"<string>"));
  let singch  = make(<type-estimate-limited-instance>, singleton: a-char, class: dylan-value(#"<character>"));
  let substr  = make(<type-estimate-limited-class>, 
                     subclass: dylan-value(#"<string>"));
  let subnum  = make(<type-estimate-limited-class>,
                     subclass: dylan-value(#"<number>"));
  let limint  = make(<type-estimate-limited-integer>, min: 0, max: 2);

  // Bottom
  ~type-estimate-instance?(one, bottom)
  // *** Raw
  // Unions
  & type-estimate-instance?(one, struint)
  & type-estimate-instance?(foo, struint)
  & type-estimate-instance?(foo, string)
  & type-estimate-instance?(one, integer)
  // *** Values
  // Classes
  & type-estimate-instance?(one, integer)
  // *** Functions
  // *** Limited
  // *** Limited collections
  // Singletons
  & type-estimate-instance?(one, sing1)
  & type-estimate-instance?(foo, singfoo)
  & ~type-estimate-instance?(one, singfoo)
  & ~type-estimate-instance?(foo, sing1)
  & type-estimate-instance?(a-char, object)
  & type-estimate-instance?(a-char, charact)
  & type-estimate-instance?(a-char, singch)
  // Subclass
  & type-estimate-instance?(dylan-value(#"<string>"), substr)
  & type-estimate-instance?(dylan-value(#"<byte-string>"), substr)
  & type-estimate-instance?(dylan-value(#"<integer>"), subnum)
  & ~type-estimate-instance?(dylan-value(#"<integer>"), substr)
  & ~type-estimate-instance?(dylan-value(#"<byte-string>"), subnum)
  // Limited integer
  & type-estimate-instance?(one, limint)
  & ~type-estimate-instance?(three, limint)
end;

define typist-algebra-test typist-disjoint?
  // See if we believe type-estimate-disjoint? is working.
  // *** A more elaborate test suite would be nice!
  dynamic-bind (*progress-stream*           = #f,
                *demand-load-library-only?* = #f)  // with-compiler-muzzled
    let lib

  = compile-template("define sealed abstract class <a> (<object>) end;\n"
		      // Note <b> is trying hard to be uncooperative!
		     "define open   concrete class <b> (<object>) end;\n"
		     "define sealed concrete class <c> (<a>, <b>) end;\n",
		     compiler: compile-library-until-optimized);
    let mod = lookup-module-in(language-definition(lib), #"scratch-module");
    with-testing-context (lib)
      let a = make-variable-name-fragment-in-module(#"<a>", mod);
      let b = make-variable-name-fragment-in-module(#"<b>", mod);
      type-estimate-subtype?(as(<type-estimate>, lookup-value(a)),
			     as(<type-estimate>, lookup-value(b)))
    & ~type-estimate-disjoint?(as(<type-estimate>, lookup-value(a)),
			       as(<type-estimate>, lookup-value(b)))
    end
  end &
  begin
    let sing  = make(<type-estimate-limited-instance>, singleton: 'a', class: dylan-value(#"<character>"));
    let obj   = make(<type-estimate-class>, class: dylan-value(#"<object>"));
    let char  = make(<type-estimate-class>, class: dylan-value(#"<character>"));
    let num   = make(<type-estimate-class>, class: dylan-value(#"<number>"));
    ~type-estimate-disjoint?(sing, obj) &
    ~type-estimate-disjoint?(sing, char) &
     type-estimate-disjoint?(sing, num)
  end
end;

define typist-algebra-test typist-as-<type-estimate>
  // Test as(<type-estimate>, ...).
  let model-integer   = dylan-value(#"<integer>");
  let model-float     = dylan-value(#"<float>");
  let model-raw       = dylan-value(#"<raw-integer>");
  let model-lim-int   = ^limited-integer(min: 0, max: 63);
  let model-singleton = ^singleton(a:);
  let model-union     = ^type-union(model-integer, model-float);

  type-estimate-match?(make(<type-estimate-class>, class: model-integer),
                       as(<type-estimate>, model-integer))
  & type-estimate-match?(make(<type-estimate-raw>, raw: model-raw),
                         as(<type-estimate>, model-raw))
  & type-estimate-match?(make(<type-estimate-limited-integer>, min: 0, max: 63),
                         as(<type-estimate>, model-lim-int))
  & type-estimate-match?(make(<type-estimate-limited-instance>, 
                              singleton: a:, class: dylan-value(#"<symbol>")), 
                         as(<type-estimate>, model-singleton))
  & type-estimate-match?(make(<type-estimate-union>, 
                              unionees: list(make(<type-estimate-class>, 
                                                  class: model-integer),
                                             make(<type-estimate-class>,
                                                  class: model-float))), 
                         as(<type-estimate>, model-union))
end;

define typist-algebra-test typist-as-<&type>
  // Test as(<&type>, ...).
  let model-integer   = dylan-value(#"<integer>");
  let model-float     = dylan-value(#"<float>");
  let model-raw       = dylan-value(#"<raw-integer>");
  let model-lim-int   = ^limited-integer(min: 0, max: 63);
  let model-singleton = ^singleton(a:);
  let model-union     = ^type-union(model-integer, model-float);

  model-integer == as(<&type>, make(<type-estimate-class>, class: model-integer))
  & model-raw == as(<&type>, make(<type-estimate-raw>, raw: model-raw))
  & begin 
      let new-model-lim-int = as(<&type>, make(<type-estimate-limited-integer>, min: 0, max: 63));
      ^limited-integer-min(new-model-lim-int) == ^limited-integer-min(model-lim-int) &
      ^limited-integer-max(new-model-lim-int) == ^limited-integer-max(model-lim-int)
    end
  & ^singleton-object(model-singleton) ==
    ^singleton-object(as(<&type>, make(<type-estimate-limited-instance>,
				       singleton: a:, class: dylan-value(#"<symbol>"))))
  & begin
      let new-model-union = as(<&type>, make(<type-estimate-union>, 
                                             unionees: list(make(<type-estimate-class>,
                                                                 class: model-integer),
                                                            make(<type-estimate-class>,
                                                                 class: model-float))));
      ^union-type1(new-model-union) == ^union-type1(model-union) &
      ^union-type2(new-model-union) == ^union-type2(model-union)
    end
end;
