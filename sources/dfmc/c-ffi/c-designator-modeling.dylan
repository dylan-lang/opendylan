module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Copyright 1996 Functional Objects, Inc.  All rights reserved.


// !@#$ todo: rearrange to make only leaf classes be concrete.
//

define primary &class <designator-class> (<class>)
  // these probably need to be around at run time
  runtime-constant &slot alignment-of :: <integer>, init-value: 0;
  runtime-constant &slot size-of :: <integer>,      init-value: 0;
  runtime-constant lazy &slot referenced-type, init-value: #f;
  runtime-constant lazy &slot concrete-pointer-type, init-value: #f;
  runtime-constant lazy &slot abstract-pointer-type, init-value: #f;

  // compile time only slots
  lazy slot ^mapped-import-type, init-keyword: mapped-import-type:; // <name>?
  lazy slot ^mapped-export-type, init-keyword: mapped-export-type:; // <name>?
  lazy slot ^low-level-type, // <name>?
    init-keyword: low-level-type:,
    init-value: #f;
  //  slot ^raw-type-name, init-keyword: raw-type-name:, // <name>
  //    init-value: #f;
  lazy slot ^raw-type-info /* :: false-or(<abstract-raw-type>) */ = #f; // FORWARD REF
  lazy slot ^raw-dereferencer-name, // <name>
         init-keyword: raw-dereferencer-name:, init-value: #f;
  lazy slot ^bitfield-dereferencer-name, // <name>
         init-keyword: bitfield-dereferencer-name:, init-value: #f;
  lazy slot ^boxer-function-name,// <name>? maybe unnecessary.
         init-keyword: boxer-function-name:, init-value: #f; 
  lazy slot ^import-function, init-value: #f; // false-or(<name>)

  lazy slot ^export-function, init-value: #f; // false-or(<name>)

  lazy slot ^unboxer-function-name, // <name>? maybe unnecessary.
         init-keyword: unboxer-function-name:, init-value: #f;
  lazy slot ^options :: <sequence>, init-value: #();
  lazy slot struct-fields :: <sequence> = #(), init-keyword: struct-fields:;
       slot proxy? :: <boolean>, init-value: #f;
  lazy constant slot pointer-type-name, init-value: #f, init-keyword: pointer-type-name:;
  lazy constant slot concrete-class-name, init-value: #f, init-keyword: concrete-class-name:;
  lazy constant slot raw-struct-name, init-value: #f, init-keyword: raw-struct-name:;

  // This is now just self.
  lazy slot ^concrete-class, init-value: #f;

end;

define-compiler-metaclass(#"<designator-class>", <&designator-class>);

define /* abstract */ &class <c-struct/union-designator-class> (<designator-class>)
end;  

define primary &class <c-struct-designator-class> (<c-struct/union-designator-class>)
end;

define &class <c-union-designator-class> (<c-struct/union-designator-class>)
end;

define-compiler-metaclass(#"<c-struct-designator-class>",
			  <&c-struct-designator-class>);

define-compiler-metaclass(#"<c-union-designator-class>",
			  <&c-union-designator-class>);

define &class <c-mapped-designator-class> (<designator-class>)
end;

define-compiler-metaclass(#"<c-mapped-designator-class>",
			  <&c-mapped-designator-class>);


define &class <c-automatic-pointer-designator-class> (<designator-class>)
end;

define-compiler-metaclass(#"<c-automatic-pointer-designator-class>",
			   <&c-automatic-pointer-designator-class>);

// A distinguished evaluator for designator class expressions.

define inline method ^eval-designator (fragment)
  ^top-level-eval-type(fragment)
end method;
