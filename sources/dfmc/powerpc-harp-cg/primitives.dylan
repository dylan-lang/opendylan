module: dfmc-powerpc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro harp-transcendental-method-definer
  { define harp-transcendental-method ?:name ?fun:name}
    =>
  {
    define sideways method "op--" ## ?name(be :: <powerpc-back-end>, #rest args) => ()
      runtime-reference("$" ## ?fun);
      ?=next-method();
    end method;

  }

  { define harp-transcendental-method (?:name ?fun:name, ?names:*) }
    =>
  {
    define c-runtime-reference ?fun;
    define harp-transcendental-method ?name ?fun;
    define harp-transcendental-method (?names)
  }

  { define harp-transcendental-method (?f-op:name ?d-op:name ?fun:name, ?names:*) }
    =>
  {
    define c-runtime-reference ?fun;
    define harp-transcendental-method ?f-op ?fun;
    define harp-transcendental-method ?d-op ?fun;
    define harp-transcendental-method (?names)
  }

  { define harp-transcendental-method () }  => {}
end macro;

define harp-transcendental-method
  (floge dloge log, fetox detox exp, fsin dsin sin, fcos dcos cos, ftan dtan tan,
   fasin dasin asin, facos dacos acos, fatan datan atan);
