module: dfmc-conversion
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed generic ^map-congruency-classes (f :: <function>, t :: <&object>) => ();

define method ^map-congruency-classes (f :: <function>, t :: <&type>) => ()
end method;

define method ^map-congruency-classes (f :: <function>, t :: <&class>) => ()
  f(t)
end method;

define method ^map-congruency-classes (f :: <function>, t :: <&singleton>) => ()
  ^map-congruency-classes(f, ^object-class(^singleton-object(t)))
end method;

define method ^map-congruency-classes (f :: <function>, t :: <&subclass>) => ()
  ^map-congruency-classes(f, ^subclass-class(t))
end method;

// define method ^map-congruency-classes (f :: <function>, t :: <&limited-collection>) => ()
//   ^map-congruency-classes(f, ^limited-collection(t))
//   ^map-congruency-classes(f, ^limited-element-type(t))
// end method;

define method ^map-congruency-classes (f :: <function>, t :: <&union>) => ()
  ^map-congruency-classes(f, t.^union-type1);
  ^map-congruency-classes(f, t.^union-type2);
end method;

define function ^map-congruency-classes-sov 
    (f :: <function>, x :: <simple-object-vector>, n :: <integer>) => ()
  local method loop (i :: <integer>) => (well? :: <boolean>)
	  if (i >= 0) ^map-congruency-classes(f, x[i]); loop(i - 1) end
	end method;
  loop(n - 1)
end function;

define method ^map-congruency-classes (f :: <function>, sig :: <&signature>) => ()
  ^map-congruency-classes-sov(f, ^signature-required(sig), ^signature-number-required(sig));
  when (^signature-rest-value(sig))
    ^map-congruency-classes(f, ^signature-rest-value(sig));
  end when;
  ^map-congruency-classes-sov(f, ^signature-values(sig), ^signature-number-values(sig));
end method;

define method ^map-congruency-classes (f :: <function>, l :: <&lambda>) => ()
  ^map-congruency-classes(f, ^function-signature(l))
end method;

define method ^map-congruency-classes (f :: <function>, m :: <&accessor-method>) => ()
  let sd :: <&slot-descriptor> = ^method-slot-descriptor(m);
  ^map-congruency-classes(f, ^slot-owner(sd));
  ^map-congruency-classes(f, ^slot-type(sd));
end method;

////////// NOT NEEDED

/*
define method ^map-congruency-classes (f :: <function>, d :: <&standalone-domain>) => ()
  for (i from 0 below ^domain-number-required(d))
    ^map-congruency-classes(f, ^domain-type(d, i))
  end for
end method;

define method ^map-congruency-classes (f :: <function>, d :: <&method-domain>) => ()
  ^map-congruency-classes(f, ^domain-method(d))
end method;

define method ^map-congruency-classes (f :: <function>, g :: <&incremental-generic-function>)
 => ()
  ^map-congruency-classes(f, function-signature(g));
  map(curry(^map-congruency-classes, f), ^generic-function-methods(g));
  do(method(x) ^map-congruency-classes(f, first(x)) end, ^generic-function-incomplete-methods(g));
  map(curry(^map-congruency-classes, f), ^generic-function-incomplete-domains(g));
  (iterate loop (d :: false-or(<&domain>) = ^incremental-gf-domain-info(g))
     if (d)
       let d :: <&domain> = d;
       ^map-congruency-classes(f, d);
       loop(^domain-next(d))
     end if
  end iterate);
end method;
*/
