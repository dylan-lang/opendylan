Module:    struct-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *structtest-ior-file* = "c:\\temp\\structtest.ior";

define variable *struct-a* :: <StructureA>
 = make(<StructureA>, // a-short: -32768, a-short: 32768,
                      a-long: -40000, a-ulong: 40000,
                      a-octet: 128);
define variable *struct-b* :: <StructureB>
 = make(<StructureB>, b-boolean: #f);
define variable *struct-c* :: <StructureC>
 = make(<StructureC>, c-string: "Tinky Winky", c-struct: *struct-b*, c-char: 'A');
define variable *struct-d* :: <StructureD>
 = make(<StructureD>, d-float: 0.0, d-double: 1.0d0);

// Define these temporarily until Scepter/Orb does.
//
define sideways method \= (struct-1 :: <StructureA>, struct-2 :: <StructureA>)
 => (equal? :: <boolean>)
  ((StructureA/a-long(struct-1) = StructureA/a-long(struct-2))
 & (StructureA/a-ulong(struct-1) = StructureA/a-ulong(struct-2))
 & (StructureA/a-octet(struct-1) = StructureA/a-octet(struct-2)))
end method;

define sideways method \= (struct-1 :: <StructureB>, struct-2 :: <StructureB>)
 => (equal? :: <boolean>)
  (StructureB/b-boolean(struct-1) = StructureB/b-boolean(struct-2))
end method;

define sideways method \= (struct-1 :: <StructureC>, struct-2 :: <StructureC>)
 => (equal? :: <boolean>)
  ((StructureC/c-string(struct-1) = StructureC/c-string(struct-2))
 & (StructureC/c-struct(struct-1) = StructureC/c-struct(struct-2))
 & (StructureC/c-char(struct-1) = StructureC/c-char(struct-2)))
end method;

define sideways method \= (struct-1 :: <StructureD>, struct-2 :: <StructureD>)
 => (equal? :: <boolean>)
  ((StructureD/d-float(struct-1) = StructureD/d-float(struct-2))
 & (StructureD/d-double(struct-1) = StructureD/d-double(struct-2)))
end method;

define test struct-any-test ()
  let struct-a-any = as(CORBA/<any>, *struct-a*);
  check-true("Coerce *struct-a* to an any", instance?(struct-a-any, CORBA/<any>));
  check-equal("Coerce any back to <StructureA>", *struct-a*, as(<StructureA>, struct-a-any));

  let struct-b-any = as(CORBA/<any>, *struct-b*);
  check-true("Coerce *struct-b* to an any", instance?(struct-b-any, CORBA/<any>));
  check-equal("Coerce any back to <StructureB>", *struct-b*, as(<StructureB>, struct-b-any));

  let struct-c-any = as(CORBA/<any>, *struct-c*);
  check-true("Coerce *struct-c* to an any", instance?(struct-c-any, CORBA/<any>));
  check-equal("Coerce any back to <StructureC>", *struct-c*, as(<StructureC>, struct-c-any));

  let struct-d-any = as(CORBA/<any>, *struct-d*);
  check-true("Coerce *struct-d* to an any", instance?(struct-d-any, CORBA/<any>));
  check-equal("Coerce any back to <StructureD>", *struct-d*, as(<StructureD>, struct-d-any));
end test;

//define test struct-attribute-test ()
//end test;

define test struct-in-parameter-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = corba/orb/get-default-context(orb);
  let structtest = as(<structtest>, corba/orb/file-to-object(orb, *structtest-ior-file*));
  check-true("<StructureA> in parameter", structtest/in-parameter-a(structtest, *struct-a*));
  check-true("<StructureB> in parameter", structtest/in-parameter-b(structtest, *struct-b*));
  check-true("<StructureC> in parameter", structtest/in-parameter-c(structtest, *struct-c*));
  check-true("<StructureD> in parameter", structtest/in-parameter-d(structtest, *struct-d*));
end test;

define test struct-result-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = corba/orb/get-default-context(orb);
  let structtest = as(<structtest>, corba/orb/file-to-object(orb, *structtest-ior-file*));
  check-equal("<StructureA> result", structtest/result-a(structtest), *struct-a*);
  check-equal("<StructureB> result", structtest/result-b(structtest), *struct-b*);
  check-equal("<StructureC> result", structtest/result-c(structtest), *struct-c*);
  check-equal("<StructureD> result", structtest/result-d(structtest), *struct-d*);
end test;

define suite struct-test-suite ()
  test struct-any-test;
//  test struct-attribute-test;
  test struct-in-parameter-test;
  test struct-result-test;
end suite;

