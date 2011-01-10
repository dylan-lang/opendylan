Module: corba-tests-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite context-tests ()
  test context-checks;
end suite;

// TEST PLAN
//
//    corba/<Context>,
//    corba/context/context-name, [x]
//    corba/context/parent, [x]
//    corba/context/create-child, [x]
//    corba/context/set-one-value, [x]
//    corba/context/set-values, [x]
//    corba/context/delete-values, [x]
//    corba/context/get-values, [x]
//    corba/$ctx-restrict-scope;
//    "wildcards"

define test context-checks ()
  let orb :: corba/<orb> = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let default-context :: corba/<context> = corba/orb/get-default-context(orb);

  corba/context/set-one-value(default-context, "Foo", as(corba/<any>, "Bar"));
  let results :: corba/<nvlist> = corba/context/get-values(default-context, "", 0, copy-sequence("Foo"));
  check("can get-values of set-one-value", \=, copy-sequence("Bar"), as(corba/<string>, corba/namedvalue/argument(results[0])));

  let values :: corba/<nvlist> = make(corba/<nvlist>);
  values := add!(values, make(corba/<namedvalue>, name: "Baz", argument: as(corba/<any>, "Quux"), len: 0, arg-modes: 0));
  corba/context/set-values(default-context, values);
  let new-results :: corba/<nvlist> = corba/context/get-values(default-context, "", 0, copy-sequence("Baz"));
  check("can get-values of set-values", \=, copy-sequence("Quux"), as(corba/<string>, corba/namedvalue/argument(new-results[0])));

  corba/context/delete-values(default-context, copy-sequence("Baz"));
  check-condition("delete-values works", <simple-error>, corba/context/get-values(default-context, "", 0, copy-sequence("Baz")));

  let child-context :: corba/<context> = corba/context/create-child(default-context, "Child Context");
  check("can retrieve context name", \=, corba/context/context-name(child-context), copy-sequence("Child Context"));
  check("can retrieve parent context", \=, corba/context/parent(child-context), default-context);

  corba/context/set-one-value(child-context, copy-sequence("ChildFoo"), as(corba/<any>, "ChildBar"));
  let child-results :: corba/<nvlist> = corba/context/get-values(child-context, "", 0, copy-sequence("ChildFoo"));
  check("Can retrieve shadowed value", \=, copy-sequence("ChildBar"), as(corba/<string>, corba/namedvalue/argument(child-results[0])));

  let all-results :: corba/<nvlist> = corba/context/get-values(child-context, "", 0, copy-sequence("Foo*"));
  check("Can retrieve inherited values", \=, 2, size(all-results));

  let scope-start-results :: corba/<nvlist>
    = corba/context/get-values(default-context, copy-sequence("Child Context"), 0, copy-sequence("ChildFoo"));
  check("Can use scope start", \=, copy-sequence("ChildBar"), as(corba/<string>, corba/namedvalue/argument(scope-start-results[0])));

  check-condition("check restricted context scope",
		  <simple-error>,
		  corba/context/get-values(child-context, "", corba/$ctx-restrict-scope, copy-sequence("Foo")));
end test;

