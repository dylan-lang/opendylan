Module:    naming-service-test-suite
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *naming-service* = make-nil(CosNaming/<NamingContext>);
define variable *object1* = make-nil(CORBA/<Object>);
define variable *object2* = make-nil(CORBA/<Object>);

define test connect-test ()
  let orb = CORBA/ORB-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  check("Resolve Naming Service", method () *naming-service* := as(CosNaming/<NamingContext>, CORBA/ORB/resolve-initial-references(orb, "NameService")) end);
  check-false("Naming service reference not nil", corba/object/is-nil(*naming-service*));
  check-false("Naming service object exists", corba/object/non-existent(*naming-service*));
  *object1* = *naming-service*;
end test;

define test invalid-name-test ()
  let invalid-name = make(CosNaming/<Name>);
  check-condition("Invalid name passed to bind", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/bind(*naming-service*, invalid-name, *object1*));
  check-condition("Invalid name passed to rebind", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/rebind(*naming-service*, invalid-name, *object1*));
  check-condition("Invalid name passed to resolve", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/resolve(*naming-service*, invalid-name));
  check-condition("Invalid name passed to unbind", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/unbind(*naming-service*, invalid-name));
  check-condition("Invalid name passed to bind_context", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/bind-context(*naming-service*, invalid-name, *naming-service*));
  check-condition("Invalid name passed to rebind_context", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/rebind-context(*naming-service*, invalid-name, *naming-service*));
  check-condition("Invalid name passed to bind_new_context", CosNaming/NamingContext/<InvalidName>, CosNaming/NamingContext/bind-new-context(*naming-service*, invalid-name));
end test;

define test bind-test ()
  let bind = CosNaming/NamingContext/bind;
  let rebind = CosNaming/NamingContext/rebind;
  let unbind = CosNaming/NamingContext/unbind;
  let resolve = CosNaming/NamingContext/resolve;
  let <AlreadyBound> = CosNaming/NamingContext/<AlreadyBound>;
  let <NotFound> = CosNaming/NamingContext/<NotFound>;

  let name1 = add!(make(CosNaming/<Name>), make(CosNaming/<NameComponent>, id: "foo", kind: "bar"));
  let name2 = add!(make(CosNaming/<Name>), make(CosNaming/<NameComponent>, id: "foo", kind: "baz"));
  let name3 = add!(make(CosNaming/<Name>), make(CosNaming/<NameComponent>, id: "fop", kind: "bar"));

  check-false("Bind name1 to object1", bind(*naming-service*, name1, *object1*));
  check-condition("Exception binding name1 to object2", <AlreadyBound>, bind(*naming-service*, name1, *object2*));
  check-false("Rebind name1 to object2", rebind(*naming-service*, name1, *object2*));

  check-false("Bind name2 to object1", bind(*naming-service*, name2, *object1*));
  check-false("Bind name3 to object1", bind(*naming-service*, name3, *object1*));

  check("Resolve name1", resolve, *naming-service*, name1);
  check("Resolve name2", resolve, *naming-service*, name2);
  check("Resolve name3", resolve, *naming-service*, name3);

  check-false("Unbind name1", unbind(*naming-service*, name1));
  check-false("Unbind name2", unbind(*naming-service*, name2));
  check-false("Unbind name3", unbind(*naming-service*, name3));

  check-condition("Resolve name1 signals error", <NotFound>, resolve(*naming-service*, name1));
  check-condition("Resolve name2 signals error", <NotFound>, resolve(*naming-service*, name2));
  check-condition("Resolve name3 signals error", <NotFound>, resolve(*naming-service*, name3));
end test;

define test context-test ()
  let bind = CosNaming/NamingContext/bind;
  let unbind = CosNaming/NamingContext/unbind;
  let resolve = CosNaming/NamingContext/resolve;
  let new-context = CosNaming/NamingContext/new-context;
  let bind-context = CosNaming/NamingContext/bind-context;
  let rebind-context = CosNaming/NamingContext/rebind-context;
  let bind-new-context = CosNaming/NamingContext/bind-new-context;
  let destroy = CosNaming/NamingContext/destroy;
  let <NotEmpty> = CosNaming/NamingContext/<NotEmpty>;

  let name1 = add!(make(CosNaming/<Name>), make(CosNaming/<NameComponent>, id: "foo", kind: "context"));
  let context1 = #f;
  check("Create context1", method () context1 := new-context(*naming-service*) end);
  check-false("Bind context1", bind-context(*naming-service*, name1, context1));

  let name2 = add(name1, make(CosNaming/<NameComponent>, id: "bar", kind: "context"));
  let context2 = #f;
  check("Create and bind context2", method () context2 := bind-new-context(*naming-service*, name2) end);

  let name3 = add(name1, make(CosNaming/<NameComponent>, id: "baz", kind: "object"));
  check-false("Bind object1 in context1", bind(*naming-service*, name3, *object1*));

  let name5 = make(CosNaming/<NameComponent>, id: "baz", kind: "object");
  let name4 = add(name2, name5);
  check-false("Bind object2 in context2", bind(*naming-service*, name4, *object2*));

  check("Resolve object1", resolve, *naming-service*, name3);
  check("Resolve object2", resolve, *naming-service*, name4);

  check-condition("Destroy context2 signals error", <NotEmpty>, destroy(context2));
  check-false("Unbind object2 in context2", unbind(*naming-service*, name4));
  check-false("Destroy context2", destroy(context2));

  check-condition("Request on context2 signals error", CORBA/<Object-Not-Exist>, resolve(context2, add!(make(CosNaming/<Name>), name5)));

  check-false("Unbind object1 in context1", unbind(*naming-service*, name3));
  check-condition("Destroy context1 signals error", <NotEmpty>, destroy(context1));
  check-false("Unbind context2 in context1", unbind(*naming-service*, name2));
  check-false("Destroy context1", destroy(context1));

  check-false("Unbind context1", unbind(*naming-service*, name1));
end test;

define constant $objects = #["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"];
define constant $contexts = #["nc-one", "nc-two", "nc-three", "nc-four", "nc-five", "nc-six", "nc-seven", "nc-eight", "nc-nine", "nc-ten"];
define constant $total-bindings = size($objects) + size($contexts);

define test list-test ()
  let bind = CosNaming/NamingContext/bind;
  let bind-new-context = CosNaming/NamingContext/bind-new-context;
  let unbind = CosNaming/NamingContext/unbind;
  let resolve = CosNaming/NamingContext/resolve;
  let list = CosNaming/NamingContext/list;
  let next-one = CosNaming/BindingIterator/next-one;
  let next-n = CosNaming/BindingIterator/next-n;
  let destroy = CosNaming/BindingIterator/destroy;

  let name-component = make(CosNaming/<NameComponent>, id: "foo", kind: "bar");
  let name = make(CosNaming/<Name>, size: 1, fill: name-component);

  local method bind-name (id :: <string>)
          name-component.CosNaming/NameComponent/id := id;
          bind(*naming-service*, name, *object1*);
        end method;

  local method unbind-name (id :: <string>)
          name-component.CosNaming/NameComponent/id := id;
          unbind(*naming-service*, name);
        end method;

  local method bind-context (id :: <string>)
          name-component.CosNaming/NameComponent/id := id;
          bind-new-context(*naming-service*, name);
        end method;

  local method unbind-context (id :: <string>)
          name-component.CosNaming/NameComponent/id := id;
          let context = resolve(*naming-service*, name);
          unbind(*naming-service*, name);
          CosNaming/NamingContext/destroy(context);
        end method;

  local method note-name (ids :: <table>, binding :: CosNaming/<Binding>)
          let name = CosNaming/Binding/binding-name(binding);
          let id = as(<symbol>, CosNaming/NameComponent/id(name[0]));
          let count = element(ids, id, default: 0);
          ids[id] := count + 1;
        end method;

  local method check-names (message :: <string>, ids :: <table>)
          check-true(concatenate(message, ": counts"), every?(method (count) count = 1 end, ids));
          check-equal(concatenate(message, ": number"), size(key-sequence(ids)), $total-bindings);
        end method;

  do(bind-name, $objects);
  do(bind-context, $contexts);

  let (bindings, iterator) = list(*naming-service*, $total-bindings);
  check-equal("Full list bindings count", size(bindings), $total-bindings);
  check-true("Full list reference is nil", Corba/Object/is-nil(iterator));
  let ids = make(<table>);
  do(curry(note-name, ids), bindings);
  check-names("Full list names check", ids);

  let (bindings, iterator) = CosNaming/NamingContext/list(*naming-service*, $total-bindings + 100);
  check-equal("Over-full list bindings count", size(bindings), $total-bindings);
  check-true("Over-full list reference is nil", CORBA/Object/is-nil(iterator));
  remove-all-keys!(ids);
  do(curry(note-name, ids), bindings);
  check-names("Over-full list names check", ids);

  let (bindings, iterator) = CosNaming/NamingContext/list(*naming-service*, $total-bindings - 15);
  check-equal("Partial list bindings count", size(bindings), $total-bindings - 15);
  check-false("Partial list reference is not nil", CORBA/Object/is-nil(iterator));
  remove-all-keys!(ids);
  do(curry(note-name, ids), bindings);

  let (success?, binding) = CosNaming/BindingIterator/next-one(iterator);
  check-true("next-one success", success?);
  note-name(ids, binding);

  let (success?, bindings) = CosNaming/BindingIterator/next-n(iterator, 5);
  check-true("next-n(iterator, 5) success", success?);
  check-equal("next-n(iterator, 5) bindings", size(bindings), 5);
  do(curry(note-name, ids), bindings);

  let (success?, bindings) = CosNaming/BindingIterator/next-n(iterator, 9);
  check-true("next-n(iterator, 9) success", success?);
  check-equal("next-n(iterator, 9) bindings", size(bindings), 9);
  do(curry(note-name, ids), bindings);

  let (success?, bindings) = CosNaming/BindingIterator/next-n(iterator, $total-bindings);
  check-false("next-n on exhausted iterator returns #f", success?);
  check-equal("next-n on exhausted iterator bindings", size(bindings), 0);

  let (success?, binding) = CosNaming/BindingIterator/next-one(iterator);
  check-false("next-one on exhausted iterator returns #f", success?);

  CosNaming/BindingIterator/destroy(iterator);
  check-condition("Request on destroyed iterator signals error", CORBA/<Object-Not-Exist>, next-one(iterator));

  check-names("Partial list names check", ids);

  do(unbind-name, $objects);
  do(unbind-context, $contexts);
end test;

define suite naming-service-test-suite ()
  test connect-test;
  test invalid-name-test;
  test bind-test;
  test context-test;
  test list-test;
end suite;

define method main () => ()
  run-test-application(naming-service-test-suite);
end method;

main();

