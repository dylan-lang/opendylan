Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module any-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    any-test-suite;
end module any-client;

define module array-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    array-test-suite;
end module array-client;

define module bank-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use corba-tests-stubs;
  use testworks;
  export
    bank-test-suite;
end module;

define module chat-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use format;
  use testworks;
  use corba-tests-skeletons;
  export
    chat-test-suite;
end module chat-client;

define module constant-client
  use functional-dylan;
  use dylan-orb;
  use testworks;
  use corba-tests-stubs;
  export
    constant-test-suite;
end module constant-client;

define module enum-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use format;
  use testworks;
  use corba-tests-stubs;
  export
    enum-test-suite;
end module enum-client;

define module grid-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    grid-test-suite;
end module;

define module pragma-client
  use functional-dylan;
  use dylan-orb;
  use testworks;
  use corba-tests-protocol;
  export
    pragma-test-suite;
end module pragma-client;

define module pseudo-objects-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    pseudo-objects-test-suite;
end module pseudo-objects-client;

define module sequence-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    sequence-test-suite;
end module sequence-client;

define module struct-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    struct-test-suite;
end module struct-client;

define module tree-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    $a-tree,
    tree-equal?,
    tree-test-suite;
end module tree-client;

define module union-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use corba-tests-stubs;
  export
    union-test-suite;
end module;

define module ir-client
  use functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
  use testworks;
  use format;
  use corba-tests-stubs;
  export
    ir-test-suite;
end module;

define module corba-tests-client
  use generic-arithmetic-functional-dylan;
  use dylan-orb;
  use dylan-orb-internals;
                                // ---*** behind the scenes access for
                                // test-suite:
                                // with-marshalling-stream,
                                // <marshalling-stream>, marshall,
                                // unmarshall,
                                // architecture-little-endian?
  use testworks;
  use corba-tests-utilities;
  use any-client;
  use array-client;
  use bank-client;
  use chat-client;
  use constant-client;
  use enum-client;
  use grid-client;
  use pragma-client;
  use pseudo-objects-client;
  use sequence-client;
  use struct-client;
  use tree-client;
  use union-client;
  use ir-client;

  use corba-tests-protocol;

  export
    corba-test-suite;
end module corba-tests-client;

