Module: socket-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test make-socket( description: "Can we make a socket?")
  let foo = make(<socket>, host-name: "nirvana.functionalobjects.com", port: 80);
  check-equal("make socket, remote host-name",
	      host-name(socket-inet-address(foo)),
	      "nirvana.functionalobjects.com");
  check-equal("make socket, remote port slot",
	      socket-port(foo),
	      80);
  check-condition("make socket with no port",
		  <socket-error>,
		  make(<socket>, host-name: "nirvana.functionalobjects.com"));
  check-condition("make socket to bad host-name",
		  <unknown-host-error>,
		  make(<socket>, host-name: "bad-host.bad-domain.org", 
		  port: 80));  
end test make-socket;

define test make-inet-address(description: "Check slots of inet-address object")
  let foo = make(<inet-address>);
  check-equal("NULL inet-address, host-name slot",
	      foo.host-name,
	      "0.0.0.0");
  check-equal("NULL inet-address, host-address slot",
	      foo.host-address,
	      "0.0.0.0");
  check-equal("NULL inet-address, address slot",
	      foo.address,
	      as(<byte-vector>,#[0,0,0,0]));

  let bar = make(<inet-address>, host-name: "meteor.menlo.functionalobjects.com");
  check-equal("make using host-name, Host-name slot",
	      bar.host-name,
	      "meteor.menlo.functionalobjects.com");
  check-equal("make using host-name, Host-address slot",
	      bar.host-address,
	      "198.3.233.15");
  check-equal("make using host-name, address slot",
	      bar.address,
	      as(<byte-vector>,#[198,3,233,15]));
  check-equal("making using host-name, address-family slot",
	      bar.address-family,
	      $AF-INET);

  // Make an <inet-address> using a in-addr vector
  let addr :: <byte-vector> = as(<byte-vector>,#[198,3,233,15]);
  check-equal("addr for make using address",
	      addr,
	      as(<byte-vector>,#[198,3,233,15]));
  let inet-addr = make(<inet-address>, address: addr);

  check-true("make using address, host-name slot",
	      subsequence-position("meteor.menlo.functionalobjects.com", 
				   inet-addr.host-name) == 0);
  check-equal("make using address, host-address slot",
	      inet-addr.host-address,
	      "198.3.233.15");
  check-equal("make using address, address slot",
	      inet-addr.address,
	      as(<byte-vector>,#[198,3,233,15]));
end test make-inet-address;

define test make-numeric-host-name(description: "Making IP address from numeric host-names")
  let foo = make(<inet-address>, host-name: "198.3.233.15");
  check-equal("numeric host-name, host-name slot",
	      foo.host-name,
	      "198.3.233.15");
  check-equal("numeric host-name, host-address slot",
	      foo.host-address,
	      "198.3.233.15");
  check-equal("numeric host-name, address slot",
	      foo.address,
	      as(<byte-vector>,#[198,3,233,15]));

  // this was an early java bug. making an inet-address when the numeric address could
  // not be found
  foo := make(<inet-address>, host-name: "198.3.231.4");
  check-equal("numeric host-name, host-name slot",
	      foo.host-name,
	      "198.3.231.4");
  check-equal("numeric host-name, host-address slot",
	      foo.host-address,
	      "198.3.231.4");
  check-equal("numeric host-name, address slot",
	      foo.address,
	      as(<byte-vector>,#[198,3,231,4]));
end test make-numeric-host-name;

define test constructors(description: "Test the constructor functions")
  let foo = inet-address-by-host-name("nirvana.functionalobjects.com");
  check-equal("inet-address-by-host-name, host-name slot",
	      foo.host-name,
	      "nirvana.functionalobjects.com");
  check-equal("inet-address-by-host-name, host-address slot",
	      foo.host-address,
	      "192.124.144.117");
  check-equal("inet-address-by-host-name, address slot",
	      foo.address,
	      as(<byte-vector>,#[192,124,144,117]));
  check-condition("Making a invalid hostname",
		  <unknown-host-error>,
		  inet-address-by-host-name("bad-host.bad-domain.org"));
end test constructors;

define suite inet-address-module-test-suite(description: "Exercise those 'ol inet-address objects")
  test make-inet-address;
  test make-numeric-host-name;
  test constructors;
end suite;

define test socket-conditions-classes(description: "Are things correctly subclassed?")
  check-true("<socket-condition>",
	     subtype?(<socket-condition>,<condition>));
  check-true("<socket-warning>",
	     subtype?(<socket-warning>,<socket-condition>)
	       & subtype?(<socket-warning>,<warning>));
  check-true("<socket-error>",
	     subtype?(<socket-error>,<socket-condition>)
	       & subtype?(<socket-error>,<error>));
  check-true("<unknown-host-error>",
	     subtype?(<unknown-host-error>,<socket-error>));
end test socket-conditions-classes;

define test make-socket-conditions( description: "Can we make a socket condition")
  check-condition("socket-warning()",
		  <socket-warning>,
		  socket-warning("Testing"));
  check-condition("socket-error()",
		  <socket-error>,
		  socket-error("Testing"));
end test make-socket-condition;

define suite socket-conditions-test-suite(description: "Testing socket conditions")
  test socket-conditions-classes;
  test make-socket-conditions;
end suite socket-conditions-test-suite;

define suite socket-module-test-suite(description: "Testing sockets")
  test make-socket;
end suite socket-module-test-suite; 

define suite socket-test-suite(description: "all test for library")
  suite socket-conditions-test-suite;
  suite inet-address-module-test-suite;
  suite socket-module-test-suite;
end suite socket-test-suite;
