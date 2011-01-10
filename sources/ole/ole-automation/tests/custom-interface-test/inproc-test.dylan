Module:    custom-interface-test
Synopsis:  Test use of a dual interface implemented in an in-process server.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *inproc-server* :: <interface> = $null-interface;

define constant $inproc-server-class-id = 
	as(<REFIID>, "{0844834F-265D-11D2-9A67-006097C90313}");

define test inproc-server-test 
		    (name: "inproc-server-test",
		     description: "test dual interface as separate DLL")

  // format-out("starting inproc-server-test\n");

  // Load the server DLL
  check-false("start in-process server",
    null-pointer?(
      *inproc-server* := create-COM-instance($inproc-server-class-id,
					     context: $CLSCTX-INPROC-SERVER))
    );

  unless ( null?(*inproc-server*) )

    // AddRef on the donkey interface to keep it from being terminated.
    let ( status, donkey ) = QueryInterface(*inproc-server*, $IID-IDispatch);
    let dylan-interface = dylan-interface(*inproc-server*);

    check-equal("create-COM-instance returns dylan interface",
		dylan-interface, *inproc-server*);

    test-server(dylan-interface);

    let c-interface = make(<interface>,
			   address: pointer-address(*inproc-server*));
    if ( object-class(c-interface) ~= object-class(*inproc-server*) )
      test-server(c-interface);
    end if;

    check-not-crash("final release of in-process dual interface",
	    Release(donkey));

    check-equal("unload in-process server",
		Release(*inproc-server*),
		0);

  end; // not null server
  // format-out("ending inproc-server-test\n");
end test inproc-server-test;
