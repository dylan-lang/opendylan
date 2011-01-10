Module:    inproc-dual
Synopsis:  Test use of custom interfaces in a coclass.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $class-id = 
	as(<REFIID>, "{0844834F-265D-11D2-9A67-006097C90313}");

define COM-interface <inproc-coclass-object> ( <simple-component-object> )
end;

define coclass $test-coclass-type-info
  name "InProcCoclassTest";
  documentation "Test dual interface in an in-process server";
  uuid $class-id;
  component-class <inproc-coclass-object>;
  interface <dll-donkey>;
end;


in-process-automation-server(typeinfo: $test-coclass-type-info,
			     title: "Test in-proc server with dual interface",
			     class-id: $class-id,
			     prog-id: "HQNexamples.InprocDualTest",
			     class: <inproc-coclass-object>);

