Module:    parameter-type-tests
Synopsis:  Tests of parameter types for COM interfaces.
Author:    Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define coclass $VtableParmTest-type-info
  name "VtableParmTest";
  documentation "Test parameter types for a vtable interface";
  uuid $VtableParmTest-uuid;
  interface <S-IVtableParmTest>;
end;

in-process-automation-server(typeinfo: $VtableParmTest-type-info,
			     title: "Test parameter types with in-proc vtable "
				    "server",
			     class-id: $VtableParmTest-uuid,
			     prog-id: "HQNexamples.InprocVtableParmTest");
