Module: bank-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Establish in-process entry points, in case that's how we're used.
in-process-automation-server(typeinfo: $bank-component,
			     prog-id: $bank-prog-id);

// Establish local-server entry point, in case that's how we're used.
define function main () => ();
  if (OLE-util-register-only?())
    // invoked to [un]register only - create a type library, and enter
    // everything in registry.
    register-coclass($bank-component, $bank-prog-id)
  else
    // make the server frame, initialize and refresh it.
    let server-frame = make(<server-frame>);
    refresh(server-frame);
    // Make and register the class factory for clients to call, and then
    // just show what's happening in the server-frame.
    with-class-factory (class: <bank>,
			clsid: $Bank-component.guid-value,
			server-frame: server-frame)
      start-frame(server-frame);
    end;
  end;
end main;

unless (OLE-util-in-process-startup?())
  main();
end;
