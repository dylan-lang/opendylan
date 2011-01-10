Module:    sample-OLE-server
Synopsis:  in-process server support
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Definitions needed to support an in-process server.  Everything in this
// file can be safely omitted if only a local server is needed.

// application class ID for in-process server
define constant $GUID-IN-PROCESS = "{1FDDEC71-005B-11D0-89FD-02070119F639}";

// In-process server OLE object class
define COM-interface <in-process-simple-server> (<simple-server>,
						 <ole-in-process-server>)
end;

// Set up DLL entry points.
initialize-ole-server(<in-process-simple-server>,
		      $GUID-IN-PROCESS,
		      "HQN.DylanSample.1",
		      "Dylan simple in-process OLE Server", // for "insert object"
		      short-name: "Dylan OLE Server"); // name for menus


