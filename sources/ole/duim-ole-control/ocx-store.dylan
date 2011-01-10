Module:    DUIM-OLE-Control
Synopsis:  Persistent storage for OLE Controls using DUIM.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Eventually this whole file may need to be moved to the `DUIM-OLE-Server' 
// library as part of supporting in-process servers.  In the meantime,
// the `sideways' adjective was added to the following methods to suppress
// warnings about ``method ... visible to sibling libraries''.

define open generic max-storage-size(frame) => (max-size);

define sideways method OLE-part-Load-From-Stream
	(this :: <basic-DUIM-OLE-server>, istream :: <LPSTREAM>) => ();

  Output-Debug-String("OLE-part-Load-From-Stream\r\n");
  let frame = this.app-frame;
  // Read the user data
  load-frame-from-storage(frame, istream);
  values()
end method OLE-part-Load-From-Stream;


define sideways method OLE-part-Save-To-Stream(this :: <basic-DUIM-OLE-server>,
					       istream :: <LPSTREAM>) => ();
  Output-Debug-String("OLE-part-Save-To-Stream\r\n");

  // write the user data to the stream
  let frame = this.app-frame;
  save-frame-to-storage(frame, istream);
end method OLE-part-Save-To-Stream;

define sideways method OLE-part-max-save-size(this :: <basic-DUIM-OLE-server>)
 => (max-size);
  let frame = this.app-frame;
  max-storage-size(frame)
end method;

// default method:
define method max-storage-size(frame :: <object>) => (size);
  #f
end method;
