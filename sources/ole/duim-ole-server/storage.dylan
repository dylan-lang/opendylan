Module:    DUIM-OLE-server
Synopsis:  Persistent storage of data for DUIM server applications.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $data-stream-name :: <LPOLESTR> = as(<LPOLESTR>, "DUIMdata");


define method OLE-part-Create-Streams(obj :: <DUIM-OLE-server>,
				      storage :: <LPSTORAGE>)
 => ();

  OLE-part-Release-Streams(obj);

  // create a stream to save the user's data
  obj.user-data-stream := OLE-util-Create-Stream(storage, $data-stream-name);
  values()
end method;


define method OLE-part-Open-Streams(obj :: <DUIM-OLE-server>,
				    storage :: <LPSTORAGE>)
 => ();
  OLE-part-Release-Streams(obj);
  obj.user-data-stream := OLE-util-open-stream(storage, $data-stream-name);
  values();
end method;


define method OLE-part-Release-Streams(obj :: <DUIM-OLE-server>) => ();

  Release(obj.user-data-stream);
  obj.user-data-stream := $null-istream;
  values()
end method;


define method OLE-part-Save-To-Storage(this :: <DUIM-OLE-server>,
				       lpStg :: <Interface>,
				       fSameAsLoad :: <boolean>) => ();

  Output-Debug-String("OLE-part-Save-To-Storage\r\n");

  let istream :: <LPSTREAM> = $null-istream;

  if ( ~ fSameAsLoad )
    // Create temporary streams in a storage.
    istream := OLE-util-Create-Stream(lpStg, $data-stream-name);
  else
    istream := this.user-data-stream;
    AddRef(istream);
  end if;

  // replace old contents of stream
  istream-rewrite(istream);

  // write the user data to the stream
  let frame = this.app-frame;
  frame.embedded-data-changed? := #f;
  save-frame-to-storage( frame, istream );

  Release(istream);

  values();
end method OLE-part-Save-To-Storage;


define open generic save-frame-to-storage (frame, stream);

// Default method does nothing.
define method save-frame-to-storage ( frame, stream ) => ();
  values()
end method;

/* example of real method for an application program:
define method save-frame-to-storage ( frame :: <foo-frame>,
				      stream :: <storage-istream> ) => ();
  // write the application data
  istream-write-integer(stream, frame.foo );
  istream-write-integer(stream, frame.bar );
  istream-write-integer(stream, frame.baz );
  values()
end method;
*/


define method OLE-part-Load-From-Storage(this :: <DUIM-OLE-server>,
					 width :: <integer>,
					 height :: <integer>) => ();

  Output-Debug-String("OLE-part-Load-From-Storage\r\n");

  // update the size
  let doc = this.get-doc;
  let frame = doc.get-app.app-frame;
  let doc-sheet = doc.doc-sheet;
  let ( left, top, right, bottom ) = duim/sheet-edges(doc-sheet);
  let delta-width = width - ( right - left );
  let delta-height = height - ( bottom - top );
  unless ( zero?(delta-width) & zero?(delta-height) )
    duim/set-sheet-edges(doc-sheet, left, top, left + width, top + height);
    let app-sheet :: duim/<sheet> = duim/top-level-sheet(frame);
    unless ( app-sheet == doc-sheet )
      let ( left, top, right, bottom ) = duim/sheet-edges(app-sheet);
      duim/set-sheet-edges(app-sheet, left, top, right + delta-width,
			   bottom + delta-height);
    end unless;
  end unless;

  // Read the user data
  load-frame-from-storage (frame, this.user-data-stream);
  frame.embedded-data-changed? := #f;
  values()
end method OLE-part-Load-From-Storage;



define open generic load-frame-from-storage (frame, stream);

// Default method does nothing.
define method load-frame-from-storage ( frame, stream ) => ();
  values()
end method;

/* // example of real method for an application program:
define method load-frame-from-storage ( frame :: <foo-frame>,
				        stream :: <storage-istream> ) => ();
  // Read the application data
  frame.foo := istream-read-integer(stream);
  frame.bar := istream-read-integer(stream);
  frame.baz := istream-read-integer(stream);
  values()
end method;
*/


define method OLE-part-dirty? ( obj :: <basic-DUIM-OLE-server> )
				=> dirty? :: <boolean>;
  obj.app-frame.embedded-data-changed?
end method;

define method OLE-part-dirty?-setter (value :: <boolean>,
				      obj :: <basic-DUIM-OLE-server> )
				=> (dirty? :: <boolean>);
  obj.app-frame.embedded-data-changed? := value
end method;

define open generic frame-init-new-object(frame);

define method OLE-part-init-new (this :: <basic-DUIM-OLE-server>)
  Output-Debug-String("OLE-part-init-new\r\n");
  let frame = this.app-frame;
  frame-init-new-object(frame)
end method;

// default method
define method frame-init-new-object ( frame :: <object> );
  // Note: The COM documentation suggests returning E_NOTIMPL from 
  //  IPersistStreamInit::InitNew if it doesn't actually do anything,
  //  but Visual Basic 5.0 does not accept that as a valid response.
  $S-OK
end method;
