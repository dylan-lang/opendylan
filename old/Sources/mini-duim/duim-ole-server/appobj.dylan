Module:    OLE-Server-Framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define COM-interface <CSimpSvrObj> ( <ole-server-framework> )

  slot m-lpDoc :: <CSimpSvrDoc>;      // Back pointer

  slot m-lpDataStream :: <LPSTREAM>;

 // add application-specific slots here	

end <CSimpSvrObj>;

define method initialize ( this :: <CSimpSvrObj>,
			  #key lpSimpSvrDoc :: <CSimpSvrDoc>, #all-keys) => ();
  next-method();

  this.m-lpDoc := lpSimpSvrDoc;

  this.m-lpDataStream := null-pointer(<LPSTREAM>);

  values();
end initialize;

define method terminate (this :: <CSimpSvrObj>) => ();

  Output-Debug-String("In CSimpSvrObj's Destructor \r\n");

  let DocObj :: <CSimpSvrDoc> = this.m-lpDoc;

  // if we were started by ole, post ourselves a close message
  // But this method only gets called when run stand alone! ???
  if ( IsStartedByOle(GetApp(DocObj)) )
    PostMessage(GethAppWnd(DocObj), $WM-SYSCOMMAND, $SC-CLOSE, 0);
  end if;

  // clear the OBJ ptr in the doc class
  ClearObj(DocObj);

  next-method()
end method terminate ;


define method GetApp(obj :: <CSimpSvrObj>) => value :: <CSimpSvrApp>; 
  GetApp(obj.m-lpDoc)
end method GetApp;

//**********************************************************************
//
// CSimpSvrObj::Draw
//
// Purpose:
//
//      Draws the object into an arbitrary DC
//
// Parameters:
//
//      HDC hDC - DC to draw into
//
// Return Value:
//
//      NONE
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CreateBrushIndirect         Windows API
//      SelectObject                Windows API
//      Rectangle                   Windows API
//      DeleteObject                Windows API
//
// Comments:
//
//
//********************************************************************

define method Draw(this :: <CSimpSvrObj>, hDC :: <HDC>, fMeta :: <boolean>)
 => ();

  Output-Debug-String("In CSimpSvrObj::Draw\r\n");

  let doc :: <CSimpSvrDoc> = this.m-lpDoc;

  if ( ~ fMeta ) // for WM_PAINT; this code doesn't currently get used! ???
    let scale :: <real> = this.m-scale;
    // Output-Debug-String( format-to-string( "Drawing Scale %=\r\n",scale) );
    let ( x-size, y-size ) = GetSize(doc);
    SetMapMode(hDC, $MM-ANISOTROPIC);
    SetWindowOrgEx(hDC,  truncate/(this.m-xOffset, scale), 
		   truncate/(this.m-yOffset, scale), $NULL-POINT);
    SetWindowExtEx(hDC, x-size, y-size, $NULL-POINT);
    SetViewportExtEx(hDC,  truncate(x-size * scale), 
		     truncate(y-size * scale), $NULL-POINT);
  end if;

  let sheet = doc.m-doc-sheet;
  let mirror = duim/sheet-mirror(sheet);
  let oldDC = mirror.duim/%DC;
  let medium = duim/sheet-medium(sheet);
// block ()
    mirror.duim/%DC := hDC;
    duim/invalidate-drawing-state(medium);
    duim/handle-event(sheet,
		      make(duim/<window-repaint-event>,
			   sheet: sheet,
			   region: duim/sheet-region(sheet)));
// cleanup
    duim/invalidate-drawing-state(medium);
    mirror.duim/%DC := oldDC;
// end block;

  values();
end method Draw;

//**********************************************************************
//
// CSimpSvrObj::GetMetaFilePict
//
// Purpose:
//
//      Returns a handle to a metafile representation of the object.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      Handle to the metafile.
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      GlobalAlloc                     Windows API
//      GlobalLock                      Windows API
//      SetWindowOrg                    Windows API
//      SetWindowExt                    Windows API
//      CreateMetaFile                  Windows API
//      CloseMetaFile                   Windows API
//      GlobalUnlock                    Windows API
//      XformWidthInPixelsToHimetric    OLESTD
//      XformHeightInPixelsToHimetric   OLESTD
//      CSimpSvrObj::Draw               OBJ.CPP
//
// Comments:
//
//
//********************************************************************

define method GetSize ( doc :: <CSimpSvrDoc> ) 
 => (width :: <integer>, height :: <integer>);
  let sheet = duim/top-level-sheet(GetApp(doc).app-frame);
  duim/box-size(duim/sheet-region(sheet));
end method;

define method GetMetaFilePict(this :: <CSimpSvrObj>, kind :: <fixnum> )
	=> value :: <HANDLE>;

  Output-Debug-String("In CSimpSvrObj::GetMetaFilePict\r\n");

  // allocate the memory for the METAFILEPICT structure
  let hMFP :: <HANDLE> = GlobalAlloc(logior($GMEM-SHARE,$GHND),
				     size-of(<METAFILEPICT>));
  let lpMFP :: <LPMETAFILEPICT> = as(<LPMETAFILEPICT>, GlobalLock(hMFP));

  // get the size of the object in HIMETRIC
  let ( width, height ) = GetSize(this.m-lpDoc);
  let pt-x = XformWidthInPixelsToHimetric($NULL-HDC, width);
  let pt-y = XformHeightInPixelsToHimetric($NULL-HDC, height);

  // fill out the METAFILEPICT structure
  lpMFP.mm-value := $MM-ANISOTROPIC;
  lpMFP.xExt-value := pt-x;
  lpMFP.yExt-value := pt-y;

  // Create the metafile
  let hDC :: <HDC> = 
    if ( kind = $TYMED-ENHMF )
      CreateEnhMetaFile($NULL-HDC, $NULL-string,
			$NULL-RECT, $NULL-string)
    else
      CreateMetaFile($NULL-string)
    end if;

  SetWindowOrgEx(hDC, 0, 0, $NULL-POINT);
  SetWindowExtEx(hDC, width, height, null-pointer(<LPSIZE>));

  Draw(this, hDC, #t);

  lpMFP.hMF-value := 
    if ( kind = $TYMED-ENHMF )
      CloseEnhMetaFile(hDC)
    else
      CloseMetaFile(hDC)
    end if;

  // unlock the metafilepict
  GlobalUnlock(hMFP);

 /* return */  hMFP 
end method GetMetaFilePict;


//**********************************************************************
//
// CSimpSvrObj::SaveToStorage
//
// Purpose:
//
//      Saves the object to the passed storage
//
// Parameters:
//
//      LPSTORAGE lpStg - Storage in which to save the object
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::CreateStream      OLE
//      IStream::Write              OLE
//      IStream::Release            OLE
//
// Comments:
//
//      A real app will want to do better error checking / returning
//
//********************************************************************

define constant $NULL-ULARGE-INTEGER =
  null-pointer( <ULARGE-INTEGER>.pointer-type );

define constant $unsigned-large-zero :: <PULARGE-INTEGER> =
  make( <PULARGE-INTEGER>, value: 0 );

define constant $large-zero :: <PLARGE-INTEGER> =
  make( <PLARGE-INTEGER>, value: 0 );

define method OLE-part-Save-To-Storage(this :: <CSimpSvrObj>,
				       lpStg :: <Interface>,
				       fSameAsLoad :: <boolean>) => ();

  Output-Debug-String("In CSimpSvrObj::SaveToStorage\r\n");

  let lpTempData :: <LPSTREAM> = $NULL-interface;

  if ( ~fSameAsLoad )
    lpTempData := CreateTempStreams(this.m-PersistStorage, lpStg); 
  else
    lpTempData := this.m-lpDataStream;
    AddRef(lpTempData);
  end if;

  IStream/SetSize(lpTempData, $unsigned-large-zero);
  IStream/Seek(lpTempData, $large-zero,
	       $STREAM-SEEK-SET, $NULL-ULARGE-INTEGER);

  // write the data to the stream
  let frame = this.m-lpDoc.m-lpApp.app-frame;
  save-frame-to-storage( frame, lpTempData );
  frame.ole-frame-dirty? := #f;

  Release(lpTempData);

  values();
end method OLE-part-Save-To-Storage;


define open generic save-frame-to-storage (frame, stream);

// Default method does nothing.
define method save-frame-to-storage ( frame, stream ) => ();
  values()
end method;

/* example of real method for an application program:
define method save-frame-to-storage ( frame :: <foo-frame>,
				      stream :: <LPSTREAM> ) => ();
  // write the application data
  istream-write-integer(stream, frame.foo );
  istream-write-integer(stream, frame.bar );
  istream-write-integer(stream, frame.baz );
  values()
end method;
*/

//**********************************************************************
//
// CPersistStorage::CreateStreams
//
// Purpose:
//
//      Creates temporary streams in a storage.
//
// Parameters:
//
//      LPSTORAGE lpStg                 - Pointer to the storage
//
//      LPSTREAM FAR* lplpTempColor     - Color Stream
//
//      LPSTREAM FAR* lplpTempSize      - Size Stream
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::Release           OLE
//
// Comments:
//
//
//********************************************************************

define constant $stream-name :: <LPOLESTR> = as(<LPOLESTR>, "DUIMdata");

define method CreateTempStreams(this :: <CPersistStorage>,
				lpStg :: <LPSTORAGE>)
 => ( data-stream :: <LPSTREAM> );

  let ( status, data-stream ) =
    IStorage/CreateStream(lpStg, $stream-name,
			  logior( logior($STGM-READWRITE, $STGM-CREATE),
				 $STGM-SHARE-EXCLUSIVE),
			  0, 0);

  as( <LPSTREAM>, data-stream )
end method CreateTempStreams;

//**********************************************************************
//
// CSimpSvrObj::LoadFromStorage
//
// Purpose:
//
//      Loads the object from the passed storage
//
// Parameters:
//
//      LPSTORAGE lpStg     - Storage in which to load the object from
//
// Return Value:
//
//      None.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::OpenStream        OLE
//      IStream::Read               OLE
//      IStream::Release            OLE
//
// Comments:
//
//
//********************************************************************


define method OLE-part-Load-From-Storage(this :: <CSimpSvrObj>) => ();

  Output-Debug-String("In CSimpSvrObj::LoadFromStorage\r\n");

  // Read the data
  let frame = this.m-lpDoc.m-lpApp.app-frame;
  let stream = this.m-lpDataStream;
  load-frame-from-storage (frame, stream);
  frame.ole-frame-dirty? := #f;

  values();
end method OLE-part-Load-From-Storage;

define open generic load-frame-from-storage (frame, stream);

// Default method does nothing.
define method load-frame-from-storage ( frame, stream ) => ();
  values()
end method;

/* // example of real method for an application program:
define method load-frame-from-storage ( frame :: <foo-frame>,
				        stream :: <LPSTREAM> ) => ();
  // Read the application data
  frame.foo := istream-read-integer(stream);
  frame.bar := istream-read-integer(stream);
  frame.baz := istream-read-integer(stream);
  values()
end method;
*/

//********************************************************************


define method OLE-part-dirty? ( obj :: <CSimpSvrObj> ) => dirty? :: <boolean>;
  let app = GetApp(obj);
  let frame = app.app-frame;
  frame.ole-frame-dirty?
end method;
