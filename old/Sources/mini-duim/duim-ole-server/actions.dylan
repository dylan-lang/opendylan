Module:    OLE-Server-Framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file defines the application-specific methods that are called
// from the generic OLE server framework.
// (In this version, "application-specific" means the DUIM glue layer, not
//  the actual application program using DUIM.)


define method OLE-part-class-ID (obj :: <CSimpSvrObj>) => ID :: <REFGUID>;
  let app = GetApp(obj);
  app.app-class-id
end method;

define method OLE-part-title (obj :: <CSimpSvrObj>) => title :: <LPOLESTR>;
  let frame :: duim/<frame> = GetApp(obj).app-frame;
  OLESTR(duim/frame-title(frame) | "Simple DUIM OLE Server" )
end method;

// Open as separate top-level window (instead of in-place).
define method OLE-part-open-out(this :: <CSimpSvrObj>)
 => window :: <HWND>; 

  let doc :: <CSimpSvrDoc> = this.m-lpDoc;

  HideHatchWnd(doc);

  // Show app window.
  ShowAppWnd(GetApp(doc), $SW-SHOWNORMAL);
  let top-window = GethAppWnd(doc);
  SetParent(GethDocWnd(doc), top-window);
  top-window
end method;

// Hide the application window (for out-of-place activation):
define method OLE-part-hide (obj :: <CSimpSvrObj>) => ();
  HideAppWnd(GetApp(obj));
  values()
end method;


define method OLE-part-in-place-activate ( obj :: <CSimpSvrObj> );
  
  let Doc = obj.m-lpDoc;

  // show the hatch window
  ShowHatchWnd(Doc);

  let hatch-window = GethHatchWnd(Doc);
  let doc-window = GethDocWnd(Doc);

  // Set the parenting
  SetParent(hatch-window, obj.m-hWndParent);
  SetParent(doc-window, hatch-window);

  values ( hatch-window, doc-window )
end method;

define method OLE-part-set-focus ( obj :: <CSimpSvrObj> ) => ();
  SetFocus(GethDocWnd(obj.m-lpDoc));
  values()
end method;

define method OLE-part-insert-menus ( obj :: <CSimpSvrObj>,
				     hmenuShared :: <HMENU>,
				     nFirstGroup :: <fixnum> ) => ();

  let frame :: duim/<frame> = GetApp(obj).app-frame;
  let menu-bar = duim/frame-menu-bar(frame);
  for (sub-menu in reverse(duim/sheet-children(menu-bar)))
    let sub-hmenu = duim/make-win32-menu(sub-menu);
    let ok? = InsertMenu(hmenuShared, nFirstGroup,
			 logior($MF-BYPOSITION, $MF-POPUP),
			 pointer-address(sub-hmenu),
			 TEXT(duim/gadget-label(sub-menu)));
  end;
  values()
end method;

define method OLE-part-in-place-deactivate ( obj :: <CSimpSvrObj> ) => ();

  let doc = obj.m-lpDoc;
  // change the parenting
  SetParent(GethDocWnd(doc), GethAppWnd(doc)); // child, new parent
  SetParent(GethHatchWnd(doc),GethDocWnd(doc));
  values()
end method;

define method OLE-part-doc-window ( obj :: <CSimpSvrObj> )
 => doc-window :: <HWND>;
  GethDocWnd(obj.m-lpDoc)
end method;

define method OLE-part-hatch-window ( obj :: <CSimpSvrObj> )
 => hatch-window :: <HWND>;
  GethHatchWnd(obj.m-lpDoc)
end method;

// The window handle to which menu choices will be dispatched:
define method OLE-part-top-window ( obj :: <CSimpSvrObj> )
  GethAppWnd(obj.m-lpDoc)
end method;

define method OLE-part-get-data (obj :: <CSimpSvrObj>,
				 pformatetcIn :: <LPFORMATETC>,
				 pmedium :: <LPSTGMEDIUM> )
 => status :: <HRESULT>;
	
  // Check to the FORMATETC and fill pmedium if valid.
  if ( (pformatetcIn.cfFormat-value = $CF-METAFILEPICT)
	& (pformatetcIn.dwAspect-value = $DVASPECT-CONTENT)
	& (logand(pformatetcIn.tymed-value,
		  logior($TYMED-MFPICT, $TYMED-ENHMF)) ~= 0) )

    let kind =
      // Use new-style metafile if the container supports it.
      if ( logand(pformatetcIn.tymed-value, $TYMED-ENHMF) ~= 0 )
	$TYMED-ENHMF
      else
	$TYMED-MFPICT
      end if;
    let hmfPict :: <HANDLE> = GetMetaFilePict(obj, kind);
    pmedium.tymed-value := kind;
    pmedium.u-value.hGlobal-value := hmfPict;
    pmedium.pUnkForRelease-value := $null-interface;
    $S-OK
  else $DATA-E-FORMATETC
  end if
end method;




//**********************************************************************
//
// CPersistStorage::CreateStreams
//
// Purpose:
//
//      Creates the streams that are held open for the object's lifetime.
//
// Parameters:
//
//      LPSTORAGE lpStg -   Storage in which to create the streams
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
//      IStream::Release            OLE
//      IStorage::CreateStream      OLE
//
// Comments:
//
//
//********************************************************************

define method OLE-part-Create-Streams(lpObj :: <CSimpSvrObj>,
				      lpStg :: <LPSTORAGE>)
 => ();

  Release(lpObj.m-lpDataStream);

  // create a stream to save the data
  let ( status, ppstm ) =
    IStorage/CreateStream(lpStg, $stream-name,
			  logior($STGM-READWRITE, $STGM-SHARE-EXCLUSIVE,
				 $STGM-CREATE),
			  0, 0);
  lpObj.m-lpDataStream := ppstm;

  values()
end method;

//**********************************************************************
//
// CPersistStorage::OpenStreams
//
// Purpose:
//
//      Opens the streams in a storage.
//
// Parameters:
//
//      LPSTORAGE lpStg -   Storage in which to open the streams.
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


define method OLE-part-Open-Streams(lpObj :: <CSimpSvrObj>,
				    lpStg :: <LPSTORAGE>)
 => ();
  Release(lpObj.m-lpDataStream);

  let ( status, ppstm ) =
    IStorage/OpenStream(lpStg, $stream-name, $NULL-VOID,
			logior($STGM-READWRITE,$STGM-SHARE-EXCLUSIVE),
			0);
  lpObj.m-lpDataStream := ppstm;

  values();
end method;

//**********************************************************************
//
// CPersistStorage::ReleaseStreamsAndStorage
//
// Purpose:
//
//      Releases the stream and storage ptrs
//
// Parameters:
//
//      None
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


define method OLE-part-Release-Streams-And-Storage(lpObj :: <CSimpSvrObj>)
 => ();

  unless ( null?( lpObj.m-lpDataStream ) )
    Release(lpObj.m-lpDataStream);
    lpObj.m-lpDataStream := null-pointer(<LPSTREAM>);
  end unless;

  unless ( null?( lpObj.m-lpStorage ) )
    Release(lpObj.m-lpStorage);
    lpObj.m-lpStorage := null-pointer(<LPSTORAGE>);
  end unless;

  values()
end method;
