Module:    sample-OLE-container
Synopsis:  class definitions for OLE container application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define COM-interface <CSimpleApp> ( <IUnknown> /* , <CSafeRefCount> */  ) 

/* public: */ 
	
  slot m-hAppWnd :: <HWND>;         // main window handle
	
  slot m-hInst :: <HINSTANCE>;          // application instance
	
  slot m-hMainMenu :: <HMENU>;
	
  slot m-hFileMenu :: <HMENU>;
	
  slot m-hEditMenu :: <HMENU>;
	
  slot m-hHelpMenu :: <HMENU>;
	
  slot m-hCascadeMenu :: <HMENU>;

  slot m-OleInPlaceFrame :: <COleInPlaceFrame>; // IOleInPlaceFrame Implementation

	
  slot m-lpDoc :: false-or(<CSimpleDoc>);   // pointer to document object
	
  slot m-fInitialized :: <boolean>;        // OLE initialization flag
	
  slot m-fCSHMode :: <boolean>;
	
  slot m-fMenuMode :: <boolean>;
	
  slot m-hwndUIActiveObj :: <HWND>; // HWND of UIActive Object
	
  slot m-hStdPal :: <HPALETTE>;     // Color palette used by container
	
  slot m-fAppActive :: <boolean>;      // TRUE if app is active

  slot nullRect :: <LPRECT>;
end <CSimpleApp>;


define COM-interface <CSimpleDoc> ( <IUnknown> /* , <CSafeRefCount> */  ) 

/* public: */ 
	
  slot m-lpStorage :: <LPSTORAGE>;
	
  slot m-lpActiveObject :: <LPOLEINPLACEACTIVEOBJECT>;
	
  slot m-fInPlaceActive :: <boolean>;
	
  // slot m-fAddMyUI :: <boolean>;
	
  slot m-fModifiedMenu :: <boolean>;

	
  slot m-lpSite :: false-or(<CSimpleSite>);
	
  slot m-lpApp :: <CSimpleApp>;

	
  slot m-hDocWnd :: <HWND>;

end <CSimpleDoc>;

define COM-interface <CAdviseSink> ( <IAdviseSink> ) 

  // slot m-nCount :: <integer>, init-value: 0;
	
  constant slot m-pSite :: <CSimpleSite>, init-keyword: pSite:;

end <CAdviseSink>;

define COM-interface <COleClientSite> ( <IOleClientSite> ) 

  // slot m-nCount :: <integer>, init-value: 0;
	
  constant slot m-pSite :: <CSimpleSite>, init-keyword: pSite:;

end <COleClientSite>;

define COM-interface <COleInPlaceFrame> ( <IOleInPlaceFrame> ) 

  // slot m-nCount :: <integer>, init-value: 0;
	
  constant slot m-pApp :: <CSimpleApp>, init-keyword: App:;

end <COleInPlaceFrame>;

define COM-interface <COleInPlaceSite> ( <IOleInPlaceSite> ) 
	
  // slot m-nCount :: <integer>, init-value: 0;
	
  constant slot m-pSite :: <CSimpleSite>, init-keyword: pSite:;

end <COleInPlaceSite>;


define constant $IDM-ABOUT = 100;

// define constant $IDM-INSERT = 101;

define constant $IDM-VERB0 = 1000;


define COM-interface <CSimpleSite> ( <IUnknown> /* , <CSafeRefCount> */  ) 

/* public: */ 
  // slot m-dwConnection :: <integer>;
  slot m-lpOleObject :: <LPOLEOBJECT>;
  slot m-lpInPlaceObject :: <LPOLEINPLACEOBJECT>;
  slot m-hwndIPObj :: <HWND>;
  slot m-dwDrawAspect :: <integer>;
  slot m-pSizel :: <LPSIZEL>;  // was:  SIZEL m_sizel;
  slot m-fInPlaceActive :: <boolean>;
  slot m-fObjectOpen :: <boolean>;
  slot m-lpObjStorage :: <LPSTORAGE>;
	
  slot m-AdviseSink :: <CAdviseSink>;	
//  slot m-OleInPlaceSite :: <COleInPlaceSite>;
  slot m-OleClientSite :: <COleClientSite>;
	
  slot m-lpDoc :: <CSimpleDoc>;

end <CSimpleSite>;

