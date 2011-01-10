Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define COM-interface <CClassFactory> ( <IClassFactory> )

  slot m-lpApp :: <CSimpSvrApp>, required-init-keyword: App: ;

end <CClassFactory>;


define COM-interface <CDataObject> ( <IDataObject> )

  slot m-lpObj :: <CSimpSvrObj>, required-init-keyword: SimpSvrObj: ;
	
end <CDataObject>;


define COM-interface <CExternalConnection> ( <IExternalConnection> )

  slot m-lpObj :: <CSimpSvrObj>,		// Ptr to object
    required-init-keyword: SimpSvrObj: ;
  
  slot m-dwStrong :: <integer>,		// Connection Count
    init-value: 0;

end <CExternalConnection>;


define COM-interface <COleInPlaceActiveObject> ( <IOleInPlaceActiveObject> )

  slot m-lpObj :: <CSimpSvrObj>,  // back ptr
    required-init-keyword: SimpSvrObj: ;

end <COleInPlaceActiveObject>;


define COM-interface <COleInPlaceObject> ( <IOleInPlaceObject> )

  slot m-lpObj :: <CSimpSvrObj>, required-init-keyword: SimpSvrObj: ;
	
end <COleInPlaceObject>;


define COM-interface <COleObject> ( <IOleObject> )

  slot m-lpObj :: <CSimpSvrObj>, required-init-keyword: SimpSvrObj: ;
	
  slot m-fOpen :: <boolean>, init-value: #f;

end <COleObject>;


define COM-interface <CPersistStorage> ( <IPersistStorage> )

  slot m-lpObj :: <CSimpSvrObj>, required-init-keyword: SimpSvrObj: ;
	
//  slot m-fSameAsLoad :: <boolean>;

end <CPersistStorage>;


define COM-interface <CSimpSvrObj> ( <IUnknown> )

  slot m-lpDoc :: <CSimpSvrDoc>;      // Back pointer
	
  slot m-fInPlaceActive :: <boolean>;	  // Used during InPlace Negotiation
  slot m-fInPlaceVisible :: <boolean>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-fUIActive :: <boolean>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-hmenuShared :: <HMENU>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-hOleMenu :: <HOLEMENU>;	  // "	"  "  "	  "   "	  "   "	  "

  //slot m_posRect :: <RECT>;		  // "	"  "  "	  "   "	  "   "	  "
  //  Note: <RECT> is a C-struct, so we need a pointer instead.
  slot m-pPosRect :: <LPRECT>;
	
  //slot m_FrameInfo :: <OLEINPLACEFRAMEINFO>;
  //  Note: <OLEINPLACEFRAMEINFO> is a C-struct, so we need a pointer instead.
  slot m-pFrameInfo :: <LPOLEINPLACEFRAMEINFO>;
	
  slot m-fSaveWithSameAsLoad :: <boolean>;
	
  slot m-fNoScribbleMode :: <boolean>;
	
  slot m-dwRegister :: <integer>;   // Registered in ROT

  slot m-red :: <integer>;	 // current color
  slot m-green :: <integer>;
  slot m-blue :: <integer>;

  //slot m_size :: <POINT>;
  //  Note: <POINT> is a C-struct, so we need a pointer instead.
  slot m-pSize :: <LPPOINT>;		    // current size
	
  slot m-xOffset :: <integer>;
  slot m-yOffset :: <integer>;
	
  slot m-scale :: <real>;
	
  slot m-hWndParent :: <HWND>;		    // parent window handle

  // interfaces used
	
  slot m-lpStorage :: <LPSTORAGE>;

  slot m-lpColorStm :: <LPSTREAM>;
  slot m-lpSizeStm  :: <LPSTREAM>;
	
  slot m-lpOleClientSite :: <Interface>;      // null or IOleClientSite
	
  slot m-lpOleAdviseHolder :: <Interface>;    // null or IOleAdviseHolder
	
  slot m-lpDataAdviseHolder :: <Interface>;   // null or IDataAdviseHolder
	
  slot m-lpFrame :: <Interface>;	      // null or IOleInPlaceFrame
	
  slot m-lpCntrDoc :: <Interface>;	      // null or IOleInPlaceUIWindow
	
  slot m-lpIPSite :: <Interface>;	      // null or IOleInPlaceSite

  // interface implemented
	
  slot m-OleObject :: <COleObject>;			// IOleObject
	
  slot m-PersistStorage :: <CPersistStorage>;		// IPersistStorage
	
  slot m-DataObject :: <CDataObject>;			// IDataObject
	
  slot m-OleInPlaceActiveObject :: <COleInPlaceActiveObject>;	// IOleInPlaceActiveObject
	
  slot m-OleInPlaceObject :: <COleInPlaceObject>;	// IOleInPlaceObject
	
//  slot m-ExternalConnection :: <CExternalConnection>;

end <CSimpSvrObj>;



define method IsInPlaceActive(this :: <CSimpSvrObj>)
	=> value :: <boolean>; 
 /* return */	this.m-fInPlaceActive 
end method IsInPlaceActive;

/*
define method IsInPlaceVisible(this :: <CSimpSvrObj>)
	=> value :: <boolean>; 
 /* return */	this.m-fInPlaceVisible 
end method IsInPlaceVisible;
*/
/*
define method IsUIActive(this :: <CSimpSvrObj>)
	=> value :: <boolean>; 
 /* return */	this.m-fUIActive 
end method IsUIActive;
*/
/*
define method GetStorage(this :: <CSimpSvrObj>) => storage :: <LPSTORAGE>; 
 /* return */	this.m-lpStorage 
end method GetStorage;
*/
define method GetOleClientSite(this :: <CSimpSvrObj>)
 => site :: <LPOLECLIENTSITE>; 
 /* return */	this.m-lpOleClientSite 
end method GetOleClientSite;

define method GetDataAdviseHolder(this :: <CSimpSvrObj>)
				=> holder :: <LPDATAADVISEHOLDER>; 
 /* return */	this.m-lpDataAdviseHolder 
end method GetDataAdviseHolder;

define method GetOleAdviseHolder(this :: <CSimpSvrObj>)
				=> holder :: <LPOLEADVISEHOLDER>; 
 /* return */	this.m-lpOleAdviseHolder 
end method GetOleAdviseHolder;

define method GetInPlaceFrame(this :: <CSimpSvrObj>)
 => frame :: <LPOLEINPLACEFRAME>; 
 /* return */	this.m-lpFrame 
end method GetInPlaceFrame;

define method GetUIWindow(this :: <CSimpSvrObj>)
 => window :: <LPOLEINPLACEUIWINDOW>; 
 /* return */	this.m-lpCntrDoc 
end method GetUIWindow;

/*
define method GetInPlaceSite(this :: <CSimpSvrObj>)
 => site :: <LPOLEINPLACESITE>; 
 /* return */	this.m-lpIPSite 
end method GetInPlaceSite;
*/
define method GetOleObject(this :: <CSimpSvrObj>)
	=> value :: <COleObject>; 
 /* return */ this.m-OleObject 
end method GetOleObject;

define method GetPersistStorage(this :: <CSimpSvrObj>)
	=> value :: <CPersistStorage>; 
 /* return */ this.m-PersistStorage 
end method GetPersistStorage;

define method GetDataObject(this :: <CSimpSvrObj>)
	=> value :: <CDataObject>; 
 /* return */ this.m-DataObject 
end method GetDataObject;

/*
define method GetOleInPlaceActiveObject(this :: <CSimpSvrObj>)
	=> value :: <COleInPlaceActiveObject>; 
 /* return */  this.m-OleInPlaceActiveObject 
end method GetOleInPlaceActiveObject;
*/
define method GetOleInPlaceObject(this :: <CSimpSvrObj>)
	=> value :: <COleInPlaceObject>; 
 /* return */  this.m-OleInPlaceObject 
end method GetOleInPlaceObject;

define method ClearOleClientSite(this :: <CSimpSvrObj>) => (); 
   this.m-lpOleClientSite := $NULL-Interface; 
   values();
end method ClearOleClientSite;

define method ClearDataAdviseHolder(this :: <CSimpSvrObj>) => (); 
   this.m-lpDataAdviseHolder := $NULL-Interface; 
   values();
end method ClearDataAdviseHolder;

define method ClearOleAdviseHolder(this :: <CSimpSvrObj>) => (); 
   this.m-lpOleAdviseHolder := $NULL-Interface; 
   values();
end method ClearOleAdviseHolder;

/*
define method GetPosRect(this :: <CSimpSvrObj>) => value :: <LPRECT>; 
 /* return */ this.m-pPosRect 
end method GetPosRect;
*/
/*
define method GetSize(this :: <CSimpSvrObj>) => value :: <LPPOINT>; 
 /* return */   this.m-pSize 
end method GetSize;
*/
define method GetFrameInfo(this :: <CSimpSvrObj>)
	=> value :: <LPOLEINPLACEFRAMEINFO>; 
 /* return */  this.m-pFrameInfo 
end method GetFrameInfo;

define method GetRotRegister(this :: <CSimpSvrObj>)
	=> value :: <integer>; 
 /* return */	this.m-dwRegister 
end method GetRotRegister;

define method SetColor(this :: <CSimpSvrObj>,
		       nRed :: <integer>, nGreen :: <integer>, nBlue :: <integer>)
		=> (); 
  this.m-red   := nRed;
  this.m-green := nGreen;
  this.m-blue  := nBlue;
  values();
end method SetColor;

define method RotateColor(this :: <CSimpSvrObj>) => (); 
  this.m-red   := this.m-red   + 10;
  this.m-green := this.m-green + 10;
  this.m-blue  := this.m-blue  + 10;
  values();
end method RotateColor;

