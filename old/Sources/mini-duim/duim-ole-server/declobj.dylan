Module:    OLE-Server-Framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For "int", allow more than 16 bits, but still fit in a fixnum:
//	(Note: can't use names <int> or <long> here because those are
//		exported by Win32-Common with a different meaning.)
define constant <fixnum> = limited(<integer>, min: -#x800000, max: #x7FFFFF);
define constant <signed-long> = limited(<integer>, min: -#x80000000,
					max: #x7FFFFFFF);

define constant <unsigned-fixnum> = limited(<integer>, min: 0, max: #x7FFFFF);

//---- Change this to #f for the final version:		???
define constant $enable-debug-messages = #t;

// Display message in debugger (if any):
define method Output-Debug-String ( string :: <string> ) => ();
  if( $enable-debug-messages )
    // TEXT caches the converted string to avoid memory leak.
    OutputDebugString( TEXT(string) );
  end if;
  values()
end method;


//**********************************************************************
// File name: ido.h
//
//	Definition of CDataObject
//
//**********************************************************************

define COM-interface <CDataObject> ( <IDataObject> )

  slot m-lpObj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;
	
end <CDataObject>;


//**********************************************************************
// File name: IOIPAO.H
//
//	Definition of COleInPlaceActiveObject
//
//**********************************************************************

define COM-interface <COleInPlaceActiveObject> ( <IOleInPlaceActiveObject> )

  slot m-lpObj :: <ole-server-framework>,  // back ptr
    required-init-keyword: server-framework: ;

end <COleInPlaceActiveObject>;


//**********************************************************************
// File name: ioipo.h
//
//	Definition of COleInPlaceObject
//
//**********************************************************************

define COM-interface <COleInPlaceObject> ( <IOleInPlaceObject> )

  slot m-lpObj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;
	
end <COleInPlaceObject>;


//**********************************************************************
// File name: ioo.h
//
//	Definition of COleObject
//
//**********************************************************************

define COM-interface <COleObject> ( <IOleObject> )

  slot m-lpObj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;

  // For out-of-place activation, this slot holds the window handle;
  // false for in-place activation.
  slot open-as-separate-window /* :: false-or(<HWND>) */, init-value: #f;

end <COleObject>;


//**********************************************************************
// File name: ips.h
//
//	Definition of CPersistStorage
//
//**********************************************************************

define COM-interface <CPersistStorage> ( <IPersistStorage> )

  slot m-lpObj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;
	
  slot m-fSameAsLoad :: <boolean>;

end <CPersistStorage>;


//**********************************************************************
// File name: iec.h
//
//	Definition of CExternalConnection
//
//**********************************************************************

define COM-interface <CExternalConnection> ( <IExternalConnection> )

  slot m-lpObj :: <ole-server-framework>,		// Ptr to object
    required-init-keyword: server-framework: ;
  
  slot m-dwStrong :: <unsigned-fixnum>,		// Connection Count
    init-value: 0;

end <CExternalConnection>;

//**********************************************************************
// File name: obj.h
//
//	Definition of CSimpSvrObj
//
//**********************************************************************

define COM-interface <ole-server-framework> ( <IUnknown> )

  slot m-fInPlaceActive :: <boolean>;	  // Used during InPlace Negotiation
  slot m-fInPlaceVisible :: <boolean>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-fUIActive :: <boolean>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-hmenuShared :: <HMENU>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-hOleMenu :: <HOLEMENU>;	  // "	"  "  "	  "   "	  "   "	  "
  slot m-pPosRect :: <LPRECT>;	  	  // "	"  "  "	  "   "	  "   "	  "
	
  slot m-pFrameInfo :: <LPOLEINPLACEFRAMEINFO>;
	
  slot m-fSaveWithSameAsLoad :: <boolean>;
	
  slot m-fNoScribbleMode :: <boolean>;
	
  slot m-dwRegister :: <unsigned-fixnum>;   // Registered in ROT

  slot m-xOffset :: <fixnum>;
  slot m-yOffset :: <fixnum>;
	
  slot m-scale :: <real>;
	
  slot m-hWndParent :: <HWND>;		    // parent window handle

  // interfaces used
	
  slot m-lpStorage :: <LPSTORAGE>;

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
	
  slot m-ExternalConnection :: <CExternalConnection>;

  slot m-fClosing :: <boolean>;

end <ole-server-framework>;



define method IsInPlaceActive(this :: <ole-server-framework>)
	=> value :: <boolean>; 
 /* return */	this.m-fInPlaceActive 
end method IsInPlaceActive;

define method IsInPlaceVisible(this :: <ole-server-framework>)
	=> value :: <boolean>; 
 /* return */	this.m-fInPlaceVisible 
end method IsInPlaceVisible;

define method IsUIActive(this :: <ole-server-framework>)
	=> value :: <boolean>; 
 /* return */	this.m-fUIActive 
end method IsUIActive;

define method GetStorage(this :: <ole-server-framework>) => <LPSTORAGE>; 
 /* return */	this.m-lpStorage 
end method GetStorage;

define method GetOleClientSite(this :: <ole-server-framework>) => <LPOLECLIENTSITE>; 
 /* return */	this.m-lpOleClientSite 
end method GetOleClientSite;

define method GetDataAdviseHolder(this :: <ole-server-framework>)
				=> <LPDATAADVISEHOLDER>; 
 /* return */	this.m-lpDataAdviseHolder 
end method GetDataAdviseHolder;

define method GetOleAdviseHolder(this :: <ole-server-framework>)
				=> <LPOLEADVISEHOLDER>; 
 /* return */	this.m-lpOleAdviseHolder 
end method GetOleAdviseHolder;

define method GetInPlaceFrame(this :: <ole-server-framework>) => <LPOLEINPLACEFRAME>; 
 /* return */	this.m-lpFrame 
end method GetInPlaceFrame;

define method GetUIWindow(this :: <ole-server-framework>) => <LPOLEINPLACEUIWINDOW>; 
 /* return */	this.m-lpCntrDoc 
end method GetUIWindow;

define method GetInPlaceSite(this :: <ole-server-framework>) => <LPOLEINPLACESITE>; 
 /* return */	this.m-lpIPSite 
end method GetInPlaceSite;

define method GetOleObject(this :: <ole-server-framework>)
	=> value :: <COleObject>; 
 /* return */ this.m-OleObject 
end method GetOleObject;

define method GetPersistStorage(this :: <ole-server-framework>)
	=> value :: <CPersistStorage>; 
 /* return */ this.m-PersistStorage 
end method GetPersistStorage;

define method GetDataObject(this :: <ole-server-framework>)
	=> value :: <CDataObject>; 
 /* return */ this.m-DataObject 
end method GetDataObject;

define method GetOleInPlaceActiveObject(this :: <ole-server-framework>)
	=> value :: <COleInPlaceActiveObject>; 
 /* return */  this.m-OleInPlaceActiveObject 
end method GetOleInPlaceActiveObject;

define method GetOleInPlaceObject(this :: <ole-server-framework>)
	=> value :: <COleInPlaceObject>; 
 /* return */  this.m-OleInPlaceObject 
end method GetOleInPlaceObject;

define method ClearOleClientSite(this :: <ole-server-framework>) => (); 
   this.m-lpOleClientSite := $NULL-Interface; 
   values();
end method ClearOleClientSite;

define method ClearDataAdviseHolder(this :: <ole-server-framework>) => (); 
   this.m-lpDataAdviseHolder := $NULL-Interface; 
   values();
end method ClearDataAdviseHolder;

define method ClearOleAdviseHolder(this :: <ole-server-framework>) => (); 
   this.m-lpOleAdviseHolder := $NULL-Interface; 
   values();
end method ClearOleAdviseHolder;

define method GetFrameInfo(this :: <ole-server-framework>)
	=> value :: <LPOLEINPLACEFRAMEINFO>; 
 /* return */  this.m-pFrameInfo 
end method GetFrameInfo;

define method GetRotRegister(this :: <ole-server-framework>)
	=> value :: <unsigned-fixnum>; 
 /* return */	this.m-dwRegister 
end method GetRotRegister;


/// just for convenience:
define constant $NULL-HDC :: <HDC> = null-pointer(<HDC>);
