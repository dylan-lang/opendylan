Module:    OLE-Server-Framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
// File name: app.h
//
//      Definition of CSimpSvrApp
//
//**********************************************************************


define COM-interface <CSimpSvrApp> ( <IUnknown> )

  slot app-frame :: duim/<frame>;
        
  slot m-hAppWnd :: <HWND>;             // main window handle
        
  slot m-hInst :: <HINSTANCE>;          // application instance
        
  slot m-fStartByOle :: <boolean>;         // TRUE if app started by OLE

  slot app-class-id :: <REFGUID>, init-keyword: id:;
        
  slot m-dwRegisterClass :: <unsigned-fixnum>;    // returned by RegisterClassFactory

        
  slot m-OleObject :: <LPOLEOBJECT>; // point to "dummy" object
        
  slot m-lpDoc :: <Interface> ; // null or <CSimpSvrDoc>; // pointer to document object
        
  slot m-fInitialized :: <boolean>;         // OLE initialization flag
  slot m-fOleStdInit :: <boolean>;          // OLE support library init flag
        
  slot nullRect :: <LPRECT>;               // used in inplace negotiation

end class <CSimpSvrApp>;

define method GethAppWnd(this :: <CSimpSvrApp>) => value :: <HWND>; 
 /* return */   this.m-hAppWnd 
end method GethAppWnd;

define method GethInst(this :: <CSimpSvrApp>)
	=> value :: <HINSTANCE>; 
 /* return */   this.m-hInst 
end method GethInst;

define method IsStartedByOle(this :: <CSimpSvrApp>)
	=> value :: <boolean>; 
 /* return */   this.m-fStartByOle 
end method IsStartedByOle;

define method IsInitialized(this :: <CSimpSvrApp>)
	=> value :: <boolean>; 
 /* return */   this.m-fInitialized 
end method IsInitialized;

define method GetRegisterClass(this :: <CSimpSvrApp>)
	=> value :: <unsigned-fixnum>; 
 /* return */   this.m-dwRegisterClass 
end method GetRegisterClass;

define method GetDoc(this :: <CSimpSvrApp>)
	=> value :: <CSimpSvrDoc>; 
 /* return */   this.m-lpDoc 
end method GetDoc;

define method ClearDoc(this :: <CSimpSvrApp>) => (); 
   this.m-lpDoc :=  $NULL-interface; 
   values();
end method ClearDoc;

define method GetOleObject(this :: <CSimpSvrApp>)
	=> value :: <Interface>; 
 /* return */   this.m-OleObject 
end method GetOleObject;


// "declapp.dyl" generated Jul 26, 1995 by "gray"
// Converted from "/u/gray/ole/samples/simpsvr/doc.h" dated Jun 2, 1995
//**********************************************************************
// File name: doc.h
//
//      Definition of CSimpSvrDoc
//
//**********************************************************************


define COM-interface <CSimpSvrDoc> ( <IUnknown> )
        
  slot m-lpApp :: <CSimpSvrApp>;
        
  slot m-lpObj ; // ?bug?  :: false-or(<CSimpSvrObj>);

        
  slot m-hDocWnd :: <HWND>;
        
  slot m-hHatchWnd :: <HWND>;

  slot m-doc-sheet;
        
end class <CSimpSvrDoc>;

// member access

define method GethDocWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   this.m-hDocWnd 
end method GethDocWnd;

define method GethHatchWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   this.m-hHatchWnd 
end method GethHatchWnd;

define method GethAppWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   GethAppWnd(this.m-lpApp) 
end method GethAppWnd;

define method GetApp(this :: <CSimpSvrDoc>)
	=> value :: <CSimpSvrApp>; 
 /* return */   this.m-lpApp 
end method GetApp;

define method GetObj(this :: <CSimpSvrDoc>)
	=> value :: <CSimpSvrObj>; 
 /* return */   this.m-lpObj 
end method GetObj;

define method ClearObj(this :: <CSimpSvrDoc>) => (); 
   this.m-lpObj :=  $NULL-interface; 
   values();
end method ClearObj;

