Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define COM-interface <CSimpSvrApp> ( <IUnknown> )
        
  slot m-hAppWnd :: <HWND>;             // main window handle
        
  slot m-hInst :: <HINSTANCE>;          // application instance
        
  slot m-fStartByOle :: <boolean>;         // TRUE if app started by OLE
        
  slot m-dwRegisterClass :: <integer>;    // returned by RegisterClassFactory

        
  slot m-hMainMenu :: <HMENU>;
        
  slot m-hColorMenu :: <HMENU>;
        
//  slot m-hHelpMenu :: <HMENU>;

        
  slot m-OleObject :: <LPOLEOBJECT>; // point to "dummy" object
        
  slot m-lpDoc :: <Interface> ; // null or <CSimpSvrDoc>; // pointer to document object
        
  slot m-fInitialized :: <boolean>;         // OLE initialization flag
        
 // slot nullRect :: <LPRECT>;               // used in inplace negotiation

end <CSimpSvrApp>;

define inline method GethAppWnd(this :: <CSimpSvrApp>) => value :: <HWND>; 
 /* return */   this.m-hAppWnd 
end method GethAppWnd;

define inline method GethInst(this :: <CSimpSvrApp>)
	=> value :: <HINSTANCE>; 
 /* return */   this.m-hInst 
end method GethInst;

define inline method IsStartedByOle(this :: <CSimpSvrApp>)
	=> value :: <boolean>; 
 /* return */   this.m-fStartByOle 
end method IsStartedByOle;

define inline method IsInitialized(this :: <CSimpSvrApp>)
	=> value :: <boolean>; 
 /* return */   this.m-fInitialized 
end method IsInitialized;

define inline method GetRegisterClass(this :: <CSimpSvrApp>)
	=> value :: <integer>; 
 /* return */   this.m-dwRegisterClass 
end method GetRegisterClass;

define inline method GetDoc(this :: <CSimpSvrApp>)
	=> value :: <CSimpSvrDoc>; 
 /* return */   this.m-lpDoc 
end method GetDoc;

define inline method ClearDoc(this :: <CSimpSvrApp>) => (); 
   this.m-lpDoc :=  $NULL-interface; 
   values();
end method ClearDoc;

define inline method GetOleObject(this :: <CSimpSvrApp>)
	=> value :: <Interface>; 
 /* return */   this.m-OleObject 
end method GetOleObject;
        
/*
define inline method GetMainMenu(this :: <CSimpSvrApp>) => value :: <HMENU>; 
 /* return */   this.m-hMainMenu 
end method GetMainMenu;
*/
define inline method GetColorMenu(this :: <CSimpSvrApp>)
	=> value :: <HMENU>; 
 /* return */   this.m-hColorMenu 
end method GetColorMenu;

/*
define inline method GetHelpMenu(this :: <CSimpSvrApp>) => value :: <HMENU>; 
 /* return */   this.m-hHelpMenu 
end method GetHelpMenu;
*/


define COM-interface <CSimpSvrDoc> ( <IUnknown> )
        
  slot m-lpApp :: <CSimpSvrApp>;
        
  slot m-lpObj :: false-or(<CSimpSvrObj>);
        
  slot m-hDocWnd :: <HWND>;
        
  slot m-hHatchWnd :: <HWND>;
        
  slot m-fClosing :: <boolean>;

end <CSimpSvrDoc>;

// member access

define inline method GethDocWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   this.m-hDocWnd 
end method GethDocWnd;

define inline method GethHatchWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   this.m-hHatchWnd 
end method GethHatchWnd;

define inline method GethAppWnd(this :: <CSimpSvrDoc>) => value :: <HWND>; 
 /* return */   GethAppWnd(this.m-lpApp) 
end method GethAppWnd;

define inline method GetApp(this :: <CSimpSvrDoc>)
	=> value :: <CSimpSvrApp>; 
 /* return */   this.m-lpApp 
end method GetApp;

define inline method GetObj(this :: <CSimpSvrDoc>)
	=> value :: false-or(<CSimpSvrObj>); 
 /* return */   this.m-lpObj 
end method GetObj;

define inline method ClearObj(this :: <CSimpSvrDoc>) => (); 
   this.m-lpObj := #f;
   values();
end method ClearObj;
