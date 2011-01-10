Module:    OLE-Server
Synopsis:  Definitions of OLE interface classes.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//==========	OLE interface classes    ===========

define COM-interface <CDataObject> ( <IDataObject> )

  constant slot get-obj :: <basic-ole-server>,
		required-init-keyword: server-framework: ;
end <CDataObject>;


define COM-interface <COleInPlaceActiveObject> ( <IOleInPlaceActiveObject> )

  constant slot get-obj :: <basic-ole-server>,  // back ptr
    required-init-keyword: server-framework: ;
end <COleInPlaceActiveObject>;


define COM-interface <COleInPlaceObject> ( <IOleInPlaceObject> )

  constant slot get-obj :: <basic-ole-server>,
		required-init-keyword: server-framework: ;
end <COleInPlaceObject>;


define open COM-interface <COleObject> ( <IOleObject> )

  constant slot get-obj :: <basic-ole-server>,
		required-init-keyword: server-framework: ;

  // For out-of-place activation, this slot holds the window handle;
  // false for in-place activation.
  slot open-as-separate-window :: false-or(<HWND>), init-value: #f;

  slot container-application-name, init-value: #f;
  slot container-document-name, init-value: #f;

end <COleObject>;


define COM-interface <CPersistStorage> ( <IPersistStorage> )

  constant slot get-obj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;
  slot container-IStorage	:: <LPSTORAGE> = null-pointer(<LPSTORAGE>);
  slot size-storage-stream 	:: <LPSTREAM> = $null-istream;
  slot no-scribble-mode?	:: <boolean> = #f;
  slot save-with-same-as-load?	:: <boolean> = #f;
end <CPersistStorage>;


define COM-interface <CExternalConnection> ( <IExternalConnection> )

  constant slot get-obj :: <basic-ole-server>,
    required-init-keyword: server-framework: ;
  
  slot strong-connection-count :: <unsigned-fixnum>, init-value: 0;

end <CExternalConnection>;

