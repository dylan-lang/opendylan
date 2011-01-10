Module:    OLE-Container
Synopsis:  Implementation of the IAdviseSink interface, by which the container
	   receives notifications from embedded servers.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open primary COM-interface <container-advise-sink> ( <IAdviseSink> ) 
  constant slot get-site :: <contained-object>, required-init-keyword: site:;
end <container-advise-sink>;


define method IAdviseSink/OnDataChange(this :: <container-advise-sink>,
				       pFormatetc :: <FORMATETC>,
				       pStgmed :: <STGMEDIUM>) => ();
  OutputDebugString("IAdviseSink/OnDataChange\r\n");
  let doc = this.get-site;
  note-data-change(doc.document-application, doc, pFormatetc, pStgmed);
  values()
end method IAdviseSink/OnDataChange;

define open generic note-data-change(app, doc, format, medium);

define method note-data-change(app :: <container-app>,
			       doc :: <contained-object>,
			       pFormatetc /* :: <FORMATETC> */,
			       pStgmed /* :: <STGMEDIUM> */ ) => ();
  // default method does nothing.
  ignore(app, doc, pFormatetc, pStgmed);
end;



//**********************************************************************
//
// CAdviseSink::OnViewChange
//
// Purpose:
//
//      Notifies us that the view has changed and needs to be updated.
//
// Parameters:
//
//      DWORD dwAspect  - Aspect that has changed
//
//      LONG lindex     - Index that has changed
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      InvalidateRect              Windows API
//      IViewObject2::GetExtent     Object
//
// Comments:
//
//********************************************************************


define method IAdviseSink/OnViewChange(this :: <container-advise-sink>,
				       aspect :: <fixnum>,
				       lindex /* :: <signed-long> */) => ();

  OutputDebugString("IAdviseSink/OnViewChange\r\n");
  let doc = this.get-site;  
  if ( aspect = doc.document-draw-aspect )
    // get a pointer to IViewObject2
    let ( hErr :: <HRESULT>, interface ) =
      QueryInterface(doc.document-ole-object, $IID-IViewObject2);
    if ( SUCCEEDED?(hErr) ) 
      let view-object :: <LPVIEWOBJECT2> =
	pointer-cast(<LPVIEWOBJECT2>, interface);
      // get extent of the object
      // NOTE: this method will never be remoted; it can be called w/i
      // this async method
      IViewObject2/GetExtent(view-object, aspect, -1,
			     null-pointer(<LPDVTARGETDEVICE>),
			     doc.site-object-size);
      Release(view-object);
    end if;
  end if;
  note-view-change(doc.document-application, doc, aspect);
  values()
end method IAdviseSink/OnViewChange;

define open generic note-view-change(app, doc, aspect);

define method note-view-change(app :: <container-app>,
			       doc :: <contained-object>,
			       aspect :: <integer>) => ();
  if ( aspect = doc.document-draw-aspect )
    InvalidateRect(doc.document-container-window, $NULL-RECT, #t);
  end if;
end;


define method IAdviseSink/OnRename(this :: <container-advise-sink>,
				   pmk :: <LPMONIKER>)
 => ();
  OutputDebugString("IAdviseSink/OnRename\r\n");
  values()
end method IAdviseSink/OnRename;


define method IAdviseSink/OnSave(this :: <container-advise-sink>) => ();
  OutputDebugString("IAdviseSink/OnSave\r\n");
  let doc = this.get-site;
  note-document-save(doc.document-application, doc);
end method IAdviseSink/OnSave;

define open generic note-document-save(app, doc);

define method note-document-save(app :: <container-app>,
				 doc :: <contained-object>) => ();
  // default method does nothing.
  ignore(app, doc);
end;


define method IAdviseSink/OnClose(this :: <container-advise-sink>) => ();
  OutputDebugString("IAdviseSink/OnClose\r\n");
  let doc = this.get-site;
  note-document-close(doc.document-application, doc);
end method IAdviseSink/OnClose;

define open generic note-document-close(app, doc);

define method note-document-close (app :: <container-app>,
				   doc :: <contained-object>) => ();
  // default method does nothing.
  ignore(app, doc);
end;
