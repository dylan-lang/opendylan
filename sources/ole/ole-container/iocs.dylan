Module:    OLE-Container
Synopsis:  Implementation of IOleClientSite interface, which is called by the
	   server to request services from the container.  The container
	   provides one instance of IOleClientSite for every compound document
	   object it contains.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open primary COM-interface <container-ole-client-site>
	( <IOleClientSite> ) 
  constant slot get-site :: <contained-object>, required-init-keyword: site:;
end <container-ole-client-site>;


// Called by the object when it wants to be saved to persistent storage
define method IOleClientSite/SaveObject(this :: <container-ole-client-site>)
				=> status :: <HRESULT>;

  let sc :: <SCODE> = $E-FAIL;

  OutputDebugString( "IOleClientSite/SaveObject\r\n");

  // get a pointer to IPersistStorage
  let ( hErr :: <HRESULT>, interface :: <Interface> ) =
    QueryInterface(this.get-site.document-ole-object, $IID-IPersistStorage);
  if ( SUCCEEDED?(hErr) )
    let persist-storage :: <LPPERSISTSTORAGE> =
      pointer-cast(<LPPERSISTSTORAGE>, interface);
    // save the object
    sc := OleSave(persist-storage, this.get-site.document-sub-storage, #t);
    IPersistStorage/SaveCompleted(persist-storage, null-pointer(<LPSTORAGE>));
    Release(persist-storage);
  end if;
  sc 
end method IOleClientSite/SaveObject;

// This function is not implemented because we don't support linking.
define method IOleClientSite/GetMoniker (this :: <container-ole-client-site>,
					dwAssign :: <unsigned-fixnum>,
					dwWhichMoniker :: <unsigned-fixnum>)
	=> ( status :: <HRESULT>, pmk :: <LPMONIKER> );

  OutputDebugString( "IOleClientSite/GetMoniker\r\n");

  values( $E-NOTIMPL, $NULL-interface )
end method IOleClientSite/GetMoniker;

// not implemented (used for linking)
define method IOleClientSite/GetContainer(this :: <container-ole-client-site> )
	=> ( status :: <HRESULT>, pContainer :: <LPOLECONTAINER> );
	
  OutputDebugString( "IOleClientSite/GetContainer\r\n");
  values( $E-NOTIMPL, $NULL-interface )
end method IOleClientSite/GetContainer;

// not implemented (used for linking)
define method IOleClientSite/ShowObject(this :: <container-ole-client-site>)
 => status :: <HRESULT>;

  OutputDebugString( "IOleClientSite/ShowObject\r\n");
  $S-OK
end method IOleClientSite/ShowObject;

// Object calls this method when it is opening/closing non-InPlace Window
//
// Parameters:
//
//      BOOL fShow  - TRUE if Window is opening, FALSE if closing
define method IOleClientSite/OnShowWindow(this :: <container-ole-client-site>,
					  show? :: <boolean>) 
 => status :: <HRESULT>;

  OutputDebugString( "IOleClientSite/OnShowWindow\r\n");
  let site = this.get-site;
  site.document-open-out-of-place? := show?;
  let window = site.document-container-window;
  InvalidateRect(window, $NULL-RECT, #t);

  // if object window is closing, then bring container window to top
  if ( ~ show? ) 
    BringWindowToTop(window);
    SetFocus(window);
  end if;
  $S-OK 
end method IOleClientSite/OnShowWindow;

// this operation is not adequately specified to be of any use.
define method IOleClientSite/RequestNewObjectLayout
    (this :: <container-ole-client-site>) => (status :: <HRESULT>)
  $E-NOTIMPL 
end method IOleClientSite/RequestNewObjectLayout;
