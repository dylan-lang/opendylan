Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// CAdviseSink::OnDataChange
//
// Purpose:
//
//      Not Implemented (needs to be stubbed out)
//
// Parameters:
//
//      Not Implemented (needs to be stubbed out)
//
// Return Value:
//
//      Not Implemented (needs to be stubbed out)
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented (needs to be stubbed out)
//
//********************************************************************


define method IAdviseSink/OnDataChange(this :: <CAdviseSink>,
				       pFormatetc :: <FORMATETC>,
				       pStgmed :: <STGMEDIUM>) => ();
  OutputDebugString("In IAS::OnDataChange\r\n");
  values()
end method IAdviseSink/OnDataChange;

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


define method IAdviseSink/OnViewChange(this :: <CAdviseSink>,
				       dwAspect /* :: <integer> */,
				       lindex /* :: <signed-long> */) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);

  OutputDebugString("In IAS::OnViewChange\r\n");

  // get a pointer to IViewObject2
  let ( hErr :: <HRESULT>, interface ) =
    QueryInterface(this.m-pSite.m-lpOleObject, $IID-IViewObject2);
  if ( hErr = $NOERROR ) 
    let lpViewObject2 :: <LPVIEWOBJECT2> =
      pointer-cast(<LPVIEWOBJECT2>, interface);
    // get extent of the object
    // NOTE: this method will never be remoted; it can be called w/i
    // this async method
    IViewObject2/GetExtent(lpViewObject2, $DVASPECT-CONTENT, -1,
			   null-pointer(<LPDVTARGETDEVICE>),
			   this.m-pSite.m-pSizel);
    Release(lpViewObject2);
  end if;
  
  InvalidateRect(this.m-pSite.m-lpDoc.m-hDocWnd, $NULL-RECT, #t);
  values()
end method IAdviseSink/OnViewChange;

//**********************************************************************
//
// CAdviseSink::OnRename
//
// Purpose:
//
//      Not Implemented (needs to be stubbed out)
//
// Parameters:
//
//      Not Implemented (needs to be stubbed out)
//
// Return Value:
//
//      Not Implemented (needs to be stubbed out)
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented (needs to be stubbed out)
//
//********************************************************************


define method IAdviseSink/OnRename(this :: <CAdviseSink>, pmk :: <LPMONIKER>)
 => ();
  OutputDebugString("In IAS::OnRename\r\n");
  values()
end method IAdviseSink/OnRename;

//**********************************************************************
//
// CAdviseSink::OnSave
//
// Purpose:
//
//      Not Implemented (needs to be stubbed out)
//
// Parameters:
//
//      Not Implemented (needs to be stubbed out)
//
// Return Value:
//
//      Not Implemented (needs to be stubbed out)
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented (needs to be stubbed out)
//
//********************************************************************


define method IAdviseSink/OnSave(this :: <CAdviseSink>) => ();
  OutputDebugString("In IAS::OnSave\r\n");
  values()
end method IAdviseSink/OnSave;

//**********************************************************************
//
// CAdviseSink::OnClose
//
// Purpose:
//
//      Not Implemented (needs to be stubbed out)
//
// Parameters:
//
//      Not Implemented (needs to be stubbed out)
//
// Return Value:
//
//      Not Implemented (needs to be stubbed out)
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented (needs to be stubbed out)
//
//********************************************************************


define method IAdviseSink/OnClose(this :: <CAdviseSink>) => ();
  OutputDebugString("In IAS::OnClose\r\n");
  values()
end method IAdviseSink/OnClose;
