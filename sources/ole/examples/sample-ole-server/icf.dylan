Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//**********************************************************************
//
// CClassFactory::CreateInstance
//
// Purpose:
//
//      Instantiates a new OLE object
//
// Parameters:
//
//      LPUNKNOWN pUnkOuter     - Pointer to the controlling unknown
//
//      REFIID riid             - The interface type to fill in ppvObject
//
//      LPVOID FAR* ppvObject   - Out pointer for the object
//
// Return Value:
//
//      S_OK                    - Creation was successful
//      CLASS_E_NOAGGREGATION   - Tried to be created as part of an aggregate
//
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpSvrDoc::CreateObject   DOC.CPP
//
// Comments:
//
//
//********************************************************************


define method IClassFactory/CreateInstance(this :: <CClassFactory>,
			     UnkOuter :: <LPUNKNOWN>, riid :: <REFIID>)
		=> ( status :: <HRESULT>, Object :: <Interface> );

  OutputDebugString("In CClassFactory::CreateInstance\r\n");

  // we don't support aggregation...
  if ( ~ null?(UnkOuter) )
    values( $CLASS-E-NOAGGREGATION, $NULL-interface )
  else
    CreateObject(this.m-lpApp.m-lpDoc, riid);
  end if;
end method IClassFactory/CreateInstance;
