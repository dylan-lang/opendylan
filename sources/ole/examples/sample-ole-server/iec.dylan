Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// CExternalConnection::AddConnection
//
// Purpose:
//
//      Called when another connection is made to the object.
//
// Parameters:
//
//      DWORD extconn   -   Type of connection
//
//      DWORD reserved  -   Reserved
//
// Return Value:
//
//      Strong connection count
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//
//********************************************************************


define method IExternalConnection/AddConnection(this :: <CExternalConnection>,
			    extconn :: <integer>,
			    reserved :: <integer>)
		=> count :: <integer>;

  OutputDebugString("In CExternalConnection::AddConnection\r\n");

  if ( logand(extconn, $EXTCONN-STRONG) ~= 0 )
    this.m-dwStrong := this.m-dwStrong + 1
  else
    0
  end if

end method IExternalConnection/AddConnection;

//**********************************************************************
//
// CExternalConnection::ReleaseConnection
//
// Purpose:
//
//      Called when a connection to the object is released.
//
// Parameters:
//
//      DWORD extconn               - Type of Connection
//
//      DWORD reserved              - Reserved
//
//      BOOL fLastReleaseCloses     - Close flag
//
// Return Value:
//
//      The new reference count
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      COleObject::Close           IOO.CPP
//
// Comments:
//
//
//********************************************************************


define method IExternalConnection/ReleaseConnection
		(this :: <CExternalConnection>,
		 extconn :: <integer>,
		 reserved :: <integer>,
		 fLastReleaseCloses :: <boolean>)
		=> count :: <integer>;

  OutputDebugString("In CExternalConnection::ReleaseConnection\r\n");

  if ( logand(extconn,$EXTCONN-STRONG) ~= 0 )
		
    let dwSave :: <integer> = (this.m-dwStrong := this.m-dwStrong - 1);

    if ( zero?(dwSave) & fLastReleaseCloses )
      IOleObject/Close(this.m-lpObj.m-OleObject, $OLECLOSE-SAVEIFDIRTY);
    end if;
    dwSave 
  else
    0
  end if
end method IExternalConnection/ReleaseConnection;
