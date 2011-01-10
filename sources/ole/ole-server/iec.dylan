Module:    OLE-Server
Synopsis:  Methods for class <CExternalConnection>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Called when another connection is made to the object.
// Returns the strong connection count.

define method IExternalConnection/AddConnection(this :: <CExternalConnection>,
			    extconn :: <unsigned-fixnum>, //type of connection
			    reserved /* :: <unsigned-fixnum> */)
		=> (count :: <unsigned-fixnum>);

  Output-Debug-String("IExternalConnection/AddConnection\r\n");

  if ( logand(extconn, $EXTCONN-STRONG) ~= 0 )
    this.strong-connection-count := this.strong-connection-count + 1
  else
    0
  end if
end method IExternalConnection/AddConnection;


// Called when a connection to the object is released.
// Returns the new reference count.

define method IExternalConnection/ReleaseConnection
		(this :: <CExternalConnection>,
		 extconn :: <unsigned-fixnum>, // type of connection
		 reserved /* :: <unsigned-fixnum> */,
		 last-release-closes? :: <boolean>)
 => (count :: <unsigned-fixnum>);

  Output-Debug-String("IExternalConnection/ReleaseConnection\r\n");

  if ( logand(extconn,$EXTCONN-STRONG) ~= 0 )
		
    let count :: <fixnum> =
      (this.strong-connection-count := this.strong-connection-count - 1);

    if ( zero?(count) & last-release-closes? )
      IOleObject/Close(this.get-obj.server-IOleObject, $OLECLOSE-SAVEIFDIRTY);
    end if;
    count 
  else
    0
  end if
end method IExternalConnection/ReleaseConnection;
