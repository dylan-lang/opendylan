module:     remote-access-path
synopsis:   Implementation of debugger connections
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <REMOTE-DEBUGGER-CONNECTION-IMPLEMENTATION>
//    A debugger connection to a process known to be running on a
//    different machine than the development environment.

define class <remote-debugger-connection-implementation> (<remote-debugger-connection>)
  slot connection-server :: Rtmgr/<NubServer>;
  slot connection-orb;
  slot nub :: Rtmgr/<RemoteNub>;
end class;

define sideways method make (class == <remote-debugger-connection>,
			     #rest args, #key, #all-keys) => (connection)
  apply(make, <remote-debugger-connection-implementation>, args)
end method;


// Some hard-coded constants for connecting to the Remote Debugger
// Debugger Server

define constant $DebuggerServerId = "Functional Developer Debugger Server";

define constant $DebuggerServerPOA = "DebuggerServerPOA";

define constant $RootPOA = "RootPOA";

define constant $RootPOAs =
  as(limited(<vector>, of: <string>),
     vector($RootPOA, $DebuggerServerPOA));

define constant $DebuggerServerPort = 7777;

define constant $RepositoryID = "IDL:functionalobjects.com/Rtmgr/NubServer:1.0";


define method initialize
    (connection :: <remote-debugger-connection-implementation>, #key, #all-keys) => ()

  next-method();

  let orb = CORBA/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let location =
    make(IIOP/<location>,
	 repository-id: $RepositoryID,
	 host: connection.connection-network-address,
	 port: $DebuggerServerPort,
	 adaptor-path: $RootPOAs,
	 objectid: $DebuggerServerId);
  let server =
    as(Rtmgr/<NubServer>,
       CORBA/ORB/create-reference(orb, location));

  let password-ok = 
    block ()
      Rtmgr/NubServer/verify-local-password
	(server,
	 connection.connection-password,
	 $local-hostname);
    exception (condition :: CORBA/<exception>)
      error(make(<open-debugger-connection-failure>,
		 network-address: connection.connection-network-address));
    end block;

  if (password-ok == 1)
    let hostname = Rtmgr/NubServer/get-local-hostname(server);

    connection.connection-orb := orb;
    connection.connection-server := server;
    connection.connection-hostname := hostname;
    connection.connection-open? := #t;
    ignore(connection.connection-open?);

    add!(*open-debugger-connections*, connection);
  else
    close-remote-debugger-connection(connection);
    error(make(<open-debugger-connection-password-mismatch>,
	       wrong-password: connection.connection-password));
  end if;
end method;


define class <remote-access-connection> (<access-connection>)
  slot connection-server :: Rtmgr/<NubServer>,
    init-keyword: server:;
  slot connection-orb,
    init-keyword: orb:;
  slot nub :: Rtmgr/<RemoteNub>;
end class;


///// START-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <application-access-path> and calls the server function to create 
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define method start-application-on-connection
  (conn :: <remote-access-connection>,
   command :: <string>, 
   arguments :: <string>,
   symbol-file-directories :: <sequence>,
   working-directory :: false-or(<string>),
   library-search-paths :: <sequence>,
   #key own-shell? = #t) => ()

  let create-shell =
    if (own-shell?)
      1
    else
      0
    end if;

  conn.nub :=
    block ()
      Rtmgr/NubServer/CreateNub
	(conn.connection-server,
	 command,
	 $local-hostname);
    exception (condition :: CORBA/<exception>)
      signal(make(<access-path-creation-error>));
    end block;

  let access-path = access-path-server();

  Rtmgr/RemoteNub/OpenNub
    (conn.nub, access-path.AccessPath-reference);

  let (process, success) =
    Rtmgr/RemoteNub/open-local-tether
    (conn.nub,
     command, 
     arguments, 
     as(<STRING-SEQ>, symbol-file-directories),
     as(<STRING-SEQ>, library-search-paths),
     working-directory | "",
     create-shell);

  if (success == 0)
    signal(make(<access-path-creation-error>));
  else
    conn.connection-process := as-remote-pointer(process);
    add!(conn.access-debugger-connection.connection-open-tethers, conn);
  end if;
end method;


///// ATTACH-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <process-access-path> and calls the server function to attach to
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define method attach-application-on-connection
  (conn :: <remote-access-connection>,
   process :: <remote-process>,
   symbol-file-directories :: <sequence>,
   system-info :: <string>) => ()

  conn.nub :=
    block ()
      Rtmgr/NubServer/CreateNub
	(conn.connection-server,
	 process.remote-process-name,
	 $local-hostname);
    exception (condition :: CORBA/<exception>)
      signal(make(<access-path-creation-error>));
    end block;

  let access-path = access-path-server();

  Rtmgr/RemoteNub/OpenNub
    (conn.nub, access-path.AccessPath-reference);

  let (process, success) =
    Rtmgr/RemoteNub/attach-local-tether
    (conn.nub,
     as-integer(process.nub-descriptor),  
     process.remote-process-name,
     process.remote-process-system-identifier,
     process.remote-process-actual-identifier,
     as(<STRING-SEQ>, symbol-file-directories),
     system-info);

  if (success == 0)
    signal(make(<access-path-creation-error>));
  else
    conn.connection-process := as-remote-pointer(process);
    add!(conn.access-debugger-connection.connection-open-tethers, conn);
  end if;
end method;
