Module: orb-connections
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method ensure-reclaimer-started (manager :: <connection-manager>)
  unless (connection-manager-reclaimer-thread(manager))
    connection-manager-reclaimer-thread(manager) :=
      make(<thread>,
	   function: method () reclaim-connections(manager) end method,
	   name: "Connection reclaimer");
  end unless;
end method;

define method reclaim-connections (manager :: <connection-manager>)
  while (#t)
    sleep(connection-manager-reclaimer-sleep(manager));
    with-each-connection (connection = manager)
      if (~connection-busy?(connection)
	    & (size(connection-requests(connection)) = 0)
	    & empty?(connection-manager-sender-mailbox(manager)))
	close-connection(manager, connection);
      else
	connection-busy?(connection) := #f;
      end if;
    end;
  end while;
end method;
