Module:    environment-server
Author:    Hugh Greene
Synopsis:  Controlling the Environment from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Startup and shutdown for all servers together.

define function dde-server-environment-message-receiver
    (message :: <environment-message>)
  select (message by instance?)
    <environment-started-message>  => server-start();
    <environment-stopping-message> => server-stop();
    otherwise			   => #f /* do nothing */;
  end;
end function dde-server-environment-message-receiver;

tune-in($environment-channel, dde-server-environment-message-receiver,
	message-type: <environment-message>);

define function server-start () => ()
    dde/server-start();
end function;

define function server-stop () => ()
    dde/server-stop();
end function;
