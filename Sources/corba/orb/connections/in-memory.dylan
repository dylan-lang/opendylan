Module: orb-connections
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple in-memory connection stream for testing
/// without sockets.

define class <in-memory-connection-manager> (<connection-manager>)
end class;

/// Basically make an stream object with two internal streams.
/// Two calls to make of the outer stream class are allowed (client and server).
/// Each is set up to read and write from the opposite internal streams.

define class <in-memory-connection-stream> (<stream>)
  constant slot connection-stream-streams :: <table> = make(<table>);
  slot connection-stream-agent :: <symbol> = #"client";
end class;

define sealed domain make (subclass(<in-memory-connection-stream>));
define sealed domain initialize (<in-memory-connection-stream>);

define constant $connection-stream-agents :: <table> = make(<table>);

define method initialize (stream :: <in-memory-connection-stream>, #key host, port)
  next-method();
  let id = connection-stream-agent-id(host, port);
  let agent = current-agent(id);
  connection-stream-agent(stream) := agent;
  for (agent in #[#"client", #"server"])
    element(connection-stream-streams(stream), agent) :=
      make(<in-memory-connection-stream-stream>, host: host, port: port, agent: agent);
  end for;
end method;

define method current-agent (id :: <symbol>)
  let agent = element($connection-stream-agents, id, default: #"client");  
  if (agent = #"error")
    error("cannot have more than two ends of a connection: %s", id)
  end if;
  element($connection-stream-agents, id) := 
    select (agent)
      #"client" => #"server";
      #"server" => #"error";
    end select;
  agent
end method;

define method opposite-agent (agent :: <symbol>)
  select (agent)
    #"client" => #"server";
    #"server" => #"client";
  end select;
end method;  

define method connection-stream-agent-id(host :: <string>, port :: <integer>)
  as(<symbol>, format-to-string("%s:%d", host, port));
end method;

define class <in-memory-connection-stream-stream> (<stream>)
  slot connection-stream-buffer :: <stretchy-vector> = make(<stretchy-vector>);
  slot connection-stream-input-index :: <integer> = 0;
  constant slot connection-stream-semaphore :: <semaphore> = make(<semaphore>);
end class;

define sealed domain make (subclass(<in-memory-connection-stream-stream>));
define sealed domain initialize (<in-memory-connection-stream-stream>);

//define method connection-stream-output-index (stream :: <in-memory-connection-stream-stream>)
//  size(connection-stream-buffer(stream));
//end method;

define constant $connection-stream-streams :: <table> = make(<table>);

define method make (class == <in-memory-connection-stream-stream>, #key host, port, agent)
 => (object :: <in-memory-connection-stream-stream>)
  let id = connection-stream-id(host, port, agent);
  let stream = element($connection-stream-streams, id, default: #f);
  if (stream)
    stream
  else
    element($connection-stream-streams, id) := next-method();
  end if;
end method;

define method connection-stream-id
    (host :: <string>, port :: <integer>, agent :: <symbol>)
  as(<symbol>, format-to-string("%s:%d(%s)", host, port, as(<string>, agent)));
end method;

define method wait-for-connection-stream-contents (stream :: <in-memory-connection-stream-stream>)
  wait-for(connection-stream-semaphore(stream));
end method;

define method note-connection-stream-contents (stream :: <in-memory-connection-stream-stream>)
  release(connection-stream-semaphore(stream));
end method;

define method read-element (stream :: <in-memory-connection-stream>, #key on-end-of-stream = unsupplied())
 => (byte)
  let dstream = element(connection-stream-streams(stream), connection-stream-agent(stream));
  wait-for-connection-stream-contents(dstream);
  let index = connection-stream-input-index(dstream);
  if (index > size(connection-stream-buffer(dstream)))
    if (unsupplied?(on-end-of-stream))
      error(make(<end-of-stream-error>, stream: stream));
    else
      on-end-of-stream
    end if;
  else
    let byte = connection-stream-buffer(dstream)[index];
    connection-stream-input-index(dstream) := index + 1;
    byte
  end if;
end method;

define method write-element (stream :: <in-memory-connection-stream>, byte :: <integer>)
 => ()
  let dstream = element(connection-stream-streams(stream), opposite-agent(connection-stream-agent(stream)));
  connection-stream-buffer(dstream) := add!(connection-stream-buffer(dstream), byte);
  note-connection-stream-contents(dstream);
end method;

define method receive-connections
    (manager :: <in-memory-connection-manager>,
     port :: false-or(<integer>),
     initialize-callback :: <function>,
     connection-callback :: <function>)
  ignore(initialize-callback);
  connection-callback(make(connection-manager-stream-class(manager), host: hostname(), port: port));
end method;

define method connection-manager-error-class (manager :: <in-memory-connection-manager>)
  <stream-error>
end method;

define method connection-manager-stream-class (manager :: <in-memory-connection-manager>)
  <in-memory-connection-stream>
end method;

