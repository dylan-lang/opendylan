module: dfmc-visualization
author: Hannes Mehnert
copyright: 2009, all rights reversed
synopsis: Dylan side of graphical visualization of DFM control flow graphs

define constant $default-host = "127.0.0.1";
define constant $default-port = 1234;

define class <dfmc-graph-visualization> (<object>)
  slot socket :: false-or(<socket>) = #f;
  constant slot connection-id :: <symbol>, required-init-keyword: id:;
end;

define function write-to-visualizer (v :: <dfmc-graph-visualization>, data)
  let newstream = make(<string-stream>, direction: #"output");
  print-s-expression(newstream, data);
  let s-expression = stream-contents(newstream);
  format(*standard-error*, "write: %s\n", s-expression);
  let siz = integer-to-string(s-expression.size, base: 16, size: 6);
  format(v.socket, "%s%s", siz, s-expression);
  force-output(v.socket);
end;

define function connect-to-server
 (v :: <dfmc-graph-visualization>, #key host, port)
  unless (v.socket)
    v.socket := make(<tcp-socket>,
                     host: host | $default-host,
                     port: port | $default-port);
    write-to-visualizer(v, list(v.connection-id, "welcome"));
    write-to-visualizer(v, list("ready", "for", "takeoff"));
    write-to-visualizer(v, list(list("this", "is", "a", "list"), list("and", "some", "numbers"), "are", list(23, 42, 23), "there"));
  end;
end;

begin
  start-sockets();
  connect-to-server(make(<dfmc-graph-visualization>, id: #"fooobar"));
end;


