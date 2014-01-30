Module:    environment-visualization-communication
Author:   Hannes Mehnert
Copyright:    Dylan Hackers 2014
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define class <graph-visualization> (<object>)
  constant slot visualization-host :: <string>,
    required-init-keyword: host:;
  constant slot visualization-port :: <integer>,
    required-init-keyword: port:;
  slot visualization-socket :: false-or(<socket>) = #f;
  constant slot connection-id :: <symbol> = #"Dylan-Graph-Visualization";
  slot visualization-type :: one-of(#"json", #"sexp") = #"json";
  slot visualization-filter :: false-or(<string>) = #f;
  slot visualization-serializer :: false-or(<serializer>) = #f;
end;

define function trace? (v :: <graph-visualization>, data) => (res :: <boolean>)
  block (ret)
    unless (v.visualization-filter)
      ret(#t)
    end;
    for (x from 0 below data.size by 2)
      if (data[x] == #"formid")
        if (data[x + 1] = v.visualization-filter)
          ret(#t)
        else
          ret(#f)
        end;
      end;
    end;
    #t
  end;
end;

define function write-to-visualizer (v :: <graph-visualization>, data)
  if (trace?(v, data))
    //assume data is a plist!
    let ser = v.visualization-serializer;
    write-start-object(ser);
    for (i from 0 below data.size by 2,
         j from 1 by 2)
      write-field(ser, data[i], data[j]);
      unless (i = data.size)
        write-separator-object(ser);
      end unless;
    end for;
    write-end-object(ser);
    write(v.visualization-socket, "\n");
    force-output(v.visualization-socket);
  end;
end;
/*
define function read-from-visualizer (v :: <graph-visualization>) => (result)
  let stream = v.visualization-socket;
  //let stream = *standard-input*;
  let length = string-to-integer(read(stream, 6), base: 16);
  let line = read(stream, length);
  //format(*standard-output*, "read: %s\n", line);
  //let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
  //format(*standard-output*, "parsed: %=\n", expr);
  //expr;
end;
*/
define function connect-to-server
 (v :: <graph-visualization>)
  unless (v.visualization-socket)
    v.visualization-socket := make(<tcp-socket>,
                                   host: v.visualization-host,
                                   port: v.visualization-port);
    v.visualization-serializer
      := make(if (v.visualization-type == #"sexp")
                <sexpr-serializer>
              else
                <json-serializer>
              end,
              stream: v.visualization-socket);

    write-to-visualizer(v, list(#"type", #"hello",
                                #"connection-identifier", v.connection-id));
//    v.system-info := read-from-visualizer(v);
  end;
end;

define function disconnect-from-server
 (v :: <graph-visualization>)
  if (v.visualization-socket)
    close(v.visualization-socket);
    v.visualization-socket := #f;
    v.visualization-serializer := #f;
  end;
end;

begin
  start-sockets();
end;

