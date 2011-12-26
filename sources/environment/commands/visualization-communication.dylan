Module:    environment-visualization-communication
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define class <graph-visualization> (<object>)
  constant slot visualization-host :: <string>,
    required-init-keyword: host:;
  constant slot visualization-port :: <integer>,
    required-init-keyword: port:;
  slot visualization-socket :: false-or(<socket>) = #f;
  constant slot connection-id :: <symbol> = #"Dylan-Graph-Visualization";
  slot system-info = #f;
end;

define function write-to-visualizer (v :: <graph-visualization>, data)
  let newstream = make(<string-stream>, direction: #"output");
  print-s-expression(newstream, data);
  let s-expression = stream-contents(newstream);
  //format(*standard-error*, "write: %s\n", s-expression);
  let siz = integer-to-string(s-expression.size, base: 16, size: 6);
//  block()
    format(v.visualization-socket, "%s%s", siz, s-expression);
    //format(*standard-output*, "%s%s", siz, s-expression);
    //force-output(*standard-output*);
    let res = read-from-visualizer(v);
    unless (res = #(#"ok"))
      format(*standard-output*, "expected ok, but got %=\n", res);
    end;
//  exception (c :: <condition>)
//    format(*standard-output*, "failed communication: %=\n", c);
//  end;
end;

define function read-from-visualizer (v :: <graph-visualization>) => (result)
  let stream = v.visualization-socket;
  //let stream = *standard-input*;
  let length = string-to-integer(read(stream, 6), base: 16);
  let line = read(stream, length);
  //format(*standard-output*, "read: %s\n", line);
  let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
  //format(*standard-output*, "parsed: %=\n", expr);
  expr;
end;

define function connect-to-server
 (v :: <graph-visualization>)
  unless (v.visualization-socket)
    v.visualization-socket := make(<tcp-socket>,
                                   host: v.visualization-host,
                                   port: v.visualization-port);
    write-to-visualizer(v, list(#"connection-identifier", v.connection-id));
    v.system-info := read-from-visualizer(v);
  end;
end;

define function disconnect-from-server
 (v :: <graph-visualization>)
  if (v.visualization-socket)
    close(v.visualization-socket);
    v.visualization-socket := #f
  end;
end;

define function write-data (vis :: <graph-visualization>, key :: <symbol>, e, env, #rest arguments)
      write-to-visualizer(vis, apply(list, key, env, arguments));

//  let form = e.form;
//  let env = form.identifier;
//  let cr = if (instance?(form, <&method>))
//             let cr = form.model-creator; //sometimes model-creator is already a <compilation-record>
//             unless (instance?(cr, <compilation-record>))
//               cr := cr.form-compilation-record;
//             end;
//             cr;
//           elseif (instance?(form, <top-level-form>))
//             form.form-compilation-record;
//           end;
//  if (cr)
//    let sr = cr.compilation-record-source-record;
//    let loc = if (instance?(sr, <flat-file-source-record>))
//                sr.source-record-location.locator-name
//              else
//                "unknown"
//              end;
////    if (loc = "new-dispatch.dylan")

////     if (member?(env, list("%remove-method-from-library", "automatic-finalization-function"),
////              test: method(x, y) copy-sequence(x, end: min(x.size, y.size)) = y end))
//      write-to-visualizer(vis, apply(list, key, env, arguments));
////    end;
//  else
//    //uhm... shouldn't be here
//  end;
end;

begin
  start-sockets();
end;

