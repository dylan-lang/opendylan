module: dswank

define method start-compiler ()
  let input-stream = make(<string-stream>, direction: #"input");
  let output-stream = make(<string-stream>, direction: #"output");
  make-environment-command-line-server
    (input-stream:   input-stream,
     output-stream:  output-stream);
end;

define function run-compiler (server, string :: <string>) => (res :: <collection>)
  execute-command-line(server, string);
  let res = stream-contents(server.server-output-stream);
  split(res, '\n');
end;
