module: dswank
synopsis: 
author: 
copyright: 

define constant $emacs-commands = make(<table>);

define macro emacs-command-definer
  {
   define emacs-command ?:name (?args:*)
     ?:body
   end
} => {
   define function ?name (?args);
     ?body
   end;
   $emacs-commands[":" ## ?#"name"] := ?name;
 }
end;

define emacs-command emacs-rex (command, package, thread-id, request-id)
  // do funky stuff
  block()
    let function = $swank-functions[command.head];
    let result = apply(function, command.tail);
    list(#":return", list(#":ok", result), request-id);
  exception(e :: <error>)
    list(#":return", #(#":abort"), request-id);
  end;
end;

define constant $swank-functions = make(<table>);

define macro swank-function-definer
  {
   define swank-function ?:name (?args:*)
     ?:body
   end
} => {
   define function ?name (?args);
     ?body
   end;
   $swank-functions["swank:" ## ?#"name"] := ?name;
 }
end;

define swank-function connection-info ()
  #(#":pid", 23, 
    #":style", #":fd-handler",
    #":lisp-implementation", #(#":type", "dylan",
                               #":name", "opendylan",
                               #":version", "1.0beta5"),
    #":version", "2008-01-27");
end;

define swank-function find-definitions-for-emacs (symbol-name)
  list(list(symbol-name, #(#":file", "/home/andreas/trunk/fundev/sources/environment/dswank/dswank.dylan", #":position", 0)));
end;

define function main()
  start-sockets();
  let socket = make(<server-socket>, port: 3456);
  let stream = accept(socket);
  while(#t)
    let length = string-to-integer(read(stream, 6), base: 16);
    let line = read(stream, length);
    format(*standard-error*, "%s\n", line);
    let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
    let function = $emacs-commands[expr.head];
    let result = apply(function, expr.tail);
    let newstream = make(<string-stream>, direction: #"output");
    print-s-expression(newstream, result);
    let s-expression = stream-contents(newstream);
    format(*standard-error*, "%s\n", s-expression);
    let siz = integer-to-string(s-expression.size, base: 16, size: 6);
    format(stream, "%s%s", siz, s-expression);
  end;
end;

main();