module: dswank
synopsis: 
author: 
copyright: 

define thread variable *server* = start-compiler();
define thread variable *package* = #f;

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
    *package* := package;
    let result = apply(function, command.tail);
    list(#":return", list(#":ok", result), request-id);
  exception(e :: <error>)
    format(*standard-error*, "Received error during evalution:\n%=\n", e);
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
    #":version", "2008-03-18");
end;

define swank-function find-definitions-for-emacs (symbol-name)
  let project = open-projects()[0];
  let library = #f;
  let module = #f;
  local method check-and-set-module (lib)
          unless(module)
            module := find-module(project, *package*, library: lib);
            if (module)
              library := lib;
            end;
          end;
        end;
  check-and-set-module(project-library(project));
  do-project-used-libraries (check-and-set-module, project, project);

  let env-obj = find-environment-object(project, symbol-name,
                                        library: library,
                                        module: module);

  let location = environment-object-source-location(project, env-obj);

  let source-name =
    if (location)
      let source-record = location.source-location-source-record;
      select (source-record by instance?)
        <file-source-record> =>
          let location = source-record.source-record-location;
          file-exists?(location) & locator-as-string(<byte-string>, location);
        otherwise =>
          source-record.source-record-name;
      end;
    end;
  
  let (name, lineno)
    = source-line-location(location.source-location-source-record,
                           location.source-location-start-line);
  let column = location.source-location-start-column;

  list(list(symbol-name,
            list(#":location",
                 list(#":file", as(<string>, source-name)),
                 list(#":line", lineno, column),
                 #())));
end;

define swank-function listener-eval (arg)
  pair(#":values", run-compiler(*server*, arg))
end;

define function main()
  start-sockets();
  let socket = make(<server-socket>, port: 3456);
  let stream = accept(socket);
  while(#t)
    let length = string-to-integer(read(stream, 6), base: 16);
    let line = read(stream, length);
    format(*standard-error*, "read: %s", line);
    let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
    let function = $emacs-commands[expr.head];
    let result = apply(function, expr.tail);
    let newstream = make(<string-stream>, direction: #"output");
    print-s-expression(newstream, result);
    let s-expression = stream-contents(newstream);
    format(*standard-error*, "write: %s\n", s-expression);
    let siz = integer-to-string(s-expression.size, base: 16, size: 6);
    format(stream, "%s%s", siz, s-expression);
  end;
end;

main();