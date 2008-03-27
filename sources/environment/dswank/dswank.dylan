module: dswank
synopsis: swank support for opendylan. swank is the protocol of SLIME 
author: Andreas Bogk and Hannes Mehnert
copyright: (c) 2008; All rights reversed.

define thread variable *server* = #f;
define thread variable *project* = #f;
define thread variable *library* = #f;
define thread variable *module* = #f;

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
  block()
    let function = $swank-functions[command.head];
    *module* := package;
    let result = apply(function, command.tail);
    list(#":return", list(#":ok", result), request-id);
  exception(e :: <error>)
    format(*standard-error*, "Received error during evalution:\n%=\n", e);
    walk-stack();
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
    #":version", "2008-03-24");
end;

define swank-function quit-lisp ()
  exit-application(0);
end;

define swank-function list-all-package-names (t)
  let res = #();
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file" & filename ~= "Open-Source-License.txt")
            if (last(filename) ~= '~')
              res := pair(filename, res);
            end;
          end;
        end;
  let regs = find-registries("x86", "linux");
  let reg-paths = map(registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
  res;
end;

define swank-function set-package (package-name)
  run-compiler(*server*, concatenate("open ", package-name));
  *project* := find-project(package-name);
  list(package-name, package-name);
end;

define swank-function default-directory ()
  as(<string>, working-directory());
end;

define swank-function set-default-directory (new-directory)
  working-directory-setter(expand-pathname(new-directory));
  new-directory;
end;

define swank-function compile-file-for-emacs (filename, foo)
  block(ret)
    for (p in open-projects())
      for (source in project-sources(p))
        if (source.source-record-location = filename)
          *project* := p;
          ret();
        end;
      end;
    end;
  end;
  run-compiler(*server*, concatenate("build ", *project*.project-name));
  //slime expects a list with 2 elements, so be it
  #("NIL", "2.1");
end;

define swank-function compiler-notes-for-emacs ()
  let warnings = project-warnings(*project*);
  let res = make(<stretchy-vector>);
  for (w in warnings)
    let message = compiler-warning-full-message(*project*, w);
    let short-message = compiler-warning-short-message(*project*, w);
    let location = get-location-as-sexp(#f, w).tail.head;
    let severity = if (instance?(w, <compiler-error-object>))
                     #":error"
                   elseif (instance?(w, <serious-compiler-warning-object>))
                     #":warning"
                   else
                     #":note"
                   end;
    let warning =
      list(#":message", message,
           #":severity", severity,
           #":location", location,
           #":references", #(),
           #":short-message", short-message);
    res := add!(res, warning);
  end;
  res;
end;

//define swank-function describe-symbol (symbol-name)
//end;

define function get-environment-object (symbol-name)
  let library = #f;
  let module = #f;
  let project = #f;
  local method check-and-set-module (p, lib)
          unless(module)
            module := find-module(p, *module*, library: lib);
            if (module)
              library := lib;
            end;
          end;
        end;

  for (p in open-projects())
    unless (project)
      check-and-set-module(p, project-library(p));
      do-project-used-libraries(curry(check-and-set-module, p), p, p);
      if (library)
        project := p;
      end;
    end;
  end;
  *project* := project;
  *library* := library;
  *module* := module;

  find-environment-object(project, symbol-name,
                          library: library,
                          module: module);
end;

define function get-location-as-sexp (search, env-obj)
  let location = environment-object-source-location(*project*, env-obj);
    

  if (location)
    let source-record = location.source-location-source-record;
    let source-name =
      select (source-record by instance?)
        <file-source-record> =>
          let location = source-record.source-record-location;
          file-exists?(location) & locator-as-string(<byte-string>, location);
        otherwise =>
          source-record.source-record-name;
      end;

    let (name, lineno)
      = source-line-location(location.source-location-source-record,
                             location.source-location-start-line);
    let column = location.source-location-start-column;
    let hname = environment-object-home-name(*project*, env-obj);
    let name = if (hname)
                 environment-object-primitive-name(*project*, hname);
               else
                 "unknown"
               end;

    list(name,
         list(#":location",
              list(#":file", as(<string>, source-name)),
              list(#":line", lineno, column),
              if (search)
                list(#":snippet" search)
              else
                #()
              end));
  else
    list(#"unknown", #(#":error", "No error location available"));
  end;
end;

define swank-function find-definitions-for-emacs (symbol-name)
  let env-obj = get-environment-object(symbol-name);
  list(get-location-as-sexp(#f, env-obj));
end;

define swank-function listener-eval (arg)
  run-compiler(*server*, arg);
  list(#":values", "Done.")
end;

define swank-function xref (quoted-arg, quoted-name)
  let function = $xref-functions[quoted-arg.tail.head];
  let env-obj = get-environment-object(quoted-name.tail.head);
  let result = function(env-obj);
  let res = map(curry(get-location-as-sexp, quoted-name.tail.head),
                reverse(result));
  res;
end;

define constant $xref-functions = make(<table>);

define macro xref-function-definer
  {
   define xref-function ?:name (?args:*)
     ?:body
   end
} => {
   define function ?name (?args);
     ?body
   end;
   $xref-functions[":" ## ?#"name"] := ?name;
 }
end;

define xref-function calls (env-obj)
  source-form-clients(*project*, env-obj);
end;

define function print-function-parameters
    (server :: <server>, function-object :: <function-object>,
     namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  with-output-to-string (stream)
    let <object>-class = find-environment-object(server, $<object>-id);
    let (required, rest, key, all-keys?, next) // ... values, rest-value)
      = function-parameters(server, function-object);
    format(stream, "(");
    local method do-parameter (parameter :: <parameter>) => ()
            let keyword 
              = instance?(parameter, <optional-parameter>)
                  & parameter.parameter-keyword;
            let type = parameter.parameter-type;
	    if (keyword)
	      format(stream, "%s: ", keyword)
	    end;
	    format(stream, "%s", parameter.parameter-name);
            unless (type == <object>-class)
              format(stream, " :: %s",
                     environment-object-display-name(server, type, namespace)
                       | "<?>")
            end
          end method do-parameter;
    local method do-parameters (parameters :: <parameters>) => ()
            for (parameter :: <parameter> in parameters,
                 separator = "" then ", ")
              format(stream, separator);
              do-parameter(parameter)
            end for;
          end method do-parameters;
    do-parameters(required);
    let printed-something = size(required) > 0;
    local method print-separator () => ()
            if (printed-something)
              format(stream, ", ");
            else
              printed-something := #t;
            end;
          end method print-separator;
    if (next)
      print-separator();
      format(stream, "#next ");
      do-parameter(next);
    end;
    if (rest)
      print-separator();
      format(stream, "#rest ");
      do-parameter(rest);
    end;
    case
      key & size(key) > 0 =>
        print-separator();
        format(stream, "#key ");
        do-parameters(key);
        if (all-keys?) 
          format(stream, ", #all-keys")
        end;
      all-keys? =>
        print-separator();
        format(stream, "#key, #all-keys");
      otherwise =>
        #f;
    end;
    format(stream, ")");
  end
end function print-function-parameters;

define swank-function operator-arglist (symbol, package)
  let env-obj = get-environment-object(symbol);
  if (instance?(env-obj, <function-object>))
    print-function-parameters(*project*, env-obj, *module*)
  else
    #"nil"
  end;
end;

define function write-to-emacs (stream, s-expression)
  let newstream = make(<string-stream>, direction: #"output");
  print-s-expression(newstream, s-expression);
  let s-expression = stream-contents(newstream);
//  format(*standard-error*, "write: %s\n", s-expression);
  let siz = integer-to-string(s-expression.size, base: 16, size: 6);
  format(stream, "%s%s", siz, s-expression);
end;

define function main()
  start-sockets();
  let socket = make(<server-socket>, port: 4005);
  let stream = accept(socket);
  *server* := start-compiler(stream);
  while(#t)
    let length = string-to-integer(read(stream, 6), base: 16);
    let line = read(stream, length);
    format(*standard-error*, "read: %s\n", line);
    let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
    let function = $emacs-commands[expr.head];
    let result = apply(function, expr.tail);
    write-to-emacs(stream, result);
  end;
end;

main();
