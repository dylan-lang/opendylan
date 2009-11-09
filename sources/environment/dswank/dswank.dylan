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
    //walk-stack();
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
  list(#":pid", 23, 
       #":style", #":fd-handler",
       #":lisp-implementation", list(#":type", "dylan",
                                     #":name", release-product-name(),
                                     #":version", release-version()),
       #":version", "2009-11-06",
       #":package", #(#":name", "opendylan", #":prompt", "opendylan"));
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
  let regs = find-registries($machine-name, $os-name);
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
  let notes = compiler-notes-for-emacs();
  list("T", notes);
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

define swank-function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name);
  environment-object-description(*project*, env, *module*)
end;

define function get-environment-object (symbols)
  let env = split(symbols, ":");
  let symbol-name = env[0];
  let library = #f;
  let module = #f;
  let project = #f;

  if (env.size == 3)
    project := *project*;
    library := find-library(project, env[2]);
    module := find-module(project, env[1], library: library);
  end;

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
  if (env.size == 1)
    *project* := project;
    *library* := library;
    *module* := module;
  end;

  find-environment-object(project, symbol-name,
                          library: library,
                          module: module);
end;

define function get-location-as-sexp (search, env-obj)
  let location = environment-object-source-location(*project*, env-obj);
    

  if (location)
    let source-name
      = print-environment-object-location(*project*,
					  env-obj,
					  absolute-path?: #t); 

    let (name, lineno)
      = source-line-location(location.source-location-source-record,
                             location.source-location-start-line);
    let column = location.source-location-start-column;
    let hname = environment-object-home-name(*project*, env-obj);
    let name =  if (hname)
		  environment-object-primitive-name(*project*, hname);
		else
		  "unknown"
		end;

    list(name,
         list(#":location",
              list(#":file", as(<string>, source-name)),
              list(#":line", lineno, column),
              if (search)
                list(#":snippet", search)
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
  let res = map(curry(get-location-as-sexp, quoted-name.tail.head), result);
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

define xref-function references (env-obj)
  source-form-clients(*project*, env-obj);
end;

define xref-function sets (env-obj)
  // FIXME: returns all references, needs to find actual setters
  source-form-clients(*project*, env-obj);
end;

define xref-function binds (env-obj)
  // FIXME: returns all references, needs to find actual setters
  source-form-clients(*project*, env-obj);
end;

define xref-function macroexpands (env-obj)
  macro-call-source-forms(*project*, env-obj);
end;

define xref-function specializes (function)
  let generic
    = select (function by instance?)
        <generic-function-object> => function;
        <method-object>           => method-generic-function(*project*, function);
        otherwise                 => #f;
      end;
  if (generic)
    concatenate-as(<vector>,
                   vector(generic), generic-function-object-methods(*project*, generic));
  else
    #()
  end
end;

define xref-function callers (env-obj)
  source-form-clients(*project*, env-obj);
end;

define xref-function callees (env-obj)
  // FIXME: filter for function definitions
  source-form-used-definitions(*project*, env-obj);
end;

define swank-function operator-arglist (symbol, package)
  let env-obj = get-environment-object(symbol);
  if (instance?(env-obj, <function-object>))
    concatenate(print-function-parameters(*project*, env-obj, *module*),
                " => ", print-function-values(*project*, env-obj, *module*));
  else
    #"nil"
  end;
end;

define function get-names (env-objs)
  let module = if (instance?(*module*, <string>))
		 find-module(*project*, *module*)
	       else
		 *module*
	       end;
  sort!(map(rcurry(curry(environment-object-display-name, *project*),
		   module, qualify-names?: #t), env-objs));
end;

define swank-function dylan-subclasses (symbols)
  let env-obj = get-environment-object(symbols);
  get-names(class-direct-subclasses(*project*, env-obj));
end;

define swank-function dylan-superclasses (symbol)
  let env-obj = get-environment-object(symbol);
  get-names(class-direct-superclasses(*project*, env-obj));
end;

define function write-to-emacs (stream, s-expression)
  let newstream = make(<string-stream>, direction: #"output");
  print-s-expression(newstream, s-expression);
  let s-expression = stream-contents(newstream);
  //format(*standard-error*, "write: %s\n", s-expression);
  let siz = integer-to-string(s-expression.size, base: 16, size: 6);
  format(stream, "%s%s", siz, s-expression);
end;

define function main(args)
  start-sockets();
  let tmpfile = #f;
  let port = 4005;
  unless (args.size >= 1 & args[0] = "--listen")
    let line = read-line(*standard-input*);
    let sexp = read-lisp(make(<string-stream>, direction: #"input", contents: line));
    tmpfile :=
      block(ret)
        for (call in sexp.tail)
          if (call.head == #"funcall")
            if (call.tail.head.tail.head = "swank:start-server")
              ret(call[2])
            end;
          end;
        end;
        error("error parsing swank startup command");
      end;
  end;
  local method open ()
          block ()
            let socket = make(<server-socket>, port: port);
            format(*standard-output*,
		   "Waiting for connection on port %d\n",
		   port);
            if (tmpfile)
              with-open-file (file = tmpfile, direction: #"output")
                write(file, integer-to-string(port));
              end;
            end;
            socket;
          exception (e :: <error>)
            port := port + 1;
            open();
          end;
        end;
  let socket = open();
  let stream = accept(socket);
  *server* := start-compiler(stream);
  let greeting = concatenate("Welcome to dswank - the ",
			     release-full-name(),
			     " SLIME interface\n");
  write-to-emacs(stream, list(#":write-string", greeting));
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

main(application-arguments());
