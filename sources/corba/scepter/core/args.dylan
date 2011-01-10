Module:    scepter-driver-implementation
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <option-error> (<idl-error>)
end class;

define open class <named-option-error> (<option-error>)
  constant slot condition-option-name :: <string>, required-init-keyword: option-name:;
end class;

define sealed class <missing-argument> (<named-option-error>)
  inherited slot idl-condition-string = "missing argument after ";
end class;
define sealed domain make (singleton(<missing-argument>));
define sealed domain initialize (<missing-argument>);

define method idl-condition-body (stream :: <stream>, condition :: <missing-argument>)
 => ()
  format(stream, "\'%s\' flag", condition.condition-option-name);
end method;

define sealed class <option-not-implemented> (<named-option-error>)
  inherited slot idl-condition-string = "option not yet implemented ";
end class;
define sealed domain make (singleton(<option-not-implemented>));
define sealed domain initialize (<option-not-implemented>);

define method idl-condition-body (stream :: <stream>, condition :: <option-not-implemented>)
 => ()
  format(stream, "\'%s\'", condition.condition-option-name);
end method;

define sealed class <illegal-option> (<named-option-error>)
  inherited slot idl-condition-string = "illegal option ";
end class;
define sealed domain make (singleton(<illegal-option>));
define sealed domain initialize (<illegal-option>);

define sealed method idl-condition-body (stream :: <stream>, condition :: <illegal-option>)
 => ()
  format(stream, "\'%s\'", condition.condition-option-name);
end method;

/*
define sealed class <bad-W-option> (<named-option-error>)
  inherited slot command-line-option = "W";
end class;
define sealed domain make (singleton(<bad-W-option>));
define sealed domain initialize (<bad-W-option>);

define method scepter-condition (scepter :: <scepter>, condition :: <bad-W-option>)
  format(scepter.scepter-output, "\n%s: -W must be followed by 'p' or 'b'",
         scepter.scepter-program-name);
end method;
*/

define method split-on (delimiter? :: <function>, input :: <string>)
  let tokens :: <deque> = make(<deque>);
  let next-token :: <deque> = make(<deque>);
  let inside-string? :: <boolean> = #f;
  for (char in input)
    case
      ~inside-string? & char.delimiter?
	=> unless (next-token.empty?)
	     push-last(tokens, as(<string>, next-token));
	     next-token.size := 0;
	   end unless;

      char = '\"'
	=> inside-string? := ~inside-string?;

      otherwise
	=> push-last(next-token, char);
    end case;
  end for;
  unless (next-token.empty?)
    push-last(tokens, as(<string>, next-token));
  end unless;
  tokens;
end method;

define method comma? (char == ',')
  #t;
end method;

define method comma? (char :: <character>)
  #f;
end method;

define sealed class <scepter-option> (<object>)
  constant slot scepter-option-name :: <string>, required-init-keyword: name:;
  constant slot scepter-option-short-name :: false-or(<string>) = #f, init-keyword: short-name:;
  constant slot scepter-option-callback :: <function>, required-init-keyword: callback:;
  constant slot scepter-option-init-callback :: false-or(<function>) = #f, init-keyword: init-callback:;
  constant slot scepter-option-usage :: false-or(<string>) = #f, init-keyword: usage:;
  constant slot scepter-option-value? :: <boolean> = #f, init-keyword: value?:;
  constant slot scepter-option-hidden? :: <boolean> = #f, init-keyword: hidden?:;
  slot scepter-option-values :: <list> = #();
end class;

define sealed domain make (subclass(<scepter-option>));
define sealed domain initialize (<scepter-option>);

define macro scepter-option-definer
  { define scepter-option ?args:* end }
  =>
  { make(<scepter-option>, ?args) }
end macro;

define variable *scepter-options* :: <table> = make(<table>);
define variable *scepter-options-short* :: <table> = make(<table>);

define method initialize (option :: <scepter-option>, #key, #all-keys)
  next-method();
  *scepter-options*[as(<symbol>, option.scepter-option-name)] := option;
  if (option.scepter-option-short-name)
    *scepter-options-short*[as(<symbol>, option.scepter-option-short-name)] := option;
  end if;
end method;

define method scepter-option-add-value (value :: <string>, option :: <scepter-option>)
  option.scepter-option-values := pair(value, option.scepter-option-values);
end method;

define method scepter-option-supplied (option :: <scepter-option>)
 => ()
  option.scepter-option-values := #("");
end method;

define method scepter-option-supplied? (option :: <scepter-option>)
 => (supplied? :: <boolean>)
  ~empty?(option.scepter-option-values);
end method;

define method scepter-initialize-options (scepter :: <scepter>) => ()
  debug-out(#"driver", "Scepter initializing options");
  for (option in scepter.scepter-options)
    option.scepter-option-values := #();
    let init-callback = option.scepter-option-init-callback;
    if (init-callback) init-callback(option, scepter) end;
  end for;
end method;
        
define method scepter-find-option (scepter :: <scepter>, arg-name :: <string>)
 => (option :: false-or(<scepter-option>), rest :: false-or(<string>))
  let option = element(*scepter-options*, as(<symbol>, arg-name), default: #f)
    | element(*scepter-options-short*, as(<symbol>, arg-name), default: #f);
  values(option, #f);
end method;

/*
define scepter-option
    name: "-",
    usage: "-\t\t\t- read from standard input",
    value?: #f,
    callback: method () *files* := add!(*files*, "standard input") end method,
    init-callback: method () *files* := make(<stretchy-vector>) end method
end scepter-option;
*/

define scepter-option
    name: "escape",
    short-name: "e",
    usage: "/escape:...             - local implementation-specific escape",
    value?: #t,
    hidden?: #t,
    callback: method (option, scepter)
		let local-escapes = scepter.scepter-local-escapes;
		let value = option.scepter-option-values.first;
		scepter.scepter-local-escapes := concatenate!(local-escapes, value, " ")
	      end method,
    init-callback: method (option, scepter)
		     scepter.scepter-local-escapes := make(<stretchy-vector>)
		   end method
end scepter-option;

define scepter-option
    name: "preprocess",
    short-name: "p",
    usage: "/preprocess             - runs preprocessor only, prints on stdout",
    value?: #f,
    callback:      method (option, scepter)
                     scepter.scepter-preprocess-only? := #t;
                   end method,
    init-callback: method (option, scepter)
                     scepter.scepter-preprocess-only? := #f;
                   end method
end scepter-option;

define scepter-option
    name: "parse",
    usage: "/parse                  - parse IDL only",
    value?: #f,
    callback:      method (option, scepter)
		     scepter.scepter-parse-only? := #t;
		   end method,
    init-callback: method (option, scepter)
		     scepter.scepter-parse-only? := #f;
		   end method
end scepter-option;

define scepter-option
    name: "define",
    short-name: "d",
    usage: "/define:name[=value]    - defines name for preprocessor",
    value?: #t,
    callback:      method (option, value)
                     signal(make(<option-not-implemented>));
                   end method
end scepter-option;

/* --- Not currently used
define method add-include-path (dir) => ()
  push(*cpp-include-path*, dir)
end method;
*/

define scepter-option
    name: "include",
    short-name: "i",
    usage: "/include:dir            - includes dir in search path for preprocessor",
    value?: #t,
    callback:      method (option, scepter)
		     do(curry(push, *cpp-include-path*), option.scepter-option-values);
	           end method
end scepter-option;

define scepter-option
    name: "undefine",
    short-name: "u",
    usage: "/undefine:name          - undefines name for preprocessor",
    value?: #t,
    callback:      method (option, scepter)
                     signal(make(<option-not-implemented>));
                   end method
end scepter-option;


define scepter-option
    name: "language",
    short-name: "l",
    usage: "/language:name          - (deprecated) use specified back end",
    value?: #t,
    callback: setup-scepter-back-ends,
    init-callback: reset-scepter-back-ends
end scepter-option;

define scepter-option
    name: "back-end",
    usage: "/back-end:name          - use specified back end",
    value?: #t,
    callback: setup-scepter-back-ends,
    init-callback: reset-scepter-back-ends
end scepter-option;

define scepter-option
    name: "front-end",
    usage: "/front-end:name         - use specified front end",
    value?: #t,
    hidden?: #t,
    callback: setup-scepter-front-end,
    init-callback: reset-scepter-front-end
end scepter-option;

define scepter-option
    name: "stdout",
    short-name: "o",
    usage: "/stdout                 - output to stdout rather than back-end specific file",
    value?: #f,
    hidden?: #t,
    callback:      method (option, scepter)
                     scepter.scepter-write-to-standard-output? := #t;
                   end method,
    init-callback: method (option, scepter)
                     scepter.scepter-write-to-standard-output? := #f;
                   end method
end scepter-option;

define scepter-option
    name: "trace",
    short-name: "t",
    usage: "/trace                  - traces compilation stages",
    value?: #f,
    callback:      method (option, scepter)
                     scepter.scepter-informative? := #t;
                   end method,
    init-callback: method (option, scepter)
                     scepter.scepter-informative? := #f;
                   end method
end scepter-option;

define scepter-option
    name: "nowarnings",
    short-name: "w",
    usage: "/nowarnings             - suppresses IDL compiler warning messages",
    value?: #f,
    callback:      method (option, scepter)
                     ignore(option);
                     scepter.scepter-warnings? := #f;
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-warnings? := #t;
                   end method
end scepter-option;

define scepter-option
    name: "case",
    short-name: "c",
    usage: "/case                   - case sensitive reserved words (only look for lower case reserved words)",
    value?: #f,
    callback:      method (option, scepter)
                     ignore(option);
                     scepter.scepter-case-sensitive-reserved-words? := #t;
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-case-sensitive-reserved-words? := #f;
                   end method
end scepter-option;

define scepter-option
    name: "mode",
    short-name: "m",
    usage: "/mode:mode              - failure mode",
    value?: #t,
    hidden?: #t,
    callback:      method (option, scepter)
		     let value = option.scepter-option-values.first;
                     scepter.scepter-failure-mode := as(<symbol>, value)
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-failure-mode := #f;
                   end method
end scepter-option;

define scepter-option
    name: "failure",
    short-name: "f",
    usage: "/failure:index          - failure point (token index)",
    value?: #t,
    hidden?: #t,
    callback:      method (option, scepter)
		     let value = option.scepter-option-values.first;
                     scepter.scepter-failure-token := string-to-integer(value)
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-failure-token := 0;
                   end method
end scepter-option;

define scepter-option
    name: "debugger",
    usage: "/debugger               - enter the debugger if the compiler crashes",
    value?: #f,
    callback:      method (option, scepter)
                     ignore(option);
                     debugging?() := #t;
                     debug-parts() := #(#"console", #"driver");
                     scepter.scepter-break-on-errors? := #t;
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     debugging?() := #f;
                     scepter.scepter-break-on-errors? := #f;
                   end method
end scepter-option;

define method output-directory-setter (option, scepter) => ()
  let dir = option.scepter-option-values.first;
  scepter.scepter-output-directory
    := merge-locators(as(<directory-locator>, dir), scepter.scepter-directory);
end method;

define scepter-option
    name: "directory",
    usage: "/directory:dir          - directory into which Dylan library subdirs are placed",
    value?: #t,
    callback: output-directory-setter,
    init-callback: method (option, scepter)
                     scepter.scepter-output-directory := #f;
                   end method
end scepter-option;

define scepter-option
    name: "prefix",
    usage: "/prefix:name            - prefix for subdirectory names",
    value?: #t,
    callback:      method (option, scepter)
		     let prefix = option.scepter-option-values.first;
                     scepter.scepter-dylan-subdir-prefix := concatenate!(prefix, "-");
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-dylan-subdir-prefix := "";
                   end method
end scepter-option;

define scepter-option
    name: "protocol",
    usage: "/protocol               - generate protocol library only",
    value?: #f,
    callback:      method (option, scepter)
                     ignore(option);
                     scepter.scepter-protocol-option? := #t;
                   end method,
    init-callback: method (option, scepter)
                     ignore(option);
                     scepter.scepter-protocol-option? := #f;
                   end method
end scepter-option;

define scepter-option
    name: "skeletons",
    usage: "/skeletons              - generate libraries required by server only",
    value?: #f,
    callback: method (option, scepter) scepter.scepter-skeletons-option? := #t; end method,
    init-callback: method (option, scepter) scepter.scepter-skeletons-option? := #f end method
end scepter-option;

define scepter-option
    name: "stubs",
    usage: "/stubs                  - generate libraries required by client only",
    value?: #f,
    callback: method (option, scepter) scepter.scepter-stubs-option? := #t; end method,
    init-callback: method (option, scepter) scepter.scepter-stubs-option? := #f end method
end scepter-option;

define scepter-option
    name: "runtime",
    usage: "/runtime:pathname       - pathname of runtime project file",
    value?: #t,
    hidden?: #t,
    callback: method (option, scepter)
		let file = option.scepter-option-values.first;
		scepter.scepter-orb-project-file := as(<file-locator>, file);
	      end method,
    init-callback: method (option, scepter) scepter.scepter-orb-project-file := #f; end method
end scepter-option;

define scepter-option
    name: "clean",
    usage: "/clean                  - force compilation of IDL",
    value?: #f,
    callback:      method (option, scepter)
		     scepter.scepter-clean-option? := #t;
		   end method,
    init-callback: method (option, scepter)
		     scepter.scepter-clean-option? := #f;
		   end method
end scepter-option;

define scepter-option
    name: "libraries",
    usage: "/libraries              - use these libraries instead of default ORB runtime",
    value?: #t,
    callback: method (option, scepter)
		let libraries = option.scepter-option-values.first;
		scepter.scepter-dylan-orb-runtime-libraries := split-on(comma?, libraries);
	      end method,
    init-callback: method (option, scepter) scepter.scepter-dylan-orb-runtime-libraries := $default-dylan-orb-runtime-libraries; end method
end scepter-option;

define scepter-option
    name: "modules",
    usage: "/modules                - use these modules instead of default ORB runtime",
    value?: #t,
    callback: method (option, scepter)
		let modules = option.scepter-option-values.first;
		scepter.scepter-dylan-orb-runtime-modules := split-on(comma?, modules); end method,
    init-callback: method (option, scepter) scepter.scepter-dylan-orb-runtime-modules := $default-dylan-orb-runtime-modules; end method
end scepter-option;

define scepter-option
    name: "main",
    usage: "/main                   - only generate code for main IDL files and not for included IDL files",
    value?: #f,
    callback: method (option, scepter) scepter.scepter-emit-imported? := #f; end method,
    init-callback: method (option, scepter) scepter.scepter-emit-imported? := #t; end method
end scepter-option;

define scepter-option
    name: "IDL-file",
    usage: "/IDL-file filename",
    value?: #t,
    hidden?: #t,
    callback: method (option, scepter)
                do(method (source) scepter.scepter-sources := add!(scepter.scepter-sources, source) end,
                   option.scepter-option-values);
              end method
end scepter-option;

