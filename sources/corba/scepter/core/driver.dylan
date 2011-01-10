Module:    scepter-driver-implementation
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *scepter* = #f;

define inline-only function get-scepter ()
 *scepter*;
end function;

define inline function set-scepter (scepter :: <scepter>)
  *scepter* := scepter;
end function;

define macro with-scepter
  { with-scepter (?scepter:expression)
       ?body:body
    end }
  => { dynamic-bind (*scepter* = ?scepter)
         ?body
       end }
end macro;

define open abstract class <scepter-ast-root> (<object>)
end class;

define open abstract class <scepter-declarator> (<object>)
end class;

define open generic declarator-imported? (declarator :: <scepter-declarator>) => (imported? :: <boolean>);


define constant $default-dylan-orb-runtime-libraries = vector("functional-dylan", "dylan-orb");
define constant $default-dylan-orb-runtime-modules = vector("functional-dylan", "dylan-orb", "dylan-orb-internals");

define constant <failure-mode> =
  one-of(#f,
	 #"termination",
	 #"insertion",
	 #"deletion",
	 #"permutation"
	);


// SCEPTER

define open abstract class <scepter> (<object>)
  constant slot scepter-directory :: <locator>,
    required-init-keyword: directory:;

  slot scepter-scopes :: <deque> = make(<deque>);
  slot scepter-nodes :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-root :: false-or(<scepter-ast-root>) = #f;
  slot scepter-errors :: <deque> = make(<deque>);
  slot scepter-source :: false-or(<scepter-source>) = #f;
  slot scepter-program-name :: <string> = "IDL compiler";

  slot scepter-output :: <stream>
    = make(<indenting-stream>, inner-stream: *standard-output*);

  slot scepter-front-end :: false-or(<scepter-front-end>) = #f;
  slot scepter-back-ends :: <deque> = make(<deque>);
  slot scepter-write-to-standard-output? :: <boolean> = #f;
  slot scepter-sources :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-compile? :: <boolean> = #t;
  slot scepter-informative? :: <boolean> = #t;
  slot scepter-warnings? :: <boolean> = #t;
  slot scepter-preprocess-only? :: <boolean> = #f;
  slot scepter-parse-only? :: <boolean> = #f;
  slot scepter-case-sensitive-reserved-words? :: <boolean> = #f;
  slot scepter-output-directory :: false-or(<locator>) = #f;
  slot scepter-dylan-subdir-prefix :: <string> = "";
  slot scepter-protocol-option? :: <boolean> = #f;
  slot scepter-skeletons-option? :: <boolean> = #f;
  slot scepter-stubs-option? :: <boolean> = #f;
  slot scepter-orb-project-file :: false-or(<locator>) = #f;
  slot scepter-clean-option? :: <boolean> = #f;
  slot scepter-force-build? :: <boolean> = #f;


// variables below here not currently used in earnest

//define variable *main-filename* :: false-or(<string>) = #f;
//define variable *real-filename* :: false-or(<string>) = #f;
  slot scepter-in-main-file? :: <boolean> = #f;
  slot scepter-imported? :: <boolean> = #f;
//define variable *compiler-flags* :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-local-escapes :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-pragmas :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-read-from-stdin? :: <boolean> = #f;
//define variable *include-file-names* :: <stretchy-vector> = make(<stretchy-vector>);
  slot scepter-emit-imported? :: <boolean> = #t;

  slot scepter-dylan-orb-runtime-libraries = $default-dylan-orb-runtime-libraries;
  slot scepter-dylan-orb-runtime-modules = $default-dylan-orb-runtime-modules;

  slot scepter-failure-mode :: <failure-mode> = #f;
  slot scepter-failure-token :: <integer> = 0;
  slot scepter-break-on-errors? :: <boolean> = #f;
end class;


// PUBLIC GENERICS

define sealed generic scepter-invoke (driver :: <scepter>) => (results :: false-or(<sequence>));
define sealed generic scepter-condition (scepter :: <scepter>, condition :: <condition>);


// PUBLIC GENERICS TO BE SPECIALISED BY CLIENTS

define open generic scepter-parse-options (driver :: <scepter>) => ();
define open generic scepter-process-options (driver :: <scepter>) => ();
define open generic scepter-initialize-options (driver :: <scepter>) => ();
define open generic scepter-default-front-end-class (scepter :: <scepter>) => (front-end-class :: subclass(<scepter-front-end>));
define open generic scepter-default-back-end-class (scepter :: <scepter>) => (back-end-class :: subclass(<scepter-back-end>));
define open generic scepter-report-condition (driver :: <scepter>, condition :: <condition>) => ();
define open generic scepter-format-progress-start (scepter :: <scepter>, control-string, #rest args);
define open generic scepter-format-progress-end (scepter :: <scepter>, result, control-string, #rest args);

define macro with-progress
  { with-progress (?scepter:expression, ?args:*) ?body:body end}
  =>
  { let _completed = #f;
    block ()
      if (scepter-informative?(?scepter))
        scepter-format-progress-start(?scepter, ?args);
      end if;
      ?body;
    afterwards
      _completed := #t;
    cleanup 
      if (scepter-informative?(?scepter))
        scepter-format-progress-end(?scepter, _completed, ?args);
      end if;
    end block; }
end macro;


// DEFAULT METHODS

define method scepter-invoke (scepter :: <scepter>)
 => (results :: false-or(<sequence>))
  debug-out(#"driver", "Scepter invoked");
  with-scepter (scepter)
    block ()
      let handler (<error>, test: curry(scepter-handle-condition?, scepter))
	= curry(scepter-handle-any-condition, scepter);
      scepter-process-options(scepter);
      if (scepter.scepter-compile?)
	let sources = scepter.scepter-sources;
        map(curry(scepter-drive-compilation, scepter), sources);
      end if;
    exception (<abort>)
    end block;
  end with-scepter;
end method;

define method scepter-drive-compilation (scepter :: <scepter>,
					 source-name :: <string>)
 => (results :: false-or(<sequence>));
  let results = #f;
  block ()
    if (empty?(scepter.scepter-back-ends))
      let back-end-class = scepter-default-back-end-class(scepter);
      let back-end = make(back-end-class, scepter: scepter);
      push(scepter.scepter-back-ends, back-end);
    end if;

    with-progress(scepter, "Initializing")
      do(initialize-scepter-back-end, scepter.scepter-back-ends);
    end with-progress;
    let front-end = scepter.scepter-front-end;
    let source = scepter-front-end-make-source(front-end, source-name);
    scepter.scepter-source := source;
    let root = scepter-front-end-generate-ast(front-end, source);
    if (scepter.scepter-informative?)
//      format(scepter.scepter-output, "\nLines Parsed: %d",
//	     scepter.scepter-line-number);
      format(scepter.scepter-output, "\nErrors Reported: %d",
	     scepter.scepter-errors.size);
    end if;
    unless (scepter.scepter-parse-only?)
      with-progress(scepter, "Emitting %s", as(<string>, source))
	local method run-back-end-emitter (back-end :: <scepter-back-end>)
	  if (scepter-back-end-emitter?(back-end, scepter.scepter-errors))
	    run-scepter-back-end-emitter(back-end, root, front-end, source)
	  else
	    make(<scepter-back-end-result>, success?: #f);
	  end if;
	end method;
	results := map(run-back-end-emitter, scepter.scepter-back-ends);
      end with-progress;
    end unless;
    with-progress(scepter, "Finalizing")
      do(finalize-scepter-back-end, scepter.scepter-back-ends);
    end with-progress;
  exception (<abort>)
  end block;
  results;
end method;

define method scepter-handle-condition? (scepter :: <scepter>, condition)
  ~scepter.scepter-break-on-errors?
end method;

define method scepter-handle-any-condition
    (scepter :: <scepter>, condition :: <condition>, next-handler :: <function>)
  ignore(next-handler);
  scepter-condition(scepter, condition);
  abort();
end method;

define method scepter-options (scepter :: <scepter>)
  ignore(scepter);
  *scepter-options*;
end method;

define method scepter-process-options (scepter :: <scepter>)
 => ()
  scepter-initialize-options(scepter);
  block ()
    scepter-parse-options(scepter);
    for (option in scepter.scepter-options)
      if (option.scepter-option-supplied?)
        option.scepter-option-callback(option, scepter);
      end if;
    end for;
  exception (condition :: <option-error>)
    scepter-condition(scepter, condition);
    scepter.scepter-compile? := #f;
  end block;
end method;

define method scepter-clean-build? (scepter :: <scepter>)
 => (clean-build? :: <boolean>)
  scepter.scepter-clean-option? | scepter.scepter-force-build?;
end method;


// IDL ERROR REPORTING

define method similar-idl-error? (condition1 :: <condition>, condition2 :: <condition>)
  idl-condition-similar?(condition1, condition2)
  | idl-condition-similar?(condition2, condition1);
end method;

define method scepter-condition (scepter :: <scepter>, condition :: <condition>)
  let similar-error? = (~scepter.scepter-errors.empty?) & similar-idl-error?(condition, scepter.scepter-errors.first);
  unless (similar-error?)
    scepter-report-condition(scepter, condition);
    push(scepter.scepter-errors, condition);
  end unless;
  similar-error?;
end method;

