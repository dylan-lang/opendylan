Module: dfmc-common
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// COMPILATION CONTEXT

define compiler-open generic compiled-to-definitions? 
    (context :: <compilation-context>);

define compiler-open generic compiled-to-definitions?-setter
    (value, context :: <compilation-context>);

define compiler-open generic compilation-from-definitions-started?
    (context :: <compilation-context>);

define compiler-open generic compilation-from-definitions-started?-setter
    (value, context :: <compilation-context>);

// A library description or an interactive layer.
define compiler-open abstract dood-class <compilation-context> (<object>)
  slot compiled-to-definitions? :: <boolean> = #f;
  slot compilation-from-definitions-started? :: <boolean> = #f;
  slot compilation-definitions-inconsistent? :: <boolean> = #f;
  lazy slot compilation-context-records :: <vector> = #[];
  weak slot compilation-timings :: <list> = #(),
    reinit-expression: #();
end;

define compiler-open generic library-description-personal? (library-description);

define macro with-inconsistent-definitions
  { with-inconsistent-definitions (?cc:expression) ?:body end }
    =>
  { begin
      let cc = ?cc;
      let inconsistent? = cc.compilation-definitions-inconsistent?;
      cc.compilation-definitions-inconsistent? := #t;
      ?body;
      // restore it if body exits normally, otherwise aborting with
      // inconsistent defs.
      cc.compilation-definitions-inconsistent? := inconsistent?;
    end }
end macro;

/// DOOD-DFMC-OBJECT

// define constant <dood-dfmc-object> = <dood-mapped-and-owned-object>;
define constant <dood-dfmc-object> = <dood-mapped-object>;

/// DYNAMIC CONTEXT

define compiler-open generic current-library-description () => (false-or-ld);
define compiler-open generic current-library-description? (ld) => (well? :: <boolean>);

define compiler-open generic current-top-level-library-description
  () => (false-or-ld);
define compiler-open generic current-top-level-library-description?
  (ld) => (well? :: <boolean>);

define compiler-open generic current-library-in-context? (ld) => (well? :: <boolean>);
define compiler-open generic current-back-end () => (back-end);
define compiler-open generic current-back-end-name () => (name :: false-or(<symbol>));
define compiler-open generic current-compilation-mode () => (mode :: <symbol>);
define compiler-open generic current-processor-name () => (name :: <symbol>);
define compiler-open generic current-os-name () => (name :: <symbol>);
define compiler-open generic compiling-dylan-library? () => (well? :: <boolean>);

define compiler-open generic word-size () => (number-bytes :: <integer>);

/// EVALUATION

// should these return multiple values?
define compiler-open generic &eval (env, object) => object;
define compiler-open generic &constant-eval (env, object) => object;

define compiler-open generic eval (top-level-lambda);
define compiler-open generic constant-eval (top-level-lambda);

define compiler-open generic compile-stage (object) => (object);

define compiler-open generic run-stage (object) => (object);

/// FRAGMENTS

define compiler-open generic make-variable-name-fragment (name) => (variable-name);

define compiler-open generic resolve-qualified-variable-name-module
    (name, module, library, source-location) => (module);

/// DIAGNOSTIC TOOLS

define compiler-open generic describe (o) => ();
define compiler-open generic describe* (o) => ();

/// STRIP INCREMENTAL SLOTS

define compiler-open generic strip-incremental-slots (x);

define method strip-incremental-slots (x) end;

// GTS DEBUGGING
// #() to turn off, #("all") to turn everything on:
define variable *gts-debug* = #(); 
define function gts-debug(id, format-string, #rest r)
  if (~empty?(*gts-debug*) & 
      (member?("all", *gts-debug*, test: \=) | member?(id, *gts-debug*, test: \=)))
    format-out("[%s] ", id);
    apply(format-out, format-string, r);
  end if;
end function;
