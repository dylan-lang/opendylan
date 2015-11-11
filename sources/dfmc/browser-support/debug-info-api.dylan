module: dfmc-browser-support
Synopsis: Debug Info browsing routines
Author: Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Support

define function context-back-end
    (context :: dfmc-<library-description>)
    => (back-end)
  let name = dfmc-library-description-compiler-back-end-name(context);
  let platform-name = dfmc-library-description-platform-name(context);
  find-back-end-object(name, platform-name)
end function;


define function not-yet-implemented (api :: <byte-string>)
  error("Debug lookup protocol error: the function %s is not yet implemented", api);
end function;


define function no-default-error (api :: <byte-string>)
  error("Debug lookup protocol error: the function %s has no default implementation", api);
end function;


define macro apply-to-arg-spec
  { apply-to-arg-spec
      ?fn:expression,
      ?arg-spec:*
    end }
    => { ?fn(?arg-spec) }

arg-spec:
  { } => { }
  { \#key ?key-spec:* } => { ?key-spec }
  { ?:name :: ?type:expression, ... } => { ?name, ... }

key-spec:
  { } => { }
  { ?:name = ?init:expression, ... }  => { ?#"name", ?name, ... }
  { ?:name, ... }  => { ?#"name", ?name, ... }
end macro;


define macro back-ended-function-definer
  { define back-ended-function ?:name
        (?context:name :: ?type:expression, ?args:*) => (?vals:*)
      ?default-body:body
    end }
  =>
    { define open generic "back-end-" ## ?name
          (back-end :: dfmc-<back-end>, ?context :: ?type, ?args) => (?vals);

      define method "back-end-" ## ?name
          (back-end :: dfmc-<back-end>, ?context :: ?type, ?args) => (?vals)
        ?default-body
      end;

      define function ?name
          (?context :: ?type, ?args) => (?vals)
        with-context (?context)
          let back-end = ?context . context-back-end;
          apply-to-arg-spec "back-end-" ## ?name, back-end, ?context, ?args end;
        end
      end }
end macro;


define macro defaulted-generic-definer
  { define defaulted-generic ?:name
        (?args:*) => (?vals:*)
      ?:body
    end }
  => { define open generic ?name (?args) => (?vals);
       define method ?name (?args) => (?vals)
         ?body
       end }
end macro;


/// symbolic names



define defaulted-generic source-form-symbolic-name
    (context :: dfmc-<library-description>, source-form :: <source-form>)
    => (name :: false-or(<byte-string>))
   not-yet-implemented("source-form-symbolic-name");
end defaulted-generic;


define function symbolic-name-source-form
    (context :: dfmc-<library-description>, name :: <byte-string>)
    => (sf :: false-or(<source-form>))
   not-yet-implemented("symbolic-name-source-form");
end function;



define function variable-symbolic-name
    (context :: dfmc-<library-description>, variable :: <variable>)
    => (name :: false-or(<byte-string>))
   not-yet-implemented("variable-symbolic-name");
end function;


define function symbolic-name-variable
    (context :: dfmc-<library-description>, name :: <byte-string>)
    => (variable :: false-or(<variable>))
   not-yet-implemented("symbolic-name-variable");
end function;



define defaulted-generic compiled-lambda-symbolic-name
    (context :: dfmc-<library-description>, compiled-lambda :: <compiled-lambda>)
    => (name :: false-or(<byte-string>))
  #f;
end defaulted-generic;


define back-ended-function symbolic-name-compiled-lambda
    (context :: dfmc-<library-description>, name :: <byte-string>, #key file-name)
    => (compiled-lambda :: false-or(<compiled-lambda>))
  #f;
end back-ended-function;


define function symbolic-name-component-name
    (context :: dfmc-<library-description>, name :: <byte-string>)
    => (component-name :: false-or(<byte-string>))
  not-yet-implemented("symbolic-name-component-name");
end function;


define function component-name-context
    (context :: dfmc-<library-description>, debug-target, name :: <byte-string>)
    => (context :: false-or(dfmc-<library-description>))
   let name = as-lowercase(name);
   block (found)
     local method return-if-ok (ld :: dfmc-<library-description>)
             if (ld.compilation-context-component-name = name)
               found(library-execution-context(ld, debug-target));
             end if;
           end method;
     return-if-ok(context);
     do(return-if-ok, context.dfmc-all-used-library-descriptions);
     #f;
   end block;
end function;

define function library-name-context
    (context :: dfmc-<library-description>, debug-target, name :: <byte-string>)
    => (context :: false-or(dfmc-<library-description>))
   let name = as-lowercase(name);
   block (found)
     local method return-if-ok (ld :: dfmc-<library-description>)
             if (ld.compilation-context-library-name = name)
               found(library-execution-context(ld, debug-target));
             end if;
           end method;
     return-if-ok(context);
     do(return-if-ok, context.dfmc-all-used-library-descriptions);
     #f;
   end block;
end function;

define method library-execution-context
    (context :: dfmc-<library-description>, debug-target)
    => (execution-context :: dfmc-<library-description>)
  context;
end method;

define method library-execution-context
    (context :: dfmc-<project-library-description>, debug-target)
    => (execution-context :: dfmc-<library-description>)
  dfmc-lookup-interactive-context(debug-target, context, default: context);
end method;


define back-ended-function source-position-compiled-lambda
    (context :: dfmc-<library-description>,
     sr :: dfmc-<source-record>,
     line-no :: <integer>,
     #key interactive-only? = #t)
    => (compiled-lambda :: false-or(<compiled-lambda>),
        code-offset :: false-or(<integer>))
  values(#f, #f);
end back-ended-function;


define back-ended-function source-form-compiled-lambda
    (context :: dfmc-<library-description>, source-form :: <source-form>)
    => (compiled-lambda :: false-or(<compiled-lambda>))
  not-yet-implemented("source-form-compiled-lambda");
end back-ended-function;


define defaulted-generic compiled-lambda-source-form
    (context :: dfmc-<library-description>, compiled-lambda :: <compiled-lambda>)
    => (source-form :: false-or(<source-form>))
  not-yet-implemented("compiled-lambda-source-form");
end defaulted-generic;


define back-ended-function source-form-compiled-lambda-symbolic-name
    (context :: dfmc-<library-description>, source-form :: <source-form>)
    => (name :: false-or(<byte-string>))
  not-yet-implemented("source-form-compiled-lambda-symbolic-name");
end back-ended-function;


define back-ended-function compiled-lambda-symbolic-name-source-form
    (context :: dfmc-<library-description>, name :: <byte-string>)
    => (source-form :: false-or(<source-form>))
  not-yet-implemented("compiled-lambda-symbolic-name-source-form");
end back-ended-function;



/// source mapping



define defaulted-generic compiled-lambda-source-location
    (compiled-lambda :: <compiled-lambda>, code-offset :: <integer>,
     #key exact? = #t,
          line-only? = #t,
          interactive-only? = #t)
    => (source-location :: false-or(<source-location>),
        exact? :: <boolean>)
  values(#f, #f);
end defaulted-generic;



define defaulted-generic compiled-lambda-code-offset
    (compiled-lambda :: <compiled-lambda>, source-location :: <source-location>,
     #key exact? = #t,
          line-only? = #t,
          interactive-only? = #t)
    => (code-offset :: false-or(<integer>))
  #f;
end defaulted-generic;



define defaulted-generic compiled-lambda-mapped-source-locations
    (compiled-lambda :: <compiled-lambda>,
     #key line-only? = #t,
          interactive-only? = #t)
    => (locations :: <sequence>)
  #[]
end defaulted-generic;


define defaulted-generic compiled-lambda-mapped-code-offsets
    (compiled-lambda :: <compiled-lambda>,
     #key line-only? = #t,
          interactive-only? = #t)
    => (code-offsets :: <sequence>)
  #[]
end defaulted-generic;





/// local variables




define defaulted-generic local-variable-argument?
    (context :: dfmc-<library-description>, var :: <local-variable>)
    => (variable? :: <boolean>)
  #f;  // The conservative answer
end defaulted-generic;



define open generic local-variable-debug-name
    (context :: dfmc-<library-description>, var :: <local-variable>)
    => (name :: <byte-string>);




define back-ended-function local-variable-debug-name-dylan-name
    (context :: dfmc-<library-description>, symbolic-name :: <byte-string>)
    => (dylan-name :: <byte-string>)
  symbolic-name;
end back-ended-function;




define defaulted-generic local-variable-type
    (context :: dfmc-<library-description>, var :: <local-variable>)
    => (type)
  #t; // indicates canonical Dylan representation.
end defaulted-generic;



define open generic local-variable-location
    (context :: dfmc-<library-description>, var :: <local-variable>)
    => (base-reg :: <integer>, indirections :: <sequence>);


define defaulted-generic compiled-lambda-local-variables
    (compiled-lambda :: <compiled-lambda>, code-offset :: <integer>)
    => (local-variables :: <sequence>)
 #[];
end defaulted-generic;




define defaulted-generic compiled-lambda-frame-boundaries
    (compiled-lambda :: <compiled-lambda>)
    => (start-offsets :: <sequence>, stop-offsets :: <sequence>)
  values(#[], #[]);
end defaulted-generic;


/// components


define back-ended-function compilation-context-initializer-symbolic-name
    (context :: dfmc-<library-description>)
    => (symbolic-name :: <byte-string>, component-name :: <byte-string>)
  no-default-error("compilation-context-initializer-symbolic-name");
end back-ended-function;


define function compilation-context-library-name
    (context :: dfmc-<library-description>)
  => (component-name :: false-or(<byte-string>))
  let name = context.dfmc-library-description-emit-name;
  if (name) as-lowercase(as(<byte-string>, name)) else #f end;
end function;


define function settings-executable
    (#key executable = #f, #all-keys)
    => (executable :: false-or(<byte-string>))
  executable
end function;



define function compilation-context-component-name
    (context :: dfmc-<library-description>)
  => (component-name :: false-or(<byte-string>))
  apply(settings-executable, context.dfmc-library-description-build-settings)
    | context.compilation-context-library-name;
end function;


define function compilation-context-dylan-component-name
    (context :: dfmc-<library-description>)
  => (component-name :: <byte-string>)
  let dyl = context.dfmc-library-description-dylan-library;
  if (dyl)
    dyl.compilation-context-component-name;
  else "dylan";
  end if;
end function;


define function compilation-context-runtime-component-name
    (context :: dfmc-<library-description>)
  => (component-name :: <byte-string>)
  context.compilation-context-dylan-component-name;
end function;


