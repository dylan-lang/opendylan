Module:   dfmc-conversion
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Converters
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Switches.

// TODO: OBSOLETE?
// define variable *emit-spurious-temporary-transfers* = #f;

//// Hacks and stubs.

// Allow model objects to be inserted as "literals" into code.

define sideways method as-fragment (o :: <&top>)
  parsed-literal(o)
end method;

define inline function make-for-fragment-with-temporary
    (env :: <environment>, class :: <class>, fragment, #rest options)
 => (comp :: <computation>, temp :: false-or(<temporary>))
  apply(make-with-temporary, env, class,
        source-location: fragment-source-location(fragment),
        options)
end function;

// do a conservative check whether a reference is a constant.
// specifically, do not invoke the typist (which constant-value? does).
// (moved from optimization/constant-folding.dylan (gts, 10/17/97))

define inline function fast-constant-value?
    (ref) => (well? :: <boolean>, model)
  if (instance?(ref, <object-reference>))
    values(#t, reference-value(ref))
  elseif (instance?(ref, <temporary>)
            & instance?(ref.generator, <make-closure>))
    let m = computation-closure-method(ref.generator);
    if (^function-signature(m))
      values(#t, m)
    else
      values(#f, #f)
    end
  else
    values(#f, #f)
  end
end;

define inline function fast-constant-value
    (ref) => (model)
  let (well?, model) = fast-constant-value?(ref);
  model
end function;

// TODO: Both the following functions are hacks pending true source location
// information becoming available.

// Guess an approximate context.

define compiler-sideways method dfm-context-id (env :: <environment>) => (context-id-or-false)
  let path = #f;
  block (return)
    for (cursor = env then cursor.outer.lambda-environment,
         while: cursor & cursor.lambda)
      let debug-name = cursor.lambda.^debug-name;
      if (debug-name)
        let debug-string = as-lowercase(as(<string>, debug-name));
        path := if (path)
                  concatenate(debug-string, ".", path)
                else
                  debug-string
                end;
      end;
      /*
      let def? = cursor.lambda.model-has-definition?;
      if (def?)
        return(cursor.lambda.^debug-name);
      end;
      */
    finally
      path;
    end
  exception (<error>)
    #f;
  end;
end method;

define compiler-sideways method dfm-context-id
    (comp :: <computation>) => (context-id-or-false)
  dfm-context-id(comp.environment);
end method;

define method form-context-id (form)
  #f
end method;

define method form-context-id (form :: <variable-defining-form>)
  let names = form-variable-names(form);
  if (size(names) = 1)
    as-lowercase(fragment-name-string(names.first));
  else
    next-method();
  end;
end method;

define method model-context-id (object)
  #f
end method;

define method model-context-id (class :: <&class>)
  as-lowercase(as(<string>, class.^debug-name));
end method;

define method model-id (object)
  model-definition(object) | object
end method;

define method model-ids (objects)
  map(model-id, objects)
end method;

//// Syntax-independent (mostly) conversion tools

/// CONVERSION

define abstract class <value-context> (<object>) end;

define class <ignore-value-context> (<value-context>) end;
define class <single-value-context> (<value-context>) end;

define constant $ignore :: <value-context> = make(<ignore-value-context>);
define constant $single :: <value-context> = make(<single-value-context>);

define class <multiple-value-context> (<value-context>)
  slot mvc-properties :: <integer> = 0;
end class;

define inline function pack-any-to-bool(x) => (z :: <integer>)
  pack-boolean(~ ( ~ x))
end function;

define leaf packed-slots mvc-properties
    (<multiple-value-context>, <object>)
  eval slot mvc-rest? = #t,
    field-size: 1,
    pack-function: pack-any-to-bool,
    unpack-function: unpack-boolean;
  field slot mvc-num-values = 0,
    field-size: $max-number-values-field-size;
end packed-slots;

define method initialize
    (context :: <multiple-value-context>, #rest all-keys,
     #key mvc-num-values, mvc-rest? = #"not")
  next-method();
  apply(initialize-packed-slots, context, all-keys);
  if (mvc-num-values)
    context.mvc-num-values := mvc-num-values;
  end if;
  if (mvc-rest? ~== #"not")
    context.mvc-rest? := mvc-rest?;
  end if;
end method;

define method make-mvc-cache
    (rest? :: <boolean>) => (res :: <simple-object-vector>)
  let cache = make(<simple-object-vector>, size: $max-number-values);
  for (i from 0 below $max-number-values)
    cache[i]
      := make(<multiple-value-context>, mvc-num-values: i, mvc-rest?: rest?);
  end for;
  cache
end method;

define variable *mvc-caches-initialized?* = #f;

define sealed method make
    (class == <multiple-value-context>,
     #key mvc-rest? = #f, mvc-num-values = 0, #all-keys)
 => (res :: <multiple-value-context>)
  if (*mvc-caches-initialized?*)
    if (mvc-rest?)
      $rest-mvc-cache[mvc-num-values]
    else
      $mvc-cache[mvc-num-values]
    end if
  else
    next-method()
  end if
end method;

define constant $rest-mvc-cache :: <simple-object-vector> = make-mvc-cache(#t);
define constant $mvc-cache :: <simple-object-vector>      = make-mvc-cache(#f);

*mvc-caches-initialized?* := #t;

define method print-object(c :: <value-context>, s :: <stream>) => ()
  select (c by instance?)
    <ignore-value-context> => format(s, "$ignore");
    <single-value-context> => format(s, "$single");
    <multiple-value-context>
      => format(s, "(%d,%s)", mvc-num-values(c),
          if (mvc-rest?(c)) "#rest" else "" end);
  end select;
end method;

define generic context-num-values(c :: <value-context>) => (i :: <integer>);

define method context-num-values(c :: <multiple-value-context>) => (i :: <integer>)
  mvc-num-values(c);
end method;

define method context-num-values(c :: <single-value-context>) => (i :: <integer>)
  1
end method;

define method context-num-values(c :: <ignore-value-context>) => (i :: <integer>)
  0
end method;

define generic context-rest?(c :: <value-context>) => (ans :: <boolean>);

define method context-rest?(c :: <multiple-value-context>) => (ans :: <boolean>)
  mvc-rest?(c);
end method;

define method context-rest?(c :: <single-value-context>) => (ans :: <boolean>)
  #f
end method;

define method context-rest?(c :: <ignore-value-context>) => (ans :: <boolean>)
  #f
end method;

// define method make (class == <multiple-value-context>, #rest all-keys,
//                     #key has-rest?, num-values, initial? = #f)
//   if ((~ local?) & has-rest? & (num-values = 0) & (~ initial?))
//     $all-rest;
//   else
//     next-method()
//   end if;
// end method;

define constant $all-rest :: <multiple-value-context>
  = make(<multiple-value-context>, mvc-rest?: #t,
         mvc-num-values: 0);

define method do-convert
    (env :: <environment>, context :: <value-context>, object)
  convert(env, context, object)
end method;

define method do-convert
    (env :: <environment>, context :: <value-context>, f :: <fragment>)
  with-parent-fragment (f)
    convert(env, context, f)
  end;
end method;

define generic convert
    (env :: <environment>, context :: <value-context>, object)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>));

define generic convert-reference
    (env :: <environment>, context :: <value-context>, object, #key)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>));

/// MULTIPLE-VALUES

// object :: type-union(singleton(#f), <variable-reference>, <multiple-value-temporary>)
define generic match-values-with-context
    (env :: <environment>, context :: <value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>));

define method match-values-with-context
    (env :: <environment>, context :: <value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  format-out("XXXXX mvwc no matching specializers\n\tcontext=%=, obj=%=.\n", context, object);
  format-out("\t classes: context=%=, obj=%=.\n",
   object-class(context), object-class(object));
  format-out("\tfirst=%=, last=%=.\n", first, last);
  // break("bogus mvwc");
  values(first, last, object);
end method;

define function record-context-in-mvt!
    (context :: <value-context>, mvt :: <multiple-value-temporary>) => ()
  let (num-vals, rest?) =
    select (context by instance?)
      <ignore-value-context> => values(0, #f);
      <single-value-context> => values(1, #f);
      <multiple-value-context>
        => values(mvc-num-values(context), mvc-rest?(context));
  end select;
  mvt.required-values := num-vals;
  mvt.rest-values? := rest?
end function;

// GOT MULTIPLE

// ... and nothing is expected
define method match-values-with-context
    (env :: <environment>, context :: <ignore-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <multiple-value-temporary>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  record-context-in-mvt!(context, object);
  values(first, last, #f)
end method;

// ... and one thing is expected
define method match-values-with-context
    (env :: <environment>, context :: <single-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <multiple-value-temporary>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  record-context-in-mvt!(context, object);
  let (extract-c, extract-t)
    = make-with-temporary
        (last.environment, <extract-single-value>, value: object);
  join-2x1-t!(first, last, extract-c, extract-t);
end method;

// ... and multiple things are expected
define method match-values-with-context
    (env :: <environment>, context :: <multiple-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <multiple-value-temporary>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  record-context-in-mvt!(context, object);
  values(first, last, object);
end method;

// GOT NOTHING

// ... and nothing is expected
define inline method match-values-with-context
    (env :: <environment>, context :: <ignore-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object == #f)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  values(first, last, #f)
end method;

// ... and one thing is expected
define method match-values-with-context
    (env :: <environment>, context :: <single-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object == #f)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  // return ^#f
  values(first, last, make-object-reference(#f))
end method;

// ... and multiple things are expected
define method match-values-with-context
    (env :: <environment>, context :: <multiple-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object == #f)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  let mv = pad-multiple-values(env, context);  // fill with #f
  let (values-c, values-t) = convert-values(env, mv, #f);
  join-2x1-t!(first, last, values-c, values-t);
end method;

define function pad-multiple-values
    (env :: <environment>, context :: <value-context>,
     #rest references)
 => (res :: <simple-object-vector>)
  let required  = context-num-values(context);
  let rest?     = context-rest?(context);
  let max-size  = max(required, if (rest?) size(references) else 0 end);
  let mv :: <simple-object-vector> = make(<vector>, size: max-size);
  for (i :: <integer> from 0 below max-size, ref in references)
    mv[i] := ref;
  finally
    for (j :: <integer> from i below max-size)
      mv[i] := make-object-reference(#f);
    end for;
  end for;
  mv
end function;

/// GOT SINGLE VALUE

// ... and nothing is expected

define inline method match-values-with-context
    (env :: <environment>, context :: <ignore-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <value-reference>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>));
  values(first, last, #f)
end method;

// ... and one thing is expected
define inline method match-values-with-context
    (env :: <environment>, context :: <single-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <value-reference>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>));
  // how convenient
  values(first, last, object)
end method;

// ... and multiple things are expected
define inline method match-values-with-context
    (env :: <environment>, context :: <multiple-value-context>,
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: <value-reference>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>));
  let mv = pad-multiple-values(env, context, object);  // pad with #f
  let (values-c, values-t) = convert-values(env, mv, #f);
  join-2x1-t!(first, last, values-c, values-t);
end method;

/// TEMPORARY-VALUE-CONTEXT

define method temporary-value-context
    (tmp :: <temporary>) => (res :: <value-context>)
 $single
end method;

define method temporary-value-context
    (tmp :: <multiple-value-temporary>) => (res :: <value-context>)
  make(<multiple-value-context>,
       mvc-num-values: required-values(tmp),
       mvc-rest?: rest-values?(tmp));
end method;

define method temporary-value-context
    (tmp == #f) => (res :: <value-context>)
 $ignore
end method;

define inline function match-values-with-temporary
    (env :: <environment>, temporary :: false-or(<temporary>),
     first :: false-or(<computation>), last :: false-or(<computation>),
     object :: false-or(<value-reference>))
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     temp :: false-or(<value-reference>))
  match-values-with-context
    (env, temporary-value-context(temporary), first, last, object)
end function;

define inline function convert-value-reference
    (env :: <environment>, context :: <value-context>, object,
     class :: subclass(<value-reference>))
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let value = if (context == $ignore) #f else object end;
  let object = make-compile-time-literal(value);
  let reference = make(class, value: mapped-model(object));
  add-user!(object, reference);
  match-values-with-context
    (env, context, #f, #f, reference);
end;

define inline function convert-binding-value-reference
    (env :: <environment>, context :: <value-context>, b :: <binding>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let reference = make(<defined-constant-reference>, value: b);
  match-values-with-context
    (env, context, #f, #f, reference);
end;

define inline method convert-object-reference
    (env :: <environment>, context :: <value-context>, object)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-value-reference(env, context, object, <object-reference>)
end method;

define inline function convert-object-reference-1
    (env :: <environment>, object)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  convert-object-reference(env, $single, object)
end function;

define inline function make-object-reference
    (object) => (ref :: <value-reference>)
  let (ignore-first, ignore-last, ref)
    = convert-object-reference-1($top-level-environment, object);
  ref
end function;

define inline function make-value-reference
    (object, ref-class :: <class>) => (ref :: <value-reference>)
  let (ignore-first, ignore-last, ref)
    = convert-value-reference($top-level-environment, $single, object, ref-class);
  ref
end function;

define inline function convert-method-reference
    (env :: <environment>, context :: <value-context>, object :: <&method>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-value-reference(env, context, object, <method-reference>)
end function;

define method convert-reference
    (env :: <environment>, context :: <value-context>, object, #key)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-object-reference(env, context, object)
end method;

// TODO: MIGHT NOT BE NEEDED -- MORE OF A STOP-GAP FIRE-WALL

define method convert-reference
    (env :: <environment>, context :: <value-context>,
     object :: <&lambda-or-code>, #key)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-method-reference(env, context, object)
end method;

define serious-program-warning <undefined-binding-reference>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Reference to undefined binding %=.";
  format-arguments variable-name;
end serious-program-warning;

define thread variable *generating-undefined-reference-warning* = #f;

define method convert-reference
    (env :: <environment>, context :: <value-context>,
     var :: <module-binding>, #key fragment)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
       temp :: false-or(<value-reference>))
  if (binding-definition-missing?(var)
        & ~*generating-undefined-reference-warning*)
    note(<undefined-binding-reference>,
         source-location: fragment & fragment-source-location(fragment),
         context-id:      dfm-context-id(env),
         variable-name:   var);
    let binding-name = as(<string>, fragment | binding-identifier(var));
    let restart-message = "Retry reference to binding \"%s\".";
    let error-message = "Reference to undefined binding \"%s\".";
    let (check-first, check-last, check-t)
      = dynamic-bind (*generating-undefined-reference-warning* = #t)
          do-convert(env, $single,
                     #{ while (?var == ?&unbound)
                          cerror(?restart-message, ?error-message,
                                 ?binding-name);
                        end;
                        ?var });
        end;
    match-values-with-context
      (env, context, check-first, check-last, check-t);
  else
    let (model-object, found?) =
      // TODO: DONT REGISTER DEPENDENCIES IF CONTEXT = $ignore
      binding-constant-model-object(var, error-if-circular?: #f);
    let (inlineable?, inline-object) = inlineable?(model-object);
    // TODO: check inlining before doing this.
    if (found? & inlineable?)
      convert-object-reference(env, context, inline-object);
    elseif (constant?(var))
      convert-binding-value-reference(env, context, var)
    else
      if (context == $ignore)
        convert-object-reference(env, context, #f)
      else
        let (computation, temporary)
          = make-with-temporary(env, <variable-reference>, value: var);
        add-user!(var, computation);
        match-values-with-context
          (env, context, computation, computation, temporary);
      end if
    end
  end
end method;

define method convert-reference
    (env :: <environment>, context :: <value-context>,
     binding :: <interactor-binding>, #key fragment)
 => (first :: false-or(<computation>), last :: false-or(<computation>), temp :: false-or(<value-reference>))
  let reference = make(<interactor-binding-reference>, value: binding);
  match-values-with-context
    (env, context, #f, #f, reference);
end method;

define method convert-reference
    (env :: <environment>, context :: <value-context>,
     ref :: <value-reference>, #key fragment)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  if (context == $ignore)
    convert-object-reference(env, context, #f)
  else
    match-values-with-context
      (env, context, #f, #f, ref);
  end if
end method;

define serious-program-warning <omitted-marker-reference>
  format-string "Invalid use of the \"omitted\" marker _";
end serious-program-warning;

define serious-program-warning <macro-value-reference> (<manual-parser-error>)
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Reference to the macro %= as a run-time value.";
  format-arguments variable-name;
end serious-program-warning;

define method convert-reference
    (env :: <environment>, context :: <value-context>,
     name :: <variable-name-fragment>, #key)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  // TODO: DONT REGISTER DEPENDENCIES IF CONTEXT = $ignore
  if (omitted-fragment?(name))
    note(<omitted-marker-reference>,
         source-location: fragment-source-location(name));
    // Doesn't return
  end;
  let (binding, type, found-env) = lookup(env, name);
  if (binding
        & instance?(binding, <module-binding>)
        & instance?(binding-definition(binding, default: #f),
                      <expander-defining-form>))
    /*
    note(<macro-value-reference>,
         source-location: fragment-source-location(name),
         context-id:      dfm-context-id(env),
         variable-name:   name);
    convert-error-call(env, context,
                       concatenate("Reference to macro \"",
                                   as(<string>, name),
                                   "\" as a run-time value."))
    */
    convert-using-definition
      (env, context, binding-definition(binding), name);
  else
    when (found-env) // NOT TOP LEVEL ENVIRONMENT
      let lambda-env = lambda-environment(env);
      when (lambda-env ~== lambda-environment(found-env))
        lambda-has-free-references?(lambda(lambda-env)) := #t;
      end when;
    end when;
    if (found-env
          & instance?(binding, <object-reference>)
          & instance?(reference-value(binding), <&macro>))
      format-out("Local reference macro.");
      let expander
        = macro-expander-function
            (expander-macro-object(reference-value(binding)));
      let expansion = expander(env, name);
      do-convert(env, context, expansion);
    else
      convert-reference(env, context, binding, fragment: name)
    end;
  end
end method;

// for internal references
define inline function convert-global-reference
    (env :: <environment>, name :: <variable-name-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>);
  convert-reference(env, $single, lookup-binding(name))
end function;

define inline function make-global-reference
    (name :: <variable-name-fragment>) => (ref :: <value-reference>);
  let (ignore-first, ignore-last, ref)
    = convert-global-reference($top-level-environment, name);
  ref
end function;

define inline function convert-dylan-reference
    (name :: <name>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>);
  convert-object-reference-1($top-level-environment, dylan-value(name))
end function;

define inline function make-dylan-reference
    (name :: <name>) => (ref :: <value-reference>);
  let (ignore-first, ignore-last, ref)
    = convert-dylan-reference(name);
  ref
end function;

// multiple values

define method convert-1 (env :: <environment>, code)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  do-convert(env, $single, code);
end method convert-1;

define function extract-single-value
    (first :: false-or(<computation>), last :: false-or(<computation>), t :: <temporary>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  if (t.multiple-values?)
    let (extract-c, extract-t)
      = make-with-temporary(last.environment, <extract-single-value>,
                            value: t);
    if (t.generator == last)
      join-2x1-t!(first, last, extract-c, extract-t)
    else
      insert-computation-after!(t.generator, extract-c);
      values(first, last, extract-t);
    end if;
  else
    values(first, last, t)
  end if
end function;

define method convert-values
    (env :: <environment>,
     required :: <simple-object-vector>, rest :: false-or(<value-reference>))
 => (values-c :: <values>, mv-t :: <multiple-value-temporary>)
  let (values-c, mv-t)
    = make-with-temporary(env, <values>,
                          values:          required,
                          rest-value:      rest,
                          temporary-class: <multiple-value-temporary>);
  mv-t.required-values := size(required);
  mv-t.rest-values?    := rest ~== #f;
  values(values-c, mv-t)
end method convert-values;

// more syntax-independent tools

define function convert-expressions
    (env :: <environment>, argument-forms :: <sequence>,
     #key form-extractor = identity, form-converter = convert-1)
 => (first :: false-or(<computation>), last :: false-or(<computation>), temporaries)
  let n-args = size(argument-forms);
  let temporaries = make(<vector>, size: n-args);
  let first = #f;
  let last  = #f;
  for (argument in argument-forms, i from 0)
    let (arg-first, arg-last, arg-temporary) =
      form-converter(env, form-extractor(argument));
    temporaries[i] := arg-temporary;
    let (_first, _last) = join-2x2!(first, last, arg-first, arg-last);
    first := _first;
    last  := _last;
  end for;
  values(first, last, temporaries)
end;

define method primitive-temporary-class (primitive :: <&primitive>)
  let signature
    = primitive-signature(primitive);
  if (^signature-rest-value(signature))
    <multiple-value-temporary>
  else
    select (^signature-number-values(signature))
      0         => #f;
      1         => <temporary>;
      otherwise => <multiple-value-temporary>;
    end
  end
end method;

define method primitive-temporary-class (primitive)
  <temporary>
end method;

define function convert-primitive-call
    (env :: <environment>, context :: <value-context>,
     class :: subclass(<primitive-call>), primitive, argument-forms)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  let (arg-first, arg-last, temporaries)
    = convert-expressions(env, argument-forms);
  let temporary-class
    = primitive-temporary-class(primitive);
  let (call, temporary)
    = make-with-temporary
        (env, class,
         temporary-class: temporary-class,
         primitive: primitive,
         arguments: temporaries);
  if (instance?(temporary, <multiple-value-temporary>))
    record-context-in-mvt!(context, temporary);
  end if;
  let (f-c, l-c) = join-2x1!(arg-first, arg-last, call);
  match-values-with-context
    (env, context, f-c, l-c, temporary)
end;

define program-warning <operator-not-a-function>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Operator not a function: %=.";
  format-arguments variable-name;
end program-warning;

define program-warning <function-call-result-not-used>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "The result of this call to %s is not used.";
  format-arguments variable-name;
end program-warning;

define inline function side-effect-free-function? // used once
    (value :: <&function>) => (well? :: <boolean>)
  // Just = for now, since this is such a common special case. Note that we
  // do this test here because we don't currently have a good way to
  // distinguish real not-used cases from "discovered" not-used cases
  // mid-optimization.
  value == dylan-value(#"=")
end function;

define inline function unused-value-context? // used once
    (context :: <value-context>) => (well? :: <boolean>)
  // This doesn't seem to get canonicalized as you might hope...
  context == $ignore
    | (instance?(context, <multiple-value-context>)
         & mvc-num-values(context) == 0
         & ~mvc-rest?(context))
end function;

define function convert-function-call
    (env :: <environment>, context :: <value-context>,
     class :: subclass(<function-call>), form, function-form, argument-forms,
     #key temporary-class = context-temporary-class(context), typecheck-function? = #t)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  let (first, function-last, function) = convert-1(env, function-form);

  // assure callable at run-time
  let (cv?, value) = fast-constant-value?(function);   // e.g. function :: <method-reference>

  let (first, last, fun-temp) =
    if (typecheck-function?)
      if (cv?)
        if (~instance?(value, <&function>))
          note(<operator-not-a-function>,
               source-location: fragment-source-location(function-form),
               context-id:      dfm-context-id(env),
               variable-name:   function);
          let (error-first, error-last, error-t)
            = convert-error-call(env, context,
                                 format-to-string("Operator is not a function: \"%=\".", function));
          join-2x2-t!(first, function-last,
                      error-first, error-last, error-t);
        else
          if (unused-value-context?(context)
                & side-effect-free-function?(value))
            note(<function-call-result-not-used>,
                 source-location: fragment-source-location(function-form),
                 context-id:      dfm-context-id(env),
                 variable-name:   value.model-variable-name);
          end;
          values(first, function-last, function);
        end if;
      else  // if not constant, must check dynamically
        let function-type-temp =
          make-object-reference(dylan-value(#"<function>"));
        let (check-c, check-t) =
          make-with-temporary
            (env, <check-type>,
             value: function , type: function-type-temp);
        join-2x1-t!(first, function-last, check-c, check-t);
      end if;
    else
      values(first, function-last, function);
    end if;

  let (arg-first, arg-last, temporaries)
    = convert-expressions(env, argument-forms);
  let (first, last) = join-2x2!(first, last, arg-first, arg-last);
  let (call, temporary)
    = make-for-fragment-with-temporary
        (env, class, form,
         temporary-class: temporary-class,
         function:        fun-temp,
         arguments:       temporaries);
//  join-2x1-t!(f-c, l-c, call, temporary);
  let (first, last, temporary) = join-2x1-t!(first, last, call, temporary);
  match-values-with-context
    (env, context, first, last, temporary);
end;

define function convert-error-call
    (env :: <environment>, context :: <value-context>, error-string)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  // avoid infinite recurse if "error" is undefined somehow.
  let function  = make-object-reference(dylan-value(#"error"));
  let temporary = make-object-reference(error-string);
  let (call, temporary)
    = make-with-temporary(env, <simple-call>,
                          temporary-class: context-temporary-class(context),
                          function: function,
                          arguments: vector(temporary));
  if (instance?(temporary, <multiple-value-temporary>))
    record-context-in-mvt!(context, temporary);
  end if;
  match-values-with-context
    (env, context, call, call, temporary);
end;

define method convert-body
    (env :: <environment>, context :: <value-context>, body)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-body(env, context, as-body(body))
end method;

define method convert-body
    (env :: <environment>, context :: <value-context>, body :: <body-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-body(env, context, body.fragment-constituents);
end method;

define method convert-body
    (env :: <environment>, context :: <value-context>, forms :: <empty-list>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-object-reference(env, context, &false)
end method;

define method convert-body
    (env :: <environment>, context :: <value-context>, forms :: <pair>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     ref :: false-or(<value-reference>))
  iterate grovel (first = #f, last = #f, forms = forms)
    if (empty?(forms.tail))
      let (f, l, r) = do-convert(env, context, forms.head);
      let (f2, l2) = join-2x2-t!(first, last, f, l, r);
      match-values-with-context
        (env, context, f2, l2, r); /* gts, 7/24/97 */
    else
      let (f, l) = do-convert(env, $ignore, forms.head);
      let (first, last) = join-2x2!(first, last, f, l);
      grovel(first, last, tail(forms))
    end if;
  end iterate;
end method convert-body;

define method convert-begin-1
    (env :: <environment>, context :: <value-context>, body, after)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let (body-first, body-last, temporary) = do-convert(env, context, body);
  if (instance?(context, <multiple-value-context>))
    let (spill, spill-temporary)
      = make-with-temporary
      (env, <multiple-value-spill>, value: temporary);
    let (f-c, l-c) = join-2x1!(body-first, body-last, spill);
    let (after-first, after-last) = do-convert(env, $ignore, after);
    let (f-c, l-c) = join-2x2!(f-c, l-c, after-first, after-last);
    let (unspill, unspill-temporary)
      = make-with-temporary
      (env, <multiple-value-unspill>, value: spill-temporary,
       temporary-class: <multiple-value-temporary>);
    join-2x1-t!(f-c, l-c, unspill, unspill-temporary);
  else
    let (after-first, after-last) = do-convert(env, $ignore, after);
    join-2x2-t!(body-first, body-last, after-first, after-last, temporary);
  end
end method;

define method context-temporary-class (context :: <ignore-value-context>)
  #f
end method;

define method context-temporary-class (context :: <single-value-context>)
  <temporary>
end method;

define method context-temporary-class (context :: <multiple-value-context>)
  <multiple-value-temporary>
end method;

define method convert-if
    (env :: <environment>, context :: <value-context>,
     test-clause, then-clause, else-clause)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  let (test-first, test-last, test-t) = convert-1(env, test-clause);
  let (then-first, then-last, then-t)
    = do-convert(env, context, then-clause);
  let (else-first, else-last, else-t)
    = do-convert(env, context, else-clause);
  let merge-t-class = context-temporary-class(context);
  let if-c = make-in-environment
               (env, <if>,
                test: test-t,
                consequent:  then-first,
                alternative: else-first);
  let test-first = join-2x1!(test-first, test-last, if-c);

  let (then-t, else-t)
    = if (context == $ignore) values(#f, #f) else values(then-t, else-t) end if;

  if (then-first)
    previous-computation(then-first) := if-c;
  end if;
  if (else-first)
    previous-computation(else-first) := if-c;
  end if;

  let (merge, temporary)
    = make-with-temporary
        (env, <if-merge>,
         temporary-class: merge-t-class,
         previous-computation: if-c,
         left-previous-computation:  then-last | if-c,
         right-previous-computation: else-last | if-c,
         left-value:  then-t,
         right-value: else-t);
  if (instance?(temporary, <multiple-value-temporary>))
    record-context-in-mvt!(context, temporary);
  end if;
  next-computation(if-c) := merge;
  if (then-last)
    next-computation(then-last) := merge;
  end if;
  consequent(if-c) := then-first | merge;
  if (else-last)
    next-computation(else-last) := merge;
  end if;
  alternative(if-c) := else-first | merge;

  match-values-with-context
     (env, context, test-first, merge, temporary); /* gts, 7/24/97 */
end method convert-if;

define method convert-block
    (env :: <environment>, context :: <value-context>,
     class :: subclass(<block>), name, form)
 => (block-c :: <computation>, block-t :: <temporary>,
     body-first :: false-or(<computation>),
     body-last :: false-or(<computation>),
     body-t :: type-union(<object-reference>, false-or(<temporary>)),
     entry-state :: <entry-state>)
  let (block-c, block-t, es, body-env)
    = convert-entry-state(env, context, class, name);
  let (body-first, body-last, body-t) =
    do-convert(body-env, context, form);
  values(block-c, block-t, body-first, body-last, body-t, es)
end method convert-block;

define method convert-entry-state
    (env :: <environment>, context :: <value-context>,
     class :: subclass(<block>), name)
 => (block-c :: <computation>, block-t :: <temporary>,
     entry-state :: <entry-state>, new-env :: <environment>)

  // TODO: treat the context cases

  let lambda-env = lambda-environment(env);
  let entry-state
    = make-in-environment
        (env, <entry-state>, name: name, value: #f);
  let (block-c, block-t)
    = make-with-temporary
        (env, class, entry-state: entry-state,
         temporary-class: <multiple-value-temporary>);
  record-context-in-mvt!(context, block-t);
  entry-state.generator := block-c;
  entry-state.me-block := block-c;
  lambda-env.entries := add!(lambda-env.entries, entry-state);
  let new-env = if (name)
                  make-local-lexical-environment
                    (name, entry-state, #f, env)
                else
                  env
                end if;
  values(block-c, block-t, entry-state, new-env)
end method convert-entry-state;

// TODO:  Now there _is_ a separate environment, so this code can go away.

// !@#$ SHOULD BE IN UNIT
// !@#$ THIS MIGHT JUST BE BETTER ACCOMPLISHED BY HAVING SEPARATE ENV
// !@#$ BUT CURRENTLY THAT DOESN'T WORK BECAUSE OF BROKEN ENV MERGING CODE

define thread variable *bind-exit-name-counter* = 0;

// TODO: CORRECTNESS: What is the hygiene context of this generated variable?

define method make-exit-name (name)
  let exit-string
    = format-to-string
        ("%s%%exit-%d", name.fragment-identifier, *bind-exit-name-counter*);
  *bind-exit-name-counter* := *bind-exit-name-counter* + 1;
  make-in-expansion
    (<variable-name-fragment>, name: as(<symbol>, exit-string))
end method make-exit-name;

define method convert-bind-exit
    (env :: <environment>, context :: <value-context>, name, body)

  // TODO: tighten up temporaries based on context

  let exit-name = make-exit-name(name);
  //  = as-name(#{ "%exit-" ## ?name }); // TODO: hidden hygiene context
  let (block-c, block-t, body-first, body-last, body-t, es)
    = convert-block
        (env, $all-rest, <bind-exit>, exit-name,
         #{ begin
              local method ?name (#rest exit-values)
                      %inlineable();
                      %dynamic-extent(exit-values);
                      %return-from(?exit-name, exit-values);
                    end method;
              ?body
            end });
  let end-c
    = make-in-environment(env, <end-exit-block>, entry-state: es);

  let body-first = join-2x1!(body-first, body-last, end-c);

  block-c.body := body-first;
  body-first.previous-computation := block-c;

  let (merge-c, merge-t)
    = make-with-temporary
        (env, <bind-exit-merge>,
         left-value: block-t,
         left-previous-computation: block-c,
         right-value: body-t,
         right-previous-computation: body-last,
         temporary-class: <multiple-value-temporary>);
  record-context-in-mvt!(context, merge-t);
  block-c.next-computation     := merge-c;
  merge-c.previous-computation := block-c;
  match-values-with-context
    (env, context, block-c, merge-c, merge-t)
end method convert-bind-exit;

define method convert-return-from
    (env :: <environment>, context :: <value-context>, exit-name, argument)
  let entry-state = lookup(env, exit-name);
  let (value-first, value-last, value-temporary) = convert-1(env, argument);
  let (values-c, mv-t) = convert-values(env, #[], value-temporary);
  record-context-in-mvt!(context, mv-t);
  let value-first = join-2x1!(value-first, value-last, values-c);
  let (exit, exit-temporary) // exit-temporary is vestigial
    = make-with-temporary
        (env, <exit>,
         entry-state: entry-state, value: mv-t,
         temporary-class: <multiple-value-temporary>);
  record-context-in-mvt!(context, exit-temporary);
  add-user!(entry-state, exit);
  entry-state.exits := add!(entry-state.exits, exit);
  join-2x1-t!(value-first, values-c, exit, exit-temporary);
end method convert-return-from;

define method convert-unwind-protect
    (env :: <environment>, context :: <value-context>,
     protected-forms, cleanup-forms)
  let (block-c, block-t, body-first, body-last, body-t, es)
    = convert-block(env, context, <unwind-protect>, #f, protected-forms);
  let end-protected
    = make-in-environment(env, <end-protected-block>, entry-state: es);
  let body-first = join-2x1!(body-first, body-last, end-protected);

  block-c.body := body-first;
  body-first.previous-computation := block-c;

  let (cleanup-first, cleanup-last) = convert-body(env, $ignore, cleanup-forms);
  // (gts,98feb12) block-c.protected-temporary := body-t;
  block-c.protected-end := end-protected;   // gts,98feb12
  end-protected.return-temp := body-t;     // new (gts,98feb04)
  add-user!(body-t, end-protected);  // was: add-user!(body-t, block-c); (gts,98feb04)
  let end-cleanup
    = make-in-environment(env, <end-cleanup-block>, entry-state: es);
  let cleanup-first = join-2x1!(cleanup-first, cleanup-last, end-cleanup);
  block-c.cleanups := cleanup-first;
  block-c.cleanups-end := end-cleanup;
  cleanup-first.previous-computation := block-c;
  replace-temporary-in-users!(block-t, body-t);
  values(block-c, block-c, body-t)
end method convert-unwind-protect;

define method record-binding-assignment (temp :: <temporary>, c)
  temp.assignments := add-new!(temp.assignments, c);
end method;

define method record-binding-assignment (binding :: <module-binding>, c)
  // binding.binding-assignments := add-new!(binding.binding-assignments, c);
end method;

define program-warning <assignment-to-own-value>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string
    "Suspicious assignment %s := %s of a variable to its own value.";
  format-arguments variable-name, variable-name again;
end program-warning;

define program-warning <wrong-type-in-assignment>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-type,
    required-init-keyword: type:;
  slot condition-rhs,
    required-init-keyword: rhs:;
  format-string    "Illegal assignment of variable \"%=\" of type %= to %=.";
  format-arguments variable-name, type, rhs;
end program-warning;

define method convert-dynamic-assignment-check
    (env :: <environment>, context :: <value-context>,
       binding :: <binding>, the-name, value-temp)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
       ref :: false-or(<value-reference>))
  // Punt.
  values(#f, #f, value-temp)
end method;

/*
define method convert-dynamic-assignment-check
    (env :: <environment>, context :: <value-context>,
       binding :: <module-binding>, the-name, value-temp)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
       ref :: false-or(<value-reference>))
  let type-temp
    = make(<type-reference>, binding: binding);
  let (c, t)
    = make-with-temporary
        (env, <assignment-check-type>,
         value: value-temp , type: type-temp,
         lhs-variable-name: the-name);
  values(c, c, t);
end method;
*/

define method convert-assignment-with-binding
    (env :: <environment>, context :: <value-context>, binding :: <binding>,
       the-name, the-value, fragment)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  ignore(fragment);
  let (value-first, value-last, value-temp) = convert-1(env, the-value);
  if (value-temp == binding)
    note(<assignment-to-own-value>,
         source-location: fragment-source-location(fragment),
         // context-id: dfm-context-id(env),
         variable-name: the-name);
  end if;
  let (first, last, temp, ok?)
    = values(value-first, value-last, value-temp, #t);

  // check type of rhs
  // STILL TO DO:  add support for dynamic types.  Currently, only constant
  //   types are dealt with.

  let binding-type = binding-type(binding);

  let (check-first, check-last, check-temp)
    = if (binding-type)        // if lhs has a (static) type constraint
        let (rhs-constant?, rhs-value) = fast-constant-value?(value-temp);
        if (rhs-constant?)
          if (^instance?(rhs-value, binding-type))
            values(#f, #f, temp);
          else
            note(<wrong-type-in-assignment>,
                 variable-name: the-name,
                 type: binding-type,
                 rhs: rhs-value,
                 source-location: fragment-source-location(fragment));
            ok? := #f;
            convert-error-call
              (env, context,
               format-to-string("Illegal assignment of variable \"%=\" of type %= to %=.",
               the-name, binding-type, rhs-value))
          end if;
        elseif (inlineable?(binding-type))// if rhs not a constant, gen. dynamic type check
          let (type-first, type-last, type-temp)
            = convert-reference(env, $single, binding-type);
          let (c, t)
            = make-with-temporary
                (env, <assignment-check-type>,
                 value: value-temp , type: type-temp,
                 lhs-variable-name: the-name);
          join-2x1-t!(type-first, type-last, c, t);
        else
          // give up here -- need a way to do checks of dynamic types.
          convert-dynamic-assignment-check
            (env, context, binding, the-name, value-temp);
        end if;
      else  // if no type constraint
        convert-dynamic-assignment-check
          (env, context, binding, the-name, value-temp);
      end if;

  let (first, last, temp)
    = join-2x2-t!(first, last, check-first, check-last, check-temp);

  let (assignment-c, temp)
    = if (ok?)
        let (c, t)
          = make-with-temporary
              (env, <set!>, binding: binding, value: temp);
        record-binding-assignment(binding, c);
        values(c, t);
      else
        values(#f, temp);
      end if;
  let (first, last)
    = join-2x1!(first, last, assignment-c);
  match-values-with-context
    (env, context, first, last, temp);
end method;

// should this go in dfmc/namespace/binding.dylan?

define generic binding-type (b :: <binding>) => (f-or-type :: false-or(<&type>));

define method binding-type (b :: <binding>) => (f-or-type :: false-or(<&type>))
  #f;
end method;

define method binding-type (b :: <module-binding>) => (f-or-type :: false-or(<&type>))
  let (model, found?) = binding-constant-type-model-object(b);
  if (found?) model else #f end if;
end method;

define method binding-type (b :: <lexical-specialized-variable>) => (f-or-type :: false-or(<&type>))
  specializer(b)
end method;

define program-warning <undefined-binding-assignment>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Assignment to undefined binding %=.";
  format-arguments variable-name;
end program-warning;

define program-warning <constant-binding-assignment>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Assignment to constant binding %=.";
  format-arguments variable-name;
end program-warning;

define method convert-assignment-with-binding
    (env :: <environment>, context :: <value-context>,
     variable :: <module-binding>, the-name, the-value, fragment)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  if (defined?(variable))
    if (~constant?(variable))
      next-method()
    else
      note(<constant-binding-assignment>,
           source-location: fragment-source-location(fragment),
           context-id:      dfm-context-id(env),
           variable-name:   variable);
      convert-error-call(env, context,
                         concatenate("Assignment to constant binding \"",
                                     as(<string>, the-name),
                                     "\"."))
    end
  else
    note(<undefined-binding-assignment>,
         source-location: fragment-source-location(fragment),
         context-id:      dfm-context-id(env),
         variable-name:   variable);
    convert-error-call(env, context,
                       concatenate("Assignment to undefined binding \"",
                                   as(<string>, the-name),
                                   "\"."))
  end;
end method;

define serious-program-warning <interactor-binding-assignment>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Assignment to the constant interactor binding %=.";
  format-arguments variable-name;
end serious-program-warning;

define method convert-assignment-with-binding
    (env :: <environment>, context :: <value-context>,
     variable :: <interactor-binding>, the-name, the-value, fragment)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  note(<interactor-binding-assignment>,
       source-location: fragment-source-location(fragment),
       context-id:      dfm-context-id(env),
       variable-name:   variable.name);
  convert-error-call
    (env, context, concatenate("Assignment to interactor binding \"",
                               as(<string>, the-name), "\"."));
end method;

define method convert-assignment
    (env :: <environment>, context :: <value-context>,
     the-name, the-value, source-location)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  convert-assignment-with-binding
    (env, context, lookup(env, the-name), the-name, the-value, source-location);
end method;

define function parse-parameters-into-d
    (env :: <environment>, lambda-env :: <lambda-lexical-environment>, sig-spec)
  let required-specs
    = spec-argument-required-variable-specs(sig-spec);
  let key-specs
    = spec-argument-key-variable-specs(sig-spec);
  let spec-rest?
    = spec-argument-rest?(sig-spec);
  let spec-key?
    = if (spec-argument-key?(sig-spec)) #t else #f end;
  let keys-start
    = size(required-specs)
        + if (spec-rest? | spec-key?) 1 else 0 end;
  let variables
    = make(<simple-object-vector>, size: keys-start + size(key-specs));
  let var-index
    = 0;
  local method add-to-variables!(variable)
          variables[var-index] := variable;
          var-index := var-index + 1;
          variable
        end,
        method push-variable! (name, variable)
          add-to-variables!(add-variable!(lambda-env, name, variable));
        end,
        method insert-rest-variable! (name)
          push-variable!(name,
                         make(<lexical-rest-variable>,
                              name: name,
                              environment: lambda-env));
        end;
  let object-type = dylan-value(#"<object>");
  for (var-spec in required-specs)
    let name = spec-variable-name(var-spec);
    push-variable!(name,
                   make(<lexical-required-variable>,
                        name: name,
                        environment: lambda-env,
                        specializer: object-type));
  end;
  if (spec-rest?)
    insert-rest-variable!
      (spec-variable-name(spec-argument-rest-variable-spec(sig-spec)));
  elseif (spec-key?)
    insert-rest-variable!
      (implicit-rest-variable-name());
  end;
  for (key-spec in key-specs)
    add-to-variables!(make(<lexical-keyword-variable>,
                           name: spec-variable-name(key-spec),
                           environment: lambda-env));
  end;
  values(variables, keys-start)
end;

// TODO: CORRECTNESS: What hygiene context?

define method implicit-rest-variable-name ()
  make-in-expansion(<variable-name-fragment>, name: #"_rest");
end method;

define function bind-local-variable (env :: <environment>,
                                     name :: <variable-name-fragment>,
                                     type)
 => (new-env :: <local-lexical-environment>,
     variable :: <lexical-local-variable>)
  let type-value = if (instance?(type, <value-reference>))
                     fast-constant-value(type)
                   else
                     type
                   end;
  let variable = make(<lexical-local-variable>,
                      name: name,
                      specializer: instance?(type-value, <&type>) &
                                     type-value,
                      environment: lambda-environment(env));
  values(make-local-lexical-environment(name, variable,        type, env),
         variable)
end;

define function make-key-variable-specifiers-vector
    (env :: <environment>, specs :: <variable-specs>)
 => (vector :: <simple-object-vector>, shared-keys :: <object-table>)
  let key-counts = make(<object-table>);
  for (spec in specs, index :: <integer> from 0)
    let key = &eval(env, spec-keyword-expression(spec));
    let entry = element(key-counts, key, default: #f);
    if (entry)
      entry.tail := entry.tail + 1;
    else
      element(key-counts, key) := pair(index, 1);
    end;
  end;
  values(make(<vector>, size: size(specs) * 2), key-counts);
end function;

define function convert-keyword-initialization-d
    (env :: <environment>,
     function-t :: <temporary>,
     keys-first :: false-or(<computation>),
     keys-last :: false-or(<computation>),
     key-spec :: <key-variable-spec>,
     key :: <symbol>,
     variable :: <lexical-keyword-variable>,
     shared? :: <boolean>, // shared with others with the same key
     input? :: <boolean>,  // the input of the shared set
     specifiers :: <simple-object-vector>,
     index :: <integer>)
 => (new-env :: <local-lexical-environment>,
     new-keys-first :: false-or(<computation>),
     new-keys-last :: false-or(<computation>))
  let j = index * 2;
  specifiers[j] := key;
  let name = spec-variable-name(key-spec);
  let default-expression = spec-default-expression(key-spec);

  let (type-first, type-last, type-temp) =
    convert-key-type(env, function-t, index);
  let type = fast-constant-value(type-temp);
  let static-type = instance?(type, <&type>) & type;

  let (key-literal?, key-literal-value) =
    if (instance?(default-expression, <literal-constant-fragment>))
      let key-literal-value =
        make-compile-time-literal(fragment-value(default-expression));
      if (static-type & ^instance?(key-literal-value, static-type))
        values(#t, key-literal-value)
      end;
    end;
  let (new-env, new-keys-first, new-keys-last, defaulted-value) =
    if (~shared? & key-literal?)
      specifiers[j] := key;
      specifiers[j + 1] := key-literal-value;
      values(make-local-lexical-environment(name, variable, type-temp, env),
             keys-first, keys-last, variable)
    else
      // TODO: TIGHTEN UP EXTRACT AFTER WORKING
      specifiers[j] := key;
      specifiers[j + 1] := &unbound;
      let function =
        make-dylan-reference(#"==");
      let unbound-temp
        = make-object-reference(&unbound);
      let (call, temporary) =
        make-with-temporary(env, <simple-call>,
                            function: function,
                            arguments: vector(variable, unbound-temp));
      let (keys-first, keys-last)
        = join-2x1!(keys-first, keys-last, call);

      let (default-first, default-last, default-t) =
        convert-1(env, default-expression);
      // TODO: MOVE THIS INTO MERGE
      let (default-mt, default-mt-t) =
        // We mustn't eliminate the defaulting code if an input keyword
        // is attached to more than one variable.
        if (~shared?)
          make-with-temporary
            (env, <keyword-default>,
             value: default-t,
             specifiers: specifiers,
             index: index,
             keyword-variable: variable);
        else
          make-with-temporary
            (env, <temporary-transfer>,
             value: default-t);
        end;
      let (default-first, default-last)
        = join-2x1!(default-first, default-last, default-mt);
      let (new-env, new-variable) =
        bind-local-variable(env, name, #f);
      let if-c = make-in-environment
                   (env, <if>, test: temporary,
                    consequent: default-first, alternative: #f);
      let merge-c =
        make-in-environment
          (env, <if-merge>,
           left-value: default-mt-t,
           left-previous-computation: default-mt,
           right-value: variable,
           right-previous-computation: if-c,
           temporary: new-variable,
           previous-computation: if-c);
      new-variable.generator := merge-c;
      if-c.next-computation  := merge-c;
      if-c.alternative       := merge-c;
      default-mt.next-computation := merge-c;
      let (keys-first, keys-last)
        = join-2x2!(keys-first, keys-last, if-c, merge-c);
      default-first.previous-computation := if-c;
      values(new-env, keys-first, keys-last, new-variable)
    end;

  if (static-type & ~type-checked-at-run-time?(static-type))
    values(new-env, new-keys-first, new-keys-last)
  else
    let (new-keys-first, new-keys-last)
      = join-2x2!(new-keys-first, new-keys-last,
                  type-first, type-last);
    let (new-env, new-variable) =
      bind-local-variable(new-env, name, type-temp);
    let check-c =
      make-in-environment
        (env, <keyword-check-type>,
         value: defaulted-value, type: type-temp,
         temporary: new-variable);
    let (new-keys-first, new-keys-last)
      = join-2x1!(new-keys-first, new-keys-last, check-c);
    new-variable.generator := check-c;
    values(new-env, new-keys-first, new-keys-last)
  end
end;

//// Next-method function generation.

// This method generates an environment for use in converting the
// keyword defaults and body of the method. It returns the new
// environment and the variable temporary for the name. The use
// of this name is watched, and the code to initialize the
// next-method function generated only if necessary.

define method bind-next-method
    (env :: <environment>, next-spec :: <next-variable-spec>)
 => (new-env :: <local-lexical-environment>, ref :: <value-reference>)
  let next-method-name = spec-variable-name(next-spec);
  // Create the augmented lookup environment for use in the keyword
  // defaults and body.
  // Type should be false-or(<function>).
  bind-local-variable(env, next-method-name, #f);
end method;

// This function is called if the next-method variable is referenced in
// order to generate whatever code is necesary.

// TODO: Lots of things. It may be that there should be a next-method
// call instruction in the DFM to make statically resolving next
// methods easier. We can do a scan to see whether the
// users use next-method as a function to call and/or as a value
// for testing, and generate only the appropriate code. I suspect that
// most of the time we won't have to worry about making it look like #f
// since people don't seem to do that test much in practice. If it is
// tested we can "split" the temporary according to role if that makes
// analysis easier. There's a generic optimisation behind this kind of
// splitting that we should think about first, however.

// TODO: Respect the #f test case. The old compiler didn't, so we don't
// have to worry for any of our libraries while getting up to speed.
// See the above comment for notes on this.

define method convert-next-method-into
    (env :: <environment>, f :: <&lambda>, signature-spec :: <signature-spec>,
     next-ref :: <value-reference>)
 => ()
  f.^function-next? := #t;
  let fragment
    = generate-next-method-function-fragment(f, signature-spec, next-ref);
  let (f-start, f-end, f-temp) = convert-1(env, fragment);
  insert-computations-after!(f.body, f-start, f-end);
  replace-temporary-in-users!(next-ref, f-temp);
end method;

// The next-methods value has to be grabbed and stashed away as the
// first action of the function since it is a transient value that
// will get smashed by calls in the function's body.

// TODO: This is a naive transcription of what was in the old compiler.
// Can do better even without a next-method computation - in particular
// could generate the next-method function with a fixed number of
// arguments in most cases.

define method rest-name
    (f :: <&lambda>, signature-spec :: <signature-spec>) => (res)
  if (spec-argument-optionals?(signature-spec))
    let rest-spec = spec-argument-rest-variable-spec(signature-spec);
    if (rest-spec)
      spec-variable-name(rest-spec);
    else
      implicit-rest-variable-name();
    end;
  end;
end method;

define method generate-next-method-function-fragment
    (f :: <&lambda>, sig-spec :: <signature-spec>, next-temp) => (fragment)
  let capture
    = #{ let _next-methods_ :: <list> = primitive-next-methods-parameter() };
  let required-specs
    = spec-argument-required-variable-specs(sig-spec);
  let required-names
    = map(spec-variable-name, required-specs);
  let rest-name
    = rest-name(f, sig-spec);
  let all-names
    = if (rest-name)
        concatenate(required-names, list(rest-name));
      else
        required-names;
      end;
  let no-arguments-call
    = #{ %method-call
           (_next-methods_.head, _next-methods_.tail, ??all-names, ...) };
  let arguments-call
    = #{ %method-apply
           (_next-methods_.head, _next-methods_.tail, _next-method-args_) };
  let fragment
    = #{ ?capture;
         method (#rest _next-method-args_)
           %dynamic-extent(_next-method-args_);
           if (_next-methods_ ~== #())
             if (empty?(_next-method-args_))
               ?no-arguments-call;
             else
               ?arguments-call;
             end;
           else
             error("No next method.");
           end;
         end; };
  as-body(fragment);
end method;

// Primitives are:
//
//   primitive-next-methods-parameter
//   %method-call
//   %method-call-with-optionals
//   %method-apply

define &converter %method-call
  { %method-call(?function:expression, ?arguments) }
    => convert-function-call
         (env, context, <method-call>, form, function, arguments, typecheck-function?: #f);
 arguments:
  { ?:expression }
    => list(expression);
  { ?:expression, ... }
    => pair(expression, ...);
end &converter;

define &converter %method-apply
  { %method-apply(?function:expression, ?arguments) }
    => convert-function-call
         (env, context, <method-apply>, form, function, arguments, typecheck-function?: #f);
 arguments:
  { ?:expression }
    => list(expression);
  { ?:expression, ... }
    => pair(expression, ...);
end &converter;

define method bind-rest
    (env :: <environment>, f :: <&lambda>, sig-spec :: <signature-spec>)
 => (new-env :: <local-lexical-environment>, ref :: <value-reference>)
  let rest-name = rest-name(f, sig-spec);
  // Create the augmented lookup environment for use in the keyword
  // defaults and body.
  lambda-rest?(f) := #t;
  bind-local-variable(env, rest-name, dylan-value(#"<simple-object-vector>"));
end method;

define method convert-copy-rest-into
    (env :: <environment>, f :: <&lambda>, sig-spec :: <signature-spec>,
     rest-ref :: <value-reference>)
 => ()
  lambda-rest?(f) := #t;
  let rest-name = rest-name(f, sig-spec);
  let fragment
    = if (dynamic-extent?(rest-ref) == #t)
        #{ ?rest-name }
      else
        #{ primitive-copy-vector(?rest-name) }
      end;
  let (f-start, f-end, f-temp) = convert-1(env, fragment);
  insert-computations-after!(f.body, f-start, f-end);
  replace-temporary-in-users!(rest-ref, f-temp);
end method;

//// Lambda body DFM generation.

define function convert-lambda-into*-d
    (env :: <environment>, function-t :: <value-reference>,
     f :: <&lambda>, the-body)
  let sig-spec = signature-spec(f);
  let lambda-env = make(<lambda-lexical-environment>, outer: env, lambda: f);
  add-inner!(env, lambda-env);
  let (variables, keys-start) =
    parse-parameters-into-d(env, lambda-env, sig-spec);
  f.environment := lambda-env;
  f.parameters := variables;
  let bind-computation = make-in-environment(lambda-env, <bind>);
  let (copy-rest-env, rest-temp)
    = if (spec-argument-optionals?(sig-spec))
        bind-rest(lambda-env, f, sig-spec)
      else
        values(lambda-env, #f)
      end if;
  let next-variable-spec = spec-argument-next-variable-spec(sig-spec);
  let (next-method-env, next-temp)
    = if (next-variable-spec)
        bind-next-method(copy-rest-env, next-variable-spec);
      else
        values(copy-rest-env, #f);
      end;
  let inner-env = next-method-env;
  let vars-first = bind-computation;
  let vars-last  = bind-computation;
  for (var-spec in spec-argument-required-variable-specs(sig-spec),
       index from 0)
    let (type-first, type-last, type-t) =
      convert-required-type(lambda-env, function-t, index);
    let (_vars-first, _vars-last)
      = join-2x2!(vars-first, vars-last, type-first, type-last);
    let (constant-type?, constant-type-value) = fast-constant-value?(type-t);
    if (constant-type?)
      specializer(variables[index]) := constant-type-value;
      vars-last := _vars-last;
    else
      let (new-env, new-variable) =
        bind-local-variable(inner-env, spec-variable-name(var-spec), #f);
      let guarantee-c =
        make-in-environment
          (lambda-env, <guarantee-type>,
           value: variables[index],
           type: type-t,
           temporary: new-variable);
      generator(new-variable) := guarantee-c;
      let (_vars-first, _vars-last) = join-2x1!(_vars-first, _vars-last, guarantee-c);
      vars-last := _vars-last;
      inner-env := new-env;
    end if;
  end;
  let key-variable-specs = spec-argument-key-variable-specs(sig-spec);
  if (~empty?(key-variable-specs))
    let (specifiers, key-counts)
      = make-key-variable-specifiers-vector(env, key-variable-specs);
    for (key-spec in key-variable-specs,
         key-var-index :: <integer> from keys-start,
         key-index :: <integer> from 0)
      // Does this key-spec share a key with other key specs, and
      // if so, is it the input variable they all share?
      let key = &eval(env, spec-keyword-expression(key-spec));
      let entry = element(key-counts, key);
      let shared-index = entry.head;
      let shared? = (entry.tail ~== 1);
      let input? = (shared-index == key-index);
      let var = variables[key-var-index];
      let input-var
        = if (shared?)
            variables[keys-start + shared-index]
          else
            var
          end;
      let (new-env, new-vars-first, new-vars-last) =
        convert-keyword-initialization-d
          (inner-env, function-t, vars-first, vars-last, key-spec,
           key, input-var, shared?, input?, specifiers, key-index);
      inner-env := new-env;
      vars-last := new-vars-last;
    end for;
    f.keyword-specifiers := specifiers;
  end if;
  let required-values
    = spec-value-required-variable-specs(sig-spec);
  let rest-type
    = spec-value-rest-variable-spec(sig-spec);
  let number-of-required-values
    = size(required-values);
  let ret-types-first
    = vars-last;
  let ret-types-last
    = vars-last;
  let fixed-types
    = make(<simple-object-vector>, size: number-of-required-values);
  for (i :: <integer> from 0 below number-of-required-values)
    let (type-first, type-last, type-temp) =
      convert-value-type(lambda-env, function-t, i);
    let (_types-first, _types-last)
      = join-2x2!(ret-types-first, ret-types-last, type-first, type-last);
    ret-types-first := _types-first;
    ret-types-last  := _types-last;
    fixed-types[i]
      := type-checked-at-run-time?(fast-constant-value(type-temp)) & type-temp;
  end for;
  let (ret-types-first, ret-types-last, rest-type-temp)
    = if (rest-type)
        let (type-first, type-last, type-temp) =
          convert-rest-value-type(lambda-env, function-t);
        let type-temp
          = type-checked-at-run-time?(fast-constant-value(type-temp)) & type-temp;
        join-2x2-t!(ret-types-first, ret-types-last, type-first, type-last, type-temp);
      else
        values(ret-types-first, ret-types-last, #f)
      end if;
  let (body-first, body-last, value-temp)
    = convert-body
        (inner-env, make(<multiple-value-context>,
                         mvc-num-values: number-of-required-values,
                         mvc-rest?: rest-type),
         the-body);
  let (vars-first, body-last)
    = join-2x2!(vars-first, ret-types-last, body-first, body-last);

  let (types-first, adj-temp) =
    // if an exact signature (no #rest return), enforce the correct number of
    // returned values
    if (~ rest-type)
      let (adj-c, adj-t) =
        make-with-temporary
          (lambda-env, <adjust-multiple-values>,
           value: value-temp,
           number-of-required-values: number-of-required-values,
           temporary-class: <multiple-value-temporary>);
        mvt-transfer-values!(value-temp, adj-t);
        join-1x1!(body-last, adj-c);
        values(adj-c, adj-t);
    else
      values(body-last, value-temp);
    end if;

  let (types-last, check-temp) =
    if (rest-type-temp)
      let (check-c, check-temp) =
        make-with-temporary
          (lambda-env, <multiple-value-result-check-type-rest>,
           value:           adj-temp,
           types:           fixed-types,
           rest-type:       rest-type-temp,
           temporary-class: <multiple-value-temporary>);
      mvt-transfer-values!(adj-temp, check-temp);
      let (types-first, types-last)
        = join-1x1!(types-first, check-c);
      values(types-last, check-temp)
    elseif (size(fixed-types) = 0 /* | (size(fixed-types) = 1 & ~fixed-types[0]) */)
      // no types to check
      values(types-first, adj-temp)
    // elseif (size(fixed-types) = 1)
    //   let (check-c, check-temp) =
    //     make-with-temporary(lambda-env, <single-value-result-check-type>,
    //                         value: value-temp,
    //                         type: fixed-types[0]);
    //    let (types-first, types-last)
    //      = join-2x1!(types-first, types-last, check-c);
    //    values(types-last, value-temp)
    else
      let (check-c, check-temp) =
        make-with-temporary
          (lambda-env, <multiple-value-result-check-type>,
           value:           adj-temp,
           types:           fixed-types,
           temporary-class: <multiple-value-temporary>);
      mvt-transfer-values!(adj-temp, check-temp);
      let (types-first, types-last)
        = join-1x1!(types-first, check-c);
      values(types-last, check-temp);
    end;
  let return
    = make-in-environment
        (lambda-env, <return>, value: check-temp);
  bind-return(bind-computation) := return;
  join-1x1!(types-last, return);
  f.body := bind-computation;
  // TODO: should there be a f.body-last?
  // Was the next-method variable actually used?
  if (next-temp & used?(next-temp))
    convert-next-method-into(copy-rest-env, f, sig-spec, next-temp);
  else
    ^function-next?(f) := #f;
  end if;
  if (rest-temp & used?(rest-temp))
    convert-copy-rest-into(lambda-env, f, sig-spec, rest-temp);
  else
    lambda-rest?(f) := #f;
  end if;
  strip-bindings(lambda-env);
  f
end function convert-lambda-into*-d;

/// TODO: SHOULD USE TYPIST???

define function function-temporary-make-closure
    (function-t :: <temporary>) => (res :: false-or(<make-closure>))
  let gen = generator(function-t);
  if (instance?(gen, <make-closure>))
    gen
  end if
end function;

define inline method ^signature-type*
    (sig :: <&signature>, static-accessor :: <function>, index :: false-or(<integer>))
 => (type :: false-or(<&type>), found? :: <boolean>)
  let types = static-accessor(sig);
  let type  = if (index)
                element(types, index)
              else
                types
              end if;
  let type? = instance?(type, <&type>);
  values(type? & type, type?)
end method;

define method ^function-signature-type*
    (function-t :: <value-reference>,
     static-accessor :: <function>, dynamic-accessor :: <function>,
     index :: false-or(<integer>))
 => (type :: false-or(<&type>), found? :: <boolean>)
  values(#f, #f)
end method;

define method ^function-signature-type*
    (function-t :: <method-reference>,
     static-accessor :: <function>, dynamic-accessor :: <function>,
     index :: false-or(<integer>))
 => (type :: false-or(<&type>), found? :: <boolean>)
  let sig = ^function-signature(reference-value(function-t));
  let (type, found?) = ^signature-type*(sig, static-accessor, index);
  let type? = instance?(type, <&type>);
  values(type? & type, type? & found?)
end method;

define method ^function-signature-type*
    (function-t :: <temporary>,
     static-accessor :: <function>, dynamic-accessor :: <function>,
     index :: false-or(<integer>))
 => (type :: false-or(<&type>), found? :: <boolean>)
  let mc = function-temporary-make-closure(function-t);
  if (mc)
    let sig-t = computation-signature-value(mc);
    let (type, found?) =
      if (sig-t)
        let type-ref = dynamic-accessor(sig-t, index);
        let (type-constant?, type-value) = fast-constant-value?(type-ref);
        values(type-value, type-constant?)
      else
        let sig = ^function-signature(computation-closure-method(mc));
        ^signature-type*(sig, static-accessor, index);
      end if;
    let type? = instance?(type, <&type>);
    values(type? & type, type? & found?)
  else
    values(#f, #f)
  end if;
end method;

define method ^make-signature-argument-reference
    (sig-t :: <temporary>, vector-index :: false-or(<integer>),
     argument-index :: <integer>)
 => (res :: <value-reference>)
  let arg = ^make-signature-argument(sig-t, argument-index);
  if (vector-index)
    ^vector-element-reference(arg, vector-index)
  else
    arg
  end if
end method;

// TODO: This stuff is an ugly hack to work around referencing through
// inserted rest vector copies. When vector copies are no longer
// inserted so eagerly, this can go away.

define constant <argument-sequence> = <simple-object-vector>;

define method maybe-vector-element-references
    (c) => (res :: false-or(<argument-sequence>))
  #f
end method;

define method maybe-vector-element-references
    (c :: <stack-vector>) => (res :: false-or(<argument-sequence>))
  c.arguments
end method;

define method maybe-vector-element-references
    (c :: <primitive-call>) => (res :: false-or(<argument-sequence>))
  let primitive = c.primitive;
  primitive == dylan-value(#"primitive-copy-vector")
    & maybe-vector-element-references(c.arguments.first)
end method;

define method maybe-vector-element-references
    (c :: <simple-call>) => (res :: false-or(<argument-sequence>))
  select (function-value(c))
    dylan-value(#"immutable-vector"), dylan-value(#"immutable-type-vector")
      => if (call-iep?(c))
           maybe-vector-element-references(c.arguments.first)
         else
           arguments(c)
         end if;
    otherwise
      => #f;
  end select;
end method;

define method maybe-vector-element-references
    (ref :: <temporary>) => (res :: false-or(<argument-sequence>))
  maybe-vector-element-references(ref.generator);
end method;

define method maybe-vector-element-references
    (ref :: <object-reference>) => (res :: false-or(<argument-sequence>))
  maybe-vector-object-element-references(reference-value(ref));
end method;

define method maybe-vector-object-element-references
    (object :: <sequence>) => (res :: false-or(<argument-sequence>))
  map-as(<argument-sequence>,
         method (elt) make(<object-reference>, value: elt) end,
         object);
end method;


define inline method ^vector-element-reference
    (c, index :: <integer>)
 => (res :: false-or(<value-reference>))
  let refs = maybe-vector-element-references(c);
  refs & element(refs, index, default: #f)
end method;

define method ^make-signature-argument
    (sig-t :: <temporary>, index :: <integer>) => (res :: <value-reference>)
  let call = generator(sig-t);
  let args = arguments(call);
  let arg  = element(args, index);
  arg
end method;

/// make-<signature> and make-<keyword-signature>
/// must have the following argument order for the following to be valid
/// (next?, required-types, value-types, rest-value-type, properties, keys, key-types)

define method ^function-required-type*
    (function-t :: <value-reference>, index :: <integer>)
 => (type :: false-or(<&type>), found? :: <boolean>)
  ^function-signature-type*
    (function-t, ^signature-required,
     rcurry(^make-signature-argument-reference, 1), index)
end method;

define method ^function-value-type*
    (function-t :: <value-reference>, index :: <integer>)
 => (type :: false-or(<&type>), found? :: <boolean>)
  ^function-signature-type*
    (function-t, ^signature-values,
     rcurry(^make-signature-argument-reference, 2), index)
end method;

define method ^function-rest-value-type*
    (function-t :: <value-reference>, index)
 => (type :: false-or(<&type>), found? :: <boolean>)
  ^function-signature-type*
    (function-t, ^signature-rest-value,
     rcurry(^make-signature-argument-reference, 3), #f)
end method;

define method ^function-key-type*
    (function-t :: <value-reference>, index :: <integer>)
 => (type :: false-or(<&type>), found? :: <boolean>)
  ^function-signature-type*
    (function-t, ^signature-key-types,
     rcurry(^make-signature-argument-reference, 6), index)
end method;

define function convert-signature-type
    (env :: <environment>, function-t :: <temporary>,
     name :: <symbol>, accessor :: <function>, index :: false-or(<integer>))
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  let (type-value, type-found?) = accessor(function-t, index);
  if (type-found?)
    let type-ref = make(<object-reference>, value: type-value);
    values(#f, #f, type-ref)
  else
    let function
      = make-global-reference(make-variable-name-fragment(name));
    let index-t
      = index & make-object-reference(index);
    let (call, call-t) =
      make-with-temporary
        (env, <simple-call>,
         function:  function,
         arguments: if (index)
                      vector(function-t, index-t)
                    else
                      vector(function-t)
                    end if);
    values(call, call, call-t)
  end if
end;

define function convert-required-type
    (env :: <environment>, function-t :: <temporary>, index :: <integer>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  convert-signature-type(env,
                         function-t,
                         #"function-required-type",
                         ^function-required-type*,
                         index);
end;

define function convert-key-type
    (env :: <environment>, function-t :: <temporary>, index :: <integer>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  convert-signature-type(env,
                         function-t,
                         #"function-key-type",
                         ^function-key-type*,
                         index);
end;

define function convert-value-type
    (env :: <environment>, function-t :: <temporary>, index :: <integer>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  convert-signature-type(env,
                         function-t,
                         #"function-value-type",
                         ^function-value-type*,
                         index);
end;

define function convert-rest-value-type
    (env :: <environment>, function-t :: <temporary>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  // TODO: EXTEND THIS FOR VALUES CASE
  convert-signature-type(env,
                         function-t,
                         #"function-rest-value-type",
                         ^function-rest-value-type*,
                         #f);
end;

define function spec-type-expression-checking
    (spec :: <variable-spec>) => (type-expr :: <fragment>)
  check-signature-variable(*current-dependent*, spec, #f);
  spec-type-expression(spec);
end function;

define function convert-signature-types
    (env :: <environment>, variable-specs :: <variable-specs>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  // immutable-type-vector(...)
  if (empty?(variable-specs))
      convert-object-reference-1(env, #[])
  else
    let (args-first, args-last, args) =
      convert-expressions
        (env, variable-specs, form-extractor: spec-type-expression-checking);
    if (every?(method (t) instance?(fast-constant-value(t), <&type>) end, args))
      convert-object-reference-1
        (env,
         as-sig-types(map-as(<simple-object-vector>, fast-constant-value, args)))
    else
      let function =
        make-dylan-reference(#"immutable-type-vector");
      let (call, call-t) =
        make-with-temporary
          (env, <simple-call>, function:  function, arguments: args);
      join-2x1-t!(args-first, args-last, call, call-t);
    end
  end
end;

define method convert-signature
    (env :: <environment>, sig-spec :: <signature-spec>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  // make-<signature>/ make-<keyword-signature>
  //   (required, keys, key-types, values, rest-value, signature-properties)
  let key? = if (spec-argument-key?(sig-spec)) #t else #f end;
  let (req-first, req-last, req-t) =
    convert-signature-types(env, spec-argument-required-variable-specs(sig-spec));
  let keys-t =
    key? &
      make-object-reference
        (compute-variables-spec-keys(env, sig-spec));
  let (key-first, key-last, key-t) =
    key? &
      convert-signature-types(env, spec-argument-key-variable-specs(sig-spec));
  let (val-first, val-last, val-t) =
    convert-signature-types(env, spec-value-required-variable-specs(sig-spec));
  let (rest-val-first, rest-val-last, rest-val-t) =
    if (spec-value-rest?(sig-spec))
      convert-type-expression
        (env, spec-type-expression(spec-value-rest-variable-spec(sig-spec)))
    else
      convert-object-reference-1(env, &false)
    end;
  if (fast-constant-value?(req-t)
        & (~key? | (fast-constant-value?(keys-t) & fast-constant-value?(key-t)))
        & fast-constant-value?(val-t) & fast-constant-value?(rest-val-t))
    convert-object-reference-1
      (env,
       compute-signature-using-types
         (sig-spec,
          fast-constant-value(req-t),
          fast-constant-value(val-t),
          fast-constant-value(rest-val-t),
          if (key?) fast-constant-value(keys-t) else #[] end,
          if (key?) fast-constant-value(key-t)  else #[] end,
	  #[]))
  else
    let next-t =
      make-object-reference(#t);
    let (sig-first, sig-last)
      = if (key?)
          let (sig-first, sig-last)
            = join-2x2!(req-first, req-last, key-first, key-last);
          join-2x2!(sig-first, sig-last, val-first, val-last);
        else
          join-2x2!(req-first, req-last, val-first, val-last);
        end;
    let (sig-first, sig-last)
      = join-2x2!(sig-first, sig-last, rest-val-first, rest-val-last);
    let signature-properties =
      ^pack-signature-properties
        (rest-value?:     spec-value-rest?(sig-spec),
         rest?:           spec-argument-rest?(sig-spec),
         all-keys?:       spec-argument-all-keys?(sig-spec),
         key?:            key?,
         number-values:   spec-value-number-required(sig-spec),
         number-required: spec-argument-number-required(sig-spec));
    let sig-prop-t =
      make-object-reference(signature-properties);
    let function =
      if (key?)
        make-dylan-reference(#"make-<keyword-signature>")
      else
        make-dylan-reference(#"make-<signature>")
      end if;
    let (call, call-t) =
      make-with-temporary
      (env, <simple-call>,
       function:  function,
       arguments:
         if (key?) // NEXT MUST BE FIRST ARG FOR BELOW
           vector(next-t, req-t, val-t, rest-val-t, sig-prop-t, keys-t, key-t)
         else
           vector(next-t, req-t, val-t, rest-val-t, sig-prop-t)
         end);
    let (call-first, call-last, single-t) =
      extract-single-value(call, call, call-t);
    join-2x2-t!(sig-first, sig-last, call-first, call-last, single-t);
  end if
end;

define compiler-sideways method ^function-next?-setter
    (y :: <boolean>, x :: <&lambda>) => (z :: <boolean>)
  lambda-next?(x) := y;
  if (^function-signature(x)) // might be dynamic
    ^signature-next?(^function-signature(x)) := y;
  else
    // SET NEXT? ARGUMENT TO MAKE-<SIGNATURE>
    let users    = users(x);
    assert(size(users) == 1,
           "size of users %= of dynamic lambda %= should be one",
           users, x);
    let make-c   = first(users);
    assert(instance?(make-c, <make-closure>),
           "only user %= of dynamic lambda %= should be make-closure",
           make-c);
    let sig-t    = computation-signature-value(first(users));
    let next-arg = ^make-signature-argument(sig-t, 0);
    reference-value(next-arg) := y;
  end if;
end method;

define function convert-method-and-signature
    (env :: <environment>, sig-t :: <value-reference>, the-method :: <&method>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     ref :: <value-reference>)
  let (sig-constant?, sig-constant-value) = fast-constant-value?(sig-t);
  if (sig-constant?)
    ^function-signature(the-method) := sig-constant-value;
    if (top-level-environment?(env))
      convert-method-reference(env, $single, the-method)
    else
      make-with-temporary*
        (env, <make-closure>, method: the-method, signature: #f)
    end if
  else
    make-with-temporary*
      (env, <make-closure>, method: the-method, signature: sig-t)
  end if
end function;

define function do-parse-local-method (method-form)
  macro-case (fragment-argument(method-form))
    { ?:name ?spec:* }
    => begin
         let (sig-spec, body) = parse-method-signature(name, spec);
         values(name, sig-spec, body)
       end;
  end;
end;

// TODO: warn if multiple uses of same name
define function convert-labels
    (env :: <environment>, context :: <value-context>,
     definitions :: <list>, the-body)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let labels-first = #f;
  let labels-last  = #f;
  collecting (names, functions, methods)
    for (definition in definitions)
      let (name, sig-spec, body) = do-parse-local-method(definition);
      let (sig-first, sig-last, sig-t) =
        convert-signature(env, sig-spec);
      let (_labels-first, _labels-last)
        = join-2x2!(labels-first, labels-last, sig-first, sig-last);
      let the-method =
        compute-method-explicitly
           (<&method>, #f, name, sig-spec, as-body(body));
      let (function-first, function-last, function-t) =
        convert-method-and-signature(env, sig-t, the-method);
      let (_labels-first, _labels-last)
        = join-2x2!(_labels-first, _labels-last, function-first, function-last);
      labels-first := _labels-first;
      labels-last  := _labels-last;
      collect-into(names, name); collect-into(functions, function-t);
      collect-into(methods, the-method);
    end for;
    for (function in collected(functions),
         the-method in collected(methods))
      if (instance?(function, <temporary>)) // make-closure?
        let make-closure-c = generator(function);
        let init-closure-c
          = make-in-environment
              (env, <initialize-closure>,
               closure: function, method: the-method);
        computation-init-closure(make-closure-c) := init-closure-c;
        let (_labels-first, _labels-last)
          = join-2x1!(labels-first, labels-last, init-closure-c);
        labels-first := _labels-first;
        labels-last  := _labels-last;
      end if;
    end for;
    let inner-env = env;
    for (name in collected(names),
         function in collected(functions))
      inner-env := make-local-lexical-environment(name, function, #f, inner-env);
    end for;
    for (function in collected(functions),
         the-method in collected(methods))
      convert-lambda-into*-d
        (inner-env, function, the-method, body-spec(the-method));
    end for;
    let (body-first, body-last, body-t) = convert-body(inner-env, context, the-body);
    let (labels-first, labels-last)
      = join-2x2!(labels-first, labels-last, body-first, body-last);
    values(labels-first, labels-last, body-t)
  end collecting;
end function;

define function do-parse-local-macro
    (macro-form :: <fragment>)
 => (name :: <variable-name-fragment>, rules :: <fragment>)
  macro-case (fragment-argument(macro-form))
    { ?:name ?spec:* }
    => begin
         values(name, spec);
       end;
  end;
end function;

define function convert-macros
    (env :: <environment>, context :: <value-context>,
       definitions :: <list>, the-body)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let inner-env = env;
  for (macro-def in definitions)
    let (macro-name, macro-rules)
      = do-parse-local-macro(macro-def);
    let (descriptor, expander)
      = do-compile-macro
          (macro-name, #(), macro-rules,
             definition-context: pair(env, fragment-module(macro-name)));
    let object
      = make(<&macro>,
             definition: *current-dependent*,
             macro-object: descriptor);
    inner-env
      := make-local-lexical-environment
           (macro-name, make-object-reference(object), #f, inner-env);
  end for;
  convert-body(inner-env, context, the-body);
end function;

define method convert-initialization
    (env :: <environment>, context :: <value-context>, the-name, the-value)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  // This function is only ever called on single value expressions,
  // typically variable references.
  let (init-first, init-last, init-t) = convert-1(env, the-value);
  let the-binding = lookup(env, the-name, reference?: #f);
  let definition-class
    = if (binding-previously-defined?(the-binding))
        <redefinition>
      else
        <definition>
      end;
  let defn-c = make-in-environment
                 (env, definition-class,
                  value:       init-t,
                  binding:     the-binding);
  record-binding-assignment(the-binding, defn-c);
  // TODO: Is there any need to record the initializing computation of
  // a binding?
  // the-binding.definition := defn-c;
  let (init-first, init-last) = join-2x1!(init-first, init-last, defn-c);
  match-values-with-context
    (env, context, init-first, init-last, #f)
end method convert-initialization;

define method convert-type-initialization
    (env :: <environment>, context :: <value-context>, the-name, the-value)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  // This function is only ever called on single value expressions,
  // typically variable references.
  let (init-first, init-last, init-t) = convert-1(env, the-value);
  let the-binding = lookup(env, the-name, reference?: #f);
  let definition-class
    = if (binding-previously-defined?(the-binding))
        <type-redefinition>
      else
        <type-definition>
      end;
  let defn-c = make-in-environment
                 (env, definition-class,
                  value:       init-t,
                  binding:     the-binding);
  record-binding-assignment(the-binding, defn-c);
  let (init-first, init-last) = join-2x1!(init-first, init-last, defn-c);
  match-values-with-context
    (env, context, init-first, init-last, #f)
end method convert-type-initialization;

//// Syntax-dependent conversion tools

// Call conversion engine

define method convert
    (env :: <environment>, context :: <value-context>, form :: <&top>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-object-reference(env, context, form)
end method;

define method convert
    (env :: <environment>, context :: <value-context>, form :: <fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  error("Unrecognised code fragment for conversion: %=", form);
end method;

define method convert
    (env :: <environment>, context :: <value-context>, form)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert(env, context, as-body(form));
end method;

// Primitive expression types

/*
define program-warning <literal-reference-value-not-used>
  slot condition-literal,
    required-init-keyword: literal:;
  format-string    "The value of the literal %s is not used.";
  format-arguments literal;
end program-warning;
*/

define method convert
    (env :: <environment>, context :: <value-context>,
     form :: <literal-constant-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let object = form.fragment-value;
  if (instance?(object, <module-binding>))
    convert-reference(env, context, object)
  else
    /*
    if (unused-value-context?(context))
      note(<literal-reference-value-not-used>,
           source-location: fragment-source-location(form),
           literal: form);
    end;
    */
    convert-object-reference(env, context, object)
  end;
end method;

/*
define program-warning <variable-reference-value-not-used>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "The value of the variable reference %s is not used.";
  format-arguments variable-name;
end program-warning;
*/

define method convert
    (env :: <environment>, context :: <value-context>,
     form :: <variable-name-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  /*
  if (unused-value-context?(context))
    note(<variable-reference-value-not-used>,
         source-location: fragment-source-location(form),
         variable-name: form);
  end;
  */
  convert-reference(env, context, form)
end method;

define serious-program-warning <invalid-left-hand-side-for-assignment>
  slot condition-left-hand-side,
    required-init-keyword: left-hand-side:;
  format-string
    "Invalid left hand side %s for assignment - only variables, function "
    "calls, and function macro calls may appear before the := operator.";
  format-arguments left-hand-side;
end serious-program-warning;

define serious-program-warning <invalid-function-call-for-assignment>
  slot condition-left-hand-side,
    required-init-keyword: left-hand-side:;
  format-string
    "Invalid function call %s for assignment - the function in the call "
    "must be a variable name from which the setter name can be derived.";
  format-arguments left-hand-side;
end serious-program-warning;

define &converter \:=
  { \:= (?lhs:expression, ?rhs:expression) }
    => select (lhs by instance?)
         <variable-name-fragment>
           => convert-assignment(env, context, lhs, rhs, form);
         <function-call-fragment>
           => let getter-name = fragment-function(lhs);
              let args = fragment-arguments(lhs);
              let new-value = rhs;
              if (instance?(getter-name, <variable-name-fragment>))
                convert
                  (env, context,
                   #{ begin
                        let _tmp :: <top> = ?new-value;
                        ?getter-name ## "-setter" (_tmp, ??args, ...);
                        _tmp
                      end });
              else
                note(<invalid-function-call-for-assignment>,
                     source-location: fragment-source-location(lhs),
                     context-id: dfm-context-id(env),
                     left-hand-side: lhs);
                convert-error-fallback(env, context);
              end;
         <function-macro-fragment>
           => let getter-name = fragment-macro(lhs);
              let args = export-fragment-tokens(fragment-body-fragment(lhs));
              let new-value = rhs;
              convert
                (env, context,
                 #{ begin
                      let _tmp :: <top> = ?new-value;
                      ?getter-name ## "-setter" (_tmp, ?args);
                      _tmp
                    end });
         otherwise
           => note(<invalid-left-hand-side-for-assignment>,
                   source-location: fragment-source-location(lhs),
                   context-id: dfm-context-id(env),
                   left-hand-side: lhs);
              convert-error-fallback(env, context);
       end;
end &converter;

define &converter \&
  { \& (?left:expression, ?more:*) }
    => convert(env, context, #{ if (?left) ?more else #f end });
end &converter;

define &converter \|
  { \| (?left:expression, ?more:*) }
    => convert(env, context,
               #{ begin
                    let _tmp = ?left;
                    if (_tmp) _tmp else ?more end;
                  end });
end &converter;

define serious-program-warning <non-top-level-definition>
  slot condition-fragment,
    required-init-keyword: fragment:;
  format-string    "Non-top-level definition %s encountered - ignoring.";
  format-arguments fragment;
end serious-program-warning;

define function non-top-level-definition
    (env :: <environment>, context :: <value-context>, form)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  note(<non-top-level-definition>,
       source-location: fragment-source-location(form),
       context-id:      dfm-context-id(env),
       fragment: form);
  // Need a no-values converter.
  convert-body(env, context, #());
end function;

define method convert
    (env :: <environment>, context :: <value-context>,
     form :: <macro-definition-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  non-top-level-definition(env, context, form)
end method;

// TODO: CORRECTNESS: Fix order of evaluation for calls other than prefix
// calls.

define method convert
    (env :: <environment>, context :: <value-context>,
     form :: <function-call-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let function-form = form.fragment-function;
  let function-object = operator-eval(env, function-form);
  with-parent-fragment (form)
    convert-using-object(env, context, function-object, form);
  end;
end method;

define function operator-eval (env :: <environment>, form)
  when (instance?(form, <variable-name-fragment>))
    let binding = lookup(env, form);
    when (instance?(binding, <binding>))
      binding-constant-model-object(binding, error-if-circular?: #f)
    end
  end
end;

define inline method omitted-fragment?
    (f :: <fragment>) => (well? :: <boolean>)
  instance?(f, <variable-name-fragment>) & fragment-name(f) == #"_"
end method;

define method curried-arguments? (args :: <sequence>) => (well? :: <boolean>)
  any?(omitted-fragment?, args);
end method;

define method convert-using-object
    (env :: <environment>, context :: <value-context>,
       object :: <object>, form :: <function-call-fragment>)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  let f = form.fragment-function;
  let args = form.fragment-arguments;
  if (curried-arguments?(args))
    convert-curried-function-call(env, context, form, f, args);
  else
    convert-function-call(env, context, <simple-call>, form, f, args);
  end;
end method;

define method convert-curried-function-call
    (env :: <environment>, context :: <value-context>,
       form :: <function-call-fragment>, f :: <fragment>, args :: <sequence>)
  let count = 0;
  collecting (remaining-args, all-args)
    for (arg in args)
      if (omitted-fragment?(arg))
        count := count + 1;
        let remaining-arg
          = make-variable-name-like
              (arg,
               name: as(<symbol>, format-to-string("curried %s", count)),
               source-location: fragment-source-location(arg));
        collect-last-into(remaining-args, remaining-arg);
        collect-last-into(all-args, remaining-arg);
      else
        collect-last-into(all-args, arg);
      end;
    end;
    let remaining-args = collected(remaining-args);
    let all-args = collected(all-args);
    // Manually constructing this call avoids issues with reparsing
    // operator calls.
    let call = make(object-class(form),
                    source-location: fragment-source-location(form),
                    function: f,
                    arguments: all-args);
    let thunk = #{ method (??remaining-args, ...) ?call end };
    convert(env, context, thunk);
  end;
end method;

define method convert-using-object
    (env :: <environment>, context :: <value-context>,
     object :: <&primitive>, form :: <function-call-fragment>)
 => (first :: <computation>, last :: <computation>, ref :: false-or(<value-reference>))
  convert-primitive-call
    (env, context,
     <primitive-call>, object, form.fragment-arguments)
end method;


define serious-program-warning <inner-expander-parse-error>
  slot condition-macro-name, required-init-keyword: macro-name:;
  format-string "Skipping %s macro call due to previous syntax error.";
  format-arguments macro-name;
end serious-program-warning;

define method convert-error-fallback
    (env :: <environment>, context :: <value-context>)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     ref :: false-or(<value-reference>))
  convert-reference(env, context, &false)
end method;

define method convert
    (env :: <environment>, context :: <value-context>, form :: <macro-call-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  let macro-form = form.fragment-macro;
  // macros can't be shadowed by lexical variables.
  // debug-assert(lookup(env, macro-form) == lookup-binding(macro-form))
  with-parent-fragment (form)
    convert-using-definition(env, context, macro-definition(macro-form), form);
  end;
end method;

//// Hacky compiler debug hooks.

// TODO: For the environment, there should be a flag in the macro definition
// object or something.

define variable *traced-expander-names* = #();

define function trace-macro (name) => ()
  let name = as(<symbol>, name);
  *traced-expander-names* := add-new!(*traced-expander-names*, name);
end function;

define function untrace-macro (name) => ()
  let name = as(<symbol>, name);
  *traced-expander-names* := remove!(*traced-expander-names*, name);
end function;

define method convert-using-definition
    (env :: <environment>, context :: <value-context>,
     def :: <expander-defining-form>, form :: <macro-call-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  handling-parse-errors
    let name = fragment-name(fragment-macro(form));
    let trace-it? = member?(name, *traced-expander-names*);
    if (trace-it?)
      format-out("Macro %s > %=\n", name, form);
    end;
    let expansion
      = with-expansion-source-location
            (fragment-record(form), fragment-source-position(form))
          form-expander(def)(env, form);
        end;
    if (trace-it?)
      format-out("Macro %s: %=\n", name, as-body(expansion));
    end;
    do-convert(env, context, expansion);
  on-error (message)
    signal(message);
    note(<inner-expander-parse-error>,
         source-location: fragment-source-location(form),
         context-id:      dfm-context-id(env),
         macro-name:      fragment-macro(form));
    convert-error-fallback(env, context);
  end;
end method;

define method convert-using-definition
    (env :: <environment>, context :: <value-context>,
     def :: <&definition-definition>, form)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  non-top-level-definition(env, context, form)
end method;

define method convert-using-definition
    (env :: <environment>, context :: <value-context>,
     def :: <&converter-definition>, form :: <macro-call-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  handling-parse-errors
    with-expansion-source-location
        (fragment-record(form), fragment-source-position(form))
      form-expander(def)(env, context, form);
    end;
  on-error (message)
    signal(message);
    note(<inner-expander-parse-error>,
         source-location: fragment-source-location(form),
         context-id:      dfm-context-id(env),
         macro-name:      fragment-macro(form));
    convert-error-fallback(env, context);
  end;
end method;

define method convert
    (env :: <environment>, context :: <value-context>, form :: <body-fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>))
  convert-body(env, context, form);
end method;

//// Core statement converters.

define &converter begin
  { begin ?:body end }
    => do-convert(env, context, body);
end &converter;

define &converter if
  { if (?test:expression) ?true-action:body ?false-action end }
    => convert-if(env, context, test, true-action, false-action)
false-action:
  { }
    => #{ #f }
  { \else ?:body }
    => body;
  { \elseif (?test:expression) ?:body ?false-action }
    => #{ if (?test) ?body else ?false-action end }
end &converter;

define function convert-type-expression (env :: <environment>, type)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>,
     type-value)
  if (type)
    let (type-first, type-last, type-temp) = convert-1(env, type);
    let (constant-value?, constant-value) = fast-constant-value?(type-temp);
    if (constant-value? & instance?(constant-value, <&type>))
      values(type-first, type-last, type-temp, constant-value)
    else
      let <type>-temp =
        make-dylan-reference(#"<type>");
      let (check-c, check-temp) =
        make-with-temporary
          (env, <check-type>, value: type-temp, type: <type>-temp);
      let (f, l, t) =
        join-2x1-t!(type-first, type-last, check-c, check-temp);
      values(f, l, t, #f);
    end
  else
    let t = make-dylan-reference(#"<object>");
    values(#f, #f, t, #f);
  end;
end;

define function variable-name-fragment?
    (x) => (res :: false-or(<variable-name-fragment>))
  macro-case (x)
    { ?:name }   => name;
    { ?other:* } => #f;
  end;
end function;

define function do-convert-single-value-let
    (env :: <environment>, context :: <value-context>,
     name, type, expression, body)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: false-or(<value-reference>));
  let named-static-unchecked-type =
    begin let name = variable-name-fragment?(type);
      if (name)
        let type-model = lookup-constant-model-object(env, name);
        type-model &
            ~type-checked-at-run-time?(type-model) &
          type-model
      end;
    end;
  let (body-env, value-first, value-last) =
    // optimize the common case of untyped "let"
    // later optimizations would get rid of the check-type, etc., but
    // why waste the extra work?
    if (named-static-unchecked-type)
      let (value-first, value-last, value-temp) = convert-1(env, expression);
      let (body-env, variable) =
        bind-local-variable(env, name, named-static-unchecked-type);
      let value-computation = value-temp.generator;
      let (value-first, value-last) =
        if (value-computation &
              value-temp.environment == variable.environment &
              ~instance?(value-temp, <binding>) &
              ~used?(value-temp) & empty?(value-temp.assignments))
          // if this is the result of some computation which is otherwise
          // unused, smash the value temporary to be the variable
          value-temp.generator := #f;
          value-computation.temporary := variable;
          variable.generator := value-computation;
          values(value-first, value-last)
        else
          // do a temporary transfer
          let value-c = make-in-environment
                          (env, <temporary-transfer>,
                           value: value-temp,
                           temporary: variable);
          variable.generator := value-c;
          join-2x1!(value-first, value-last, value-c);
        end;
      values(body-env, value-first, value-last)
    else
      let (type-first, type-last, type-temp) =
        convert-type-expression(env, type);
      let (value-first, value-last, value-temp) = convert-1(env, expression);
      let (first-c, last-c)
        = join-2x2!(type-first, type-last, value-first, value-last);
      let (body-env, variable) =
        bind-local-variable(env, name, type-temp);
      let check-c =
        make-in-environment
          (env, <check-type>,
           value: value-temp, type: type-temp,
           temporary: variable);
      let (first-c, last-c) = join-2x1!(first-c, last-c, check-c);
      variable.generator := check-c;
      values(body-env, first-c, last-c)
    end;
  let (body-first, body-last, body-temp) = do-convert(body-env, context, body);
  join-2x2-t!(value-first, value-last, body-first, body-last, body-temp);
end;

define function do-convert-let
    (env :: <environment>, context :: <value-context>,
     parameters, expression, body)
 => (first :: false-or(<computation>), last :: false-or(<computation>),
     ref :: false-or(<value-reference>));
  let lambda-env = lambda-environment(env);
  let let-first = #f;
  let let-last  = #f;
  local method convert-type (spec)
          let type-expression = spec-type-expression(spec);
          let (type-first, type-last, type-temp) =
            convert-type-expression(env, type-expression);
          let (_let-first, _let-last)
            = join-2x2!(let-first, let-last, type-first, type-last);
          let-first := _let-first;
          let-last  := _let-last;
          type-temp
        end;
  let bindings-spec = parse-value-bindings(parameters);
  let fixed-variable-specs = spec-value-required-variable-specs(bindings-spec);
  let fixed-types = map-as(<vector>, convert-type, fixed-variable-specs);
  let rest-variable-spec = spec-value-rest-variable-spec(bindings-spec);
  let rest-type = rest-variable-spec & convert-type(rest-variable-spec);
  let let-context = make(<multiple-value-context>,
                        mvc-num-values: size(fixed-types),
                        mvc-rest?: rest-type);
  // format-out("converting let %=, with context = %=.\n", parameters, let-context);
  let (value-first, value-last, value-temp)
    = convert(env, let-context, expression);
  let (let-first, let-last)
    = join-2x2!(let-first, let-last, value-first, value-last);
  let (check-c, check-temp) =
    if (rest-variable-spec)
      make-with-temporary(lambda-env, <multiple-value-check-type-rest>,
                          value: value-temp,
                          types: map(method (fixed-type)
                                       type-checked-at-run-time?
                                         (fast-constant-value(fixed-type))
                                         & fixed-type
                                     end,
                                     fixed-types),
                          rest-type: type-checked-at-run-time?
                                       (fast-constant-value(rest-type))
                                     & rest-type,
                          temporary-class: <multiple-value-temporary>);
    else
      make-with-temporary(lambda-env, <multiple-value-check-type>,
                          value: value-temp,
                          types: fixed-types,
                          temporary-class: <multiple-value-temporary>);
    end;
  mvt-transfer-values!(value-temp, check-temp);
  let (let-first, let-last)
    = join-2x1!(let-first, let-last, check-c);
  let body-env = env;
  for (spec in fixed-variable-specs,
       type in fixed-types,
       index from 0)
    let (new-env, variable) =
      bind-local-variable(body-env, spec-variable-name(spec), type);
    let extract-c
      = make-in-environment(body-env, <extract-single-value>,
                            value: check-temp,
                            index: index,
                            temporary: variable);
    let (_let-first, _let-last)
      = join-2x1!(let-first, let-last, extract-c);
    let-first := _let-first;
    let-last  := _let-last;
    variable.generator := extract-c;
    body-env := new-env;
  end for;
  if (rest-variable-spec)
    let (new-env, variable) =
      bind-local-variable
        (body-env, spec-variable-name(rest-variable-spec), rest-type);
    let rest-c
      = make-in-environment(body-env, <extract-rest-value>,
                            value: check-temp,
                            index: size(fixed-variable-specs),
                            temporary: variable);
    let (_let-first, _let-last)
      = join-2x1!(let-first, let-last, rest-c);
    let-first := _let-first;
    let-last  := _let-last;
    variable.generator := rest-c;
    body-env := new-env;
  end if;
  let (body-first, body-last, body-temp) =
    do-convert(body-env, context, body);
  join-2x2-t!(let-first, let-last, body-first, body-last, body-temp);
end;

// TODO: Use an :operand constraint here rather than the "delay expression
// parsing" trick.

define &converter \let
  { \let ?:name :: ?type = ?:expression; ?:body }
    => do-convert-single-value-let(env, context, name, type, expression, body);
  { \let (?:name :: ?type) = ?:expression; ?:body }
    => do-convert-single-value-let(env, context, name, type, expression, body);
  { \let (?parameters:*) = ?:expression; ?:body }
    => do-convert-let(env, context, parameters, expression, body);
  { \let \handler (?type, ?options:*) = ?fn:expression; ?:body }
    => convert(env, context, #{ %let-handler ((?type, ?options) = ?fn) ?body end });
  { \let \handler ?type = ?fn:expression; ?:body }
    => convert(env, context, #{ %let-handler ((?type) = ?fn) ?body end });
type:
  { ?:expression }
    => #{ ?expression }
end &converter;

define program-warning <unexpected-name-in-anonymous-method>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string
    "Unexpected name %s in anonymous method - ignoring.";
  format-arguments
    variable-name;
end program-warning;

define &converter \method
  { \method ?:name ?method-form:* end }
    => begin
         note(<unexpected-name-in-anonymous-method>,
              source-location: fragment-source-location(name),
              context-id: dfm-context-id(env),
              variable-name: name);
         do-convert-method (env, context, #f, method-form, #f);
       end;
  { \method ?method-form:* end }
    => do-convert-method (env, context, #f, method-form, #f);
end &converter;

define &converter \generic-method
  { \generic-method ( ?form:expression ) ?method-form:* end }
    => do-convert-method (env, context, fragment-value(form), method-form, #t);
end &converter;

define &converter \%signature
  { \%signature(?spec:*) }
    => convert-signature(env, fragment-value(spec));
end &converter;

define &converter \constant-method
  { \constant-method (?form:expression) ?method-form:expression end }
    => do-convert-method
         (env, context, fragment-value(form), fragment-value(method-form), #f);
end &converter;

define method do-convert-method
    (env, context :: <value-context>, definition, form,
     generic-method? :: <boolean>)
  let name = definition & form-variable-name(definition);
  let (sig-spec, body) = parse-method-signature(#f, form);
  if (generic-method?)
    ensure-next-method-binding(sig-spec);
  end if;
  let (sig-first, sig-last, sig-t) =
    convert-signature(env, sig-spec);
  let the-method =
    compute-method-explicitly
      (<&method>, definition, name, sig-spec, as-body(body));
  let (function-first, function-last, single-function-t) =
    convert-method-and-signature(env, sig-t, the-method);
  let (function-first, function-last, function-t) =
    match-values-with-context
      (env, context, function-first, function-last, single-function-t);
  let (function-first, function-last)
    = if (instance?(function-t, <temporary>) // make-closure?
            & ~definition                 // but no definition
            )
        let make-closure-c = lambda-make-closure(the-method);
        let init-closure-c
          = make-in-environment
              (env, <initialize-closure>,
               closure: single-function-t, method: the-method);
        computation-init-closure(make-closure-c) := init-closure-c;
        join-2x1!(function-first, function-last, init-closure-c)
      else
        values(function-first, function-last)
      end if;
  let (first-c, last-c)
    = join-2x2!(sig-first, sig-last, function-first, function-last);
  convert-lambda-into*-d
    (env, single-function-t, the-method, body-spec(the-method));
  values(first-c, last-c, function-t)
end method;

define function convert-method-to-model-as
    (class :: <class>, name, form) => (model :: <&method>)
  let (sig-spec, body) = parse-method-signature(name, form);
  let signature
    = compute-signature(#f, sig-spec);
  let body
    = as-body(body);
  let model
    = compute-method-explicitly
        (class, #f, name, sig-spec, body);
  ^function-signature(model) := signature;
  model
end;

define function convert-method-to-model (name, form) => (model :: <&method>)
  convert-method-to-model-as(<&method>, name, form);
end;

define serious-program-warning <invalid-syntax-for-local-method>
  format-string    "Invalid syntax for local method - ignoring.";
end serious-program-warning;

define method convert-inlineable
    (env :: <environment>, context :: <value-context>)
  let lambda = lambda(lambda-environment(env));
  lambda-inlineable?(lambda) := #t;
  match-values-with-context
    (env, context, #f, #f, #f)
end method;

define &converter %inlineable
  { %inlineable() }
    => convert-inlineable(env, context);
end &converter;

define &converter \local
  { \local ?local-methods:*; ?:body }
    => if (every?(method-fragment?, local-methods))
         convert-labels(env, context, local-methods, body);
       elseif (every?(macro-fragment?, local-methods))
         convert-macros(env, context, local-methods, body);
       else
         // error
       end;
local-methods:
  { }
    => #();
  { ?local-method:*, ?local-methods:* }
    => begin
         if (method-fragment?(local-method) | macro-fragment?(local-method))
           pair(local-method, local-methods)
         else
           note(<invalid-syntax-for-local-method>,
                source-location: fragment-source-location(local-method),
                context-id:      dfm-context-id(env));
           local-methods
         end;
       end;
local-method:
  { ?method-form:* ?:name } // end method foo
    => method-form;
  { ?method-form:* }
    => method-form;
end &converter;

// Block is build in terms of the primitive converters:
//
//   %with-exit (?name) ?body end
//   %with-cleanup ?body cleanup ?cleanup-body end
//   %with-afterwards ?body afterwards ?afterwards-body end
//   %with-exception (?stuff) ?body exception ?afterwards-body end

define &macro block

  { block (?:name) ?ebody end }
     => #{ %with-exit (?name) ?ebody end }
  { block () ?ebody end }
     => #{ ?ebody }

 ebody: // Left-recursive so leftmost clause is innermost
  {   ...
    \exception (?excp:*, /* #rest */ ?options:*) // TODO: Use #rest.
      ?:body }
   => #{ %with-exception (?excp, ?options)
          ...
        exception
          ?body
        end }
  { ?abody \cleanup ?cleanup-body:body}
    => #{ %with-cleanup
           ?abody
         cleanup
           ?cleanup-body
         end }
  { ?abody }
    => #{ ?abody }

 abody:
  { ?main:body \afterwards ?after:body }
    => #{ %with-afterwards
           ?main
         afterwards
           ?after
         end }
  { ?main:body }
    => #{ ?main }

/*
 excp:
  { ?type:expression }           => #{ ?type }
  { ?:name :: ?type:expression } => #{ ?type, condition: ?name }
*/

end &macro;

define &converter %with-afterwards
  { %with-afterwards ?:body \afterwards ?after-body:body end }
    => convert-begin-1(env, context, body, after-body);
end &converter;

define &converter %with-cleanup
  { %with-cleanup ?:body \cleanup ?cleanup-body:body end }
    => convert-unwind-protect(env, context, body, cleanup-body);
end &converter;

define &converter %with-exit
  { %with-exit (?:name) ?:body end }
    => convert-bind-exit(env, context, name, body);
end &converter;

// TODO: Use #rest ?options again.

define &macro %with-exception
  { %with-exception (?type:expression, /* #rest */ ?options:*) ?stuff:* end }
    => #{ %with-exception (_dummy_ :: ?type, ?options) ?stuff end }
  { %with-exception (?:name :: ?type:expression, #rest ?options:*)
      ?protected:body
    \exception
      ?handling:body
    end }
    // TODO: URGENT: Much of this expansion is screwed. Get it right!
    // The handler really does have to take a non-local exit to
    // unwind things before it executes the handler body.
    => #{ block (_unwind-exception_)
           let _original-handlers_ = *current-handlers*;
           local method _handler-function_ (?name :: ?type, _next-handler_)
             %dynamic-extent(_handler-function_);
             *current-handlers* := _original-handlers_;
             let (#rest all-values) = begin ?handling end;
             apply(_unwind-exception_, all-values);
             // let _value_ = begin ?handling end;
             // _unwind-exception_(_value_);
           end method;
           *current-handlers*
             := pair(make-handler(?type, _handler-function_, ?options),
                     *current-handlers*);
           block ()
             ?protected
           cleanup
             *current-handlers* := _original-handlers_;
           end;
         end };
end &macro;

define method convert-dynamic-extent
    (env :: <environment>, context :: <value-context>, name)
  let var = lookup(env, name);
  var.dynamic-extent? := #t;
  match-values-with-context
    (env, context, #f, #f, #f)
end method;

define &converter %dynamic-extent
  { %dynamic-extent(?:name) }
    => convert-dynamic-extent(env, context, name);
end &converter;

define &converter %guarantee-type
  { %guarantee-type(?:expression, ?type:*) }
    => convert-guarantee-type(env, context, expression, type);
end &converter;

define function convert-guarantee-type
    (env :: <environment>, context :: <value-context>, expression, type)
 => (first :: <computation>, last :: <computation>, ref :: <value-reference>);
  let (expr-first, expr-last, expr-t) = convert-1(env, expression);
  let (type-first, type-last, type-t) = convert-type-expression(env, type);
  let (first-c, last-c)
    = join-2x2!(expr-first, expr-last, type-first, type-last);
  let (guarantee, guarantee-t) =
    make-with-temporary(env, <guarantee-type>,
                        value: expr-t,
                        type: type-t);
  let (first-c, last-c)
    = join-2x1!(first-c, last-c, guarantee);
  match-values-with-context
    (env, context, first-c, last-c, guarantee-t)
end;

define &converter %return-from
  { %return-from(?:name, ?:expression) }
    => convert-return-from(env, context, name, expression);
end &converter;

define &macro %let-handler
  { %let-handler ((?type:expression, #rest ?options:*) = ?fn:expression)
      ?:body
    end }
    => #{ let _original-handlers_ = *current-handlers*;
         block ()
           *current-handlers*
             := pair(make-handler(?type, ?fn, ?options), *current-handlers*);
           ?body
         cleanup
           *current-handlers* := _original-handlers_;
         end }
end &macro;

define &converter %initialize-binding
  { %initialize-binding (?binding:name, ?value:expression) }
    => convert-initialization(env, context, binding, value);
end &converter;

define &converter %initialize-binding-type
  { %initialize-binding-type (?binding:name, ?value:expression) }
    => convert-type-initialization(env, context, binding, value);
end &converter;

//// Prefix converters left to deal with.

define method convert-stack-vector
    (env :: <environment>, context :: <value-context>, argument-forms)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  let (arg-first, arg-last, temporaries)
    = convert-expressions(env, argument-forms);
  let (call, temporary)
    = make-with-temporary(env, <stack-vector>,
                          temporary-class: <stack-vector-temporary>,
                          arguments: temporaries);
  let (first-c, last-c) = join-2x1!(arg-first, arg-last, call);
  temporary.number-values := size(argument-forms);
  match-values-with-context
    (env, context, first-c, last-c, temporary)
end method;

define &converter %stack-vector
  { %stack-vector(?arguments) }
    => convert-stack-vector(env, context, arguments);
 arguments:
  { }
    => #();
  { ?:expression }
    => list(expression);
  { ?:expression, ... }
    => pair(expression, ...);
end &converter;

// define &converter %slot-value
//   { %slot-value
//       (?object:expression, ?slotd:expression, ?offset:expression) }
//     => begin
//          let (arg-first, arg-last, arg-temps)
//            = convert-expressions(env, vector(object, slotd, offset));
//          let object-temp = arg-temps[0];
//          let slotd-temp = arg-temps[1];
//          let offset-temp = arg-temps[2];
//          let (slot-value, temp)
//            = make-with-temporary
//                (env, <slot-value>,
//                 instance:        object-temp,
//                 slot-descriptor: slotd-temp,
//                 slot-offset:     offset-temp);
//          let (all-first, all-last)
//            = join-2x1!(arg-first, arg-last, slot-value);
//          match-values-with-context
//            (env, context, all-first, all-last, temp);
//        end;
// end &converter;

define &converter %slot-value-setter
  { %slot-value-setter
      (?val:expression,
         ?object:expression, ?slotd:expression, ?offset:expression) }
    => begin
         let (arg-first, arg-last, arg-temps)
           = convert-expressions(env, vector(val, object));
         let val-temp = arg-temps[0];
         let object-temp = arg-temps[1];
         let (slot-value, temp)
           = make-with-temporary
               (env, <slot-value-setter>,
                new-value:       val-temp,
                instance:        object-temp,
                slot-descriptor: fragment-value(slotd),
                slot-offset:     fragment-value(offset));
         let (all-first, all-last)
           = join-2x1!(arg-first, arg-last, slot-value);
         // break("%%slot-value-setter");
         match-values-with-context
           (env, context, all-first, all-last, temp);
       end;
end &converter;

// define &converter %repeated-slot-value
//   { %repeated-slot-value
//       (?object:expression, ?slotd:expression, ?index:expression) }
//     => #f;
// end &converter;
//
// define &converter %repeated-slot-value-setter
//   { %repeated-slot-value
//       (?val:expression,
//          ?object:expression, ?slotd:expression, ?index:expression) }
//     => #f;
// end &converter;

//// Library special form.

define method convert-current-library (env)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>)
  let lib = language-definition(current-library-description());
  convert-reference(env, $single, namespace-model(lib))
end method;

define &converter \%current-library
  { \%current-library() }
    => convert-current-library(env);
end &converter;



//// Thread special forms.

define &converter %conditional-update-variable
  { %conditional-update-variable
      (?:name, ?newval:expression, ?oldval:expression) }
    => do-convert-condition-update-variable(env, context, name, newval, oldval);
end &converter;

define method do-convert-condition-update-variable
    (env :: <environment>, context :: <value-context>,
     name :: <name-fragment>, newval :: <fragment>, oldval :: <fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>);
  convert-condition-update-variable-using-binding
    (env, context, lookup(env, name), name, newval, oldval);
end method;

define program-warning <local-binding-conditional-update>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Conditional update of local binding %=.";
  format-arguments variable-name;
end program-warning;

define program-warning <undefined-binding-conditional-update>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Conditional update of undefined binding %=.";
  format-arguments variable-name;
end program-warning;

define program-warning <non-locked-binding-conditional-update>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Conditional update of binding %= not declared locked.";
  format-arguments variable-name;
end program-warning;

define method convert-condition-update-variable-using-binding
    (env :: <environment>, context :: <value-context>,
     binding :: <binding>, name :: <name-fragment>,
     newval :: <fragment>, oldval :: <fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>);
  note(<local-binding-conditional-update>,
       source-location: fragment-source-location(name),
       context-id:      dfm-context-id(env),
       variable-name:   name);
  convert-error-call
    (env, context, format-to-string
            ("Conditional update of local binding %=.", as(<string>, name)));
end method;

define method convert-condition-update-variable-using-binding
    (env :: <environment>, context :: <value-context>,
     binding :: <module-binding>,
     name :: <name-fragment>, newval :: <fragment>, oldval :: <fragment>)
 => (first :: false-or(<computation>), last :: false-or(<computation>), ref :: <value-reference>);
  if (~defined?(binding))
    note(<undefined-binding-conditional-update>,
         source-location: fragment-source-location(name),
         context-id:      dfm-context-id(env),
         variable-name:   name);
    convert-error-call
      (env, context,
       format-to-string
         ("Conditional update of undefined binding %=.",
          as(<string>, name)));
  elseif (~binding-locked?(binding))
    note(<non-locked-binding-conditional-update>,
         source-location: fragment-source-location(name),
         context-id:      dfm-context-id(env),
         variable-name:   name);
    convert-error-call
      (env, context,
       format-to-string
         ("Conditional update of binding %= not declared locked.",
          as(<string>, name)));
  else
    let (value-first, value-last, value-temporary) = convert-1(env, newval);
    let (test-first, test-last, test-temporary) = convert-1(env, oldval);
    let (first-c, last-c)
      = join-2x2!(value-first, value-last, test-first, test-last);
    let (update-c, temp)
      = make-with-temporary
          (env, <conditional-update!>,
           binding:    binding,
           value:      value-temporary,
           test-value: test-temporary);
    record-binding-assignment(binding, update-c);
    let (first-c, last-c) = join-2x1!(first-c, last-c, update-c);
    match-values-with-context
      (env, context, first-c, last-c, temp)
  end;
end method;

//// Conversion of top level initializers.

define method convert-top-level-initializer
    (form, #key debug-name = "top-level-initializer")
  // TODO: This works around the lack of true hygiene in the compiler
  // at the moment. The template fed to convert-method-to... loses
  // context, so we have to set it up dynamically.
  with-fragment-info (form)
    let form = #{ () => () ?form };
    let model = convert-method-to-model(debug-name, form);
    model.lambda-initializer? := #t;
    model
  end;
end method;

//// Hack!!! Displaced.

// See dfmc-namespaces for the generic function definition corresponding
// to these methods.

define compiler-sideways method binding-constant-model-object
    (binding :: <module-binding>, #key error-if-circular? = #t)
 => (model-object, found? :: <boolean>)
  if (constant?(binding))
    let model-object =
      binding-model-object(binding,
                           default: $unfound,
                           error-if-circular?: error-if-circular?);
    if (found?(model-object) & ~instance?(model-object, <unknown>))
      values(model-object, #t)
    else
      values(#f, #f)
    end
  else
      values(#f, #f)
  end
end;

define compiler-sideways method binding-constant-model-object
    (binding :: <lexical-variable>, #key error-if-circular? = #t)
 => (model-object, found? :: <boolean>)
  values(#f, #f)
end;

define compiler-sideways method binding-constant-model-object
    (binding :: <interactor-binding>, #key error-if-circular? = #t)
 => (model-object, found? :: <boolean>)
  values(#f, #f)
end;

define method lookup-constant-model-object
    (env, name :: <variable-name-fragment>, #key reference? = #t)
 => (model-object, found? :: <boolean>)
  let binding = lookup(env, name, reference?: reference?);
  if (binding)
    binding-constant-model-object(binding)
  else
    values(#f, #f)
  end
end method;

//// Evaluators.

// TODO: This is all a gross hack, to be replaced when the real evaluator
// comes on stream again.

define method &top-level-eval
    (fragment :: <boolean>, #key on-failure = #f) => (vals)
  make-compile-time-literal(fragment);
end method;

define method &top-level-eval
    (fragment :: <object>, #key on-failure = #f) => (vals)
  &top-level-eval(as-expression(fragment), on-failure: on-failure);
end method;

define method &top-level-eval
    (fragment :: <&top>, #key on-failure = #f) => (vals)
  fragment
end method;

define method &top-level-eval
    (fragment :: <expression-fragment>, #key on-failure = #f)
 => (vals)
  on-failure
end method;

define method &top-level-eval
    (fragment :: <variable-name-fragment>, #key on-failure = #f) => (models)
  let binding = lookup(#f, fragment);
  if (binding)
    &top-level-eval(binding, on-failure: on-failure)
  else
    on-failure
  end;
end method;

define method &top-level-eval
    (binding :: <module-binding>, #key on-failure = #f) => (models)
  let (model, found?) = binding-constant-model-object(binding);
  if (found?) model else on-failure end
end method;

define method &top-level-eval
    (fragment :: <literal-constant-fragment>, #key on-failure = #f)
 => (models)
  let object = fragment-value(fragment);
  if (instance?(object, <module-binding>))
    &top-level-eval(object, on-failure: on-failure)
  else
    make-compile-time-literal(object)
  end;
end method;

define method &top-level-eval
    (fragment :: <function-call-fragment>, #key on-failure = #f)
      => (models)
  let function = ^top-level-eval(fragment-function(fragment));
  let override = lookup-compile-stage-function(function);
  if (override)
    let argument-values =
      ^top-level-eval-sequence(fragment-arguments(fragment));
    if (argument-values)
      block ()
        apply(override, argument-values);
      exception (<error>)
        on-failure
      end
    else
      on-failure
    end;
  else
    on-failure
  end;
end method;

// this method allows macro calls to usefully be used as specializers
define method &top-level-eval
    (fragment :: <macro-call-fragment>, #key on-failure = #f) => (vals);
  let macro-form = fragment.fragment-macro;
  let definition = macro-definition(macro-form);
  if (definition &
        ~instance?(definition, <&converter-definition>) &
        ~instance?(definition, <&definition-definition>))
    &top-level-eval(form-expander(definition)(#f, fragment));
  else
    on-failure
  end if;
end;

// This method is necessary because macros always expand
// into <body-fragment>s
define method &top-level-eval
    (fragment :: <body-fragment>, #key on-failure = #f) => (vals);
  let constituents = fragment-constituents(fragment);
  if (constituents.size = 1)
    &top-level-eval(constituents[0]);
  else
    // we can only do things for value here so fail if we got a body
    // that might have side effects.
    on-failure
  end;
end;

define constant $eval-failure :: <list> = list("eval failure");

define method ^top-level-eval-sequence
    (fragments :: <sequence>, #key on-failure = #f) => (models)
  block (return)
    collecting (results)
      for (fragment in fragments)
        let model = ^top-level-eval(fragment, on-failure: $eval-failure);
        if (model ~== $eval-failure)
          collect-into(results, model);
        else
          return(on-failure);
        end;
      end;
      collected(results);
    end;
  end;
end method;

define inline method ^top-level-eval (fragment, #rest options)
  apply(&top-level-eval, fragment, options);
end method;

define method ^top-level-eval-using-optimization
    (fragment, #key on-failure = #f, type-variables = #[]) => (maybe-value)
  let m = convert-method-to-model-as
            (<&method>, "type-initializer", #{ () ?fragment });

  ensure-method-optimized(m);
  let (constant?, result) = lambda-returns-constant?(m);
  if (constant?)
    result
  else
    on-failure
  end;
end method;

define method ^eval-type-expression-sequence
    (env :: <simple-object-vector>, fragments :: <sequence>, #key on-failure = #f) => (models)
  block (return)
    collecting (results)
      for (fragment in fragments)
        let model = &eval-type-expression(env, fragment, on-failure: $eval-failure);
        if (model ~== $eval-failure)
          collect-into(results, model);
        else
          return(on-failure);
        end;
      end;
      collected(results);
    end;
  end;
end method;

define method &eval-type-expression
    (env :: <simple-object-vector>, binding :: <module-binding>, #key on-failure = #f) => (models)
  let (model, found?) = binding-constant-model-object(binding);
  if (found?) model else on-failure end
end method;

define method &eval-type-expression
    (env :: <simple-object-vector>, fragment :: <literal-constant-fragment>, #key on-failure = #f) 
 => (models)
  let object = fragment-value(fragment);
  if (instance?(object, <module-binding>))
    &eval-type-expression(env, object, on-failure: on-failure)
  else
    make-compile-time-literal(object)
  end;
end method;

define method &eval-type-expression
    (env :: <simple-object-vector>, fragment :: <variable-name-fragment>, #key on-failure = #f) => (models)
  let tv-value = choose(compose(curry(\==, fragment.fragment-name),
				^type-variable-name),
			env);
  if (tv-value.size == 1)
    tv-value.first;
  elseif (tv-value.size > 1)
    error("unexpected!")
  else
    let binding = lookup(#f, fragment);
    if (binding)
      &eval-type-expression(env, binding, on-failure: on-failure)
    else
      on-failure
    end
  end;
end method;

define method &eval-type-expression 
    (env :: <simple-object-vector>, fragment :: <function-call-fragment>, #key on-failure = #f) 
      => (models)
  let function = ^top-level-eval(fragment-function(fragment));
  let override = lookup-compile-stage-function(function);
  if (override)
    let argument-values =
      ^eval-type-expression-sequence(env, fragment-arguments(fragment));
    if (argument-values)
      block ()
        apply(override, argument-values);
      exception (<error>)
        on-failure
      end
    else
      on-failure
    end;
  else
    on-failure
  end;
end method;
// Must be a type.
define method ^top-level-eval-type (fragment, #key on-failure = #f, type-variables = #[])
  // Try quickie top level eval.
  let result 
    //= ^top-level-eval(fragment, on-failure: on-failure);
    = &eval-type-expression(type-variables, fragment, on-failure: on-failure);
  // Try harder if the above failed.
  let result
    = if (result == on-failure)
        ^top-level-eval-using-optimization(fragment, on-failure: on-failure, type-variables: type-variables)
      else
        result
      end;
  if (result == on-failure)
    on-failure
 // elseif (~instance?(result, <&type>))
 //   // Put a warning here.
 //   on-failure
  else
    result
  end;
end method;

// TODO: Lose these dummy definitions.

define compiler-sideways method &eval (env, fragment) => (object)
  block ()
    ^top-level-eval(fragment);
  exception (<error>)
    &unbound;
  end block;
end method;

///////
/////// STATIC CASE WHICH WILL GO AWAY
///////

define function parse-parameters-into
    (env :: <environment>, lambda-env :: <lambda-lexical-environment>,
     sig-spec :: <signature-spec>)
  let type-vars
    = spec-type-variables(sig-spec);
  let required-specs
    = spec-argument-required-variable-specs(sig-spec);
  let key-specs
    = spec-argument-key-variable-specs(sig-spec);
  let spec-rest?
    = spec-argument-rest?(sig-spec);
  let spec-key?
    = spec-argument-key?(sig-spec);
  let keys-start =
    size(required-specs) + size(type-vars)
      + if (spec-rest? | spec-key?) 1 else 0 end;
  let variables
    = make(<simple-object-vector>, size: keys-start + size(key-specs));
  let var-index
    = 0;
  local method add-to-variables!(variable)
          variables[var-index] := variable;
          var-index := var-index + 1;
          variable
        end,
        method push-variable! (name, variable)
          add-to-variables!(add-variable!(lambda-env, name, variable));
        end,
        method insert-rest-variable! (name)
          push-variable!(name,
			 make(<lexical-rest-variable>,
			      name: name,
			      environment: lambda-env));
	end;
  for (var-spec in type-vars)
    let name = spec-variable-name(var-spec);
    push-variable!(name,
		   make(<lexical-required-type-variable>,
			name:
			  name,
			environment: 
			  lambda-env,
			specializer: 
			  spec-type-expression(var-spec)));
  end;
  for (var-spec in required-specs)
    let name = spec-variable-name(var-spec);
    push-variable!(name,
                   make(<lexical-required-variable>,
                        name:
                          name,
                        environment:
                          lambda-env,
                        // TODO: dynamic type expressions
                        specializer:
                          &eval(env, spec-type-expression(var-spec))));
  end;
  if (spec-rest?)
    insert-rest-variable!
      (spec-variable-name(spec-argument-rest-variable-spec(sig-spec)));
  elseif (spec-key?)
    insert-rest-variable!
      (implicit-rest-variable-name());
  end;
  for (key-spec in key-specs)
    add-to-variables!(make(<lexical-keyword-variable>,
                           name: spec-variable-name(key-spec),
                           environment: lambda-env));
  end;
  values(variables, keys-start)
end;

define program-warning <bad-keyword-type>
  slot condition-type-given,
    required-init-keyword: type-given:;
  slot condition-more-info,
    required-init-keyword: more-info:;
  format-string    "Keyword type \"%=\" %s.";
  format-arguments type-given, more-info;
end;

define function convert-keyword-initialization
    (env :: <environment>,
     keys-first :: false-or(<computation>),
     keys-last :: false-or(<computation>),
     key-spec :: <key-variable-spec>,
     key :: <symbol>,
     variable :: <lexical-keyword-variable>,
     shared? :: <boolean>, // shared with others with the same key
     input? :: <boolean>,  // the input of the shared set
     specifiers :: <simple-object-vector>,
     index :: <integer>, f :: <&lambda>)
 => (new-env :: <local-lexical-environment>,
     new-keys-first :: false-or(<computation>),
     new-keys-last :: false-or(<computation>))
  let j = index * 2;
  let name = spec-variable-name(key-spec);
  let default-expression = spec-default-expression(key-spec);

  // TODO:  evaluate type expressions in the correct environment,
  //        and handle dynamic types.
  let type = ^top-level-eval(spec-type-expression(key-spec));
  // (gts,98jan), change from simply error'ing
  let (keys-first, keys-last) =
    if (instance?(type, <&type>))
      values(keys-first, keys-last);
    else
      let form = f & model-definition(f);
      note(<bad-keyword-type>,
           source-location: f & model-source-location(f),
           form: form,
           type-given: spec-type-expression(key-spec),
           more-info: if (type)
                        "cannot be evaluated at compile time"
                      else
                        "is not a <type>"
                      end if);
      type := dylan-value(#"<object>");
      let (type-error-f, type-error-l, type-error-ref) =
        convert-error-call(env, $ignore,
                           concatenate("Invalid keyword type \"",
                                       as(<string>, spec-type-expression(key-spec)),
                                       "\"."));
      join-2x2!(keys-first, keys-last, type-error-f, type-error-l);
    end if;

  let (key-literal?, key-literal-value) =
    if (instance?(default-expression, <literal-constant-fragment>))
      let key-literal-value =
        make-compile-time-literal(fragment-value(default-expression));
      if (^instance?(key-literal-value, type))
        values(#t, key-literal-value)
      end;
    end;

  let (new-env, new-keys-first, new-keys-last, defaulted-value) =
    if (~shared? & key-literal?)
      specifiers[j] := key;
      specifiers[j + 1] := key-literal-value;
      values(make-local-lexical-environment(name, variable, type, env),
             keys-first, keys-last, variable)
    else
      specifiers[j] := key;
      specifiers[j + 1] := &unbound;
      let function =
        make-dylan-reference(#"==");
      let unbound-temp =
        make-object-reference(&unbound);
      // TODO: FIX AFTER WORKING
      let (call, temporary) =
        make-with-temporary(env, <simple-call>,
                            function: function,
                            arguments: vector(variable, unbound-temp));
      let (keys-first, keys-last)
        = join-2x1!(keys-first, keys-last, call);

      let (default-first, default-last, default-t) =
        convert-1(env, default-expression);
      // TODO: MOVE INTO MERGE
      let (default-mt, default-mt-t) =
        // We mustn't eliminate the defaulting code if an input keyword
        // is attached to more than one variable.
        if (~shared?)
          make-with-temporary
            (env, <keyword-default>,
             value: default-t,
             specifiers: specifiers,
             index: index,
             keyword-variable: variable);
        else
          make-with-temporary
            (env, <temporary-transfer>,
             value: default-t);
        end;
      let (default-first, default-last)
        = join-2x1!(default-first, default-last, default-mt);
      let (new-env, new-variable) =
        bind-local-variable(env, name, #f);
      let if-c = make-in-environment
                   (env, <if>, test: temporary,
                    consequent: default-first, alternative: #f);
      let merge-c =
        make-in-environment
          (env, <if-merge>,
           left-value: default-mt-t,
           left-previous-computation: default-last,
           right-value: variable,
           right-previous-computation: if-c,
           temporary: new-variable,
           previous-computation: if-c);
      new-variable.generator := merge-c;
      if-c.next-computation  := merge-c;
      if-c.alternative       := merge-c;
      let (keys-first, keys-last)
        = join-2x2!(keys-first, keys-last, if-c, merge-c);
      default-first.previous-computation := if-c;
      default-last.next-computation := merge-c;
      values(new-env, keys-first, keys-last, new-variable)
    end;

  if (type-checked-at-run-time?(type))
    let type-temp =
      make-object-reference(type);
    let (new-env, new-variable) =
      bind-local-variable(new-env, name, type-temp);
    let check-c =
      make-in-environment
        (env, <keyword-check-type>,
         value: defaulted-value, type: type-temp,
         temporary: new-variable);
    let (new-keys-first, new-keys-last)
      = join-2x1!(new-keys-first, new-keys-last, check-c);
    new-variable.generator := check-c;
    values(new-env, new-keys-first, new-keys-last)
  else
    values(new-env, new-keys-first, new-keys-last)
  end
end;


define program-warning <multiple-values-declared-in-single-value-lambda>
  slot condition-lambda,
    required-init-keyword: lambda:;
  format-string    "Multiple values declared in single value lambda %=.";
  format-arguments lambda;
end program-warning;

///
/// DYLAN SPECIFIC RETURN TYPE RULES
///
define function maybe-tighten-return-values
    (env :: <environment>, fn :: <&lambda>, fixed-types)
  when (^make-method?(fn))
    let sig = ^function-signature(fn);
    when (sig)
      let sig-class = ^make-return-class-from-signature(fn);
      let vtype     = first(^signature-values(sig));
      unless (vtype == sig-class)
        let type-temp = make-object-reference(sig-class);
        remove-user!(vtype, first(fixed-types));
        first(fixed-types) := type-temp;
      end unless;
    end when;
  end when;
end function;

define method convert-lambda-into*
    (env :: <environment>, f :: <&lambda>, the-body, #key multiple-values? = #t)
  let sig-spec = signature-spec(f);
  let lambda-env = make(<lambda-lexical-environment>, outer: env, lambda: f);
  add-inner!(env, lambda-env);
  let (variables, keys-start) =
    parse-parameters-into(env, lambda-env, sig-spec);
  f.environment := lambda-env;
  f.parameters := variables;
  let bind-computation = make-in-environment(lambda-env, <bind>);
  let (copy-rest-env, rest-temp)
    = if (spec-argument-optionals?(sig-spec))
        bind-rest(lambda-env, f, sig-spec)
      else
        values(lambda-env, #f)
      end if;
  let next-variable-spec = spec-argument-next-variable-spec(sig-spec);
  let (next-method-env, next-temp)
    = if (next-variable-spec)
        bind-next-method(copy-rest-env, next-variable-spec);
      else
        values(copy-rest-env, #f);
      end;
  let inner-env = next-method-env;
  let keys-first = bind-computation;
  let keys-last  = bind-computation;
  let key-variable-specs = spec-argument-key-variable-specs(sig-spec);
  if (~empty?(key-variable-specs))
    let (specifiers, key-counts)
      = make-key-variable-specifiers-vector(env, key-variable-specs);
    for (key-spec in key-variable-specs,
         key-var-index :: <integer> from keys-start,
         key-index :: <integer> from 0)
      // Does this key-spec share a key with other key specs, and
      // if so, is it the input variable they all share?
      let key = &eval(env, spec-keyword-expression(key-spec));
      let entry = element(key-counts, key);
      let shared-index = entry.head;
      let shared? = (entry.tail ~== 1);
      let input? = (shared-index == key-index);
      let var = variables[key-var-index];
      let input-var
        = if (shared?)
            variables[keys-start + shared-index]
          else
            var
          end;
      let (new-env, new-keys-first, new-keys-last) =
        convert-keyword-initialization
          (inner-env,
           keys-first, keys-last, key-spec,
           key, input-var, shared?, input?, specifiers, key-index, f);
      inner-env := new-env;
      keys-first := new-keys-first;
      keys-last  := new-keys-last;
    end for;
    f.keyword-specifiers := specifiers;
  end if;
  let required-values
    = spec-value-required-variable-specs(sig-spec);
  let rest-type
    = spec-value-rest-variable-spec(sig-spec);
  let number-of-required-values
    = size(required-values);
  let body-context
    = if (multiple-values?)
        make(<multiple-value-context>,
             mvc-num-values: number-of-required-values,
             mvc-rest?:      rest-type)
      else
        $single
      end if;
  let (body-first, body-last, value-temp)
    = convert-body(inner-env, body-context, the-body);
  let (keys-first, body-last)
    = join-2x2!(keys-first, keys-last, body-first, body-last);
  let (body-last, value-temp)
    = if (multiple-values?)
        let types-first = body-last;
        let types-last  = body-last;
        local method convert-type (variable-spec)
                let (type-first, type-last, type-temp) =
                  convert-type-expression
                    (lambda-env, spec-type-expression(variable-spec));
                let (_types-first, _types-last)
                  = join-2x2!(types-first, types-last, type-first, type-last);
                types-first := _types-first;
                types-last  := _types-last;
                type-checked-at-run-time?(fast-constant-value(type-temp)) &
                  type-temp
              end;
        let fixed-types = map(convert-type, required-values);

        // should probably just call match-values-with-context() for the following:
        let (adj-c, adj-temp) =

/***** gts 20nov97
  No longer do adjusts, because harp back-end does adjust after a function call.
  That is, for a computation such as:
     *t1 := call(f, ...)
  where *t1 has sig. (2, #rest) and f has sig. (#rest), the MV area will be
  guarangeed to have at least two values (possibly #f's) before the next
  computation.

          if (~ rest-type)
            // if an exact signature (no #rest return), enforce the exact number of
            // returned values
            let(c, t)
              = make-with-temporary
                  (lambda-env, <adjust-multiple-values>,
                   value: value-temp,
                   number-of-required-values: number-of-required-values,
                   temporary-class: <multiple-value-temporary>);
             mvt-transfer-values!(value-temp, t);
             values(c, t);
          elseif (number-of-required-values > 0)
            // rest-spec and some required values:
            // ensure that there are enough values so that mv-check-type later
            // is ok -- do adj-mv-rest, a.k.a. "adjust up".
            let(c, t)
              = make-with-temporary
                  (lambda-env, <adjust-multiple-values-rest>,
                   value: value-temp,
                   number-of-required-values: number-of-required-values,
                   temporary-class: <multiple-value-temporary>);
            mvt-transfer-values!(value-temp, t);
            values(c, t);
          else  // rest-spec and no required values -- no adjusts needed
 *****/
            values(#f, value-temp);
/*****          end if;  *****/

        let type-temp = rest-type & convert-type(rest-type);
        let (first, last, temp)
          = join-1x1-t!(types-last, adj-c, adj-temp);

        let (check-c, check-t)
          = if (type-temp)
              let (check-c, check-temp) =
                make-with-temporary
                  (lambda-env, <multiple-value-result-check-type-rest>,
                   value: temp,
                   types: fixed-types,
                   rest-type: type-temp,
                   temporary-class: <multiple-value-temporary>);
              mvt-transfer-values!(temp, check-temp);
              values(check-c, check-temp);
            elseif (size(fixed-types) > 0)
              maybe-tighten-return-values(env, f, fixed-types);
              let (check-c, check-temp) =
                make-with-temporary(lambda-env,
                  <multiple-value-result-check-type>,
                  value: temp,
                  types: fixed-types,
                  temporary-class: <multiple-value-temporary>);
              mvt-transfer-values!(temp, check-temp);
              values(check-c, check-temp);
            else
              values(#f, temp);
            end if;
          let (first, last, temp)
            = join-1x1-t!(last, check-c, check-t);
          values(last, temp);
      else  // not multiple-values?
        if (number-of-required-values > 1 | rest-type)
           note(<multiple-values-declared-in-single-value-lambda>,
                source-location: f & model-source-location(f),
                context-id:      dfm-context-id(env),
                expression:      f);
        end if;
        values(body-last, value-temp)
      end if;
  let return
    = make-in-environment
        (lambda-env, <return>, value: value-temp);
  bind-return(bind-computation) := return;
  join-1x1!(body-last, return);
  f.body := bind-computation;
  // TODO: should there be a f.body-last?
  // Was the next-method variable actually used?
  if (next-temp & used?(next-temp))
    convert-next-method-into(copy-rest-env, f, sig-spec, next-temp);
  else
    ^function-next?(f) := #f;
  end if;
  if (rest-temp & used?(rest-temp))
    convert-copy-rest-into(lambda-env, f, sig-spec, rest-temp);
  else
    lambda-rest?(f) := #f;
  end;
  strip-bindings(lambda-env);
  f
end method convert-lambda-into*;
