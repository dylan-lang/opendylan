Module:   dfmc-runtime-execution
Author:   Jonathan Bachrach
Synopsis: Evaluation of Runtime DFM programs
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *symbol-table* :: <object-table> = make(<table>);
define variable *loaded-libraries* :: <object-table> = make(<table>);

define variable *runstage* :: <object-table> = make(<table>);

define constant <closure> = <lambda>;

define macro closure-slot-definer
  { define closure-slot ?:name at ?offset:expression }
    => { define method ?name (function :: <closure>) => (res)
           environment-element(function, ?offset)
         end method;
         define method ?name ## "-setter" (value, function :: <closure>)
           environment-element(function, ?offset) := value
         end method }
end macro;

define closure-slot function-model at 0;

define constant $number-reserved-closure-slots = 1;

//// MACHINE STATE

define class <machine-state> (<object>)
  constant slot state-closure = #f,  init-keyword: closure:;
  constant slot state-&closure = #f, init-keyword: &closure:;
  constant slot state-temporaries :: <simple-object-vector>, // could be a repeated slot
    required-init-keyword: temporaries:;
  constant slot state-next-methods,
    required-init-keyword: next-methods:;
  constant slot state-loop-continues :: <object-table> = make(<table>);
  constant slot state-runstage :: <object-table>,
    required-init-keyword: runstage:;
end class <machine-state>;

define method initialize (state :: <machine-state>, #key, #all-keys)
  next-method();
  ^binding-value(state, dylan-binding(#"-")) := \-;
end method;

define method state-library (state :: <machine-state>) => (res)
  model-library(state-&closure(state))
end method;

define method external-object? (state :: <machine-state>, x) => (well?)
  model-library(x) ~== state-library(state)
end method;

define method external-binding? (state :: <machine-state>, x :: <module-binding>) => (well?)
  namespace-library-description(x.binding-home) ~== state-library(state)
end method;

define method load-dll (name) => ()
  format-out("DLL LOADING %s\n", name);
  let merged-name = merged-project-name(as(<string>, name));
  let project = lookup-named-project(as(<symbol>, merged-name));
  let dll-name = project & project.project-executable-name;
  format-out("  DLL-NAME %s\n", dll-name);
  unless (dll-name)
    error("Can't find library %s", name)
  end;
  load-library(dll-name);
end method;

define method ensure-loaded-dll (name)
  element(*loaded-libraries*, name, default: #f)
    | begin
        load-dll(name);
        element(*loaded-libraries*, name) := #t;
      end;
end method;

define method ^slow-binding-value
    (state :: <machine-state>, binding :: <module-binding>) => (value)
  if (external-binding?(state, binding))
    let identifier    = as(<string>, name(binding));
    let module        = binding-home(binding);
    let module-name   = namespace-name(module);
    let library       = home-library(module);
    let library-name  = namespace-name(library);
    ensure-loaded-dll(library-name);
    let value         = variable-value(identifier, module-name, library-name);
    value
  else
    let &value        = binding-model-or-hollow-object(binding);
    let  value        = runstage(state, &value);
    value
  end if;
end method;

define method ^binding-value
    (state :: <machine-state>, binding :: <module-binding>) => (value)
  let value = element(*symbol-table*, binding, default: not-found());
  if (found?(value))
    value
  else
    element(*symbol-table*, binding) := ^slow-binding-value(state, binding);
  end if;
end method;

define method ^binding-value-setter
    (value, state :: <machine-state>, binding :: <module-binding>)
  element(*symbol-table*, binding) := value;
end method;

define method lookup-external-object (state :: <machine-state>, x) => (value)
  let variable-name = model-variable-name(x);
  let binding       = untracked-lookup-binding(variable-name);
  ^binding-value(state, binding);
end method;

define method simple-method-iep (x :: <lambda>) => (res)
  // HACK: DO THIS RIGHT
  let mep-offset = 2; // slot-offset();
  initialized-slot-element(x, mep-offset)
end method;

define method lookup-external-object (state :: <machine-state>, x :: <&iep>) => (value)
  let function = lookup-external-object(state, function(x));
  primitive-wrap-machine-word(primitive-cast-pointer-as-raw(simple-method-iep(function)))
end method;

define method runstage (state :: <machine-state>, x) => (value)
  let y = element(state-runstage(state), x, default: not-found());
  if (found?(y))
    y
  else
    element(state-runstage(state), x)
      := if (external-object?(state, x))
           lookup-external-object(state, x)
         else
           make-runstage(state, x);
         end if;
  end if
end method;

define method runstage (state :: <machine-state>, x :: <string>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <symbol>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <empty-list>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <number>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <boolean>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <character>) => (value)
  x
end method;

define method runstage (state :: <machine-state>, x :: <&raw-machine-word>) => (value)
  primitive-wrap-machine-word(primitive-cast-pointer-as-raw(^raw-object-value(x)))
end method;

// define method runstage (state :: <machine-state>, x :: <&iep>) => (value)
//   // have to handle specially
//   #f
// end method;

define method make-runstage (state :: <machine-state>, x) => (value)
  error("No Runstage for %=.", x);
end method;

define method make-runstage (state :: <machine-state>, x :: <vector>) => (value)
  map(curry(runstage, state), x)
end method;

define method make-runstage (state :: <machine-state>, x :: <pair>) => (value)
  pair(runstage(state, head(x)), runstage(state, tail(x)))
end method;

define constant allocate-object = walker-allocate-object;

define method default-make-runstage
    (state :: <machine-state>, &object, #key class, extra-size = 0)
 => (value)
  let &class     = &object-class(&object);
  let  class     = class | runstage(state, &class);
  let &rpt-slotd = ^repeated-slot-descriptor(&class);
  let &size      = if (&rpt-slotd)
                     let &size-slotd = ^size-slot-descriptor(&rpt-slotd);
                     ^slot-value(&object, &size-slotd)
                   else
                     0
                   end if;
  let  size      = &size + extra-size;
  let object     = allocate-object(class, size);
  element(state-runstage(state), &object) := object;
  for (&slotd in ^instance-slot-descriptors(&class),
        slotd in  instance-slot-descriptors( class))
    block ()
      slot-value(object, slotd)
        := runstage(state, ^slot-value(&object, &slotd));
    exception (<error>)
      // slot type error for hollow objects
    end block;
  end for;
  when (&rpt-slotd)
    let rpt-slotd  = repeated-slot-descriptor(class);
    let size-slotd = size-slot-descriptor(rpt-slotd);
    slot-value(object, size-slotd) := size;
    for (i :: <integer> from 0 below &size)
      repeated-slot-value(object, rpt-slotd, i)
        := runstage(state, ^repeated-slot-value(&object, &rpt-slotd, i));
    end for;
  end when;
  object
end method;

define method make-runstage (state :: <machine-state>, x :: <&namespace>) => (value)
  default-make-runstage(state, x);
end method;

define method make-runstage (state :: <machine-state>, x :: <&used-library>) => (value)
  default-make-runstage(state, x);
end method;

define inline method install-unwrapped-value (x, offset :: <integer>)
  let wrapped = initialized-slot-element(x, offset);
  primitive-slot-value(x, integer-as-raw(offset))
    := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(wrapped));
end method;

define method make-runstage (state :: <machine-state>, x :: <&mm-wrapper>) => (value)
  let mmw = default-make-runstage(state, x);
  install-unwrapped-value(mmw, 2);
  install-unwrapped-value(mmw, 3);
  // TODO: patterns
  mmw
end method;

define method make-runstage (state :: <machine-state>, x :: <&implementation-class>) => (value)
  default-make-runstage(state, x);
end method;

define method make-runstage (state :: <machine-state>, x :: <&slot-descriptor>) => (value)
  default-make-runstage(state, x);
end method;

define method make-runstage (state :: <machine-state>, x :: <&type>) => (value)
  let type = default-make-runstage(state, x);
  install-unwrapped-value(type, 0);
  type
end method;

define method make-runstage (state :: <machine-state>, x :: <&runtime-object>) => (value)
  #f
end method;

define method make-runstage (state :: <machine-state>, x :: <&code>) => (value)
  #f
end method;

define method make-runstage (state :: <machine-state>, x :: <&signature>) => (value)
  default-make-runstage(state, x);
end method;

define method make-runstage (state :: <machine-state>, x :: <&generic-function>) => (value)
  let generic = default-make-runstage(state, x);
  primitive-set-generic-function-entrypoints(generic);
  generic
end method;

define method install-dfm-execution-entry-points (lambda :: <lambda>)
  install-dfm-execution-xep(lambda);
  install-dfm-execution-mep(lambda);
  install-dfm-execution-iep(lambda);
end method;

define method make-runstage-method
    (state :: <machine-state>, x :: <&lambda>, class :: <class>) => (value)
  let lambda
    = default-make-runstage(state, x, class: class, extra-size: $number-reserved-closure-slots);
  block ()
    if (instance?(function-signature(lambda), <signature>))
      install-dfm-execution-entry-points(lambda);
    end if;
  exception (<error>) // unbound
  end block;
  function-model(lambda) := x;
  lambda
end method;

define method make-runstage (state :: <machine-state>, x :: <&lambda>) => (value)
  let class
    = if (best-function-key?(x))
        <keyword-closure-method>
      else
        <simple-closure-method>
      end if;
  make-runstage-method(state, x, class);
end method;


define constant $uninitialized-temporary
  = make(<unknown>, name: #"uninitialized-temporary");

define method unchecked-frame-fetch
    (state :: <machine-state>, offset :: <integer>)
  state.state-temporaries[offset]
end method;

/* TODO: OBSOLETE?
define method frame-fetch (state :: <machine-state>, offset :: <integer>)
  let value = unchecked-frame-fetch(state, offset);
  if (value == $uninitialized-temporary)
    error("fetched uninitialized temporary")
  end if;
  value
end method;
*/

define method frame-fetch-setter
    (new-value, state :: <machine-state>, offset :: <integer>)
  state.state-temporaries[offset] := new-value;
end method;

define method make
    (class :: subclass(<machine-state>), #rest initargs, #key frame-size = 0)
 => (res :: <machine-state>)
  apply(next-method, class,
        temporaries: make(<simple-object-vector>,
                          size: frame-size, fill: $uninitialized-temporary),
        initargs)
end method make;

define method closure-offset
    (environment :: <lambda-lexical-environment>, tmp :: <temporary>)
  let closure = environment.closure;
  let closure-size = closure.size;
  iterate check (offset = 0, index = 0)
    if (index >= closure-size)
      #f
    // elseif (closure-self-reference?(tmp, environment))
    //   check(offset, index + 1)
    elseif (closure[index] == tmp)
      offset + $number-reserved-closure-slots
    else
      check(offset + 1, index + 1)
    end if
  end iterate;
end method;

define method closure-offset (lambda :: <&lambda>, tmp :: <temporary>)
  if (tmp.closed-over?)
    closure-offset(lambda.environment, tmp)
  end if
end method;

define function top-level-closure-reference?
    (o, lambda :: <&lambda>) => (well? :: <boolean>)
  method-top-level?(lambda)
end function;

define method unchecked-fetch
    (the-state :: <machine-state>, object :: <temporary>)
  let offset = closure-offset(the-state.state-&closure, object);
  if (offset)
    if (top-level-closure-reference?(object, state-&closure(the-state)))
      state-closure(the-state)
    else
      environment-element(the-state.state-closure, offset);
    end if
  else
    unchecked-frame-fetch(the-state, object.frame-offset)
  end if;
end method;

define method fetch
    (state :: <machine-state>, object :: <temporary>)
  let value = unchecked-fetch(state, object);
  if (value == $uninitialized-temporary)
    error("fetched uninitialized temporary")
  end if;
  value
end method;

define method fetch-setter
    (new-value, the-state :: <machine-state>, object :: <temporary>)
  if (object.used?)
    let offset = closure-offset(the-state.state-&closure, object);
    if (offset)
      let closure = the-state.state-closure;
      environment-element(closure, offset) := new-value
    else
      frame-fetch(the-state, object.frame-offset) := new-value
    end if
  end if
end method;

define method shadow-fetch
    (state :: <machine-state>, object :: <temporary>)
  unchecked-frame-fetch(state, frame-offset(object) + 1)
end method;

define method shadow-fetch-setter
    (new-value, state :: <machine-state>, object :: <temporary>)
  if (object.used?)
    frame-fetch(state, frame-offset(object) + 1) := new-value
  end if
end method;

define method unchecked-fetch
    (state :: <machine-state>, binding :: <module-binding>)
  ^binding-value(state, binding)
end method;

define method fetch
    (state :: <machine-state>, binding :: <module-binding>)
  unchecked-fetch(state, binding)
end method;

define method fetch-setter
    (value, state :: <machine-state>, binding :: <module-binding>)
  ^binding-value(state, binding) := value;
end method;

define method unchecked-fetch
    (state :: <machine-state>, object :: <object-reference>)
  runstage(state, reference-value(object))
end method;

define method fetch
    (state :: <machine-state>, object :: <object-reference>)
  unchecked-fetch(state, object)
end method;

define method unchecked-fetch
    (state :: <machine-state>, object :: <defined-constant-reference>)
  unchecked-fetch(state, referenced-binding(object))
end method;

define method fetch
    (state :: <machine-state>, object :: <defined-constant-reference>)
  unchecked-fetch(state, object)
end method;

define method fetch
    (state :: <machine-state>, object == #f)
  #f
end method;

define method fetch-setter
    (new-value, the-state :: <machine-state>, object == #f)
end method;

//// EXECUTION

define method execute-computations
    (state :: <machine-state>, start :: <computation>, stop :: false-or(<computation>))
  iterate loop (c = start)
    if (c & c ~== stop)
      execute(state, c);
      loop(next-computation(c))
    end if;
  end iterate;
end method;

define method execute (state :: <machine-state>, c :: <bind>)
end method;

define function shallow-copy-instance-with-size (object, size) => (copy);
  let class     = object-class(object);
  let rpt-slotd = repeated-slot-descriptor(class);
  let sz        = if (rpt-slotd)
                    if (size)
                      size
                    else
                      let size-slotd = size-slot-descriptor(rpt-slotd);
                      slot-value(object, size-slotd)
                    end if
                  else
                    0
                  end if;
  let copy   = allocate-object(class, sz);
  let slotds = slot-descriptors(class);
  for (slotd in slotds)
    let sd = as-walker-slot-descriptor(class, slotd);
    walker-slot-value(copy, sd) := walker-slot-value(object, sd);
  end;
  if (sz)
    let size-slotd = size-slot-descriptor(rpt-slotd);
    slot-value(copy, size-slotd) := sz;
    for (i :: <integer> from 0 below sz)
      repeated-slot-value(copy, rpt-slotd, i)
        := repeated-slot-value(object, rpt-slotd, i);
    end for;
  end if;
  copy
end function;

define method create-closure
    (lambda :: <lambda>, data :: <simple-object-vector>, sig :: <signature>)
  let closure
    = shallow-copy-instance-with-size
        (lambda, size(data) + $number-reserved-closure-slots);
  function-signature(closure) := sig;
  install-dfm-execution-entry-points(closure);
  for (i :: <integer> from $number-reserved-closure-slots, e in data)
    environment-element(closure, i) := e;
  end for;
  closure
end method;

define method create-closure-value (state :: <machine-state>, object)
  unchecked-fetch(state, object)
end method;

define method create-closure-data
    (state :: <machine-state>, lambda :: <&lambda>, initialized? :: <boolean>)
  if (initialized?)
    map-as(<vector>, curry(create-closure-value, state),
           lambda.environment.closure)
  else
    make(<vector>, size: size(lambda.environment.closure))
  end if
end method;

define method initialize-closure-data
    (state :: <machine-state>, &lambda :: <&lambda>, lambda :: <closure>)
  for (i :: <integer> from $number-reserved-closure-slots,
       e in &lambda.environment.closure)
    environment-element(lambda, i) := create-closure-value(state, e);
  end for;
end method;

define method execute (state :: <machine-state>, c :: <make-closure>)
  let model :: <&lambda> = computation-closure-method(c);
  let lambda :: <lambda> = runstage(state, model);
  let sigtmp = computation-signature-value(c);
  let sigval :: <signature>
    = if (sigtmp) fetch(state, sigtmp) else function-signature(lambda) end;
  let closure-data
    = create-closure-data(state, model, computation-init-closure?(c));
  let closure
    = create-closure(lambda, closure-data, sigval);
  fetch(state, c.temporary) := closure;
  // ** execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <initialize-closure>)
  let lambda :: <lambda>   = fetch(state, computation-closure(c));
  let &lambda :: <&lambda> = function-model(lambda);
  initialize-closure-data(state, &lambda, lambda);
end method;

define method execute (state :: <machine-state>, c :: <variable-reference>)
  fetch(state, c.temporary) := fetch(state, c.referenced-binding);
end method;

define method execute (state :: <machine-state>, c :: <assignment>)
  let new-value = fetch(state, c.computation-value);
  fetch(state, c.assigned-binding) := new-value;
  fetch(state, c.temporary) := new-value;
end method;

define method execute (state :: <machine-state>, c :: <definition>)
  // this only applies to module-scoped variables
  let new-value = fetch(state, c.computation-value);
  ^binding-value(state, c.assigned-binding) := new-value;
  fetch(state, c.temporary) := new-value;
end method;

define method execute (state :: <machine-state>, c :: <temporary-transfer-computation>)
  fetch(state, c.temporary) := fetch(state, c.computation-value);
end method;

// define method break-it (function)
//   break("Function %=", function);
// end method;

define thread variable *call-depth* = 0;
define thread variable *trace-interpreter?* = #f;

define method trace-format (string :: <string>, #rest args)
  if (*trace-interpreter?*)
    for (i from 0 below *call-depth*) format-out("  "); end;
    block ()
      apply(format-out, string, args);
    exception (<error>)
      format-out("\n");
    end block;
  end if;
end method;

define macro maybe-multiple-value-bind
  { maybe-multiple-value-bind (?tmp:expression)
     ?:body
    end }
    => { if (instance?(?tmp, <multiple-value-temporary>))
           let (#rest results) = ?body;
           results
         else
           ?body
         end if }
end macro;

define method bind-call-arguments
    (state :: <machine-state>, c :: <call>) => (args :: <simple-object-vector>)
  let args     = make(<vector>, size: size(c.arguments) + 1);
  args[size(args) - 1] := #[];
  map-into(args, curry(fetch, state), c.arguments);
  args
end method;

define method bind-apply-arguments
    (state :: <machine-state>, c :: <call>) => (args :: <simple-object-vector>)
  map-as(<simple-object-vector>, curry(fetch, state), c.arguments);
end method;

define method real-call-arguments
    (args :: <simple-object-vector>) => (args :: <simple-object-vector>)
  concatenate(copy-sequence(args, end: size(args) - 1), args[size(args) - 1])
end method;

define macro trace-call
  { trace-call (?kind:expression, ?function:expression, ?args:expression)
     ?:body
    end }
    => { begin
           trace-format("%s %= TO %=\n", ?kind, ?function, real-call-arguments(?args));
           let result =
             dynamic-bind (*call-depth* = *call-depth* + 1)
               ?body
             end dynamic-bind;
           trace-format("=> %=\n", result);
           result
         end }
end macro;

define method execute (state :: <machine-state>, c :: <function-call>)
  let function = fetch(state, c.function);
  let args     = bind-call-arguments(state, c);
  let result =
    trace-call ("CALLING", function, args)
      maybe-multiple-value-bind (c.temporary)
        primitive-apply(function, args);
      end maybe-multiple-value-bind;
    end trace-call;
  fetch(state, c.temporary) := result;
end method;

define method execute (state :: <machine-state>, c :: <apply>)
  let function = fetch(state, c.function);
  let args     = bind-apply-arguments(state, c);
  let result =
    trace-call ("APPLYING", function, args)
      maybe-multiple-value-bind (c.temporary)
        primitive-apply(function, args);
      end maybe-multiple-value-bind;
    end trace-call;
  fetch(state, c.temporary) := result;
end method;

define method execute (state :: <machine-state>, c :: <method-call>)
  let function     = fetch(state, c.function);
  let next-methods = fetch(state, c.next-methods);
  let args         = bind-call-arguments(state, c);
  let result =
    trace-call ("METHOD-CALLING", function, args)
      maybe-multiple-value-bind (c.temporary)
        primitive-mep-apply(function, next-methods, args);
      end maybe-multiple-value-bind;
    end trace-call;
  fetch(state, c.temporary) := result;
end method;

define method execute (state :: <machine-state>, c :: <method-apply>)
  let function     = fetch(state, c.function);
  let next-methods = fetch(state, c.next-methods);
  let args         = bind-apply-arguments(state, c);
  let result =
    trace-call ("METHOD-APPLYING", function, args)
      maybe-multiple-value-bind (c.temporary)
        primitive-mep-apply(function, next-methods, real-call-arguments(args));
      end maybe-multiple-value-bind;
    end trace-call;
  fetch(state, c.temporary) := result;
end method;

define method execute (state :: <machine-state>, c :: <primitive-call>)
  fetch(state, c.temporary)
    := select (primitive(c))
         dylan-value(#"primitive-copy-vector")
           => copy-sequence(fetch(state, arguments(c)[0]));
         dylan-value(#"primitive-next-methods-parameter")
           => state-next-methods(state);
         otherwise // noop
           => runstage(state, #f);
       end select;
end method;

define method runtime-allocate-registers (f :: <&lambda>) => ();
  let e :: <lambda-lexical-environment> = f.environment;
  let offset = 0;
  let number-parameters = size(parameters(f));
  for (tmp in e.temporaries, i :: <integer> from 0)
    if (i <= number-parameters | used?(tmp))
      let increment
        = if (instance?(generator(tmp), <loop-merge>)) 2 else 1 end;
      tmp.frame-offset := offset;
      offset           := offset + increment;
    else
      tmp.frame-offset := 0;
    end if;
  end for;
  e.frame-size := offset + 1;
end method;

define function execute-lambda-dfm
    (&lambda :: <&lambda>, lambda :: <lambda>, next-methods, #rest arguments)
  runtime-allocate-registers(&lambda);
  let new-state
    = make(<machine-state>,
           frame-size:   &lambda.frame-size,
           closure:      lambda,
           &closure:     &lambda,
           runstage:     *runstage*,
           next-methods: next-methods);
  for (parameter in parameters(&lambda),
       argument  in arguments)
    // format-out("BINDING %= TO %=\n", parameter, argument);
    fetch(new-state, parameter) := argument;
  end for;
  let bind   = &lambda.body;
  let return = bind-return(bind);
  execute-computations(new-state, bind, return);
  apply(values, fetch(new-state, return.computation-value))
end function;

define inline function execute-dfm (next, #rest arguments)
  let next-methods = primitive-next-methods-parameter();
  let  lambda      = primitive-function-parameter(); // TODO: MAKE THIS CORRECT
  let &lambda      = function-model(lambda);
  ignore(next);
  apply(execute-lambda-dfm, &lambda, lambda, next-methods, arguments)
end function;

define macro req-dfm-execution-definer
  { define req-dfm-execution ?:name () }
    => { define function ?name ## "-req-dfm-execution" (#next next)
           execute-dfm(next)
         end function }
  { define req-dfm-execution ?:name (?arguments:*) }
    => { define function ?name ## "-req-dfm-execution" (?arguments, #next next)
           execute-dfm(next, ?arguments)
         end function }
end macro;

define macro rst-dfm-execution-definer
  { define rst-dfm-execution ?:name () }
    => { define function ?name ## "-rst-dfm-execution" (#next next, #rest rest)
           %dynamic-extent(rest);
           execute-dfm(next, rest)
         end function }
  { define rst-dfm-execution ?:name (?arguments:*) }
    => { define function ?name ## "-rst-dfm-execution" (?arguments, #next next, #rest rest)
           %dynamic-extent(rest);
           execute-dfm(next, ?arguments, rest)
         end function }
end macro;

define macro key-dfm-execution-definer
  { define key-dfm-execution ?:name () }
    => { define function ?name ## "-key-dfm-execution" (#next next, #rest rest, #key, #all-keys)
           %dynamic-extent(rest);
           execute-dfm(next, rest)
         end function }
  { define key-dfm-execution ?:name (?arguments:*) }
    => { define function ?name ## "-key-dfm-execution" (?arguments, #next next, #rest rest, #key, #all-keys)
           %dynamic-extent(rest);
           execute-dfm(next, ?arguments, rest)
         end function }
end macro;

define constant $max-number-required = 9;

define req-dfm-execution the-0 ();
define req-dfm-execution the-1 (a1);
define req-dfm-execution the-2 (a1, a2);
define req-dfm-execution the-3 (a1, a2, a3);
define req-dfm-execution the-4 (a1, a2, a3, a4);
define req-dfm-execution the-5 (a1, a2, a3, a4, a5);
define req-dfm-execution the-6 (a1, a2, a3, a4, a5, a6);
define req-dfm-execution the-7 (a1, a2, a3, a4, a5, a6, a7);
define req-dfm-execution the-8 (a1, a2, a3, a4, a5, a6, a7, a8);
define req-dfm-execution the-9 (a1, a2, a3, a4, a5, a6, a7, a8, a9);

define rst-dfm-execution the-0 ();
define rst-dfm-execution the-1 (a1);
define rst-dfm-execution the-2 (a1, a2);
define rst-dfm-execution the-3 (a1, a2, a3);
define rst-dfm-execution the-4 (a1, a2, a3, a4);
define rst-dfm-execution the-5 (a1, a2, a3, a4, a5);
define rst-dfm-execution the-6 (a1, a2, a3, a4, a5, a6);
define rst-dfm-execution the-7 (a1, a2, a3, a4, a5, a6, a7);
define rst-dfm-execution the-8 (a1, a2, a3, a4, a5, a6, a7, a8);
define rst-dfm-execution the-9 (a1, a2, a3, a4, a5, a6, a7, a8, a9);

define key-dfm-execution the-0 ();
define key-dfm-execution the-1 (a1);
define key-dfm-execution the-2 (a1, a2);
define key-dfm-execution the-3 (a1, a2, a3);
define key-dfm-execution the-4 (a1, a2, a3, a4);
define key-dfm-execution the-5 (a1, a2, a3, a4, a5);
define key-dfm-execution the-6 (a1, a2, a3, a4, a5, a6);
define key-dfm-execution the-7 (a1, a2, a3, a4, a5, a6, a7);
define key-dfm-execution the-8 (a1, a2, a3, a4, a5, a6, a7, a8);
define key-dfm-execution the-9 (a1, a2, a3, a4, a5, a6, a7, a8, a9);

define function number-mep-parameters (lambda :: <lambda>) => (res :: <integer>)
  let signature = function-signature(lambda);
  signature-number-required(signature)
    // + if (signature-rest?(signature)) 1 else 0 end
    + if (signature-key?(signature))
        truncate/(size(keyword-specifiers(lambda)), 2)
      else
        0
      end;
end function;

define function appropriate-dfm-execution-function
    (lambda :: <lambda>, #key number-required) => (function :: <lambda>)
  let signature = function-signature(lambda);
  let number-required = number-required | signature-number-required(signature);
  if (number-required > $max-number-required)
    error("Interpreter only supports %= parameters %= requested\n",
          $max-number-required, number-required);
  end if;
  let function
    = if (signature-key?(signature))
        select (number-required)
          0 => the-0-key-dfm-execution;
          1 => the-1-key-dfm-execution;
          2 => the-2-key-dfm-execution;
          3 => the-3-key-dfm-execution;
          4 => the-4-key-dfm-execution;
          5 => the-5-key-dfm-execution;
          6 => the-6-key-dfm-execution;
          7 => the-7-key-dfm-execution;
          8 => the-8-key-dfm-execution;
          9 => the-9-key-dfm-execution;
        end select;
      elseif (signature-rest?(signature))
        select (number-required)
          0 => the-0-rst-dfm-execution;
          1 => the-1-rst-dfm-execution;
          2 => the-2-rst-dfm-execution;
          3 => the-3-rst-dfm-execution;
          4 => the-4-rst-dfm-execution;
          5 => the-5-rst-dfm-execution;
          6 => the-6-rst-dfm-execution;
          7 => the-7-rst-dfm-execution;
          8 => the-8-rst-dfm-execution;
          9 => the-9-rst-dfm-execution;
        end select;
      else
        select (number-required)
          0 => the-0-req-dfm-execution;
          1 => the-1-req-dfm-execution;
          2 => the-2-req-dfm-execution;
          3 => the-3-req-dfm-execution;
          4 => the-4-req-dfm-execution;
          5 => the-5-req-dfm-execution;
          6 => the-6-req-dfm-execution;
          7 => the-7-req-dfm-execution;
          8 => the-8-req-dfm-execution;
          9 => the-9-req-dfm-execution;
        end select;
      end if;
  function
end function;

define function install-dfm-execution-xep (lambda :: <lambda>)
  let function = appropriate-dfm-execution-function(lambda);
  // HACK: DO THIS RIGHT
  let xep-offset = 0; // slot-offset();
  slot-element(lambda, xep-offset)
    := initialized-slot-element(function, xep-offset);
end function;

define method install-dfm-execution-mep (lambda :: <lambda>)
  let function
    = appropriate-dfm-execution-function
        (lambda, number-required: number-mep-parameters(lambda));
  // HACK: DO THIS RIGHT
  let mep-offset = 2; // slot-offset();
  slot-element(lambda, mep-offset)
    := initialized-slot-element(function, mep-offset);
end method;

define method install-dfm-execution-mep (lambda :: <keyword-method>)
  let function
    = appropriate-dfm-execution-function(lambda);
  // HACK: DO THIS RIGHT
  let mep-offset = 2; // slot-offset();
  slot-element(lambda, mep-offset)
    := initialized-slot-element(function, mep-offset);
end method;

define method install-dfm-execution-iep (lambda :: <lambda>)
end method;

define method install-dfm-execution-iep (lambda :: <keyword-method>)
  let function
    = appropriate-dfm-execution-function
        (lambda, number-required: number-mep-parameters(lambda));
  // HACK: DO THIS RIGHT
  let iep-offset = 3; // slot-offset();
  slot-element(lambda, iep-offset)
    := initialized-slot-element(function, iep-offset);
end method;

define method execute (state :: <machine-state>, c :: <stack-vector>)
  fetch(state, c.temporary)
    := map-as(<vector>, curry(fetch, state), c.arguments);
end method;

define method register-loop-continue
    (state :: <machine-state>, c :: <loop>, continue :: <function>)
  state-loop-continues(state)[c] := continue;
end method;

define method loop-continue
    (state :: <machine-state>, c :: <loop>)
  state-loop-continues(state)[c]()
end method;

define method execute (state :: <machine-state>, c :: <loop>)
  for (merge in loop-merges(c))
    let tmp = temporary(merge);
    if (tmp & used?(tmp))
      shadow-fetch(state, tmp)
        := fetch(state, loop-merge-parameter(merge));
    end if;
  end for;
  block (return)
    while (#t)
      block (continue)
        register-loop-continue(state, c, continue);
        execute-computations(state, c.loop-body, c.next-computation);
        return();
      end block;
    end while;
  end block;
end method;

define method execute (state :: <machine-state>, c :: <loop-call>)
  let loop = loop-call-loop(c);
  for (initial-merge in loop-merges(loop),
       call-merge    in loop-call-merges(c))
    let tmp = temporary(initial-merge);
    if (tmp & used?(tmp))
      shadow-fetch(state, tmp)
        := fetch(state, loop-merge-argument(call-merge));
    end if;
  end for;
  loop-continue(state, loop);
end method;

define method execute (state :: <machine-state>, c :: <end-loop>)
end method;

define method execute (state :: <machine-state>, c :: <loop-merge>)
  if (loop-merge-initial?(c))
    fetch(state, temporary(c)) := shadow-fetch(state, temporary(c));
  else
    fetch(state, temporary(c)) := fetch(state, loop-merge-parameter(c));
  end if;
end method;

define method maybe-transfer-merge
    (state :: <machine-state>, merge :: <computation>, refn :: <function>)
  if (instance?(merge, <merge>))
    let ref         = refn(merge);
    let merge-tmp   = temporary(merge);
    let merge-used? = merge-tmp & used?(merge-tmp);
    if (merge-used? & ref)
      fetch(state, merge-tmp) := fetch(state, ref);
    end if;
  end if;
end method;

define method execute (state :: <machine-state>, c :: <if>)
  let merge = next-computation(c);
  if (fetch(state, c.test) == #f)
    execute-computations(state, c.alternative, merge);
    maybe-transfer-merge(state, merge, merge-right-value);
  else
    execute-computations(state, c.consequent, merge);
    maybe-transfer-merge(state, merge, merge-left-value);
  end if;
end method;

define method execute (state :: <machine-state>, c :: <nop-computation>)
end method;

define method execute (state :: <machine-state>, c :: <return>)
end method;

define method execute (state :: <machine-state>, c :: <bind-exit>)
  block (return)
    let merge = next-computation(c);
    fetch(state, c.entry-state)
      := method (#rest exit-values)
           fetch(state, temporary(merge)) := exit-values;
           return()
         end method;
    execute-computations(state, c.body, c.next-computation);
    maybe-transfer-merge(state, merge, merge-right-value);
  end block;
end method;

define method execute (state :: <machine-state>, c :: <unwind-protect>)
  let (#rest body-values)
    = block ()
        execute-computations(state, c.body, c.next-computation);
      cleanup
        execute-computations(state, c.cleanups, c.next-computation);
      end block;
  fetch(state, c.temporary) := body-values;
end method;

define method execute (state :: <machine-state>, c :: <exit>)
  let return = fetch(state, c.entry-state);
  apply(return, fetch(state, c.computation-value))
end method;

define method execute (state :: <machine-state>, c :: <end-exit-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-exit-block>"
end method;

define method execute (state :: <machine-state>, c :: <end-protected-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-protected-block>"
end method;

define method execute (state :: <machine-state>, c :: <end-cleanup-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-cleanup-block>"
end method;

/// multiple values

define method execute (state :: <machine-state>, c :: <values>)
  fetch(state, c.temporary)
    := begin
         let fixed = map-as(<simple-object-vector>,
                            curry(fetch, state), c.fixed-values);
         if (c.rest-value)
           concatenate(fixed, fetch(state, c.rest-value))
         else
           fixed
         end if
       end;
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-spill>)
  fetch(state, c.temporary) := fetch(state, c.computation-value);
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-unspill>)
  fetch(state, c.temporary) := fetch(state, c.computation-value);
end method;

define method execute (state :: <machine-state>, c :: <extract-single-value>)
  let mv = fetch(state, c.computation-value);
  fetch(state, c.temporary) := element(mv, c.index, default: #f);
end method;

define method execute (state :: <machine-state>, c :: <extract-rest-value>)
  let mv = fetch(state, c.computation-value);
  fetch(state, c.temporary)
    := run-stage(if (c.index > mv.size)
                   #[]
                 else
                   copy-sequence(mv, start: c.index)
                 end if);
end method;

define method execute
    (state :: <machine-state>, c :: <adjust-multiple-values>)
  let mv = fetch(state, c.computation-value);
  let count = size(mv);
  let n = number-of-required-values(c);
  fetch(state, c.temporary) :=
    if (count = n)
      mv
    elseif (count > n)
      copy-sequence(mv, end: n)
    else
      replace-subsequence!
        (make(<simple-object-vector>, size: n, fill: #f),
         mv, end: count)
    end;
end method;

define method execute
    (state :: <machine-state>, c :: <adjust-multiple-values-rest>)
  let mv = fetch(state, c.computation-value);
  let count = size(mv);
  let n = number-of-required-values(c);
  fetch(state, c.temporary) :=
    if (count >= n)
      mv
    else
      replace-subsequence!
        (make(<simple-object-vector>, size: n, fill: #f),
         mv, end: count)
    end;
end method;

/// types

define method execute (state :: <machine-state>, c :: <check-type>)
  let value = fetch(state, computation-value(c));
  check-type(value, fetch(state, type(c)));
  fetch(state, temporary(c)) := value;
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-check-type>)
  let vals = fetch(state, computation-value(c));
  let typs = types(c);
  if (size(vals) ~== size(typs))
    error("TYPE CHECK SIZE FAILURE\n");
  end if;
  for (val in vals, typ in typs)
    when (typ)
      check-type(val, fetch(state, typ));
    end when;
  end for;
  fetch(state, temporary(c)) := vals;
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-check-type-rest>)
  let vals = fetch(state, computation-value(c));
  let typs = types(c);
  if (size(vals) >= size(typs))
    error("TYPE CHECK SIZE FAILURE\n");
  end if;
  for (val in vals, typ in typs, i :: <integer> from 0)
    when (typ)
      check-type(val, fetch(state, typ));
    end when;
  finally
    let rest-typ = fetch(state, rest-type(c));
    for (j :: <integer> from i below size(vals))
      check-type(vals[j], rest-typ)
    end for;
  end for;
  fetch(state, temporary(c)) := vals;
end method;

/// cell for assignment

define class <xcell> (<object>)
  slot cell-value, init-keyword: value:;
end class <xcell>;

define method execute (state :: <machine-state>, c :: <make-cell>)
  fetch(state, c.temporary)
    := make(<xcell>, value: fetch(state, c.computation-value));
end method execute;

define method execute (state :: <machine-state>, c :: <get-cell-value>)
  fetch(state, c.temporary) := fetch(state, c.computation-cell).cell-value;
end method execute;

define method execute (state :: <machine-state>, c :: <set-cell-value!>)
  fetch(state, c.temporary)
    := (fetch(state, c.computation-cell).cell-value
          := fetch(state, c.computation-value));
end method execute;

/// TOP LEVEL FORMS

define constant $dummy-lambda = method () end;

define method interpret-lambda (l :: <&lambda>)
  without-dependency-tracking
    execute-lambda-dfm(l, $dummy-lambda, #f)
  end without-dependency-tracking;
end method;

define method interpret-initializer-method (l :: <&lambda>)
  when (l.body)
    with-library-context (model-library(l))
      interpret-lambda(l);
    end with-library-context;
  end when;
end method;

define variable *transaction-id* = -1;
define constant $transactions :: <table> = make(<table>);

define method register-transaction (value) => (transaction-id)
  *transaction-id* := *transaction-id* + 1;
  $transactions[*transaction-id*] := value;
  *transaction-id*
end method;

define sideways method unregister-interpreter-transaction (transaction-id) => ()
  remove-key!($transactions, *transaction-id*)
end method;

define method interpreter-transaction-value (transaction-id) => (val)
  element($transactions, transaction-id, default: #[])
end method;

define sideways method interpret-top-level-form
    (form :: <top-level-form>, #key trace? = #f) => (transaction-id)
  let init-method = form-init-method(form);
  let vals =
    if (init-method)
      dynamic-bind (*trace-interpreter?* = trace?)
        interpret-initializer-method(init-method);
      end dynamic-bind;
    end if;
  register-transaction(vals);
end method;

