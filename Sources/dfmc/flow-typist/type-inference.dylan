Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/////////////////////////////////////////////////////////////////////////
///                      
///                      The driver for initial typing of a lambda
///
/////////////////////////////////////////////////////////////////////////

define variable *trace-typist?* = #f;

define function type-infer-lambda 
  (l :: <&lambda>, css :: <call-site-summary>)
  => (result :: <&type>, summaries :: false-or(<table>))
  let result-type = css.result-type;
  if (~instance?(result-type, <unknown-type>))
    values(result-type, #f);
  else
    if (css.compressed?)
      css.compressed? := #f;
    end;
    unless (l.body) 
      regenerate-dfm-for(l);
    end;
    do-type-infer-lambda(l, css);
  end;
end;

define function regenerate-dfm-for(l :: <&lambda>) => ()
  let old-opt = l.lambda-optimized?;
  l.lambda-optimized? := #f;
  maybe-compute-and-install-method-dfm(l); 
  l.lambda-optimized? := old-opt;
end;

define sideways sealed method force-type-infer-lambda
    (l :: <&lambda>, css :: <call-site-summary>) => ()
  do-type-infer-lambda(l, css);
  process-delayed-typist-notes(css);
end;

define variable *KM-FLAG* = #f;

// the main loop for the initial typing of a function
define function do-type-infer-lambda 
  (l :: <&lambda>, s :: <call-site-summary>) 
  => (type :: <&type>, summaries :: <table>)
  if (instance?(s.result-type, <unknown-type>))
    s.result-type := typist-<bottom-type>()
  end;
  //without-dependency-tracking
    let work-agenda = make(<work-agenda>);
    schedule-for-initial-typing(work-agenda, l.body, s);
    iterate loop ()
      if (~queue-empty?(work-agenda.initial-work))
        let item = work-agenda.initial-work.queue-front;
        let c = item.node;
        let s = item.call-site-summary;
        type-infer-lambda-phase-1(c, s, work-agenda);
        pop-work-item(work-agenda, work-agenda.initial-work);
        push-current-work(work-agenda);
        loop();
      elseif (~queue-empty?(work-agenda.re-type-work))
        let item = work-agenda.re-type-work.queue-front;
        let s = item.call-site-summary;
        work-agenda.used-call-site-summaries[s] := s;
        refine-initial-node-type(item.node, s, work-agenda);
        pop-work-item(work-agenda, work-agenda.re-type-work);
        push-current-work(work-agenda);
        loop()
      end if;
    end iterate;

    format-out("\n--------------------\n");
    // access(dfmc-back-end, print-method-out)(l);
    format-out("For css %=,\n", s);
    format-out("\t lambda %= has result type\n\t  %=.\n", l, s.result-type);
    check-inferred-type(l, s.result-type);
    format-out("--------------------\n");

    values(s.result-type, work-agenda.used-call-site-summaries);
  //end;
end;

define inline function type-infer-lambda-phase-1 (comp, css, work-agenda)
  if (instance?(css.result-type, <unknown-type>))
    css.result-type := typist-<bottom-type>()
  end;
  work-agenda.used-call-site-summaries[css] := css;
  local method type-node (c) 
//          format-out("Initializing %= [%=]\n", c, css);
          initialize-node-type(c, css, work-agenda);
        end;
  typist-walk-computations(type-node, comp, css, #f);
  push-current-work(work-agenda);
end;

define function type-computations (from, to, css)  => ()
  when (from & to)
    let work-agenda = make(<work-agenda>);
    local method type-node (c) 
            initialize-node-type(c, css, work-agenda);
          end;
    schedule-for-initial-typing(work-agenda, from, css);
    iterate loop ()
      if (~queue-empty?(work-agenda.initial-work))
        let item = work-agenda.initial-work.queue-front;
        let c = item.node;
        let s = item.call-site-summary;
        typist-walk-computations(type-node, c, s, to.next-computation);
        pop-work-item(work-agenda, work-agenda.initial-work);
        push-current-work(work-agenda);
        loop();
      elseif (~queue-empty?(work-agenda.re-type-work))
        let item = work-agenda.re-type-work.queue-front;
        let s = item.call-site-summary;
        work-agenda.used-call-site-summaries[s] := s;
        refine-initial-node-type(item.node, s, work-agenda);
        pop-work-item(work-agenda, work-agenda.re-type-work);
        push-current-work(work-agenda);
        loop()
      end if;
    end iterate;
  end;
end;


define function update-type-computations (l :: <&lambda>, css)  => ()
  let default = instance?(css, <default-call-site-summary>);
  let work-agenda = make(<work-agenda>);
  let pool :: <type-pool> = css.pool;
  local method type-node (c) 
          unless (~c.temporary | element(pool, c.temporary, default: #f))
            if (default) 
//              unless (instance?(c, <unwind-protect>))
//                format-out("Woops - %= in %= is untyped.\n", c, css); 
//              end;
              initialize-node-type(c, css, work-agenda);
            else
              initialize-node-type(c, css, work-agenda);
            end
          end
        end;
  schedule-for-initial-typing(work-agenda, l.body, css);
  iterate loop ()
    if (~queue-empty?(work-agenda.initial-work))
      let item = work-agenda.initial-work.queue-front;
      let c = item.node;
      let s = item.call-site-summary;
      typist-walk-computations(type-node, c, s, #f);
      pop-work-item(work-agenda, work-agenda.initial-work);
      push-current-work(work-agenda);
      loop();
    elseif (~queue-empty?(work-agenda.re-type-work))
      let item = work-agenda.re-type-work.queue-front;
      let s = item.call-site-summary;
      work-agenda.used-call-site-summaries[s] := s;
      refine-initial-node-type(item.node, s, work-agenda);
      pop-work-item(work-agenda, work-agenda.re-type-work);
      push-current-work(work-agenda);
      loop()
    end if;
  end iterate;
end;

define method type-call-in-all-summaries (call :: <apply>, example-css)  => ()
  for-all-summaries (css in example-css.css-lambda.call-site-summaries)
    if (instance?(css, <call-site-summary>))
      if (css.compressed?)
        css.compressed? := #f;
//        format-out("Retyping all of %=\n", css);
        force-type-infer-lambda(css.css-lambda, css);
      end;
      type-computations(call, call, css)  
    end
  end
end;


define method type-call-in-all-summaries (call :: <call>, example-css)  => ()
  let default-css = get-default-call-site-summary(example-css.css-lambda);
  let arg-types = 
    map(method (arg) lookup-type(arg, default-css, call) end, call.arguments);
  type-computations(call, call, default-css); 
  let comp-rec = element(default-css.computation-records, call, default: #f);
  let call-result = lookup-type(call.temporary, default-css, call);
  let lambda = example-css.css-lambda;

  let summary-worth-updating? =
    ~lambda.lambda-top-level? 
    | method-inlineable?(lambda)
    | instance?(lambda, <&copy-down-method>);
  
  for-all-summaries (css in example-css.css-lambda.call-site-summaries)
    if ( instance?(css, <call-site-summary>) 
       & css ~== default-css & summary-worth-updating? )
      if (css.compressed?)
        css.compressed? := #f;
//        format-out("Retyping all of %=\n", css);
        force-type-infer-lambda(css.css-lambda, css);
      end;
    
      let css-arg-types = 
        map(method (arg) lookup-type(arg, css, call) end, 
            call.arguments);
       
      if ( comp-rec
         & every?(method(arg1, arg2) ^subtype?(arg1, arg2) end, 
                 arg-types, css-arg-types) )
        let real-temp = get-renaming(call.temporary, css, call);
        css.pool[real-temp] := call-result;
        css.computation-records[call] := shallow-copy-instance(comp-rec);
      else
        type-computations(call, call, css)  
      end
    end
  end
end;


/// lookup-type produces a type given a variable or an object.
/// These are typically the operands of a computation.
define generic lookup-type (object, css :: <call-site-summary>, computation)
  => (result :: <&type>);

define method lookup-type 
  (temp :: <temporary>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  lookup-variable-type(temp, css, computation, method (x, y) #f end);
end;

define method lookup-type 
  (temp :: <multiple-value-temporary>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
    lookup-variable-type(temp, css, computation, method (x, y) #f end);
end;

define method lookup-type   (ref :: <object-reference>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  cached-singleton(reference-value(ref));
end;


define function lookup-variable-type 
  (lv :: <temporary>, css :: <call-site-summary>, comp, fn :: <function>) 
  => (r :: false-or(<&type>));
  let real-temp = get-renaming(lv, css, comp);  
  element(css.pool, real-temp, default: #f) |
  fn(lv, css) |
  // so we didn`t find it in our pool. Maybe its been closed over somewhere
  begin
    let current-lambda = css.css-lambda;
    let outer-lambda = lv.environment.lambda;
    if (outer-lambda & outer-lambda ~== current-lambda)
/*      There may be lots of call-site-summaries for the outer lambda, so 
        it's a bit worrying if we have to iterate over all of them.  But
        at the moment we have no guarantee that the default method has been
        typed at this point so we can't just look it up in that...  
        This approach needs to be rethought.  I'm not convinced this is either
        efficient or safe... 
*/
      let dcss = get-default-call-site-summary(outer-lambda);  
      if (instance?(dcss.result-type, <unknown-type>))
        collecting (types)
          for (c in outer-lambda.call-site-summaries)
            unless (instance?(c, <specialized-call-site-summary>) & compressed?(c))
              collect-into(types, lookup-type(lv, c, comp));
            end
          end;
          reduce1(curry(maybe-make-inferred-union, comp), collected(types));
        end
      else
        lookup-type(lv, dcss, comp)
      end
    else
      typist-<bottom-type>(); // just say bottom !!    
    end
  end;
end; 


define method lookup-type 
  (lv :: <lexical-variable>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  lookup-variable-type(lv, css, computation, method(x ,y) #f end);
end; 

define inline function get-required-arg-type
  (lv :: <lexical-required-variable> , css :: <call-site-summary>)
  => (r :: false-or(<&type>))
  
  let md = model-definition(css.css-lambda);
   
  let index =
    if (instance?(md, <function-defining-form>))
      let name = lv.name;
      let args = 
        md.form-signature.spec-arguments-spec.spec-required-variable-specs;
      find-key(args, method(arg) arg.spec-variable-name == name end)

    else // Do I still need this?
      let params = css.css-lambda.parameters;

      unless (params)
        // Damn! Presumably the DFM has been removed.
        // TODO - do I really need to get all the DFM, or can I just reestablish
        // the parameters
        regenerate-dfm-for(css.css-lambda);
        params := css.css-lambda.parameters;
      end;
  
      find-key(params, curry(\== , lv));
    end;

  index & css.arg-types[index];
end;
  
define method lookup-type 
  (lrv :: <lexical-required-variable>, css :: <call-site-summary>,computation)
  => (result :: <&type>)
  local method get-arg-type (lrv, css)
          if (lrv.environment.lambda == css.css-lambda)
            (css.pool[lrv] := 
              ( get-required-arg-type(lrv, css) |
                begin
                  let type = lrv.specializer;
                  if (instance?(type, <&type>))
                    type;
                  else
                    typist-<object-type>();
                  end;
                end))
          end;
        end;
  lookup-variable-type(lrv, css, computation, get-arg-type);
end;

define method lookup-type 
  (llv :: <lexical-local-variable>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  local method get-arg-type (llv, css)
/*
          if (llv.environment.lambda == css.css-lambda)
            (css.pool[llv] := begin
                                let type = llv.specializer;
                                if (instance?(type, <&type>))
                                  type
                                else
                                  typist-<object-type>();
                                end;
                              end)
          end;
*/
          if (llv.environment.lambda == css.css-lambda)
            css.pool[llv] := typist-<bottom-type>();
	  else
            #f
          end
        end;
  lookup-variable-type(llv, css, computation, get-arg-type);
end; 

define method lookup-type 
  (lrv :: <lexical-rest-variable>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  local method get-arg-type (lrv, c)
          if (lrv.environment.lambda == c.css-lambda)
            typist-sov-type();
          end;
        end;
  lookup-variable-type(lrv, css, computation, get-arg-type);
end;

define inline function get-keyword-arg-type
  (lkv :: <lexical-keyword-variable> , css :: <call-site-summary>)
  => (r :: false-or(<&type>))
  let name = as(<symbol>, lkv.name);
  let index = find-key(css.css-lambda.keyword-specifiers,
                       curry(\== , name));
  if (index)
    let i = css.css-lambda.^function-signature.^signature-number-required + (index / 2);
    css.arg-types[i + 1];
  end;
end;
  
define method lookup-type 
  (lkv :: <lexical-keyword-variable>, css :: <call-site-summary>,computation)
  => (result :: <&type>)
  local method get-arg-type (lkv, css)
          if (lkv.environment.lambda == css.css-lambda)
            css.pool[lkv] := 
              ( get-keyword-arg-type(lkv, css) |
                begin
                  let type = lkv.specializer;
                  if (instance?(type, <&type>))
                    type
                  else
                    typist-<object-type>();
                  end;
                end);
          end;
        end;
  lookup-variable-type(lkv, css, computation, get-arg-type);
end;

define method lookup-type 
  (dcr :: <defined-constant-reference>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  lookup-type(dcr.referenced-binding, css, computation);
end;

define function type-infer-using-declared-type
    (mb :: <module-binding>) 
  let (type-val, computed?)
    = binding-constant-type-model-object
        (mb, error-if-circular?: #f);
  if (computed?)
    type-val
  else
    typist-<object-type>()
  end;
end function;

define method lookup-type 
  (mb :: <module-binding>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  case
    constant?(mb)
      => let mb = mb.binding-canonical-binding;
         let (val, computed?) = binding-constant-model-object(mb);
         case
	   computed?
	     => cached-singleton(val);
           otherwise
             => let it = mb.inferred-type;
                if (^subtype?(it, typist-<bottom-type>()) | it == typist-<unknown-type>())
                  typist-<object-type>()
                else
                  it
                end;
         end;
    exported?(mb) // must believe its type decl
      => type-infer-using-declared-type(mb);
    otherwise
      => /* Its an unexported module variable. If we`re doing a tight
            compilation we can work out its type from all its
	    assignments. Otherwise just return its declared type or
	    <object>. */
         // DAMN - this approach relies on the optimizer not being called before the
         // whole library has been typed.  Unfortunately the current compiler calls the
         // optimizer all over the place, in particular before the library has been typed.
         case
           // TODO:
           #t => type-infer-using-declared-type(mb);
           otherwise
             => /* Record the reader's dependency on all the writers
  	           and then return an inferred union of all the
		   assignment types */
                record-dependency(mb, computation, css);
                mb.inferred-type;
         end;
  end case;
end;

define macro singleton-lookup-type-definer
  { define singleton-lookup-type end } => { } // base case.
  { define  singleton-lookup-type
      ?var:name :: ?dfm-type:name ;
      ?more:* 
    end } 
   => { define method lookup-type
	  (?var :: ?dfm-type, css :: <call-site-summary>, computation :: <computation>) 
             => (result :: <&type>)
          cached-singleton(?var);
	end;
        define singleton-lookup-type 
          ?more
        end } 
end;

define singleton-lookup-type 
  obj :: <&type>;
  obj :: <&class>;
  obj :: <&object>;
  //obj :: <unbound>;
  obj :: <number>;
  obj :: <&raw-integer>;
  obj :: <boolean>;
  obj :: <&raw-boolean>;
  obj :: <character>;
  obj :: <string>;
  obj :: <symbol>;
  obj :: <vector>;
  obj :: <list>;
  obj :: <&callable-object>;
end;


define method lookup-type 
  (obj :: <&singleton>, css :: <call-site-summary>, computation)
  => (result :: <&type>)
  cached-singleton(obj);
end;


// This is where the type is for boxes (although this is wrong for now)
define method lookup-type 
    (mbv :: <make-cell>, css :: <call-site-summary>, computation)
    => (result :: <&type>)
  error("shouldn`t");
  element(css.pool, mbv, default: typist-<unknown-type>());
end;




//////////////////////////////////////////////////////////////////////////////////
///
///                      INITIALIZE-NODE-TYPE
///
//////////////////////////////////////////////////////////////////////////////////

// A couple of helper functions.

define inline function initialize-node-type-internal
  (c :: <computation>, css :: <call-site-summary>, wa :: <work-agenda>)
  css.pool[c.temporary] := infer-node-type(c, css, wa);
end;

define inline function dependents? (temp)
  temp & ~empty?(temp.users)
end;

// creates initial mapping between temporaries and types.
define generic initialize-node-type 
  (c :: <computation>, call-site-summary, work-agenda) => ();

// Don't do anything by default
define method initialize-node-type 
  (c :: <computation>, css, work-agenda) => ()
  error("unimplemented");
end;

define macro standard-initialize-node-type-definer
  { define standard-initialize-node-type end } => { } // base case.
  { define standard-initialize-node-type
      ?var:name :: ?dfm-type:name ;
      ?more:* 
    end } 
   => { define method initialize-node-type
	  (?var :: ?dfm-type, css :: <call-site-summary> , wa :: <work-agenda>) 
          => ()
          initialize-node-type-internal(?var, css, wa);
	end;
        define standard-initialize-node-type 
          ?more
        end } 
end;

define standard-initialize-node-type
  vr :: <variable-reference>;
  mcl :: <make-closure>;
//  icl :: <initialize-closure>;
  tx :: <temporary-transfer-computation>;
  kd :: <keyword-default>;
  c :: <adjust-multiple-values>;
  c :: <adjust-multiple-values-rest>;
  ct :: <check-type>;
  mvct :: <multiple-value-check-type>;
  mvctr :: <multiple-value-check-type-rest>;
  gt :: <guarantee-type>;
  esv :: <extract-single-value>;
  erv :: <extract-rest-value>;
  im :: <if-merge>;
  v :: <values>;
  s :: <any-slot-value>;
  // kdvm :: <keyword-default-value-marker>;
end;

define macro nop-initialize-node-type-definer
  { define nop-initialize-node-type end } => { } // base case.
  { define nop-initialize-node-type
      ?var:name :: ?dfm-type:name ;
      ?more:* 
    end } 
   => { define method initialize-node-type
	  (?var :: ?dfm-type, css :: <call-site-summary> , wa :: <work-agenda>) 
          => ()
	end;
        define nop-initialize-node-type 
          ?more
        end } 
end;

define nop-initialize-node-type
  c :: <bind>;
  c :: <nop-computation>;
  c :: <loop>;
  c :: <unwind-protect>;
  c :: <end>;
  e :: <end-exit-block>;
end;

define method initialize-node-type 
  (icl :: <initialize-closure>, css :: <call-site-summary>, wa :: <work-agenda>) => ()
  initialize-node-type-internal(icl, css, wa);
  let meth = icl.computation-closure-method;
  let comp-css = get-default-call-site-summary(meth);
  if (comp-css.result-type == typist-<unknown-type>())
    type-infer-lambda-phase-1(meth.body, comp-css, wa);
  end;
end;


//TODO: this could be smarter if it can see all the calls and knows it doesn't escape
// then it doesn't need to force the default typing.
/* in fact until we are smarter during optimization we need to produce a default typing
define method ensure-typed
  (or :: <object-reference>, l :: <&lambda>, css :: <call-site-summary> , wa :: <work-agenda>) 
  unless (lambda-top-level?(l))
    let temp = or.temporary;
    local method just-called? (comp)
            instance?(comp, <call>)
            & temp == comp.function // if its not the function it must be in the args
            & ~member?(temp, comp.arguments)
          end;
    unless(every?(just-called?, temp.users))
      // can`t see a caller, so make sure it gets typed
      maybe-compute-and-install-method-dfm(l);
      let new-css = get-default-call-site-summary(l);
      when (new-css.result-type == typist-<unknown-type>())
//         add-caller(css, new-css); 
        /* add-callee(new-css, css); */
        /* schedule-for-initial-typing(wa, l.body ,new-css); */
        /* do-type-infer-lambda(l, new-css); */
        type-infer-lambda-phase-1(l.body, new-css, wa);
      end;
    end;
  end;
end;
*/

define method ensure-typed
    (or :: <object-reference>, l :: <&lambda>, 
     css :: <call-site-summary> , wa :: <work-agenda>) 
  unless (lambda-top-level?(l))
    let new-css = get-default-call-site-summary(l);
    when (new-css.result-type == typist-<unknown-type>())
      type-infer-lambda(l, new-css);
    end;
  end;

/* gts,98jun01 -- most of this is unused -- rewrite is above
  unless (lambda-top-level?(l))
    let temp = or.temporary;
    local method local-caller? (comp)
            instance?(comp, <call>) & (temp == comp.function)
          end;

    let new-css = get-default-call-site-summary(l);
    when (new-css.result-type == typist-<unknown-type>())
      when (any?(local-caller?, temp.users))
//        add-caller(css, new-css); 
//        add-callee(new-css, css);
      end;

      type-infer-lambda(l, new-css);
    end;
  end;
*/
end;

define method ensure-typed
  (comp, obj :: <&singleton>, css, wa)
  ensure-typed(comp, obj.^singleton-object, css, wa);
end;

define method ensure-typed (c, x, y, z) end;

/* If we need this, need to widen the GF
define method initialize-node-type  
  (or :: <object-reference>, css :: <call-site-summary>, 
   work-agenda :: <work-agenda>) => ()
  ensure-typed(or, or.computation-value, css, work-agenda);
  initialize-node-type-internal(or, css, work-agenda);
end;
*/

// ASSIGNMENTS OF VARIOUS KINDS

// In general assignment introduces dependencies which are represented
// as part of the FLOW in DFM so we have to resort to some nastiness.

// we can store the type of the box under the <make-cell> that created
// it. Since we can always get back there via the temporaries generator
/*
define method initialize-node-type (c :: <make-cell>, css, work-agenda) => ()
  let box-type = c.inferred-type;
  if (#f /*instance?(box-type, <unknown-type>)*/)
    let current-type = lookup-type(c.computation-value, css, c);
    css.pool[c.temporary] := (c.inferred-type := current-type);
    unless (^subtype?(current-type, box-type))
      schedule-filtered-dependents(work-agenda, c.type-dependencies, css);
    end;
  else
    let current-type = infer-node-type(c, css, work-agenda);
    let new-box-type = maybe-make-inferred-union(c, box-type, current-type);
    css.pool[c.temporary] := (c.inferred-type := new-box-type);
    unless (^subtype?(current-type, box-type))
      schedule-dependents(work-agenda, c.type-dependencies);
    end;
  end
end;
*/

define method initialize-node-type (c :: <make-cell>, css, work-agenda) => ()
  let box-type = c.inferred-type;
  let current-type = 
    if (instance?(box-type, <unknown-type>))
      lookup-type(c.computation-value, css, c);
    else
      infer-node-type(c, css, work-agenda);
    end;
  let new-box-type = maybe-make-inferred-union(c, box-type, current-type);
  css.pool[c.temporary] := (c.inferred-type := new-box-type);

/*
  // What if the cell contains another cell...
  if (instance?(c.computation-value.generator, <make-cell>))
    record-dependency(c.computation-value.generator, c, css);
  end;  
*/

  unless (^subtype?(current-type, box-type))
    schedule-filtered-dependents(work-agenda, c.type-dependencies, css);
  end;
end;

define method initialize-node-type 
  (gbv :: <get-cell-value>, css, work-agenda) => ()
  let gen-comp = gbv.computation-cell.generator;
//  if (instance?(gen-comp, <make-cell>))
    record-dependency(gen-comp, gbv, css);
//  end;
  initialize-node-type-internal(gbv, css, work-agenda);
  //because of loops we need to re-visit the comp
  schedule-filtered-dependents(work-agenda, gbv.computation-cell.generator.type-dependencies, css);
end;

define method initialize-node-type 
  (sbv :: <set-cell-value!>, css, work-agenda) => ()
  let gen-comp = sbv.computation-cell.generator;
//  unless (instance?(gen-comp, <make-cell>))
//    error("Typing <set-cell-value!> of a cell containing a cell not implemented yet!");
//  end;
  let type = infer-node-type(sbv, css, work-agenda);
  css.pool[sbv.temporary] := type;
  if (*KM-FLAG*)
    format-out("int %=: old: %=, new: %=\n",
      sbv, gen-comp.inferred-type, type);
  end;
  gen-comp.inferred-type := 
    maybe-make-inferred-union(sbv, gen-comp.inferred-type, type);
  schedule-filtered-dependents(work-agenda, gen-comp.type-dependencies, css);
end;

define method initialize-node-type 
  (a :: <assignment>, css, work-agenda) => ()
  let type = infer-node-type(a, css, work-agenda);
  css.pool[a.temporary] := type;
  a.assigned-binding.inferred-type :=
    maybe-make-inferred-union(a, a.assigned-binding.inferred-type, type);
  schedule-filtered-dependents(work-agenda, a.assigned-binding.type-dependencies, css);
end;

define method initialize-node-type 
  (a :: <type-definition>, css, work-agenda) => ()
/*
  let type = infer-node-type(a, css, work-agenda);
  css.pool[a.temporary] := type;
  a.typed-binding.inferred-type :=
    maybe-make-inferred-union(a, a.typed-binding.inferred-type, type);
  schedule-filtered-dependents(work-agenda, a.typed-binding.type-dependencies, css);
*/
end;

define method initialize-node-type 
  (a :: <conditional-update!>, css, work-agenda) => ()
  let type = infer-node-type(a, css, work-agenda);
  css.pool[a.temporary] := dylan-value(#"<boolean>");
  a.assigned-binding.inferred-type :=
    maybe-make-inferred-union(a, a.assigned-binding.inferred-type, type);
  schedule-dependents(work-agenda, a.assigned-binding.type-dependencies);
end;

//  merge
define method initialize-node-type
  (lm :: <bind-exit-merge>, css :: <call-site-summary> , wa :: <work-agenda>) 
  => ()
  let tmp = lm.merge-right-value;
  unless (element(css.pool, tmp, default: #f))
    css.pool[tmp] := typist-<bottom-type>();
  end;
  initialize-node-type-internal(lm, css, wa);
  schedule-for-retyping(wa, lm, css);
end;
        
define method initialize-node-type
  (lm :: <loop-merge>, css :: <call-site-summary> , wa :: <work-agenda>) 
  => ()
  let tmp = lm.merge-right-value;
  unless (element(css.pool, tmp, default: #f))
    css.pool[tmp] := typist-<bottom-type>();
  end;
  schedule-for-retyping(wa, lm, css);
  initialize-node-type-internal(lm, css, wa);
end;
        
// BLOCKS (<bind-exit> and <unwind-protect>)

define generic sources(c :: <computation>);

define method sources(c :: <computation>)
  vector(c.computation-value)
end;

define method sources(bm :: <binary-merge>)
  vector(bm.merge-left-value, bm.merge-right-value)
end;

// we can't figure out the type of a <bind-exit> until we've type all
// the calls to the exit function so just queue the node for re-typing
define method initialize-node-type 
  (be :: <bind-exit>, css :: <call-site-summary>, work-agenda) => ()
  record-dependency(be, be, css); 
  css.pool[be.temporary] := typist-<bottom-type>();
  css.pool[be] := typist-<bottom-type>();
  // TODO: if we don`t immediately descend through calls delete this
  for (user in be.temporary.users)
    css.pool[user.temporary] := typist-<bottom-type>();
    for (temp in user.sources)
      unless (element(css.pool, temp, default: #f))
        css.pool[temp] := typist-<bottom-type>();
      end;
    end;
  end;
end;

define method initialize-node-type 
  (be :: <bind-exit>, css :: <specialized-call-site-summary>, work-agenda) => ()
  // Yuck! The treatment of bind-exit seems to require that the default summary
  // is processed first...

  let dcss = css.default-summary;
  if (instance?(dcss.result-type, <unknown-type>))
    type-infer-lambda(dcss.css-lambda, dcss);
//    type-infer-lambda-phase-1(dcss.css-lambda.body, dcss, work-agenda);
  end;

  let temp = be.temporary;
  css.pool[temp] := lookup-type(temp, dcss, be);
end;


// RETURN
define method initialize-node-type 
  (r :: <return>, css, work-agenda) => ()
  let new-result-type = infer-node-type(r, css, work-agenda);

  unless (^subtype?(new-result-type, css.result-type))
    css.result-type := new-result-type;
    schedule-dependents(work-agenda, css.type-dependencies);
  end
end;

// EXIT
define method initialize-node-type 
  (exit :: <exit>, css, work-agenda) => ()
  let dependents = exit.entry-state.generator.type-dependencies;
  let type = infer-node-type(exit, css, work-agenda);
  css.pool[exit.temporary] := typist-<bottom-type>();
  iterate loop (item :: false-or(<dependency-record>) = dependents.queue-front)
    when (item)
      item.call-site-summary.pool[item.node] := 
        maybe-make-inferred-union(item.node, type, item.call-site-summary.pool[item.node]);
        schedule-for-retyping(work-agenda, item.node, item.call-site-summary);
      loop(item.next-item);
    end;
  end;

end;



// FIGURING OUT THE RESULT TYPE OF A COMPUTATION.

// infer the result type(s) of a node
define generic infer-node-type (c :: <computation>, css, w-l)
  => (result :: <&type>);

// Don't provide a default so if this gets called on something unexpected we blow up.

define macro lookup-type-infer-node-type-definer
  { define lookup-type-infer-node-type end } => { } // base case.
  { define lookup-type-infer-node-type
      ?var:name :: ?dfm-type:name , ?accessor:name ;
      ?more:* 
    end } 
   => { define method infer-node-type
	  (?var :: ?dfm-type, css :: <call-site-summary> , wa :: <work-agenda>) 
          => (result :: <&type>)
          lookup-type(?var.?accessor, css, ?var);
	end;
        define lookup-type-infer-node-type 
          ?more
        end } 
end;

define lookup-type-infer-node-type
//  or :: <object-reference>, computation-value ; 
//  If we really need the above we need to widen the declaration of the
//  infer-node-type generic
  tx :: <temporary-transfer-computation>, computation-value ;
  kd :: <keyword-default>, computation-value ;
  sbv :: <set-cell-value!>, computation-value ;
  a :: <assignment>, computation-value ;
  a :: <type-definition>, computation-value ;
  r :: <return>, computation-value ;
  e :: <exit>, computation-value ;
  mcl :: <make-closure>, computation-closure-method ;
  mcl :: <initialize-closure>, computation-closure-method ;
end;

define method infer-node-type (vr :: <variable-reference>, css :: <call-site-summary> , wa :: <work-agenda>) 
  => (result :: <&type>)
  lookup-type(vr.referenced-binding, css, vr);
end;


// ADJUST MV
define method infer-node-type (amv :: <adjust-multiple-values>, css, w-l)
  => (result :: <&type>)
  let v = lookup-type(amv.computation-value, css, amv);
  if (^subtype?(v, typist-<bottom-type>()) | v == typist-<unknown-type>())
    v;
  else
    let old-fixed-types = v.fixed-types;
    let num-req = amv.number-of-required-values;
    let num-supplied = old-fixed-types.size;
    if (num-req = num-supplied)
      if (v.values-rest-type)
        make(<values-type>,
             types: copy-sequence(old-fixed-types, start: 0, end: num-req),
             rest-type: #f);
      else
        v
      end
    elseif (num-req < num-supplied)
      make(<values-type>,
           types: copy-sequence(old-fixed-types, start: 0, end: num-req),
           rest-type: #f);
    else
      let fill-type =
        if (v.values-rest-type)
          maybe-make-inferred-union(amv, v.values-rest-type, typist-<false-type>())
        else
          typist-<false-type>()
	end;
      let new-fixed-type = concatenate(old-fixed-types, 
                                       make(<vector>, 
                                            size: num-req - num-supplied,
                                            fill: fill-type));
      make(<values-type>,
           types: new-fixed-type,
           rest-type: #f);
    end;
  end;
end;

define method infer-node-type (amv :: <adjust-multiple-values-rest>, css, w-l)
  => (result :: <&type>)
  let v = lookup-type(amv.computation-value, css, amv);
  if (^subtype?(v, typist-<bottom-type>()) | v == typist-<unknown-type>())
    v;
  else
    let old-fixed-types = v.fixed-types;
    let num-req = amv.number-of-required-values;
    let num-supplied = old-fixed-types.size;
    if (num-req <= num-supplied)
      make(<values-type>,
           types: copy-sequence(old-fixed-types, start: 0, end: num-req),
           rest-type: #f);
    else
      let fill-type =
        if (v.values-rest-type)
          maybe-make-inferred-union(amv, v.values-rest-type, typist-<false-type>())
        else
          typist-<false-type>()
	end;
      let new-fixed-type = concatenate(old-fixed-types, 
                                       make(<vector>, 
                                            size: num-req - num-supplied,
                                            fill: fill-type));
      make(<values-type>,
           types: new-fixed-type,
           rest-type: v.values-rest-type);
    end;
  end;
end;

// GUARANTEE TYPE
define method infer-node-type (gt :: <guarantee-type>, css :: <call-site-summary>, w-l :: <work-agenda>)
  => (result :: <&type>)
  gt.static-guaranteed-type 
  | poor-mans-check-type-intersection(gt, gt.guaranteed-type, css)
end;

// ASSIGNMENTS
define method infer-node-type (mb :: <make-cell>, css, w-l)
  => (result :: <&type>)
  // we store the type of the box in computation
  cached-type-union(mb.inferred-type, lookup-type(mb.computation-value, css, mb));
end;

define method infer-node-type (bv :: <get-cell-value>, css, w-l)
  => (result :: <&type>)
  // we store the type of the box in the -creation computation.
  let b = bv.computation-cell;
  let temp = get-renaming(b, css, bv); 
  if (temp == b)
//    if (instance?(b.generator, <get-cell-value>))
//      infer-node-type(b.generator, css, w-l)
//    else
      b.generator.inferred-type;
//    end
  else
    css.pool[temp];
  end;
end;

// MERGE

define inline function lookup-left-type(m :: <if-merge>, css)
  let if-c = m.previous-computation;
  let value = m.merge-left-value;
  let renaming = get-true-renaming-at(value, css, if-c);
  if (renaming)
    css.pool[renaming]
  else
    lookup-type(value, css, m)
  end;
end;

define inline function lookup-right-type(m :: <if-merge>, css)
  let if-c = m.previous-computation;
  let value = m.merge-right-value;
  let renaming = get-false-renaming-at(value, css, if-c);
  if (renaming)
    css.pool[renaming]
  else
    lookup-type(value, css, m)
  end;
end;

define method infer-node-type (m :: <if-merge>, css, w-l)
  => (result :: <&type>)
  let path = element(css.branch-taken, m.previous-computation, default: #f);
  select (path)
    true: => lookup-left-type(m, css);
    false: => lookup-right-type(m, css);
    otherwise =>
      maybe-make-inferred-union(m, lookup-left-type(m, css),
                                   lookup-right-type(m, css));
  end;
end;

define method infer-node-type (m :: <binary-merge>, css, w-l)
  => (result :: <&type>)
  maybe-make-inferred-union(m, lookup-type(m.merge-left-value, css, m),
                               lookup-type(m.merge-right-value, css, m));
end;

// SLOT-VALUE support
define method infer-node-type (s :: <any-slot-value>, css, w-l)
  => (result :: <&type>)
  s.computation-slot-descriptor.^slot-type;
end;

define method infer-node-type (s :: <any-repeated-slot-value>, css, w-l)
  => (result :: <&type>)
  if (computation-repeated-byte?(s))
    dylan-value(#"<raw-byte-character>")
  else
    s.computation-slot-descriptor.^slot-type;
  end
end;

// TODO: These two could use use intersection of type when they're unions
define method infer-node-type (s :: <slot-value-setter>, css, w-l)
  => (result :: <&type>)
  let new-type = lookup-type(s.computation-new-value, css, s);
  let required-type =  s.computation-slot-descriptor.^slot-type;
  if (^subtype?(new-type, required-type))
    new-type
  else
    required-type;
  end;
end;

define method infer-node-type (s :: <repeated-slot-value-setter>, css, w-l)
  => (result :: <&type>)
  let new-type = lookup-type(s.computation-new-value, css, s);
  let required-type =  s.computation-slot-descriptor.^slot-type;
  if (^subtype?(new-type, required-type))
    new-type
  else
    required-type;
  end;
end;

//NB. This is only expected to be called from refine-initial-type!!
define method infer-node-type (be :: <bind-exit>, css, w-l)
  => (result :: <&type>)
//  css.pool[be];
//  At the moment the treatment of <bind-exit> is unsafe.  So we just play safe
//  here!
  make(<values-type>, 
       types: $the-empty-vector,
       rest-type: typist-<object-type>())
end;

// VALUES
// this is hacked up a bit to support exit-fns.
define method infer-node-type (v :: <values>, css, w-l)
  => (result :: <&type>)
  let fixed = v.fixed-values;
  let rest = v.rest-value;
  let inferred-rest-type = when (rest) lookup-type(rest, css, v) end;
  let temp = v.temporary;
  let temp-users = temp.users;
  if (empty?(fixed)
      & temp-users.size = 1
      & instance?(temp-users[0], <exit>)) 
    if (instance?(rest, <stack-vector-temporary>))
      inferred-rest-type;
    elseif (instance?(rest, <lexical-rest-variable>))
      // QUESTION: Why should this be different?
      let inferred-rest-type = 
        css.arg-types[css.css-lambda.^function-signature.^signature-number-required];
      let type =
        if (instance?(inferred-rest-type, <rest-values-type>))
          make(<values-type>, 
             types: inferred-rest-type.fixed-types,
             rest-type: inferred-rest-type.values-rest-type);
        else
          make(<values-type>, 
             types: $the-empty-vector,
             rest-type: typist-<object-type>() /*inferred-rest-type*/)
        end;
      type
    else
      make(<values-type>, 
           types: $the-empty-vector, 
           rest-type: inferred-rest-type);
    end;
  else
    block (return)
      local method get-type(tv)
              let type = lookup-type(tv, css, v);
              if (instance?(type, <values-type>))
                if (empty?(type.fixed-types))
                  type.values-rest-type | typist-<false-type>();
                else
                  type.fixed-types[0];
                end;
              elseif (^subtype?(type, typist-<bottom-type>()))
                return(type);
              else
                type;
              end;
            end;
      let types = map-as(<simple-object-vector>, get-type, fixed);
      make(<values-type>, 
           types: types,
           rest-type: inferred-rest-type);
    end;
  end;
end;

define method infer-node-type 
  (xsv :: <extract-single-value>, css, w-l)
  => (result :: <&type>)
  let values-type = lookup-type(xsv.computation-value, css, xsv);
  // Assume the optimizer has got everything into shape and that 
  // <extract-single-value> really is working on a mv-temporary
  // with a corresponding values-type type. Even so the type could be
  // `unknown`.
  if ((values-type ==  typist-<unknown-type>()) | 
      ^subtype?(values-type, typist-<bottom-type>()))
    values-type;
  else
    let i = xsv.index;
    let fixed-t = values-type.fixed-types;
    if (i < fixed-t.size)
      fixed-t[i];
    elseif (values-type.values-rest-type)
      // can't know what type it might be
      cached-type-union(typist-<false-type>(), values-type.values-rest-type);
    else
      // there aren't any `rest values'
      typist-<false-type>();
    end;
  end;
end;
  
// TODO: this needs revisiting when limited collections are supported better.
define method infer-node-type (xrv :: <extract-rest-value>, css, w-l)
  => (result :: <&type>)
  // for the moment we can't say much about this
  typist-values-rest-type();
end;

// TODO: this should really do an intersection of the inferred type and the
// constrained type

define function poor-mans-check-type-intersection
    (comp :: <computation>, temp :: false-or(<value-reference>), css)
 => (intersection :: <&type>)
  let inferred-type = lookup-type(comp.computation-value, css, comp);
  if (temp)
    let c-type = lookup-type(temp, css, comp);
    let constrained-type = select(c-type by instance?)
                             <&singleton> => 
                                select (c-type.^singleton-object by instance?)
                                  <&type> => c-type.^singleton-object;
                                  otherwise => c-type;
                                end;
                             otherwise => c-type;
                           end;
    if (instance?(constrained-type, <&type>))
      if (^subtype?(inferred-type, constrained-type))
        // hopefully this one will get folded later although we could do it
        // now but the 'walker' might die (although I don't think it would).
        if (^subtype?(inferred-type, typist-<bottom-type>()))
          constrained-type
        else
          inferred-type;
        end
      elseif (^subtype?(constrained-type, inferred-type))
        constrained-type;
      elseif (guaranteed-disjoint?(constrained-type, inferred-type))
        if (instance?(css, <default-call-site-summary>))
          delayed-typist-note(css, comp,
                      <run-time-type-error>, 
                      source-location: dfm-source-location(comp),
                      context-id: dfm-context-id(comp),
                      expected-type: constrained-type,
                      inferred-type: inferred-type);
//          break("rtte");
	elseif (*warn-for-all-summaries*)
          delayed-typist-note(css, comp,
                      <possible-run-time-type-error>, 
                      source-location: dfm-source-location(comp),
                      context-id: dfm-context-id(comp),
                      expected-type: constrained-type,
                      inferred-type: inferred-type,
                      default-path: find-default-css-caller(css));
        end;
        // typist-<bottom-type>();
        constrained-type
      else
        //this is where we should 'intersect' the types. But...
        constrained-type;
      end;
    else
      // not known at compile time so assuming execution gets past
      // this check we can assume its at most inferred type.
      inferred-type;
    end;
  else
    // optimizer has decided check is unneccassary
    inferred-type;
  end;
end;


define method infer-node-type (ct :: <check-type>, css, w-l)
  => (result :: <&type>)
  poor-mans-check-type-intersection(ct, ct.type, css)
end;

// TODO: again this should produce an intersection.

// However I question the design of this set of instructions. arguably
// the check type could be done after the extract-*-value instruction.

// NB. this puts the burden on ^subtype?

define inline function maybe-extract-singleton (type)
  select(type by instance?)
    <&singleton> => 
      select (type.^singleton-object by instance?)
        <&type> => type.^singleton-object;
        otherwise => type;
      end;
    otherwise => type;
  end;
end;

define function check-type-intersection
  (inferred-type :: <&type>, desired-type :: <&type>)
    => (intersect :: <&type>, guaranteed-disjoint? :: <boolean>)
  if (^subtype?(desired-type, inferred-type))
    values(desired-type, #f)
  elseif (^subtype?(inferred-type, desired-type))
    values(inferred-type, #f)
  elseif (guaranteed-disjoint?(desired-type, inferred-type))
    // typist-<bottom-type>();
    if (instance?(inferred-type, <&raw-type>))
      values(inferred-type, #t)  // Gross hack - we assume the programmer
                                 // knows what he is doing!
    else
      values(desired-type, #t)
    end
  else
    // This is where we should intersect but.. do nothing
    values(desired-type, #f)
  end;
end;

define method note-disjoint
  (inferred-type :: <&type>, desired-type :: <&type>, 
   c :: <computation>, css :: <default-call-site-summary>)
  let note-class = 
    if (instance?(c, <result-check-type-computation>))
      <run-time-result-type-error>
    else
      <run-time-type-error>
    end;
  delayed-typist-note(css, c,
    note-class,
    source-location: dfm-source-location(c),
    context-id: dfm-context-id(c),
    expected-type: desired-type,
    inferred-type: inferred-type);
end;

define method note-disjoint
  (inferred-type :: <&type>, desired-type :: <&type>, 
   c :: <computation>, css :: <specialized-call-site-summary>)
  if (*warn-for-all-summaries*)
    delayed-typist-note(css, c,
      <possible-run-time-type-error>, 
      source-location: dfm-source-location(c),
      context-id: dfm-context-id(c),
      expected-type: desired-type,
      inferred-type: inferred-type,
      default-path: find-default-css-caller(css));
  end;
end;

define method infer-node-type 
  (ct :: <multiple-value-check-type>, css, w-l)
    => (result :: <&type>)
  let inferred-type = lookup-type(ct.computation-value, css, ct);

  if (^subtype?(inferred-type, typist-<bottom-type>()))
    inferred-type;

  else // Hopefully we have a values type
    let values-type = inferred-type;

    local method get-type (temp)
            if (temp)
              maybe-extract-singleton(lookup-type(temp, css, ct));
            else
              // somebody decided runtime check unnecessary
              typist-<object-type>();
            end;
          end;

    let values-fixed = values-type.fixed-types;
    let values-fixed-size = values-fixed.size;
    let checked-fixed-size = ct.types.size;

    let result-fixed = map-as(<vector>, get-type, ct.types);

    let warn? = #t;  // We only want to give at most one warning

    local method note-if-disjoint(disjoint? :: <boolean>)
      if (disjoint? & warn?)
        warn? := #f;
        note-disjoint(inferred-type, 
                      make(<values-type>, 
                           types: map-as(<vector>, get-type, ct.types)),
                      ct, css);
      end
    end;

    for (i from 0 below checked-fixed-size, inferred in values-fixed)
      if (ct.types[i])
        let (intersect, disjoint?) = 
          check-type-intersection(inferred, result-fixed[i]);
        note-if-disjoint(disjoint?);  
        result-fixed[i] := intersect; 
      else // somebody decided runtime check unnecessary
        result-fixed[i] := inferred; 
      end
    end;

    when (values-fixed-size < checked-fixed-size)
      let rest = values-type.values-rest-type;
      let rest-values-type =
        if (~rest)
          // No rest type, so default to singleton(#f).
          typist-<false-type>();
        else
          // Use rest type union singleton(#f)
          maybe-make-inferred-union(ct, rest, typist-<false-type>());
	end;

      for (i from values-fixed-size below checked-fixed-size)
        // Pad out checked types with intersection of checked
        // type and the fill type.
        if (ct.types[i])
          let (intersect, disjoint?) = 
            check-type-intersection(rest-values-type, result-fixed[i]);
          note-if-disjoint(disjoint?);  
          result-fixed[i] := intersect;   
        else // somebody decided runtime check unnecessary
          result-fixed[i] := rest-values-type; 
        end
      end
    end;
    make(<values-type>, types: result-fixed);
  end
end;


// TODO: Share more with the previous method

define method infer-node-type 
  (ct :: <multiple-value-check-type-rest>, css, w-l)
    => (result :: <&type>)
  let inferred-type = lookup-type(ct.computation-value, css, ct);

  if (^subtype?(inferred-type, typist-<bottom-type>()))
    inferred-type;

  else // Hopefully we have a values type
    let values-type = inferred-type;

    local method get-type (temp)
            if (temp)
              maybe-extract-singleton(lookup-type(temp, css, ct));
            else
              // somebody decided runtime check unnecessary
              typist-<object-type>();
            end;
          end;

    let values-fixed = values-type.fixed-types;
    let values-fixed-size = values-fixed.size;
    let checked-fixed-size = ct.types.size;

    let result-fixed = make(<vector>, 
                             size: max(values-fixed-size, checked-fixed-size));
    let checked-rest-type = 
      if (ct.rest-type)
        maybe-extract-singleton(lookup-type(ct.rest-type, css, ct));
      else
        typist-<object-type>();
       end;
    map-into(result-fixed, get-type, ct.types);

    let values-rest-type = values-type.values-rest-type;

    let warn? = #t;  // We only want to give at most one warning

    local method note-if-disjoint(disjoint? :: <boolean>)
      if (disjoint? & warn?)
        warn? := #f;  // Don't warn again
        note-disjoint(inferred-type,
                      make(<values-type>, 
                           types: map-as(<vector>, get-type, ct.types), 
                           rest-type: checked-rest-type),
                      ct, css);
      end
    end;

    for (i from 0 below checked-fixed-size, inferred in values-fixed)
      if (ct.types[i])
        let (intersect, disjoint?) = 
          check-type-intersection(inferred, result-fixed[i]);
        result-fixed[i] := intersect; 
        note-if-disjoint(disjoint?);  
      else // somebody decided runtime check unnecessary
        result-fixed[i] := inferred; 
      end
    end;

    when (values-fixed-size < checked-fixed-size)
      let rest-values-type =
        if (~values-rest-type)
          // No rest type, so default to singleton(#f).
          typist-<false-type>();
        else
          // Use rest type union singleton(#f)
          maybe-make-inferred-union(ct, values-rest-type, typist-<false-type>());
	end;

      for (i from values-fixed-size below checked-fixed-size)
        // Pad out checked types with intersection of checked
        // type and the fill type.
        if (ct.types[i])
          let (intersect, disjoint?) = 
            check-type-intersection(rest-values-type, result-fixed[i]);
          result-fixed[i] := intersect;   
          note-if-disjoint(disjoint?);  
        else // somebody decided runtime check unnecessary
          result-fixed[i] := rest-values-type; 
	end
      end
    end;

    // Now make sure rest of fixed types, if any, are compatible 
    // with checked-rest-type.
    if (ct.rest-type)
      for (i from checked-fixed-size below values-fixed-size)
        let (intersect, disjoint?) = 
          check-type-intersection(values-fixed[i], checked-rest-type);
        result-fixed[i] := intersect;
        note-if-disjoint(disjoint?);  
      end;
    else
      for (i from checked-fixed-size below values-fixed-size)
        result-fixed[i] := values-fixed[i]
      end
    end;

    // Result-fixed now contains the fixed types.  Also need to 
    // check if the rest type, if any, is compatible.
 
    if (~values-rest-type)
      make(<values-type>, types: result-fixed)
    elseif (ct.rest-type)
      let (intersect, disjoint?) = 
        check-type-intersection(values-rest-type, checked-rest-type);
      note-if-disjoint(disjoint?);  
      make(<values-type>, types: result-fixed, rest-type: intersect)
    else
      make(<values-type>, types: result-fixed, rest-type: values-rest-type)
    end
  end
end;

/*
define method infer-node-type 
  (ct :: <multiple-value-check-type-rest>, css, w-l)
  => (result :: <&type>)
  let inferred-type = lookup-type(ct.computation-value, css, ct);
  local method get-type (temp)
          if (temp)
            maybe-extract-singleton(lookup-type(temp, css, ct));
          else
            // somebody decided runtime check unnecessary
            typist-<object-type>();
          end;
        end;
  let types = map-as(<vector>, get-type, ct.types);
  let r-type = if (ct.rest-type)
                 maybe-extract-singleton(lookup-type(ct.rest-type, css, ct));
               else
                 typist-<object-type>();
               end;
  let constrained-type = make(<values-type>, types: types, rest-type: r-type);
  if (^subtype?(inferred-type, constrained-type))
    //assume this is true for the moment. If turns out not to be
    //(because e.g. there are dynamic types in the constrained type)
    // this won't be folded and an error will occur at runtime.
    inferred-type;
  elseif (^subtype?(constrained-type, inferred-type))
    constrained-type;
  elseif (guaranteed-disjoint?(constrained-type, inferred-type))
    delayed-typist-note(css, ct,
                   if (instance?(ct, <result-check-type-computation>))
                     <run-time-result-type-error>
                   else
                     <run-time-type-error>
                   end, 
                   source-location: dfm-source-location(ct),
                   context-id: dfm-context-id(ct),
                   expected-type: constrained-type,
                   inferred-type: inferred-type);
    // typist-<bottom-type>();
    constrained-type
  else
    constrained-type;
  end;
end;
*/

/*
define method infer-node-type 
  (kdvm :: <keyword-default-value-marker>, css, w-l)
  => (result :: <&type>)
  let val = keyword-default-value-specifiers(kdvm)[keyword-default-value-index(kdvm) * 2 + 1];
  select (val.object-class)
    <&type>,
    <&class>,
    <&object>,
    <number>,
    <&raw-integer>,
    <boolean>,
    <&raw-boolean>,
    <character>,
    <string>,
    <symbol>,
    <vector>,
    <list>,
    <&callable-object>
      => cached-singleton(val);
    otherwise
      => typist-<object-type>();
  end;
end;
*/



/*
;;; refine-initial-node-type is what retypes nodes. Note that it propagates type info
;;; by scheduling dependent nodes for retyping if the result of infering the type of
;;; the node is not a subtype of the previous 'inference`.
*/

define inline function refine-initial-type-internal 
  (c :: <computation>, css :: <call-site-summary>, wa :: <work-agenda>)
  let temp = c.temporary;
  if (dependents?(temp))
    let initial-type = lookup-type(temp, css, c);
    let new-type = infer-node-type(c, css, wa);
    unless (^subtype?(new-type, initial-type))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] :=  maybe-make-inferred-union(c, new-type, initial-type);
      schedule-users(wa, temp.users, css);
    end;
  end;
end;

define generic refine-initial-node-type
    (c :: <computation>, call-site-summary , work-agenda) => ();

define macro standard-initial-type-refinement-definer
  { define standard-initial-type-refinement end } => { } // base case.
  { define standard-initial-type-refinement 
      ?var:name :: ?dfm-type:name ;
      ?more:* 
    end } 
   => { define method refine-initial-node-type
	  (?var :: ?dfm-type, css, wa) => ()
	  refine-initial-type-internal(?var, css, wa);
	end;
        define standard-initial-type-refinement 
          ?more
        end } 
end;

define standard-initial-type-refinement 
//  vr :: <variable-reference>;
  mcl :: <make-closure>;
  icl :: <initialize-closure>;
  tx :: <temporary-transfer-computation>;
  kd :: <keyword-default>;
  gt :: <guarantee-type>;
  //gbv :: <get-cell-value>;
  esv :: <extract-single-value>;
  erv :: <extract-rest-value>;
  m :: <binary-merge>;
  v :: <values>;
  // call :: <call>;
  ct :: <check-type>;
  amv :: <adjust-multiple-values>;
  amv :: <adjust-multiple-values-rest>;
  mvct :: <multiple-value-check-type>;
  mvctr :: <multiple-value-check-type-rest>;  
  s :: <any-slot-value>;
  //kdvm :: <keyword-default-value-marker>;
end;

define method refine-initial-node-type
    (c :: <loop-merge>, css :: <call-site-summary>, wa :: <work-agenda>) => ()
  let temp = c.temporary;
  if (dependents?(temp))
    let initial-type = lookup-type(temp, css, c);
    let new-type = infer-node-type(c, css, wa);
    unless (^subtype?(new-type, initial-type))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] :=  maybe-make-inferred-union(c, new-type, initial-type);
      schedule-users(wa, temp.users, css);
    end;
  end;
end;

define method refine-initial-node-type
    (c :: <variable-reference>, css , wa) => ();
  let temp = c.temporary;
  if (dependents?(temp))
    let initial-type = lookup-type(temp, css, c);
    let new-type = infer-node-type(c, css, wa);
//    format-out("  Previous: %=, new: %=\n", initial-type, new-type);
    unless (^subtype?(new-type, initial-type))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] :=  maybe-make-inferred-union(c, new-type, initial-type);
//      format-out("  Union = %=\n", css.pool[temp]);
//      format-out("  Users = %=\n", temp.users);
      schedule-users(wa, temp.users, css);
    end;
  end;
end;


define method refine-initial-node-type
    (mbv :: <make-cell>, css , wa) => ();
  let temp = mbv.temporary;
  if (dependents?(temp))
    let initial-type = mbv.inferred-type;
    let new-type = infer-node-type(mbv, css, wa);
    unless (^subtype?(new-type, initial-type))
      // don't need to consider renamings here(?!)
      mbv.inferred-type := maybe-make-inferred-union(mbv, initial-type, new-type);
      schedule-filtered-dependents(wa, mbv.type-dependencies, css);
    end;
  end;
end;


define method refine-initial-node-type
    (gbv :: <get-cell-value>, css , wa) => ();
  let temp = gbv.temporary;
  if (dependents?(temp))
    let initial-type = lookup-type(temp, css, gbv);
    let new-type = infer-node-type(gbv, css, wa);
    unless (^subtype?(new-type, initial-type))
      schedule-renaming-computations(temp, css, wa);
      schedule-renaming-computations(gbv.computation-cell, css, wa);
      css.pool[temp] :=  maybe-make-inferred-union(gbv, new-type, initial-type);
      schedule-users(wa, temp.users, css);
    end;
  end;
end;

define method refine-initial-node-type
    (sbv :: <set-cell-value!>, css , wa) => ();
  let temp = sbv.temporary;
  let initial-type = lookup-type(temp, css, sbv);
  let new-type = infer-node-type(sbv, css, wa);
  if (*KM-FLAG*)
    format-out("rnt %=: old: %=, new: %=\n",
      sbv, initial-type, new-type);
  end;
  unless (^subtype?(new-type, initial-type))
    if (dependents?(temp))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] := new-type;
      schedule-users(wa, temp.users, css);
    end;
    schedule-filtered-dependents(wa, sbv.computation-cell.generator.type-dependencies, css);
    sbv.computation-cell.generator.inferred-type := 
      maybe-make-inferred-union(sbv, sbv.computation-cell.generator.inferred-type, new-type);
  end;
end;

define method refine-initial-node-type
  (a :: <assignment>, css, wa) => ()
  let temp = a.temporary;
  let initial-type = lookup-type(temp, css, a);
  let new-type = infer-node-type(a, css, wa);
  unless (^subtype?(new-type, initial-type))
    if (dependents?(temp))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] := new-type;
      schedule-users(wa, temp.users, css);
    end;
    a.assigned-binding.inferred-type :=
      maybe-make-inferred-union(a, a.assigned-binding.inferred-type, new-type);
    schedule-filtered-dependents(wa, a.assigned-binding.type-dependencies, css);
  end;
end;

define method refine-initial-node-type
  (a :: <type-definition>, css, wa) => ()
/*
  let temp = a.temporary;
  let initial-type = lookup-type(temp, css, a);
  let new-type = infer-node-type(a, css, wa);
  unless (^subtype?(new-type, initial-type))
    if (dependents?(temp))
      schedule-renaming-computations(temp, css, wa);
      css.pool[temp] := new-type;
      schedule-users(wa, temp.users, css);
    end;
    a.typed-binding.inferred-type :=
      maybe-make-inferred-union(a, a.typed-binding.inferred-type, new-type);
    schedule-filtered-dependents(wa, a.typed-binding.type-dependencies, css);
  end;
*/
end;

define method refine-initial-node-type
  (a :: <conditional-update!>, css, wa) => ()
  let initial-type = a.assigned-binding.inferred-type;
  let new-type = infer-node-type(a, css, wa);
  unless (^subtype?(new-type, initial-type))
    a.assigned-binding.inferred-type :=
      maybe-make-inferred-union(a, initial-type, new-type);
    schedule-dependents(wa, a.assigned-binding.type-dependencies);
  end;
end;


define method refine-initial-node-type
    (be :: <bind-exit>, css :: <default-call-site-summary>, wa) => ()
  let temp = be.temporary;
  if (dependents?(temp))
    let initial-type = lookup-type(temp, css, be);
    let type = infer-node-type(be, css, wa);
    let new-type = if (type == typist-<unknown-type>())
                     make(<values-type>, 
                          types:  $the-empty-vector,
                          rest-type: typist-values-rest-type());
                   else
                     type;
                   end;
    unless (^subtype?(new-type, initial-type))
      let union = maybe-make-inferred-union(be, new-type, initial-type);   
      for (css in css.css-lambda.call-site-summaries)
        // Don`t need to consider renamings here (?!)
        css.pool[temp] := union;
      end;
      schedule-users(wa, temp.users, css);
    end;
  end;
end;

define method refine-initial-node-type
    (call :: <return>, css , wa) => ();
  // if there are no users of the result of this call there is no point
  // in retyping it.
  let initial-type = lookup-type(call.temporary, css, call);
  let new-type = infer-node-type(call, css, wa);

  unless (^subtype?(new-type, initial-type))
    css.result-type := new-type;
    schedule-dependents(wa, css.type-dependencies);
  end;
end;

define method refine-initial-node-type
  (exit :: <exit>, css, work-agenda) => ()
  let new-type = infer-node-type(exit, css, work-agenda);
  css.pool[exit.temporary] := typist-<bottom-type>();
  let dependents = exit.entry-state.generator.type-dependencies;
  if (~queue-empty?(dependents)) 
    // Dont need to consider renamings here
    iterate loop (item :: <typist-work-item> = dependents.queue-front)
      let initial-type = item.call-site-summary.pool[item.node.temporary];
      unless (^subtype?(new-type, initial-type))
        item.call-site-summary.pool[item.node.temporary] := 
          maybe-make-inferred-union(item.node, initial-type, new-type);
          schedule-for-retyping(work-agenda, item.node, item.call-site-summary);
      end;
      when (item.next-item)
        loop(item.next-item);
      end;
    end;
  end;
end;


define method refine-initial-node-type
    (up :: <unwind-protect>, css , wa) => ();
end;

// The following needs to go somewhere else eventually...

define generic constant-value?(ref, css, comp)
  => (constant? :: <boolean>, value :: <object>);

define method constant-value? 
  (ref :: <object-reference>, css, comp)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <object-reference>.
  values(#t, reference-value(ref))
end method;


define method constant-value? 
  (ref :: <defined-constant-reference>, css, comp)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <defined-constant-reference>.
  // TODO: DOESN'T HANDLE FALSE
  let value = computation-value(ref);
  if (value)
    let (inlineable?, inline-value) = inlineable?(value);
    if (inlineable?)
      values(#t, inline-value)
    else
      values(#f, #f)
    end if
  else
    values(#f, #f)
  end if;
end method;

define method constant-value?
    (ref :: <temporary>, css, comp)
   => (constant-value? :: <boolean>, constant-value)
  // If this temporary is estimated as a singleton, extract the constant.
  let type = lookup-type(ref, css, comp);
  if (instance?(type, <&singleton>))
    values(#t, ^singleton-object(type))
  else
    values(#f, #f)
  end
end method;

define method constant-value? 
  (ref :: <value-reference>, css, comp)
   => (constant-value? :: <boolean>, constant-value)
  // Other kinds of <value-reference>s are not constants.
  values(#f, #f)
end method;
