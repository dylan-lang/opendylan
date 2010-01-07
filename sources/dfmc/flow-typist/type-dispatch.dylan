Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <dispatch-record> (<function-call-record>) 
end;

define method copy-dispatch-result (obj :: <dispatch-record>)
  => (obj :: <dispatch-record>)
  make(obj.object-class, function: obj.called-function, computation: obj.computation,
                         context: obj.context, input-types: obj.input-types,
                         type: obj.inferred-type, summaries: obj.candidate-summaries);
end;


define abstract class <successful-dispatch> (<dispatch-record>)
end;

define class <generic-constant-folded> (<successful-dispatch>, <constant-folded>)
end;


define abstract class <successfully-dispatched-call> (<successful-dispatch>)
  slot dispatched-method, 
    required-init-keyword: method: ;
  slot next-method-list :: <list> = #(),
    init-keyword: next-method-list: ;
end;

define method copy-dispatch-result (obj :: <successfully-dispatched-call>)
  => (obj :: <successfully-dispatched-call>);
  make(obj.object-class, function: obj.called-function, computation: obj.computation,
                         context: obj.context, input-types: obj.input-types,
                         type: obj.inferred-type, summaries: obj.candidate-summaries,
                         method: obj.dispatched-method,
                         next-method-list: obj.next-method-list);
end;

define class <unique-method> (<successfully-dispatched-call>)
end;



define class <next-method-called> (<successfully-dispatched-call>)
end;

define abstract class <failed-dispatch> (<dispatch-record>)
end;

define class <failed-apply> (<failed-dispatch>)
end;

define abstract class <open-generic> (<failed-dispatch>)
end;

define class <open-gf-no-sealing> (<open-generic>)
end;

define class <open-gf-sealed-domains> (<open-generic>)
end;

define abstract class <failed-sealed-generic> (<failed-dispatch>)
end;

define abstract class <lack-of-methods> (<failed-sealed-generic>)
end;


define abstract class <imprecision-failure> (<failed-sealed-generic>)
end;

define class <no-applicable-methods> (<lack-of-methods>)
end;

define class <no-applicable-methods+success> (<lack-of-methods>, <successfully-dispatched-call>)
end;

define class <no-applicable-methods+imprecision> (<lack-of-methods>, <imprecision-failure>)
end;


define class <imprecise-argument-types> (<imprecision-failure>)
  slot potentially-applicable-methods, init-keyword: methods:;
end;

define method copy-dispatch-result (obj :: <imprecise-argument-types>)
  => (obj :: <imprecise-argument-types>)
  make(<imprecise-argument-types>,
        function: obj.called-function, computation: obj.computation,
        context: obj.context, input-types: obj.input-types,
        type: obj.inferred-type, summaries: obj.candidate-summaries,
        methods: obj.potentially-applicable-methods);
end;


define class <union-argument-types> (<imprecision-failure>)
end;

define class <bottom-in-arg-list> (<failed-dispatch>)
end;

define class <megamorphic-args> (<failed-dispatch>)
end;

define abstract class <ambiguous-dispatch> (<failed-dispatch>)
  slot ambiguous-methods :: <list> = #(),
    init-keyword: ambiguous-methods:;
end;

define method copy-dispatch-result (obj :: <ambiguous-dispatch>)
  => (obj :: <ambiguous-dispatch>)
  make(obj.object-class,
        function: obj.called-function, computation: obj.computation,
        context: obj.context, input-types: obj.input-types,
        type: obj.inferred-type, summaries: obj.candidate-summaries,
        ambiguous-methods: obj.ambiguous-methods);
end;


// This is created a point of detection
define class <ambiguous-methods> (<ambiguous-dispatch>)
end;

// The following  can result from merging dispatch results
define class <ambiguous+success> (<ambiguous-dispatch>, <successfully-dispatched-call>)
end;

define method copy-dispatch-result (obj :: <ambiguous+success>)
  => (obj :: <ambiguous+success>)
  make(<ambiguous+success>,
        function: obj.called-function, computation: obj.computation,
        context: obj.context, input-types: obj.input-types,
        type: obj.inferred-type, summaries: obj.candidate-summaries,
        method: obj.dispatched-method, ambiguous-methods: obj.ambiguous-methods);
end;

define class <ambiguous+imprecision> (<ambiguous-dispatch>, <imprecise-argument-types>)
end;

define method copy-dispatch-result (obj :: <ambiguous+imprecision>)
  => (obj :: <ambiguous+imprecision>)
  make(<ambiguous+imprecision>,
        function: obj.called-function, computation: obj.computation,
        context: obj.context, input-types: obj.input-types,
        type: obj.inferred-type, summaries: obj.candidate-summaries,
        methods: obj.potentially-applicable-methods, ambiguous-methods: obj.ambiguous-methods);
end;

define class <ambiguous+no-result> (<ambiguous-dispatch>, <lack-of-methods>)
end;



define generic dispatch-result-merge 
  (r1 :: <dispatch-record>, r2 :: <dispatch-record>)
  => (r3 :: <dispatch-record>);


define macro dispatch-result-merge-rules-definer
  // Expand a bunch of rules into methods for merging dispatch results
  { define dispatch-result-merge-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a dispatch-result-merge method.
  { ?tname1:name :: ?typ1:name, ?tname2:name :: ?typ2:name <- ?expr:expression }
  => { define method dispatch-result-merge(?tname1 :: ?typ1, ?tname2 :: ?typ2)
	=> (merge :: <dispatch-record>);
         ?expr
       end }
end;

define inline function merge-dispatch-result-details 
  (r1 :: <dispatch-record>, r2 :: <dispatch-record>, r3 :: <dispatch-record>)
  => (r4 :: <dispatch-record>);
  r3.candidate-summaries := union(r1.candidate-summaries, r2.candidate-summaries);
  r3.inferred-type := maybe-make-inferred-union
                       (r1.computation, r1.inferred-type, r2.inferred-type);

/*  unless (instance?(r3.inferred-type, <values-type>) | instance?(r3.inferred-type, <&bottom-type>))
    error("merging dispatch results did not yield a <values-type>");
  end;
*/

  r3;
end;

define dispatch-result-merge-rules
  // merging successes
  s1 :: <successfully-dispatched-call>, s2 :: <successfully-dispatched-call> <-
    begin
      let merged-types = 
//      if (s1.context.compressed?)
          map(method(t1 :: <&type>, t2 :: <&type>)
                maybe-make-inferred-union(s1.computation, t1, t2)
              end, s1.input-types, s2.input-types);
//      else
          // This is what Sizer had - not sure why though...
//        get-arg-types(s1.computation.arguments, s1.context, s1.computation);
//      end;
      let u = make(if (s1.dispatched-method == s2.dispatched-method)
                     s1.object-class;
                   else
                     <union-argument-types>;
                   end,
                   method: s1.dispatched-method,
                   function: s1.called-function,
                   computation: s1.computation,
                   input-types: merged-types,
                   context: s1.context);
      merge-dispatch-result-details(s1, s2, u);
    end;

  //merging a success with an imprecise result
  s :: <successfully-dispatched-call>, i :: <imprecision-failure> <-
    merge-dispatch-result-details(i, s, i);

  i :: <imprecision-failure>, s :: <successfully-dispatched-call> <-
    merge-dispatch-result-details(i, s, i);

  // merging imprecise results
  i1 :: <imprecise-argument-types>, i2 :: <union-argument-types> <-
     merge-dispatch-result-details(i1, i2, i1);

  i1 :: <imprecise-argument-types>, i2 :: <imprecise-argument-types> <-
     begin
       i1.potentially-applicable-methods :=
         union(i1.potentially-applicable-methods,i2.potentially-applicable-methods);
       merge-dispatch-result-details(i1, i2, i1);
     end;

  i1 :: <union-argument-types>, i2 :: <imprecise-argument-types> <-
     merge-dispatch-result-details(i2, i1, i2);

  // dealing with ambiguity (yuck!!)
  a1 :: <ambiguous-dispatch>,  a2 :: <ambiguous-dispatch> <-
    // a2 must be new and be typed <bottom>
    begin
      a1.ambiguous-methods := union(a1.ambiguous-methods, a2.ambiguous-methods);
      a1;
    end;

  a :: <ambiguous-methods>,  s :: <successfully-dispatched-call> <-
    begin
      let a+s = make(<ambiguous+success>,
                     function: a.called-function,
                     computation: a.computation,
                     method: s.dispatched-method,
                     input-types: a.input-types,
                     context: a.context);
      a+s.candidate-summaries := s.candidate-summaries;
      a+s.inferred-type := s.inferred-type;
      a+s;
    end;

  s :: <successfully-dispatched-call>, a :: <ambiguous-methods> <-
    dispatch-result-merge(a, s);

  a1 :: <ambiguous-methods>,  a2 :: <ambiguous+success> <-
    // shouldn`t happen really
    next-method(a2, a1);

  a :: <ambiguous+success>, s :: <successfully-dispatched-call> <-
    if (a.dispatched-method == s.dispatched-method)
      merge-dispatch-result-details(a, s, a);
    else
      //now we've got imprecision as well
      let a+i = make(<ambiguous+imprecision>,
                   function: a.called-function,
                   computation: a.computation,
                   input-types: a.input-types,
                   context: a.context);
      merge-dispatch-result-details(a, s, a+i);
    end;

  a+s :: <ambiguous+success>, i :: <imprecise-argument-types> <-
    begin
      let a+i = make(<ambiguous+imprecision>,
                   function: a+s.called-function,
                   computation: a+s.computation,
                   input-types: a+s.input-types,
                   context: a+s.context);
      a+i.potentially-applicable-methods := i.potentially-applicable-methods;
      a+i.ambiguous-methods := a+s.ambiguous-methods;
      merge-dispatch-result-details(a+s, i, a+i);
    end;

  i :: <imprecision-failure> , a :: <ambiguous-methods> <-
    begin
      let a+i = make(<ambiguous+imprecision>,
                   function: a.called-function,
                   computation: a.computation,
                   input-types: a.input-types,
                   context: a.context);
      a+i.potentially-applicable-methods := i.potentially-applicable-methods;
      a+i.ambiguous-methods := a.ambiguous-methods;
      merge-dispatch-result-details(i, a, a+i);
    end;


    a :: <ambiguous-methods>, i :: <imprecision-failure> <-
    begin
      let a+i = make(<ambiguous+imprecision>,
                   function: a.called-function,
                   computation: a.computation,
                   input-types: a.input-types,
                   context: a.context);
      a+i.potentially-applicable-methods := i.potentially-applicable-methods;
      a+i.ambiguous-methods := a.ambiguous-methods;
      merge-dispatch-result-details(i, a, a+i);
    end;


  // dealing with no applicable methods (yuck!!@!!)
  a1 :: <lack-of-methods>,  a2 :: <lack-of-methods> <-
    // a2 contributes nothing new so drop it
    a1;

  a :: <ambiguous-methods>,  n :: <no-applicable-methods> <-
    begin
      let a+n = make(<ambiguous+no-result>,
                     function: a.called-function,
                     computation: a.computation,
                     input-types: a.input-types,
                     context: a.context);
      a+n.candidate-summaries := a.candidate-summaries;
      a+n.inferred-type := a.inferred-type;
      a+n;
    end;

  n :: <no-applicable-methods>, a :: <ambiguous-methods> <-
     dispatch-result-merge(a, n);

  s :: <successfully-dispatched-call>, n :: <no-applicable-methods> <-
    begin
      let n+s = make(<no-applicable-methods+success>,
                     function: n.called-function,
                     computation: n.computation,
                     method: s.dispatched-method,
                     input-types: n.input-types,
                     context: n.context);
      merge-dispatch-result-details(n+s, s, n+s);
    end;

  n :: <no-applicable-methods>,  s :: <successfully-dispatched-call> <-
     dispatch-result-merge(s, n);

  i :: <imprecision-failure>, n :: <lack-of-methods> <-
    begin
      let i+n = make(<no-applicable-methods+imprecision>,
                     function: n.called-function,
                     computation: n.computation,
                     input-types: n.input-types,
                     context: n.context);
//      i+n.potentially-applicable-methods := i.potentially-applicable-methods;
      merge-dispatch-result-details(i+n, i, i+n);
    end;

  n :: <lack-of-methods>,  i :: <imprecision-failure> <-
     dispatch-result-merge(i, n);


  a :: <ambiguous+success>, n :: <no-applicable-methods> <-
    begin
      //now we've got a failure as well
      let a+n = make(<ambiguous+no-result>,
                   function: n.called-function,
                   computation: n.computation,
                   input-types: n.input-types,
                   context: n.context);
      merge-dispatch-result-details(a+n, a, a+n);
    end;

  a+i :: <ambiguous+imprecision>, l :: <no-applicable-methods> <-
    begin
      let a+n = make(<ambiguous+no-result>,
                   function: a+i.called-function,
                   computation: a+i.computation,
                   input-types: a+i.input-types,
                   context: a+i.context);
      a+n.potentially-applicable-methods := a+i.potentially-applicable-methods;
      a+n.ambiguous-methods := a+i.ambiguous-methods;
      merge-dispatch-result-details(a+n, a+i, a+n);
    end;
end;



define method typist-all-applicable-methods-guaranteed-known? 
    (f :: <&generic-function>, arg-list :: <arg-types>, call, css :: <call-site-summary>) 
    => (known? :: <boolean>, result :: false-or(<open-generic>))
  // TODO: Consider dynamic screw cases.
  if (^generic-function-sealed?(f))
    values(#t, #f);
  else
    let gf-domains = ^generic-function-domains(f);
    if (empty?(gf-domains))
      values(#f, create-function-call-record
                  (<open-gf-no-sealing>, f, arg-list, call, css));
    else
      local method domain-guaranteed-joint? (domain)
              let specializers = ^domain-types(domain);              
              arguments-guaranteed-joint?(arg-list, specializers, specializers.size )
            end;
      if (any?(domain-guaranteed-joint?, gf-domains))
        values(#t, #f);
      else
        values(#f, create-function-call-record
                    (<open-gf-sealed-domains>, f, arg-list, call, css));
      end;
    end;
  end;
end method;


define method ^typist-next? (function :: <&method>) => (value :: <boolean>)
  // This information is only available once the DFM for the method has
  // been generated.
  if (instance?(function, <&accessor-method>))
    #f
  elseif (function.body)
    ^function-next?(function)
  else
    let dcss = get-default-call-site-summary(function);
    if (instance?(dcss.result-type, <unknown-type>))
      // We might not have processed this function yet
      ^next?(function)
    else
      ^function-next?(function)
    end;
  end;
end method ^typist-next?;


define method typist-estimate-effective-method
  (f :: <&generic-function>, arg-list :: <arg-types>, 
   call :: <function-call>, css :: <call-site-summary>)
    => (result :: <dispatch-record>);
  let guaranteed-sorted = #();
  let guaranteed-others = #();
  let potentially-applicable = #();
  local method merge-method (m, sorted)
    if (empty?(sorted))
      local method after-m? (other-m)
        guaranteed-method-precedes?(m, other-m, arg-list);
      end;
      block (return)
        for (other-m in guaranteed-others)
          unless (after-m?(other-m))
            guaranteed-others := pair(m, guaranteed-others);
            return(#());
          end unless;
        end for;
        list(m);
      end block;
    else
      let lead = sorted.first;
      select (guaranteed-method-relationship(m, lead, arg-list))
        $method1-precedes
          => pair(m, sorted);
        $method2-precedes
          => pair(lead, merge-method(m, sorted.tail));
        $methods-unordered
          => // Unordered methods are no use to us, so abandon everything
             // from here on.
             guaranteed-others := pair(m, concatenate!(sorted, guaranteed-others));
             #();
      end;
    end;
  end;
  for (m in f.^generic-function-methods-known)
    let sig = ^function-signature(m);
    let num-required = ^signature-number-required(sig);
    let specializers = ^signature-required(sig);
    if (empty?(potentially-applicable))
      if (arguments-guaranteed-joint?(arg-list, specializers, num-required))
        guaranteed-sorted := merge-method(m, guaranteed-sorted);
      elseif (arguments-potentially-joint?(arg-list, specializers, num-required))
        potentially-applicable := pair(m, concatenate!(guaranteed-sorted, guaranteed-others));
        // reset these so we don't believe we've got guaranteed applicable methods.
        guaranteed-sorted := #();
        guaranteed-others := #();
      end;
    elseif (arguments-potentially-joint?(arg-list, specializers, num-required))
      potentially-applicable := pair(m, potentially-applicable);
    end;
  end;
  if (~empty?(guaranteed-sorted))
    let m = guaranteed-sorted[0];
    if (~m.^typist-next?)
      create-function-call-record
       (<unique-method>, f, arg-list, call, css, method: m);
    else
      create-function-call-record
       (<next-method-called>, f, arg-list, call, css, 
        method: m, next-method-list: tail(guaranteed-sorted));
    end;
  elseif (~empty?(guaranteed-others))
    create-function-call-record
     (<ambiguous-methods>, f, arg-list, call, css, ambiguous-methods: guaranteed-others);
  elseif (empty?(potentially-applicable))
    create-function-call-record
     (<no-applicable-methods>, f, arg-list, call, css);
  else
    find-potentially-applicable-methods(f, arg-list, call, css, potentially-applicable);
  end;
end method;

define method find-potentially-applicable-methods
  (f :: <&generic-function>, arg-list :: <arg-types>, call, css,  methods :: <list>)
  => (result :: <dispatch-record>);
  let sorted-potentially-applicable-methods = list(list(methods.head));
  for (m in methods.tail)
    block (exit)
      for (chains = sorted-potentially-applicable-methods then chains.tail,
           until: empty?(chains))
        let chain = chains[0];
        select (strictly-guaranteed-method-relationship(m, chain[0], arg-list))
          $method1-precedes
            => chains[0] := pair(m, chain);
               exit();
          $method2-precedes
            => exit();
          $methods-unordered 
            => #f;
        end;
      end;
      sorted-potentially-applicable-methods := 
        pair(list(m), sorted-potentially-applicable-methods);
    end;  
  end;
  let potentially-applicable-methods = map-as(<list>, head, sorted-potentially-applicable-methods);
  create-function-call-record
   (<imprecise-argument-types>, f, arg-list, call, css,
    methods: potentially-applicable-methods);
end;


define constant $methods-unordered = #"unordered";
define constant $method1-precedes  = #"method1";
define constant $method2-precedes  = #"method2";

// This is pretty much as per the DRM specification, but using the
// conservative "guaranteed-xxx" predicates.

define method strictly-guaranteed-method-relationship
    (m1 :: <&method>, m2 :: <&method>, args :: <arg-types>)
 => (relationship)
  let precedes-somewhere? = #f;
  let follows-somewhere? = #f;
  let specs1 = m1.^function-signature.^signature-required;
  let specs2 = m2.^function-signature.^signature-required;
  for (spec1 in specs1, spec2 in specs2, arg in args)
    let constrained-spec1 = if (^subtype?(arg, spec1)) arg else spec1 end;
    let constrained-spec2 = if (^subtype?(arg, spec2)) arg else spec2 end;
    case
      spec1 == spec2
        => ; // continue
      ^subtype?(spec1, spec2) & guaranteed-joint?(constrained-spec2, spec1)
        => precedes-somewhere? := #t;
      ^subtype?(spec2, spec1) & guaranteed-joint?(constrained-spec1, spec2)
        => follows-somewhere? := #t;
      otherwise
        => ; // continue
    end;
  end;
  if (precedes-somewhere?)
    if (follows-somewhere?)
      $methods-unordered
    else
      $method1-precedes
    end
  else
    if (follows-somewhere?)
      $method2-precedes
    else
      $methods-unordered
    end;
  end;
end method;
