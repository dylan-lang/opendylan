Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <computation-record> (<object>) 
  slot computation,
    required-init-keyword: computation:;
  slot context,
    required-init-keyword: context:;
  slot input-types :: <list>,
    required-init-keyword: input-types: ;
  slot inferred-type :: <&type> = typist-<bottom-type>(),
    init-keyword: type:;
  slot optimized? = #f;
end;

define method create-computation-record
  (class :: subclass(<computation-record>), computation, context, input-types, #rest args, #key, #all-keys)
   => (record :: <computation-record>);
  apply(make, class, 
        computation: computation, 
        context: context, 
        input-types: input-types, 
        args);
end;

define inline function record-computation-record
  (css :: <call-site-summary>, comp, record :: <computation-record>)
   => (r2 :: <computation-record>);
  css.computation-records[comp] := record;
end;

define function get-computation-record
  (css :: <call-site-summary>, comp :: <computation>)
  => (result :: false-or(<computation-record>));
  element(css.computation-records,comp, default: #f);
end;



// Some records for <call>s. Dispatch records are defined with the dispatch code.

define abstract class <call-record> (<computation-record>) 
end;

define class <illegal-call-record> (<call-record>) 
end;

define abstract class <constant-folded> (<object>)
  slot call-values,
    required-init-keyword: call-values:;
end;

define class <primitive-constant-folded> (<call-record>, <constant-folded>)
end;

define class <primitive-call-record> (<call-record>)
end;

define abstract class <function-call-record> (<call-record>)
  slot called-function,
    required-init-keyword: function:;
  // call site sumaries associated with the call
  slot candidate-summaries :: <list> = #(),
    init-keyword: summaries:;
end;


define method create-function-call-record
  (cl :: subclass(<function-call-record>), 
   function, input-types, computation, context, 
   #rest rest-args, #key, #all-keys)
   => (result :: <function-call-record>);

  apply(make, cl, 
        function:, function,
        computation:, computation, 
        context:, context, 
        input-types:, input-types, 
//        pair(foo:, pair(1, rest-args)));  // Why did AndyS write this?
        rest-args);
end;


define class <method-call-record> (<function-call-record>)
end;

define class <applied-method-record> (<function-call-record>)
end;

define class <method-constant-folded> (<function-call-record>, <constant-folded>)
end;



