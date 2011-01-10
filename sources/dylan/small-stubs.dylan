module:    internal
Synopsis:  Stubs for dylan-small
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// DISPATCH-CACHES

define constant *call-site-caches-enabled?* = #f;

define constant *profile-all-terminal-engine-nodes?* = #f;

define function %profile-count-low-setter
    (new-low :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
end function;

define function %profile-count-high-setter
    (new-low :: <machine-word>, di :: <profiling-call-site-cache-header-engine-node>)
end function;

define function install-cache-header-engine-node-next
    (old :: <cache-header-engine-node>,
     next :: type-union(<method>, <engine-node>),
     gf :: <generic-function>)
 => ()
end function;

define function handle-profiling-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-simple-typechecked-cache-head 
  (ds :: <dispatch-state>, cache, old :: <simple-typechecked-cache-header-engine-node>)
  => ();
end function;

define function compute-argument-precheck-mask (ds :: <dispatch-state>, cache)
 => ();
end function;

define function handle-partial-dispatch-cache-head
    (ds :: <dispatch-state>, cache, old :: <partial-dispatch-cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-simple-call-site-cache-head
    (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
 => (root-engine);
end function;

define function handle-unknown-cache-head
  (ds :: <dispatch-state>, cache, old :: <cache-header-engine-node>)
  => ();
end function;

define function cache-header-punt (ds :: <dispatch-state>, cache, e :: <cache-header-engine-node>)
 => ()
end function;

define function partial-dispatch-megamorphic-punt? ()
  #f
end function;

define constant *partial-dispatch?* = #f;


/// FLOAT

define inline method contagious-type
    (x :: <single-float>, y :: <single-float>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define inline method contagious-type
    (x :: <single-float>, y :: <abstract-integer>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define inline method contagious-type
    (x :: <abstract-integer>, y :: <single-float>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define sealed inline method as (class == <single-float>, x :: <integer>)
 => (result :: <single-float>)
  primitive-raw-as-single-float(primitive-integer-as-single-float(integer-as-raw(x)))
end method as;

define sealed inline method zero? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-equals?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(0.0))
end method zero?;

define sealed inline method positive? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-less-than?
    (primitive-single-float-as-raw(0.0),
     primitive-single-float-as-raw(x))
end method positive?;

define sealed inline method negative? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-less-than?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(0.0))
end method negative?;

define sealed inline method truncate/ (real :: <single-float>, divisor :: <single-float>)
 => (result :: <integer>, remainder :: <single-float>)
  let divided = real / divisor;
  let result = raw-as-integer
		 (primitive-single-float-as-integer
		    (primitive-single-float-as-raw(divided)));
  values(result, divided - as(<single-float>, result))
end method truncate/;


/// DEBUGGING

define inline function assert (#rest arguments)
end function;


/// THREADS

define class <lock> (<object>)
end class;

define constant <simple-lock>     = <lock>;
define constant <recursive-lock>  = <lock>;
define constant <timeout-expired> = <object>;
define constant <notification>    = <object>;

define variable *dylan-library-initialized?* = #t;

define function thread-name (thread) => (name)
end function;

define function make-foreign-thread () => (thread)
  #f
end;

define function make-simple-lock () => (lock)
  #f
end function;

define function make-notification (lock) => (notification)
  #f
end function;

define function sequence-point () 
end function;

define function synchronize-side-effects () 
end function;

define sealed method release (lock, #key) => ()
end method;

define sealed method wait-for (lock, #key timeout) => (success?)
end method;

define function handle1 (thread)
  #f
end function;

define function current-thread () 
  #f
end function;


/// DOMAIN

define method domain-type (x :: <partial-dispatch-cache-header-engine-node>, index :: <integer>) => (res)
  <object>
end method;

define method domain-number-required (d :: <partial-dispatch-cache-header-engine-node>)
 => (n :: <integer>)
  signature-number-required(function-signature(parent-gf(d)))
end method;

define constant $runtime-library :: <library>
  = make(<library>, name: "the runtime system");

define constant $runtime-module :: <module>
  = make(<module>, name: "phony module", home: $runtime-library);

kludge-up-init-value(<class>, class-module, $runtime-module);

define function %register-subclass-dependent-generic (dep, c)
end function;

define function %add-method-domain (g, m, lib, check-sealing?)
end function;

define method map-congruency-classes (f :: <function>, d :: <domain>) => ()
end method;

define function domain-conflict? (g, m, lib, check-sealing?, op)
end function;

define function domain-disjoint? (g, m, scu, gf)
end function;

define function domain-match? (d1, d2)
end function;

define function %remove-method-domain (g, m, lib)
end function;

define function %redefine-generic (g, name, module, sig, sealed?)
end function;

define function generic-function-incomplete-domains (g) 
  #()
end function;

define function generic-function-incomplete-methods (g) 
  #()
end function;

define function method-incomplete-domains (m) 
  #()
end function;


/// DOUBLE-INTEGER

define function %double-integer-low (object) => (res :: <machine-word>)
  as(<machine-word>, 0)
end function;

define function %double-integer-high (object) => (res :: <machine-word>)
  as(<machine-word>, 0)
end function;

define function decode-single-float (object) => (res :: <machine-word>)
  as(<machine-word>, 0)
end function;


/// RANGE

define function range (#key from: f, below: b)
  let z = make(<vector>, size: b - f + 1);
  for (i from 0, e from f below b)
    z[i] := e;
  end for;
  z
end function;


/// MULTIPLE-COLLECTION

define function multiple-collection (c, #rest mc)
  #()
end function;


/// ACCUMULATOR

define class <keyed-accumulator> (<mutable-collection>)
end class;

define constant <sequence-accumulator> = <keyed-accumulator>;

define function convert-accumulator-as (type, acc)
end function;


/// VECTOR

define constant <simple-element-type-vector>   = <simple-object-vector>;


//
// STRETCHY-VECTOR
//

define open abstract class <stretchy-sequence> (<stretchy-collection>, <sequence>)
end class <stretchy-sequence>;

define open abstract class <stretchy-mutable-sequence> 
    (<stretchy-sequence>, <mutable-sequence>)
end class <stretchy-mutable-sequence>;

define open abstract class <stretchy-vector> (<stretchy-sequence>, <vector>)
end class <stretchy-vector>;


/// DYLAN-SPY

define method spy-invoke-dylan-under-coded-restart
   (interactor-level :: <integer>, func :: <function>, #rest arguments)
       => (#rest r)
end method;
