Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method open-emit-output
    (back-end :: <harp-back-end>, filename,
     #key assembler-output? = unsupplied(),
          harp-output? = unsupplied(),
          model-object-protocol? = #t,
          dynamic-linking-protocol? = #f,
          download? = #f,
     #all-keys)
 => (outputter :: <harp-outputter>);
  let outputters = make(<stretchy-object-vector>);

  if (supplied?(assembler-output?) & assembler-output?)
    add!(outputters,
         make-harp-outputter(current-back-end(), filename,
                             type: #"mnemonic-assembler"));
  end if;

  case
    download? =>
      add!(outputters,
           make-harp-outputter(back-end, filename,
                               print-harp?: harp-output?,
                               type: #"downloader"));
    current-os-name() == #"linux" | current-os-name() == #"freebsd" =>
      add!(outputters,
           make-harp-outputter(back-end, filename,
                               print-harp?: harp-output?,
                               type: #"elf-as-outputter"));
    otherwise =>
      add!(outputters,
           make-harp-outputter(back-end, filename, print-harp?: harp-output?));
  end case;

  let outputter
    = if (outputters.size = 1)
        outputters.first
      else
        apply(multiplex-outputters, outputters)
      end if;
  model-object-protocol? & model-object-protocol(outputter);
  dynamic-linking-protocol? & dynamic-linking-protocol(outputter);
  outputter
end method;

define method open-emit-output 
    (back-end :: <harp-back-end>, name == #f,
     #key model-object-protocol? = #t,
          dynamic-linking-protocol? = #f,
     #all-keys) => (outputter :: <harp-outputter>)
  let outputter = make-harp-outputter(back-end, name, type: #"interactive");
  model-object-protocol? & model-object-protocol(outputter);
  dynamic-linking-protocol? & dynamic-linking-protocol(outputter);
  outputter
end method;

define method close-emit-output
     (back-end :: <harp-back-end>, outputter, filename) => ()
  close-harp-outputter(back-end, outputter, filename: filename);
end method;

define sideways method initialize-back-end
    (back-end :: <harp-back-end>) => ()
  initialize-back-end-variables(back-end);
end method;


// C Compilers on some platforms prepend all names with an underscore;
// others (e.g. Linux) don't

define open generic c-name
    (back-end :: <harp-back-end>, name :: <string>) => (name :: <string>);
 
define sideways method c-name
    (back-end :: <harp-back-end>, name :: <string>) => (name :: <string>)
 concatenate("_", name)
end method c-name;

define open generic shared-library-entry-point-name
    (back-end :: <harp-back-end>, name :: <string>) => (name :: <string>);

define open generic shared-library-runtime-entry-point-name
    (back-end :: <harp-back-end>) => (name :: <string>);


define open generic emit-imported-name
    (back-end :: <harp-back-end>, stream, o) => (name :: <string>);


// Some platforms require an extra indirection for imports

define open generic make-imported-constant-reference
    (back-end :: <harp-back-end>, o,
     #key indirect?) => (name :: <constant-reference>);

define method make-imported-constant-reference
    (back-end :: <harp-back-end>, o,
     #key indirect?) => (name :: <constant-reference>)
  ins--indirect-constant-ref(back-end, o, import?: #t);
end method;


define thread variable *harp-outputter* = #f;

define thread variable *stream-outputters?* = #f;

define thread variable *emitting-data?* = #t;

define thread variable *emitting-init-code?* = #f;

define thread variable *loose-mode?* = #f;
define thread variable *interactive-mode?* = #f;

define thread variable *live-nlx-tags* = #();

define thread variable *tail-calls* = #();

define thread variable *compiling-dylan?* = #f;

define thread variable *current-heap* = #f;

define thread variable *current-compilation* = #f;

define variable *display-messages* = #f;

define method format-out?(#rest args) => ()
  if (*display-messages*)
    apply(format-out, args);
  end if;
end method format-out?;

define constant $dummy-name = outputter-name-unsupplied();

define constant $runtime-module-binding-type-marker = "T";

// Now that we use model-objects, define an object which directly refers
// to the runtime; for table-lookup efficiency, don't merely use strings 

define class <runtime-object>(<object>)
  constant slot runtime-object-name :: <string>, init-keyword: name:;
end class;

define class <dood-runtime-object>(<runtime-object>)
end class;

define class <local-runtime-object>(<dood-runtime-object>)
end class;

define class <c-runtime-object>(<dood-runtime-object>)
end class;


define constant $runtime-objects :: <string-table> =
  make(<string-table>);

// PROXY for <dood-runtime-object>

define class <dood-runtime-object-proxy> (<dood-proxy>)
  constant slot dood-proxy-runtime-object :: <string>,
    required-init-keyword: object:;
end class;

define sealed domain make (subclass(<dood-runtime-object-proxy>));
define sealed domain initialize (<dood-runtime-object-proxy>);

define method dood-make-runtime-object-proxy
    (dood :: <dood>, object :: <dood-runtime-object>) => (proxy :: <dood-proxy>)
  make(<dood-runtime-object-proxy>, object: object.runtime-object-name)
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <dood-runtime-object>)
 => (proxy :: <dood-runtime-object-proxy>)
  dood-as-proxy(dood, object, dood-make-runtime-object-proxy)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-runtime-object-proxy>)
 => (object :: <dood-runtime-object>)
  $runtime-objects[proxy.dood-proxy-runtime-object]
end method;


define class <dylan-object>(<runtime-object>)
  slot dylan-model-object, init-keyword: object:;
end class;

define constant make-runtime-object =
  method (reference :: <string>) => (runtime-object :: <runtime-object>)
    make(<runtime-object>, name: reference)
  end method;

define constant make-runtime-reference =
  method (reference :: <string>) => (runtime-reference :: <constant-reference>)
    let runtime-object :: <dood-runtime-object> =  make(<dood-runtime-object>, name: reference);
    $runtime-objects[reference] := runtime-object;
    ins--constant-ref(current-back-end() | $dummy-harp-back-end, runtime-object);
  end method;

define constant make-local-runtime-reference =
  method (reference :: <string>) => (runtime-reference :: <constant-reference>)
    let runtime-object :: <dood-runtime-object> =  make(<local-runtime-object>, name: reference);
    $runtime-objects[reference] := runtime-object;
    ins--constant-ref(current-back-end() | $dummy-harp-back-end, runtime-object);
  end method;

define constant make-c-runtime-reference =
  method (reference :: <string>) => (runtime-reference :: <constant-reference>)
    let runtime-object :: <dood-runtime-object> =  make(<c-runtime-object>, name: reference);
    $runtime-objects[reference] := runtime-object;
    ins--constant-ref(current-back-end() | $dummy-harp-back-end, runtime-object);
  end method;


// Make runtime-models for all the entry-points

define constant $number-xeps = 10;
define constant $number-meps = $number-xeps;

define constant $number-gf-xeps = 7;
define constant $number-discriminators = 7;
define constant $number-keyeds = 3;


define entry-points xep xep;
define entry-points xep rest-xep;
define entry-points xep rest-key-xep;

define code-entry-points xep apply-xep;

define entry-points mep rest-key-mep;

define entry-points gf-xep new-gf-xep;
define entry-points gf-xep new-gf-optional-xep;


define constant $named-entry-points = make(<string-table>);

define named-entry-point slotacc-repeated-instance-getter-xep;
define named-entry-point slotacc-repeated-instance-setter-xep;
define named-entry-point slotacc-single-q-class-getter-xep;
define named-entry-point slotacc-single-q-instance-getter-xep;
define named-entry-point slotacc-single-q-class-setter-xep;
define named-entry-point slotacc-single-q-instance-setter-xep;

define named-entry-point general-engine-node-1-entry;
define named-entry-point general-engine-node-2-entry;
define named-entry-point general-engine-node-3-entry;
define named-entry-point general-engine-node-n-entry;
define named-entry-point general-engine-node-spread-entry;

define named-entry-points keyed single-method-entry;
define named-entry-points keyed implicit-keyed-single-method-entry;
define named-entry-points keyed explicit-keyed-single-method-entry;
define named-entry-points keyed unrestricted-keyed-single-method-entry;

define named-entry-point cache-header-entry;
define named-entry-point profiling-cache-header-entry;
define named-entry-point ambiguous-methods;

define named-entry-point boxed-instance-slot-getter;
define named-entry-point boxed-instance-slot-setter;
define named-entry-point boxed-repeated-instance-slot-getter;
define named-entry-point boxed-repeated-instance-slot-setter;

define named-entry-point raw-byte-repeated-instance-slot-getter;
define named-entry-point raw-byte-repeated-instance-slot-setter;

define named-entry-points discriminator typecheck-discriminator-engine;
define named-entry-points discriminator if-type-discriminator-engine;
define named-entry-points discriminator monomorphic-by-class-discriminator-engine;
define named-entry-points discriminator discriminate-on-argument-entry;

define named-entry-point-in dispatch-engine dylan %gf-dispatch-absent;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-inapplicable;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-ambiguous-methods;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-boxed-class-slot-getter;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-boxed-class-slot-setter;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-typecheck;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-if-type;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-linear-by-class;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-hashed-by-class;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-linear-by-singleton-class;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-hashed-by-singleton-class;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-immediate-linear-singleton;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-immediate-hashed-noreloc-singleton;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-immediate-hashed-singleton;
define named-entry-point-in dispatch-engine dylan %gf-dispatch-slow-linear-singleton;


// Define some Dylan constants


define thread variable $false = #f;
define thread variable $true = #f;

define thread variable $current-handlers = #f;

define thread variable $dylan-integer = #f;
define thread variable $dylan-byte-character = #f;
define thread variable $dylan-unicode-character = #f;


define variable $dylan-resolve-symbol-iep = make(<dylan-object>);

define variable $dylan-unbound = make(<dylan-object>, object: $unbound);

define variable $dylan-unbound-instance-slot-iep = make(<dylan-object>);

define variable $dylan-type-check-error = make(<dylan-object>);


// Define unique canonical-model-objects for these two

define constant $empty-vector = #[];
define constant $empty-string = "";

define constant $symbol-fixup-name = #"%resolve-symbol";


// Make runtime-models for all directly referenced primitives


define runtime-reference primitive-make-closure;

define runtime-reference primitive-make-keyword-closure;

define runtime-reference primitive-initialize-closure;

define runtime-reference primitive-initialize-keyword-closure;

define runtime-reference primitive-make-closure-with-environment;

define runtime-reference primitive-make-keyword-closure-with-environment;



define runtime-reference primitive-make-closure-with-signature;

define runtime-reference primitive-make-keyword-closure-with-signature;

define runtime-reference primitive-make-closure-with-environment-signature;

define runtime-reference primitive-make-keyword-closure-with-environment-signature;

define runtime-reference primitive-make-method-with-signature;

define runtime-reference primitive-make-keyword-method-with-signature;


define runtime-reference primitive-apply;

define runtime-reference primitive-mep-apply;

define runtime-reference primitive-mep-apply-with-optionals;


define runtime-reference primitive-nlx;

define runtime-reference primitive-build-bind-exit-frame;

define runtime-reference primitive-build-unwind-protect-frame;

define runtime-reference primitive-unwind-protect-cleanup;


define runtime-reference primitive-make-box;

define runtime-reference primitive-make-raw-box;

define runtime-reference primitive-make-single-float-box;

define runtime-reference primitive-make-double-float-box;


define runtime-reference primitive-type-check;

// define runtime-reference primitive-type-check-values;

define runtime-reference primitive-type-check-rest-values;


define runtime-reference primitive-heap-vector-remaining-values;

define runtime-reference primitive-set-mv-from-vector;

// define runtime-reference primitive-adjust-mv;

// define runtime-reference primitive-adjust-mv-rest;

define runtime-reference primitive-pad-mv;


define runtime-reference primitive-vector;


define runtime-reference primitive-debug-message;

define local-runtime-reference primitive-runtime-module-handle;


define c-runtime-reference dylan-call-in;

define c-runtime-reference dylan-call-in-syscall;


define c-runtime-reference primitive-alloc;

define c-runtime-reference primitive-alloc-s1;
define c-runtime-reference primitive-alloc-s2;
define c-runtime-reference primitive-alloc-s;

// define c-runtime-reference primitive-alloc-s-r;
define c-runtime-reference primitive-alloc-s-rbf;
// define c-runtime-reference primitive-alloc-rbfz;
define c-runtime-reference primitive-alloc-s-rbfz;

define c-runtime-reference primitive-alloc-rf;
define c-runtime-reference primitive-alloc-leaf-rf;
define c-runtime-reference primitive-alloc-leaf-rhf;
define c-runtime-reference primitive-alloc-leaf-rdwf;
define c-runtime-reference primitive-alloc-leaf-rsff;
define c-runtime-reference primitive-alloc-leaf-rdff;

define c-runtime-reference primitive-alloc-s-rf;
define c-runtime-reference primitive-alloc-s-rhf;
define c-runtime-reference primitive-alloc-s-rdwf;
define c-runtime-reference primitive-alloc-s-rsff;
define c-runtime-reference primitive-alloc-s-rdff;


define c-runtime-reference primitive-alloc-leaf-s;
// define c-runtime-reference primitive-alloc-leaf-s-rb;
define c-runtime-reference primitive-alloc-leaf-s-rbf;
define c-runtime-reference primitive-alloc-leaf-s-rbfz;
define c-runtime-reference primitive-alloc-leaf-rbfz;


define c-runtime-reference primitive-alloc-exact-awl-s-r;
define c-runtime-reference primitive-alloc-exact-awl-rf;
define c-runtime-reference primitive-alloc-weak-awl-s-r;
define c-runtime-reference primitive-alloc-weak-awl-rf;

define c-runtime-reference mps--malloc;

define dylan-reference $direct-object-classes internal dylan;

ignore($$direct-object-classes);

define sideways method back-end-word-size 
    (back-end :: <harp-back-end>) => (number-bytes :: <integer>)
  bytes%(back-end, 1);
end method back-end-word-size;
