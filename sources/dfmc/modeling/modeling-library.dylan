Module:   dylan-user
Synopsis: Library and module definitions for object model.
Author:   Keith Playford, Jonathan Bachrach, and Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-modeling
  use dylan;
  use generic-arithmetic;
  use big-integers;
  use dfmc-common;
  use dfmc-namespace;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-definitions;
  export dfmc-modeling;
end library dfmc-modeling;

define macro module-with-models-definer
  { define module-with-models ?:name ?specs end }
    => { define module ?name ?specs end }

 specs:
  { }                        => { }
  { export ?bindings; ... } => { create ?bindings; ... }
  { create ?bindings; ... } => { create ?bindings; ... }
  { use ?clause:*; ... }     => { use ?clause; ... }

 bindings:
  { }               => { }
  { ?binding, ... } => { ?binding, ... }

 binding:
  { slot ?:name }  => { ?name, ?name ## "-setter" }
  { &slot ?:name } => { // "&" ## ?name, "&" ## ?name ## "-setter",
                        "^" ## ?name, "^" ## ?name ## "-setter" }
  { &getter ?:name } => { // "&" ## ?name
                        "^" ## ?name }
  { &setter ?:name } => { // "&" ## ?name ## "-setter",
                         "^" ## ?name ## "-setter" }
  { &name ?:name } => { // "&" ## ?name,
                        "^" ## ?name }
  { slot-offset ?:name }  => { ?name ## "-runtime-slot-offset" }
  { ?:name }       => { ?name }

end macro module-with-models-definer;

define module make/initialize-aliases
  use dylan,
    rename: { make => ^make,
              initialize => ^initialize },
    export: all;
end module;

define module-with-models dfmc-modeling
  use dylan;
  use dylan-extensions,
    import: { $machine-word-size, immutable-vector };
  use machine-word-lowlevel,
    import: { coerce-integer-to-machine-word,
              machine-word-logior,
              machine-word-unsigned-shift-left };
  use generic-arithmetic,
    prefix: "generic/";
  use dfmc-common;
  use dfmc-imports;
  use dfmc-namespace;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-definitions;
  use make/initialize-aliases,
    export: { ^make,
              ^initialize };

  export
    <model-value>, ^id?,
    <virtual-object>,
    <heap-deferred-model>,
    // *** slot value,
    // *** define!,
//    slot %element,
    // *** compile-stage, run-stage,
    make-compile-time-literal,
    &object-class, ^object-class, // &object-class should go away
    install-&class-mapping,
    binding-name,
    // ^make,
    // ^initialize,
    ^initialize-class,
    ^initialize-slot-descriptor,
    ^slot-descriptor,
    ^slot-offset,
    ^slot-value, ^repeated-slot-value,
    ^slot-value-setter, ^repeated-slot-value-setter;
  export
    direct-object?; // direct-object-value;

  export
//    <&values>,
    <&static-values>,
//      &values,
//      &values-element,
      &values-model-objects;
//    <&dynamic-values>;

  export
    $dylan-system-subtype-bit-type-names;

  export
    <&top>,
    <&object>,
//    <&value-object>,
    <&boolean>,
      &true, &false,
//    <&unbound>,
      &unbound,
//    <&character>, <&byte-character>,
//    <&number>,
//    <&complex>,
//    <&real>,
//    <&rational>,
//    <&abstract-integer>, <&integer>,
//    <&float>,
    <&single-float>,
      ^%single-float-data,
    <&double-float>,
      ^%double-float-data;

  export
    <&namespace>,
//      &slot namespace-name,
//    <&library-version>,
      &setter library-major-version,
      &setter library-minor-version,
      &setter library-build-count,
    <&library>,
      // &slot used-libraries,      // defined in dfmc-namespace
      // slot ^library-description, // defined in dfmc-namespace
      &slot library-defined-generics,
      &slot library-number-static-dispatches,
      &slot library-number-dynamic-dispatches,
      slot  library-accumulating-defined-generics,
    <&used-library>,
      &getter used-library,
//      &slot used-library-binding,
    <&module>,
      ^home-library,
    form-module-model,
    model-module-model;

  export
    <&machine-word>;
//      &slot %machine-word-data;

    // functions and other code objects

  export
    <&signature>, <&keyword-signature>,
//      ^signature-default-types,
      as-sig-types,
      &slot signature-required,
      ^signature-rest?,
      ^signature-key?,
      &setter signature-next?,
      &getter signature-keys,
      &getter signature-key-types,
      ^signature-all-keys?,
      &getter signature-sealed-domain?,
      &getter signature-values,
      &getter signature-rest-value,
      &getter signature-properties,
      ^signature-optionals?,
      ^signature-number-required, ^signature-number-required-setter,
      ^signature-number-values,
      ^pack-signature-properties,
      ^signature-number-keys,

    <&domain>,
      ^domain-library,
      &slot domain-next,
      ^domain-types,
      ^domain-type,
      ^domain-number-required,
    <&method-domain>,
//      &slot domain-method,
    <&standalone-domain>,
    <&callable-object>,
    <&code>,
      slot function,
    <&iep>,
      code, code-setter,
    <&runtime-object>,
    <&shared-entry-point>,
      &name entry-point-key?,
      &name entry-point-rest?,
      &name entry-point-optionals?,
      &name entry-point-number-required,
      &name entry-point-number-keys,
      &name entry-point-name,
    <&xep>,
      <&generic-function-xep>,
      <&lambda-xep>,
      <&slot-accessor-xep>,
    <&mep>,
      <&keyword-method-mep>,
    <&any-kernel-ep>,
    <&kernel-ep>,
    <&deferred-iep>,
    <&function>,
      &slot debug-name,
      &slot function-signature,
      &slot xep,
      &slot iep,
      slot signature-spec, // private-signature-spec, // HACK: FOR DFM-COPIER
      ^function-specializers,
    <&lambda-or-code>,
    <&lambda>,
      slot lambda-body,
      slot body-spec,
//      slot data,
      iep,
      slot xep,
      slot keyword-specifiers,
      slot parameters-dynamic-extent,
      slot lambda-top-level?,
      slot lambda-heap, private-lambda-heap,           // HACK: FOR DFM-COPIER
      slot environment,
      slot parameters,
      slot body,
      slot optimization-queue,
      slot lambda-optimized?,
      slot lambda-initializer?,
      slot lambda-has-free-references?,
      slot lambda-log-number-temporaries,
      slot lambda-copied-down?,
      slot lambda-runtime-function?,
    <&method>,
      &slot mep,
      &slot next?,
      &slot function-next?,
      lambda-next?-setter,
      slot lambda-rest?,
      slot lambda-inlineable?,
//      mep,
//    <&incremental-method-mixin>,
//      &slot %incr-method-library,
    <&keyword-method>,
//    <&incremental-keyword-method>,
    <&closure-method-mixin>,
//    <&closure-method>,
    <&keyword-closure-method>,
//    <&incremental-closure-method>,
//    <&incremental-keyword-closure-method>,
    <&initializer-method>,
    <&slot-initializer-method>,
    <&copy-down-method>,
    <&accessor-method>,
      &slot method-slot-descriptor,
//    <&getter-accessor-method>,
    <&setter-accessor-method>,
//    <&single-accessor-method>,
//    <&repeated-accessor-method>,
    <&getter-method>,
    <&setter-method>,
    <&repeated-getter-method>,
    <&repeated-setter-method>,
//    <&incremental-getter-method>,
//    <&incremental-setter-method>,
//    <&incremental-repeated-getter-method>,
//    <&incremental-repeated-setter-method>,
    <&generic-function>,
      &slot %gf-cache,
      ^generic-function-methods,
//      &slot discriminator,
      slot  %generic-function-methods,
      slot  %generic-function-methods-initialized?,
      slot  %generic-function-domains,
      slot  %generic-function-domains-initialized?,
      slot  parameters-dynamic-extent,
      ^generic-function-sealed?,
//      slot ^generic-function-compiler-open?,
      slot ^generic-function-cache-info,
      ^generic-function-domains,
    <&sealed-generic-function>,
    <&incremental-generic-function>,
//      &slot incremental-gf-module,
      &setter incremental-gf-domain-info;
//      &slot incremental-gf-method-libraries,
//      &slot incremental-gf-sealed?,
//      &slot incremental-gf-method-complete?,
//      &slot incremental-gf-signatured?;
      // $number-gf-discriminators;

    // types

  export
    type-checked-at-run-time?;

  export
//    <&obsolete-instance>,
//    <&miscellaneous-obsolete-instance>,
    <&type>,
//      &slot instance?-iep,
//      slot %instance?-iep,
      ^instance?-function,
      ^instance?,
      ^subtype?,
//      ^instantiable?,
//      ^base-type,
      ^type-equivalent?,
      ^known-disjoint?,
//      ^known-pseudosubtype?,
//      ^pseudosubtype?,
    <&class>,
      &slot class-implementation-class,
      &slot class-subtype-bit,
      &slot debug-name,
    <&implementation-class>,
      &slot class-properties,
      &getter iclass-class,
      &slot class-mm-wrapper,
      &slot direct-superclasses,
      &slot all-superclasses,
      &slot direct-subclasses,
        // ^all-direct-subclasses-known?,
        // ^direct-subclasses-known,
        ^all-subclasses-if-sealed,
        ^sealed-with-no-subclasses?,
      &slot direct-slot-descriptors,
      &slot slot-descriptors,
      &slot direct-inherited-slot-descriptors,
      &slot direct-initialization-argument-descriptors,
//      &slot direct-methods,
      ^class-abstract?,
      ^class-primary?,
      ^class-sealed?,
      &slot instance-storage-size,
      &slot repeated-slot-descriptor,
      &slot instance-slot-descriptors,
      &slot class-slot-descriptors,
      &slot defaulted-initialization-arguments-slot,
      &setter class-slot-storage,
      &slot class-constructor,
      &setter iclass-dispatch-key,
      &setter class-rcpl-vector,
      &slot class-rcpl-position,
      &setter class-rcpl-other-positions,
      &slot iclass-dependent-generics,
      &slot iclass-subclass-dependent-generics,
      // &slot class-Caseau-gene-set,
      // &slot class-Caseau-gene,
      &setter class-module,
      &setter class-known-joint,
      slot ^class-complete?,
      slot ^iclass-type-complete?,
      slot ^iclass-instantiable?,
      slot ^iclass-subclasses-fixed?,
      &slot class-incremental?,
      slot ^slots-initialized-state,
      &slot slots-have-fixed-offsets?-bit,
      &slot slots-have-fixed-offsets?-computed?,
      slot ^class-incremental-rcpl-positions,
      ^least-primary-superclass,
      ^iclass-number-to-key,
    <&value-class>,
      &slot value-class-comparitor,
    <&function-class>,
    <&virtual-class>,
    <&slot-initial-value-descriptor>,
      &getter init-supplied?,
      &slot init-value?,
      &slot init-evaluated?,
      &slot init-data-slot,
    <&slot-keyword-initialization-descriptor>,
      ^init-keyword,
      ^init-keyword-required?,
    <&slot-descriptor>,
      &name slot-getter,
      &name slot-setter,
      &slot slot-type,
      &name slot-owner,
//      &slot slot-storage-size,
      slot emitted-type-name,
      slot model-object-getter,
      slot model-object-setter,
      compute-compile-stage-getter,
      compute-compile-stage-setter,
    <&any-instance-slot-descriptor>,
    <&instance-slot-descriptor>,
    <&virtual-slot-descriptor>,
    <&repeated-slot-descriptor>,
      &slot size-slot-descriptor,
//    <&any-class-slot-descriptor>,
    <&class-slot-descriptor>,
    <&each-subclass-slot-descriptor>,
    <&init-arg-descriptor>,
//      &slot init-arg-type,
    <&inherited-slot-descriptor>,
      ^inherited-slot-getter,
    <&mm-wrapper>,
      &getter mm-wrapper-implementation-class,
//      &slot mm-wrapper-subtype-mask,
//      &slot mm-wrapper-fixed-part,
//      &slot mm-wrapper-variable-part,
//      &slot mm-wrapper-pattern-element,
//      ^mm-wrapper-number-patterns,
    model-<mm-wrapper>,
//    <&limited-type>,
//      ^limited,
    <&limited-collection-type>,
      &getter limited-collection-class,
      &getter limited-collection-concrete-class,
      &getter limited-collection-element-type,
      &getter limited-collection-size,
      &getter limited-collection-dimensions,
      lookup-any-limited-collection-element-type,
    <&limited-integer>,
      ^limited-integer,
      &getter limited-integer-min,
      &getter limited-integer-max,
    <&singleton>,
      &getter singleton-object,
      ^singleton,
    <&union>,
      ^union-type1,
      ^union-type2,
      ^type-union,
    <&subclass>,
      &getter subclass-class, ^subclass;

  // runtime slot-offsets
  export
    slot-offset generic-function-methods,
    slot-offset mep,
    slot-offset iep,
    slot-offset class-mm-wrapper,
    slot-offset class-constructor,
    slot-offset class-implementation-class;

  export
//    engine-node$k-absent,
//    engine-node$k-inapplicable,
//    engine-node$k-unkeyed-single-method,
//    engine-node$k-implicit-keyed-single-method,
//    engine-node$k-explicit-keyed-single-method,
//    engine-node$k-unrestricted-keyed-single-method,
//    engine-node$k-reserved-terminal-n-a,
//    engine-node$k-reserved-terminal-n-b,
//    engine-node$k-reserved-terminal-n-c,
//    engine-node$k-reserved-terminal-n-d,
//    engine-node$k-reserved-terminal-n-e,
//    engine-node$k-reserved-terminal-n-f,
//    engine-node$k-reserved-terminal-n-g,
//    engine-node$k-cache-header,
//    engine-node$k-profiling-cache-header,
//    engine-node$k-ambiguous-methods,
//    engine-node$k-first-slot-engine-node,
//    engine-node$k-boxed-instance-slot-getter,
//    engine-node$k-boxed-instance-slot-setter,
//    engine-node$k-boxed-repeated-instance-slot-getter,
//    engine-node$k-boxed-repeated-instance-slot-setter,
//    engine-node$k-boxed-class-slot-getter,
//    engine-node$k-boxed-class-slot-setter,
//    engine-node$k-raw-byte-repeated-instance-slot-getter,
//    engine-node$k-raw-byte-repeated-instance-slot-setter,
//    engine-node$k-reserved-slot-a-getter,
//    engine-node$k-reserved-slot-a-setter,
//    engine-node$k-reserved-slot-b-getter,
//    engine-node$k-reserved-slot-b-setter,
    engine-node$k-reserved-repeated-slot-a-getter,
//    engine-node$k-reserved-repeated-slot-a-setter,
//    engine-node$k-reserved-repeated-slot-b-getter,
    engine-node$k-reserved-repeated-slot-b-setter,
//    engine-node$k-slot-engine-node-count,
//    engine-node$k-typecheck,
//    engine-node$k-if-type,
//    engine-node$k-linear-by-class,
//    engine-node$k-hashed-by-class,
//    engine-node$k-linear-by-singleton-class,
//    engine-node$k-hashed-by-singleton-class,
//    engine-node$k-immediate-linear-singleton,
//    engine-node$k-immediate-hashed-noreloc-singleton,
//    engine-node$k-immediate-hashed-singleton,
//    engine-node$k-value-object-linear-singleton,
//    engine-node$k-monomorphic-by-class,
//    engine-node$k-reserved-discriminator-a,
//    engine-node$k-reserved-discriminator-b,
//    engine-node$k-reserved-discriminator-c,
//    engine-node$k-reserved-discriminator-d,
//    engine-node$k-reserved-discriminator-e,
//    engine-node$k-reserved-discriminator-f,
//    engine-node$k-reserved-discriminator-g,
//    engine-node$k-reserved-discriminator-h,
//    engine-node$k-reserved-discriminator-i,
//    engine-node$k-reserved-discriminator-j,
//    engine-node$k-reserved-discriminator-k,
//    engine-node$k-reserved-discriminator-l,
//    engine-node$k-reserved-discriminator-m,
//    engine-node$k-reserved-discriminator-n,
//    engine-node$k-reserved-discriminator-o,
//    engine-node$k-reserved-discriminator-p,
//    engine-node$k-reserved-discriminator-q,
//    engine-node$k-reserved-discriminator-r,
//    engine-node$k-reserved-discriminator-s,
//    engine-node$k-reserved-discriminator-t,
//    engine-node$k-reserved-discriminator-u,
//    properties$m-entry-type,
//    properties$s-entry-type,
//    properties$v-entry-type,
//    properties$v-data,
//    engine-node$v-data-start,
//    smen$v-nrequired,
//    smen$s-nrequired,
//    smen$m-nrequired,
//    smen$v-restp,
//    smen$m-restp,
//    smen$v-data-start,

    $simple-typechecked-cache-arguments-limit,
    stchen$v-checkedmask,
//    stchen$s-checkedmask,
    stchen$m-checkedmask,
    $partial-dispatch-arguments-limit,
    pdisp$v-typemask,
    pdisp$s-typemask,
    pdisp$m-typemask;

//    discriminator$v-argnum,
//    discriminator$s-argnum,
//    discriminator$m-argnum,
//    discriminator$v-nrequired,
//    discriminator$s-nrequired,
//    discriminator$m-nrequired,
//    discriminator$v-restp,
//    discriminator$m-restp,
//    discriminator$v-data-start;

//  export
//    $engine-node-entry-point-names,
//    $engine-node-callback-names;
  export
    <&engine-node-ep>,
      ^engine-node,
      ^entry-point-name,
      ^engine-node-ep-optionals?,
      ^engine-node-ep-number-required,
    <&discriminator-ep>,
    <&function-linked-engine-node-ep>,
    <&rogue-engine-node-ep>,
//    <&properties-provider>,
      ^properties,
    <&engine-node>,
      ^engine-node-callback,
//      &slot engine-node-data-1,
//      &slot engine-node-data-2,
//      &slot engine-node-data-3,
        ^engine-node-entry-point,
//      ^entry-type-number,
//    <&dispatch-engine-invocable>,
//    <&terminal-engine-node>,
    <&singular-terminal-engine-node>,
//    <&uniquified-terminal-engine-node>,
//    <&unshared-terminal-engine-node>,
    <&cache-header-engine-node>,
      &slot cache-header-engine-node-next,
      &slot cache-header-engine-node-parent,
    <&simple-typechecked-cache-header-engine-node>,
      ^stchen-checkedmask,
    <&partial-dispatch-cache-header-engine-node>,
      &slot partial-dispatch-type,
      ^pdisp-type-mask,
    <&simple-call-site-cache-header-engine-node>,
    <&profiling-call-site-cache-header-engine-node>,
      &slot profiling-call-site-cache-header-engine-node-count-1,
      &slot profiling-call-site-cache-header-engine-node-count-2,
      &slot profiling-call-site-cache-header-engine-node-id,
      &slot profiling-call-site-cache-header-engine-node-library,
       slot profiling-call-site-cache-header-engine-node-call,
    <&discriminator>,
      ^discriminator-argnum,
      ^discriminator-optionals?,
      ^discriminator-nrequired,
    <&absent-engine-node>,
//    <&inapplicable-engine-node>,
//    <&ambiguous-methods-engine-node>,
//    <&single-method-engine-node>,
//      ^smen-nrequired,
//      ^smen-optionals?,
//    <&unkeyed-single-method-engine-node>,
    <&keyed-single-method-engine-node>;
//    <&explicit-keyed-single-method-engine-node>,
//    <&implicit-keyed-single-method-engine-node>,
//    <&unrestricted-keyed-single-method-engine-node>,
//    <&class-keyed-discriminator>,
//    <&linear-class-keyed-discriminator>,
//      &slot lckd-index,
//    <&hashed-class-keyed-discriminator>,
//    <&by-class-discriminator>,
//    <&by-singleton-class-discriminator>,
//    <&linear-by-class-discriminator>,
//    <&hashed-by-class-discriminator>,
//    <&linear-by-singleton-class-discriminator>,
//    <&hashed-by-singleton-class-discriminator>,
//    <&typecheck-discriminator>,
//    <&if-type-discriminator>,
//    <&monomorphic-by-class-discriminator>,
//    <&singleton-discriminator>,
//    <&linear-singleton-discriminator>,
//      &slot lsd-index,
//    <&immediate-linear-singleton-discriminator>,
//    <&value-object-linear-singleton-discriminator>,
//    <&slot-access-engine-node>,
//    <&slot-setter-engine-node>,
//    <&slot-getter-engine-node>,
//    <&single-slot-access-engine-node>,
//    <&repeated-slot-access-engine-node>,
//    <&instance-slot-engine-node>,
//    <&class-slot-engine-node>,
//    <&boxed-instance-slot-engine-node>,
//    <&byte-slot-engine-node>,
//    <&boxed-instance-slot-getter-engine-node>,
//    <&boxed-instance-slot-setter-engine-node>,
//    <&boxed-repeated-instance-slot-getter-engine-node>,
//    <&boxed-repeated-instance-slot-setter-engine-node>,
//    <&byte-slot-getter-engine-node>,
//    <&byte-slot-setter-engine-node>,
//    <&repeated-byte-slot-getter-engine-node>,
//    <&repeated-byte-slot-setter-engine-node>,
//    <&boxed-class-slot-getter-engine-node>,
//    <&boxed-class-slot-setter-engine-node>;

//  export
//    <&gf-cache-info>,
//      &slot gf-cache-info-users,
//    <&simple-typechecked-gf-cache-info>,
//      &slot simple-typechecked-gf-cache-info-entries,
//      &slot simple-typechecked-gf-cache-info-argmask,
//    <&partial-dispatch-gf-cache-info>,
//      &slot partial-dispatch-gf-cache-info-caches;

  export
    method-number, domain-number;

  export
    inlineable?;

 // raw-types

  export
    <&top-type>,
    <&bottom-type>,
    <&raw-object>,
      ^raw-object-value,
    <&raw-type>, raw-type?,
      raw-type-size, raw-type-alignment,
      raw-type-getter-name, raw-type-setter-name,
      raw-type-boxed-class-name, raw-type-boxer-name, raw-type-unboxer-name,
      raw-type-getter, raw-type-setter,
      raw-type-boxed-class, raw-type-boxer, raw-type-unboxer,
      raw-type-c-name,
      /*
      raw-signed-8-bit-integer, <&raw-signed-8-bit-integer>,
      raw-unsigned-8-bit-integer, <&raw-unsigned-8-bit-integer>,
      raw-signed-16-bit-integer, <&raw-signed-16-bit-integer>,
      raw-unsigned-16-bit-integer, <&raw-unsigned-16-bit-integer>,
      raw-signed-32-bit-integer, <&raw-signed-32-bit-integer>,
      raw-unsigned-32-bit-integer, <&raw-unsigned-32-bit-integer>,
      raw-signed-64-bit-integer, <&raw-signed-64-bit-integer>,
      raw-unsigned-64-bit-integer, <&raw-unsigned-64-bit-integer>,
      raw-ieee-single-float, <&raw-ieee-single-float>,
      // raw-ieee-double-float, <&raw-ieee-double-float>,
      // raw-ieee-extended-float, <&raw-ieee-extended-float>,
      */
      raw-c-signed-char, // <&raw-c-signed-char>,
      raw-c-unsigned-char, // <&raw-c-unsigned-char>,
      raw-c-signed-short, // <&raw-c-signed-short>,
      raw-c-unsigned-short, // <&raw-c-unsigned-short>,
      raw-c-signed-int, // <&raw-c-signed-int>,
      raw-c-unsigned-int, // <&raw-c-unsigned-int>,
      raw-c-signed-long, // <&raw-c-signed-long>,
      raw-c-unsigned-long, // <&raw-c-unsigned-long>,
      raw-c-signed-long-long, // <&raw-c-signed-long-long>,
      raw-c-unsigned-long-long, // <&raw-c-unsigned-long-long>,
      raw-c-float, // <&raw-c-float>,
      raw-c-double, // <&raw-c-double>,
      raw-c-long-double, // <&raw-c-long-double>,
      raw-c-void, // <&raw-c-void>,
      raw-c-pointer, // <&raw-c-pointer>,
      raw-c-size-t, raw-c-ssize-t,
      raw-boolean, <&raw-boolean>,
      raw-byte-character, <&raw-byte-character>, // TODO: OBSOLETE
      raw-unicode-character, // <&raw-unicode-character>, // TODO: OBSOLETE ???
      raw-byte, // <&raw-byte>,
      raw-double-byte, // <&raw-double-byte>,
      raw-byte-string, <&raw-byte-string>,
      raw-integer, // <&raw-integer>,
      raw-single-float, <&raw-single-float>,
      raw-machine-word, <&raw-machine-word>,
      raw-double-float, <&raw-double-float>,
      raw-extended-float, // <&raw-extended-float>,
      raw-pointer, <&raw-pointer>,
      raw-address, // <&raw-address>,
      make-raw-literal;

  export
    repeated-representation-byte?,
    repeated-representation-size,
    raw-repeated-representation?,
    repeated-representation,
    cell-representation;

  // raw struct/union types

  export
    <&raw-aggregate-type>,
    <&raw-struct-type>,
    <&raw-union-type>,
      raw-aggregate-members,
      raw-aggregate-options,
    <raw-aggregate-member>,
    <raw-aggregate-ordinary-member>,
    <raw-aggregate-array-member>,
    <raw-struct-bitfield-member>,
      member-name,
      member-raw-type,
      member-bitfield-width,
      member-array-length,
    compute-raw-aggregate-layout,
    compute-raw-aggregate-member;


  // primitives

  export
    <&primitive>,
      primitive-value,
      primitive-signature,
      primitive-descriptor-getter-name,
      primitive-side-effecting?,
      primitive-dynamic-extent?,
      primitive-stateless?,
    <primitive-descriptor>,
      primitive-emitter,
//      &eval-in-dylan,
    &primitive-definer;

  // c-ffi

  export
    <&c-function>,
    <&c-callable-function>,
      c-modifiers,
      c-signature,
      c-signature-setter,
      alternate-name,
    <&c-variable>,
      dll-import?,
      dll-export?,
    <&objc-msgsend>;

  // collections

  export
//    <&object-with-elements>,
    <&mutable-object-with-elements>,
//    <&collection>,
    <&limited-collection>,
      &getter element-type,
//    <&sequence>,
//    <&mutable-collection>,
//    <&mutable-sequence>,
//    <&array>,
//    <&vector>, <&simple-object-vector>,
//      &name size,
      &slot vector-element,
//      &empty-simple-object-vector,
//    <&list>,
//      &slot head,
//      &slot tail,
//    <&empty-list>,
//    <&pair>,
//    <&string>, <&byte-string>,
//      &slot string-element,
      frame-size,
//      &empty-byte-string,
//    <&symbol>,
//      &slot symbol-name,
      ^symbol?,
    <uninterned-symbol>,
      symbol-name,
      deep-copy-symbol,
//    <&converter>,
//      expander-function,
    <&macro>,
      expander-macro-object,
    <unknown>;

  export
    <&expander>;

  export
    // make define &class available
    &class-definer,
    // all the rest are due to hygeine failure
    compiler-class-definer,
    model-class-definer,
    compiler-class-accessors-definer;

end module-with-models dfmc-modeling;
