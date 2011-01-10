module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-back-end
  use functional-dylan;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-c-ffi;
  use dfmc-back-end-protocol;
  export dfmc-back-end;
end library;

define module dfmc-back-end
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-conversion;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-c-ffi;
  use dfmc-back-end-protocol, export: all;

  export 

    *current-environment*,

    allocate-registers,
    
    back-end-word-size,
    back-end-lambda-size,
    back-end-record-repeated-object-sizes?,

    maybe-label!,
    label!,
    label, 
    label?,
    \with-labeling-from-dynamic,
    *init-labeling-state*, // !@#$ hygiene
    <labeling-state>,      // !@#$ hygiene

    <code-walker>,
    maybe-walk,
    walk,
    subsequent-walk,
    before-walk,
    do-walk,

    // !!! should be back-end accessors

    $singular-terminal-engine-node-prefix,

    local-mangle,
    hygienic-mangle,
    global-mangle,

    module-mangled-name,
    binding-mangled-name,

    <emitter>,
    emitter-stream,
    emitter-back-end,
    make-emitter,

    name-emitter,
    string-emitter,

    load-bound-object?,
    load-bound-reference?,
    <load-bound-reference>,
      load-bound-referenced-object,
      <load-bound-binding-reference>,
        load-bound-referencing-binding,
      <load-bound-code-reference>,
      <load-bound-instance-slot-reference>,
        load-bound-referencing-object,
        load-bound-referencing-slot,
      <load-bound-repeated-slot-reference>,
        load-bound-referencing-slot-index,

    <model-heap>,
    heap-compilation-record,
    heap-root-init-code,
    heap-root-system-init-code,
    heap-defined-bindings,
    heap-back-pointers,
    heap-referenced-bindings,
    heap-defined-objects,
    heap-defined-object-sequence,
    heap-defined-repeated-object-sizes,
    heap-referenced-repeated-object-sizes,
    heap-referenced-objects,
    heap-load-bound-references,
    heap-imported-object?,
    heap-imported-binding?,
    heap-symbols,
    precompute-library-heaps,
    compute-library-reachable-heap,
    compute-and-install-compilation-record-heap,
    emit-compilation-record-heap,
    *heap-record-back-pointers?*,
    all-heap-stats,
    heap-stats,
    single-heap-stats,
    diff-heap-stats,
    *retract-dfm?*,
	 
    library-imported-object?,
    library-imported-binding?,

    emit-object,
    emit-name,
    emit-name-internal,
    emit-reference,
    emit-indirect-reference,
    emit-all,

    initialize-back-end, \with-back-end-initialization,

    // HACK
    print-method-out,
    print-method,
    print-referenced-object;
end module;

