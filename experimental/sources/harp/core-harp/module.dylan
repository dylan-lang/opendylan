module:    dylan-user
Synopsis:  The module definitions for the HARP library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module common-harp-imports
  use functional-extensions,
    rename: { <stretchy-object-vector> => <stretchy-vector> },
    export: all;
  use dylan-extensions, 
    import: {false-or, as-keyword, <abstract-integer>,
	     \without-bounds-checks,
	     element-no-bounds-check, element-no-bounds-check-setter,
	     <machine-word>, <simple-machine-word-vector>,
	     <simple-integer-vector>, empty,
	     <simple-vector>, \limited-vector-definer,
	     \limited-stretchy-vector-definer,
	     <double-integer>, $minimum-unsigned-machine-word, %double-integer-low,
	     \packed-slots-definer, initialize-packed-slots,
	     integer-as-raw},
    export: all;
  use big-integers,
    prefix: "generic-",
    export: all;
  use streams, export: all;
  use standard-io, import: {*standard-output*}, export: all;
  use format, export: all;
  use format-out, export: all;
  use print, export: all;
  use byte-vector, export: all;
end module;


define module standard-imports
  use common-harp-imports, export: all;
  use dylan, 
    rename: { <stretchy-vector> => <any-stretchy-vector> }, 
    export: all;
end module;

define module big-integer-imports
  use common-harp-imports, export: all;
  use generic-arithmetic-dylan, 
    rename: { <stretchy-vector> => <any-stretchy-vector> }, 
    export: all;
  use big-integers, export: all;
end module;


define module harp-utils
  use standard-imports;
  use machine-word-lowlevel;
  export
    \alias-with-setter-definer,
    inc!, dec!, pushnew!, push!, list-pop!, stretchy-vector-pop!,
    logcount,
    <vector-32bit>, <bit-set>,
    $int-size$, $log-int-size$, $bit-unit-size$, $bit-set-word-size$,
    $bit-set-zero$, $bit-set-not-zero$,
    $empty-bit-set,
    make-bit-set, get-bit-from-set, get-bit-from-set!,
    set-bit-in-set, set-bit-in-set!, set-bit-in-word,
    unset-bit-in-set, unset-bit-in-word,
    bit-set-offset, bit-set-mask,
    bit-set-or, bit-set-or!, bit-set-and, 
    bit-set-xor, bit-set-andc2,
    bit-set-or-andc2,
    copy-bit-set, clear-bit-set,
    bit-set-update,
    bit-set-as-vector, vector-as-bit-set,
    pack-bitset, unpack-value-as-bitset,
    \do-bit-set,

    <bit-subset>,
    $empty-bit-subset,
    make-bit-subset,
    unset-bit-in-subset
    ;
end module;


define module harp-registers
  use standard-imports;
  use harp-utils;
  export
    <vreg-state>,
    vr-vect, vr-vect-setter,
    green-vr-vect, green-vr-vect-setter,
    next-vreg-id, next-vreg-id-setter,
    allow-vreg-reuse, allow-vreg-reuse-setter,
    gregs-to-reuse, gregs-to-reuse-setter,
    nregs-to-reuse, nregs-to-reuse-setter,
    reused-vreg-count, reused-vreg-count-setter,
    fresh-vreg-count, fresh-vreg-count-setter,
    dont-reuse-vregs, dont-reuse-vregs-setter,
    report-on-vreg-reuse, report-on-vreg-reuse-setter,
    unique-registers, unique-registers-setter,
    allocated-reals, allocated-reals-setter,
    allocated-preserved, allocated-preserved-setter,
    number-preserved, number-preserved-setter,
    raw-size, raw-size-setter,
    next-ng-spill, next-ng-spill-setter, 
    next-gc-spill, next-gc-spill-setter,
    next-sf-spill, next-sf-spill-setter,
    next-df-spill, next-df-spill-setter,
    ng-spill-central, ng-spill-central-setter, 
    gc-spill-central, gc-spill-central-setter,
    sf-spill-central, sf-spill-central-setter,
    df-spill-central, df-spill-central-setter,
    <register>, <virtual-register>, 
    <integer-virtual-register>,
    <floating-virtual-register>,
    <greg>, <nreg>, <sfreg>, <dfreg>,
    <simple-virtual-register-vector>,
    $evrv,
    invalid-virtual-register,
    invalid-virtual-register?,
    <stretchy-virtual-register-vector>,

    virtual-register-id, virtual-register-id-setter,
    virtual-register-name, virtual-register-name-setter,
    virtual-register-named-indirections, virtual-register-named-indirections-setter,
    virtual-register-colour, virtual-register-colour-setter,
    virtual-register-available-colours, virtual-register-available-colours-setter,
    virtual-register-clash-start, virtual-register-clash-start-setter,
    virtual-register-colour-pref, virtual-register-colour-pref-setter,
    virtual-register-spill-set, virtual-register-spill-set-setter,
    virtual-register-clash-count, virtual-register-clash-count-setter,
    virtual-register-virtual-pref, virtual-register-virtual-pref-setter,
    virtual-register-vreg-state, virtual-register-vreg-state-setter,

    <real-register>,
    real-register-mask, real-register-mask-setter,
    real-register-number, real-register-number-setter,
    real-register-pname, real-register-pname-setter,
    real-register-preserved-mask, real-register-preserved-mask-setter,
    real-register-c-preserved-mask, real-register-c-preserved-mask-setter,

    <spill>, <ispill>, <gspill>, <nspill>, <fspill>, <sfspill>, <dfspill>,
    spill-offset, spill-offset-setter,
    <spill-set>,
    spill-set-limit, spill-set-limit-setter,
    spill-set-scatter, spill-set-scatter-setter,
    spill-set-central, spill-set-central-setter,
    make-spill-set,
    <central-spill>,
    central-spill-holder, central-spill-holder-setter,
    central-spill-maker, central-spill-maker-setter,
    central-spill-lower-limit, central-spill-lower-limit-setter,
    $spill-set-no-reusage;
end module;


define module harp-register-model
  use standard-imports;
  use harp-utils;
  use harp-registers, import: {<register>, <real-register>};
  export
    <register-model>,
    invalid-register,
    reg-frame, reg-frame-setter,
    reg-stack, reg-stack-setter,
    reg-caller-stack, reg-caller-stack-setter,
    reg-environment, reg-environment-setter,
    reg-function, reg-function-setter,
    reg-mlist, reg-mlist-setter,
    reg-result, reg-result-setter,
    reg-result-out, reg-result-out-setter,
    reg-float-result, reg-float-result-setter,
    reg-arg-count, reg-arg-count-setter,
    reg-mv-count, reg-mv-count-setter,
    reg-arg0, reg-arg0-setter,
    reg-arg1, reg-arg1-setter,
    reg-arg2, reg-arg2-setter,
    reg-arg3, reg-arg3-setter,
    reg-arg4, reg-arg4-setter,
    reg-arg5, reg-arg5-setter,
    reg-arg6, reg-arg6-setter,
    reg-arg7, reg-arg7-setter,
    reg-float-arg0, reg-float-arg0-setter,
    reg-float-arg1, reg-float-arg1-setter,
    reg-float-arg2, reg-float-arg2-setter,
    reg-float-arg3, reg-float-arg3-setter,
    reg-float-arg4, reg-float-arg4-setter,
    reg-float-arg5, reg-float-arg5-setter,
    reg-float-arg6, reg-float-arg6-setter,
    reg-float-arg7, reg-float-arg7-setter,
    reg-machine-arguments, reg-machine-arguments-setter,
    reg-float-machine-arguments, reg-float-machine-arguments-setter,
    reg-c-machine-arguments, reg-c-machine-arguments-setter,
    reg-c-float-machine-arguments, reg-c-float-machine-arguments-setter,
    reg-arg-masks, reg-arg-masks-setter,
    reg-arg-masks-out, reg-arg-masks-out-setter,
    reg-c-arg-masks, reg-c-arg-masks-setter,
    reg-c-arg-masks-out, reg-c-arg-masks-out-setter,
    reg-link, reg-link-setter,
    reg-tmp1, reg-tmp1-setter,
    reg-tmp2, reg-tmp2-setter,
    reg-tmp3, reg-tmp3-setter,
    reg-tmp4, reg-tmp4-setter,
    reg-tmp5, reg-tmp5-setter,
    reg-ftmp1, reg-ftmp1-setter,
    reg-ftmp2, reg-ftmp2-setter,
    reg-c-result, reg-c-result-setter,
    reg-c-result2, reg-c-result2-setter,
    reg-c-float-result, reg-c-float-result-setter,
    reg-c-stack, reg-c-stack-setter,
    reg-c-frame, reg-c-frame-setter,
    real-register-vector, real-register-vector-setter,
    preserved-register-vector, preserved-register-vector-setter,
    c-preserved-register-vector, c-preserved-register-vector-setter,
    savable-registers, savable-registers-setter,
    arguments-passed-in-registers, arguments-passed-in-registers-setter,
    float-arguments-passed-in-registers, float-arguments-passed-in-registers-setter,
    c-arguments-passed-in-registers, c-arguments-passed-in-registers-setter,
    c-float-arguments-passed-in-registers, c-float-arguments-passed-in-registers-setter,
    all-allocatable, all-allocatable-setter,
    not-preserved, not-preserved-setter,
    c-not-preserved, c-not-preserved-setter;
end module;

define module harp-register-model-for-clients
  use harp-register-model,
   export:
   {
    <register-model>,
    reg-frame, reg-stack, reg-caller-stack, reg-environment, reg-function,
    reg-mlist, reg-result, reg-result-out, reg-float-result,
    reg-arg-count, reg-mv-count,
    reg-arg0, reg-arg1, reg-arg2, reg-arg3, 
    reg-arg4, reg-arg5, reg-arg6, reg-arg7, 
    reg-float-arg0, reg-float-arg1, reg-float-arg2, reg-float-arg3, 
    reg-float-arg4, reg-float-arg5, reg-float-arg6, reg-float-arg7, 
    reg-machine-arguments, reg-float-machine-arguments, 
    reg-c-machine-arguments, reg-c-float-machine-arguments, 
    reg-arg-masks, reg-arg-masks-out, reg-link,
    reg-tmp1, reg-tmp2, reg-tmp3, reg-tmp4, reg-tmp5, reg-ftmp1, reg-ftmp2,
    reg-c-result, reg-c-result2, reg-c-float-result, reg-c-stack, reg-c-frame,
    real-register-vector, preserved-register-vector,
    c-preserved-register-vector, savable-registers,
    arguments-passed-in-registers, c-arguments-passed-in-registers,
    float-arguments-passed-in-registers, c-float-arguments-passed-in-registers,
    all-allocatable,  not-preserved,  c-not-preserved
  }
end module;

define module harp-constant-references
  use standard-imports;
  use harp-utils;
  export
    <constant-reference>, 
    <address-constant-reference>, 
    <indirect-constant-reference>,
    <imported-constant-reference>,
    <interactor-constant-reference>,
    <i-constant-reference>, 
    <f-constant-reference>, 
    <sf-constant-reference>, 
    <df-constant-reference>, 
    <i-address-constant-reference>, 
    <i-indirect-constant-reference>,
    <sf-address-constant-reference>, 
    <sf-indirect-constant-reference>,
    <df-address-constant-reference>, 
    <df-indirect-constant-reference>,
    cr-refers-to, cr-refers-to-object, cr-const-offset,
    model-object-as-string,
    <labelled-constant>,
    <explicit-labelled-constant>,
    <labelled-absolute-constant>,
    <labelled-absolute-constant-high>,
    <labelled-absolute-constant-low>,
    <labelled-relative-constant>,
    <labelled-constant-with-opcode>,
    labelled-constant-size,
    labelled-constant-index, labelled-constant-index-setter,
    labelled-constant-reference, labelled-constant-reference-setter,
    opcode, opcode-value,
    <code-address-constant>,
    <relative-address-constant>,
    <relative-address-constant-high>,
    <relative-address-constant-low>,
    relative-offset, relative-offset-setter,
    <debug-info-constant>,
    <start-frame-marker>, <end-frame-marker>, 
    <code-locator-constant>,
    locator-live-variables, locator-live-variables-setter, 
    locator-with-stack?, locator-with-stack?-setter, 
    locator-data, locator-data-setter;
end module;


define module harp-basic-block
  use standard-imports;
  export
    <basic-block>,
    bb-start, bb-start-setter,
    bb-end, bb-end-setter,
    bb-taags, bb-taags-setter,
    bb-fall-thru, bb-fall-thru-setter,
    bb-next-set, bb-next-set-setter,
    bb-prev-set, bb-prev-set-setter,
    bb-other-set, bb-other-set-setter,
    bb-defs, bb-defs-setter,
    bb-last-entry, bb-last-entry-setter,
    bb-live-entry, bb-live-entry-setter,
    bb-colour, bb-colour-setter,
    bb-stack-state, bb-stack-state-setter,
    bb-needs-leaf, bb-needs-leaf-setter,
    bb-loop-depth, bb-loop-depth-setter,
    bb-copy-of, bb-copy-of-setter,
    copy-bb,
    <simple-basic-block-vector>,
    <stretchy-basic-block-vector>,
    $empty-basic-block,
    $no-colour, $red-colour, $green-colour,
    $yellow-colour, $brown-colour, $before-move-home-colour,
    $no-stack-state, $before-stack-state, $with-stack-state,
    $self-stack-state, $lf-stack-state, $constants-stack-state,
    print-bb-stack-state, print-bb-colour
    ;
end module;


define module harp-vars
  use standard-imports;
  use harp-basic-block, import: {<basic-block>, <stretchy-basic-block-vector>, $no-colour};
  use harp-registers, import: {<vreg-state>};
  use harp-utils, import: {<vector-32bit>, $empty-bit-set};
  export
    <harp-variables>,
    <instructions-vector>,
    sv-instructions, sv-instructions-setter,
    fp-instructions, fp-instructions-setter,
    current-bb, current-bb-setter,
    top-block, top-block-setter,
    bb-colour-flag, bb-colour-flag-setter,
    all-the-sdis, all-the-sdis-setter,
    optimize-leaf-case, optimize-leaf-case-setter,
    all-preserved-mask, all-preserved-mask-setter,
    processor-specific-return-offset, processor-specific-return-offset-setter,
    debug-code-select, debug-code-select-setter,
    taag-no, taag-no-setter,
    blocks-coalesced-yet, blocks-coalesced-yet-setter,
    current-instruction-can-be-moved, current-instruction-can-be-moved-setter,
    prev-instruction-can-be-moved, prev-instruction-can-be-moved-setter,
    word-size-of-bit-set, word-size-of-bit-set-setter,
    block-fixed-offset, block-fixed-offset-setter,
    current-block, current-block-setter,
    with-stack, with-stack-setter,
    code-vector, code-vector-setter,
    pgm-vect, pgm-vect-setter,
    in-back-end, in-back-end-setter,
    building-stack-frame, building-stack-frame-setter,
    reload-registers, reload-registers-setter,
    respill-registers, respill-registers-setter,
    vreg-state, vreg-state-setter,
    function-name, function-name-setter,
    compiling-defasm, compiling-defasm-setter,
    compiling-call-in, compiling-call-in-setter,
    asm-line-pos, asm-line-pos-setter,
    arg-spill-count, arg-spill-count-setter,
    named-variables-tables, named-variables-tables-setter,
    external-references, external-references-setter, 
    referenced-data-words, referenced-data-words-setter, 
    virtual-register-clashes, virtual-register-clashes-setter,
    make-harp-variables, make-harp-variables-internal,
    empty-code?, $empty-stretchy-vector,
    empty-named-variables?, $empty-named-variables-tables,
    make-named-variables, clear-named-variables;
end module;


define module harp-op
  use standard-imports;
  export 
    nil-fn, false-fn, no-template,
    <reg-fn>, <spread-fn>,
    <op>,
    op-name, op-name-setter,
    op-eliminatable, op-eliminatable-setter,
    op-disallow-fn, op-disallow-fn-setter,
    op-prefer-fn, op-prefer-fn-setter,
    op-clash-fn, op-clash-fn-setter,
    op-destroys-fn, op-destroys-fn-setter,
    op-c-preserved-destroys-fn, op-c-preserved-destroys-fn-setter,
    op-implicit-uses, op-implicit-uses-setter,
    op-spread, op-spread-setter,
    op-code-gen-fn, op-code-gen-fn-setter,
    op-reverse-op, op-reverse-op-setter,
    op-constant-fold-op, op-constant-fold-op-setter,
    op-flag, op-flag-setter,
    op-no-schedule, op-no-schedule-setter,
    op-bb-ender, op-bb-ender-setter,
    op-external-transfer, op-external-transfer-setter,
    op-does-jump, op-does-jump-setter,
    op-for-effective-address, op-for-effective-address-setter,
    op-is-move, op-is-move-setter,
    op-is-rem, op-is-rem-setter,
    op-is-scl, op-is-scl-setter,
    op-stack-dependent, op-stack-dependent-setter,
    op-keep-virtual, op-keep-virtual-setter,
    op-info, op-info-setter,
    stack-op?, stack-op?-setter,
    copy-op, modify-op, make-op;
end module;


define module base-harp
  use standard-imports;
  use dylan-primitives;
  use harp-utils, export: all;
  use harp-registers, export: all;
  use harp-register-model, export: all;
  use harp-constant-references, export: all;
  use harp-basic-block, export: all;
  use harp-op, export: all;
  use harp-vars, export: all;
  use harp-cg-back-end, 
    import: {<harp-cg-back-end>, <local-variable>, <lambda-compiled-data>},
    export: all;

  create <compiled-lambda>, <fully-compiled-lambda>, flush-compiled-lambda-post-output, emit-1, harp-move, m-ref;

  export
    // macros
    \ins-op, \ins-op-setter, 
    \ins-tag, \ins-tag-setter, 
    \constant-fn, 
    \for-instructions-in-basic-block, 
    \for-instructions-in-basic-block-backwards, 
    \for-instruction-defs,  \for-instruction-uses,
    \set-def, \set-use,
    \ins-operand-element, 
    \op-element, \op-element-setter,
    \update-op, \with-ops-in, \mark-reverse-ops,
    \with-xyz-macro-definer, \with-txyz-macro-definer,
    \xyz-definer-macro-definer, \ins-method-definer,
    \spread-function-definer, \register-function-definer,
    \output-function-definer, \instruction-function-definer,
    \harp-out, \harp-reapply, \emit,
    \call-instruction,
    \ensure-mreg, \loop-sdis,
    \with-harp,
    \preserving-instructions,

    // support
    harp-error, harp-warning, harp-message, pattern-error,
    copy-shared-variables, clear-shared-variables,

    // constants
    instruction-op-index, instruction-special-index, 
    instruction-defs-index, instruction-uses-index, 
    instruction-defs-slots, instruction-uses-slots,
    instruction-size, empty-rset,
  
    // instruction-support
    <abstract-instruction-set>, default-abstract-instruction-set,
    parent-instruction-set, initialize-instruction-set-defaults,
    make-instruction-set,

    // harp-back-end
    <harp-back-end>,
    registers, registers-setter,
    instructions,
    variables, variables-setter,
    code-item-increment,
    estimate-harp-instructions-size,
    estimate-harp-variables-size,
    estimate-harp-virtual-registers-size,
    initialize-back-end-variables,

    // register-support
    make-sf-register, make-df-register, 
    make-g-register, make-n-register, 
    make-register, move-reg, make-temp-register,
    mark-vreg-no-reusage, note-vreg-dead, 
    note-vrvect-truncated, colour, arg-spill-location, 
    \do-virtual-register-clashes,
    virtual-registers-clash, factorial-add,

    // tag
    <tag>, make-tag,
    tag-no, tag-no-setter,
    tag-bb, tag-bb-setter,
    tag-variables, tag-variables-setter,

    // basic-block-support
    make-bb,
    bb-fixed-offset, bb-fixed-offset-setter,
    bb-preceding-sdis, bb-preceding-sdis-setter,
    bb-seen, bb-seen-setter,
    bb-code-ptr, bb-code-ptr-setter,
    bb-branch-inf, bb-branch-inf-setter,

    // bb
    empty-bb?, find-bb, block-synonym, 
    finish-bb, make-current-bb, make-current-this-bb,
    taag-out, conditional-branch-windup,
    make-fall-thru-bb, add-destination-tags-to-bb, 
    fast-nsubstitute,  replace-old-with-new!,
    bb-redirect, output-unconditional-branch-instruction,
    ensure-room-in-array, ensure-room-in-vector, 
    instruction-uses, instruction-defs,

    // constant-ref-support
    ins--constant-ref, ins--indirect-constant-ref,
    ins--interactor-constant-ref,
    ins--sf-constant-ref, ins--sf-indirect-constant-ref,
    ins--df-constant-ref, ins--df-indirect-constant-ref, 
    ins--register-external, ins--reference-data, 
    ins--reference-sf-data, ins--reference-df-data,
    labelled-constant-increment,

    // conditional slot-packing
    \packed-slots?-definer,
    pack-integer?, unpack-integer?,
    pack-boolean?, unpack-boolean?,
    pack-object?, unpack-object?;

end module;


define module base-harp-for-clients
  use harp-register-model-for-clients, export: all;
  use base-harp,
   export:
   {
    // macros
    \with-harp,

    // support
    harp-error, harp-warning, harp-message, 

    // harp-back-end
    <harp-back-end>, registers, instructions,
    estimate-harp-instructions-size,
    estimate-harp-variables-size,
    estimate-harp-virtual-registers-size,
    initialize-back-end-variables,

    // register-support
    <register>, <real-register>, <virtual-register>, 
    <integer-virtual-register>,
    <floating-virtual-register>,
    <greg>, <nreg>, <sfreg>, <dfreg>,
    really-new-greg, really-new-nreg,
    make-sf-register, make-df-register, 
    make-g-register, make-n-register, 
    make-register, move-reg, make-temp-register,
    arg-spill-location, 

    // tag
    <tag>, make-tag,

    // constant-ref-support
    <constant-reference>, 
    <address-constant-reference>, 
    <indirect-constant-reference>,
    <imported-constant-reference>, 
    <interactor-constant-reference>,
    <sf-constant-reference>, 
    <df-constant-reference>,
    cr-refers-to, cr-refers-to-object, 
    model-object-as-string,
    ins--constant-ref, ins--indirect-constant-ref,
    ins--interactor-constant-ref,
    ins--sf-constant-ref, ins--sf-indirect-constant-ref,
    ins--df-constant-ref, ins--df-indirect-constant-ref,
    ins--register-external, ins--reference-data, 
    ins--reference-sf-data, ins--reference-df-data,

    // compiled-lambda
    flush-compiled-lambda-post-output,

    // misc
    replace-old-with-new!

   }
end module;



define module harp-instructions
  use standard-imports;
  use base-harp;
  export   
    // harp-definitions
    \with-none,   \with-u,      \with-uu, 
    \with-uuu,    \with-uuuu,   \with-uuuuu,  
    \with-uuuuuu, \with-ux,     \with-d, 
    \with-du,     \with-duu,    \with-duuu,   
    \with-duuuu,  \with-dux,    \with-dd, 
    \with-ddu,    \with-dduu,   \with-dduuu, 
    \with-dduuuu, \with-ddux,   \with-ddux,
    \with-t,      \with-tu,     \with-tuu, 
    \with-tuuu,   \with-tuuuu,  \with-tuuuuu,  
    \with-tuuuuuu,\with-tux,    \with-td, 
    \with-tdu,    \with-tduu,   \with-tduuu,   
    \with-tduuuu, \with-tdux,   \with-tdd, 
    \with-tddu,   \with-tdduu,  \with-tdduuu, 
    \with-tdduuuu,\with-tddux,  \with-tddux,

    \define-du,   \define-duu,  \define-duuu,
    \define-duuuu,\define-uuuu, \define-ddu,
    \define-dduu, \define-dduuu,
    \define-tu,   \define-tuu,  \define-td,
    \define-tdu,  \define-tddu, \define-tduu, 
    \define-u,    \define-uu,   \define-uuu,  
    \define-d,    \define-none, \define-tuuu, 
    \define-t,    \define-tduu, \define-tdduu,

    \none-def,    \u-def,       \uu-def, 
    \uuu-def,     \uuuu-def,    \uuuuu-def,  
    \uuuuuu-def,  \ux-def,      \d-def, 
    \du-def,      \duu-def,     \duuu-def,   
    \duuuu-def,   \dux-def,     \dd-def, 
    \ddu-def,     \dduu-def,    \dduuu-def, 
    \dduuuu-def,  \ddux-def,    \ddux-def,
    \t-def,       \tu-def,      \tuu-def, 
    \tuuu-def,    \tuuuu-def,   \tuuuuu-def,  
    \tuuuuuu-def, \tux-def,     \td-def, 
    \tdu-def,     \tduu-def,    \tduuu-def,   
    \tduuuu-def,  \tdux-def,    \tdd-def, 
    \tddu-def,    \tdduu-def,   \tdduuu-def, 
    \tdduuuu-def, \tddux-def,   \tddux-def,

    \none-def-setter,    \u-def-setter,       \uu-def-setter, 
    \uuu-def-setter,     \uuuu-def-setter,    \uuuuu-def-setter,  
    \uuuuuu-def-setter,  \ux-def-setter,      \d-def-setter, 
    \du-def-setter,      \duu-def-setter,     \duuu-def-setter,   
    \duuuu-def-setter,   \dux-def-setter,     \dd-def-setter, 
    \ddu-def-setter,     \dduu-def-setter,    \dduuu-def-setter, 
    \dduuuu-def-setter,  \ddux-def-setter,    \ddux-def-setter,
    \t-def-setter,       \tu-def-setter,      \tuu-def-setter, 
    \tuuu-def-setter,    \tuuuu-def-setter,   \tuuuuu-def-setter,  
    \tuuuuuu-def-setter, \tux-def-setter,     \td-def-setter, 
    \tdu-def-setter,     \tduu-def-setter,    \tduuu-def-setter,   
    \tduuuu-def-setter,  \tdux-def-setter,    \tdd-def-setter, 
    \tddu-def-setter,    \tdduu-def-setter,   \tdduuu-def-setter, 
    \tdduuuu-def-setter, \tddux-def-setter,   \tddux-def-setter,

    \none-uze,    \u-uze,       \uu-uze, 
    \uuu-uze,     \uuuu-uze,    \uuuuu-uze,  
    \uuuuuu-uze,  \ux-uze,      \d-uze, 
    \du-uze,      \duu-uze,     \duuu-uze,   
    \duuuu-uze,   \dux-uze,     \dd-uze, 
    \ddu-uze,     \dduu-uze,    \dduuu-uze, 
    \dduuuu-uze,  \ddux-uze,    \ddux-uze,
    \t-uze,       \tu-uze,      \tuu-uze, 
    \tuuu-uze,    \tuuuu-uze,   \tuuuuu-uze,  
    \tuuuuuu-uze, \tux-uze,     \td-uze, 
    \tdu-uze,     \tduu-uze,    \tduuu-uze,   
    \tduuuu-uze,  \tdux-uze,    \tdd-uze, 
    \tddu-uze,    \tdduu-uze,   \tdduuu-uze, 
    \tdduuuu-uze, \tddux-uze,   \tddux-uze,

    \none-uze-setter,    \u-uze-setter,       \uu-uze-setter, 
    \uuu-uze-setter,     \uuuu-uze-setter,    \uuuuu-uze-setter,  
    \uuuuuu-uze-setter,  \ux-uze-setter,      \d-uze-setter, 
    \du-uze-setter,      \duu-uze-setter,     \duuu-uze-setter,   
    \duuuu-uze-setter,   \dux-uze-setter,     \dd-uze-setter, 
    \ddu-uze-setter,     \dduu-uze-setter,    \dduuu-uze-setter, 
    \dduuuu-uze-setter,  \ddux-uze-setter,    \ddux-uze-setter,
    \t-uze-setter,       \tu-uze-setter,      \tuu-uze-setter, 
    \tuuu-uze-setter,    \tuuuu-uze-setter,   \tuuuuu-uze-setter,  
    \tuuuuuu-uze-setter, \tux-uze-setter,     \td-uze-setter, 
    \tdu-uze-setter,     \tduu-uze-setter,    \tduuu-uze-setter,   
    \tduuuu-uze-setter,  \tdux-uze-setter,    \tdd-uze-setter, 
    \tddu-uze-setter,    \tdduu-uze-setter,   \tdduuu-uze-setter, 
    \tdduuuu-uze-setter, \tddux-uze-setter,   \tddux-uze-setter,

    // instruction-macros
    \instruction-set-definer, 

    // bb-macros
    \output-instruction, \output-instruction-args,

    // harp-spread
    spread-none,   spread-du,     spread-duu,
    spread-duuu,   spread-uuuu,   spread-duuuu, 
    spread-uuuuu,  spread-uuuuuu, spread-ddu,  
    spread-dduu,   spread-dduuu,  spread-tu,   
    spread-tuu,    spread-td,     spread-tdu,  
    spread-tduu,   spread-tddu,   spread-tdduu,
    spread-uu,     spread-uuu,    spread-d,    
    spread-tuuu,   spread-t,      spread-u,
    spread-t-pop,
    default-overflow-function-clashes,
    default-double-overflow-function-clashes,

    // harp-outputters
    output-error,  output-none, output-tuuu,
    output-tuu,    output-tduu, output-td,
    output-t,      output-tu,   output-du,
    output-d,      output-u,    output-uu,
    output-duu,    output-uuu,  output-ddu,
    output-dduu,   output-duuu, output-duuuu,
    output-uuuu,   output-tdduu,output-dduuu,

    // core-instructions
    <core-instruction-set>, default-core-instructions, 
    ins--scl, harp-scl, harp-scl-setter,
    ins--strong-scl, harp-strong-scl, harp-strong-scl-setter,
    ins--move, harp-move-setter, // harp-move, // created in base-harp
    ins--add, harp-add, harp-add-setter,
    ins--sub, harp-sub, harp-sub-setter,
    ins--beq, harp-beq, harp-beq-setter,
    ins--bge, harp-bge, harp-bge-setter,
    ins--bgt, harp-bgt, harp-bgt-setter,
    ins--ble, harp-ble, harp-ble-setter,
    ins--blt, harp-blt, harp-blt-setter,
    ins--bne, harp-bne, harp-bne-setter,
    ins--bra, harp-bra, harp-bra-setter,
    ins--rem, harp-rem, harp-rem-setter,
    ins--load-stack-arg-n, harp-load-stack-arg-n, harp-load-stack-arg-n-setter,
    ins--tag, ins--taag, ins--immediate-literal;

end module;

define module harp-instructions-for-clients
  use harp-instructions,
    export:
    { 
    ins--scl,
    ins--strong-scl,
    ins--move,
    ins--add,
    ins--sub,
    ins--beq,
    ins--bge,
    ins--bgt,
    ins--ble,
    ins--blt,
    ins--bne,
    ins--bra,
    ins--rem,
    ins--load-stack-arg-n,
    ins--tag, ins--taag, ins--immediate-literal
    }
end module;



define module harp-templates
  use big-integer-imports;
  use base-harp;
  export   
    // template-macros
    \template-function,
    \template-definer-macro-definer,
    \local-template-definer-macro-definer,
    \op-template-definer,
    \local-template-definer,
    \local-fn, \call-local,
    \define-shared-template-functions,
    \define-curried-template-functions,
    \template-function-aux,
    \compile-clauses,
    \compile-one-clause,
    \compile-one-test,
    template-curry,

    // harp-predicates
    const-ref,
    canonicalised-const-ref,
    unsigned-canonicalised-const-ref,
    any-spill-ref,
    spill-ref,
    ispill-ref,
    fspill-ref,
    dfspill-ref,
    sfspill-ref,
    // m-ref,  // now created in base-harp
    m/spill-ref, <m/spill-ref>,
    m/const-ref, <m/const-ref>,
    spill/const-ref, <spill/const-ref>,
    any-constant-ref,
    constant-ref,
    i-constant-ref,
    f-constant-ref,
    sf-constant-ref,
    df-constant-ref,
    any-indirect-constant-ref,
    indirect-constant-ref,
    i-indirect-constant-ref,
    f-indirect-constant-ref,
    sf-indirect-constant-ref,
    df-indirect-constant-ref,
    ic/spill-ref, <ic/spill-ref>,
    ic/m/spill-ref, <ic/m/spill-ref>,
    f-ic/spill-ref, <f-ic/spill-ref>,
    df-ic/spill-ref, <df-ic/spill-ref>,
    sf-ic/spill-ref, <sf-ic/spill-ref>,
    any-address-constant-ref,
    address-constant-ref,
    i-address-constant-ref,
    f-address-constant-ref,
    sf-address-constant-ref,
    df-address-constant-ref,
    ac/const-ref, <ac/const-ref>,
    signed-twelve-bits?,
    signed-thirteen-bits?,
    signed-sixteen-bits?,
    unsigned-four-bits?,
    signed-eight-bits?,
    unsigned-eight-bits?,
    sixteen-bit-const-ref,
    eight-bit-const-ref,
    unsigned-fifteen-bit-const-ref,
    canonicalise-int,
    unsigned-canonicalise-int,
    zero-number?;
end module;


define module harp-debug-info
  use standard-imports;
  use base-harp;
  use source-records, 
    exclude: { source-record-start-line, source-record-end-line };
  export
    <source-position>,
    <relative-source-position>,
    <absolute-source-position>,
    source-position-source-record,
    start-offset-into-source-record,
    end-offset-into-source-record,
    function-relative-line-number,
    function-relative-code-position,
    source-record-start-line,
    source-record-end-line,
    source-record-file-name,
    locator-as-absolute-source-position,
    make-relative-source-position, 
    <named-variable>, 
    variable-name, 
    <variable-in-register>, <variable-in-spill>,
    <variable-in-frame-spill>, <variable-in-leaf-spill>,
    <variable-indirections>, <variable-indirection-offset>,
    <named-variable-in-register>, 
    <named-variable-in-spill>, 
    <named-variable-in-leaf-spill>,
    <indirections-variable-in-register>, 
    <indirections-variable-in-spill>,
    <indirections-variable-in-leaf-spill>,
    <indirections-variable-in-indirection>, 
    <named-indirection>, 
    make-indirection-variable,
    variable-indirections, variable-indirection-offset,
    variable-register-enumeration, variable-frame-pointer-offset,
    spill-frame-pointer-offset, 
    real-register-debug-info-enumeration, 
    real-register-from-debug-info-enumeration, 
    map-register-as-variable,
    <debug-scope>, <debug-scope-with-frame>, <debug-scope-no-frame>,
    debug-scope-with-frame?,
    named-variables, nested-scopes,
    start-code-offset, end-code-offset,
    add-debug-scope,
    <debug-scopes>,
    make-zero-debug-scopes, make-debug-scopes,
    empty-debug-scopes?, empty-variables?,
    start-debug-scopes, size-debug-scopes,
    \for-debug-scope, \for-reversed-debug-scope, \for-debug-var,
    pack-debug-scopes,
    print-debug-scopes,
    debug-scopes-as-vector, debug-vars-as-list,
    concatenate-variables,
    dummy-harp-source-locator?;
end module;


define module harp-debug-info-for-clients
  use harp-debug-info,
   export:
   {
    make-indirection-variable,
    dummy-harp-source-locator?
   }
end module;



define module harp-outputter
  use standard-imports;
  use locators;
  use base-harp;
  use harp-debug-info;
  use source-records,
    import: {<source-location>, object-source-location-lines};
  use dood;
  export
    // Main interface
    <harp-outputter>,
    outputter-line-pos, outputter-line-pos-setter,
    lambda-name, lambda-name-internal, lambda-name-internal-setter,
    lambda-code, lambda-referenced-data, 
    lambda-labels, lambda-location, external-lambda-location,
    lambda-variable-scopes, lambda-variable-scopes-internal, lambda-all-variable-scopes,
    lambda-all-variable-names,
    lambda-all-locators, lambda-selected-locators, 
    lambda-frame-g-size, lambda-frame-n-size,
    lambda-externals, lambda-harp-print-info, 
    lambda-is-public?, lambda-is-export?,
    make-harp-outputter,
    make-interactive-print-outputter,
    make-binary-interactive-assembler-outputter,
    make-mnemonic-interactive-assembler-outputter,
    make-interactive-outputter,
    multiplex-outputters,
    close-harp-outputter,
    model-object-protocol,
    model-object-protocol?, model-object-protocol?-setter,
    dynamic-linking-protocol,
    model-object-name,
    outputter-model-object, outputter-model-object-name,
    make-derived-model-object, make-string-model-object,
    outputter-name-unsupplied, outputter-name-supplied?,
    canonical-code-object, canonical-data-object, 
    canonical-lambda-object, canonical-interactor-object,
    output-compilation-record-data,
    output-comment,
    output-line-comment,
    output-external,
    output-implicit-externals,
    output-public,
    output-export,
    output-definition,
    output-variable,
    output-header,
    output-footer,
    output-data-start,
    output-code-start,
    output-glue-symbols, 
    output-data-item,
    output-data-byte,
    output-data-footer,
    output-compiled-lambda,
    outputter-downloadable-data,
    print-harp-by-default,
    // Interface for implementors of the protocol
    <stream-wrapper-outputter>, outputter-stream,
    <harp-print-outputter>, <harp-interactive-print-outputter>,
    <harp-assembler-outputter>, 
    <harp-streamed-assembler-outputter>,
    <harp-unstreamed-assembler-outputter>,
    <harp-binary-assembler-outputter>,
    <harp-mnemonic-assembler-outputter>,
    <harp-mnemonic-streamed-assembler-outputter>,
    <harp-mnemonic-unstreamed-assembler-outputter>,
    <harp-binary-streamed-assembler-outputter>,
    <harp-binary-unstreamed-assembler-outputter>,
    do-outputters,
    make-harp-outputter-by-type,
    default-harp-output-type,
    file-extension-for-outputter-type,
    stream-type-for-outputter-type,
    open-output-stream,
    close-output-stream;
end module;



define module harp-dylan-parameterizations
  use standard-imports;
  export
    // function-offsets
    function-xep-offset, function-iep-offset, function-mep-offset, 
    function-keywords-offset, function-signature-offset,
    closure-environment-offset, keyword-closure-environment-offset,
    method-size, keyword-method-size, 
    simple-closure-size, keyword-closure-size, 
    generic-function-engine-offset,
    engine-node-data-offset,
    engine-node-properties-offset, engine-node-callback-offset, 
    engine-node-data-1-offset, engine-node-data-2-offset, 
    engine-node-data-3-offset, engine-node-entry-point-offset, 
    signature-required-offset, signature-properties-offset, 
    mm-wrapper-subtype-mask-offset,
    cache-header-engine-node-next-offset,
    profiling-cache-header-engine-node-count-1-offset,
    profiling-cache-header-engine-node-count-2-offset,
    type-instancep-function-offset,
    size-of-bind-exit-frame, size-of-unwind-protect-frame;
  export
    $mm-wrapper-subtype-value-object-mask,
    $mm-wrapper-subtype-mm-wrapper-mask,
    $mm-wrapper-subtype-class-mask,
    $mm-wrapper-subtype-implementation-class-mask,
    $mm-wrapper-subtype-by-class-discriminator-mask,
    $mm-wrapper-subtype-abstract-integer-mask,
    $mm-wrapper-subtype-function-mask,
    $mm-wrapper-subtype-sequence-mask,
    $mm-wrapper-subtype-string-mask,
    $mm-wrapper-subtype-error-mask,
    $mm-wrapper-subtype-collection-mask,
    $mm-wrapper-subtype-cache-header-engine-node-mask;
  /*
  export
    engine-node$k-absent,
    engine-node$k-inapplicable,
    engine-node$k-unkeyed-single-method,
    engine-node$k-implicit-keyed-single-method,
    engine-node$k-explicit-keyed-single-method,
    engine-node$k-unrestricted-keyed-single-method,
    engine-node$k-reserved-terminal-n-a,
    engine-node$k-reserved-terminal-n-b,
    engine-node$k-reserved-terminal-n-c,
    engine-node$k-reserved-terminal-n-d,
    engine-node$k-reserved-terminal-n-e,
    engine-node$k-reserved-terminal-n-f,
    engine-node$k-reserved-terminal-n-g,
    engine-node$k-profiling-cache-header,
    engine-node$k-cache-header,
    engine-node$k-ambiguous-methods,
    engine-node$k-first-slot-engine-node,
    engine-node$k-boxed-instance-slot-getter,
    engine-node$k-boxed-instance-slot-setter,
    engine-node$k-boxed-repeated-instance-slot-getter,
    engine-node$k-boxed-repeated-instance-slot-setter,
    engine-node$k-boxed-class-slot-getter,
    engine-node$k-boxed-class-slot-setter,
    engine-node$k-raw-byte-repeated-instance-slot-getter,
    engine-node$k-raw-byte-repeated-instance-slot-setter,
    engine-node$k-reserved-slot-a-getter,
    engine-node$k-reserved-slot-a-setter,
    engine-node$k-reserved-slot-b-getter,
    engine-node$k-reserved-slot-b-setter,
    engine-node$k-reserved-repeated-slot-a-getter,
    engine-node$k-reserved-repeated-slot-a-setter,
    engine-node$k-reserved-repeated-slot-b-getter,
    engine-node$k-reserved-repeated-slot-b-setter,
    engine-node$k-slot-engine-node-count,
    engine-node$k-typecheck,
    engine-node$k-if-type,
    engine-node$k-linear-by-class,
    engine-node$k-hashed-by-class,
    engine-node$k-linear-by-singleton-class,
    engine-node$k-hashed-by-singleton-class,
    engine-node$k-immediate-linear-singleton,
    engine-node$k-immediate-hashed-noreloc-singleton,
    engine-node$k-immediate-hashed-singleton,
    engine-node$k-value-object-linear-singleton,
    engine-node$k-reserved-discriminator-a,
    engine-node$k-reserved-discriminator-b,
    engine-node$k-reserved-discriminator-c,
    engine-node$k-reserved-discriminator-d,
    engine-node$k-reserved-discriminator-e,
    engine-node$k-reserved-discriminator-f,
    engine-node$k-reserved-discriminator-g,
    engine-node$k-reserved-discriminator-h,
    engine-node$k-reserved-discriminator-i,
    engine-node$k-reserved-discriminator-j,
    engine-node$k-reserved-discriminator-k,
    engine-node$k-reserved-discriminator-l,
    engine-node$k-reserved-discriminator-m,
    engine-node$k-reserved-discriminator-n,
    engine-node$k-reserved-discriminator-o,
    engine-node$k-reserved-discriminator-p,
    engine-node$k-reserved-discriminator-q,
    engine-node$k-reserved-discriminator-r,
    engine-node$k-reserved-discriminator-s,
    engine-node$k-reserved-discriminator-t,
    engine-node$k-reserved-discriminator-u;
  */
  export
    // properties$m-entry-type,
    properties$s-entry-type,
    properties$v-entry-type,
    properties$v-data-start,
    engine-node$v-data-start,
    smen$v-nrequired,
    smen$s-nrequired,
    smen$m-nrequired,
    smen$v-restp,
    // smen$m-restp,
    smen$v-data-start,
    $simple-typechecked-cache-arguments-limit,
    stchen$v-checkedmask,
    stchen$s-checkedmask,
    stchen$m-checkedmask,
    discriminator$v-argnum,
    discriminator$s-argnum,
    // discriminator$m-argnum,
    discriminator$v-nrequired,
    discriminator$s-nrequired,
    // discriminator$m-nrequired,
    discriminator$v-restp,
    // discriminator$m-restp,
    discriminator$v-data-start;
end module;


define module main-harp
  use standard-imports;
  use machine-word-lowlevel;
  use base-harp;
  use harp-instructions;
  use harp-outputter;
  use harp-debug-info;
  use harp-dylan-parameterizations, export: all;
  export  

    // indep-utils
    prefer, rset-from-args, rset-from-list, prset-from-list, r-union, 
    list-from-register-vector, list-from-rset, list-from-prset,
    r-membr, signed-frame-pointer-offset,
    frame-pointer-offset, 

    // consistency
    harp-consistency-check, check-consistent-operand,

    // sdi
    <new-sdi>, 
    new-sdi-code-holder, new-sdi-preceding-sdis,
    new-sdi-code-fragment, new-sdi-code-fragment-setter,

    // asm-allocate
    optimize-leaf-case-1, optimize-leaf-case-2,
    set-liveness, set-thingy-vect, set-use-defs,
    allowable-colours, make-pref-vector,
    make-arg-spill, arg-spill?,
    arg-spill-offset-to-arg-number, 
    uniquely-spill,

    // asm-colour-graph
    sort-by-block-clashes, select-spill, 
    select-spill-by-colour, select-spill-by-number, 
    the-real-dfreg, 

    // asm-code-select
    do-scheduling, 
    emit-2, emit-sdi, //    emit-1 is created in base-harp
    emit-constant-ref, emit-constant-ref-relative,
    emit-constant-ref-with-opcode, emit-labelled-constant, 

    // asm-linearise
    linearise,

    // asm-lambda-code
    assemble-compiled-lambda, 
    fill-compiled-lambda-code-and-labels, fill-code-for-item,
    compiled-lambda-size, code-item-code-and-label-size,
    print-linearised-harp, 
    compiled-lambda-debug-scopes,
    strip-locators-from-debug-info,

    // post-cg-lambda
    pre-cg-lambda, post-cg-lambda,

    // asm-top-level,
    build-pgm-vector,

    // harp-invoke
    print-harp-debug-by-default,
    invoke-harp, invoke-harp-asm;

end module;


define module main-harp-for-clients
  use harp-dylan-parameterizations, export: all;
  use main-harp,
   export:
   {

    // consistency
    harp-consistency-check,

    // harp-invoke
    print-harp-debug-by-default,
    invoke-harp, invoke-harp-asm

   }
end module;


define module print-harp
  use standard-imports;
  use base-harp;
  use main-harp;
  use harp-templates;
  use harp-debug-info;
  export
    print-instructions, 
    print-basic-blocks;
end module;

define module harp-for-extenders
  use standard-imports, export: {false-or, as-keyword};
  use base-harp, export: all;
  use main-harp, export: all;
  use harp-instructions, export: all;
  use harp-templates, export: all;
  use print-harp, export: all;
  use harp-outputter, export: all;
  use harp-debug-info, export: all;
end module;


define module harp
  use standard-imports, export: {false-or, as-keyword};
  use base-harp-for-clients, export: all;
  use main-harp-for-clients, export: all;
  use harp-instructions-for-clients, export: all;
  use print-harp, export: all;
  use harp-outputter, export: all;
  use harp-debug-info-for-clients, export: all;
end module;

