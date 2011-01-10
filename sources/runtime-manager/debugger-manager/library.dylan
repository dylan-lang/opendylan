Module:    dylan-user
Synopsis:  The Debugger Manager Library
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library debugger-manager
  use functional-dylan;
  use big-integers;
  use collections;
  use access-path;
  use tether-downloader;
  use interactive-symbol-table;
  use dfmc-browser-support;
  use dfmc-namespace;		// To extend ACTIVE-LEXICAL-VARIABLES GF.
  use dfmc-mangling;
  use source-records;
  export debugger-manager;
end library debugger-manager;

define module debugger-manager
  use functional-dylan;
  use table-extensions,
    import: {<string-table>};
  use access-path,
    exclude: { kill-application },
    export: all;
  use tether-downloader;
  use interactive-symbol-table;
  use dfmc-derived-information, 
    rename: {<source-locator> => <dfmc-source-locator>};
  use dfmc-namespace;
  use source-records;
  create
           
    ///// <DEBUG-TARGET>

    <debug-target>,
    debug-target-access-path,
    debug-target-symbol-table,
    debug-target-compilation-context,
    debug-target-compilation-context-setter,
    find-library-called,
    obtain-component-name,

    <interactor-return-breakpoint>,
    interaction-request-application-state,
    interaction-request-application-state-setter,

    ///// Debugger transaction caching utilities.

    <page-relative-object-table>,
    <page-relative-object-table-entry>,
    add-object,
    enquire-object,
    remove-object,
    invalidate-page-relative-object-table,

    ///// <STOP-REASON>s

    stop-reason-debug-points,
    <debugger-generated-stop-reason>,
    stop-reason-client-data,
    <debugger-stop-application-stop-reason>,
    <dylan-invoke-debugger-stop-reason>,
    dylan-error-message-string,
    <dylan-debug-message-stop-reason>,
    dylan-debug-message-string,
    <source-code-alignment-stop-reason>,
    <interactor-return-stop-reason>,
    <interactive-thread-initialized-stop-reason>,
    interactive-thread-name,
    interactor-transaction-id,
    interactor-return-values,
    setup-interactor,
    handle-interactor-return,
    <class-breakpoint-stop-reason>,
    class-breakpoint-class,
    class-breakpoint-size,

    ///// Application control

    stop-application,
    kill-application,
    restart-application,
    manage-running-application,
    application-stopped?,
    application-stopped?-setter,

    ///// <DEBUG-POINT>

    <debug-point>,
    <breakpoint>,
    <watchpoint>,
    <tracepoint>,
    <entry-tracepoint>,
    <return-tracepoint>,
    make-return-tracepoint,
    initialize-return-tracepoint,
    corresponding-entry-tracepoint,
    dylan-trace-entry-arguments,
    dylan-trace-return-values,
    handle-debug-point-event,
    register-debug-point,
    deregister-debug-point,
    <debug-point-error>,

    application-running-on-code-entry?,
    application-running-on-code-entry?-setter,
    application-just-interacted?,
    application-just-interacted?-setter,
    interactor-deferred-id-table,

    ///// <DYLAN-NAME-CONTEXT>

    <dylan-name-context>,
    context-library, context-module,
    context-library-setter, context-module-setter,
    demangle-dylan-name,	// Convenience function for console dbg only!
    demangle-local-dylan-name,	// Ditto.
    mangle-local-dylan-name,	// Ditto ditto ;-)
    mangle-in-context,		// This is absolutely the last one!

    ///// Transactions on dylan values

    read-dylan-value,
    write-dylan-value,
    read-instance-slot-element,

    ///// Print and Inspect

    print-dylan-object,
    describe-dylan-object,
    get-inspector-values,
    dylan-class-browser-information,
    dylan-class-slot-storage,
    dylan-object?,
    dylan-object-size,
    resolve-dylan-name,
    resolve-dylan-keyword,
    dylan-keyword-name,
    resolve-symbolic-name,
    find-closest-symbolic-name,
    dylan-object-class,
    foreign-object-type,
    dylan-object-immediate-value,
    find-dylan-name,
    dylan-method-iep,
    dylan-generic-function-methods,
    dylan-method-specializers,
    dylan-slot-descriptor-getter,
    remote-instance?,
    remote-subclass?,
    remote-member?,
    remote-collection-inspect,
    remote-collection-size,
    remote-class-inspect,
    remote-singleton-inspect,
    remote-generic-function-inspect,
    remote-method-inspect,
    remote-slot-inspect,
    remote-pair-inspect,
    remote-range-inspect,
    remote-signature-inspect,
    dylan-value-unbound?,
    dylan-runtime-unbound-marker,
    dylan-runtime-boolean-markers,
    classify-runtime-value,

    ///// Dylan Thread Inspections.

    remote-thread-information,
    thread-current-active-handlers,
    thread-current-local-variables,
    thread-available-for-interaction?,
    spawn-interactive-thread,
    thread-local-variable?,
    evaluate-thread-local-variable,
    set-thread-local-variable,
    get-thread-interactor-level,
    application-primary-thread,
    application-selected-thread,
    application-selected-thread-setter,

    ///// Remote Walker Convenience Functions

    remote-instance-wrapper,
    wrapper-trace-information,

    ///// Debugger Transactions and Object Registration

    <remote-object>,
    <object-registration-error>,
    register-remote-object,
    free-remote-object,
    remote-object-value,
//    remote-object-valid?,
    object-requires-registration?,
    call-debugger-function,

    ///// Thread-local object history management.

    flush-thread-history,
    record-object-in-thread-history,
    retrieve-object-from-thread-history,
    <history-place-holder>,
    history-place-holder-thread,
    history-place-holder-index,

    ///// Registering regions of interactively allocated memory

    register-exact-roots,
    register-static-roots,
    register-ambiguous-roots,
    register-interactive-code,
    fixup-imported-data-region,
    fixup-unimported-data-region,
    use-thread-for-spy-functions,
    target-spy-thread,
    select-thread-for-spy,

    ///// Stack Backtracing

    <application-stack-frame>,
    first-stack-frame,
    next-stack-frame,
    previous-stack-frame,
    stack-frame-thread,
    <dylan-stack-frame-mixin>,	// Obsolescent
    <call-frame>,
    call-frame-description,
    call-frame-function,
    call-frame-code-offset,
    call-frame-nearest-source-locator,
    call-frame-aligned-at-source-locator?,
    call-frame-return-address,
    call-frame-instruction-pointer,
    call-frame-frame-pointer,
    <dylan-call-frame>,		// Obsolescent
    dylan-call-frame?,		// This is to be the replacement.
    <implementation-stack-frame>,
    <bind-exit-frame>,
    <unwind-protect-frame>,
//    <dynamic-bind-frame>,
    thread-current-mv-vector,

    ///// Finding dylan components.

    application-dylan-library,
    application-dylan-runtime-library,
    dylan-application?,

    ///// Invoking dylan code (Used by console dbg)
    <dylan-return-breakpoint>,
    dylan-function-result,
    invoke-dylan,

    ///// Tracking library initialization.

    <library-initialization-phase>,
    handle-library-initialization-phase,

    ///// Lexical Variables,

    number-of-lexical-variables,
    active-dylan-lexical-variables,
    number-of-active-dylan-variables,
    live-frame-lexical-variables,

    ///// Source Stepping

    instruct-thread-to-step-over,
    instruct-thread-to-step-out,
    instruct-thread-to-step-into,
    align-thread-to-source-location,

    ///// Restarts

    <remote-restart>,
    remote-restart-description,
    remote-restart-abort?,
    available-restarts-for-thread,
    signal-restart-on-thread,

    ///// TODO: Place holders shunted up from access-path.
    ///// Need to import from source-records library...
    ///// (Maybe we don't need the last one). The compiler
    ///// should be generally able to provide enough
    ///// information - it is the primary source of
    ///// locators.

    <source-locator>,
    source-locator-linenumber,
    source-locator-file,
    source-location-remote-address,
    remote-address-source-location,
          
    /////
    ///// RUNTIME-CONTEXT
    /////

    <runtime-context>,
    runtime-context-debug-target,
    runtime-context-thread,
    runtime-context-frame,
    current-runtime-context,

    /////
    ///// PROFILING
    /////
    application-profiling?,
    control-profiling,
    start-profiling,
    stop-profiling,
    profile-data,
    reset-profile-data,
    <application-profile>,
    application-snapshot-skip,
    application-snapshots,
    application-profile-threads,
    <application-snapshot>,
    application-thread-snapshot,
    wall-time-increment,
    page-faults-increment,
    thread-snapshots,
    <thread-snapshot>,
    <instruction-pointers>,
    profile-thread,
    cpu-time-increment,
    allocation-increment,
    allocated-class,
    instruction-pointers,
    set-application-class-breakpoint,
    clear-application-class-breakpoint,
    clear-application-class-breakpoints,


    /////
    ///// EXTENSION-INTERFACES (Not 'arf!)
    /////

    load-runtime-component,
    \spy-function-definer,
    <c-spy-function-descriptor>,
    spy-function-runtime-name,
    spy-function-runtime-component,
    call-spy,
    call-spy-on-thread,
    <spy-call-error>,
    spy-call-function-descriptor,
    spy-call-debug-target,
    spy-call-arguments,
    <spy-function-not-located>,
    <spy-call-aborted>,
    <spy-call-no-available-thread>,
    <spy-call-cannot-use-thread>,
    spy-call-selected-thread;
end module debugger-manager;

define module dm-internals
  use functional-dylan;
  use threads, rename: {thread-name => thread-name-internal};
  use dylan-extensions, import: {<double-integer>};
  use table-extensions, import: {<string-table>};
  use simple-format;
  use access-path,
    import: { kill-application => kill };
  use debugger-manager;
  use tether-downloader;
  use interactive-symbol-table;
  use dfmc-derived-information, 
    rename: {<source-locator> => <dfmc-source-locator>};
  use dfmc-namespace;
  use dfmc-mangling;
  use source-records;
end module dm-internals;

