module:       dylan-user
synopsis:     Module definitions for the access-path library
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module access-path
  
   use functional-dylan;

   create

      ///// <access-path>

      <access-path>,
      <application-access-path>,
      access-path-abstract-handle,
      access-path-abstract-handle-setter,
      access-path-application,
      access-path-arguments,
      access-path-process,
      access-path-core-file,
      <access-path-creation-error>,
      $max-spy-function-arguments,
      spy-function-argument-remote-vector,
      spy-function-argument-remote-vector-setter,
      $max-stepping-locations,
      stepping-locations-remote-vector,
      stepping-locations-remote-vector-setter,

      $access-ok,

      ///// Remote Objects

      <debugger-connection>,
      *open-debugger-connections*,
      connection-hostname, connection-hostname-setter,
      connection-open?, connection-open?-setter,
      connection-password,
      describe-debugger-connection,
      do-open-debugger-connections,
      <access-connection>,
      make-access-connection,
      do-open-access-connections,
      connection-open-tethers,
      connection-process, connection-process-setter,
      connection-process-list, connection-process-list-setter,
      access-debugger-connection,
      <local-debugger-connection>,
      <remote-debugger-connection>,
      close-remote-debugger-connection,
      access-connection-description,
      connection-network-address,
      <debugger-connection-failure>,
      <open-debugger-connection-failure>,
      attempted-connection-network-address,
      <open-debugger-connection-password-mismatch>,
      attempted-connection-password,
      host-machine,
      $local-hostname,
      do-processes,
      get-process-page-fault-count,

      <remote-process>,
      remote-process-name,
      remote-process-system-identifier,
      remote-process-actual-identifier,

      <remote-thread>,
      do-threads,
      register-thread,
      thread-name,
      thread-access-path,
      get-thread-cpu-time,
      get-process-wall-clock-time,
      number-of-active-threads,
      thread-state,
      thread-priority,
      thread-suspended?,
      thread-permanently-suspended?,
      thread-permanently-suspended?-setter,
      access-path-application-object,
      nub-descriptor,
      rnub-descriptor,
      *next-thread-id*,
      stack-size, stack-size-setter,
      stack-size-valid?, stack-size-valid?-setter,
      stack-trace-valid?, stack-trace-valid?-setter,
      thread-stack, thread-stack-setter,

      <remote-library>,
      do-libraries,
      library-version,
      library-base-address,
      library-image-name,
      library-core-name,
      library-object-files,
      extend-remote-library,
      find-or-make-library,
      self-contained-component?,
      self-contained-component?-setter,

      <remote-object-file>,
      remote-object-file-core-name,
      remote-object-file-source-extension,
      remote-object-file-object-extension,
      remote-object-file-path,
      remote-object-file-library,
      remote-object-file-language,
      remote-object-file-client-data,

      ///// <remote-value>

      <remote-value>,
      as-integer,
      as-signed-integer,
      as-remote-value,
      as-remote-pointer,
      indexed-remote-value,
      byte-indexed-remote-value,
      remote-value-byte-size,
      tagged-remote-value-as-integer,
      tagged-remote-value-as-character,
      integer-as-tagged-remote-value,
      character-as-tagged-remote-value,
      remote-value-=,
      remote-value-<,
      remote-value-<=,
      as-integer-losing-precision,
      as-remote-value-losing-precision,
      remote-value-as-string,
      string-as-remote-value,
      remote-value-low-order-bits,

      ///// Register and memory access

      <remote-register>,
      <unassigned-remote-register>,
      <active-remote-register>,
      register-name,
//    register-type,  // TODO
      do-registers,
      find-register,
      active-register,
      register-to-enumeration-code,
      enumeration-code-to-register,
      <remote-location>,
      <remote-type>,

      page-read-permission?,
      page-write-permission?,
      page-execute-permission?,
      remote-virtual-page-size,
      remote-address-page-number,
      page-relative-address,
      perform-coff-relocation,
      calculate-stack-address,

      <remote-access-violation-error>,
      read-value, write-value,
      read-8b, write-8b,
      read-16b, write-16b,
      read-32b, write-32b,
      read-64b, write-64b,
      read-single-float, write-single-float,
      read-double-float, write-double-float,
      read-byte-string, write-byte-string,
//    read-byte-vector, write-byte-vector, // TODO (or use byte-string APIs)
//    read-unicode-string, write-unicode-string, // TODO

      <NUB>, <NUBLIBRARY>, <NUBHANDLE>, <NUBPROCESS>, <NUBTHREAD>,
      <THREAD-CONTEXT>,

      ///// Application Control

      restart,
      stop,
      continue,
      continue-unhandled,
      suspend-thread,
      resume-thread, dylan-resume-thread,
      step,
      step-over,
      step-out,
      application-state-running?,
      application-state-stopped?,
      application-state-unstarted?,
      application-state-post-mortem?,
      kill-application,
      close-application,
      register-exit-process-function,

      ///// Remote Function Calling

      remote-call,
      remote-call-result,
      remote-restore-context,
      remote-call-spy,

      ///// Breakpoints and Watchpoints

      enable-breakpoint, disable-breakpoint, query-breakpoint?,
      enable-read-watchpoint, disable-read-watchpoint, 
      query-read-watchpoint?,
      enable-write-watchpoint, disable-write-watchpoint, 
      query-write-watchpoint?,
      enable-execute-watchpoint, disable-execute-watchpoint, 
      query-execute-watchpoint?,
      recover-breakpoint,

      ///// Stepping at source code level.

      $step-operation-step-out,
      $step-operation-step-over,
      $step-operation-step-into,

      apply-thread-stepping-control,
      remove-all-stepping-control-for-thread,

      ///// <stop-reason>

      wait-for-stop-reason,
      inform-profiling-started,
      inform-profiling-stopped,
      exception-name,
      receiving-first-chance?,
      receiving-first-chance?-setter,
      first-chance-exception?,
      receivable-first-chance-exceptions,

      <stop-reason>,
      <internal-stop-reason>,
      <basic-stop-reason>,
      <language-level-stop-reason>,
      <external-stop-reason>,
      stop-reason-process,
      stop-reason-thread,
      <process-stop-reason>,
      <create-process-stop-reason>,
      stop-reason-executable-component,
      <exit-process-stop-reason>,
      stop-reason-process-exit-code,
      <thread-stop-reason>,
      <create-thread-stop-reason>,
      create-thread-event-handler,
      interactive-thread-break-event-handler,
      stop-reason-thread-exit-code,
      <exit-thread-stop-reason>,
      <library-stop-reason>,
      stop-reason-library,
      <load-library-stop-reason>,
      <unload-library-stop-reason>,
      <RIP-stop-reason>,
      <output-debug-string-stop-reason>,
      stop-reason-debug-string,
      stop-reason-exit-code,
      <debug-point-stop-reason>,
      stop-reason-debug-point-address,
      <breakpoint-stop-reason>,
      <source-step-stop-reason>,
      <source-step-out-stop-reason>,
      <source-step-over-stop-reason>,
      <source-step-into-stop-reason>,
      <single-step-stop-reason>,
      <watchpoint-stop-reason>,
      <read-watchpoint-stop-reason>,
      <write-watchpoint-stop-reason>,
      <execute-watchpoint-stop-reason>,
      <exception-stop-reason>,
      stop-reason-exception-address,
      stop-reason-exception-first-chance?,
      <stack-overflow-exception-stop-reason>,
      <integer-overflow-exception-stop-reason>,
      <invoke-debugger-stop-reason>,
      <system-initialized-stop-reason>,
      <system-invoke-debugger-stop-reason>,
      <memory-exception-stop-reason>,
      <access-violation-stop-reason>,
      stop-reason-access-violation-address,
      stop-reason-access-violation-operation,
      $access-violation-undecidable,
      $access-violation-on-read,
      $access-violation-on-write,
      $access-violation-on-execute,
      <array-bounds-exception-stop-reason>,
      <instruction-exception-stop-reason>,
      <illegal-instruction-exception-stop-reason>,
      <privileged-instruction-exception-stop-reason>,
      <arithmetic-exception-stop-reason>,
      <float-exception-stop-reason>,
      <denormal-exception-stop-reason>,
      <float-divide-by-zero-exception-stop-reason>,
      <inexact-result-exception-stop-reason>,
      <invalid-float-operation-exception-stop-reason>,
      <float-overflow-exception-stop-reason>,
      <float-underflow-exception-stop-reason>,
      <float-stack-check-exception-stop-reason>,
      <integer-exception-stop-reason>,
      <integer-divide-by-zero-exception-stop-reason>,
      <noncontinuable-exception-stop-reason>,
      <unclassified-exception-stop-reason>,
      <timeout-stop-reason>,
      <unhandled-stop-reason>,

      ///// Stack Backtraces

      <stack-frame>,
      <function-frame>,
      initialize-stack-trace,
      number-of-frames-on-stack,
      next-frame,
      previous-frame,
      frame-pointer,
      frame-return-address,
      frame-instruction-address,
      older-stack-frame?,
      register-interactive-code-segment,
      link-previous, link-previous-setter,
      link-next, link-next-setter,
      partial-lexicals-read?, partial-lexicals-read?-setter,
      lexicals-count, lexicals-count-setter,
      lexicals-nub-table,
      lexicals, lexicals-setter,
      stack-frame-pointer,
      frame-thread,
      full-lexicals-read?, full-lexicals-read?-setter,
      lexicals-nub-table, lexicals-nub-table-setter,
      next-instruction,

      ///// Frame arguments and lexicals

      <lexical-variable>,
      lexical-variable-name,
      lexical-variable-address,
//    lexical-variable-type, // TODO
      do-frame-arguments,
      do-frame-lexicals,
      find-lexical-variable,

      ///// Symbol Lookup

      <remote-symbol>,
      <remote-function>,
      remote-function-debug-start,
      remote-function-debug-end,
      remote-function-end,
      first-frame-breakable-address,
      last-frame-breakable-address,
      remote-symbol-name,
      remote-symbol-address,
      remote-symbol-language,
//    remote-symbol-type,  // TODO
      remote-symbol-library,
      remote-symbol-object-file,
      remote-symbol-storage-status,
      remote-symbol-source-location-map, remote-symbol-source-location-map-setter,
      definitely-no-source-locations, definitely-no-source-locations-setter,
      do-symbols,
      nearest-symbols,
      find-symbol,
      symbol-relative-address,
      address-within-definition?,
      function-bounding-addresses,
      classify-symbolic-name,

      ///// Symbol languages

      $symbol-language-C,
      $symbol-language-C++,
      $symbol-language-Fortran,
      $symbol-language-MASM,
      $symbol-language-Pascal,
      $symbol-language-BASIC,
      $symbol-language-COBOL,
      $symbol-language-Dylan,

      ///// Instruction Interpretation.

      interpret-instruction-at-current-location,
      $flowLinear,
      $flowCallIndirect,
      $flowCallDirect,
      $flowJumpIndirect,
      $flowJumpDirect,
      $flowReturn,
      $flowInterrupt,
      $flowIllegal,

      ///// Source location maps

      <source-location-map>,
      function-source-location-map,
      source-filename,
      number-of-locations,
      base-linenumber,
      base-address,
      source-location-description,
      nearest-source-locations,
      resolve-source-location,
      function-recorded-source-locations,

      ///// Dylan-specific extensions to the access path
      //    (We need to keep these to a minimum).

      dylan-thread-environment-block-address,
      dylan-thread-mv-buffer-live?,
      dylan-calculate-destination-for-step-into,
      dylan-current-function,

      ///// Profiler Extensions (Keithd)

      <profiler-stop-reason>,
      <profiler-unhandled-stop-reason>;


  create
    create-thread-stop-reason-handler,
    nub-debug-message;

  create
    *debugging-debugger?*,
    make-debugger-stream,
    close-debugger-stream,
    debugger-message,
    debugger-error;

end module;


define module access-path-nub

  create
    remote-value-as-string-on-connection,
    string-as-remote-value-on-connection,
    start-application-on-connection,
    attach-application-on-connection,
    get-process-page-fault-count-on-connection,
    get-thread-cpu-time-on-connection,
    construct-thread-object,
    construct-library-object,
    register-vector-on-connection,
    read-value-from-register,
    read-value-from-memory,
    write-value-to-register,
    write-value-to-memory,
    read-single-float-from-register,
    read-single-float-from-memory,
    write-single-float-to-register,
    write-single-float-to-memory,
    read-double-float-from-register,
    read-double-float-from-memory,
    write-double-float-to-register,
    write-double-float-to-memory,
    read-byte-string-from-memory,
    write-byte-string-to-memory,
    calculate-stack-address-on-connection,
    virtual-page-size-on-connection,
    remote-value-byte-size-on-connection,
    page-read-permission-on-connection?,
    page-write-permission-on-connection?,
    page-relative-address-on-connection,
    perform-coff-relocation-on-connection,
    wait-for-stop-reason-with-timeout,
    wait-for-stop-reason-no-timeout,
    get-debug-event-process-exit-code,
    get-debug-event-thread-exit-code,
    get-debug-event-string-information,
    get-debug-event-library,
    get-debug-event-thread,
    get-debug-event-process,
    get-exception-address,
    exception-is-first-chance?,
    get-exception-violation-address,
    get-exception-violation-op,
    first-debugger-invocation?,
    connection-can-receive-first-chance,
    connection-set-first-chance,
    connection-unset-first-chance,
    connection-thread-stopped-at-first-chance?,
    restart-application,
    kill-app-on-connection,
    register-exit-process-function-on-connection,
    close-application-on-connection,
    stop-application,
    continue-application,
    unhandled-continue-application,
    suspend-application-thread,
    resume-application-thread,
    application-thread-permanently-suspended?,
    application-thread-permanently-suspended?-setter,
    step-application,
    step-over-application,
    step-out-application,
    update-thread-stack-size-on-connection,
    update-thread-stack-trace-on-connection,
    read-frame-lexicals,
    older-stack-frame-on-connection?,
    register-interactive-segment-on-connection,
    set-breakpoint-in-application,
    clear-breakpoint-in-application,
    recover-breakpoint-in-application,
    query-breakpoint-in-application,
    apply-thread-stepping-control-on-connection,
    remove-all-stepping-control-for-thread-on-connection,
    remote-call-on-connection,
    remote-call-result-on-connection,
    remote-restore-context-on-connection,
    remote-call-spy-on-connection,
    nearest-symbols-from-nub,
    collect-nearest-symbols,
    find-symbol-in-library,
    symbol-relative-address-on-connection,
    function-bounding-addresses-on-connection,
    construct-source-location-map,
    resolve-source-location-on-connection,
    interpret-instruction-on-connection,
    calculate-step-into-on-connection,
    teb-on-connection,
    current-function-on-connection,
    mv-buffer-live-on-connection,
    inform-profiling-started-on-connection,
    inform-profiling-stopped-on-connection,
    get-process-wc-time-on-connection;

end module;

define module access-path-implementation
  use functional-dylan;
  use dylan-extensions,
     import: {<machine-word>, 
              <double-integer>, 
              $minimum-unsigned-machine-word, integer-as-raw},
     export: all;
  use dylan-primitives;
  use machine-word-lowlevel;
  use threads, exclude: {thread-name};
  use format;
  use format-out;
  use print;
  use streams, import: {<file-stream>, <stream>, force-output, close};
  use locators, import: {<file-locator>};
  use file-system;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use c-ffi;

  use access-path;
  use access-path-nub;
end module;

