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
   use format;

   create

      ///// <access-path>

      <access-path>,
      <access-path-creation-error>,
      access-path-abstract-handle,
      access-path-abstract-handle-setter,
      access-path-application,
      access-path-arguments,
      access-path-process,
      access-path-core-file,

      ///// Remote Objects

      <debugger-connection>,
      host-machine,
      do-processes,
      <remote-process>,
      
      <remote-thread>,
      do-threads,
      number-of-active-threads,
      thread-name,
      thread-state,
      thread-priority,
      thread-suspended?,

      <remote-library>,
      do-libraries,
      library-version,
      library-image-name,
      library-core-name,

      ///// <remote-value>

      <remote-value>,
      as-integer,
      as-remote-value,
      indexed-remote-value,
      tagged-remote-value-as-integer,
      tagged-remote-value-as-character,
      integer-as-tagged-remote-value,
      character-as-tagged-remote-value,
      byte-indexed-remote-value,
      remote-value-=,
      remote-value-<,
      remote-value-<=,
      as-integer-losing-precision,
      as-remote-value-losing-precision,
      remote-value-as-string,
      remote-value-byte-size,

      ///// Register and memory access

      <remote-register>,
      <unassigned-remote-register>,
      <active-remote-register>,
      register-name,
      register-type,
      do-registers,
      active-register,
      <remote-location>,
      <remote-type>,

      page-read-permission?,
      page-write-permission?,
      page-execute-permission?,
      remote-virtual-page-size,

      calculate-stack-address,
      page-relative-address,
      perform-coff-relocation,

      <remote-access-violation-error>,
      read-value, write-value,
      read-8b, write-8b,
      read-16b, write-16b,
      read-32b, write-32b,
      read-64b, write-64b,
      read-single-float, write-single-float,
      read-double-float, write-double-float,
      read-byte-string, write-byte-string,
      read-byte-vector, write-byte-vector,
      read-unicode-string, write-unicode-string,

      ///// Application Control

      restart,
      stop,
      continue,
      continue-handled,
      suspend-thread,
      resume-thread,
      step,
      step-over,
      step-out,
      application-state-running?,
      application-state-stopped?,
      application-state-unstarted?,
      application-state-post-mortem?,
      kill-application,

      ///// Remote Function Calling

      remote-call,
      remote-call-result,
      remote-restore-context,
      remote-call-spy,

      ///// Breakpoints and Watchpoints

      enable-breakpoint, disable-breakpoint, query-breakpoint?,
      enable-read-watchpoint, disable-read-watchpoint, query-read-watchpoint?,
      enable-write-watchpoint, disable-write-watchpoint, 
      query-write-watchpoint?,
      enable-execute-watchpoint, disable-execute-watchpoint, 
      query-execute-watchpoint?,

      ///// <stop-reason>

      wait-for-stop-reason,
      process-stop-reasons,
      stop-processing-stop-reasons,
      flush-all-stop-reasons,

      <stop-reason>,
      <internal-stop-reason>,
      <language-level-stop-reason>,
      <basic-stop-reason>,
      <external-stop-reason>,
      stop-reason-process,
      stop-reason-thread,
      <process-stop-reason>,
      <create-process-stop-reason>,
      <exit-process-stop-reason>,
      stop-reason-process-exit-code,
      <thread-stop-reason>,
      <create-thread-stop-reason>,
      stop-reason-thread-exit-code,
      <exit-thread-stop-reason>,
      <library-stop-reason>,
      stop-reason-library,
      <load-library-stop-reason>,
      <unload-library-stop-reason>,
      <RIP-stop-reason>,
      stop-reason-exit-code,
      <debug-point-stop-reason>,
      stop-reason-debug-point-address,
      <breakpoint-stop-reason>,
      <break-stop-reason>,
      <single-step-stop-reason>,
      <watchpoint-stop-reason>,
      <read-watchpoint-stop-reason>,
      <write-watchpoint-stop-reason>,
      <execute-watchpoint-stop-reason>,
      <exception-stop-reason>,
      <invoke-debugger-stop-reason>,
      <memory-exception-stop-reason>,
      <access-violation-stop-reason>,
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
      <output-debug-string-stop-reason>,
      stop-reason-debug-string,
      <profiler-stop-reason>,
      first-chance-receivable-exceptions,
      receiving-first-chance?,
      receiving-first-chance?-setter,
      exception-name,

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

      ///// Frame arguments and lexicals

      <lexical-variable>,
      lexical-variable-name,
      lexical-variable-address,
      lexical-variable-type,
      do-frame-arguments,
      do-frame-lexicals,
      find-lexical-variable,
      live-lexicals-count,

      ///// Symbol Lookup

      <remote-symbol>,
      remote-symbol-name,
      remote-symbol-address,
      remote-symbol-type,
      remote-symbol-library,
      remote-symbol-language,
      do-symbols,
      nearest-symbols,
      find-symbol,
      symbol-relative-address,
      first-frame-breakable-address,
      last-frame-breakable-address,
      address-within-definition?,
      function-bounding-addresses,
      $symbol-language-C, $symbol-language-C++,
      $symbol-language-Fortran, $symbol-language-Dylan,
      $symbol-language-MASM, $symbol-language-BASIC,
      $symbol-language-Pascal, $symbol-language-COBOL,

      ///// Source Location Mapping.

      <source-location-map>,
      source-filename,
      number-of-locations,
      base-linenumber,
      base-address,
      source-location-description,
      nearest-source-locations,
      function-source-location-map,
      resolve-source-location,

      ///// Interpreting Call Instructions

      $flowLinear, $flowCallDirect, $flowCallIndirect, $flowJumpIndirect,
      $flowJumpDirect, $flowReturn, $flowInterrupt, $flowIllegal,
      interpret-instruction-at-current-location,

      ///// Low level Dylan-specific APIs

      dylan-current-unwind-protect-frame,
      dylan-unwind-protect-frame-contents,
      dylan-bind-exit-frame-contents,
      dylan-argument-registers,
      dylan-current-function,
      dylan-installed-handlers,
      dylan-resolve-keyword,

      ///// Profiler Extensions (keithd)
      inform-profiling-started,
      inform-profiling-stopped;

end module;


define module access-path-implementation

       use functional-dylan;
       use format;
       use access-path;

end module;

