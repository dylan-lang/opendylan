module:       devel-dbg-ui
synopsis:     A tty user interface for the Devel Dylan Debugger.
author:       Paul "Debugger" Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <DEBUGGER-COMMAND>


define abstract class <debugger-command> (<object>)
end class;

define class <null-command> (<debugger-command>)
end class;


define abstract class <context-debugger-command> (<debugger-command>)
end class;

define abstract class <zero-context-debugger-command> (<debugger-command>)
end class;

define class <cry-in-the-dark-command> (<zero-context-debugger-command>)
end class;

define class <bad-command> (<debugger-command>)
end class;

define class <last-command-command> (<debugger-command>)
end class;


///// <ALTER-CONTEXT-COMMAND>


define abstract class <alter-context-command> (<context-debugger-command>)
end class;

define class <exceptions-command> (<alter-context-command>)
end class;

define class <change-thread-command> (<alter-context-command>)

       constant slot new-thread :: <integer>,
            required-init-keyword: to:;

end class;

define class <change-library-command> (<alter-context-command>)

       constant slot new-library :: <string>,
            required-init-keyword: to:;

end class;

define class <change-module-command> (<alter-context-command>)

       constant slot new-module :: <string>,
            required-init-keyword: to:;

end class;

define class <change-language-command> (<alter-context-command>)
end class;

define class <mode-C-command> (<change-language-command>)
end class;

define class <mode-Dylan-command> (<change-language-command>)
end class;


///// <OPEN-APPLICATION-COMMAND>

define abstract class <establish-tether-command>
    (<zero-context-debugger-command>)
  constant slot command-connection :: <debugger-connection>,
    init-keyword: connection:,
    init-function: host-machine;
end class;

define class <enumerate-processes-command> (<establish-tether-command>)
end class;

define class <open-application-command> (<establish-tether-command>)

       constant slot application-filename :: <string>,
            required-init-keyword: filename:;

       constant slot application-command-arguments :: <string>,
            init-value: "",
            init-keyword: arguments:;

end class;

define class <attach-application-command> (<establish-tether-command>)
  constant slot application-process-id :: <string>,
    required-init-keyword: id:;
  constant slot system-JIT-information :: <string>,
    init-value: "",
    init-keyword: JIT-bit:;
end class;

define method application-filename
    (c :: <attach-application-command>) => (n :: <string>)
  format-to-string
     ("Attached process <%s>", c.application-process-id)
end method;

define method application-command-arguments
    (c :: <attach-application-command>) => (n :: <string>)
  "(Command arguments not obtainable - didn't launch from debugger.)"
end method;

/*
define class <open-and-interact-command> (<open-application-command>)

  constant slot application-target-library :: <string>,
    init-value: "dylan",
    init-keyword: library:;

  constant slot application-target-module :: <string>,
    init-value: "dylan",
    init-keyword: module:;

end class;
*/

///// ADDITIONAL COMMANDS FOR PROFILING APPLICATIONS

define class <profile-application-command> (<open-application-command>)
end class;

define abstract class <profiler-command> (<context-debugger-command>)
end class;

define abstract class <profiler-begin-command> (<profiler-command>)

  constant slot profiler-command-threads :: false-or(<sequence>),
    init-value: #f,
    init-keyword: threads:;

  constant slot profiler-command-options :: <profiler-options>,
    required-init-keyword: options:;

end class;

define class <start-profiling-open-application-command>
                (<profiler-begin-command>)
end class;

define class <profile-within-invocation-command>
                (<profiler-begin-command>)

  constant slot profiler-command-function :: <simple-expression>,
    required-init-keyword: function:;

end class;

define class <profile-debug-command> (<profiler-command>)
end class;

define class <profile-aggregates-command> (<profiler-command>)

  constant slot profile-aggregates-command-arguments :: <sequence>,
    required-init-keyword: arguments:;

end class;

define class <profile-aggregates-1-command> (<profile-aggregates-command>)
end class;

define class <profile-aggregates-2-command> (<profile-aggregates-command>)
end class;

define class <profile-filter-command> (<profiler-command>)

  constant slot profile-filter-command-argument :: false-or(<profile-set>),
    required-init-keyword: argument:;

end class;

define class <profile-filter-1-command> (<profile-filter-command>)
end class;

define class <profile-filter-2-command> (<profile-filter-command>)
end class;

define class <profile-set-command> (<profiler-command>)

  constant slot profile-set-command-name :: <symbol>,
    required-init-keyword: name:;

  constant slot profile-set-command-set :: false-or(<profile-set>),
    required-init-keyword: set:;

end class;

define class <profile-height-command> (<profiler-command>)

  constant slot profile-height-command-argument :: false-or(<integer>),
    required-init-keyword: argument:;

end class;

define class <profile-limit-command> (<profiler-command>)

  constant slot profile-limit-command-argument :: false-or(<single-float>),
    required-init-keyword: argument:;

end class;

define class <profile-limit-0-command> (<profile-limit-command>)

end class;

define class <profile-limit-1-command> (<profile-limit-command>)

end class;

define class <profile-limit-2-command> (<profile-limit-command>)

end class;

define class <profile-top-n-command> (<profiler-command>)

  constant slot profile-top-n-command-argument :: false-or(<integer>),
    required-init-keyword: argument:;

end class;

define class <profile-top-n-0-command> (<profile-top-n-command>)

end class;

define class <profile-top-n-1-command> (<profile-top-n-command>)

end class;

define class <profile-top-n-2-command> (<profile-top-n-command>)

end class;

define class <profile-inclusive-command> (<profiler-command>)

  constant slot profile-inclusive-command-argument,
    required-init-keyword: argument:;

end class;

define class <profile-exclusive-command> (<profiler-command>)

  constant slot profile-exclusive-command-argument,
    required-init-keyword: argument:;

end class;

define class <profile-sets-command> (<profiler-command>)

end class;

define class <profile-show-aggregates-command> (<profiler-command>)
  constant slot profile-show-aggregates-command-argument :: <simple-expression>,
    required-init-keyword: argument:;

end class;

define class <stop-profiling-open-application-command>
                (<profiler-command>)
end class;

define class <stop-profiling-and-show-hotspots-command>
                (<profiler-command>)
end class;

define class <stop-profiling-and-ignore-results-command>
                (<profiler-command>)
end class;


///// <VIEW-MULTIPLE-VALUES-COMMAND>
//    The command to display the thread-local multiple values as dylan
//    objects.

define class <view-multiple-values-command> (<context-debugger-command>)
end class;


///// <SET-COMMAND>
//    The command to write a value into an address in memory.

define class <set-command> (<context-debugger-command>)

  constant slot set-command-lhs :: <simple-expression>,
    required-init-keyword: lhs-expression:;

  constant slot set-command-rhs :: <simple-expression>,
    required-init-keyword: rhs-expression:;

end class;


///// <EXIT-DEBUGGER-COMMAND>


define class <exit-debugger-command> (<zero-context-debugger-command>)
end class;


///// <APPLICATION-CONTROL-COMMAND>


define abstract class <application-control-command> 
     (<context-debugger-command>)
end class;

define class <restart-command> (<application-control-command>)
end class;

define class <continue-command> (<application-control-command>)
end class;

define class <continue-via-restart-command> (<continue-command>)

  constant slot command-restart-index :: <integer>,
    required-init-keyword: index:;

end class;

define class <enumerate-restarts-command> (<context-debugger-command>)
end class;

define class <thread-control-command> (<application-control-command>)

       constant slot thread-number :: <integer>,
            required-init-keyword: thread-number:;

end class;

define class <suspend-command> (<thread-control-command>)
end class;

define class <resume-command> (<thread-control-command>)
end class;

define class <step-command> (<application-control-command>)
end class;

define class <step-over-command> (<step-command>)
end class;

define class <step-into-command> (<step-command>)
end class;

define class <step-out-command> (<step-command>)
end class;

define class <kill-command> (<application-control-command>)
end class;


///// <SOURCE-LOCATION-COMMAND>

define abstract class <source-location-command>
                        (<context-debugger-command>)
end class;

define class <where-command> (<source-location-command>)
end class;


///// <NEARTO-COMMAND>

define class <nearto-command> (<context-debugger-command>)

  constant slot target-address :: <simple-expression>,
    required-init-keyword: target:;

end class;


///// <STACK-COMMAND>


define abstract class <stack-command> (<context-debugger-command>)
end class;

/*
define class <filter-command> (<stack-command>)

  // We have the commands "filter" and "reveal", both represented by this
  // class (even though it is named <filter-command>). The value of this
  // slot decides between a filter and a reveal. This is simpler than
  // using a load of mixin classes.

  constant slot filtering? :: <boolean>,
    init-value: #f,
    init-keyword: filtering?:;

end class;

define class <filter-context-command> (<filter-command>)

  constant slot filtered-module :: <string>,
    required-init-keyword: module:;

  constant slot filtered-library :: <string>,
    required-init-keyword: library:;

end class;

define class <filter-dll-command> (<filter-command>)

  constant slot filtered-dll :: <remote-library>,
    required-init-keyword: dll:;

end class;

define class <filter-foreign-command> (<filter-command>)
end class;

define class <filter-cleanups-command> (<filter-command>)
end class;

define class <filter-primitives-command> (<filter-command>)
end class;
*/

define class <backtrace-command> (<stack-command>)

  constant slot verbose-trace? :: <boolean>,
    init-value: #f,
    init-keyword: verbose?:;

end class;

define class <bug-report-command> (<context-debugger-command>)
end class;

define class <shutup-command> (<context-debugger-command>)
end class;

define class <limited-backtrace-command> (<backtrace-command>)

  constant slot frame-count-limit :: <integer>,
    required-init-keyword: limit:;

end class;

define class <frame-command> (<stack-command>)
end class;

define class <into-frame-command> (<stack-command>)

       constant slot frame-index :: <integer>,
            required-init-keyword: index:;

end class;

define class <up-command> (<stack-command>)
end class;

define class <down-command> (<stack-command>)
end class;

define class <top-command> (<stack-command>)
end class;

define class <bottom-command> (<stack-command>)
end class;


///// <DOWNLOAD-OBJECT-FILES-COMMAND>

define class <download-object-files-command> (<context-debugger-command>)

  constant slot command-object-filename-sequence :: <sequence>,
    required-init-keyword: filenames:;

end class;


///// <DEBUG-POINT-COMMAND>


define abstract class <debug-point-command> (<context-debugger-command>)
end class;

define abstract class <cookie-breakpoint-command> (<debug-point-command>)

  constant slot command-breakpoint-cookie :: <integer>,
    required-init-keyword: cookie:;

end class;

define class <blanket-ignore-command> (<cookie-breakpoint-command>)
end class;

define class <counted-ignore-command> (<cookie-breakpoint-command>)

  constant slot command-ignore-count :: <integer>,
    required-init-keyword: count:;

end class;

define class <disable-breakpoint-command> (<cookie-breakpoint-command>)
end class;

define class <enable-breakpoint-command> (<cookie-breakpoint-command>)
end class;

define class <clear-cookie-breakpoint-command> (<cookie-breakpoint-command>)
end class;

define class <breakpoints-command> (<debug-point-command>)
end class;

define class <break-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;

define class <break-source-command> (<debug-point-command>)

  constant slot breakpoint-source-filename :: <string>,
    required-init-keyword: file:;

  constant slot breakpoint-source-linenumber :: <integer>,
    required-init-keyword: line:;

  constant slot breakpoint-source-column :: <integer>,
    required-init-keyword: column:;

  constant slot breakpoint-source-dll :: <string>,
    required-init-keyword: dll:;

end class;

define class <break-class-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;

define class <clear-source-command> (<debug-point-command>)

  constant slot clear-source-filename :: <string>,
    required-init-keyword: file:;

  constant slot clear-source-linenumber :: <integer>,
    required-init-keyword: line:;

  constant slot clear-source-column :: <integer>,
    required-init-keyword: column:;

end class;

define class <trace-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;

define class <clear-command> (<debug-point-command>)
end class;

define class <clear-specific-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;

define class <clear-class-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;

define class <clear-all-class-command> (<debug-point-command>)
end class;

define class <untrace-command> (<debug-point-command>)
end class;

define class <untrace-specific-command> (<debug-point-command>)

       constant slot address :: <simple-expression>,
            required-init-keyword: address:;

end class;


///// <DISPLAY-COMMAND>


define abstract class <display-command> (<context-debugger-command>)

       constant slot starting-address :: <simple-expression>,
            required-init-keyword: from:;

       constant slot line-count :: <integer>,
            init-value: 8,
            init-keyword: lines:;

       constant slot display-format :: <symbol>,
            init-value: #"word",
            init-keyword: as:;

end class;

define class <display-from-command> (<display-command>)
end class;

///// <PRINT-INSPECT-COMMAND>

define class <add-print-directive-command> (<context-debugger-command>)

  constant slot new-directive :: <debugger-print-directive>,
    required-init-keyword: new-directive:;

end class;

define abstract class <print-inspect-command> (<context-debugger-command>)

       constant slot object :: <simple-expression>,
            required-init-keyword: object:;

end class;

define class <print-command> (<print-inspect-command>)
end class;

define class <describe-command> (<print-inspect-command>)
end class;

define class <explode-command> (<print-inspect-command>)
end class;

define class <walk-command> (<print-inspect-command>)

  constant slot command-class-set :: <sequence>,
    init-value: #[],
    init-keyword: classes:;

  constant slot command-module-set :: <sequence>,
    init-value: #[],
    init-keyword: modules:;

  constant slot command-class-status :: <symbol>,
    init-value: #"trace-everything",
    init-keyword: class-status:;

  constant slot command-module-status :: <symbol>,
    init-value: #"trace-everything",
    init-keyword: module-status:;

end class;

define class <evaluate-expression-command> (<context-debugger-command>)

  constant slot expression :: <debugger-expression>,
    required-init-keyword: expression:;

end class;


///// <ENUMERATE-COMMAND>


define abstract class <enumerate-command> (<context-debugger-command>)
end class;

define class <enumerate-threads-command> (<enumerate-command>)
end class;

define class <enumerate-libraries-command> (<enumerate-command>)
end class;

define class <enumerate-registers-command> (<enumerate-command>)
end class;


///// A command to invoke the compiler.

define class <drop-into-compiler-command> (<context-debugger-command>)
end class;

define class <execute-source-command> (<context-debugger-command>)

  constant slot command-source-string :: false-or(<string>),
    init-value: #f,
    init-keyword: source-string:;

end class;

define class <switch-listener-modes-command> (<context-debugger-command>)
end class;


///// REMOTE DEBUGGING COMMANDS

define abstract class <remote-tether-command> (<context-debugger-command>)
end class;

define class <enumerate-open-connections-command> (<remote-tether-command>)
end class;

define class <attempt-connection-command> (<remote-tether-command>)
  constant slot command-network-address :: <string>,
    required-init-keyword: network-address:;
  constant slot command-password :: <string>,
    required-init-keyword: password:;
end class;

