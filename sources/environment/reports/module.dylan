Module:    Dylan-User
Synopsis:  The reports provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-reports
  use environment-imports,
    exclude: { info-name };
  use environment-protocols;
  use environment-manager;
  use licensing, 
    import: { license-info };

  // Report protocols
  export <report>,
         <report-format>,
         <bug-report>,
         <project-report>,
         <report-error>,
         create-multi-file-report,
         create-report-to-string,
         write-report;

  // Report info
  export <report-info>,
         available-reports,
         find-report-info,
         report-info-name,
         report-info-title,
         report-info-class,
         report-info-edition,
         report-info-formats,
         report-info-format-name,
         report-info-multi-file?;

  // Bug reporting
  export <bug-report>,
         write-bug-report-thread-backtrace,
         write-bug-report-stack-frame;

  // Library reporting
  export <library-report>,
         <module-report>;

  // Profiling
  export <profile-summary-info>,
         <profile-thread-info>,
         <profile-function-info>,
         <profile-class-info>,
         process-profile-summary,
         process-profile-call-history,
         info-class,
         info-count,
         info-thread,
         info-threads,
         info-objects,
         info-function,
         info-allocation,
         info-page-faults,
         info-cpu-time,
         info-wall-time;

  // Profile Call history
  export <profile-call-history>,
         <profile-frame-history>,
         <profile-frame-allocated-class>,
         call-history-root-references,
         call-history-total-cpu-time,
         call-history-total-wall-time,
         profile-frame-frame,
         profile-frame-references,
         profile-frame-start-cpu-time,
         profile-frame-start-wall-time,
         profile-frame-cpu-time,
         profile-frame-wall-time,
         profile-frame-allocation,
         profile-frame-allocated-class,
         profile-frame-source-location;
end module environment-reports;
