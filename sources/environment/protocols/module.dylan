Module:    Dylan-User
Synopsis:  Environment Protocols
Author:    Andy Armstrong, Chris Page, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//--- Useful module so that all of the environment libraries see the
//--- same set of names, and get the same exclusions.
define module environment-imports
  use functional-dylan,
    exclude: { slot-setter, slot-getter, slot-type, slot-allocation,
	       range-by, range-to,
               method-specializers,
               application-filename, application-arguments },
    export: all;
  use streams, export: all;
  use format,  export: all;
  use threads, export: all;
  use date, export: all;
  use file-system, export: all;
  use operating-system,
    rename: { application-filename => os/application-filename,
	      application-arguments => os/application-arguments,
	      run-application => os/run-application },
    export: all;
  use channels, export: all;
  use settings, export: all;
  use locators, export: all;
  use plists, export: all;
  use machine-words,
    export: { <machine-word> };
  use collectors, export: all;
  use machine-words,
    import: { u%+ => mw/+,
	      u%- => mw/- },
    export: all;

  use memory-manager,
    import: { collect-garbage },
    export: all;

  use source-records, export: all;
  use file-source-records, export: all;
  use release-info, export: all;
  use cl-strings, import: { string-pluralize }, export: { string-pluralize };
end module environment-imports;

define module environment-protocols
  use environment-imports;

  // Server objects
  export <server>,
         <closed-server-error>,
         <invalid-object-error>,
         condition-project,
         condition-object,
         record-client-query,
         server-project;

  // IDs
  export <id>,
         <library-id>,
         <module-id>,
         <definition-id>,
         <method-id>,
         <object-location-id>,
         <library-object-location-id>,
         id-filename,
         id-generic-function,
         id-library,
         id-line-number,
         id-module,
         id-name,
         id-specializers;

  // Environment objects
  export <environment-object>,
         <environment-object-with-id>,
         <environment-object-with-library>,
         note-object-properties-changed,
         environment-object-id,
         environment-object-exists?,
         environment-object-primitive-name, get-environment-object-primitive-name,
         environment-object-basic-name,
         environment-object-display-name,
         environment-object-unique-name,
         environment-object-type,
         environment-object-type-name,
         environment-object-source,
         environment-object-source-location,
         environment-object-home-server?,
         environment-object-home-name,
         environment-object-name,
         environment-object-library,
         find-environment-object,
         make-environment-object,
         parse-environment-object-name,
         parse-module-name,
         print-environment-object,
         print-environment-object-to-string,
         print-environment-object-name,
         print-environment-object-name-to-string,
         source-location-environment-object;

  // Environment options
  export <environment-options>;

  // Compiler objects
  export <compiler-object>,
         compiler-object-proxy,
         invalidate-compiler-proxy;

  // Application objects
  export <application-object>,
         <application-code-object>,
         application-object-class,
         application-object-proxy, application-object-proxy-setter,
         application-object-address,
         invalidate-application-proxy;

  // Unbound objects
  export <unbound-object>,
         $unbound-object;

  // Address objects
  export <address-display-format>,
         <data-display-format>,
         <data-display-size>,
         <address-object>,
         $invalid-address-object,
         address-application-object,
         address-to-string,
         string-to-address,
         indirect-address,
         indexed-address,
         address-read-memory-contents,
         address-read-application-object;
         
  // Register objects
  export <register-category>,
         <register-object>,
         application-registers,
         do-application-registers,
         register-contents,
         register-contents-address,
         lookup-register-by-name;

  // Component objects
  export <component-object>,
         component-image-filename,
         component-version,
         component-version-string,
         lookup-component-by-name,
         application-components,
         do-application-components;

  // Application and compiler objects
  export <application-and-compiler-object>;

  // Composite objects
  export <composite-object>,
         composite-object-size,
         composite-object-contents;

  // User objects
  export <user-object>,
         user-object-slot-value,
         user-object-slot-values;

  // User class info
  export <user-class-info>,
         user-class-info-class,
         user-class-info-id,
         user-object-class-mappings;

  // Internal objects
  export <internal-object>;

  // Foreign objects
  export <foreign-object>;

  // Dylan objects
  export <dylan-object>,
         <dylan-application-object>,
         <immediate-application-object>,
         <dylan-compiler-object>,
         $dylan-library-id,
         $dylan-module-id,
         $dylan-extensions-module-id,
         $dispatch-engine-module-id,
         $<object>-id,
         $<class>-id,
         $<method>-id,
         $<generic-function>-id;

  // Dylan expression objects
  export <expression-object>,
         <type-expression-object>,
         <complex-type-expression-object>;

  // Dylan application objects
  export <character-object>,
         <string-object>,
         <symbol-object>,
         <number-object>,
         <integer-object>,
         number-object-to-string;

  // Boolean objects
  export <boolean-object>,
         boolean-object-true?,
         $true-object,
         $false-object;

  // Collection objects
  export <collection-object>,
         <sequence-object>,
         <explicit-key-collection-object>,
         <array-object>,
         <range-object>,
         <pair-object>,
         collection-size,
         collection-keys,
         collection-elements,
         do-collection-keys,
         do-collection-elements,
         range-start,
         range-end,
         range-by,
         pair-head,
         pair-tail;

  // Source forms
  export <source-form-object>,
         do-used-definitions,
         do-client-source-forms,
         source-form-has-clients?,
         source-form-uses-definitions?,
         source-form-used-definitions,
         source-form-clients;

  // Macro calls
  export <macro-call-object>,
         do-macro-call-source-forms,
         macro-call-source-forms;

  // Non-definition source forms
  export <simple-macro-call-object>,
         <top-level-expression-object>;

  // Definition objects
  export <definition-object>,
         definition-modifiers,
         definition-interactive-locations,
         definition-known-locations,
         find-named-definition;

  // Breakpoints
  export <breakpoint-object>,
         <environment-object-breakpoint-object>,
         <class-breakpoint-object>,
         <function-breakpoint-object>,
         <simple-function-breakpoint-object>,
         <generic-function-breakpoint-object>,
         <method-breakpoint-object>,
         <source-location-breakpoint-object>,
         <breakpoint-state>,
         <breakpoint-direction>,
         $default-breakpoint-stop?,
         $default-breakpoint-message?,
         $default-breakpoint-transient?,
         $default-breakpoint-enabled?,
         $default-breakpoint-profile?,
         $default-breakpoint-test,
         $default-breakpoint-entry-function?,
         $default-breakpoint-directions,
         destroy-breakpoint,
         initialize-breakpoint,
         reinitialize-breakpoint,
         do-generic-breakpoint-methods,
         current-stop-breakpoints,
         find-breakpoint,
         project-breakpoints,
         source-location-breakpoints,
         environment-object-breakpoints,
         breakpoint-object, breakpoint-object-setter, 
         breakpoint-project,
         breakpoint-stop?, breakpoint-stop?-setter,
         breakpoint-message?, breakpoint-message?-setter,
         breakpoint-transient?, breakpoint-transient?-setter,
         breakpoint-enabled?, breakpoint-enabled?-setter,
         breakpoint-profile?, breakpoint-profile?-setter,
         breakpoint-test, breakpoint-test-setter,
         breakpoint-entry-function?, breakpoint-entry-function?-setter,
         breakpoint-entry-point?, breakpoint-entry-point?-setter,
         breakpoint-directions, breakpoint-directions-setter,
         note-breakpoint-state-changed,
         server-note-breakpoint-state-changed,
	 \with-compressed-breakpoint-state-changes,
	 do-with-compressed-breakpoint-state-changes,
         note-breakpoint-state-changes-failed,
         trace-function;

  // Threads objects
  export <thread-object>,
         thread-stack-trace,
         thread-complete-stack-trace,
         thread-index,
         thread-state,
         thread-runtime-state, thread-runtime-state-setter,
         thread-suspended?, thread-suspended?-setter,
         create-application-thread,
         suspend-application-thread,
         resume-application-thread,
         thread-current-interactor-level,
         add-application-object-to-thread-history,
         application-default-interactor-thread;

  // Restarts
  export <restart-object>,
         application-thread-restarts,
         application-restart-message,
         application-restart-abort?,
         invoke-application-restart;

  // Machines
  export <machine>,
         machine-network-address,
         machine-hostname,
         environment-host-machine,
         do-machine-connections,
         close-connection-to-machine,
         machine-connection-open?,
         <remote-debug-connection-error>,
         <remote-connection-closed-error>,
         <remote-connection-failed-error>,
         <remote-connection-password-mismatch-error>,
         failed-connection,
         failed-network-address,
         failed-password,
         <attempted-to-close-local-connection>,

         debug-iterator;   // A function that calls debug-message("%=", x)
                           // for any argument x. From the debugger, this
                           // can be used to test DO-xxx protocols.

  // Processes
  export <process>,
         process-host-machine,
         process-executable-file,
         process-id,
         lookup-process-by-id,
         process-debuggable?,
         do-active-processes,
         do-processes-on-machine; // Not for use by clients, and
                                  // not documented.
  
  // Applications
  export <application>,
         <application-state>,
         <application-startup-option>,
         \with-application-transaction,
         perform-application-transaction,
         application-startup-option,
         application-client,
         application-machine,
         application-filename,
         application-arguments,
         application-temporary-stop?, application-temporary-stop?-setter,
         application-state, application-state-setter,
         application-threads,
         application-running?,
         application-stopped?,
         application-closed?,
         application-tethered?,
         application-debug?,
         application-pause-before-termination?,
         application-just-hit-breakpoint?,
         application-just-hit-dylan-error?,
         application-just-hit-error?,
         application-just-interacted?,
         application-just-stepped?,
         application-stop-reason-message,
         close-application,
         continue-application,
         ensure-application-proxy,
         find-application-proxy,
         application-proxy-id,
         run-application,
         initialize-application-client,
         attach-live-application,
	 note-run-application-failed,
         stop-application,
         make-project-application,
         step-application-into,
         step-application-over,
         step-application-out,
         update-application,
         note-application-initialized;

  // Compiler databases
  export <compiler-database>,
         ensure-database-proxy,
         find-compiler-database-proxy,
         compiler-database-proxy-id,
	 invalidate-compiler-database;

  // Project objects
  export <project-object>,
         <compilation-mode>,
         <project-target-type>,
         <project-interface-type>,
         active-project, active-project-setter,
         project-name,
         project-properties, project-properties-setter,
         project-proxy, project-proxy-setter,
         project-application, project-application-setter,
         project-compiler-database, project-compiler-database-setter,
         project-database-changed?, project-sources-changed?,
         project-opened-by-user?, project-opened-by-user?-setter,
         project-used-libraries,
         project-used-projects,
         do-project-used-libraries,
         do-project-file-libraries,
         do-used-projects,
         edit-source-location,
         edit-source-record,
         edit-definition,
         open-project,
         find-project,
         create-new-user-project,
         open-project-from-file,
         create-exe-project-from-file,
         import-project-from-file,
         close-project,
         project-add-source-record,
         project-remove-source-record,
         project-reorder-source-records,
         save-project,
         save-project-database,
         open-projects,
         current-project, current-project-setter,
         project-library,
         project-start-function-name, project-start-function-name-setter,
         project-start-function,
         project-read-only?,
         project-can-be-built?,
         project-can-be-debugged?,
         project-compiled?,
         project-sources,
         project-canonical-sources,
         project-canonical-source-record,
         project-canonical-filename,
         project-other-sources,
         project-directory,
         project-filename,
         project-build-filename, project-build-filename-setter,
         project-full-build-filename,
         project-debug-filename, project-debug-filename-setter,
         project-debug-arguments, project-debug-arguments-setter,
         project-debug-machine-address, project-debug-machine-address-setter,
         project-debug-machine, project-debug-machine-setter,
         project-debug-directory, project-debug-directory-setter,
         project-build-directory,
         project-bin-directory,
         project-release-directory,
         project-server-path,
         project-compilation-mode, project-compilation-mode-setter,
         project-compiler-back-end, project-compiler-back-end-setter,
         project-target-type, project-target-type-setter,
         project-interface-type, project-interface-type-setter,
         project-base-address, project-base-address-setter,
         project-major-version, project-major-version-setter,
         project-minor-version, project-minor-version-setter,
         find-project-source-record,
         find-source-record-library,
         session-property,
         session-property-setter,
         source-record-top-level-forms,
         source-record-projects,
         source-record-colorization-info,
         open-project-compiler-database,
         parse-project-source,
         build-project,
         remove-project-build-products,
         default-build-script, default-build-script-setter,
         link-project,
         note-user-project-opened;

  // Playground
  export find-playground-project,
         playground-project-name,
         playground-project?,
         playground-application-filename;

  // File extensions
  export environment-locator-type,
         executable-file-extension,
         project-file-extension,
         lid-file-extension,
         dylan-file-extension;

  // Interactive evaluation
  export project-execute-code, project-macroexpand-code,
         project-valid-code?,
         project-runtime-context,
         record-return-values,
         application-state-at-code-entry,
         <execution-id>,
         <execution-info>,
         execution-info-id,
         execution-info-thread,
         project-execution-info,
         project-remove-execution-info,
         project-bind-variable,
         fetch-interactor-return-values,
         dispose-interactor-return-values,
	 transaction-id-source-record;

  // Name objects
  export <name-object>,
         <module-name-object>,
         <binding-name-object>,
         name-value,
         name-type,
         name-imported?,
         name-exported?;

  // Namespace objects
  export <namespace-object>,
         find-name,
         name-namespace,
         namespace-names,
         do-namespace-names;

  // Library objects
  export <library-object>,
         find-library,
         library-modules,
         library-default-module,
         library-project,
         library-project-filename,
         library-filename,
         library-interactive?,
         library-read-only?, library-read-only?-setter,
         do-library-modules;

  // Module objects
  export <module-object>,
         file-module,
         find-module,
         module-project-proxy,
         do-module-definitions,
         module-definitions;

  // Macros
  export <macro-object>;

  // Variable objects
  export <variable-object>,
         <module-variable-object>,
         <global-variable-object>,
         <thread-variable-object>,
         variable-type,
         variable-value;

  // Constant objects
  export <constant-object>;

  // Functions
  export <function-object>,
         <foreign-function-object>,
         <dylan-function-object>,
         <simple-function-object>,
         <generic-function-object>,
         <method-object>,
         <method-constant-object>,
         <internal-method-object>,
         <parameter>, <parameters>,
         <optional-parameter>, <optional-parameters>,
         function-parameters,
         do-generic-function-methods,
         generic-function-object-methods,
         method-generic-function,
         method-specializers,
         parameter-name,
         parameter-type,
         parameter-keyword,
         parameter-default-value;

  // Domains
  export <domain-object>,
         domain-specializers;

  // Type objects
  export <type-object>;

  // Singleton objects
  export <singleton-object>,
         singleton-value;

  // Classes
  export <class-object>,
         do-direct-subclasses,
         do-direct-superclasses,
         do-direct-methods,
         do-direct-slots,
         do-all-methods,
         do-all-superclasses,
         do-all-slots,
         do-init-keywords,
         class-direct-subclasses,
         class-direct-superclasses,
         class-direct-methods,
         class-direct-slots,
         class-slots;

  // Slots
  export <slot-object>,
         slot-class,
         slot-getter,
         slot-setter,
         slot-type,
         slot-init-kind,
         slot-init-keyword,
         slot-allocation;

  // Local variable objects
  export <local-variable-object>;

  // Stack frame objects
  export <stack-frame-object>,
         stack-frame-function,
         stack-frame-environment-object,
         stack-frame-source-location,
         stack-frame-thread,
         stack-frame-type,
         stack-frame-next-frame,
         stack-frame-previous-frame,
         stack-frame-local-variables,
         stack-frame-local-variable-count;

  // Compiler warnings
  export <warning-object>,
         <project-warning-object>,
         <compiler-warning-object>,
         <serious-compiler-warning-object>,
         <compiler-error-object>,
         compiler-warning-short-message,
         compiler-warning-full-message,
         environment-object-proxy,
         warning-owner,
         do-compiler-warnings,
         project-warnings,
         source-form-compiler-warnings;

  // Condition objects
  export <condition-object>;

  // DUIM objects
  export <duim-object>,
         <duim-frame-manager>,
         <duim-frame>,
         <duim-port>,
         <duim-sheet>,
         <duim-gadget>,
         duim-object-composite?,
         do-duim-object-children,
         duim-object-children;

  // Profiling
  export <profile-state>,
         profiling-enabled?,
         project-default-profile-options, 
	   project-default-profile-options-setter,
         project-last-profile,
         profile-snapshot-available-values,
         start-profiling-application,
         ensure-profiling-started,
         stop-profiling-application,
         clear-profiling-results,
         process-profiling-results;

  // Profiling options
  export <profile-options>,
         <profile-sampling-options>,
         <profile-sampling-style>,
	 <profile-snapshot-options>,
         profile-sampling-options,
         profile-sampling-style,
         profile-sampling-rate,
         profile-snapshot-options,
	 profile-snapshot-values,
	 profile-snapshot-stack-depth;

  // Profile results
  export <application-profile>,
         application-profile-options,
         application-profile-snapshots,
         do-application-profile-snapshots,
         application-profile-threads,
         do-application-profile-threads,
         application-total-snapshots,
         application-total-wall-time, application-total-wall-time-setter,
         application-total-page-faults, application-total-page-faults-setter,
         <application-snapshot>,
         application-snapshot-wall-time,
         application-snapshot-page-faults,
         application-snapshot-thread-snapshot,
         application-snapshot-thread-snapshots,
         do-application-snapshot-thread-snapshots,
         thread-profile-snapshots,
         do-thread-profile-snapshots,
         <thread-snapshot>,
         thread-snapshot-thread,
         thread-snapshot-cpu-time,
         thread-snapshot-allocated-class,
         thread-snapshot-allocation,
         thread-snapshot-stack-size,
         thread-snapshot-frame-snapshots,
         process-thread-snapshot-frame-snapshots,
         thread-snapshot-functions,
         do-thread-snapshot-functions,
         <thread-frame-snapshot>,
         frame-snapshot-function,
         frame-snapshot-source-location;

  // Channels
  export $project-channel,
         <project-message>,
         <project-object-message>,
         <project-opened-message>,
         <project-closed-message>,
         <project-now-active-message>,
         <no-active-project-message>,
         <project-sources-updated-message>,
         <project-database-updated-message>,
         <project-warnings-updated-message>,
         message-project,
         <breakpoint-state-change-message>,
         <all-breakpoints-state-change-message>,
         <single-breakpoint-state-change-message>,
         <breakpoint-state-changes-failed-message>,
         message-breakpoint-state,
         message-breakpoint,
         message-breakpoints,
         <profiling-message>,
         <profiling-state-change-message>,
         message-enabled?;

  // Application messages
  export <application-message>,
         <run-application-requested-message>,
         <run-application-failed-message>,
         <application-initialized-message>,
         <application-state-changed-message>,
         <application-threads-changed-message>,
         <thread-message>,
         <thread-interactive-warnings-message>,
         message-thread,
         message-transaction-id,
         message-warnings;

  // Printing utilities
  export print-source-location,
         application-state-label,
         thread-state-label;

  export environment-object-contents,
         environment-object-description,
         print-function-parameters,
         print-function-values,
         print-environment-object-location;


  export $n/a,
         $type-n/a,
         $no-information-available,
         $unknown-name,
         $unknown,
         $project-not-built,
         $interactive-definition,
         $not-available,
         $not-applicable;
end module environment-protocols;
