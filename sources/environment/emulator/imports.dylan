Module:    emulator-environment-backend
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro lisp-interface-definer
  { define lisp-interface ?imports end }
    => { ?imports }
imports:
  { } => { }
  { ?import; ... } => { ?import; ... }
import:
  { functions ?import-specs }
    => { import-cl-functions(?import-specs) }
  { values ?import-specs }
    => { import-cl-values(?import-specs) }
  { classes ?import-specs }
    => { import-cl-classes(?import-specs) }
import-specs:
  { } => { }
  { ?import-spec, ... } => { ?import-spec, ... }
import-spec:
  { ?symbol:name from ?package:name as ?alias:name }
    => { ?package(?symbol) (as: ?alias) }
  { ?symbol:name from ?package:name }
    => { ?package(?symbol) (as: ?symbol) }
  { ?symbol:name as ?alias:name }
    => { ?symbol (as: ?alias) }
  { ?symbol:name }
    => { ?symbol (as: ?symbol) }
end macro;


// Lisp
define lisp-interface
  functions dylan-boolean-value from cl-user;
  functions print-emulator-object from cl-user,
            object-slot-contents from cl-user,
            object-slot-value from cl-user;
  functions find-dylan-variable from cl-user,
            dylan-variable-value from cl-user,
            dylan-variable-module from cl-user,
            dylan-variable-imported? from cl-user,
            dylan-variable-exported? from cl-user,
            dylan-slot-contents from cl-user,
            dylan-slot? from cl-user;
  functions dylan-object-class from cl-user,
            dylan-object-source-location from cl-user,
            dylan-class? from cl-user,
            dylan-class-name from cl-user,
            dylan-class-initargs from cl-user;
  functions all-dylan-libraries from cl-user,
            all-dylan-modules from cl-user,
            dylan-module-name from cl-user,
            dylan-module-library from cl-user,
            dylan-module-used-modules from cl-user,
            dylan-module-client-modules from cl-user,
            do-dylan-module-variables from cl-user,
            find-dylan-module from cl-user,
            dylan-module? from cl-user;
  functions dylan-symbol? from cl-user,
            dylan-constant? from cl-user,
            dylan-symbol-name from cl-user, 
            dylan-function-name from cl-user,
            dylan-function-parameters from cl-user,
            dylan-generic-function-methods from cl-user,
            dylan-method-specializers from cl-user,
            dylan-singleton-value from cl-user;
  functions dylan-range-contents from cl-user;
  functions find-dylan-project from cl-user,
            dylan-project-name from cl-user,
            dylan-project-directory from cl-user,
            dylan-project-libraries from cl-user,
            dylan-project-library from cl-user,
            dylan-project-contents from cl-user,
            file-truename from cl-user,
            edit-source-file from cl-user,
            compile-dylan-project-file from cl-user,
            load-dylan-project-file from cl-user,
            dylan-project-file-loaded? from cl-user,
            dylan-project-file-compiled? from cl-user;
  functions find-dylan-library from cl-user,
            dylan-library-name from cl-user,
            dylan-library-used-libraries from cl-user,
            dylan-library-client-libraries from cl-user,
            do-dylan-library-modules from cl-user;
  functions do-dylan-definitions-used-definitions from cl-user,
            do-dylan-definitions-client-definitions from cl-user;
  functions do-dylan-compiler-warnings from cl-user,
            dylan-warning-message from cl-user,
            dylan-warning-definition from cl-user;
  classes   standard-method from cl as <cl-standard-method>;
  classes   dylan-library from cl-user as <dylan-library>,
            <translator-module> from dylan as <dylan-module>;
end;

/*--- This is no longer used!
// Debugger support
define lisp-interface
  functions stack-frame-function from cl-user,
	    function-name from cl-user,
	    stack-frame-next-call-frame from cl-user,
	    stack-frame-next-active-frame from cl-user,
	    stack-frame-previous-call-frame from cl-user,
	    stack-frame-previous-active-frame from cl-user,
	    stack-frame-previous-frame from cl-user,
	    stack-frame-previous-interesting-frame from cl-user,
	    stack-frame-interesting-p from cl-user as stack-frame-interesting?,
	    stack-frame-active-p from cl-user as stack-frame-active?,
	    stack-frame-value-disposition from cl-user,
	    stack-frame-relative-exit-pc from cl-user,
	    stack-frame-argument-value from cl-user,
	    stack-frame-local-value from cl-user,
	    stack-frame-number-of-locals from cl-user,
	    stack-frame-rest-arg-value from cl-user,
	    stack-frame-argument-name from cl-user,
	    stack-frame-rest-arg-name from cl-user,
	    stack-frame-local-name from cl-user,
	    eval-in-frame-environment from cl-user,
	    dylan-frame-p from cl-user as dylan-stack-frame?,
	    dylan-function-p from cl-user as dylan-function?,
            return-from-stack-frame from cl-user,
            restart-stack-frame from cl-user,
            set-trap-on-exit from cl-user;
end;
*/

define lisp-interface
  functions install-notifier-function from cl-user,
            install-debugger-function from cl-user,
            invoke-restart-interactively from conditions;
end;

// CLOS
define lisp-interface
  functions class-direct-superclasses from clos as lisp-class-direct-superclasses,
            class-direct-subclasses from clos as lisp-class-direct-subclasses,
            class-direct-slots from clos as lisp-class-direct-slots,
            class-slots from clos as lisp-class-slots,
            class-direct-methods from clos as lisp-class-direct-methods;
  functions slot-definition-type from cl,
            slot-definition-initargs from cl,
            slot-definition-initform from cl,
            slot-definition-getter from clos,
            slot-definition-setter from clos;
  functions method-generic-function from cl as lisp-method-generic-function;
  functions princ-to-string from cl;
end;

/*---*** No longer used
// Infix Listener
define lisp-interface
  classes capi-infix-listener-pane from cl-user as <capi-infix-listener-pane>;
  functions capi-infix-listener-pane-value from cl-user,
            evaluate-dylan-form from cl-user,
            bind-dylan-variable from cl-user,
            parsed-dylan-form from cl-user;
end;

// Editor
define lisp-interface
  classes editor-pane from capi as <capi-editor-pane>;
  functions editor-pane-text from capi;
  functions editor-pane-text-setter from cl-user;
end;

// Tools
define lisp-interface
  functions environment-browse-object from cl-user;
end;
*/

// Multiprocessing
define lisp-interface
  functions all-processes from mp,
            process-name from mp;
  classes process from mp as <lisp-process>;
end;
