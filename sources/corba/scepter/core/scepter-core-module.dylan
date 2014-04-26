Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-driver
  create
    get-scepter,
    set-scepter,
    \with-scepter,
    <scepter>,
    scepter-directory,
    scepter-force-build?, scepter-force-build?-setter,
    scepter-scopes, scepter-scopes-setter,
    scepter-nodes, scepter-nodes-setter,
    scepter-root, scepter-root-setter,
    scepter-errors, scepter-errors-setter,
    scepter-source, scepter-source-setter,
    scepter-program-name, scepter-program-name-setter,
    scepter-output, scepter-output-setter,
    scepter-front-end, scepter-front-end-setter,
    scepter-back-ends, scepter-back-ends-setter,
    scepter-write-to-standard-output?,
    scepter-sources, scepter-sources-setter,
    scepter-compile?, scepter-compile?-setter,
    scepter-informative?, scepter-informative?-setter,
    scepter-warnings?,
    scepter-preprocess-only?,
    scepter-case-sensitive-reserved-words?, scepter-case-sensitive-reserved-words?-setter,
    scepter-output-directory,
    scepter-dylan-subdir-prefix,
    scepter-protocol-option?,
    scepter-skeletons-option?,
    scepter-stubs-option?,
    scepter-orb-project-file,
    scepter-in-main-file?, scepter-in-main-file?-setter,
    scepter-imported?, scepter-imported?-setter,
    scepter-local-escapes,
    scepter-pragmas, scepter-pragmas-setter,
    scepter-read-from-stdin?, scepter-read-from-stdin?-setter,
    scepter-emit-imported?,
    scepter-dylan-orb-runtime-libraries,
    scepter-dylan-orb-runtime-modules,
    scepter-failure-mode, scepter-failure-mode-setter,
    scepter-failure-token, scepter-failure-token-setter,
    scepter-break-on-errors?,
    scepter-invoke,
    scepter-options,
    scepter-process-options,
    scepter-condition,
    scepter-parse-options,
    scepter-default-front-end-class,
    scepter-default-back-end-class,
    scepter-report-condition,
    scepter-format-progress-start,
    scepter-format-progress-end,
    scepter-clean-build?;

  create
    <option-error>,
    <named-option-error>,
    <missing-argument>,
    <illegal-option>,
    <scepter-option>,
    \scepter-option-definer,
    scepter-option-name,
    scepter-option-value?,
    scepter-option-callback,
    scepter-option-hidden?,
    scepter-option-usage,
    scepter-option-values,
    scepter-option-add-value,
    scepter-option-supplied,
    scepter-option-supplied?,
    scepter-initialize-options,
    scepter-find-option;

  create
    <scepter-ast-root>,
    <scepter-declarator>,
    declarator-imported?;

  create
    \with-progress;

end module;

define module scepter-back-end
  use scepter-driver, export: { scepter-option-definer };
  use scepter-utilities, export: all;
  create
    <scepter-back-end-result>,
    scepter-back-end-success?,
    scepter-back-end-dylan-subprojects,
    scepter-back-end-modified-dylan-projects,
    \scepter-back-end-definer,
    all-scepter-back-end-classes,
    scepter-back-end-class,
    <scepter-back-end>,
    scepter-back-end-scepter,
    scepter-back-end-banner,
    scepter-back-end-filename-extension,
    scepter-back-end-command-line-syntax,
    run-scepter-back-end-emitter,
    scepter-back-end-emitter?,
    scepter-back-end-emit,
    initialize-scepter-back-end,
    finalize-scepter-back-end,
    setup-scepter-back-ends,
    reset-scepter-back-ends,
    emit-declarator?;
end module;

define module scepter-front-end
  use scepter-driver, export: all;
  use scepter-utilities, export: all;
  create
    <scepter-source>,
    scepter-source-modified,
    scepter-source-base-name,
    scepter-source-output-directory,
    \scepter-front-end-definer,
    all-scepter-front-end-classes,
    scepter-front-end-class,
    <scepter-front-end>,
    scepter-front-end-scepter,
    scepter-front-end-banner,
    scepter-front-end-make-source,
    scepter-front-end-generate-ast,
    setup-scepter-front-end,
    reset-scepter-front-end;
end module;

define module scepter-error
  create
    <idl-condition-restart>,
    <idl-condition>,
    <idl-error>,
    idl-condition-string,
    idl-condition-source,
    idl-condition-recoverable?,
    idl-condition-serious?,
    idl-condition-title,
    idl-condition-body,
    idl-condition-similar?;
end module;

define module scepter-driver-implementation
  use generic-arithmetic-common-dylan;
  use simple-debugging;
  use file-system;
  use format;
  use operating-system;
  use standard-io;
  use streams;
  use locators;
  use threads;
  use cpp;
  use scepter-driver;
  use scepter-back-end;
  use scepter-front-end;
  use scepter-error;
  use scepter-utilities;
end module;

define module scepter-back-end-implementation
  use date;
  use format;
  use generic-arithmetic-common-dylan;
  use streams;
  use scepter-back-end;
  use scepter-front-end;
  use scepter-error;
end module;

define module scepter-front-end-implementation
  use date;
  use common-dylan;
  use format;
  use streams;
  use locators;
  use scepter-front-end;
  use scepter-error;
end module;

define module scepter-error-implementation
  use common-dylan;
  use format;
  use scepter-error;
  use scepter-driver;
  use scepter-front-end;
end module;


