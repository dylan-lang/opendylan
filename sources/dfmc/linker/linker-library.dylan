module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-linker
  use functional-dylan;
  use dfmc-core;
  use dfmc-back-end;
  export dfmc-linker;
end library;

define module dfmc-linker
  use functional-dylan;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-imports;
  export 
    <linker>,
    emit-library-record,
    emit-library-records,
    emit-gluefile,
    emit-mainfile,
    emit-glue;

  export
    link-and-download,
    download-for-interactive-execution;

  export 
    <makefile-target>,
      <unix-makefile-target>, <win32-makefile-target>, <MPW-makefile-target>,
    source-suffix, object-suffix, resource-suffix,
    output-basename,
    makefile-variable-ref, makefile-pathname, makefile-command,
    link-target-separator, makefile-linker-option-massage,
    library-unit-ref, library-unit-link-ref, relative-makefile-unit-ref, executable-unit-ref,
    source-unit, object-unit, executable-unit, source-units, object-units, 
    object-unit-name, object-units-names, library-unit-name,
    additional-platform-libraries,
    backend-object-file-name, backend-resource-file-name,
    emit-makefile-object-dependencies,
    glue-unit, main-unit,
    personal-library?,
    runtime-unit-link-ref,
    makefile-target, makefile-target-using-os-name,
    make-install-command,
    emit-target-makefile,
    emit-makefile-rule,
    emit-makefile-header,
    emit-makefile-footer,
    emit-makefile-program-setups,
    emit-makefile-separator,
    emit-makefile-definition,
    emit-makefile-units,
    emit-makefile;
  
end module;

