Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library variable-search
  use dylan;
end library;

define module variable-search
  use dylan;
  use internal,
    import: { class-debug-name, function-debug-name, 
              translate-cl-class-name, hack-function-debug-name,
              <translator-module>, module-name, module-package,
              $translator-modules, find-translator-module };
  export
    locate-variable, variable-value;
end module;

// eof
