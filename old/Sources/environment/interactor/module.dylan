Module:    Dylan-User
Synopsis:  Environment Interactor
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module environment-interactor
  use environment-imports;

  use duim-internals;
  use environment-protocols;
  use environment-framework;

  export <interactor-control>,
         <interactor-source-form>,
         interactor-execute-code,
         interactor-describe-object,
         interactor-last-values,
         frame-describe-object,
         frame-reexecute-source-form;
end;
