Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite orbix-include-idl (setup-function: setup-orbix-include-idl,
                                cleanup-function: cleanup-orbix-include-idl)
end suite;

define method setup-orbix-include-idl ()
  push(*cpp-include-path*, ".");
  setup-idl-files(*orbix-include-files*);
  scepter-case-sensitive-reserved-words?(get-scepter()) := #t;
end method;

define method cleanup-orbix-include-idl ()
  cleanup-idl-files(*orbix-include-files*);
  scepter-reset-options(get-scepter());
end method;

