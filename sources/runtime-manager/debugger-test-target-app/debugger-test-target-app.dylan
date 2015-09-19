Module:    debugger-test-target-app
Author:    Peter S. Housel
Synopsis:  An application library for test-suite access-path-test-suite
Copyright:    Original Code is Copyright 2015 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function main()
  let scenario = element(application-arguments(), 0, default: #f);
  select (scenario by \=)
    otherwise =>
      #f;
  end select;
  exit-application(0);
end function;

main()
