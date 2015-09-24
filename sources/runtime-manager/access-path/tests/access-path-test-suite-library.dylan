Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2015 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library access-path-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use access-path;
  use local-access-path;
  use remote-access-path;
  use dylan-orb;
  use dfmc-mangling;

  export access-path-test-suite;
end library;

define module access-path-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use access-path;
  use dylan-orb;
  use dfmc-mangling;
  use simple-format;

  export access-path-test-suite;
end module;
