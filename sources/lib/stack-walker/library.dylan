module:    dylan-user
copyright: Original Code is Copyright (c) 2007 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library stack-walker
  use common-dylan;
  use c-ffi;

  export stack-walker;
end;

define module stack-walker
  use common-dylan;
  use c-ffi;

  export walk-stack;
end;
