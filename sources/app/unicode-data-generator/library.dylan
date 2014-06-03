Module: dylan-user
Synopsis: Library definition
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

define library unicode-data-generator
  use common-dylan;
  use io;
  use system;
  use regular-expressions;
end library;

define module unicode-data-generator
  use common-dylan;
  use format;
  use format-out;
  use streams;
  use file-system;
  use regular-expressions;
end module;
