Module:    dylan-user
Synopsis:  Automated test of the network library
Copyright: (c) 2018 Open Dylan
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library network-test
  use common-dylan;
  use testworks;
  use network;
  use system;
  use io;
  use C-FFI;

  export network-test;
end library network-test;

define module network-test
  use common-dylan;
  use simple-format, import: { format-to-string, format-out };
  use testworks;
  use threads;
  use sockets;
  use streams;
  use date;
  use C-FFI;
end module network-test;
