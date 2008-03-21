module: dylan-user

define library dswank
  use common-dylan;
  use io;
  use network;
  use lisp-reader;
end library;

define module dswank
  use common-dylan;
  use lisp-reader;
  use format;
  use streams;
  use standard-io;
  use sockets;
end module;
