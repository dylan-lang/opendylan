module: dylan-user

define library dswank
  use common-dylan;
  use io;
  use network;
  use lisp-reader;
  use environment-commands;
  use environment-protocols;
  use commands;
  use source-records;
  use file-source-records;
  use system;
end library;

define module dswank
  use common-dylan;
  use lisp-reader;
  use format;
  use streams;
  use standard-io;
  use sockets;
  use environment-commands;
  use environment-protocols;
  use command-lines;
  use commands;
  use source-records;
  use file-source-records;
  use file-system;
  use locators;
end module;
