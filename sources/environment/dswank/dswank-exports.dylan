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
  use registry-projects;
  //use stack-walker;
  use release-info;
end library;

define module dswank
  use common-dylan, exclude: { format-to-string };
  use lisp-reader;
  use format;
  use streams;
  use standard-io;
  use sockets;
  use environment-commands;
  use environment-protocols, exclude: { application-filename, application-arguments };
  use command-lines;
  use commands;
  use source-records;
  use file-source-records;
  use file-system;
  use locators;
  use registry-projects;
  //use stack-walker;
  use release-info;
end module;
