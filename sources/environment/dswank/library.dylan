module:    dylan-user
author:    Andreas Bogk and Hannes Mehnert
copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library dswank
  use common-dylan;
  use io;
  use network;
  use lisp-reader;
  use environment-commands;
  use environment-protocols;
  use build-system;
  use commands;
  use environment-internal-commands;
  use source-records;
  use file-source-records;
  use system;
  use registry-projects;
  //use stack-walker;
  use release-info;
  use dfmc-back-end-implementations;
end library;

define module dswank
  use common-dylan, exclude: { format-to-string };
  use lisp-reader;
  use format;
  use streams;
  use standard-io;
  use sockets;
  use environment-commands;
  use environment-internal-commands;
  use environment-protocols,
    exclude: { application-filename,
	       application-arguments,
	       default-build-script,
	       default-build-script-setter,
	       run-application };
  use build-system;
  use command-lines;
  use commands;
  use source-records;
  use file-source-records;
  use file-system;
  use locators;
  use registry-projects;
  //use stack-walker;
  use release-info;
  use operating-system;
end module;
