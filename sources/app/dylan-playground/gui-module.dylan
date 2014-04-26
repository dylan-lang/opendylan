Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  A Dylan application to play around in
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module gui-dylan-playground
  use common-dylan;
  use simple-random;
  use threads;
  use finalization;
  use machine-words;

  use operating-system;
  use date;
  use file-system;
  use locators;

  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;

  use bit-vector;
  use bit-set;
  use byte-vector;
  use collectors;
  use set;
  use table-extensions;

  use duim;
end module gui-dylan-playground;
