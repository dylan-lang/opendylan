module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library appl-names

 use dylan;
 use idvm-namespace;
 use simple-streams;
 use simple-print;
 use simple-format;
 use equal-table;
 use set;
 use locators;
 use byte-vector;
 use streams;
 use functional-extensions;
 use c-ffi;
 use win32-common;
 use win32-gdi;
 use win32-kernel;
 use win32-user;
 use win32-dialog;

  export appl-names;

end library appl-names;


define module dylan-names

 use dylan;
 use idvm-namespace;
 use internal;
 use dylan;
 use mop;

end module dylan-names;

define module simple-streams-names

 use simple-streams;
 use idvm-namespace;
 use dylan;

end module simple-streams-names;

define module simple-print-names

 use simple-print;
 use idvm-namespace;
 use dylan;

end module simple-print-names;

define module simple-format-names

 use simple-format;
 use idvm-namespace;
 use dylan;

end module simple-format-names;

define module equal-table-names

 use dylan;
 use idvm-namespace;
 use equal-table;

end module equal-table-names;

define module set-names

 use set;
 use idvm-namespace;
 use dylan;

end module set-names;

define module locators-names

 use locators;
 use idvm-namespace;
 use dylan;

end module locators-names;

define module byte-vector-names

 use byte-vector;
 use idvm-namespace;
 use dylan;

end module byte-vector-names;

define module streams-names

 use dylan;
 use idvm-namespace;
 use streams;

end module streams-names;

define module core-streams-names

 use dylan;
 use idvm-namespace;
 use core-streams;

end module core-streams-names;

define module wizard-streams-names

 use dylan;
 use idvm-namespace;
 use wizard-streams;

end module wizard-streams-names;

define module sequence-streams-names

 use dylan;
 use idvm-namespace;
 use sequence-streams;

end module sequence-streams-names;

define module buffered-streams-names

 use dylan;
 use idvm-namespace;
 use buffered-streams;

end module buffered-streams-names;

define module file-streams-names

 use dylan;
 use idvm-namespace;
 use file-streams;

end module file-streams-names;

define module wrapper-streams-names

 use dylan;
 use idvm-namespace;
 use wrapper-streams;

end module wrapper-streams-names;

define module stream-conveniences-names

 use dylan;
 use idvm-namespace;
 use stream-conveniences;

end module stream-conveniences-names;

define module functional-extensions-names

 use dylan;
 use idvm-namespace;
 use functional-extensions;

end module functional-extensions-names;

define module c-ffi-names

 use dylan;
 use idvm-namespace;
 use c-ffi;

end module c-ffi-names;

define module win32-common-names

 use dylan;
 use idvm-namespace;
 use win32-common;

end module win32-common-names;

define module win32-gdi-names

 use dylan;
 use idvm-namespace;
 use win32-gdi;

end module win32-gdi-names;

define module win32-kernel-names

 use dylan;
 use idvm-namespace;
 use win32-kernel;

end module win32-kernel-names;

define module win32-user-names

 use dylan;
 use idvm-namespace;
 use win32-default-handler;
 use win32-user;

end module win32-user-names;

define module win32-dialog-names

 use dylan;
 use idvm-namespace;
 use win32-dialog;

end module win32-dialog-names;

define module appl-names

  use dylan;

end module appl-names;

/*
define module appl-names

  use dylan-names;
  use simple-streams-names;
  use simple-format-names;
  use equal-table-names;
  use set-names;
  use locators-names;
  use byte-vector-names;
  use streams-names;
  use functional-extensions-names;

  use c-ffi-names;
  use win32-common-names;
  use win32-gdi-names;
  use win32-kernel-names;
  use win32-user-names;
  use win32-dialog-names;


end module appl-names;
*/