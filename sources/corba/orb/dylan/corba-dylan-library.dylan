Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-dylan
  use dylan; // threads
  use generic-arithmetic; 
  use big-integers;
  use collections; // table-extensions
  use system; // operating-system, date
  use io; // streams, print, standard-io, format
  export corba-dylan;
end library;

define module corba-dylan
  use generic-arithmetic-common-dylan,
    exclude: { format-to-string },
    export: all;
  use dylan-arithmetic,
    export: all,
    prefix: "dylan/";
  use dylan-extensions, 
    export: {encode-single-float,
	     decode-single-float,
	     encode-double-float,
	     decode-double-float,
	     register-application-exit-function,
	     <double-integer>,
	     <machine-word>,
             keyboard-interrupt?,
             keyboard-interrupt-polling?-setter,
             <keyboard-interrupt>};
  use table-extensions, exclude: { table }, export: all;
  use threads, export: all;
  use format, export: all;
  use streams, export: all;
  use print, export: all;
  use standard-io, export: all;
  use operating-system, export: all;
  use file-system, export: all;
  use date, export: all;
  use settings, export: all;
  use simple-debugging, export: all;
end module;


