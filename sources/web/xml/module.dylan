Module:       Dylan-User
Synopsis:     XML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module xml
  create parse-xml-from-file,
	 parse-xml-from-stream,
	 print-xml-to-file,
	 print-xml-to-stream,
	 print-xml-to-string;
end module xml;

define module xml-internals
  use functional-dylan,
    exclude: { position, position-if, count };
  use dylan-extensions,
    import: { \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error };
  use simple-format;			// for debugging
  use threads;
  use streams;
  use expat;
  use dom-internals;

  use xml, export: all;
end module xml-internals;
