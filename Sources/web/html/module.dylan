Module:       Dylan-User
Synopsis:     HTML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module html
  create parse-html-from-file,
	 parse-html-from-stream,
	 print-html-to-file,
	 print-html-to-stream,
	 print-html-to-string;

  create <html-parse-error>,
	 <invalid-character>,
	 <unbalanced-angle-brackets>,
	 <unbalanced-quotes>,
	 <unexpected-end-tag>,
	 <unexpected-attributes>,
	 <undefined-html-character>;
end module html;

define module html-internals
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
  use dom-internals;

  use html, export: all;
end module html-internals;
