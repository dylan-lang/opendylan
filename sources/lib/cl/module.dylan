Module:    Dylan-User
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module CL-macros
  create \push!, \pop!;
end module CL-macros;

define module CL-sequences
  create cl-position, cl-position-if,
         cl-find, cl-find-if,
         cl-assoc, cl-assoc-if,
         cl-count, cl-count-if,
         cl-remove, cl-remove-if,
         cl-remove!, cl-remove-if!,
         cl-substitute, cl-substitute-if,
         cl-substitute!, cl-substitute-if!,
         cl-remove-duplicates, cl-remove-duplicates!,
         cl-search,
         cl-mismatch,
         cl-merge;
end module CL-sequences;

define module CL-plists
  create get-property,
         \put-property!, do-put-property!,
         \remove-property!, do-remove-property!,
         remove-keywords,
         \with-keywords-removed;
end module CL-plists;

define module CL-strings
  create char-equal?, char-not-equal?,
         char-less?, char-not-less?,
         char-greater?, char-not-greater?,
         string-equal?, string-not-equal?,
         string-less?, string-not-less?,
         string-greater?, string-not-greater?,
         alpha-char?, digit-char?,
         alphanumeric-char?,
         upper-case?, lower-case?,
         standard-char?,
         graphic-char?,
         ordinary-char?,
         whitespace-char?,
         string-capitalize, string-capitalize!,
         string-capitalize-words, string-capitalize-words!,
         string-trim, string-left-trim, string-right-trim,
         string-search-set, string-search-not-set,
         string-pluralize,
         string-a-or-an;
end module CL-strings;

define module CL-internals
  use functional-dylan;
  use format;

  use CL-macros, export: all;
  use CL-sequences, export: all;
  use CL-plists, export: all;
  use CL-strings, export: all;

  export do-char-equal?,
	 do-char-less?,
	 do-char-greater?,
	 do-alpha-char?,
	 do-digit-char?,
	 do-upper-case?,
	 do-lower-case?,
	 do-standard-char?,
	 do-graphic-char?;
end module CL-internals;
