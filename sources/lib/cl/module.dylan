Module:    Dylan-User
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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

// The only thing left here is string-pluralize. Use the strings library
// instead.
define module CL-strings
  create string-pluralize;
end module;

define module CL-internals
  use common-dylan;
  use format;
  use strings;

  use CL-macros, export: all;
  use CL-sequences, export: all;
  use CL-strings, export: all;
end module CL-internals;
