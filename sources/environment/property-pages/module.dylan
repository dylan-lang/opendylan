Module:    Dylan-User
Synopsis:  Environment property pages
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-property-pages
  use environment-imports;	// this gets common-dylan
  use operating-system,
    import: { environment-variable };
  use file-system;

  use environment-protocols;

  use duim-internals,
    exclude: { position, string-pluralize,
	       get-property,
	       \put-property!, do-put-property!,
	       \remove-property!, do-remove-property!,
	       remove-keywords, \with-keywords-removed };

  use file-source-records;

  use environment-manager;
  use environment-framework;
  use environment-tools;

  // Currently, we don't export anything
end module environment-property-pages;
