Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library locators
  use dylan;
  use functional-extensions;
  use equal-table;
  use system;
  export locators, locator-internals;
end library;

define module locators
  use basic-locator-classes,
    export: { <locator>, <physical-locator> };
  create
    // <locator>,			// Now in basic-locator-classes
    <file-locator>,
    <directory-locator>,
    <abstract-locator>,
    // <physical-locator>,		// Now in basic-locator-classes
    <native-locator>,
    <url-locator>,
    <posix-locator>,
    <microsoft-locator>,
    <path>,
    <host-path>,
    <type-path>,
    <directory-path>,
    wild-locator?,
    locators-match?,
    absolute-locator?,
    relative-locator?,
    translate-locator,
    merge-locators,
    abbreviate-locator,
    simplify-locator,
    override-locator, //was: replace-components
    default-locator,  //was: default-components
    //register-abstract-host, // obsolete
    //remove-abstract-host!, // obsolete
    //clear-abstract-host-table!, // obsolete
    *abstract-host-table*,
    make-abstract-host-table,
    clear-abstract-host-table,
    add-abstract-host,
    remove-abstract-host,
    locator-case-sensitive?,
    locator-scheme,
    locator-host,
    locator-port,
    locator-user-id,
    locator-password,
    locator-volume,
    locator-directory,
    locator-directory-path,
    locator-base,
    locator-type,
    locator-version,
    locator-search-keys,
    locator-transfer-type,
    locator-prefix,
    locator-name,
    locator-extension,
    locator-suffix,
    path-elements,
    locator-pretty-name;
end module;

define module locator-internals
  use dylan;
  use functional-extensions, 
    exclude: { position, elements, elements-setter };
  use equal-table, 
    import: {<equal-table>};
  use operating-system;
  use basic-locator-classes;
  use locators;

  export
    // exported for printers
    byte-size,
    form-code,
    type-code,
    raw-base,
    raw-name,
    raw-type,
    <unc-locator>,
    <ms-dos-locator>,
    <ftp-transfer-type>,
    <ftp-locator>,
    <http-locator>,
    <mailto-locator>,

    // exported for tests
    <match-path>,
    <posix-file-locator>,
    <posix-directory-locator>,
    <posix-directory-path>,
    <posix-type-path>,
    <abstract-file-locator>,
    <abstract-directory-locator>,
    <abstract-directory-path>,
    <abstract-match>,
    <match-path>,
    <http-directory-locator>,
    <http-file-locator>,
    <ftp-directory-locator>,
    <ftp-file-locator>,
    <unc-file-locator>,
    <unc-directory-locator>,
    <ms-dos-file-locator>,
    <ms-dos-directory-locator>,
    <microsoft-path>,
    <microsoft-directory-path>,
    <microsoft-type-path>,
    <microsoft-host-path>,
    match,
    instantiate,
    <locator-error>,
    <locator-print-error>,
    <locator-parse-error>,
    <locator-translation-error>,
    <locator-merge-error>;
end module;

/* Here is a useful chart of locator components.  -mf


	schm host vol  port uid  pass dir  base type vers skey xfer

prefix	 *    *    *    *    *    *    *

name                                        *    *

extension                                        *

suffix                                                *    *    *

----------------------------------------------------------------------------------------

abstract      *                        *    *    *    *

posix                                  *    *    *

ms-dos             *                   *    *    *

unc           *                        *    *    *

http	 *    *         *              *    *    *         *

ftp	 *    *         *    *    *    *    *    *              *

mailto	 *    *              *

*/
