Module:       dylan-user
Synopsis:     Portable operating system API
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system
  use dylan;
  use common-dylan;
  use io;
  export
    operating-system,
    date,
    locators,
    locators-internals,
    file-system,
    file-system-internals,
    simple-xml,
    settings,
    settings-internals;
end library system;

define module simple-xml
  create <xml-error>;

  create <xml-document>,
         document-location, document-location-setter,
         document-element, document-element-setter,
         read-xml-document;

  create <xml-node>,
         node-attribute, node-attribute-setter,
         node-attributes,
         node-children,
         node-name,
         node-text, node-text-setter;

  create <xml-element>;

  create select-node-text,
         select-nodes,
         select-single-node;
end module simple-xml;

define module operating-system
  use common-extensions,
    import: { application-name,
              application-filename,
              application-arguments,
              exit-application,
              register-application-exit-function },
    export: all;

  create $architecture-little-endian?,
         $os-name, $os-variant, $os-version,
         $platform-name, $machine-name,
         login-name, login-group, owner-name, owner-organization,
         environment-variable, environment-variable-setter,
         tokenize-environment-variable,
         run-application,
         <application-process>,
         wait-for-application-process,
         \with-application-output,
         create-application-event,
         wait-for-application-event,
         signal-application-event,
         load-library,
         current-process-id,
         parent-process-id;

  create command-line-option-prefix;
end module operating-system;

define module date
  create
    <date>,
    encode-date, decode-date,
    date-year,
    date-month,
    date-day,
    date-hours,
    date-minutes,
    date-seconds,
    date-microseconds,
    date-time-zone-offset, date-time-zone-offset-setter,
    <day-of-week>,
    date-day-of-week,
    format-date,
    parse-date-string,
    parse-iso8601-string,
    as-iso8601-string,
    as-rfc822-string,
    as-rfc1123-string,
    current-date,
    current-timestamp,
    local-time-zone-offset,
    local-time-zone-name,
    //---*** Other time zone tools?  (I.e., conversions from/to zone name to/from offset)
    local-daylight-savings-time?,
    <duration>,
    <year/month-duration>, <day/time-duration>,
    encode-year/month-duration, encode-day/time-duration,
    decode-duration,
    <date-arithmetic-error>, <date-arithmetic-invalid-day>,
    <date-arithmetic-error-restart>, <date-arithmetic-use-last-valid-day>;
end module date;

///---*** DO WE NEED TO EXPORT NATIVE MAC LOCATORS & FRIENDS?
///---*** (If so, we'll need stubs on other platforms)

define module locators
  use locators-protocol,
    export: all;

  // Protocol functions
  create locator-host,
         locator-server,
         locator-volume,
         locator-directory,
         locator-relative?,
         locator-path,
         locator-base,
         locator-extension,
         locator-name;

  // Classes
  create <directory-locator>,
         <file-locator>,
         <native-directory-locator>,
         <native-file-locator>;

  // Coercion protocols
  create locator-as-string,
         string-as-locator;

  // Locator conditions
  create <locator-error>,
         locator-error;

  // Utilities
  create simplify-locator,
         subdirectory-locator,
         relative-locator,
         merge-locators;

  // Web locators
  create <web-locator>,
         <url>,
         <server-url>,
         <http-server>,
         <https-server>,
         <ftp-server>,
         <file-server>,
         <directory-url>,
         <file-url>,
         <file-index-url>,
         <cgi-url>,
         <mail-to-locator>,
         locator-address,
         locator-cgi-string,
         locator-default-port,
         locator-file,
         locator-index,
         locator-password,
         locator-port,
         locator-protocol,
         locator-username,
         http-parser,
         https-parser,
         ftp-parser,
         file-parser,
         mailto-parser;
end module locators;

define module locators-internals
  use common-dylan;
  use dylan-direct-c-ffi;
  use dylan-extensions,
    import: { \copy-down-method-definer };
  use streams-protocol;
  use locators, export: all;

  // Useful utilities
  create case-insensitive=,
         current-directory-locator?,
         delimiter-to-string,
         find-delimiter,
         find-delimiters,
         find-delimiter-from-end,
         find-delimiters-from-end,
         locator-might-have-links?,
         locator-test,
         prefix-equal?;

  // Paths
  create canonicalize-path,
         parse-path,
         path-to-string;
end module locators-internals;

define module file-system
  // Conditions
  create <file-system-error>,
         <file-error>,
         <file-exists-error>,
         <file-does-not-exist-error>,
         <invalid-file-permissions-error>,
         file-error-locator;

  // File streams
  // Multi-buffered streams
  create \with-open-file;
  use streams-internals,
    export: { <file-stream>,
              <byte-file-stream>,
              type-for-file-stream,
              stream-locator,
              writable-file-stream-position-setter,

              <buffer-vector>,
              <multi-buffered-stream>,
              multi-buffered-stream-position-setter,
              write-4-aligned-bytes-from-word,
              read-4-aligned-bytes-as-word,
              write-4-aligned-bytes, write-8-aligned-bytes,
              read-4-aligned-bytes, read-8-aligned-bytes,
              multi-buffer-working-set,
              multi-buffer-reads,
              multi-buffer-bytes,
              multi-buffer-total-working-set,
              multi-buffer-total-reads,
              multi-buffer-total-bytes };

  // File system locators
  create <file-system-locator>,
         <file-system-file-locator>,
         <file-system-directory-locator>,
         file-system-separator;

  // Posix locators
  create <posix-file-system-locator>,
         <posix-directory-locator>,
         <posix-file-locator>;

  // Microsoft locators
  create <microsoft-server-locator>,
         <microsoft-unc-locator>,
         <microsoft-volume-locator>,
         <microsoft-file-system-locator>,
         <microsoft-directory-locator>,
         <microsoft-file-locator>;

  // Native locators
  create <native-file-system-locator>;

  // File and directory operations
  create <pathname>,
         <file-type>,
         <copy/rename-disposition>,
         expand-pathname,
         shorten-pathname,
         file-exists?,
         file-type,
         link-target,
         delete-file,
         copy-file,
         rename-file,
         file-properties,
         file-property, file-property-setter,
         do-directory,
         directory-contents,
         create-directory,
         delete-directory,
         ensure-directories-exist,
         directory-empty?,
         home-directory,
         working-directory, working-directory-setter,
         temp-directory,
         root-directories;
end module file-system;

define module file-system-internals
  // File streams
  use streams-internals,
    export: { <general-file-stream>,
              <byte-char-file-stream> };

  // Multi-buffered streams
  use streams-internals,
    export: { <general-multi-buffered-stream>,
              <byte-multi-buffered-stream>,
              <byte-char-multi-buffered-stream> };
end module file-system-internals;

define module settings
  create \settings-definer,
         \settings-class-definer,
         \settings-key-definer,
         \settings-slots-definer,
         \settings-slot-definer,
         $settings-table;
  create remove-value!;
  create <settings>,
         <system-settings>,
         <site-settings>,
         <site-software-settings>,
         <local-settings>,
         <local-software-settings>,
         <local-hardware-settings>,
         <default-user-settings>,
         <default-user-software-settings>,
         <current-user-settings>,
         <current-user-software-settings>;
  //--- Work around emulator macro hygiene problems
  create $uninitialized,
         <uninitialized>,
         initialized?, uninitialized?,
         \uninitialized-or;
end module settings;

define module settings-internals
  use settings, export: all;

  export initialize-settings,
         invalidate-settings-caches,
         settings-parent,
         settings-handle,
         settings-key-name, settings-key-name-setter,
         make-key,
         register-key, unregister-key,
         get-value, set-value,
         do-remove-value!;

  //---*** BOOTSTRAPPING: Remove these exports and declare
  //---*** the generics dynamic after 2.1a1 is released...
  export invalidate-settings-caches,
         %settings-key-name-setter;
end module settings-internals;

define module system-internals
  use common-dylan;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use threads;
  use format;
  use format-out;
  use streams-internals;
  use operating-system, export: all;
  use date, export: all;
  use locators-internals, export: all;
  use file-system, export: all;
  use file-system-internals, export: all;
  use simple-xml, export: all;
  use settings-internals, export: all;
end module system-internals;
