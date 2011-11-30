Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library functional-dylan
  use dylan,
    export: all;
  use common-dylan,
    exclude: { common-dylan,
	       common-extensions,
	       simple-io,
	       functional-objects-extras },
    export: all;
  use common-dylan;
  export
    functional-dylan,
    functional-extensions,
    simple-format,
    functional-locators-protocol;
end library;

define module functional-extensions
  use common-extensions,
    exclude: { <byte-vector>, 
	       format-to-string },
    export: all;
  use common-extensions,
    import: { tokenize-command-line => tokenize-command-string },
    export: all;
  use functional-objects-extras,
    export: { <closable-object>,
	      default-last-handler };
  use simple-profiling,
    export: all;
  use dylan-extensions,
    export: { <byte-character>,
	      <unicode-character>,
	      <unicode-string>,
	      \last-handler-definer,
              <set>,
              <object-set>,
	      \packed-slots-definer,
	      initialize-packed-slots,
              compute-initial-packed-slot,
	      pack-tristate,  unpack-tristate,
              pack-quadstate, unpack-quadstate,
              pack-boolean,   unpack-boolean,
              packed-slots-end-count,
              $end-count-<object> };
end module functional-extensions;

define module simple-format
  use common-extensions,
    export: { format-to-string };
  use simple-io,
    export: { format-out };
end module simple-format;

define module functional-locators-protocol
  use locators-protocol,
    export: all;
  use functional-objects-extras,
    export: { <locator-defaults>,
	      <server-locator>,
	      <physical-locator>,
	      <directory-locator>,
	      <file-locator> };
end module functional-locators-protocol;

define module functional-dylan
  use common-dylan,
    exclude: { <byte-vector>, 
	       format-to-string,
	       tokenize-command-line },
    export: all;
  use functional-extensions,
    export: all;
end module;
