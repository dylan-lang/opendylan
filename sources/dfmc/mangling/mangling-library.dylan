module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-mangling
  use dylan;
  use common-dylan, import: { common-extensions };
  export dfmc-mangling;
end library;

define module dfmc-mangling
  use dylan;
  use common-extensions,
    import: { \table-definer, false-or,
              concatenate!, string-to-integer };
  use dylan-extensions, import: { <byte-character>, iterate };
  export
    <mangler>,
    <mangler-with-options>,
    <demangler>,

    $module-separator,
    $library-separator,
    $library-separator,
    $local-suffix,
    $hygiene-marker,
    $escape-separator,
    $method-marker,
    $method-mangled-marker,
    $method-mangled-marker-string,
    $slot-marker,
    $slot-mangled-marker,
    $slot-mangled-marker-string,
    $domain-mangled-marker-string,
    $constant-prefix,
    $symbol-prefix,
    $indirection-prefix,
    $wrapper-suffix,
    $iep-suffix,
    $min-character-code,
    $max-character-code,

    mangler-position,
    mangler-as-string,
    mangler-reset,
    mangle-raw-into,
    mangle-name-into,
    mangle-name-raw,
    mangle-name-hygienically,
    mangle-name-locally,

    mangle-binding-spread,
    mangle-namespace-spread-into,

    mangle-integer,
    mangle-constant,
    mangle-symbol,
    mangle-generic-method,
    mangle-local-method,
    mangle-domain,
    mangle-slot-descriptor,
    mangle-wrapper,

    demangle-name-raw,
    demangle-name-locally,
    demangle-binding-spread,
    demangler-extract-characteristics,
    demangler-extract-generic-function-name,
    demangler-extract-library-name,
    demangler-extract-callable-object-name,
    demangler-extract-method-details;
end module;

