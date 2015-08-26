module: dylan-user
copyright: See LICENSE file in this distribution.

define library gmodule
  use dylan;
  use common-dylan;
  use c-ffi;
  use glib;
  use gobject;

  export gmodule;
end library;

define module gmodule
  use dylan;
  use common-dylan;
  use c-ffi;
  use glib;
  use gobject-glue;

  export
    <GModuleFlags*>,
    <GModuleFlags>,
    $g-module-bind-mask,
    $g-module-bind-local,
    $g-module-bind-lazy,
    g-module-supported,
    g-module-error,
    g-module-build-path,
    g-module-symbol,
    g-module-name,
    g-module-make-resident,
    g-module-close,
    <GModule>;
end module;
