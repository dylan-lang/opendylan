module: dylan-user
copyright: See LICENSE file in this distribution.

define library gmodule
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;
  use glib;

  export gmodule;
end library;

define module gmodule
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;
  use glib;

  export
    <GModuleFlags*>,
    <GModuleFlags>,
    $G-MODULE-BIND-MASK,
    $G-MODULE-BIND-LOCAL,
    $G-MODULE-BIND-LAZY,
    g-module-supported,
    g-module-error,
    g-module-build-path,
    g-module-symbol,
    g-module-name,
    g-module-make-resident,
    g-module-close,
    <GModule>;
end module;
