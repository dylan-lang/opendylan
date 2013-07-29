module: dylan-user
author: Francesco Ceccon
copyright: See LICENSE file in this distribution.

define library gobject-glue
  use common-dylan;
  use c-ffi;
  use dylan;

  use glib;
  use gobject;

  export gobject-glue;
end library;

define module gobject-glue
  use common-dylan;
  use c-ffi;
  use dylan;
  use finalization;
  use dylan-primitives;
  use dylan-extensions, import: { debug-name, integer-as-raw, raw-as-integer };

  use glib;
  use gobject;

  export \with-gdk-lock,
    g-signal-connect,
    g-value-nullify,
    g-value-set-value,
    g-value-to-dylan,
    all-subclasses,
    property-setter-definer,
    property-getter-definer;
end module;
