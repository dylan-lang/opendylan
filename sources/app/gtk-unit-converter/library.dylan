module: dylan-user

define library gtk-unit-converter
  use common-dylan;
  use io;
  use gobject;
  use gtk;
  use gdk;
end library;

define module gtk-unit-converter
  use common-dylan;
  use format-out;
  use gobject-glue;
  use gtk;
  use gtk-properties;
  use gdk;
end module;
