module: dylan-user
copyright: See LICENSE file in this distribution.

define library xlib
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject;

  export xlib;
end library;

define module xlib
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;

  export
    x-open-display,
    <XWindowAttributes>,
    <XVisualInfo>,
    <XTrapezoid>,
    <XFontStruct>,
    <XImage>,
    <XConfigureEvent>,
    <XEvent>,
    <_XEvent>,
    <Visual>,
    <Screen>,
    <Display>;
end module;
