module: xlib
synopsis: generated bindings for the xlib library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;
ignore(<C-void**>);

define C-pointer-type <GError*> => <GError>;
ignore(<GError*>);

define C-struct <_Display>
  pointer-type-name: <Display>;
end C-struct;

define C-struct <_Screen>
  pointer-type-name: <Screen>;
end C-struct;

define C-struct <_Visual>
  pointer-type-name: <Visual>;
end C-struct;

define C-union <_XEvent>
  pointer-type-name: <XEvent>;
end C-union;

define C-struct <_XConfigureEvent>
  pointer-type-name: <XConfigureEvent>;
end C-struct;

define C-struct <_XImage>
  pointer-type-name: <XImage>;
end C-struct;

define C-struct <_XFontStruct>
  pointer-type-name: <XFontStruct>;
end C-struct;

define C-struct <_XTrapezoid>
  pointer-type-name: <XTrapezoid>;
end C-struct;

define C-struct <_XVisualInfo>
  pointer-type-name: <XVisualInfo>;
end C-struct;

define C-struct <_XWindowAttributes>
  pointer-type-name: <XWindowAttributes>;
end C-struct;

define C-function x-open-display
  c-name: "XOpenDisplay";
end;

