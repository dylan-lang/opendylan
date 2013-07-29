module: cairo
synopsis: generated bindings for the cairo library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define C-struct <_cairoContext>
  pointer-type-name: <cairoContext>;
end C-struct;

define C-struct <_cairoSurface>
  pointer-type-name: <cairoSurface>;
end C-struct;

define C-struct <_cairoMatrix>
  pointer-type-name: <cairoMatrix>;
end C-struct;

define C-struct <_cairoPattern>
  pointer-type-name: <cairoPattern>;
end C-struct;

define C-struct <_cairoRegion>
  pointer-type-name: <cairoRegion>;
end C-struct;

define constant $CAIRO-CONTENT-COLOR = 4096;
define constant $CAIRO-CONTENT-ALPHA = 8192;
define constant $CAIRO-CONTENT-COLOR-ALPHA = 12288;
define constant <cairoContent> = <C-int>;
define C-pointer-type <cairoContent*> => <cairoContent>;

define C-struct <_cairoFontOptions>
  pointer-type-name: <cairoFontOptions>;
end C-struct;

define C-struct <_cairoFontType>
  pointer-type-name: <cairoFontType>;
end C-struct;

define C-struct <_cairoFontFace>
  pointer-type-name: <cairoFontFace>;
end C-struct;

define C-struct <_cairoScaledFont>
  pointer-type-name: <cairoScaledFont>;
end C-struct;

define C-struct <_cairoPath>
  pointer-type-name: <cairoPath>;
end C-struct;

define C-struct <_cairoRectangleInt>
  slot cairorectangleint-x :: <C-signed-int>;
  slot cairorectangleint-y :: <C-signed-int>;
  slot cairorectangleint-width :: <C-signed-int>;
  slot cairorectangleint-height :: <C-signed-int>;
  pointer-type-name: <cairoRectangleInt>;
end C-struct;

define C-function cairo-image-surface-create
  c-name: "cairo_image_surface_create";
end;

