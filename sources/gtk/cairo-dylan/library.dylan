module: dylan-user
copyright: See LICENSE file in this distribution.

define library cairo
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;

  export cairo;
end library;

define module cairo
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;

  export
    cairo-image-surface-create,
    cairorectangleint-height-setter,
    cairorectangleint-height,
    cairorectangleint-width-setter,
    cairorectangleint-width,
    cairorectangleint-y-setter,
    cairorectangleint-y,
    cairorectangleint-x-setter,
    cairorectangleint-x,
    <cairoRectangleInt>,
    <cairoPath>,
    <cairoScaledFont>,
    <cairoFontFace>,
    <cairoFontType>,
    <cairoFontOptions>,
    <cairoContent*>,
    <cairoContent>,
    $CAIRO-CONTENT-COLOR-ALPHA,
    $CAIRO-CONTENT-ALPHA,
    $CAIRO-CONTENT-COLOR,
    <cairoRegion>,
    <cairoPattern>,
    <cairoMatrix>,
    <cairoSurface>,
    <cairoContext>;
end module;
