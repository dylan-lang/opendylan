module: dylan-user
copyright: See LICENSE file in this distribution.

define library cairo
  use dylan;
  use common-dylan;
  use c-ffi;

  export cairo;
end library;

define module cairo
  use dylan;
  use common-dylan;
  use c-ffi;
  use gobject-glue;

  export
    cairo-image-surface-create,
    cairo-rectangle-int-height-setter,
    cairo-rectangle-int-height,
    cairo-rectangle-int-width-setter,
    cairo-rectangle-int-width,
    cairo-rectangle-int-y-setter,
    cairo-rectangle-int-y,
    cairo-rectangle-int-x-setter,
    cairo-rectangle-int-x,
    <cairoRectangleInt>,
    <cairoPath>,
    <cairoScaledFont>,
    <cairoFontFace>,
    <cairoFontType>,
    <cairoFontOptions>,
    <cairoContent*>,
    <cairoContent>,
    $cairo-content-color-alpha,
    $cairo-content-alpha,
    $cairo-content-color,
    <cairoRegion>,
    <cairoPattern>,
    <cairoMatrix>,
    <cairoSurface>,
    <cairoContext>;
end module;
