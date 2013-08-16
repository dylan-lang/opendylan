module: pango-cairo
synopsis: bindings for the pango-cairo library
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define C-struct <_PangoCairoFont>
  pointer-type-name: <PangoCairoFont>;
end;

define C-struct <_PangoCairoFontMap>
  pointer-type-name: <PangoCairoFontMap>;
end;

define constant <gpointer> = <C-void*>;
define C-pointer-type <gpointer*> => <gpointer>;

define C-function pango-cairo-font-map-get-type
  result res :: <GType>;
  c-name: "pango_cairo_font_map_get_type";
end;

define C-function pango-cairo-font-map-new
  result res :: <PangoFontMap>;
  c-name: "pango_cairo_font_map_new";
end;

define C-function pango-cairo-font-map-new-for-font-type
  input parameter fonttype_ :: <CairoFontType>;
  result res :: <PangoFontMap>;
  c-name: "pango_cairo_font_map_new_for_font_type";
end;

define C-function pango-cairo-font-map-get-default
  result res :: <PangoFontMap>;
  c-name: "pango_cairo_font_map_get_default";
end;

define C-function pango-cairo-font-map-set-default
  input parameter fontmap_ :: <PangoCairoFontMap>;
  c-name: "pango_cairo_font_map_set_default";
end;

define C-function pango-cairo-font-map-get-font-type
  input parameter fontmap_ :: <PangoCairoFontMap>;
  result res :: <CairoFontType>;
  c-name: "pango_cairo_font_map_get_font_type";
end;

define C-function pango-cairo-font-map-set-resolution
  input parameter fontmap_ :: <PangoCairoFontMap>;
  input parameter dpi_ :: <C-double>;
  c-name: "pango_cairo_font_map_set_resolution";
end;

define C-function pango-cairo-font-map-get-resolution
  input parameter fontmap_ :: <PangoCairoFontMap>;
  result res :: <C-double>;
  c-name: "pango_cairo_font_map_get_resolution";
end;

define C-function pango-cairo-font-map-create-context
  input parameter fontmap_ :: <PangoCairoFontMap>;
  result res :: <PangoContext>;
  c-name: "pango_cairo_font_map_create_context";
end;

define C-function pango-cairo-font-get-type
  result res :: <GType>;
  c-name: "pango_cairo_font_get_type";
end;

define C-function pango-cairo-font-get-scaled-font
  input parameter font_ :: <PangoCairoFont>;
  result res :: <CairoScaledFont>;
  c-name: "pango_cairo_font_get_scaled_font";
end;

define C-function pango-cairo-update-context
  input parameter cr_ :: <CairoContext>;
  input parameter context_ :: <PangoContext>;
  c-name: "pango_cairo_update_context";
end;

define C-function pango-cairo-context-set-font-options
  input parameter context_ :: <PangoContext>;
  input parameter options_ :: <CairoFontOptions>;
  c-name: "pango_cairo_context_set_font_options";
end;

define C-function pango-cairo-context-get-font-options
  input parameter context_ :: <PangoContext>;
  result res :: <CairoFontOptions>;
  c-name: "pango_cairo_context_get_font_options";
end;

define C-function pango-cairo-context-set-resolution
  input parameter context_ :: <PangoContext>;
  input parameter dpi_ :: <C-double>;
  c-name: "pango_cairo_context_set_resolution";
end;

define C-function pango-cairo-context-get-resolution
  input parameter context_ :: <PangoContext>;
  result res :: <C-double>;
  c-name: "pango_cairo_context_get_resolution";
end;

define C-function pango-cairo-context-set-shape-renderer
  input parameter context_ :: <PangoContext>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <gpointer>;
  input parameter dnotify_ :: <C-function-pointer>;
  c-name: "pango_cairo_context_set_shape_renderer";
end;

define C-function pango-cairo-context-get-shape-renderer
  input parameter context_ :: <PangoContext>;
  input parameter data_ :: <gpointer*>;
  result res :: <C-function-pointer>;
  c-name: "pango_cairo_context_get_shape_renderer";
end;

define C-function pango-cairo-create-context
  input parameter cr_ :: <CairoContext>;
  result res :: <PangoContext>;
  c-name: "pango_cairo_create_context";
end;

define C-function pango-cairo-create-layout
  input parameter cr_ :: <CairoContext>;
  result res :: <PangoLayout>;
  c-name: "pango_cairo_create_layout";
end;

define C-function pango-cairo-update-layout
  input parameter cr_ :: <CairoContext>;
  input parameter layout_ :: <PangoLayout>;
  c-name: "pango_cairo_update_layout";
end;

define C-function pango-cairo-show-glyph-string
  input parameter cr_ :: <CairoContext>;
  input parameter font_ :: <PangoFont>;
  input parameter glyphs_ :: <PangoGlyphString>;
  c-name: "pango_cairo_show_glyph_string";
end;

define C-function pango-cairo-show-glyph-item
  input parameter cr_ :: <CairoContext>;
  input parameter text_ :: <c-string>;
  input parameter glyph-item_ :: <PangoGlyphItem>;
  c-name: "pango_cairo_show_glyph_item";
end;

define C-function pango-cairo-show-layout-line
  input parameter cr_ :: <CairoContext>;
  input parameter line_ :: <PangoLayoutLine>;
  c-name: "pango_cairo_show_layout_line";
end;

define C-function pango-cairo-show-layout
  input parameter cr_ :: <CairoContext>;
  input parameter layout_ :: <PangoLayout>;
  c-name: "pango_cairo_show_layout";
end;

define C-function pango-cairo-show-error-underline
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "pango_cairo_show_error_underline";
end;

define C-function pango-cairo-glyph-string-path
  input parameter cr_ :: <CairoContext>;
  input parameter font_ :: <PangoFont>;
  input parameter glyphs_ :: <PangoGlyphString>;
  c-name: "pango_cairo_glyph_string_path";
end;

define C-function pango-cairo-layout-line-path
  input parameter cr_ :: <CairoContext>;
  input parameter line_ :: <PangoLayoutLine>;
  c-name: "pango_cairo_layout_line_path";
end;

define C-function pango-cairo-layout-path
  input parameter cr_ :: <CairoContext>;
  input parameter layout_ :: <PangoLayout>;
  c-name: "pango_cairo_layout_path";
end;

define C-function pango-cairo-error-underline-path
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "pango_cairo_error_underline_path";
end;

