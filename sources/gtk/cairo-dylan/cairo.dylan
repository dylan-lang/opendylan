module: cairo
synopsis: generated bindings for the cairo library
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define C-pointer-type <unsigned-char**> => <unsigned-char*>;
define C-pointer-type <int*> => <C-signed-int>;
define C-pointer-type <unsigned-long*> => <C-unsigned-long>;

define C-function cairo-version
  result res :: <C-signed-int>;
  c-name: "cairo_version";
end;

define C-function cairo-version-string
  result res :: <c-string>;
  c-name: "cairo_version_string";
end;

define C-struct <_CairoContext>
end;
define C-pointer-type <CairoContext> => <_CairoContext>;

define C-struct <_CairoSurface>
end;
define C-pointer-type <CairoSurface> => <_CairoSurface>;
define C-pointer-type <CairoSurface*> => <CairoSurface>;

define C-struct <_CairoDevice>
end;
define C-pointer-type <CairoDevice> => <_CairoDevice>;

define C-struct <_CairoMatrix>
  slot cairo-matrix-xx :: <C-double>;
  slot cairo-matrix-yx :: <C-double>;
  slot cairo-matrix-xy :: <C-double>;
  slot cairo-matrix-yy :: <C-double>;
  slot cairo-matrix-x0 :: <C-double>;
  slot cairo-matrix-y0 :: <C-double>;
end;
define C-pointer-type <CairoMatrix> => <_CairoMatrix>;

define C-struct <_CairoPattern>
end;
define C-pointer-type <CairoPattern> => <_CairoPattern>;

define constant <CairoDestroyFunc> = <C-function-pointer>;

define C-struct <_CairoUserDataKey>
  slot cairo-user-data-key-unused :: <C-signed-int>;
end;
define C-pointer-type <CairoUserDataKey> => <_CairoUserDataKey>;

define constant <CairoStatus> = <C-int>;
define constant $CAIRO-STATUS-SUCCESS = 0;
define constant $CAIRO-STATUS-NO-MEMORY = 1;
define constant $CAIRO-STATUS-INVALID-RESTORE = 2;
define constant $CAIRO-STATUS-INVALID-POP-GROUP = 3;
define constant $CAIRO-STATUS-NO-CURRENT-POINT = 4;
define constant $CAIRO-STATUS-INVALID-MATRIX = 5;
define constant $CAIRO-STATUS-INVALID-STATUS = 6;
define constant $CAIRO-STATUS-NULL-POINTER = 7;
define constant $CAIRO-STATUS-INVALID-STRING = 8;
define constant $CAIRO-STATUS-INVALID-PATH-DATA = 9;
define constant $CAIRO-STATUS-READ-ERROR = 10;
define constant $CAIRO-STATUS-WRITE-ERROR = 11;
define constant $CAIRO-STATUS-SURFACE-FINISHED = 12;
define constant $CAIRO-STATUS-SURFACE-TYPE-MISMATCH = 13;
define constant $CAIRO-STATUS-PATTERN-TYPE-MISMATCH = 14;
define constant $CAIRO-STATUS-INVALID-CONTENT = 15;
define constant $CAIRO-STATUS-INVALID-FORMAT = 16;
define constant $CAIRO-STATUS-INVALID-VISUAL = 17;
define constant $CAIRO-STATUS-FILE-NOT-FOUND = 18;
define constant $CAIRO-STATUS-INVALID-DASH = 19;
define constant $CAIRO-STATUS-INVALID-DSC-COMMENT = 20;
define constant $CAIRO-STATUS-INVALID-INDEX = 21;
define constant $CAIRO-STATUS-CLIP-NOT-REPRESENTABLE = 22;
define constant $CAIRO-STATUS-TEMP-FILE-ERROR = 23;
define constant $CAIRO-STATUS-INVALID-STRIDE = 24;
define constant $CAIRO-STATUS-FONT-TYPE-MISMATCH = 25;
define constant $CAIRO-STATUS-USER-FONT-IMMUTABLE = 26;
define constant $CAIRO-STATUS-USER-FONT-ERROR = 27;
define constant $CAIRO-STATUS-NEGATIVE-COUNT = 28;
define constant $CAIRO-STATUS-INVALID-CLUSTERS = 29;
define constant $CAIRO-STATUS-INVALID-SLANT = 30;
define constant $CAIRO-STATUS-INVALID-WEIGHT = 31;
define constant $CAIRO-STATUS-INVALID-SIZE = 32;
define constant $CAIRO-STATUS-USER-FONT-NOT-IMPLEMENTED = 33;
define constant $CAIRO-STATUS-DEVICE-TYPE-MISMATCH = 34;
define constant $CAIRO-STATUS-DEVICE-ERROR = 35;
define constant $CAIRO-STATUS-LAST-STATUS = 36;

define constant <CairoContent> = <C-int>;
define constant $CAIRO-CONTENT-COLOR = 4096;
define constant $CAIRO-CONTENT-ALPHA = 8192;
define constant $CAIRO-CONTENT-COLOR-ALPHA = 12288;

define C-pointer-type <unsigned-char*> => <C-unsigned-char>;
define constant <CairoWriteFunc> = <C-function-pointer>;
define constant <CairoReadFunc> = <C-function-pointer>;
define C-function cairo-create
  input parameter target_ :: <CairoSurface>;
  result res :: <CairoContext>;
  c-name: "cairo_create";
end;

define C-function cairo-reference
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoContext>;
  c-name: "cairo_reference";
end;

define C-function cairo-destroy
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_destroy";
end;

define C-function cairo-get-reference-count
  input parameter cr_ :: <CairoContext>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_get_reference_count";
end;

define C-function cairo-get-user-data
  input parameter cr_ :: <CairoContext>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_get_user_data";
end;

define C-function cairo-set-user-data
  input parameter cr_ :: <CairoContext>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_set_user_data";
end;

define C-function cairo-save
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_save";
end;

define C-function cairo-restore
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_restore";
end;

define C-function cairo-push-group
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_push_group";
end;

define C-function cairo-push-group-with-content
  input parameter cr_ :: <CairoContext>;
  input parameter content_ :: <CairoContent>;
  c-name: "cairo_push_group_with_content";
end;

define C-function cairo-pop-group
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoPattern>;
  c-name: "cairo_pop_group";
end;

define C-function cairo-pop-group-to-source
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_pop_group_to_source";
end;

define constant <CairoOperator> = <C-int>;
define constant $CAIRO-OPERATOR-CLEAR = 0;
define constant $CAIRO-OPERATOR-SOURCE = 1;
define constant $CAIRO-OPERATOR-OVER = 2;
define constant $CAIRO-OPERATOR-IN = 3;
define constant $CAIRO-OPERATOR-OUT = 4;
define constant $CAIRO-OPERATOR-ATOP = 5;
define constant $CAIRO-OPERATOR-DEST = 6;
define constant $CAIRO-OPERATOR-DEST-OVER = 7;
define constant $CAIRO-OPERATOR-DEST-IN = 8;
define constant $CAIRO-OPERATOR-DEST-OUT = 9;
define constant $CAIRO-OPERATOR-DEST-ATOP = 10;
define constant $CAIRO-OPERATOR-XOR = 11;
define constant $CAIRO-OPERATOR-ADD = 12;
define constant $CAIRO-OPERATOR-SATURATE = 13;
define constant $CAIRO-OPERATOR-MULTIPLY = 14;
define constant $CAIRO-OPERATOR-SCREEN = 15;
define constant $CAIRO-OPERATOR-OVERLAY = 16;
define constant $CAIRO-OPERATOR-DARKEN = 17;
define constant $CAIRO-OPERATOR-LIGHTEN = 18;
define constant $CAIRO-OPERATOR-COLOR-DODGE = 19;
define constant $CAIRO-OPERATOR-COLOR-BURN = 20;
define constant $CAIRO-OPERATOR-HARD-LIGHT = 21;
define constant $CAIRO-OPERATOR-SOFT-LIGHT = 22;
define constant $CAIRO-OPERATOR-DIFFERENCE = 23;
define constant $CAIRO-OPERATOR-EXCLUSION = 24;
define constant $CAIRO-OPERATOR-HSL-HUE = 25;
define constant $CAIRO-OPERATOR-HSL-SATURATION = 26;
define constant $CAIRO-OPERATOR-HSL-COLOR = 27;
define constant $CAIRO-OPERATOR-HSL-LUMINOSITY = 28;

define C-function cairo-set-operator
  input parameter cr_ :: <CairoContext>;
  input parameter op_ :: <CairoOperator>;
  c-name: "cairo_set_operator";
end;

define C-function cairo-set-source
  input parameter cr_ :: <CairoContext>;
  input parameter source_ :: <CairoPattern>;
  c-name: "cairo_set_source";
end;

define C-function cairo-set-source-rgb
  input parameter cr_ :: <CairoContext>;
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  c-name: "cairo_set_source_rgb";
end;

define C-function cairo-set-source-rgba
  input parameter cr_ :: <CairoContext>;
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  input parameter alpha_ :: <C-double>;
  c-name: "cairo_set_source_rgba";
end;

define C-function cairo-set-source-surface
  input parameter cr_ :: <CairoContext>;
  input parameter surface_ :: <CairoSurface>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "cairo_set_source_surface";
end;

define C-function cairo-set-tolerance
  input parameter cr_ :: <CairoContext>;
  input parameter tolerance_ :: <C-double>;
  c-name: "cairo_set_tolerance";
end;

define constant <CairoAntialias> = <C-int>;
define constant $CAIRO-ANTIALIAS-DEFAULT = 0;
define constant $CAIRO-ANTIALIAS-NONE = 1;
define constant $CAIRO-ANTIALIAS-GRAY = 2;
define constant $CAIRO-ANTIALIAS-SUBPIXEL = 3;

define C-function cairo-set-antialias
  input parameter cr_ :: <CairoContext>;
  input parameter antialias_ :: <CairoAntialias>;
  c-name: "cairo_set_antialias";
end;

define constant <CairoFillRule> = <C-int>;
define constant $CAIRO-FILL-RULE-WINDING = 0;
define constant $CAIRO-FILL-RULE-EVEN-ODD = 1;

define C-function cairo-set-fill-rule
  input parameter cr_ :: <CairoContext>;
  input parameter fill-rule_ :: <CairoFillRule>;
  c-name: "cairo_set_fill_rule";
end;

define C-function cairo-set-line-width
  input parameter cr_ :: <CairoContext>;
  input parameter width_ :: <C-double>;
  c-name: "cairo_set_line_width";
end;

define constant <CairoLineCap> = <C-int>;
define constant $CAIRO-LINE-CAP-BUTT = 0;
define constant $CAIRO-LINE-CAP-ROUND = 1;
define constant $CAIRO-LINE-CAP-SQUARE = 2;

define C-function cairo-set-line-cap
  input parameter cr_ :: <CairoContext>;
  input parameter line-cap_ :: <CairoLineCap>;
  c-name: "cairo_set_line_cap";
end;

define constant <CairoLineJoin> = <C-int>;
define constant $CAIRO-LINE-JOIN-MITER = 0;
define constant $CAIRO-LINE-JOIN-ROUND = 1;
define constant $CAIRO-LINE-JOIN-BEVEL = 2;

define C-function cairo-set-line-join
  input parameter cr_ :: <CairoContext>;
  input parameter line-join_ :: <CairoLineJoin>;
  c-name: "cairo_set_line_join";
end;

define C-pointer-type <double*> => <C-double>;
define C-function cairo-set-dash
  input parameter cr_ :: <CairoContext>;
  input parameter dashes_ :: <double*>;
  input parameter num-dashes_ :: <C-signed-int>;
  input parameter offset_ :: <C-double>;
  c-name: "cairo_set_dash";
end;

define C-function cairo-set-miter-limit
  input parameter cr_ :: <CairoContext>;
  input parameter limit_ :: <C-double>;
  c-name: "cairo_set_miter_limit";
end;

define C-function cairo-translate
  input parameter cr_ :: <CairoContext>;
  input parameter tx_ :: <C-double>;
  input parameter ty_ :: <C-double>;
  c-name: "cairo_translate";
end;

define C-function cairo-scale
  input parameter cr_ :: <CairoContext>;
  input parameter sx_ :: <C-double>;
  input parameter sy_ :: <C-double>;
  c-name: "cairo_scale";
end;

define C-function cairo-rotate
  input parameter cr_ :: <CairoContext>;
  input parameter angle_ :: <C-double>;
  c-name: "cairo_rotate";
end;

define C-function cairo-transform
  input parameter cr_ :: <CairoContext>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_transform";
end;

define C-function cairo-set-matrix
  input parameter cr_ :: <CairoContext>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_set_matrix";
end;

define C-function cairo-identity-matrix
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_identity_matrix";
end;

define C-function cairo-user-to-device
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <double*>;
  input parameter y_ :: <double*>;
  c-name: "cairo_user_to_device";
end;

define C-function cairo-user-to-device-distance
  input parameter cr_ :: <CairoContext>;
  input parameter dx_ :: <double*>;
  input parameter dy_ :: <double*>;
  c-name: "cairo_user_to_device_distance";
end;

define C-function cairo-device-to-user
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <double*>;
  input parameter y_ :: <double*>;
  c-name: "cairo_device_to_user";
end;

define C-function cairo-device-to-user-distance
  input parameter cr_ :: <CairoContext>;
  input parameter dx_ :: <double*>;
  input parameter dy_ :: <double*>;
  c-name: "cairo_device_to_user_distance";
end;

define C-function cairo-new-path
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_new_path";
end;

define C-function cairo-move-to
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "cairo_move_to";
end;

define C-function cairo-new-sub-path
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_new_sub_path";
end;

define C-function cairo-line-to
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "cairo_line_to";
end;

define C-function cairo-curve-to
  input parameter cr_ :: <CairoContext>;
  input parameter x1_ :: <C-double>;
  input parameter y1_ :: <C-double>;
  input parameter x2_ :: <C-double>;
  input parameter y2_ :: <C-double>;
  input parameter x3_ :: <C-double>;
  input parameter y3_ :: <C-double>;
  c-name: "cairo_curve_to";
end;

define C-function cairo-arc
  input parameter cr_ :: <CairoContext>;
  input parameter xc_ :: <C-double>;
  input parameter yc_ :: <C-double>;
  input parameter radius_ :: <C-double>;
  input parameter angle1_ :: <C-double>;
  input parameter angle2_ :: <C-double>;
  c-name: "cairo_arc";
end;

define C-function cairo-arc-negative
  input parameter cr_ :: <CairoContext>;
  input parameter xc_ :: <C-double>;
  input parameter yc_ :: <C-double>;
  input parameter radius_ :: <C-double>;
  input parameter angle1_ :: <C-double>;
  input parameter angle2_ :: <C-double>;
  c-name: "cairo_arc_negative";
end;

define C-function cairo-rel-move-to
  input parameter cr_ :: <CairoContext>;
  input parameter dx_ :: <C-double>;
  input parameter dy_ :: <C-double>;
  c-name: "cairo_rel_move_to";
end;

define C-function cairo-rel-line-to
  input parameter cr_ :: <CairoContext>;
  input parameter dx_ :: <C-double>;
  input parameter dy_ :: <C-double>;
  c-name: "cairo_rel_line_to";
end;

define C-function cairo-rel-curve-to
  input parameter cr_ :: <CairoContext>;
  input parameter dx1_ :: <C-double>;
  input parameter dy1_ :: <C-double>;
  input parameter dx2_ :: <C-double>;
  input parameter dy2_ :: <C-double>;
  input parameter dx3_ :: <C-double>;
  input parameter dy3_ :: <C-double>;
  c-name: "cairo_rel_curve_to";
end;

define C-function cairo-rectangle
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "cairo_rectangle";
end;

define C-function cairo-close-path
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_close_path";
end;

define C-function cairo-path-extents
  input parameter cr_ :: <CairoContext>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  input parameter x2_ :: <double*>;
  input parameter y2_ :: <double*>;
  c-name: "cairo_path_extents";
end;

define C-function cairo-paint
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_paint";
end;

define C-function cairo-paint-with-alpha
  input parameter cr_ :: <CairoContext>;
  input parameter alpha_ :: <C-double>;
  c-name: "cairo_paint_with_alpha";
end;

define C-function cairo-mask
  input parameter cr_ :: <CairoContext>;
  input parameter pattern_ :: <CairoPattern>;
  c-name: "cairo_mask";
end;

define C-function cairo-mask-surface
  input parameter cr_ :: <CairoContext>;
  input parameter surface_ :: <CairoSurface>;
  input parameter surface-x_ :: <C-double>;
  input parameter surface-y_ :: <C-double>;
  c-name: "cairo_mask_surface";
end;

define C-function cairo-stroke
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_stroke";
end;

define C-function cairo-stroke-preserve
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_stroke_preserve";
end;

define C-function cairo-fill
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_fill";
end;

define C-function cairo-fill-preserve
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_fill_preserve";
end;

define C-function cairo-copy-page
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_copy_page";
end;

define C-function cairo-show-page
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_show_page";
end;

define C-function cairo-in-stroke
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  result res :: <C-boolean>;
  c-name: "cairo_in_stroke";
end;

define C-function cairo-in-fill
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  result res :: <C-boolean>;
  c-name: "cairo_in_fill";
end;

define C-function cairo-in-clip
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  result res :: <C-boolean>;
  c-name: "cairo_in_clip";
end;

define C-function cairo-stroke-extents
  input parameter cr_ :: <CairoContext>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  input parameter x2_ :: <double*>;
  input parameter y2_ :: <double*>;
  c-name: "cairo_stroke_extents";
end;

define C-function cairo-fill-extents
  input parameter cr_ :: <CairoContext>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  input parameter x2_ :: <double*>;
  input parameter y2_ :: <double*>;
  c-name: "cairo_fill_extents";
end;

define C-function cairo-reset-clip
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_reset_clip";
end;

define C-function cairo-clip
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_clip";
end;

define C-function cairo-clip-preserve
  input parameter cr_ :: <CairoContext>;
  c-name: "cairo_clip_preserve";
end;

define C-function cairo-clip-extents
  input parameter cr_ :: <CairoContext>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  input parameter x2_ :: <double*>;
  input parameter y2_ :: <double*>;
  c-name: "cairo_clip_extents";
end;

define C-struct <_CairoRectangle>
  slot cairo-rectangle-x :: <C-double>;
  slot cairo-rectangle-y :: <C-double>;
  slot cairo-rectangle-width :: <C-double>;
  slot cairo-rectangle-height :: <C-double>;
end;
define C-pointer-type <CairoRectangle> => <_CairoRectangle>;

define C-struct <_CairoRectangleList>
  slot cairo-rectangle-list-status :: <CairoStatus>;
  slot cairo-rectangle-list-rectangles :: <CairoRectangle>;
  slot cairo-rectangle-list-num-rectangles :: <C-signed-int>;
end;
define C-pointer-type <CairoRectangleList> => <_CairoRectangleList>;

define C-function cairo-copy-clip-rectangle-list
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoRectangleList>;
  c-name: "cairo_copy_clip_rectangle_list";
end;

define C-function cairo-rectangle-list-destroy
  input parameter rectangle-list_ :: <CairoRectangleList>;
  c-name: "cairo_rectangle_list_destroy";
end;

define C-struct <_CairoScaledFont>
end;
define C-pointer-type <CairoScaledFont> => <_CairoScaledFont>;

define C-struct <_CairoFontFace>
end;
define C-pointer-type <CairoFontFace> => <_CairoFontFace>;

define C-struct <_CairoGlyph>
  slot cairo-glyph-index :: <C-unsigned-long>;
  slot cairo-glyph-x :: <C-double>;
  slot cairo-glyph-y :: <C-double>;
end;
define C-pointer-type <CairoGlyph> => <_CairoGlyph>;
define C-pointer-type <CairoGlyph*> => <CairoGlyph>;

define C-function cairo-glyph-allocate
  input parameter num-glyphs_ :: <C-signed-int>;
  result res :: <CairoGlyph>;
  c-name: "cairo_glyph_allocate";
end;

define C-function cairo-glyph-free
  input parameter glyphs_ :: <CairoGlyph>;
  c-name: "cairo_glyph_free";
end;

define C-struct <_CairoTextCluster>
  slot cairo-text-cluster-num-bytes :: <C-signed-int>;
  slot cairo-text-cluster-num-glyphs :: <C-signed-int>;
end;
define C-pointer-type <CairoTextCluster> => <_CairoTextCluster>;
define C-pointer-type <CairoTextCluster*> => <CairoTextCluster>;

define C-function cairo-text-cluster-allocate
  input parameter num-clusters_ :: <C-signed-int>;
  result res :: <CairoTextCluster>;
  c-name: "cairo_text_cluster_allocate";
end;

define C-function cairo-text-cluster-free
  input parameter clusters_ :: <CairoTextCluster>;
  c-name: "cairo_text_cluster_free";
end;

define constant <CairoTextClusterFlags> = <C-int>;
define constant $CAIRO-TEXT-CLUSTER-FLAG-BACKWARD = 1;
define C-pointer-type <CairoTextClusterFlags*> => <CairoTextClusterFlags>;

define C-struct <_CairoTextExtents>
  slot cairo-text-extents-x-bearing :: <C-double>;
  slot cairo-text-extents-y-bearing :: <C-double>;
  slot cairo-text-extents-width :: <C-double>;
  slot cairo-text-extents-height :: <C-double>;
  slot cairo-text-extents-x-advance :: <C-double>;
  slot cairo-text-extents-y-advance :: <C-double>;
end;
define C-pointer-type <CairoTextExtents> => <_CairoTextExtents>;

define C-struct <_CairoFontExtents>
  slot cairo-font-extents-ascent :: <C-double>;
  slot cairo-font-extents-descent :: <C-double>;
  slot cairo-font-extents-height :: <C-double>;
  slot cairo-font-extents-max-x-advance :: <C-double>;
  slot cairo-font-extents-max-y-advance :: <C-double>;
end;
define C-pointer-type <CairoFontExtents> => <_CairoFontExtents>;

define constant <CairoFontSlant> = <C-int>;
define constant $CAIRO-FONT-SLANT-NORMAL = 0;
define constant $CAIRO-FONT-SLANT-ITALIC = 1;
define constant $CAIRO-FONT-SLANT-OBLIQUE = 2;

define constant <CairoFontWeight> = <C-int>;
define constant $CAIRO-FONT-WEIGHT-NORMAL = 0;
define constant $CAIRO-FONT-WEIGHT-BOLD = 1;

define constant <CairoSubpixelOrder> = <C-int>;
define constant $CAIRO-SUBPIXEL-ORDER-DEFAULT = 0;
define constant $CAIRO-SUBPIXEL-ORDER-RGB = 1;
define constant $CAIRO-SUBPIXEL-ORDER-BGR = 2;
define constant $CAIRO-SUBPIXEL-ORDER-VRGB = 3;
define constant $CAIRO-SUBPIXEL-ORDER-VBGR = 4;

define constant <CairoHintStyle> = <C-int>;
define constant $CAIRO-HINT-STYLE-DEFAULT = 0;
define constant $CAIRO-HINT-STYLE-NONE = 1;
define constant $CAIRO-HINT-STYLE-SLIGHT = 2;
define constant $CAIRO-HINT-STYLE-MEDIUM = 3;
define constant $CAIRO-HINT-STYLE-FULL = 4;

define constant <CairoHintMetrics> = <C-int>;
define constant $CAIRO-HINT-METRICS-DEFAULT = 0;
define constant $CAIRO-HINT-METRICS-OFF = 1;
define constant $CAIRO-HINT-METRICS-ON = 2;

define C-struct <_CairoFontOptions>
end;
define C-pointer-type <CairoFontOptions> => <_CairoFontOptions>;

define C-function cairo-font-options-create
  result res :: <CairoFontOptions>;
  c-name: "cairo_font_options_create";
end;

define C-function cairo-font-options-copy
  input parameter original_ :: <CairoFontOptions>;
  result res :: <CairoFontOptions>;
  c-name: "cairo_font_options_copy";
end;

define C-function cairo-font-options-destroy
  input parameter options_ :: <CairoFontOptions>;
  c-name: "cairo_font_options_destroy";
end;

define C-function cairo-font-options-status
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoStatus>;
  c-name: "cairo_font_options_status";
end;

define C-function cairo-font-options-merge
  input parameter options_ :: <CairoFontOptions>;
  input parameter other_ :: <CairoFontOptions>;
  c-name: "cairo_font_options_merge";
end;

define C-function cairo-font-options-equal
  input parameter options_ :: <CairoFontOptions>;
  input parameter other_ :: <CairoFontOptions>;
  result res :: <C-boolean>;
  c-name: "cairo_font_options_equal";
end;

define C-function cairo-font-options-hash
  input parameter options_ :: <CairoFontOptions>;
  result res :: <C-unsigned-long>;
  c-name: "cairo_font_options_hash";
end;

define C-function cairo-font-options-set-antialias
  input parameter options_ :: <CairoFontOptions>;
  input parameter antialias_ :: <CairoAntialias>;
  c-name: "cairo_font_options_set_antialias";
end;

define C-function cairo-font-options-get-antialias
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoAntialias>;
  c-name: "cairo_font_options_get_antialias";
end;

define C-function cairo-font-options-set-subpixel-order
  input parameter options_ :: <CairoFontOptions>;
  input parameter subpixel-order_ :: <CairoSubpixelOrder>;
  c-name: "cairo_font_options_set_subpixel_order";
end;

define C-function cairo-font-options-get-subpixel-order
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoSubpixelOrder>;
  c-name: "cairo_font_options_get_subpixel_order";
end;

define C-function cairo-font-options-set-hint-style
  input parameter options_ :: <CairoFontOptions>;
  input parameter hint-style_ :: <CairoHintStyle>;
  c-name: "cairo_font_options_set_hint_style";
end;

define C-function cairo-font-options-get-hint-style
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoHintStyle>;
  c-name: "cairo_font_options_get_hint_style";
end;

define C-function cairo-font-options-set-hint-metrics
  input parameter options_ :: <CairoFontOptions>;
  input parameter hint-metrics_ :: <CairoHintMetrics>;
  c-name: "cairo_font_options_set_hint_metrics";
end;

define C-function cairo-font-options-get-hint-metrics
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoHintMetrics>;
  c-name: "cairo_font_options_get_hint_metrics";
end;

define C-function cairo-select-font-face
  input parameter cr_ :: <CairoContext>;
  input parameter family_ :: <c-string>;
  input parameter slant_ :: <CairoFontSlant>;
  input parameter weight_ :: <CairoFontWeight>;
  c-name: "cairo_select_font_face";
end;

define C-function cairo-set-font-size
  input parameter cr_ :: <CairoContext>;
  input parameter size_ :: <C-double>;
  c-name: "cairo_set_font_size";
end;

define C-function cairo-set-font-matrix
  input parameter cr_ :: <CairoContext>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_set_font_matrix";
end;

define C-function cairo-get-font-matrix
  input parameter cr_ :: <CairoContext>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_get_font_matrix";
end;

define C-function cairo-set-font-options
  input parameter cr_ :: <CairoContext>;
  input parameter options_ :: <CairoFontOptions>;
  c-name: "cairo_set_font_options";
end;

define C-function cairo-get-font-options
  input parameter cr_ :: <CairoContext>;
  input parameter options_ :: <CairoFontOptions>;
  c-name: "cairo_get_font_options";
end;

define C-function cairo-set-font-face
  input parameter cr_ :: <CairoContext>;
  input parameter font-face_ :: <CairoFontFace>;
  c-name: "cairo_set_font_face";
end;

define C-function cairo-get-font-face
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoFontFace>;
  c-name: "cairo_get_font_face";
end;

define C-function cairo-set-scaled-font
  input parameter cr_ :: <CairoContext>;
  input parameter scaled-font_ :: <CairoScaledFont>;
  c-name: "cairo_set_scaled_font";
end;

define C-function cairo-get-scaled-font
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoScaledFont>;
  c-name: "cairo_get_scaled_font";
end;

define C-function cairo-show-text
  input parameter cr_ :: <CairoContext>;
  input parameter utf8_ :: <c-string>;
  c-name: "cairo_show_text";
end;

define C-function cairo-show-glyphs
  input parameter cr_ :: <CairoContext>;
  input parameter glyphs_ :: <CairoGlyph>;
  input parameter num-glyphs_ :: <C-signed-int>;
  c-name: "cairo_show_glyphs";
end;

define C-function cairo-show-text-glyphs
  input parameter cr_ :: <CairoContext>;
  input parameter utf8_ :: <c-string>;
  input parameter utf8-len_ :: <C-signed-int>;
  input parameter glyphs_ :: <CairoGlyph>;
  input parameter num-glyphs_ :: <C-signed-int>;
  input parameter clusters_ :: <CairoTextCluster>;
  input parameter num-clusters_ :: <C-signed-int>;
  input parameter cluster-flags_ :: <CairoTextClusterFlags>;
  c-name: "cairo_show_text_glyphs";
end;

define C-function cairo-text-path
  input parameter cr_ :: <CairoContext>;
  input parameter utf8_ :: <c-string>;
  c-name: "cairo_text_path";
end;

define C-function cairo-glyph-path
  input parameter cr_ :: <CairoContext>;
  input parameter glyphs_ :: <CairoGlyph>;
  input parameter num-glyphs_ :: <C-signed-int>;
  c-name: "cairo_glyph_path";
end;

define C-function cairo-text-extents
  input parameter cr_ :: <CairoContext>;
  input parameter utf8_ :: <c-string>;
  input parameter extents_ :: <CairoTextExtents>;
  c-name: "cairo_text_extents";
end;

define C-function cairo-glyph-extents
  input parameter cr_ :: <CairoContext>;
  input parameter glyphs_ :: <CairoGlyph>;
  input parameter num-glyphs_ :: <C-signed-int>;
  input parameter extents_ :: <CairoTextExtents>;
  c-name: "cairo_glyph_extents";
end;

define C-function cairo-font-extents
  input parameter cr_ :: <CairoContext>;
  input parameter extents_ :: <CairoFontExtents>;
  c-name: "cairo_font_extents";
end;

define C-function cairo-font-face-reference
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoFontFace>;
  c-name: "cairo_font_face_reference";
end;

define C-function cairo-font-face-destroy
  input parameter font-face_ :: <CairoFontFace>;
  c-name: "cairo_font_face_destroy";
end;

define C-function cairo-font-face-get-reference-count
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_font_face_get_reference_count";
end;

define C-function cairo-font-face-status
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoStatus>;
  c-name: "cairo_font_face_status";
end;

define constant <CairoFontType> = <C-int>;
define constant $CAIRO-FONT-TYPE-TOY = 0;
define constant $CAIRO-FONT-TYPE-FT = 1;
define constant $CAIRO-FONT-TYPE-WIN32 = 2;
define constant $CAIRO-FONT-TYPE-QUARTZ = 3;
define constant $CAIRO-FONT-TYPE-USER = 4;

define C-function cairo-font-face-get-type
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoFontType>;
  c-name: "cairo_font_face_get_type";
end;

define C-function cairo-font-face-get-user-data
  input parameter font-face_ :: <CairoFontFace>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_font_face_get_user_data";
end;

define C-function cairo-font-face-set-user-data
  input parameter font-face_ :: <CairoFontFace>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_font_face_set_user_data";
end;

define C-function cairo-scaled-font-create
  input parameter font-face_ :: <CairoFontFace>;
  input parameter font-matrix_ :: <CairoMatrix>;
  input parameter ctm_ :: <CairoMatrix>;
  input parameter options_ :: <CairoFontOptions>;
  result res :: <CairoScaledFont>;
  c-name: "cairo_scaled_font_create";
end;

define C-function cairo-scaled-font-reference
  input parameter scaled-font_ :: <CairoScaledFont>;
  result res :: <CairoScaledFont>;
  c-name: "cairo_scaled_font_reference";
end;

define C-function cairo-scaled-font-destroy
  input parameter scaled-font_ :: <CairoScaledFont>;
  c-name: "cairo_scaled_font_destroy";
end;

define C-function cairo-scaled-font-get-reference-count
  input parameter scaled-font_ :: <CairoScaledFont>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_scaled_font_get_reference_count";
end;

define C-function cairo-scaled-font-status
  input parameter scaled-font_ :: <CairoScaledFont>;
  result res :: <CairoStatus>;
  c-name: "cairo_scaled_font_status";
end;

define C-function cairo-scaled-font-get-type
  input parameter scaled-font_ :: <CairoScaledFont>;
  result res :: <CairoFontType>;
  c-name: "cairo_scaled_font_get_type";
end;

define C-function cairo-scaled-font-get-user-data
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_scaled_font_get_user_data";
end;

define C-function cairo-scaled-font-set-user-data
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_scaled_font_set_user_data";
end;

define C-function cairo-scaled-font-extents
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter extents_ :: <CairoFontExtents>;
  c-name: "cairo_scaled_font_extents";
end;

define C-function cairo-scaled-font-text-extents
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter utf8_ :: <c-string>;
  input parameter extents_ :: <CairoTextExtents>;
  c-name: "cairo_scaled_font_text_extents";
end;

define C-function cairo-scaled-font-glyph-extents
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter glyphs_ :: <CairoGlyph>;
  input parameter num-glyphs_ :: <C-signed-int>;
  input parameter extents_ :: <CairoTextExtents>;
  c-name: "cairo_scaled_font_glyph_extents";
end;

define C-function cairo-scaled-font-text-to-glyphs
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter utf8_ :: <c-string>;
  input parameter utf8-len_ :: <C-signed-int>;
  input parameter glyphs_ :: <CairoGlyph*>;
  input parameter num-glyphs_ :: <int*>;
  input parameter clusters_ :: <CairoTextCluster*>;
  input parameter num-clusters_ :: <int*>;
  input parameter cluster-flags_ :: <CairoTextClusterFlags*>;
  result res :: <CairoStatus>;
  c-name: "cairo_scaled_font_text_to_glyphs";
end;

define C-function cairo-scaled-font-get-font-face
  input parameter scaled-font_ :: <CairoScaledFont>;
  result res :: <CairoFontFace>;
  c-name: "cairo_scaled_font_get_font_face";
end;

define C-function cairo-scaled-font-get-font-matrix
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter font-matrix_ :: <CairoMatrix>;
  c-name: "cairo_scaled_font_get_font_matrix";
end;

define C-function cairo-scaled-font-get-ctm
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter ctm_ :: <CairoMatrix>;
  c-name: "cairo_scaled_font_get_ctm";
end;

define C-function cairo-scaled-font-get-scale-matrix
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter scale-matrix_ :: <CairoMatrix>;
  c-name: "cairo_scaled_font_get_scale_matrix";
end;

define C-function cairo-scaled-font-get-font-options
  input parameter scaled-font_ :: <CairoScaledFont>;
  input parameter options_ :: <CairoFontOptions>;
  c-name: "cairo_scaled_font_get_font_options";
end;

define C-function cairo-toy-font-face-create
  input parameter family_ :: <c-string>;
  input parameter slant_ :: <CairoFontSlant>;
  input parameter weight_ :: <CairoFontWeight>;
  result res :: <CairoFontFace>;
  c-name: "cairo_toy_font_face_create";
end;

define C-function cairo-toy-font-face-get-family
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <c-string>;
  c-name: "cairo_toy_font_face_get_family";
end;

define C-function cairo-toy-font-face-get-slant
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoFontSlant>;
  c-name: "cairo_toy_font_face_get_slant";
end;

define C-function cairo-toy-font-face-get-weight
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoFontWeight>;
  c-name: "cairo_toy_font_face_get_weight";
end;

define C-function cairo-user-font-face-create
  result res :: <CairoFontFace>;
  c-name: "cairo_user_font_face_create";
end;

define constant <CairoUserScaledFontInitFunc> = <C-function-pointer>;
define constant <CairoUserScaledFontRenderGlyphFunc> = <C-function-pointer>;
define constant <CairoUserScaledFontTextToGlyphsFunc> = <C-function-pointer>;
define constant <CairoUserScaledFontUnicodeToGlyphFunc> = <C-function-pointer>;


define C-function cairo-user-font-face-set-init-func
  input parameter font-face_ :: <CairoFontFace>;
  input parameter init-func_ :: <CairoUserScaledFontInitFunc>;
  c-name: "cairo_user_font_face_set_init_func";
end;

define C-function cairo-user-font-face-set-render-glyph-func
  input parameter font-face_ :: <CairoFontFace>;
  input parameter render-glyph-func_ :: <CairoUserScaledFontRenderGlyphFunc>;
  c-name: "cairo_user_font_face_set_render_glyph_func";
end;

define C-function cairo-user-font-face-set-text-to-glyphs-func
  input parameter font-face_ :: <CairoFontFace>;
  input parameter text-to-glyphs-func_ :: <CairoUserScaledFontTextToGlyphsFunc>;
  c-name: "cairo_user_font_face_set_text_to_glyphs_func";
end;

define C-function cairo-user-font-face-set-unicode-to-glyph-func
  input parameter font-face_ :: <CairoFontFace>;
  input parameter unicode-to-glyph-func_ :: <CairoUserScaledFontUnicodeToGlyphFunc>;
  c-name: "cairo_user_font_face_set_unicode_to_glyph_func";
end;

define C-function cairo-user-font-face-get-init-func
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoUserScaledFontInitFunc>;
  c-name: "cairo_user_font_face_get_init_func";
end;

define C-function cairo-user-font-face-get-render-glyph-func
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoUserScaledFontRenderGlyphFunc>;
  c-name: "cairo_user_font_face_get_render_glyph_func";
end;

define C-function cairo-user-font-face-get-text-to-glyphs-func
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoUserScaledFontTextToGlyphsFunc>;
  c-name: "cairo_user_font_face_get_text_to_glyphs_func";
end;

define C-function cairo-user-font-face-get-unicode-to-glyph-func
  input parameter font-face_ :: <CairoFontFace>;
  result res :: <CairoUserScaledFontUnicodeToGlyphFunc>;
  c-name: "cairo_user_font_face_get_unicode_to_glyph_func";
end;

define C-function cairo-get-operator
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoOperator>;
  c-name: "cairo_get_operator";
end;

define C-function cairo-get-source
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoPattern>;
  c-name: "cairo_get_source";
end;

define C-function cairo-get-tolerance
  input parameter cr_ :: <CairoContext>;
  result res :: <C-double>;
  c-name: "cairo_get_tolerance";
end;

define C-function cairo-get-antialias
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoAntialias>;
  c-name: "cairo_get_antialias";
end;

define C-function cairo-has-current-point
  input parameter cr_ :: <CairoContext>;
  result res :: <C-boolean>;
  c-name: "cairo_has_current_point";
end;

define C-function cairo-get-current-point
  input parameter cr_ :: <CairoContext>;
  input parameter x_ :: <double*>;
  input parameter y_ :: <double*>;
  c-name: "cairo_get_current_point";
end;

define C-function cairo-get-fill-rule
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoFillRule>;
  c-name: "cairo_get_fill_rule";
end;

define C-function cairo-get-line-width
  input parameter cr_ :: <CairoContext>;
  result res :: <C-double>;
  c-name: "cairo_get_line_width";
end;

define C-function cairo-get-line-cap
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoLineCap>;
  c-name: "cairo_get_line_cap";
end;

define C-function cairo-get-line-join
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoLineJoin>;
  c-name: "cairo_get_line_join";
end;

define C-function cairo-get-miter-limit
  input parameter cr_ :: <CairoContext>;
  result res :: <C-double>;
  c-name: "cairo_get_miter_limit";
end;

define C-function cairo-get-dash-count
  input parameter cr_ :: <CairoContext>;
  result res :: <C-signed-int>;
  c-name: "cairo_get_dash_count";
end;

define C-function cairo-get-dash
  input parameter cr_ :: <CairoContext>;
  input parameter dashes_ :: <double*>;
  input parameter offset_ :: <double*>;
  c-name: "cairo_get_dash";
end;

define C-function cairo-get-matrix
  input parameter cr_ :: <CairoContext>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_get_matrix";
end;

define C-function cairo-get-target
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoSurface>;
  c-name: "cairo_get_target";
end;

define C-function cairo-get-group-target
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoSurface>;
  c-name: "cairo_get_group_target";
end;

define constant <CairoPathDataType> = <C-int>;
define constant $CAIRO-PATH-MOVE-TO = 0;
define constant $CAIRO-PATH-LINE-TO = 1;
define constant $CAIRO-PATH-CURVE-TO = 2;
define constant $CAIRO-PATH-CLOSE-PATH = 3;

define C-struct <anonymous-165>
  slot anonymous-165-type :: <CairoPathDataType>;
  slot anonymous-165-length :: <C-signed-int>;
end;

define C-struct <anonymous-166>
  slot anonymous-166-x :: <C-double>;
  slot anonymous-166-y :: <C-double>;
end;

define C-union <_CairoPathData>
  slot cairo-path-data-header :: <anonymous-165>;
  slot cairo-path-data-point :: <anonymous-166>;
end;
define C-pointer-type <CairoPathData> => <_CairoPathData>;

define C-struct <_CairoPath>
  slot cairo-path-status :: <CairoStatus>;
  slot cairo-path-data :: <CairoPathData>;
  slot cairo-path-num-data :: <C-signed-int>;
end;
define C-pointer-type <CairoPath> => <_CairoPath>;

define C-function cairo-copy-path
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoPath>;
  c-name: "cairo_copy_path";
end;

define C-function cairo-copy-path-flat
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoPath>;
  c-name: "cairo_copy_path_flat";
end;

define C-function cairo-append-path
  input parameter cr_ :: <CairoContext>;
  input parameter path_ :: <CairoPath>;
  c-name: "cairo_append_path";
end;

define C-function cairo-path-destroy
  input parameter path_ :: <CairoPath>;
  c-name: "cairo_path_destroy";
end;

define C-function cairo-status
  input parameter cr_ :: <CairoContext>;
  result res :: <CairoStatus>;
  c-name: "cairo_status";
end;

define C-function cairo-status-to-string
  input parameter status_ :: <CairoStatus>;
  result res :: <c-string>;
  c-name: "cairo_status_to_string";
end;

define C-function cairo-device-reference
  input parameter device_ :: <CairoDevice>;
  result res :: <CairoDevice>;
  c-name: "cairo_device_reference";
end;

define constant <CairoDeviceType> = <C-int>;
define constant $CAIRO-DEVICE-TYPE-DRM = 0;
define constant $CAIRO-DEVICE-TYPE-GL = 1;
define constant $CAIRO-DEVICE-TYPE-SCRIPT = 2;
define constant $CAIRO-DEVICE-TYPE-XCB = 3;
define constant $CAIRO-DEVICE-TYPE-XLIB = 4;
define constant $CAIRO-DEVICE-TYPE-XML = 5;

define C-function cairo-device-get-type
  input parameter device_ :: <CairoDevice>;
  result res :: <CairoDeviceType>;
  c-name: "cairo_device_get_type";
end;

define C-function cairo-device-status
  input parameter device_ :: <CairoDevice>;
  result res :: <CairoStatus>;
  c-name: "cairo_device_status";
end;

define C-function cairo-device-acquire
  input parameter device_ :: <CairoDevice>;
  result res :: <CairoStatus>;
  c-name: "cairo_device_acquire";
end;

define C-function cairo-device-release
  input parameter device_ :: <CairoDevice>;
  c-name: "cairo_device_release";
end;

define C-function cairo-device-flush
  input parameter device_ :: <CairoDevice>;
  c-name: "cairo_device_flush";
end;

define C-function cairo-device-finish
  input parameter device_ :: <CairoDevice>;
  c-name: "cairo_device_finish";
end;

define C-function cairo-device-destroy
  input parameter device_ :: <CairoDevice>;
  c-name: "cairo_device_destroy";
end;

define C-function cairo-device-get-reference-count
  input parameter device_ :: <CairoDevice>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_device_get_reference_count";
end;

define C-function cairo-device-get-user-data
  input parameter device_ :: <CairoDevice>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_device_get_user_data";
end;

define C-function cairo-device-set-user-data
  input parameter device_ :: <CairoDevice>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_device_set_user_data";
end;

define C-function cairo-surface-create-similar
  input parameter other_ :: <CairoSurface>;
  input parameter content_ :: <CairoContent>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <CairoSurface>;
  c-name: "cairo_surface_create_similar";
end;

define C-function cairo-surface-create-for-rectangle
  input parameter target_ :: <CairoSurface>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_surface_create_for_rectangle";
end;

define C-function cairo-surface-reference
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoSurface>;
  c-name: "cairo_surface_reference";
end;

define C-function cairo-surface-finish
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_finish";
end;

define C-function cairo-surface-destroy
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_destroy";
end;

define C-function cairo-surface-get-device
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoDevice>;
  c-name: "cairo_surface_get_device";
end;

define C-function cairo-surface-get-reference-count
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_surface_get_reference_count";
end;

define C-function cairo-surface-status
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoStatus>;
  c-name: "cairo_surface_status";
end;

define constant <CairoSurfaceType> = <C-int>;
define constant $CAIRO-SURFACE-TYPE-IMAGE = 0;
define constant $CAIRO-SURFACE-TYPE-PDF = 1;
define constant $CAIRO-SURFACE-TYPE-PS = 2;
define constant $CAIRO-SURFACE-TYPE-XLIB = 3;
define constant $CAIRO-SURFACE-TYPE-XCB = 4;
define constant $CAIRO-SURFACE-TYPE-GLITZ = 5;
define constant $CAIRO-SURFACE-TYPE-QUARTZ = 6;
define constant $CAIRO-SURFACE-TYPE-WIN32 = 7;
define constant $CAIRO-SURFACE-TYPE-BEOS = 8;
define constant $CAIRO-SURFACE-TYPE-DIRECTFB = 9;
define constant $CAIRO-SURFACE-TYPE-SVG = 10;
define constant $CAIRO-SURFACE-TYPE-OS2 = 11;
define constant $CAIRO-SURFACE-TYPE-WIN32-PRINTING = 12;
define constant $CAIRO-SURFACE-TYPE-QUARTZ-IMAGE = 13;
define constant $CAIRO-SURFACE-TYPE-SCRIPT = 14;
define constant $CAIRO-SURFACE-TYPE-QT = 15;
define constant $CAIRO-SURFACE-TYPE-RECORDING = 16;
define constant $CAIRO-SURFACE-TYPE-VG = 17;
define constant $CAIRO-SURFACE-TYPE-GL = 18;
define constant $CAIRO-SURFACE-TYPE-DRM = 19;
define constant $CAIRO-SURFACE-TYPE-TEE = 20;
define constant $CAIRO-SURFACE-TYPE-XML = 21;
define constant $CAIRO-SURFACE-TYPE-SKIA = 22;
define constant $CAIRO-SURFACE-TYPE-SUBSURFACE = 23;

define C-function cairo-surface-get-type
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoSurfaceType>;
  c-name: "cairo_surface_get_type";
end;

define C-function cairo-surface-get-content
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoContent>;
  c-name: "cairo_surface_get_content";
end;

define C-function cairo-surface-write-to-png
  input parameter surface_ :: <CairoSurface>;
  input parameter filename_ :: <c-string>;
  result res :: <CairoStatus>;
  c-name: "cairo_surface_write_to_png";
end;

define C-function cairo-surface-write-to-png-stream
  input parameter surface_ :: <CairoSurface>;
  input parameter write-func_ :: <CairoWriteFunc>;
  input parameter closure_ :: <C-void*>;
  result res :: <CairoStatus>;
  c-name: "cairo_surface_write_to_png_stream";
end;

define C-function cairo-surface-get-user-data
  input parameter surface_ :: <CairoSurface>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_surface_get_user_data";
end;

define C-function cairo-surface-set-user-data
  input parameter surface_ :: <CairoSurface>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_surface_set_user_data";
end;

define C-function cairo-surface-get-mime-data
  input parameter surface_ :: <CairoSurface>;
  input parameter mime-type_ :: <c-string>;
  input parameter data_ :: <unsigned-char**>;
  input parameter length_ :: <unsigned-long*>;
  c-name: "cairo_surface_get_mime_data";
end;

define C-function cairo-surface-set-mime-data
  input parameter surface_ :: <CairoSurface>;
  input parameter mime-type_ :: <c-string>;
  input parameter data_ :: <unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  input parameter closure_ :: <C-void*>;
  result res :: <CairoStatus>;
  c-name: "cairo_surface_set_mime_data";
end;

define C-function cairo-surface-get-font-options
  input parameter surface_ :: <CairoSurface>;
  input parameter options_ :: <CairoFontOptions>;
  c-name: "cairo_surface_get_font_options";
end;

define C-function cairo-surface-flush
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_flush";
end;

define C-function cairo-surface-mark-dirty
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_mark_dirty";
end;

define C-function cairo-surface-mark-dirty-rectangle
  input parameter surface_ :: <CairoSurface>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "cairo_surface_mark_dirty_rectangle";
end;

define C-function cairo-surface-set-device-offset
  input parameter surface_ :: <CairoSurface>;
  input parameter x-offset_ :: <C-double>;
  input parameter y-offset_ :: <C-double>;
  c-name: "cairo_surface_set_device_offset";
end;

define C-function cairo-surface-get-device-offset
  input parameter surface_ :: <CairoSurface>;
  input parameter x-offset_ :: <double*>;
  input parameter y-offset_ :: <double*>;
  c-name: "cairo_surface_get_device_offset";
end;

define C-function cairo-surface-set-fallback-resolution
  input parameter surface_ :: <CairoSurface>;
  input parameter x-pixels-per-inch_ :: <C-double>;
  input parameter y-pixels-per-inch_ :: <C-double>;
  c-name: "cairo_surface_set_fallback_resolution";
end;

define C-function cairo-surface-get-fallback-resolution
  input parameter surface_ :: <CairoSurface>;
  input parameter x-pixels-per-inch_ :: <double*>;
  input parameter y-pixels-per-inch_ :: <double*>;
  c-name: "cairo_surface_get_fallback_resolution";
end;

define C-function cairo-surface-copy-page
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_copy_page";
end;

define C-function cairo-surface-show-page
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_surface_show_page";
end;

define C-function cairo-surface-has-show-text-glyphs
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-boolean>;
  c-name: "cairo_surface_has_show_text_glyphs";
end;

define constant <CairoFormat> = <C-int>;
define constant $CAIRO-FORMAT-INVALID = -1;
define constant $CAIRO-FORMAT-ARGB32 = 0;
define constant $CAIRO-FORMAT-RGB24 = 1;
define constant $CAIRO-FORMAT-A8 = 2;
define constant $CAIRO-FORMAT-A1 = 3;
define constant $CAIRO-FORMAT-RGB16-565 = 4;

define C-function cairo-image-surface-create
  input parameter format_ :: <CairoFormat>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <CairoSurface>;
  c-name: "cairo_image_surface_create";
end;

define C-function cairo-format-stride-for-width
  input parameter format_ :: <CairoFormat>;
  input parameter width_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "cairo_format_stride_for_width";
end;

define C-function cairo-image-surface-create-for-data
  input parameter data_ :: <unsigned-char*>;
  input parameter format_ :: <CairoFormat>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter stride_ :: <C-signed-int>;
  result res :: <CairoSurface>;
  c-name: "cairo_image_surface_create_for_data";
end;

define C-function cairo-image-surface-get-data
  input parameter surface_ :: <CairoSurface>;
  result res :: <unsigned-char*>;
  c-name: "cairo_image_surface_get_data";
end;

define C-function cairo-image-surface-get-format
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoFormat>;
  c-name: "cairo_image_surface_get_format";
end;

define C-function cairo-image-surface-get-width
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-signed-int>;
  c-name: "cairo_image_surface_get_width";
end;

define C-function cairo-image-surface-get-height
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-signed-int>;
  c-name: "cairo_image_surface_get_height";
end;

define C-function cairo-image-surface-get-stride
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-signed-int>;
  c-name: "cairo_image_surface_get_stride";
end;

define C-function cairo-image-surface-create-from-png
  input parameter filename_ :: <c-string>;
  result res :: <CairoSurface>;
  c-name: "cairo_image_surface_create_from_png";
end;

define C-function cairo-image-surface-create-from-png-stream
  input parameter read-func_ :: <CairoReadFunc>;
  input parameter closure_ :: <C-void*>;
  result res :: <CairoSurface>;
  c-name: "cairo_image_surface_create_from_png_stream";
end;

define C-function cairo-recording-surface-create
  input parameter content_ :: <CairoContent>;
  input parameter extents_ :: <CairoRectangle>;
  result res :: <CairoSurface>;
  c-name: "cairo_recording_surface_create";
end;

define C-function cairo-recording-surface-ink-extents
  input parameter surface_ :: <CairoSurface>;
  input parameter x0_ :: <double*>;
  input parameter y0_ :: <double*>;
  input parameter width_ :: <double*>;
  input parameter height_ :: <double*>;
  c-name: "cairo_recording_surface_ink_extents";
end;

define C-function cairo-pattern-create-rgb
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_create_rgb";
end;

define C-function cairo-pattern-create-rgba
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  input parameter alpha_ :: <C-double>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_create_rgba";
end;

define C-function cairo-pattern-create-for-surface
  input parameter surface_ :: <CairoSurface>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_create_for_surface";
end;

define C-function cairo-pattern-create-linear
  input parameter x0_ :: <C-double>;
  input parameter y0_ :: <C-double>;
  input parameter x1_ :: <C-double>;
  input parameter y1_ :: <C-double>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_create_linear";
end;

define C-function cairo-pattern-create-radial
  input parameter cx0_ :: <C-double>;
  input parameter cy0_ :: <C-double>;
  input parameter radius0_ :: <C-double>;
  input parameter cx1_ :: <C-double>;
  input parameter cy1_ :: <C-double>;
  input parameter radius1_ :: <C-double>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_create_radial";
end;

define C-function cairo-pattern-reference
  input parameter pattern_ :: <CairoPattern>;
  result res :: <CairoPattern>;
  c-name: "cairo_pattern_reference";
end;

define C-function cairo-pattern-destroy
  input parameter pattern_ :: <CairoPattern>;
  c-name: "cairo_pattern_destroy";
end;

define C-function cairo-pattern-get-reference-count
  input parameter pattern_ :: <CairoPattern>;
  result res :: <C-unsigned-int>;
  c-name: "cairo_pattern_get_reference_count";
end;

define C-function cairo-pattern-status
  input parameter pattern_ :: <CairoPattern>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_status";
end;

define C-function cairo-pattern-get-user-data
  input parameter pattern_ :: <CairoPattern>;
  input parameter key_ :: <CairoUserDataKey>;
  result res :: <C-void*>;
  c-name: "cairo_pattern_get_user_data";
end;

define C-function cairo-pattern-set-user-data
  input parameter pattern_ :: <CairoPattern>;
  input parameter key_ :: <CairoUserDataKey>;
  input parameter user-data_ :: <C-void*>;
  input parameter destroy_ :: <CairoDestroyFunc>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_set_user_data";
end;

define constant <CairoPatternType> = <C-int>;
define constant $CAIRO-PATTERN-TYPE-SOLID = 0;
define constant $CAIRO-PATTERN-TYPE-SURFACE = 1;
define constant $CAIRO-PATTERN-TYPE-LINEAR = 2;
define constant $CAIRO-PATTERN-TYPE-RADIAL = 3;

define C-function cairo-pattern-get-type
  input parameter pattern_ :: <CairoPattern>;
  result res :: <CairoPatternType>;
  c-name: "cairo_pattern_get_type";
end;

define C-function cairo-pattern-add-color-stop-rgb
  input parameter pattern_ :: <CairoPattern>;
  input parameter offset_ :: <C-double>;
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  c-name: "cairo_pattern_add_color_stop_rgb";
end;

define C-function cairo-pattern-add-color-stop-rgba
  input parameter pattern_ :: <CairoPattern>;
  input parameter offset_ :: <C-double>;
  input parameter red_ :: <C-double>;
  input parameter green_ :: <C-double>;
  input parameter blue_ :: <C-double>;
  input parameter alpha_ :: <C-double>;
  c-name: "cairo_pattern_add_color_stop_rgba";
end;

define C-function cairo-pattern-set-matrix
  input parameter pattern_ :: <CairoPattern>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_pattern_set_matrix";
end;

define C-function cairo-pattern-get-matrix
  input parameter pattern_ :: <CairoPattern>;
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_pattern_get_matrix";
end;

define constant <CairoExtend> = <C-int>;
define constant $CAIRO-EXTEND-NONE = 0;
define constant $CAIRO-EXTEND-REPEAT = 1;
define constant $CAIRO-EXTEND-REFLECT = 2;
define constant $CAIRO-EXTEND-PAD = 3;

define C-function cairo-pattern-set-extend
  input parameter pattern_ :: <CairoPattern>;
  input parameter extend_ :: <CairoExtend>;
  c-name: "cairo_pattern_set_extend";
end;

define C-function cairo-pattern-get-extend
  input parameter pattern_ :: <CairoPattern>;
  result res :: <CairoExtend>;
  c-name: "cairo_pattern_get_extend";
end;

define constant <CairoFilter> = <C-int>;
define constant $CAIRO-FILTER-FAST = 0;
define constant $CAIRO-FILTER-GOOD = 1;
define constant $CAIRO-FILTER-BEST = 2;
define constant $CAIRO-FILTER-NEAREST = 3;
define constant $CAIRO-FILTER-BILINEAR = 4;
define constant $CAIRO-FILTER-GAUSSIAN = 5;

define C-function cairo-pattern-set-filter
  input parameter pattern_ :: <CairoPattern>;
  input parameter filter_ :: <CairoFilter>;
  c-name: "cairo_pattern_set_filter";
end;

define C-function cairo-pattern-get-filter
  input parameter pattern_ :: <CairoPattern>;
  result res :: <CairoFilter>;
  c-name: "cairo_pattern_get_filter";
end;

define C-function cairo-pattern-get-rgba
  input parameter pattern_ :: <CairoPattern>;
  input parameter red_ :: <double*>;
  input parameter green_ :: <double*>;
  input parameter blue_ :: <double*>;
  input parameter alpha_ :: <double*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_rgba";
end;

define C-function cairo-pattern-get-surface
  input parameter pattern_ :: <CairoPattern>;
  input parameter surface_ :: <CairoSurface*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_surface";
end;

define C-function cairo-pattern-get-color-stop-rgba
  input parameter pattern_ :: <CairoPattern>;
  input parameter index_ :: <C-signed-int>;
  input parameter offset_ :: <double*>;
  input parameter red_ :: <double*>;
  input parameter green_ :: <double*>;
  input parameter blue_ :: <double*>;
  input parameter alpha_ :: <double*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_color_stop_rgba";
end;

define C-function cairo-pattern-get-color-stop-count
  input parameter pattern_ :: <CairoPattern>;
  input parameter count_ :: <int*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_color_stop_count";
end;

define C-function cairo-pattern-get-linear-points
  input parameter pattern_ :: <CairoPattern>;
  input parameter x0_ :: <double*>;
  input parameter y0_ :: <double*>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_linear_points";
end;

define C-function cairo-pattern-get-radial-circles
  input parameter pattern_ :: <CairoPattern>;
  input parameter x0_ :: <double*>;
  input parameter y0_ :: <double*>;
  input parameter r0_ :: <double*>;
  input parameter x1_ :: <double*>;
  input parameter y1_ :: <double*>;
  input parameter r1_ :: <double*>;
  result res :: <CairoStatus>;
  c-name: "cairo_pattern_get_radial_circles";
end;

define C-function cairo-matrix-init
  input parameter matrix_ :: <CairoMatrix>;
  input parameter xx_ :: <C-double>;
  input parameter yx_ :: <C-double>;
  input parameter xy_ :: <C-double>;
  input parameter yy_ :: <C-double>;
  input parameter x0_ :: <C-double>;
  input parameter y0_ :: <C-double>;
  c-name: "cairo_matrix_init";
end;

define C-function cairo-matrix-init-identity
  input parameter matrix_ :: <CairoMatrix>;
  c-name: "cairo_matrix_init_identity";
end;

define C-function cairo-matrix-init-translate
  input parameter matrix_ :: <CairoMatrix>;
  input parameter tx_ :: <C-double>;
  input parameter ty_ :: <C-double>;
  c-name: "cairo_matrix_init_translate";
end;

define C-function cairo-matrix-init-scale
  input parameter matrix_ :: <CairoMatrix>;
  input parameter sx_ :: <C-double>;
  input parameter sy_ :: <C-double>;
  c-name: "cairo_matrix_init_scale";
end;

define C-function cairo-matrix-init-rotate
  input parameter matrix_ :: <CairoMatrix>;
  input parameter radians_ :: <C-double>;
  c-name: "cairo_matrix_init_rotate";
end;

define C-function cairo-matrix-translate
  input parameter matrix_ :: <CairoMatrix>;
  input parameter tx_ :: <C-double>;
  input parameter ty_ :: <C-double>;
  c-name: "cairo_matrix_translate";
end;

define C-function cairo-matrix-scale
  input parameter matrix_ :: <CairoMatrix>;
  input parameter sx_ :: <C-double>;
  input parameter sy_ :: <C-double>;
  c-name: "cairo_matrix_scale";
end;

define C-function cairo-matrix-rotate
  input parameter matrix_ :: <CairoMatrix>;
  input parameter radians_ :: <C-double>;
  c-name: "cairo_matrix_rotate";
end;

define C-function cairo-matrix-invert
  input parameter matrix_ :: <CairoMatrix>;
  result res :: <CairoStatus>;
  c-name: "cairo_matrix_invert";
end;

define C-function cairo-matrix-multiply
  input parameter result_ :: <CairoMatrix>;
  input parameter a_ :: <CairoMatrix>;
  input parameter b_ :: <CairoMatrix>;
  c-name: "cairo_matrix_multiply";
end;

define C-function cairo-matrix-transform-distance
  input parameter matrix_ :: <CairoMatrix>;
  input parameter dx_ :: <double*>;
  input parameter dy_ :: <double*>;
  c-name: "cairo_matrix_transform_distance";
end;

define C-function cairo-matrix-transform-point
  input parameter matrix_ :: <CairoMatrix>;
  input parameter x_ :: <double*>;
  input parameter y_ :: <double*>;
  c-name: "cairo_matrix_transform_point";
end;

define C-struct <_CairoRegion>
end;
define C-pointer-type <CairoRegion> => <_CairoRegion>;

define C-struct <_CairoRectangleInt>
  slot cairo-rectangle-int-x :: <C-signed-int>;
  slot cairo-rectangle-int-y :: <C-signed-int>;
  slot cairo-rectangle-int-width :: <C-signed-int>;
  slot cairo-rectangle-int-height :: <C-signed-int>;
end;
define C-pointer-type <CairoRectangleInt> => <_CairoRectangleInt>;

define constant <CairoRegionOverlap> = <C-int>;
define constant $CAIRO-REGION-OVERLAP-IN = 0;
define constant $CAIRO-REGION-OVERLAP-OUT = 1;
define constant $CAIRO-REGION-OVERLAP-PART = 2;

define C-function cairo-region-create
  result res :: <CairoRegion>;
  c-name: "cairo_region_create";
end;

define C-function cairo-region-create-rectangle
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoRegion>;
  c-name: "cairo_region_create_rectangle";
end;

define C-function cairo-region-create-rectangles
  input parameter rects_ :: <CairoRectangleInt>;
  input parameter count_ :: <C-signed-int>;
  result res :: <CairoRegion>;
  c-name: "cairo_region_create_rectangles";
end;

define C-function cairo-region-copy
  input parameter original_ :: <CairoRegion>;
  result res :: <CairoRegion>;
  c-name: "cairo_region_copy";
end;

define C-function cairo-region-reference
  input parameter region_ :: <CairoRegion>;
  result res :: <CairoRegion>;
  c-name: "cairo_region_reference";
end;

define C-function cairo-region-destroy
  input parameter region_ :: <CairoRegion>;
  c-name: "cairo_region_destroy";
end;

define C-function cairo-region-equal
  input parameter a_ :: <CairoRegion>;
  input parameter b_ :: <CairoRegion>;
  result res :: <C-boolean>;
  c-name: "cairo_region_equal";
end;

define C-function cairo-region-status
  input parameter region_ :: <CairoRegion>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_status";
end;

define C-function cairo-region-get-extents
  input parameter region_ :: <CairoRegion>;
  input parameter extents_ :: <CairoRectangleInt>;
  c-name: "cairo_region_get_extents";
end;

define C-function cairo-region-num-rectangles
  input parameter region_ :: <CairoRegion>;
  result res :: <C-signed-int>;
  c-name: "cairo_region_num_rectangles";
end;

define C-function cairo-region-get-rectangle
  input parameter region_ :: <CairoRegion>;
  input parameter nth_ :: <C-signed-int>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  c-name: "cairo_region_get_rectangle";
end;

define C-function cairo-region-is-empty
  input parameter region_ :: <CairoRegion>;
  result res :: <C-boolean>;
  c-name: "cairo_region_is_empty";
end;

define C-function cairo-region-contains-rectangle
  input parameter region_ :: <CairoRegion>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoRegionOverlap>;
  c-name: "cairo_region_contains_rectangle";
end;

define C-function cairo-region-contains-point
  input parameter region_ :: <CairoRegion>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "cairo_region_contains_point";
end;

define C-function cairo-region-translate
  input parameter region_ :: <CairoRegion>;
  input parameter dx_ :: <C-signed-int>;
  input parameter dy_ :: <C-signed-int>;
  c-name: "cairo_region_translate";
end;

define C-function cairo-region-subtract
  input parameter dst_ :: <CairoRegion>;
  input parameter other_ :: <CairoRegion>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_subtract";
end;

define C-function cairo-region-subtract-rectangle
  input parameter dst_ :: <CairoRegion>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_subtract_rectangle";
end;

define C-function cairo-region-intersect
  input parameter dst_ :: <CairoRegion>;
  input parameter other_ :: <CairoRegion>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_intersect";
end;

define C-function cairo-region-intersect-rectangle
  input parameter dst_ :: <CairoRegion>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_intersect_rectangle";
end;

define C-function cairo-region-union
  input parameter dst_ :: <CairoRegion>;
  input parameter other_ :: <CairoRegion>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_union";
end;

define C-function cairo-region-union-rectangle
  input parameter dst_ :: <CairoRegion>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_union_rectangle";
end;

define C-function cairo-region-xor
  input parameter dst_ :: <CairoRegion>;
  input parameter other_ :: <CairoRegion>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_xor";
end;

define C-function cairo-region-xor-rectangle
  input parameter dst_ :: <CairoRegion>;
  input parameter rectangle_ :: <CairoRectangleInt>;
  result res :: <CairoStatus>;
  c-name: "cairo_region_xor_rectangle";
end;

define C-function cairo-debug-reset-static-data
  c-name: "cairo_debug_reset_static_data";
end;

define constant $CAIRO-VERSION = 11002;

define constant $CAIRO-MIME-TYPE-JPEG = "image/jpeg";

define constant $CAIRO-MIME-TYPE-PNG = "image/png";

define constant $CAIRO-MIME-TYPE-JP2 = "image/jp2";

define constant $CAIRO-MIME-TYPE-URI = "text/x-uri";

define constant <CairoPDFVersion> = <C-int>;
define constant $CAIRO-PDF-VERSION-1-4 = 0;
define constant $CAIRO-PDF-VERSION-1-5 = 1;
define C-pointer-type <CairoPDFVersion*> => <CairoPDFVersion>;
define C-pointer-type <CairoPDFVersion**> => <CairoPDFVersion*>;

define C-function cairo-pdf-surface-create
  input parameter filename_ :: <c-string>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_pdf_surface_create";
end;

define C-function cairo-pdf-surface-create-for-stream
  input parameter write-func_ :: <CairoWriteFunc>;
  input parameter closure_ :: <C-void*>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_pdf_surface_create_for_stream";
end;

define C-function cairo-pdf-surface-restrict-to-version
  input parameter surface_ :: <CairoSurface>;
  input parameter version_ :: <CairoPDFVersion>;
  c-name: "cairo_pdf_surface_restrict_to_version";
end;

define C-function cairo-pdf-get-versions
  input parameter versions_ :: <CairoPDFVersion**>;
  input parameter num-versions_ :: <int*>;
  c-name: "cairo_pdf_get_versions";
end;

define C-function cairo-pdf-version-to-string
  input parameter version_ :: <CairoPDFVersion>;
  result res :: <c-string>;
  c-name: "cairo_pdf_version_to_string";
end;

define C-function cairo-pdf-surface-set-size
  input parameter surface_ :: <CairoSurface>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  c-name: "cairo_pdf_surface_set_size";
end;

define constant <CairoPSLevel> = <C-int>;
define constant $CAIRO-PS-LEVEL-2 = 0;
define constant $CAIRO-PS-LEVEL-3 = 1;
define C-pointer-type <CairoPSLevel*> => <CairoPSLevel>;
define C-pointer-type <CairoPSLevel**> => <CairoPSLevel*>;

define C-function cairo-ps-surface-create
  input parameter filename_ :: <c-string>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_ps_surface_create";
end;

define C-function cairo-ps-surface-create-for-stream
  input parameter write-func_ :: <CairoWriteFunc>;
  input parameter closure_ :: <C-void*>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_ps_surface_create_for_stream";
end;

define C-function cairo-ps-surface-restrict-to-level
  input parameter surface_ :: <CairoSurface>;
  input parameter level_ :: <CairoPSLevel>;
  c-name: "cairo_ps_surface_restrict_to_level";
end;

define C-function cairo-ps-get-levels
  input parameter levels_ :: <CairoPSLevel**>;
  input parameter num-levels_ :: <int*>;
  c-name: "cairo_ps_get_levels";
end;

define C-function cairo-ps-level-to-string
  input parameter level_ :: <CairoPSLevel>;
  result res :: <c-string>;
  c-name: "cairo_ps_level_to_string";
end;

define C-function cairo-ps-surface-set-eps
  input parameter surface_ :: <CairoSurface>;
  input parameter eps_ :: <C-boolean>;
  c-name: "cairo_ps_surface_set_eps";
end;

define C-function cairo-ps-surface-get-eps
  input parameter surface_ :: <CairoSurface>;
  result res :: <C-boolean>;
  c-name: "cairo_ps_surface_get_eps";
end;

define C-function cairo-ps-surface-set-size
  input parameter surface_ :: <CairoSurface>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  c-name: "cairo_ps_surface_set_size";
end;

define C-function cairo-ps-surface-dsc-comment
  input parameter surface_ :: <CairoSurface>;
  input parameter comment_ :: <c-string>;
  c-name: "cairo_ps_surface_dsc_comment";
end;

define C-function cairo-ps-surface-dsc-begin-setup
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_ps_surface_dsc_begin_setup";
end;

define C-function cairo-ps-surface-dsc-begin-page-setup
  input parameter surface_ :: <CairoSurface>;
  c-name: "cairo_ps_surface_dsc_begin_page_setup";
end;

define constant <CairoSVGVersion> = <C-int>;
define constant $CAIRO-SVG-VERSION-1-1 = 0;
define constant $CAIRO-SVG-VERSION-1-2 = 1;
define C-pointer-type <CairoSVGVersion*> => <CairoSVGVersion>;
define C-pointer-type <CairoSVGVersion**> => <CairoSVGVersion*>;

define C-function cairo-svg-surface-create
  input parameter filename_ :: <c-string>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_svg_surface_create";
end;

define C-function cairo-svg-surface-create-for-stream
  input parameter write-func_ :: <CairoWriteFunc>;
  input parameter closure_ :: <C-void*>;
  input parameter width-in-points_ :: <C-double>;
  input parameter height-in-points_ :: <C-double>;
  result res :: <CairoSurface>;
  c-name: "cairo_svg_surface_create_for_stream";
end;

define C-function cairo-svg-surface-restrict-to-version
  input parameter surface_ :: <CairoSurface>;
  input parameter version_ :: <CairoSVGVersion>;
  c-name: "cairo_svg_surface_restrict_to_version";
end;

define C-function cairo-svg-get-versions
  input parameter versions_ :: <CairoSVGVersion**>;
  input parameter num-versions_ :: <int*>;
  c-name: "cairo_svg_get_versions";
end;

define C-function cairo-svg-version-to-string
  input parameter version_ :: <CairoSVGVersion>;
  result res :: <c-string>;
  c-name: "cairo_svg_version_to_string";
end;

define constant $CAIRO-VERSION-MAJOR = 1;

define constant $CAIRO-VERSION-MINOR = 10;

define constant $CAIRO-VERSION-MICRO = 2;

