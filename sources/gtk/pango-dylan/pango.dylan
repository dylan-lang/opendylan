module: pango
synopsis: generated bindings for the Pango library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define constant $ANALYSIS-FLAG-CENTERED-BASELINE = 1;

define constant $ATTR-INDEX-FROM-TEXT-BEGINNING = 0;

define constant $PANGO-ALIGN-LEFT = 0;
define constant $PANGO-ALIGN-CENTER = 1;
define constant $PANGO-ALIGN-RIGHT = 2;
define constant <PangoAlignment> = <C-int>;
define C-pointer-type <PangoAlignment*> => <PangoAlignment>;

define C-struct <_PangoAnalysis>
  slot pangoanalysis-shape-engine :: <PangoEngineShape>;
  slot pangoanalysis-lang-engine :: <PangoEngineLang>;
  slot pangoanalysis-font :: <PangoFont>;
  slot pangoanalysis-level :: <C-unsigned-char>;
  slot pangoanalysis-gravity :: <C-unsigned-char>;
  slot pangoanalysis-flags :: <C-unsigned-char>;
  slot pangoanalysis-script :: <C-unsigned-char>;
  slot pangoanalysis-language :: <PangoLanguage>;
  slot pangoanalysis-extra-attrs :: <GSList>;
  pointer-type-name: <PangoAnalysis>;
end C-struct;

define C-struct <_PangoAttrClass>
  slot pangoattrclass-type :: <PangoAttrType>;
  constant slot pangoattrclass-copy :: <C-void*>;
  constant slot pangoattrclass-destroy :: <C-function-pointer>;
  constant slot pangoattrclass-equal :: <C-function-pointer>;
  pointer-type-name: <PangoAttrClass>;
end C-struct;

define C-struct <_PangoAttrColor>
  slot pangoattrcolor-attr :: <PangoAttribute>;
  slot pangoattrcolor-color :: <PangoColor>;
  pointer-type-name: <PangoAttrColor>;
end C-struct;

define C-struct <_PangoAttrFloat>
  slot pangoattrfloat-attr :: <PangoAttribute>;
  slot pangoattrfloat-value :: <C-double>;
  pointer-type-name: <PangoAttrFloat>;
end C-struct;

define C-struct <_PangoAttrFontDesc>
  slot pangoattrfontdesc-attr :: <PangoAttribute>;
  slot pangoattrfontdesc-desc :: <PangoFontDescription>;
  pointer-type-name: <PangoAttrFontDesc>;
end C-struct;

define C-struct <_PangoAttrInt>
  slot pangoattrint-attr :: <PangoAttribute>;
  slot pangoattrint-value :: <C-signed-int>;
  pointer-type-name: <PangoAttrInt>;
end C-struct;

define C-struct <_PangoAttrIterator>
  pointer-type-name: <PangoAttrIterator>;
end C-struct;

define C-function pango-attr-iterator-destroy
  input parameter self :: <PangoAttrIterator>;
  c-name: "pango_attr_iterator_destroy";
end;

define C-function pango-attr-iterator-get-attrs
  input parameter self :: <PangoAttrIterator>;
  result res :: <GSList>;
  c-name: "pango_attr_iterator_get_attrs";
end;

define C-function pango-attr-iterator-get-font
  input parameter self :: <PangoAttrIterator>;
  input parameter desc_ :: <PangoFontDescription>;
  input parameter language_ :: <PangoLanguage>;
  input parameter extra_attrs_ :: <GSList>;
  c-name: "pango_attr_iterator_get_font";
end;

define C-function pango-attr-iterator-next
  input parameter self :: <PangoAttrIterator>;
  result res :: <C-boolean>;
  c-name: "pango_attr_iterator_next";
end;

define C-function pango-attr-iterator-range
  input parameter self :: <PangoAttrIterator>;
  output parameter start_ :: <C-signed-int*>;
  output parameter end_ :: <C-signed-int*>;
  c-name: "pango_attr_iterator_range";
end;

define C-struct <_PangoAttrLanguage>
  slot pangoattrlanguage-attr :: <PangoAttribute>;
  slot pangoattrlanguage-value :: <PangoLanguage>;
  pointer-type-name: <PangoAttrLanguage>;
end C-struct;

define C-struct <_PangoAttrList>
  pointer-type-name: <PangoAttrList>;
end C-struct;

define C-function pango-attr-list-new
  result res :: <PangoAttrList>;
  c-name: "pango_attr_list_new";
end;

define C-function pango-attr-list-change
  input parameter self :: <PangoAttrList>;
  input parameter attr_ :: <PangoAttribute>;
  c-name: "pango_attr_list_change";
end;

define C-function pango-attr-list-copy
  input parameter self :: <PangoAttrList>;
  result res :: <PangoAttrList>;
  c-name: "pango_attr_list_copy";
end;

define C-function pango-attr-list-filter
  input parameter self :: <PangoAttrList>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  result res :: <PangoAttrList>;
  c-name: "pango_attr_list_filter";
end;

define C-function pango-attr-list-insert
  input parameter self :: <PangoAttrList>;
  input parameter attr_ :: <PangoAttribute>;
  c-name: "pango_attr_list_insert";
end;

define C-function pango-attr-list-insert-before
  input parameter self :: <PangoAttrList>;
  input parameter attr_ :: <PangoAttribute>;
  c-name: "pango_attr_list_insert_before";
end;

define C-function pango-attr-list-ref
  input parameter self :: <PangoAttrList>;
  result res :: <PangoAttrList>;
  c-name: "pango_attr_list_ref";
end;

define C-function pango-attr-list-splice
  input parameter self :: <PangoAttrList>;
  input parameter other_ :: <PangoAttrList>;
  input parameter pos_ :: <C-signed-int>;
  input parameter len_ :: <C-signed-int>;
  c-name: "pango_attr_list_splice";
end;

define C-function pango-attr-list-unref
  input parameter self :: <PangoAttrList>;
  c-name: "pango_attr_list_unref";
end;

define C-struct <_PangoAttrShape>
  slot pangoattrshape-attr :: <PangoAttribute>;
  slot pangoattrshape-ink-rect :: <PangoRectangle>;
  slot pangoattrshape-logical-rect :: <PangoRectangle>;
  slot pangoattrshape-data :: <C-void*>;
  slot pangoattrshape-copy-func :: <C-void*>;
  slot pangoattrshape-destroy-func :: <C-function-pointer>;
  pointer-type-name: <PangoAttrShape>;
end C-struct;

define C-struct <_PangoAttrSize>
  slot pangoattrsize-attr :: <PangoAttribute>;
  slot pangoattrsize-size :: <C-signed-int>;
  slot pangoattrsize-absolute :: <C-unsigned-int>;
  pointer-type-name: <PangoAttrSize>;
end C-struct;

define C-struct <_PangoAttrString>
  slot pangoattrstring-attr :: <PangoAttribute>;
  slot pangoattrstring-value :: <C-string>;
  pointer-type-name: <PangoAttrString>;
end C-struct;

define constant $PANGO-ATTR-INVALID = 0;
define constant $PANGO-ATTR-LANGUAGE = 1;
define constant $PANGO-ATTR-FAMILY = 2;
define constant $PANGO-ATTR-STYLE = 3;
define constant $PANGO-ATTR-WEIGHT = 4;
define constant $PANGO-ATTR-VARIANT = 5;
define constant $PANGO-ATTR-STRETCH = 6;
define constant $PANGO-ATTR-SIZE = 7;
define constant $PANGO-ATTR-FONT-DESC = 8;
define constant $PANGO-ATTR-FOREGROUND = 9;
define constant $PANGO-ATTR-BACKGROUND = 10;
define constant $PANGO-ATTR-UNDERLINE = 11;
define constant $PANGO-ATTR-STRIKETHROUGH = 12;
define constant $PANGO-ATTR-RISE = 13;
define constant $PANGO-ATTR-SHAPE = 14;
define constant $PANGO-ATTR-SCALE = 15;
define constant $PANGO-ATTR-FALLBACK = 16;
define constant $PANGO-ATTR-LETTER-SPACING = 17;
define constant $PANGO-ATTR-UNDERLINE-COLOR = 18;
define constant $PANGO-ATTR-STRIKETHROUGH-COLOR = 19;
define constant $PANGO-ATTR-ABSOLUTE-SIZE = 20;
define constant $PANGO-ATTR-GRAVITY = 21;
define constant $PANGO-ATTR-GRAVITY-HINT = 22;
define constant <PangoAttrType> = <C-int>;
define C-pointer-type <PangoAttrType*> => <PangoAttrType>;

define C-struct <_PangoAttribute>
  slot pangoattribute-klass :: <PangoAttrClass>;
  slot pangoattribute-start-index :: <C-unsigned-int>;
  slot pangoattribute-end-index :: <C-unsigned-int>;
  pointer-type-name: <PangoAttribute>;
end C-struct;

define C-function pango-attribute-destroy
  input parameter self :: <PangoAttribute>;
  c-name: "pango_attribute_destroy";
end;

define C-function pango-attribute-equal
  input parameter self :: <PangoAttribute>;
  input parameter attr2_ :: <PangoAttribute>;
  result res :: <C-boolean>;
  c-name: "pango_attribute_equal";
end;

define C-function pango-attribute-init
  input parameter self :: <PangoAttribute>;
  input parameter klass_ :: <PangoAttrClass>;
  c-name: "pango_attribute_init";
end;

define constant $PANGO-BIDI-TYPE-L = 0;
define constant $PANGO-BIDI-TYPE-LRE = 1;
define constant $PANGO-BIDI-TYPE-LRO = 2;
define constant $PANGO-BIDI-TYPE-R = 3;
define constant $PANGO-BIDI-TYPE-AL = 4;
define constant $PANGO-BIDI-TYPE-RLE = 5;
define constant $PANGO-BIDI-TYPE-RLO = 6;
define constant $PANGO-BIDI-TYPE-PDF = 7;
define constant $PANGO-BIDI-TYPE-EN = 8;
define constant $PANGO-BIDI-TYPE-ES = 9;
define constant $PANGO-BIDI-TYPE-ET = 10;
define constant $PANGO-BIDI-TYPE-AN = 11;
define constant $PANGO-BIDI-TYPE-CS = 12;
define constant $PANGO-BIDI-TYPE-NSM = 13;
define constant $PANGO-BIDI-TYPE-BN = 14;
define constant $PANGO-BIDI-TYPE-B = 15;
define constant $PANGO-BIDI-TYPE-S = 16;
define constant $PANGO-BIDI-TYPE-WS = 17;
define constant $PANGO-BIDI-TYPE-ON = 18;
define constant <PangoBidiType> = <C-int>;
define C-pointer-type <PangoBidiType*> => <PangoBidiType>;

define C-struct <_PangoColor>
  slot pangocolor-red :: <C-unsigned-short>;
  slot pangocolor-green :: <C-unsigned-short>;
  slot pangocolor-blue :: <C-unsigned-short>;
  pointer-type-name: <PangoColor>;
end C-struct;

define C-function pango-color-copy
  input parameter self :: <PangoColor>;
  result res :: <PangoColor>;
  c-name: "pango_color_copy";
end;

define C-function pango-color-free
  input parameter self :: <PangoColor>;
  c-name: "pango_color_free";
end;

define C-function pango-color-parse
  input parameter self :: <PangoColor>;
  input parameter spec_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_color_parse";
end;

define C-function pango-color-to-string
  input parameter self :: <PangoColor>;
  result res :: <C-string>;
  c-name: "pango_color_to_string";
end;

define open C-subtype <PangoContext> (<GObject>)
end C-subtype;

define C-pointer-type <PangoContext*> => <PangoContext>;

define C-function pango-context-new
  result res :: <PangoContext>;
  c-name: "pango_context_new";
end;

define C-function pango-context-changed
  input parameter self :: <PangoContext>;
  c-name: "pango_context_changed";
end;

define C-function pango-context-get-base-dir
  input parameter self :: <PangoContext>;
  result res :: <PangoDirection>;
  c-name: "pango_context_get_base_dir";
end;

define C-function pango-context-get-base-gravity
  input parameter self :: <PangoContext>;
  result res :: <PangoGravity>;
  c-name: "pango_context_get_base_gravity";
end;

define C-function pango-context-get-font-description
  input parameter self :: <PangoContext>;
  result res :: <PangoFontDescription>;
  c-name: "pango_context_get_font_description";
end;

define C-function pango-context-get-font-map
  input parameter self :: <PangoContext>;
  result res :: <PangoFontMap>;
  c-name: "pango_context_get_font_map";
end;

define C-function pango-context-get-gravity
  input parameter self :: <PangoContext>;
  result res :: <PangoGravity>;
  c-name: "pango_context_get_gravity";
end;

define C-function pango-context-get-gravity-hint
  input parameter self :: <PangoContext>;
  result res :: <PangoGravityHint>;
  c-name: "pango_context_get_gravity_hint";
end;

define C-function pango-context-get-language
  input parameter self :: <PangoContext>;
  result res :: <PangoLanguage>;
  c-name: "pango_context_get_language";
end;

define C-function pango-context-get-matrix
  input parameter self :: <PangoContext>;
  result res :: <PangoMatrix>;
  c-name: "pango_context_get_matrix";
end;

define C-function pango-context-get-metrics
  input parameter self :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  input parameter language_ :: <PangoLanguage>;
  result res :: <PangoFontMetrics>;
  c-name: "pango_context_get_metrics";
end;

define C-function pango-context-get-serial
  input parameter self :: <PangoContext>;
  result res :: <C-unsigned-int>;
  c-name: "pango_context_get_serial";
end;

define C-function pango-context-list-families
  input parameter self :: <PangoContext>;
  output parameter families_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_families_ :: <C-signed-int*>;
  c-name: "pango_context_list_families";
end;

define C-function pango-context-load-font
  input parameter self :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  result res :: <PangoFont>;
  c-name: "pango_context_load_font";
end;

define C-function pango-context-load-fontset
  input parameter self :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  input parameter language_ :: <PangoLanguage>;
  result res :: <PangoFontset>;
  c-name: "pango_context_load_fontset";
end;

define C-function pango-context-set-base-dir
  input parameter self :: <PangoContext>;
  input parameter direction_ :: <PangoDirection>;
  c-name: "pango_context_set_base_dir";
end;

define C-function pango-context-set-base-gravity
  input parameter self :: <PangoContext>;
  input parameter gravity_ :: <PangoGravity>;
  c-name: "pango_context_set_base_gravity";
end;

define C-function pango-context-set-font-description
  input parameter self :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  c-name: "pango_context_set_font_description";
end;

define C-function pango-context-set-font-map
  input parameter self :: <PangoContext>;
  input parameter font_map_ :: <PangoFontMap>;
  c-name: "pango_context_set_font_map";
end;

define C-function pango-context-set-gravity-hint
  input parameter self :: <PangoContext>;
  input parameter hint_ :: <PangoGravityHint>;
  c-name: "pango_context_set_gravity_hint";
end;

define C-function pango-context-set-language
  input parameter self :: <PangoContext>;
  input parameter language_ :: <PangoLanguage>;
  c-name: "pango_context_set_language";
end;

define C-function pango-context-set-matrix
  input parameter self :: <PangoContext>;
  input parameter matrix_ :: <PangoMatrix>;
  c-name: "pango_context_set_matrix";
end;

define C-struct <_PangoContextClass>
  pointer-type-name: <PangoContextClass>;
end C-struct;

define C-struct <_PangoCoverage>
  pointer-type-name: <PangoCoverage>;
end C-struct;

define C-function pango-coverage-get
  input parameter self :: <PangoCoverage>;
  input parameter index__ :: <C-signed-int>;
  result res :: <PangoCoverageLevel>;
  c-name: "pango_coverage_get";
end;

define C-function pango-coverage-max
  input parameter self :: <PangoCoverage>;
  input parameter other_ :: <PangoCoverage>;
  c-name: "pango_coverage_max";
end;

define C-function pango-coverage-set
  input parameter self :: <PangoCoverage>;
  input parameter index__ :: <C-signed-int>;
  input parameter level_ :: <PangoCoverageLevel>;
  c-name: "pango_coverage_set";
end;

define C-function pango-coverage-to-bytes
  input parameter self :: <PangoCoverage>;
  output parameter bytes_ :: <C-unsigned-char*>;
  output parameter n_bytes_ :: <C-signed-int*>;
  c-name: "pango_coverage_to_bytes";
end;

define C-function pango-coverage-unref
  input parameter self :: <PangoCoverage>;
  c-name: "pango_coverage_unref";
end;

define constant $PANGO-COVERAGE-NONE = 0;
define constant $PANGO-COVERAGE-FALLBACK = 1;
define constant $PANGO-COVERAGE-APPROXIMATE = 2;
define constant $PANGO-COVERAGE-EXACT = 3;
define constant <PangoCoverageLevel> = <C-int>;
define C-pointer-type <PangoCoverageLevel*> => <PangoCoverageLevel>;

define constant $PANGO-DIRECTION-LTR = 0;
define constant $PANGO-DIRECTION-RTL = 1;
define constant $PANGO-DIRECTION-TTB-LTR = 2;
define constant $PANGO-DIRECTION-TTB-RTL = 3;
define constant $PANGO-DIRECTION-WEAK-LTR = 4;
define constant $PANGO-DIRECTION-WEAK-RTL = 5;
define constant $PANGO-DIRECTION-NEUTRAL = 6;
define constant <PangoDirection> = <C-int>;
define C-pointer-type <PangoDirection*> => <PangoDirection>;

define constant $ENGINE-TYPE-LANG = "PangoEngineLang";

define constant $ENGINE-TYPE-SHAPE = "PangoEngineShape";

define constant $PANGO-ELLIPSIZE-NONE = 0;
define constant $PANGO-ELLIPSIZE-START = 1;
define constant $PANGO-ELLIPSIZE-MIDDLE = 2;
define constant $PANGO-ELLIPSIZE-END = 3;
define constant <PangoEllipsizeMode> = <C-int>;
define C-pointer-type <PangoEllipsizeMode*> => <PangoEllipsizeMode>;

define C-struct <_PangoEngineLang>
  pointer-type-name: <PangoEngineLang>;
end C-struct;

define C-struct <_PangoEngineShape>
  pointer-type-name: <PangoEngineShape>;
end C-struct;

define open C-subtype <PangoFont> (<GObject>)
end C-subtype;

define C-pointer-type <PangoFont*> => <PangoFont>;

define C-function pango-font-descriptions-free
  input parameter descs_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_descs_ :: <C-signed-int>;
  c-name: "pango_font_descriptions_free";
end;

define C-function pango-font-describe
  input parameter self :: <PangoFont>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_describe";
end;

define C-function pango-font-describe-with-absolute-size
  input parameter self :: <PangoFont>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_describe_with_absolute_size";
end;

define C-function pango-font-get-font-map
  input parameter self :: <PangoFont>;
  result res :: <PangoFontMap>;
  c-name: "pango_font_get_font_map";
end;

define C-function pango-font-get-glyph-extents
  input parameter self :: <PangoFont>;
  input parameter glyph_ :: <C-unsigned-int>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_font_get_glyph_extents";
end;

define C-function pango-font-get-metrics
  input parameter self :: <PangoFont>;
  input parameter language_ :: <PangoLanguage>;
  result res :: <PangoFontMetrics>;
  c-name: "pango_font_get_metrics";
end;

define C-struct <_PangoFontDescription>
  pointer-type-name: <PangoFontDescription>;
end C-struct;

define C-function pango-font-description-new
  result res :: <PangoFontDescription>;
  c-name: "pango_font_description_new";
end;

define C-function pango-font-description-better-match
  input parameter self :: <PangoFontDescription>;
  input parameter old_match_ :: <PangoFontDescription>;
  input parameter new_match_ :: <PangoFontDescription>;
  result res :: <C-boolean>;
  c-name: "pango_font_description_better_match";
end;

define C-function pango-font-description-copy
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_description_copy";
end;

define C-function pango-font-description-copy-static
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_description_copy_static";
end;

define C-function pango-font-description-equal
  input parameter self :: <PangoFontDescription>;
  input parameter desc2_ :: <PangoFontDescription>;
  result res :: <C-boolean>;
  c-name: "pango_font_description_equal";
end;

define C-function pango-font-description-free
  input parameter self :: <PangoFontDescription>;
  c-name: "pango_font_description_free";
end;

define C-function pango-font-description-get-family
  input parameter self :: <PangoFontDescription>;
  result res :: <C-string>;
  c-name: "pango_font_description_get_family";
end;

define C-function pango-font-description-get-gravity
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoGravity>;
  c-name: "pango_font_description_get_gravity";
end;

define C-function pango-font-description-get-set-fields
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoFontMask>;
  c-name: "pango_font_description_get_set_fields";
end;

define C-function pango-font-description-get-size
  input parameter self :: <PangoFontDescription>;
  result res :: <C-signed-int>;
  c-name: "pango_font_description_get_size";
end;

define C-function pango-font-description-get-size-is-absolute
  input parameter self :: <PangoFontDescription>;
  result res :: <C-boolean>;
  c-name: "pango_font_description_get_size_is_absolute";
end;

define C-function pango-font-description-get-stretch
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoStretch>;
  c-name: "pango_font_description_get_stretch";
end;

define C-function pango-font-description-get-style
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoStyle>;
  c-name: "pango_font_description_get_style";
end;

define C-function pango-font-description-get-variant
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoVariant>;
  c-name: "pango_font_description_get_variant";
end;

define C-function pango-font-description-get-weight
  input parameter self :: <PangoFontDescription>;
  result res :: <PangoWeight>;
  c-name: "pango_font_description_get_weight";
end;

define C-function pango-font-description-hash
  input parameter self :: <PangoFontDescription>;
  result res :: <C-unsigned-int>;
  c-name: "pango_font_description_hash";
end;

define C-function pango-font-description-merge
  input parameter self :: <PangoFontDescription>;
  input parameter desc_to_merge_ :: <PangoFontDescription>;
  input parameter replace_existing_ :: <C-boolean>;
  c-name: "pango_font_description_merge";
end;

define C-function pango-font-description-merge-static
  input parameter self :: <PangoFontDescription>;
  input parameter desc_to_merge_ :: <PangoFontDescription>;
  input parameter replace_existing_ :: <C-boolean>;
  c-name: "pango_font_description_merge_static";
end;

define C-function pango-font-description-set-absolute-size
  input parameter self :: <PangoFontDescription>;
  input parameter size_ :: <C-double>;
  c-name: "pango_font_description_set_absolute_size";
end;

define C-function pango-font-description-set-family
  input parameter self :: <PangoFontDescription>;
  input parameter family_ :: <C-string>;
  c-name: "pango_font_description_set_family";
end;

define C-function pango-font-description-set-family-static
  input parameter self :: <PangoFontDescription>;
  input parameter family_ :: <C-string>;
  c-name: "pango_font_description_set_family_static";
end;

define C-function pango-font-description-set-gravity
  input parameter self :: <PangoFontDescription>;
  input parameter gravity_ :: <PangoGravity>;
  c-name: "pango_font_description_set_gravity";
end;

define C-function pango-font-description-set-size
  input parameter self :: <PangoFontDescription>;
  input parameter size_ :: <C-signed-int>;
  c-name: "pango_font_description_set_size";
end;

define C-function pango-font-description-set-stretch
  input parameter self :: <PangoFontDescription>;
  input parameter stretch_ :: <PangoStretch>;
  c-name: "pango_font_description_set_stretch";
end;

define C-function pango-font-description-set-style
  input parameter self :: <PangoFontDescription>;
  input parameter style_ :: <PangoStyle>;
  c-name: "pango_font_description_set_style";
end;

define C-function pango-font-description-set-variant
  input parameter self :: <PangoFontDescription>;
  input parameter variant_ :: <PangoVariant>;
  c-name: "pango_font_description_set_variant";
end;

define C-function pango-font-description-set-weight
  input parameter self :: <PangoFontDescription>;
  input parameter weight_ :: <PangoWeight>;
  c-name: "pango_font_description_set_weight";
end;

define C-function pango-font-description-to-filename
  input parameter self :: <PangoFontDescription>;
  result res :: <C-string>;
  c-name: "pango_font_description_to_filename";
end;

define C-function pango-font-description-to-string
  input parameter self :: <PangoFontDescription>;
  result res :: <C-string>;
  c-name: "pango_font_description_to_string";
end;

define C-function pango-font-description-unset-fields
  input parameter self :: <PangoFontDescription>;
  input parameter to_unset_ :: <PangoFontMask>;
  c-name: "pango_font_description_unset_fields";
end;

define C-function pango-font-description-from-string
  input parameter str_ :: <C-string>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_description_from_string";
end;

define open C-subtype <PangoFontFace> (<GObject>)
end C-subtype;

define C-pointer-type <PangoFontFace*> => <PangoFontFace>;

define C-function pango-font-face-describe
  input parameter self :: <PangoFontFace>;
  result res :: <PangoFontDescription>;
  c-name: "pango_font_face_describe";
end;

define C-function pango-font-face-get-face-name
  input parameter self :: <PangoFontFace>;
  result res :: <C-string>;
  c-name: "pango_font_face_get_face_name";
end;

define C-function pango-font-face-is-synthesized
  input parameter self :: <PangoFontFace>;
  result res :: <C-boolean>;
  c-name: "pango_font_face_is_synthesized";
end;

define C-function pango-font-face-list-sizes
  input parameter self :: <PangoFontFace>;
  output parameter sizes_ :: <C-signed-int*>;
  output parameter n_sizes_ :: <C-signed-int*>;
  c-name: "pango_font_face_list_sizes";
end;

define open C-subtype <PangoFontFamily> (<GObject>)
end C-subtype;

define C-pointer-type <PangoFontFamily*> => <PangoFontFamily>;

define C-function pango-font-family-get-name
  input parameter self :: <PangoFontFamily>;
  result res :: <C-string>;
  c-name: "pango_font_family_get_name";
end;

define C-function pango-font-family-is-monospace
  input parameter self :: <PangoFontFamily>;
  result res :: <C-boolean>;
  c-name: "pango_font_family_is_monospace";
end;

define C-function pango-font-family-list-faces
  input parameter self :: <PangoFontFamily>;
  output parameter faces_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_faces_ :: <C-signed-int*>;
  c-name: "pango_font_family_list_faces";
end;

define open C-subtype <PangoFontMap> (<GObject>)
end C-subtype;

define C-pointer-type <PangoFontMap*> => <PangoFontMap>;

define C-function pango-font-map-create-context
  input parameter self :: <PangoFontMap>;
  result res :: <PangoContext>;
  c-name: "pango_font_map_create_context";
end;

define C-function pango-font-map-get-serial
  input parameter self :: <PangoFontMap>;
  result res :: <C-unsigned-int>;
  c-name: "pango_font_map_get_serial";
end;

define C-function pango-font-map-list-families
  input parameter self :: <PangoFontMap>;
  output parameter families_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_families_ :: <C-signed-int*>;
  c-name: "pango_font_map_list_families";
end;

define C-function pango-font-map-load-font
  input parameter self :: <PangoFontMap>;
  input parameter context_ :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  result res :: <PangoFont>;
  c-name: "pango_font_map_load_font";
end;

define C-function pango-font-map-load-fontset
  input parameter self :: <PangoFontMap>;
  input parameter context_ :: <PangoContext>;
  input parameter desc_ :: <PangoFontDescription>;
  input parameter language_ :: <PangoLanguage>;
  result res :: <PangoFontset>;
  c-name: "pango_font_map_load_fontset";
end;

define constant $PANGO-FONT-MASK-FAMILY = 1;
define constant $PANGO-FONT-MASK-STYLE = 2;
define constant $PANGO-FONT-MASK-VARIANT = 4;
define constant $PANGO-FONT-MASK-WEIGHT = 8;
define constant $PANGO-FONT-MASK-STRETCH = 16;
define constant $PANGO-FONT-MASK-SIZE = 32;
define constant $PANGO-FONT-MASK-GRAVITY = 64;
define constant <PangoFontMask> = <C-int>;
define C-pointer-type <PangoFontMask*> => <PangoFontMask>;

define C-struct <_PangoFontMetrics>
  pointer-type-name: <PangoFontMetrics>;
end C-struct;

define C-function pango-font-metrics-get-approximate-char-width
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_approximate_char_width";
end;

define C-function pango-font-metrics-get-approximate-digit-width
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_approximate_digit_width";
end;

define C-function pango-font-metrics-get-ascent
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_ascent";
end;

define C-function pango-font-metrics-get-descent
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_descent";
end;

define C-function pango-font-metrics-get-strikethrough-position
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_strikethrough_position";
end;

define C-function pango-font-metrics-get-strikethrough-thickness
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_strikethrough_thickness";
end;

define C-function pango-font-metrics-get-underline-position
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_underline_position";
end;

define C-function pango-font-metrics-get-underline-thickness
  input parameter self :: <PangoFontMetrics>;
  result res :: <C-signed-int>;
  c-name: "pango_font_metrics_get_underline_thickness";
end;

define C-function pango-font-metrics-ref
  input parameter self :: <PangoFontMetrics>;
  result res :: <PangoFontMetrics>;
  c-name: "pango_font_metrics_ref";
end;

define C-function pango-font-metrics-unref
  input parameter self :: <PangoFontMetrics>;
  c-name: "pango_font_metrics_unref";
end;

define open C-subtype <PangoFontset> (<GObject>)
end C-subtype;

define C-pointer-type <PangoFontset*> => <PangoFontset>;

define C-function pango-fontset-foreach
  input parameter self :: <PangoFontset>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "pango_fontset_foreach";
end;

define C-function pango-fontset-get-font
  input parameter self :: <PangoFontset>;
  input parameter wc_ :: <C-unsigned-int>;
  result res :: <PangoFont>;
  c-name: "pango_fontset_get_font";
end;

define C-function pango-fontset-get-metrics
  input parameter self :: <PangoFontset>;
  result res :: <PangoFontMetrics>;
  c-name: "pango_fontset_get_metrics";
end;

define C-struct <_PangoGlyphGeometry>
  slot pangoglyphgeometry-width :: <C-signed-int>;
  slot pangoglyphgeometry-x-offset :: <C-signed-int>;
  slot pangoglyphgeometry-y-offset :: <C-signed-int>;
  pointer-type-name: <PangoGlyphGeometry>;
end C-struct;

define C-struct <_PangoGlyphInfo>
  slot pangoglyphinfo-glyph :: <C-unsigned-int>;
  slot pangoglyphinfo-geometry :: <PangoGlyphGeometry>;
  slot pangoglyphinfo-attr :: <PangoGlyphVisAttr>;
  pointer-type-name: <PangoGlyphInfo>;
end C-struct;

define C-struct <_PangoGlyphItem>
  slot pangoglyphitem-item :: <PangoItem>;
  slot pangoglyphitem-glyphs :: <PangoGlyphString>;
  pointer-type-name: <PangoGlyphItem>;
end C-struct;

define C-function pango-glyph-item-apply-attrs
  input parameter self :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  input parameter list_ :: <PangoAttrList>;
  result res :: <GSList>;
  c-name: "pango_glyph_item_apply_attrs";
end;

define C-function pango-glyph-item-copy
  input parameter self :: <PangoGlyphItem>;
  result res :: <PangoGlyphItem>;
  c-name: "pango_glyph_item_copy";
end;

define C-function pango-glyph-item-free
  input parameter self :: <PangoGlyphItem>;
  c-name: "pango_glyph_item_free";
end;

define C-function pango-glyph-item-get-logical-widths
  input parameter self :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  input parameter logical_widths_ :: <C-signed-int*>;
  c-name: "pango_glyph_item_get_logical_widths";
end;

define C-function pango-glyph-item-letter-space
  input parameter self :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  input parameter log_attrs_ :: <C-unsigned-char*> /* Not supported */;
  input parameter letter_spacing_ :: <C-signed-int>;
  c-name: "pango_glyph_item_letter_space";
end;

define C-function pango-glyph-item-split
  input parameter self :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  input parameter split_index_ :: <C-signed-int>;
  result res :: <PangoGlyphItem>;
  c-name: "pango_glyph_item_split";
end;

define C-struct <_PangoGlyphItemIter>
  slot pangoglyphitemiter-glyph-item :: <PangoGlyphItem>;
  slot pangoglyphitemiter-text :: <C-string>;
  slot pangoglyphitemiter-start-glyph :: <C-signed-int>;
  slot pangoglyphitemiter-start-index :: <C-signed-int>;
  slot pangoglyphitemiter-start-char :: <C-signed-int>;
  slot pangoglyphitemiter-end-glyph :: <C-signed-int>;
  slot pangoglyphitemiter-end-index :: <C-signed-int>;
  slot pangoglyphitemiter-end-char :: <C-signed-int>;
  pointer-type-name: <PangoGlyphItemIter>;
end C-struct;

define C-function pango-glyph-item-iter-copy
  input parameter self :: <PangoGlyphItemIter>;
  result res :: <PangoGlyphItemIter>;
  c-name: "pango_glyph_item_iter_copy";
end;

define C-function pango-glyph-item-iter-free
  input parameter self :: <PangoGlyphItemIter>;
  c-name: "pango_glyph_item_iter_free";
end;

define C-function pango-glyph-item-iter-init-end
  input parameter self :: <PangoGlyphItemIter>;
  input parameter glyph_item_ :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_glyph_item_iter_init_end";
end;

define C-function pango-glyph-item-iter-init-start
  input parameter self :: <PangoGlyphItemIter>;
  input parameter glyph_item_ :: <PangoGlyphItem>;
  input parameter text_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_glyph_item_iter_init_start";
end;

define C-function pango-glyph-item-iter-next-cluster
  input parameter self :: <PangoGlyphItemIter>;
  result res :: <C-boolean>;
  c-name: "pango_glyph_item_iter_next_cluster";
end;

define C-function pango-glyph-item-iter-prev-cluster
  input parameter self :: <PangoGlyphItemIter>;
  result res :: <C-boolean>;
  c-name: "pango_glyph_item_iter_prev_cluster";
end;

define C-struct <_PangoGlyphString>
  slot pangoglyphstring-num-glyphs :: <C-signed-int>;
  slot pangoglyphstring-glyphs :: <PangoGlyphInfo>;
  slot pangoglyphstring-log-clusters :: <C-signed-int*>;
  constant slot pangoglyphstring-space :: <C-signed-int>;
  pointer-type-name: <PangoGlyphString>;
end C-struct;

define C-function pango-glyph-string-new
  result res :: <PangoGlyphString>;
  c-name: "pango_glyph_string_new";
end;

define C-function pango-glyph-string-copy
  input parameter self :: <PangoGlyphString>;
  result res :: <PangoGlyphString>;
  c-name: "pango_glyph_string_copy";
end;

define C-function pango-glyph-string-extents
  input parameter self :: <PangoGlyphString>;
  input parameter font_ :: <PangoFont>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_glyph_string_extents";
end;

define C-function pango-glyph-string-extents-range
  input parameter self :: <PangoGlyphString>;
  input parameter start_ :: <C-signed-int>;
  input parameter end_ :: <C-signed-int>;
  input parameter font_ :: <PangoFont>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_glyph_string_extents_range";
end;

define C-function pango-glyph-string-free
  input parameter self :: <PangoGlyphString>;
  c-name: "pango_glyph_string_free";
end;

define C-function pango-glyph-string-get-logical-widths
  input parameter self :: <PangoGlyphString>;
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter embedding_level_ :: <C-signed-int>;
  input parameter logical_widths_ :: <C-signed-int*>;
  c-name: "pango_glyph_string_get_logical_widths";
end;

define C-function pango-glyph-string-get-width
  input parameter self :: <PangoGlyphString>;
  result res :: <C-signed-int>;
  c-name: "pango_glyph_string_get_width";
end;

define C-function pango-glyph-string-index-to-x
  input parameter self :: <PangoGlyphString>;
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter analysis_ :: <PangoAnalysis>;
  input parameter index__ :: <C-signed-int>;
  input parameter trailing_ :: <C-boolean>;
  output parameter x_pos_ :: <C-signed-int*>;
  c-name: "pango_glyph_string_index_to_x";
end;

define C-function pango-glyph-string-set-size
  input parameter self :: <PangoGlyphString>;
  input parameter new_len_ :: <C-signed-int>;
  c-name: "pango_glyph_string_set_size";
end;

define C-function pango-glyph-string-x-to-index
  input parameter self :: <PangoGlyphString>;
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter analysis_ :: <PangoAnalysis>;
  input parameter x_pos_ :: <C-signed-int>;
  output parameter index__ :: <C-signed-int*>;
  output parameter trailing_ :: <C-signed-int*>;
  c-name: "pango_glyph_string_x_to_index";
end;

define C-struct <_PangoGlyphVisAttr>
  slot pangoglyphvisattr-is-cluster-start :: <C-unsigned-int>;
  pointer-type-name: <PangoGlyphVisAttr>;
end C-struct;

define constant $PANGO-GRAVITY-SOUTH = 0;
define constant $PANGO-GRAVITY-EAST = 1;
define constant $PANGO-GRAVITY-NORTH = 2;
define constant $PANGO-GRAVITY-WEST = 3;
define constant $PANGO-GRAVITY-AUTO = 4;
define constant <PangoGravity> = <C-int>;
define C-pointer-type <PangoGravity*> => <PangoGravity>;

define constant $PANGO-GRAVITY-HINT-NATURAL = 0;
define constant $PANGO-GRAVITY-HINT-STRONG = 1;
define constant $PANGO-GRAVITY-HINT-LINE = 2;
define constant <PangoGravityHint> = <C-int>;
define C-pointer-type <PangoGravityHint*> => <PangoGravityHint>;

define C-struct <_PangoItem>
  slot pangoitem-offset :: <C-signed-int>;
  slot pangoitem-length :: <C-signed-int>;
  slot pangoitem-num-chars :: <C-signed-int>;
  slot pangoitem-analysis :: <PangoAnalysis>;
  pointer-type-name: <PangoItem>;
end C-struct;

define C-function pango-item-new
  result res :: <PangoItem>;
  c-name: "pango_item_new";
end;

define C-function pango-item-copy
  input parameter self :: <PangoItem>;
  result res :: <PangoItem>;
  c-name: "pango_item_copy";
end;

define C-function pango-item-free
  input parameter self :: <PangoItem>;
  c-name: "pango_item_free";
end;

define C-function pango-item-split
  input parameter self :: <PangoItem>;
  input parameter split_index_ :: <C-signed-int>;
  input parameter split_offset_ :: <C-signed-int>;
  result res :: <PangoItem>;
  c-name: "pango_item_split";
end;

define C-struct <_PangoLanguage>
  pointer-type-name: <PangoLanguage>;
end C-struct;

define C-function pango-language-get-sample-string
  input parameter self :: <PangoLanguage>;
  result res :: <C-string>;
  c-name: "pango_language_get_sample_string";
end;

define C-function pango-language-get-scripts
  input parameter self :: <PangoLanguage>;
  output parameter num_scripts_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "pango_language_get_scripts";
end;

define C-function pango-language-includes-script
  input parameter self :: <PangoLanguage>;
  input parameter script_ :: <PangoScript>;
  result res :: <C-boolean>;
  c-name: "pango_language_includes_script";
end;

define C-function pango-language-matches
  input parameter self :: <PangoLanguage>;
  input parameter range_list_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_language_matches";
end;

define C-function pango-language-to-string
  input parameter self :: <PangoLanguage>;
  result res :: <C-string>;
  c-name: "pango_language_to_string";
end;

define C-function pango-language-from-string
  input parameter language_ :: <C-string>;
  result res :: <PangoLanguage>;
  c-name: "pango_language_from_string";
end;

define C-function pango-language-get-default
  result res :: <PangoLanguage>;
  c-name: "pango_language_get_default";
end;

define open C-subtype <PangoLayout> (<GObject>)
end C-subtype;

define C-pointer-type <PangoLayout*> => <PangoLayout>;

define C-function pango-layout-new
  input parameter context_ :: <PangoContext>;
  result res :: <PangoLayout>;
  c-name: "pango_layout_new";
end;

define C-function pango-layout-context-changed
  input parameter self :: <PangoLayout>;
  c-name: "pango_layout_context_changed";
end;

define C-function pango-layout-copy
  input parameter self :: <PangoLayout>;
  result res :: <PangoLayout>;
  c-name: "pango_layout_copy";
end;

define C-function pango-layout-get-alignment
  input parameter self :: <PangoLayout>;
  result res :: <PangoAlignment>;
  c-name: "pango_layout_get_alignment";
end;

define C-function pango-layout-get-attributes
  input parameter self :: <PangoLayout>;
  result res :: <PangoAttrList>;
  c-name: "pango_layout_get_attributes";
end;

define C-function pango-layout-get-auto-dir
  input parameter self :: <PangoLayout>;
  result res :: <C-boolean>;
  c-name: "pango_layout_get_auto_dir";
end;

define C-function pango-layout-get-baseline
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_baseline";
end;

define C-function pango-layout-get-character-count
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_character_count";
end;

define C-function pango-layout-get-context
  input parameter self :: <PangoLayout>;
  result res :: <PangoContext>;
  c-name: "pango_layout_get_context";
end;

define C-function pango-layout-get-cursor-pos
  input parameter self :: <PangoLayout>;
  input parameter index__ :: <C-signed-int>;
  output parameter strong_pos_ :: <PangoRectangle>;
  output parameter weak_pos_ :: <PangoRectangle>;
  c-name: "pango_layout_get_cursor_pos";
end;

define C-function pango-layout-get-ellipsize
  input parameter self :: <PangoLayout>;
  result res :: <PangoEllipsizeMode>;
  c-name: "pango_layout_get_ellipsize";
end;

define C-function pango-layout-get-extents
  input parameter self :: <PangoLayout>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_get_extents";
end;

define C-function pango-layout-get-font-description
  input parameter self :: <PangoLayout>;
  result res :: <PangoFontDescription>;
  c-name: "pango_layout_get_font_description";
end;

define C-function pango-layout-get-height
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_height";
end;

define C-function pango-layout-get-indent
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_indent";
end;

define C-function pango-layout-get-iter
  input parameter self :: <PangoLayout>;
  result res :: <PangoLayoutIter>;
  c-name: "pango_layout_get_iter";
end;

define C-function pango-layout-get-justify
  input parameter self :: <PangoLayout>;
  result res :: <C-boolean>;
  c-name: "pango_layout_get_justify";
end;

define C-function pango-layout-get-line
  input parameter self :: <PangoLayout>;
  input parameter line_ :: <C-signed-int>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_layout_get_line";
end;

define C-function pango-layout-get-line-count
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_line_count";
end;

define C-function pango-layout-get-line-readonly
  input parameter self :: <PangoLayout>;
  input parameter line_ :: <C-signed-int>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_layout_get_line_readonly";
end;

define C-function pango-layout-get-lines
  input parameter self :: <PangoLayout>;
  result res :: <GSList>;
  c-name: "pango_layout_get_lines";
end;

define C-function pango-layout-get-lines-readonly
  input parameter self :: <PangoLayout>;
  result res :: <GSList>;
  c-name: "pango_layout_get_lines_readonly";
end;

define C-function pango-layout-get-log-attrs
  input parameter self :: <PangoLayout>;
  output parameter attrs_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_attrs_ :: <C-signed-int*>;
  c-name: "pango_layout_get_log_attrs";
end;

define C-function pango-layout-get-log-attrs-readonly
  input parameter self :: <PangoLayout>;
  output parameter n_attrs_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "pango_layout_get_log_attrs_readonly";
end;

define C-function pango-layout-get-pixel-extents
  input parameter self :: <PangoLayout>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_get_pixel_extents";
end;

define C-function pango-layout-get-pixel-size
  input parameter self :: <PangoLayout>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "pango_layout_get_pixel_size";
end;

define C-function pango-layout-get-serial
  input parameter self :: <PangoLayout>;
  result res :: <C-unsigned-int>;
  c-name: "pango_layout_get_serial";
end;

define C-function pango-layout-get-single-paragraph-mode
  input parameter self :: <PangoLayout>;
  result res :: <C-boolean>;
  c-name: "pango_layout_get_single_paragraph_mode";
end;

define C-function pango-layout-get-size
  input parameter self :: <PangoLayout>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "pango_layout_get_size";
end;

define C-function pango-layout-get-spacing
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_spacing";
end;

define C-function pango-layout-get-tabs
  input parameter self :: <PangoLayout>;
  result res :: <PangoTabArray>;
  c-name: "pango_layout_get_tabs";
end;

define C-function pango-layout-get-text
  input parameter self :: <PangoLayout>;
  result res :: <C-string>;
  c-name: "pango_layout_get_text";
end;

define C-function pango-layout-get-unknown-glyphs-count
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_unknown_glyphs_count";
end;

define C-function pango-layout-get-width
  input parameter self :: <PangoLayout>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_get_width";
end;

define C-function pango-layout-get-wrap
  input parameter self :: <PangoLayout>;
  result res :: <PangoWrapMode>;
  c-name: "pango_layout_get_wrap";
end;

define C-function pango-layout-index-to-line-x
  input parameter self :: <PangoLayout>;
  input parameter index__ :: <C-signed-int>;
  input parameter trailing_ :: <C-boolean>;
  output parameter line_ :: <C-signed-int*>;
  output parameter x_pos_ :: <C-signed-int*>;
  c-name: "pango_layout_index_to_line_x";
end;

define C-function pango-layout-index-to-pos
  input parameter self :: <PangoLayout>;
  input parameter index__ :: <C-signed-int>;
  output parameter pos_ :: <PangoRectangle>;
  c-name: "pango_layout_index_to_pos";
end;

define C-function pango-layout-is-ellipsized
  input parameter self :: <PangoLayout>;
  result res :: <C-boolean>;
  c-name: "pango_layout_is_ellipsized";
end;

define C-function pango-layout-is-wrapped
  input parameter self :: <PangoLayout>;
  result res :: <C-boolean>;
  c-name: "pango_layout_is_wrapped";
end;

define C-function pango-layout-move-cursor-visually
  input parameter self :: <PangoLayout>;
  input parameter strong_ :: <C-boolean>;
  input parameter old_index_ :: <C-signed-int>;
  input parameter old_trailing_ :: <C-signed-int>;
  input parameter direction_ :: <C-signed-int>;
  output parameter new_index_ :: <C-signed-int*>;
  output parameter new_trailing_ :: <C-signed-int*>;
  c-name: "pango_layout_move_cursor_visually";
end;

define C-function pango-layout-set-alignment
  input parameter self :: <PangoLayout>;
  input parameter alignment_ :: <PangoAlignment>;
  c-name: "pango_layout_set_alignment";
end;

define C-function pango-layout-set-attributes
  input parameter self :: <PangoLayout>;
  input parameter attrs_ :: <PangoAttrList>;
  c-name: "pango_layout_set_attributes";
end;

define C-function pango-layout-set-auto-dir
  input parameter self :: <PangoLayout>;
  input parameter auto_dir_ :: <C-boolean>;
  c-name: "pango_layout_set_auto_dir";
end;

define C-function pango-layout-set-ellipsize
  input parameter self :: <PangoLayout>;
  input parameter ellipsize_ :: <PangoEllipsizeMode>;
  c-name: "pango_layout_set_ellipsize";
end;

define C-function pango-layout-set-font-description
  input parameter self :: <PangoLayout>;
  input parameter desc_ :: <PangoFontDescription>;
  c-name: "pango_layout_set_font_description";
end;

define C-function pango-layout-set-height
  input parameter self :: <PangoLayout>;
  input parameter height_ :: <C-signed-int>;
  c-name: "pango_layout_set_height";
end;

define C-function pango-layout-set-indent
  input parameter self :: <PangoLayout>;
  input parameter indent_ :: <C-signed-int>;
  c-name: "pango_layout_set_indent";
end;

define C-function pango-layout-set-justify
  input parameter self :: <PangoLayout>;
  input parameter justify_ :: <C-boolean>;
  c-name: "pango_layout_set_justify";
end;

define C-function pango-layout-set-markup
  input parameter self :: <PangoLayout>;
  input parameter markup_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  c-name: "pango_layout_set_markup";
end;

define C-function pango-layout-set-markup-with-accel
  input parameter self :: <PangoLayout>;
  input parameter markup_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter accel_marker_ :: <C-unsigned-int>;
  output parameter accel_char_ :: <C-unsigned-int*>;
  c-name: "pango_layout_set_markup_with_accel";
end;

define C-function pango-layout-set-single-paragraph-mode
  input parameter self :: <PangoLayout>;
  input parameter setting_ :: <C-boolean>;
  c-name: "pango_layout_set_single_paragraph_mode";
end;

define C-function pango-layout-set-spacing
  input parameter self :: <PangoLayout>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "pango_layout_set_spacing";
end;

define C-function pango-layout-set-tabs
  input parameter self :: <PangoLayout>;
  input parameter tabs_ :: <PangoTabArray>;
  c-name: "pango_layout_set_tabs";
end;

define C-function pango-layout-set-text
  input parameter self :: <PangoLayout>;
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  c-name: "pango_layout_set_text";
end;

define C-function pango-layout-set-width
  input parameter self :: <PangoLayout>;
  input parameter width_ :: <C-signed-int>;
  c-name: "pango_layout_set_width";
end;

define C-function pango-layout-set-wrap
  input parameter self :: <PangoLayout>;
  input parameter wrap_ :: <PangoWrapMode>;
  c-name: "pango_layout_set_wrap";
end;

define C-function pango-layout-xy-to-index
  input parameter self :: <PangoLayout>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter index__ :: <C-signed-int*>;
  output parameter trailing_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "pango_layout_xy_to_index";
end;

define C-struct <_PangoLayoutClass>
  pointer-type-name: <PangoLayoutClass>;
end C-struct;

define C-struct <_PangoLayoutIter>
  pointer-type-name: <PangoLayoutIter>;
end C-struct;

define C-function pango-layout-iter-at-last-line
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-boolean>;
  c-name: "pango_layout_iter_at_last_line";
end;

define C-function pango-layout-iter-copy
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoLayoutIter>;
  c-name: "pango_layout_iter_copy";
end;

define C-function pango-layout-iter-free
  input parameter self :: <PangoLayoutIter>;
  c-name: "pango_layout_iter_free";
end;

define C-function pango-layout-iter-get-baseline
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_iter_get_baseline";
end;

define C-function pango-layout-iter-get-char-extents
  input parameter self :: <PangoLayoutIter>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_iter_get_char_extents";
end;

define C-function pango-layout-iter-get-cluster-extents
  input parameter self :: <PangoLayoutIter>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_iter_get_cluster_extents";
end;

define C-function pango-layout-iter-get-index
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-signed-int>;
  c-name: "pango_layout_iter_get_index";
end;

define C-function pango-layout-iter-get-layout
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoLayout>;
  c-name: "pango_layout_iter_get_layout";
end;

define C-function pango-layout-iter-get-layout-extents
  input parameter self :: <PangoLayoutIter>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_iter_get_layout_extents";
end;

define C-function pango-layout-iter-get-line
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_layout_iter_get_line";
end;

define C-function pango-layout-iter-get-line-extents
  input parameter self :: <PangoLayoutIter>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_iter_get_line_extents";
end;

define C-function pango-layout-iter-get-line-readonly
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_layout_iter_get_line_readonly";
end;

define C-function pango-layout-iter-get-line-yrange
  input parameter self :: <PangoLayoutIter>;
  output parameter y0__ :: <C-signed-int*>;
  output parameter y1__ :: <C-signed-int*>;
  c-name: "pango_layout_iter_get_line_yrange";
end;

define C-function pango-layout-iter-get-run
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoGlyphItem>;
  c-name: "pango_layout_iter_get_run";
end;

define C-function pango-layout-iter-get-run-extents
  input parameter self :: <PangoLayoutIter>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_iter_get_run_extents";
end;

define C-function pango-layout-iter-get-run-readonly
  input parameter self :: <PangoLayoutIter>;
  result res :: <PangoGlyphItem>;
  c-name: "pango_layout_iter_get_run_readonly";
end;

define C-function pango-layout-iter-next-char
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-boolean>;
  c-name: "pango_layout_iter_next_char";
end;

define C-function pango-layout-iter-next-cluster
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-boolean>;
  c-name: "pango_layout_iter_next_cluster";
end;

define C-function pango-layout-iter-next-line
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-boolean>;
  c-name: "pango_layout_iter_next_line";
end;

define C-function pango-layout-iter-next-run
  input parameter self :: <PangoLayoutIter>;
  result res :: <C-boolean>;
  c-name: "pango_layout_iter_next_run";
end;

define C-struct <_PangoLayoutLine>
  slot pangolayoutline-layout :: <PangoLayout>;
  slot pangolayoutline-start-index :: <C-signed-int>;
  slot pangolayoutline-length :: <C-signed-int>;
  slot pangolayoutline-runs :: <GSList>;
  slot pangolayoutline-is-paragraph-start :: <C-unsigned-int>;
  slot pangolayoutline-resolved-dir :: <C-unsigned-int>;
  pointer-type-name: <PangoLayoutLine>;
end C-struct;

define C-function pango-layout-line-get-extents
  input parameter self :: <PangoLayoutLine>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_line_get_extents";
end;

define C-function pango-layout-line-get-pixel-extents
  input parameter self :: <PangoLayoutLine>;
  output parameter ink_rect_ :: <PangoRectangle>;
  output parameter logical_rect_ :: <PangoRectangle>;
  c-name: "pango_layout_line_get_pixel_extents";
end;

define C-function pango-layout-line-get-x-ranges
  input parameter self :: <PangoLayoutLine>;
  input parameter start_index_ :: <C-signed-int>;
  input parameter end_index_ :: <C-signed-int>;
  output parameter ranges_ :: <C-signed-int*>;
  output parameter n_ranges_ :: <C-signed-int*>;
  c-name: "pango_layout_line_get_x_ranges";
end;

define C-function pango-layout-line-index-to-x
  input parameter self :: <PangoLayoutLine>;
  input parameter index__ :: <C-signed-int>;
  input parameter trailing_ :: <C-boolean>;
  output parameter x_pos_ :: <C-signed-int*>;
  c-name: "pango_layout_line_index_to_x";
end;

define C-function pango-layout-line-ref
  input parameter self :: <PangoLayoutLine>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_layout_line_ref";
end;

define C-function pango-layout-line-unref
  input parameter self :: <PangoLayoutLine>;
  c-name: "pango_layout_line_unref";
end;

define C-function pango-layout-line-x-to-index
  input parameter self :: <PangoLayoutLine>;
  input parameter x_pos_ :: <C-signed-int>;
  output parameter index__ :: <C-signed-int*>;
  output parameter trailing_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "pango_layout_line_x_to_index";
end;

define C-struct <_PangoLogAttr>
  slot pangologattr-is-line-break :: <C-unsigned-int>;
  slot pangologattr-is-mandatory-break :: <C-unsigned-int>;
  slot pangologattr-is-char-break :: <C-unsigned-int>;
  slot pangologattr-is-white :: <C-unsigned-int>;
  slot pangologattr-is-cursor-position :: <C-unsigned-int>;
  slot pangologattr-is-word-start :: <C-unsigned-int>;
  slot pangologattr-is-word-end :: <C-unsigned-int>;
  slot pangologattr-is-sentence-boundary :: <C-unsigned-int>;
  slot pangologattr-is-sentence-start :: <C-unsigned-int>;
  slot pangologattr-is-sentence-end :: <C-unsigned-int>;
  slot pangologattr-backspace-deletes-character :: <C-unsigned-int>;
  slot pangologattr-is-expandable-space :: <C-unsigned-int>;
  slot pangologattr-is-word-boundary :: <C-unsigned-int>;
  pointer-type-name: <PangoLogAttr>;
end C-struct;

define C-struct <_PangoMatrix>
  slot pangomatrix-xx :: <C-double>;
  slot pangomatrix-xy :: <C-double>;
  slot pangomatrix-yx :: <C-double>;
  slot pangomatrix-yy :: <C-double>;
  slot pangomatrix-x0 :: <C-double>;
  slot pangomatrix-y0 :: <C-double>;
  pointer-type-name: <PangoMatrix>;
end C-struct;

define C-function pango-matrix-concat
  input parameter self :: <PangoMatrix>;
  input parameter new_matrix_ :: <PangoMatrix>;
  c-name: "pango_matrix_concat";
end;

define C-function pango-matrix-copy
  input parameter self :: <PangoMatrix>;
  result res :: <PangoMatrix>;
  c-name: "pango_matrix_copy";
end;

define C-function pango-matrix-free
  input parameter self :: <PangoMatrix>;
  c-name: "pango_matrix_free";
end;

define C-function pango-matrix-get-font-scale-factor
  input parameter self :: <PangoMatrix>;
  result res :: <C-double>;
  c-name: "pango_matrix_get_font_scale_factor";
end;

define C-function pango-matrix-rotate
  input parameter self :: <PangoMatrix>;
  input parameter degrees_ :: <C-double>;
  c-name: "pango_matrix_rotate";
end;

define C-function pango-matrix-scale
  input parameter self :: <PangoMatrix>;
  input parameter scale_x_ :: <C-double>;
  input parameter scale_y_ :: <C-double>;
  c-name: "pango_matrix_scale";
end;

define C-function pango-matrix-transform-distance
  input parameter self :: <PangoMatrix>;
  input output parameter dx_ :: <C-double*>;
  input output parameter dy_ :: <C-double*>;
  c-name: "pango_matrix_transform_distance";
end;

define C-function pango-matrix-transform-pixel-rectangle
  input parameter self :: <PangoMatrix>;
  input output parameter rect_ :: <PangoRectangle>;
  c-name: "pango_matrix_transform_pixel_rectangle";
end;

define C-function pango-matrix-transform-point
  input parameter self :: <PangoMatrix>;
  input output parameter x_ :: <C-double*>;
  input output parameter y_ :: <C-double*>;
  c-name: "pango_matrix_transform_point";
end;

define C-function pango-matrix-transform-rectangle
  input parameter self :: <PangoMatrix>;
  input output parameter rect_ :: <PangoRectangle>;
  c-name: "pango_matrix_transform_rectangle";
end;

define C-function pango-matrix-translate
  input parameter self :: <PangoMatrix>;
  input parameter tx_ :: <C-double>;
  input parameter ty_ :: <C-double>;
  c-name: "pango_matrix_translate";
end;

define constant $RENDER-TYPE-NONE = "PangoRenderNone";

define C-struct <_PangoRectangle>
  slot pangorectangle-x :: <C-signed-int>;
  slot pangorectangle-y :: <C-signed-int>;
  slot pangorectangle-width :: <C-signed-int>;
  slot pangorectangle-height :: <C-signed-int>;
  pointer-type-name: <PangoRectangle>;
end C-struct;

define constant $PANGO-RENDER-PART-FOREGROUND = 0;
define constant $PANGO-RENDER-PART-BACKGROUND = 1;
define constant $PANGO-RENDER-PART-UNDERLINE = 2;
define constant $PANGO-RENDER-PART-STRIKETHROUGH = 3;
define constant <PangoRenderPart> = <C-int>;
define C-pointer-type <PangoRenderPart*> => <PangoRenderPart>;

define open C-subtype <PangoRenderer> (<GObject>)
  constant slot pangorenderer-parent-instance :: <GObject>;
  constant slot pangorenderer-underline :: <PangoUnderline>;
  constant slot pangorenderer-strikethrough :: <C-boolean>;
  constant slot pangorenderer-active-count :: <C-signed-int>;
  constant slot pangorenderer-matrix :: <PangoMatrix>;
  constant slot pangorenderer-priv :: <PangoRendererPrivate>;
end C-subtype;

define C-pointer-type <PangoRenderer*> => <PangoRenderer>;

define C-function pango-renderer-activate
  input parameter self :: <PangoRenderer>;
  c-name: "pango_renderer_activate";
end;

define C-function pango-renderer-deactivate
  input parameter self :: <PangoRenderer>;
  c-name: "pango_renderer_deactivate";
end;

define C-function pango-renderer-draw-error-underline
  input parameter self :: <PangoRenderer>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_error_underline";
end;

define C-function pango-renderer-draw-glyph
  input parameter self :: <PangoRenderer>;
  input parameter font_ :: <PangoFont>;
  input parameter glyph_ :: <C-unsigned-int>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "pango_renderer_draw_glyph";
end;

define C-function pango-renderer-draw-glyph-item
  input parameter self :: <PangoRenderer>;
  input parameter text_ :: <C-string>;
  input parameter glyph_item_ :: <PangoGlyphItem>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_glyph_item";
end;

define C-function pango-renderer-draw-glyphs
  input parameter self :: <PangoRenderer>;
  input parameter font_ :: <PangoFont>;
  input parameter glyphs_ :: <PangoGlyphString>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_glyphs";
end;

define C-function pango-renderer-draw-layout
  input parameter self :: <PangoRenderer>;
  input parameter layout_ :: <PangoLayout>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_layout";
end;

define C-function pango-renderer-draw-layout-line
  input parameter self :: <PangoRenderer>;
  input parameter line_ :: <PangoLayoutLine>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_layout_line";
end;

define C-function pango-renderer-draw-rectangle
  input parameter self :: <PangoRenderer>;
  input parameter part_ :: <PangoRenderPart>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "pango_renderer_draw_rectangle";
end;

define C-function pango-renderer-draw-trapezoid
  input parameter self :: <PangoRenderer>;
  input parameter part_ :: <PangoRenderPart>;
  input parameter y1__ :: <C-double>;
  input parameter x11_ :: <C-double>;
  input parameter x21_ :: <C-double>;
  input parameter y2_ :: <C-double>;
  input parameter x12_ :: <C-double>;
  input parameter x22_ :: <C-double>;
  c-name: "pango_renderer_draw_trapezoid";
end;

define C-function pango-renderer-get-color
  input parameter self :: <PangoRenderer>;
  input parameter part_ :: <PangoRenderPart>;
  result res :: <PangoColor>;
  c-name: "pango_renderer_get_color";
end;

define C-function pango-renderer-get-layout
  input parameter self :: <PangoRenderer>;
  result res :: <PangoLayout>;
  c-name: "pango_renderer_get_layout";
end;

define C-function pango-renderer-get-layout-line
  input parameter self :: <PangoRenderer>;
  result res :: <PangoLayoutLine>;
  c-name: "pango_renderer_get_layout_line";
end;

define C-function pango-renderer-get-matrix
  input parameter self :: <PangoRenderer>;
  result res :: <PangoMatrix>;
  c-name: "pango_renderer_get_matrix";
end;

define C-function pango-renderer-part-changed
  input parameter self :: <PangoRenderer>;
  input parameter part_ :: <PangoRenderPart>;
  c-name: "pango_renderer_part_changed";
end;

define C-function pango-renderer-set-color
  input parameter self :: <PangoRenderer>;
  input parameter part_ :: <PangoRenderPart>;
  input parameter color_ :: <PangoColor>;
  c-name: "pango_renderer_set_color";
end;

define C-function pango-renderer-set-matrix
  input parameter self :: <PangoRenderer>;
  input parameter matrix_ :: <PangoMatrix>;
  c-name: "pango_renderer_set_matrix";
end;

define C-struct <_PangoRendererClass>
  constant slot pangorendererclass-parent-class :: <GObjectClass>;
  constant slot pangorendererclass-draw-glyphs :: <C-function-pointer>;
  constant slot pangorendererclass-draw-rectangle :: <C-function-pointer>;
  constant slot pangorendererclass-draw-error-underline :: <C-function-pointer>;
  constant slot pangorendererclass-draw-shape :: <C-function-pointer>;
  constant slot pangorendererclass-draw-trapezoid :: <C-function-pointer>;
  constant slot pangorendererclass-draw-glyph :: <C-function-pointer>;
  constant slot pangorendererclass-part-changed :: <C-function-pointer>;
  constant slot pangorendererclass-begin :: <C-function-pointer>;
  constant slot pangorendererclass-end :: <C-function-pointer>;
  constant slot pangorendererclass-prepare-run :: <C-function-pointer>;
  constant slot pangorendererclass-draw-glyph-item :: <C-function-pointer>;
  constant slot pangorendererclass--pango-reserved2 :: <C-void*>;
  constant slot pangorendererclass--pango-reserved3 :: <C-void*>;
  constant slot pangorendererclass--pango-reserved4 :: <C-void*>;
  pointer-type-name: <PangoRendererClass>;
end C-struct;

define C-struct <_PangoRendererPrivate>
  pointer-type-name: <PangoRendererPrivate>;
end C-struct;

define constant $SCALE = 1024;

define constant $PANGO-SCRIPT-INVALID-CODE = -1;
define constant $PANGO-SCRIPT-COMMON = 0;
define constant $PANGO-SCRIPT-INHERITED = 1;
define constant $PANGO-SCRIPT-ARABIC = 2;
define constant $PANGO-SCRIPT-ARMENIAN = 3;
define constant $PANGO-SCRIPT-BENGALI = 4;
define constant $PANGO-SCRIPT-BOPOMOFO = 5;
define constant $PANGO-SCRIPT-CHEROKEE = 6;
define constant $PANGO-SCRIPT-COPTIC = 7;
define constant $PANGO-SCRIPT-CYRILLIC = 8;
define constant $PANGO-SCRIPT-DESERET = 9;
define constant $PANGO-SCRIPT-DEVANAGARI = 10;
define constant $PANGO-SCRIPT-ETHIOPIC = 11;
define constant $PANGO-SCRIPT-GEORGIAN = 12;
define constant $PANGO-SCRIPT-GOTHIC = 13;
define constant $PANGO-SCRIPT-GREEK = 14;
define constant $PANGO-SCRIPT-GUJARATI = 15;
define constant $PANGO-SCRIPT-GURMUKHI = 16;
define constant $PANGO-SCRIPT-HAN = 17;
define constant $PANGO-SCRIPT-HANGUL = 18;
define constant $PANGO-SCRIPT-HEBREW = 19;
define constant $PANGO-SCRIPT-HIRAGANA = 20;
define constant $PANGO-SCRIPT-KANNADA = 21;
define constant $PANGO-SCRIPT-KATAKANA = 22;
define constant $PANGO-SCRIPT-KHMER = 23;
define constant $PANGO-SCRIPT-LAO = 24;
define constant $PANGO-SCRIPT-LATIN = 25;
define constant $PANGO-SCRIPT-MALAYALAM = 26;
define constant $PANGO-SCRIPT-MONGOLIAN = 27;
define constant $PANGO-SCRIPT-MYANMAR = 28;
define constant $PANGO-SCRIPT-OGHAM = 29;
define constant $PANGO-SCRIPT-OLD-ITALIC = 30;
define constant $PANGO-SCRIPT-ORIYA = 31;
define constant $PANGO-SCRIPT-RUNIC = 32;
define constant $PANGO-SCRIPT-SINHALA = 33;
define constant $PANGO-SCRIPT-SYRIAC = 34;
define constant $PANGO-SCRIPT-TAMIL = 35;
define constant $PANGO-SCRIPT-TELUGU = 36;
define constant $PANGO-SCRIPT-THAANA = 37;
define constant $PANGO-SCRIPT-THAI = 38;
define constant $PANGO-SCRIPT-TIBETAN = 39;
define constant $PANGO-SCRIPT-CANADIAN-ABORIGINAL = 40;
define constant $PANGO-SCRIPT-YI = 41;
define constant $PANGO-SCRIPT-TAGALOG = 42;
define constant $PANGO-SCRIPT-HANUNOO = 43;
define constant $PANGO-SCRIPT-BUHID = 44;
define constant $PANGO-SCRIPT-TAGBANWA = 45;
define constant $PANGO-SCRIPT-BRAILLE = 46;
define constant $PANGO-SCRIPT-CYPRIOT = 47;
define constant $PANGO-SCRIPT-LIMBU = 48;
define constant $PANGO-SCRIPT-OSMANYA = 49;
define constant $PANGO-SCRIPT-SHAVIAN = 50;
define constant $PANGO-SCRIPT-LINEAR-B = 51;
define constant $PANGO-SCRIPT-TAI-LE = 52;
define constant $PANGO-SCRIPT-UGARITIC = 53;
define constant $PANGO-SCRIPT-NEW-TAI-LUE = 54;
define constant $PANGO-SCRIPT-BUGINESE = 55;
define constant $PANGO-SCRIPT-GLAGOLITIC = 56;
define constant $PANGO-SCRIPT-TIFINAGH = 57;
define constant $PANGO-SCRIPT-SYLOTI-NAGRI = 58;
define constant $PANGO-SCRIPT-OLD-PERSIAN = 59;
define constant $PANGO-SCRIPT-KHAROSHTHI = 60;
define constant $PANGO-SCRIPT-UNKNOWN = 61;
define constant $PANGO-SCRIPT-BALINESE = 62;
define constant $PANGO-SCRIPT-CUNEIFORM = 63;
define constant $PANGO-SCRIPT-PHOENICIAN = 64;
define constant $PANGO-SCRIPT-PHAGS-PA = 65;
define constant $PANGO-SCRIPT-NKO = 66;
define constant $PANGO-SCRIPT-KAYAH-LI = 67;
define constant $PANGO-SCRIPT-LEPCHA = 68;
define constant $PANGO-SCRIPT-REJANG = 69;
define constant $PANGO-SCRIPT-SUNDANESE = 70;
define constant $PANGO-SCRIPT-SAURASHTRA = 71;
define constant $PANGO-SCRIPT-CHAM = 72;
define constant $PANGO-SCRIPT-OL-CHIKI = 73;
define constant $PANGO-SCRIPT-VAI = 74;
define constant $PANGO-SCRIPT-CARIAN = 75;
define constant $PANGO-SCRIPT-LYCIAN = 76;
define constant $PANGO-SCRIPT-LYDIAN = 77;
define constant $PANGO-SCRIPT-BATAK = 78;
define constant $PANGO-SCRIPT-BRAHMI = 79;
define constant $PANGO-SCRIPT-MANDAIC = 80;
define constant $PANGO-SCRIPT-CHAKMA = 81;
define constant $PANGO-SCRIPT-MEROITIC-CURSIVE = 82;
define constant $PANGO-SCRIPT-MEROITIC-HIEROGLYPHS = 83;
define constant $PANGO-SCRIPT-MIAO = 84;
define constant $PANGO-SCRIPT-SHARADA = 85;
define constant $PANGO-SCRIPT-SORA-SOMPENG = 86;
define constant $PANGO-SCRIPT-TAKRI = 87;
define constant <PangoScript> = <C-int>;
define C-pointer-type <PangoScript*> => <PangoScript>;

define C-struct <_PangoScriptIter>
  pointer-type-name: <PangoScriptIter>;
end C-struct;

define C-function pango-script-iter-free
  input parameter self :: <PangoScriptIter>;
  c-name: "pango_script_iter_free";
end;

define C-function pango-script-iter-get-range
  input parameter self :: <PangoScriptIter>;
  output parameter start_ :: <C-string>;
  output parameter end_ :: <C-string>;
  output parameter script_ :: <PangoScript*>;
  c-name: "pango_script_iter_get_range";
end;

define C-function pango-script-iter-next
  input parameter self :: <PangoScriptIter>;
  result res :: <C-boolean>;
  c-name: "pango_script_iter_next";
end;

define constant $PANGO-STRETCH-ULTRA-CONDENSED = 0;
define constant $PANGO-STRETCH-EXTRA-CONDENSED = 1;
define constant $PANGO-STRETCH-CONDENSED = 2;
define constant $PANGO-STRETCH-SEMI-CONDENSED = 3;
define constant $PANGO-STRETCH-NORMAL = 4;
define constant $PANGO-STRETCH-SEMI-EXPANDED = 5;
define constant $PANGO-STRETCH-EXPANDED = 6;
define constant $PANGO-STRETCH-EXTRA-EXPANDED = 7;
define constant $PANGO-STRETCH-ULTRA-EXPANDED = 8;
define constant <PangoStretch> = <C-int>;
define C-pointer-type <PangoStretch*> => <PangoStretch>;

define constant $PANGO-STYLE-NORMAL = 0;
define constant $PANGO-STYLE-OBLIQUE = 1;
define constant $PANGO-STYLE-ITALIC = 2;
define constant <PangoStyle> = <C-int>;
define C-pointer-type <PangoStyle*> => <PangoStyle>;

define constant $PANGO-TAB-LEFT = 0;
define constant <PangoTabAlign> = <C-int>;
define C-pointer-type <PangoTabAlign*> => <PangoTabAlign>;

define C-struct <_PangoTabArray>
  pointer-type-name: <PangoTabArray>;
end C-struct;

define C-function pango-tab-array-new
  input parameter initial_size_ :: <C-signed-int>;
  input parameter positions_in_pixels_ :: <C-boolean>;
  result res :: <PangoTabArray>;
  c-name: "pango_tab_array_new";
end;

define C-function pango-tab-array-copy
  input parameter self :: <PangoTabArray>;
  result res :: <PangoTabArray>;
  c-name: "pango_tab_array_copy";
end;

define C-function pango-tab-array-free
  input parameter self :: <PangoTabArray>;
  c-name: "pango_tab_array_free";
end;

define C-function pango-tab-array-get-positions-in-pixels
  input parameter self :: <PangoTabArray>;
  result res :: <C-boolean>;
  c-name: "pango_tab_array_get_positions_in_pixels";
end;

define C-function pango-tab-array-get-size
  input parameter self :: <PangoTabArray>;
  result res :: <C-signed-int>;
  c-name: "pango_tab_array_get_size";
end;

define C-function pango-tab-array-get-tab
  input parameter self :: <PangoTabArray>;
  input parameter tab_index_ :: <C-signed-int>;
  output parameter alignment_ :: <PangoTabAlign*>;
  output parameter location_ :: <C-signed-int*>;
  c-name: "pango_tab_array_get_tab";
end;

define C-function pango-tab-array-get-tabs
  input parameter self :: <PangoTabArray>;
  output parameter alignments_ :: <PangoTabAlign*>;
  output parameter locations_ :: <C-signed-int*>;
  c-name: "pango_tab_array_get_tabs";
end;

define C-function pango-tab-array-resize
  input parameter self :: <PangoTabArray>;
  input parameter new_size_ :: <C-signed-int>;
  c-name: "pango_tab_array_resize";
end;

define C-function pango-tab-array-set-tab
  input parameter self :: <PangoTabArray>;
  input parameter tab_index_ :: <C-signed-int>;
  input parameter alignment_ :: <PangoTabAlign>;
  input parameter location_ :: <C-signed-int>;
  c-name: "pango_tab_array_set_tab";
end;

define constant $UNKNOWN-GLYPH-HEIGHT = 14;

define constant $UNKNOWN-GLYPH-WIDTH = 10;

define constant $PANGO-UNDERLINE-NONE = 0;
define constant $PANGO-UNDERLINE-SINGLE = 1;
define constant $PANGO-UNDERLINE-DOUBLE = 2;
define constant $PANGO-UNDERLINE-LOW = 3;
define constant $PANGO-UNDERLINE-ERROR = 4;
define constant <PangoUnderline> = <C-int>;
define C-pointer-type <PangoUnderline*> => <PangoUnderline>;

define constant $PANGO-VARIANT-NORMAL = 0;
define constant $PANGO-VARIANT-SMALL-CAPS = 1;
define constant <PangoVariant> = <C-int>;
define C-pointer-type <PangoVariant*> => <PangoVariant>;

define constant $PANGO-WEIGHT-THIN = 100;
define constant $PANGO-WEIGHT-ULTRALIGHT = 200;
define constant $PANGO-WEIGHT-LIGHT = 300;
define constant $PANGO-WEIGHT-BOOK = 380;
define constant $PANGO-WEIGHT-NORMAL = 400;
define constant $PANGO-WEIGHT-MEDIUM = 500;
define constant $PANGO-WEIGHT-SEMIBOLD = 600;
define constant $PANGO-WEIGHT-BOLD = 700;
define constant $PANGO-WEIGHT-ULTRABOLD = 800;
define constant $PANGO-WEIGHT-HEAVY = 900;
define constant $PANGO-WEIGHT-ULTRAHEAVY = 1000;
define constant <PangoWeight> = <C-int>;
define C-pointer-type <PangoWeight*> => <PangoWeight>;

define constant $PANGO-WRAP-WORD = 0;
define constant $PANGO-WRAP-CHAR = 1;
define constant $PANGO-WRAP-WORD-CHAR = 2;
define constant <PangoWrapMode> = <C-int>;
define C-pointer-type <PangoWrapMode*> => <PangoWrapMode>;

define C-struct <_Pango_ScriptForLang>
  slot pango-scriptforlang-lang :: <C-signed-char*>;
  slot pango-scriptforlang-scripts :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <Pango_ScriptForLang>;
end C-struct;

define C-function pango-attr-type-get-name
  input parameter type_ :: <PangoAttrType>;
  result res :: <C-string>;
  c-name: "pango_attr_type_get_name";
end;

define C-function pango-attr-type-register
  input parameter name_ :: <C-string>;
  result res :: <PangoAttrType>;
  c-name: "pango_attr_type_register";
end;

define C-function pango-bidi-type-for-unichar
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <PangoBidiType>;
  c-name: "pango_bidi_type_for_unichar";
end;

define C-function pango-break
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter analysis_ :: <PangoAnalysis>;
  input parameter attrs_ :: <C-unsigned-char*> /* Not supported */;
  input parameter attrs_len_ :: <C-signed-int>;
  c-name: "pango_break";
end;

define C-function pango-extents-to-pixels
  input parameter inclusive_ :: <PangoRectangle>;
  input parameter nearest_ :: <PangoRectangle>;
  c-name: "pango_extents_to_pixels";
end;

define C-function pango-find-base-dir
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  result res :: <PangoDirection>;
  c-name: "pango_find_base_dir";
end;

define C-function pango-find-paragraph-boundary
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  output parameter paragraph_delimiter_index_ :: <C-signed-int*>;
  output parameter next_paragraph_start_ :: <C-signed-int*>;
  c-name: "pango_find_paragraph_boundary";
end;

define C-function pango-get-log-attrs
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter level_ :: <C-signed-int>;
  input parameter language_ :: <PangoLanguage>;
  input parameter log_attrs_ :: <C-unsigned-char*> /* Not supported */;
  input parameter attrs_len_ :: <C-signed-int>;
  c-name: "pango_get_log_attrs";
end;

define C-function pango-get-mirror-char
  input parameter ch_ :: <C-unsigned-int>;
  input parameter mirrored_ch_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "pango_get_mirror_char";
end;

define C-function pango-gravity-get-for-matrix
  input parameter matrix_ :: <PangoMatrix>;
  result res :: <PangoGravity>;
  c-name: "pango_gravity_get_for_matrix";
end;

define C-function pango-gravity-get-for-script
  input parameter script_ :: <PangoScript>;
  input parameter base_gravity_ :: <PangoGravity>;
  input parameter hint_ :: <PangoGravityHint>;
  result res :: <PangoGravity>;
  c-name: "pango_gravity_get_for_script";
end;

define C-function pango-gravity-get-for-script-and-width
  input parameter script_ :: <PangoScript>;
  input parameter wide_ :: <C-boolean>;
  input parameter base_gravity_ :: <PangoGravity>;
  input parameter hint_ :: <PangoGravityHint>;
  result res :: <PangoGravity>;
  c-name: "pango_gravity_get_for_script_and_width";
end;

define C-function pango-gravity-to-rotation
  input parameter gravity_ :: <PangoGravity>;
  result res :: <C-double>;
  c-name: "pango_gravity_to_rotation";
end;

define C-function pango-is-zero-width
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "pango_is_zero_width";
end;

define C-function pango-itemize
  input parameter context_ :: <PangoContext>;
  input parameter text_ :: <C-string>;
  input parameter start_index_ :: <C-signed-int>;
  input parameter length_ :: <C-signed-int>;
  input parameter attrs_ :: <PangoAttrList>;
  input parameter cached_iter_ :: <PangoAttrIterator>;
  result res :: <GList>;
  c-name: "pango_itemize";
end;

define C-function pango-itemize-with-base-dir
  input parameter context_ :: <PangoContext>;
  input parameter base_dir_ :: <PangoDirection>;
  input parameter text_ :: <C-string>;
  input parameter start_index_ :: <C-signed-int>;
  input parameter length_ :: <C-signed-int>;
  input parameter attrs_ :: <PangoAttrList>;
  input parameter cached_iter_ :: <PangoAttrIterator>;
  result res :: <GList>;
  c-name: "pango_itemize_with_base_dir";
end;

define C-function pango-log2vis-get-embedding-levels
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter pbase_dir_ :: <PangoDirection>;
  result res :: <C-unsigned-char*>;
  c-name: "pango_log2vis_get_embedding_levels";
end;

define C-function pango-markup-parser-finish
  input parameter context_ :: <GMarkupParseContext>;
  output parameter attr_list_ :: <PangoAttrList>;
  output parameter text_ :: <C-string>;
  output parameter accel_char_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "pango_markup_parser_finish";
end;

define C-function pango-markup-parser-new
  input parameter accel_marker_ :: <C-unsigned-int>;
  result res :: <GMarkupParseContext>;
  c-name: "pango_markup_parser_new";
end;

define C-function pango-parse-enum
  input parameter type_ :: <C-long>;
  input parameter str_ :: <C-string>;
  output parameter value_ :: <C-signed-int*>;
  input parameter warn_ :: <C-boolean>;
  output parameter possible_values_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_parse_enum";
end;

define C-function pango-parse-markup
  input parameter markup_text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter accel_marker_ :: <C-unsigned-int>;
  output parameter attr_list_ :: <PangoAttrList>;
  output parameter text_ :: <C-string>;
  output parameter accel_char_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "pango_parse_markup";
end;

define C-function pango-parse-stretch
  input parameter str_ :: <C-string>;
  output parameter stretch_ :: <PangoStretch*>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_stretch";
end;

define C-function pango-parse-style
  input parameter str_ :: <C-string>;
  output parameter style_ :: <PangoStyle*>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_style";
end;

define C-function pango-parse-variant
  input parameter str_ :: <C-string>;
  output parameter variant_ :: <PangoVariant*>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_variant";
end;

define C-function pango-parse-weight
  input parameter str_ :: <C-string>;
  output parameter weight_ :: <PangoWeight*>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_weight";
end;

define C-function pango-quantize-line-geometry
  input output parameter thickness_ :: <C-signed-int*>;
  input output parameter position_ :: <C-signed-int*>;
  c-name: "pango_quantize_line_geometry";
end;

define C-function pango-read-line
  input parameter stream_ :: <C-void*>;
  output parameter str_ :: <GString>;
  result res :: <C-signed-int>;
  c-name: "pango_read_line";
end;

define C-function pango-reorder-items
  input parameter logical_items_ :: <GList>;
  result res :: <GList>;
  c-name: "pango_reorder_items";
end;

define C-function pango-scan-int
  input output parameter pos_ :: <C-string>;
  output parameter out_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "pango_scan_int";
end;

define C-function pango-scan-string
  input output parameter pos_ :: <C-string>;
  output parameter out_ :: <GString>;
  result res :: <C-boolean>;
  c-name: "pango_scan_string";
end;

define C-function pango-scan-word
  input output parameter pos_ :: <C-string>;
  output parameter out_ :: <GString>;
  result res :: <C-boolean>;
  c-name: "pango_scan_word";
end;

define C-function pango-script-for-unichar
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <PangoScript>;
  c-name: "pango_script_for_unichar";
end;

define C-function pango-script-get-sample-language
  input parameter script_ :: <PangoScript>;
  result res :: <PangoLanguage>;
  c-name: "pango_script_get_sample_language";
end;

define C-function pango-shape
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter analysis_ :: <PangoAnalysis>;
  input parameter glyphs_ :: <PangoGlyphString>;
  c-name: "pango_shape";
end;

define C-function pango-shape-full
  input parameter item_text_ :: <C-string>;
  input parameter item_length_ :: <C-signed-int>;
  input parameter paragraph_text_ :: <C-string>;
  input parameter paragraph_length_ :: <C-signed-int>;
  input parameter analysis_ :: <PangoAnalysis>;
  input parameter glyphs_ :: <PangoGlyphString>;
  c-name: "pango_shape_full";
end;

define C-function pango-skip-space
  input output parameter pos_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "pango_skip_space";
end;

define C-function pango-split-file-list
  input parameter str_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "pango_split_file_list";
end;

define C-function pango-trim-string
  input parameter str_ :: <C-string>;
  result res :: <C-string>;
  c-name: "pango_trim_string";
end;

define C-function pango-unichar-direction
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <PangoDirection>;
  c-name: "pango_unichar_direction";
end;

define C-function pango-units-from-double
  input parameter d_ :: <C-double>;
  result res :: <C-signed-int>;
  c-name: "pango_units_from_double";
end;

define C-function pango-units-to-double
  input parameter i_ :: <C-signed-int>;
  result res :: <C-double>;
  c-name: "pango_units_to_double";
end;

define C-function pango-version
  result res :: <C-signed-int>;
  c-name: "pango_version";
end;

define C-function pango-version-check
  input parameter required_major_ :: <C-signed-int>;
  input parameter required_minor_ :: <C-signed-int>;
  input parameter required_micro_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "pango_version_check";
end;

define C-function pango-version-string
  result res :: <C-string>;
  c-name: "pango_version_string";
end;

