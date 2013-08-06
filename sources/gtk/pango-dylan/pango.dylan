module: pango
synopsis: generated bindings for the Pango library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;
ignore(<C-void**>);

define C-pointer-type <GError*> => <GError>;
ignore(<GError*>);

define constant $analysis-flag-centered-baseline = 1;

define constant $attr-index-from-text-beginning = 0;

define constant $pango-align-left = 0;
define constant $pango-align-center = 1;
define constant $pango-align-right = 2;
define constant <PangoAlignment> = <C-int>;
define C-pointer-type <PangoAlignment*> => <PangoAlignment>;

define C-struct <_PangoAnalysis>
  slot pango-analysis-shape-engine :: <PangoEngineShape>;
  slot pango-analysis-lang-engine :: <PangoEngineLang>;
  slot pango-analysis-font :: <PangoFont>;
  slot pango-analysis-level :: <C-unsigned-char>;
  slot pango-analysis-gravity :: <C-unsigned-char>;
  slot pango-analysis-flags :: <C-unsigned-char>;
  slot pango-analysis-script :: <C-unsigned-char>;
  slot pango-analysis-language :: <PangoLanguage>;
  slot pango-analysis-extra-attrs :: <GSList>;
  pointer-type-name: <PangoAnalysis>;
end C-struct;

define C-struct <_PangoAttrClass>
  slot pango-attr-class-type :: <PangoAttrType>;
  constant slot pango-attr-class-copy :: <C-void*>;
  constant slot pango-attr-class-destroy :: <C-function-pointer>;
  constant slot pango-attr-class-equal :: <C-function-pointer>;
  pointer-type-name: <PangoAttrClass>;
end C-struct;

define C-struct <_PangoAttrColor>
  slot pango-attr-color-attr :: <PangoAttribute>;
  slot pango-attr-color-color :: <PangoColor>;
  pointer-type-name: <PangoAttrColor>;
end C-struct;

define C-struct <_PangoAttrFloat>
  slot pango-attr-float-attr :: <PangoAttribute>;
  slot pango-attr-float-value :: <C-double>;
  pointer-type-name: <PangoAttrFloat>;
end C-struct;

define C-struct <_PangoAttrFontDesc>
  slot pango-attr-font-desc-attr :: <PangoAttribute>;
  slot pango-attr-font-desc-desc :: <PangoFontDescription>;
  pointer-type-name: <PangoAttrFontDesc>;
end C-struct;

define C-struct <_PangoAttrInt>
  slot pango-attr-int-attr :: <PangoAttribute>;
  slot pango-attr-int-value :: <C-signed-int>;
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
  input parameter start_ :: <C-signed-int*>;
  input parameter end_ :: <C-signed-int*>;
  c-name: "pango_attr_iterator_range";
end;

define C-struct <_PangoAttrLanguage>
  slot pango-attr-language-attr :: <PangoAttribute>;
  slot pango-attr-language-value :: <PangoLanguage>;
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
  slot pango-attr-shape-attr :: <PangoAttribute>;
  slot pango-attr-shape-ink-rect :: <PangoRectangle>;
  slot pango-attr-shape-logical-rect :: <PangoRectangle>;
  slot pango-attr-shape-data :: <C-void*>;
  slot pango-attr-shape-copy-func :: <C-void*>;
  slot pango-attr-shape-destroy-func :: <C-function-pointer>;
  pointer-type-name: <PangoAttrShape>;
end C-struct;

define C-struct <_PangoAttrSize>
  slot pango-attr-size-attr :: <PangoAttribute>;
  slot pango-attr-size-size :: <C-signed-int>;
  slot pango-attr-size-absolute :: <C-unsigned-int>;
  pointer-type-name: <PangoAttrSize>;
end C-struct;

define C-struct <_PangoAttrString>
  slot pango-attr-string-attr :: <PangoAttribute>;
  slot pango-attr-string-value :: <C-string>;
  pointer-type-name: <PangoAttrString>;
end C-struct;

define constant $pango-attr-invalid = 0;
define constant $pango-attr-language = 1;
define constant $pango-attr-family = 2;
define constant $pango-attr-style = 3;
define constant $pango-attr-weight = 4;
define constant $pango-attr-variant = 5;
define constant $pango-attr-stretch = 6;
define constant $pango-attr-size = 7;
define constant $pango-attr-font-desc = 8;
define constant $pango-attr-foreground = 9;
define constant $pango-attr-background = 10;
define constant $pango-attr-underline = 11;
define constant $pango-attr-strikethrough = 12;
define constant $pango-attr-rise = 13;
define constant $pango-attr-shape = 14;
define constant $pango-attr-scale = 15;
define constant $pango-attr-fallback = 16;
define constant $pango-attr-letter-spacing = 17;
define constant $pango-attr-underline-color = 18;
define constant $pango-attr-strikethrough-color = 19;
define constant $pango-attr-absolute-size = 20;
define constant $pango-attr-gravity = 21;
define constant $pango-attr-gravity-hint = 22;
define constant <PangoAttrType> = <C-int>;
define C-pointer-type <PangoAttrType*> => <PangoAttrType>;

define C-struct <_PangoAttribute>
  slot pango-attribute-klass :: <PangoAttrClass>;
  slot pango-attribute-start-index :: <C-unsigned-int>;
  slot pango-attribute-end-index :: <C-unsigned-int>;
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

define constant $pango-bidi-type-l = 0;
define constant $pango-bidi-type-lre = 1;
define constant $pango-bidi-type-lro = 2;
define constant $pango-bidi-type-r = 3;
define constant $pango-bidi-type-al = 4;
define constant $pango-bidi-type-rle = 5;
define constant $pango-bidi-type-rlo = 6;
define constant $pango-bidi-type-pdf = 7;
define constant $pango-bidi-type-en = 8;
define constant $pango-bidi-type-es = 9;
define constant $pango-bidi-type-et = 10;
define constant $pango-bidi-type-an = 11;
define constant $pango-bidi-type-cs = 12;
define constant $pango-bidi-type-nsm = 13;
define constant $pango-bidi-type-bn = 14;
define constant $pango-bidi-type-b = 15;
define constant $pango-bidi-type-s = 16;
define constant $pango-bidi-type-ws = 17;
define constant $pango-bidi-type-on = 18;
define constant <PangoBidiType> = <C-int>;
define C-pointer-type <PangoBidiType*> => <PangoBidiType>;

define C-struct <_PangoColor>
  slot pango-color-red :: <C-unsigned-short>;
  slot pango-color-green :: <C-unsigned-short>;
  slot pango-color-blue :: <C-unsigned-short>;
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
  input parameter bytes_ :: <C-unsigned-char*>;
  input parameter n_bytes_ :: <C-signed-int*>;
  c-name: "pango_coverage_to_bytes";
end;

define C-function pango-coverage-unref
  input parameter self :: <PangoCoverage>;
  c-name: "pango_coverage_unref";
end;

define constant $pango-coverage-none = 0;
define constant $pango-coverage-fallback = 1;
define constant $pango-coverage-approximate = 2;
define constant $pango-coverage-exact = 3;
define constant <PangoCoverageLevel> = <C-int>;
define C-pointer-type <PangoCoverageLevel*> => <PangoCoverageLevel>;

define constant $pango-direction-ltr = 0;
define constant $pango-direction-rtl = 1;
define constant $pango-direction-ttb-ltr = 2;
define constant $pango-direction-ttb-rtl = 3;
define constant $pango-direction-weak-ltr = 4;
define constant $pango-direction-weak-rtl = 5;
define constant $pango-direction-neutral = 6;
define constant <PangoDirection> = <C-int>;
define C-pointer-type <PangoDirection*> => <PangoDirection>;

define constant $engine-type-lang = "PangoEngineLang";

define constant $engine-type-shape = "PangoEngineShape";

define constant $pango-ellipsize-none = 0;
define constant $pango-ellipsize-start = 1;
define constant $pango-ellipsize-middle = 2;
define constant $pango-ellipsize-end = 3;
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
  input parameter sizes_ :: <C-signed-int*>;
  input parameter n_sizes_ :: <C-signed-int*>;
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

define constant $pango-font-mask-family = 1;
define constant $pango-font-mask-style = 2;
define constant $pango-font-mask-variant = 4;
define constant $pango-font-mask-weight = 8;
define constant $pango-font-mask-stretch = 16;
define constant $pango-font-mask-size = 32;
define constant $pango-font-mask-gravity = 64;
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
  slot pango-glyph-geometry-width :: <C-signed-int>;
  slot pango-glyph-geometry-x-offset :: <C-signed-int>;
  slot pango-glyph-geometry-y-offset :: <C-signed-int>;
  pointer-type-name: <PangoGlyphGeometry>;
end C-struct;

define C-struct <_PangoGlyphInfo>
  slot pango-glyph-info-glyph :: <C-unsigned-int>;
  slot pango-glyph-info-geometry :: <PangoGlyphGeometry>;
  slot pango-glyph-info-attr :: <PangoGlyphVisAttr>;
  pointer-type-name: <PangoGlyphInfo>;
end C-struct;

define C-struct <_PangoGlyphItem>
  slot pango-glyph-item-item :: <PangoItem>;
  slot pango-glyph-item-glyphs :: <PangoGlyphString>;
  pointer-type-name: <PangoGlyphItem>;
end C-struct;

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
  input parameter log_attrs_ :: <PangoLogAttr>;
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
  slot pango-glyph-item-iter-glyph-item :: <PangoGlyphItem>;
  slot pango-glyph-item-iter-text :: <C-string>;
  slot pango-glyph-item-iter-start-glyph :: <C-signed-int>;
  slot pango-glyph-item-iter-start-index :: <C-signed-int>;
  slot pango-glyph-item-iter-start-char :: <C-signed-int>;
  slot pango-glyph-item-iter-end-glyph :: <C-signed-int>;
  slot pango-glyph-item-iter-end-index :: <C-signed-int>;
  slot pango-glyph-item-iter-end-char :: <C-signed-int>;
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
  slot pango-glyph-string-num-glyphs :: <C-signed-int>;
  slot pango-glyph-string-glyphs :: <PangoGlyphInfo>;
  slot pango-glyph-string-log-clusters :: <C-signed-int*>;
  constant slot pango-glyph-string-space :: <C-signed-int>;
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
  input parameter ink_rect_ :: <PangoRectangle>;
  input parameter logical_rect_ :: <PangoRectangle>;
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
  input parameter x_pos_ :: <C-signed-int*>;
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
  input parameter index__ :: <C-signed-int*>;
  input parameter trailing_ :: <C-signed-int*>;
  c-name: "pango_glyph_string_x_to_index";
end;

define C-struct <_PangoGlyphVisAttr>
  slot pango-glyph-vis-attr-is-cluster-start :: <C-unsigned-int>;
  pointer-type-name: <PangoGlyphVisAttr>;
end C-struct;

define constant $pango-gravity-south = 0;
define constant $pango-gravity-east = 1;
define constant $pango-gravity-north = 2;
define constant $pango-gravity-west = 3;
define constant $pango-gravity-auto = 4;
define constant <PangoGravity> = <C-int>;
define C-pointer-type <PangoGravity*> => <PangoGravity>;

define constant $pango-gravity-hint-natural = 0;
define constant $pango-gravity-hint-strong = 1;
define constant $pango-gravity-hint-line = 2;
define constant <PangoGravityHint> = <C-int>;
define C-pointer-type <PangoGravityHint*> => <PangoGravityHint>;

define C-struct <_PangoItem>
  slot pango-item-offset :: <C-signed-int>;
  slot pango-item-length :: <C-signed-int>;
  slot pango-item-num-chars :: <C-signed-int>;
  slot pango-item-analysis :: <PangoAnalysis>;
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
  result res :: <PangoScript>;
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
  input parameter n_attrs_ :: <C-signed-int*>;
  result res :: <PangoLogAttr>;
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
  input parameter new_trailing_ :: <C-signed-int*>;
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
  input parameter logical_rect_ :: <PangoRectangle>;
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
  slot pango-layout-line-layout :: <PangoLayout>;
  slot pango-layout-line-start-index :: <C-signed-int>;
  slot pango-layout-line-length :: <C-signed-int>;
  slot pango-layout-line-runs :: <GSList>;
  slot pango-layout-line-is-paragraph-start :: <C-unsigned-int>;
  slot pango-layout-line-resolved-dir :: <C-unsigned-int>;
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
  slot pango-log-attr-is-line-break :: <C-unsigned-int>;
  slot pango-log-attr-is-mandatory-break :: <C-unsigned-int>;
  slot pango-log-attr-is-char-break :: <C-unsigned-int>;
  slot pango-log-attr-is-white :: <C-unsigned-int>;
  slot pango-log-attr-is-cursor-position :: <C-unsigned-int>;
  slot pango-log-attr-is-word-start :: <C-unsigned-int>;
  slot pango-log-attr-is-word-end :: <C-unsigned-int>;
  slot pango-log-attr-is-sentence-boundary :: <C-unsigned-int>;
  slot pango-log-attr-is-sentence-start :: <C-unsigned-int>;
  slot pango-log-attr-is-sentence-end :: <C-unsigned-int>;
  slot pango-log-attr-backspace-deletes-character :: <C-unsigned-int>;
  slot pango-log-attr-is-expandable-space :: <C-unsigned-int>;
  slot pango-log-attr-is-word-boundary :: <C-unsigned-int>;
  pointer-type-name: <PangoLogAttr>;
end C-struct;

define C-struct <_PangoMatrix>
  slot pango-matrix-xx :: <C-double>;
  slot pango-matrix-xy :: <C-double>;
  slot pango-matrix-yx :: <C-double>;
  slot pango-matrix-yy :: <C-double>;
  slot pango-matrix-x0 :: <C-double>;
  slot pango-matrix-y0 :: <C-double>;
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

define constant $render-type-none = "PangoRenderNone";

define C-struct <_PangoRectangle>
  slot pango-rectangle-x :: <C-signed-int>;
  slot pango-rectangle-y :: <C-signed-int>;
  slot pango-rectangle-width :: <C-signed-int>;
  slot pango-rectangle-height :: <C-signed-int>;
  pointer-type-name: <PangoRectangle>;
end C-struct;

define constant $pango-render-part-foreground = 0;
define constant $pango-render-part-background = 1;
define constant $pango-render-part-underline = 2;
define constant $pango-render-part-strikethrough = 3;
define constant <PangoRenderPart> = <C-int>;
define C-pointer-type <PangoRenderPart*> => <PangoRenderPart>;

define open C-subtype <PangoRenderer> (<GObject>)
  constant slot pango-renderer-parent-instance :: <GObject>;
  constant slot pango-renderer-underline :: <PangoUnderline>;
  constant slot pango-renderer-strikethrough :: <C-boolean>;
  constant slot pango-renderer-active-count :: <C-signed-int>;
  constant slot pango-renderer-matrix :: <PangoMatrix>;
  constant slot pango-renderer-priv :: <PangoRendererPrivate>;
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
  constant slot pango-renderer-class-parent-class :: <GObjectClass>;
  constant slot pango-renderer-class-draw-glyphs :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-rectangle :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-error-underline :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-shape :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-trapezoid :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-glyph :: <C-function-pointer>;
  constant slot pango-renderer-class-part-changed :: <C-function-pointer>;
  constant slot pango-renderer-class-begin :: <C-function-pointer>;
  constant slot pango-renderer-class-end :: <C-function-pointer>;
  constant slot pango-renderer-class-prepare-run :: <C-function-pointer>;
  constant slot pango-renderer-class-draw-glyph-item :: <C-function-pointer>;
  constant slot pango-renderer-class-_pango-reserved2 :: <C-void*>;
  constant slot pango-renderer-class-_pango-reserved3 :: <C-void*>;
  constant slot pango-renderer-class-_pango-reserved4 :: <C-void*>;
  pointer-type-name: <PangoRendererClass>;
end C-struct;

define C-struct <_PangoRendererPrivate>
  pointer-type-name: <PangoRendererPrivate>;
end C-struct;

define constant $scale = 1024;

define constant $pango-script-invalid-code = -1;
define constant $pango-script-common = 0;
define constant $pango-script-inherited = 1;
define constant $pango-script-arabic = 2;
define constant $pango-script-armenian = 3;
define constant $pango-script-bengali = 4;
define constant $pango-script-bopomofo = 5;
define constant $pango-script-cherokee = 6;
define constant $pango-script-coptic = 7;
define constant $pango-script-cyrillic = 8;
define constant $pango-script-deseret = 9;
define constant $pango-script-devanagari = 10;
define constant $pango-script-ethiopic = 11;
define constant $pango-script-georgian = 12;
define constant $pango-script-gothic = 13;
define constant $pango-script-greek = 14;
define constant $pango-script-gujarati = 15;
define constant $pango-script-gurmukhi = 16;
define constant $pango-script-han = 17;
define constant $pango-script-hangul = 18;
define constant $pango-script-hebrew = 19;
define constant $pango-script-hiragana = 20;
define constant $pango-script-kannada = 21;
define constant $pango-script-katakana = 22;
define constant $pango-script-khmer = 23;
define constant $pango-script-lao = 24;
define constant $pango-script-latin = 25;
define constant $pango-script-malayalam = 26;
define constant $pango-script-mongolian = 27;
define constant $pango-script-myanmar = 28;
define constant $pango-script-ogham = 29;
define constant $pango-script-old-italic = 30;
define constant $pango-script-oriya = 31;
define constant $pango-script-runic = 32;
define constant $pango-script-sinhala = 33;
define constant $pango-script-syriac = 34;
define constant $pango-script-tamil = 35;
define constant $pango-script-telugu = 36;
define constant $pango-script-thaana = 37;
define constant $pango-script-thai = 38;
define constant $pango-script-tibetan = 39;
define constant $pango-script-canadian-aboriginal = 40;
define constant $pango-script-yi = 41;
define constant $pango-script-tagalog = 42;
define constant $pango-script-hanunoo = 43;
define constant $pango-script-buhid = 44;
define constant $pango-script-tagbanwa = 45;
define constant $pango-script-braille = 46;
define constant $pango-script-cypriot = 47;
define constant $pango-script-limbu = 48;
define constant $pango-script-osmanya = 49;
define constant $pango-script-shavian = 50;
define constant $pango-script-linear-b = 51;
define constant $pango-script-tai-le = 52;
define constant $pango-script-ugaritic = 53;
define constant $pango-script-new-tai-lue = 54;
define constant $pango-script-buginese = 55;
define constant $pango-script-glagolitic = 56;
define constant $pango-script-tifinagh = 57;
define constant $pango-script-syloti-nagri = 58;
define constant $pango-script-old-persian = 59;
define constant $pango-script-kharoshthi = 60;
define constant $pango-script-unknown = 61;
define constant $pango-script-balinese = 62;
define constant $pango-script-cuneiform = 63;
define constant $pango-script-phoenician = 64;
define constant $pango-script-phags-pa = 65;
define constant $pango-script-nko = 66;
define constant $pango-script-kayah-li = 67;
define constant $pango-script-lepcha = 68;
define constant $pango-script-rejang = 69;
define constant $pango-script-sundanese = 70;
define constant $pango-script-saurashtra = 71;
define constant $pango-script-cham = 72;
define constant $pango-script-ol-chiki = 73;
define constant $pango-script-vai = 74;
define constant $pango-script-carian = 75;
define constant $pango-script-lycian = 76;
define constant $pango-script-lydian = 77;
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

define constant $pango-stretch-ultra-condensed = 0;
define constant $pango-stretch-extra-condensed = 1;
define constant $pango-stretch-condensed = 2;
define constant $pango-stretch-semi-condensed = 3;
define constant $pango-stretch-normal = 4;
define constant $pango-stretch-semi-expanded = 5;
define constant $pango-stretch-expanded = 6;
define constant $pango-stretch-extra-expanded = 7;
define constant $pango-stretch-ultra-expanded = 8;
define constant <PangoStretch> = <C-int>;
define C-pointer-type <PangoStretch*> => <PangoStretch>;

define constant $pango-style-normal = 0;
define constant $pango-style-oblique = 1;
define constant $pango-style-italic = 2;
define constant <PangoStyle> = <C-int>;
define C-pointer-type <PangoStyle*> => <PangoStyle>;

define constant $pango-tab-left = 0;
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

define constant $unknown-glyph-height = 14;

define constant $unknown-glyph-width = 10;

define constant $pango-underline-none = 0;
define constant $pango-underline-single = 1;
define constant $pango-underline-double = 2;
define constant $pango-underline-low = 3;
define constant $pango-underline-error = 4;
define constant <PangoUnderline> = <C-int>;
define C-pointer-type <PangoUnderline*> => <PangoUnderline>;

define constant $pango-variant-normal = 0;
define constant $pango-variant-small-caps = 1;
define constant <PangoVariant> = <C-int>;
define C-pointer-type <PangoVariant*> => <PangoVariant>;

define constant $pango-weight-thin = 100;
define constant $pango-weight-ultralight = 200;
define constant $pango-weight-light = 300;
define constant $pango-weight-book = 380;
define constant $pango-weight-normal = 400;
define constant $pango-weight-medium = 500;
define constant $pango-weight-semibold = 600;
define constant $pango-weight-bold = 700;
define constant $pango-weight-ultrabold = 800;
define constant $pango-weight-heavy = 900;
define constant $pango-weight-ultraheavy = 1000;
define constant <PangoWeight> = <C-int>;
define C-pointer-type <PangoWeight*> => <PangoWeight>;

define constant $pango-wrap-word = 0;
define constant $pango-wrap-char = 1;
define constant $pango-wrap-word-char = 2;
define constant <PangoWrapMode> = <C-int>;
define C-pointer-type <PangoWrapMode*> => <PangoWrapMode>;

define C-struct <_Pango_ScriptForLang>
  slot pango-_script-for-lang-lang :: <C-unsigned-char*>;
  slot pango-_script-for-lang-scripts :: <C-unsigned-char*> /* Not supported */;
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
  input parameter attrs_ :: <PangoLogAttr>;
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
  input parameter paragraph_delimiter_index_ :: <C-signed-int*>;
  input parameter next_paragraph_start_ :: <C-signed-int*>;
  c-name: "pango_find_paragraph_boundary";
end;

define C-function pango-get-log-attrs
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter level_ :: <C-signed-int>;
  input parameter language_ :: <PangoLanguage>;
  input parameter log_attrs_ :: <PangoLogAttr>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "pango_parse_markup";
end;

define C-function pango-parse-stretch
  input parameter str_ :: <C-string>;
  input parameter stretch_ :: <PangoStretch>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_stretch";
end;

define C-function pango-parse-style
  input parameter str_ :: <C-string>;
  input parameter style_ :: <PangoStyle>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_style";
end;

define C-function pango-parse-variant
  input parameter str_ :: <C-string>;
  input parameter variant_ :: <PangoVariant>;
  input parameter warn_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "pango_parse_variant";
end;

define C-function pango-parse-weight
  input parameter str_ :: <C-string>;
  input parameter weight_ :: <PangoWeight>;
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

