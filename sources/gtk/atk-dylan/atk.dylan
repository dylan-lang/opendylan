module: atk
synopsis: generated bindings for the Atk library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

// Interface
define open C-subtype <AtkAction> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkAction*> => <AtkAction>;

define C-function atk-action-do-action
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_action_do_action";
end;

define C-function atk-action-get-description
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_action_get_description";
end;

define C-function atk-action-get-keybinding
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_action_get_keybinding";
end;

define C-function atk-action-get-localized-name
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_action_get_localized_name";
end;

define C-function atk-action-get-n-actions
  input parameter self :: <AtkAction>;
  result res :: <C-signed-int>;
  c-name: "atk_action_get_n_actions";
end;

define C-function atk-action-get-name
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_action_get_name";
end;

define C-function atk-action-set-description
  input parameter self :: <AtkAction>;
  input parameter i_ :: <C-signed-int>;
  input parameter desc_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "atk_action_set_description";
end;

define C-struct <_AtkActionIface>
  constant slot atkactioniface-parent :: <GTypeInterface>;
  constant slot atkactioniface-do-action :: <C-function-pointer>;
  constant slot atkactioniface-get-n-actions :: <C-function-pointer>;
  constant slot atkactioniface-get-description :: <C-function-pointer>;
  constant slot atkactioniface-get-name :: <C-function-pointer>;
  constant slot atkactioniface-get-keybinding :: <C-function-pointer>;
  constant slot atkactioniface-set-description :: <C-function-pointer>;
  constant slot atkactioniface-get-localized-name :: <C-function-pointer>;
  constant slot atkactioniface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkActionIface>;
end C-struct;

define C-struct <_AtkAttribute>
  slot atkattribute-name :: <C-string>;
  slot atkattribute-value :: <C-string>;
  pointer-type-name: <AtkAttribute>;
end C-struct;

define C-function atk-attribute-set-free
  input parameter attrib_set_ :: <GSList>;
  c-name: "atk_attribute_set_free";
end;

define constant $BINARY-AGE = 20810;

// Interface
define open C-subtype <AtkComponent> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkComponent*> => <AtkComponent>;

define C-function atk-component-contains
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter coord_type_ :: <AtkCoordType>;
  result res :: <C-boolean>;
  c-name: "atk_component_contains";
end;

define C-function atk-component-get-alpha
  input parameter self :: <AtkComponent>;
  result res :: <C-double>;
  c-name: "atk_component_get_alpha";
end;

define C-function atk-component-get-extents
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int*>;
  input parameter y_ :: <C-signed-int*>;
  input parameter width_ :: <C-signed-int*>;
  input parameter height_ :: <C-signed-int*>;
  input parameter coord_type_ :: <AtkCoordType>;
  c-name: "atk_component_get_extents";
end;

define C-function atk-component-get-layer
  input parameter self :: <AtkComponent>;
  result res :: <AtkLayer>;
  c-name: "atk_component_get_layer";
end;

define C-function atk-component-get-mdi-zorder
  input parameter self :: <AtkComponent>;
  result res :: <C-signed-int>;
  c-name: "atk_component_get_mdi_zorder";
end;

define C-function atk-component-get-position
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int*>;
  input parameter y_ :: <C-signed-int*>;
  input parameter coord_type_ :: <AtkCoordType>;
  c-name: "atk_component_get_position";
end;

define C-function atk-component-get-size
  input parameter self :: <AtkComponent>;
  input parameter width_ :: <C-signed-int*>;
  input parameter height_ :: <C-signed-int*>;
  c-name: "atk_component_get_size";
end;

define C-function atk-component-grab-focus
  input parameter self :: <AtkComponent>;
  result res :: <C-boolean>;
  c-name: "atk_component_grab_focus";
end;

define C-function atk-component-ref-accessible-at-point
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter coord_type_ :: <AtkCoordType>;
  result res :: <AtkObject>;
  c-name: "atk_component_ref_accessible_at_point";
end;

define C-function atk-component-remove-focus-handler
  input parameter self :: <AtkComponent>;
  input parameter handler_id_ :: <C-unsigned-int>;
  c-name: "atk_component_remove_focus_handler";
end;

define C-function atk-component-set-extents
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter coord_type_ :: <AtkCoordType>;
  result res :: <C-boolean>;
  c-name: "atk_component_set_extents";
end;

define C-function atk-component-set-position
  input parameter self :: <AtkComponent>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter coord_type_ :: <AtkCoordType>;
  result res :: <C-boolean>;
  c-name: "atk_component_set_position";
end;

define C-function atk-component-set-size
  input parameter self :: <AtkComponent>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_component_set_size";
end;

define C-struct <_AtkComponentIface>
  constant slot atkcomponentiface-parent :: <GTypeInterface>;
  constant slot atkcomponentiface-add-focus-handler :: <C-void*>;
  constant slot atkcomponentiface-contains :: <C-function-pointer>;
  constant slot atkcomponentiface-ref-accessible-at-point :: <C-function-pointer>;
  constant slot atkcomponentiface-get-extents :: <C-function-pointer>;
  constant slot atkcomponentiface-get-position :: <C-function-pointer>;
  constant slot atkcomponentiface-get-size :: <C-function-pointer>;
  constant slot atkcomponentiface-grab-focus :: <C-function-pointer>;
  constant slot atkcomponentiface-remove-focus-handler :: <C-function-pointer>;
  constant slot atkcomponentiface-set-extents :: <C-function-pointer>;
  constant slot atkcomponentiface-set-position :: <C-function-pointer>;
  constant slot atkcomponentiface-set-size :: <C-function-pointer>;
  constant slot atkcomponentiface-get-layer :: <C-function-pointer>;
  constant slot atkcomponentiface-get-mdi-zorder :: <C-function-pointer>;
  constant slot atkcomponentiface-bounds-changed :: <C-function-pointer>;
  constant slot atkcomponentiface-get-alpha :: <C-function-pointer>;
  pointer-type-name: <AtkComponentIface>;
end C-struct;

define constant $ATK-XY-SCREEN = 0;
define constant $ATK-XY-WINDOW = 1;
define constant <AtkCoordType> = <C-int>;
define C-pointer-type <AtkCoordType*> => <AtkCoordType>;

// Interface
define open C-subtype <AtkDocument> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkDocument*> => <AtkDocument>;

define C-function atk-document-get-attribute-value
  input parameter self :: <AtkDocument>;
  input parameter attribute_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "atk_document_get_attribute_value";
end;

define C-function atk-document-get-attributes
  input parameter self :: <AtkDocument>;
  result res :: <GSList>;
  c-name: "atk_document_get_attributes";
end;

define C-function atk-document-get-document
  input parameter self :: <AtkDocument>;
  result res :: <C-void*>;
  c-name: "atk_document_get_document";
end;

define C-function atk-document-get-document-type
  input parameter self :: <AtkDocument>;
  result res :: <C-string>;
  c-name: "atk_document_get_document_type";
end;

define C-function atk-document-set-attribute-value
  input parameter self :: <AtkDocument>;
  input parameter attribute_name_ :: <C-string>;
  input parameter attribute_value_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "atk_document_set_attribute_value";
end;

define C-struct <_AtkDocumentIface>
  constant slot atkdocumentiface-parent :: <GTypeInterface>;
  constant slot atkdocumentiface-get-document-type :: <C-function-pointer>;
  constant slot atkdocumentiface-get-document :: <C-function-pointer>;
  constant slot atkdocumentiface-get-document-locale :: <C-function-pointer>;
  constant slot atkdocumentiface-get-document-attributes :: <C-function-pointer>;
  constant slot atkdocumentiface-get-document-attribute-value :: <C-function-pointer>;
  constant slot atkdocumentiface-set-document-attribute :: <C-function-pointer>;
  constant slot atkdocumentiface-pad1 :: <C-function-pointer>;
  constant slot atkdocumentiface-pad2 :: <C-function-pointer>;
  constant slot atkdocumentiface-pad3 :: <C-function-pointer>;
  constant slot atkdocumentiface-pad4 :: <C-function-pointer>;
  pointer-type-name: <AtkDocumentIface>;
end C-struct;

// Interface
define open C-subtype <AtkEditableText> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkEditableText*> => <AtkEditableText>;

define C-function atk-editable-text-copy-text
  input parameter self :: <AtkEditableText>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  c-name: "atk_editable_text_copy_text";
end;

define C-function atk-editable-text-cut-text
  input parameter self :: <AtkEditableText>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  c-name: "atk_editable_text_cut_text";
end;

define C-function atk-editable-text-delete-text
  input parameter self :: <AtkEditableText>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  c-name: "atk_editable_text_delete_text";
end;

define C-function atk-editable-text-insert-text
  input parameter self :: <AtkEditableText>;
  input parameter string_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  input parameter position_ :: <C-signed-int*>;
  c-name: "atk_editable_text_insert_text";
end;

define C-function atk-editable-text-paste-text
  input parameter self :: <AtkEditableText>;
  input parameter position_ :: <C-signed-int>;
  c-name: "atk_editable_text_paste_text";
end;

define C-function atk-editable-text-set-run-attributes
  input parameter self :: <AtkEditableText>;
  input parameter attrib_set_ :: <GSList>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_editable_text_set_run_attributes";
end;

define C-function atk-editable-text-set-text-contents
  input parameter self :: <AtkEditableText>;
  input parameter string_ :: <C-string>;
  c-name: "atk_editable_text_set_text_contents";
end;

define C-struct <_AtkEditableTextIface>
  constant slot atkeditabletextiface-parent-interface :: <GTypeInterface>;
  constant slot atkeditabletextiface-set-run-attributes :: <C-function-pointer>;
  constant slot atkeditabletextiface-set-text-contents :: <C-function-pointer>;
  constant slot atkeditabletextiface-insert-text :: <C-function-pointer>;
  constant slot atkeditabletextiface-copy-text :: <C-function-pointer>;
  constant slot atkeditabletextiface-cut-text :: <C-function-pointer>;
  constant slot atkeditabletextiface-delete-text :: <C-function-pointer>;
  constant slot atkeditabletextiface-paste-text :: <C-function-pointer>;
  constant slot atkeditabletextiface-pad1 :: <C-function-pointer>;
  constant slot atkeditabletextiface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkEditableTextIface>;
end C-struct;

define open C-subtype <AtkGObjectAccessible> (<AtkObject>)
  constant slot atkgobjectaccessible-parent :: <AtkObject>;
end C-subtype;

define C-pointer-type <AtkGObjectAccessible*> => <AtkGObjectAccessible>;

define C-function atk-gobject-accessible-for-object
  input parameter obj_ :: <GObject>;
  result res :: <AtkObject>;
  c-name: "atk_gobject_accessible_for_object";
end;

define C-function atk-gobject-accessible-get-object
  input parameter self :: <AtkGObjectAccessible>;
  result res :: <GObject>;
  c-name: "atk_gobject_accessible_get_object";
end;

define C-struct <_AtkGObjectAccessibleClass>
  constant slot atkgobjectaccessibleclass-parent-class :: <AtkObjectClass>;
  constant slot atkgobjectaccessibleclass-pad1 :: <C-function-pointer>;
  constant slot atkgobjectaccessibleclass-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkGObjectAccessibleClass>;
end C-struct;

define open C-subtype <AtkHyperlink> (<GObject>)
  constant slot atkhyperlink-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkHyperlink*> => <AtkHyperlink>;

define property-getter hyperlink-end-index :: <C-signed-int> on <AtkHyperlink> end;
define property-getter hyperlink-number-of-anchors :: <C-signed-int> on <AtkHyperlink> end;
define property-getter hyperlink-selected-link :: <C-boolean> on <AtkHyperlink> end;
define property-getter hyperlink-start-index :: <C-signed-int> on <AtkHyperlink> end;
define C-function atk-hyperlink-get-end-index
  input parameter self :: <AtkHyperlink>;
  result res :: <C-signed-int>;
  c-name: "atk_hyperlink_get_end_index";
end;

define C-function atk-hyperlink-get-n-anchors
  input parameter self :: <AtkHyperlink>;
  result res :: <C-signed-int>;
  c-name: "atk_hyperlink_get_n_anchors";
end;

define C-function atk-hyperlink-get-object
  input parameter self :: <AtkHyperlink>;
  input parameter i_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_hyperlink_get_object";
end;

define C-function atk-hyperlink-get-start-index
  input parameter self :: <AtkHyperlink>;
  result res :: <C-signed-int>;
  c-name: "atk_hyperlink_get_start_index";
end;

define C-function atk-hyperlink-get-uri
  input parameter self :: <AtkHyperlink>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_hyperlink_get_uri";
end;

define C-function atk-hyperlink-is-inline
  input parameter self :: <AtkHyperlink>;
  result res :: <C-boolean>;
  c-name: "atk_hyperlink_is_inline";
end;

define C-function atk-hyperlink-is-valid
  input parameter self :: <AtkHyperlink>;
  result res :: <C-boolean>;
  c-name: "atk_hyperlink_is_valid";
end;

define C-struct <_AtkHyperlinkClass>
  constant slot atkhyperlinkclass-parent :: <GObjectClass>;
  constant slot atkhyperlinkclass-get-uri :: <C-function-pointer>;
  constant slot atkhyperlinkclass-get-object :: <C-function-pointer>;
  constant slot atkhyperlinkclass-get-end-index :: <C-function-pointer>;
  constant slot atkhyperlinkclass-get-start-index :: <C-function-pointer>;
  constant slot atkhyperlinkclass-is-valid :: <C-function-pointer>;
  constant slot atkhyperlinkclass-get-n-anchors :: <C-function-pointer>;
  constant slot atkhyperlinkclass-link-state :: <C-function-pointer>;
  constant slot atkhyperlinkclass-is-selected-link :: <C-function-pointer>;
  constant slot atkhyperlinkclass-link-activated :: <C-function-pointer>;
  constant slot atkhyperlinkclass-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkHyperlinkClass>;
end C-struct;

// Interface
define open C-subtype <AtkHyperlinkImpl> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkHyperlinkImpl*> => <AtkHyperlinkImpl>;

define C-function atk-hyperlink-impl-get-hyperlink
  input parameter self :: <AtkHyperlinkImpl>;
  result res :: <AtkHyperlink>;
  c-name: "atk_hyperlink_impl_get_hyperlink";
end;

define C-struct <_AtkHyperlinkImplIface>
  constant slot atkhyperlinkimpliface-parent :: <GTypeInterface>;
  constant slot atkhyperlinkimpliface-get-hyperlink :: <C-function-pointer>;
  constant slot atkhyperlinkimpliface-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkHyperlinkImplIface>;
end C-struct;

define constant $ATK-HYPERLINK-IS-INLINE = 1;
define constant <AtkHyperlinkStateFlags> = <C-int>;
define C-pointer-type <AtkHyperlinkStateFlags*> => <AtkHyperlinkStateFlags>;

// Interface
define open C-subtype <AtkHypertext> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkHypertext*> => <AtkHypertext>;

define C-function atk-hypertext-get-link
  input parameter self :: <AtkHypertext>;
  input parameter link_index_ :: <C-signed-int>;
  result res :: <AtkHyperlink>;
  c-name: "atk_hypertext_get_link";
end;

define C-function atk-hypertext-get-link-index
  input parameter self :: <AtkHypertext>;
  input parameter char_index_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_hypertext_get_link_index";
end;

define C-function atk-hypertext-get-n-links
  input parameter self :: <AtkHypertext>;
  result res :: <C-signed-int>;
  c-name: "atk_hypertext_get_n_links";
end;

define C-struct <_AtkHypertextIface>
  constant slot atkhypertextiface-parent :: <GTypeInterface>;
  constant slot atkhypertextiface-get-link :: <C-function-pointer>;
  constant slot atkhypertextiface-get-n-links :: <C-function-pointer>;
  constant slot atkhypertextiface-get-link-index :: <C-function-pointer>;
  constant slot atkhypertextiface-link-selected :: <C-function-pointer>;
  constant slot atkhypertextiface-pad1 :: <C-function-pointer>;
  constant slot atkhypertextiface-pad2 :: <C-function-pointer>;
  constant slot atkhypertextiface-pad3 :: <C-function-pointer>;
  pointer-type-name: <AtkHypertextIface>;
end C-struct;

define constant $INTERFACE-AGE = 1;

// Interface
define open C-subtype <AtkImage> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkImage*> => <AtkImage>;

define C-function atk-image-get-image-description
  input parameter self :: <AtkImage>;
  result res :: <C-string>;
  c-name: "atk_image_get_image_description";
end;

define C-function atk-image-get-image-locale
  input parameter self :: <AtkImage>;
  result res :: <C-string>;
  c-name: "atk_image_get_image_locale";
end;

define C-function atk-image-get-image-position
  input parameter self :: <AtkImage>;
  input parameter x_ :: <C-signed-int*>;
  input parameter y_ :: <C-signed-int*>;
  input parameter coord_type_ :: <AtkCoordType>;
  c-name: "atk_image_get_image_position";
end;

define C-function atk-image-get-image-size
  input parameter self :: <AtkImage>;
  input parameter width_ :: <C-signed-int*>;
  input parameter height_ :: <C-signed-int*>;
  c-name: "atk_image_get_image_size";
end;

define C-function atk-image-set-image-description
  input parameter self :: <AtkImage>;
  input parameter description_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "atk_image_set_image_description";
end;

define C-struct <_AtkImageIface>
  constant slot atkimageiface-parent :: <GTypeInterface>;
  constant slot atkimageiface-get-image-position :: <C-function-pointer>;
  constant slot atkimageiface-get-image-description :: <C-function-pointer>;
  constant slot atkimageiface-get-image-size :: <C-function-pointer>;
  constant slot atkimageiface-set-image-description :: <C-function-pointer>;
  constant slot atkimageiface-get-image-locale :: <C-function-pointer>;
  constant slot atkimageiface-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkImageIface>;
end C-struct;

define C-struct <_AtkImplementor>
  pointer-type-name: <AtkImplementor>;
end C-struct;

define C-function atk-implementor-ref-accessible
  input parameter self :: <AtkImplementor>;
  result res :: <AtkObject>;
  c-name: "atk_implementor_ref_accessible";
end;

// Interface
define open C-subtype <AtkImplementorIface> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkImplementorIface*> => <AtkImplementorIface>;

define C-struct <_AtkKeyEventStruct>
  slot atkkeyeventstruct-type :: <C-signed-int>;
  slot atkkeyeventstruct-state :: <C-unsigned-int>;
  slot atkkeyeventstruct-keyval :: <C-unsigned-int>;
  slot atkkeyeventstruct-length :: <C-signed-int>;
  slot atkkeyeventstruct-string :: <C-string>;
  slot atkkeyeventstruct-keycode :: <C-unsigned-short>;
  slot atkkeyeventstruct-timestamp :: <C-unsigned-int>;
  pointer-type-name: <AtkKeyEventStruct>;
end C-struct;

define constant $ATK-KEY-EVENT-PRESS = 0;
define constant $ATK-KEY-EVENT-RELEASE = 1;
define constant $ATK-KEY-EVENT-LAST-DEFINED = 2;
define constant <AtkKeyEventType> = <C-int>;
define C-pointer-type <AtkKeyEventType*> => <AtkKeyEventType>;

define constant $ATK-LAYER-INVALID = 0;
define constant $ATK-LAYER-BACKGROUND = 1;
define constant $ATK-LAYER-CANVAS = 2;
define constant $ATK-LAYER-WIDGET = 3;
define constant $ATK-LAYER-MDI = 4;
define constant $ATK-LAYER-POPUP = 5;
define constant $ATK-LAYER-OVERLAY = 6;
define constant $ATK-LAYER-WINDOW = 7;
define constant <AtkLayer> = <C-int>;
define C-pointer-type <AtkLayer*> => <AtkLayer>;

define constant $MAJOR-VERSION = 2;

define constant $MICRO-VERSION = 0;

define constant $MINOR-VERSION = 8;

define open C-subtype <AtkMisc> (<GObject>)
  constant slot atkmisc-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkMisc*> => <AtkMisc>;

define C-function atk-misc-get-instance
  result res :: <AtkMisc>;
  c-name: "atk_misc_get_instance";
end;

define C-function atk-misc-threads-enter
  input parameter self :: <AtkMisc>;
  c-name: "atk_misc_threads_enter";
end;

define C-function atk-misc-threads-leave
  input parameter self :: <AtkMisc>;
  c-name: "atk_misc_threads_leave";
end;

define C-struct <_AtkMiscClass>
  constant slot atkmiscclass-parent :: <GObjectClass>;
  constant slot atkmiscclass-threads-enter :: <C-function-pointer>;
  constant slot atkmiscclass-threads-leave :: <C-function-pointer>;
  constant slot atkmiscclass-vfuncs :: <C-void*>;
  pointer-type-name: <AtkMiscClass>;
end C-struct;

define open C-subtype <AtkNoOpObject> (<AtkObject>)
  constant slot atknoopobject-parent :: <AtkObject>;
end C-subtype;

define C-pointer-type <AtkNoOpObject*> => <AtkNoOpObject>;

define C-function atk-no-op-object-new
  input parameter obj_ :: <GObject>;
  result res :: <AtkObject>;
  c-name: "atk_no_op_object_new";
end;

define C-struct <_AtkNoOpObjectClass>
  constant slot atknoopobjectclass-parent-class :: <AtkObjectClass>;
  pointer-type-name: <AtkNoOpObjectClass>;
end C-struct;

define open C-subtype <AtkNoOpObjectFactory> (<AtkObjectFactory>)
  constant slot atknoopobjectfactory-parent :: <AtkObjectFactory>;
end C-subtype;

define C-pointer-type <AtkNoOpObjectFactory*> => <AtkNoOpObjectFactory>;

define C-function atk-no-op-object-factory-new
  result res :: <AtkObjectFactory>;
  c-name: "atk_no_op_object_factory_new";
end;

define C-struct <_AtkNoOpObjectFactoryClass>
  constant slot atknoopobjectfactoryclass-parent-class :: <AtkObjectFactoryClass>;
  pointer-type-name: <AtkNoOpObjectFactoryClass>;
end C-struct;

define open C-subtype <AtkObject> (<GObject>)
  constant slot atkobject-parent :: <GObject>;
  constant slot atkobject-description :: <C-string>;
  constant slot atkobject-name :: <C-string>;
  constant slot atkobject-accessible-parent :: <AtkObject>;
  constant slot atkobject-role :: <AtkRole>;
  constant slot atkobject-relation-set :: <AtkRelationSet>;
  constant slot atkobject-layer :: <AtkLayer>;
end C-subtype;

define C-pointer-type <AtkObject*> => <AtkObject>;

define property-getter object-accessible-component-layer :: <C-signed-int> on <AtkObject> end;
define property-getter object-accessible-component-mdi-zorder :: <C-signed-int> on <AtkObject> end;
define property-getter object-accessible-description :: <C-string> on <AtkObject> end;
define property-setter object-accessible-description :: <C-string> on <AtkObject> end;
define property-getter object-accessible-hypertext-nlinks :: <C-signed-int> on <AtkObject> end;
define property-getter object-accessible-name :: <C-string> on <AtkObject> end;
define property-setter object-accessible-name :: <C-string> on <AtkObject> end;
define property-getter object-accessible-parent :: <AtkObject> on <AtkObject> end;
define property-setter object-accessible-parent :: <AtkObject> on <AtkObject> end;
define property-getter object-accessible-role :: <C-signed-int> on <AtkObject> end;
define property-setter object-accessible-role :: <C-signed-int> on <AtkObject> end;
define property-getter object-accessible-table-caption :: <C-string> on <AtkObject> end;
define property-setter object-accessible-table-caption :: <C-string> on <AtkObject> end;
define property-getter object-accessible-table-caption-object :: <AtkObject> on <AtkObject> end;
define property-setter object-accessible-table-caption-object :: <AtkObject> on <AtkObject> end;
define property-getter object-accessible-table-column-description :: <C-string> on <AtkObject> end;
define property-setter object-accessible-table-column-description :: <C-string> on <AtkObject> end;
define property-getter object-accessible-table-column-header :: <AtkObject> on <AtkObject> end;
define property-setter object-accessible-table-column-header :: <AtkObject> on <AtkObject> end;
define property-getter object-accessible-table-row-description :: <C-string> on <AtkObject> end;
define property-setter object-accessible-table-row-description :: <C-string> on <AtkObject> end;
define property-getter object-accessible-table-row-header :: <AtkObject> on <AtkObject> end;
define property-setter object-accessible-table-row-header :: <AtkObject> on <AtkObject> end;
define property-getter object-accessible-table-summary :: <AtkObject> on <AtkObject> end;
define property-setter object-accessible-table-summary :: <AtkObject> on <AtkObject> end;
define property-getter object-accessible-value :: <C-double> on <AtkObject> end;
define property-setter object-accessible-value :: <C-double> on <AtkObject> end;
define C-function atk-object-add-relationship
  input parameter self :: <AtkObject>;
  input parameter relationship_ :: <AtkRelationType>;
  input parameter target_ :: <AtkObject>;
  result res :: <C-boolean>;
  c-name: "atk_object_add_relationship";
end;

define C-function atk-object-get-attributes
  input parameter self :: <AtkObject>;
  result res :: <GSList>;
  c-name: "atk_object_get_attributes";
end;

define C-function atk-object-get-description
  input parameter self :: <AtkObject>;
  result res :: <C-string>;
  c-name: "atk_object_get_description";
end;

define C-function atk-object-get-index-in-parent
  input parameter self :: <AtkObject>;
  result res :: <C-signed-int>;
  c-name: "atk_object_get_index_in_parent";
end;

define C-function atk-object-get-n-accessible-children
  input parameter self :: <AtkObject>;
  result res :: <C-signed-int>;
  c-name: "atk_object_get_n_accessible_children";
end;

define C-function atk-object-get-name
  input parameter self :: <AtkObject>;
  result res :: <C-string>;
  c-name: "atk_object_get_name";
end;

define C-function atk-object-get-object-locale
  input parameter self :: <AtkObject>;
  result res :: <C-string>;
  c-name: "atk_object_get_object_locale";
end;

define C-function atk-object-get-parent
  input parameter self :: <AtkObject>;
  result res :: <AtkObject>;
  c-name: "atk_object_get_parent";
end;

define C-function atk-object-get-role
  input parameter self :: <AtkObject>;
  result res :: <AtkRole>;
  c-name: "atk_object_get_role";
end;

define C-function atk-object-initialize
  input parameter self :: <AtkObject>;
  input parameter data_ :: <C-void*>;
  c-name: "atk_object_initialize";
end;

define C-function atk-object-notify-state-change
  input parameter self :: <AtkObject>;
  input parameter state_ :: <C-unsigned-long>;
  input parameter value_ :: <C-boolean>;
  c-name: "atk_object_notify_state_change";
end;

define C-function atk-object-ref-accessible-child
  input parameter self :: <AtkObject>;
  input parameter i_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_object_ref_accessible_child";
end;

define C-function atk-object-ref-relation-set
  input parameter self :: <AtkObject>;
  result res :: <AtkRelationSet>;
  c-name: "atk_object_ref_relation_set";
end;

define C-function atk-object-ref-state-set
  input parameter self :: <AtkObject>;
  result res :: <AtkStateSet>;
  c-name: "atk_object_ref_state_set";
end;

define C-function atk-object-remove-property-change-handler
  input parameter self :: <AtkObject>;
  input parameter handler_id_ :: <C-unsigned-int>;
  c-name: "atk_object_remove_property_change_handler";
end;

define C-function atk-object-remove-relationship
  input parameter self :: <AtkObject>;
  input parameter relationship_ :: <AtkRelationType>;
  input parameter target_ :: <AtkObject>;
  result res :: <C-boolean>;
  c-name: "atk_object_remove_relationship";
end;

define C-function atk-object-set-description
  input parameter self :: <AtkObject>;
  input parameter description_ :: <C-string>;
  c-name: "atk_object_set_description";
end;

define C-function atk-object-set-name
  input parameter self :: <AtkObject>;
  input parameter name_ :: <C-string>;
  c-name: "atk_object_set_name";
end;

define C-function atk-object-set-parent
  input parameter self :: <AtkObject>;
  input parameter parent_ :: <AtkObject>;
  c-name: "atk_object_set_parent";
end;

define C-function atk-object-set-role
  input parameter self :: <AtkObject>;
  input parameter role_ :: <AtkRole>;
  c-name: "atk_object_set_role";
end;

define C-struct <_AtkObjectClass>
  constant slot atkobjectclass-parent :: <GObjectClass>;
  constant slot atkobjectclass-get-name :: <C-function-pointer>;
  constant slot atkobjectclass-get-description :: <C-function-pointer>;
  constant slot atkobjectclass-get-parent :: <C-function-pointer>;
  constant slot atkobjectclass-get-n-children :: <C-function-pointer>;
  constant slot atkobjectclass-ref-child :: <C-void*>;
  constant slot atkobjectclass-get-index-in-parent :: <C-function-pointer>;
  constant slot atkobjectclass-ref-relation-set :: <C-function-pointer>;
  constant slot atkobjectclass-get-role :: <C-function-pointer>;
  constant slot atkobjectclass-get-layer :: <C-function-pointer>;
  constant slot atkobjectclass-get-mdi-zorder :: <C-function-pointer>;
  constant slot atkobjectclass-ref-state-set :: <C-function-pointer>;
  constant slot atkobjectclass-set-name :: <C-function-pointer>;
  constant slot atkobjectclass-set-description :: <C-function-pointer>;
  constant slot atkobjectclass-set-parent :: <C-function-pointer>;
  constant slot atkobjectclass-set-role :: <C-function-pointer>;
  constant slot atkobjectclass-connect-property-change-handler :: <C-void*>;
  constant slot atkobjectclass-remove-property-change-handler :: <C-function-pointer>;
  constant slot atkobjectclass-initialize :: <C-function-pointer>;
  constant slot atkobjectclass-children-changed :: <C-function-pointer>;
  constant slot atkobjectclass-focus-event :: <C-function-pointer>;
  constant slot atkobjectclass-property-change :: <C-void*>;
  constant slot atkobjectclass-state-change :: <C-function-pointer>;
  constant slot atkobjectclass-visible-data-changed :: <C-function-pointer>;
  constant slot atkobjectclass-active-descendant-changed :: <C-function-pointer>;
  constant slot atkobjectclass-get-attributes :: <C-function-pointer>;
  constant slot atkobjectclass-get-object-locale :: <C-function-pointer>;
  constant slot atkobjectclass-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkObjectClass>;
end C-struct;

define open C-subtype <AtkObjectFactory> (<GObject>)
  constant slot atkobjectfactory-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkObjectFactory*> => <AtkObjectFactory>;

define C-function atk-object-factory-create-accessible
  input parameter self :: <AtkObjectFactory>;
  input parameter obj_ :: <GObject>;
  result res :: <AtkObject>;
  c-name: "atk_object_factory_create_accessible";
end;

define C-function atk-object-factory-get-accessible-type
  input parameter self :: <AtkObjectFactory>;
  result res :: <C-long>;
  c-name: "atk_object_factory_get_accessible_type";
end;

define C-function atk-object-factory-invalidate
  input parameter self :: <AtkObjectFactory>;
  c-name: "atk_object_factory_invalidate";
end;

define C-struct <_AtkObjectFactoryClass>
  constant slot atkobjectfactoryclass-parent-class :: <GObjectClass>;
  constant slot atkobjectfactoryclass-create-accessible :: <C-void*>;
  constant slot atkobjectfactoryclass-invalidate :: <C-function-pointer>;
  constant slot atkobjectfactoryclass-get-accessible-type :: <C-function-pointer>;
  constant slot atkobjectfactoryclass-pad1 :: <C-function-pointer>;
  constant slot atkobjectfactoryclass-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkObjectFactoryClass>;
end C-struct;

define open C-subtype <AtkPlug> (<AtkObject>)
  constant slot atkplug-parent :: <AtkObject>;
end C-subtype;

define C-pointer-type <AtkPlug*> => <AtkPlug>;

define C-function atk-plug-new
  result res :: <AtkObject>;
  c-name: "atk_plug_new";
end;

define C-function atk-plug-get-id
  input parameter self :: <AtkPlug>;
  result res :: <C-string>;
  c-name: "atk_plug_get_id";
end;

define C-struct <_AtkPlugClass>
  constant slot atkplugclass-parent-class :: <AtkObjectClass>;
  constant slot atkplugclass-get-object-id :: <C-function-pointer>;
  pointer-type-name: <AtkPlugClass>;
end C-struct;

define C-struct <_AtkRectangle>
  slot atkrectangle-x :: <C-signed-int>;
  slot atkrectangle-y :: <C-signed-int>;
  slot atkrectangle-width :: <C-signed-int>;
  slot atkrectangle-height :: <C-signed-int>;
  pointer-type-name: <AtkRectangle>;
end C-struct;

define open C-subtype <AtkRegistry> (<GObject>)
end C-subtype;

define C-pointer-type <AtkRegistry*> => <AtkRegistry>;

define C-function atk-registry-get-factory
  input parameter self :: <AtkRegistry>;
  input parameter type_ :: <C-long>;
  result res :: <AtkObjectFactory>;
  c-name: "atk_registry_get_factory";
end;

define C-function atk-registry-get-factory-type
  input parameter self :: <AtkRegistry>;
  input parameter type_ :: <C-long>;
  result res :: <C-long>;
  c-name: "atk_registry_get_factory_type";
end;

define C-function atk-registry-set-factory-type
  input parameter self :: <AtkRegistry>;
  input parameter type_ :: <C-long>;
  input parameter factory_type_ :: <C-long>;
  c-name: "atk_registry_set_factory_type";
end;

define open C-subtype <AtkRelation> (<GObject>)
  constant slot atkrelation-parent :: <GObject>;
  constant slot atkrelation-target :: <GPtrArray>;
  constant slot atkrelation-relationship :: <AtkRelationType>;
end C-subtype;

define C-pointer-type <AtkRelation*> => <AtkRelation>;

define property-getter relation-relation-type :: <AtkRelationType> on <AtkRelation> end;
define property-setter relation-relation-type :: <AtkRelationType> on <AtkRelation> end;
define property-getter relation-target :: <GValueArray> on <AtkRelation> end;
define property-setter relation-target :: <GValueArray> on <AtkRelation> end;
define C-function atk-relation-new
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter relationship_ :: <AtkRelationType>;
  result res :: <AtkRelation>;
  c-name: "atk_relation_new";
end;

define C-function atk-relation-add-target
  input parameter self :: <AtkRelation>;
  input parameter target_ :: <AtkObject>;
  c-name: "atk_relation_add_target";
end;

define C-function atk-relation-get-relation-type
  input parameter self :: <AtkRelation>;
  result res :: <AtkRelationType>;
  c-name: "atk_relation_get_relation_type";
end;

define C-function atk-relation-get-target
  input parameter self :: <AtkRelation>;
  result res :: <GPtrArray>;
  c-name: "atk_relation_get_target";
end;

define C-function atk-relation-remove-target
  input parameter self :: <AtkRelation>;
  input parameter target_ :: <AtkObject>;
  result res :: <C-boolean>;
  c-name: "atk_relation_remove_target";
end;

define C-struct <_AtkRelationClass>
  constant slot atkrelationclass-parent :: <GObjectClass>;
  pointer-type-name: <AtkRelationClass>;
end C-struct;

define open C-subtype <AtkRelationSet> (<GObject>)
  constant slot atkrelationset-parent :: <GObject>;
  constant slot atkrelationset-relations :: <GPtrArray>;
end C-subtype;

define C-pointer-type <AtkRelationSet*> => <AtkRelationSet>;

define C-function atk-relation-set-new
  result res :: <AtkRelationSet>;
  c-name: "atk_relation_set_new";
end;

define C-function atk-relation-set-add
  input parameter self :: <AtkRelationSet>;
  input parameter relation_ :: <AtkRelation>;
  c-name: "atk_relation_set_add";
end;

define C-function atk-relation-set-add-relation-by-type
  input parameter self :: <AtkRelationSet>;
  input parameter relationship_ :: <AtkRelationType>;
  input parameter target_ :: <AtkObject>;
  c-name: "atk_relation_set_add_relation_by_type";
end;

define C-function atk-relation-set-contains
  input parameter self :: <AtkRelationSet>;
  input parameter relationship_ :: <AtkRelationType>;
  result res :: <C-boolean>;
  c-name: "atk_relation_set_contains";
end;

define C-function atk-relation-set-contains-target
  input parameter self :: <AtkRelationSet>;
  input parameter relationship_ :: <AtkRelationType>;
  input parameter target_ :: <AtkObject>;
  result res :: <C-boolean>;
  c-name: "atk_relation_set_contains_target";
end;

define C-function atk-relation-set-get-n-relations
  input parameter self :: <AtkRelationSet>;
  result res :: <C-signed-int>;
  c-name: "atk_relation_set_get_n_relations";
end;

define C-function atk-relation-set-get-relation
  input parameter self :: <AtkRelationSet>;
  input parameter i_ :: <C-signed-int>;
  result res :: <AtkRelation>;
  c-name: "atk_relation_set_get_relation";
end;

define C-function atk-relation-set-get-relation-by-type
  input parameter self :: <AtkRelationSet>;
  input parameter relationship_ :: <AtkRelationType>;
  result res :: <AtkRelation>;
  c-name: "atk_relation_set_get_relation_by_type";
end;

define C-function atk-relation-set-remove
  input parameter self :: <AtkRelationSet>;
  input parameter relation_ :: <AtkRelation>;
  c-name: "atk_relation_set_remove";
end;

define C-struct <_AtkRelationSetClass>
  constant slot atkrelationsetclass-parent :: <GObjectClass>;
  constant slot atkrelationsetclass-pad1 :: <C-function-pointer>;
  constant slot atkrelationsetclass-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkRelationSetClass>;
end C-struct;

define constant $ATK-RELATION-NULL = 0;
define constant $ATK-RELATION-CONTROLLED-BY = 1;
define constant $ATK-RELATION-CONTROLLER-FOR = 2;
define constant $ATK-RELATION-LABEL-FOR = 3;
define constant $ATK-RELATION-LABELLED-BY = 4;
define constant $ATK-RELATION-MEMBER-OF = 5;
define constant $ATK-RELATION-NODE-CHILD-OF = 6;
define constant $ATK-RELATION-FLOWS-TO = 7;
define constant $ATK-RELATION-FLOWS-FROM = 8;
define constant $ATK-RELATION-SUBWINDOW-OF = 9;
define constant $ATK-RELATION-EMBEDS = 10;
define constant $ATK-RELATION-EMBEDDED-BY = 11;
define constant $ATK-RELATION-POPUP-FOR = 12;
define constant $ATK-RELATION-PARENT-WINDOW-OF = 13;
define constant $ATK-RELATION-DESCRIBED-BY = 14;
define constant $ATK-RELATION-DESCRIPTION-FOR = 15;
define constant $ATK-RELATION-NODE-PARENT-OF = 16;
define constant $ATK-RELATION-LAST-DEFINED = 17;
define constant <AtkRelationType> = <C-int>;
define C-pointer-type <AtkRelationType*> => <AtkRelationType>;

define constant $ATK-ROLE-INVALID = 0;
define constant $ATK-ROLE-ACCEL-LABEL = 1;
define constant $ATK-ROLE-ALERT = 2;
define constant $ATK-ROLE-ANIMATION = 3;
define constant $ATK-ROLE-ARROW = 4;
define constant $ATK-ROLE-CALENDAR = 5;
define constant $ATK-ROLE-CANVAS = 6;
define constant $ATK-ROLE-CHECK-BOX = 7;
define constant $ATK-ROLE-CHECK-MENU-ITEM = 8;
define constant $ATK-ROLE-COLOR-CHOOSER = 9;
define constant $ATK-ROLE-COLUMN-HEADER = 10;
define constant $ATK-ROLE-COMBO-BOX = 11;
define constant $ATK-ROLE-DATE-EDITOR = 12;
define constant $ATK-ROLE-DESKTOP-ICON = 13;
define constant $ATK-ROLE-DESKTOP-FRAME = 14;
define constant $ATK-ROLE-DIAL = 15;
define constant $ATK-ROLE-DIALOG = 16;
define constant $ATK-ROLE-DIRECTORY-PANE = 17;
define constant $ATK-ROLE-DRAWING-AREA = 18;
define constant $ATK-ROLE-FILE-CHOOSER = 19;
define constant $ATK-ROLE-FILLER = 20;
define constant $ATK-ROLE-FONT-CHOOSER = 21;
define constant $ATK-ROLE-FRAME = 22;
define constant $ATK-ROLE-GLASS-PANE = 23;
define constant $ATK-ROLE-HTML-CONTAINER = 24;
define constant $ATK-ROLE-ICON = 25;
define constant $ATK-ROLE-IMAGE = 26;
define constant $ATK-ROLE-INTERNAL-FRAME = 27;
define constant $ATK-ROLE-LABEL = 28;
define constant $ATK-ROLE-LAYERED-PANE = 29;
define constant $ATK-ROLE-LIST = 30;
define constant $ATK-ROLE-LIST-ITEM = 31;
define constant $ATK-ROLE-MENU = 32;
define constant $ATK-ROLE-MENU-BAR = 33;
define constant $ATK-ROLE-MENU-ITEM = 34;
define constant $ATK-ROLE-OPTION-PANE = 35;
define constant $ATK-ROLE-PAGE-TAB = 36;
define constant $ATK-ROLE-PAGE-TAB-LIST = 37;
define constant $ATK-ROLE-PANEL = 38;
define constant $ATK-ROLE-PASSWORD-TEXT = 39;
define constant $ATK-ROLE-POPUP-MENU = 40;
define constant $ATK-ROLE-PROGRESS-BAR = 41;
define constant $ATK-ROLE-PUSH-BUTTON = 42;
define constant $ATK-ROLE-RADIO-BUTTON = 43;
define constant $ATK-ROLE-RADIO-MENU-ITEM = 44;
define constant $ATK-ROLE-ROOT-PANE = 45;
define constant $ATK-ROLE-ROW-HEADER = 46;
define constant $ATK-ROLE-SCROLL-BAR = 47;
define constant $ATK-ROLE-SCROLL-PANE = 48;
define constant $ATK-ROLE-SEPARATOR = 49;
define constant $ATK-ROLE-SLIDER = 50;
define constant $ATK-ROLE-SPLIT-PANE = 51;
define constant $ATK-ROLE-SPIN-BUTTON = 52;
define constant $ATK-ROLE-STATUSBAR = 53;
define constant $ATK-ROLE-TABLE = 54;
define constant $ATK-ROLE-TABLE-CELL = 55;
define constant $ATK-ROLE-TABLE-COLUMN-HEADER = 56;
define constant $ATK-ROLE-TABLE-ROW-HEADER = 57;
define constant $ATK-ROLE-TEAR-OFF-MENU-ITEM = 58;
define constant $ATK-ROLE-TERMINAL = 59;
define constant $ATK-ROLE-TEXT = 60;
define constant $ATK-ROLE-TOGGLE-BUTTON = 61;
define constant $ATK-ROLE-TOOL-BAR = 62;
define constant $ATK-ROLE-TOOL-TIP = 63;
define constant $ATK-ROLE-TREE = 64;
define constant $ATK-ROLE-TREE-TABLE = 65;
define constant $ATK-ROLE-UNKNOWN = 66;
define constant $ATK-ROLE-VIEWPORT = 67;
define constant $ATK-ROLE-WINDOW = 68;
define constant $ATK-ROLE-HEADER = 69;
define constant $ATK-ROLE-FOOTER = 70;
define constant $ATK-ROLE-PARAGRAPH = 71;
define constant $ATK-ROLE-RULER = 72;
define constant $ATK-ROLE-APPLICATION = 73;
define constant $ATK-ROLE-AUTOCOMPLETE = 74;
define constant $ATK-ROLE-EDITBAR = 75;
define constant $ATK-ROLE-EMBEDDED = 76;
define constant $ATK-ROLE-ENTRY = 77;
define constant $ATK-ROLE-CHART = 78;
define constant $ATK-ROLE-CAPTION = 79;
define constant $ATK-ROLE-DOCUMENT-FRAME = 80;
define constant $ATK-ROLE-HEADING = 81;
define constant $ATK-ROLE-PAGE = 82;
define constant $ATK-ROLE-SECTION = 83;
define constant $ATK-ROLE-REDUNDANT-OBJECT = 84;
define constant $ATK-ROLE-FORM = 85;
define constant $ATK-ROLE-LINK = 86;
define constant $ATK-ROLE-INPUT-METHOD-WINDOW = 87;
define constant $ATK-ROLE-TABLE-ROW = 88;
define constant $ATK-ROLE-TREE-ITEM = 89;
define constant $ATK-ROLE-DOCUMENT-SPREADSHEET = 90;
define constant $ATK-ROLE-DOCUMENT-PRESENTATION = 91;
define constant $ATK-ROLE-DOCUMENT-TEXT = 92;
define constant $ATK-ROLE-DOCUMENT-WEB = 93;
define constant $ATK-ROLE-DOCUMENT-EMAIL = 94;
define constant $ATK-ROLE-COMMENT = 95;
define constant $ATK-ROLE-LIST-BOX = 96;
define constant $ATK-ROLE-GROUPING = 97;
define constant $ATK-ROLE-IMAGE-MAP = 98;
define constant $ATK-ROLE-NOTIFICATION = 99;
define constant $ATK-ROLE-INFO-BAR = 100;
define constant $ATK-ROLE-LEVEL-BAR = 101;
define constant $ATK-ROLE-LAST-DEFINED = 102;
define constant <AtkRole> = <C-int>;
define C-pointer-type <AtkRole*> => <AtkRole>;

// Interface
define open C-subtype <AtkSelection> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkSelection*> => <AtkSelection>;

define C-function atk-selection-add-selection
  input parameter self :: <AtkSelection>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_selection_add_selection";
end;

define C-function atk-selection-clear-selection
  input parameter self :: <AtkSelection>;
  result res :: <C-boolean>;
  c-name: "atk_selection_clear_selection";
end;

define C-function atk-selection-get-selection-count
  input parameter self :: <AtkSelection>;
  result res :: <C-signed-int>;
  c-name: "atk_selection_get_selection_count";
end;

define C-function atk-selection-is-child-selected
  input parameter self :: <AtkSelection>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_selection_is_child_selected";
end;

define C-function atk-selection-ref-selection
  input parameter self :: <AtkSelection>;
  input parameter i_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_selection_ref_selection";
end;

define C-function atk-selection-remove-selection
  input parameter self :: <AtkSelection>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_selection_remove_selection";
end;

define C-function atk-selection-select-all-selection
  input parameter self :: <AtkSelection>;
  result res :: <C-boolean>;
  c-name: "atk_selection_select_all_selection";
end;

define C-struct <_AtkSelectionIface>
  constant slot atkselectioniface-parent :: <GTypeInterface>;
  constant slot atkselectioniface-add-selection :: <C-function-pointer>;
  constant slot atkselectioniface-clear-selection :: <C-function-pointer>;
  constant slot atkselectioniface-ref-selection :: <C-function-pointer>;
  constant slot atkselectioniface-get-selection-count :: <C-function-pointer>;
  constant slot atkselectioniface-is-child-selected :: <C-function-pointer>;
  constant slot atkselectioniface-remove-selection :: <C-function-pointer>;
  constant slot atkselectioniface-select-all-selection :: <C-function-pointer>;
  constant slot atkselectioniface-selection-changed :: <C-function-pointer>;
  constant slot atkselectioniface-pad1 :: <C-function-pointer>;
  constant slot atkselectioniface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkSelectionIface>;
end C-struct;

define open C-subtype <AtkSocket> (<AtkObject>)
  constant slot atksocket-parent :: <AtkObject>;
  constant slot atksocket-embedded-plug-id :: <C-string>;
end C-subtype;

define C-pointer-type <AtkSocket*> => <AtkSocket>;

define C-function atk-socket-new
  result res :: <AtkObject>;
  c-name: "atk_socket_new";
end;

define C-function atk-socket-embed
  input parameter self :: <AtkSocket>;
  input parameter plug_id_ :: <C-string>;
  c-name: "atk_socket_embed";
end;

define C-function atk-socket-is-occupied
  input parameter self :: <AtkSocket>;
  result res :: <C-boolean>;
  c-name: "atk_socket_is_occupied";
end;

define C-struct <_AtkSocketClass>
  constant slot atksocketclass-parent-class :: <AtkObjectClass>;
  constant slot atksocketclass-embed :: <C-function-pointer>;
  pointer-type-name: <AtkSocketClass>;
end C-struct;

define open C-subtype <AtkStateSet> (<GObject>)
  constant slot atkstateset-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkStateSet*> => <AtkStateSet>;

define C-function atk-state-set-new
  result res :: <AtkStateSet>;
  c-name: "atk_state_set_new";
end;

define C-function atk-state-set-add-state
  input parameter self :: <AtkStateSet>;
  input parameter type_ :: <AtkStateType>;
  result res :: <C-boolean>;
  c-name: "atk_state_set_add_state";
end;

define C-function atk-state-set-add-states
  input parameter self :: <AtkStateSet>;
  input parameter types_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_types_ :: <C-signed-int>;
  c-name: "atk_state_set_add_states";
end;

define C-function atk-state-set-and-sets
  input parameter self :: <AtkStateSet>;
  input parameter compare_set_ :: <AtkStateSet>;
  result res :: <AtkStateSet>;
  c-name: "atk_state_set_and_sets";
end;

define C-function atk-state-set-clear-states
  input parameter self :: <AtkStateSet>;
  c-name: "atk_state_set_clear_states";
end;

define C-function atk-state-set-contains-state
  input parameter self :: <AtkStateSet>;
  input parameter type_ :: <AtkStateType>;
  result res :: <C-boolean>;
  c-name: "atk_state_set_contains_state";
end;

define C-function atk-state-set-contains-states
  input parameter self :: <AtkStateSet>;
  input parameter types_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_types_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_state_set_contains_states";
end;

define C-function atk-state-set-is-empty
  input parameter self :: <AtkStateSet>;
  result res :: <C-boolean>;
  c-name: "atk_state_set_is_empty";
end;

define C-function atk-state-set-or-sets
  input parameter self :: <AtkStateSet>;
  input parameter compare_set_ :: <AtkStateSet>;
  result res :: <AtkStateSet>;
  c-name: "atk_state_set_or_sets";
end;

define C-function atk-state-set-remove-state
  input parameter self :: <AtkStateSet>;
  input parameter type_ :: <AtkStateType>;
  result res :: <C-boolean>;
  c-name: "atk_state_set_remove_state";
end;

define C-function atk-state-set-xor-sets
  input parameter self :: <AtkStateSet>;
  input parameter compare_set_ :: <AtkStateSet>;
  result res :: <AtkStateSet>;
  c-name: "atk_state_set_xor_sets";
end;

define C-struct <_AtkStateSetClass>
  constant slot atkstatesetclass-parent :: <GObjectClass>;
  pointer-type-name: <AtkStateSetClass>;
end C-struct;

define constant $ATK-STATE-INVALID = 0;
define constant $ATK-STATE-ACTIVE = 1;
define constant $ATK-STATE-ARMED = 2;
define constant $ATK-STATE-BUSY = 3;
define constant $ATK-STATE-CHECKED = 4;
define constant $ATK-STATE-DEFUNCT = 5;
define constant $ATK-STATE-EDITABLE = 6;
define constant $ATK-STATE-ENABLED = 7;
define constant $ATK-STATE-EXPANDABLE = 8;
define constant $ATK-STATE-EXPANDED = 9;
define constant $ATK-STATE-FOCUSABLE = 10;
define constant $ATK-STATE-FOCUSED = 11;
define constant $ATK-STATE-HORIZONTAL = 12;
define constant $ATK-STATE-ICONIFIED = 13;
define constant $ATK-STATE-MODAL = 14;
define constant $ATK-STATE-MULTI-LINE = 15;
define constant $ATK-STATE-MULTISELECTABLE = 16;
define constant $ATK-STATE-OPAQUE = 17;
define constant $ATK-STATE-PRESSED = 18;
define constant $ATK-STATE-RESIZABLE = 19;
define constant $ATK-STATE-SELECTABLE = 20;
define constant $ATK-STATE-SELECTED = 21;
define constant $ATK-STATE-SENSITIVE = 22;
define constant $ATK-STATE-SHOWING = 23;
define constant $ATK-STATE-SINGLE-LINE = 24;
define constant $ATK-STATE-STALE = 25;
define constant $ATK-STATE-TRANSIENT = 26;
define constant $ATK-STATE-VERTICAL = 27;
define constant $ATK-STATE-VISIBLE = 28;
define constant $ATK-STATE-MANAGES-DESCENDANTS = 29;
define constant $ATK-STATE-INDETERMINATE = 30;
define constant $ATK-STATE-TRUNCATED = 31;
define constant $ATK-STATE-REQUIRED = 32;
define constant $ATK-STATE-INVALID-ENTRY = 33;
define constant $ATK-STATE-SUPPORTS-AUTOCOMPLETION = 34;
define constant $ATK-STATE-SELECTABLE-TEXT = 35;
define constant $ATK-STATE-DEFAULT = 36;
define constant $ATK-STATE-ANIMATED = 37;
define constant $ATK-STATE-VISITED = 38;
define constant $ATK-STATE-LAST-DEFINED = 39;
define constant <AtkStateType> = <C-int>;
define C-pointer-type <AtkStateType*> => <AtkStateType>;

// Interface
define open C-subtype <AtkStreamableContent> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkStreamableContent*> => <AtkStreamableContent>;

define C-function atk-streamable-content-get-mime-type
  input parameter self :: <AtkStreamableContent>;
  input parameter i_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_streamable_content_get_mime_type";
end;

define C-function atk-streamable-content-get-n-mime-types
  input parameter self :: <AtkStreamableContent>;
  result res :: <C-signed-int>;
  c-name: "atk_streamable_content_get_n_mime_types";
end;

define C-function atk-streamable-content-get-stream
  input parameter self :: <AtkStreamableContent>;
  input parameter mime_type_ :: <C-string>;
  result res :: <GIOChannel>;
  c-name: "atk_streamable_content_get_stream";
end;

define C-function atk-streamable-content-get-uri
  input parameter self :: <AtkStreamableContent>;
  input parameter mime_type_ :: <C-string>;
  result res :: <C-string>;
  c-name: "atk_streamable_content_get_uri";
end;

define C-struct <_AtkStreamableContentIface>
  constant slot atkstreamablecontentiface-parent :: <GTypeInterface>;
  constant slot atkstreamablecontentiface-get-n-mime-types :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-get-mime-type :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-get-stream :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-get-uri :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-pad1 :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-pad2 :: <C-function-pointer>;
  constant slot atkstreamablecontentiface-pad3 :: <C-function-pointer>;
  pointer-type-name: <AtkStreamableContentIface>;
end C-struct;

// Interface
define open C-subtype <AtkTable> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkTable*> => <AtkTable>;

define C-function atk-table-add-column-selection
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_add_column_selection";
end;

define C-function atk-table-add-row-selection
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_add_row_selection";
end;

define C-function atk-table-get-caption
  input parameter self :: <AtkTable>;
  result res :: <AtkObject>;
  c-name: "atk_table_get_caption";
end;

define C-function atk-table-get-column-at-index
  input parameter self :: <AtkTable>;
  input parameter index__ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_column_at_index";
end;

define C-function atk-table-get-column-description
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_table_get_column_description";
end;

define C-function atk-table-get-column-extent-at
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_column_extent_at";
end;

define C-function atk-table-get-column-header
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_table_get_column_header";
end;

define C-function atk-table-get-index-at
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_index_at";
end;

define C-function atk-table-get-n-columns
  input parameter self :: <AtkTable>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_n_columns";
end;

define C-function atk-table-get-n-rows
  input parameter self :: <AtkTable>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_n_rows";
end;

define C-function atk-table-get-row-at-index
  input parameter self :: <AtkTable>;
  input parameter index__ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_row_at_index";
end;

define C-function atk-table-get-row-description
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_table_get_row_description";
end;

define C-function atk-table-get-row-extent-at
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_row_extent_at";
end;

define C-function atk-table-get-row-header
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_table_get_row_header";
end;

define C-function atk-table-get-selected-columns
  input parameter self :: <AtkTable>;
  input parameter selected_ :: <C-signed-int*>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_selected_columns";
end;

define C-function atk-table-get-selected-rows
  input parameter self :: <AtkTable>;
  input parameter selected_ :: <C-signed-int*>;
  result res :: <C-signed-int>;
  c-name: "atk_table_get_selected_rows";
end;

define C-function atk-table-get-summary
  input parameter self :: <AtkTable>;
  result res :: <AtkObject>;
  c-name: "atk_table_get_summary";
end;

define C-function atk-table-is-column-selected
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_is_column_selected";
end;

define C-function atk-table-is-row-selected
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_is_row_selected";
end;

define C-function atk-table-is-selected
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_is_selected";
end;

define C-function atk-table-ref-at
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter column_ :: <C-signed-int>;
  result res :: <AtkObject>;
  c-name: "atk_table_ref_at";
end;

define C-function atk-table-remove-column-selection
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_remove_column_selection";
end;

define C-function atk-table-remove-row-selection
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_table_remove_row_selection";
end;

define C-function atk-table-set-caption
  input parameter self :: <AtkTable>;
  input parameter caption_ :: <AtkObject>;
  c-name: "atk_table_set_caption";
end;

define C-function atk-table-set-column-description
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  input parameter description_ :: <C-string>;
  c-name: "atk_table_set_column_description";
end;

define C-function atk-table-set-column-header
  input parameter self :: <AtkTable>;
  input parameter column_ :: <C-signed-int>;
  input parameter header_ :: <AtkObject>;
  c-name: "atk_table_set_column_header";
end;

define C-function atk-table-set-row-description
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter description_ :: <C-string>;
  c-name: "atk_table_set_row_description";
end;

define C-function atk-table-set-row-header
  input parameter self :: <AtkTable>;
  input parameter row_ :: <C-signed-int>;
  input parameter header_ :: <AtkObject>;
  c-name: "atk_table_set_row_header";
end;

define C-function atk-table-set-summary
  input parameter self :: <AtkTable>;
  input parameter accessible_ :: <AtkObject>;
  c-name: "atk_table_set_summary";
end;

define C-struct <_AtkTableIface>
  constant slot atktableiface-parent :: <GTypeInterface>;
  constant slot atktableiface-ref-at :: <C-function-pointer>;
  constant slot atktableiface-get-index-at :: <C-function-pointer>;
  constant slot atktableiface-get-column-at-index :: <C-function-pointer>;
  constant slot atktableiface-get-row-at-index :: <C-function-pointer>;
  constant slot atktableiface-get-n-columns :: <C-function-pointer>;
  constant slot atktableiface-get-n-rows :: <C-function-pointer>;
  constant slot atktableiface-get-column-extent-at :: <C-function-pointer>;
  constant slot atktableiface-get-row-extent-at :: <C-function-pointer>;
  constant slot atktableiface-get-caption :: <C-function-pointer>;
  constant slot atktableiface-get-column-description :: <C-function-pointer>;
  constant slot atktableiface-get-column-header :: <C-function-pointer>;
  constant slot atktableiface-get-row-description :: <C-function-pointer>;
  constant slot atktableiface-get-row-header :: <C-function-pointer>;
  constant slot atktableiface-get-summary :: <C-function-pointer>;
  constant slot atktableiface-set-caption :: <C-function-pointer>;
  constant slot atktableiface-set-column-description :: <C-function-pointer>;
  constant slot atktableiface-set-column-header :: <C-function-pointer>;
  constant slot atktableiface-set-row-description :: <C-function-pointer>;
  constant slot atktableiface-set-row-header :: <C-function-pointer>;
  constant slot atktableiface-set-summary :: <C-function-pointer>;
  constant slot atktableiface-get-selected-columns :: <C-function-pointer>;
  constant slot atktableiface-get-selected-rows :: <C-function-pointer>;
  constant slot atktableiface-is-column-selected :: <C-function-pointer>;
  constant slot atktableiface-is-row-selected :: <C-function-pointer>;
  constant slot atktableiface-is-selected :: <C-function-pointer>;
  constant slot atktableiface-add-row-selection :: <C-function-pointer>;
  constant slot atktableiface-remove-row-selection :: <C-function-pointer>;
  constant slot atktableiface-add-column-selection :: <C-function-pointer>;
  constant slot atktableiface-remove-column-selection :: <C-function-pointer>;
  constant slot atktableiface-row-inserted :: <C-function-pointer>;
  constant slot atktableiface-column-inserted :: <C-function-pointer>;
  constant slot atktableiface-row-deleted :: <C-function-pointer>;
  constant slot atktableiface-column-deleted :: <C-function-pointer>;
  constant slot atktableiface-row-reordered :: <C-function-pointer>;
  constant slot atktableiface-column-reordered :: <C-function-pointer>;
  constant slot atktableiface-model-changed :: <C-function-pointer>;
  constant slot atktableiface-pad1 :: <C-function-pointer>;
  constant slot atktableiface-pad2 :: <C-function-pointer>;
  constant slot atktableiface-pad3 :: <C-function-pointer>;
  constant slot atktableiface-pad4 :: <C-function-pointer>;
  pointer-type-name: <AtkTableIface>;
end C-struct;

// Interface
define open C-subtype <AtkText> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkText*> => <AtkText>;

define C-function atk-text-free-ranges
  input parameter ranges_ :: <C-unsigned-char*> /* Not supported */;
  c-name: "atk_text_free_ranges";
end;

define C-function atk-text-add-selection
  input parameter self :: <AtkText>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_text_add_selection";
end;

define C-function atk-text-get-bounded-ranges
  input parameter self :: <AtkText>;
  input parameter rect_ :: <AtkTextRectangle>;
  input parameter coord_type_ :: <AtkCoordType>;
  input parameter x_clip_type_ :: <AtkTextClipType>;
  input parameter y_clip_type_ :: <AtkTextClipType>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "atk_text_get_bounded_ranges";
end;

define C-function atk-text-get-caret-offset
  input parameter self :: <AtkText>;
  result res :: <C-signed-int>;
  c-name: "atk_text_get_caret_offset";
end;

define C-function atk-text-get-character-at-offset
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  result res :: <C-unsigned-int>;
  c-name: "atk_text_get_character_at_offset";
end;

define C-function atk-text-get-character-count
  input parameter self :: <AtkText>;
  result res :: <C-signed-int>;
  c-name: "atk_text_get_character_count";
end;

define C-function atk-text-get-character-extents
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  input parameter x_ :: <C-signed-int*>;
  input parameter y_ :: <C-signed-int*>;
  input parameter width_ :: <C-signed-int*>;
  input parameter height_ :: <C-signed-int*>;
  input parameter coords_ :: <AtkCoordType>;
  c-name: "atk_text_get_character_extents";
end;

define C-function atk-text-get-default-attributes
  input parameter self :: <AtkText>;
  result res :: <GSList>;
  c-name: "atk_text_get_default_attributes";
end;

define C-function atk-text-get-n-selections
  input parameter self :: <AtkText>;
  result res :: <C-signed-int>;
  c-name: "atk_text_get_n_selections";
end;

define C-function atk-text-get-offset-at-point
  input parameter self :: <AtkText>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter coords_ :: <AtkCoordType>;
  result res :: <C-signed-int>;
  c-name: "atk_text_get_offset_at_point";
end;

define C-function atk-text-get-range-extents
  input parameter self :: <AtkText>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  input parameter coord_type_ :: <AtkCoordType>;
  input parameter rect_ :: <AtkTextRectangle>;
  c-name: "atk_text_get_range_extents";
end;

define C-function atk-text-get-run-attributes
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  output parameter start_offset_ :: <C-signed-int*>;
  output parameter end_offset_ :: <C-signed-int*>;
  result res :: <GSList>;
  c-name: "atk_text_get_run_attributes";
end;

define C-function atk-text-get-selection
  input parameter self :: <AtkText>;
  input parameter selection_num_ :: <C-signed-int>;
  output parameter start_offset_ :: <C-signed-int*>;
  output parameter end_offset_ :: <C-signed-int*>;
  result res :: <C-string>;
  c-name: "atk_text_get_selection";
end;

define C-function atk-text-get-text
  input parameter self :: <AtkText>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_text_get_text";
end;

define C-function atk-text-get-text-after-offset
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  input parameter boundary_type_ :: <AtkTextBoundary>;
  output parameter start_offset_ :: <C-signed-int*>;
  output parameter end_offset_ :: <C-signed-int*>;
  result res :: <C-string>;
  c-name: "atk_text_get_text_after_offset";
end;

define C-function atk-text-get-text-at-offset
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  input parameter boundary_type_ :: <AtkTextBoundary>;
  output parameter start_offset_ :: <C-signed-int*>;
  output parameter end_offset_ :: <C-signed-int*>;
  result res :: <C-string>;
  c-name: "atk_text_get_text_at_offset";
end;

define C-function atk-text-get-text-before-offset
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  input parameter boundary_type_ :: <AtkTextBoundary>;
  output parameter start_offset_ :: <C-signed-int*>;
  output parameter end_offset_ :: <C-signed-int*>;
  result res :: <C-string>;
  c-name: "atk_text_get_text_before_offset";
end;

define C-function atk-text-remove-selection
  input parameter self :: <AtkText>;
  input parameter selection_num_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_text_remove_selection";
end;

define C-function atk-text-set-caret-offset
  input parameter self :: <AtkText>;
  input parameter offset_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_text_set_caret_offset";
end;

define C-function atk-text-set-selection
  input parameter self :: <AtkText>;
  input parameter selection_num_ :: <C-signed-int>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "atk_text_set_selection";
end;

define constant $ATK-TEXT-ATTR-INVALID = 0;
define constant $ATK-TEXT-ATTR-LEFT-MARGIN = 1;
define constant $ATK-TEXT-ATTR-RIGHT-MARGIN = 2;
define constant $ATK-TEXT-ATTR-INDENT = 3;
define constant $ATK-TEXT-ATTR-INVISIBLE = 4;
define constant $ATK-TEXT-ATTR-EDITABLE = 5;
define constant $ATK-TEXT-ATTR-PIXELS-ABOVE-LINES = 6;
define constant $ATK-TEXT-ATTR-PIXELS-BELOW-LINES = 7;
define constant $ATK-TEXT-ATTR-PIXELS-INSIDE-WRAP = 8;
define constant $ATK-TEXT-ATTR-BG-FULL-HEIGHT = 9;
define constant $ATK-TEXT-ATTR-RISE = 10;
define constant $ATK-TEXT-ATTR-UNDERLINE = 11;
define constant $ATK-TEXT-ATTR-STRIKETHROUGH = 12;
define constant $ATK-TEXT-ATTR-SIZE = 13;
define constant $ATK-TEXT-ATTR-SCALE = 14;
define constant $ATK-TEXT-ATTR-WEIGHT = 15;
define constant $ATK-TEXT-ATTR-LANGUAGE = 16;
define constant $ATK-TEXT-ATTR-FAMILY-NAME = 17;
define constant $ATK-TEXT-ATTR-BG-COLOR = 18;
define constant $ATK-TEXT-ATTR-FG-COLOR = 19;
define constant $ATK-TEXT-ATTR-BG-STIPPLE = 20;
define constant $ATK-TEXT-ATTR-FG-STIPPLE = 21;
define constant $ATK-TEXT-ATTR-WRAP-MODE = 22;
define constant $ATK-TEXT-ATTR-DIRECTION = 23;
define constant $ATK-TEXT-ATTR-JUSTIFICATION = 24;
define constant $ATK-TEXT-ATTR-STRETCH = 25;
define constant $ATK-TEXT-ATTR-VARIANT = 26;
define constant $ATK-TEXT-ATTR-STYLE = 27;
define constant $ATK-TEXT-ATTR-LAST-DEFINED = 28;
define constant <AtkTextAttribute> = <C-int>;
define C-pointer-type <AtkTextAttribute*> => <AtkTextAttribute>;

define constant $ATK-TEXT-BOUNDARY-CHAR = 0;
define constant $ATK-TEXT-BOUNDARY-WORD-START = 1;
define constant $ATK-TEXT-BOUNDARY-WORD-END = 2;
define constant $ATK-TEXT-BOUNDARY-SENTENCE-START = 3;
define constant $ATK-TEXT-BOUNDARY-SENTENCE-END = 4;
define constant $ATK-TEXT-BOUNDARY-LINE-START = 5;
define constant $ATK-TEXT-BOUNDARY-LINE-END = 6;
define constant <AtkTextBoundary> = <C-int>;
define C-pointer-type <AtkTextBoundary*> => <AtkTextBoundary>;

define constant $ATK-TEXT-CLIP-NONE = 0;
define constant $ATK-TEXT-CLIP-MIN = 1;
define constant $ATK-TEXT-CLIP-MAX = 2;
define constant $ATK-TEXT-CLIP-BOTH = 3;
define constant <AtkTextClipType> = <C-int>;
define C-pointer-type <AtkTextClipType*> => <AtkTextClipType>;

define C-struct <_AtkTextIface>
  constant slot atktextiface-parent :: <GTypeInterface>;
  constant slot atktextiface-get-text :: <C-function-pointer>;
  constant slot atktextiface-get-text-after-offset :: <C-function-pointer>;
  constant slot atktextiface-get-text-at-offset :: <C-function-pointer>;
  constant slot atktextiface-get-character-at-offset :: <C-function-pointer>;
  constant slot atktextiface-get-text-before-offset :: <C-function-pointer>;
  constant slot atktextiface-get-caret-offset :: <C-function-pointer>;
  constant slot atktextiface-get-run-attributes :: <C-function-pointer>;
  constant slot atktextiface-get-default-attributes :: <C-function-pointer>;
  constant slot atktextiface-get-character-extents :: <C-function-pointer>;
  constant slot atktextiface-get-character-count :: <C-function-pointer>;
  constant slot atktextiface-get-offset-at-point :: <C-function-pointer>;
  constant slot atktextiface-get-n-selections :: <C-function-pointer>;
  constant slot atktextiface-get-selection :: <C-function-pointer>;
  constant slot atktextiface-add-selection :: <C-function-pointer>;
  constant slot atktextiface-remove-selection :: <C-function-pointer>;
  constant slot atktextiface-set-selection :: <C-function-pointer>;
  constant slot atktextiface-set-caret-offset :: <C-function-pointer>;
  constant slot atktextiface-text-changed :: <C-function-pointer>;
  constant slot atktextiface-text-caret-moved :: <C-function-pointer>;
  constant slot atktextiface-text-selection-changed :: <C-function-pointer>;
  constant slot atktextiface-text-attributes-changed :: <C-function-pointer>;
  constant slot atktextiface-get-range-extents :: <C-function-pointer>;
  constant slot atktextiface-get-bounded-ranges :: <C-function-pointer>;
  constant slot atktextiface-pad4 :: <C-function-pointer>;
  pointer-type-name: <AtkTextIface>;
end C-struct;

define C-struct <_AtkTextRange>
  slot atktextrange-bounds :: <AtkTextRectangle>;
  slot atktextrange-start-offset :: <C-signed-int>;
  slot atktextrange-end-offset :: <C-signed-int>;
  slot atktextrange-content :: <C-string>;
  pointer-type-name: <AtkTextRange>;
end C-struct;

define C-struct <_AtkTextRectangle>
  slot atktextrectangle-x :: <C-signed-int>;
  slot atktextrectangle-y :: <C-signed-int>;
  slot atktextrectangle-width :: <C-signed-int>;
  slot atktextrectangle-height :: <C-signed-int>;
  pointer-type-name: <AtkTextRectangle>;
end C-struct;

define open C-subtype <AtkUtil> (<GObject>)
  constant slot atkutil-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkUtil*> => <AtkUtil>;

define C-struct <_AtkUtilClass>
  constant slot atkutilclass-parent :: <GObjectClass>;
  constant slot atkutilclass-add-global-event-listener :: <C-void*>;
  constant slot atkutilclass-remove-global-event-listener :: <C-function-pointer>;
  constant slot atkutilclass-add-key-event-listener :: <C-void*>;
  constant slot atkutilclass-remove-key-event-listener :: <C-function-pointer>;
  constant slot atkutilclass-get-root :: <C-void*>;
  constant slot atkutilclass-get-toolkit-name :: <C-function-pointer>;
  constant slot atkutilclass-get-toolkit-version :: <C-function-pointer>;
  pointer-type-name: <AtkUtilClass>;
end C-struct;

// Interface
define open C-subtype <AtkValue> (<C-void*>)
end C-subtype;

define C-pointer-type <AtkValue*> => <AtkValue>;

define C-function atk-value-get-current-value
  input parameter self :: <AtkValue>;
  input parameter value_ :: <GValue>;
  c-name: "atk_value_get_current_value";
end;

define C-function atk-value-get-maximum-value
  input parameter self :: <AtkValue>;
  input parameter value_ :: <GValue>;
  c-name: "atk_value_get_maximum_value";
end;

define C-function atk-value-get-minimum-increment
  input parameter self :: <AtkValue>;
  input parameter value_ :: <GValue>;
  c-name: "atk_value_get_minimum_increment";
end;

define C-function atk-value-get-minimum-value
  input parameter self :: <AtkValue>;
  input parameter value_ :: <GValue>;
  c-name: "atk_value_get_minimum_value";
end;

define C-function atk-value-set-current-value
  input parameter self :: <AtkValue>;
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "atk_value_set_current_value";
end;

define C-struct <_AtkValueIface>
  constant slot atkvalueiface-parent :: <GTypeInterface>;
  constant slot atkvalueiface-get-current-value :: <C-function-pointer>;
  constant slot atkvalueiface-get-maximum-value :: <C-function-pointer>;
  constant slot atkvalueiface-get-minimum-value :: <C-function-pointer>;
  constant slot atkvalueiface-set-current-value :: <C-function-pointer>;
  constant slot atkvalueiface-get-minimum-increment :: <C-function-pointer>;
  constant slot atkvalueiface-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkValueIface>;
end C-struct;

// Interface
define open C-subtype <AtkWindow> (<AtkObject>)
end C-subtype;

define C-pointer-type <AtkWindow*> => <AtkWindow>;

define C-struct <_AtkWindowIface>
  constant slot atkwindowiface-parent :: <GTypeInterface>;
  constant slot atkwindowiface--padding-dummy :: <C-void*>;
  pointer-type-name: <AtkWindowIface>;
end C-struct;

define C-struct <_Atk_PropertyValues>
  slot atk-propertyvalues-property-name :: <C-string>;
  slot atk-propertyvalues-old-value :: <GValue>;
  slot atk-propertyvalues-new-value :: <GValue>;
  pointer-type-name: <Atk_PropertyValues>;
end C-struct;

define C-struct <_Atk_Registry>
  slot atk-registry-parent :: <GObject>;
  slot atk-registry-factory-type-registry :: <GHashTable>;
  slot atk-registry-factory-singleton-cache :: <GHashTable>;
  pointer-type-name: <Atk_Registry>;
end C-struct;

define C-struct <_Atk_RegistryClass>
  slot atk-registryclass-parent-class :: <GObjectClass>;
  pointer-type-name: <Atk_RegistryClass>;
end C-struct;

define C-function atk-focus-tracker-notify
  input parameter object_ :: <AtkObject>;
  c-name: "atk_focus_tracker_notify";
end;

define C-function atk-get-binary-age
  result res :: <C-unsigned-int>;
  c-name: "atk_get_binary_age";
end;

define C-function atk-get-default-registry
  result res :: <AtkRegistry>;
  c-name: "atk_get_default_registry";
end;

define C-function atk-get-focus-object
  result res :: <AtkObject>;
  c-name: "atk_get_focus_object";
end;

define C-function atk-get-interface-age
  result res :: <C-unsigned-int>;
  c-name: "atk_get_interface_age";
end;

define C-function atk-get-major-version
  result res :: <C-unsigned-int>;
  c-name: "atk_get_major_version";
end;

define C-function atk-get-micro-version
  result res :: <C-unsigned-int>;
  c-name: "atk_get_micro_version";
end;

define C-function atk-get-minor-version
  result res :: <C-unsigned-int>;
  c-name: "atk_get_minor_version";
end;

define C-function atk-get-root
  result res :: <AtkObject>;
  c-name: "atk_get_root";
end;

define C-function atk-get-toolkit-name
  result res :: <C-string>;
  c-name: "atk_get_toolkit_name";
end;

define C-function atk-get-toolkit-version
  result res :: <C-string>;
  c-name: "atk_get_toolkit_version";
end;

define C-function atk-get-version
  result res :: <C-string>;
  c-name: "atk_get_version";
end;

define C-function atk-relation-type-for-name
  input parameter name_ :: <C-string>;
  result res :: <AtkRelationType>;
  c-name: "atk_relation_type_for_name";
end;

define C-function atk-relation-type-get-name
  input parameter type_ :: <AtkRelationType>;
  result res :: <C-string>;
  c-name: "atk_relation_type_get_name";
end;

define C-function atk-relation-type-register
  input parameter name_ :: <C-string>;
  result res :: <AtkRelationType>;
  c-name: "atk_relation_type_register";
end;

define C-function atk-remove-focus-tracker
  input parameter tracker_id_ :: <C-unsigned-int>;
  c-name: "atk_remove_focus_tracker";
end;

define C-function atk-remove-global-event-listener
  input parameter listener_id_ :: <C-unsigned-int>;
  c-name: "atk_remove_global_event_listener";
end;

define C-function atk-remove-key-event-listener
  input parameter listener_id_ :: <C-unsigned-int>;
  c-name: "atk_remove_key_event_listener";
end;

define C-function atk-role-for-name
  input parameter name_ :: <C-string>;
  result res :: <AtkRole>;
  c-name: "atk_role_for_name";
end;

define C-function atk-role-get-localized-name
  input parameter role_ :: <AtkRole>;
  result res :: <C-string>;
  c-name: "atk_role_get_localized_name";
end;

define C-function atk-role-get-name
  input parameter role_ :: <AtkRole>;
  result res :: <C-string>;
  c-name: "atk_role_get_name";
end;

define C-function atk-role-register
  input parameter name_ :: <C-string>;
  result res :: <AtkRole>;
  c-name: "atk_role_register";
end;

define C-function atk-state-type-for-name
  input parameter name_ :: <C-string>;
  result res :: <AtkStateType>;
  c-name: "atk_state_type_for_name";
end;

define C-function atk-state-type-get-name
  input parameter type_ :: <AtkStateType>;
  result res :: <C-string>;
  c-name: "atk_state_type_get_name";
end;

define C-function atk-state-type-register
  input parameter name_ :: <C-string>;
  result res :: <AtkStateType>;
  c-name: "atk_state_type_register";
end;

define C-function atk-text-attribute-for-name
  input parameter name_ :: <C-string>;
  result res :: <AtkTextAttribute>;
  c-name: "atk_text_attribute_for_name";
end;

define C-function atk-text-attribute-get-name
  input parameter attr_ :: <AtkTextAttribute>;
  result res :: <C-string>;
  c-name: "atk_text_attribute_get_name";
end;

define C-function atk-text-attribute-get-value
  input parameter attr_ :: <AtkTextAttribute>;
  input parameter index__ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "atk_text_attribute_get_value";
end;

define C-function atk-text-attribute-register
  input parameter name_ :: <C-string>;
  result res :: <AtkTextAttribute>;
  c-name: "atk_text_attribute_register";
end;

