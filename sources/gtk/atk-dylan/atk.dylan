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
  constant slot atk-action-iface-parent :: <GTypeInterface>;
  constant slot atk-action-iface-do-action :: <C-function-pointer>;
  constant slot atk-action-iface-get-n-actions :: <C-function-pointer>;
  constant slot atk-action-iface-get-description :: <C-function-pointer>;
  constant slot atk-action-iface-get-name :: <C-function-pointer>;
  constant slot atk-action-iface-get-keybinding :: <C-function-pointer>;
  constant slot atk-action-iface-set-description :: <C-function-pointer>;
  constant slot atk-action-iface-get-localized-name :: <C-function-pointer>;
  constant slot atk-action-iface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkActionIface>;
end C-struct;

define C-struct <_AtkAttribute>
  slot atk-attribute-name :: <C-string>;
  slot atk-attribute-value :: <C-string>;
  pointer-type-name: <AtkAttribute>;
end C-struct;

define C-function atk-attribute-set-free
  input parameter attrib_set_ :: <GSList>;
  c-name: "atk_attribute_set_free";
end;

define constant $binary-age = 20810;

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
  constant slot atk-component-iface-parent :: <GTypeInterface>;
  constant slot atk-component-iface-add-focus-handler :: <C-void*>;
  constant slot atk-component-iface-contains :: <C-function-pointer>;
  constant slot atk-component-iface-ref-accessible-at-point :: <C-function-pointer>;
  constant slot atk-component-iface-get-extents :: <C-function-pointer>;
  constant slot atk-component-iface-get-position :: <C-function-pointer>;
  constant slot atk-component-iface-get-size :: <C-function-pointer>;
  constant slot atk-component-iface-grab-focus :: <C-function-pointer>;
  constant slot atk-component-iface-remove-focus-handler :: <C-function-pointer>;
  constant slot atk-component-iface-set-extents :: <C-function-pointer>;
  constant slot atk-component-iface-set-position :: <C-function-pointer>;
  constant slot atk-component-iface-set-size :: <C-function-pointer>;
  constant slot atk-component-iface-get-layer :: <C-function-pointer>;
  constant slot atk-component-iface-get-mdi-zorder :: <C-function-pointer>;
  constant slot atk-component-iface-bounds-changed :: <C-function-pointer>;
  constant slot atk-component-iface-get-alpha :: <C-function-pointer>;
  pointer-type-name: <AtkComponentIface>;
end C-struct;

define constant $atk-xy-screen = 0;
define constant $atk-xy-window = 1;
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
  constant slot atk-document-iface-parent :: <GTypeInterface>;
  constant slot atk-document-iface-get-document-type :: <C-function-pointer>;
  constant slot atk-document-iface-get-document :: <C-function-pointer>;
  constant slot atk-document-iface-get-document-locale :: <C-function-pointer>;
  constant slot atk-document-iface-get-document-attributes :: <C-function-pointer>;
  constant slot atk-document-iface-get-document-attribute-value :: <C-function-pointer>;
  constant slot atk-document-iface-set-document-attribute :: <C-function-pointer>;
  constant slot atk-document-iface-pad1 :: <C-function-pointer>;
  constant slot atk-document-iface-pad2 :: <C-function-pointer>;
  constant slot atk-document-iface-pad3 :: <C-function-pointer>;
  constant slot atk-document-iface-pad4 :: <C-function-pointer>;
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
  constant slot atk-editable-text-iface-parent-interface :: <GTypeInterface>;
  constant slot atk-editable-text-iface-set-run-attributes :: <C-function-pointer>;
  constant slot atk-editable-text-iface-set-text-contents :: <C-function-pointer>;
  constant slot atk-editable-text-iface-insert-text :: <C-function-pointer>;
  constant slot atk-editable-text-iface-copy-text :: <C-function-pointer>;
  constant slot atk-editable-text-iface-cut-text :: <C-function-pointer>;
  constant slot atk-editable-text-iface-delete-text :: <C-function-pointer>;
  constant slot atk-editable-text-iface-paste-text :: <C-function-pointer>;
  constant slot atk-editable-text-iface-pad1 :: <C-function-pointer>;
  constant slot atk-editable-text-iface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkEditableTextIface>;
end C-struct;

define open C-subtype <AtkGObjectAccessible> (<AtkObject>)
  constant slot atk-g-object-accessible-parent :: <AtkObject>;
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
  constant slot atk-g-object-accessible-class-parent-class :: <AtkObjectClass>;
  constant slot atk-g-object-accessible-class-pad1 :: <C-function-pointer>;
  constant slot atk-g-object-accessible-class-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkGObjectAccessibleClass>;
end C-struct;

define open C-subtype <AtkHyperlink> (<GObject>)
  constant slot atk-hyperlink-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkHyperlink*> => <AtkHyperlink>;

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
  constant slot atk-hyperlink-class-parent :: <GObjectClass>;
  constant slot atk-hyperlink-class-get-uri :: <C-function-pointer>;
  constant slot atk-hyperlink-class-get-object :: <C-function-pointer>;
  constant slot atk-hyperlink-class-get-end-index :: <C-function-pointer>;
  constant slot atk-hyperlink-class-get-start-index :: <C-function-pointer>;
  constant slot atk-hyperlink-class-is-valid :: <C-function-pointer>;
  constant slot atk-hyperlink-class-get-n-anchors :: <C-function-pointer>;
  constant slot atk-hyperlink-class-link-state :: <C-function-pointer>;
  constant slot atk-hyperlink-class-is-selected-link :: <C-function-pointer>;
  constant slot atk-hyperlink-class-link-activated :: <C-function-pointer>;
  constant slot atk-hyperlink-class-pad1 :: <C-function-pointer>;
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
  constant slot atk-hyperlink-impl-iface-parent :: <GTypeInterface>;
  constant slot atk-hyperlink-impl-iface-get-hyperlink :: <C-function-pointer>;
  constant slot atk-hyperlink-impl-iface-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkHyperlinkImplIface>;
end C-struct;

define constant $atk-hyperlink-is-inline = 1;
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
  constant slot atk-hypertext-iface-parent :: <GTypeInterface>;
  constant slot atk-hypertext-iface-get-link :: <C-function-pointer>;
  constant slot atk-hypertext-iface-get-n-links :: <C-function-pointer>;
  constant slot atk-hypertext-iface-get-link-index :: <C-function-pointer>;
  constant slot atk-hypertext-iface-link-selected :: <C-function-pointer>;
  constant slot atk-hypertext-iface-pad1 :: <C-function-pointer>;
  constant slot atk-hypertext-iface-pad2 :: <C-function-pointer>;
  constant slot atk-hypertext-iface-pad3 :: <C-function-pointer>;
  pointer-type-name: <AtkHypertextIface>;
end C-struct;

define constant $interface-age = 1;

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
  constant slot atk-image-iface-parent :: <GTypeInterface>;
  constant slot atk-image-iface-get-image-position :: <C-function-pointer>;
  constant slot atk-image-iface-get-image-description :: <C-function-pointer>;
  constant slot atk-image-iface-get-image-size :: <C-function-pointer>;
  constant slot atk-image-iface-set-image-description :: <C-function-pointer>;
  constant slot atk-image-iface-get-image-locale :: <C-function-pointer>;
  constant slot atk-image-iface-pad1 :: <C-function-pointer>;
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
  slot atk-key-event-struct-type :: <C-signed-int>;
  slot atk-key-event-struct-state :: <C-unsigned-int>;
  slot atk-key-event-struct-keyval :: <C-unsigned-int>;
  slot atk-key-event-struct-length :: <C-signed-int>;
  slot atk-key-event-struct-string :: <C-string>;
  slot atk-key-event-struct-keycode :: <C-unsigned-short>;
  slot atk-key-event-struct-timestamp :: <C-unsigned-int>;
  pointer-type-name: <AtkKeyEventStruct>;
end C-struct;

define constant $atk-key-event-press = 0;
define constant $atk-key-event-release = 1;
define constant $atk-key-event-last-defined = 2;
define constant <AtkKeyEventType> = <C-int>;
define C-pointer-type <AtkKeyEventType*> => <AtkKeyEventType>;

define constant $atk-layer-invalid = 0;
define constant $atk-layer-background = 1;
define constant $atk-layer-canvas = 2;
define constant $atk-layer-widget = 3;
define constant $atk-layer-mdi = 4;
define constant $atk-layer-popup = 5;
define constant $atk-layer-overlay = 6;
define constant $atk-layer-window = 7;
define constant <AtkLayer> = <C-int>;
define C-pointer-type <AtkLayer*> => <AtkLayer>;

define constant $major-version = 2;

define constant $micro-version = 0;

define constant $minor-version = 8;

define open C-subtype <AtkMisc> (<GObject>)
  constant slot atk-misc-parent :: <GObject>;
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
  constant slot atk-misc-class-parent :: <GObjectClass>;
  constant slot atk-misc-class-threads-enter :: <C-function-pointer>;
  constant slot atk-misc-class-threads-leave :: <C-function-pointer>;
  constant slot atk-misc-class-vfuncs :: <C-void*>;
  pointer-type-name: <AtkMiscClass>;
end C-struct;

define open C-subtype <AtkNoOpObject> (<AtkObject>)
  constant slot atk-no-op-object-parent :: <AtkObject>;
end C-subtype;

define C-pointer-type <AtkNoOpObject*> => <AtkNoOpObject>;

define C-function atk-no-op-object-new
  input parameter obj_ :: <GObject>;
  result res :: <AtkObject>;
  c-name: "atk_no_op_object_new";
end;

define C-struct <_AtkNoOpObjectClass>
  constant slot atk-no-op-object-class-parent-class :: <AtkObjectClass>;
  pointer-type-name: <AtkNoOpObjectClass>;
end C-struct;

define open C-subtype <AtkNoOpObjectFactory> (<AtkObjectFactory>)
  constant slot atk-no-op-object-factory-parent :: <AtkObjectFactory>;
end C-subtype;

define C-pointer-type <AtkNoOpObjectFactory*> => <AtkNoOpObjectFactory>;

define C-function atk-no-op-object-factory-new
  result res :: <AtkObjectFactory>;
  c-name: "atk_no_op_object_factory_new";
end;

define C-struct <_AtkNoOpObjectFactoryClass>
  constant slot atk-no-op-object-factory-class-parent-class :: <AtkObjectFactoryClass>;
  pointer-type-name: <AtkNoOpObjectFactoryClass>;
end C-struct;

define open C-subtype <AtkObject> (<GObject>)
  constant slot atk-object-parent :: <GObject>;
  constant slot atk-object-description :: <C-string>;
  constant slot atk-object-name :: <C-string>;
  constant slot atk-object-accessible-parent :: <AtkObject>;
  constant slot atk-object-role :: <AtkRole>;
  constant slot atk-object-relation-set :: <AtkRelationSet>;
  constant slot atk-object-layer :: <AtkLayer>;
end C-subtype;

define C-pointer-type <AtkObject*> => <AtkObject>;

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
  constant slot atk-object-class-parent :: <GObjectClass>;
  constant slot atk-object-class-get-name :: <C-function-pointer>;
  constant slot atk-object-class-get-description :: <C-function-pointer>;
  constant slot atk-object-class-get-parent :: <C-function-pointer>;
  constant slot atk-object-class-get-n-children :: <C-function-pointer>;
  constant slot atk-object-class-ref-child :: <C-void*>;
  constant slot atk-object-class-get-index-in-parent :: <C-function-pointer>;
  constant slot atk-object-class-ref-relation-set :: <C-function-pointer>;
  constant slot atk-object-class-get-role :: <C-function-pointer>;
  constant slot atk-object-class-get-layer :: <C-function-pointer>;
  constant slot atk-object-class-get-mdi-zorder :: <C-function-pointer>;
  constant slot atk-object-class-ref-state-set :: <C-function-pointer>;
  constant slot atk-object-class-set-name :: <C-function-pointer>;
  constant slot atk-object-class-set-description :: <C-function-pointer>;
  constant slot atk-object-class-set-parent :: <C-function-pointer>;
  constant slot atk-object-class-set-role :: <C-function-pointer>;
  constant slot atk-object-class-connect-property-change-handler :: <C-void*>;
  constant slot atk-object-class-remove-property-change-handler :: <C-function-pointer>;
  constant slot atk-object-class-initialize :: <C-function-pointer>;
  constant slot atk-object-class-children-changed :: <C-function-pointer>;
  constant slot atk-object-class-focus-event :: <C-function-pointer>;
  constant slot atk-object-class-property-change :: <C-void*>;
  constant slot atk-object-class-state-change :: <C-function-pointer>;
  constant slot atk-object-class-visible-data-changed :: <C-function-pointer>;
  constant slot atk-object-class-active-descendant-changed :: <C-function-pointer>;
  constant slot atk-object-class-get-attributes :: <C-function-pointer>;
  constant slot atk-object-class-get-object-locale :: <C-function-pointer>;
  constant slot atk-object-class-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkObjectClass>;
end C-struct;

define open C-subtype <AtkObjectFactory> (<GObject>)
  constant slot atk-object-factory-parent :: <GObject>;
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
  constant slot atk-object-factory-class-parent-class :: <GObjectClass>;
  constant slot atk-object-factory-class-create-accessible :: <C-void*>;
  constant slot atk-object-factory-class-invalidate :: <C-function-pointer>;
  constant slot atk-object-factory-class-get-accessible-type :: <C-function-pointer>;
  constant slot atk-object-factory-class-pad1 :: <C-function-pointer>;
  constant slot atk-object-factory-class-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkObjectFactoryClass>;
end C-struct;

define open C-subtype <AtkPlug> (<AtkObject>)
  constant slot atk-plug-parent :: <AtkObject>;
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
  constant slot atk-plug-class-parent-class :: <AtkObjectClass>;
  constant slot atk-plug-class-get-object-id :: <C-function-pointer>;
  pointer-type-name: <AtkPlugClass>;
end C-struct;

define C-struct <_AtkRectangle>
  slot atk-rectangle-x :: <C-signed-int>;
  slot atk-rectangle-y :: <C-signed-int>;
  slot atk-rectangle-width :: <C-signed-int>;
  slot atk-rectangle-height :: <C-signed-int>;
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
  constant slot atk-relation-parent :: <GObject>;
  constant slot atk-relation-target :: <GPtrArray>;
  constant slot atk-relation-relationship :: <AtkRelationType>;
end C-subtype;

define C-pointer-type <AtkRelation*> => <AtkRelation>;

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
  constant slot atk-relation-class-parent :: <GObjectClass>;
  pointer-type-name: <AtkRelationClass>;
end C-struct;

define open C-subtype <AtkRelationSet> (<GObject>)
  constant slot atk-relation-set-parent :: <GObject>;
  constant slot atk-relation-set-relations :: <GPtrArray>;
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
  constant slot atk-relation-set-class-parent :: <GObjectClass>;
  constant slot atk-relation-set-class-pad1 :: <C-function-pointer>;
  constant slot atk-relation-set-class-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkRelationSetClass>;
end C-struct;

define constant $atk-relation-null = 0;
define constant $atk-relation-controlled-by = 1;
define constant $atk-relation-controller-for = 2;
define constant $atk-relation-label-for = 3;
define constant $atk-relation-labelled-by = 4;
define constant $atk-relation-member-of = 5;
define constant $atk-relation-node-child-of = 6;
define constant $atk-relation-flows-to = 7;
define constant $atk-relation-flows-from = 8;
define constant $atk-relation-subwindow-of = 9;
define constant $atk-relation-embeds = 10;
define constant $atk-relation-embedded-by = 11;
define constant $atk-relation-popup-for = 12;
define constant $atk-relation-parent-window-of = 13;
define constant $atk-relation-described-by = 14;
define constant $atk-relation-description-for = 15;
define constant $atk-relation-node-parent-of = 16;
define constant $atk-relation-last-defined = 17;
define constant <AtkRelationType> = <C-int>;
define C-pointer-type <AtkRelationType*> => <AtkRelationType>;

define constant $atk-role-invalid = 0;
define constant $atk-role-accel-label = 1;
define constant $atk-role-alert = 2;
define constant $atk-role-animation = 3;
define constant $atk-role-arrow = 4;
define constant $atk-role-calendar = 5;
define constant $atk-role-canvas = 6;
define constant $atk-role-check-box = 7;
define constant $atk-role-check-menu-item = 8;
define constant $atk-role-color-chooser = 9;
define constant $atk-role-column-header = 10;
define constant $atk-role-combo-box = 11;
define constant $atk-role-date-editor = 12;
define constant $atk-role-desktop-icon = 13;
define constant $atk-role-desktop-frame = 14;
define constant $atk-role-dial = 15;
define constant $atk-role-dialog = 16;
define constant $atk-role-directory-pane = 17;
define constant $atk-role-drawing-area = 18;
define constant $atk-role-file-chooser = 19;
define constant $atk-role-filler = 20;
define constant $atk-role-font-chooser = 21;
define constant $atk-role-frame = 22;
define constant $atk-role-glass-pane = 23;
define constant $atk-role-html-container = 24;
define constant $atk-role-icon = 25;
define constant $atk-role-image = 26;
define constant $atk-role-internal-frame = 27;
define constant $atk-role-label = 28;
define constant $atk-role-layered-pane = 29;
define constant $atk-role-list = 30;
define constant $atk-role-list-item = 31;
define constant $atk-role-menu = 32;
define constant $atk-role-menu-bar = 33;
define constant $atk-role-menu-item = 34;
define constant $atk-role-option-pane = 35;
define constant $atk-role-page-tab = 36;
define constant $atk-role-page-tab-list = 37;
define constant $atk-role-panel = 38;
define constant $atk-role-password-text = 39;
define constant $atk-role-popup-menu = 40;
define constant $atk-role-progress-bar = 41;
define constant $atk-role-push-button = 42;
define constant $atk-role-radio-button = 43;
define constant $atk-role-radio-menu-item = 44;
define constant $atk-role-root-pane = 45;
define constant $atk-role-row-header = 46;
define constant $atk-role-scroll-bar = 47;
define constant $atk-role-scroll-pane = 48;
define constant $atk-role-separator = 49;
define constant $atk-role-slider = 50;
define constant $atk-role-split-pane = 51;
define constant $atk-role-spin-button = 52;
define constant $atk-role-statusbar = 53;
define constant $atk-role-table = 54;
define constant $atk-role-table-cell = 55;
define constant $atk-role-table-column-header = 56;
define constant $atk-role-table-row-header = 57;
define constant $atk-role-tear-off-menu-item = 58;
define constant $atk-role-terminal = 59;
define constant $atk-role-text = 60;
define constant $atk-role-toggle-button = 61;
define constant $atk-role-tool-bar = 62;
define constant $atk-role-tool-tip = 63;
define constant $atk-role-tree = 64;
define constant $atk-role-tree-table = 65;
define constant $atk-role-unknown = 66;
define constant $atk-role-viewport = 67;
define constant $atk-role-window = 68;
define constant $atk-role-header = 69;
define constant $atk-role-footer = 70;
define constant $atk-role-paragraph = 71;
define constant $atk-role-ruler = 72;
define constant $atk-role-application = 73;
define constant $atk-role-autocomplete = 74;
define constant $atk-role-editbar = 75;
define constant $atk-role-embedded = 76;
define constant $atk-role-entry = 77;
define constant $atk-role-chart = 78;
define constant $atk-role-caption = 79;
define constant $atk-role-document-frame = 80;
define constant $atk-role-heading = 81;
define constant $atk-role-page = 82;
define constant $atk-role-section = 83;
define constant $atk-role-redundant-object = 84;
define constant $atk-role-form = 85;
define constant $atk-role-link = 86;
define constant $atk-role-input-method-window = 87;
define constant $atk-role-table-row = 88;
define constant $atk-role-tree-item = 89;
define constant $atk-role-document-spreadsheet = 90;
define constant $atk-role-document-presentation = 91;
define constant $atk-role-document-text = 92;
define constant $atk-role-document-web = 93;
define constant $atk-role-document-email = 94;
define constant $atk-role-comment = 95;
define constant $atk-role-list-box = 96;
define constant $atk-role-grouping = 97;
define constant $atk-role-image-map = 98;
define constant $atk-role-notification = 99;
define constant $atk-role-info-bar = 100;
define constant $atk-role-level-bar = 101;
define constant $atk-role-last-defined = 102;
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
  constant slot atk-selection-iface-parent :: <GTypeInterface>;
  constant slot atk-selection-iface-add-selection :: <C-function-pointer>;
  constant slot atk-selection-iface-clear-selection :: <C-function-pointer>;
  constant slot atk-selection-iface-ref-selection :: <C-function-pointer>;
  constant slot atk-selection-iface-get-selection-count :: <C-function-pointer>;
  constant slot atk-selection-iface-is-child-selected :: <C-function-pointer>;
  constant slot atk-selection-iface-remove-selection :: <C-function-pointer>;
  constant slot atk-selection-iface-select-all-selection :: <C-function-pointer>;
  constant slot atk-selection-iface-selection-changed :: <C-function-pointer>;
  constant slot atk-selection-iface-pad1 :: <C-function-pointer>;
  constant slot atk-selection-iface-pad2 :: <C-function-pointer>;
  pointer-type-name: <AtkSelectionIface>;
end C-struct;

define open C-subtype <AtkSocket> (<AtkObject>)
  constant slot atk-socket-parent :: <AtkObject>;
  constant slot atk-socket-embedded-plug-id :: <C-string>;
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
  constant slot atk-socket-class-parent-class :: <AtkObjectClass>;
  constant slot atk-socket-class-embed :: <C-function-pointer>;
  pointer-type-name: <AtkSocketClass>;
end C-struct;

define open C-subtype <AtkStateSet> (<GObject>)
  constant slot atk-state-set-parent :: <GObject>;
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
  constant slot atk-state-set-class-parent :: <GObjectClass>;
  pointer-type-name: <AtkStateSetClass>;
end C-struct;

define constant $atk-state-invalid = 0;
define constant $atk-state-active = 1;
define constant $atk-state-armed = 2;
define constant $atk-state-busy = 3;
define constant $atk-state-checked = 4;
define constant $atk-state-defunct = 5;
define constant $atk-state-editable = 6;
define constant $atk-state-enabled = 7;
define constant $atk-state-expandable = 8;
define constant $atk-state-expanded = 9;
define constant $atk-state-focusable = 10;
define constant $atk-state-focused = 11;
define constant $atk-state-horizontal = 12;
define constant $atk-state-iconified = 13;
define constant $atk-state-modal = 14;
define constant $atk-state-multi-line = 15;
define constant $atk-state-multiselectable = 16;
define constant $atk-state-opaque = 17;
define constant $atk-state-pressed = 18;
define constant $atk-state-resizable = 19;
define constant $atk-state-selectable = 20;
define constant $atk-state-selected = 21;
define constant $atk-state-sensitive = 22;
define constant $atk-state-showing = 23;
define constant $atk-state-single-line = 24;
define constant $atk-state-stale = 25;
define constant $atk-state-transient = 26;
define constant $atk-state-vertical = 27;
define constant $atk-state-visible = 28;
define constant $atk-state-manages-descendants = 29;
define constant $atk-state-indeterminate = 30;
define constant $atk-state-truncated = 31;
define constant $atk-state-required = 32;
define constant $atk-state-invalid-entry = 33;
define constant $atk-state-supports-autocompletion = 34;
define constant $atk-state-selectable-text = 35;
define constant $atk-state-default = 36;
define constant $atk-state-animated = 37;
define constant $atk-state-visited = 38;
define constant $atk-state-last-defined = 39;
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
  constant slot atk-streamable-content-iface-parent :: <GTypeInterface>;
  constant slot atk-streamable-content-iface-get-n-mime-types :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-get-mime-type :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-get-stream :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-get-uri :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-pad1 :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-pad2 :: <C-function-pointer>;
  constant slot atk-streamable-content-iface-pad3 :: <C-function-pointer>;
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
  constant slot atk-table-iface-parent :: <GTypeInterface>;
  constant slot atk-table-iface-ref-at :: <C-function-pointer>;
  constant slot atk-table-iface-get-index-at :: <C-function-pointer>;
  constant slot atk-table-iface-get-column-at-index :: <C-function-pointer>;
  constant slot atk-table-iface-get-row-at-index :: <C-function-pointer>;
  constant slot atk-table-iface-get-n-columns :: <C-function-pointer>;
  constant slot atk-table-iface-get-n-rows :: <C-function-pointer>;
  constant slot atk-table-iface-get-column-extent-at :: <C-function-pointer>;
  constant slot atk-table-iface-get-row-extent-at :: <C-function-pointer>;
  constant slot atk-table-iface-get-caption :: <C-function-pointer>;
  constant slot atk-table-iface-get-column-description :: <C-function-pointer>;
  constant slot atk-table-iface-get-column-header :: <C-function-pointer>;
  constant slot atk-table-iface-get-row-description :: <C-function-pointer>;
  constant slot atk-table-iface-get-row-header :: <C-function-pointer>;
  constant slot atk-table-iface-get-summary :: <C-function-pointer>;
  constant slot atk-table-iface-set-caption :: <C-function-pointer>;
  constant slot atk-table-iface-set-column-description :: <C-function-pointer>;
  constant slot atk-table-iface-set-column-header :: <C-function-pointer>;
  constant slot atk-table-iface-set-row-description :: <C-function-pointer>;
  constant slot atk-table-iface-set-row-header :: <C-function-pointer>;
  constant slot atk-table-iface-set-summary :: <C-function-pointer>;
  constant slot atk-table-iface-get-selected-columns :: <C-function-pointer>;
  constant slot atk-table-iface-get-selected-rows :: <C-function-pointer>;
  constant slot atk-table-iface-is-column-selected :: <C-function-pointer>;
  constant slot atk-table-iface-is-row-selected :: <C-function-pointer>;
  constant slot atk-table-iface-is-selected :: <C-function-pointer>;
  constant slot atk-table-iface-add-row-selection :: <C-function-pointer>;
  constant slot atk-table-iface-remove-row-selection :: <C-function-pointer>;
  constant slot atk-table-iface-add-column-selection :: <C-function-pointer>;
  constant slot atk-table-iface-remove-column-selection :: <C-function-pointer>;
  constant slot atk-table-iface-row-inserted :: <C-function-pointer>;
  constant slot atk-table-iface-column-inserted :: <C-function-pointer>;
  constant slot atk-table-iface-row-deleted :: <C-function-pointer>;
  constant slot atk-table-iface-column-deleted :: <C-function-pointer>;
  constant slot atk-table-iface-row-reordered :: <C-function-pointer>;
  constant slot atk-table-iface-column-reordered :: <C-function-pointer>;
  constant slot atk-table-iface-model-changed :: <C-function-pointer>;
  constant slot atk-table-iface-pad1 :: <C-function-pointer>;
  constant slot atk-table-iface-pad2 :: <C-function-pointer>;
  constant slot atk-table-iface-pad3 :: <C-function-pointer>;
  constant slot atk-table-iface-pad4 :: <C-function-pointer>;
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

define constant $atk-text-attr-invalid = 0;
define constant $atk-text-attr-left-margin = 1;
define constant $atk-text-attr-right-margin = 2;
define constant $atk-text-attr-indent = 3;
define constant $atk-text-attr-invisible = 4;
define constant $atk-text-attr-editable = 5;
define constant $atk-text-attr-pixels-above-lines = 6;
define constant $atk-text-attr-pixels-below-lines = 7;
define constant $atk-text-attr-pixels-inside-wrap = 8;
define constant $atk-text-attr-bg-full-height = 9;
define constant $atk-text-attr-rise = 10;
define constant $atk-text-attr-underline = 11;
define constant $atk-text-attr-strikethrough = 12;
define constant $atk-text-attr-size = 13;
define constant $atk-text-attr-scale = 14;
define constant $atk-text-attr-weight = 15;
define constant $atk-text-attr-language = 16;
define constant $atk-text-attr-family-name = 17;
define constant $atk-text-attr-bg-color = 18;
define constant $atk-text-attr-fg-color = 19;
define constant $atk-text-attr-bg-stipple = 20;
define constant $atk-text-attr-fg-stipple = 21;
define constant $atk-text-attr-wrap-mode = 22;
define constant $atk-text-attr-direction = 23;
define constant $atk-text-attr-justification = 24;
define constant $atk-text-attr-stretch = 25;
define constant $atk-text-attr-variant = 26;
define constant $atk-text-attr-style = 27;
define constant $atk-text-attr-last-defined = 28;
define constant <AtkTextAttribute> = <C-int>;
define C-pointer-type <AtkTextAttribute*> => <AtkTextAttribute>;

define constant $atk-text-boundary-char = 0;
define constant $atk-text-boundary-word-start = 1;
define constant $atk-text-boundary-word-end = 2;
define constant $atk-text-boundary-sentence-start = 3;
define constant $atk-text-boundary-sentence-end = 4;
define constant $atk-text-boundary-line-start = 5;
define constant $atk-text-boundary-line-end = 6;
define constant <AtkTextBoundary> = <C-int>;
define C-pointer-type <AtkTextBoundary*> => <AtkTextBoundary>;

define constant $atk-text-clip-none = 0;
define constant $atk-text-clip-min = 1;
define constant $atk-text-clip-max = 2;
define constant $atk-text-clip-both = 3;
define constant <AtkTextClipType> = <C-int>;
define C-pointer-type <AtkTextClipType*> => <AtkTextClipType>;

define C-struct <_AtkTextIface>
  constant slot atk-text-iface-parent :: <GTypeInterface>;
  constant slot atk-text-iface-get-text :: <C-function-pointer>;
  constant slot atk-text-iface-get-text-after-offset :: <C-function-pointer>;
  constant slot atk-text-iface-get-text-at-offset :: <C-function-pointer>;
  constant slot atk-text-iface-get-character-at-offset :: <C-function-pointer>;
  constant slot atk-text-iface-get-text-before-offset :: <C-function-pointer>;
  constant slot atk-text-iface-get-caret-offset :: <C-function-pointer>;
  constant slot atk-text-iface-get-run-attributes :: <C-function-pointer>;
  constant slot atk-text-iface-get-default-attributes :: <C-function-pointer>;
  constant slot atk-text-iface-get-character-extents :: <C-function-pointer>;
  constant slot atk-text-iface-get-character-count :: <C-function-pointer>;
  constant slot atk-text-iface-get-offset-at-point :: <C-function-pointer>;
  constant slot atk-text-iface-get-n-selections :: <C-function-pointer>;
  constant slot atk-text-iface-get-selection :: <C-function-pointer>;
  constant slot atk-text-iface-add-selection :: <C-function-pointer>;
  constant slot atk-text-iface-remove-selection :: <C-function-pointer>;
  constant slot atk-text-iface-set-selection :: <C-function-pointer>;
  constant slot atk-text-iface-set-caret-offset :: <C-function-pointer>;
  constant slot atk-text-iface-text-changed :: <C-function-pointer>;
  constant slot atk-text-iface-text-caret-moved :: <C-function-pointer>;
  constant slot atk-text-iface-text-selection-changed :: <C-function-pointer>;
  constant slot atk-text-iface-text-attributes-changed :: <C-function-pointer>;
  constant slot atk-text-iface-get-range-extents :: <C-function-pointer>;
  constant slot atk-text-iface-get-bounded-ranges :: <C-function-pointer>;
  constant slot atk-text-iface-pad4 :: <C-function-pointer>;
  pointer-type-name: <AtkTextIface>;
end C-struct;

define C-struct <_AtkTextRange>
  slot atk-text-range-bounds :: <AtkTextRectangle>;
  slot atk-text-range-start-offset :: <C-signed-int>;
  slot atk-text-range-end-offset :: <C-signed-int>;
  slot atk-text-range-content :: <C-string>;
  pointer-type-name: <AtkTextRange>;
end C-struct;

define C-struct <_AtkTextRectangle>
  slot atk-text-rectangle-x :: <C-signed-int>;
  slot atk-text-rectangle-y :: <C-signed-int>;
  slot atk-text-rectangle-width :: <C-signed-int>;
  slot atk-text-rectangle-height :: <C-signed-int>;
  pointer-type-name: <AtkTextRectangle>;
end C-struct;

define open C-subtype <AtkUtil> (<GObject>)
  constant slot atk-util-parent :: <GObject>;
end C-subtype;

define C-pointer-type <AtkUtil*> => <AtkUtil>;

define C-struct <_AtkUtilClass>
  constant slot atk-util-class-parent :: <GObjectClass>;
  constant slot atk-util-class-add-global-event-listener :: <C-void*>;
  constant slot atk-util-class-remove-global-event-listener :: <C-function-pointer>;
  constant slot atk-util-class-add-key-event-listener :: <C-void*>;
  constant slot atk-util-class-remove-key-event-listener :: <C-function-pointer>;
  constant slot atk-util-class-get-root :: <C-void*>;
  constant slot atk-util-class-get-toolkit-name :: <C-function-pointer>;
  constant slot atk-util-class-get-toolkit-version :: <C-function-pointer>;
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
  constant slot atk-value-iface-parent :: <GTypeInterface>;
  constant slot atk-value-iface-get-current-value :: <C-function-pointer>;
  constant slot atk-value-iface-get-maximum-value :: <C-function-pointer>;
  constant slot atk-value-iface-get-minimum-value :: <C-function-pointer>;
  constant slot atk-value-iface-set-current-value :: <C-function-pointer>;
  constant slot atk-value-iface-get-minimum-increment :: <C-function-pointer>;
  constant slot atk-value-iface-pad1 :: <C-function-pointer>;
  pointer-type-name: <AtkValueIface>;
end C-struct;

// Interface
define open C-subtype <AtkWindow> (<AtkObject>)
end C-subtype;

define C-pointer-type <AtkWindow*> => <AtkWindow>;

define C-struct <_AtkWindowIface>
  constant slot atk-window-iface-parent :: <GTypeInterface>;
  constant slot atk-window-iface-_padding-dummy :: <C-void*>;
  pointer-type-name: <AtkWindowIface>;
end C-struct;

define C-struct <_Atk_PropertyValues>
  slot atk-_property-values-property-name :: <C-string>;
  slot atk-_property-values-old-value :: <GValue>;
  slot atk-_property-values-new-value :: <GValue>;
  pointer-type-name: <Atk_PropertyValues>;
end C-struct;

define C-struct <_Atk_Registry>
  slot atk-_registry-parent :: <GObject>;
  slot atk-_registry-factory-type-registry :: <GHashTable>;
  slot atk-_registry-factory-singleton-cache :: <GHashTable>;
  pointer-type-name: <Atk_Registry>;
end C-struct;

define C-struct <_Atk_RegistryClass>
  slot atk-_registry-class-parent-class :: <GObjectClass>;
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

