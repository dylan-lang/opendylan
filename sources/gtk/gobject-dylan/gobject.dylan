module: gobject
synopsis: generated bindings for the GObject library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define open C-subtype <GBinding> (<GObject>)
end C-subtype;

define C-pointer-type <GBinding*> => <GBinding>;

define C-function g-binding-get-flags
  input parameter self :: <GBinding>;
  result res :: <GBindingFlags>;
  c-name: "g_binding_get_flags";
end;

define C-function g-binding-get-source
  input parameter self :: <GBinding>;
  result res :: <GObject>;
  c-name: "g_binding_get_source";
end;

define C-function g-binding-get-source-property
  input parameter self :: <GBinding>;
  result res :: <C-string>;
  c-name: "g_binding_get_source_property";
end;

define C-function g-binding-get-target
  input parameter self :: <GBinding>;
  result res :: <GObject>;
  c-name: "g_binding_get_target";
end;

define C-function g-binding-get-target-property
  input parameter self :: <GBinding>;
  result res :: <C-string>;
  c-name: "g_binding_get_target_property";
end;

define constant $G-BINDING-DEFAULT = 0;
define constant $G-BINDING-BIDIRECTIONAL = 1;
define constant $G-BINDING-SYNC-CREATE = 2;
define constant $G-BINDING-INVERT-BOOLEAN = 4;
define constant <GBindingFlags> = <C-int>;
define C-pointer-type <GBindingFlags*> => <GBindingFlags>;

define C-struct <_GCClosure>
  slot gcclosure-closure :: <GClosure>;
  slot gcclosure-callback :: <C-void*>;
  pointer-type-name: <GCClosure>;
end C-struct;

define C-function g-cclosure-marshal-BOOLEAN--BOXED-BOXED
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_BOOLEAN__BOXED_BOXED";
end;

define C-function g-cclosure-marshal-BOOLEAN--FLAGS
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_BOOLEAN__FLAGS";
end;

define C-function g-cclosure-marshal-STRING--OBJECT-POINTER
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_STRING__OBJECT_POINTER";
end;

define C-function g-cclosure-marshal-VOID--BOOLEAN
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__BOOLEAN";
end;

define C-function g-cclosure-marshal-VOID--BOXED
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__BOXED";
end;

define C-function g-cclosure-marshal-VOID--CHAR
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__CHAR";
end;

define C-function g-cclosure-marshal-VOID--DOUBLE
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__DOUBLE";
end;

define C-function g-cclosure-marshal-VOID--ENUM
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__ENUM";
end;

define C-function g-cclosure-marshal-VOID--FLAGS
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__FLAGS";
end;

define C-function g-cclosure-marshal-VOID--FLOAT
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__FLOAT";
end;

define C-function g-cclosure-marshal-VOID--INT
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__INT";
end;

define C-function g-cclosure-marshal-VOID--LONG
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__LONG";
end;

define C-function g-cclosure-marshal-VOID--OBJECT
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__OBJECT";
end;

define C-function g-cclosure-marshal-VOID--PARAM
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__PARAM";
end;

define C-function g-cclosure-marshal-VOID--POINTER
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__POINTER";
end;

define C-function g-cclosure-marshal-VOID--STRING
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__STRING";
end;

define C-function g-cclosure-marshal-VOID--UCHAR
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__UCHAR";
end;

define C-function g-cclosure-marshal-VOID--UINT
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__UINT";
end;

define C-function g-cclosure-marshal-VOID--UINT-POINTER
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__UINT_POINTER";
end;

define C-function g-cclosure-marshal-VOID--ULONG
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__ULONG";
end;

define C-function g-cclosure-marshal-VOID--VARIANT
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__VARIANT";
end;

define C-function g-cclosure-marshal-VOID--VOID
  input parameter closure_ :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_VOID__VOID";
end;

define C-function g-cclosure-marshal-generic
  input parameter closure_ :: <GClosure>;
  input parameter return_gvalue_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <GValue>;
  input parameter invocation_hint_ :: <C-void*>;
  input parameter marshal_data_ :: <C-void*>;
  c-name: "g_cclosure_marshal_generic";
end;

define C-struct <_GClosure>
  constant slot gclosure-ref-count :: <C-unsigned-int>;
  constant slot gclosure-meta-marshal-nouse :: <C-unsigned-int>;
  constant slot gclosure-n-guards :: <C-unsigned-int>;
  constant slot gclosure-n-fnotifiers :: <C-unsigned-int>;
  constant slot gclosure-n-inotifiers :: <C-unsigned-int>;
  constant slot gclosure-in-inotify :: <C-unsigned-int>;
  constant slot gclosure-floating :: <C-unsigned-int>;
  constant slot gclosure-derivative-flag :: <C-unsigned-int>;
  slot gclosure-in-marshal :: <C-unsigned-int>;
  slot gclosure-is-invalid :: <C-unsigned-int>;
  constant slot gclosure-marshal :: <C-function-pointer>;
  constant slot gclosure-data :: <C-void*>;
  constant slot gclosure-notifiers :: <GClosureNotifyData>;
  pointer-type-name: <GClosure>;
end C-struct;

define C-function g-closure-new-object
  input parameter sizeof_closure_ :: <C-unsigned-int>;
  input parameter object_ :: <GObject>;
  result res :: <GClosure>;
  c-name: "g_closure_new_object";
end;

define C-function g-closure-new-simple
  input parameter sizeof_closure_ :: <C-unsigned-int>;
  input parameter data_ :: <C-void*>;
  result res :: <GClosure>;
  c-name: "g_closure_new_simple";
end;

define C-function g-closure-invalidate
  input parameter self :: <GClosure>;
  c-name: "g_closure_invalidate";
end;

define C-function g-closure-invoke
  input parameter self :: <GClosure>;
  input parameter return_value_ :: <GValue>;
  input parameter n_param_values_ :: <C-unsigned-int>;
  input parameter param_values_ :: <C-unsigned-char*> /* Not supported */;
  input parameter invocation_hint_ :: <C-void*>;
  c-name: "g_closure_invoke";
end;

define C-function g-closure-ref
  input parameter self :: <GClosure>;
  result res :: <GClosure>;
  c-name: "g_closure_ref";
end;

define C-function g-closure-sink
  input parameter self :: <GClosure>;
  c-name: "g_closure_sink";
end;

define C-function g-closure-unref
  input parameter self :: <GClosure>;
  c-name: "g_closure_unref";
end;

define C-struct <_GClosureNotifyData>
  slot gclosurenotifydata-data :: <C-void*>;
  slot gclosurenotifydata-notify :: <C-function-pointer>;
  pointer-type-name: <GClosureNotifyData>;
end C-struct;

define constant $G-CONNECT-AFTER = 1;
define constant $G-CONNECT-SWAPPED = 2;
define constant <GConnectFlags> = <C-int>;
define C-pointer-type <GConnectFlags*> => <GConnectFlags>;

define C-struct <_GEnumClass>
  slot genumclass-g-type-class :: <GTypeClass>;
  slot genumclass-minimum :: <C-signed-int>;
  slot genumclass-maximum :: <C-signed-int>;
  slot genumclass-n-values :: <C-unsigned-int>;
  slot genumclass-values :: <GEnumValue>;
  pointer-type-name: <GEnumClass>;
end C-struct;

define C-struct <_GEnumValue>
  slot genumvalue-value :: <C-signed-int>;
  slot genumvalue-value-name :: <C-string>;
  slot genumvalue-value-nick :: <C-string>;
  pointer-type-name: <GEnumValue>;
end C-struct;

define C-struct <_GFlagsClass>
  slot gflagsclass-g-type-class :: <GTypeClass>;
  slot gflagsclass-mask :: <C-unsigned-int>;
  slot gflagsclass-n-values :: <C-unsigned-int>;
  slot gflagsclass-values :: <GFlagsValue>;
  pointer-type-name: <GFlagsClass>;
end C-struct;

define C-struct <_GFlagsValue>
  slot gflagsvalue-value :: <C-unsigned-int>;
  slot gflagsvalue-value-name :: <C-string>;
  slot gflagsvalue-value-nick :: <C-string>;
  pointer-type-name: <GFlagsValue>;
end C-struct;

define open C-subtype <GInitiallyUnowned> (<GObject>)
  constant slot ginitiallyunowned-g-type-instance :: <GTypeInstance>;
  constant slot ginitiallyunowned-ref-count :: <C-unsigned-int>;
  constant slot ginitiallyunowned-qdata :: <GData>;
end C-subtype;

define C-pointer-type <GInitiallyUnowned*> => <GInitiallyUnowned>;

define C-struct <_GInitiallyUnownedClass>
  constant slot ginitiallyunownedclass-g-type-class :: <GTypeClass>;
  constant slot ginitiallyunownedclass-construct-properties :: <GSList>;
  constant slot ginitiallyunownedclass-constructor :: <C-void*>;
  constant slot ginitiallyunownedclass-set-property :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-get-property :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-dispose :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-finalize :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-dispatch-properties-changed :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-notify :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-constructed :: <C-function-pointer>;
  constant slot ginitiallyunownedclass-flags :: <C-unsigned-long>;
  constant slot ginitiallyunownedclass-pdummy :: <C-void*>;
  pointer-type-name: <GInitiallyUnownedClass>;
end C-struct;

define C-struct <_GInterfaceInfo>
  slot ginterfaceinfo-interface-init :: <C-function-pointer>;
  slot ginterfaceinfo-interface-finalize :: <C-function-pointer>;
  slot ginterfaceinfo-interface-data :: <C-void*>;
  pointer-type-name: <GInterfaceInfo>;
end C-struct;

define open C-subtype <GObject> (<C-void*>)
  constant slot gobject-g-type-instance :: <GTypeInstance>;
  constant slot gobject-ref-count :: <C-unsigned-int>;
  constant slot gobject-qdata :: <GData>;
end C-subtype;

define C-pointer-type <GObject*> => <GObject>;

define C-function g-object-newv
  input parameter object_type_ :: <C-long>;
  input parameter n_parameters_ :: <C-unsigned-int>;
  input parameter parameters_ :: <C-unsigned-char*> /* Not supported */;
  result res :: <GObject>;
  c-name: "g_object_newv";
end;

define C-function g-object-compat-control
  input parameter what_ :: <C-unsigned-long>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-long>;
  c-name: "g_object_compat_control";
end;

define C-function g-object-interface-find-property
  input parameter g_iface_ :: <C-void*>;
  input parameter property_name_ :: <C-string>;
  result res :: <GParamSpec>;
  c-name: "g_object_interface_find_property";
end;

define C-function g-object-interface-install-property
  input parameter g_iface_ :: <C-void*>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "g_object_interface_install_property";
end;

define C-function g-object-interface-list-properties
  input parameter g_iface_ :: <C-void*>;
  output parameter n_properties_p_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "g_object_interface_list_properties";
end;

define C-function g-object-bind-property
  input parameter self :: <GObject>;
  input parameter source_property_ :: <C-string>;
  input parameter target_ :: <GObject>;
  input parameter target_property_ :: <C-string>;
  input parameter flags_ :: <GBindingFlags>;
  result res :: <GBinding>;
  c-name: "g_object_bind_property";
end;

define C-function g-object-bind-property-with-closures
  input parameter self :: <GObject>;
  input parameter source_property_ :: <C-string>;
  input parameter target_ :: <GObject>;
  input parameter target_property_ :: <C-string>;
  input parameter flags_ :: <GBindingFlags>;
  input parameter transform_to_ :: <GClosure>;
  input parameter transform_from_ :: <GClosure>;
  result res :: <GBinding>;
  c-name: "g_object_bind_property_with_closures";
end;

define C-function g-object-force-floating
  input parameter self :: <GObject>;
  c-name: "g_object_force_floating";
end;

define C-function g-object-freeze-notify
  input parameter self :: <GObject>;
  c-name: "g_object_freeze_notify";
end;

define C-function g-object-get-data
  input parameter self :: <GObject>;
  input parameter key_ :: <C-string>;
  result res :: <C-void*>;
  c-name: "g_object_get_data";
end;

define C-function g-object-get-property
  input parameter self :: <GObject>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "g_object_get_property";
end;

define C-function g-object-get-qdata
  input parameter self :: <GObject>;
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-void*>;
  c-name: "g_object_get_qdata";
end;

define C-function g-object-is-floating
  input parameter self :: <GObject>;
  result res :: <C-boolean>;
  c-name: "g_object_is_floating";
end;

define C-function g-object-notify
  input parameter self :: <GObject>;
  input parameter property_name_ :: <C-string>;
  c-name: "g_object_notify";
end;

define C-function g-object-notify-by-pspec
  input parameter self :: <GObject>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "g_object_notify_by_pspec";
end;

define C-function g-object-ref
  input parameter self :: <GObject>;
  result res :: <GObject>;
  c-name: "g_object_ref";
end;

define C-function g-object-ref-sink
  input parameter self :: <GObject>;
  result res :: <GObject>;
  c-name: "g_object_ref_sink";
end;

define C-function g-object-replace-data
  input parameter self :: <GObject>;
  input parameter key_ :: <C-string>;
  input parameter oldval_ :: <C-void*>;
  input parameter newval_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  input parameter old_destroy_ :: <C-function-pointer>;
  result res :: <C-boolean>;
  c-name: "g_object_replace_data";
end;

define C-function g-object-replace-qdata
  input parameter self :: <GObject>;
  input parameter quark_ :: <C-unsigned-int>;
  input parameter oldval_ :: <C-void*>;
  input parameter newval_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  input parameter old_destroy_ :: <C-function-pointer>;
  result res :: <C-boolean>;
  c-name: "g_object_replace_qdata";
end;

define C-function g-object-run-dispose
  input parameter self :: <GObject>;
  c-name: "g_object_run_dispose";
end;

define C-function g-object-set-data
  input parameter self :: <GObject>;
  input parameter key_ :: <C-string>;
  input parameter data_ :: <C-void*>;
  c-name: "g_object_set_data";
end;

define C-function g-object-set-property
  input parameter self :: <GObject>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "g_object_set_property";
end;

define C-function g-object-steal-data
  input parameter self :: <GObject>;
  input parameter key_ :: <C-string>;
  result res :: <C-void*>;
  c-name: "g_object_steal_data";
end;

define C-function g-object-steal-qdata
  input parameter self :: <GObject>;
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-void*>;
  c-name: "g_object_steal_qdata";
end;

define C-function g-object-thaw-notify
  input parameter self :: <GObject>;
  c-name: "g_object_thaw_notify";
end;

define C-function g-object-unref
  input parameter self :: <GObject>;
  c-name: "g_object_unref";
end;

define C-function g-object-watch-closure
  input parameter self :: <GObject>;
  input parameter closure_ :: <GClosure>;
  c-name: "g_object_watch_closure";
end;

define C-struct <_GObjectClass>
  constant slot gobjectclass-g-type-class :: <GTypeClass>;
  constant slot gobjectclass-construct-properties :: <GSList>;
  constant slot gobjectclass-constructor :: <C-void*>;
  constant slot gobjectclass-set-property :: <C-function-pointer>;
  constant slot gobjectclass-get-property :: <C-function-pointer>;
  constant slot gobjectclass-dispose :: <C-function-pointer>;
  constant slot gobjectclass-finalize :: <C-function-pointer>;
  constant slot gobjectclass-dispatch-properties-changed :: <C-function-pointer>;
  constant slot gobjectclass-notify :: <C-function-pointer>;
  constant slot gobjectclass-constructed :: <C-function-pointer>;
  constant slot gobjectclass-flags :: <C-unsigned-long>;
  constant slot gobjectclass-pdummy :: <C-void*>;
  pointer-type-name: <GObjectClass>;
end C-struct;

define C-function g-object-class-find-property
  input parameter self :: <GObjectClass>;
  input parameter property_name_ :: <C-string>;
  result res :: <GParamSpec>;
  c-name: "g_object_class_find_property";
end;

define C-function g-object-class-install-properties
  input parameter self :: <GObjectClass>;
  input parameter n_pspecs_ :: <C-unsigned-int>;
  input parameter pspecs_ :: <C-unsigned-char*> /* Not supported */;
  c-name: "g_object_class_install_properties";
end;

define C-function g-object-class-install-property
  input parameter self :: <GObjectClass>;
  input parameter property_id_ :: <C-unsigned-int>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "g_object_class_install_property";
end;

define C-function g-object-class-list-properties
  input parameter self :: <GObjectClass>;
  output parameter n_properties_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "g_object_class_list_properties";
end;

define C-function g-object-class-override-property
  input parameter self :: <GObjectClass>;
  input parameter property_id_ :: <C-unsigned-int>;
  input parameter name_ :: <C-string>;
  c-name: "g_object_class_override_property";
end;

define C-struct <_GObjectConstructParam>
  slot gobjectconstructparam-pspec :: <GParamSpec>;
  slot gobjectconstructparam-value :: <GValue>;
  pointer-type-name: <GObjectConstructParam>;
end C-struct;

define constant $PARAM-MASK = 255;

define constant $PARAM-READWRITE = 0;

define constant $PARAM-STATIC-STRINGS = 0;

define constant $PARAM-USER-SHIFT = 8;

define constant $G-PARAM-READABLE = 1;
define constant $G-PARAM-WRITABLE = 2;
define constant $G-PARAM-CONSTRUCT = 4;
define constant $G-PARAM-CONSTRUCT-ONLY = 8;
define constant $G-PARAM-LAX-VALIDATION = 16;
define constant $G-PARAM-STATIC-NAME = 32;
define constant $G-PARAM-PRIVATE = 32;
define constant $G-PARAM-STATIC-NICK = 64;
define constant $G-PARAM-STATIC-BLURB = 128;
define constant $G-PARAM-DEPRECATED = 2147483648;
define constant <GParamFlags> = <C-int>;
define C-pointer-type <GParamFlags*> => <GParamFlags>;

define open C-subtype <GParamSpec> (<C-void*>)
  constant slot gparamspec-g-type-instance :: <GTypeInstance>;
  constant slot gparamspec-name :: <C-string>;
  constant slot gparamspec-flags :: <GParamFlags>;
  constant slot gparamspec-value-type :: <C-long>;
  constant slot gparamspec-owner-type :: <C-long>;
  constant slot gparamspec--nick :: <C-string>;
  constant slot gparamspec--blurb :: <C-string>;
  constant slot gparamspec-qdata :: <GData>;
  constant slot gparamspec-ref-count :: <C-unsigned-int>;
  constant slot gparamspec-param-id :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpec*> => <GParamSpec>;

define C-function g-param-spec-get-blurb
  input parameter self :: <GParamSpec>;
  result res :: <C-string>;
  c-name: "g_param_spec_get_blurb";
end;

define C-function g-param-spec-get-name
  input parameter self :: <GParamSpec>;
  result res :: <C-string>;
  c-name: "g_param_spec_get_name";
end;

define C-function g-param-spec-get-nick
  input parameter self :: <GParamSpec>;
  result res :: <C-string>;
  c-name: "g_param_spec_get_nick";
end;

define C-function g-param-spec-get-qdata
  input parameter self :: <GParamSpec>;
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-void*>;
  c-name: "g_param_spec_get_qdata";
end;

define C-function g-param-spec-get-redirect-target
  input parameter self :: <GParamSpec>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_get_redirect_target";
end;

define C-function g-param-spec-set-qdata
  input parameter self :: <GParamSpec>;
  input parameter quark_ :: <C-unsigned-int>;
  input parameter data_ :: <C-void*>;
  c-name: "g_param_spec_set_qdata";
end;

define C-function g-param-spec-sink
  input parameter self :: <GParamSpec>;
  c-name: "g_param_spec_sink";
end;

define C-function g-param-spec-steal-qdata
  input parameter self :: <GParamSpec>;
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-void*>;
  c-name: "g_param_spec_steal_qdata";
end;

define open C-subtype <GParamSpecBoolean> (<GParamSpec>)
  constant slot gparamspecboolean-parent-instance :: <GParamSpec>;
  constant slot gparamspecboolean-default-value :: <C-boolean>;
end C-subtype;

define C-pointer-type <GParamSpecBoolean*> => <GParamSpecBoolean>;

define open C-subtype <GParamSpecBoxed> (<GParamSpec>)
  constant slot gparamspecboxed-parent-instance :: <GParamSpec>;
end C-subtype;

define C-pointer-type <GParamSpecBoxed*> => <GParamSpecBoxed>;

define open C-subtype <GParamSpecChar> (<GParamSpec>)
  constant slot gparamspecchar-parent-instance :: <GParamSpec>;
  constant slot gparamspecchar-minimum :: <C-signed-char>;
  constant slot gparamspecchar-maximum :: <C-signed-char>;
  constant slot gparamspecchar-default-value :: <C-signed-char>;
end C-subtype;

define C-pointer-type <GParamSpecChar*> => <GParamSpecChar>;

define C-struct <_GParamSpecClass>
  constant slot gparamspecclass-g-type-class :: <GTypeClass>;
  constant slot gparamspecclass-value-type :: <C-long>;
  constant slot gparamspecclass-finalize :: <C-function-pointer>;
  constant slot gparamspecclass-value-set-default :: <C-function-pointer>;
  constant slot gparamspecclass-value-validate :: <C-function-pointer>;
  constant slot gparamspecclass-values-cmp :: <C-function-pointer>;
  constant slot gparamspecclass-dummy :: <C-void*>;
  pointer-type-name: <GParamSpecClass>;
end C-struct;

define open C-subtype <GParamSpecDouble> (<GParamSpec>)
  constant slot gparamspecdouble-parent-instance :: <GParamSpec>;
  constant slot gparamspecdouble-minimum :: <C-double>;
  constant slot gparamspecdouble-maximum :: <C-double>;
  constant slot gparamspecdouble-default-value :: <C-double>;
  constant slot gparamspecdouble-epsilon :: <C-double>;
end C-subtype;

define C-pointer-type <GParamSpecDouble*> => <GParamSpecDouble>;

define open C-subtype <GParamSpecEnum> (<GParamSpec>)
  constant slot gparamspecenum-parent-instance :: <GParamSpec>;
  constant slot gparamspecenum-enum-class :: <GEnumClass>;
  constant slot gparamspecenum-default-value :: <C-signed-int>;
end C-subtype;

define C-pointer-type <GParamSpecEnum*> => <GParamSpecEnum>;

define open C-subtype <GParamSpecFlags> (<GParamSpec>)
  constant slot gparamspecflags-parent-instance :: <GParamSpec>;
  constant slot gparamspecflags-flags-class :: <GFlagsClass>;
  constant slot gparamspecflags-default-value :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpecFlags*> => <GParamSpecFlags>;

define open C-subtype <GParamSpecFloat> (<GParamSpec>)
  constant slot gparamspecfloat-parent-instance :: <GParamSpec>;
  constant slot gparamspecfloat-minimum :: <C-float>;
  constant slot gparamspecfloat-maximum :: <C-float>;
  constant slot gparamspecfloat-default-value :: <C-float>;
  constant slot gparamspecfloat-epsilon :: <C-float>;
end C-subtype;

define C-pointer-type <GParamSpecFloat*> => <GParamSpecFloat>;

define open C-subtype <GParamSpecGType> (<GParamSpec>)
  constant slot gparamspecgtype-parent-instance :: <GParamSpec>;
  constant slot gparamspecgtype-is-a-type :: <C-long>;
end C-subtype;

define C-pointer-type <GParamSpecGType*> => <GParamSpecGType>;

define open C-subtype <GParamSpecInt> (<GParamSpec>)
  constant slot gparamspecint-parent-instance :: <GParamSpec>;
  constant slot gparamspecint-minimum :: <C-signed-int>;
  constant slot gparamspecint-maximum :: <C-signed-int>;
  constant slot gparamspecint-default-value :: <C-signed-int>;
end C-subtype;

define C-pointer-type <GParamSpecInt*> => <GParamSpecInt>;

define open C-subtype <GParamSpecInt64> (<GParamSpec>)
  constant slot gparamspecint64-parent-instance :: <GParamSpec>;
  constant slot gparamspecint64-minimum :: <C-signed-long>;
  constant slot gparamspecint64-maximum :: <C-signed-long>;
  constant slot gparamspecint64-default-value :: <C-signed-long>;
end C-subtype;

define C-pointer-type <GParamSpecInt64*> => <GParamSpecInt64>;

define open C-subtype <GParamSpecLong> (<GParamSpec>)
  constant slot gparamspeclong-parent-instance :: <GParamSpec>;
  constant slot gparamspeclong-minimum :: <C-signed-long>;
  constant slot gparamspeclong-maximum :: <C-signed-long>;
  constant slot gparamspeclong-default-value :: <C-signed-long>;
end C-subtype;

define C-pointer-type <GParamSpecLong*> => <GParamSpecLong>;

define open C-subtype <GParamSpecObject> (<GParamSpec>)
  constant slot gparamspecobject-parent-instance :: <GParamSpec>;
end C-subtype;

define C-pointer-type <GParamSpecObject*> => <GParamSpecObject>;

define open C-subtype <GParamSpecOverride> (<GParamSpec>)
  constant slot gparamspecoverride-parent-instance :: <GParamSpec>;
  constant slot gparamspecoverride-overridden :: <GParamSpec>;
end C-subtype;

define C-pointer-type <GParamSpecOverride*> => <GParamSpecOverride>;

define open C-subtype <GParamSpecParam> (<GParamSpec>)
  constant slot gparamspecparam-parent-instance :: <GParamSpec>;
end C-subtype;

define C-pointer-type <GParamSpecParam*> => <GParamSpecParam>;

define open C-subtype <GParamSpecPointer> (<GParamSpec>)
  constant slot gparamspecpointer-parent-instance :: <GParamSpec>;
end C-subtype;

define C-pointer-type <GParamSpecPointer*> => <GParamSpecPointer>;

define C-struct <_GParamSpecPool>
  pointer-type-name: <GParamSpecPool>;
end C-struct;

define C-function g-param-spec-pool-insert
  input parameter self :: <GParamSpecPool>;
  input parameter pspec_ :: <GParamSpec>;
  input parameter owner_type_ :: <C-long>;
  c-name: "g_param_spec_pool_insert";
end;

define C-function g-param-spec-pool-list
  input parameter self :: <GParamSpecPool>;
  input parameter owner_type_ :: <C-long>;
  output parameter n_pspecs_p_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "g_param_spec_pool_list";
end;

define C-function g-param-spec-pool-list-owned
  input parameter self :: <GParamSpecPool>;
  input parameter owner_type_ :: <C-long>;
  result res :: <GList>;
  c-name: "g_param_spec_pool_list_owned";
end;

define C-function g-param-spec-pool-lookup
  input parameter self :: <GParamSpecPool>;
  input parameter param_name_ :: <C-string>;
  input parameter owner_type_ :: <C-long>;
  input parameter walk_ancestors_ :: <C-boolean>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_pool_lookup";
end;

define C-function g-param-spec-pool-remove
  input parameter self :: <GParamSpecPool>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "g_param_spec_pool_remove";
end;

define C-function g-param-spec-pool-new
  input parameter type_prefixing_ :: <C-boolean>;
  result res :: <GParamSpecPool>;
  c-name: "g_param_spec_pool_new";
end;

define open C-subtype <GParamSpecString> (<GParamSpec>)
  constant slot gparamspecstring-parent-instance :: <GParamSpec>;
  constant slot gparamspecstring-default-value :: <C-string>;
  constant slot gparamspecstring-cset-first :: <C-string>;
  constant slot gparamspecstring-cset-nth :: <C-string>;
  constant slot gparamspecstring-substitutor :: <C-signed-char>;
  constant slot gparamspecstring-null-fold-if-empty :: <C-unsigned-int>;
  constant slot gparamspecstring-ensure-non-null :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpecString*> => <GParamSpecString>;

define C-struct <_GParamSpecTypeInfo>
  slot gparamspectypeinfo-instance-size :: <C-unsigned-short>;
  slot gparamspectypeinfo-n-preallocs :: <C-unsigned-short>;
  constant slot gparamspectypeinfo-instance-init :: <C-function-pointer>;
  slot gparamspectypeinfo-value-type :: <C-long>;
  constant slot gparamspectypeinfo-finalize :: <C-function-pointer>;
  constant slot gparamspectypeinfo-value-set-default :: <C-function-pointer>;
  constant slot gparamspectypeinfo-value-validate :: <C-function-pointer>;
  constant slot gparamspectypeinfo-values-cmp :: <C-function-pointer>;
  pointer-type-name: <GParamSpecTypeInfo>;
end C-struct;

define open C-subtype <GParamSpecUChar> (<GParamSpec>)
  constant slot gparamspecuchar-parent-instance :: <GParamSpec>;
  constant slot gparamspecuchar-minimum :: <C-unsigned-char>;
  constant slot gparamspecuchar-maximum :: <C-unsigned-char>;
  constant slot gparamspecuchar-default-value :: <C-unsigned-char>;
end C-subtype;

define C-pointer-type <GParamSpecUChar*> => <GParamSpecUChar>;

define open C-subtype <GParamSpecUInt> (<GParamSpec>)
  constant slot gparamspecuint-parent-instance :: <GParamSpec>;
  constant slot gparamspecuint-minimum :: <C-unsigned-int>;
  constant slot gparamspecuint-maximum :: <C-unsigned-int>;
  constant slot gparamspecuint-default-value :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpecUInt*> => <GParamSpecUInt>;

define open C-subtype <GParamSpecUInt64> (<GParamSpec>)
  constant slot gparamspecuint64-parent-instance :: <GParamSpec>;
  constant slot gparamspecuint64-minimum :: <C-unsigned-long>;
  constant slot gparamspecuint64-maximum :: <C-unsigned-long>;
  constant slot gparamspecuint64-default-value :: <C-unsigned-long>;
end C-subtype;

define C-pointer-type <GParamSpecUInt64*> => <GParamSpecUInt64>;

define open C-subtype <GParamSpecULong> (<GParamSpec>)
  constant slot gparamspeculong-parent-instance :: <GParamSpec>;
  constant slot gparamspeculong-minimum :: <C-unsigned-long>;
  constant slot gparamspeculong-maximum :: <C-unsigned-long>;
  constant slot gparamspeculong-default-value :: <C-unsigned-long>;
end C-subtype;

define C-pointer-type <GParamSpecULong*> => <GParamSpecULong>;

define open C-subtype <GParamSpecUnichar> (<GParamSpec>)
  constant slot gparamspecunichar-parent-instance :: <GParamSpec>;
  constant slot gparamspecunichar-default-value :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpecUnichar*> => <GParamSpecUnichar>;

define open C-subtype <GParamSpecValueArray> (<GParamSpec>)
  constant slot gparamspecvaluearray-parent-instance :: <GParamSpec>;
  constant slot gparamspecvaluearray-element-spec :: <GParamSpec>;
  constant slot gparamspecvaluearray-fixed-n-elements :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GParamSpecValueArray*> => <GParamSpecValueArray>;

define open C-subtype <GParamSpecVariant> (<GParamSpec>)
  constant slot gparamspecvariant-parent-instance :: <GParamSpec>;
  constant slot gparamspecvariant-type :: <GVariantType>;
  constant slot gparamspecvariant-default-value :: <GVariant>;
  constant slot gparamspecvariant-padding :: <C-void*>;
end C-subtype;

define C-pointer-type <GParamSpecVariant*> => <GParamSpecVariant>;

define C-struct <_GParameter>
  slot gparameter-name :: <C-string>;
  slot gparameter-value :: <GValue>;
  pointer-type-name: <GParameter>;
end C-struct;

define constant $SIGNAL-FLAGS-MASK = 511;

define constant $SIGNAL-MATCH-MASK = 63;

define constant $G-SIGNAL-RUN-FIRST = 1;
define constant $G-SIGNAL-RUN-LAST = 2;
define constant $G-SIGNAL-RUN-CLEANUP = 4;
define constant $G-SIGNAL-NO-RECURSE = 8;
define constant $G-SIGNAL-DETAILED = 16;
define constant $G-SIGNAL-ACTION = 32;
define constant $G-SIGNAL-NO-HOOKS = 64;
define constant $G-SIGNAL-MUST-COLLECT = 128;
define constant $G-SIGNAL-DEPRECATED = 256;
define constant <GSignalFlags> = <C-int>;
define C-pointer-type <GSignalFlags*> => <GSignalFlags>;

define C-struct <_GSignalInvocationHint>
  slot gsignalinvocationhint-signal-id :: <C-unsigned-int>;
  slot gsignalinvocationhint-detail :: <C-unsigned-int>;
  slot gsignalinvocationhint-run-type :: <GSignalFlags>;
  pointer-type-name: <GSignalInvocationHint>;
end C-struct;

define constant $G-SIGNAL-MATCH-ID = 1;
define constant $G-SIGNAL-MATCH-DETAIL = 2;
define constant $G-SIGNAL-MATCH-CLOSURE = 4;
define constant $G-SIGNAL-MATCH-FUNC = 8;
define constant $G-SIGNAL-MATCH-DATA = 16;
define constant $G-SIGNAL-MATCH-UNBLOCKED = 32;
define constant <GSignalMatchType> = <C-int>;
define C-pointer-type <GSignalMatchType*> => <GSignalMatchType>;

define C-struct <_GSignalQuery>
  slot gsignalquery-signal-id :: <C-unsigned-int>;
  slot gsignalquery-signal-name :: <C-string>;
  slot gsignalquery-itype :: <C-long>;
  slot gsignalquery-signal-flags :: <GSignalFlags>;
  slot gsignalquery-return-type :: <C-long>;
  slot gsignalquery-n-params :: <C-unsigned-int>;
  slot gsignalquery-param-types :: <C-long*>;
  pointer-type-name: <GSignalQuery>;
end C-struct;

define constant $TYPE-FLAG-RESERVED-ID-BIT = 1;

define constant $TYPE-FUNDAMENTAL-MAX = 255;

define constant $TYPE-FUNDAMENTAL-SHIFT = 2;

define constant $TYPE-RESERVED-BSE-FIRST = 32;

define constant $TYPE-RESERVED-BSE-LAST = 48;

define constant $TYPE-RESERVED-GLIB-FIRST = 22;

define constant $TYPE-RESERVED-GLIB-LAST = 31;

define constant $TYPE-RESERVED-USER-FIRST = 49;

define C-union <_GTypeCValue>
  slot gtypecvalue-v-int :: <C-signed-int>;
  slot gtypecvalue-v-long :: <C-signed-long>;
  slot gtypecvalue-v-int64 :: <C-signed-long>;
  slot gtypecvalue-v-double :: <C-double>;
  slot gtypecvalue-v-pointer :: <C-void*>;
  pointer-type-name: <GTypeCValue>;
end C-union;

define C-struct <_GTypeClass>
  constant slot gtypeclass-g-type :: <C-long>;
  pointer-type-name: <GTypeClass>;
end C-struct;

define C-function g-type-class-peek-parent
  input parameter self :: <GTypeClass>;
  result res :: <GTypeClass>;
  c-name: "g_type_class_peek_parent";
end;

define C-function g-type-class-unref
  input parameter self :: <GTypeClass>;
  c-name: "g_type_class_unref";
end;

define C-function g-type-class-add-private
  input parameter g_class_ :: <C-void*>;
  input parameter private_size_ :: <C-unsigned-long>;
  c-name: "g_type_class_add_private";
end;

define C-function g-type-class-peek
  input parameter type_ :: <C-long>;
  result res :: <GTypeClass>;
  c-name: "g_type_class_peek";
end;

define C-function g-type-class-peek-static
  input parameter type_ :: <C-long>;
  result res :: <GTypeClass>;
  c-name: "g_type_class_peek_static";
end;

define C-function g-type-class-ref
  input parameter type_ :: <C-long>;
  result res :: <GTypeClass>;
  c-name: "g_type_class_ref";
end;

define constant $G-TYPE-FLAG-ABSTRACT = 16;
define constant $G-TYPE-FLAG-VALUE-ABSTRACT = 32;
define constant <GTypeFlags> = <C-int>;
define C-pointer-type <GTypeFlags*> => <GTypeFlags>;

define constant $G-TYPE-FLAG-CLASSED = 1;
define constant $G-TYPE-FLAG-INSTANTIATABLE = 2;
define constant $G-TYPE-FLAG-DERIVABLE = 4;
define constant $G-TYPE-FLAG-DEEP-DERIVABLE = 8;
define constant <GTypeFundamentalFlags> = <C-int>;
define C-pointer-type <GTypeFundamentalFlags*> => <GTypeFundamentalFlags>;

define C-struct <_GTypeFundamentalInfo>
  slot gtypefundamentalinfo-type-flags :: <GTypeFundamentalFlags>;
  pointer-type-name: <GTypeFundamentalInfo>;
end C-struct;

define C-struct <_GTypeInfo>
  slot gtypeinfo-class-size :: <C-unsigned-short>;
  slot gtypeinfo-base-init :: <C-function-pointer>;
  slot gtypeinfo-base-finalize :: <C-function-pointer>;
  slot gtypeinfo-class-init :: <C-function-pointer>;
  slot gtypeinfo-class-finalize :: <C-function-pointer>;
  slot gtypeinfo-class-data :: <C-void*>;
  slot gtypeinfo-instance-size :: <C-unsigned-short>;
  slot gtypeinfo-n-preallocs :: <C-unsigned-short>;
  slot gtypeinfo-instance-init :: <C-function-pointer>;
  slot gtypeinfo-value-table :: <GTypeValueTable>;
  pointer-type-name: <GTypeInfo>;
end C-struct;

define C-struct <_GTypeInstance>
  constant slot gtypeinstance-g-class :: <GTypeClass>;
  pointer-type-name: <GTypeInstance>;
end C-struct;

define C-struct <_GTypeInterface>
  constant slot gtypeinterface-g-type :: <C-long>;
  constant slot gtypeinterface-g-instance-type :: <C-long>;
  pointer-type-name: <GTypeInterface>;
end C-struct;

define C-function g-type-interface-peek-parent
  input parameter self :: <GTypeInterface>;
  result res :: <GTypeInterface>;
  c-name: "g_type_interface_peek_parent";
end;

define C-function g-type-interface-add-prerequisite
  input parameter interface_type_ :: <C-long>;
  input parameter prerequisite_type_ :: <C-long>;
  c-name: "g_type_interface_add_prerequisite";
end;

define C-function g-type-interface-get-plugin
  input parameter instance_type_ :: <C-long>;
  input parameter interface_type_ :: <C-long>;
  result res :: <GTypePlugin>;
  c-name: "g_type_interface_get_plugin";
end;

define C-function g-type-interface-peek
  input parameter instance_class_ :: <GTypeClass>;
  input parameter iface_type_ :: <C-long>;
  result res :: <GTypeInterface>;
  c-name: "g_type_interface_peek";
end;

define C-function g-type-interface-prerequisites
  input parameter interface_type_ :: <C-long>;
  output parameter n_prerequisites_ :: <C-unsigned-int*>;
  result res :: <C-long*>;
  c-name: "g_type_interface_prerequisites";
end;

define open C-subtype <GTypeModule> (<GObject>)
  constant slot gtypemodule-parent-instance :: <GObject>;
  constant slot gtypemodule-use-count :: <C-unsigned-int>;
  constant slot gtypemodule-type-infos :: <GSList>;
  constant slot gtypemodule-interface-infos :: <GSList>;
  constant slot gtypemodule-name :: <C-string>;
end C-subtype;

define C-pointer-type <GTypeModule*> => <GTypeModule>;

define C-function g-type-module-add-interface
  input parameter self :: <GTypeModule>;
  input parameter instance_type_ :: <C-long>;
  input parameter interface_type_ :: <C-long>;
  input parameter interface_info_ :: <GInterfaceInfo>;
  c-name: "g_type_module_add_interface";
end;

define C-function g-type-module-register-enum
  input parameter self :: <GTypeModule>;
  input parameter name_ :: <C-string>;
  input parameter const_static_values_ :: <GEnumValue>;
  result res :: <C-long>;
  c-name: "g_type_module_register_enum";
end;

define C-function g-type-module-register-flags
  input parameter self :: <GTypeModule>;
  input parameter name_ :: <C-string>;
  input parameter const_static_values_ :: <GFlagsValue>;
  result res :: <C-long>;
  c-name: "g_type_module_register_flags";
end;

define C-function g-type-module-register-type
  input parameter self :: <GTypeModule>;
  input parameter parent_type_ :: <C-long>;
  input parameter type_name_ :: <C-string>;
  input parameter type_info_ :: <GTypeInfo>;
  input parameter flags_ :: <GTypeFlags>;
  result res :: <C-long>;
  c-name: "g_type_module_register_type";
end;

define C-function g-type-module-set-name
  input parameter self :: <GTypeModule>;
  input parameter name_ :: <C-string>;
  c-name: "g_type_module_set_name";
end;

define C-function g-type-module-unuse
  input parameter self :: <GTypeModule>;
  c-name: "g_type_module_unuse";
end;

define C-function g-type-module-use
  input parameter self :: <GTypeModule>;
  result res :: <C-boolean>;
  c-name: "g_type_module_use";
end;

define C-struct <_GTypeModuleClass>
  constant slot gtypemoduleclass-parent-class :: <GObjectClass>;
  constant slot gtypemoduleclass-load :: <C-function-pointer>;
  constant slot gtypemoduleclass-unload :: <C-function-pointer>;
  constant slot gtypemoduleclass-reserved1 :: <C-function-pointer>;
  constant slot gtypemoduleclass-reserved2 :: <C-function-pointer>;
  constant slot gtypemoduleclass-reserved3 :: <C-function-pointer>;
  constant slot gtypemoduleclass-reserved4 :: <C-function-pointer>;
  pointer-type-name: <GTypeModuleClass>;
end C-struct;

// Interface
define open C-subtype <GTypePlugin> (<C-void*>)
end C-subtype;

define C-pointer-type <GTypePlugin*> => <GTypePlugin>;

define C-function g-type-plugin-complete-interface-info
  input parameter self :: <GTypePlugin>;
  input parameter instance_type_ :: <C-long>;
  input parameter interface_type_ :: <C-long>;
  input parameter info_ :: <GInterfaceInfo>;
  c-name: "g_type_plugin_complete_interface_info";
end;

define C-function g-type-plugin-complete-type-info
  input parameter self :: <GTypePlugin>;
  input parameter g_type_ :: <C-long>;
  input parameter info_ :: <GTypeInfo>;
  input parameter value_table_ :: <GTypeValueTable>;
  c-name: "g_type_plugin_complete_type_info";
end;

define C-function g-type-plugin-unuse
  input parameter self :: <GTypePlugin>;
  c-name: "g_type_plugin_unuse";
end;

define C-function g-type-plugin-use
  input parameter self :: <GTypePlugin>;
  c-name: "g_type_plugin_use";
end;

define C-struct <_GTypePluginClass>
  constant slot gtypepluginclass-base-iface :: <GTypeInterface>;
  slot gtypepluginclass-use-plugin :: <C-function-pointer>;
  slot gtypepluginclass-unuse-plugin :: <C-function-pointer>;
  slot gtypepluginclass-complete-type-info :: <C-function-pointer>;
  slot gtypepluginclass-complete-interface-info :: <C-function-pointer>;
  pointer-type-name: <GTypePluginClass>;
end C-struct;

define C-struct <_GTypeQuery>
  slot gtypequery-type :: <C-long>;
  slot gtypequery-type-name :: <C-string>;
  slot gtypequery-class-size :: <C-unsigned-int>;
  slot gtypequery-instance-size :: <C-unsigned-int>;
  pointer-type-name: <GTypeQuery>;
end C-struct;

define C-struct <_GTypeValueTable>
  constant slot gtypevaluetable-value-init :: <C-function-pointer>;
  constant slot gtypevaluetable-value-free :: <C-function-pointer>;
  constant slot gtypevaluetable-value-copy :: <C-function-pointer>;
  constant slot gtypevaluetable-value-peek-pointer :: <C-void*>;
  slot gtypevaluetable-collect-format :: <C-string>;
  constant slot gtypevaluetable-collect-value :: <C-function-pointer>;
  slot gtypevaluetable-lcopy-format :: <C-string>;
  constant slot gtypevaluetable-lcopy-value :: <C-function-pointer>;
  pointer-type-name: <GTypeValueTable>;
end C-struct;

define constant $VALUE-COLLECT-FORMAT-MAX-LENGTH = 8;

define constant $VALUE-NOCOPY-CONTENTS = 134217728;

define C-struct <_GValue>
  constant slot gvalue-g-type :: <C-long>;
  slot gvalue-data :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GValue>;
end C-struct;

define C-function g-value-copy
  input parameter self :: <GValue>;
  input parameter dest_value_ :: <GValue>;
  c-name: "g_value_copy";
end;

define C-function g-value-dup-object
  input parameter self :: <GValue>;
  result res :: <GObject>;
  c-name: "g_value_dup_object";
end;

define C-function g-value-dup-string
  input parameter self :: <GValue>;
  result res :: <C-string>;
  c-name: "g_value_dup_string";
end;

define C-function g-value-dup-variant
  input parameter self :: <GValue>;
  result res :: <GVariant>;
  c-name: "g_value_dup_variant";
end;

define C-function g-value-fits-pointer
  input parameter self :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_value_fits_pointer";
end;

define C-function g-value-get-boolean
  input parameter self :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_value_get_boolean";
end;

define C-function g-value-get-boxed
  input parameter self :: <GValue>;
  result res :: <C-void*>;
  c-name: "g_value_get_boxed";
end;

define C-function g-value-get-char
  input parameter self :: <GValue>;
  result res :: <C-signed-char>;
  c-name: "g_value_get_char";
end;

define C-function g-value-get-double
  input parameter self :: <GValue>;
  result res :: <C-double>;
  c-name: "g_value_get_double";
end;

define C-function g-value-get-enum
  input parameter self :: <GValue>;
  result res :: <C-signed-int>;
  c-name: "g_value_get_enum";
end;

define C-function g-value-get-flags
  input parameter self :: <GValue>;
  result res :: <C-unsigned-int>;
  c-name: "g_value_get_flags";
end;

define C-function g-value-get-float
  input parameter self :: <GValue>;
  result res :: <C-float>;
  c-name: "g_value_get_float";
end;

define C-function g-value-get-gtype
  input parameter self :: <GValue>;
  result res :: <C-long>;
  c-name: "g_value_get_gtype";
end;

define C-function g-value-get-int
  input parameter self :: <GValue>;
  result res :: <C-signed-int>;
  c-name: "g_value_get_int";
end;

define C-function g-value-get-int64
  input parameter self :: <GValue>;
  result res :: <C-signed-long>;
  c-name: "g_value_get_int64";
end;

define C-function g-value-get-long
  input parameter self :: <GValue>;
  result res :: <C-signed-long>;
  c-name: "g_value_get_long";
end;

define C-function g-value-get-object
  input parameter self :: <GValue>;
  result res :: <GObject>;
  c-name: "g_value_get_object";
end;

define C-function g-value-get-param
  input parameter self :: <GValue>;
  result res :: <GParamSpec>;
  c-name: "g_value_get_param";
end;

define C-function g-value-get-pointer
  input parameter self :: <GValue>;
  result res :: <C-void*>;
  c-name: "g_value_get_pointer";
end;

define C-function g-value-get-schar
  input parameter self :: <GValue>;
  result res :: <C-signed-char>;
  c-name: "g_value_get_schar";
end;

define C-function g-value-get-string
  input parameter self :: <GValue>;
  result res :: <C-string>;
  c-name: "g_value_get_string";
end;

define C-function g-value-get-uchar
  input parameter self :: <GValue>;
  result res :: <C-unsigned-char>;
  c-name: "g_value_get_uchar";
end;

define C-function g-value-get-uint
  input parameter self :: <GValue>;
  result res :: <C-unsigned-int>;
  c-name: "g_value_get_uint";
end;

define C-function g-value-get-uint64
  input parameter self :: <GValue>;
  result res :: <C-unsigned-long>;
  c-name: "g_value_get_uint64";
end;

define C-function g-value-get-ulong
  input parameter self :: <GValue>;
  result res :: <C-unsigned-long>;
  c-name: "g_value_get_ulong";
end;

define C-function g-value-get-variant
  input parameter self :: <GValue>;
  result res :: <GVariant>;
  c-name: "g_value_get_variant";
end;

define C-function g-value-init
  input parameter self :: <GValue>;
  input parameter g_type_ :: <C-long>;
  result res :: <GValue>;
  c-name: "g_value_init";
end;

define C-function g-value-peek-pointer
  input parameter self :: <GValue>;
  result res :: <C-void*>;
  c-name: "g_value_peek_pointer";
end;

define C-function g-value-reset
  input parameter self :: <GValue>;
  result res :: <GValue>;
  c-name: "g_value_reset";
end;

define C-function g-value-set-boolean
  input parameter self :: <GValue>;
  input parameter v_boolean_ :: <C-boolean>;
  c-name: "g_value_set_boolean";
end;

define C-function g-value-set-boxed
  input parameter self :: <GValue>;
  input parameter v_boxed_ :: <C-void*>;
  c-name: "g_value_set_boxed";
end;

define C-function g-value-set-boxed-take-ownership
  input parameter self :: <GValue>;
  input parameter v_boxed_ :: <C-void*>;
  c-name: "g_value_set_boxed_take_ownership";
end;

define C-function g-value-set-char
  input parameter self :: <GValue>;
  input parameter v_char_ :: <C-signed-char>;
  c-name: "g_value_set_char";
end;

define C-function g-value-set-double
  input parameter self :: <GValue>;
  input parameter v_double_ :: <C-double>;
  c-name: "g_value_set_double";
end;

define C-function g-value-set-enum
  input parameter self :: <GValue>;
  input parameter v_enum_ :: <C-signed-int>;
  c-name: "g_value_set_enum";
end;

define C-function g-value-set-flags
  input parameter self :: <GValue>;
  input parameter v_flags_ :: <C-unsigned-int>;
  c-name: "g_value_set_flags";
end;

define C-function g-value-set-float
  input parameter self :: <GValue>;
  input parameter v_float_ :: <C-float>;
  c-name: "g_value_set_float";
end;

define C-function g-value-set-gtype
  input parameter self :: <GValue>;
  input parameter v_gtype_ :: <C-long>;
  c-name: "g_value_set_gtype";
end;

define C-function g-value-set-instance
  input parameter self :: <GValue>;
  input parameter instance_ :: <C-void*>;
  c-name: "g_value_set_instance";
end;

define C-function g-value-set-int
  input parameter self :: <GValue>;
  input parameter v_int_ :: <C-signed-int>;
  c-name: "g_value_set_int";
end;

define C-function g-value-set-int64
  input parameter self :: <GValue>;
  input parameter v_int64_ :: <C-signed-long>;
  c-name: "g_value_set_int64";
end;

define C-function g-value-set-long
  input parameter self :: <GValue>;
  input parameter v_long_ :: <C-signed-long>;
  c-name: "g_value_set_long";
end;

define C-function g-value-set-object
  input parameter self :: <GValue>;
  input parameter v_object_ :: <GObject>;
  c-name: "g_value_set_object";
end;

define C-function g-value-set-param
  input parameter self :: <GValue>;
  input parameter param_ :: <GParamSpec>;
  c-name: "g_value_set_param";
end;

define C-function g-value-set-pointer
  input parameter self :: <GValue>;
  input parameter v_pointer_ :: <C-void*>;
  c-name: "g_value_set_pointer";
end;

define C-function g-value-set-schar
  input parameter self :: <GValue>;
  input parameter v_char_ :: <C-signed-char>;
  c-name: "g_value_set_schar";
end;

define C-function g-value-set-static-boxed
  input parameter self :: <GValue>;
  input parameter v_boxed_ :: <C-void*>;
  c-name: "g_value_set_static_boxed";
end;

define C-function g-value-set-static-string
  input parameter self :: <GValue>;
  input parameter v_string_ :: <C-string>;
  c-name: "g_value_set_static_string";
end;

define C-function g-value-set-string
  input parameter self :: <GValue>;
  input parameter v_string_ :: <C-string>;
  c-name: "g_value_set_string";
end;

define C-function g-value-set-string-take-ownership
  input parameter self :: <GValue>;
  input parameter v_string_ :: <C-string>;
  c-name: "g_value_set_string_take_ownership";
end;

define C-function g-value-set-uchar
  input parameter self :: <GValue>;
  input parameter v_uchar_ :: <C-unsigned-char>;
  c-name: "g_value_set_uchar";
end;

define C-function g-value-set-uint
  input parameter self :: <GValue>;
  input parameter v_uint_ :: <C-unsigned-int>;
  c-name: "g_value_set_uint";
end;

define C-function g-value-set-uint64
  input parameter self :: <GValue>;
  input parameter v_uint64_ :: <C-unsigned-long>;
  c-name: "g_value_set_uint64";
end;

define C-function g-value-set-ulong
  input parameter self :: <GValue>;
  input parameter v_ulong_ :: <C-unsigned-long>;
  c-name: "g_value_set_ulong";
end;

define C-function g-value-set-variant
  input parameter self :: <GValue>;
  input parameter variant_ :: <GVariant>;
  c-name: "g_value_set_variant";
end;

define C-function g-value-take-boxed
  input parameter self :: <GValue>;
  input parameter v_boxed_ :: <C-void*>;
  c-name: "g_value_take_boxed";
end;

define C-function g-value-take-string
  input parameter self :: <GValue>;
  input parameter v_string_ :: <C-string>;
  c-name: "g_value_take_string";
end;

define C-function g-value-take-variant
  input parameter self :: <GValue>;
  input parameter variant_ :: <GVariant>;
  c-name: "g_value_take_variant";
end;

define C-function g-value-transform
  input parameter self :: <GValue>;
  input parameter dest_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_value_transform";
end;

define C-function g-value-unset
  input parameter self :: <GValue>;
  c-name: "g_value_unset";
end;

define C-function g-value-type-compatible
  input parameter src_type_ :: <C-long>;
  input parameter dest_type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_value_type_compatible";
end;

define C-function g-value-type-transformable
  input parameter src_type_ :: <C-long>;
  input parameter dest_type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_value_type_transformable";
end;

define C-struct <_GValueArray>
  slot gvaluearray-n-values :: <C-unsigned-int>;
  slot gvaluearray-values :: <GValue>;
  constant slot gvaluearray-n-prealloced :: <C-unsigned-int>;
  pointer-type-name: <GValueArray>;
end C-struct;

define C-function g-value-array-new
  input parameter n_prealloced_ :: <C-unsigned-int>;
  result res :: <GValueArray>;
  c-name: "g_value_array_new";
end;

define C-function g-value-array-append
  input parameter self :: <GValueArray>;
  input parameter value_ :: <GValue>;
  result res :: <GValueArray>;
  c-name: "g_value_array_append";
end;

define C-function g-value-array-copy
  input parameter self :: <GValueArray>;
  result res :: <GValueArray>;
  c-name: "g_value_array_copy";
end;

define C-function g-value-array-free
  input parameter self :: <GValueArray>;
  c-name: "g_value_array_free";
end;

define C-function g-value-array-get-nth
  input parameter self :: <GValueArray>;
  input parameter index__ :: <C-unsigned-int>;
  result res :: <GValue>;
  c-name: "g_value_array_get_nth";
end;

define C-function g-value-array-insert
  input parameter self :: <GValueArray>;
  input parameter index__ :: <C-unsigned-int>;
  input parameter value_ :: <GValue>;
  result res :: <GValueArray>;
  c-name: "g_value_array_insert";
end;

define C-function g-value-array-prepend
  input parameter self :: <GValueArray>;
  input parameter value_ :: <GValue>;
  result res :: <GValueArray>;
  c-name: "g_value_array_prepend";
end;

define C-function g-value-array-remove
  input parameter self :: <GValueArray>;
  input parameter index__ :: <C-unsigned-int>;
  result res :: <GValueArray>;
  c-name: "g_value_array_remove";
end;

define C-function g-value-array-sort-with-data
  input parameter self :: <GValueArray>;
  input parameter compare_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  result res :: <GValueArray>;
  c-name: "g_value_array_sort_with_data";
end;

define C-struct <_GWeakRef>
  pointer-type-name: <GWeakRef>;
end C-struct;

define C-union <_G_Value--data--union>
  slot g-value--data--union-v-int :: <C-signed-int>;
  slot g-value--data--union-v-uint :: <C-unsigned-int>;
  slot g-value--data--union-v-long :: <C-signed-long>;
  slot g-value--data--union-v-ulong :: <C-unsigned-long>;
  slot g-value--data--union-v-int64 :: <C-signed-long>;
  slot g-value--data--union-v-uint64 :: <C-unsigned-long>;
  slot g-value--data--union-v-float :: <C-float>;
  slot g-value--data--union-v-double :: <C-double>;
  slot g-value--data--union-v-pointer :: <C-void*>;
  pointer-type-name: <G_Value--data--union>;
end C-union;

define C-function g-boxed-free
  input parameter boxed_type_ :: <C-long>;
  input parameter boxed_ :: <C-void*>;
  c-name: "g_boxed_free";
end;

define C-function g-enum-complete-type-info
  input parameter g_enum_type_ :: <C-long>;
  input parameter info_ :: <GTypeInfo>;
  input parameter const_values_ :: <GEnumValue>;
  c-name: "g_enum_complete_type_info";
end;

define C-function g-enum-register-static
  input parameter name_ :: <C-string>;
  input parameter const_static_values_ :: <GEnumValue>;
  result res :: <C-long>;
  c-name: "g_enum_register_static";
end;

define C-function g-flags-complete-type-info
  input parameter g_flags_type_ :: <C-long>;
  input parameter info_ :: <GTypeInfo>;
  input parameter const_values_ :: <GFlagsValue>;
  c-name: "g_flags_complete_type_info";
end;

define C-function g-flags-register-static
  input parameter name_ :: <C-string>;
  input parameter const_static_values_ :: <GFlagsValue>;
  result res :: <C-long>;
  c-name: "g_flags_register_static";
end;

define C-function g-gtype-get-type
  result res :: <C-long>;
  c-name: "g_gtype_get_type";
end;

define C-function g-param-spec-boolean
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter default_value_ :: <C-boolean>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_boolean";
end;

define C-function g-param-spec-boxed
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter boxed_type_ :: <C-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_boxed";
end;

define C-function g-param-spec-char
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-signed-char>;
  input parameter maximum_ :: <C-signed-char>;
  input parameter default_value_ :: <C-signed-char>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_char";
end;

define C-function g-param-spec-double
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-double>;
  input parameter maximum_ :: <C-double>;
  input parameter default_value_ :: <C-double>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_double";
end;

define C-function g-param-spec-enum
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter enum_type_ :: <C-long>;
  input parameter default_value_ :: <C-signed-int>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_enum";
end;

define C-function g-param-spec-flags
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter flags_type_ :: <C-long>;
  input parameter default_value_ :: <C-unsigned-int>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_flags";
end;

define C-function g-param-spec-float
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-float>;
  input parameter maximum_ :: <C-float>;
  input parameter default_value_ :: <C-float>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_float";
end;

define C-function g-param-spec-gtype
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter is_a_type_ :: <C-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_gtype";
end;

define C-function g-param-spec-int
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-signed-int>;
  input parameter maximum_ :: <C-signed-int>;
  input parameter default_value_ :: <C-signed-int>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_int";
end;

define C-function g-param-spec-int64
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-signed-long>;
  input parameter maximum_ :: <C-signed-long>;
  input parameter default_value_ :: <C-signed-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_int64";
end;

define C-function g-param-spec-long
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-signed-long>;
  input parameter maximum_ :: <C-signed-long>;
  input parameter default_value_ :: <C-signed-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_long";
end;

define C-function g-param-spec-object
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter object_type_ :: <C-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_object";
end;

define C-function g-param-spec-param
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter param_type_ :: <C-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_param";
end;

define C-function g-param-spec-pointer
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_pointer";
end;

define C-function g-param-spec-string
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter default_value_ :: <C-string>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_string";
end;

define C-function g-param-spec-uchar
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-unsigned-char>;
  input parameter maximum_ :: <C-unsigned-char>;
  input parameter default_value_ :: <C-unsigned-char>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_uchar";
end;

define C-function g-param-spec-uint
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-unsigned-int>;
  input parameter maximum_ :: <C-unsigned-int>;
  input parameter default_value_ :: <C-unsigned-int>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_uint";
end;

define C-function g-param-spec-uint64
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-unsigned-long>;
  input parameter maximum_ :: <C-unsigned-long>;
  input parameter default_value_ :: <C-unsigned-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_uint64";
end;

define C-function g-param-spec-ulong
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter minimum_ :: <C-unsigned-long>;
  input parameter maximum_ :: <C-unsigned-long>;
  input parameter default_value_ :: <C-unsigned-long>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_ulong";
end;

define C-function g-param-spec-unichar
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter default_value_ :: <C-unsigned-int>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_unichar";
end;

define C-function g-param-spec-variant
  input parameter name_ :: <C-string>;
  input parameter nick_ :: <C-string>;
  input parameter blurb_ :: <C-string>;
  input parameter type_ :: <GVariantType>;
  input parameter default_value_ :: <GVariant>;
  input parameter flags_ :: <GParamFlags>;
  result res :: <GParamSpec>;
  c-name: "g_param_spec_variant";
end;

define C-function g-param-type-register-static
  input parameter name_ :: <C-string>;
  input parameter pspec_info_ :: <GParamSpecTypeInfo>;
  result res :: <C-long>;
  c-name: "g_param_type_register_static";
end;

define C-function g-param-value-convert
  input parameter pspec_ :: <GParamSpec>;
  input parameter src_value_ :: <GValue>;
  input parameter dest_value_ :: <GValue>;
  input parameter strict_validation_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_param_value_convert";
end;

define C-function g-param-value-defaults
  input parameter pspec_ :: <GParamSpec>;
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_param_value_defaults";
end;

define C-function g-param-value-set-default
  input parameter pspec_ :: <GParamSpec>;
  input parameter value_ :: <GValue>;
  c-name: "g_param_value_set_default";
end;

define C-function g-param-value-validate
  input parameter pspec_ :: <GParamSpec>;
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_param_value_validate";
end;

define C-function g-param-values-cmp
  input parameter pspec_ :: <GParamSpec>;
  input parameter value1_ :: <GValue>;
  input parameter value2_ :: <GValue>;
  result res :: <C-signed-int>;
  c-name: "g_param_values_cmp";
end;

define C-function g-pointer-type-register-static
  input parameter name_ :: <C-string>;
  result res :: <C-long>;
  c-name: "g_pointer_type_register_static";
end;

define C-function g-signal-accumulator-first-wins
  input parameter ihint_ :: <GSignalInvocationHint>;
  input parameter return_accu_ :: <GValue>;
  input parameter handler_return_ :: <GValue>;
  input parameter dummy_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_signal_accumulator_first_wins";
end;

define C-function g-signal-accumulator-true-handled
  input parameter ihint_ :: <GSignalInvocationHint>;
  input parameter return_accu_ :: <GValue>;
  input parameter handler_return_ :: <GValue>;
  input parameter dummy_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_signal_accumulator_true_handled";
end;

define C-function g-signal-add-emission-hook
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter hook_func_ :: <C-function-pointer>;
  input parameter hook_data_ :: <C-void*>;
  input parameter data_destroy_ :: <C-function-pointer>;
  result res :: <C-unsigned-long>;
  c-name: "g_signal_add_emission_hook";
end;

define C-function g-signal-chain-from-overridden
  input parameter instance_and_params_ :: <C-unsigned-char*> /* Not supported */;
  input parameter return_value_ :: <GValue>;
  c-name: "g_signal_chain_from_overridden";
end;

define C-function g-signal-connect-closure
  input parameter instance_ :: <C-void*>;
  input parameter detailed_signal_ :: <C-string>;
  input parameter closure_ :: <GClosure>;
  input parameter after_ :: <C-boolean>;
  result res :: <C-unsigned-long>;
  c-name: "g_signal_connect_closure";
end;

define C-function g-signal-connect-closure-by-id
  input parameter instance_ :: <C-void*>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter closure_ :: <GClosure>;
  input parameter after_ :: <C-boolean>;
  result res :: <C-unsigned-long>;
  c-name: "g_signal_connect_closure_by_id";
end;

define C-function g-signal-emitv
  input parameter instance_and_params_ :: <C-unsigned-char*> /* Not supported */;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter return_value_ :: <GValue>;
  c-name: "g_signal_emitv";
end;

define C-function g-signal-get-invocation-hint
  input parameter instance_ :: <C-void*>;
  result res :: <GSignalInvocationHint>;
  c-name: "g_signal_get_invocation_hint";
end;

define C-function g-signal-handler-block
  input parameter instance_ :: <C-void*>;
  input parameter handler_id_ :: <C-unsigned-long>;
  c-name: "g_signal_handler_block";
end;

define C-function g-signal-handler-disconnect
  input parameter instance_ :: <C-void*>;
  input parameter handler_id_ :: <C-unsigned-long>;
  c-name: "g_signal_handler_disconnect";
end;

define C-function g-signal-handler-find
  input parameter instance_ :: <C-void*>;
  input parameter mask_ :: <GSignalMatchType>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter closure_ :: <GClosure>;
  input parameter func_ :: <C-void*>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-long>;
  c-name: "g_signal_handler_find";
end;

define C-function g-signal-handler-is-connected
  input parameter instance_ :: <C-void*>;
  input parameter handler_id_ :: <C-unsigned-long>;
  result res :: <C-boolean>;
  c-name: "g_signal_handler_is_connected";
end;

define C-function g-signal-handler-unblock
  input parameter instance_ :: <C-void*>;
  input parameter handler_id_ :: <C-unsigned-long>;
  c-name: "g_signal_handler_unblock";
end;

define C-function g-signal-handlers-block-matched
  input parameter instance_ :: <C-void*>;
  input parameter mask_ :: <GSignalMatchType>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter closure_ :: <GClosure>;
  input parameter func_ :: <C-void*>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_signal_handlers_block_matched";
end;

define C-function g-signal-handlers-destroy
  input parameter instance_ :: <C-void*>;
  c-name: "g_signal_handlers_destroy";
end;

define C-function g-signal-handlers-disconnect-matched
  input parameter instance_ :: <C-void*>;
  input parameter mask_ :: <GSignalMatchType>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter closure_ :: <GClosure>;
  input parameter func_ :: <C-void*>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_signal_handlers_disconnect_matched";
end;

define C-function g-signal-handlers-unblock-matched
  input parameter instance_ :: <C-void*>;
  input parameter mask_ :: <GSignalMatchType>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter closure_ :: <GClosure>;
  input parameter func_ :: <C-void*>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_signal_handlers_unblock_matched";
end;

define C-function g-signal-has-handler-pending
  input parameter instance_ :: <C-void*>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  input parameter may_be_blocked_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_signal_has_handler_pending";
end;

define C-function g-signal-list-ids
  input parameter itype_ :: <C-long>;
  output parameter n_ids_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-int*>;
  c-name: "g_signal_list_ids";
end;

define C-function g-signal-lookup
  input parameter name_ :: <C-string>;
  input parameter itype_ :: <C-long>;
  result res :: <C-unsigned-int>;
  c-name: "g_signal_lookup";
end;

define C-function g-signal-name
  input parameter signal_id_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "g_signal_name";
end;

define C-function g-signal-override-class-closure
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter instance_type_ :: <C-long>;
  input parameter class_closure_ :: <GClosure>;
  c-name: "g_signal_override_class_closure";
end;

define C-function g-signal-parse-name
  input parameter detailed_signal_ :: <C-string>;
  input parameter itype_ :: <C-long>;
  output parameter signal_id_p_ :: <C-unsigned-int*>;
  output parameter detail_p_ :: <C-unsigned-int*>;
  input parameter force_detail_quark_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_signal_parse_name";
end;

define C-function g-signal-query
  input parameter signal_id_ :: <C-unsigned-int>;
  output parameter query_ :: <GSignalQuery>;
  c-name: "g_signal_query";
end;

define C-function g-signal-remove-emission-hook
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter hook_id_ :: <C-unsigned-long>;
  c-name: "g_signal_remove_emission_hook";
end;

define C-function g-signal-set-va-marshaller
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter instance_type_ :: <C-long>;
  input parameter va_marshaller_ :: <object> /* <C-XXX-interface> */;
  c-name: "g_signal_set_va_marshaller";
end;

define C-function g-signal-stop-emission
  input parameter instance_ :: <C-void*>;
  input parameter signal_id_ :: <C-unsigned-int>;
  input parameter detail_ :: <C-unsigned-int>;
  c-name: "g_signal_stop_emission";
end;

define C-function g-signal-stop-emission-by-name
  input parameter instance_ :: <C-void*>;
  input parameter detailed_signal_ :: <C-string>;
  c-name: "g_signal_stop_emission_by_name";
end;

define C-function g-signal-type-cclosure-new
  input parameter itype_ :: <C-long>;
  input parameter struct_offset_ :: <C-unsigned-int>;
  result res :: <GClosure>;
  c-name: "g_signal_type_cclosure_new";
end;

define C-function g-source-set-closure
  input parameter source_ :: <GSource>;
  input parameter closure_ :: <GClosure>;
  c-name: "g_source_set_closure";
end;

define C-function g-source-set-dummy-callback
  input parameter source_ :: <GSource>;
  c-name: "g_source_set_dummy_callback";
end;

define C-function g-strdup-value-contents
  input parameter value_ :: <GValue>;
  result res :: <C-string>;
  c-name: "g_strdup_value_contents";
end;

define C-function g-type-add-class-private
  input parameter class_type_ :: <C-long>;
  input parameter private_size_ :: <C-unsigned-long>;
  c-name: "g_type_add_class_private";
end;

define C-function g-type-add-interface-dynamic
  input parameter instance_type_ :: <C-long>;
  input parameter interface_type_ :: <C-long>;
  input parameter plugin_ :: <GTypePlugin>;
  c-name: "g_type_add_interface_dynamic";
end;

define C-function g-type-add-interface-static
  input parameter instance_type_ :: <C-long>;
  input parameter interface_type_ :: <C-long>;
  input parameter info_ :: <GInterfaceInfo>;
  c-name: "g_type_add_interface_static";
end;

define C-function g-type-check-class-is-a
  input parameter g_class_ :: <GTypeClass>;
  input parameter is_a_type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_type_check_class_is_a";
end;

define C-function g-type-check-instance
  input parameter instance_ :: <GTypeInstance>;
  result res :: <C-boolean>;
  c-name: "g_type_check_instance";
end;

define C-function g-type-check-instance-is-a
  input parameter instance_ :: <GTypeInstance>;
  input parameter iface_type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_type_check_instance_is_a";
end;

define C-function g-type-check-is-value-type
  input parameter type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_type_check_is_value_type";
end;

define C-function g-type-check-value
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "g_type_check_value";
end;

define C-function g-type-check-value-holds
  input parameter value_ :: <GValue>;
  input parameter type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_type_check_value_holds";
end;

define C-function g-type-children
  input parameter type_ :: <C-long>;
  output parameter n_children_ :: <C-unsigned-int*>;
  result res :: <C-long*>;
  c-name: "g_type_children";
end;

define C-function g-type-default-interface-peek
  input parameter g_type_ :: <C-long>;
  result res :: <GTypeInterface>;
  c-name: "g_type_default_interface_peek";
end;

define C-function g-type-default-interface-ref
  input parameter g_type_ :: <C-long>;
  result res :: <GTypeInterface>;
  c-name: "g_type_default_interface_ref";
end;

define C-function g-type-default-interface-unref
  input parameter g_iface_ :: <GTypeInterface>;
  c-name: "g_type_default_interface_unref";
end;

define C-function g-type-depth
  input parameter type_ :: <C-long>;
  result res :: <C-unsigned-int>;
  c-name: "g_type_depth";
end;

define C-function g-type-ensure
  input parameter type_ :: <C-long>;
  c-name: "g_type_ensure";
end;

define C-function g-type-free-instance
  input parameter instance_ :: <GTypeInstance>;
  c-name: "g_type_free_instance";
end;

define C-function g-type-from-name
  input parameter name_ :: <C-string>;
  result res :: <C-long>;
  c-name: "g_type_from_name";
end;

define C-function g-type-fundamental
  input parameter type_id_ :: <C-long>;
  result res :: <C-long>;
  c-name: "g_type_fundamental";
end;

define C-function g-type-fundamental-next
  result res :: <C-long>;
  c-name: "g_type_fundamental_next";
end;

define C-function g-type-get-plugin
  input parameter type_ :: <C-long>;
  result res :: <GTypePlugin>;
  c-name: "g_type_get_plugin";
end;

define C-function g-type-get-qdata
  input parameter type_ :: <C-long>;
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-void*>;
  c-name: "g_type_get_qdata";
end;

define C-function g-type-get-type-registration-serial
  result res :: <C-unsigned-int>;
  c-name: "g_type_get_type_registration_serial";
end;

define C-function g-type-interfaces
  input parameter type_ :: <C-long>;
  output parameter n_interfaces_ :: <C-unsigned-int*>;
  result res :: <C-long*>;
  c-name: "g_type_interfaces";
end;

define C-function g-type-is-a
  input parameter type_ :: <C-long>;
  input parameter is_a_type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "g_type_is_a";
end;

define C-function g-type-name
  input parameter type_ :: <C-long>;
  result res :: <C-string>;
  c-name: "g_type_name";
end;

define C-function g-type-name-from-class
  input parameter g_class_ :: <GTypeClass>;
  result res :: <C-string>;
  c-name: "g_type_name_from_class";
end;

define C-function g-type-name-from-instance
  input parameter instance_ :: <GTypeInstance>;
  result res :: <C-string>;
  c-name: "g_type_name_from_instance";
end;

define C-function g-type-next-base
  input parameter leaf_type_ :: <C-long>;
  input parameter root_type_ :: <C-long>;
  result res :: <C-long>;
  c-name: "g_type_next_base";
end;

define C-function g-type-parent
  input parameter type_ :: <C-long>;
  result res :: <C-long>;
  c-name: "g_type_parent";
end;

define C-function g-type-qname
  input parameter type_ :: <C-long>;
  result res :: <C-unsigned-int>;
  c-name: "g_type_qname";
end;

define C-function g-type-query
  input parameter type_ :: <C-long>;
  output parameter query_ :: <GTypeQuery>;
  c-name: "g_type_query";
end;

define C-function g-type-register-dynamic
  input parameter parent_type_ :: <C-long>;
  input parameter type_name_ :: <C-string>;
  input parameter plugin_ :: <GTypePlugin>;
  input parameter flags_ :: <GTypeFlags>;
  result res :: <C-long>;
  c-name: "g_type_register_dynamic";
end;

define C-function g-type-register-fundamental
  input parameter type_id_ :: <C-long>;
  input parameter type_name_ :: <C-string>;
  input parameter info_ :: <GTypeInfo>;
  input parameter finfo_ :: <GTypeFundamentalInfo>;
  input parameter flags_ :: <GTypeFlags>;
  result res :: <C-long>;
  c-name: "g_type_register_fundamental";
end;

define C-function g-type-register-static
  input parameter parent_type_ :: <C-long>;
  input parameter type_name_ :: <C-string>;
  input parameter info_ :: <GTypeInfo>;
  input parameter flags_ :: <GTypeFlags>;
  result res :: <C-long>;
  c-name: "g_type_register_static";
end;

define C-function g-type-set-qdata
  input parameter type_ :: <C-long>;
  input parameter quark_ :: <C-unsigned-int>;
  input parameter data_ :: <C-void*>;
  c-name: "g_type_set_qdata";
end;

define C-function g-type-test-flags
  input parameter type_ :: <C-long>;
  input parameter flags_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_type_test_flags";
end;

