module: gtk-internal

define interface
  #include "gtk/gtk.h",
    import: all-recursive,
    exclude: {
      "G_TYPE_INVALID",
      "G_TYPE_NONE",
      "G_TYPE_CHAR",
      "G_TYPE_UCHAR",
      "G_TYPE_BOOLEAN",
      "G_TYPE_INT",
      "G_TYPE_UINT",
      "G_TYPE_LONG",
      "G_TYPE_ULONG",
      "G_TYPE_INT64",
      "G_TYPE_UINT64",
      "G_TYPE_ENUM",
      "G_TYPE_FLAGS",
      "G_TYPE_FLOAT",
      "G_TYPE_DOUBLE",
      "G_TYPE_STRING",
      "G_TYPE_POINTER",
      "G_TYPE_BOXED",
      "G_TYPE_PARAM",
      "G_TYPE_OBJECT",
      "G_TYPE_INTERFACE",
      "_IO_ferror",
      "remove",
      "_IO_sgetn",
      "__woverflow",
      "_IO_funlockfile",
      "__overflow",
      "_IO_flockfile",
      "tmpnam_r",
      "_IO_padn",
      "_IO_ftrylockfile",
      "clock_getcpuclockid",
      "_IO_getc",
      "_IO_free_backup_area",
      "__underflow",
      "__wunderflow",
      "__uflow",
      "fread_unlocked",
      "_IO_peekc_locked",
      "_IO_feof",
      "fflush_unlocked",
      "fwrite_unlocked",
      "dysize",
      "_IO_vfscanf",
      "__wuflow",
      "fgetc_unlocked",
      "stime",
      "_IO_vfprintf",
      "_IO_seekoff",
      "_IO_putc",
      "_IO_seekpos",
      "fputc_unlocked",
      "clock_nanosleep",
      "g_scanner_cur_value", // returns a union of size 8
      "glib_dummy_decl",
      "_g_utf8_make_valid",
      "g_string_append_c_inline", //why that?
      "clock_gettime", "clock_settime", "clock_getres", "alloca", //kludge
      "_gtk_tooltip_focus_out",
      "_gtk_tooltip_handle_event"
      "_gtk_widget_get_cursor_color"
      "_gtk_rc_style_set_rc_property"
      "_gtk_icon_theme_ensure_builtin_cache",
      "_gtk_rc_match_widget_class",
      "_gtk_rc_style_get_color_hashes",
      "_gtk_binding_parse_binding",
      "_gtk_cell_layout_buildable_custom_tag_end",
      "_gtk_action_sync_sensitive",
      "_gtk_binding_entry_add_signall",
      "_gtk_widget_propagate_composited_changed",
      "_gtk_action_sync_menu_visible",
      "_gtk_rc_style_unset_rc_property",
      "_gtk_rc_parse_widget_class_path",
      "_gtk_tooltip_toggle_keyboard_mode",
      "_gtk_rc_free_widget_class_path",
      "_gtk_cell_layout_buildable_custom_tag_start",
      "_gtk_action_sync_visible",
      "_gtk_recent_manager_sync",
      "_gtk_cell_layout_buildable_add_child",
      "_gtk_menu_item_popdown_submenu",
      "_gtk_tooltip_focus_in",
      "_gtk_tooltip_hide",
      "_gtk_rc_context_destroy",
      "_gtk_style_shade",
      "_gtk_widget_get_cursor_gc",
      "_g_param_type_register_static_constant",
      "g_thread_init_with_errorcheck_mutexes",
      "_gtk_container_clear_resize_widgets",
      "_gtk_accel_label_class_get_accelerator_label",
      "_gtk_accel_path_is_valid",
      "_gtk_accel_map_remove_group",
      "_gtk_accel_map_add_group",
      "_gtk_accel_map_init",
      "_gtk_binding_signal_new",
      "_gtk_binding_reset_parsed",
      "_gtk_combo_box_editing_canceled",
      "_gtk_clist_create_cell_layout",
      "_gtk_icon_factory_ensure_default_icons",
      "_gtk_icon_factory_list_ids",
      "_gtk_icon_set_invalidate_caches",
      "_gtk_icon_theme_check_reload",
      "_gtk_menu_bar_cycle_focus",
      "_gtk_modules_settings_changed",
      "_gtk_modules_init",
      "_gtk_get_module_path",
      "_gtk_find_module",
      "_gtk_plug_remove_from_socket",
      "_gtk_plug_add_to_socket",
      "_gtk_get_lc_ctype",
      "_gtk_boolean_handled_accumulator",
      "_gtk_check_button_get_props",
      "_gtk_dialog_set_ignore_separator",
      "_gtk_dialog_get_response_for_widget",
      "_gtk_scrolled_window_get_scrollbar_spacing",
      "_gtk_size_group_queue_resize",
      "_gtk_size_group_compute_requisition",
      "_gtk_size_group_get_child_requisition",
      "_gtk_clipboard_store_all",
      "_gtk_clipboard_handle_event",
      "_gtk_text_tag_table_remove_buffer",
      "_gtk_text_tag_table_add_buffer",
      "_gtk_text_buffer_notify_will_remove_tag",
      "_gtk_text_buffer_get_line_log_attrs",
      "_gtk_text_buffer_get_btree",
      "_gtk_text_buffer_spew",
      "_gtk_button_paint",
      "_gtk_button_set_depressed",
      "_gtk_toolbar_rebuild_menu",
      "_gtk_toolbar_get_default_space_size",
      "_gtk_toolbar_paint_space_line",
      "_gtk_toolbar_elide_underscores",
      "_gtk_tool_button_get_button",
      "_gtk_menu_item_popup_submenu",
      "_gtk_menu_item_is_selectable",
      "_gtk_menu_item_refresh_accel_path",
      "_gtk_tool_item_toolbar_reconfigured",
      "_gtk_tooltips_toggle_keyboard_mode",
      "_gtk_selection_property_notify",
      "_gtk_selection_notify",
      "_gtk_selection_incr_event",
      "_gtk_selection_request",
      "_gtk_drag_dest_handle_event",
      "_gtk_drag_source_handle_event",
      "_gtk_menu_shell_remove_mnemonic",
      "_gtk_menu_shell_add_mnemonic",
      "_gtk_menu_shell_get_popup_delay",
      "_gtk_menu_shell_activate",
      "_gtk_menu_shell_select_last",
      "_gtk_action_emit_activate",
      "_gtk_action_group_emit_post_activate",
      "_gtk_action_group_emit_pre_activate",
      "_gtk_action_group_emit_disconnect_proxy",
      "_gtk_action_group_emit_connect_proxy",
      "_gtk_button_box_child_requisition",
      "_gtk_scale_format_value",
      "_gtk_scale_get_value_size",
      "_gtk_scale_clear_layout",
      "_gtk_range_get_wheel_delta",
      "_gtk_container_focus_sort",
      "_gtk_container_dequeue_resize_handler",
      "_gtk_container_child_composite_name",
      "_gtk_container_queue_resize",
      "_gtk_accel_group_reconnect",
      "_gtk_accel_group_detach",
      "_gtk_accel_group_attach",
      "_gtk_style_init_for_settings",
      "_gtk_style_peek_property_value",
      "_gtk_rc_context_get_default_font_name",
      "_gtk_rc_style_lookup_rc_property",
      "_gtk_rc_init",
      "_gtk_settings_parse_convert",
      "_gtk_rc_property_parser_from_type",
      "_gtk_settings_handle_event",
      "_gtk_settings_reset_rc_values",
      "_gtk_settings_set_property_value_from_rc",
      "_g_signals_destroy",
      "_g_async_queue_get_mutex",
      "_g_log_fallback_handler",
      "_g_getenv_nomalloc",
      "_gtk_window_get_group",
      "_gtk_entry_get_borders",
      "gdk_screen_set_resolution_libgtk_only",
      "gdk_screen_get_font_options_libgtk_only",
      "gdk_screen_set_font_options_libgtk_only",
      "gdk_screen_get_resolution_libgtk_only",
      "g_signal_init",
      "g_value_transforms_init",
      "g_param_spec_types_init",
      "g_object_type_init",
      "g_boxed_type_init",
      "g_param_type_init",
      "g_enum_types_init",
      "g_value_types_init",
      "g_value_c_init",
      "_gtk_widget_peek_colormap",
      "_gtk_widget_propagate_screen_changed",
      "_gtk_widget_propagate_hierarchy_changed",
      "_gtk_widget_get_aux_info",
      "_gtk_widget_grab_notify",
      "_gtk_widget_get_accel_path",
      "_gtk_window_query_nonaccels",
      "_gtk_window_keys_foreach",
      "_gtk_window_set_is_active",
      "_gtk_window_unset_focus_and_default",
      "_gtk_window_set_has_toplevel_focus",
      "_gtk_window_group_get_current_grab",
      "_gtk_window_constrain_size",
      "_gtk_window_reposition",
      "_gtk_window_internal_set_focus"
  	},
    map: {"gchar*" => <byte-string>,
          "char*" => <byte-string>,
          "GCallback" => <function>},
    rename: {"gtk_init" => %gtk-init },
    name-mapper: gtk-name-mapping;

  struct "struct _GTypeInstance",
    superclasses: {<C-void*>};
  struct "struct _AtkImplementorIface",
    superclasses: {<C-void*>};
  struct "struct _GtkTreeModel",
    superclasses: {<C-void*>};
  struct "struct _GtkTreeSortable",
    superclasses: {<C-void*>};
  struct "struct _GtkTreeDragSource",
    superclasses: {<C-void*>};
  struct "struct _GtkTreeDragDest",
    superclasses: {<C-void*>};
  struct "struct _GtkEditable",
    superclasses: {<C-void*>};
  struct "struct _GtkCellEditable",
    superclasses: {<C-void*>};
  struct "struct _GtkFileChooser",
    superclasses: {<C-void*>};
  pointer "char**" => <c-string-vector>,
    superclasses: {<c-vector>};
  function "g_signal_connect_data",
    equate-argument: { 1 => <GObject>};
  function "g_signal_connect_closure",
    equate-argument: { 1 => <GObject>};
  function "g_closure_set_meta_marshal",
    map-argument: { 2 => <object> };
  function "g_closure_new_simple",
    map-argument: { 2 => <object> };
  function "gdk_window_get_pointer",
    output-argument: 2,
    output-argument: 3,
    output-argument: 4;
  struct "struct _GObject",
    superclasses: {<_GTypeInstance>};
  struct "struct _GTypeModule",
    superclasses: {<_GObject>, <_GTypePlugin>};
  struct "struct _AtkGObjectAccessible",
    superclasses: {<_AtkObject>};
  struct "struct _AtkHyperlink",
    superclasses: {<_GObject>, <_AtkAction>};
  struct "struct _AtkNoOpObject",
    superclasses: {<_AtkObject>, <_AtkComponent>, <_AtkAction>, <_AtkEditableText>, <_AtkHypertext>, <_AtkImage>, <_AtkSelection>, <_AtkTable>, <_AtkText>, <_AtkValue>};
  struct "struct _AtkNoOpObjectFactory",
    superclasses: {<_AtkObjectFactory>};
  struct "struct _AtkObject",
    superclasses: {<_GObject>};
  struct "struct _AtkObjectFactory",
    superclasses: {<_GObject>};
  struct "struct _AtkRegistry",
    superclasses: {<_GObject>};
  struct "struct _AtkRelation",
    superclasses: {<_GObject>};
  struct "struct _AtkRelationSet",
    superclasses: {<_GObject>};
  struct "struct _AtkStateSet",
    superclasses: {<_GObject>};
  struct "struct _AtkUtil",
    superclasses: {<_GObject>};
  struct "struct _PangoContext",
    superclasses: {<_GObject>};
  struct "struct _PangoFontFamily",
    superclasses: {<_GObject>};
  struct "struct _PangoFontFace",
    superclasses: {<_GObject>};
  struct "struct _PangoFont",
    superclasses: {<_GObject>};
  struct "struct _PangoFontMap",
    superclasses: {<_GObject>};
  struct "struct _PangoFontset",
    superclasses: {<_GObject>};
  struct "struct _PangoLayout",
    superclasses: {<_GObject>};
  struct "struct _PangoRenderer",
    superclasses: {<_GObject>};
  struct "struct _GdkPixbuf",
    superclasses: {<_GObject>};
  struct "struct _GdkPixbufSimpleAnim",
    superclasses: {<_GdkPixbufAnimation>};
  struct "struct _GdkPixbufAnimation",
    superclasses: {<_GObject>};
  struct "struct _GdkPixbufAnimationIter",
    superclasses: {<_GObject>};
  struct "struct _GdkPixbufLoader",
    superclasses: {<_GObject>};
  struct "struct _GdkDisplay",
    superclasses: {<_GObject>};
  struct "struct _GdkColormap",
    superclasses: {<_GObject>};
  struct "struct _GdkDrawable",
    superclasses: {<_GObject>};
  struct "struct _GdkDragContext",
    superclasses: {<_GObject>};
  struct "struct _GdkGC",
    superclasses: {<_GObject>};
  struct "struct _GdkDisplayManager",
    superclasses: {<_GObject>};
  struct "struct _GdkImage",
    superclasses: {<_GObject>};
  struct "struct _GdkDevice",
    superclasses: {<_GObject>};
  struct "struct _GdkKeymap",
    superclasses: {<_GObject>};
  struct "struct _GdkPangoRenderer",
    superclasses: {<_PangoRenderer>};
  struct "struct _GdkScreen",
    superclasses: {<_GObject>};
  struct "struct _GdkVisual",
    superclasses: {<_GObject>};
  struct "struct _GtkAboutDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkActionGroup",
    superclasses: {<_GObject>};
  struct "struct _GtkAccelGroup",
    superclasses: {<_GObject>};
  struct "struct _GtkAccelLabel",
    superclasses: {<_GtkLabel>, <_AtkImplementorIface>};
  struct "struct _GtkAccelMap",
    superclasses: {<_GObject>};
  struct "struct _GtkAccessible",
    superclasses: {<_AtkObject>};
  struct "struct _GtkAction",
    superclasses: {<_GObject>};
  struct "struct _GtkAspectFrame",
    superclasses: {<_GtkFrame>, <_AtkImplementorIface>};
  struct "struct _GtkAdjustment",
    superclasses: {<_GtkObject>};
  struct "struct _GtkAlignment",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkArrow",
    superclasses: {<_GtkMisc>, <_AtkImplementorIface>};
  struct "struct _GtkAspectFrame",
    superclasses: {<_GtkFrame>, <_AtkImplementorIface>};
  struct "struct _GtkButtonBox",
    superclasses: {<_GtkBox>, <_AtkImplementorIface>};
  struct "struct _GtkBin",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkCalendar",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkBox",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkButtonBox",
    superclasses: {<_GtkBox>, <_AtkImplementorIface>};
  struct "struct _GtkButton",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkCellRendererCombo",
    superclasses: {<_GtkCellRendererText>};
  struct "struct _GtkCellRenderer",
    superclasses: {<_GtkObject>};
  struct "struct _GtkCellRendererProgress",
    superclasses: {<_GtkCellRenderer>};
  struct "struct _GtkCellRendererPixbuf",
    superclasses: {<_GtkCellRenderer>};
  struct "struct _GtkHandleBox",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkCellRendererText",
    superclasses: {<_GtkCellRenderer>};
  struct "struct _GtkCellRendererToggle",
    superclasses: {<_GtkCellRenderer>};
  struct "struct _GtkCellView",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>, <_GtkCellLayout>};
  struct "struct _GtkCheckButton",
    superclasses: {<_GtkToggleButton>, <_AtkImplementorIface>};
  struct "struct _GtkCheckMenuItem",
    superclasses: {<_GtkMenuItem>, <_AtkImplementorIface>};
  struct "struct _GtkCList",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkClipboard",
    superclasses: {<_GObject>};
  struct "struct _GtkColorButton",
    superclasses: {<_GtkButton>, <_AtkImplementorIface>};
  struct "struct _GtkColorSelection",
    superclasses: {<_GtkVBox>, <_AtkImplementorIface>};
  struct "struct _GtkColorSelectionDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkCombo",
    superclasses: {<_GtkHBox>, <_AtkImplementorIface>};
  struct "struct _GtkComboBox",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>, <_GtkCellLayout>, <_GtkCellEditable>};
  struct "struct _GtkComboBoxEntry",
    superclasses: {<_GtkComboBox>, <_AtkImplementorIface>, <_GtkCellLayout>, <_GtkCellEditable>};
  struct "struct _GtkContainer",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkCTree",
    superclasses: {<_GtkCList>, <_AtkImplementorIface>};
  struct "struct _GtkCurve",
    superclasses: {<_GtkDrawingArea>, <_AtkImplementorIface>};
  struct "struct _GtkDialog",
    superclasses: {<_GtkWindow>, <_AtkImplementorIface>};
  struct "struct _GtkDrawingArea",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkEntry",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>, <_GtkCellEditable>, <_GtkEditable>};
  struct "struct _GtkEntryCompletion",
    superclasses: {<_GObject>, <_GtkCellLayout>};
  struct "struct _GtkEventBox",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkExpander",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkFileChooserButton",
    superclasses: {<_GtkHBox>, <_AtkImplementorIface>, <_GtkFileChooser>};
  struct "struct _GtkFileChooserDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>, <_GtkFileChooser>};
  struct "struct _GtkFileChooserWidget",
    superclasses: {<_GtkVBox>, <_AtkImplementorIface>, <_GtkFileChooser>};
  struct "struct _GtkFileFilter",
    superclasses: {<_GtkObject>};
  struct "struct _GtkFileSelection",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkFixed",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkFontButton",
    superclasses: {<_GtkButton>, <_AtkImplementorIface>};
  struct "struct _GtkFontSelection",
    superclasses: {<_GtkVBox>, <_AtkImplementorIface>};
  struct "struct _GtkFontSelectionDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkFrame",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkGammaCurve",
    superclasses: {<_GtkVBox>, <_AtkImplementorIface>};
  struct "struct _GtkHScrollbar",
    superclasses: {<_GtkScrollbar>, <_AtkImplementorIface>};
  struct "struct _GtkHButtonBox",
    superclasses: {<_GtkButtonBox>, <_AtkImplementorIface>};
  struct "struct _GtkHBox",
    superclasses: {<_GtkBox>, <_AtkImplementorIface>};
  struct "struct _GtkHPaned",
    superclasses: {<_GtkPaned>, <_AtkImplementorIface>};
  struct "struct _GtkHRuler",
    superclasses: {<_GtkRuler>, <_AtkImplementorIface>};
  struct "struct _GtkHScale",
    superclasses: {<_GtkScale>, <_AtkImplementorIface>};
  struct "struct _GtkIconFactory",
    superclasses: {<_GObject>};
  struct "struct _GtkHSeparator",
    superclasses: {<_GtkSeparator>, <_AtkImplementorIface>};
  struct "struct _GtkImageMenuItem",
    superclasses: {<_GtkMenuItem>, <_AtkImplementorIface>};
  struct "struct _GtkIconTheme",
    superclasses: {<_GObject>};
  struct "struct _GtkIconView",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>, <_GtkCellLayout>};
  struct "struct _GtkImage",
    superclasses: {<_GtkMisc>, <_AtkImplementorIface>};
  struct "struct _GtkIMContextSimple",
    superclasses: {<_GtkIMContext>};
  struct "struct _GtkIMContext",
    superclasses: {<_GObject>};
  struct "struct _GtkIMMulticontext",
    superclasses: {<_GtkIMContext>};
  struct "struct _GtkRadioToolButton",
    superclasses: {<_GtkToggleToolButton>, <_AtkImplementorIface>};
  struct "struct _GtkInputDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkInvisible",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkItem",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkItemFactory",
    superclasses: {<_GtkObject>};
  struct "struct _GtkLabel",
    superclasses: {<_GtkMisc>, <_AtkImplementorIface>};
  struct "struct _GtkLayout",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkList",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkListItem",
    superclasses: {<_GtkItem>, <_AtkImplementorIface>};
  struct "struct _GtkListStore",
    superclasses: {<_GObject>, <_GtkTreeModel>, <_GtkTreeDragSource>, <_GtkTreeDragDest>, <_GtkTreeSortable>};
  struct "struct _GtkMenu",
    superclasses: {<_GtkMenuShell>, <_AtkImplementorIface>};
  struct "struct _GtkMenuBar",
    superclasses: {<_GtkMenuShell>, <_AtkImplementorIface>};
  struct "struct _GtkMenuItem",
    superclasses: {<_GtkItem>, <_AtkImplementorIface>};
  struct "struct _GtkMenuShell",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkMenuToolButton",
    superclasses: {<_GtkToolButton>, <_AtkImplementorIface>};
  struct "struct _GtkMessageDialog",
    superclasses: {<_GtkDialog>, <_AtkImplementorIface>};
  struct "struct _GtkMisc",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkNotebook",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkObject",
    superclasses: {<_GObject>};
  struct "struct _GtkOptionMenu",
    superclasses: {<_GtkButton>, <_AtkImplementorIface>};
  struct "struct _GtkPaned",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkPixmap",
    superclasses: {<_GtkMisc>, <_AtkImplementorIface>};
  struct "struct _GtkPlug",
    superclasses: {<_GtkWindow>, <_AtkImplementorIface>};
  struct "struct _GtkPreview",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkProgress",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkProgressBar",
    superclasses: {<_GtkProgress>, <_AtkImplementorIface>};
  struct "struct _GtkRadioAction",
    superclasses: {<_GtkToggleAction>};
  struct "struct _GtkRadioButton",
    superclasses: {<_GtkCheckButton>, <_AtkImplementorIface>};
  struct "struct _GtkRadioMenuItem",
    superclasses: {<_GtkCheckMenuItem>, <_AtkImplementorIface>};
  struct "struct _GtkScrollbar",
    superclasses: {<_GtkRange>, <_AtkImplementorIface>};
  struct "struct _GtkRange",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkRcStyle",
    superclasses: {<_GObject>};
  struct "struct _GtkRuler",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkScale",
    superclasses: {<_GtkRange>, <_AtkImplementorIface>};
  struct "struct _GtkSeparatorMenuItem",
    superclasses: {<_GtkMenuItem>, <_AtkImplementorIface>};
  struct "struct _GtkScrolledWindow",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkSeparator",
    superclasses: {<_GtkWidget>, <_AtkImplementorIface>};
  struct "struct _GtkSeparatorToolItem",
    superclasses: {<_GtkToolItem>, <_AtkImplementorIface>};
  struct "struct _GtkSettings",
    superclasses: {<_GObject>};
  struct "struct _GtkSizeGroup",
    superclasses: {<_GObject>};
  struct "struct _GtkSocket",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkSpinButton",
    superclasses: {<_GtkEntry>, <_AtkImplementorIface>, <_GtkCellEditable>, <_GtkEditable>};
  struct "struct _GtkStatusbar",
    superclasses: {<_GtkHBox>, <_AtkImplementorIface>};
  struct "struct _GtkStyle",
    superclasses: {<_GObject>};
  struct "struct _GtkTable",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkTearoffMenuItem",
    superclasses: {<_GtkMenuItem>, <_AtkImplementorIface>};
  struct "struct _GtkTextBuffer",
    superclasses: {<_GObject>};
  struct "struct _GtkTextChildAnchor",
    superclasses: {<_GObject>};
  struct "struct _GtkTextMark",
    superclasses: {<_GObject>};
  struct "struct _GtkTextTag",
    superclasses: {<_GObject>};
  struct "struct _GtkTextTagTable",
    superclasses: {<_GObject>};
  struct "struct _GtkTextView",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkTipsQuery",
    superclasses: {<_GtkLabel>, <_AtkImplementorIface>};
  struct "struct _GtkToggleAction",
    superclasses: {<_GtkAction>};
  struct "struct _GtkToggleButton",
    superclasses: {<_GtkButton>, <_AtkImplementorIface>};
  struct "struct _GtkToggleToolButton",
    superclasses: {<_GtkToolButton>, <_AtkImplementorIface>};
  struct "struct _GtkToolbar",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkToolButton",
    superclasses: {<_GtkToolItem>, <_AtkImplementorIface>};
  struct "struct _GtkToolItem",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkTooltips",
    superclasses: {<_GtkObject>};
  struct "struct _GtkTreeModelFilter",
    superclasses: {<_GObject>, <_GtkTreeModel>, <_GtkTreeDragSource>};
  struct "struct _GtkTreeModelSort",
    superclasses: {<_GObject>, <_GtkTreeModel>, <_GtkTreeDragSource>, <_GtkTreeSortable>};
  struct "struct _GtkTreeSelection",
    superclasses: {<_GObject>};
  struct "struct _GtkTreeStore",
    superclasses: {<_GObject>, <_GtkTreeModel>, <_GtkTreeDragSource>, <_GtkTreeDragDest>, <_GtkTreeSortable>};
  struct "struct _GtkTreeView",
    superclasses: {<_GtkContainer>, <_AtkImplementorIface>};
  struct "struct _GtkTreeViewColumn",
    superclasses: {<_GtkObject>, <_GtkCellLayout>};
  struct "struct _GtkUIManager",
    superclasses: {<_GObject>};
  struct "struct _GtkVButtonBox",
    superclasses: {<_GtkButtonBox>, <_AtkImplementorIface>};
  struct "struct _GtkVBox",
    superclasses: {<_GtkBox>, <_AtkImplementorIface>};
  struct "struct _GtkViewport",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkVPaned",
    superclasses: {<_GtkPaned>, <_AtkImplementorIface>};
  struct "struct _GtkVRuler",
    superclasses: {<_GtkRuler>, <_AtkImplementorIface>};
  struct "struct _GtkVScale",
    superclasses: {<_GtkScale>, <_AtkImplementorIface>};
  struct "struct _GtkVScrollbar",
    superclasses: {<_GtkScrollbar>, <_AtkImplementorIface>};
  struct "struct _GtkVSeparator",
    superclasses: {<_GtkSeparator>, <_AtkImplementorIface>};
  struct "struct _GtkWidget",
    superclasses: {<_GtkObject>, <_AtkImplementorIface>};
  struct "struct _GtkWindow",
    superclasses: {<_GtkBin>, <_AtkImplementorIface>};
  struct "struct _GtkWindowGroup",
    superclasses: {<_GObject>};
end interface;

