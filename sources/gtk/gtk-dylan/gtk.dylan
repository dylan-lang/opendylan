module: gtk
synopsis: generated bindings for the Gtk library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;
ignore(<C-void**>);

define C-pointer-type <GError*> => <GError>;
ignore(<GError*>);

define open C-subtype <GtkAboutDialog> (<GtkDialog>)
  constant slot gtk-about-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-about-dialog-priv :: <GtkAboutDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkAboutDialog*> => <GtkAboutDialog>;

define C-function gtk-about-dialog-new
  result res :: <GtkWidget>;
  c-name: "gtk_about_dialog_new";
end;

define C-function gtk-about-dialog-add-credit-section
  input parameter self :: <GtkAboutDialog>;
  input parameter section_name_ :: <C-string>;
  input parameter people_ :: <C-string>;
  c-name: "gtk_about_dialog_add_credit_section";
end;

define C-function gtk-about-dialog-get-artists
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string*>;
  c-name: "gtk_about_dialog_get_artists";
end;

define C-function gtk-about-dialog-get-authors
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string*>;
  c-name: "gtk_about_dialog_get_authors";
end;

define C-function gtk-about-dialog-get-comments
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_comments";
end;

define C-function gtk-about-dialog-get-copyright
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_copyright";
end;

define C-function gtk-about-dialog-get-documenters
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string*>;
  c-name: "gtk_about_dialog_get_documenters";
end;

define C-function gtk-about-dialog-get-license
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_license";
end;

define C-function gtk-about-dialog-get-license-type
  input parameter self :: <GtkAboutDialog>;
  result res :: <GtkLicense>;
  c-name: "gtk_about_dialog_get_license_type";
end;

define C-function gtk-about-dialog-get-logo
  input parameter self :: <GtkAboutDialog>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_about_dialog_get_logo";
end;

define C-function gtk-about-dialog-get-logo-icon-name
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_logo_icon_name";
end;

define C-function gtk-about-dialog-get-program-name
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_program_name";
end;

define C-function gtk-about-dialog-get-translator-credits
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_translator_credits";
end;

define C-function gtk-about-dialog-get-version
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_version";
end;

define C-function gtk-about-dialog-get-website
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_website";
end;

define C-function gtk-about-dialog-get-website-label
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-string>;
  c-name: "gtk_about_dialog_get_website_label";
end;

define C-function gtk-about-dialog-get-wrap-license
  input parameter self :: <GtkAboutDialog>;
  result res :: <C-boolean>;
  c-name: "gtk_about_dialog_get_wrap_license";
end;

define C-function gtk-about-dialog-set-artists
  input parameter self :: <GtkAboutDialog>;
  input parameter artists_ :: <C-string*>;
  c-name: "gtk_about_dialog_set_artists";
end;

define C-function gtk-about-dialog-set-authors
  input parameter self :: <GtkAboutDialog>;
  input parameter authors_ :: <C-string*>;
  c-name: "gtk_about_dialog_set_authors";
end;

define C-function gtk-about-dialog-set-comments
  input parameter self :: <GtkAboutDialog>;
  input parameter comments_ :: <C-string>;
  c-name: "gtk_about_dialog_set_comments";
end;

define C-function gtk-about-dialog-set-copyright
  input parameter self :: <GtkAboutDialog>;
  input parameter copyright_ :: <C-string>;
  c-name: "gtk_about_dialog_set_copyright";
end;

define C-function gtk-about-dialog-set-documenters
  input parameter self :: <GtkAboutDialog>;
  input parameter documenters_ :: <C-string*>;
  c-name: "gtk_about_dialog_set_documenters";
end;

define C-function gtk-about-dialog-set-license
  input parameter self :: <GtkAboutDialog>;
  input parameter license_ :: <C-string>;
  c-name: "gtk_about_dialog_set_license";
end;

define C-function gtk-about-dialog-set-license-type
  input parameter self :: <GtkAboutDialog>;
  input parameter license_type_ :: <GtkLicense>;
  c-name: "gtk_about_dialog_set_license_type";
end;

define C-function gtk-about-dialog-set-logo
  input parameter self :: <GtkAboutDialog>;
  input parameter logo_ :: <GdkPixbuf>;
  c-name: "gtk_about_dialog_set_logo";
end;

define C-function gtk-about-dialog-set-logo-icon-name
  input parameter self :: <GtkAboutDialog>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_about_dialog_set_logo_icon_name";
end;

define C-function gtk-about-dialog-set-program-name
  input parameter self :: <GtkAboutDialog>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_about_dialog_set_program_name";
end;

define C-function gtk-about-dialog-set-translator-credits
  input parameter self :: <GtkAboutDialog>;
  input parameter translator_credits_ :: <C-string>;
  c-name: "gtk_about_dialog_set_translator_credits";
end;

define C-function gtk-about-dialog-set-version
  input parameter self :: <GtkAboutDialog>;
  input parameter version_ :: <C-string>;
  c-name: "gtk_about_dialog_set_version";
end;

define C-function gtk-about-dialog-set-website
  input parameter self :: <GtkAboutDialog>;
  input parameter website_ :: <C-string>;
  c-name: "gtk_about_dialog_set_website";
end;

define C-function gtk-about-dialog-set-website-label
  input parameter self :: <GtkAboutDialog>;
  input parameter website_label_ :: <C-string>;
  c-name: "gtk_about_dialog_set_website_label";
end;

define C-function gtk-about-dialog-set-wrap-license
  input parameter self :: <GtkAboutDialog>;
  input parameter wrap_license_ :: <C-boolean>;
  c-name: "gtk_about_dialog_set_wrap_license";
end;

define C-struct <_GtkAboutDialogClass>
  constant slot gtk-about-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-about-dialog-class-activate-link :: <C-function-pointer>;
  constant slot gtk-about-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-about-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-about-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-about-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAboutDialogClass>;
end C-struct;

define C-struct <_GtkAboutDialogPrivate>
  pointer-type-name: <GtkAboutDialogPrivate>;
end C-struct;

define constant $gtk-accel-visible = 1;
define constant $gtk-accel-locked = 2;
define constant $gtk-accel-mask = 7;
define constant <GtkAccelFlags> = <C-int>;
define C-pointer-type <GtkAccelFlags*> => <GtkAccelFlags>;

define open C-subtype <GtkAccelGroup> (<GObject>)
  constant slot gtk-accel-group-parent :: <GObject>;
  constant slot gtk-accel-group-priv :: <GtkAccelGroupPrivate>;
end C-subtype;

define C-pointer-type <GtkAccelGroup*> => <GtkAccelGroup>;

define C-function gtk-accel-group-new
  result res :: <GtkAccelGroup>;
  c-name: "gtk_accel_group_new";
end;

define C-function gtk-accel-group-from-accel-closure
  input parameter closure_ :: <GClosure>;
  result res :: <GtkAccelGroup>;
  c-name: "gtk_accel_group_from_accel_closure";
end;

define C-function gtk-accel-group-activate
  input parameter self :: <GtkAccelGroup>;
  input parameter accel_quark_ :: <C-unsigned-int>;
  input parameter acceleratable_ :: <GObject>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_group_activate";
end;

define C-function gtk-accel-group-connect
  input parameter self :: <GtkAccelGroup>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  input parameter accel_flags_ :: <GtkAccelFlags>;
  input parameter closure_ :: <GClosure>;
  c-name: "gtk_accel_group_connect";
end;

define C-function gtk-accel-group-connect-by-path
  input parameter self :: <GtkAccelGroup>;
  input parameter accel_path_ :: <C-string>;
  input parameter closure_ :: <GClosure>;
  c-name: "gtk_accel_group_connect_by_path";
end;

define C-function gtk-accel-group-disconnect
  input parameter self :: <GtkAccelGroup>;
  input parameter closure_ :: <GClosure>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_group_disconnect";
end;

define C-function gtk-accel-group-disconnect-key
  input parameter self :: <GtkAccelGroup>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_group_disconnect_key";
end;

define C-function gtk-accel-group-find
  input parameter self :: <GtkAccelGroup>;
  input parameter find_func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  result res :: <GtkAccelKey>;
  c-name: "gtk_accel_group_find";
end;

define C-function gtk-accel-group-get-is-locked
  input parameter self :: <GtkAccelGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_group_get_is_locked";
end;

define C-function gtk-accel-group-get-modifier-mask
  input parameter self :: <GtkAccelGroup>;
  result res :: <GdkModifierType>;
  c-name: "gtk_accel_group_get_modifier_mask";
end;

define C-function gtk-accel-group-lock
  input parameter self :: <GtkAccelGroup>;
  c-name: "gtk_accel_group_lock";
end;

define C-function gtk-accel-group-query
  input parameter self :: <GtkAccelGroup>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  output parameter n_entries_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_accel_group_query";
end;

define C-function gtk-accel-group-unlock
  input parameter self :: <GtkAccelGroup>;
  c-name: "gtk_accel_group_unlock";
end;

define C-struct <_GtkAccelGroupClass>
  constant slot gtk-accel-group-class-parent-class :: <GObjectClass>;
  constant slot gtk-accel-group-class-accel-changed :: <C-function-pointer>;
  constant slot gtk-accel-group-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-accel-group-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-accel-group-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-accel-group-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAccelGroupClass>;
end C-struct;

define C-struct <_GtkAccelGroupEntry>
  slot gtk-accel-group-entry-key :: <GtkAccelKey>;
  slot gtk-accel-group-entry-closure :: <GClosure>;
  slot gtk-accel-group-entry-accel-path-quark :: <C-unsigned-int>;
  pointer-type-name: <GtkAccelGroupEntry>;
end C-struct;

define C-struct <_GtkAccelGroupPrivate>
  pointer-type-name: <GtkAccelGroupPrivate>;
end C-struct;

define C-struct <_GtkAccelKey>
  slot gtk-accel-key-accel-key :: <C-unsigned-int>;
  slot gtk-accel-key-accel-mods :: <GdkModifierType>;
  slot gtk-accel-key-accel-flags :: <C-unsigned-int>;
  pointer-type-name: <GtkAccelKey>;
end C-struct;

define open C-subtype <GtkAccelLabel> (<GtkLabel>)
  constant slot gtk-accel-label-label :: <GtkLabel>;
  constant slot gtk-accel-label-priv :: <GtkAccelLabelPrivate>;
end C-subtype;

define C-pointer-type <GtkAccelLabel*> => <GtkAccelLabel>;

define C-function gtk-accel-label-new
  input parameter string_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_accel_label_new";
end;

define C-function gtk-accel-label-get-accel-widget
  input parameter self :: <GtkAccelLabel>;
  result res :: <GtkWidget>;
  c-name: "gtk_accel_label_get_accel_widget";
end;

define C-function gtk-accel-label-get-accel-width
  input parameter self :: <GtkAccelLabel>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_accel_label_get_accel_width";
end;

define C-function gtk-accel-label-refetch
  input parameter self :: <GtkAccelLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_label_refetch";
end;

define C-function gtk-accel-label-set-accel-closure
  input parameter self :: <GtkAccelLabel>;
  input parameter accel_closure_ :: <GClosure>;
  c-name: "gtk_accel_label_set_accel_closure";
end;

define C-function gtk-accel-label-set-accel-widget
  input parameter self :: <GtkAccelLabel>;
  input parameter accel_widget_ :: <GtkWidget>;
  c-name: "gtk_accel_label_set_accel_widget";
end;

define C-struct <_GtkAccelLabelClass>
  constant slot gtk-accel-label-class-parent-class :: <GtkLabelClass>;
  constant slot gtk-accel-label-class-signal-quote1 :: <C-string>;
  constant slot gtk-accel-label-class-signal-quote2 :: <C-string>;
  constant slot gtk-accel-label-class-mod-name-shift :: <C-string>;
  constant slot gtk-accel-label-class-mod-name-control :: <C-string>;
  constant slot gtk-accel-label-class-mod-name-alt :: <C-string>;
  constant slot gtk-accel-label-class-mod-separator :: <C-string>;
  constant slot gtk-accel-label-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-accel-label-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-accel-label-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-accel-label-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAccelLabelClass>;
end C-struct;

define C-struct <_GtkAccelLabelPrivate>
  pointer-type-name: <GtkAccelLabelPrivate>;
end C-struct;

define open C-subtype <GtkAccelMap> (<GObject>)
end C-subtype;

define C-pointer-type <GtkAccelMap*> => <GtkAccelMap>;

define C-function gtk-accel-map-add-entry
  input parameter accel_path_ :: <C-string>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  c-name: "gtk_accel_map_add_entry";
end;

define C-function gtk-accel-map-add-filter
  input parameter filter_pattern_ :: <C-string>;
  c-name: "gtk_accel_map_add_filter";
end;

define C-function gtk-accel-map-change-entry
  input parameter accel_path_ :: <C-string>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  input parameter replace_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_map_change_entry";
end;

define C-function gtk-accel-map-foreach
  input parameter data_ :: <C-void*>;
  input parameter foreach_func_ :: <C-function-pointer>;
  c-name: "gtk_accel_map_foreach";
end;

define C-function gtk-accel-map-foreach-unfiltered
  input parameter data_ :: <C-void*>;
  input parameter foreach_func_ :: <C-function-pointer>;
  c-name: "gtk_accel_map_foreach_unfiltered";
end;

define C-function gtk-accel-map-get
  result res :: <GtkAccelMap>;
  c-name: "gtk_accel_map_get";
end;

define C-function gtk-accel-map-load
  input parameter file_name_ :: <C-string>;
  c-name: "gtk_accel_map_load";
end;

define C-function gtk-accel-map-load-fd
  input parameter fd_ :: <C-signed-int>;
  c-name: "gtk_accel_map_load_fd";
end;

define C-function gtk-accel-map-load-scanner
  input parameter scanner_ :: <GScanner>;
  c-name: "gtk_accel_map_load_scanner";
end;

define C-function gtk-accel-map-lock-path
  input parameter accel_path_ :: <C-string>;
  c-name: "gtk_accel_map_lock_path";
end;

define C-function gtk-accel-map-lookup-entry
  input parameter accel_path_ :: <C-string>;
  output parameter key_ :: <GtkAccelKey>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_map_lookup_entry";
end;

define C-function gtk-accel-map-save
  input parameter file_name_ :: <C-string>;
  c-name: "gtk_accel_map_save";
end;

define C-function gtk-accel-map-save-fd
  input parameter fd_ :: <C-signed-int>;
  c-name: "gtk_accel_map_save_fd";
end;

define C-function gtk-accel-map-unlock-path
  input parameter accel_path_ :: <C-string>;
  c-name: "gtk_accel_map_unlock_path";
end;

define C-struct <_GtkAccelMapClass>
  pointer-type-name: <GtkAccelMapClass>;
end C-struct;

define open C-subtype <GtkAccessible> (<AtkObject>)
  constant slot gtk-accessible-parent :: <AtkObject>;
  constant slot gtk-accessible-priv :: <GtkAccessiblePrivate>;
end C-subtype;

define C-pointer-type <GtkAccessible*> => <GtkAccessible>;

define C-function gtk-accessible-connect-widget-destroyed
  input parameter self :: <GtkAccessible>;
  c-name: "gtk_accessible_connect_widget_destroyed";
end;

define C-function gtk-accessible-get-widget
  input parameter self :: <GtkAccessible>;
  result res :: <GtkWidget>;
  c-name: "gtk_accessible_get_widget";
end;

define C-function gtk-accessible-set-widget
  input parameter self :: <GtkAccessible>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_accessible_set_widget";
end;

define C-struct <_GtkAccessibleClass>
  constant slot gtk-accessible-class-parent-class :: <AtkObjectClass>;
  constant slot gtk-accessible-class-connect-widget-destroyed :: <C-function-pointer>;
  constant slot gtk-accessible-class-widget-set :: <C-function-pointer>;
  constant slot gtk-accessible-class-widget-unset :: <C-function-pointer>;
  constant slot gtk-accessible-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-accessible-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAccessibleClass>;
end C-struct;

define C-struct <_GtkAccessiblePrivate>
  pointer-type-name: <GtkAccessiblePrivate>;
end C-struct;

define open C-subtype <GtkAction> (<GObject>)
  constant slot gtk-action-object :: <GObject>;
  constant slot gtk-action-private-data :: <GtkActionPrivate>;
end C-subtype;

define C-pointer-type <GtkAction*> => <GtkAction>;

define C-function gtk-action-new
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter tooltip_ :: <C-string>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkAction>;
  c-name: "gtk_action_new";
end;

define C-function gtk-action-activate
  input parameter self :: <GtkAction>;
  c-name: "gtk_action_activate";
end;

define C-function gtk-action-block-activate
  input parameter self :: <GtkAction>;
  c-name: "gtk_action_block_activate";
end;

define C-function gtk-action-connect-accelerator
  input parameter self :: <GtkAction>;
  c-name: "gtk_action_connect_accelerator";
end;

define C-function gtk-action-create-icon
  input parameter self :: <GtkAction>;
  input parameter icon_size_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_action_create_icon";
end;

define C-function gtk-action-create-menu
  input parameter self :: <GtkAction>;
  result res :: <GtkWidget>;
  c-name: "gtk_action_create_menu";
end;

define C-function gtk-action-create-menu-item
  input parameter self :: <GtkAction>;
  result res :: <GtkWidget>;
  c-name: "gtk_action_create_menu_item";
end;

define C-function gtk-action-create-tool-item
  input parameter self :: <GtkAction>;
  result res :: <GtkWidget>;
  c-name: "gtk_action_create_tool_item";
end;

define C-function gtk-action-disconnect-accelerator
  input parameter self :: <GtkAction>;
  c-name: "gtk_action_disconnect_accelerator";
end;

define C-function gtk-action-get-accel-closure
  input parameter self :: <GtkAction>;
  result res :: <GClosure>;
  c-name: "gtk_action_get_accel_closure";
end;

define C-function gtk-action-get-accel-path
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_accel_path";
end;

define C-function gtk-action-get-always-show-image
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_always_show_image";
end;

define C-function gtk-action-get-gicon
  input parameter self :: <GtkAction>;
  result res :: <GIcon>;
  c-name: "gtk_action_get_gicon";
end;

define C-function gtk-action-get-icon-name
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_icon_name";
end;

define C-function gtk-action-get-is-important
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_is_important";
end;

define C-function gtk-action-get-label
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_label";
end;

define C-function gtk-action-get-name
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_name";
end;

define C-function gtk-action-get-proxies
  input parameter self :: <GtkAction>;
  result res :: <GSList>;
  c-name: "gtk_action_get_proxies";
end;

define C-function gtk-action-get-sensitive
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_sensitive";
end;

define C-function gtk-action-get-short-label
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_short_label";
end;

define C-function gtk-action-get-stock-id
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_stock_id";
end;

define C-function gtk-action-get-tooltip
  input parameter self :: <GtkAction>;
  result res :: <C-string>;
  c-name: "gtk_action_get_tooltip";
end;

define C-function gtk-action-get-visible
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_visible";
end;

define C-function gtk-action-get-visible-horizontal
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_visible_horizontal";
end;

define C-function gtk-action-get-visible-vertical
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_get_visible_vertical";
end;

define C-function gtk-action-is-sensitive
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_is_sensitive";
end;

define C-function gtk-action-is-visible
  input parameter self :: <GtkAction>;
  result res :: <C-boolean>;
  c-name: "gtk_action_is_visible";
end;

define C-function gtk-action-set-accel-group
  input parameter self :: <GtkAction>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_action_set_accel_group";
end;

define C-function gtk-action-set-accel-path
  input parameter self :: <GtkAction>;
  input parameter accel_path_ :: <C-string>;
  c-name: "gtk_action_set_accel_path";
end;

define C-function gtk-action-set-always-show-image
  input parameter self :: <GtkAction>;
  input parameter always_show_ :: <C-boolean>;
  c-name: "gtk_action_set_always_show_image";
end;

define C-function gtk-action-set-gicon
  input parameter self :: <GtkAction>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_action_set_gicon";
end;

define C-function gtk-action-set-icon-name
  input parameter self :: <GtkAction>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_action_set_icon_name";
end;

define C-function gtk-action-set-is-important
  input parameter self :: <GtkAction>;
  input parameter is_important_ :: <C-boolean>;
  c-name: "gtk_action_set_is_important";
end;

define C-function gtk-action-set-label
  input parameter self :: <GtkAction>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_action_set_label";
end;

define C-function gtk-action-set-sensitive
  input parameter self :: <GtkAction>;
  input parameter sensitive_ :: <C-boolean>;
  c-name: "gtk_action_set_sensitive";
end;

define C-function gtk-action-set-short-label
  input parameter self :: <GtkAction>;
  input parameter short_label_ :: <C-string>;
  c-name: "gtk_action_set_short_label";
end;

define C-function gtk-action-set-stock-id
  input parameter self :: <GtkAction>;
  input parameter stock_id_ :: <C-string>;
  c-name: "gtk_action_set_stock_id";
end;

define C-function gtk-action-set-tooltip
  input parameter self :: <GtkAction>;
  input parameter tooltip_ :: <C-string>;
  c-name: "gtk_action_set_tooltip";
end;

define C-function gtk-action-set-visible
  input parameter self :: <GtkAction>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_action_set_visible";
end;

define C-function gtk-action-set-visible-horizontal
  input parameter self :: <GtkAction>;
  input parameter visible_horizontal_ :: <C-boolean>;
  c-name: "gtk_action_set_visible_horizontal";
end;

define C-function gtk-action-set-visible-vertical
  input parameter self :: <GtkAction>;
  input parameter visible_vertical_ :: <C-boolean>;
  c-name: "gtk_action_set_visible_vertical";
end;

define C-function gtk-action-unblock-activate
  input parameter self :: <GtkAction>;
  c-name: "gtk_action_unblock_activate";
end;

define C-struct <_GtkActionClass>
  constant slot gtk-action-class-parent-class :: <GObjectClass>;
  constant slot gtk-action-class-activate :: <C-function-pointer>;
  constant slot gtk-action-class-menu-item-type :: <C-long>;
  constant slot gtk-action-class-toolbar-item-type :: <C-long>;
  constant slot gtk-action-class-create-menu-item :: <C-function-pointer>;
  constant slot gtk-action-class-create-tool-item :: <C-function-pointer>;
  constant slot gtk-action-class-connect-proxy :: <C-function-pointer>;
  constant slot gtk-action-class-disconnect-proxy :: <C-function-pointer>;
  constant slot gtk-action-class-create-menu :: <C-function-pointer>;
  constant slot gtk-action-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-action-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-action-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-action-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkActionClass>;
end C-struct;

define C-struct <_GtkActionEntry>
  slot gtk-action-entry-name :: <C-string>;
  slot gtk-action-entry-stock-id :: <C-string>;
  slot gtk-action-entry-label :: <C-string>;
  slot gtk-action-entry-accelerator :: <C-string>;
  slot gtk-action-entry-tooltip :: <C-string>;
  slot gtk-action-entry-callback :: <C-function-pointer>;
  pointer-type-name: <GtkActionEntry>;
end C-struct;

define open C-subtype <GtkActionGroup> (<GObject>)
  constant slot gtk-action-group-parent :: <GObject>;
  constant slot gtk-action-group-priv :: <GtkActionGroupPrivate>;
end C-subtype;

define C-pointer-type <GtkActionGroup*> => <GtkActionGroup>;

define C-function gtk-action-group-new
  input parameter name_ :: <C-string>;
  result res :: <GtkActionGroup>;
  c-name: "gtk_action_group_new";
end;

define C-function gtk-action-group-add-action
  input parameter self :: <GtkActionGroup>;
  input parameter action_ :: <GtkAction>;
  c-name: "gtk_action_group_add_action";
end;

define C-function gtk-action-group-add-action-with-accel
  input parameter self :: <GtkActionGroup>;
  input parameter action_ :: <GtkAction>;
  input parameter accelerator_ :: <C-string>;
  c-name: "gtk_action_group_add_action_with_accel";
end;

define C-function gtk-action-group-get-action
  input parameter self :: <GtkActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GtkAction>;
  c-name: "gtk_action_group_get_action";
end;

define C-function gtk-action-group-get-name
  input parameter self :: <GtkActionGroup>;
  result res :: <C-string>;
  c-name: "gtk_action_group_get_name";
end;

define C-function gtk-action-group-get-sensitive
  input parameter self :: <GtkActionGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_action_group_get_sensitive";
end;

define C-function gtk-action-group-get-visible
  input parameter self :: <GtkActionGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_action_group_get_visible";
end;

define C-function gtk-action-group-list-actions
  input parameter self :: <GtkActionGroup>;
  result res :: <GList>;
  c-name: "gtk_action_group_list_actions";
end;

define C-function gtk-action-group-remove-action
  input parameter self :: <GtkActionGroup>;
  input parameter action_ :: <GtkAction>;
  c-name: "gtk_action_group_remove_action";
end;

define C-function gtk-action-group-set-sensitive
  input parameter self :: <GtkActionGroup>;
  input parameter sensitive_ :: <C-boolean>;
  c-name: "gtk_action_group_set_sensitive";
end;

define C-function gtk-action-group-set-translate-func
  input parameter self :: <GtkActionGroup>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "gtk_action_group_set_translate_func";
end;

define C-function gtk-action-group-set-translation-domain
  input parameter self :: <GtkActionGroup>;
  input parameter domain_ :: <C-string>;
  c-name: "gtk_action_group_set_translation_domain";
end;

define C-function gtk-action-group-set-visible
  input parameter self :: <GtkActionGroup>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_action_group_set_visible";
end;

define C-function gtk-action-group-translate-string
  input parameter self :: <GtkActionGroup>;
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gtk_action_group_translate_string";
end;

define C-struct <_GtkActionGroupClass>
  constant slot gtk-action-group-class-parent-class :: <GObjectClass>;
  constant slot gtk-action-group-class-get-action :: <C-function-pointer>;
  constant slot gtk-action-group-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-action-group-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-action-group-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-action-group-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkActionGroupClass>;
end C-struct;

define C-struct <_GtkActionGroupPrivate>
  pointer-type-name: <GtkActionGroupPrivate>;
end C-struct;

define C-struct <_GtkActionPrivate>
  pointer-type-name: <GtkActionPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkActionable> (<GtkWidget>)
end C-subtype;

define C-pointer-type <GtkActionable*> => <GtkActionable>;

define C-function gtk-actionable-get-action-name
  input parameter self :: <GtkActionable>;
  result res :: <C-string>;
  c-name: "gtk_actionable_get_action_name";
end;

define C-function gtk-actionable-get-action-target-value
  input parameter self :: <GtkActionable>;
  result res :: <GVariant>;
  c-name: "gtk_actionable_get_action_target_value";
end;

define C-function gtk-actionable-set-action-name
  input parameter self :: <GtkActionable>;
  input parameter action_name_ :: <C-string>;
  c-name: "gtk_actionable_set_action_name";
end;

define C-function gtk-actionable-set-action-target-value
  input parameter self :: <GtkActionable>;
  input parameter target_value_ :: <GVariant>;
  c-name: "gtk_actionable_set_action_target_value";
end;

define C-function gtk-actionable-set-detailed-action-name
  input parameter self :: <GtkActionable>;
  input parameter detailed_action_name_ :: <C-string>;
  c-name: "gtk_actionable_set_detailed_action_name";
end;

define C-struct <_GtkActionableInterface>
  constant slot gtk-actionable-interface-g-iface :: <GTypeInterface>;
  constant slot gtk-actionable-interface-get-action-name :: <C-function-pointer>;
  constant slot gtk-actionable-interface-set-action-name :: <C-function-pointer>;
  constant slot gtk-actionable-interface-get-action-target-value :: <C-function-pointer>;
  constant slot gtk-actionable-interface-set-action-target-value :: <C-function-pointer>;
  pointer-type-name: <GtkActionableInterface>;
end C-struct;

// Interface
define open C-subtype <GtkActivatable> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkActivatable*> => <GtkActivatable>;

define C-function gtk-activatable-do-set-related-action
  input parameter self :: <GtkActivatable>;
  input parameter action_ :: <GtkAction>;
  c-name: "gtk_activatable_do_set_related_action";
end;

define C-function gtk-activatable-get-related-action
  input parameter self :: <GtkActivatable>;
  result res :: <GtkAction>;
  c-name: "gtk_activatable_get_related_action";
end;

define C-function gtk-activatable-get-use-action-appearance
  input parameter self :: <GtkActivatable>;
  result res :: <C-boolean>;
  c-name: "gtk_activatable_get_use_action_appearance";
end;

define C-function gtk-activatable-set-related-action
  input parameter self :: <GtkActivatable>;
  input parameter action_ :: <GtkAction>;
  c-name: "gtk_activatable_set_related_action";
end;

define C-function gtk-activatable-set-use-action-appearance
  input parameter self :: <GtkActivatable>;
  input parameter use_appearance_ :: <C-boolean>;
  c-name: "gtk_activatable_set_use_action_appearance";
end;

define C-function gtk-activatable-sync-action-properties
  input parameter self :: <GtkActivatable>;
  input parameter action_ :: <GtkAction>;
  c-name: "gtk_activatable_sync_action_properties";
end;

define C-struct <_GtkActivatableIface>
  constant slot gtk-activatable-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-activatable-iface-update :: <C-function-pointer>;
  constant slot gtk-activatable-iface-sync-action-properties :: <C-function-pointer>;
  pointer-type-name: <GtkActivatableIface>;
end C-struct;

define open C-subtype <GtkAdjustment> (<GInitiallyUnowned>)
  constant slot gtk-adjustment-parent-instance :: <GInitiallyUnowned>;
  constant slot gtk-adjustment-priv :: <GtkAdjustmentPrivate>;
end C-subtype;

define C-pointer-type <GtkAdjustment*> => <GtkAdjustment>;

define C-function gtk-adjustment-new
  input parameter value_ :: <C-double>;
  input parameter lower_ :: <C-double>;
  input parameter upper_ :: <C-double>;
  input parameter step_increment_ :: <C-double>;
  input parameter page_increment_ :: <C-double>;
  input parameter page_size_ :: <C-double>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_adjustment_new";
end;

define C-function gtk-adjustment-changed
  input parameter self :: <GtkAdjustment>;
  c-name: "gtk_adjustment_changed";
end;

define C-function gtk-adjustment-clamp-page
  input parameter self :: <GtkAdjustment>;
  input parameter lower_ :: <C-double>;
  input parameter upper_ :: <C-double>;
  c-name: "gtk_adjustment_clamp_page";
end;

define C-function gtk-adjustment-configure
  input parameter self :: <GtkAdjustment>;
  input parameter value_ :: <C-double>;
  input parameter lower_ :: <C-double>;
  input parameter upper_ :: <C-double>;
  input parameter step_increment_ :: <C-double>;
  input parameter page_increment_ :: <C-double>;
  input parameter page_size_ :: <C-double>;
  c-name: "gtk_adjustment_configure";
end;

define C-function gtk-adjustment-get-lower
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_lower";
end;

define C-function gtk-adjustment-get-minimum-increment
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_minimum_increment";
end;

define C-function gtk-adjustment-get-page-increment
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_page_increment";
end;

define C-function gtk-adjustment-get-page-size
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_page_size";
end;

define C-function gtk-adjustment-get-step-increment
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_step_increment";
end;

define C-function gtk-adjustment-get-upper
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_upper";
end;

define C-function gtk-adjustment-get-value
  input parameter self :: <GtkAdjustment>;
  result res :: <C-double>;
  c-name: "gtk_adjustment_get_value";
end;

define C-function gtk-adjustment-set-lower
  input parameter self :: <GtkAdjustment>;
  input parameter lower_ :: <C-double>;
  c-name: "gtk_adjustment_set_lower";
end;

define C-function gtk-adjustment-set-page-increment
  input parameter self :: <GtkAdjustment>;
  input parameter page_increment_ :: <C-double>;
  c-name: "gtk_adjustment_set_page_increment";
end;

define C-function gtk-adjustment-set-page-size
  input parameter self :: <GtkAdjustment>;
  input parameter page_size_ :: <C-double>;
  c-name: "gtk_adjustment_set_page_size";
end;

define C-function gtk-adjustment-set-step-increment
  input parameter self :: <GtkAdjustment>;
  input parameter step_increment_ :: <C-double>;
  c-name: "gtk_adjustment_set_step_increment";
end;

define C-function gtk-adjustment-set-upper
  input parameter self :: <GtkAdjustment>;
  input parameter upper_ :: <C-double>;
  c-name: "gtk_adjustment_set_upper";
end;

define C-function gtk-adjustment-set-value
  input parameter self :: <GtkAdjustment>;
  input parameter value_ :: <C-double>;
  c-name: "gtk_adjustment_set_value";
end;

define C-function gtk-adjustment-value-changed
  input parameter self :: <GtkAdjustment>;
  c-name: "gtk_adjustment_value_changed";
end;

define C-struct <_GtkAdjustmentClass>
  constant slot gtk-adjustment-class-parent-class :: <GInitiallyUnownedClass>;
  constant slot gtk-adjustment-class-changed :: <C-function-pointer>;
  constant slot gtk-adjustment-class-value-changed :: <C-function-pointer>;
  constant slot gtk-adjustment-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-adjustment-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-adjustment-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-adjustment-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAdjustmentClass>;
end C-struct;

define C-struct <_GtkAdjustmentPrivate>
  pointer-type-name: <GtkAdjustmentPrivate>;
end C-struct;

define constant $gtk-align-fill = 0;
define constant $gtk-align-start = 1;
define constant $gtk-align-end = 2;
define constant $gtk-align-center = 3;
define constant <GtkAlign> = <C-int>;
define C-pointer-type <GtkAlign*> => <GtkAlign>;

define open C-subtype <GtkAlignment> (<GtkBin>)
  constant slot gtk-alignment-bin :: <GtkBin>;
  constant slot gtk-alignment-priv :: <GtkAlignmentPrivate>;
end C-subtype;

define C-pointer-type <GtkAlignment*> => <GtkAlignment>;

define C-function gtk-alignment-new
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  input parameter xscale_ :: <C-float>;
  input parameter yscale_ :: <C-float>;
  result res :: <GtkWidget>;
  c-name: "gtk_alignment_new";
end;

define C-function gtk-alignment-get-padding
  input parameter self :: <GtkAlignment>;
  output parameter padding_top_ :: <C-unsigned-int*>;
  output parameter padding_bottom_ :: <C-unsigned-int*>;
  output parameter padding_left_ :: <C-unsigned-int*>;
  output parameter padding_right_ :: <C-unsigned-int*>;
  c-name: "gtk_alignment_get_padding";
end;

define C-function gtk-alignment-set
  input parameter self :: <GtkAlignment>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  input parameter xscale_ :: <C-float>;
  input parameter yscale_ :: <C-float>;
  c-name: "gtk_alignment_set";
end;

define C-function gtk-alignment-set-padding
  input parameter self :: <GtkAlignment>;
  input parameter padding_top_ :: <C-unsigned-int>;
  input parameter padding_bottom_ :: <C-unsigned-int>;
  input parameter padding_left_ :: <C-unsigned-int>;
  input parameter padding_right_ :: <C-unsigned-int>;
  c-name: "gtk_alignment_set_padding";
end;

define C-struct <_GtkAlignmentClass>
  constant slot gtk-alignment-class-parent-class :: <GtkBinClass>;
  constant slot gtk-alignment-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-alignment-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-alignment-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-alignment-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAlignmentClass>;
end C-struct;

define C-struct <_GtkAlignmentPrivate>
  pointer-type-name: <GtkAlignmentPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkAppChooser> (<GtkWidget>)
end C-subtype;

define C-pointer-type <GtkAppChooser*> => <GtkAppChooser>;

define C-function gtk-app-chooser-get-app-info
  input parameter self :: <GtkAppChooser>;
  result res :: <GAppInfo>;
  c-name: "gtk_app_chooser_get_app_info";
end;

define C-function gtk-app-chooser-get-content-type
  input parameter self :: <GtkAppChooser>;
  result res :: <C-string>;
  c-name: "gtk_app_chooser_get_content_type";
end;

define C-function gtk-app-chooser-refresh
  input parameter self :: <GtkAppChooser>;
  c-name: "gtk_app_chooser_refresh";
end;

define open C-subtype <GtkAppChooserButton> (<GtkComboBox>)
  constant slot gtk-app-chooser-button-parent :: <GtkComboBox>;
  constant slot gtk-app-chooser-button-priv :: <GtkAppChooserButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkAppChooserButton*> => <GtkAppChooserButton>;

define C-function gtk-app-chooser-button-new
  input parameter content_type_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_app_chooser_button_new";
end;

define C-function gtk-app-chooser-button-append-custom-item
  input parameter self :: <GtkAppChooserButton>;
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_app_chooser_button_append_custom_item";
end;

define C-function gtk-app-chooser-button-append-separator
  input parameter self :: <GtkAppChooserButton>;
  c-name: "gtk_app_chooser_button_append_separator";
end;

define C-function gtk-app-chooser-button-get-heading
  input parameter self :: <GtkAppChooserButton>;
  result res :: <C-string>;
  c-name: "gtk_app_chooser_button_get_heading";
end;

define C-function gtk-app-chooser-button-get-show-default-item
  input parameter self :: <GtkAppChooserButton>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_button_get_show_default_item";
end;

define C-function gtk-app-chooser-button-get-show-dialog-item
  input parameter self :: <GtkAppChooserButton>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_button_get_show_dialog_item";
end;

define C-function gtk-app-chooser-button-set-active-custom-item
  input parameter self :: <GtkAppChooserButton>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_app_chooser_button_set_active_custom_item";
end;

define C-function gtk-app-chooser-button-set-heading
  input parameter self :: <GtkAppChooserButton>;
  input parameter heading_ :: <C-string>;
  c-name: "gtk_app_chooser_button_set_heading";
end;

define C-function gtk-app-chooser-button-set-show-default-item
  input parameter self :: <GtkAppChooserButton>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_button_set_show_default_item";
end;

define C-function gtk-app-chooser-button-set-show-dialog-item
  input parameter self :: <GtkAppChooserButton>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_button_set_show_dialog_item";
end;

define C-struct <_GtkAppChooserButtonClass>
  constant slot gtk-app-chooser-button-class-parent-class :: <GtkComboBoxClass>;
  constant slot gtk-app-chooser-button-class-custom-item-activated :: <C-function-pointer>;
  constant slot gtk-app-chooser-button-class-padding :: <C-void*>;
  pointer-type-name: <GtkAppChooserButtonClass>;
end C-struct;

define C-struct <_GtkAppChooserButtonPrivate>
  pointer-type-name: <GtkAppChooserButtonPrivate>;
end C-struct;

define open C-subtype <GtkAppChooserDialog> (<GtkDialog>)
  constant slot gtk-app-chooser-dialog-parent :: <GtkDialog>;
  constant slot gtk-app-chooser-dialog-priv :: <GtkAppChooserDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkAppChooserDialog*> => <GtkAppChooserDialog>;

define C-function gtk-app-chooser-dialog-new
  input parameter parent_ :: <GtkWindow>;
  input parameter flags_ :: <GtkDialogFlags>;
  input parameter file_ :: <GFile>;
  result res :: <GtkWidget>;
  c-name: "gtk_app_chooser_dialog_new";
end;

define C-function gtk-app-chooser-dialog-new-for-content-type
  input parameter parent_ :: <GtkWindow>;
  input parameter flags_ :: <GtkDialogFlags>;
  input parameter content_type_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_app_chooser_dialog_new_for_content_type";
end;

define C-function gtk-app-chooser-dialog-get-heading
  input parameter self :: <GtkAppChooserDialog>;
  result res :: <C-string>;
  c-name: "gtk_app_chooser_dialog_get_heading";
end;

define C-function gtk-app-chooser-dialog-get-widget
  input parameter self :: <GtkAppChooserDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_app_chooser_dialog_get_widget";
end;

define C-function gtk-app-chooser-dialog-set-heading
  input parameter self :: <GtkAppChooserDialog>;
  input parameter heading_ :: <C-string>;
  c-name: "gtk_app_chooser_dialog_set_heading";
end;

define C-struct <_GtkAppChooserDialogClass>
  constant slot gtk-app-chooser-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-app-chooser-dialog-class-padding :: <C-void*>;
  pointer-type-name: <GtkAppChooserDialogClass>;
end C-struct;

define C-struct <_GtkAppChooserDialogPrivate>
  pointer-type-name: <GtkAppChooserDialogPrivate>;
end C-struct;

define open C-subtype <GtkAppChooserWidget> (<GtkBox>)
  constant slot gtk-app-chooser-widget-parent :: <GtkBox>;
  constant slot gtk-app-chooser-widget-priv :: <GtkAppChooserWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkAppChooserWidget*> => <GtkAppChooserWidget>;

define C-function gtk-app-chooser-widget-new
  input parameter content_type_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_app_chooser_widget_new";
end;

define C-function gtk-app-chooser-widget-get-default-text
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-string>;
  c-name: "gtk_app_chooser_widget_get_default_text";
end;

define C-function gtk-app-chooser-widget-get-show-all
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_get_show_all";
end;

define C-function gtk-app-chooser-widget-get-show-default
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_get_show_default";
end;

define C-function gtk-app-chooser-widget-get-show-fallback
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_get_show_fallback";
end;

define C-function gtk-app-chooser-widget-get-show-other
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_get_show_other";
end;

define C-function gtk-app-chooser-widget-get-show-recommended
  input parameter self :: <GtkAppChooserWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_get_show_recommended";
end;

define C-function gtk-app-chooser-widget-set-default-text
  input parameter self :: <GtkAppChooserWidget>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_app_chooser_widget_set_default_text";
end;

define C-function gtk-app-chooser-widget-set-show-all
  input parameter self :: <GtkAppChooserWidget>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_set_show_all";
end;

define C-function gtk-app-chooser-widget-set-show-default
  input parameter self :: <GtkAppChooserWidget>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_set_show_default";
end;

define C-function gtk-app-chooser-widget-set-show-fallback
  input parameter self :: <GtkAppChooserWidget>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_set_show_fallback";
end;

define C-function gtk-app-chooser-widget-set-show-other
  input parameter self :: <GtkAppChooserWidget>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_set_show_other";
end;

define C-function gtk-app-chooser-widget-set-show-recommended
  input parameter self :: <GtkAppChooserWidget>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_app_chooser_widget_set_show_recommended";
end;

define C-struct <_GtkAppChooserWidgetClass>
  constant slot gtk-app-chooser-widget-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-app-chooser-widget-class-application-selected :: <C-function-pointer>;
  constant slot gtk-app-chooser-widget-class-application-activated :: <C-function-pointer>;
  constant slot gtk-app-chooser-widget-class-populate-popup :: <C-function-pointer>;
  constant slot gtk-app-chooser-widget-class-padding :: <C-void*>;
  pointer-type-name: <GtkAppChooserWidgetClass>;
end C-struct;

define C-struct <_GtkAppChooserWidgetPrivate>
  pointer-type-name: <GtkAppChooserWidgetPrivate>;
end C-struct;

define open C-subtype <GtkApplication> (<GApplication>)
  constant slot gtk-application-parent :: <GApplication>;
  constant slot gtk-application-priv :: <GtkApplicationPrivate>;
end C-subtype;

define C-pointer-type <GtkApplication*> => <GtkApplication>;

define C-function gtk-application-new
  input parameter application_id_ :: <C-string>;
  input parameter flags_ :: <GApplicationFlags>;
  result res :: <GtkApplication>;
  c-name: "gtk_application_new";
end;

define C-function gtk-application-add-accelerator
  input parameter self :: <GtkApplication>;
  input parameter accelerator_ :: <C-string>;
  input parameter action_name_ :: <C-string>;
  input parameter parameter_ :: <GVariant>;
  c-name: "gtk_application_add_accelerator";
end;

define C-function gtk-application-add-window
  input parameter self :: <GtkApplication>;
  input parameter window_ :: <GtkWindow>;
  c-name: "gtk_application_add_window";
end;

define C-function gtk-application-get-app-menu
  input parameter self :: <GtkApplication>;
  result res :: <GMenuModel>;
  c-name: "gtk_application_get_app_menu";
end;

define C-function gtk-application-get-menubar
  input parameter self :: <GtkApplication>;
  result res :: <GMenuModel>;
  c-name: "gtk_application_get_menubar";
end;

define C-function gtk-application-get-windows
  input parameter self :: <GtkApplication>;
  result res :: <GList>;
  c-name: "gtk_application_get_windows";
end;

define C-function gtk-application-inhibit
  input parameter self :: <GtkApplication>;
  input parameter window_ :: <GtkWindow>;
  input parameter flags_ :: <GtkApplicationInhibitFlags>;
  input parameter reason_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_application_inhibit";
end;

define C-function gtk-application-is-inhibited
  input parameter self :: <GtkApplication>;
  input parameter flags_ :: <GtkApplicationInhibitFlags>;
  result res :: <C-boolean>;
  c-name: "gtk_application_is_inhibited";
end;

define C-function gtk-application-remove-accelerator
  input parameter self :: <GtkApplication>;
  input parameter action_name_ :: <C-string>;
  input parameter parameter_ :: <GVariant>;
  c-name: "gtk_application_remove_accelerator";
end;

define C-function gtk-application-remove-window
  input parameter self :: <GtkApplication>;
  input parameter window_ :: <GtkWindow>;
  c-name: "gtk_application_remove_window";
end;

define C-function gtk-application-set-app-menu
  input parameter self :: <GtkApplication>;
  input parameter app_menu_ :: <GMenuModel>;
  c-name: "gtk_application_set_app_menu";
end;

define C-function gtk-application-set-menubar
  input parameter self :: <GtkApplication>;
  input parameter menubar_ :: <GMenuModel>;
  c-name: "gtk_application_set_menubar";
end;

define C-function gtk-application-uninhibit
  input parameter self :: <GtkApplication>;
  input parameter cookie_ :: <C-unsigned-int>;
  c-name: "gtk_application_uninhibit";
end;

define C-struct <_GtkApplicationClass>
  constant slot gtk-application-class-parent-class :: <GApplicationClass>;
  constant slot gtk-application-class-window-added :: <C-function-pointer>;
  constant slot gtk-application-class-window-removed :: <C-function-pointer>;
  constant slot gtk-application-class-padding :: <C-void*>;
  pointer-type-name: <GtkApplicationClass>;
end C-struct;

define constant $gtk-application-inhibit-logout = 1;
define constant $gtk-application-inhibit-switch = 2;
define constant $gtk-application-inhibit-suspend = 4;
define constant $gtk-application-inhibit-idle = 8;
define constant <GtkApplicationInhibitFlags> = <C-int>;
define C-pointer-type <GtkApplicationInhibitFlags*> => <GtkApplicationInhibitFlags>;

define C-struct <_GtkApplicationPrivate>
  pointer-type-name: <GtkApplicationPrivate>;
end C-struct;

define open C-subtype <GtkApplicationWindow> (<GtkWindow>)
  constant slot gtk-application-window-parent-instance :: <GtkWindow>;
  constant slot gtk-application-window-priv :: <GtkApplicationWindowPrivate>;
end C-subtype;

define C-pointer-type <GtkApplicationWindow*> => <GtkApplicationWindow>;

define C-function gtk-application-window-new
  input parameter application_ :: <GtkApplication>;
  result res :: <GtkWidget>;
  c-name: "gtk_application_window_new";
end;

define C-function gtk-application-window-get-show-menubar
  input parameter self :: <GtkApplicationWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_application_window_get_show_menubar";
end;

define C-function gtk-application-window-set-show-menubar
  input parameter self :: <GtkApplicationWindow>;
  input parameter show_menubar_ :: <C-boolean>;
  c-name: "gtk_application_window_set_show_menubar";
end;

define C-struct <_GtkApplicationWindowClass>
  constant slot gtk-application-window-class-parent-class :: <GtkWindowClass>;
  constant slot gtk-application-window-class-padding :: <C-void*>;
  pointer-type-name: <GtkApplicationWindowClass>;
end C-struct;

define C-struct <_GtkApplicationWindowPrivate>
  pointer-type-name: <GtkApplicationWindowPrivate>;
end C-struct;

define open C-subtype <GtkArrow> (<GtkMisc>)
  constant slot gtk-arrow-misc :: <GtkMisc>;
  constant slot gtk-arrow-priv :: <GtkArrowPrivate>;
end C-subtype;

define C-pointer-type <GtkArrow*> => <GtkArrow>;

define C-function gtk-arrow-new
  input parameter arrow_type_ :: <GtkArrowType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  result res :: <GtkWidget>;
  c-name: "gtk_arrow_new";
end;

define C-function gtk-arrow-set
  input parameter self :: <GtkArrow>;
  input parameter arrow_type_ :: <GtkArrowType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  c-name: "gtk_arrow_set";
end;

define C-struct <_GtkArrowClass>
  constant slot gtk-arrow-class-parent-class :: <GtkMiscClass>;
  constant slot gtk-arrow-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-arrow-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-arrow-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-arrow-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkArrowClass>;
end C-struct;

define constant $gtk-arrows-both = 0;
define constant $gtk-arrows-start = 1;
define constant $gtk-arrows-end = 2;
define constant <GtkArrowPlacement> = <C-int>;
define C-pointer-type <GtkArrowPlacement*> => <GtkArrowPlacement>;

define C-struct <_GtkArrowPrivate>
  pointer-type-name: <GtkArrowPrivate>;
end C-struct;

define constant $gtk-arrow-up = 0;
define constant $gtk-arrow-down = 1;
define constant $gtk-arrow-left = 2;
define constant $gtk-arrow-right = 3;
define constant $gtk-arrow-none = 4;
define constant <GtkArrowType> = <C-int>;
define C-pointer-type <GtkArrowType*> => <GtkArrowType>;

define open C-subtype <GtkAspectFrame> (<GtkFrame>)
  constant slot gtk-aspect-frame-frame :: <GtkFrame>;
  constant slot gtk-aspect-frame-priv :: <GtkAspectFramePrivate>;
end C-subtype;

define C-pointer-type <GtkAspectFrame*> => <GtkAspectFrame>;

define C-function gtk-aspect-frame-new
  input parameter label_ :: <C-string>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  input parameter ratio_ :: <C-float>;
  input parameter obey_child_ :: <C-boolean>;
  result res :: <GtkWidget>;
  c-name: "gtk_aspect_frame_new";
end;

define C-function gtk-aspect-frame-set
  input parameter self :: <GtkAspectFrame>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  input parameter ratio_ :: <C-float>;
  input parameter obey_child_ :: <C-boolean>;
  c-name: "gtk_aspect_frame_set";
end;

define C-struct <_GtkAspectFrameClass>
  constant slot gtk-aspect-frame-class-parent-class :: <GtkFrameClass>;
  constant slot gtk-aspect-frame-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-aspect-frame-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-aspect-frame-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-aspect-frame-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkAspectFrameClass>;
end C-struct;

define C-struct <_GtkAspectFramePrivate>
  pointer-type-name: <GtkAspectFramePrivate>;
end C-struct;

define open C-subtype <GtkAssistant> (<GtkWindow>)
  constant slot gtk-assistant-parent :: <GtkWindow>;
  constant slot gtk-assistant-priv :: <GtkAssistantPrivate>;
end C-subtype;

define C-pointer-type <GtkAssistant*> => <GtkAssistant>;

define C-function gtk-assistant-new
  result res :: <GtkWidget>;
  c-name: "gtk_assistant_new";
end;

define C-function gtk-assistant-add-action-widget
  input parameter self :: <GtkAssistant>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_assistant_add_action_widget";
end;

define C-function gtk-assistant-append-page
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_assistant_append_page";
end;

define C-function gtk-assistant-commit
  input parameter self :: <GtkAssistant>;
  c-name: "gtk_assistant_commit";
end;

define C-function gtk-assistant-get-current-page
  input parameter self :: <GtkAssistant>;
  result res :: <C-signed-int>;
  c-name: "gtk_assistant_get_current_page";
end;

define C-function gtk-assistant-get-n-pages
  input parameter self :: <GtkAssistant>;
  result res :: <C-signed-int>;
  c-name: "gtk_assistant_get_n_pages";
end;

define C-function gtk-assistant-get-nth-page
  input parameter self :: <GtkAssistant>;
  input parameter page_num_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_assistant_get_nth_page";
end;

define C-function gtk-assistant-get-page-complete
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_assistant_get_page_complete";
end;

define C-function gtk-assistant-get-page-header-image
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_assistant_get_page_header_image";
end;

define C-function gtk-assistant-get-page-side-image
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_assistant_get_page_side_image";
end;

define C-function gtk-assistant-get-page-title
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_assistant_get_page_title";
end;

define C-function gtk-assistant-get-page-type
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <GtkAssistantPageType>;
  c-name: "gtk_assistant_get_page_type";
end;

define C-function gtk-assistant-insert-page
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_assistant_insert_page";
end;

define C-function gtk-assistant-next-page
  input parameter self :: <GtkAssistant>;
  c-name: "gtk_assistant_next_page";
end;

define C-function gtk-assistant-prepend-page
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_assistant_prepend_page";
end;

define C-function gtk-assistant-previous-page
  input parameter self :: <GtkAssistant>;
  c-name: "gtk_assistant_previous_page";
end;

define C-function gtk-assistant-remove-action-widget
  input parameter self :: <GtkAssistant>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_assistant_remove_action_widget";
end;

define C-function gtk-assistant-remove-page
  input parameter self :: <GtkAssistant>;
  input parameter page_num_ :: <C-signed-int>;
  c-name: "gtk_assistant_remove_page";
end;

define C-function gtk-assistant-set-current-page
  input parameter self :: <GtkAssistant>;
  input parameter page_num_ :: <C-signed-int>;
  c-name: "gtk_assistant_set_current_page";
end;

define C-function gtk-assistant-set-forward-page-func
  input parameter self :: <GtkAssistant>;
  input parameter page_func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_assistant_set_forward_page_func";
end;

define C-function gtk-assistant-set-page-complete
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter complete_ :: <C-boolean>;
  c-name: "gtk_assistant_set_page_complete";
end;

define C-function gtk-assistant-set-page-header-image
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_assistant_set_page_header_image";
end;

define C-function gtk-assistant-set-page-side-image
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_assistant_set_page_side_image";
end;

define C-function gtk-assistant-set-page-title
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_assistant_set_page_title";
end;

define C-function gtk-assistant-set-page-type
  input parameter self :: <GtkAssistant>;
  input parameter page_ :: <GtkWidget>;
  input parameter type_ :: <GtkAssistantPageType>;
  c-name: "gtk_assistant_set_page_type";
end;

define C-function gtk-assistant-update-buttons-state
  input parameter self :: <GtkAssistant>;
  c-name: "gtk_assistant_update_buttons_state";
end;

define C-struct <_GtkAssistantClass>
  constant slot gtk-assistant-class-parent-class :: <GtkWindowClass>;
  constant slot gtk-assistant-class-prepare :: <C-function-pointer>;
  constant slot gtk-assistant-class-apply :: <C-function-pointer>;
  constant slot gtk-assistant-class-close :: <C-function-pointer>;
  constant slot gtk-assistant-class-cancel :: <C-function-pointer>;
  constant slot gtk-assistant-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-assistant-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-assistant-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-assistant-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-assistant-class-_gtk-reserved5 :: <C-void*>;
  pointer-type-name: <GtkAssistantClass>;
end C-struct;

define constant $gtk-assistant-page-content = 0;
define constant $gtk-assistant-page-intro = 1;
define constant $gtk-assistant-page-confirm = 2;
define constant $gtk-assistant-page-summary = 3;
define constant $gtk-assistant-page-progress = 4;
define constant $gtk-assistant-page-custom = 5;
define constant <GtkAssistantPageType> = <C-int>;
define C-pointer-type <GtkAssistantPageType*> => <GtkAssistantPageType>;

define C-struct <_GtkAssistantPrivate>
  pointer-type-name: <GtkAssistantPrivate>;
end C-struct;

define constant $gtk-expand = 1;
define constant $gtk-shrink = 2;
define constant $gtk-fill = 4;
define constant <GtkAttachOptions> = <C-int>;
define C-pointer-type <GtkAttachOptions*> => <GtkAttachOptions>;

define constant $gtk-binary-age = 402;

define open C-subtype <GtkBin> (<GtkContainer>)
  constant slot gtk-bin-container :: <GtkContainer>;
  constant slot gtk-bin-priv :: <GtkBinPrivate>;
end C-subtype;

define C-pointer-type <GtkBin*> => <GtkBin>;

define C-function gtk-bin-get-child
  input parameter self :: <GtkBin>;
  result res :: <GtkWidget>;
  c-name: "gtk_bin_get_child";
end;

define C-struct <_GtkBinClass>
  constant slot gtk-bin-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-bin-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-bin-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-bin-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-bin-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkBinClass>;
end C-struct;

define C-struct <_GtkBinPrivate>
  pointer-type-name: <GtkBinPrivate>;
end C-struct;

define C-struct <_GtkBindingArg>
  slot gtk-binding-arg-arg-type :: <C-long>;
  pointer-type-name: <GtkBindingArg>;
end C-struct;

define C-struct <_GtkBindingEntry>
  slot gtk-binding-entry-keyval :: <C-unsigned-int>;
  slot gtk-binding-entry-modifiers :: <GdkModifierType>;
  slot gtk-binding-entry-binding-set :: <GtkBindingSet>;
  slot gtk-binding-entry-destroyed :: <C-unsigned-int>;
  slot gtk-binding-entry-in-emission :: <C-unsigned-int>;
  slot gtk-binding-entry-marks-unbound :: <C-unsigned-int>;
  slot gtk-binding-entry-set-next :: <GtkBindingEntry>;
  slot gtk-binding-entry-hash-next :: <GtkBindingEntry>;
  slot gtk-binding-entry-signals :: <GtkBindingSignal>;
  pointer-type-name: <GtkBindingEntry>;
end C-struct;

define C-function gtk-binding-entry-add-signal-from-string
  input parameter binding_set_ :: <GtkBindingSet>;
  input parameter signal_desc_ :: <C-string>;
  result res :: <GTokenType>;
  c-name: "gtk_binding_entry_add_signal_from_string";
end;

define C-function gtk-binding-entry-add-signall
  input parameter binding_set_ :: <GtkBindingSet>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  input parameter signal_name_ :: <C-string>;
  input parameter binding_args_ :: <GSList>;
  c-name: "gtk_binding_entry_add_signall";
end;

define C-function gtk-binding-entry-remove
  input parameter binding_set_ :: <GtkBindingSet>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  c-name: "gtk_binding_entry_remove";
end;

define C-function gtk-binding-entry-skip
  input parameter binding_set_ :: <GtkBindingSet>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  c-name: "gtk_binding_entry_skip";
end;

define C-struct <_GtkBindingSet>
  slot gtk-binding-set-set-name :: <C-string>;
  slot gtk-binding-set-priority :: <C-signed-int>;
  slot gtk-binding-set-widget-path-pspecs :: <GSList>;
  slot gtk-binding-set-widget-class-pspecs :: <GSList>;
  slot gtk-binding-set-class-branch-pspecs :: <GSList>;
  slot gtk-binding-set-entries :: <GtkBindingEntry>;
  slot gtk-binding-set-current :: <GtkBindingEntry>;
  slot gtk-binding-set-parsed :: <C-unsigned-int>;
  pointer-type-name: <GtkBindingSet>;
end C-struct;

define C-function gtk-binding-set-activate
  input parameter self :: <GtkBindingSet>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  input parameter object_ :: <GObject>;
  result res :: <C-boolean>;
  c-name: "gtk_binding_set_activate";
end;

define C-function gtk-binding-set-add-path
  input parameter self :: <GtkBindingSet>;
  input parameter path_type_ :: <GtkPathType>;
  input parameter path_pattern_ :: <C-string>;
  input parameter priority_ :: <GtkPathPriorityType>;
  c-name: "gtk_binding_set_add_path";
end;

define C-function gtk-binding-set-find
  input parameter set_name_ :: <C-string>;
  result res :: <GtkBindingSet>;
  c-name: "gtk_binding_set_find";
end;

define C-struct <_GtkBindingSignal>
  slot gtk-binding-signal-next :: <GtkBindingSignal>;
  slot gtk-binding-signal-signal-name :: <C-string>;
  slot gtk-binding-signal-n-args :: <C-unsigned-int>;
  slot gtk-binding-signal-args :: <GtkBindingArg>;
  pointer-type-name: <GtkBindingSignal>;
end C-struct;

define C-struct <_GtkBorder>
  slot gtk-border-left :: <C-signed-short>;
  slot gtk-border-right :: <C-signed-short>;
  slot gtk-border-top :: <C-signed-short>;
  slot gtk-border-bottom :: <C-signed-short>;
  pointer-type-name: <GtkBorder>;
end C-struct;

define C-function gtk-border-new
  result res :: <GtkBorder>;
  c-name: "gtk_border_new";
end;

define C-function gtk-border-copy
  input parameter self :: <GtkBorder>;
  result res :: <GtkBorder>;
  c-name: "gtk_border_copy";
end;

define C-function gtk-border-free
  input parameter self :: <GtkBorder>;
  c-name: "gtk_border_free";
end;

define constant $gtk-border-style-none = 0;
define constant $gtk-border-style-solid = 1;
define constant $gtk-border-style-inset = 2;
define constant $gtk-border-style-outset = 3;
define constant $gtk-border-style-hidden = 4;
define constant $gtk-border-style-dotted = 5;
define constant $gtk-border-style-dashed = 6;
define constant $gtk-border-style-double = 7;
define constant $gtk-border-style-groove = 8;
define constant $gtk-border-style-ridge = 9;
define constant <GtkBorderStyle> = <C-int>;
define C-pointer-type <GtkBorderStyle*> => <GtkBorderStyle>;

define open C-subtype <GtkBox> (<GtkContainer>)
  constant slot gtk-box-container :: <GtkContainer>;
  constant slot gtk-box-priv :: <GtkBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkBox*> => <GtkBox>;

define C-function gtk-box-new
  input parameter orientation_ :: <GtkOrientation>;
  input parameter spacing_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_box_new";
end;

define C-function gtk-box-get-homogeneous
  input parameter self :: <GtkBox>;
  result res :: <C-boolean>;
  c-name: "gtk_box_get_homogeneous";
end;

define C-function gtk-box-get-spacing
  input parameter self :: <GtkBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_box_get_spacing";
end;

define C-function gtk-box-pack-end
  input parameter self :: <GtkBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter expand_ :: <C-boolean>;
  input parameter fill_ :: <C-boolean>;
  input parameter padding_ :: <C-unsigned-int>;
  c-name: "gtk_box_pack_end";
end;

define C-function gtk-box-pack-start
  input parameter self :: <GtkBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter expand_ :: <C-boolean>;
  input parameter fill_ :: <C-boolean>;
  input parameter padding_ :: <C-unsigned-int>;
  c-name: "gtk_box_pack_start";
end;

define C-function gtk-box-query-child-packing
  input parameter self :: <GtkBox>;
  input parameter child_ :: <GtkWidget>;
  output parameter expand_ :: <C-int*>;
  output parameter fill_ :: <C-int*>;
  output parameter padding_ :: <C-unsigned-int*>;
  output parameter pack_type_ :: <GtkPackType*>;
  c-name: "gtk_box_query_child_packing";
end;

define C-function gtk-box-reorder-child
  input parameter self :: <GtkBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_box_reorder_child";
end;

define C-function gtk-box-set-child-packing
  input parameter self :: <GtkBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter expand_ :: <C-boolean>;
  input parameter fill_ :: <C-boolean>;
  input parameter padding_ :: <C-unsigned-int>;
  input parameter pack_type_ :: <GtkPackType>;
  c-name: "gtk_box_set_child_packing";
end;

define C-function gtk-box-set-homogeneous
  input parameter self :: <GtkBox>;
  input parameter homogeneous_ :: <C-boolean>;
  c-name: "gtk_box_set_homogeneous";
end;

define C-function gtk-box-set-spacing
  input parameter self :: <GtkBox>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "gtk_box_set_spacing";
end;

define C-struct <_GtkBoxClass>
  constant slot gtk-box-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-box-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-box-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkBoxClass>;
end C-struct;

define C-struct <_GtkBoxPrivate>
  pointer-type-name: <GtkBoxPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkBuildable> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkBuildable*> => <GtkBuildable>;

define C-function gtk-buildable-add-child
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter child_ :: <GObject>;
  input parameter type_ :: <C-string>;
  c-name: "gtk_buildable_add_child";
end;

define C-function gtk-buildable-construct-child
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter name_ :: <C-string>;
  result res :: <GObject>;
  c-name: "gtk_buildable_construct_child";
end;

define C-function gtk-buildable-custom-finished
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter child_ :: <GObject>;
  input parameter tagname_ :: <C-string>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_buildable_custom_finished";
end;

define C-function gtk-buildable-custom-tag-end
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter child_ :: <GObject>;
  input parameter tagname_ :: <C-string>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_buildable_custom_tag_end";
end;

define C-function gtk-buildable-custom-tag-start
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter child_ :: <GObject>;
  input parameter tagname_ :: <C-string>;
  output parameter parser_ :: <GMarkupParser>;
  output parameter data_ :: <C-void**>;
  result res :: <C-boolean>;
  c-name: "gtk_buildable_custom_tag_start";
end;

define C-function gtk-buildable-get-internal-child
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter childname_ :: <C-string>;
  result res :: <GObject>;
  c-name: "gtk_buildable_get_internal_child";
end;

define C-function gtk-buildable-get-name
  input parameter self :: <GtkBuildable>;
  result res :: <C-string>;
  c-name: "gtk_buildable_get_name";
end;

define C-function gtk-buildable-parser-finished
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  c-name: "gtk_buildable_parser_finished";
end;

define C-function gtk-buildable-set-buildable-property
  input parameter self :: <GtkBuildable>;
  input parameter builder_ :: <GtkBuilder>;
  input parameter name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_buildable_set_buildable_property";
end;

define C-function gtk-buildable-set-name
  input parameter self :: <GtkBuildable>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_buildable_set_name";
end;

define C-struct <_GtkBuildableIface>
  constant slot gtk-buildable-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-buildable-iface-set-name :: <C-function-pointer>;
  constant slot gtk-buildable-iface-get-name :: <C-function-pointer>;
  constant slot gtk-buildable-iface-add-child :: <C-function-pointer>;
  constant slot gtk-buildable-iface-set-buildable-property :: <C-function-pointer>;
  constant slot gtk-buildable-iface-construct-child :: <C-function-pointer>;
  constant slot gtk-buildable-iface-custom-tag-start :: <C-function-pointer>;
  constant slot gtk-buildable-iface-custom-tag-end :: <C-function-pointer>;
  constant slot gtk-buildable-iface-custom-finished :: <C-function-pointer>;
  constant slot gtk-buildable-iface-parser-finished :: <C-function-pointer>;
  constant slot gtk-buildable-iface-get-internal-child :: <C-function-pointer>;
  pointer-type-name: <GtkBuildableIface>;
end C-struct;

define open C-subtype <GtkBuilder> (<GObject>)
  constant slot gtk-builder-parent-instance :: <GObject>;
  constant slot gtk-builder-priv :: <GtkBuilderPrivate>;
end C-subtype;

define C-pointer-type <GtkBuilder*> => <GtkBuilder>;

define C-function gtk-builder-new
  result res :: <GtkBuilder>;
  c-name: "gtk_builder_new";
end;

define C-function gtk-builder-add-from-file
  input parameter self :: <GtkBuilder>;
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_from_file";
end;

define C-function gtk-builder-add-from-resource
  input parameter self :: <GtkBuilder>;
  input parameter resource_path_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_from_resource";
end;

define C-function gtk-builder-add-from-string
  input parameter self :: <GtkBuilder>;
  input parameter buffer_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_from_string";
end;

define C-function gtk-builder-add-objects-from-file
  input parameter self :: <GtkBuilder>;
  input parameter filename_ :: <C-string>;
  input parameter object_ids_ :: <C-string*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_objects_from_file";
end;

define C-function gtk-builder-add-objects-from-resource
  input parameter self :: <GtkBuilder>;
  input parameter resource_path_ :: <C-string>;
  input parameter object_ids_ :: <C-string*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_objects_from_resource";
end;

define C-function gtk-builder-add-objects-from-string
  input parameter self :: <GtkBuilder>;
  input parameter buffer_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter object_ids_ :: <C-string*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_add_objects_from_string";
end;

define C-function gtk-builder-connect-signals
  input parameter self :: <GtkBuilder>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_builder_connect_signals";
end;

define C-function gtk-builder-connect-signals-full
  input parameter self :: <GtkBuilder>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_builder_connect_signals_full";
end;

define C-function gtk-builder-get-object
  input parameter self :: <GtkBuilder>;
  input parameter name_ :: <C-string>;
  result res :: <GObject>;
  c-name: "gtk_builder_get_object";
end;

define C-function gtk-builder-get-objects
  input parameter self :: <GtkBuilder>;
  result res :: <GSList>;
  c-name: "gtk_builder_get_objects";
end;

define C-function gtk-builder-get-translation-domain
  input parameter self :: <GtkBuilder>;
  result res :: <C-string>;
  c-name: "gtk_builder_get_translation_domain";
end;

define C-function gtk-builder-get-type-from-name
  input parameter self :: <GtkBuilder>;
  input parameter type_name_ :: <C-string>;
  result res :: <C-long>;
  c-name: "gtk_builder_get_type_from_name";
end;

define C-function gtk-builder-set-translation-domain
  input parameter self :: <GtkBuilder>;
  input parameter domain_ :: <C-string>;
  c-name: "gtk_builder_set_translation_domain";
end;

define C-function gtk-builder-value-from-string
  input parameter self :: <GtkBuilder>;
  input parameter pspec_ :: <GParamSpec>;
  input parameter string_ :: <C-string>;
  output parameter value_ :: <GValue>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_builder_value_from_string";
end;

define C-function gtk-builder-value-from-string-type
  input parameter self :: <GtkBuilder>;
  input parameter type_ :: <C-long>;
  input parameter string_ :: <C-string>;
  output parameter value_ :: <GValue>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_builder_value_from_string_type";
end;

define C-struct <_GtkBuilderClass>
  constant slot gtk-builder-class-parent-class :: <GObjectClass>;
  constant slot gtk-builder-class-get-type-from-name :: <C-function-pointer>;
  constant slot gtk-builder-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-builder-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkBuilderClass>;
end C-struct;

define constant $gtk-builder-error-invalid-type-function = 0;
define constant $gtk-builder-error-unhandled-tag = 1;
define constant $gtk-builder-error-missing-attribute = 2;
define constant $gtk-builder-error-invalid-attribute = 3;
define constant $gtk-builder-error-invalid-tag = 4;
define constant $gtk-builder-error-missing-property-value = 5;
define constant $gtk-builder-error-invalid-value = 6;
define constant $gtk-builder-error-version-mismatch = 7;
define constant $gtk-builder-error-duplicate-id = 8;
define constant <GtkBuilderError> = <C-int>;
define C-pointer-type <GtkBuilderError*> => <GtkBuilderError>;

define C-struct <_GtkBuilderPrivate>
  pointer-type-name: <GtkBuilderPrivate>;
end C-struct;

define open C-subtype <GtkButton> (<GtkBin>)
  constant slot gtk-button-bin :: <GtkBin>;
  constant slot gtk-button-priv :: <GtkButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkButton*> => <GtkButton>;

define C-function gtk-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_button_new";
end;

define C-function gtk-button-new-from-stock
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_button_new_from_stock";
end;

define C-function gtk-button-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_button_new_with_label";
end;

define C-function gtk-button-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_button_new_with_mnemonic";
end;

define C-function gtk-button-clicked
  input parameter self :: <GtkButton>;
  c-name: "gtk_button_clicked";
end;

define C-function gtk-button-enter
  input parameter self :: <GtkButton>;
  c-name: "gtk_button_enter";
end;

define C-function gtk-button-get-alignment
  input parameter self :: <GtkButton>;
  output parameter xalign_ :: <C-float*>;
  output parameter yalign_ :: <C-float*>;
  c-name: "gtk_button_get_alignment";
end;

define C-function gtk-button-get-event-window
  input parameter self :: <GtkButton>;
  result res :: <GdkWindow>;
  c-name: "gtk_button_get_event_window";
end;

define C-function gtk-button-get-focus-on-click
  input parameter self :: <GtkButton>;
  result res :: <C-boolean>;
  c-name: "gtk_button_get_focus_on_click";
end;

define C-function gtk-button-get-image
  input parameter self :: <GtkButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_button_get_image";
end;

define C-function gtk-button-get-image-position
  input parameter self :: <GtkButton>;
  result res :: <GtkPositionType>;
  c-name: "gtk_button_get_image_position";
end;

define C-function gtk-button-get-label
  input parameter self :: <GtkButton>;
  result res :: <C-string>;
  c-name: "gtk_button_get_label";
end;

define C-function gtk-button-get-relief
  input parameter self :: <GtkButton>;
  result res :: <GtkReliefStyle>;
  c-name: "gtk_button_get_relief";
end;

define C-function gtk-button-get-use-stock
  input parameter self :: <GtkButton>;
  result res :: <C-boolean>;
  c-name: "gtk_button_get_use_stock";
end;

define C-function gtk-button-get-use-underline
  input parameter self :: <GtkButton>;
  result res :: <C-boolean>;
  c-name: "gtk_button_get_use_underline";
end;

define C-function gtk-button-leave
  input parameter self :: <GtkButton>;
  c-name: "gtk_button_leave";
end;

define C-function gtk-button-pressed
  input parameter self :: <GtkButton>;
  c-name: "gtk_button_pressed";
end;

define C-function gtk-button-released
  input parameter self :: <GtkButton>;
  c-name: "gtk_button_released";
end;

define C-function gtk-button-set-alignment
  input parameter self :: <GtkButton>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  c-name: "gtk_button_set_alignment";
end;

define C-function gtk-button-set-focus-on-click
  input parameter self :: <GtkButton>;
  input parameter focus_on_click_ :: <C-boolean>;
  c-name: "gtk_button_set_focus_on_click";
end;

define C-function gtk-button-set-image
  input parameter self :: <GtkButton>;
  input parameter image_ :: <GtkWidget>;
  c-name: "gtk_button_set_image";
end;

define C-function gtk-button-set-image-position
  input parameter self :: <GtkButton>;
  input parameter position_ :: <GtkPositionType>;
  c-name: "gtk_button_set_image_position";
end;

define C-function gtk-button-set-label
  input parameter self :: <GtkButton>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_button_set_label";
end;

define C-function gtk-button-set-relief
  input parameter self :: <GtkButton>;
  input parameter newstyle_ :: <GtkReliefStyle>;
  c-name: "gtk_button_set_relief";
end;

define C-function gtk-button-set-use-stock
  input parameter self :: <GtkButton>;
  input parameter use_stock_ :: <C-boolean>;
  c-name: "gtk_button_set_use_stock";
end;

define C-function gtk-button-set-use-underline
  input parameter self :: <GtkButton>;
  input parameter use_underline_ :: <C-boolean>;
  c-name: "gtk_button_set_use_underline";
end;

define open C-subtype <GtkButtonBox> (<GtkBox>)
  constant slot gtk-button-box-box :: <GtkBox>;
  constant slot gtk-button-box-priv :: <GtkButtonBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkButtonBox*> => <GtkButtonBox>;

define C-function gtk-button-box-new
  input parameter orientation_ :: <GtkOrientation>;
  result res :: <GtkWidget>;
  c-name: "gtk_button_box_new";
end;

define C-function gtk-button-box-get-child-non-homogeneous
  input parameter self :: <GtkButtonBox>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_button_box_get_child_non_homogeneous";
end;

define C-function gtk-button-box-get-child-secondary
  input parameter self :: <GtkButtonBox>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_button_box_get_child_secondary";
end;

define C-function gtk-button-box-get-layout
  input parameter self :: <GtkButtonBox>;
  result res :: <GtkButtonBoxStyle>;
  c-name: "gtk_button_box_get_layout";
end;

define C-function gtk-button-box-set-child-non-homogeneous
  input parameter self :: <GtkButtonBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter non_homogeneous_ :: <C-boolean>;
  c-name: "gtk_button_box_set_child_non_homogeneous";
end;

define C-function gtk-button-box-set-child-secondary
  input parameter self :: <GtkButtonBox>;
  input parameter child_ :: <GtkWidget>;
  input parameter is_secondary_ :: <C-boolean>;
  c-name: "gtk_button_box_set_child_secondary";
end;

define C-function gtk-button-box-set-layout
  input parameter self :: <GtkButtonBox>;
  input parameter layout_style_ :: <GtkButtonBoxStyle>;
  c-name: "gtk_button_box_set_layout";
end;

define C-struct <_GtkButtonBoxClass>
  constant slot gtk-button-box-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-button-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-button-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-button-box-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-button-box-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkButtonBoxClass>;
end C-struct;

define C-struct <_GtkButtonBoxPrivate>
  pointer-type-name: <GtkButtonBoxPrivate>;
end C-struct;

define constant $gtk-buttonbox-spread = 1;
define constant $gtk-buttonbox-edge = 2;
define constant $gtk-buttonbox-start = 3;
define constant $gtk-buttonbox-end = 4;
define constant $gtk-buttonbox-center = 5;
define constant <GtkButtonBoxStyle> = <C-int>;
define C-pointer-type <GtkButtonBoxStyle*> => <GtkButtonBoxStyle>;

define C-struct <_GtkButtonClass>
  constant slot gtk-button-class-parent-class :: <GtkBinClass>;
  constant slot gtk-button-class-pressed :: <C-function-pointer>;
  constant slot gtk-button-class-released :: <C-function-pointer>;
  constant slot gtk-button-class-clicked :: <C-function-pointer>;
  constant slot gtk-button-class-enter :: <C-function-pointer>;
  constant slot gtk-button-class-leave :: <C-function-pointer>;
  constant slot gtk-button-class-activate :: <C-function-pointer>;
  constant slot gtk-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkButtonClass>;
end C-struct;

define C-struct <_GtkButtonPrivate>
  pointer-type-name: <GtkButtonPrivate>;
end C-struct;

define constant $gtk-buttons-none = 0;
define constant $gtk-buttons-ok = 1;
define constant $gtk-buttons-close = 2;
define constant $gtk-buttons-cancel = 3;
define constant $gtk-buttons-yes-no = 4;
define constant $gtk-buttons-ok-cancel = 5;
define constant <GtkButtonsType> = <C-int>;
define C-pointer-type <GtkButtonsType*> => <GtkButtonsType>;

define open C-subtype <GtkCalendar> (<GtkWidget>)
  constant slot gtk-calendar-widget :: <GtkWidget>;
  constant slot gtk-calendar-priv :: <GtkCalendarPrivate>;
end C-subtype;

define C-pointer-type <GtkCalendar*> => <GtkCalendar>;

define C-function gtk-calendar-new
  result res :: <GtkWidget>;
  c-name: "gtk_calendar_new";
end;

define C-function gtk-calendar-clear-marks
  input parameter self :: <GtkCalendar>;
  c-name: "gtk_calendar_clear_marks";
end;

define C-function gtk-calendar-get-date
  input parameter self :: <GtkCalendar>;
  output parameter year_ :: <C-unsigned-int*>;
  output parameter month_ :: <C-unsigned-int*>;
  output parameter day_ :: <C-unsigned-int*>;
  c-name: "gtk_calendar_get_date";
end;

define C-function gtk-calendar-get-day-is-marked
  input parameter self :: <GtkCalendar>;
  input parameter day_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_calendar_get_day_is_marked";
end;

define C-function gtk-calendar-get-detail-height-rows
  input parameter self :: <GtkCalendar>;
  result res :: <C-signed-int>;
  c-name: "gtk_calendar_get_detail_height_rows";
end;

define C-function gtk-calendar-get-detail-width-chars
  input parameter self :: <GtkCalendar>;
  result res :: <C-signed-int>;
  c-name: "gtk_calendar_get_detail_width_chars";
end;

define C-function gtk-calendar-get-display-options
  input parameter self :: <GtkCalendar>;
  result res :: <GtkCalendarDisplayOptions>;
  c-name: "gtk_calendar_get_display_options";
end;

define C-function gtk-calendar-mark-day
  input parameter self :: <GtkCalendar>;
  input parameter day_ :: <C-unsigned-int>;
  c-name: "gtk_calendar_mark_day";
end;

define C-function gtk-calendar-select-day
  input parameter self :: <GtkCalendar>;
  input parameter day_ :: <C-unsigned-int>;
  c-name: "gtk_calendar_select_day";
end;

define C-function gtk-calendar-select-month
  input parameter self :: <GtkCalendar>;
  input parameter month_ :: <C-unsigned-int>;
  input parameter year_ :: <C-unsigned-int>;
  c-name: "gtk_calendar_select_month";
end;

define C-function gtk-calendar-set-detail-func
  input parameter self :: <GtkCalendar>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_calendar_set_detail_func";
end;

define C-function gtk-calendar-set-detail-height-rows
  input parameter self :: <GtkCalendar>;
  input parameter rows_ :: <C-signed-int>;
  c-name: "gtk_calendar_set_detail_height_rows";
end;

define C-function gtk-calendar-set-detail-width-chars
  input parameter self :: <GtkCalendar>;
  input parameter chars_ :: <C-signed-int>;
  c-name: "gtk_calendar_set_detail_width_chars";
end;

define C-function gtk-calendar-set-display-options
  input parameter self :: <GtkCalendar>;
  input parameter flags_ :: <GtkCalendarDisplayOptions>;
  c-name: "gtk_calendar_set_display_options";
end;

define C-function gtk-calendar-unmark-day
  input parameter self :: <GtkCalendar>;
  input parameter day_ :: <C-unsigned-int>;
  c-name: "gtk_calendar_unmark_day";
end;

define C-struct <_GtkCalendarClass>
  constant slot gtk-calendar-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-calendar-class-month-changed :: <C-function-pointer>;
  constant slot gtk-calendar-class-day-selected :: <C-function-pointer>;
  constant slot gtk-calendar-class-day-selected-double-click :: <C-function-pointer>;
  constant slot gtk-calendar-class-prev-month :: <C-function-pointer>;
  constant slot gtk-calendar-class-next-month :: <C-function-pointer>;
  constant slot gtk-calendar-class-prev-year :: <C-function-pointer>;
  constant slot gtk-calendar-class-next-year :: <C-function-pointer>;
  constant slot gtk-calendar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-calendar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-calendar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-calendar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCalendarClass>;
end C-struct;

define constant $gtk-calendar-show-heading = 1;
define constant $gtk-calendar-show-day-names = 2;
define constant $gtk-calendar-no-month-change = 4;
define constant $gtk-calendar-show-week-numbers = 8;
define constant $gtk-calendar-show-details = 32;
define constant <GtkCalendarDisplayOptions> = <C-int>;
define C-pointer-type <GtkCalendarDisplayOptions*> => <GtkCalendarDisplayOptions>;

define C-struct <_GtkCalendarPrivate>
  pointer-type-name: <GtkCalendarPrivate>;
end C-struct;

define open C-subtype <GtkCellArea> (<GInitiallyUnowned>)
  constant slot gtk-cell-area-parent-instance :: <GInitiallyUnowned>;
  constant slot gtk-cell-area-priv :: <GtkCellAreaPrivate>;
end C-subtype;

define C-pointer-type <GtkCellArea*> => <GtkCellArea>;

define C-function gtk-cell-area-activate
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  input parameter edit_only_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_activate";
end;

define C-function gtk-cell-area-activate-cell
  input parameter self :: <GtkCellArea>;
  input parameter widget_ :: <GtkWidget>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter event_ :: <GdkEvent>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_activate_cell";
end;

define C-function gtk-cell-area-add
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_add";
end;

define C-function gtk-cell-area-add-focus-sibling
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter sibling_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_add_focus_sibling";
end;

define C-function gtk-cell-area-apply-attributes
  input parameter self :: <GtkCellArea>;
  input parameter tree_model_ :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter is_expander_ :: <C-boolean>;
  input parameter is_expanded_ :: <C-boolean>;
  c-name: "gtk_cell_area_apply_attributes";
end;

define C-function gtk-cell-area-attribute-connect
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter attribute_ :: <C-string>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_cell_area_attribute_connect";
end;

define C-function gtk-cell-area-attribute-disconnect
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter attribute_ :: <C-string>;
  c-name: "gtk_cell_area_attribute_disconnect";
end;

define C-function gtk-cell-area-cell-get-property
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_cell_area_cell_get_property";
end;

define C-function gtk-cell-area-cell-set-property
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_cell_area_cell_set_property";
end;

define C-function gtk-cell-area-copy-context
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  result res :: <GtkCellAreaContext>;
  c-name: "gtk_cell_area_copy_context";
end;

define C-function gtk-cell-area-create-context
  input parameter self :: <GtkCellArea>;
  result res :: <GtkCellAreaContext>;
  c-name: "gtk_cell_area_create_context";
end;

define C-function gtk-cell-area-event
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter event_ :: <GdkEvent>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  result res :: <C-signed-int>;
  c-name: "gtk_cell_area_event";
end;

define C-function gtk-cell-area-focus
  input parameter self :: <GtkCellArea>;
  input parameter direction_ :: <GtkDirectionType>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_focus";
end;

define C-function gtk-cell-area-foreach
  input parameter self :: <GtkCellArea>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  c-name: "gtk_cell_area_foreach";
end;

define C-function gtk-cell-area-foreach-alloc
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter background_area_ :: <cairoRectangleInt>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  c-name: "gtk_cell_area_foreach_alloc";
end;

define C-function gtk-cell-area-get-cell-allocation
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  output parameter allocation_ :: <cairoRectangleInt>;
  c-name: "gtk_cell_area_get_cell_allocation";
end;

define C-function gtk-cell-area-get-cell-at-position
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter alloc_area_ :: <cairoRectangleInt>;
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_get_cell_at_position";
end;

define C-function gtk-cell-area-get-current-path-string
  input parameter self :: <GtkCellArea>;
  result res :: <C-string>;
  c-name: "gtk_cell_area_get_current_path_string";
end;

define C-function gtk-cell-area-get-edit-widget
  input parameter self :: <GtkCellArea>;
  result res :: <GtkCellEditable>;
  c-name: "gtk_cell_area_get_edit_widget";
end;

define C-function gtk-cell-area-get-edited-cell
  input parameter self :: <GtkCellArea>;
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_get_edited_cell";
end;

define C-function gtk-cell-area-get-focus-cell
  input parameter self :: <GtkCellArea>;
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_get_focus_cell";
end;

define C-function gtk-cell-area-get-focus-from-sibling
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_get_focus_from_sibling";
end;

define C-function gtk-cell-area-get-focus-siblings
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  result res :: <GList>;
  c-name: "gtk_cell_area_get_focus_siblings";
end;

define C-function gtk-cell-area-get-preferred-height
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_get_preferred_height";
end;

define C-function gtk-cell-area-get-preferred-height-for-width
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter width_ :: <C-signed-int>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_get_preferred_height_for_width";
end;

define C-function gtk-cell-area-get-preferred-width
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_get_preferred_width";
end;

define C-function gtk-cell-area-get-preferred-width-for-height
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter height_ :: <C-signed-int>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_get_preferred_width_for_height";
end;

define C-function gtk-cell-area-get-request-mode
  input parameter self :: <GtkCellArea>;
  result res :: <GtkSizeRequestMode>;
  c-name: "gtk_cell_area_get_request_mode";
end;

define C-function gtk-cell-area-has-renderer
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_has_renderer";
end;

define C-function gtk-cell-area-inner-cell-area
  input parameter self :: <GtkCellArea>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  output parameter inner_area_ :: <cairoRectangleInt>;
  c-name: "gtk_cell_area_inner_cell_area";
end;

define C-function gtk-cell-area-is-activatable
  input parameter self :: <GtkCellArea>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_is_activatable";
end;

define C-function gtk-cell-area-is-focus-sibling
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter sibling_ :: <GtkCellRenderer>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_area_is_focus_sibling";
end;

define C-function gtk-cell-area-remove
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_remove";
end;

define C-function gtk-cell-area-remove-focus-sibling
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter sibling_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_remove_focus_sibling";
end;

define C-function gtk-cell-area-render
  input parameter self :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cr_ :: <cairoContext>;
  input parameter background_area_ :: <cairoRectangleInt>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  input parameter paint_focus_ :: <C-boolean>;
  c-name: "gtk_cell_area_render";
end;

define C-function gtk-cell-area-request-renderer
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter orientation_ :: <GtkOrientation>;
  input parameter widget_ :: <GtkWidget>;
  input parameter for_size_ :: <C-signed-int>;
  output parameter minimum_size_ :: <C-signed-int*>;
  output parameter natural_size_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_request_renderer";
end;

define C-function gtk-cell-area-set-focus-cell
  input parameter self :: <GtkCellArea>;
  input parameter renderer_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_area_set_focus_cell";
end;

define C-function gtk-cell-area-stop-editing
  input parameter self :: <GtkCellArea>;
  input parameter canceled_ :: <C-boolean>;
  c-name: "gtk_cell_area_stop_editing";
end;

define open C-subtype <GtkCellAreaBox> (<GtkCellArea>)
  constant slot gtk-cell-area-box-parent-instance :: <GtkCellArea>;
  constant slot gtk-cell-area-box-priv :: <GtkCellAreaBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkCellAreaBox*> => <GtkCellAreaBox>;

define C-function gtk-cell-area-box-new
  result res :: <GtkCellArea>;
  c-name: "gtk_cell_area_box_new";
end;

define C-function gtk-cell-area-box-get-spacing
  input parameter self :: <GtkCellAreaBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_cell_area_box_get_spacing";
end;

define C-function gtk-cell-area-box-pack-end
  input parameter self :: <GtkCellAreaBox>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  input parameter align_ :: <C-boolean>;
  input parameter fixed_ :: <C-boolean>;
  c-name: "gtk_cell_area_box_pack_end";
end;

define C-function gtk-cell-area-box-pack-start
  input parameter self :: <GtkCellAreaBox>;
  input parameter renderer_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  input parameter align_ :: <C-boolean>;
  input parameter fixed_ :: <C-boolean>;
  c-name: "gtk_cell_area_box_pack_start";
end;

define C-function gtk-cell-area-box-set-spacing
  input parameter self :: <GtkCellAreaBox>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "gtk_cell_area_box_set_spacing";
end;

define C-struct <_GtkCellAreaBoxClass>
  constant slot gtk-cell-area-box-class-parent-class :: <GtkCellAreaClass>;
  constant slot gtk-cell-area-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-area-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-area-box-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-area-box-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellAreaBoxClass>;
end C-struct;

define C-struct <_GtkCellAreaBoxPrivate>
  pointer-type-name: <GtkCellAreaBoxPrivate>;
end C-struct;

define C-struct <_GtkCellAreaClass>
  constant slot gtk-cell-area-class-parent-class :: <GInitiallyUnownedClass>;
  constant slot gtk-cell-area-class-add :: <C-function-pointer>;
  constant slot gtk-cell-area-class-remove :: <C-function-pointer>;
  constant slot gtk-cell-area-class-foreach :: <C-function-pointer>;
  constant slot gtk-cell-area-class-foreach-alloc :: <C-function-pointer>;
  constant slot gtk-cell-area-class-event :: <C-function-pointer>;
  constant slot gtk-cell-area-class-render :: <C-function-pointer>;
  constant slot gtk-cell-area-class-apply-attributes :: <C-function-pointer>;
  constant slot gtk-cell-area-class-create-context :: <C-function-pointer>;
  constant slot gtk-cell-area-class-copy-context :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-request-mode :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-preferred-width :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-preferred-height-for-width :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-preferred-height :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-preferred-width-for-height :: <C-function-pointer>;
  constant slot gtk-cell-area-class-set-cell-property :: <C-function-pointer>;
  constant slot gtk-cell-area-class-get-cell-property :: <C-function-pointer>;
  constant slot gtk-cell-area-class-focus :: <C-function-pointer>;
  constant slot gtk-cell-area-class-is-activatable :: <C-function-pointer>;
  constant slot gtk-cell-area-class-activate :: <C-function-pointer>;
  constant slot gtk-cell-area-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-cell-area-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkCellAreaClass>;
end C-struct;

define C-function gtk-cell-area-class-find-cell-property
  input parameter self :: <GtkCellAreaClass>;
  input parameter property_name_ :: <C-string>;
  result res :: <GParamSpec>;
  c-name: "gtk_cell_area_class_find_cell_property";
end;

define C-function gtk-cell-area-class-install-cell-property
  input parameter self :: <GtkCellAreaClass>;
  input parameter property_id_ :: <C-unsigned-int>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "gtk_cell_area_class_install_cell_property";
end;

define C-function gtk-cell-area-class-list-cell-properties
  input parameter self :: <GtkCellAreaClass>;
  output parameter n_properties_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_cell_area_class_list_cell_properties";
end;

define open C-subtype <GtkCellAreaContext> (<GObject>)
  constant slot gtk-cell-area-context-parent-instance :: <GObject>;
  constant slot gtk-cell-area-context-priv :: <GtkCellAreaContextPrivate>;
end C-subtype;

define C-pointer-type <GtkCellAreaContext*> => <GtkCellAreaContext>;

define C-function gtk-cell-area-context-allocate
  input parameter self :: <GtkCellAreaContext>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_cell_area_context_allocate";
end;

define C-function gtk-cell-area-context-get-allocation
  input parameter self :: <GtkCellAreaContext>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_context_get_allocation";
end;

define C-function gtk-cell-area-context-get-area
  input parameter self :: <GtkCellAreaContext>;
  result res :: <GtkCellArea>;
  c-name: "gtk_cell_area_context_get_area";
end;

define C-function gtk-cell-area-context-get-preferred-height
  input parameter self :: <GtkCellAreaContext>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_context_get_preferred_height";
end;

define C-function gtk-cell-area-context-get-preferred-height-for-width
  input parameter self :: <GtkCellAreaContext>;
  input parameter width_ :: <C-signed-int>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_context_get_preferred_height_for_width";
end;

define C-function gtk-cell-area-context-get-preferred-width
  input parameter self :: <GtkCellAreaContext>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_context_get_preferred_width";
end;

define C-function gtk-cell-area-context-get-preferred-width-for-height
  input parameter self :: <GtkCellAreaContext>;
  input parameter height_ :: <C-signed-int>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_cell_area_context_get_preferred_width_for_height";
end;

define C-function gtk-cell-area-context-push-preferred-height
  input parameter self :: <GtkCellAreaContext>;
  input parameter minimum_height_ :: <C-signed-int>;
  input parameter natural_height_ :: <C-signed-int>;
  c-name: "gtk_cell_area_context_push_preferred_height";
end;

define C-function gtk-cell-area-context-push-preferred-width
  input parameter self :: <GtkCellAreaContext>;
  input parameter minimum_width_ :: <C-signed-int>;
  input parameter natural_width_ :: <C-signed-int>;
  c-name: "gtk_cell_area_context_push_preferred_width";
end;

define C-function gtk-cell-area-context-reset
  input parameter self :: <GtkCellAreaContext>;
  c-name: "gtk_cell_area_context_reset";
end;

define C-struct <_GtkCellAreaContextClass>
  constant slot gtk-cell-area-context-class-parent-class :: <GObjectClass>;
  constant slot gtk-cell-area-context-class-allocate :: <C-function-pointer>;
  constant slot gtk-cell-area-context-class-reset :: <C-function-pointer>;
  constant slot gtk-cell-area-context-class-get-preferred-height-for-width :: <C-function-pointer>;
  constant slot gtk-cell-area-context-class-get-preferred-width-for-height :: <C-function-pointer>;
  constant slot gtk-cell-area-context-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-area-context-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-area-context-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-area-context-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-cell-area-context-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-cell-area-context-class-_gtk-reserved6 :: <C-void*>;
  pointer-type-name: <GtkCellAreaContextClass>;
end C-struct;

define C-struct <_GtkCellAreaContextPrivate>
  pointer-type-name: <GtkCellAreaContextPrivate>;
end C-struct;

define C-struct <_GtkCellAreaPrivate>
  pointer-type-name: <GtkCellAreaPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkCellEditable> (<GtkWidget>)
end C-subtype;

define C-pointer-type <GtkCellEditable*> => <GtkCellEditable>;

define C-function gtk-cell-editable-editing-done
  input parameter self :: <GtkCellEditable>;
  c-name: "gtk_cell_editable_editing_done";
end;

define C-function gtk-cell-editable-remove-widget
  input parameter self :: <GtkCellEditable>;
  c-name: "gtk_cell_editable_remove_widget";
end;

define C-function gtk-cell-editable-start-editing
  input parameter self :: <GtkCellEditable>;
  input parameter event_ :: <GdkEvent>;
  c-name: "gtk_cell_editable_start_editing";
end;

define C-struct <_GtkCellEditableIface>
  constant slot gtk-cell-editable-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-cell-editable-iface-editing-done :: <C-function-pointer>;
  constant slot gtk-cell-editable-iface-remove-widget :: <C-function-pointer>;
  constant slot gtk-cell-editable-iface-start-editing :: <C-function-pointer>;
  pointer-type-name: <GtkCellEditableIface>;
end C-struct;

// Interface
define open C-subtype <GtkCellLayout> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkCellLayout*> => <GtkCellLayout>;

define C-function gtk-cell-layout-add-attribute
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter attribute_ :: <C-string>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_cell_layout_add_attribute";
end;

define C-function gtk-cell-layout-clear
  input parameter self :: <GtkCellLayout>;
  c-name: "gtk_cell_layout_clear";
end;

define C-function gtk-cell-layout-clear-attributes
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  c-name: "gtk_cell_layout_clear_attributes";
end;

define C-function gtk-cell-layout-get-area
  input parameter self :: <GtkCellLayout>;
  result res :: <GtkCellArea>;
  c-name: "gtk_cell_layout_get_area";
end;

define C-function gtk-cell-layout-get-cells
  input parameter self :: <GtkCellLayout>;
  result res :: <GList>;
  c-name: "gtk_cell_layout_get_cells";
end;

define C-function gtk-cell-layout-pack-end
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_cell_layout_pack_end";
end;

define C-function gtk-cell-layout-pack-start
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_cell_layout_pack_start";
end;

define C-function gtk-cell-layout-reorder
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_cell_layout_reorder";
end;

define C-function gtk-cell-layout-set-cell-data-func
  input parameter self :: <GtkCellLayout>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter func_ :: <C-function-pointer>;
  input parameter func_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_cell_layout_set_cell_data_func";
end;

define C-struct <_GtkCellLayoutIface>
  constant slot gtk-cell-layout-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-cell-layout-iface-pack-start :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-pack-end :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-clear :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-add-attribute :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-set-cell-data-func :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-clear-attributes :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-reorder :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-get-cells :: <C-function-pointer>;
  constant slot gtk-cell-layout-iface-get-area :: <C-function-pointer>;
  pointer-type-name: <GtkCellLayoutIface>;
end C-struct;

define open C-subtype <GtkCellRenderer> (<GInitiallyUnowned>)
  constant slot gtk-cell-renderer-parent-instance :: <GInitiallyUnowned>;
  constant slot gtk-cell-renderer-priv :: <GtkCellRendererPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRenderer*> => <GtkCellRenderer>;

define C-function gtk-cell-renderer-activate
  input parameter self :: <GtkCellRenderer>;
  input parameter event_ :: <GdkEvent>;
  input parameter widget_ :: <GtkWidget>;
  input parameter path_ :: <C-string>;
  input parameter background_area_ :: <cairoRectangleInt>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_activate";
end;

define C-function gtk-cell-renderer-get-aligned-area
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  input parameter flags_ :: <GtkCellRendererState>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  output parameter aligned_area_ :: <cairoRectangleInt>;
  c-name: "gtk_cell_renderer_get_aligned_area";
end;

define C-function gtk-cell-renderer-get-alignment
  input parameter self :: <GtkCellRenderer>;
  output parameter xalign_ :: <C-float*>;
  output parameter yalign_ :: <C-float*>;
  c-name: "gtk_cell_renderer_get_alignment";
end;

define C-function gtk-cell-renderer-get-fixed-size
  input parameter self :: <GtkCellRenderer>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_fixed_size";
end;

define C-function gtk-cell-renderer-get-padding
  input parameter self :: <GtkCellRenderer>;
  output parameter xpad_ :: <C-signed-int*>;
  output parameter ypad_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_padding";
end;

define C-function gtk-cell-renderer-get-preferred-height
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  output parameter minimum_size_ :: <C-signed-int*>;
  output parameter natural_size_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_preferred_height";
end;

define C-function gtk-cell-renderer-get-preferred-height-for-width
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  input parameter width_ :: <C-signed-int>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_preferred_height_for_width";
end;

define C-function gtk-cell-renderer-get-preferred-size
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  output parameter minimum_size_ :: <GtkRequisition>;
  output parameter natural_size_ :: <GtkRequisition>;
  c-name: "gtk_cell_renderer_get_preferred_size";
end;

define C-function gtk-cell-renderer-get-preferred-width
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  output parameter minimum_size_ :: <C-signed-int*>;
  output parameter natural_size_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_preferred_width";
end;

define C-function gtk-cell-renderer-get-preferred-width-for-height
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  input parameter height_ :: <C-signed-int>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_preferred_width_for_height";
end;

define C-function gtk-cell-renderer-get-request-mode
  input parameter self :: <GtkCellRenderer>;
  result res :: <GtkSizeRequestMode>;
  c-name: "gtk_cell_renderer_get_request_mode";
end;

define C-function gtk-cell-renderer-get-sensitive
  input parameter self :: <GtkCellRenderer>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_get_sensitive";
end;

define C-function gtk-cell-renderer-get-size
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  output parameter x_offset_ :: <C-signed-int*>;
  output parameter y_offset_ :: <C-signed-int*>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_cell_renderer_get_size";
end;

define C-function gtk-cell-renderer-get-state
  input parameter self :: <GtkCellRenderer>;
  input parameter widget_ :: <GtkWidget>;
  input parameter cell_state_ :: <GtkCellRendererState>;
  result res :: <GtkStateFlags>;
  c-name: "gtk_cell_renderer_get_state";
end;

define C-function gtk-cell-renderer-get-visible
  input parameter self :: <GtkCellRenderer>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_get_visible";
end;

define C-function gtk-cell-renderer-is-activatable
  input parameter self :: <GtkCellRenderer>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_is_activatable";
end;

define C-function gtk-cell-renderer-render
  input parameter self :: <GtkCellRenderer>;
  input parameter cr_ :: <cairoContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter background_area_ :: <cairoRectangleInt>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  c-name: "gtk_cell_renderer_render";
end;

define C-function gtk-cell-renderer-set-alignment
  input parameter self :: <GtkCellRenderer>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  c-name: "gtk_cell_renderer_set_alignment";
end;

define C-function gtk-cell-renderer-set-fixed-size
  input parameter self :: <GtkCellRenderer>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_cell_renderer_set_fixed_size";
end;

define C-function gtk-cell-renderer-set-padding
  input parameter self :: <GtkCellRenderer>;
  input parameter xpad_ :: <C-signed-int>;
  input parameter ypad_ :: <C-signed-int>;
  c-name: "gtk_cell_renderer_set_padding";
end;

define C-function gtk-cell-renderer-set-sensitive
  input parameter self :: <GtkCellRenderer>;
  input parameter sensitive_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_set_sensitive";
end;

define C-function gtk-cell-renderer-set-visible
  input parameter self :: <GtkCellRenderer>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_set_visible";
end;

define C-function gtk-cell-renderer-start-editing
  input parameter self :: <GtkCellRenderer>;
  input parameter event_ :: <GdkEvent>;
  input parameter widget_ :: <GtkWidget>;
  input parameter path_ :: <C-string>;
  input parameter background_area_ :: <cairoRectangleInt>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  input parameter flags_ :: <GtkCellRendererState>;
  result res :: <GtkCellEditable>;
  c-name: "gtk_cell_renderer_start_editing";
end;

define C-function gtk-cell-renderer-stop-editing
  input parameter self :: <GtkCellRenderer>;
  input parameter canceled_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_stop_editing";
end;

define open C-subtype <GtkCellRendererAccel> (<GtkCellRendererText>)
  constant slot gtk-cell-renderer-accel-parent :: <GtkCellRendererText>;
  constant slot gtk-cell-renderer-accel-priv :: <GtkCellRendererAccelPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererAccel*> => <GtkCellRendererAccel>;

define C-function gtk-cell-renderer-accel-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_accel_new";
end;

define C-struct <_GtkCellRendererAccelClass>
  constant slot gtk-cell-renderer-accel-class-parent-class :: <GtkCellRendererTextClass>;
  constant slot gtk-cell-renderer-accel-class-accel-edited :: <C-function-pointer>;
  constant slot gtk-cell-renderer-accel-class-accel-cleared :: <C-function-pointer>;
  constant slot gtk-cell-renderer-accel-class-_gtk-reserved0 :: <C-void*>;
  constant slot gtk-cell-renderer-accel-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-accel-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-accel-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-accel-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererAccelClass>;
end C-struct;

define constant $gtk-cell-renderer-accel-mode-gtk = 0;
define constant $gtk-cell-renderer-accel-mode-other = 1;
define constant $gtk-cell-renderer-accel-mode-modifier-tap = 2;
define constant <GtkCellRendererAccelMode> = <C-int>;
define C-pointer-type <GtkCellRendererAccelMode*> => <GtkCellRendererAccelMode>;

define C-struct <_GtkCellRendererAccelPrivate>
  pointer-type-name: <GtkCellRendererAccelPrivate>;
end C-struct;

define C-struct <_GtkCellRendererClass>
  constant slot gtk-cell-renderer-class-parent-class :: <GInitiallyUnownedClass>;
  constant slot gtk-cell-renderer-class-get-request-mode :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-preferred-width :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-preferred-height-for-width :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-preferred-height :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-preferred-width-for-height :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-aligned-area :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-get-size :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-render :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-activate :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-start-editing :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-editing-canceled :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-editing-started :: <C-function-pointer>;
  constant slot gtk-cell-renderer-class-priv :: <GtkCellRendererClassPrivate>;
  constant slot gtk-cell-renderer-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererClass>;
end C-struct;

define C-struct <_GtkCellRendererClassPrivate>
  pointer-type-name: <GtkCellRendererClassPrivate>;
end C-struct;

define open C-subtype <GtkCellRendererCombo> (<GtkCellRendererText>)
  constant slot gtk-cell-renderer-combo-parent :: <GtkCellRendererText>;
  constant slot gtk-cell-renderer-combo-priv :: <GtkCellRendererComboPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererCombo*> => <GtkCellRendererCombo>;

define C-function gtk-cell-renderer-combo-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_combo_new";
end;

define C-struct <_GtkCellRendererComboClass>
  constant slot gtk-cell-renderer-combo-class-parent :: <GtkCellRendererTextClass>;
  constant slot gtk-cell-renderer-combo-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-combo-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-combo-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-combo-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererComboClass>;
end C-struct;

define C-struct <_GtkCellRendererComboPrivate>
  pointer-type-name: <GtkCellRendererComboPrivate>;
end C-struct;

define constant $gtk-cell-renderer-mode-inert = 0;
define constant $gtk-cell-renderer-mode-activatable = 1;
define constant $gtk-cell-renderer-mode-editable = 2;
define constant <GtkCellRendererMode> = <C-int>;
define C-pointer-type <GtkCellRendererMode*> => <GtkCellRendererMode>;

define open C-subtype <GtkCellRendererPixbuf> (<GtkCellRenderer>)
  constant slot gtk-cell-renderer-pixbuf-parent :: <GtkCellRenderer>;
  constant slot gtk-cell-renderer-pixbuf-priv :: <GtkCellRendererPixbufPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererPixbuf*> => <GtkCellRendererPixbuf>;

define C-function gtk-cell-renderer-pixbuf-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_pixbuf_new";
end;

define C-struct <_GtkCellRendererPixbufClass>
  constant slot gtk-cell-renderer-pixbuf-class-parent-class :: <GtkCellRendererClass>;
  constant slot gtk-cell-renderer-pixbuf-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-pixbuf-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-pixbuf-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-pixbuf-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererPixbufClass>;
end C-struct;

define C-struct <_GtkCellRendererPixbufPrivate>
  pointer-type-name: <GtkCellRendererPixbufPrivate>;
end C-struct;

define C-struct <_GtkCellRendererPrivate>
  pointer-type-name: <GtkCellRendererPrivate>;
end C-struct;

define open C-subtype <GtkCellRendererProgress> (<GtkCellRenderer>)
  constant slot gtk-cell-renderer-progress-parent-instance :: <GtkCellRenderer>;
  constant slot gtk-cell-renderer-progress-priv :: <GtkCellRendererProgressPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererProgress*> => <GtkCellRendererProgress>;

define C-function gtk-cell-renderer-progress-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_progress_new";
end;

define C-struct <_GtkCellRendererProgressClass>
  constant slot gtk-cell-renderer-progress-class-parent-class :: <GtkCellRendererClass>;
  constant slot gtk-cell-renderer-progress-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-progress-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-progress-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-progress-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererProgressClass>;
end C-struct;

define C-struct <_GtkCellRendererProgressPrivate>
  pointer-type-name: <GtkCellRendererProgressPrivate>;
end C-struct;

define open C-subtype <GtkCellRendererSpin> (<GtkCellRendererText>)
  constant slot gtk-cell-renderer-spin-parent :: <GtkCellRendererText>;
  constant slot gtk-cell-renderer-spin-priv :: <GtkCellRendererSpinPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererSpin*> => <GtkCellRendererSpin>;

define C-function gtk-cell-renderer-spin-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_spin_new";
end;

define C-struct <_GtkCellRendererSpinClass>
  constant slot gtk-cell-renderer-spin-class-parent :: <GtkCellRendererTextClass>;
  constant slot gtk-cell-renderer-spin-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-spin-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-spin-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-spin-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererSpinClass>;
end C-struct;

define C-struct <_GtkCellRendererSpinPrivate>
  pointer-type-name: <GtkCellRendererSpinPrivate>;
end C-struct;

define open C-subtype <GtkCellRendererSpinner> (<GtkCellRenderer>)
  constant slot gtk-cell-renderer-spinner-parent :: <GtkCellRenderer>;
  constant slot gtk-cell-renderer-spinner-priv :: <GtkCellRendererSpinnerPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererSpinner*> => <GtkCellRendererSpinner>;

define C-function gtk-cell-renderer-spinner-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_spinner_new";
end;

define C-struct <_GtkCellRendererSpinnerClass>
  constant slot gtk-cell-renderer-spinner-class-parent-class :: <GtkCellRendererClass>;
  constant slot gtk-cell-renderer-spinner-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-spinner-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-spinner-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-spinner-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererSpinnerClass>;
end C-struct;

define C-struct <_GtkCellRendererSpinnerPrivate>
  pointer-type-name: <GtkCellRendererSpinnerPrivate>;
end C-struct;

define constant $gtk-cell-renderer-selected = 1;
define constant $gtk-cell-renderer-prelit = 2;
define constant $gtk-cell-renderer-insensitive = 4;
define constant $gtk-cell-renderer-sorted = 8;
define constant $gtk-cell-renderer-focused = 16;
define constant $gtk-cell-renderer-expandable = 32;
define constant $gtk-cell-renderer-expanded = 64;
define constant <GtkCellRendererState> = <C-int>;
define C-pointer-type <GtkCellRendererState*> => <GtkCellRendererState>;

define open C-subtype <GtkCellRendererText> (<GtkCellRenderer>)
  constant slot gtk-cell-renderer-text-parent :: <GtkCellRenderer>;
  constant slot gtk-cell-renderer-text-priv :: <GtkCellRendererTextPrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererText*> => <GtkCellRendererText>;

define C-function gtk-cell-renderer-text-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_text_new";
end;

define C-function gtk-cell-renderer-text-set-fixed-height-from-font
  input parameter self :: <GtkCellRendererText>;
  input parameter number_of_rows_ :: <C-signed-int>;
  c-name: "gtk_cell_renderer_text_set_fixed_height_from_font";
end;

define C-struct <_GtkCellRendererTextClass>
  constant slot gtk-cell-renderer-text-class-parent-class :: <GtkCellRendererClass>;
  constant slot gtk-cell-renderer-text-class-edited :: <C-function-pointer>;
  constant slot gtk-cell-renderer-text-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-text-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-text-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-text-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererTextClass>;
end C-struct;

define C-struct <_GtkCellRendererTextPrivate>
  pointer-type-name: <GtkCellRendererTextPrivate>;
end C-struct;

define open C-subtype <GtkCellRendererToggle> (<GtkCellRenderer>)
  constant slot gtk-cell-renderer-toggle-parent :: <GtkCellRenderer>;
  constant slot gtk-cell-renderer-toggle-priv :: <GtkCellRendererTogglePrivate>;
end C-subtype;

define C-pointer-type <GtkCellRendererToggle*> => <GtkCellRendererToggle>;

define C-function gtk-cell-renderer-toggle-new
  result res :: <GtkCellRenderer>;
  c-name: "gtk_cell_renderer_toggle_new";
end;

define C-function gtk-cell-renderer-toggle-get-activatable
  input parameter self :: <GtkCellRendererToggle>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_get_activatable";
end;

define C-function gtk-cell-renderer-toggle-get-active
  input parameter self :: <GtkCellRendererToggle>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_get_active";
end;

define C-function gtk-cell-renderer-toggle-get-radio
  input parameter self :: <GtkCellRendererToggle>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_get_radio";
end;

define C-function gtk-cell-renderer-toggle-set-activatable
  input parameter self :: <GtkCellRendererToggle>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_set_activatable";
end;

define C-function gtk-cell-renderer-toggle-set-active
  input parameter self :: <GtkCellRendererToggle>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_set_active";
end;

define C-function gtk-cell-renderer-toggle-set-radio
  input parameter self :: <GtkCellRendererToggle>;
  input parameter radio_ :: <C-boolean>;
  c-name: "gtk_cell_renderer_toggle_set_radio";
end;

define C-struct <_GtkCellRendererToggleClass>
  constant slot gtk-cell-renderer-toggle-class-parent-class :: <GtkCellRendererClass>;
  constant slot gtk-cell-renderer-toggle-class-toggled :: <C-function-pointer>;
  constant slot gtk-cell-renderer-toggle-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-renderer-toggle-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-renderer-toggle-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-renderer-toggle-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellRendererToggleClass>;
end C-struct;

define C-struct <_GtkCellRendererTogglePrivate>
  pointer-type-name: <GtkCellRendererTogglePrivate>;
end C-struct;

define open C-subtype <GtkCellView> (<GtkWidget>)
  constant slot gtk-cell-view-parent-instance :: <GtkWidget>;
  constant slot gtk-cell-view-priv :: <GtkCellViewPrivate>;
end C-subtype;

define C-pointer-type <GtkCellView*> => <GtkCellView>;

define C-function gtk-cell-view-new
  result res :: <GtkWidget>;
  c-name: "gtk_cell_view_new";
end;

define C-function gtk-cell-view-new-with-context
  input parameter area_ :: <GtkCellArea>;
  input parameter context_ :: <GtkCellAreaContext>;
  result res :: <GtkWidget>;
  c-name: "gtk_cell_view_new_with_context";
end;

define C-function gtk-cell-view-new-with-markup
  input parameter markup_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_cell_view_new_with_markup";
end;

define C-function gtk-cell-view-new-with-pixbuf
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <GtkWidget>;
  c-name: "gtk_cell_view_new_with_pixbuf";
end;

define C-function gtk-cell-view-new-with-text
  input parameter text_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_cell_view_new_with_text";
end;

define C-function gtk-cell-view-get-displayed-row
  input parameter self :: <GtkCellView>;
  result res :: <GtkTreePath>;
  c-name: "gtk_cell_view_get_displayed_row";
end;

define C-function gtk-cell-view-get-draw-sensitive
  input parameter self :: <GtkCellView>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_view_get_draw_sensitive";
end;

define C-function gtk-cell-view-get-fit-model
  input parameter self :: <GtkCellView>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_view_get_fit_model";
end;

define C-function gtk-cell-view-get-model
  input parameter self :: <GtkCellView>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_cell_view_get_model";
end;

define C-function gtk-cell-view-get-size-of-row
  input parameter self :: <GtkCellView>;
  input parameter path_ :: <GtkTreePath>;
  output parameter requisition_ :: <GtkRequisition>;
  result res :: <C-boolean>;
  c-name: "gtk_cell_view_get_size_of_row";
end;

define C-function gtk-cell-view-set-background-color
  input parameter self :: <GtkCellView>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_cell_view_set_background_color";
end;

define C-function gtk-cell-view-set-background-rgba
  input parameter self :: <GtkCellView>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_cell_view_set_background_rgba";
end;

define C-function gtk-cell-view-set-displayed-row
  input parameter self :: <GtkCellView>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_cell_view_set_displayed_row";
end;

define C-function gtk-cell-view-set-draw-sensitive
  input parameter self :: <GtkCellView>;
  input parameter draw_sensitive_ :: <C-boolean>;
  c-name: "gtk_cell_view_set_draw_sensitive";
end;

define C-function gtk-cell-view-set-fit-model
  input parameter self :: <GtkCellView>;
  input parameter fit_model_ :: <C-boolean>;
  c-name: "gtk_cell_view_set_fit_model";
end;

define C-function gtk-cell-view-set-model
  input parameter self :: <GtkCellView>;
  input parameter model_ :: <GtkTreeModel>;
  c-name: "gtk_cell_view_set_model";
end;

define C-struct <_GtkCellViewClass>
  constant slot gtk-cell-view-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-cell-view-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-cell-view-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-cell-view-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-cell-view-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCellViewClass>;
end C-struct;

define C-struct <_GtkCellViewPrivate>
  pointer-type-name: <GtkCellViewPrivate>;
end C-struct;

define open C-subtype <GtkCheckButton> (<GtkToggleButton>)
  constant slot gtk-check-button-toggle-button :: <GtkToggleButton>;
end C-subtype;

define C-pointer-type <GtkCheckButton*> => <GtkCheckButton>;

define C-function gtk-check-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_check_button_new";
end;

define C-function gtk-check-button-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_check_button_new_with_label";
end;

define C-function gtk-check-button-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_check_button_new_with_mnemonic";
end;

define C-struct <_GtkCheckButtonClass>
  constant slot gtk-check-button-class-parent-class :: <GtkToggleButtonClass>;
  constant slot gtk-check-button-class-draw-indicator :: <C-function-pointer>;
  constant slot gtk-check-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-check-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-check-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-check-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCheckButtonClass>;
end C-struct;

define open C-subtype <GtkCheckMenuItem> (<GtkMenuItem>)
  constant slot gtk-check-menu-item-menu-item :: <GtkMenuItem>;
  constant slot gtk-check-menu-item-priv :: <GtkCheckMenuItemPrivate>;
end C-subtype;

define C-pointer-type <GtkCheckMenuItem*> => <GtkCheckMenuItem>;

define C-function gtk-check-menu-item-new
  result res :: <GtkWidget>;
  c-name: "gtk_check_menu_item_new";
end;

define C-function gtk-check-menu-item-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_check_menu_item_new_with_label";
end;

define C-function gtk-check-menu-item-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_check_menu_item_new_with_mnemonic";
end;

define C-function gtk-check-menu-item-get-active
  input parameter self :: <GtkCheckMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_check_menu_item_get_active";
end;

define C-function gtk-check-menu-item-get-draw-as-radio
  input parameter self :: <GtkCheckMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_check_menu_item_get_draw_as_radio";
end;

define C-function gtk-check-menu-item-get-inconsistent
  input parameter self :: <GtkCheckMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_check_menu_item_get_inconsistent";
end;

define C-function gtk-check-menu-item-set-active
  input parameter self :: <GtkCheckMenuItem>;
  input parameter is_active_ :: <C-boolean>;
  c-name: "gtk_check_menu_item_set_active";
end;

define C-function gtk-check-menu-item-set-draw-as-radio
  input parameter self :: <GtkCheckMenuItem>;
  input parameter draw_as_radio_ :: <C-boolean>;
  c-name: "gtk_check_menu_item_set_draw_as_radio";
end;

define C-function gtk-check-menu-item-set-inconsistent
  input parameter self :: <GtkCheckMenuItem>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_check_menu_item_set_inconsistent";
end;

define C-function gtk-check-menu-item-toggled
  input parameter self :: <GtkCheckMenuItem>;
  c-name: "gtk_check_menu_item_toggled";
end;

define C-struct <_GtkCheckMenuItemClass>
  constant slot gtk-check-menu-item-class-parent-class :: <GtkMenuItemClass>;
  constant slot gtk-check-menu-item-class-toggled :: <C-function-pointer>;
  constant slot gtk-check-menu-item-class-draw-indicator :: <C-function-pointer>;
  constant slot gtk-check-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-check-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-check-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-check-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCheckMenuItemClass>;
end C-struct;

define C-struct <_GtkCheckMenuItemPrivate>
  pointer-type-name: <GtkCheckMenuItemPrivate>;
end C-struct;

define open C-subtype <GtkClipboard> (<GObject>)
end C-subtype;

define C-pointer-type <GtkClipboard*> => <GtkClipboard>;

define C-function gtk-clipboard-get
  input parameter selection_ :: <GdkAtom>;
  result res :: <GtkClipboard>;
  c-name: "gtk_clipboard_get";
end;

define C-function gtk-clipboard-get-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter selection_ :: <GdkAtom>;
  result res :: <GtkClipboard>;
  c-name: "gtk_clipboard_get_for_display";
end;

define C-function gtk-clipboard-clear
  input parameter self :: <GtkClipboard>;
  c-name: "gtk_clipboard_clear";
end;

define C-function gtk-clipboard-get-display
  input parameter self :: <GtkClipboard>;
  result res :: <GdkDisplay>;
  c-name: "gtk_clipboard_get_display";
end;

define C-function gtk-clipboard-get-owner
  input parameter self :: <GtkClipboard>;
  result res :: <GObject>;
  c-name: "gtk_clipboard_get_owner";
end;

define C-function gtk-clipboard-request-contents
  input parameter self :: <GtkClipboard>;
  input parameter target_ :: <GdkAtom>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_contents";
end;

define C-function gtk-clipboard-request-image
  input parameter self :: <GtkClipboard>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_image";
end;

define C-function gtk-clipboard-request-rich-text
  input parameter self :: <GtkClipboard>;
  input parameter buffer_ :: <GtkTextBuffer>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_rich_text";
end;

define C-function gtk-clipboard-request-targets
  input parameter self :: <GtkClipboard>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_targets";
end;

define C-function gtk-clipboard-request-text
  input parameter self :: <GtkClipboard>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_text";
end;

define C-function gtk-clipboard-request-uris
  input parameter self :: <GtkClipboard>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_clipboard_request_uris";
end;

define C-function gtk-clipboard-set-can-store
  input parameter self :: <GtkClipboard>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  c-name: "gtk_clipboard_set_can_store";
end;

define C-function gtk-clipboard-set-image
  input parameter self :: <GtkClipboard>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_clipboard_set_image";
end;

define C-function gtk-clipboard-set-text
  input parameter self :: <GtkClipboard>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  c-name: "gtk_clipboard_set_text";
end;

define C-function gtk-clipboard-store
  input parameter self :: <GtkClipboard>;
  c-name: "gtk_clipboard_store";
end;

define C-function gtk-clipboard-wait-for-contents
  input parameter self :: <GtkClipboard>;
  input parameter target_ :: <GdkAtom>;
  result res :: <GtkSelectionData>;
  c-name: "gtk_clipboard_wait_for_contents";
end;

define C-function gtk-clipboard-wait-for-image
  input parameter self :: <GtkClipboard>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_clipboard_wait_for_image";
end;

define C-function gtk-clipboard-wait-for-rich-text
  input parameter self :: <GtkClipboard>;
  input parameter buffer_ :: <GtkTextBuffer>;
  output parameter format_ :: <GdkAtom>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "gtk_clipboard_wait_for_rich_text";
end;

define C-function gtk-clipboard-wait-for-targets
  input parameter self :: <GtkClipboard>;
  output parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_targets_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_for_targets";
end;

define C-function gtk-clipboard-wait-for-text
  input parameter self :: <GtkClipboard>;
  result res :: <C-string>;
  c-name: "gtk_clipboard_wait_for_text";
end;

define C-function gtk-clipboard-wait-for-uris
  input parameter self :: <GtkClipboard>;
  result res :: <C-string*>;
  c-name: "gtk_clipboard_wait_for_uris";
end;

define C-function gtk-clipboard-wait-is-image-available
  input parameter self :: <GtkClipboard>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_is_image_available";
end;

define C-function gtk-clipboard-wait-is-rich-text-available
  input parameter self :: <GtkClipboard>;
  input parameter buffer_ :: <GtkTextBuffer>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_is_rich_text_available";
end;

define C-function gtk-clipboard-wait-is-target-available
  input parameter self :: <GtkClipboard>;
  input parameter target_ :: <GdkAtom>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_is_target_available";
end;

define C-function gtk-clipboard-wait-is-text-available
  input parameter self :: <GtkClipboard>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_is_text_available";
end;

define C-function gtk-clipboard-wait-is-uris-available
  input parameter self :: <GtkClipboard>;
  result res :: <C-boolean>;
  c-name: "gtk_clipboard_wait_is_uris_available";
end;

define open C-subtype <GtkColorButton> (<GtkButton>)
  constant slot gtk-color-button-button :: <GtkButton>;
  constant slot gtk-color-button-priv :: <GtkColorButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkColorButton*> => <GtkColorButton>;

define C-function gtk-color-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_color_button_new";
end;

define C-function gtk-color-button-new-with-color
  input parameter color_ :: <GdkColor>;
  result res :: <GtkWidget>;
  c-name: "gtk_color_button_new_with_color";
end;

define C-function gtk-color-button-new-with-rgba
  input parameter rgba_ :: <GdkRGBA>;
  result res :: <GtkWidget>;
  c-name: "gtk_color_button_new_with_rgba";
end;

define C-function gtk-color-button-get-alpha
  input parameter self :: <GtkColorButton>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_color_button_get_alpha";
end;

define C-function gtk-color-button-get-color
  input parameter self :: <GtkColorButton>;
  output parameter color_ :: <GdkColor>;
  c-name: "gtk_color_button_get_color";
end;

define C-function gtk-color-button-get-rgba
  input parameter self :: <GtkColorButton>;
  output parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_button_get_rgba";
end;

define C-function gtk-color-button-get-title
  input parameter self :: <GtkColorButton>;
  result res :: <C-string>;
  c-name: "gtk_color_button_get_title";
end;

define C-function gtk-color-button-get-use-alpha
  input parameter self :: <GtkColorButton>;
  result res :: <C-boolean>;
  c-name: "gtk_color_button_get_use_alpha";
end;

define C-function gtk-color-button-set-alpha
  input parameter self :: <GtkColorButton>;
  input parameter alpha_ :: <C-unsigned-short>;
  c-name: "gtk_color_button_set_alpha";
end;

define C-function gtk-color-button-set-color
  input parameter self :: <GtkColorButton>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_color_button_set_color";
end;

define C-function gtk-color-button-set-rgba
  input parameter self :: <GtkColorButton>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_button_set_rgba";
end;

define C-function gtk-color-button-set-title
  input parameter self :: <GtkColorButton>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_color_button_set_title";
end;

define C-function gtk-color-button-set-use-alpha
  input parameter self :: <GtkColorButton>;
  input parameter use_alpha_ :: <C-boolean>;
  c-name: "gtk_color_button_set_use_alpha";
end;

define C-struct <_GtkColorButtonClass>
  constant slot gtk-color-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-color-button-class-color-set :: <C-function-pointer>;
  constant slot gtk-color-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-color-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-color-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-color-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkColorButtonClass>;
end C-struct;

define C-struct <_GtkColorButtonPrivate>
  pointer-type-name: <GtkColorButtonPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkColorChooser> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkColorChooser*> => <GtkColorChooser>;

define C-function gtk-color-chooser-add-palette
  input parameter self :: <GtkColorChooser>;
  input parameter orientation_ :: <GtkOrientation>;
  input parameter colors_per_line_ :: <C-signed-int>;
  input parameter n_colors_ :: <C-signed-int>;
  input parameter colors_ :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_color_chooser_add_palette";
end;

define C-function gtk-color-chooser-get-rgba
  input parameter self :: <GtkColorChooser>;
  input parameter color_ :: <GdkRGBA>;
  c-name: "gtk_color_chooser_get_rgba";
end;

define C-function gtk-color-chooser-get-use-alpha
  input parameter self :: <GtkColorChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_color_chooser_get_use_alpha";
end;

define C-function gtk-color-chooser-set-rgba
  input parameter self :: <GtkColorChooser>;
  input parameter color_ :: <GdkRGBA>;
  c-name: "gtk_color_chooser_set_rgba";
end;

define C-function gtk-color-chooser-set-use-alpha
  input parameter self :: <GtkColorChooser>;
  input parameter use_alpha_ :: <C-boolean>;
  c-name: "gtk_color_chooser_set_use_alpha";
end;

define open C-subtype <GtkColorChooserDialog> (<GtkDialog>)
  constant slot gtk-color-chooser-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-color-chooser-dialog-priv :: <GtkColorChooserDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkColorChooserDialog*> => <GtkColorChooserDialog>;

define C-function gtk-color-chooser-dialog-new
  input parameter title_ :: <C-string>;
  input parameter parent_ :: <GtkWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_color_chooser_dialog_new";
end;

define C-struct <_GtkColorChooserDialogClass>
  constant slot gtk-color-chooser-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-color-chooser-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-color-chooser-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-color-chooser-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-color-chooser-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkColorChooserDialogClass>;
end C-struct;

define C-struct <_GtkColorChooserDialogPrivate>
  pointer-type-name: <GtkColorChooserDialogPrivate>;
end C-struct;

define C-struct <_GtkColorChooserInterface>
  constant slot gtk-color-chooser-interface-base-interface :: <GTypeInterface>;
  constant slot gtk-color-chooser-interface-get-rgba :: <C-function-pointer>;
  constant slot gtk-color-chooser-interface-set-rgba :: <C-function-pointer>;
  constant slot gtk-color-chooser-interface-add-palette :: <C-function-pointer>;
  constant slot gtk-color-chooser-interface-color-activated :: <C-function-pointer>;
  constant slot gtk-color-chooser-interface-padding :: <C-void*>;
  pointer-type-name: <GtkColorChooserInterface>;
end C-struct;

define open C-subtype <GtkColorChooserWidget> (<GtkBox>)
  constant slot gtk-color-chooser-widget-parent-instance :: <GtkBox>;
  constant slot gtk-color-chooser-widget-priv :: <GtkColorChooserWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkColorChooserWidget*> => <GtkColorChooserWidget>;

define C-function gtk-color-chooser-widget-new
  result res :: <GtkWidget>;
  c-name: "gtk_color_chooser_widget_new";
end;

define C-struct <_GtkColorChooserWidgetClass>
  constant slot gtk-color-chooser-widget-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-color-chooser-widget-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkColorChooserWidgetClass>;
end C-struct;

define C-struct <_GtkColorChooserWidgetPrivate>
  pointer-type-name: <GtkColorChooserWidgetPrivate>;
end C-struct;

define open C-subtype <GtkColorSelection> (<GtkBox>)
  constant slot gtk-color-selection-parent-instance :: <GtkBox>;
  constant slot gtk-color-selection-private-data :: <GtkColorSelectionPrivate>;
end C-subtype;

define C-pointer-type <GtkColorSelection*> => <GtkColorSelection>;

define C-function gtk-color-selection-new
  result res :: <GtkWidget>;
  c-name: "gtk_color_selection_new";
end;

define C-function gtk-color-selection-palette-from-string
  input parameter str_ :: <C-string>;
  output parameter colors_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_colors_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_color_selection_palette_from_string";
end;

define C-function gtk-color-selection-palette-to-string
  input parameter colors_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_colors_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "gtk_color_selection_palette_to_string";
end;

define C-function gtk-color-selection-get-current-alpha
  input parameter self :: <GtkColorSelection>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_color_selection_get_current_alpha";
end;

define C-function gtk-color-selection-get-current-color
  input parameter self :: <GtkColorSelection>;
  output parameter color_ :: <GdkColor>;
  c-name: "gtk_color_selection_get_current_color";
end;

define C-function gtk-color-selection-get-current-rgba
  input parameter self :: <GtkColorSelection>;
  output parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_selection_get_current_rgba";
end;

define C-function gtk-color-selection-get-has-opacity-control
  input parameter self :: <GtkColorSelection>;
  result res :: <C-boolean>;
  c-name: "gtk_color_selection_get_has_opacity_control";
end;

define C-function gtk-color-selection-get-has-palette
  input parameter self :: <GtkColorSelection>;
  result res :: <C-boolean>;
  c-name: "gtk_color_selection_get_has_palette";
end;

define C-function gtk-color-selection-get-previous-alpha
  input parameter self :: <GtkColorSelection>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_color_selection_get_previous_alpha";
end;

define C-function gtk-color-selection-get-previous-color
  input parameter self :: <GtkColorSelection>;
  output parameter color_ :: <GdkColor>;
  c-name: "gtk_color_selection_get_previous_color";
end;

define C-function gtk-color-selection-get-previous-rgba
  input parameter self :: <GtkColorSelection>;
  output parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_selection_get_previous_rgba";
end;

define C-function gtk-color-selection-is-adjusting
  input parameter self :: <GtkColorSelection>;
  result res :: <C-boolean>;
  c-name: "gtk_color_selection_is_adjusting";
end;

define C-function gtk-color-selection-set-current-alpha
  input parameter self :: <GtkColorSelection>;
  input parameter alpha_ :: <C-unsigned-short>;
  c-name: "gtk_color_selection_set_current_alpha";
end;

define C-function gtk-color-selection-set-current-color
  input parameter self :: <GtkColorSelection>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_color_selection_set_current_color";
end;

define C-function gtk-color-selection-set-current-rgba
  input parameter self :: <GtkColorSelection>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_selection_set_current_rgba";
end;

define C-function gtk-color-selection-set-has-opacity-control
  input parameter self :: <GtkColorSelection>;
  input parameter has_opacity_ :: <C-boolean>;
  c-name: "gtk_color_selection_set_has_opacity_control";
end;

define C-function gtk-color-selection-set-has-palette
  input parameter self :: <GtkColorSelection>;
  input parameter has_palette_ :: <C-boolean>;
  c-name: "gtk_color_selection_set_has_palette";
end;

define C-function gtk-color-selection-set-previous-alpha
  input parameter self :: <GtkColorSelection>;
  input parameter alpha_ :: <C-unsigned-short>;
  c-name: "gtk_color_selection_set_previous_alpha";
end;

define C-function gtk-color-selection-set-previous-color
  input parameter self :: <GtkColorSelection>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_color_selection_set_previous_color";
end;

define C-function gtk-color-selection-set-previous-rgba
  input parameter self :: <GtkColorSelection>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gtk_color_selection_set_previous_rgba";
end;

define C-struct <_GtkColorSelectionClass>
  constant slot gtk-color-selection-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-color-selection-class-color-changed :: <C-function-pointer>;
  constant slot gtk-color-selection-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-color-selection-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-color-selection-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-color-selection-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkColorSelectionClass>;
end C-struct;

define open C-subtype <GtkColorSelectionDialog> (<GtkDialog>)
  constant slot gtk-color-selection-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-color-selection-dialog-priv :: <GtkColorSelectionDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkColorSelectionDialog*> => <GtkColorSelectionDialog>;

define C-function gtk-color-selection-dialog-new
  input parameter title_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_color_selection_dialog_new";
end;

define C-function gtk-color-selection-dialog-get-color-selection
  input parameter self :: <GtkColorSelectionDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_color_selection_dialog_get_color_selection";
end;

define C-struct <_GtkColorSelectionDialogClass>
  constant slot gtk-color-selection-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-color-selection-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-color-selection-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-color-selection-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-color-selection-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkColorSelectionDialogClass>;
end C-struct;

define C-struct <_GtkColorSelectionDialogPrivate>
  pointer-type-name: <GtkColorSelectionDialogPrivate>;
end C-struct;

define C-struct <_GtkColorSelectionPrivate>
  pointer-type-name: <GtkColorSelectionPrivate>;
end C-struct;

define open C-subtype <GtkComboBox> (<GtkBin>)
  constant slot gtk-combo-box-parent-instance :: <GtkBin>;
  constant slot gtk-combo-box-priv :: <GtkComboBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkComboBox*> => <GtkComboBox>;

define C-function gtk-combo-box-new
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new";
end;

define C-function gtk-combo-box-new-with-area
  input parameter area_ :: <GtkCellArea>;
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new_with_area";
end;

define C-function gtk-combo-box-new-with-area-and-entry
  input parameter area_ :: <GtkCellArea>;
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new_with_area_and_entry";
end;

define C-function gtk-combo-box-new-with-entry
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new_with_entry";
end;

define C-function gtk-combo-box-new-with-model
  input parameter model_ :: <GtkTreeModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new_with_model";
end;

define C-function gtk-combo-box-new-with-model-and-entry
  input parameter model_ :: <GtkTreeModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_new_with_model_and_entry";
end;

define C-function gtk-combo-box-get-active
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_active";
end;

define C-function gtk-combo-box-get-active-id
  input parameter self :: <GtkComboBox>;
  result res :: <C-string>;
  c-name: "gtk_combo_box_get_active_id";
end;

define C-function gtk-combo-box-get-active-iter
  input parameter self :: <GtkComboBox>;
  output parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_get_active_iter";
end;

define C-function gtk-combo-box-get-add-tearoffs
  input parameter self :: <GtkComboBox>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_get_add_tearoffs";
end;

define C-function gtk-combo-box-get-button-sensitivity
  input parameter self :: <GtkComboBox>;
  result res :: <GtkSensitivityType>;
  c-name: "gtk_combo_box_get_button_sensitivity";
end;

define C-function gtk-combo-box-get-column-span-column
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_column_span_column";
end;

define C-function gtk-combo-box-get-entry-text-column
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_entry_text_column";
end;

define C-function gtk-combo-box-get-focus-on-click
  input parameter self :: <GtkComboBox>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_get_focus_on_click";
end;

define C-function gtk-combo-box-get-has-entry
  input parameter self :: <GtkComboBox>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_get_has_entry";
end;

define C-function gtk-combo-box-get-id-column
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_id_column";
end;

define C-function gtk-combo-box-get-model
  input parameter self :: <GtkComboBox>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_combo_box_get_model";
end;

define C-function gtk-combo-box-get-popup-accessible
  input parameter self :: <GtkComboBox>;
  result res :: <AtkObject>;
  c-name: "gtk_combo_box_get_popup_accessible";
end;

define C-function gtk-combo-box-get-popup-fixed-width
  input parameter self :: <GtkComboBox>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_get_popup_fixed_width";
end;

define C-function gtk-combo-box-get-row-span-column
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_row_span_column";
end;

define C-function gtk-combo-box-get-title
  input parameter self :: <GtkComboBox>;
  result res :: <C-string>;
  c-name: "gtk_combo_box_get_title";
end;

define C-function gtk-combo-box-get-wrap-width
  input parameter self :: <GtkComboBox>;
  result res :: <C-signed-int>;
  c-name: "gtk_combo_box_get_wrap_width";
end;

define C-function gtk-combo-box-popdown
  input parameter self :: <GtkComboBox>;
  c-name: "gtk_combo_box_popdown";
end;

define C-function gtk-combo-box-popup
  input parameter self :: <GtkComboBox>;
  c-name: "gtk_combo_box_popup";
end;

define C-function gtk-combo-box-popup-for-device
  input parameter self :: <GtkComboBox>;
  input parameter device_ :: <GdkDevice>;
  c-name: "gtk_combo_box_popup_for_device";
end;

define C-function gtk-combo-box-set-active
  input parameter self :: <GtkComboBox>;
  input parameter index__ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_active";
end;

define C-function gtk-combo-box-set-active-id
  input parameter self :: <GtkComboBox>;
  input parameter active_id_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_combo_box_set_active_id";
end;

define C-function gtk-combo-box-set-active-iter
  input parameter self :: <GtkComboBox>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_combo_box_set_active_iter";
end;

define C-function gtk-combo-box-set-add-tearoffs
  input parameter self :: <GtkComboBox>;
  input parameter add_tearoffs_ :: <C-boolean>;
  c-name: "gtk_combo_box_set_add_tearoffs";
end;

define C-function gtk-combo-box-set-button-sensitivity
  input parameter self :: <GtkComboBox>;
  input parameter sensitivity_ :: <GtkSensitivityType>;
  c-name: "gtk_combo_box_set_button_sensitivity";
end;

define C-function gtk-combo-box-set-column-span-column
  input parameter self :: <GtkComboBox>;
  input parameter column_span_ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_column_span_column";
end;

define C-function gtk-combo-box-set-entry-text-column
  input parameter self :: <GtkComboBox>;
  input parameter text_column_ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_entry_text_column";
end;

define C-function gtk-combo-box-set-focus-on-click
  input parameter self :: <GtkComboBox>;
  input parameter focus_on_click_ :: <C-boolean>;
  c-name: "gtk_combo_box_set_focus_on_click";
end;

define C-function gtk-combo-box-set-id-column
  input parameter self :: <GtkComboBox>;
  input parameter id_column_ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_id_column";
end;

define C-function gtk-combo-box-set-model
  input parameter self :: <GtkComboBox>;
  input parameter model_ :: <GtkTreeModel>;
  c-name: "gtk_combo_box_set_model";
end;

define C-function gtk-combo-box-set-popup-fixed-width
  input parameter self :: <GtkComboBox>;
  input parameter fixed_ :: <C-boolean>;
  c-name: "gtk_combo_box_set_popup_fixed_width";
end;

define C-function gtk-combo-box-set-row-separator-func
  input parameter self :: <GtkComboBox>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_combo_box_set_row_separator_func";
end;

define C-function gtk-combo-box-set-row-span-column
  input parameter self :: <GtkComboBox>;
  input parameter row_span_ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_row_span_column";
end;

define C-function gtk-combo-box-set-title
  input parameter self :: <GtkComboBox>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_combo_box_set_title";
end;

define C-function gtk-combo-box-set-wrap-width
  input parameter self :: <GtkComboBox>;
  input parameter width_ :: <C-signed-int>;
  c-name: "gtk_combo_box_set_wrap_width";
end;

define C-struct <_GtkComboBoxClass>
  constant slot gtk-combo-box-class-parent-class :: <GtkBinClass>;
  constant slot gtk-combo-box-class-changed :: <C-function-pointer>;
  constant slot gtk-combo-box-class-format-entry-text :: <C-function-pointer>;
  constant slot gtk-combo-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-combo-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-combo-box-class-_gtk-reserved3 :: <C-void*>;
  pointer-type-name: <GtkComboBoxClass>;
end C-struct;

define C-struct <_GtkComboBoxPrivate>
  pointer-type-name: <GtkComboBoxPrivate>;
end C-struct;

define open C-subtype <GtkComboBoxText> (<GtkComboBox>)
  constant slot gtk-combo-box-text-parent-instance :: <GtkComboBox>;
  constant slot gtk-combo-box-text-priv :: <GtkComboBoxTextPrivate>;
end C-subtype;

define C-pointer-type <GtkComboBoxText*> => <GtkComboBoxText>;

define C-function gtk-combo-box-text-new
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_text_new";
end;

define C-function gtk-combo-box-text-new-with-entry
  result res :: <GtkWidget>;
  c-name: "gtk_combo_box_text_new_with_entry";
end;

define C-function gtk-combo-box-text-append
  input parameter self :: <GtkComboBoxText>;
  input parameter id_ :: <C-string>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_append";
end;

define C-function gtk-combo-box-text-append-text
  input parameter self :: <GtkComboBoxText>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_append_text";
end;

define C-function gtk-combo-box-text-get-active-text
  input parameter self :: <GtkComboBoxText>;
  result res :: <C-string>;
  c-name: "gtk_combo_box_text_get_active_text";
end;

define C-function gtk-combo-box-text-insert
  input parameter self :: <GtkComboBoxText>;
  input parameter position_ :: <C-signed-int>;
  input parameter id_ :: <C-string>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_insert";
end;

define C-function gtk-combo-box-text-insert-text
  input parameter self :: <GtkComboBoxText>;
  input parameter position_ :: <C-signed-int>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_insert_text";
end;

define C-function gtk-combo-box-text-prepend
  input parameter self :: <GtkComboBoxText>;
  input parameter id_ :: <C-string>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_prepend";
end;

define C-function gtk-combo-box-text-prepend-text
  input parameter self :: <GtkComboBoxText>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_combo_box_text_prepend_text";
end;

define C-function gtk-combo-box-text-remove
  input parameter self :: <GtkComboBoxText>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_combo_box_text_remove";
end;

define C-function gtk-combo-box-text-remove-all
  input parameter self :: <GtkComboBoxText>;
  c-name: "gtk_combo_box_text_remove_all";
end;

define C-struct <_GtkComboBoxTextClass>
  constant slot gtk-combo-box-text-class-parent-class :: <GtkComboBoxClass>;
  constant slot gtk-combo-box-text-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-combo-box-text-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-combo-box-text-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-combo-box-text-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkComboBoxTextClass>;
end C-struct;

define C-struct <_GtkComboBoxTextPrivate>
  pointer-type-name: <GtkComboBoxTextPrivate>;
end C-struct;

define open C-subtype <GtkContainer> (<GtkWidget>)
  constant slot gtk-container-widget :: <GtkWidget>;
  constant slot gtk-container-priv :: <GtkContainerPrivate>;
end C-subtype;

define C-pointer-type <GtkContainer*> => <GtkContainer>;

define C-function gtk-container-add
  input parameter self :: <GtkContainer>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_container_add";
end;

define C-function gtk-container-check-resize
  input parameter self :: <GtkContainer>;
  c-name: "gtk_container_check_resize";
end;

define C-function gtk-container-child-get-property
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_container_child_get_property";
end;

define C-function gtk-container-child-notify
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  input parameter child_property_ :: <C-string>;
  c-name: "gtk_container_child_notify";
end;

define C-function gtk-container-child-set-property
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_container_child_set_property";
end;

define C-function gtk-container-child-type
  input parameter self :: <GtkContainer>;
  result res :: <C-long>;
  c-name: "gtk_container_child_type";
end;

define C-function gtk-container-forall
  input parameter self :: <GtkContainer>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  c-name: "gtk_container_forall";
end;

define C-function gtk-container-foreach
  input parameter self :: <GtkContainer>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  c-name: "gtk_container_foreach";
end;

define C-function gtk-container-get-border-width
  input parameter self :: <GtkContainer>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_container_get_border_width";
end;

define C-function gtk-container-get-children
  input parameter self :: <GtkContainer>;
  result res :: <GList>;
  c-name: "gtk_container_get_children";
end;

define C-function gtk-container-get-focus-chain
  input parameter self :: <GtkContainer>;
  output parameter focusable_widgets_ :: <GList>;
  result res :: <C-boolean>;
  c-name: "gtk_container_get_focus_chain";
end;

define C-function gtk-container-get-focus-child
  input parameter self :: <GtkContainer>;
  result res :: <GtkWidget>;
  c-name: "gtk_container_get_focus_child";
end;

define C-function gtk-container-get-focus-hadjustment
  input parameter self :: <GtkContainer>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_container_get_focus_hadjustment";
end;

define C-function gtk-container-get-focus-vadjustment
  input parameter self :: <GtkContainer>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_container_get_focus_vadjustment";
end;

define C-function gtk-container-get-path-for-child
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_container_get_path_for_child";
end;

define C-function gtk-container-get-resize-mode
  input parameter self :: <GtkContainer>;
  result res :: <GtkResizeMode>;
  c-name: "gtk_container_get_resize_mode";
end;

define C-function gtk-container-propagate-draw
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  input parameter cr_ :: <cairoContext>;
  c-name: "gtk_container_propagate_draw";
end;

define C-function gtk-container-remove
  input parameter self :: <GtkContainer>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_container_remove";
end;

define C-function gtk-container-resize-children
  input parameter self :: <GtkContainer>;
  c-name: "gtk_container_resize_children";
end;

define C-function gtk-container-set-border-width
  input parameter self :: <GtkContainer>;
  input parameter border_width_ :: <C-unsigned-int>;
  c-name: "gtk_container_set_border_width";
end;

define C-function gtk-container-set-focus-chain
  input parameter self :: <GtkContainer>;
  input parameter focusable_widgets_ :: <GList>;
  c-name: "gtk_container_set_focus_chain";
end;

define C-function gtk-container-set-focus-child
  input parameter self :: <GtkContainer>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_container_set_focus_child";
end;

define C-function gtk-container-set-focus-hadjustment
  input parameter self :: <GtkContainer>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_container_set_focus_hadjustment";
end;

define C-function gtk-container-set-focus-vadjustment
  input parameter self :: <GtkContainer>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_container_set_focus_vadjustment";
end;

define C-function gtk-container-set-reallocate-redraws
  input parameter self :: <GtkContainer>;
  input parameter needs_redraws_ :: <C-boolean>;
  c-name: "gtk_container_set_reallocate_redraws";
end;

define C-function gtk-container-set-resize-mode
  input parameter self :: <GtkContainer>;
  input parameter resize_mode_ :: <GtkResizeMode>;
  c-name: "gtk_container_set_resize_mode";
end;

define C-function gtk-container-unset-focus-chain
  input parameter self :: <GtkContainer>;
  c-name: "gtk_container_unset_focus_chain";
end;

define C-struct <_GtkContainerClass>
  constant slot gtk-container-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-container-class-add :: <C-function-pointer>;
  constant slot gtk-container-class-remove :: <C-function-pointer>;
  constant slot gtk-container-class-check-resize :: <C-function-pointer>;
  constant slot gtk-container-class-forall :: <C-function-pointer>;
  constant slot gtk-container-class-set-focus-child :: <C-function-pointer>;
  constant slot gtk-container-class-child-type :: <C-function-pointer>;
  constant slot gtk-container-class-composite-name :: <C-function-pointer>;
  constant slot gtk-container-class-set-child-property :: <C-function-pointer>;
  constant slot gtk-container-class-get-child-property :: <C-function-pointer>;
  constant slot gtk-container-class-get-path-for-child :: <C-function-pointer>;
  constant slot gtk-container-class-_handle-border-width :: <C-unsigned-int>;
  constant slot gtk-container-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-container-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkContainerClass>;
end C-struct;

define C-function gtk-container-class-find-child-property
  input parameter self :: <GtkContainerClass>;
  input parameter property_name_ :: <C-string>;
  result res :: <GParamSpec>;
  c-name: "gtk_container_class_find_child_property";
end;

define C-function gtk-container-class-handle-border-width
  input parameter self :: <GtkContainerClass>;
  c-name: "gtk_container_class_handle_border_width";
end;

define C-function gtk-container-class-install-child-property
  input parameter self :: <GtkContainerClass>;
  input parameter property_id_ :: <C-unsigned-int>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "gtk_container_class_install_child_property";
end;

define C-function gtk-container-class-list-child-properties
  input parameter self :: <GtkContainerClass>;
  output parameter n_properties_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_container_class_list_child_properties";
end;

define C-struct <_GtkContainerPrivate>
  pointer-type-name: <GtkContainerPrivate>;
end C-struct;

define constant $gtk-corner-top-left = 0;
define constant $gtk-corner-bottom-left = 1;
define constant $gtk-corner-top-right = 2;
define constant $gtk-corner-bottom-right = 3;
define constant <GtkCornerType> = <C-int>;
define C-pointer-type <GtkCornerType*> => <GtkCornerType>;

define open C-subtype <GtkCssProvider> (<GObject>)
  constant slot gtk-css-provider-parent-instance :: <GObject>;
  constant slot gtk-css-provider-priv :: <GtkCssProviderPrivate>;
end C-subtype;

define C-pointer-type <GtkCssProvider*> => <GtkCssProvider>;

define C-function gtk-css-provider-new
  result res :: <GtkCssProvider>;
  c-name: "gtk_css_provider_new";
end;

define C-function gtk-css-provider-get-default
  result res :: <GtkCssProvider>;
  c-name: "gtk_css_provider_get_default";
end;

define C-function gtk-css-provider-get-named
  input parameter name_ :: <C-string>;
  input parameter variant_ :: <C-string>;
  result res :: <GtkCssProvider>;
  c-name: "gtk_css_provider_get_named";
end;

define C-function gtk-css-provider-load-from-data
  input parameter self :: <GtkCssProvider>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_css_provider_load_from_data";
end;

define C-function gtk-css-provider-load-from-file
  input parameter self :: <GtkCssProvider>;
  input parameter file_ :: <GFile>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_css_provider_load_from_file";
end;

define C-function gtk-css-provider-load-from-path
  input parameter self :: <GtkCssProvider>;
  input parameter path_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_css_provider_load_from_path";
end;

define C-function gtk-css-provider-to-string
  input parameter self :: <GtkCssProvider>;
  result res :: <C-string>;
  c-name: "gtk_css_provider_to_string";
end;

define C-struct <_GtkCssProviderClass>
  constant slot gtk-css-provider-class-parent-class :: <GObjectClass>;
  constant slot gtk-css-provider-class-parsing-error :: <C-function-pointer>;
  constant slot gtk-css-provider-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-css-provider-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-css-provider-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkCssProviderClass>;
end C-struct;

define constant $gtk-css-provider-error-failed = 0;
define constant $gtk-css-provider-error-syntax = 1;
define constant $gtk-css-provider-error-import = 2;
define constant $gtk-css-provider-error-name = 3;
define constant $gtk-css-provider-error-deprecated = 4;
define constant $gtk-css-provider-error-unknown-value = 5;
define constant <GtkCssProviderError> = <C-int>;
define C-pointer-type <GtkCssProviderError*> => <GtkCssProviderError>;

define C-struct <_GtkCssProviderPrivate>
  pointer-type-name: <GtkCssProviderPrivate>;
end C-struct;

define C-struct <_GtkCssSection>
  pointer-type-name: <GtkCssSection>;
end C-struct;

define C-function gtk-css-section-get-end-line
  input parameter self :: <GtkCssSection>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_css_section_get_end_line";
end;

define C-function gtk-css-section-get-end-position
  input parameter self :: <GtkCssSection>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_css_section_get_end_position";
end;

define C-function gtk-css-section-get-file
  input parameter self :: <GtkCssSection>;
  result res :: <GFile>;
  c-name: "gtk_css_section_get_file";
end;

define C-function gtk-css-section-get-parent
  input parameter self :: <GtkCssSection>;
  result res :: <GtkCssSection>;
  c-name: "gtk_css_section_get_parent";
end;

define C-function gtk-css-section-get-section-type
  input parameter self :: <GtkCssSection>;
  result res :: <GtkCssSectionType>;
  c-name: "gtk_css_section_get_section_type";
end;

define C-function gtk-css-section-get-start-line
  input parameter self :: <GtkCssSection>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_css_section_get_start_line";
end;

define C-function gtk-css-section-get-start-position
  input parameter self :: <GtkCssSection>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_css_section_get_start_position";
end;

define C-function gtk-css-section-ref
  input parameter self :: <GtkCssSection>;
  result res :: <GtkCssSection>;
  c-name: "gtk_css_section_ref";
end;

define C-function gtk-css-section-unref
  input parameter self :: <GtkCssSection>;
  c-name: "gtk_css_section_unref";
end;

define constant $gtk-css-section-document = 0;
define constant $gtk-css-section-import = 1;
define constant $gtk-css-section-color-definition = 2;
define constant $gtk-css-section-binding-set = 3;
define constant $gtk-css-section-ruleset = 4;
define constant $gtk-css-section-selector = 5;
define constant $gtk-css-section-declaration = 6;
define constant $gtk-css-section-value = 7;
define constant <GtkCssSectionType> = <C-int>;
define C-pointer-type <GtkCssSectionType*> => <GtkCssSectionType>;

define constant $gtk-debug-misc = 1;
define constant $gtk-debug-plugsocket = 2;
define constant $gtk-debug-text = 4;
define constant $gtk-debug-tree = 8;
define constant $gtk-debug-updates = 16;
define constant $gtk-debug-keybindings = 32;
define constant $gtk-debug-multihead = 64;
define constant $gtk-debug-modules = 128;
define constant $gtk-debug-geometry = 256;
define constant $gtk-debug-icontheme = 512;
define constant $gtk-debug-printing = 1024;
define constant $gtk-debug-builder = 2048;
define constant $gtk-debug-size-request = 4096;
define constant <GtkDebugFlag> = <C-int>;
define C-pointer-type <GtkDebugFlag*> => <GtkDebugFlag>;

define constant $gtk-delete-chars = 0;
define constant $gtk-delete-word-ends = 1;
define constant $gtk-delete-words = 2;
define constant $gtk-delete-display-lines = 3;
define constant $gtk-delete-display-line-ends = 4;
define constant $gtk-delete-paragraph-ends = 5;
define constant $gtk-delete-paragraphs = 6;
define constant $gtk-delete-whitespace = 7;
define constant <GtkDeleteType> = <C-int>;
define C-pointer-type <GtkDeleteType*> => <GtkDeleteType>;

define constant $gtk-dest-default-motion = 1;
define constant $gtk-dest-default-highlight = 2;
define constant $gtk-dest-default-drop = 4;
define constant $gtk-dest-default-all = 7;
define constant <GtkDestDefaults> = <C-int>;
define C-pointer-type <GtkDestDefaults*> => <GtkDestDefaults>;

define open C-subtype <GtkDialog> (<GtkWindow>)
  constant slot gtk-dialog-window :: <GtkWindow>;
  constant slot gtk-dialog-priv :: <GtkDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkDialog*> => <GtkDialog>;

define C-function gtk-dialog-new
  result res :: <GtkWidget>;
  c-name: "gtk_dialog_new";
end;

define C-function gtk-dialog-add-action-widget
  input parameter self :: <GtkDialog>;
  input parameter child_ :: <GtkWidget>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_dialog_add_action_widget";
end;

define C-function gtk-dialog-add-button
  input parameter self :: <GtkDialog>;
  input parameter button_text_ :: <C-string>;
  input parameter response_id_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_dialog_add_button";
end;

define C-function gtk-dialog-get-action-area
  input parameter self :: <GtkDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_dialog_get_action_area";
end;

define C-function gtk-dialog-get-content-area
  input parameter self :: <GtkDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_dialog_get_content_area";
end;

define C-function gtk-dialog-get-response-for-widget
  input parameter self :: <GtkDialog>;
  input parameter widget_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_dialog_get_response_for_widget";
end;

define C-function gtk-dialog-get-widget-for-response
  input parameter self :: <GtkDialog>;
  input parameter response_id_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_dialog_get_widget_for_response";
end;

define C-function gtk-dialog-response
  input parameter self :: <GtkDialog>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_dialog_response";
end;

define C-function gtk-dialog-run
  input parameter self :: <GtkDialog>;
  result res :: <C-signed-int>;
  c-name: "gtk_dialog_run";
end;

define C-function gtk-dialog-set-alternative-button-order-from-array
  input parameter self :: <GtkDialog>;
  input parameter n_params_ :: <C-signed-int>;
  input parameter new_order_ :: <C-signed-int*>;
  c-name: "gtk_dialog_set_alternative_button_order_from_array";
end;

define C-function gtk-dialog-set-default-response
  input parameter self :: <GtkDialog>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_dialog_set_default_response";
end;

define C-function gtk-dialog-set-response-sensitive
  input parameter self :: <GtkDialog>;
  input parameter response_id_ :: <C-signed-int>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_dialog_set_response_sensitive";
end;

define C-struct <_GtkDialogClass>
  constant slot gtk-dialog-class-parent-class :: <GtkWindowClass>;
  constant slot gtk-dialog-class-response :: <C-function-pointer>;
  constant slot gtk-dialog-class-close :: <C-function-pointer>;
  constant slot gtk-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkDialogClass>;
end C-struct;

define constant $gtk-dialog-modal = 1;
define constant $gtk-dialog-destroy-with-parent = 2;
define constant <GtkDialogFlags> = <C-int>;
define C-pointer-type <GtkDialogFlags*> => <GtkDialogFlags>;

define C-struct <_GtkDialogPrivate>
  pointer-type-name: <GtkDialogPrivate>;
end C-struct;

define constant $gtk-dir-tab-forward = 0;
define constant $gtk-dir-tab-backward = 1;
define constant $gtk-dir-up = 2;
define constant $gtk-dir-down = 3;
define constant $gtk-dir-left = 4;
define constant $gtk-dir-right = 5;
define constant <GtkDirectionType> = <C-int>;
define C-pointer-type <GtkDirectionType*> => <GtkDirectionType>;

define constant $gtk-drag-result-success = 0;
define constant $gtk-drag-result-no-target = 1;
define constant $gtk-drag-result-user-cancelled = 2;
define constant $gtk-drag-result-timeout-expired = 3;
define constant $gtk-drag-result-grab-broken = 4;
define constant $gtk-drag-result-error = 5;
define constant <GtkDragResult> = <C-int>;
define C-pointer-type <GtkDragResult*> => <GtkDragResult>;

define open C-subtype <GtkDrawingArea> (<GtkWidget>)
  constant slot gtk-drawing-area-widget :: <GtkWidget>;
  constant slot gtk-drawing-area-dummy :: <C-void*>;
end C-subtype;

define C-pointer-type <GtkDrawingArea*> => <GtkDrawingArea>;

define C-function gtk-drawing-area-new
  result res :: <GtkWidget>;
  c-name: "gtk_drawing_area_new";
end;

define C-struct <_GtkDrawingAreaClass>
  constant slot gtk-drawing-area-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-drawing-area-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-drawing-area-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-drawing-area-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-drawing-area-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkDrawingAreaClass>;
end C-struct;

// Interface
define open C-subtype <GtkEditable> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkEditable*> => <GtkEditable>;

define C-function gtk-editable-copy-clipboard
  input parameter self :: <GtkEditable>;
  c-name: "gtk_editable_copy_clipboard";
end;

define C-function gtk-editable-cut-clipboard
  input parameter self :: <GtkEditable>;
  c-name: "gtk_editable_cut_clipboard";
end;

define C-function gtk-editable-delete-selection
  input parameter self :: <GtkEditable>;
  c-name: "gtk_editable_delete_selection";
end;

define C-function gtk-editable-delete-text
  input parameter self :: <GtkEditable>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  c-name: "gtk_editable_delete_text";
end;

define C-function gtk-editable-get-chars
  input parameter self :: <GtkEditable>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "gtk_editable_get_chars";
end;

define C-function gtk-editable-get-editable
  input parameter self :: <GtkEditable>;
  result res :: <C-boolean>;
  c-name: "gtk_editable_get_editable";
end;

define C-function gtk-editable-get-position
  input parameter self :: <GtkEditable>;
  result res :: <C-signed-int>;
  c-name: "gtk_editable_get_position";
end;

define C-function gtk-editable-get-selection-bounds
  input parameter self :: <GtkEditable>;
  output parameter start_pos_ :: <C-signed-int*>;
  output parameter end_pos_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_editable_get_selection_bounds";
end;

define C-function gtk-editable-insert-text
  input parameter self :: <GtkEditable>;
  input parameter new_text_ :: <C-string>;
  input parameter new_text_length_ :: <C-signed-int>;
  input output parameter position_ :: <C-signed-int*>;
  c-name: "gtk_editable_insert_text";
end;

define C-function gtk-editable-paste-clipboard
  input parameter self :: <GtkEditable>;
  c-name: "gtk_editable_paste_clipboard";
end;

define C-function gtk-editable-select-region
  input parameter self :: <GtkEditable>;
  input parameter start_pos_ :: <C-signed-int>;
  input parameter end_pos_ :: <C-signed-int>;
  c-name: "gtk_editable_select_region";
end;

define C-function gtk-editable-set-editable
  input parameter self :: <GtkEditable>;
  input parameter is_editable_ :: <C-boolean>;
  c-name: "gtk_editable_set_editable";
end;

define C-function gtk-editable-set-position
  input parameter self :: <GtkEditable>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_editable_set_position";
end;

define C-struct <_GtkEditableInterface>
  constant slot gtk-editable-interface-base-iface :: <GTypeInterface>;
  constant slot gtk-editable-interface-insert-text :: <C-function-pointer>;
  constant slot gtk-editable-interface-delete-text :: <C-function-pointer>;
  constant slot gtk-editable-interface-changed :: <C-function-pointer>;
  constant slot gtk-editable-interface-do-insert-text :: <C-function-pointer>;
  constant slot gtk-editable-interface-do-delete-text :: <C-function-pointer>;
  constant slot gtk-editable-interface-get-chars :: <C-function-pointer>;
  constant slot gtk-editable-interface-set-selection-bounds :: <C-function-pointer>;
  constant slot gtk-editable-interface-get-selection-bounds :: <C-function-pointer>;
  constant slot gtk-editable-interface-set-position :: <C-function-pointer>;
  constant slot gtk-editable-interface-get-position :: <C-function-pointer>;
  pointer-type-name: <GtkEditableInterface>;
end C-struct;

define open C-subtype <GtkEntry> (<GtkWidget>)
  constant slot gtk-entry-parent-instance :: <GtkWidget>;
  constant slot gtk-entry-priv :: <GtkEntryPrivate>;
end C-subtype;

define C-pointer-type <GtkEntry*> => <GtkEntry>;

define C-function gtk-entry-new
  result res :: <GtkWidget>;
  c-name: "gtk_entry_new";
end;

define C-function gtk-entry-new-with-buffer
  input parameter buffer_ :: <GtkEntryBuffer>;
  result res :: <GtkWidget>;
  c-name: "gtk_entry_new_with_buffer";
end;

define C-function gtk-entry-get-activates-default
  input parameter self :: <GtkEntry>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_activates_default";
end;

define C-function gtk-entry-get-alignment
  input parameter self :: <GtkEntry>;
  result res :: <C-float>;
  c-name: "gtk_entry_get_alignment";
end;

define C-function gtk-entry-get-buffer
  input parameter self :: <GtkEntry>;
  result res :: <GtkEntryBuffer>;
  c-name: "gtk_entry_get_buffer";
end;

define C-function gtk-entry-get-completion
  input parameter self :: <GtkEntry>;
  result res :: <GtkEntryCompletion>;
  c-name: "gtk_entry_get_completion";
end;

define C-function gtk-entry-get-current-icon-drag-source
  input parameter self :: <GtkEntry>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_get_current_icon_drag_source";
end;

define C-function gtk-entry-get-cursor-hadjustment
  input parameter self :: <GtkEntry>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_entry_get_cursor_hadjustment";
end;

define C-function gtk-entry-get-has-frame
  input parameter self :: <GtkEntry>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_has_frame";
end;

define C-function gtk-entry-get-icon-activatable
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_icon_activatable";
end;

define C-function gtk-entry-get-icon-area
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  output parameter icon_area_ :: <cairoRectangleInt>;
  c-name: "gtk_entry_get_icon_area";
end;

define C-function gtk-entry-get-icon-at-pos
  input parameter self :: <GtkEntry>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_get_icon_at_pos";
end;

define C-function gtk-entry-get-icon-gicon
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <GIcon>;
  c-name: "gtk_entry_get_icon_gicon";
end;

define C-function gtk-entry-get-icon-name
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_icon_name";
end;

define C-function gtk-entry-get-icon-pixbuf
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_entry_get_icon_pixbuf";
end;

define C-function gtk-entry-get-icon-sensitive
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_icon_sensitive";
end;

define C-function gtk-entry-get-icon-stock
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_icon_stock";
end;

define C-function gtk-entry-get-icon-storage-type
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <GtkImageType>;
  c-name: "gtk_entry_get_icon_storage_type";
end;

define C-function gtk-entry-get-icon-tooltip-markup
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_icon_tooltip_markup";
end;

define C-function gtk-entry-get-icon-tooltip-text
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_icon_tooltip_text";
end;

define C-function gtk-entry-get-inner-border
  input parameter self :: <GtkEntry>;
  result res :: <GtkBorder>;
  c-name: "gtk_entry_get_inner_border";
end;

define C-function gtk-entry-get-invisible-char
  input parameter self :: <GtkEntry>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_entry_get_invisible_char";
end;

define C-function gtk-entry-get-layout
  input parameter self :: <GtkEntry>;
  result res :: <PangoLayout>;
  c-name: "gtk_entry_get_layout";
end;

define C-function gtk-entry-get-layout-offsets
  input parameter self :: <GtkEntry>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gtk_entry_get_layout_offsets";
end;

define C-function gtk-entry-get-max-length
  input parameter self :: <GtkEntry>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_get_max_length";
end;

define C-function gtk-entry-get-overwrite-mode
  input parameter self :: <GtkEntry>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_overwrite_mode";
end;

define C-function gtk-entry-get-placeholder-text
  input parameter self :: <GtkEntry>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_placeholder_text";
end;

define C-function gtk-entry-get-progress-fraction
  input parameter self :: <GtkEntry>;
  result res :: <C-double>;
  c-name: "gtk_entry_get_progress_fraction";
end;

define C-function gtk-entry-get-progress-pulse-step
  input parameter self :: <GtkEntry>;
  result res :: <C-double>;
  c-name: "gtk_entry_get_progress_pulse_step";
end;

define C-function gtk-entry-get-text
  input parameter self :: <GtkEntry>;
  result res :: <C-string>;
  c-name: "gtk_entry_get_text";
end;

define C-function gtk-entry-get-text-area
  input parameter self :: <GtkEntry>;
  output parameter text_area_ :: <cairoRectangleInt>;
  c-name: "gtk_entry_get_text_area";
end;

define C-function gtk-entry-get-text-length
  input parameter self :: <GtkEntry>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_entry_get_text_length";
end;

define C-function gtk-entry-get-visibility
  input parameter self :: <GtkEntry>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_get_visibility";
end;

define C-function gtk-entry-get-width-chars
  input parameter self :: <GtkEntry>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_get_width_chars";
end;

define C-function gtk-entry-im-context-filter-keypress
  input parameter self :: <GtkEntry>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_im_context_filter_keypress";
end;

define C-function gtk-entry-layout-index-to-text-index
  input parameter self :: <GtkEntry>;
  input parameter layout_index_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_layout_index_to_text_index";
end;

define C-function gtk-entry-progress-pulse
  input parameter self :: <GtkEntry>;
  c-name: "gtk_entry_progress_pulse";
end;

define C-function gtk-entry-reset-im-context
  input parameter self :: <GtkEntry>;
  c-name: "gtk_entry_reset_im_context";
end;

define C-function gtk-entry-set-activates-default
  input parameter self :: <GtkEntry>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_entry_set_activates_default";
end;

define C-function gtk-entry-set-alignment
  input parameter self :: <GtkEntry>;
  input parameter xalign_ :: <C-float>;
  c-name: "gtk_entry_set_alignment";
end;

define C-function gtk-entry-set-buffer
  input parameter self :: <GtkEntry>;
  input parameter buffer_ :: <GtkEntryBuffer>;
  c-name: "gtk_entry_set_buffer";
end;

define C-function gtk-entry-set-completion
  input parameter self :: <GtkEntry>;
  input parameter completion_ :: <GtkEntryCompletion>;
  c-name: "gtk_entry_set_completion";
end;

define C-function gtk-entry-set-cursor-hadjustment
  input parameter self :: <GtkEntry>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_entry_set_cursor_hadjustment";
end;

define C-function gtk-entry-set-has-frame
  input parameter self :: <GtkEntry>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_entry_set_has_frame";
end;

define C-function gtk-entry-set-icon-activatable
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter activatable_ :: <C-boolean>;
  c-name: "gtk_entry_set_icon_activatable";
end;

define C-function gtk-entry-set-icon-drag-source
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter target_list_ :: <GtkTargetList>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_entry_set_icon_drag_source";
end;

define C-function gtk-entry-set-icon-from-gicon
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_entry_set_icon_from_gicon";
end;

define C-function gtk-entry-set-icon-from-icon-name
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_entry_set_icon_from_icon_name";
end;

define C-function gtk-entry-set-icon-from-pixbuf
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_entry_set_icon_from_pixbuf";
end;

define C-function gtk-entry-set-icon-from-stock
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter stock_id_ :: <C-string>;
  c-name: "gtk_entry_set_icon_from_stock";
end;

define C-function gtk-entry-set-icon-sensitive
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter sensitive_ :: <C-boolean>;
  c-name: "gtk_entry_set_icon_sensitive";
end;

define C-function gtk-entry-set-icon-tooltip-markup
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter tooltip_ :: <C-string>;
  c-name: "gtk_entry_set_icon_tooltip_markup";
end;

define C-function gtk-entry-set-icon-tooltip-text
  input parameter self :: <GtkEntry>;
  input parameter icon_pos_ :: <GtkEntryIconPosition>;
  input parameter tooltip_ :: <C-string>;
  c-name: "gtk_entry_set_icon_tooltip_text";
end;

define C-function gtk-entry-set-inner-border
  input parameter self :: <GtkEntry>;
  input parameter border_ :: <GtkBorder>;
  c-name: "gtk_entry_set_inner_border";
end;

define C-function gtk-entry-set-invisible-char
  input parameter self :: <GtkEntry>;
  input parameter ch_ :: <C-unsigned-int>;
  c-name: "gtk_entry_set_invisible_char";
end;

define C-function gtk-entry-set-max-length
  input parameter self :: <GtkEntry>;
  input parameter max_ :: <C-signed-int>;
  c-name: "gtk_entry_set_max_length";
end;

define C-function gtk-entry-set-overwrite-mode
  input parameter self :: <GtkEntry>;
  input parameter overwrite_ :: <C-boolean>;
  c-name: "gtk_entry_set_overwrite_mode";
end;

define C-function gtk-entry-set-placeholder-text
  input parameter self :: <GtkEntry>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_entry_set_placeholder_text";
end;

define C-function gtk-entry-set-progress-fraction
  input parameter self :: <GtkEntry>;
  input parameter fraction_ :: <C-double>;
  c-name: "gtk_entry_set_progress_fraction";
end;

define C-function gtk-entry-set-progress-pulse-step
  input parameter self :: <GtkEntry>;
  input parameter fraction_ :: <C-double>;
  c-name: "gtk_entry_set_progress_pulse_step";
end;

define C-function gtk-entry-set-text
  input parameter self :: <GtkEntry>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_entry_set_text";
end;

define C-function gtk-entry-set-visibility
  input parameter self :: <GtkEntry>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_entry_set_visibility";
end;

define C-function gtk-entry-set-width-chars
  input parameter self :: <GtkEntry>;
  input parameter n_chars_ :: <C-signed-int>;
  c-name: "gtk_entry_set_width_chars";
end;

define C-function gtk-entry-text-index-to-layout-index
  input parameter self :: <GtkEntry>;
  input parameter text_index_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_text_index_to_layout_index";
end;

define C-function gtk-entry-unset-invisible-char
  input parameter self :: <GtkEntry>;
  c-name: "gtk_entry_unset_invisible_char";
end;

define open C-subtype <GtkEntryBuffer> (<GObject>)
  constant slot gtk-entry-buffer-parent-instance :: <GObject>;
  constant slot gtk-entry-buffer-priv :: <GtkEntryBufferPrivate>;
end C-subtype;

define C-pointer-type <GtkEntryBuffer*> => <GtkEntryBuffer>;

define C-function gtk-entry-buffer-new
  input parameter initial_chars_ :: <C-string>;
  input parameter n_initial_chars_ :: <C-signed-int>;
  result res :: <GtkEntryBuffer>;
  c-name: "gtk_entry_buffer_new";
end;

define C-function gtk-entry-buffer-delete-text
  input parameter self :: <GtkEntryBuffer>;
  input parameter position_ :: <C-unsigned-int>;
  input parameter n_chars_ :: <C-signed-int>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_entry_buffer_delete_text";
end;

define C-function gtk-entry-buffer-emit-deleted-text
  input parameter self :: <GtkEntryBuffer>;
  input parameter position_ :: <C-unsigned-int>;
  input parameter n_chars_ :: <C-unsigned-int>;
  c-name: "gtk_entry_buffer_emit_deleted_text";
end;

define C-function gtk-entry-buffer-emit-inserted-text
  input parameter self :: <GtkEntryBuffer>;
  input parameter position_ :: <C-unsigned-int>;
  input parameter chars_ :: <C-string>;
  input parameter n_chars_ :: <C-unsigned-int>;
  c-name: "gtk_entry_buffer_emit_inserted_text";
end;

define C-function gtk-entry-buffer-get-bytes
  input parameter self :: <GtkEntryBuffer>;
  result res :: <C-unsigned-long>;
  c-name: "gtk_entry_buffer_get_bytes";
end;

define C-function gtk-entry-buffer-get-length
  input parameter self :: <GtkEntryBuffer>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_entry_buffer_get_length";
end;

define C-function gtk-entry-buffer-get-max-length
  input parameter self :: <GtkEntryBuffer>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_buffer_get_max_length";
end;

define C-function gtk-entry-buffer-get-text
  input parameter self :: <GtkEntryBuffer>;
  result res :: <C-string>;
  c-name: "gtk_entry_buffer_get_text";
end;

define C-function gtk-entry-buffer-insert-text
  input parameter self :: <GtkEntryBuffer>;
  input parameter position_ :: <C-unsigned-int>;
  input parameter chars_ :: <C-string>;
  input parameter n_chars_ :: <C-signed-int>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_entry_buffer_insert_text";
end;

define C-function gtk-entry-buffer-set-max-length
  input parameter self :: <GtkEntryBuffer>;
  input parameter max_length_ :: <C-signed-int>;
  c-name: "gtk_entry_buffer_set_max_length";
end;

define C-function gtk-entry-buffer-set-text
  input parameter self :: <GtkEntryBuffer>;
  input parameter chars_ :: <C-string>;
  input parameter n_chars_ :: <C-signed-int>;
  c-name: "gtk_entry_buffer_set_text";
end;

define C-struct <_GtkEntryBufferClass>
  constant slot gtk-entry-buffer-class-parent-class :: <GObjectClass>;
  constant slot gtk-entry-buffer-class-inserted-text :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-deleted-text :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-get-text :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-get-length :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-insert-text :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-delete-text :: <C-function-pointer>;
  constant slot gtk-entry-buffer-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-entry-buffer-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkEntryBufferClass>;
end C-struct;

define C-struct <_GtkEntryBufferPrivate>
  pointer-type-name: <GtkEntryBufferPrivate>;
end C-struct;

define C-struct <_GtkEntryClass>
  constant slot gtk-entry-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-entry-class-populate-popup :: <C-function-pointer>;
  constant slot gtk-entry-class-activate :: <C-function-pointer>;
  constant slot gtk-entry-class-move-cursor :: <C-function-pointer>;
  constant slot gtk-entry-class-insert-at-cursor :: <C-function-pointer>;
  constant slot gtk-entry-class-delete-from-cursor :: <C-function-pointer>;
  constant slot gtk-entry-class-backspace :: <C-function-pointer>;
  constant slot gtk-entry-class-cut-clipboard :: <C-function-pointer>;
  constant slot gtk-entry-class-copy-clipboard :: <C-function-pointer>;
  constant slot gtk-entry-class-paste-clipboard :: <C-function-pointer>;
  constant slot gtk-entry-class-toggle-overwrite :: <C-function-pointer>;
  constant slot gtk-entry-class-get-text-area-size :: <C-function-pointer>;
  constant slot gtk-entry-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-entry-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkEntryClass>;
end C-struct;

define open C-subtype <GtkEntryCompletion> (<GObject>)
  constant slot gtk-entry-completion-parent-instance :: <GObject>;
  constant slot gtk-entry-completion-priv :: <GtkEntryCompletionPrivate>;
end C-subtype;

define C-pointer-type <GtkEntryCompletion*> => <GtkEntryCompletion>;

define C-function gtk-entry-completion-new
  result res :: <GtkEntryCompletion>;
  c-name: "gtk_entry_completion_new";
end;

define C-function gtk-entry-completion-new-with-area
  input parameter area_ :: <GtkCellArea>;
  result res :: <GtkEntryCompletion>;
  c-name: "gtk_entry_completion_new_with_area";
end;

define C-function gtk-entry-completion-complete
  input parameter self :: <GtkEntryCompletion>;
  c-name: "gtk_entry_completion_complete";
end;

define C-function gtk-entry-completion-compute-prefix
  input parameter self :: <GtkEntryCompletion>;
  input parameter key_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gtk_entry_completion_compute_prefix";
end;

define C-function gtk-entry-completion-delete-action
  input parameter self :: <GtkEntryCompletion>;
  input parameter index__ :: <C-signed-int>;
  c-name: "gtk_entry_completion_delete_action";
end;

define C-function gtk-entry-completion-get-completion-prefix
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-string>;
  c-name: "gtk_entry_completion_get_completion_prefix";
end;

define C-function gtk-entry-completion-get-entry
  input parameter self :: <GtkEntryCompletion>;
  result res :: <GtkWidget>;
  c-name: "gtk_entry_completion_get_entry";
end;

define C-function gtk-entry-completion-get-inline-completion
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_completion_get_inline_completion";
end;

define C-function gtk-entry-completion-get-inline-selection
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_completion_get_inline_selection";
end;

define C-function gtk-entry-completion-get-minimum-key-length
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_completion_get_minimum_key_length";
end;

define C-function gtk-entry-completion-get-model
  input parameter self :: <GtkEntryCompletion>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_entry_completion_get_model";
end;

define C-function gtk-entry-completion-get-popup-completion
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_completion_get_popup_completion";
end;

define C-function gtk-entry-completion-get-popup-set-width
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_completion_get_popup_set_width";
end;

define C-function gtk-entry-completion-get-popup-single-match
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-boolean>;
  c-name: "gtk_entry_completion_get_popup_single_match";
end;

define C-function gtk-entry-completion-get-text-column
  input parameter self :: <GtkEntryCompletion>;
  result res :: <C-signed-int>;
  c-name: "gtk_entry_completion_get_text_column";
end;

define C-function gtk-entry-completion-insert-action-markup
  input parameter self :: <GtkEntryCompletion>;
  input parameter index__ :: <C-signed-int>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_entry_completion_insert_action_markup";
end;

define C-function gtk-entry-completion-insert-action-text
  input parameter self :: <GtkEntryCompletion>;
  input parameter index__ :: <C-signed-int>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_entry_completion_insert_action_text";
end;

define C-function gtk-entry-completion-insert-prefix
  input parameter self :: <GtkEntryCompletion>;
  c-name: "gtk_entry_completion_insert_prefix";
end;

define C-function gtk-entry-completion-set-inline-completion
  input parameter self :: <GtkEntryCompletion>;
  input parameter inline_completion_ :: <C-boolean>;
  c-name: "gtk_entry_completion_set_inline_completion";
end;

define C-function gtk-entry-completion-set-inline-selection
  input parameter self :: <GtkEntryCompletion>;
  input parameter inline_selection_ :: <C-boolean>;
  c-name: "gtk_entry_completion_set_inline_selection";
end;

define C-function gtk-entry-completion-set-match-func
  input parameter self :: <GtkEntryCompletion>;
  input parameter func_ :: <C-function-pointer>;
  input parameter func_data_ :: <C-void*>;
  input parameter func_notify_ :: <C-function-pointer>;
  c-name: "gtk_entry_completion_set_match_func";
end;

define C-function gtk-entry-completion-set-minimum-key-length
  input parameter self :: <GtkEntryCompletion>;
  input parameter length_ :: <C-signed-int>;
  c-name: "gtk_entry_completion_set_minimum_key_length";
end;

define C-function gtk-entry-completion-set-model
  input parameter self :: <GtkEntryCompletion>;
  input parameter model_ :: <GtkTreeModel>;
  c-name: "gtk_entry_completion_set_model";
end;

define C-function gtk-entry-completion-set-popup-completion
  input parameter self :: <GtkEntryCompletion>;
  input parameter popup_completion_ :: <C-boolean>;
  c-name: "gtk_entry_completion_set_popup_completion";
end;

define C-function gtk-entry-completion-set-popup-set-width
  input parameter self :: <GtkEntryCompletion>;
  input parameter popup_set_width_ :: <C-boolean>;
  c-name: "gtk_entry_completion_set_popup_set_width";
end;

define C-function gtk-entry-completion-set-popup-single-match
  input parameter self :: <GtkEntryCompletion>;
  input parameter popup_single_match_ :: <C-boolean>;
  c-name: "gtk_entry_completion_set_popup_single_match";
end;

define C-function gtk-entry-completion-set-text-column
  input parameter self :: <GtkEntryCompletion>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_entry_completion_set_text_column";
end;

define C-struct <_GtkEntryCompletionClass>
  constant slot gtk-entry-completion-class-parent-class :: <GObjectClass>;
  constant slot gtk-entry-completion-class-match-selected :: <C-function-pointer>;
  constant slot gtk-entry-completion-class-action-activated :: <C-function-pointer>;
  constant slot gtk-entry-completion-class-insert-prefix :: <C-function-pointer>;
  constant slot gtk-entry-completion-class-cursor-on-match :: <C-function-pointer>;
  constant slot gtk-entry-completion-class-_gtk-reserved0 :: <C-void*>;
  constant slot gtk-entry-completion-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-entry-completion-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-entry-completion-class-_gtk-reserved3 :: <C-void*>;
  pointer-type-name: <GtkEntryCompletionClass>;
end C-struct;

define C-struct <_GtkEntryCompletionPrivate>
  pointer-type-name: <GtkEntryCompletionPrivate>;
end C-struct;

define constant $gtk-entry-icon-primary = 0;
define constant $gtk-entry-icon-secondary = 1;
define constant <GtkEntryIconPosition> = <C-int>;
define C-pointer-type <GtkEntryIconPosition*> => <GtkEntryIconPosition>;

define C-struct <_GtkEntryPrivate>
  pointer-type-name: <GtkEntryPrivate>;
end C-struct;

define open C-subtype <GtkEventBox> (<GtkBin>)
  constant slot gtk-event-box-bin :: <GtkBin>;
  constant slot gtk-event-box-priv :: <GtkEventBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkEventBox*> => <GtkEventBox>;

define C-function gtk-event-box-new
  result res :: <GtkWidget>;
  c-name: "gtk_event_box_new";
end;

define C-function gtk-event-box-get-above-child
  input parameter self :: <GtkEventBox>;
  result res :: <C-boolean>;
  c-name: "gtk_event_box_get_above_child";
end;

define C-function gtk-event-box-get-visible-window
  input parameter self :: <GtkEventBox>;
  result res :: <C-boolean>;
  c-name: "gtk_event_box_get_visible_window";
end;

define C-function gtk-event-box-set-above-child
  input parameter self :: <GtkEventBox>;
  input parameter above_child_ :: <C-boolean>;
  c-name: "gtk_event_box_set_above_child";
end;

define C-function gtk-event-box-set-visible-window
  input parameter self :: <GtkEventBox>;
  input parameter visible_window_ :: <C-boolean>;
  c-name: "gtk_event_box_set_visible_window";
end;

define C-struct <_GtkEventBoxClass>
  constant slot gtk-event-box-class-parent-class :: <GtkBinClass>;
  constant slot gtk-event-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-event-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-event-box-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-event-box-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkEventBoxClass>;
end C-struct;

define C-struct <_GtkEventBoxPrivate>
  pointer-type-name: <GtkEventBoxPrivate>;
end C-struct;

define open C-subtype <GtkExpander> (<GtkBin>)
  constant slot gtk-expander-bin :: <GtkBin>;
  constant slot gtk-expander-priv :: <GtkExpanderPrivate>;
end C-subtype;

define C-pointer-type <GtkExpander*> => <GtkExpander>;

define C-function gtk-expander-new
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_expander_new";
end;

define C-function gtk-expander-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_expander_new_with_mnemonic";
end;

define C-function gtk-expander-get-expanded
  input parameter self :: <GtkExpander>;
  result res :: <C-boolean>;
  c-name: "gtk_expander_get_expanded";
end;

define C-function gtk-expander-get-label
  input parameter self :: <GtkExpander>;
  result res :: <C-string>;
  c-name: "gtk_expander_get_label";
end;

define C-function gtk-expander-get-label-fill
  input parameter self :: <GtkExpander>;
  result res :: <C-boolean>;
  c-name: "gtk_expander_get_label_fill";
end;

define C-function gtk-expander-get-label-widget
  input parameter self :: <GtkExpander>;
  result res :: <GtkWidget>;
  c-name: "gtk_expander_get_label_widget";
end;

define C-function gtk-expander-get-resize-toplevel
  input parameter self :: <GtkExpander>;
  result res :: <C-boolean>;
  c-name: "gtk_expander_get_resize_toplevel";
end;

define C-function gtk-expander-get-spacing
  input parameter self :: <GtkExpander>;
  result res :: <C-signed-int>;
  c-name: "gtk_expander_get_spacing";
end;

define C-function gtk-expander-get-use-markup
  input parameter self :: <GtkExpander>;
  result res :: <C-boolean>;
  c-name: "gtk_expander_get_use_markup";
end;

define C-function gtk-expander-get-use-underline
  input parameter self :: <GtkExpander>;
  result res :: <C-boolean>;
  c-name: "gtk_expander_get_use_underline";
end;

define C-function gtk-expander-set-expanded
  input parameter self :: <GtkExpander>;
  input parameter expanded_ :: <C-boolean>;
  c-name: "gtk_expander_set_expanded";
end;

define C-function gtk-expander-set-label
  input parameter self :: <GtkExpander>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_expander_set_label";
end;

define C-function gtk-expander-set-label-fill
  input parameter self :: <GtkExpander>;
  input parameter label_fill_ :: <C-boolean>;
  c-name: "gtk_expander_set_label_fill";
end;

define C-function gtk-expander-set-label-widget
  input parameter self :: <GtkExpander>;
  input parameter label_widget_ :: <GtkWidget>;
  c-name: "gtk_expander_set_label_widget";
end;

define C-function gtk-expander-set-resize-toplevel
  input parameter self :: <GtkExpander>;
  input parameter resize_toplevel_ :: <C-boolean>;
  c-name: "gtk_expander_set_resize_toplevel";
end;

define C-function gtk-expander-set-spacing
  input parameter self :: <GtkExpander>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "gtk_expander_set_spacing";
end;

define C-function gtk-expander-set-use-markup
  input parameter self :: <GtkExpander>;
  input parameter use_markup_ :: <C-boolean>;
  c-name: "gtk_expander_set_use_markup";
end;

define C-function gtk-expander-set-use-underline
  input parameter self :: <GtkExpander>;
  input parameter use_underline_ :: <C-boolean>;
  c-name: "gtk_expander_set_use_underline";
end;

define C-struct <_GtkExpanderClass>
  constant slot gtk-expander-class-parent-class :: <GtkBinClass>;
  constant slot gtk-expander-class-activate :: <C-function-pointer>;
  constant slot gtk-expander-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-expander-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-expander-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-expander-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkExpanderClass>;
end C-struct;

define C-struct <_GtkExpanderPrivate>
  pointer-type-name: <GtkExpanderPrivate>;
end C-struct;

define constant $gtk-expander-collapsed = 0;
define constant $gtk-expander-semi-collapsed = 1;
define constant $gtk-expander-semi-expanded = 2;
define constant $gtk-expander-expanded = 3;
define constant <GtkExpanderStyle> = <C-int>;
define C-pointer-type <GtkExpanderStyle*> => <GtkExpanderStyle>;

// Interface
define open C-subtype <GtkFileChooser> (<GtkWidget>)
end C-subtype;

define C-pointer-type <GtkFileChooser*> => <GtkFileChooser>;

define C-function gtk-file-chooser-add-filter
  input parameter self :: <GtkFileChooser>;
  input parameter filter_ :: <GtkFileFilter>;
  c-name: "gtk_file_chooser_add_filter";
end;

define C-function gtk-file-chooser-add-shortcut-folder
  input parameter self :: <GtkFileChooser>;
  input parameter folder_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_add_shortcut_folder";
end;

define C-function gtk-file-chooser-add-shortcut-folder-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_add_shortcut_folder_uri";
end;

define C-function gtk-file-chooser-get-action
  input parameter self :: <GtkFileChooser>;
  result res :: <GtkFileChooserAction>;
  c-name: "gtk_file_chooser_get_action";
end;

define C-function gtk-file-chooser-get-create-folders
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_create_folders";
end;

define C-function gtk-file-chooser-get-current-folder
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_current_folder";
end;

define C-function gtk-file-chooser-get-current-folder-file
  input parameter self :: <GtkFileChooser>;
  result res :: <GFile>;
  c-name: "gtk_file_chooser_get_current_folder_file";
end;

define C-function gtk-file-chooser-get-current-folder-uri
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_current_folder_uri";
end;

define C-function gtk-file-chooser-get-do-overwrite-confirmation
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_do_overwrite_confirmation";
end;

define C-function gtk-file-chooser-get-extra-widget
  input parameter self :: <GtkFileChooser>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_get_extra_widget";
end;

define C-function gtk-file-chooser-get-file
  input parameter self :: <GtkFileChooser>;
  result res :: <GFile>;
  c-name: "gtk_file_chooser_get_file";
end;

define C-function gtk-file-chooser-get-filename
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_filename";
end;

define C-function gtk-file-chooser-get-filenames
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_get_filenames";
end;

define C-function gtk-file-chooser-get-files
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_get_files";
end;

define C-function gtk-file-chooser-get-filter
  input parameter self :: <GtkFileChooser>;
  result res :: <GtkFileFilter>;
  c-name: "gtk_file_chooser_get_filter";
end;

define C-function gtk-file-chooser-get-local-only
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_local_only";
end;

define C-function gtk-file-chooser-get-preview-file
  input parameter self :: <GtkFileChooser>;
  result res :: <GFile>;
  c-name: "gtk_file_chooser_get_preview_file";
end;

define C-function gtk-file-chooser-get-preview-filename
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_preview_filename";
end;

define C-function gtk-file-chooser-get-preview-uri
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_preview_uri";
end;

define C-function gtk-file-chooser-get-preview-widget
  input parameter self :: <GtkFileChooser>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_get_preview_widget";
end;

define C-function gtk-file-chooser-get-preview-widget-active
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_preview_widget_active";
end;

define C-function gtk-file-chooser-get-select-multiple
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_select_multiple";
end;

define C-function gtk-file-chooser-get-show-hidden
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_show_hidden";
end;

define C-function gtk-file-chooser-get-uri
  input parameter self :: <GtkFileChooser>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_get_uri";
end;

define C-function gtk-file-chooser-get-uris
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_get_uris";
end;

define C-function gtk-file-chooser-get-use-preview-label
  input parameter self :: <GtkFileChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_get_use_preview_label";
end;

define C-function gtk-file-chooser-list-filters
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_list_filters";
end;

define C-function gtk-file-chooser-list-shortcut-folder-uris
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_list_shortcut_folder_uris";
end;

define C-function gtk-file-chooser-list-shortcut-folders
  input parameter self :: <GtkFileChooser>;
  result res :: <GSList>;
  c-name: "gtk_file_chooser_list_shortcut_folders";
end;

define C-function gtk-file-chooser-remove-filter
  input parameter self :: <GtkFileChooser>;
  input parameter filter_ :: <GtkFileFilter>;
  c-name: "gtk_file_chooser_remove_filter";
end;

define C-function gtk-file-chooser-remove-shortcut-folder
  input parameter self :: <GtkFileChooser>;
  input parameter folder_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_remove_shortcut_folder";
end;

define C-function gtk-file-chooser-remove-shortcut-folder-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_remove_shortcut_folder_uri";
end;

define C-function gtk-file-chooser-select-all
  input parameter self :: <GtkFileChooser>;
  c-name: "gtk_file_chooser_select_all";
end;

define C-function gtk-file-chooser-select-file
  input parameter self :: <GtkFileChooser>;
  input parameter file_ :: <GFile>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_select_file";
end;

define C-function gtk-file-chooser-select-filename
  input parameter self :: <GtkFileChooser>;
  input parameter filename_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_select_filename";
end;

define C-function gtk-file-chooser-select-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_select_uri";
end;

define C-function gtk-file-chooser-set-action
  input parameter self :: <GtkFileChooser>;
  input parameter action_ :: <GtkFileChooserAction>;
  c-name: "gtk_file_chooser_set_action";
end;

define C-function gtk-file-chooser-set-create-folders
  input parameter self :: <GtkFileChooser>;
  input parameter create_folders_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_create_folders";
end;

define C-function gtk-file-chooser-set-current-folder
  input parameter self :: <GtkFileChooser>;
  input parameter filename_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_current_folder";
end;

define C-function gtk-file-chooser-set-current-folder-file
  input parameter self :: <GtkFileChooser>;
  input parameter file_ :: <GFile>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_current_folder_file";
end;

define C-function gtk-file-chooser-set-current-folder-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_current_folder_uri";
end;

define C-function gtk-file-chooser-set-current-name
  input parameter self :: <GtkFileChooser>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_file_chooser_set_current_name";
end;

define C-function gtk-file-chooser-set-do-overwrite-confirmation
  input parameter self :: <GtkFileChooser>;
  input parameter do_overwrite_confirmation_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_do_overwrite_confirmation";
end;

define C-function gtk-file-chooser-set-extra-widget
  input parameter self :: <GtkFileChooser>;
  input parameter extra_widget_ :: <GtkWidget>;
  c-name: "gtk_file_chooser_set_extra_widget";
end;

define C-function gtk-file-chooser-set-file
  input parameter self :: <GtkFileChooser>;
  input parameter file_ :: <GFile>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_file";
end;

define C-function gtk-file-chooser-set-filename
  input parameter self :: <GtkFileChooser>;
  input parameter filename_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_filename";
end;

define C-function gtk-file-chooser-set-filter
  input parameter self :: <GtkFileChooser>;
  input parameter filter_ :: <GtkFileFilter>;
  c-name: "gtk_file_chooser_set_filter";
end;

define C-function gtk-file-chooser-set-local-only
  input parameter self :: <GtkFileChooser>;
  input parameter local_only_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_local_only";
end;

define C-function gtk-file-chooser-set-preview-widget
  input parameter self :: <GtkFileChooser>;
  input parameter preview_widget_ :: <GtkWidget>;
  c-name: "gtk_file_chooser_set_preview_widget";
end;

define C-function gtk-file-chooser-set-preview-widget-active
  input parameter self :: <GtkFileChooser>;
  input parameter active_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_preview_widget_active";
end;

define C-function gtk-file-chooser-set-select-multiple
  input parameter self :: <GtkFileChooser>;
  input parameter select_multiple_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_select_multiple";
end;

define C-function gtk-file-chooser-set-show-hidden
  input parameter self :: <GtkFileChooser>;
  input parameter show_hidden_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_show_hidden";
end;

define C-function gtk-file-chooser-set-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_set_uri";
end;

define C-function gtk-file-chooser-set-use-preview-label
  input parameter self :: <GtkFileChooser>;
  input parameter use_label_ :: <C-boolean>;
  c-name: "gtk_file_chooser_set_use_preview_label";
end;

define C-function gtk-file-chooser-unselect-all
  input parameter self :: <GtkFileChooser>;
  c-name: "gtk_file_chooser_unselect_all";
end;

define C-function gtk-file-chooser-unselect-file
  input parameter self :: <GtkFileChooser>;
  input parameter file_ :: <GFile>;
  c-name: "gtk_file_chooser_unselect_file";
end;

define C-function gtk-file-chooser-unselect-filename
  input parameter self :: <GtkFileChooser>;
  input parameter filename_ :: <C-string>;
  c-name: "gtk_file_chooser_unselect_filename";
end;

define C-function gtk-file-chooser-unselect-uri
  input parameter self :: <GtkFileChooser>;
  input parameter uri_ :: <C-string>;
  c-name: "gtk_file_chooser_unselect_uri";
end;

define constant $gtk-file-chooser-action-open = 0;
define constant $gtk-file-chooser-action-save = 1;
define constant $gtk-file-chooser-action-select-folder = 2;
define constant $gtk-file-chooser-action-create-folder = 3;
define constant <GtkFileChooserAction> = <C-int>;
define C-pointer-type <GtkFileChooserAction*> => <GtkFileChooserAction>;

define open C-subtype <GtkFileChooserButton> (<GtkBox>)
  constant slot gtk-file-chooser-button-parent :: <GtkBox>;
  constant slot gtk-file-chooser-button-priv :: <GtkFileChooserButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkFileChooserButton*> => <GtkFileChooserButton>;

define C-function gtk-file-chooser-button-new
  input parameter title_ :: <C-string>;
  input parameter action_ :: <GtkFileChooserAction>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_button_new";
end;

define C-function gtk-file-chooser-button-new-with-dialog
  input parameter dialog_ :: <GtkWidget>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_button_new_with_dialog";
end;

define C-function gtk-file-chooser-button-get-focus-on-click
  input parameter self :: <GtkFileChooserButton>;
  result res :: <C-boolean>;
  c-name: "gtk_file_chooser_button_get_focus_on_click";
end;

define C-function gtk-file-chooser-button-get-title
  input parameter self :: <GtkFileChooserButton>;
  result res :: <C-string>;
  c-name: "gtk_file_chooser_button_get_title";
end;

define C-function gtk-file-chooser-button-get-width-chars
  input parameter self :: <GtkFileChooserButton>;
  result res :: <C-signed-int>;
  c-name: "gtk_file_chooser_button_get_width_chars";
end;

define C-function gtk-file-chooser-button-set-focus-on-click
  input parameter self :: <GtkFileChooserButton>;
  input parameter focus_on_click_ :: <C-boolean>;
  c-name: "gtk_file_chooser_button_set_focus_on_click";
end;

define C-function gtk-file-chooser-button-set-title
  input parameter self :: <GtkFileChooserButton>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_file_chooser_button_set_title";
end;

define C-function gtk-file-chooser-button-set-width-chars
  input parameter self :: <GtkFileChooserButton>;
  input parameter n_chars_ :: <C-signed-int>;
  c-name: "gtk_file_chooser_button_set_width_chars";
end;

define C-struct <_GtkFileChooserButtonClass>
  constant slot gtk-file-chooser-button-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-file-chooser-button-class-file-set :: <C-function-pointer>;
  constant slot gtk-file-chooser-button-class-__gtk-reserved1 :: <C-void*>;
  constant slot gtk-file-chooser-button-class-__gtk-reserved2 :: <C-void*>;
  constant slot gtk-file-chooser-button-class-__gtk-reserved3 :: <C-void*>;
  constant slot gtk-file-chooser-button-class-__gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFileChooserButtonClass>;
end C-struct;

define C-struct <_GtkFileChooserButtonPrivate>
  pointer-type-name: <GtkFileChooserButtonPrivate>;
end C-struct;

define constant $gtk-file-chooser-confirmation-confirm = 0;
define constant $gtk-file-chooser-confirmation-accept-filename = 1;
define constant $gtk-file-chooser-confirmation-select-again = 2;
define constant <GtkFileChooserConfirmation> = <C-int>;
define C-pointer-type <GtkFileChooserConfirmation*> => <GtkFileChooserConfirmation>;

define open C-subtype <GtkFileChooserDialog> (<GtkDialog>)
  constant slot gtk-file-chooser-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-file-chooser-dialog-priv :: <GtkFileChooserDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkFileChooserDialog*> => <GtkFileChooserDialog>;

define C-struct <_GtkFileChooserDialogClass>
  constant slot gtk-file-chooser-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-file-chooser-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-file-chooser-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-file-chooser-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-file-chooser-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFileChooserDialogClass>;
end C-struct;

define C-struct <_GtkFileChooserDialogPrivate>
  pointer-type-name: <GtkFileChooserDialogPrivate>;
end C-struct;

define constant $gtk-file-chooser-error-nonexistent = 0;
define constant $gtk-file-chooser-error-bad-filename = 1;
define constant $gtk-file-chooser-error-already-exists = 2;
define constant $gtk-file-chooser-error-incomplete-hostname = 3;
define constant <GtkFileChooserError> = <C-int>;
define C-pointer-type <GtkFileChooserError*> => <GtkFileChooserError>;

define open C-subtype <GtkFileChooserWidget> (<GtkBox>)
  constant slot gtk-file-chooser-widget-parent-instance :: <GtkBox>;
  constant slot gtk-file-chooser-widget-priv :: <GtkFileChooserWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkFileChooserWidget*> => <GtkFileChooserWidget>;

define C-function gtk-file-chooser-widget-new
  input parameter action_ :: <GtkFileChooserAction>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_widget_new";
end;

define C-struct <_GtkFileChooserWidgetClass>
  constant slot gtk-file-chooser-widget-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-file-chooser-widget-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-file-chooser-widget-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-file-chooser-widget-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-file-chooser-widget-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFileChooserWidgetClass>;
end C-struct;

define C-struct <_GtkFileChooserWidgetPrivate>
  pointer-type-name: <GtkFileChooserWidgetPrivate>;
end C-struct;

define open C-subtype <GtkFileFilter> (<GInitiallyUnowned>)
end C-subtype;

define C-pointer-type <GtkFileFilter*> => <GtkFileFilter>;

define C-function gtk-file-filter-new
  result res :: <GtkFileFilter>;
  c-name: "gtk_file_filter_new";
end;

define C-function gtk-file-filter-add-custom
  input parameter self :: <GtkFileFilter>;
  input parameter needed_ :: <GtkFileFilterFlags>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "gtk_file_filter_add_custom";
end;

define C-function gtk-file-filter-add-mime-type
  input parameter self :: <GtkFileFilter>;
  input parameter mime_type_ :: <C-string>;
  c-name: "gtk_file_filter_add_mime_type";
end;

define C-function gtk-file-filter-add-pattern
  input parameter self :: <GtkFileFilter>;
  input parameter pattern_ :: <C-string>;
  c-name: "gtk_file_filter_add_pattern";
end;

define C-function gtk-file-filter-add-pixbuf-formats
  input parameter self :: <GtkFileFilter>;
  c-name: "gtk_file_filter_add_pixbuf_formats";
end;

define C-function gtk-file-filter-filter
  input parameter self :: <GtkFileFilter>;
  input parameter filter_info_ :: <GtkFileFilterInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_file_filter_filter";
end;

define C-function gtk-file-filter-get-name
  input parameter self :: <GtkFileFilter>;
  result res :: <C-string>;
  c-name: "gtk_file_filter_get_name";
end;

define C-function gtk-file-filter-get-needed
  input parameter self :: <GtkFileFilter>;
  result res :: <GtkFileFilterFlags>;
  c-name: "gtk_file_filter_get_needed";
end;

define C-function gtk-file-filter-set-name
  input parameter self :: <GtkFileFilter>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_file_filter_set_name";
end;

define constant $gtk-file-filter-filename = 1;
define constant $gtk-file-filter-uri = 2;
define constant $gtk-file-filter-display-name = 4;
define constant $gtk-file-filter-mime-type = 8;
define constant <GtkFileFilterFlags> = <C-int>;
define C-pointer-type <GtkFileFilterFlags*> => <GtkFileFilterFlags>;

define C-struct <_GtkFileFilterInfo>
  slot gtk-file-filter-info-contains :: <GtkFileFilterFlags>;
  slot gtk-file-filter-info-filename :: <C-string>;
  slot gtk-file-filter-info-uri :: <C-string>;
  slot gtk-file-filter-info-display-name :: <C-string>;
  slot gtk-file-filter-info-mime-type :: <C-string>;
  pointer-type-name: <GtkFileFilterInfo>;
end C-struct;

define open C-subtype <GtkFixed> (<GtkContainer>)
  constant slot gtk-fixed-container :: <GtkContainer>;
  constant slot gtk-fixed-priv :: <GtkFixedPrivate>;
end C-subtype;

define C-pointer-type <GtkFixed*> => <GtkFixed>;

define C-function gtk-fixed-new
  result res :: <GtkWidget>;
  c-name: "gtk_fixed_new";
end;

define C-function gtk-fixed-move
  input parameter self :: <GtkFixed>;
  input parameter widget_ :: <GtkWidget>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_fixed_move";
end;

define C-function gtk-fixed-put
  input parameter self :: <GtkFixed>;
  input parameter widget_ :: <GtkWidget>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_fixed_put";
end;

define C-struct <_GtkFixedChild>
  slot gtk-fixed-child-widget :: <GtkWidget>;
  slot gtk-fixed-child-x :: <C-signed-int>;
  slot gtk-fixed-child-y :: <C-signed-int>;
  pointer-type-name: <GtkFixedChild>;
end C-struct;

define C-struct <_GtkFixedClass>
  constant slot gtk-fixed-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-fixed-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-fixed-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-fixed-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-fixed-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFixedClass>;
end C-struct;

define C-struct <_GtkFixedPrivate>
  pointer-type-name: <GtkFixedPrivate>;
end C-struct;

define open C-subtype <GtkFontButton> (<GtkButton>)
  constant slot gtk-font-button-button :: <GtkButton>;
  constant slot gtk-font-button-priv :: <GtkFontButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkFontButton*> => <GtkFontButton>;

define C-function gtk-font-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_font_button_new";
end;

define C-function gtk-font-button-new-with-font
  input parameter fontname_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_button_new_with_font";
end;

define C-function gtk-font-button-get-font-name
  input parameter self :: <GtkFontButton>;
  result res :: <C-string>;
  c-name: "gtk_font_button_get_font_name";
end;

define C-function gtk-font-button-get-show-size
  input parameter self :: <GtkFontButton>;
  result res :: <C-boolean>;
  c-name: "gtk_font_button_get_show_size";
end;

define C-function gtk-font-button-get-show-style
  input parameter self :: <GtkFontButton>;
  result res :: <C-boolean>;
  c-name: "gtk_font_button_get_show_style";
end;

define C-function gtk-font-button-get-title
  input parameter self :: <GtkFontButton>;
  result res :: <C-string>;
  c-name: "gtk_font_button_get_title";
end;

define C-function gtk-font-button-get-use-font
  input parameter self :: <GtkFontButton>;
  result res :: <C-boolean>;
  c-name: "gtk_font_button_get_use_font";
end;

define C-function gtk-font-button-get-use-size
  input parameter self :: <GtkFontButton>;
  result res :: <C-boolean>;
  c-name: "gtk_font_button_get_use_size";
end;

define C-function gtk-font-button-set-font-name
  input parameter self :: <GtkFontButton>;
  input parameter fontname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_font_button_set_font_name";
end;

define C-function gtk-font-button-set-show-size
  input parameter self :: <GtkFontButton>;
  input parameter show_size_ :: <C-boolean>;
  c-name: "gtk_font_button_set_show_size";
end;

define C-function gtk-font-button-set-show-style
  input parameter self :: <GtkFontButton>;
  input parameter show_style_ :: <C-boolean>;
  c-name: "gtk_font_button_set_show_style";
end;

define C-function gtk-font-button-set-title
  input parameter self :: <GtkFontButton>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_font_button_set_title";
end;

define C-function gtk-font-button-set-use-font
  input parameter self :: <GtkFontButton>;
  input parameter use_font_ :: <C-boolean>;
  c-name: "gtk_font_button_set_use_font";
end;

define C-function gtk-font-button-set-use-size
  input parameter self :: <GtkFontButton>;
  input parameter use_size_ :: <C-boolean>;
  c-name: "gtk_font_button_set_use_size";
end;

define C-struct <_GtkFontButtonClass>
  constant slot gtk-font-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-font-button-class-font-set :: <C-function-pointer>;
  constant slot gtk-font-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-font-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-font-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-font-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFontButtonClass>;
end C-struct;

define C-struct <_GtkFontButtonPrivate>
  pointer-type-name: <GtkFontButtonPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkFontChooser> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkFontChooser*> => <GtkFontChooser>;

define C-function gtk-font-chooser-get-font
  input parameter self :: <GtkFontChooser>;
  result res :: <C-string>;
  c-name: "gtk_font_chooser_get_font";
end;

define C-function gtk-font-chooser-get-font-desc
  input parameter self :: <GtkFontChooser>;
  result res :: <PangoFontDescription>;
  c-name: "gtk_font_chooser_get_font_desc";
end;

define C-function gtk-font-chooser-get-font-face
  input parameter self :: <GtkFontChooser>;
  result res :: <PangoFontFace>;
  c-name: "gtk_font_chooser_get_font_face";
end;

define C-function gtk-font-chooser-get-font-family
  input parameter self :: <GtkFontChooser>;
  result res :: <PangoFontFamily>;
  c-name: "gtk_font_chooser_get_font_family";
end;

define C-function gtk-font-chooser-get-font-size
  input parameter self :: <GtkFontChooser>;
  result res :: <C-signed-int>;
  c-name: "gtk_font_chooser_get_font_size";
end;

define C-function gtk-font-chooser-get-preview-text
  input parameter self :: <GtkFontChooser>;
  result res :: <C-string>;
  c-name: "gtk_font_chooser_get_preview_text";
end;

define C-function gtk-font-chooser-get-show-preview-entry
  input parameter self :: <GtkFontChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_font_chooser_get_show_preview_entry";
end;

define C-function gtk-font-chooser-set-filter-func
  input parameter self :: <GtkFontChooser>;
  input parameter filter_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_font_chooser_set_filter_func";
end;

define C-function gtk-font-chooser-set-font
  input parameter self :: <GtkFontChooser>;
  input parameter fontname_ :: <C-string>;
  c-name: "gtk_font_chooser_set_font";
end;

define C-function gtk-font-chooser-set-font-desc
  input parameter self :: <GtkFontChooser>;
  input parameter font_desc_ :: <PangoFontDescription>;
  c-name: "gtk_font_chooser_set_font_desc";
end;

define C-function gtk-font-chooser-set-preview-text
  input parameter self :: <GtkFontChooser>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_font_chooser_set_preview_text";
end;

define C-function gtk-font-chooser-set-show-preview-entry
  input parameter self :: <GtkFontChooser>;
  input parameter show_preview_entry_ :: <C-boolean>;
  c-name: "gtk_font_chooser_set_show_preview_entry";
end;

define open C-subtype <GtkFontChooserDialog> (<GtkDialog>)
  constant slot gtk-font-chooser-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-font-chooser-dialog-priv :: <GtkFontChooserDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkFontChooserDialog*> => <GtkFontChooserDialog>;

define C-function gtk-font-chooser-dialog-new
  input parameter title_ :: <C-string>;
  input parameter parent_ :: <GtkWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_chooser_dialog_new";
end;

define C-struct <_GtkFontChooserDialogClass>
  constant slot gtk-font-chooser-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-font-chooser-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-font-chooser-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-font-chooser-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-font-chooser-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFontChooserDialogClass>;
end C-struct;

define C-struct <_GtkFontChooserDialogPrivate>
  pointer-type-name: <GtkFontChooserDialogPrivate>;
end C-struct;

define C-struct <_GtkFontChooserIface>
  constant slot gtk-font-chooser-iface-base-iface :: <GTypeInterface>;
  constant slot gtk-font-chooser-iface-get-font-family :: <C-function-pointer>;
  constant slot gtk-font-chooser-iface-get-font-face :: <C-function-pointer>;
  constant slot gtk-font-chooser-iface-get-font-size :: <C-function-pointer>;
  constant slot gtk-font-chooser-iface-set-filter-func :: <C-function-pointer>;
  constant slot gtk-font-chooser-iface-font-activated :: <C-function-pointer>;
  constant slot gtk-font-chooser-iface-padding :: <C-void*>;
  pointer-type-name: <GtkFontChooserIface>;
end C-struct;

define open C-subtype <GtkFontChooserWidget> (<GtkBox>)
  constant slot gtk-font-chooser-widget-parent-instance :: <GtkBox>;
  constant slot gtk-font-chooser-widget-priv :: <GtkFontChooserWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkFontChooserWidget*> => <GtkFontChooserWidget>;

define C-function gtk-font-chooser-widget-new
  result res :: <GtkWidget>;
  c-name: "gtk_font_chooser_widget_new";
end;

define C-struct <_GtkFontChooserWidgetClass>
  constant slot gtk-font-chooser-widget-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-font-chooser-widget-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkFontChooserWidgetClass>;
end C-struct;

define C-struct <_GtkFontChooserWidgetPrivate>
  pointer-type-name: <GtkFontChooserWidgetPrivate>;
end C-struct;

define open C-subtype <GtkFontSelection> (<GtkBox>)
  constant slot gtk-font-selection-parent-instance :: <GtkBox>;
  constant slot gtk-font-selection-priv :: <GtkFontSelectionPrivate>;
end C-subtype;

define C-pointer-type <GtkFontSelection*> => <GtkFontSelection>;

define C-function gtk-font-selection-new
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_new";
end;

define C-function gtk-font-selection-get-face
  input parameter self :: <GtkFontSelection>;
  result res :: <PangoFontFace>;
  c-name: "gtk_font_selection_get_face";
end;

define C-function gtk-font-selection-get-face-list
  input parameter self :: <GtkFontSelection>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_get_face_list";
end;

define C-function gtk-font-selection-get-family
  input parameter self :: <GtkFontSelection>;
  result res :: <PangoFontFamily>;
  c-name: "gtk_font_selection_get_family";
end;

define C-function gtk-font-selection-get-family-list
  input parameter self :: <GtkFontSelection>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_get_family_list";
end;

define C-function gtk-font-selection-get-font-name
  input parameter self :: <GtkFontSelection>;
  result res :: <C-string>;
  c-name: "gtk_font_selection_get_font_name";
end;

define C-function gtk-font-selection-get-preview-entry
  input parameter self :: <GtkFontSelection>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_get_preview_entry";
end;

define C-function gtk-font-selection-get-preview-text
  input parameter self :: <GtkFontSelection>;
  result res :: <C-string>;
  c-name: "gtk_font_selection_get_preview_text";
end;

define C-function gtk-font-selection-get-size
  input parameter self :: <GtkFontSelection>;
  result res :: <C-signed-int>;
  c-name: "gtk_font_selection_get_size";
end;

define C-function gtk-font-selection-get-size-entry
  input parameter self :: <GtkFontSelection>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_get_size_entry";
end;

define C-function gtk-font-selection-get-size-list
  input parameter self :: <GtkFontSelection>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_get_size_list";
end;

define C-function gtk-font-selection-set-font-name
  input parameter self :: <GtkFontSelection>;
  input parameter fontname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_font_selection_set_font_name";
end;

define C-function gtk-font-selection-set-preview-text
  input parameter self :: <GtkFontSelection>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_font_selection_set_preview_text";
end;

define C-struct <_GtkFontSelectionClass>
  constant slot gtk-font-selection-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-font-selection-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-font-selection-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-font-selection-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-font-selection-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFontSelectionClass>;
end C-struct;

define open C-subtype <GtkFontSelectionDialog> (<GtkDialog>)
  constant slot gtk-font-selection-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-font-selection-dialog-priv :: <GtkFontSelectionDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkFontSelectionDialog*> => <GtkFontSelectionDialog>;

define C-function gtk-font-selection-dialog-new
  input parameter title_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_dialog_new";
end;

define C-function gtk-font-selection-dialog-get-cancel-button
  input parameter self :: <GtkFontSelectionDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_dialog_get_cancel_button";
end;

define C-function gtk-font-selection-dialog-get-font-name
  input parameter self :: <GtkFontSelectionDialog>;
  result res :: <C-string>;
  c-name: "gtk_font_selection_dialog_get_font_name";
end;

define C-function gtk-font-selection-dialog-get-font-selection
  input parameter self :: <GtkFontSelectionDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_dialog_get_font_selection";
end;

define C-function gtk-font-selection-dialog-get-ok-button
  input parameter self :: <GtkFontSelectionDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_font_selection_dialog_get_ok_button";
end;

define C-function gtk-font-selection-dialog-get-preview-text
  input parameter self :: <GtkFontSelectionDialog>;
  result res :: <C-string>;
  c-name: "gtk_font_selection_dialog_get_preview_text";
end;

define C-function gtk-font-selection-dialog-set-font-name
  input parameter self :: <GtkFontSelectionDialog>;
  input parameter fontname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_font_selection_dialog_set_font_name";
end;

define C-function gtk-font-selection-dialog-set-preview-text
  input parameter self :: <GtkFontSelectionDialog>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_font_selection_dialog_set_preview_text";
end;

define C-struct <_GtkFontSelectionDialogClass>
  constant slot gtk-font-selection-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-font-selection-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-font-selection-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-font-selection-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-font-selection-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFontSelectionDialogClass>;
end C-struct;

define C-struct <_GtkFontSelectionDialogPrivate>
  pointer-type-name: <GtkFontSelectionDialogPrivate>;
end C-struct;

define C-struct <_GtkFontSelectionPrivate>
  pointer-type-name: <GtkFontSelectionPrivate>;
end C-struct;

define open C-subtype <GtkFrame> (<GtkBin>)
  constant slot gtk-frame-bin :: <GtkBin>;
  constant slot gtk-frame-priv :: <GtkFramePrivate>;
end C-subtype;

define C-pointer-type <GtkFrame*> => <GtkFrame>;

define C-function gtk-frame-new
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_frame_new";
end;

define C-function gtk-frame-get-label
  input parameter self :: <GtkFrame>;
  result res :: <C-string>;
  c-name: "gtk_frame_get_label";
end;

define C-function gtk-frame-get-label-align
  input parameter self :: <GtkFrame>;
  output parameter xalign_ :: <C-float*>;
  output parameter yalign_ :: <C-float*>;
  c-name: "gtk_frame_get_label_align";
end;

define C-function gtk-frame-get-label-widget
  input parameter self :: <GtkFrame>;
  result res :: <GtkWidget>;
  c-name: "gtk_frame_get_label_widget";
end;

define C-function gtk-frame-get-shadow-type
  input parameter self :: <GtkFrame>;
  result res :: <GtkShadowType>;
  c-name: "gtk_frame_get_shadow_type";
end;

define C-function gtk-frame-set-label
  input parameter self :: <GtkFrame>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_frame_set_label";
end;

define C-function gtk-frame-set-label-align
  input parameter self :: <GtkFrame>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  c-name: "gtk_frame_set_label_align";
end;

define C-function gtk-frame-set-label-widget
  input parameter self :: <GtkFrame>;
  input parameter label_widget_ :: <GtkWidget>;
  c-name: "gtk_frame_set_label_widget";
end;

define C-function gtk-frame-set-shadow-type
  input parameter self :: <GtkFrame>;
  input parameter type_ :: <GtkShadowType>;
  c-name: "gtk_frame_set_shadow_type";
end;

define C-struct <_GtkFrameClass>
  constant slot gtk-frame-class-parent-class :: <GtkBinClass>;
  constant slot gtk-frame-class-compute-child-allocation :: <C-function-pointer>;
  constant slot gtk-frame-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-frame-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-frame-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-frame-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkFrameClass>;
end C-struct;

define C-struct <_GtkFramePrivate>
  pointer-type-name: <GtkFramePrivate>;
end C-struct;

define C-struct <_GtkGradient>
  pointer-type-name: <GtkGradient>;
end C-struct;

define C-function gtk-gradient-new-linear
  input parameter x0_ :: <C-double>;
  input parameter y0_ :: <C-double>;
  input parameter x1_ :: <C-double>;
  input parameter y1_ :: <C-double>;
  result res :: <GtkGradient>;
  c-name: "gtk_gradient_new_linear";
end;

define C-function gtk-gradient-new-radial
  input parameter x0_ :: <C-double>;
  input parameter y0_ :: <C-double>;
  input parameter radius0_ :: <C-double>;
  input parameter x1_ :: <C-double>;
  input parameter y1_ :: <C-double>;
  input parameter radius1_ :: <C-double>;
  result res :: <GtkGradient>;
  c-name: "gtk_gradient_new_radial";
end;

define C-function gtk-gradient-add-color-stop
  input parameter self :: <GtkGradient>;
  input parameter offset_ :: <C-double>;
  input parameter color_ :: <GtkSymbolicColor>;
  c-name: "gtk_gradient_add_color_stop";
end;

define C-function gtk-gradient-ref
  input parameter self :: <GtkGradient>;
  result res :: <GtkGradient>;
  c-name: "gtk_gradient_ref";
end;

define C-function gtk-gradient-resolve
  input parameter self :: <GtkGradient>;
  input parameter props_ :: <GtkStyleProperties>;
  output parameter resolved_gradient_ :: <cairoPattern>;
  result res :: <C-boolean>;
  c-name: "gtk_gradient_resolve";
end;

define C-function gtk-gradient-resolve-for-context
  input parameter self :: <GtkGradient>;
  input parameter context_ :: <GtkStyleContext>;
  result res :: <cairoPattern>;
  c-name: "gtk_gradient_resolve_for_context";
end;

define C-function gtk-gradient-to-string
  input parameter self :: <GtkGradient>;
  result res :: <C-string>;
  c-name: "gtk_gradient_to_string";
end;

define C-function gtk-gradient-unref
  input parameter self :: <GtkGradient>;
  c-name: "gtk_gradient_unref";
end;

define open C-subtype <GtkGrid> (<GtkContainer>)
  constant slot gtk-grid-container :: <GtkContainer>;
  constant slot gtk-grid-priv :: <GtkGridPrivate>;
end C-subtype;

define C-pointer-type <GtkGrid*> => <GtkGrid>;

define C-function gtk-grid-new
  result res :: <GtkWidget>;
  c-name: "gtk_grid_new";
end;

define C-function gtk-grid-attach
  input parameter self :: <GtkGrid>;
  input parameter child_ :: <GtkWidget>;
  input parameter left_ :: <C-signed-int>;
  input parameter top_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_grid_attach";
end;

define C-function gtk-grid-attach-next-to
  input parameter self :: <GtkGrid>;
  input parameter child_ :: <GtkWidget>;
  input parameter sibling_ :: <GtkWidget>;
  input parameter side_ :: <GtkPositionType>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_grid_attach_next_to";
end;

define C-function gtk-grid-get-column-homogeneous
  input parameter self :: <GtkGrid>;
  result res :: <C-boolean>;
  c-name: "gtk_grid_get_column_homogeneous";
end;

define C-function gtk-grid-get-column-spacing
  input parameter self :: <GtkGrid>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_grid_get_column_spacing";
end;

define C-function gtk-grid-get-row-homogeneous
  input parameter self :: <GtkGrid>;
  result res :: <C-boolean>;
  c-name: "gtk_grid_get_row_homogeneous";
end;

define C-function gtk-grid-get-row-spacing
  input parameter self :: <GtkGrid>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_grid_get_row_spacing";
end;

define C-function gtk-grid-insert-column
  input parameter self :: <GtkGrid>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_grid_insert_column";
end;

define C-function gtk-grid-insert-next-to
  input parameter self :: <GtkGrid>;
  input parameter sibling_ :: <GtkWidget>;
  input parameter side_ :: <GtkPositionType>;
  c-name: "gtk_grid_insert_next_to";
end;

define C-function gtk-grid-insert-row
  input parameter self :: <GtkGrid>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_grid_insert_row";
end;

define C-function gtk-grid-set-column-homogeneous
  input parameter self :: <GtkGrid>;
  input parameter homogeneous_ :: <C-boolean>;
  c-name: "gtk_grid_set_column_homogeneous";
end;

define C-function gtk-grid-set-column-spacing
  input parameter self :: <GtkGrid>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_grid_set_column_spacing";
end;

define C-function gtk-grid-set-row-homogeneous
  input parameter self :: <GtkGrid>;
  input parameter homogeneous_ :: <C-boolean>;
  c-name: "gtk_grid_set_row_homogeneous";
end;

define C-function gtk-grid-set-row-spacing
  input parameter self :: <GtkGrid>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_grid_set_row_spacing";
end;

define C-struct <_GtkGridClass>
  constant slot gtk-grid-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-grid-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-grid-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkGridClass>;
end C-struct;

define C-struct <_GtkGridPrivate>
  pointer-type-name: <GtkGridPrivate>;
end C-struct;

define open C-subtype <GtkHBox> (<GtkBox>)
  constant slot gtk-h-box-box :: <GtkBox>;
end C-subtype;

define C-pointer-type <GtkHBox*> => <GtkHBox>;

define C-function gtk-hbox-new
  input parameter homogeneous_ :: <C-boolean>;
  input parameter spacing_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_hbox_new";
end;

define C-struct <_GtkHBoxClass>
  constant slot gtk-h-box-class-parent-class :: <GtkBoxClass>;
  pointer-type-name: <GtkHBoxClass>;
end C-struct;

define open C-subtype <GtkHButtonBox> (<GtkButtonBox>)
  constant slot gtk-h-button-box-button-box :: <GtkButtonBox>;
end C-subtype;

define C-pointer-type <GtkHButtonBox*> => <GtkHButtonBox>;

define C-function gtk-hbutton-box-new
  result res :: <GtkWidget>;
  c-name: "gtk_hbutton_box_new";
end;

define C-struct <_GtkHButtonBoxClass>
  constant slot gtk-h-button-box-class-parent-class :: <GtkButtonBoxClass>;
  pointer-type-name: <GtkHButtonBoxClass>;
end C-struct;

define open C-subtype <GtkHPaned> (<GtkPaned>)
  constant slot gtk-h-paned-paned :: <GtkPaned>;
end C-subtype;

define C-pointer-type <GtkHPaned*> => <GtkHPaned>;

define C-function gtk-hpaned-new
  result res :: <GtkWidget>;
  c-name: "gtk_hpaned_new";
end;

define C-struct <_GtkHPanedClass>
  constant slot gtk-h-paned-class-parent-class :: <GtkPanedClass>;
  pointer-type-name: <GtkHPanedClass>;
end C-struct;

define open C-subtype <GtkHSV> (<GtkWidget>)
  constant slot gtk-hsv-parent-instance :: <GtkWidget>;
  constant slot gtk-hsv-priv :: <GtkHSVPrivate>;
end C-subtype;

define C-pointer-type <GtkHSV*> => <GtkHSV>;

define C-function gtk-hsv-new
  result res :: <GtkWidget>;
  c-name: "gtk_hsv_new";
end;

define C-function gtk-hsv-to-rgb
  input parameter h_ :: <C-double>;
  input parameter s_ :: <C-double>;
  input parameter v_ :: <C-double>;
  output parameter r_ :: <C-double*>;
  output parameter g_ :: <C-double*>;
  output parameter b_ :: <C-double*>;
  c-name: "gtk_hsv_to_rgb";
end;

define C-function gtk-hsv-get-color
  input parameter self :: <GtkHSV>;
  output parameter h_ :: <C-double*>;
  output parameter s_ :: <C-double*>;
  output parameter v_ :: <C-double*>;
  c-name: "gtk_hsv_get_color";
end;

define C-function gtk-hsv-get-metrics
  input parameter self :: <GtkHSV>;
  output parameter size_ :: <C-signed-int*>;
  output parameter ring_width_ :: <C-signed-int*>;
  c-name: "gtk_hsv_get_metrics";
end;

define C-function gtk-hsv-is-adjusting
  input parameter self :: <GtkHSV>;
  result res :: <C-boolean>;
  c-name: "gtk_hsv_is_adjusting";
end;

define C-function gtk-hsv-set-color
  input parameter self :: <GtkHSV>;
  input parameter h_ :: <C-double>;
  input parameter s_ :: <C-double>;
  input parameter v_ :: <C-double>;
  c-name: "gtk_hsv_set_color";
end;

define C-function gtk-hsv-set-metrics
  input parameter self :: <GtkHSV>;
  input parameter size_ :: <C-signed-int>;
  input parameter ring_width_ :: <C-signed-int>;
  c-name: "gtk_hsv_set_metrics";
end;

define C-struct <_GtkHSVClass>
  constant slot gtk-hsv-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-hsv-class-changed :: <C-function-pointer>;
  constant slot gtk-hsv-class-move :: <C-function-pointer>;
  constant slot gtk-hsv-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-hsv-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-hsv-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-hsv-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkHSVClass>;
end C-struct;

define C-struct <_GtkHSVPrivate>
  pointer-type-name: <GtkHSVPrivate>;
end C-struct;

define open C-subtype <GtkHScale> (<GtkScale>)
  constant slot gtk-h-scale-scale :: <GtkScale>;
end C-subtype;

define C-pointer-type <GtkHScale*> => <GtkHScale>;

define C-function gtk-hscale-new
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_hscale_new";
end;

define C-function gtk-hscale-new-with-range
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  input parameter step_ :: <C-double>;
  result res :: <GtkWidget>;
  c-name: "gtk_hscale_new_with_range";
end;

define C-struct <_GtkHScaleClass>
  constant slot gtk-h-scale-class-parent-class :: <GtkScaleClass>;
  pointer-type-name: <GtkHScaleClass>;
end C-struct;

define open C-subtype <GtkHScrollbar> (<GtkScrollbar>)
  constant slot gtk-h-scrollbar-scrollbar :: <GtkScrollbar>;
end C-subtype;

define C-pointer-type <GtkHScrollbar*> => <GtkHScrollbar>;

define C-function gtk-hscrollbar-new
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_hscrollbar_new";
end;

define C-struct <_GtkHScrollbarClass>
  constant slot gtk-h-scrollbar-class-parent-class :: <GtkScrollbarClass>;
  pointer-type-name: <GtkHScrollbarClass>;
end C-struct;

define open C-subtype <GtkHSeparator> (<GtkSeparator>)
  constant slot gtk-h-separator-separator :: <GtkSeparator>;
end C-subtype;

define C-pointer-type <GtkHSeparator*> => <GtkHSeparator>;

define C-function gtk-hseparator-new
  result res :: <GtkWidget>;
  c-name: "gtk_hseparator_new";
end;

define C-struct <_GtkHSeparatorClass>
  constant slot gtk-h-separator-class-parent-class :: <GtkSeparatorClass>;
  pointer-type-name: <GtkHSeparatorClass>;
end C-struct;

define open C-subtype <GtkHandleBox> (<GtkBin>)
  constant slot gtk-handle-box-bin :: <GtkBin>;
  constant slot gtk-handle-box-priv :: <GtkHandleBoxPrivate>;
end C-subtype;

define C-pointer-type <GtkHandleBox*> => <GtkHandleBox>;

define C-function gtk-handle-box-new
  result res :: <GtkWidget>;
  c-name: "gtk_handle_box_new";
end;

define C-function gtk-handle-box-get-child-detached
  input parameter self :: <GtkHandleBox>;
  result res :: <C-boolean>;
  c-name: "gtk_handle_box_get_child_detached";
end;

define C-function gtk-handle-box-get-handle-position
  input parameter self :: <GtkHandleBox>;
  result res :: <GtkPositionType>;
  c-name: "gtk_handle_box_get_handle_position";
end;

define C-function gtk-handle-box-get-shadow-type
  input parameter self :: <GtkHandleBox>;
  result res :: <GtkShadowType>;
  c-name: "gtk_handle_box_get_shadow_type";
end;

define C-function gtk-handle-box-get-snap-edge
  input parameter self :: <GtkHandleBox>;
  result res :: <GtkPositionType>;
  c-name: "gtk_handle_box_get_snap_edge";
end;

define C-function gtk-handle-box-set-handle-position
  input parameter self :: <GtkHandleBox>;
  input parameter position_ :: <GtkPositionType>;
  c-name: "gtk_handle_box_set_handle_position";
end;

define C-function gtk-handle-box-set-shadow-type
  input parameter self :: <GtkHandleBox>;
  input parameter type_ :: <GtkShadowType>;
  c-name: "gtk_handle_box_set_shadow_type";
end;

define C-function gtk-handle-box-set-snap-edge
  input parameter self :: <GtkHandleBox>;
  input parameter edge_ :: <GtkPositionType>;
  c-name: "gtk_handle_box_set_snap_edge";
end;

define C-struct <_GtkHandleBoxClass>
  constant slot gtk-handle-box-class-parent-class :: <GtkBinClass>;
  constant slot gtk-handle-box-class-child-attached :: <C-function-pointer>;
  constant slot gtk-handle-box-class-child-detached :: <C-function-pointer>;
  constant slot gtk-handle-box-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-handle-box-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-handle-box-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-handle-box-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkHandleBoxClass>;
end C-struct;

define C-struct <_GtkHandleBoxPrivate>
  pointer-type-name: <GtkHandleBoxPrivate>;
end C-struct;

define open C-subtype <GtkIMContext> (<GObject>)
  constant slot gtk-im-context-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GtkIMContext*> => <GtkIMContext>;

define C-function gtk-im-context-delete-surrounding
  input parameter self :: <GtkIMContext>;
  input parameter offset_ :: <C-signed-int>;
  input parameter n_chars_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_im_context_delete_surrounding";
end;

define C-function gtk-im-context-filter-keypress
  input parameter self :: <GtkIMContext>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_im_context_filter_keypress";
end;

define C-function gtk-im-context-focus-in
  input parameter self :: <GtkIMContext>;
  c-name: "gtk_im_context_focus_in";
end;

define C-function gtk-im-context-focus-out
  input parameter self :: <GtkIMContext>;
  c-name: "gtk_im_context_focus_out";
end;

define C-function gtk-im-context-get-preedit-string
  input parameter self :: <GtkIMContext>;
  output parameter str_ :: <C-string>;
  output parameter attrs_ :: <PangoAttrList>;
  output parameter cursor_pos_ :: <C-signed-int*>;
  c-name: "gtk_im_context_get_preedit_string";
end;

define C-function gtk-im-context-get-surrounding
  input parameter self :: <GtkIMContext>;
  output parameter text_ :: <C-string>;
  input parameter cursor_index_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_im_context_get_surrounding";
end;

define C-function gtk-im-context-reset
  input parameter self :: <GtkIMContext>;
  c-name: "gtk_im_context_reset";
end;

define C-function gtk-im-context-set-client-window
  input parameter self :: <GtkIMContext>;
  input parameter window_ :: <GdkWindow>;
  c-name: "gtk_im_context_set_client_window";
end;

define C-function gtk-im-context-set-cursor-location
  input parameter self :: <GtkIMContext>;
  input parameter area_ :: <cairoRectangleInt>;
  c-name: "gtk_im_context_set_cursor_location";
end;

define C-function gtk-im-context-set-surrounding
  input parameter self :: <GtkIMContext>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  input parameter cursor_index_ :: <C-signed-int>;
  c-name: "gtk_im_context_set_surrounding";
end;

define C-function gtk-im-context-set-use-preedit
  input parameter self :: <GtkIMContext>;
  input parameter use_preedit_ :: <C-boolean>;
  c-name: "gtk_im_context_set_use_preedit";
end;

define C-struct <_GtkIMContextClass>
  constant slot gtk-im-context-class-parent-class :: <GObjectClass>;
  constant slot gtk-im-context-class-preedit-start :: <C-function-pointer>;
  constant slot gtk-im-context-class-preedit-end :: <C-function-pointer>;
  constant slot gtk-im-context-class-preedit-changed :: <C-function-pointer>;
  constant slot gtk-im-context-class-commit :: <C-function-pointer>;
  constant slot gtk-im-context-class-retrieve-surrounding :: <C-function-pointer>;
  constant slot gtk-im-context-class-delete-surrounding :: <C-function-pointer>;
  constant slot gtk-im-context-class-set-client-window :: <C-function-pointer>;
  constant slot gtk-im-context-class-get-preedit-string :: <C-function-pointer>;
  constant slot gtk-im-context-class-filter-keypress :: <C-function-pointer>;
  constant slot gtk-im-context-class-focus-in :: <C-function-pointer>;
  constant slot gtk-im-context-class-focus-out :: <C-function-pointer>;
  constant slot gtk-im-context-class-reset :: <C-function-pointer>;
  constant slot gtk-im-context-class-set-cursor-location :: <C-function-pointer>;
  constant slot gtk-im-context-class-set-use-preedit :: <C-function-pointer>;
  constant slot gtk-im-context-class-set-surrounding :: <C-function-pointer>;
  constant slot gtk-im-context-class-get-surrounding :: <C-function-pointer>;
  constant slot gtk-im-context-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-im-context-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-im-context-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-im-context-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-im-context-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-im-context-class-_gtk-reserved6 :: <C-void*>;
  pointer-type-name: <GtkIMContextClass>;
end C-struct;

define C-struct <_GtkIMContextInfo>
  slot gtk-im-context-info-context-id :: <C-string>;
  slot gtk-im-context-info-context-name :: <C-string>;
  slot gtk-im-context-info-domain :: <C-string>;
  slot gtk-im-context-info-domain-dirname :: <C-string>;
  slot gtk-im-context-info-default-locales :: <C-string>;
  pointer-type-name: <GtkIMContextInfo>;
end C-struct;

define open C-subtype <GtkIMContextSimple> (<GtkIMContext>)
  constant slot gtk-im-context-simple-object :: <GtkIMContext>;
  constant slot gtk-im-context-simple-priv :: <GtkIMContextSimplePrivate>;
end C-subtype;

define C-pointer-type <GtkIMContextSimple*> => <GtkIMContextSimple>;

define C-function gtk-im-context-simple-new
  result res :: <GtkIMContext>;
  c-name: "gtk_im_context_simple_new";
end;

define C-struct <_GtkIMContextSimpleClass>
  constant slot gtk-im-context-simple-class-parent-class :: <GtkIMContextClass>;
  pointer-type-name: <GtkIMContextSimpleClass>;
end C-struct;

define C-struct <_GtkIMContextSimplePrivate>
  pointer-type-name: <GtkIMContextSimplePrivate>;
end C-struct;

define open C-subtype <GtkIMMulticontext> (<GtkIMContext>)
  constant slot gtk-im-multicontext-object :: <GtkIMContext>;
  constant slot gtk-im-multicontext-priv :: <GtkIMMulticontextPrivate>;
end C-subtype;

define C-pointer-type <GtkIMMulticontext*> => <GtkIMMulticontext>;

define C-function gtk-im-multicontext-new
  result res :: <GtkIMContext>;
  c-name: "gtk_im_multicontext_new";
end;

define C-function gtk-im-multicontext-append-menuitems
  input parameter self :: <GtkIMMulticontext>;
  input parameter menushell_ :: <GtkMenuShell>;
  c-name: "gtk_im_multicontext_append_menuitems";
end;

define C-function gtk-im-multicontext-get-context-id
  input parameter self :: <GtkIMMulticontext>;
  result res :: <C-string>;
  c-name: "gtk_im_multicontext_get_context_id";
end;

define C-function gtk-im-multicontext-set-context-id
  input parameter self :: <GtkIMMulticontext>;
  input parameter context_id_ :: <C-string>;
  c-name: "gtk_im_multicontext_set_context_id";
end;

define C-struct <_GtkIMMulticontextClass>
  constant slot gtk-im-multicontext-class-parent-class :: <GtkIMContextClass>;
  constant slot gtk-im-multicontext-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-im-multicontext-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-im-multicontext-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-im-multicontext-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkIMMulticontextClass>;
end C-struct;

define C-struct <_GtkIMMulticontextPrivate>
  pointer-type-name: <GtkIMMulticontextPrivate>;
end C-struct;

define constant $gtk-im-preedit-nothing = 0;
define constant $gtk-im-preedit-callback = 1;
define constant $gtk-im-preedit-none = 2;
define constant <GtkIMPreeditStyle> = <C-int>;
define C-pointer-type <GtkIMPreeditStyle*> => <GtkIMPreeditStyle>;

define constant $gtk-im-status-nothing = 0;
define constant $gtk-im-status-callback = 1;
define constant $gtk-im-status-none = 2;
define constant <GtkIMStatusStyle> = <C-int>;
define C-pointer-type <GtkIMStatusStyle*> => <GtkIMStatusStyle>;

define constant $input-error = -1;

define constant $gtk-interface-age = 2;

define open C-subtype <GtkIconFactory> (<GObject>)
  constant slot gtk-icon-factory-parent-instance :: <GObject>;
  constant slot gtk-icon-factory-priv :: <GtkIconFactoryPrivate>;
end C-subtype;

define C-pointer-type <GtkIconFactory*> => <GtkIconFactory>;

define C-function gtk-icon-factory-new
  result res :: <GtkIconFactory>;
  c-name: "gtk_icon_factory_new";
end;

define C-function gtk-icon-factory-lookup-default
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_factory_lookup_default";
end;

define C-function gtk-icon-factory-add
  input parameter self :: <GtkIconFactory>;
  input parameter stock_id_ :: <C-string>;
  input parameter icon_set_ :: <GtkIconSet>;
  c-name: "gtk_icon_factory_add";
end;

define C-function gtk-icon-factory-add-default
  input parameter self :: <GtkIconFactory>;
  c-name: "gtk_icon_factory_add_default";
end;

define C-function gtk-icon-factory-lookup
  input parameter self :: <GtkIconFactory>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_factory_lookup";
end;

define C-function gtk-icon-factory-remove-default
  input parameter self :: <GtkIconFactory>;
  c-name: "gtk_icon_factory_remove_default";
end;

define C-struct <_GtkIconFactoryClass>
  constant slot gtk-icon-factory-class-parent-class :: <GObjectClass>;
  constant slot gtk-icon-factory-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-icon-factory-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-icon-factory-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-icon-factory-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkIconFactoryClass>;
end C-struct;

define C-struct <_GtkIconFactoryPrivate>
  pointer-type-name: <GtkIconFactoryPrivate>;
end C-struct;

define C-struct <_GtkIconInfo>
  pointer-type-name: <GtkIconInfo>;
end C-struct;

define C-function gtk-icon-info-new-for-pixbuf
  input parameter icon_theme_ :: <GtkIconTheme>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <GtkIconInfo>;
  c-name: "gtk_icon_info_new_for_pixbuf";
end;

define C-function gtk-icon-info-copy
  input parameter self :: <GtkIconInfo>;
  result res :: <GtkIconInfo>;
  c-name: "gtk_icon_info_copy";
end;

define C-function gtk-icon-info-free
  input parameter self :: <GtkIconInfo>;
  c-name: "gtk_icon_info_free";
end;

define C-function gtk-icon-info-get-attach-points
  input parameter self :: <GtkIconInfo>;
  output parameter points_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_points_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_info_get_attach_points";
end;

define C-function gtk-icon-info-get-base-size
  input parameter self :: <GtkIconInfo>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_info_get_base_size";
end;

define C-function gtk-icon-info-get-builtin-pixbuf
  input parameter self :: <GtkIconInfo>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_info_get_builtin_pixbuf";
end;

define C-function gtk-icon-info-get-display-name
  input parameter self :: <GtkIconInfo>;
  result res :: <C-string>;
  c-name: "gtk_icon_info_get_display_name";
end;

define C-function gtk-icon-info-get-embedded-rect
  input parameter self :: <GtkIconInfo>;
  output parameter rectangle_ :: <cairoRectangleInt>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_info_get_embedded_rect";
end;

define C-function gtk-icon-info-get-filename
  input parameter self :: <GtkIconInfo>;
  result res :: <C-string>;
  c-name: "gtk_icon_info_get_filename";
end;

define C-function gtk-icon-info-load-icon
  input parameter self :: <GtkIconInfo>;
  output parameter error_ :: <GError*>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_info_load_icon";
end;

define C-function gtk-icon-info-load-symbolic
  input parameter self :: <GtkIconInfo>;
  input parameter fg_ :: <GdkRGBA>;
  input parameter success_color_ :: <GdkRGBA>;
  input parameter warning_color_ :: <GdkRGBA>;
  input parameter error_color_ :: <GdkRGBA>;
  output parameter was_symbolic_ :: <C-int*>;
  output parameter error_ :: <GError*>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_info_load_symbolic";
end;

define C-function gtk-icon-info-load-symbolic-for-context
  input parameter self :: <GtkIconInfo>;
  input parameter context_ :: <GtkStyleContext>;
  output parameter was_symbolic_ :: <C-int*>;
  output parameter error_ :: <GError*>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_info_load_symbolic_for_context";
end;

define C-function gtk-icon-info-load-symbolic-for-style
  input parameter self :: <GtkIconInfo>;
  input parameter style_ :: <GtkStyle>;
  input parameter state_ :: <GtkStateType>;
  output parameter was_symbolic_ :: <C-int*>;
  output parameter error_ :: <GError*>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_info_load_symbolic_for_style";
end;

define C-function gtk-icon-info-set-raw-coordinates
  input parameter self :: <GtkIconInfo>;
  input parameter raw_coordinates_ :: <C-boolean>;
  c-name: "gtk_icon_info_set_raw_coordinates";
end;

define constant $gtk-icon-lookup-no-svg = 1;
define constant $gtk-icon-lookup-force-svg = 2;
define constant $gtk-icon-lookup-use-builtin = 4;
define constant $gtk-icon-lookup-generic-fallback = 8;
define constant $gtk-icon-lookup-force-size = 16;
define constant <GtkIconLookupFlags> = <C-int>;
define C-pointer-type <GtkIconLookupFlags*> => <GtkIconLookupFlags>;

define C-struct <_GtkIconSet>
  pointer-type-name: <GtkIconSet>;
end C-struct;

define C-function gtk-icon-set-new
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_set_new";
end;

define C-function gtk-icon-set-new-from-pixbuf
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_set_new_from_pixbuf";
end;

define C-function gtk-icon-set-add-source
  input parameter self :: <GtkIconSet>;
  input parameter source_ :: <GtkIconSource>;
  c-name: "gtk_icon_set_add_source";
end;

define C-function gtk-icon-set-copy
  input parameter self :: <GtkIconSet>;
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_set_copy";
end;

define C-function gtk-icon-set-get-sizes
  input parameter self :: <GtkIconSet>;
  output parameter sizes_ :: <C-signed-int*>;
  output parameter n_sizes_ :: <C-signed-int*>;
  c-name: "gtk_icon_set_get_sizes";
end;

define C-function gtk-icon-set-ref
  input parameter self :: <GtkIconSet>;
  result res :: <GtkIconSet>;
  c-name: "gtk_icon_set_ref";
end;

define C-function gtk-icon-set-render-icon
  input parameter self :: <GtkIconSet>;
  input parameter style_ :: <GtkStyle>;
  input parameter direction_ :: <GtkTextDirection>;
  input parameter state_ :: <GtkStateType>;
  input parameter size_ :: <C-signed-int>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_set_render_icon";
end;

define C-function gtk-icon-set-render-icon-pixbuf
  input parameter self :: <GtkIconSet>;
  input parameter context_ :: <GtkStyleContext>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_set_render_icon_pixbuf";
end;

define C-function gtk-icon-set-unref
  input parameter self :: <GtkIconSet>;
  c-name: "gtk_icon_set_unref";
end;

define constant $gtk-icon-size-invalid = 0;
define constant $gtk-icon-size-menu = 1;
define constant $gtk-icon-size-small-toolbar = 2;
define constant $gtk-icon-size-large-toolbar = 3;
define constant $gtk-icon-size-button = 4;
define constant $gtk-icon-size-dnd = 5;
define constant $gtk-icon-size-dialog = 6;
define constant <GtkIconSize> = <C-int>;
define C-pointer-type <GtkIconSize*> => <GtkIconSize>;

define C-struct <_GtkIconSource>
  pointer-type-name: <GtkIconSource>;
end C-struct;

define C-function gtk-icon-source-new
  result res :: <GtkIconSource>;
  c-name: "gtk_icon_source_new";
end;

define C-function gtk-icon-source-copy
  input parameter self :: <GtkIconSource>;
  result res :: <GtkIconSource>;
  c-name: "gtk_icon_source_copy";
end;

define C-function gtk-icon-source-free
  input parameter self :: <GtkIconSource>;
  c-name: "gtk_icon_source_free";
end;

define C-function gtk-icon-source-get-direction
  input parameter self :: <GtkIconSource>;
  result res :: <GtkTextDirection>;
  c-name: "gtk_icon_source_get_direction";
end;

define C-function gtk-icon-source-get-direction-wildcarded
  input parameter self :: <GtkIconSource>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_source_get_direction_wildcarded";
end;

define C-function gtk-icon-source-get-filename
  input parameter self :: <GtkIconSource>;
  result res :: <C-string>;
  c-name: "gtk_icon_source_get_filename";
end;

define C-function gtk-icon-source-get-icon-name
  input parameter self :: <GtkIconSource>;
  result res :: <C-string>;
  c-name: "gtk_icon_source_get_icon_name";
end;

define C-function gtk-icon-source-get-pixbuf
  input parameter self :: <GtkIconSource>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_source_get_pixbuf";
end;

define C-function gtk-icon-source-get-size
  input parameter self :: <GtkIconSource>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_source_get_size";
end;

define C-function gtk-icon-source-get-size-wildcarded
  input parameter self :: <GtkIconSource>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_source_get_size_wildcarded";
end;

define C-function gtk-icon-source-get-state
  input parameter self :: <GtkIconSource>;
  result res :: <GtkStateType>;
  c-name: "gtk_icon_source_get_state";
end;

define C-function gtk-icon-source-get-state-wildcarded
  input parameter self :: <GtkIconSource>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_source_get_state_wildcarded";
end;

define C-function gtk-icon-source-set-direction
  input parameter self :: <GtkIconSource>;
  input parameter direction_ :: <GtkTextDirection>;
  c-name: "gtk_icon_source_set_direction";
end;

define C-function gtk-icon-source-set-direction-wildcarded
  input parameter self :: <GtkIconSource>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_icon_source_set_direction_wildcarded";
end;

define C-function gtk-icon-source-set-filename
  input parameter self :: <GtkIconSource>;
  input parameter filename_ :: <C-string>;
  c-name: "gtk_icon_source_set_filename";
end;

define C-function gtk-icon-source-set-icon-name
  input parameter self :: <GtkIconSource>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_icon_source_set_icon_name";
end;

define C-function gtk-icon-source-set-pixbuf
  input parameter self :: <GtkIconSource>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_icon_source_set_pixbuf";
end;

define C-function gtk-icon-source-set-size
  input parameter self :: <GtkIconSource>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_icon_source_set_size";
end;

define C-function gtk-icon-source-set-size-wildcarded
  input parameter self :: <GtkIconSource>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_icon_source_set_size_wildcarded";
end;

define C-function gtk-icon-source-set-state
  input parameter self :: <GtkIconSource>;
  input parameter state_ :: <GtkStateType>;
  c-name: "gtk_icon_source_set_state";
end;

define C-function gtk-icon-source-set-state-wildcarded
  input parameter self :: <GtkIconSource>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_icon_source_set_state_wildcarded";
end;

define open C-subtype <GtkIconTheme> (<GObject>)
  constant slot gtk-icon-theme-parent-instance :: <GObject>;
  constant slot gtk-icon-theme-priv :: <GtkIconThemePrivate>;
end C-subtype;

define C-pointer-type <GtkIconTheme*> => <GtkIconTheme>;

define C-function gtk-icon-theme-new
  result res :: <GtkIconTheme>;
  c-name: "gtk_icon_theme_new";
end;

define C-function gtk-icon-theme-add-builtin-icon
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_icon_theme_add_builtin_icon";
end;

define C-function gtk-icon-theme-get-default
  result res :: <GtkIconTheme>;
  c-name: "gtk_icon_theme_get_default";
end;

define C-function gtk-icon-theme-get-for-screen
  input parameter screen_ :: <GdkScreen>;
  result res :: <GtkIconTheme>;
  c-name: "gtk_icon_theme_get_for_screen";
end;

define C-function gtk-icon-theme-append-search-path
  input parameter self :: <GtkIconTheme>;
  input parameter path_ :: <C-string>;
  c-name: "gtk_icon_theme_append_search_path";
end;

define C-function gtk-icon-theme-choose-icon
  input parameter self :: <GtkIconTheme>;
  input parameter icon_names_ :: <C-string*>;
  input parameter size_ :: <C-signed-int>;
  input parameter flags_ :: <GtkIconLookupFlags>;
  result res :: <GtkIconInfo>;
  c-name: "gtk_icon_theme_choose_icon";
end;

define C-function gtk-icon-theme-get-example-icon-name
  input parameter self :: <GtkIconTheme>;
  result res :: <C-string>;
  c-name: "gtk_icon_theme_get_example_icon_name";
end;

define C-function gtk-icon-theme-get-icon-sizes
  input parameter self :: <GtkIconTheme>;
  input parameter icon_name_ :: <C-string>;
  result res :: <C-signed-int*>;
  c-name: "gtk_icon_theme_get_icon_sizes";
end;

define C-function gtk-icon-theme-get-search-path
  input parameter self :: <GtkIconTheme>;
  output parameter path_ :: <C-string*>;
  output parameter n_elements_ :: <C-signed-int*>;
  c-name: "gtk_icon_theme_get_search_path";
end;

define C-function gtk-icon-theme-has-icon
  input parameter self :: <GtkIconTheme>;
  input parameter icon_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_theme_has_icon";
end;

define C-function gtk-icon-theme-list-contexts
  input parameter self :: <GtkIconTheme>;
  result res :: <GList>;
  c-name: "gtk_icon_theme_list_contexts";
end;

define C-function gtk-icon-theme-list-icons
  input parameter self :: <GtkIconTheme>;
  input parameter context_ :: <C-string>;
  result res :: <GList>;
  c-name: "gtk_icon_theme_list_icons";
end;

define C-function gtk-icon-theme-load-icon
  input parameter self :: <GtkIconTheme>;
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  input parameter flags_ :: <GtkIconLookupFlags>;
  output parameter error_ :: <GError*>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_icon_theme_load_icon";
end;

define C-function gtk-icon-theme-lookup-by-gicon
  input parameter self :: <GtkIconTheme>;
  input parameter icon_ :: <GIcon>;
  input parameter size_ :: <C-signed-int>;
  input parameter flags_ :: <GtkIconLookupFlags>;
  result res :: <GtkIconInfo>;
  c-name: "gtk_icon_theme_lookup_by_gicon";
end;

define C-function gtk-icon-theme-lookup-icon
  input parameter self :: <GtkIconTheme>;
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  input parameter flags_ :: <GtkIconLookupFlags>;
  result res :: <GtkIconInfo>;
  c-name: "gtk_icon_theme_lookup_icon";
end;

define C-function gtk-icon-theme-prepend-search-path
  input parameter self :: <GtkIconTheme>;
  input parameter path_ :: <C-string>;
  c-name: "gtk_icon_theme_prepend_search_path";
end;

define C-function gtk-icon-theme-rescan-if-needed
  input parameter self :: <GtkIconTheme>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_theme_rescan_if_needed";
end;

define C-function gtk-icon-theme-set-custom-theme
  input parameter self :: <GtkIconTheme>;
  input parameter theme_name_ :: <C-string>;
  c-name: "gtk_icon_theme_set_custom_theme";
end;

define C-function gtk-icon-theme-set-screen
  input parameter self :: <GtkIconTheme>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_icon_theme_set_screen";
end;

define C-function gtk-icon-theme-set-search-path
  input parameter self :: <GtkIconTheme>;
  input parameter path_ :: <C-string*>;
  input parameter n_elements_ :: <C-signed-int>;
  c-name: "gtk_icon_theme_set_search_path";
end;

define C-struct <_GtkIconThemeClass>
  constant slot gtk-icon-theme-class-parent-class :: <GObjectClass>;
  constant slot gtk-icon-theme-class-changed :: <C-function-pointer>;
  constant slot gtk-icon-theme-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-icon-theme-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-icon-theme-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-icon-theme-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkIconThemeClass>;
end C-struct;

define constant $gtk-icon-theme-not-found = 0;
define constant $gtk-icon-theme-failed = 1;
define constant <GtkIconThemeError> = <C-int>;
define C-pointer-type <GtkIconThemeError*> => <GtkIconThemeError>;

define C-struct <_GtkIconThemePrivate>
  pointer-type-name: <GtkIconThemePrivate>;
end C-struct;

define open C-subtype <GtkIconView> (<GtkContainer>)
  constant slot gtk-icon-view-parent :: <GtkContainer>;
  constant slot gtk-icon-view-priv :: <GtkIconViewPrivate>;
end C-subtype;

define C-pointer-type <GtkIconView*> => <GtkIconView>;

define C-function gtk-icon-view-new
  result res :: <GtkWidget>;
  c-name: "gtk_icon_view_new";
end;

define C-function gtk-icon-view-new-with-area
  input parameter area_ :: <GtkCellArea>;
  result res :: <GtkWidget>;
  c-name: "gtk_icon_view_new_with_area";
end;

define C-function gtk-icon-view-new-with-model
  input parameter model_ :: <GtkTreeModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_icon_view_new_with_model";
end;

define C-function gtk-icon-view-convert-widget-to-bin-window-coords
  input parameter self :: <GtkIconView>;
  input parameter wx_ :: <C-signed-int>;
  input parameter wy_ :: <C-signed-int>;
  output parameter bx_ :: <C-signed-int*>;
  output parameter by_ :: <C-signed-int*>;
  c-name: "gtk_icon_view_convert_widget_to_bin_window_coords";
end;

define C-function gtk-icon-view-create-drag-icon
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <cairoSurface>;
  c-name: "gtk_icon_view_create_drag_icon";
end;

define C-function gtk-icon-view-enable-model-drag-dest
  input parameter self :: <GtkIconView>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_icon_view_enable_model_drag_dest";
end;

define C-function gtk-icon-view-enable-model-drag-source
  input parameter self :: <GtkIconView>;
  input parameter start_button_mask_ :: <GdkModifierType>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_icon_view_enable_model_drag_source";
end;

define C-function gtk-icon-view-get-column-spacing
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_column_spacing";
end;

define C-function gtk-icon-view-get-columns
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_columns";
end;

define C-function gtk-icon-view-get-cursor
  input parameter self :: <GtkIconView>;
  output parameter path_ :: <GtkTreePath>;
  output parameter cell_ :: <GtkCellRenderer*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_cursor";
end;

define C-function gtk-icon-view-get-dest-item-at-pos
  input parameter self :: <GtkIconView>;
  input parameter drag_x_ :: <C-signed-int>;
  input parameter drag_y_ :: <C-signed-int>;
  output parameter path_ :: <GtkTreePath>;
  output parameter pos_ :: <GtkIconViewDropPosition*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_dest_item_at_pos";
end;

define C-function gtk-icon-view-get-drag-dest-item
  input parameter self :: <GtkIconView>;
  output parameter path_ :: <GtkTreePath>;
  output parameter pos_ :: <GtkIconViewDropPosition*>;
  c-name: "gtk_icon_view_get_drag_dest_item";
end;

define C-function gtk-icon-view-get-item-at-pos
  input parameter self :: <GtkIconView>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter path_ :: <GtkTreePath>;
  output parameter cell_ :: <GtkCellRenderer*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_item_at_pos";
end;

define C-function gtk-icon-view-get-item-column
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_item_column";
end;

define C-function gtk-icon-view-get-item-orientation
  input parameter self :: <GtkIconView>;
  result res :: <GtkOrientation>;
  c-name: "gtk_icon_view_get_item_orientation";
end;

define C-function gtk-icon-view-get-item-padding
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_item_padding";
end;

define C-function gtk-icon-view-get-item-row
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_item_row";
end;

define C-function gtk-icon-view-get-item-width
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_item_width";
end;

define C-function gtk-icon-view-get-margin
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_margin";
end;

define C-function gtk-icon-view-get-markup-column
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_markup_column";
end;

define C-function gtk-icon-view-get-model
  input parameter self :: <GtkIconView>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_icon_view_get_model";
end;

define C-function gtk-icon-view-get-path-at-pos
  input parameter self :: <GtkIconView>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <GtkTreePath>;
  c-name: "gtk_icon_view_get_path_at_pos";
end;

define C-function gtk-icon-view-get-pixbuf-column
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_pixbuf_column";
end;

define C-function gtk-icon-view-get-reorderable
  input parameter self :: <GtkIconView>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_reorderable";
end;

define C-function gtk-icon-view-get-row-spacing
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_row_spacing";
end;

define C-function gtk-icon-view-get-selected-items
  input parameter self :: <GtkIconView>;
  result res :: <GList>;
  c-name: "gtk_icon_view_get_selected_items";
end;

define C-function gtk-icon-view-get-selection-mode
  input parameter self :: <GtkIconView>;
  result res :: <GtkSelectionMode>;
  c-name: "gtk_icon_view_get_selection_mode";
end;

define C-function gtk-icon-view-get-spacing
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_spacing";
end;

define C-function gtk-icon-view-get-text-column
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_text_column";
end;

define C-function gtk-icon-view-get-tooltip-column
  input parameter self :: <GtkIconView>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_view_get_tooltip_column";
end;

define C-function gtk-icon-view-get-tooltip-context
  input parameter self :: <GtkIconView>;
  input output parameter x_ :: <C-signed-int*>;
  input output parameter y_ :: <C-signed-int*>;
  input parameter keyboard_tip_ :: <C-boolean>;
  output parameter model_ :: <GtkTreeModel*>;
  output parameter path_ :: <GtkTreePath>;
  output parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_tooltip_context";
end;

define C-function gtk-icon-view-get-visible-range
  input parameter self :: <GtkIconView>;
  output parameter start_path_ :: <GtkTreePath>;
  output parameter end_path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_get_visible_range";
end;

define C-function gtk-icon-view-item-activated
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_icon_view_item_activated";
end;

define C-function gtk-icon-view-path-is-selected
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_view_path_is_selected";
end;

define C-function gtk-icon-view-scroll-to-path
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter use_align_ :: <C-boolean>;
  input parameter row_align_ :: <C-float>;
  input parameter col_align_ :: <C-float>;
  c-name: "gtk_icon_view_scroll_to_path";
end;

define C-function gtk-icon-view-select-all
  input parameter self :: <GtkIconView>;
  c-name: "gtk_icon_view_select_all";
end;

define C-function gtk-icon-view-select-path
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_icon_view_select_path";
end;

define C-function gtk-icon-view-selected-foreach
  input parameter self :: <GtkIconView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_icon_view_selected_foreach";
end;

define C-function gtk-icon-view-set-column-spacing
  input parameter self :: <GtkIconView>;
  input parameter column_spacing_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_column_spacing";
end;

define C-function gtk-icon-view-set-columns
  input parameter self :: <GtkIconView>;
  input parameter columns_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_columns";
end;

define C-function gtk-icon-view-set-cursor
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter start_editing_ :: <C-boolean>;
  c-name: "gtk_icon_view_set_cursor";
end;

define C-function gtk-icon-view-set-drag-dest-item
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter pos_ :: <GtkIconViewDropPosition>;
  c-name: "gtk_icon_view_set_drag_dest_item";
end;

define C-function gtk-icon-view-set-item-orientation
  input parameter self :: <GtkIconView>;
  input parameter orientation_ :: <GtkOrientation>;
  c-name: "gtk_icon_view_set_item_orientation";
end;

define C-function gtk-icon-view-set-item-padding
  input parameter self :: <GtkIconView>;
  input parameter item_padding_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_item_padding";
end;

define C-function gtk-icon-view-set-item-width
  input parameter self :: <GtkIconView>;
  input parameter item_width_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_item_width";
end;

define C-function gtk-icon-view-set-margin
  input parameter self :: <GtkIconView>;
  input parameter margin_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_margin";
end;

define C-function gtk-icon-view-set-markup-column
  input parameter self :: <GtkIconView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_markup_column";
end;

define C-function gtk-icon-view-set-model
  input parameter self :: <GtkIconView>;
  input parameter model_ :: <GtkTreeModel>;
  c-name: "gtk_icon_view_set_model";
end;

define C-function gtk-icon-view-set-pixbuf-column
  input parameter self :: <GtkIconView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_pixbuf_column";
end;

define C-function gtk-icon-view-set-reorderable
  input parameter self :: <GtkIconView>;
  input parameter reorderable_ :: <C-boolean>;
  c-name: "gtk_icon_view_set_reorderable";
end;

define C-function gtk-icon-view-set-row-spacing
  input parameter self :: <GtkIconView>;
  input parameter row_spacing_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_row_spacing";
end;

define C-function gtk-icon-view-set-selection-mode
  input parameter self :: <GtkIconView>;
  input parameter mode_ :: <GtkSelectionMode>;
  c-name: "gtk_icon_view_set_selection_mode";
end;

define C-function gtk-icon-view-set-spacing
  input parameter self :: <GtkIconView>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_spacing";
end;

define C-function gtk-icon-view-set-text-column
  input parameter self :: <GtkIconView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_text_column";
end;

define C-function gtk-icon-view-set-tooltip-cell
  input parameter self :: <GtkIconView>;
  input parameter tooltip_ :: <GtkTooltip>;
  input parameter path_ :: <GtkTreePath>;
  input parameter cell_ :: <GtkCellRenderer>;
  c-name: "gtk_icon_view_set_tooltip_cell";
end;

define C-function gtk-icon-view-set-tooltip-column
  input parameter self :: <GtkIconView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_icon_view_set_tooltip_column";
end;

define C-function gtk-icon-view-set-tooltip-item
  input parameter self :: <GtkIconView>;
  input parameter tooltip_ :: <GtkTooltip>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_icon_view_set_tooltip_item";
end;

define C-function gtk-icon-view-unselect-all
  input parameter self :: <GtkIconView>;
  c-name: "gtk_icon_view_unselect_all";
end;

define C-function gtk-icon-view-unselect-path
  input parameter self :: <GtkIconView>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_icon_view_unselect_path";
end;

define C-function gtk-icon-view-unset-model-drag-dest
  input parameter self :: <GtkIconView>;
  c-name: "gtk_icon_view_unset_model_drag_dest";
end;

define C-function gtk-icon-view-unset-model-drag-source
  input parameter self :: <GtkIconView>;
  c-name: "gtk_icon_view_unset_model_drag_source";
end;

define C-struct <_GtkIconViewClass>
  constant slot gtk-icon-view-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-icon-view-class-item-activated :: <C-function-pointer>;
  constant slot gtk-icon-view-class-selection-changed :: <C-function-pointer>;
  constant slot gtk-icon-view-class-select-all :: <C-function-pointer>;
  constant slot gtk-icon-view-class-unselect-all :: <C-function-pointer>;
  constant slot gtk-icon-view-class-select-cursor-item :: <C-function-pointer>;
  constant slot gtk-icon-view-class-toggle-cursor-item :: <C-function-pointer>;
  constant slot gtk-icon-view-class-move-cursor :: <C-function-pointer>;
  constant slot gtk-icon-view-class-activate-cursor-item :: <C-function-pointer>;
  constant slot gtk-icon-view-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-icon-view-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-icon-view-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-icon-view-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkIconViewClass>;
end C-struct;

define constant $gtk-icon-view-no-drop = 0;
define constant $gtk-icon-view-drop-into = 1;
define constant $gtk-icon-view-drop-left = 2;
define constant $gtk-icon-view-drop-right = 3;
define constant $gtk-icon-view-drop-above = 4;
define constant $gtk-icon-view-drop-below = 5;
define constant <GtkIconViewDropPosition> = <C-int>;
define C-pointer-type <GtkIconViewDropPosition*> => <GtkIconViewDropPosition>;

define C-struct <_GtkIconViewPrivate>
  pointer-type-name: <GtkIconViewPrivate>;
end C-struct;

define open C-subtype <GtkImage> (<GtkMisc>)
  constant slot gtk-image-misc :: <GtkMisc>;
  constant slot gtk-image-priv :: <GtkImagePrivate>;
end C-subtype;

define C-pointer-type <GtkImage*> => <GtkImage>;

define C-function gtk-image-new
  result res :: <GtkWidget>;
  c-name: "gtk_image_new";
end;

define C-function gtk-image-new-from-animation
  input parameter animation_ :: <GdkPixbufAnimation>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_animation";
end;

define C-function gtk-image-new-from-file
  input parameter filename_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_file";
end;

define C-function gtk-image-new-from-gicon
  input parameter icon_ :: <GIcon>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_gicon";
end;

define C-function gtk-image-new-from-icon-name
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_icon_name";
end;

define C-function gtk-image-new-from-icon-set
  input parameter icon_set_ :: <GtkIconSet>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_icon_set";
end;

define C-function gtk-image-new-from-pixbuf
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_pixbuf";
end;

define C-function gtk-image-new-from-resource
  input parameter resource_path_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_resource";
end;

define C-function gtk-image-new-from-stock
  input parameter stock_id_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_new_from_stock";
end;

define C-function gtk-image-clear
  input parameter self :: <GtkImage>;
  c-name: "gtk_image_clear";
end;

define C-function gtk-image-get-animation
  input parameter self :: <GtkImage>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gtk_image_get_animation";
end;

define C-function gtk-image-get-gicon
  input parameter self :: <GtkImage>;
  output parameter gicon_ :: <GIcon*>;
  output parameter size_ :: <C-signed-int*>;
  c-name: "gtk_image_get_gicon";
end;

define C-function gtk-image-get-icon-name
  input parameter self :: <GtkImage>;
  output parameter icon_name_ :: <C-string>;
  output parameter size_ :: <C-signed-int*>;
  c-name: "gtk_image_get_icon_name";
end;

define C-function gtk-image-get-icon-set
  input parameter self :: <GtkImage>;
  output parameter icon_set_ :: <GtkIconSet>;
  output parameter size_ :: <C-signed-int*>;
  c-name: "gtk_image_get_icon_set";
end;

define C-function gtk-image-get-pixbuf
  input parameter self :: <GtkImage>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_image_get_pixbuf";
end;

define C-function gtk-image-get-pixel-size
  input parameter self :: <GtkImage>;
  result res :: <C-signed-int>;
  c-name: "gtk_image_get_pixel_size";
end;

define C-function gtk-image-get-stock
  input parameter self :: <GtkImage>;
  output parameter stock_id_ :: <C-string>;
  output parameter size_ :: <C-signed-int*>;
  c-name: "gtk_image_get_stock";
end;

define C-function gtk-image-get-storage-type
  input parameter self :: <GtkImage>;
  result res :: <GtkImageType>;
  c-name: "gtk_image_get_storage_type";
end;

define C-function gtk-image-set-from-animation
  input parameter self :: <GtkImage>;
  input parameter animation_ :: <GdkPixbufAnimation>;
  c-name: "gtk_image_set_from_animation";
end;

define C-function gtk-image-set-from-file
  input parameter self :: <GtkImage>;
  input parameter filename_ :: <C-string>;
  c-name: "gtk_image_set_from_file";
end;

define C-function gtk-image-set-from-gicon
  input parameter self :: <GtkImage>;
  input parameter icon_ :: <GIcon>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_image_set_from_gicon";
end;

define C-function gtk-image-set-from-icon-name
  input parameter self :: <GtkImage>;
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_image_set_from_icon_name";
end;

define C-function gtk-image-set-from-icon-set
  input parameter self :: <GtkImage>;
  input parameter icon_set_ :: <GtkIconSet>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_image_set_from_icon_set";
end;

define C-function gtk-image-set-from-pixbuf
  input parameter self :: <GtkImage>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_image_set_from_pixbuf";
end;

define C-function gtk-image-set-from-resource
  input parameter self :: <GtkImage>;
  input parameter resource_path_ :: <C-string>;
  c-name: "gtk_image_set_from_resource";
end;

define C-function gtk-image-set-from-stock
  input parameter self :: <GtkImage>;
  input parameter stock_id_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_image_set_from_stock";
end;

define C-function gtk-image-set-pixel-size
  input parameter self :: <GtkImage>;
  input parameter pixel_size_ :: <C-signed-int>;
  c-name: "gtk_image_set_pixel_size";
end;

define C-struct <_GtkImageClass>
  constant slot gtk-image-class-parent-class :: <GtkMiscClass>;
  constant slot gtk-image-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-image-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-image-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-image-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkImageClass>;
end C-struct;

define open C-subtype <GtkImageMenuItem> (<GtkMenuItem>)
  constant slot gtk-image-menu-item-menu-item :: <GtkMenuItem>;
  constant slot gtk-image-menu-item-priv :: <GtkImageMenuItemPrivate>;
end C-subtype;

define C-pointer-type <GtkImageMenuItem*> => <GtkImageMenuItem>;

define C-function gtk-image-menu-item-new
  result res :: <GtkWidget>;
  c-name: "gtk_image_menu_item_new";
end;

define C-function gtk-image-menu-item-new-from-stock
  input parameter stock_id_ :: <C-string>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_menu_item_new_from_stock";
end;

define C-function gtk-image-menu-item-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_menu_item_new_with_label";
end;

define C-function gtk-image-menu-item-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_menu_item_new_with_mnemonic";
end;

define C-function gtk-image-menu-item-get-always-show-image
  input parameter self :: <GtkImageMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_image_menu_item_get_always_show_image";
end;

define C-function gtk-image-menu-item-get-image
  input parameter self :: <GtkImageMenuItem>;
  result res :: <GtkWidget>;
  c-name: "gtk_image_menu_item_get_image";
end;

define C-function gtk-image-menu-item-get-use-stock
  input parameter self :: <GtkImageMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_image_menu_item_get_use_stock";
end;

define C-function gtk-image-menu-item-set-accel-group
  input parameter self :: <GtkImageMenuItem>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_image_menu_item_set_accel_group";
end;

define C-function gtk-image-menu-item-set-always-show-image
  input parameter self :: <GtkImageMenuItem>;
  input parameter always_show_ :: <C-boolean>;
  c-name: "gtk_image_menu_item_set_always_show_image";
end;

define C-function gtk-image-menu-item-set-image
  input parameter self :: <GtkImageMenuItem>;
  input parameter image_ :: <GtkWidget>;
  c-name: "gtk_image_menu_item_set_image";
end;

define C-function gtk-image-menu-item-set-use-stock
  input parameter self :: <GtkImageMenuItem>;
  input parameter use_stock_ :: <C-boolean>;
  c-name: "gtk_image_menu_item_set_use_stock";
end;

define C-struct <_GtkImageMenuItemClass>
  constant slot gtk-image-menu-item-class-parent-class :: <GtkMenuItemClass>;
  constant slot gtk-image-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-image-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-image-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-image-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkImageMenuItemClass>;
end C-struct;

define C-struct <_GtkImageMenuItemPrivate>
  pointer-type-name: <GtkImageMenuItemPrivate>;
end C-struct;

define C-struct <_GtkImagePrivate>
  pointer-type-name: <GtkImagePrivate>;
end C-struct;

define constant $gtk-image-empty = 0;
define constant $gtk-image-pixbuf = 1;
define constant $gtk-image-stock = 2;
define constant $gtk-image-icon-set = 3;
define constant $gtk-image-animation = 4;
define constant $gtk-image-icon-name = 5;
define constant $gtk-image-gicon = 6;
define constant <GtkImageType> = <C-int>;
define C-pointer-type <GtkImageType*> => <GtkImageType>;

define open C-subtype <GtkInfoBar> (<GtkBox>)
  constant slot gtk-info-bar-parent :: <GtkBox>;
  constant slot gtk-info-bar-priv :: <GtkInfoBarPrivate>;
end C-subtype;

define C-pointer-type <GtkInfoBar*> => <GtkInfoBar>;

define C-function gtk-info-bar-new
  result res :: <GtkWidget>;
  c-name: "gtk_info_bar_new";
end;

define C-function gtk-info-bar-add-action-widget
  input parameter self :: <GtkInfoBar>;
  input parameter child_ :: <GtkWidget>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_info_bar_add_action_widget";
end;

define C-function gtk-info-bar-add-button
  input parameter self :: <GtkInfoBar>;
  input parameter button_text_ :: <C-string>;
  input parameter response_id_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_info_bar_add_button";
end;

define C-function gtk-info-bar-get-action-area
  input parameter self :: <GtkInfoBar>;
  result res :: <GtkWidget>;
  c-name: "gtk_info_bar_get_action_area";
end;

define C-function gtk-info-bar-get-content-area
  input parameter self :: <GtkInfoBar>;
  result res :: <GtkWidget>;
  c-name: "gtk_info_bar_get_content_area";
end;

define C-function gtk-info-bar-get-message-type
  input parameter self :: <GtkInfoBar>;
  result res :: <GtkMessageType>;
  c-name: "gtk_info_bar_get_message_type";
end;

define C-function gtk-info-bar-response
  input parameter self :: <GtkInfoBar>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_info_bar_response";
end;

define C-function gtk-info-bar-set-default-response
  input parameter self :: <GtkInfoBar>;
  input parameter response_id_ :: <C-signed-int>;
  c-name: "gtk_info_bar_set_default_response";
end;

define C-function gtk-info-bar-set-message-type
  input parameter self :: <GtkInfoBar>;
  input parameter message_type_ :: <GtkMessageType>;
  c-name: "gtk_info_bar_set_message_type";
end;

define C-function gtk-info-bar-set-response-sensitive
  input parameter self :: <GtkInfoBar>;
  input parameter response_id_ :: <C-signed-int>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_info_bar_set_response_sensitive";
end;

define C-struct <_GtkInfoBarClass>
  constant slot gtk-info-bar-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-info-bar-class-response :: <C-function-pointer>;
  constant slot gtk-info-bar-class-close :: <C-function-pointer>;
  constant slot gtk-info-bar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-info-bar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-info-bar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-info-bar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkInfoBarClass>;
end C-struct;

define C-struct <_GtkInfoBarPrivate>
  pointer-type-name: <GtkInfoBarPrivate>;
end C-struct;

define open C-subtype <GtkInvisible> (<GtkWidget>)
  constant slot gtk-invisible-widget :: <GtkWidget>;
  constant slot gtk-invisible-priv :: <GtkInvisiblePrivate>;
end C-subtype;

define C-pointer-type <GtkInvisible*> => <GtkInvisible>;

define C-function gtk-invisible-new
  result res :: <GtkWidget>;
  c-name: "gtk_invisible_new";
end;

define C-function gtk-invisible-new-for-screen
  input parameter screen_ :: <GdkScreen>;
  result res :: <GtkWidget>;
  c-name: "gtk_invisible_new_for_screen";
end;

define C-function gtk-invisible-get-screen
  input parameter self :: <GtkInvisible>;
  result res :: <GdkScreen>;
  c-name: "gtk_invisible_get_screen";
end;

define C-function gtk-invisible-set-screen
  input parameter self :: <GtkInvisible>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_invisible_set_screen";
end;

define C-struct <_GtkInvisibleClass>
  constant slot gtk-invisible-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-invisible-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-invisible-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-invisible-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-invisible-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkInvisibleClass>;
end C-struct;

define C-struct <_GtkInvisiblePrivate>
  pointer-type-name: <GtkInvisiblePrivate>;
end C-struct;

define constant $gtk-junction-none = 0;
define constant $gtk-junction-corner-topleft = 1;
define constant $gtk-junction-corner-topright = 2;
define constant $gtk-junction-corner-bottomleft = 4;
define constant $gtk-junction-corner-bottomright = 8;
define constant $gtk-junction-top = 3;
define constant $gtk-junction-bottom = 12;
define constant $gtk-junction-left = 5;
define constant $gtk-junction-right = 10;
define constant <GtkJunctionSides> = <C-int>;
define C-pointer-type <GtkJunctionSides*> => <GtkJunctionSides>;

define constant $gtk-justify-left = 0;
define constant $gtk-justify-right = 1;
define constant $gtk-justify-center = 2;
define constant $gtk-justify-fill = 3;
define constant <GtkJustification> = <C-int>;
define C-pointer-type <GtkJustification*> => <GtkJustification>;

define open C-subtype <GtkLabel> (<GtkMisc>)
  constant slot gtk-label-misc :: <GtkMisc>;
  constant slot gtk-label-priv :: <GtkLabelPrivate>;
end C-subtype;

define C-pointer-type <GtkLabel*> => <GtkLabel>;

define C-function gtk-label-new
  input parameter str_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_label_new";
end;

define C-function gtk-label-new-with-mnemonic
  input parameter str_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_label_new_with_mnemonic";
end;

define C-function gtk-label-get-angle
  input parameter self :: <GtkLabel>;
  result res :: <C-double>;
  c-name: "gtk_label_get_angle";
end;

define C-function gtk-label-get-attributes
  input parameter self :: <GtkLabel>;
  result res :: <PangoAttrList>;
  c-name: "gtk_label_get_attributes";
end;

define C-function gtk-label-get-current-uri
  input parameter self :: <GtkLabel>;
  result res :: <C-string>;
  c-name: "gtk_label_get_current_uri";
end;

define C-function gtk-label-get-ellipsize
  input parameter self :: <GtkLabel>;
  result res :: <PangoEllipsizeMode>;
  c-name: "gtk_label_get_ellipsize";
end;

define C-function gtk-label-get-justify
  input parameter self :: <GtkLabel>;
  result res :: <GtkJustification>;
  c-name: "gtk_label_get_justify";
end;

define C-function gtk-label-get-label
  input parameter self :: <GtkLabel>;
  result res :: <C-string>;
  c-name: "gtk_label_get_label";
end;

define C-function gtk-label-get-layout
  input parameter self :: <GtkLabel>;
  result res :: <PangoLayout>;
  c-name: "gtk_label_get_layout";
end;

define C-function gtk-label-get-layout-offsets
  input parameter self :: <GtkLabel>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gtk_label_get_layout_offsets";
end;

define C-function gtk-label-get-line-wrap
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_line_wrap";
end;

define C-function gtk-label-get-line-wrap-mode
  input parameter self :: <GtkLabel>;
  result res :: <PangoWrapMode>;
  c-name: "gtk_label_get_line_wrap_mode";
end;

define C-function gtk-label-get-max-width-chars
  input parameter self :: <GtkLabel>;
  result res :: <C-signed-int>;
  c-name: "gtk_label_get_max_width_chars";
end;

define C-function gtk-label-get-mnemonic-keyval
  input parameter self :: <GtkLabel>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_label_get_mnemonic_keyval";
end;

define C-function gtk-label-get-mnemonic-widget
  input parameter self :: <GtkLabel>;
  result res :: <GtkWidget>;
  c-name: "gtk_label_get_mnemonic_widget";
end;

define C-function gtk-label-get-selectable
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_selectable";
end;

define C-function gtk-label-get-selection-bounds
  input parameter self :: <GtkLabel>;
  output parameter start_ :: <C-signed-int*>;
  output parameter end_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_selection_bounds";
end;

define C-function gtk-label-get-single-line-mode
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_single_line_mode";
end;

define C-function gtk-label-get-text
  input parameter self :: <GtkLabel>;
  result res :: <C-string>;
  c-name: "gtk_label_get_text";
end;

define C-function gtk-label-get-track-visited-links
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_track_visited_links";
end;

define C-function gtk-label-get-use-markup
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_use_markup";
end;

define C-function gtk-label-get-use-underline
  input parameter self :: <GtkLabel>;
  result res :: <C-boolean>;
  c-name: "gtk_label_get_use_underline";
end;

define C-function gtk-label-get-width-chars
  input parameter self :: <GtkLabel>;
  result res :: <C-signed-int>;
  c-name: "gtk_label_get_width_chars";
end;

define C-function gtk-label-select-region
  input parameter self :: <GtkLabel>;
  input parameter start_offset_ :: <C-signed-int>;
  input parameter end_offset_ :: <C-signed-int>;
  c-name: "gtk_label_select_region";
end;

define C-function gtk-label-set-angle
  input parameter self :: <GtkLabel>;
  input parameter angle_ :: <C-double>;
  c-name: "gtk_label_set_angle";
end;

define C-function gtk-label-set-attributes
  input parameter self :: <GtkLabel>;
  input parameter attrs_ :: <PangoAttrList>;
  c-name: "gtk_label_set_attributes";
end;

define C-function gtk-label-set-ellipsize
  input parameter self :: <GtkLabel>;
  input parameter mode_ :: <PangoEllipsizeMode>;
  c-name: "gtk_label_set_ellipsize";
end;

define C-function gtk-label-set-justify
  input parameter self :: <GtkLabel>;
  input parameter jtype_ :: <GtkJustification>;
  c-name: "gtk_label_set_justify";
end;

define C-function gtk-label-set-label
  input parameter self :: <GtkLabel>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_label_set_label";
end;

define C-function gtk-label-set-line-wrap
  input parameter self :: <GtkLabel>;
  input parameter wrap_ :: <C-boolean>;
  c-name: "gtk_label_set_line_wrap";
end;

define C-function gtk-label-set-line-wrap-mode
  input parameter self :: <GtkLabel>;
  input parameter wrap_mode_ :: <PangoWrapMode>;
  c-name: "gtk_label_set_line_wrap_mode";
end;

define C-function gtk-label-set-markup
  input parameter self :: <GtkLabel>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_label_set_markup";
end;

define C-function gtk-label-set-markup-with-mnemonic
  input parameter self :: <GtkLabel>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_label_set_markup_with_mnemonic";
end;

define C-function gtk-label-set-max-width-chars
  input parameter self :: <GtkLabel>;
  input parameter n_chars_ :: <C-signed-int>;
  c-name: "gtk_label_set_max_width_chars";
end;

define C-function gtk-label-set-mnemonic-widget
  input parameter self :: <GtkLabel>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_label_set_mnemonic_widget";
end;

define C-function gtk-label-set-pattern
  input parameter self :: <GtkLabel>;
  input parameter pattern_ :: <C-string>;
  c-name: "gtk_label_set_pattern";
end;

define C-function gtk-label-set-selectable
  input parameter self :: <GtkLabel>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_label_set_selectable";
end;

define C-function gtk-label-set-single-line-mode
  input parameter self :: <GtkLabel>;
  input parameter single_line_mode_ :: <C-boolean>;
  c-name: "gtk_label_set_single_line_mode";
end;

define C-function gtk-label-set-text
  input parameter self :: <GtkLabel>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_label_set_text";
end;

define C-function gtk-label-set-text-with-mnemonic
  input parameter self :: <GtkLabel>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_label_set_text_with_mnemonic";
end;

define C-function gtk-label-set-track-visited-links
  input parameter self :: <GtkLabel>;
  input parameter track_links_ :: <C-boolean>;
  c-name: "gtk_label_set_track_visited_links";
end;

define C-function gtk-label-set-use-markup
  input parameter self :: <GtkLabel>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_label_set_use_markup";
end;

define C-function gtk-label-set-use-underline
  input parameter self :: <GtkLabel>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_label_set_use_underline";
end;

define C-function gtk-label-set-width-chars
  input parameter self :: <GtkLabel>;
  input parameter n_chars_ :: <C-signed-int>;
  c-name: "gtk_label_set_width_chars";
end;

define C-struct <_GtkLabelClass>
  constant slot gtk-label-class-parent-class :: <GtkMiscClass>;
  constant slot gtk-label-class-move-cursor :: <C-function-pointer>;
  constant slot gtk-label-class-copy-clipboard :: <C-function-pointer>;
  constant slot gtk-label-class-populate-popup :: <C-function-pointer>;
  constant slot gtk-label-class-activate-link :: <C-function-pointer>;
  constant slot gtk-label-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-label-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkLabelClass>;
end C-struct;

define C-struct <_GtkLabelPrivate>
  pointer-type-name: <GtkLabelPrivate>;
end C-struct;

define C-struct <_GtkLabelSelectionInfo>
  pointer-type-name: <GtkLabelSelectionInfo>;
end C-struct;

define open C-subtype <GtkLayout> (<GtkContainer>)
  constant slot gtk-layout-container :: <GtkContainer>;
  constant slot gtk-layout-priv :: <GtkLayoutPrivate>;
end C-subtype;

define C-pointer-type <GtkLayout*> => <GtkLayout>;

define C-function gtk-layout-new
  input parameter hadjustment_ :: <GtkAdjustment>;
  input parameter vadjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_layout_new";
end;

define C-function gtk-layout-get-bin-window
  input parameter self :: <GtkLayout>;
  result res :: <GdkWindow>;
  c-name: "gtk_layout_get_bin_window";
end;

define C-function gtk-layout-get-hadjustment
  input parameter self :: <GtkLayout>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_layout_get_hadjustment";
end;

define C-function gtk-layout-get-size
  input parameter self :: <GtkLayout>;
  output parameter width_ :: <C-unsigned-int*>;
  output parameter height_ :: <C-unsigned-int*>;
  c-name: "gtk_layout_get_size";
end;

define C-function gtk-layout-get-vadjustment
  input parameter self :: <GtkLayout>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_layout_get_vadjustment";
end;

define C-function gtk-layout-move
  input parameter self :: <GtkLayout>;
  input parameter child_widget_ :: <GtkWidget>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_layout_move";
end;

define C-function gtk-layout-put
  input parameter self :: <GtkLayout>;
  input parameter child_widget_ :: <GtkWidget>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_layout_put";
end;

define C-function gtk-layout-set-hadjustment
  input parameter self :: <GtkLayout>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_layout_set_hadjustment";
end;

define C-function gtk-layout-set-size
  input parameter self :: <GtkLayout>;
  input parameter width_ :: <C-unsigned-int>;
  input parameter height_ :: <C-unsigned-int>;
  c-name: "gtk_layout_set_size";
end;

define C-function gtk-layout-set-vadjustment
  input parameter self :: <GtkLayout>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_layout_set_vadjustment";
end;

define C-struct <_GtkLayoutClass>
  constant slot gtk-layout-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-layout-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-layout-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-layout-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-layout-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkLayoutClass>;
end C-struct;

define C-struct <_GtkLayoutPrivate>
  pointer-type-name: <GtkLayoutPrivate>;
end C-struct;

define constant $gtk-license-unknown = 0;
define constant $gtk-license-custom = 1;
define constant $gtk-license-gpl-2-0 = 2;
define constant $gtk-license-gpl-3-0 = 3;
define constant $gtk-license-lgpl-2-1 = 4;
define constant $gtk-license-lgpl-3-0 = 5;
define constant $gtk-license-bsd = 6;
define constant $gtk-license-mit-x11 = 7;
define constant $gtk-license-artistic = 8;
define constant <GtkLicense> = <C-int>;
define C-pointer-type <GtkLicense*> => <GtkLicense>;

define open C-subtype <GtkLinkButton> (<GtkButton>)
  constant slot gtk-link-button-parent-instance :: <GtkButton>;
  constant slot gtk-link-button-priv :: <GtkLinkButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkLinkButton*> => <GtkLinkButton>;

define C-function gtk-link-button-new
  input parameter uri_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_link_button_new";
end;

define C-function gtk-link-button-new-with-label
  input parameter uri_ :: <C-string>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_link_button_new_with_label";
end;

define C-function gtk-link-button-get-uri
  input parameter self :: <GtkLinkButton>;
  result res :: <C-string>;
  c-name: "gtk_link_button_get_uri";
end;

define C-function gtk-link-button-get-visited
  input parameter self :: <GtkLinkButton>;
  result res :: <C-boolean>;
  c-name: "gtk_link_button_get_visited";
end;

define C-function gtk-link-button-set-uri
  input parameter self :: <GtkLinkButton>;
  input parameter uri_ :: <C-string>;
  c-name: "gtk_link_button_set_uri";
end;

define C-function gtk-link-button-set-visited
  input parameter self :: <GtkLinkButton>;
  input parameter visited_ :: <C-boolean>;
  c-name: "gtk_link_button_set_visited";
end;

define C-struct <_GtkLinkButtonClass>
  constant slot gtk-link-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-link-button-class-activate-link :: <C-function-pointer>;
  constant slot gtk-link-button-class-_gtk-padding1 :: <C-void*>;
  constant slot gtk-link-button-class-_gtk-padding2 :: <C-void*>;
  constant slot gtk-link-button-class-_gtk-padding3 :: <C-void*>;
  constant slot gtk-link-button-class-_gtk-padding4 :: <C-void*>;
  pointer-type-name: <GtkLinkButtonClass>;
end C-struct;

define C-struct <_GtkLinkButtonPrivate>
  pointer-type-name: <GtkLinkButtonPrivate>;
end C-struct;

define open C-subtype <GtkListStore> (<GObject>)
  constant slot gtk-list-store-parent :: <GObject>;
  constant slot gtk-list-store-priv :: <GtkListStorePrivate>;
end C-subtype;

define C-pointer-type <GtkListStore*> => <GtkListStore>;

define C-function gtk-list-store-newv
  input parameter n_columns_ :: <C-signed-int>;
  input parameter types_ :: <C-long*>;
  result res :: <GtkListStore>;
  c-name: "gtk_list_store_newv";
end;

define C-function gtk-list-store-append
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_append";
end;

define C-function gtk-list-store-clear
  input parameter self :: <GtkListStore>;
  c-name: "gtk_list_store_clear";
end;

define C-function gtk-list-store-insert
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_list_store_insert";
end;

define C-function gtk-list-store-insert-after
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter sibling_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_insert_after";
end;

define C-function gtk-list-store-insert-before
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter sibling_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_insert_before";
end;

define C-function gtk-list-store-insert-with-valuesv
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <C-signed-int>;
  input parameter columns_ :: <C-signed-int*>;
  input parameter values_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_values_ :: <C-signed-int>;
  c-name: "gtk_list_store_insert_with_valuesv";
end;

define C-function gtk-list-store-iter-is-valid
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_list_store_iter_is_valid";
end;

define C-function gtk-list-store-move-after
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_move_after";
end;

define C-function gtk-list-store-move-before
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_move_before";
end;

define C-function gtk-list-store-prepend
  input parameter self :: <GtkListStore>;
  output parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_prepend";
end;

define C-function gtk-list-store-remove
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_list_store_remove";
end;

define C-function gtk-list-store-set-column-types
  input parameter self :: <GtkListStore>;
  input parameter n_columns_ :: <C-signed-int>;
  input parameter types_ :: <C-long*>;
  c-name: "gtk_list_store_set_column_types";
end;

define C-function gtk-list-store-set-value
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter column_ :: <C-signed-int>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_list_store_set_value";
end;

define C-function gtk-list-store-set-valuesv
  input parameter self :: <GtkListStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter columns_ :: <C-signed-int*>;
  input parameter values_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_values_ :: <C-signed-int>;
  c-name: "gtk_list_store_set_valuesv";
end;

define C-function gtk-list-store-swap
  input parameter self :: <GtkListStore>;
  input parameter a_ :: <GtkTreeIter>;
  input parameter b_ :: <GtkTreeIter>;
  c-name: "gtk_list_store_swap";
end;

define C-struct <_GtkListStoreClass>
  constant slot gtk-list-store-class-parent-class :: <GObjectClass>;
  constant slot gtk-list-store-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-list-store-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-list-store-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-list-store-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkListStoreClass>;
end C-struct;

define C-struct <_GtkListStorePrivate>
  pointer-type-name: <GtkListStorePrivate>;
end C-struct;

define open C-subtype <GtkLockButton> (<GtkButton>)
  constant slot gtk-lock-button-parent :: <GtkButton>;
  constant slot gtk-lock-button-priv :: <GtkLockButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkLockButton*> => <GtkLockButton>;

define C-function gtk-lock-button-new
  input parameter permission_ :: <GPermission>;
  result res :: <GtkWidget>;
  c-name: "gtk_lock_button_new";
end;

define C-function gtk-lock-button-get-permission
  input parameter self :: <GtkLockButton>;
  result res :: <GPermission>;
  c-name: "gtk_lock_button_get_permission";
end;

define C-function gtk-lock-button-set-permission
  input parameter self :: <GtkLockButton>;
  input parameter permission_ :: <GPermission>;
  c-name: "gtk_lock_button_set_permission";
end;

define C-struct <_GtkLockButtonClass>
  constant slot gtk-lock-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-lock-button-class-reserved0 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved1 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved2 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved3 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved4 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved5 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved6 :: <C-function-pointer>;
  constant slot gtk-lock-button-class-reserved7 :: <C-function-pointer>;
  pointer-type-name: <GtkLockButtonClass>;
end C-struct;

define C-struct <_GtkLockButtonPrivate>
  pointer-type-name: <GtkLockButtonPrivate>;
end C-struct;

define constant $gtk-major-version = 3;

define constant $max-compose-len = 7;

define constant $gtk-micro-version = 2;

define constant $gtk-minor-version = 4;

define open C-subtype <GtkMenu> (<GtkMenuShell>)
  constant slot gtk-menu-menu-shell :: <GtkMenuShell>;
  constant slot gtk-menu-priv :: <GtkMenuPrivate>;
end C-subtype;

define C-pointer-type <GtkMenu*> => <GtkMenu>;

define C-function gtk-menu-new
  result res :: <GtkWidget>;
  c-name: "gtk_menu_new";
end;

define C-function gtk-menu-new-from-model
  input parameter model_ :: <GMenuModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_new_from_model";
end;

define C-function gtk-menu-get-for-attach-widget
  input parameter widget_ :: <GtkWidget>;
  result res :: <GList>;
  c-name: "gtk_menu_get_for_attach_widget";
end;

define C-function gtk-menu-attach
  input parameter self :: <GtkMenu>;
  input parameter child_ :: <GtkWidget>;
  input parameter left_attach_ :: <C-unsigned-int>;
  input parameter right_attach_ :: <C-unsigned-int>;
  input parameter top_attach_ :: <C-unsigned-int>;
  input parameter bottom_attach_ :: <C-unsigned-int>;
  c-name: "gtk_menu_attach";
end;

define C-function gtk-menu-attach-to-widget
  input parameter self :: <GtkMenu>;
  input parameter attach_widget_ :: <GtkWidget>;
  input parameter detacher_ :: <C-function-pointer>;
  c-name: "gtk_menu_attach_to_widget";
end;

define C-function gtk-menu-detach
  input parameter self :: <GtkMenu>;
  c-name: "gtk_menu_detach";
end;

define C-function gtk-menu-get-accel-group
  input parameter self :: <GtkMenu>;
  result res :: <GtkAccelGroup>;
  c-name: "gtk_menu_get_accel_group";
end;

define C-function gtk-menu-get-accel-path
  input parameter self :: <GtkMenu>;
  result res :: <C-string>;
  c-name: "gtk_menu_get_accel_path";
end;

define C-function gtk-menu-get-active
  input parameter self :: <GtkMenu>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_get_active";
end;

define C-function gtk-menu-get-attach-widget
  input parameter self :: <GtkMenu>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_get_attach_widget";
end;

define C-function gtk-menu-get-monitor
  input parameter self :: <GtkMenu>;
  result res :: <C-signed-int>;
  c-name: "gtk_menu_get_monitor";
end;

define C-function gtk-menu-get-reserve-toggle-size
  input parameter self :: <GtkMenu>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_get_reserve_toggle_size";
end;

define C-function gtk-menu-get-tearoff-state
  input parameter self :: <GtkMenu>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_get_tearoff_state";
end;

define C-function gtk-menu-get-title
  input parameter self :: <GtkMenu>;
  result res :: <C-string>;
  c-name: "gtk_menu_get_title";
end;

define C-function gtk-menu-popdown
  input parameter self :: <GtkMenu>;
  c-name: "gtk_menu_popdown";
end;

define C-function gtk-menu-popup-for-device
  input parameter self :: <GtkMenu>;
  input parameter device_ :: <GdkDevice>;
  input parameter parent_menu_shell_ :: <GtkWidget>;
  input parameter parent_menu_item_ :: <GtkWidget>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  input parameter button_ :: <C-unsigned-int>;
  input parameter activate_time_ :: <C-unsigned-int>;
  c-name: "gtk_menu_popup_for_device";
end;

define C-function gtk-menu-reorder-child
  input parameter self :: <GtkMenu>;
  input parameter child_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_menu_reorder_child";
end;

define C-function gtk-menu-reposition
  input parameter self :: <GtkMenu>;
  c-name: "gtk_menu_reposition";
end;

define C-function gtk-menu-set-accel-group
  input parameter self :: <GtkMenu>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_menu_set_accel_group";
end;

define C-function gtk-menu-set-accel-path
  input parameter self :: <GtkMenu>;
  input parameter accel_path_ :: <C-string>;
  c-name: "gtk_menu_set_accel_path";
end;

define C-function gtk-menu-set-active
  input parameter self :: <GtkMenu>;
  input parameter index_ :: <C-unsigned-int>;
  c-name: "gtk_menu_set_active";
end;

define C-function gtk-menu-set-monitor
  input parameter self :: <GtkMenu>;
  input parameter monitor_num_ :: <C-signed-int>;
  c-name: "gtk_menu_set_monitor";
end;

define C-function gtk-menu-set-reserve-toggle-size
  input parameter self :: <GtkMenu>;
  input parameter reserve_toggle_size_ :: <C-boolean>;
  c-name: "gtk_menu_set_reserve_toggle_size";
end;

define C-function gtk-menu-set-screen
  input parameter self :: <GtkMenu>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_menu_set_screen";
end;

define C-function gtk-menu-set-tearoff-state
  input parameter self :: <GtkMenu>;
  input parameter torn_off_ :: <C-boolean>;
  c-name: "gtk_menu_set_tearoff_state";
end;

define C-function gtk-menu-set-title
  input parameter self :: <GtkMenu>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_menu_set_title";
end;

define open C-subtype <GtkMenuBar> (<GtkMenuShell>)
  constant slot gtk-menu-bar-menu-shell :: <GtkMenuShell>;
  constant slot gtk-menu-bar-priv :: <GtkMenuBarPrivate>;
end C-subtype;

define C-pointer-type <GtkMenuBar*> => <GtkMenuBar>;

define C-function gtk-menu-bar-new
  result res :: <GtkWidget>;
  c-name: "gtk_menu_bar_new";
end;

define C-function gtk-menu-bar-new-from-model
  input parameter model_ :: <GMenuModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_bar_new_from_model";
end;

define C-function gtk-menu-bar-get-child-pack-direction
  input parameter self :: <GtkMenuBar>;
  result res :: <GtkPackDirection>;
  c-name: "gtk_menu_bar_get_child_pack_direction";
end;

define C-function gtk-menu-bar-get-pack-direction
  input parameter self :: <GtkMenuBar>;
  result res :: <GtkPackDirection>;
  c-name: "gtk_menu_bar_get_pack_direction";
end;

define C-function gtk-menu-bar-set-child-pack-direction
  input parameter self :: <GtkMenuBar>;
  input parameter child_pack_dir_ :: <GtkPackDirection>;
  c-name: "gtk_menu_bar_set_child_pack_direction";
end;

define C-function gtk-menu-bar-set-pack-direction
  input parameter self :: <GtkMenuBar>;
  input parameter pack_dir_ :: <GtkPackDirection>;
  c-name: "gtk_menu_bar_set_pack_direction";
end;

define C-struct <_GtkMenuBarClass>
  constant slot gtk-menu-bar-class-parent-class :: <GtkMenuShellClass>;
  constant slot gtk-menu-bar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-menu-bar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-menu-bar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-menu-bar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMenuBarClass>;
end C-struct;

define C-struct <_GtkMenuBarPrivate>
  pointer-type-name: <GtkMenuBarPrivate>;
end C-struct;

define C-struct <_GtkMenuClass>
  constant slot gtk-menu-class-parent-class :: <GtkMenuShellClass>;
  constant slot gtk-menu-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-menu-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-menu-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-menu-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMenuClass>;
end C-struct;

define constant $gtk-menu-dir-parent = 0;
define constant $gtk-menu-dir-child = 1;
define constant $gtk-menu-dir-next = 2;
define constant $gtk-menu-dir-prev = 3;
define constant <GtkMenuDirectionType> = <C-int>;
define C-pointer-type <GtkMenuDirectionType*> => <GtkMenuDirectionType>;

define open C-subtype <GtkMenuItem> (<GtkBin>)
  constant slot gtk-menu-item-bin :: <GtkBin>;
  constant slot gtk-menu-item-priv :: <GtkMenuItemPrivate>;
end C-subtype;

define C-pointer-type <GtkMenuItem*> => <GtkMenuItem>;

define C-function gtk-menu-item-new
  result res :: <GtkWidget>;
  c-name: "gtk_menu_item_new";
end;

define C-function gtk-menu-item-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_item_new_with_label";
end;

define C-function gtk-menu-item-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_item_new_with_mnemonic";
end;

define C-function gtk-menu-item-activate
  input parameter self :: <GtkMenuItem>;
  c-name: "gtk_menu_item_activate";
end;

define C-function gtk-menu-item-deselect
  input parameter self :: <GtkMenuItem>;
  c-name: "gtk_menu_item_deselect";
end;

define C-function gtk-menu-item-get-accel-path
  input parameter self :: <GtkMenuItem>;
  result res :: <C-string>;
  c-name: "gtk_menu_item_get_accel_path";
end;

define C-function gtk-menu-item-get-label
  input parameter self :: <GtkMenuItem>;
  result res :: <C-string>;
  c-name: "gtk_menu_item_get_label";
end;

define C-function gtk-menu-item-get-reserve-indicator
  input parameter self :: <GtkMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_item_get_reserve_indicator";
end;

define C-function gtk-menu-item-get-right-justified
  input parameter self :: <GtkMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_item_get_right_justified";
end;

define C-function gtk-menu-item-get-submenu
  input parameter self :: <GtkMenuItem>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_item_get_submenu";
end;

define C-function gtk-menu-item-get-use-underline
  input parameter self :: <GtkMenuItem>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_item_get_use_underline";
end;

define C-function gtk-menu-item-select
  input parameter self :: <GtkMenuItem>;
  c-name: "gtk_menu_item_select";
end;

define C-function gtk-menu-item-set-accel-path
  input parameter self :: <GtkMenuItem>;
  input parameter accel_path_ :: <C-string>;
  c-name: "gtk_menu_item_set_accel_path";
end;

define C-function gtk-menu-item-set-label
  input parameter self :: <GtkMenuItem>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_menu_item_set_label";
end;

define C-function gtk-menu-item-set-reserve-indicator
  input parameter self :: <GtkMenuItem>;
  input parameter reserve_ :: <C-boolean>;
  c-name: "gtk_menu_item_set_reserve_indicator";
end;

define C-function gtk-menu-item-set-right-justified
  input parameter self :: <GtkMenuItem>;
  input parameter right_justified_ :: <C-boolean>;
  c-name: "gtk_menu_item_set_right_justified";
end;

define C-function gtk-menu-item-set-submenu
  input parameter self :: <GtkMenuItem>;
  input parameter submenu_ :: <GtkWidget>;
  c-name: "gtk_menu_item_set_submenu";
end;

define C-function gtk-menu-item-set-use-underline
  input parameter self :: <GtkMenuItem>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_menu_item_set_use_underline";
end;

define C-function gtk-menu-item-toggle-size-allocate
  input parameter self :: <GtkMenuItem>;
  input parameter allocation_ :: <C-signed-int>;
  c-name: "gtk_menu_item_toggle_size_allocate";
end;

define C-function gtk-menu-item-toggle-size-request
  input parameter self :: <GtkMenuItem>;
  input parameter requisition_ :: <C-signed-int*>;
  c-name: "gtk_menu_item_toggle_size_request";
end;

define C-struct <_GtkMenuItemClass>
  constant slot gtk-menu-item-class-parent-class :: <GtkBinClass>;
  constant slot gtk-menu-item-class-hide-on-activate :: <C-unsigned-int>;
  constant slot gtk-menu-item-class-activate :: <C-function-pointer>;
  constant slot gtk-menu-item-class-activate-item :: <C-function-pointer>;
  constant slot gtk-menu-item-class-toggle-size-request :: <C-function-pointer>;
  constant slot gtk-menu-item-class-toggle-size-allocate :: <C-function-pointer>;
  constant slot gtk-menu-item-class-set-label :: <C-function-pointer>;
  constant slot gtk-menu-item-class-get-label :: <C-function-pointer>;
  constant slot gtk-menu-item-class-select :: <C-function-pointer>;
  constant slot gtk-menu-item-class-deselect :: <C-function-pointer>;
  constant slot gtk-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMenuItemClass>;
end C-struct;

define C-struct <_GtkMenuItemPrivate>
  pointer-type-name: <GtkMenuItemPrivate>;
end C-struct;

define C-struct <_GtkMenuPrivate>
  pointer-type-name: <GtkMenuPrivate>;
end C-struct;

define open C-subtype <GtkMenuShell> (<GtkContainer>)
  constant slot gtk-menu-shell-container :: <GtkContainer>;
  constant slot gtk-menu-shell-priv :: <GtkMenuShellPrivate>;
end C-subtype;

define C-pointer-type <GtkMenuShell*> => <GtkMenuShell>;

define C-function gtk-menu-shell-activate-item
  input parameter self :: <GtkMenuShell>;
  input parameter menu_item_ :: <GtkWidget>;
  input parameter force_deactivate_ :: <C-boolean>;
  c-name: "gtk_menu_shell_activate_item";
end;

define C-function gtk-menu-shell-append
  input parameter self :: <GtkMenuShell>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_menu_shell_append";
end;

define C-function gtk-menu-shell-cancel
  input parameter self :: <GtkMenuShell>;
  c-name: "gtk_menu_shell_cancel";
end;

define C-function gtk-menu-shell-deactivate
  input parameter self :: <GtkMenuShell>;
  c-name: "gtk_menu_shell_deactivate";
end;

define C-function gtk-menu-shell-deselect
  input parameter self :: <GtkMenuShell>;
  c-name: "gtk_menu_shell_deselect";
end;

define C-function gtk-menu-shell-get-parent-shell
  input parameter self :: <GtkMenuShell>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_shell_get_parent_shell";
end;

define C-function gtk-menu-shell-get-selected-item
  input parameter self :: <GtkMenuShell>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_shell_get_selected_item";
end;

define C-function gtk-menu-shell-get-take-focus
  input parameter self :: <GtkMenuShell>;
  result res :: <C-boolean>;
  c-name: "gtk_menu_shell_get_take_focus";
end;

define C-function gtk-menu-shell-insert
  input parameter self :: <GtkMenuShell>;
  input parameter child_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_menu_shell_insert";
end;

define C-function gtk-menu-shell-prepend
  input parameter self :: <GtkMenuShell>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_menu_shell_prepend";
end;

define C-function gtk-menu-shell-select-first
  input parameter self :: <GtkMenuShell>;
  input parameter search_sensitive_ :: <C-boolean>;
  c-name: "gtk_menu_shell_select_first";
end;

define C-function gtk-menu-shell-select-item
  input parameter self :: <GtkMenuShell>;
  input parameter menu_item_ :: <GtkWidget>;
  c-name: "gtk_menu_shell_select_item";
end;

define C-function gtk-menu-shell-set-take-focus
  input parameter self :: <GtkMenuShell>;
  input parameter take_focus_ :: <C-boolean>;
  c-name: "gtk_menu_shell_set_take_focus";
end;

define C-struct <_GtkMenuShellClass>
  constant slot gtk-menu-shell-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-menu-shell-class-submenu-placement :: <C-unsigned-int>;
  constant slot gtk-menu-shell-class-deactivate :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-selection-done :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-move-current :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-activate-current :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-cancel :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-select-item :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-insert :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-get-popup-delay :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-move-selected :: <C-function-pointer>;
  constant slot gtk-menu-shell-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-menu-shell-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-menu-shell-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-menu-shell-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMenuShellClass>;
end C-struct;

define C-struct <_GtkMenuShellPrivate>
  pointer-type-name: <GtkMenuShellPrivate>;
end C-struct;

define open C-subtype <GtkMenuToolButton> (<GtkToolButton>)
  constant slot gtk-menu-tool-button-parent :: <GtkToolButton>;
  constant slot gtk-menu-tool-button-priv :: <GtkMenuToolButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkMenuToolButton*> => <GtkMenuToolButton>;

define C-function gtk-menu-tool-button-new
  input parameter icon_widget_ :: <GtkWidget>;
  input parameter label_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_menu_tool_button_new";
end;

define C-function gtk-menu-tool-button-new-from-stock
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_menu_tool_button_new_from_stock";
end;

define C-function gtk-menu-tool-button-get-menu
  input parameter self :: <GtkMenuToolButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_menu_tool_button_get_menu";
end;

define C-function gtk-menu-tool-button-set-arrow-tooltip-markup
  input parameter self :: <GtkMenuToolButton>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_menu_tool_button_set_arrow_tooltip_markup";
end;

define C-function gtk-menu-tool-button-set-arrow-tooltip-text
  input parameter self :: <GtkMenuToolButton>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_menu_tool_button_set_arrow_tooltip_text";
end;

define C-function gtk-menu-tool-button-set-menu
  input parameter self :: <GtkMenuToolButton>;
  input parameter menu_ :: <GtkWidget>;
  c-name: "gtk_menu_tool_button_set_menu";
end;

define C-struct <_GtkMenuToolButtonClass>
  constant slot gtk-menu-tool-button-class-parent-class :: <GtkToolButtonClass>;
  constant slot gtk-menu-tool-button-class-show-menu :: <C-function-pointer>;
  constant slot gtk-menu-tool-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-menu-tool-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-menu-tool-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-menu-tool-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMenuToolButtonClass>;
end C-struct;

define C-struct <_GtkMenuToolButtonPrivate>
  pointer-type-name: <GtkMenuToolButtonPrivate>;
end C-struct;

define open C-subtype <GtkMessageDialog> (<GtkDialog>)
  constant slot gtk-message-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-message-dialog-priv :: <GtkMessageDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkMessageDialog*> => <GtkMessageDialog>;

define C-function gtk-message-dialog-get-image
  input parameter self :: <GtkMessageDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_message_dialog_get_image";
end;

define C-function gtk-message-dialog-get-message-area
  input parameter self :: <GtkMessageDialog>;
  result res :: <GtkWidget>;
  c-name: "gtk_message_dialog_get_message_area";
end;

define C-function gtk-message-dialog-set-image
  input parameter self :: <GtkMessageDialog>;
  input parameter image_ :: <GtkWidget>;
  c-name: "gtk_message_dialog_set_image";
end;

define C-function gtk-message-dialog-set-markup
  input parameter self :: <GtkMessageDialog>;
  input parameter str_ :: <C-string>;
  c-name: "gtk_message_dialog_set_markup";
end;

define C-struct <_GtkMessageDialogClass>
  constant slot gtk-message-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-message-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-message-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-message-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-message-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMessageDialogClass>;
end C-struct;

define C-struct <_GtkMessageDialogPrivate>
  pointer-type-name: <GtkMessageDialogPrivate>;
end C-struct;

define constant $gtk-message-info = 0;
define constant $gtk-message-warning = 1;
define constant $gtk-message-question = 2;
define constant $gtk-message-error = 3;
define constant $gtk-message-other = 4;
define constant <GtkMessageType> = <C-int>;
define C-pointer-type <GtkMessageType*> => <GtkMessageType>;

define open C-subtype <GtkMisc> (<GtkWidget>)
  constant slot gtk-misc-widget :: <GtkWidget>;
  constant slot gtk-misc-priv :: <GtkMiscPrivate>;
end C-subtype;

define C-pointer-type <GtkMisc*> => <GtkMisc>;

define C-function gtk-misc-get-alignment
  input parameter self :: <GtkMisc>;
  output parameter xalign_ :: <C-float*>;
  output parameter yalign_ :: <C-float*>;
  c-name: "gtk_misc_get_alignment";
end;

define C-function gtk-misc-get-padding
  input parameter self :: <GtkMisc>;
  output parameter xpad_ :: <C-signed-int*>;
  output parameter ypad_ :: <C-signed-int*>;
  c-name: "gtk_misc_get_padding";
end;

define C-function gtk-misc-set-alignment
  input parameter self :: <GtkMisc>;
  input parameter xalign_ :: <C-float>;
  input parameter yalign_ :: <C-float>;
  c-name: "gtk_misc_set_alignment";
end;

define C-function gtk-misc-set-padding
  input parameter self :: <GtkMisc>;
  input parameter xpad_ :: <C-signed-int>;
  input parameter ypad_ :: <C-signed-int>;
  c-name: "gtk_misc_set_padding";
end;

define C-struct <_GtkMiscClass>
  constant slot gtk-misc-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-misc-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-misc-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-misc-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-misc-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMiscClass>;
end C-struct;

define C-struct <_GtkMiscPrivate>
  pointer-type-name: <GtkMiscPrivate>;
end C-struct;

define open C-subtype <GtkMountOperation> (<GMountOperation>)
  constant slot gtk-mount-operation-parent-instance :: <GMountOperation>;
  constant slot gtk-mount-operation-priv :: <GtkMountOperationPrivate>;
end C-subtype;

define C-pointer-type <GtkMountOperation*> => <GtkMountOperation>;

define C-function gtk-mount-operation-new
  input parameter parent_ :: <GtkWindow>;
  result res :: <GMountOperation>;
  c-name: "gtk_mount_operation_new";
end;

define C-function gtk-mount-operation-get-parent
  input parameter self :: <GtkMountOperation>;
  result res :: <GtkWindow>;
  c-name: "gtk_mount_operation_get_parent";
end;

define C-function gtk-mount-operation-get-screen
  input parameter self :: <GtkMountOperation>;
  result res :: <GdkScreen>;
  c-name: "gtk_mount_operation_get_screen";
end;

define C-function gtk-mount-operation-is-showing
  input parameter self :: <GtkMountOperation>;
  result res :: <C-boolean>;
  c-name: "gtk_mount_operation_is_showing";
end;

define C-function gtk-mount-operation-set-parent
  input parameter self :: <GtkMountOperation>;
  input parameter parent_ :: <GtkWindow>;
  c-name: "gtk_mount_operation_set_parent";
end;

define C-function gtk-mount-operation-set-screen
  input parameter self :: <GtkMountOperation>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_mount_operation_set_screen";
end;

define C-struct <_GtkMountOperationClass>
  constant slot gtk-mount-operation-class-parent-class :: <GMountOperationClass>;
  constant slot gtk-mount-operation-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-mount-operation-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-mount-operation-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-mount-operation-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkMountOperationClass>;
end C-struct;

define C-struct <_GtkMountOperationPrivate>
  pointer-type-name: <GtkMountOperationPrivate>;
end C-struct;

define constant $gtk-movement-logical-positions = 0;
define constant $gtk-movement-visual-positions = 1;
define constant $gtk-movement-words = 2;
define constant $gtk-movement-display-lines = 3;
define constant $gtk-movement-display-line-ends = 4;
define constant $gtk-movement-paragraphs = 5;
define constant $gtk-movement-paragraph-ends = 6;
define constant $gtk-movement-pages = 7;
define constant $gtk-movement-buffer-ends = 8;
define constant $gtk-movement-horizontal-pages = 9;
define constant <GtkMovementStep> = <C-int>;
define C-pointer-type <GtkMovementStep*> => <GtkMovementStep>;

define open C-subtype <GtkNotebook> (<GtkContainer>)
  constant slot gtk-notebook-container :: <GtkContainer>;
  constant slot gtk-notebook-priv :: <GtkNotebookPrivate>;
end C-subtype;

define C-pointer-type <GtkNotebook*> => <GtkNotebook>;

define C-function gtk-notebook-new
  result res :: <GtkWidget>;
  c-name: "gtk_notebook_new";
end;

define C-function gtk-notebook-append-page
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_append_page";
end;

define C-function gtk-notebook-append-page-menu
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  input parameter menu_label_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_append_page_menu";
end;

define C-function gtk-notebook-get-action-widget
  input parameter self :: <GtkNotebook>;
  input parameter pack_type_ :: <GtkPackType>;
  result res :: <GtkWidget>;
  c-name: "gtk_notebook_get_action_widget";
end;

define C-function gtk-notebook-get-current-page
  input parameter self :: <GtkNotebook>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_get_current_page";
end;

define C-function gtk-notebook-get-group-name
  input parameter self :: <GtkNotebook>;
  result res :: <C-string>;
  c-name: "gtk_notebook_get_group_name";
end;

define C-function gtk-notebook-get-menu-label
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <GtkWidget>;
  c-name: "gtk_notebook_get_menu_label";
end;

define C-function gtk-notebook-get-menu-label-text
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_notebook_get_menu_label_text";
end;

define C-function gtk-notebook-get-n-pages
  input parameter self :: <GtkNotebook>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_get_n_pages";
end;

define C-function gtk-notebook-get-nth-page
  input parameter self :: <GtkNotebook>;
  input parameter page_num_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_notebook_get_nth_page";
end;

define C-function gtk-notebook-get-scrollable
  input parameter self :: <GtkNotebook>;
  result res :: <C-boolean>;
  c-name: "gtk_notebook_get_scrollable";
end;

define C-function gtk-notebook-get-show-border
  input parameter self :: <GtkNotebook>;
  result res :: <C-boolean>;
  c-name: "gtk_notebook_get_show_border";
end;

define C-function gtk-notebook-get-show-tabs
  input parameter self :: <GtkNotebook>;
  result res :: <C-boolean>;
  c-name: "gtk_notebook_get_show_tabs";
end;

define C-function gtk-notebook-get-tab-detachable
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_notebook_get_tab_detachable";
end;

define C-function gtk-notebook-get-tab-hborder
  input parameter self :: <GtkNotebook>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_notebook_get_tab_hborder";
end;

define C-function gtk-notebook-get-tab-label
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <GtkWidget>;
  c-name: "gtk_notebook_get_tab_label";
end;

define C-function gtk-notebook-get-tab-label-text
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_notebook_get_tab_label_text";
end;

define C-function gtk-notebook-get-tab-pos
  input parameter self :: <GtkNotebook>;
  result res :: <GtkPositionType>;
  c-name: "gtk_notebook_get_tab_pos";
end;

define C-function gtk-notebook-get-tab-reorderable
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_notebook_get_tab_reorderable";
end;

define C-function gtk-notebook-get-tab-vborder
  input parameter self :: <GtkNotebook>;
  result res :: <C-unsigned-short>;
  c-name: "gtk_notebook_get_tab_vborder";
end;

define C-function gtk-notebook-insert-page
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_insert_page";
end;

define C-function gtk-notebook-insert-page-menu
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  input parameter menu_label_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_insert_page_menu";
end;

define C-function gtk-notebook-next-page
  input parameter self :: <GtkNotebook>;
  c-name: "gtk_notebook_next_page";
end;

define C-function gtk-notebook-page-num
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_page_num";
end;

define C-function gtk-notebook-popup-disable
  input parameter self :: <GtkNotebook>;
  c-name: "gtk_notebook_popup_disable";
end;

define C-function gtk-notebook-popup-enable
  input parameter self :: <GtkNotebook>;
  c-name: "gtk_notebook_popup_enable";
end;

define C-function gtk-notebook-prepend-page
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_prepend_page";
end;

define C-function gtk-notebook-prepend-page-menu
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  input parameter menu_label_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_notebook_prepend_page_menu";
end;

define C-function gtk-notebook-prev-page
  input parameter self :: <GtkNotebook>;
  c-name: "gtk_notebook_prev_page";
end;

define C-function gtk-notebook-remove-page
  input parameter self :: <GtkNotebook>;
  input parameter page_num_ :: <C-signed-int>;
  c-name: "gtk_notebook_remove_page";
end;

define C-function gtk-notebook-reorder-child
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_notebook_reorder_child";
end;

define C-function gtk-notebook-set-action-widget
  input parameter self :: <GtkNotebook>;
  input parameter widget_ :: <GtkWidget>;
  input parameter pack_type_ :: <GtkPackType>;
  c-name: "gtk_notebook_set_action_widget";
end;

define C-function gtk-notebook-set-current-page
  input parameter self :: <GtkNotebook>;
  input parameter page_num_ :: <C-signed-int>;
  c-name: "gtk_notebook_set_current_page";
end;

define C-function gtk-notebook-set-group-name
  input parameter self :: <GtkNotebook>;
  input parameter group_name_ :: <C-string>;
  c-name: "gtk_notebook_set_group_name";
end;

define C-function gtk-notebook-set-menu-label
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter menu_label_ :: <GtkWidget>;
  c-name: "gtk_notebook_set_menu_label";
end;

define C-function gtk-notebook-set-menu-label-text
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter menu_text_ :: <C-string>;
  c-name: "gtk_notebook_set_menu_label_text";
end;

define C-function gtk-notebook-set-scrollable
  input parameter self :: <GtkNotebook>;
  input parameter scrollable_ :: <C-boolean>;
  c-name: "gtk_notebook_set_scrollable";
end;

define C-function gtk-notebook-set-show-border
  input parameter self :: <GtkNotebook>;
  input parameter show_border_ :: <C-boolean>;
  c-name: "gtk_notebook_set_show_border";
end;

define C-function gtk-notebook-set-show-tabs
  input parameter self :: <GtkNotebook>;
  input parameter show_tabs_ :: <C-boolean>;
  c-name: "gtk_notebook_set_show_tabs";
end;

define C-function gtk-notebook-set-tab-detachable
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter detachable_ :: <C-boolean>;
  c-name: "gtk_notebook_set_tab_detachable";
end;

define C-function gtk-notebook-set-tab-label
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_label_ :: <GtkWidget>;
  c-name: "gtk_notebook_set_tab_label";
end;

define C-function gtk-notebook-set-tab-label-text
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter tab_text_ :: <C-string>;
  c-name: "gtk_notebook_set_tab_label_text";
end;

define C-function gtk-notebook-set-tab-pos
  input parameter self :: <GtkNotebook>;
  input parameter pos_ :: <GtkPositionType>;
  c-name: "gtk_notebook_set_tab_pos";
end;

define C-function gtk-notebook-set-tab-reorderable
  input parameter self :: <GtkNotebook>;
  input parameter child_ :: <GtkWidget>;
  input parameter reorderable_ :: <C-boolean>;
  c-name: "gtk_notebook_set_tab_reorderable";
end;

define C-struct <_GtkNotebookClass>
  constant slot gtk-notebook-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-notebook-class-switch-page :: <C-function-pointer>;
  constant slot gtk-notebook-class-select-page :: <C-function-pointer>;
  constant slot gtk-notebook-class-focus-tab :: <C-function-pointer>;
  constant slot gtk-notebook-class-change-current-page :: <C-function-pointer>;
  constant slot gtk-notebook-class-move-focus-out :: <C-function-pointer>;
  constant slot gtk-notebook-class-reorder-tab :: <C-function-pointer>;
  constant slot gtk-notebook-class-insert-page :: <C-function-pointer>;
  constant slot gtk-notebook-class-create-window :: <C-void*>;
  constant slot gtk-notebook-class-page-reordered :: <C-function-pointer>;
  constant slot gtk-notebook-class-page-removed :: <C-function-pointer>;
  constant slot gtk-notebook-class-page-added :: <C-function-pointer>;
  constant slot gtk-notebook-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-notebook-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkNotebookClass>;
end C-struct;

define C-struct <_GtkNotebookPrivate>
  pointer-type-name: <GtkNotebookPrivate>;
end C-struct;

define constant $gtk-notebook-tab-first = 0;
define constant $gtk-notebook-tab-last = 1;
define constant <GtkNotebookTab> = <C-int>;
define C-pointer-type <GtkNotebookTab*> => <GtkNotebookTab>;

define constant $gtk-number-up-layout-left-to-right-top-to-bottom = 0;
define constant $gtk-number-up-layout-left-to-right-bottom-to-top = 1;
define constant $gtk-number-up-layout-right-to-left-top-to-bottom = 2;
define constant $gtk-number-up-layout-right-to-left-bottom-to-top = 3;
define constant $gtk-number-up-layout-top-to-bottom-left-to-right = 4;
define constant $gtk-number-up-layout-top-to-bottom-right-to-left = 5;
define constant $gtk-number-up-layout-bottom-to-top-left-to-right = 6;
define constant $gtk-number-up-layout-bottom-to-top-right-to-left = 7;
define constant <GtkNumberUpLayout> = <C-int>;
define C-pointer-type <GtkNumberUpLayout*> => <GtkNumberUpLayout>;

define open C-subtype <GtkNumerableIcon> (<GEmblemedIcon>)
  constant slot gtk-numerable-icon-parent :: <GEmblemedIcon>;
  constant slot gtk-numerable-icon-priv :: <GtkNumerableIconPrivate>;
end C-subtype;

define C-pointer-type <GtkNumerableIcon*> => <GtkNumerableIcon>;

define C-function gtk-numerable-icon-new
  input parameter base_icon_ :: <GIcon>;
  result res :: <GIcon>;
  c-name: "gtk_numerable_icon_new";
end;

define C-function gtk-numerable-icon-new-with-style-context
  input parameter base_icon_ :: <GIcon>;
  input parameter context_ :: <GtkStyleContext>;
  result res :: <GIcon>;
  c-name: "gtk_numerable_icon_new_with_style_context";
end;

define C-function gtk-numerable-icon-get-background-gicon
  input parameter self :: <GtkNumerableIcon>;
  result res :: <GIcon>;
  c-name: "gtk_numerable_icon_get_background_gicon";
end;

define C-function gtk-numerable-icon-get-background-icon-name
  input parameter self :: <GtkNumerableIcon>;
  result res :: <C-string>;
  c-name: "gtk_numerable_icon_get_background_icon_name";
end;

define C-function gtk-numerable-icon-get-count
  input parameter self :: <GtkNumerableIcon>;
  result res :: <C-signed-int>;
  c-name: "gtk_numerable_icon_get_count";
end;

define C-function gtk-numerable-icon-get-label
  input parameter self :: <GtkNumerableIcon>;
  result res :: <C-string>;
  c-name: "gtk_numerable_icon_get_label";
end;

define C-function gtk-numerable-icon-get-style-context
  input parameter self :: <GtkNumerableIcon>;
  result res :: <GtkStyleContext>;
  c-name: "gtk_numerable_icon_get_style_context";
end;

define C-function gtk-numerable-icon-set-background-gicon
  input parameter self :: <GtkNumerableIcon>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_numerable_icon_set_background_gicon";
end;

define C-function gtk-numerable-icon-set-background-icon-name
  input parameter self :: <GtkNumerableIcon>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_numerable_icon_set_background_icon_name";
end;

define C-function gtk-numerable-icon-set-count
  input parameter self :: <GtkNumerableIcon>;
  input parameter count_ :: <C-signed-int>;
  c-name: "gtk_numerable_icon_set_count";
end;

define C-function gtk-numerable-icon-set-label
  input parameter self :: <GtkNumerableIcon>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_numerable_icon_set_label";
end;

define C-function gtk-numerable-icon-set-style-context
  input parameter self :: <GtkNumerableIcon>;
  input parameter style_ :: <GtkStyleContext>;
  c-name: "gtk_numerable_icon_set_style_context";
end;

define C-struct <_GtkNumerableIconClass>
  constant slot gtk-numerable-icon-class-parent-class :: <GEmblemedIconClass>;
  constant slot gtk-numerable-icon-class-padding :: <C-void*>;
  pointer-type-name: <GtkNumerableIconClass>;
end C-struct;

define C-struct <_GtkNumerableIconPrivate>
  pointer-type-name: <GtkNumerableIconPrivate>;
end C-struct;

define open C-subtype <GtkOffscreenWindow> (<GtkWindow>)
  constant slot gtk-offscreen-window-parent-object :: <GtkWindow>;
end C-subtype;

define C-pointer-type <GtkOffscreenWindow*> => <GtkOffscreenWindow>;

define C-function gtk-offscreen-window-new
  result res :: <GtkWidget>;
  c-name: "gtk_offscreen_window_new";
end;

define C-function gtk-offscreen-window-get-pixbuf
  input parameter self :: <GtkOffscreenWindow>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_offscreen_window_get_pixbuf";
end;

define C-function gtk-offscreen-window-get-surface
  input parameter self :: <GtkOffscreenWindow>;
  result res :: <cairoSurface>;
  c-name: "gtk_offscreen_window_get_surface";
end;

define C-struct <_GtkOffscreenWindowClass>
  constant slot gtk-offscreen-window-class-parent-class :: <GtkWindowClass>;
  constant slot gtk-offscreen-window-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-offscreen-window-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-offscreen-window-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-offscreen-window-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkOffscreenWindowClass>;
end C-struct;

// Interface
define open C-subtype <GtkOrientable> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkOrientable*> => <GtkOrientable>;

define C-function gtk-orientable-get-orientation
  input parameter self :: <GtkOrientable>;
  result res :: <GtkOrientation>;
  c-name: "gtk_orientable_get_orientation";
end;

define C-function gtk-orientable-set-orientation
  input parameter self :: <GtkOrientable>;
  input parameter orientation_ :: <GtkOrientation>;
  c-name: "gtk_orientable_set_orientation";
end;

define C-struct <_GtkOrientableIface>
  constant slot gtk-orientable-iface-base-iface :: <GTypeInterface>;
  pointer-type-name: <GtkOrientableIface>;
end C-struct;

define constant $gtk-orientation-horizontal = 0;
define constant $gtk-orientation-vertical = 1;
define constant <GtkOrientation> = <C-int>;
define C-pointer-type <GtkOrientation*> => <GtkOrientation>;

define open C-subtype <GtkOverlay> (<GtkBin>)
  constant slot gtk-overlay-parent :: <GtkBin>;
  constant slot gtk-overlay-priv :: <GtkOverlayPrivate>;
end C-subtype;

define C-pointer-type <GtkOverlay*> => <GtkOverlay>;

define C-function gtk-overlay-new
  result res :: <GtkWidget>;
  c-name: "gtk_overlay_new";
end;

define C-function gtk-overlay-add-overlay
  input parameter self :: <GtkOverlay>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_overlay_add_overlay";
end;

define C-struct <_GtkOverlayClass>
  constant slot gtk-overlay-class-parent-class :: <GtkBinClass>;
  constant slot gtk-overlay-class-get-child-position :: <C-function-pointer>;
  constant slot gtk-overlay-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-overlay-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkOverlayClass>;
end C-struct;

define C-struct <_GtkOverlayPrivate>
  pointer-type-name: <GtkOverlayPrivate>;
end C-struct;

define constant $paper-name-a3 = "iso_a3";

define constant $paper-name-a4 = "iso_a4";

define constant $paper-name-a5 = "iso_a5";

define constant $paper-name-b5 = "iso_b5";

define constant $paper-name-executive = "na_executive";

define constant $paper-name-legal = "na_legal";

define constant $paper-name-letter = "na_letter";

define constant $path-prio-mask = 15;

define constant $print-settings-collate = "collate";

define constant $print-settings-default-source = "default-source";

define constant $print-settings-dither = "dither";

define constant $print-settings-duplex = "duplex";

define constant $print-settings-finishings = "finishings";

define constant $print-settings-media-type = "media-type";

define constant $print-settings-number-up = "number-up";

define constant $print-settings-number-up-layout = "number-up-layout";

define constant $print-settings-n-copies = "n-copies";

define constant $print-settings-orientation = "orientation";

define constant $print-settings-output-bin = "output-bin";

define constant $print-settings-output-file-format = "output-file-format";

define constant $print-settings-output-uri = "output-uri";

define constant $print-settings-page-ranges = "page-ranges";

define constant $print-settings-page-set = "page-set";

define constant $print-settings-paper-format = "paper-format";

define constant $print-settings-paper-height = "paper-height";

define constant $print-settings-paper-width = "paper-width";

define constant $print-settings-printer = "printer";

define constant $print-settings-printer-lpi = "printer-lpi";

define constant $print-settings-print-pages = "print-pages";

define constant $print-settings-quality = "quality";

define constant $print-settings-resolution = "resolution";

define constant $print-settings-resolution-x = "resolution-x";

define constant $print-settings-resolution-y = "resolution-y";

define constant $print-settings-reverse = "reverse";

define constant $print-settings-scale = "scale";

define constant $print-settings-use-color = "use-color";

define constant $print-settings-win32-driver-extra = "win32-driver-extra";

define constant $print-settings-win32-driver-version = "win32-driver-version";

define constant $priority-resize = 10;

define constant $gtk-pack-direction-ltr = 0;
define constant $gtk-pack-direction-rtl = 1;
define constant $gtk-pack-direction-ttb = 2;
define constant $gtk-pack-direction-btt = 3;
define constant <GtkPackDirection> = <C-int>;
define C-pointer-type <GtkPackDirection*> => <GtkPackDirection>;

define constant $gtk-pack-start = 0;
define constant $gtk-pack-end = 1;
define constant <GtkPackType> = <C-int>;
define C-pointer-type <GtkPackType*> => <GtkPackType>;

define constant $gtk-page-orientation-portrait = 0;
define constant $gtk-page-orientation-landscape = 1;
define constant $gtk-page-orientation-reverse-portrait = 2;
define constant $gtk-page-orientation-reverse-landscape = 3;
define constant <GtkPageOrientation> = <C-int>;
define C-pointer-type <GtkPageOrientation*> => <GtkPageOrientation>;

define C-struct <_GtkPageRange>
  slot gtk-page-range-start :: <C-signed-int>;
  slot gtk-page-range-end :: <C-signed-int>;
  pointer-type-name: <GtkPageRange>;
end C-struct;

define constant $gtk-page-set-all = 0;
define constant $gtk-page-set-even = 1;
define constant $gtk-page-set-odd = 2;
define constant <GtkPageSet> = <C-int>;
define C-pointer-type <GtkPageSet*> => <GtkPageSet>;

define open C-subtype <GtkPageSetup> (<GObject>)
end C-subtype;

define C-pointer-type <GtkPageSetup*> => <GtkPageSetup>;

define C-function gtk-page-setup-new
  result res :: <GtkPageSetup>;
  c-name: "gtk_page_setup_new";
end;

define C-function gtk-page-setup-new-from-file
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_page_setup_new_from_file";
end;

define C-function gtk-page-setup-new-from-key-file
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_page_setup_new_from_key_file";
end;

define C-function gtk-page-setup-copy
  input parameter self :: <GtkPageSetup>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_page_setup_copy";
end;

define C-function gtk-page-setup-get-bottom-margin
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_bottom_margin";
end;

define C-function gtk-page-setup-get-left-margin
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_left_margin";
end;

define C-function gtk-page-setup-get-orientation
  input parameter self :: <GtkPageSetup>;
  result res :: <GtkPageOrientation>;
  c-name: "gtk_page_setup_get_orientation";
end;

define C-function gtk-page-setup-get-page-height
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_page_height";
end;

define C-function gtk-page-setup-get-page-width
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_page_width";
end;

define C-function gtk-page-setup-get-paper-height
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_paper_height";
end;

define C-function gtk-page-setup-get-paper-size
  input parameter self :: <GtkPageSetup>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_page_setup_get_paper_size";
end;

define C-function gtk-page-setup-get-paper-width
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_paper_width";
end;

define C-function gtk-page-setup-get-right-margin
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_right_margin";
end;

define C-function gtk-page-setup-get-top-margin
  input parameter self :: <GtkPageSetup>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_page_setup_get_top_margin";
end;

define C-function gtk-page-setup-load-file
  input parameter self :: <GtkPageSetup>;
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_page_setup_load_file";
end;

define C-function gtk-page-setup-load-key-file
  input parameter self :: <GtkPageSetup>;
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_page_setup_load_key_file";
end;

define C-function gtk-page-setup-set-bottom-margin
  input parameter self :: <GtkPageSetup>;
  input parameter margin_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_page_setup_set_bottom_margin";
end;

define C-function gtk-page-setup-set-left-margin
  input parameter self :: <GtkPageSetup>;
  input parameter margin_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_page_setup_set_left_margin";
end;

define C-function gtk-page-setup-set-orientation
  input parameter self :: <GtkPageSetup>;
  input parameter orientation_ :: <GtkPageOrientation>;
  c-name: "gtk_page_setup_set_orientation";
end;

define C-function gtk-page-setup-set-paper-size
  input parameter self :: <GtkPageSetup>;
  input parameter size_ :: <GtkPaperSize>;
  c-name: "gtk_page_setup_set_paper_size";
end;

define C-function gtk-page-setup-set-paper-size-and-default-margins
  input parameter self :: <GtkPageSetup>;
  input parameter size_ :: <GtkPaperSize>;
  c-name: "gtk_page_setup_set_paper_size_and_default_margins";
end;

define C-function gtk-page-setup-set-right-margin
  input parameter self :: <GtkPageSetup>;
  input parameter margin_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_page_setup_set_right_margin";
end;

define C-function gtk-page-setup-set-top-margin
  input parameter self :: <GtkPageSetup>;
  input parameter margin_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_page_setup_set_top_margin";
end;

define C-function gtk-page-setup-to-file
  input parameter self :: <GtkPageSetup>;
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_page_setup_to_file";
end;

define C-function gtk-page-setup-to-key-file
  input parameter self :: <GtkPageSetup>;
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  c-name: "gtk_page_setup_to_key_file";
end;

define open C-subtype <GtkPaned> (<GtkContainer>)
  constant slot gtk-paned-container :: <GtkContainer>;
  constant slot gtk-paned-priv :: <GtkPanedPrivate>;
end C-subtype;

define C-pointer-type <GtkPaned*> => <GtkPaned>;

define C-function gtk-paned-new
  input parameter orientation_ :: <GtkOrientation>;
  result res :: <GtkWidget>;
  c-name: "gtk_paned_new";
end;

define C-function gtk-paned-add1
  input parameter self :: <GtkPaned>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_paned_add1";
end;

define C-function gtk-paned-add2
  input parameter self :: <GtkPaned>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_paned_add2";
end;

define C-function gtk-paned-get-child1
  input parameter self :: <GtkPaned>;
  result res :: <GtkWidget>;
  c-name: "gtk_paned_get_child1";
end;

define C-function gtk-paned-get-child2
  input parameter self :: <GtkPaned>;
  result res :: <GtkWidget>;
  c-name: "gtk_paned_get_child2";
end;

define C-function gtk-paned-get-handle-window
  input parameter self :: <GtkPaned>;
  result res :: <GdkWindow>;
  c-name: "gtk_paned_get_handle_window";
end;

define C-function gtk-paned-get-position
  input parameter self :: <GtkPaned>;
  result res :: <C-signed-int>;
  c-name: "gtk_paned_get_position";
end;

define C-function gtk-paned-pack1
  input parameter self :: <GtkPaned>;
  input parameter child_ :: <GtkWidget>;
  input parameter resize_ :: <C-boolean>;
  input parameter shrink_ :: <C-boolean>;
  c-name: "gtk_paned_pack1";
end;

define C-function gtk-paned-pack2
  input parameter self :: <GtkPaned>;
  input parameter child_ :: <GtkWidget>;
  input parameter resize_ :: <C-boolean>;
  input parameter shrink_ :: <C-boolean>;
  c-name: "gtk_paned_pack2";
end;

define C-function gtk-paned-set-position
  input parameter self :: <GtkPaned>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_paned_set_position";
end;

define C-struct <_GtkPanedClass>
  constant slot gtk-paned-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-paned-class-cycle-child-focus :: <C-function-pointer>;
  constant slot gtk-paned-class-toggle-handle-focus :: <C-function-pointer>;
  constant slot gtk-paned-class-move-handle :: <C-function-pointer>;
  constant slot gtk-paned-class-cycle-handle-focus :: <C-function-pointer>;
  constant slot gtk-paned-class-accept-position :: <C-function-pointer>;
  constant slot gtk-paned-class-cancel-position :: <C-function-pointer>;
  constant slot gtk-paned-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-paned-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-paned-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-paned-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkPanedClass>;
end C-struct;

define C-struct <_GtkPanedPrivate>
  pointer-type-name: <GtkPanedPrivate>;
end C-struct;

define C-struct <_GtkPaperSize>
  pointer-type-name: <GtkPaperSize>;
end C-struct;

define C-function gtk-paper-size-new
  input parameter name_ :: <C-string>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_paper_size_new";
end;

define C-function gtk-paper-size-new-custom
  input parameter name_ :: <C-string>;
  input parameter display_name_ :: <C-string>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_paper_size_new_custom";
end;

define C-function gtk-paper-size-new-from-key-file
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_paper_size_new_from_key_file";
end;

define C-function gtk-paper-size-new-from-ppd
  input parameter ppd_name_ :: <C-string>;
  input parameter ppd_display_name_ :: <C-string>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_paper_size_new_from_ppd";
end;

define C-function gtk-paper-size-copy
  input parameter self :: <GtkPaperSize>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_paper_size_copy";
end;

define C-function gtk-paper-size-free
  input parameter self :: <GtkPaperSize>;
  c-name: "gtk_paper_size_free";
end;

define C-function gtk-paper-size-get-default-bottom-margin
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_default_bottom_margin";
end;

define C-function gtk-paper-size-get-default-left-margin
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_default_left_margin";
end;

define C-function gtk-paper-size-get-default-right-margin
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_default_right_margin";
end;

define C-function gtk-paper-size-get-default-top-margin
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_default_top_margin";
end;

define C-function gtk-paper-size-get-display-name
  input parameter self :: <GtkPaperSize>;
  result res :: <C-string>;
  c-name: "gtk_paper_size_get_display_name";
end;

define C-function gtk-paper-size-get-height
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_height";
end;

define C-function gtk-paper-size-get-name
  input parameter self :: <GtkPaperSize>;
  result res :: <C-string>;
  c-name: "gtk_paper_size_get_name";
end;

define C-function gtk-paper-size-get-ppd-name
  input parameter self :: <GtkPaperSize>;
  result res :: <C-string>;
  c-name: "gtk_paper_size_get_ppd_name";
end;

define C-function gtk-paper-size-get-width
  input parameter self :: <GtkPaperSize>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_paper_size_get_width";
end;

define C-function gtk-paper-size-is-custom
  input parameter self :: <GtkPaperSize>;
  result res :: <C-boolean>;
  c-name: "gtk_paper_size_is_custom";
end;

define C-function gtk-paper-size-is-equal
  input parameter self :: <GtkPaperSize>;
  input parameter size2_ :: <GtkPaperSize>;
  result res :: <C-boolean>;
  c-name: "gtk_paper_size_is_equal";
end;

define C-function gtk-paper-size-set-size
  input parameter self :: <GtkPaperSize>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_paper_size_set_size";
end;

define C-function gtk-paper-size-to-key-file
  input parameter self :: <GtkPaperSize>;
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  c-name: "gtk_paper_size_to_key_file";
end;

define C-function gtk-paper-size-get-default
  result res :: <C-string>;
  c-name: "gtk_paper_size_get_default";
end;

define C-function gtk-paper-size-get-paper-sizes
  input parameter include_custom_ :: <C-boolean>;
  result res :: <GList>;
  c-name: "gtk_paper_size_get_paper_sizes";
end;

define constant $gtk-path-prio-lowest = 0;
define constant $gtk-path-prio-gtk = 4;
define constant $gtk-path-prio-application = 8;
define constant $gtk-path-prio-theme = 10;
define constant $gtk-path-prio-rc = 12;
define constant $gtk-path-prio-highest = 15;
define constant <GtkPathPriorityType> = <C-int>;
define C-pointer-type <GtkPathPriorityType*> => <GtkPathPriorityType>;

define constant $gtk-path-widget = 0;
define constant $gtk-path-widget-class = 1;
define constant $gtk-path-class = 2;
define constant <GtkPathType> = <C-int>;
define C-pointer-type <GtkPathType*> => <GtkPathType>;

define open C-subtype <GtkPlug> (<GtkWindow>)
  constant slot gtk-plug-window :: <GtkWindow>;
  constant slot gtk-plug-priv :: <GtkPlugPrivate>;
end C-subtype;

define C-pointer-type <GtkPlug*> => <GtkPlug>;

define C-function gtk-plug-new
  input parameter socket_id_ :: <C-unsigned-long>;
  result res :: <GtkWidget>;
  c-name: "gtk_plug_new";
end;

define C-function gtk-plug-new-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter socket_id_ :: <C-unsigned-long>;
  result res :: <GtkWidget>;
  c-name: "gtk_plug_new_for_display";
end;

define C-function gtk-plug-construct
  input parameter self :: <GtkPlug>;
  input parameter socket_id_ :: <C-unsigned-long>;
  c-name: "gtk_plug_construct";
end;

define C-function gtk-plug-construct-for-display
  input parameter self :: <GtkPlug>;
  input parameter display_ :: <GdkDisplay>;
  input parameter socket_id_ :: <C-unsigned-long>;
  c-name: "gtk_plug_construct_for_display";
end;

define C-function gtk-plug-get-embedded
  input parameter self :: <GtkPlug>;
  result res :: <C-boolean>;
  c-name: "gtk_plug_get_embedded";
end;

define C-function gtk-plug-get-id
  input parameter self :: <GtkPlug>;
  result res :: <C-unsigned-long>;
  c-name: "gtk_plug_get_id";
end;

define C-function gtk-plug-get-socket-window
  input parameter self :: <GtkPlug>;
  result res :: <GdkWindow>;
  c-name: "gtk_plug_get_socket_window";
end;

define C-struct <_GtkPlugClass>
  constant slot gtk-plug-class-parent-class :: <GtkWindowClass>;
  constant slot gtk-plug-class-embedded :: <C-function-pointer>;
  constant slot gtk-plug-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-plug-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-plug-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-plug-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkPlugClass>;
end C-struct;

define C-struct <_GtkPlugPrivate>
  pointer-type-name: <GtkPlugPrivate>;
end C-struct;

define constant $gtk-policy-always = 0;
define constant $gtk-policy-automatic = 1;
define constant $gtk-policy-never = 2;
define constant <GtkPolicyType> = <C-int>;
define C-pointer-type <GtkPolicyType*> => <GtkPolicyType>;

define constant $gtk-pos-left = 0;
define constant $gtk-pos-right = 1;
define constant $gtk-pos-top = 2;
define constant $gtk-pos-bottom = 3;
define constant <GtkPositionType> = <C-int>;
define C-pointer-type <GtkPositionType*> => <GtkPositionType>;

define open C-subtype <GtkPrintContext> (<GObject>)
end C-subtype;

define C-pointer-type <GtkPrintContext*> => <GtkPrintContext>;

define C-function gtk-print-context-create-pango-context
  input parameter self :: <GtkPrintContext>;
  result res :: <PangoContext>;
  c-name: "gtk_print_context_create_pango_context";
end;

define C-function gtk-print-context-create-pango-layout
  input parameter self :: <GtkPrintContext>;
  result res :: <PangoLayout>;
  c-name: "gtk_print_context_create_pango_layout";
end;

define C-function gtk-print-context-get-cairo-context
  input parameter self :: <GtkPrintContext>;
  result res :: <cairoContext>;
  c-name: "gtk_print_context_get_cairo_context";
end;

define C-function gtk-print-context-get-dpi-x
  input parameter self :: <GtkPrintContext>;
  result res :: <C-double>;
  c-name: "gtk_print_context_get_dpi_x";
end;

define C-function gtk-print-context-get-dpi-y
  input parameter self :: <GtkPrintContext>;
  result res :: <C-double>;
  c-name: "gtk_print_context_get_dpi_y";
end;

define C-function gtk-print-context-get-hard-margins
  input parameter self :: <GtkPrintContext>;
  output parameter top_ :: <C-double*>;
  output parameter bottom_ :: <C-double*>;
  output parameter left_ :: <C-double*>;
  output parameter right_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gtk_print_context_get_hard_margins";
end;

define C-function gtk-print-context-get-height
  input parameter self :: <GtkPrintContext>;
  result res :: <C-double>;
  c-name: "gtk_print_context_get_height";
end;

define C-function gtk-print-context-get-page-setup
  input parameter self :: <GtkPrintContext>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_print_context_get_page_setup";
end;

define C-function gtk-print-context-get-pango-fontmap
  input parameter self :: <GtkPrintContext>;
  result res :: <PangoFontMap>;
  c-name: "gtk_print_context_get_pango_fontmap";
end;

define C-function gtk-print-context-get-width
  input parameter self :: <GtkPrintContext>;
  result res :: <C-double>;
  c-name: "gtk_print_context_get_width";
end;

define C-function gtk-print-context-set-cairo-context
  input parameter self :: <GtkPrintContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter dpi_x_ :: <C-double>;
  input parameter dpi_y_ :: <C-double>;
  c-name: "gtk_print_context_set_cairo_context";
end;

define constant $gtk-print-duplex-simplex = 0;
define constant $gtk-print-duplex-horizontal = 1;
define constant $gtk-print-duplex-vertical = 2;
define constant <GtkPrintDuplex> = <C-int>;
define C-pointer-type <GtkPrintDuplex*> => <GtkPrintDuplex>;

define constant $gtk-print-error-general = 0;
define constant $gtk-print-error-internal-error = 1;
define constant $gtk-print-error-nomem = 2;
define constant $gtk-print-error-invalid-file = 3;
define constant <GtkPrintError> = <C-int>;
define C-pointer-type <GtkPrintError*> => <GtkPrintError>;

define open C-subtype <GtkPrintOperation> (<GObject>)
  constant slot gtk-print-operation-parent-instance :: <GObject>;
  constant slot gtk-print-operation-priv :: <GtkPrintOperationPrivate>;
end C-subtype;

define C-pointer-type <GtkPrintOperation*> => <GtkPrintOperation>;

define C-function gtk-print-operation-new
  result res :: <GtkPrintOperation>;
  c-name: "gtk_print_operation_new";
end;

define C-function gtk-print-operation-cancel
  input parameter self :: <GtkPrintOperation>;
  c-name: "gtk_print_operation_cancel";
end;

define C-function gtk-print-operation-draw-page-finish
  input parameter self :: <GtkPrintOperation>;
  c-name: "gtk_print_operation_draw_page_finish";
end;

define C-function gtk-print-operation-get-default-page-setup
  input parameter self :: <GtkPrintOperation>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_print_operation_get_default_page_setup";
end;

define C-function gtk-print-operation-get-embed-page-setup
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-boolean>;
  c-name: "gtk_print_operation_get_embed_page_setup";
end;

define C-function gtk-print-operation-get-error
  input parameter self :: <GtkPrintOperation>;
  output parameter error_ :: <GError*>;
  c-name: "gtk_print_operation_get_error";
end;

define C-function gtk-print-operation-get-has-selection
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-boolean>;
  c-name: "gtk_print_operation_get_has_selection";
end;

define C-function gtk-print-operation-get-n-pages-to-print
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_operation_get_n_pages_to_print";
end;

define C-function gtk-print-operation-get-print-settings
  input parameter self :: <GtkPrintOperation>;
  result res :: <GtkPrintSettings>;
  c-name: "gtk_print_operation_get_print_settings";
end;

define C-function gtk-print-operation-get-status
  input parameter self :: <GtkPrintOperation>;
  result res :: <GtkPrintStatus>;
  c-name: "gtk_print_operation_get_status";
end;

define C-function gtk-print-operation-get-status-string
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-string>;
  c-name: "gtk_print_operation_get_status_string";
end;

define C-function gtk-print-operation-get-support-selection
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-boolean>;
  c-name: "gtk_print_operation_get_support_selection";
end;

define C-function gtk-print-operation-is-finished
  input parameter self :: <GtkPrintOperation>;
  result res :: <C-boolean>;
  c-name: "gtk_print_operation_is_finished";
end;

define C-function gtk-print-operation-run
  input parameter self :: <GtkPrintOperation>;
  input parameter action_ :: <GtkPrintOperationAction>;
  input parameter parent_ :: <GtkWindow>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPrintOperationResult>;
  c-name: "gtk_print_operation_run";
end;

define C-function gtk-print-operation-set-allow-async
  input parameter self :: <GtkPrintOperation>;
  input parameter allow_async_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_allow_async";
end;

define C-function gtk-print-operation-set-current-page
  input parameter self :: <GtkPrintOperation>;
  input parameter current_page_ :: <C-signed-int>;
  c-name: "gtk_print_operation_set_current_page";
end;

define C-function gtk-print-operation-set-custom-tab-label
  input parameter self :: <GtkPrintOperation>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_print_operation_set_custom_tab_label";
end;

define C-function gtk-print-operation-set-default-page-setup
  input parameter self :: <GtkPrintOperation>;
  input parameter default_page_setup_ :: <GtkPageSetup>;
  c-name: "gtk_print_operation_set_default_page_setup";
end;

define C-function gtk-print-operation-set-defer-drawing
  input parameter self :: <GtkPrintOperation>;
  c-name: "gtk_print_operation_set_defer_drawing";
end;

define C-function gtk-print-operation-set-embed-page-setup
  input parameter self :: <GtkPrintOperation>;
  input parameter embed_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_embed_page_setup";
end;

define C-function gtk-print-operation-set-export-filename
  input parameter self :: <GtkPrintOperation>;
  input parameter filename_ :: <C-string>;
  c-name: "gtk_print_operation_set_export_filename";
end;

define C-function gtk-print-operation-set-has-selection
  input parameter self :: <GtkPrintOperation>;
  input parameter has_selection_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_has_selection";
end;

define C-function gtk-print-operation-set-job-name
  input parameter self :: <GtkPrintOperation>;
  input parameter job_name_ :: <C-string>;
  c-name: "gtk_print_operation_set_job_name";
end;

define C-function gtk-print-operation-set-n-pages
  input parameter self :: <GtkPrintOperation>;
  input parameter n_pages_ :: <C-signed-int>;
  c-name: "gtk_print_operation_set_n_pages";
end;

define C-function gtk-print-operation-set-print-settings
  input parameter self :: <GtkPrintOperation>;
  input parameter print_settings_ :: <GtkPrintSettings>;
  c-name: "gtk_print_operation_set_print_settings";
end;

define C-function gtk-print-operation-set-show-progress
  input parameter self :: <GtkPrintOperation>;
  input parameter show_progress_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_show_progress";
end;

define C-function gtk-print-operation-set-support-selection
  input parameter self :: <GtkPrintOperation>;
  input parameter support_selection_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_support_selection";
end;

define C-function gtk-print-operation-set-track-print-status
  input parameter self :: <GtkPrintOperation>;
  input parameter track_status_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_track_print_status";
end;

define C-function gtk-print-operation-set-unit
  input parameter self :: <GtkPrintOperation>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_print_operation_set_unit";
end;

define C-function gtk-print-operation-set-use-full-page
  input parameter self :: <GtkPrintOperation>;
  input parameter full_page_ :: <C-boolean>;
  c-name: "gtk_print_operation_set_use_full_page";
end;

define constant $gtk-print-operation-action-print-dialog = 0;
define constant $gtk-print-operation-action-print = 1;
define constant $gtk-print-operation-action-preview = 2;
define constant $gtk-print-operation-action-export = 3;
define constant <GtkPrintOperationAction> = <C-int>;
define C-pointer-type <GtkPrintOperationAction*> => <GtkPrintOperationAction>;

define C-struct <_GtkPrintOperationClass>
  constant slot gtk-print-operation-class-parent-class :: <GObjectClass>;
  constant slot gtk-print-operation-class-done :: <C-function-pointer>;
  constant slot gtk-print-operation-class-begin-print :: <C-function-pointer>;
  constant slot gtk-print-operation-class-paginate :: <C-function-pointer>;
  constant slot gtk-print-operation-class-request-page-setup :: <C-function-pointer>;
  constant slot gtk-print-operation-class-draw-page :: <C-function-pointer>;
  constant slot gtk-print-operation-class-end-print :: <C-function-pointer>;
  constant slot gtk-print-operation-class-status-changed :: <C-function-pointer>;
  constant slot gtk-print-operation-class-create-custom-widget :: <C-void*>;
  constant slot gtk-print-operation-class-custom-widget-apply :: <C-function-pointer>;
  constant slot gtk-print-operation-class-preview :: <C-function-pointer>;
  constant slot gtk-print-operation-class-update-custom-widget :: <C-function-pointer>;
  constant slot gtk-print-operation-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-print-operation-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkPrintOperationClass>;
end C-struct;

// Interface
define open C-subtype <GtkPrintOperationPreview> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkPrintOperationPreview*> => <GtkPrintOperationPreview>;

define C-function gtk-print-operation-preview-end-preview
  input parameter self :: <GtkPrintOperationPreview>;
  c-name: "gtk_print_operation_preview_end_preview";
end;

define C-function gtk-print-operation-preview-is-selected
  input parameter self :: <GtkPrintOperationPreview>;
  input parameter page_nr_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_print_operation_preview_is_selected";
end;

define C-function gtk-print-operation-preview-render-page
  input parameter self :: <GtkPrintOperationPreview>;
  input parameter page_nr_ :: <C-signed-int>;
  c-name: "gtk_print_operation_preview_render_page";
end;

define C-struct <_GtkPrintOperationPreviewIface>
  constant slot gtk-print-operation-preview-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-print-operation-preview-iface-ready :: <C-function-pointer>;
  constant slot gtk-print-operation-preview-iface-got-page-size :: <C-function-pointer>;
  constant slot gtk-print-operation-preview-iface-render-page :: <C-function-pointer>;
  constant slot gtk-print-operation-preview-iface-is-selected :: <C-function-pointer>;
  constant slot gtk-print-operation-preview-iface-end-preview :: <C-function-pointer>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-print-operation-preview-iface-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkPrintOperationPreviewIface>;
end C-struct;

define C-struct <_GtkPrintOperationPrivate>
  pointer-type-name: <GtkPrintOperationPrivate>;
end C-struct;

define constant $gtk-print-operation-result-error = 0;
define constant $gtk-print-operation-result-apply = 1;
define constant $gtk-print-operation-result-cancel = 2;
define constant $gtk-print-operation-result-in-progress = 3;
define constant <GtkPrintOperationResult> = <C-int>;
define C-pointer-type <GtkPrintOperationResult*> => <GtkPrintOperationResult>;

define constant $gtk-print-pages-all = 0;
define constant $gtk-print-pages-current = 1;
define constant $gtk-print-pages-ranges = 2;
define constant $gtk-print-pages-selection = 3;
define constant <GtkPrintPages> = <C-int>;
define C-pointer-type <GtkPrintPages*> => <GtkPrintPages>;

define constant $gtk-print-quality-low = 0;
define constant $gtk-print-quality-normal = 1;
define constant $gtk-print-quality-high = 2;
define constant $gtk-print-quality-draft = 3;
define constant <GtkPrintQuality> = <C-int>;
define C-pointer-type <GtkPrintQuality*> => <GtkPrintQuality>;

define open C-subtype <GtkPrintSettings> (<GObject>)
end C-subtype;

define C-pointer-type <GtkPrintSettings*> => <GtkPrintSettings>;

define C-function gtk-print-settings-new
  result res :: <GtkPrintSettings>;
  c-name: "gtk_print_settings_new";
end;

define C-function gtk-print-settings-new-from-file
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPrintSettings>;
  c-name: "gtk_print_settings_new_from_file";
end;

define C-function gtk-print-settings-new-from-key-file
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkPrintSettings>;
  c-name: "gtk_print_settings_new_from_key_file";
end;

define C-function gtk-print-settings-copy
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPrintSettings>;
  c-name: "gtk_print_settings_copy";
end;

define C-function gtk-print-settings-foreach
  input parameter self :: <GtkPrintSettings>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_print_settings_foreach";
end;

define C-function gtk-print-settings-get
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get";
end;

define C-function gtk-print-settings-get-bool
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_get_bool";
end;

define C-function gtk-print-settings-get-collate
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_get_collate";
end;

define C-function gtk-print-settings-get-default-source
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_default_source";
end;

define C-function gtk-print-settings-get-dither
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_dither";
end;

define C-function gtk-print-settings-get-double
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_double";
end;

define C-function gtk-print-settings-get-double-with-default
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter def_ :: <C-double>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_double_with_default";
end;

define C-function gtk-print-settings-get-duplex
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPrintDuplex>;
  c-name: "gtk_print_settings_get_duplex";
end;

define C-function gtk-print-settings-get-finishings
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_finishings";
end;

define C-function gtk-print-settings-get-int
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_int";
end;

define C-function gtk-print-settings-get-int-with-default
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter def_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_int_with_default";
end;

define C-function gtk-print-settings-get-length
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_length";
end;

define C-function gtk-print-settings-get-media-type
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_media_type";
end;

define C-function gtk-print-settings-get-n-copies
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_n_copies";
end;

define C-function gtk-print-settings-get-number-up
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_number_up";
end;

define C-function gtk-print-settings-get-number-up-layout
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkNumberUpLayout>;
  c-name: "gtk_print_settings_get_number_up_layout";
end;

define C-function gtk-print-settings-get-orientation
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPageOrientation>;
  c-name: "gtk_print_settings_get_orientation";
end;

define C-function gtk-print-settings-get-output-bin
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_output_bin";
end;

define C-function gtk-print-settings-get-page-ranges
  input parameter self :: <GtkPrintSettings>;
  output parameter num_ranges_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_print_settings_get_page_ranges";
end;

define C-function gtk-print-settings-get-page-set
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPageSet>;
  c-name: "gtk_print_settings_get_page_set";
end;

define C-function gtk-print-settings-get-paper-height
  input parameter self :: <GtkPrintSettings>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_paper_height";
end;

define C-function gtk-print-settings-get-paper-size
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPaperSize>;
  c-name: "gtk_print_settings_get_paper_size";
end;

define C-function gtk-print-settings-get-paper-width
  input parameter self :: <GtkPrintSettings>;
  input parameter unit_ :: <GtkUnit>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_paper_width";
end;

define C-function gtk-print-settings-get-print-pages
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPrintPages>;
  c-name: "gtk_print_settings_get_print_pages";
end;

define C-function gtk-print-settings-get-printer
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-string>;
  c-name: "gtk_print_settings_get_printer";
end;

define C-function gtk-print-settings-get-printer-lpi
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_printer_lpi";
end;

define C-function gtk-print-settings-get-quality
  input parameter self :: <GtkPrintSettings>;
  result res :: <GtkPrintQuality>;
  c-name: "gtk_print_settings_get_quality";
end;

define C-function gtk-print-settings-get-resolution
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_resolution";
end;

define C-function gtk-print-settings-get-resolution-x
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_resolution_x";
end;

define C-function gtk-print-settings-get-resolution-y
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-signed-int>;
  c-name: "gtk_print_settings_get_resolution_y";
end;

define C-function gtk-print-settings-get-reverse
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_get_reverse";
end;

define C-function gtk-print-settings-get-scale
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-double>;
  c-name: "gtk_print_settings_get_scale";
end;

define C-function gtk-print-settings-get-use-color
  input parameter self :: <GtkPrintSettings>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_get_use_color";
end;

define C-function gtk-print-settings-has-key
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_has_key";
end;

define C-function gtk-print-settings-load-file
  input parameter self :: <GtkPrintSettings>;
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_load_file";
end;

define C-function gtk-print-settings-load-key-file
  input parameter self :: <GtkPrintSettings>;
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_load_key_file";
end;

define C-function gtk-print-settings-set
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-string>;
  c-name: "gtk_print_settings_set";
end;

define C-function gtk-print-settings-set-bool
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-boolean>;
  c-name: "gtk_print_settings_set_bool";
end;

define C-function gtk-print-settings-set-collate
  input parameter self :: <GtkPrintSettings>;
  input parameter collate_ :: <C-boolean>;
  c-name: "gtk_print_settings_set_collate";
end;

define C-function gtk-print-settings-set-default-source
  input parameter self :: <GtkPrintSettings>;
  input parameter default_source_ :: <C-string>;
  c-name: "gtk_print_settings_set_default_source";
end;

define C-function gtk-print-settings-set-dither
  input parameter self :: <GtkPrintSettings>;
  input parameter dither_ :: <C-string>;
  c-name: "gtk_print_settings_set_dither";
end;

define C-function gtk-print-settings-set-double
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-double>;
  c-name: "gtk_print_settings_set_double";
end;

define C-function gtk-print-settings-set-duplex
  input parameter self :: <GtkPrintSettings>;
  input parameter duplex_ :: <GtkPrintDuplex>;
  c-name: "gtk_print_settings_set_duplex";
end;

define C-function gtk-print-settings-set-finishings
  input parameter self :: <GtkPrintSettings>;
  input parameter finishings_ :: <C-string>;
  c-name: "gtk_print_settings_set_finishings";
end;

define C-function gtk-print-settings-set-int
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_int";
end;

define C-function gtk-print-settings-set-length
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_print_settings_set_length";
end;

define C-function gtk-print-settings-set-media-type
  input parameter self :: <GtkPrintSettings>;
  input parameter media_type_ :: <C-string>;
  c-name: "gtk_print_settings_set_media_type";
end;

define C-function gtk-print-settings-set-n-copies
  input parameter self :: <GtkPrintSettings>;
  input parameter num_copies_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_n_copies";
end;

define C-function gtk-print-settings-set-number-up
  input parameter self :: <GtkPrintSettings>;
  input parameter number_up_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_number_up";
end;

define C-function gtk-print-settings-set-number-up-layout
  input parameter self :: <GtkPrintSettings>;
  input parameter number_up_layout_ :: <GtkNumberUpLayout>;
  c-name: "gtk_print_settings_set_number_up_layout";
end;

define C-function gtk-print-settings-set-orientation
  input parameter self :: <GtkPrintSettings>;
  input parameter orientation_ :: <GtkPageOrientation>;
  c-name: "gtk_print_settings_set_orientation";
end;

define C-function gtk-print-settings-set-output-bin
  input parameter self :: <GtkPrintSettings>;
  input parameter output_bin_ :: <C-string>;
  c-name: "gtk_print_settings_set_output_bin";
end;

define C-function gtk-print-settings-set-page-ranges
  input parameter self :: <GtkPrintSettings>;
  input parameter page_ranges_ :: <C-unsigned-char*> /* Not supported */;
  input parameter num_ranges_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_page_ranges";
end;

define C-function gtk-print-settings-set-page-set
  input parameter self :: <GtkPrintSettings>;
  input parameter page_set_ :: <GtkPageSet>;
  c-name: "gtk_print_settings_set_page_set";
end;

define C-function gtk-print-settings-set-paper-height
  input parameter self :: <GtkPrintSettings>;
  input parameter height_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_print_settings_set_paper_height";
end;

define C-function gtk-print-settings-set-paper-size
  input parameter self :: <GtkPrintSettings>;
  input parameter paper_size_ :: <GtkPaperSize>;
  c-name: "gtk_print_settings_set_paper_size";
end;

define C-function gtk-print-settings-set-paper-width
  input parameter self :: <GtkPrintSettings>;
  input parameter width_ :: <C-double>;
  input parameter unit_ :: <GtkUnit>;
  c-name: "gtk_print_settings_set_paper_width";
end;

define C-function gtk-print-settings-set-print-pages
  input parameter self :: <GtkPrintSettings>;
  input parameter pages_ :: <GtkPrintPages>;
  c-name: "gtk_print_settings_set_print_pages";
end;

define C-function gtk-print-settings-set-printer
  input parameter self :: <GtkPrintSettings>;
  input parameter printer_ :: <C-string>;
  c-name: "gtk_print_settings_set_printer";
end;

define C-function gtk-print-settings-set-printer-lpi
  input parameter self :: <GtkPrintSettings>;
  input parameter lpi_ :: <C-double>;
  c-name: "gtk_print_settings_set_printer_lpi";
end;

define C-function gtk-print-settings-set-quality
  input parameter self :: <GtkPrintSettings>;
  input parameter quality_ :: <GtkPrintQuality>;
  c-name: "gtk_print_settings_set_quality";
end;

define C-function gtk-print-settings-set-resolution
  input parameter self :: <GtkPrintSettings>;
  input parameter resolution_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_resolution";
end;

define C-function gtk-print-settings-set-resolution-xy
  input parameter self :: <GtkPrintSettings>;
  input parameter resolution_x_ :: <C-signed-int>;
  input parameter resolution_y_ :: <C-signed-int>;
  c-name: "gtk_print_settings_set_resolution_xy";
end;

define C-function gtk-print-settings-set-reverse
  input parameter self :: <GtkPrintSettings>;
  input parameter reverse_ :: <C-boolean>;
  c-name: "gtk_print_settings_set_reverse";
end;

define C-function gtk-print-settings-set-scale
  input parameter self :: <GtkPrintSettings>;
  input parameter scale_ :: <C-double>;
  c-name: "gtk_print_settings_set_scale";
end;

define C-function gtk-print-settings-set-use-color
  input parameter self :: <GtkPrintSettings>;
  input parameter use_color_ :: <C-boolean>;
  c-name: "gtk_print_settings_set_use_color";
end;

define C-function gtk-print-settings-to-file
  input parameter self :: <GtkPrintSettings>;
  input parameter file_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_print_settings_to_file";
end;

define C-function gtk-print-settings-to-key-file
  input parameter self :: <GtkPrintSettings>;
  input parameter key_file_ :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  c-name: "gtk_print_settings_to_key_file";
end;

define C-function gtk-print-settings-unset
  input parameter self :: <GtkPrintSettings>;
  input parameter key_ :: <C-string>;
  c-name: "gtk_print_settings_unset";
end;

define constant $gtk-print-status-initial = 0;
define constant $gtk-print-status-preparing = 1;
define constant $gtk-print-status-generating-data = 2;
define constant $gtk-print-status-sending-data = 3;
define constant $gtk-print-status-pending = 4;
define constant $gtk-print-status-pending-issue = 5;
define constant $gtk-print-status-printing = 6;
define constant $gtk-print-status-finished = 7;
define constant $gtk-print-status-finished-aborted = 8;
define constant <GtkPrintStatus> = <C-int>;
define C-pointer-type <GtkPrintStatus*> => <GtkPrintStatus>;

define open C-subtype <GtkProgressBar> (<GtkWidget>)
  constant slot gtk-progress-bar-parent :: <GtkWidget>;
  constant slot gtk-progress-bar-priv :: <GtkProgressBarPrivate>;
end C-subtype;

define C-pointer-type <GtkProgressBar*> => <GtkProgressBar>;

define C-function gtk-progress-bar-new
  result res :: <GtkWidget>;
  c-name: "gtk_progress_bar_new";
end;

define C-function gtk-progress-bar-get-ellipsize
  input parameter self :: <GtkProgressBar>;
  result res :: <PangoEllipsizeMode>;
  c-name: "gtk_progress_bar_get_ellipsize";
end;

define C-function gtk-progress-bar-get-fraction
  input parameter self :: <GtkProgressBar>;
  result res :: <C-double>;
  c-name: "gtk_progress_bar_get_fraction";
end;

define C-function gtk-progress-bar-get-inverted
  input parameter self :: <GtkProgressBar>;
  result res :: <C-boolean>;
  c-name: "gtk_progress_bar_get_inverted";
end;

define C-function gtk-progress-bar-get-pulse-step
  input parameter self :: <GtkProgressBar>;
  result res :: <C-double>;
  c-name: "gtk_progress_bar_get_pulse_step";
end;

define C-function gtk-progress-bar-get-show-text
  input parameter self :: <GtkProgressBar>;
  result res :: <C-boolean>;
  c-name: "gtk_progress_bar_get_show_text";
end;

define C-function gtk-progress-bar-get-text
  input parameter self :: <GtkProgressBar>;
  result res :: <C-string>;
  c-name: "gtk_progress_bar_get_text";
end;

define C-function gtk-progress-bar-pulse
  input parameter self :: <GtkProgressBar>;
  c-name: "gtk_progress_bar_pulse";
end;

define C-function gtk-progress-bar-set-ellipsize
  input parameter self :: <GtkProgressBar>;
  input parameter mode_ :: <PangoEllipsizeMode>;
  c-name: "gtk_progress_bar_set_ellipsize";
end;

define C-function gtk-progress-bar-set-fraction
  input parameter self :: <GtkProgressBar>;
  input parameter fraction_ :: <C-double>;
  c-name: "gtk_progress_bar_set_fraction";
end;

define C-function gtk-progress-bar-set-inverted
  input parameter self :: <GtkProgressBar>;
  input parameter inverted_ :: <C-boolean>;
  c-name: "gtk_progress_bar_set_inverted";
end;

define C-function gtk-progress-bar-set-pulse-step
  input parameter self :: <GtkProgressBar>;
  input parameter fraction_ :: <C-double>;
  c-name: "gtk_progress_bar_set_pulse_step";
end;

define C-function gtk-progress-bar-set-show-text
  input parameter self :: <GtkProgressBar>;
  input parameter show_text_ :: <C-boolean>;
  c-name: "gtk_progress_bar_set_show_text";
end;

define C-function gtk-progress-bar-set-text
  input parameter self :: <GtkProgressBar>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_progress_bar_set_text";
end;

define C-struct <_GtkProgressBarClass>
  constant slot gtk-progress-bar-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-progress-bar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-progress-bar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-progress-bar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-progress-bar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkProgressBarClass>;
end C-struct;

define C-struct <_GtkProgressBarPrivate>
  pointer-type-name: <GtkProgressBarPrivate>;
end C-struct;

define open C-subtype <GtkRadioAction> (<GtkToggleAction>)
  constant slot gtk-radio-action-parent :: <GtkToggleAction>;
  constant slot gtk-radio-action-private-data :: <GtkRadioActionPrivate>;
end C-subtype;

define C-pointer-type <GtkRadioAction*> => <GtkRadioAction>;

define C-function gtk-radio-action-new
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter tooltip_ :: <C-string>;
  input parameter stock_id_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  result res :: <GtkRadioAction>;
  c-name: "gtk_radio_action_new";
end;

define C-function gtk-radio-action-get-current-value
  input parameter self :: <GtkRadioAction>;
  result res :: <C-signed-int>;
  c-name: "gtk_radio_action_get_current_value";
end;

define C-function gtk-radio-action-get-group
  input parameter self :: <GtkRadioAction>;
  result res :: <GSList>;
  c-name: "gtk_radio_action_get_group";
end;

define C-function gtk-radio-action-join-group
  input parameter self :: <GtkRadioAction>;
  input parameter group_source_ :: <GtkRadioAction>;
  c-name: "gtk_radio_action_join_group";
end;

define C-function gtk-radio-action-set-current-value
  input parameter self :: <GtkRadioAction>;
  input parameter current_value_ :: <C-signed-int>;
  c-name: "gtk_radio_action_set_current_value";
end;

define C-function gtk-radio-action-set-group
  input parameter self :: <GtkRadioAction>;
  input parameter group_ :: <GSList>;
  c-name: "gtk_radio_action_set_group";
end;

define C-struct <_GtkRadioActionClass>
  constant slot gtk-radio-action-class-parent-class :: <GtkToggleActionClass>;
  constant slot gtk-radio-action-class-changed :: <C-function-pointer>;
  constant slot gtk-radio-action-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-radio-action-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-radio-action-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-radio-action-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRadioActionClass>;
end C-struct;

define C-struct <_GtkRadioActionEntry>
  slot gtk-radio-action-entry-name :: <C-string>;
  slot gtk-radio-action-entry-stock-id :: <C-string>;
  slot gtk-radio-action-entry-label :: <C-string>;
  slot gtk-radio-action-entry-accelerator :: <C-string>;
  slot gtk-radio-action-entry-tooltip :: <C-string>;
  slot gtk-radio-action-entry-value :: <C-signed-int>;
  pointer-type-name: <GtkRadioActionEntry>;
end C-struct;

define C-struct <_GtkRadioActionPrivate>
  pointer-type-name: <GtkRadioActionPrivate>;
end C-struct;

define open C-subtype <GtkRadioButton> (<GtkCheckButton>)
  constant slot gtk-radio-button-check-button :: <GtkCheckButton>;
  constant slot gtk-radio-button-priv :: <GtkRadioButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkRadioButton*> => <GtkRadioButton>;

define C-function gtk-radio-button-new
  input parameter group_ :: <GSList>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new";
end;

define C-function gtk-radio-button-new-from-widget
  input parameter radio_group_member_ :: <GtkRadioButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new_from_widget";
end;

define C-function gtk-radio-button-new-with-label
  input parameter group_ :: <GSList>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new_with_label";
end;

define C-function gtk-radio-button-new-with-label-from-widget
  input parameter radio_group_member_ :: <GtkRadioButton>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new_with_label_from_widget";
end;

define C-function gtk-radio-button-new-with-mnemonic
  input parameter group_ :: <GSList>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new_with_mnemonic";
end;

define C-function gtk-radio-button-new-with-mnemonic-from-widget
  input parameter radio_group_member_ :: <GtkRadioButton>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_button_new_with_mnemonic_from_widget";
end;

define C-function gtk-radio-button-get-group
  input parameter self :: <GtkRadioButton>;
  result res :: <GSList>;
  c-name: "gtk_radio_button_get_group";
end;

define C-function gtk-radio-button-join-group
  input parameter self :: <GtkRadioButton>;
  input parameter group_source_ :: <GtkRadioButton>;
  c-name: "gtk_radio_button_join_group";
end;

define C-function gtk-radio-button-set-group
  input parameter self :: <GtkRadioButton>;
  input parameter group_ :: <GSList>;
  c-name: "gtk_radio_button_set_group";
end;

define C-struct <_GtkRadioButtonClass>
  constant slot gtk-radio-button-class-parent-class :: <GtkCheckButtonClass>;
  constant slot gtk-radio-button-class-group-changed :: <C-function-pointer>;
  constant slot gtk-radio-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-radio-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-radio-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-radio-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRadioButtonClass>;
end C-struct;

define C-struct <_GtkRadioButtonPrivate>
  pointer-type-name: <GtkRadioButtonPrivate>;
end C-struct;

define open C-subtype <GtkRadioMenuItem> (<GtkCheckMenuItem>)
  constant slot gtk-radio-menu-item-check-menu-item :: <GtkCheckMenuItem>;
  constant slot gtk-radio-menu-item-priv :: <GtkRadioMenuItemPrivate>;
end C-subtype;

define C-pointer-type <GtkRadioMenuItem*> => <GtkRadioMenuItem>;

define C-function gtk-radio-menu-item-new-from-widget
  input parameter group_ :: <GtkRadioMenuItem>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_menu_item_new_from_widget";
end;

define C-function gtk-radio-menu-item-new-with-label
  input parameter group_ :: <GSList>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_menu_item_new_with_label";
end;

define C-function gtk-radio-menu-item-new-with-label-from-widget
  input parameter group_ :: <GtkRadioMenuItem>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_menu_item_new_with_label_from_widget";
end;

define C-function gtk-radio-menu-item-new-with-mnemonic-from-widget
  input parameter group_ :: <GtkRadioMenuItem>;
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_radio_menu_item_new_with_mnemonic_from_widget";
end;

define C-function gtk-radio-menu-item-get-group
  input parameter self :: <GtkRadioMenuItem>;
  result res :: <GSList>;
  c-name: "gtk_radio_menu_item_get_group";
end;

define C-struct <_GtkRadioMenuItemClass>
  constant slot gtk-radio-menu-item-class-parent-class :: <GtkCheckMenuItemClass>;
  constant slot gtk-radio-menu-item-class-group-changed :: <C-function-pointer>;
  constant slot gtk-radio-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-radio-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-radio-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-radio-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRadioMenuItemClass>;
end C-struct;

define C-struct <_GtkRadioMenuItemPrivate>
  pointer-type-name: <GtkRadioMenuItemPrivate>;
end C-struct;

define open C-subtype <GtkRadioToolButton> (<GtkToggleToolButton>)
  constant slot gtk-radio-tool-button-parent :: <GtkToggleToolButton>;
end C-subtype;

define C-pointer-type <GtkRadioToolButton*> => <GtkRadioToolButton>;

define C-function gtk-radio-tool-button-new
  input parameter group_ :: <GSList>;
  result res :: <GtkToolItem>;
  c-name: "gtk_radio_tool_button_new";
end;

define C-function gtk-radio-tool-button-new-from-widget
  input parameter group_ :: <GtkRadioToolButton>;
  result res :: <GtkToolItem>;
  c-name: "gtk_radio_tool_button_new_from_widget";
end;

define C-function gtk-radio-tool-button-new-with-stock-from-widget
  input parameter group_ :: <GtkRadioToolButton>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_radio_tool_button_new_with_stock_from_widget";
end;

define C-function gtk-radio-tool-button-get-group
  input parameter self :: <GtkRadioToolButton>;
  result res :: <GSList>;
  c-name: "gtk_radio_tool_button_get_group";
end;

define C-function gtk-radio-tool-button-set-group
  input parameter self :: <GtkRadioToolButton>;
  input parameter group_ :: <GSList>;
  c-name: "gtk_radio_tool_button_set_group";
end;

define C-struct <_GtkRadioToolButtonClass>
  constant slot gtk-radio-tool-button-class-parent-class :: <GtkToggleToolButtonClass>;
  constant slot gtk-radio-tool-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-radio-tool-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-radio-tool-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-radio-tool-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRadioToolButtonClass>;
end C-struct;

define open C-subtype <GtkRange> (<GtkWidget>)
  constant slot gtk-range-widget :: <GtkWidget>;
  constant slot gtk-range-priv :: <GtkRangePrivate>;
end C-subtype;

define C-pointer-type <GtkRange*> => <GtkRange>;

define C-function gtk-range-get-adjustment
  input parameter self :: <GtkRange>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_range_get_adjustment";
end;

define C-function gtk-range-get-event-window
  input parameter self :: <GtkRange>;
  result res :: <GdkWindow>;
  c-name: "gtk_range_get_event_window";
end;

define C-function gtk-range-get-fill-level
  input parameter self :: <GtkRange>;
  result res :: <C-double>;
  c-name: "gtk_range_get_fill_level";
end;

define C-function gtk-range-get-flippable
  input parameter self :: <GtkRange>;
  result res :: <C-boolean>;
  c-name: "gtk_range_get_flippable";
end;

define C-function gtk-range-get-inverted
  input parameter self :: <GtkRange>;
  result res :: <C-boolean>;
  c-name: "gtk_range_get_inverted";
end;

define C-function gtk-range-get-lower-stepper-sensitivity
  input parameter self :: <GtkRange>;
  result res :: <GtkSensitivityType>;
  c-name: "gtk_range_get_lower_stepper_sensitivity";
end;

define C-function gtk-range-get-min-slider-size
  input parameter self :: <GtkRange>;
  result res :: <C-signed-int>;
  c-name: "gtk_range_get_min_slider_size";
end;

define C-function gtk-range-get-range-rect
  input parameter self :: <GtkRange>;
  output parameter range_rect_ :: <cairoRectangleInt>;
  c-name: "gtk_range_get_range_rect";
end;

define C-function gtk-range-get-restrict-to-fill-level
  input parameter self :: <GtkRange>;
  result res :: <C-boolean>;
  c-name: "gtk_range_get_restrict_to_fill_level";
end;

define C-function gtk-range-get-round-digits
  input parameter self :: <GtkRange>;
  result res :: <C-signed-int>;
  c-name: "gtk_range_get_round_digits";
end;

define C-function gtk-range-get-show-fill-level
  input parameter self :: <GtkRange>;
  result res :: <C-boolean>;
  c-name: "gtk_range_get_show_fill_level";
end;

define C-function gtk-range-get-slider-range
  input parameter self :: <GtkRange>;
  output parameter slider_start_ :: <C-signed-int*>;
  output parameter slider_end_ :: <C-signed-int*>;
  c-name: "gtk_range_get_slider_range";
end;

define C-function gtk-range-get-slider-size-fixed
  input parameter self :: <GtkRange>;
  result res :: <C-boolean>;
  c-name: "gtk_range_get_slider_size_fixed";
end;

define C-function gtk-range-get-upper-stepper-sensitivity
  input parameter self :: <GtkRange>;
  result res :: <GtkSensitivityType>;
  c-name: "gtk_range_get_upper_stepper_sensitivity";
end;

define C-function gtk-range-get-value
  input parameter self :: <GtkRange>;
  result res :: <C-double>;
  c-name: "gtk_range_get_value";
end;

define C-function gtk-range-set-adjustment
  input parameter self :: <GtkRange>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_range_set_adjustment";
end;

define C-function gtk-range-set-fill-level
  input parameter self :: <GtkRange>;
  input parameter fill_level_ :: <C-double>;
  c-name: "gtk_range_set_fill_level";
end;

define C-function gtk-range-set-flippable
  input parameter self :: <GtkRange>;
  input parameter flippable_ :: <C-boolean>;
  c-name: "gtk_range_set_flippable";
end;

define C-function gtk-range-set-increments
  input parameter self :: <GtkRange>;
  input parameter step_ :: <C-double>;
  input parameter page_ :: <C-double>;
  c-name: "gtk_range_set_increments";
end;

define C-function gtk-range-set-inverted
  input parameter self :: <GtkRange>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_range_set_inverted";
end;

define C-function gtk-range-set-lower-stepper-sensitivity
  input parameter self :: <GtkRange>;
  input parameter sensitivity_ :: <GtkSensitivityType>;
  c-name: "gtk_range_set_lower_stepper_sensitivity";
end;

define C-function gtk-range-set-min-slider-size
  input parameter self :: <GtkRange>;
  input parameter min_size_ :: <C-signed-int>;
  c-name: "gtk_range_set_min_slider_size";
end;

define C-function gtk-range-set-range
  input parameter self :: <GtkRange>;
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  c-name: "gtk_range_set_range";
end;

define C-function gtk-range-set-restrict-to-fill-level
  input parameter self :: <GtkRange>;
  input parameter restrict_to_fill_level_ :: <C-boolean>;
  c-name: "gtk_range_set_restrict_to_fill_level";
end;

define C-function gtk-range-set-round-digits
  input parameter self :: <GtkRange>;
  input parameter round_digits_ :: <C-signed-int>;
  c-name: "gtk_range_set_round_digits";
end;

define C-function gtk-range-set-show-fill-level
  input parameter self :: <GtkRange>;
  input parameter show_fill_level_ :: <C-boolean>;
  c-name: "gtk_range_set_show_fill_level";
end;

define C-function gtk-range-set-slider-size-fixed
  input parameter self :: <GtkRange>;
  input parameter size_fixed_ :: <C-boolean>;
  c-name: "gtk_range_set_slider_size_fixed";
end;

define C-function gtk-range-set-upper-stepper-sensitivity
  input parameter self :: <GtkRange>;
  input parameter sensitivity_ :: <GtkSensitivityType>;
  c-name: "gtk_range_set_upper_stepper_sensitivity";
end;

define C-function gtk-range-set-value
  input parameter self :: <GtkRange>;
  input parameter value_ :: <C-double>;
  c-name: "gtk_range_set_value";
end;

define C-struct <_GtkRangeClass>
  constant slot gtk-range-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-range-class-slider-detail :: <C-string>;
  constant slot gtk-range-class-stepper-detail :: <C-string>;
  constant slot gtk-range-class-value-changed :: <C-function-pointer>;
  constant slot gtk-range-class-adjust-bounds :: <C-function-pointer>;
  constant slot gtk-range-class-move-slider :: <C-function-pointer>;
  constant slot gtk-range-class-get-range-border :: <C-function-pointer>;
  constant slot gtk-range-class-change-value :: <C-function-pointer>;
  constant slot gtk-range-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-range-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-range-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-range-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRangeClass>;
end C-struct;

define C-struct <_GtkRangePrivate>
  pointer-type-name: <GtkRangePrivate>;
end C-struct;

define C-struct <_GtkRcContext>
  pointer-type-name: <GtkRcContext>;
end C-struct;

define constant $gtk-rc-fg = 1;
define constant $gtk-rc-bg = 2;
define constant $gtk-rc-text = 4;
define constant $gtk-rc-base = 8;
define constant <GtkRcFlags> = <C-int>;
define C-pointer-type <GtkRcFlags*> => <GtkRcFlags>;

define open C-subtype <GtkRcStyle> (<GObject>)
  constant slot gtk-rc-style-parent-instance :: <GObject>;
  constant slot gtk-rc-style-name :: <C-string>;
  constant slot gtk-rc-style-bg-pixmap-name :: <C-string*>;
  constant slot gtk-rc-style-font-desc :: <PangoFontDescription>;
  constant slot gtk-rc-style-color-flags :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-rc-style-fg :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-rc-style-bg :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-rc-style-text :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-rc-style-base :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-rc-style-xthickness :: <C-signed-int>;
  constant slot gtk-rc-style-ythickness :: <C-signed-int>;
  constant slot gtk-rc-style-rc-properties :: <GArray>;
  constant slot gtk-rc-style-rc-style-lists :: <GSList>;
  constant slot gtk-rc-style-icon-factories :: <GSList>;
  constant slot gtk-rc-style-engine-specified :: <C-unsigned-int>;
end C-subtype;

define C-pointer-type <GtkRcStyle*> => <GtkRcStyle>;

define C-function gtk-rc-style-new
  result res :: <GtkRcStyle>;
  c-name: "gtk_rc_style_new";
end;

define C-function gtk-rc-style-copy
  input parameter self :: <GtkRcStyle>;
  result res :: <GtkRcStyle>;
  c-name: "gtk_rc_style_copy";
end;

define C-struct <_GtkRcStyleClass>
  constant slot gtk-rc-style-class-parent-class :: <GObjectClass>;
  constant slot gtk-rc-style-class-create-rc-style :: <C-void*>;
  constant slot gtk-rc-style-class-parse :: <C-function-pointer>;
  constant slot gtk-rc-style-class-merge :: <C-function-pointer>;
  constant slot gtk-rc-style-class-create-style :: <C-void*>;
  constant slot gtk-rc-style-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-rc-style-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-rc-style-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-rc-style-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRcStyleClass>;
end C-struct;

define open C-subtype <GtkRecentAction> (<GtkAction>)
  constant slot gtk-recent-action-parent-instance :: <GtkAction>;
  constant slot gtk-recent-action-priv :: <GtkRecentActionPrivate>;
end C-subtype;

define C-pointer-type <GtkRecentAction*> => <GtkRecentAction>;

define C-function gtk-recent-action-new
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter tooltip_ :: <C-string>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkAction>;
  c-name: "gtk_recent_action_new";
end;

define C-function gtk-recent-action-new-for-manager
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter tooltip_ :: <C-string>;
  input parameter stock_id_ :: <C-string>;
  input parameter manager_ :: <GtkRecentManager>;
  result res :: <GtkAction>;
  c-name: "gtk_recent_action_new_for_manager";
end;

define C-function gtk-recent-action-get-show-numbers
  input parameter self :: <GtkRecentAction>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_action_get_show_numbers";
end;

define C-function gtk-recent-action-set-show-numbers
  input parameter self :: <GtkRecentAction>;
  input parameter show_numbers_ :: <C-boolean>;
  c-name: "gtk_recent_action_set_show_numbers";
end;

define C-struct <_GtkRecentActionClass>
  constant slot gtk-recent-action-class-parent-class :: <GtkActionClass>;
  constant slot gtk-recent-action-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-recent-action-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-recent-action-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-recent-action-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRecentActionClass>;
end C-struct;

define C-struct <_GtkRecentActionPrivate>
  pointer-type-name: <GtkRecentActionPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkRecentChooser> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkRecentChooser*> => <GtkRecentChooser>;

define C-function gtk-recent-chooser-add-filter
  input parameter self :: <GtkRecentChooser>;
  input parameter filter_ :: <GtkRecentFilter>;
  c-name: "gtk_recent_chooser_add_filter";
end;

define C-function gtk-recent-chooser-get-current-item
  input parameter self :: <GtkRecentChooser>;
  result res :: <GtkRecentInfo>;
  c-name: "gtk_recent_chooser_get_current_item";
end;

define C-function gtk-recent-chooser-get-current-uri
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-string>;
  c-name: "gtk_recent_chooser_get_current_uri";
end;

define C-function gtk-recent-chooser-get-filter
  input parameter self :: <GtkRecentChooser>;
  result res :: <GtkRecentFilter>;
  c-name: "gtk_recent_chooser_get_filter";
end;

define C-function gtk-recent-chooser-get-items
  input parameter self :: <GtkRecentChooser>;
  result res :: <GList>;
  c-name: "gtk_recent_chooser_get_items";
end;

define C-function gtk-recent-chooser-get-limit
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-signed-int>;
  c-name: "gtk_recent_chooser_get_limit";
end;

define C-function gtk-recent-chooser-get-local-only
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_local_only";
end;

define C-function gtk-recent-chooser-get-select-multiple
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_select_multiple";
end;

define C-function gtk-recent-chooser-get-show-icons
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_show_icons";
end;

define C-function gtk-recent-chooser-get-show-not-found
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_show_not_found";
end;

define C-function gtk-recent-chooser-get-show-private
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_show_private";
end;

define C-function gtk-recent-chooser-get-show-tips
  input parameter self :: <GtkRecentChooser>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_get_show_tips";
end;

define C-function gtk-recent-chooser-get-sort-type
  input parameter self :: <GtkRecentChooser>;
  result res :: <GtkRecentSortType>;
  c-name: "gtk_recent_chooser_get_sort_type";
end;

define C-function gtk-recent-chooser-get-uris
  input parameter self :: <GtkRecentChooser>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "gtk_recent_chooser_get_uris";
end;

define C-function gtk-recent-chooser-list-filters
  input parameter self :: <GtkRecentChooser>;
  result res :: <GSList>;
  c-name: "gtk_recent_chooser_list_filters";
end;

define C-function gtk-recent-chooser-remove-filter
  input parameter self :: <GtkRecentChooser>;
  input parameter filter_ :: <GtkRecentFilter>;
  c-name: "gtk_recent_chooser_remove_filter";
end;

define C-function gtk-recent-chooser-select-all
  input parameter self :: <GtkRecentChooser>;
  c-name: "gtk_recent_chooser_select_all";
end;

define C-function gtk-recent-chooser-select-uri
  input parameter self :: <GtkRecentChooser>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_select_uri";
end;

define C-function gtk-recent-chooser-set-current-uri
  input parameter self :: <GtkRecentChooser>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_current_uri";
end;

define C-function gtk-recent-chooser-set-filter
  input parameter self :: <GtkRecentChooser>;
  input parameter filter_ :: <GtkRecentFilter>;
  c-name: "gtk_recent_chooser_set_filter";
end;

define C-function gtk-recent-chooser-set-limit
  input parameter self :: <GtkRecentChooser>;
  input parameter limit_ :: <C-signed-int>;
  c-name: "gtk_recent_chooser_set_limit";
end;

define C-function gtk-recent-chooser-set-local-only
  input parameter self :: <GtkRecentChooser>;
  input parameter local_only_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_local_only";
end;

define C-function gtk-recent-chooser-set-select-multiple
  input parameter self :: <GtkRecentChooser>;
  input parameter select_multiple_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_select_multiple";
end;

define C-function gtk-recent-chooser-set-show-icons
  input parameter self :: <GtkRecentChooser>;
  input parameter show_icons_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_show_icons";
end;

define C-function gtk-recent-chooser-set-show-not-found
  input parameter self :: <GtkRecentChooser>;
  input parameter show_not_found_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_show_not_found";
end;

define C-function gtk-recent-chooser-set-show-private
  input parameter self :: <GtkRecentChooser>;
  input parameter show_private_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_show_private";
end;

define C-function gtk-recent-chooser-set-show-tips
  input parameter self :: <GtkRecentChooser>;
  input parameter show_tips_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_set_show_tips";
end;

define C-function gtk-recent-chooser-set-sort-func
  input parameter self :: <GtkRecentChooser>;
  input parameter sort_func_ :: <C-function-pointer>;
  input parameter sort_data_ :: <C-void*>;
  input parameter data_destroy_ :: <C-function-pointer>;
  c-name: "gtk_recent_chooser_set_sort_func";
end;

define C-function gtk-recent-chooser-set-sort-type
  input parameter self :: <GtkRecentChooser>;
  input parameter sort_type_ :: <GtkRecentSortType>;
  c-name: "gtk_recent_chooser_set_sort_type";
end;

define C-function gtk-recent-chooser-unselect-all
  input parameter self :: <GtkRecentChooser>;
  c-name: "gtk_recent_chooser_unselect_all";
end;

define C-function gtk-recent-chooser-unselect-uri
  input parameter self :: <GtkRecentChooser>;
  input parameter uri_ :: <C-string>;
  c-name: "gtk_recent_chooser_unselect_uri";
end;

define open C-subtype <GtkRecentChooserDialog> (<GtkDialog>)
  constant slot gtk-recent-chooser-dialog-parent-instance :: <GtkDialog>;
  constant slot gtk-recent-chooser-dialog-priv :: <GtkRecentChooserDialogPrivate>;
end C-subtype;

define C-pointer-type <GtkRecentChooserDialog*> => <GtkRecentChooserDialog>;

define C-struct <_GtkRecentChooserDialogClass>
  constant slot gtk-recent-chooser-dialog-class-parent-class :: <GtkDialogClass>;
  constant slot gtk-recent-chooser-dialog-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-recent-chooser-dialog-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-recent-chooser-dialog-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-recent-chooser-dialog-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRecentChooserDialogClass>;
end C-struct;

define C-struct <_GtkRecentChooserDialogPrivate>
  pointer-type-name: <GtkRecentChooserDialogPrivate>;
end C-struct;

define constant $gtk-recent-chooser-error-not-found = 0;
define constant $gtk-recent-chooser-error-invalid-uri = 1;
define constant <GtkRecentChooserError> = <C-int>;
define C-pointer-type <GtkRecentChooserError*> => <GtkRecentChooserError>;

define C-struct <_GtkRecentChooserIface>
  constant slot gtk-recent-chooser-iface-base-iface :: <GTypeInterface>;
  constant slot gtk-recent-chooser-iface-set-current-uri :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-get-current-uri :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-select-uri :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-unselect-uri :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-select-all :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-unselect-all :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-get-items :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-get-recent-manager :: <C-void*>;
  constant slot gtk-recent-chooser-iface-add-filter :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-remove-filter :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-list-filters :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-set-sort-func :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-item-activated :: <C-function-pointer>;
  constant slot gtk-recent-chooser-iface-selection-changed :: <C-function-pointer>;
  pointer-type-name: <GtkRecentChooserIface>;
end C-struct;

define open C-subtype <GtkRecentChooserMenu> (<GtkMenu>)
  constant slot gtk-recent-chooser-menu-parent-instance :: <GtkMenu>;
  constant slot gtk-recent-chooser-menu-priv :: <GtkRecentChooserMenuPrivate>;
end C-subtype;

define C-pointer-type <GtkRecentChooserMenu*> => <GtkRecentChooserMenu>;

define C-function gtk-recent-chooser-menu-new
  result res :: <GtkWidget>;
  c-name: "gtk_recent_chooser_menu_new";
end;

define C-function gtk-recent-chooser-menu-new-for-manager
  input parameter manager_ :: <GtkRecentManager>;
  result res :: <GtkWidget>;
  c-name: "gtk_recent_chooser_menu_new_for_manager";
end;

define C-function gtk-recent-chooser-menu-get-show-numbers
  input parameter self :: <GtkRecentChooserMenu>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_chooser_menu_get_show_numbers";
end;

define C-function gtk-recent-chooser-menu-set-show-numbers
  input parameter self :: <GtkRecentChooserMenu>;
  input parameter show_numbers_ :: <C-boolean>;
  c-name: "gtk_recent_chooser_menu_set_show_numbers";
end;

define C-struct <_GtkRecentChooserMenuClass>
  constant slot gtk-recent-chooser-menu-class-parent-class :: <GtkMenuClass>;
  constant slot gtk-recent-chooser-menu-class-gtk-recent1 :: <C-function-pointer>;
  constant slot gtk-recent-chooser-menu-class-gtk-recent2 :: <C-function-pointer>;
  constant slot gtk-recent-chooser-menu-class-gtk-recent3 :: <C-function-pointer>;
  constant slot gtk-recent-chooser-menu-class-gtk-recent4 :: <C-function-pointer>;
  pointer-type-name: <GtkRecentChooserMenuClass>;
end C-struct;

define C-struct <_GtkRecentChooserMenuPrivate>
  pointer-type-name: <GtkRecentChooserMenuPrivate>;
end C-struct;

define open C-subtype <GtkRecentChooserWidget> (<GtkBox>)
  constant slot gtk-recent-chooser-widget-parent-instance :: <GtkBox>;
  constant slot gtk-recent-chooser-widget-priv :: <GtkRecentChooserWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkRecentChooserWidget*> => <GtkRecentChooserWidget>;

define C-function gtk-recent-chooser-widget-new
  result res :: <GtkWidget>;
  c-name: "gtk_recent_chooser_widget_new";
end;

define C-function gtk-recent-chooser-widget-new-for-manager
  input parameter manager_ :: <GtkRecentManager>;
  result res :: <GtkWidget>;
  c-name: "gtk_recent_chooser_widget_new_for_manager";
end;

define C-struct <_GtkRecentChooserWidgetClass>
  constant slot gtk-recent-chooser-widget-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-recent-chooser-widget-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-recent-chooser-widget-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-recent-chooser-widget-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-recent-chooser-widget-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkRecentChooserWidgetClass>;
end C-struct;

define C-struct <_GtkRecentChooserWidgetPrivate>
  pointer-type-name: <GtkRecentChooserWidgetPrivate>;
end C-struct;

define C-struct <_GtkRecentData>
  slot gtk-recent-data-display-name :: <C-string>;
  slot gtk-recent-data-description :: <C-string>;
  slot gtk-recent-data-mime-type :: <C-string>;
  slot gtk-recent-data-app-name :: <C-string>;
  slot gtk-recent-data-app-exec :: <C-string>;
  slot gtk-recent-data-groups :: <C-string>;
  slot gtk-recent-data-is-private :: <C-boolean>;
  pointer-type-name: <GtkRecentData>;
end C-struct;

define open C-subtype <GtkRecentFilter> (<GInitiallyUnowned>)
end C-subtype;

define C-pointer-type <GtkRecentFilter*> => <GtkRecentFilter>;

define C-function gtk-recent-filter-new
  result res :: <GtkRecentFilter>;
  c-name: "gtk_recent_filter_new";
end;

define C-function gtk-recent-filter-add-age
  input parameter self :: <GtkRecentFilter>;
  input parameter days_ :: <C-signed-int>;
  c-name: "gtk_recent_filter_add_age";
end;

define C-function gtk-recent-filter-add-application
  input parameter self :: <GtkRecentFilter>;
  input parameter application_ :: <C-string>;
  c-name: "gtk_recent_filter_add_application";
end;

define C-function gtk-recent-filter-add-custom
  input parameter self :: <GtkRecentFilter>;
  input parameter needed_ :: <GtkRecentFilterFlags>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter data_destroy_ :: <C-function-pointer>;
  c-name: "gtk_recent_filter_add_custom";
end;

define C-function gtk-recent-filter-add-group
  input parameter self :: <GtkRecentFilter>;
  input parameter group_ :: <C-string>;
  c-name: "gtk_recent_filter_add_group";
end;

define C-function gtk-recent-filter-add-mime-type
  input parameter self :: <GtkRecentFilter>;
  input parameter mime_type_ :: <C-string>;
  c-name: "gtk_recent_filter_add_mime_type";
end;

define C-function gtk-recent-filter-add-pattern
  input parameter self :: <GtkRecentFilter>;
  input parameter pattern_ :: <C-string>;
  c-name: "gtk_recent_filter_add_pattern";
end;

define C-function gtk-recent-filter-add-pixbuf-formats
  input parameter self :: <GtkRecentFilter>;
  c-name: "gtk_recent_filter_add_pixbuf_formats";
end;

define C-function gtk-recent-filter-filter
  input parameter self :: <GtkRecentFilter>;
  input parameter filter_info_ :: <GtkRecentFilterInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_filter_filter";
end;

define C-function gtk-recent-filter-get-name
  input parameter self :: <GtkRecentFilter>;
  result res :: <C-string>;
  c-name: "gtk_recent_filter_get_name";
end;

define C-function gtk-recent-filter-get-needed
  input parameter self :: <GtkRecentFilter>;
  result res :: <GtkRecentFilterFlags>;
  c-name: "gtk_recent_filter_get_needed";
end;

define C-function gtk-recent-filter-set-name
  input parameter self :: <GtkRecentFilter>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_recent_filter_set_name";
end;

define constant $gtk-recent-filter-uri = 1;
define constant $gtk-recent-filter-display-name = 2;
define constant $gtk-recent-filter-mime-type = 4;
define constant $gtk-recent-filter-application = 8;
define constant $gtk-recent-filter-group = 16;
define constant $gtk-recent-filter-age = 32;
define constant <GtkRecentFilterFlags> = <C-int>;
define C-pointer-type <GtkRecentFilterFlags*> => <GtkRecentFilterFlags>;

define C-struct <_GtkRecentFilterInfo>
  slot gtk-recent-filter-info-contains :: <GtkRecentFilterFlags>;
  slot gtk-recent-filter-info-uri :: <C-string>;
  slot gtk-recent-filter-info-display-name :: <C-string>;
  slot gtk-recent-filter-info-mime-type :: <C-string>;
  slot gtk-recent-filter-info-applications :: <C-string>;
  slot gtk-recent-filter-info-groups :: <C-string>;
  slot gtk-recent-filter-info-age :: <C-signed-int>;
  pointer-type-name: <GtkRecentFilterInfo>;
end C-struct;

define C-struct <_GtkRecentInfo>
  pointer-type-name: <GtkRecentInfo>;
end C-struct;

define C-function gtk-recent-info-create-app-info
  input parameter self :: <GtkRecentInfo>;
  input parameter app_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GAppInfo>;
  c-name: "gtk_recent_info_create_app_info";
end;

define C-function gtk-recent-info-exists
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_exists";
end;

define C-function gtk-recent-info-get-added
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-signed-long>;
  c-name: "gtk_recent_info_get_added";
end;

define C-function gtk-recent-info-get-age
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-signed-int>;
  c-name: "gtk_recent_info_get_age";
end;

define C-function gtk-recent-info-get-application-info
  input parameter self :: <GtkRecentInfo>;
  input parameter app_name_ :: <C-string>;
  output parameter app_exec_ :: <C-string>;
  output parameter count_ :: <C-unsigned-int*>;
  output parameter time__ :: <C-signed-long*>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_get_application_info";
end;

define C-function gtk-recent-info-get-applications
  input parameter self :: <GtkRecentInfo>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "gtk_recent_info_get_applications";
end;

define C-function gtk-recent-info-get-description
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_description";
end;

define C-function gtk-recent-info-get-display-name
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_display_name";
end;

define C-function gtk-recent-info-get-gicon
  input parameter self :: <GtkRecentInfo>;
  result res :: <GIcon>;
  c-name: "gtk_recent_info_get_gicon";
end;

define C-function gtk-recent-info-get-groups
  input parameter self :: <GtkRecentInfo>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "gtk_recent_info_get_groups";
end;

define C-function gtk-recent-info-get-icon
  input parameter self :: <GtkRecentInfo>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_recent_info_get_icon";
end;

define C-function gtk-recent-info-get-mime-type
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_mime_type";
end;

define C-function gtk-recent-info-get-modified
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-signed-long>;
  c-name: "gtk_recent_info_get_modified";
end;

define C-function gtk-recent-info-get-private-hint
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_get_private_hint";
end;

define C-function gtk-recent-info-get-short-name
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_short_name";
end;

define C-function gtk-recent-info-get-uri
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_uri";
end;

define C-function gtk-recent-info-get-uri-display
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_get_uri_display";
end;

define C-function gtk-recent-info-get-visited
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-signed-long>;
  c-name: "gtk_recent_info_get_visited";
end;

define C-function gtk-recent-info-has-application
  input parameter self :: <GtkRecentInfo>;
  input parameter app_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_has_application";
end;

define C-function gtk-recent-info-has-group
  input parameter self :: <GtkRecentInfo>;
  input parameter group_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_has_group";
end;

define C-function gtk-recent-info-is-local
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_is_local";
end;

define C-function gtk-recent-info-last-application
  input parameter self :: <GtkRecentInfo>;
  result res :: <C-string>;
  c-name: "gtk_recent_info_last_application";
end;

define C-function gtk-recent-info-match
  input parameter self :: <GtkRecentInfo>;
  input parameter info_b_ :: <GtkRecentInfo>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_info_match";
end;

define C-function gtk-recent-info-ref
  input parameter self :: <GtkRecentInfo>;
  result res :: <GtkRecentInfo>;
  c-name: "gtk_recent_info_ref";
end;

define C-function gtk-recent-info-unref
  input parameter self :: <GtkRecentInfo>;
  c-name: "gtk_recent_info_unref";
end;

define open C-subtype <GtkRecentManager> (<GObject>)
  constant slot gtk-recent-manager-parent-instance :: <GObject>;
  constant slot gtk-recent-manager-priv :: <GtkRecentManagerPrivate>;
end C-subtype;

define C-pointer-type <GtkRecentManager*> => <GtkRecentManager>;

define C-function gtk-recent-manager-new
  result res :: <GtkRecentManager>;
  c-name: "gtk_recent_manager_new";
end;

define C-function gtk-recent-manager-get-default
  result res :: <GtkRecentManager>;
  c-name: "gtk_recent_manager_get_default";
end;

define C-function gtk-recent-manager-add-full
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  input parameter recent_data_ :: <GtkRecentData>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_manager_add_full";
end;

define C-function gtk-recent-manager-add-item
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_manager_add_item";
end;

define C-function gtk-recent-manager-get-items
  input parameter self :: <GtkRecentManager>;
  result res :: <GList>;
  c-name: "gtk_recent_manager_get_items";
end;

define C-function gtk-recent-manager-has-item
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_manager_has_item";
end;

define C-function gtk-recent-manager-lookup-item
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GtkRecentInfo>;
  c-name: "gtk_recent_manager_lookup_item";
end;

define C-function gtk-recent-manager-move-item
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  input parameter new_uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_manager_move_item";
end;

define C-function gtk-recent-manager-purge-items
  input parameter self :: <GtkRecentManager>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "gtk_recent_manager_purge_items";
end;

define C-function gtk-recent-manager-remove-item
  input parameter self :: <GtkRecentManager>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_recent_manager_remove_item";
end;

define C-struct <_GtkRecentManagerClass>
  constant slot gtk-recent-manager-class-parent-class :: <GObjectClass>;
  constant slot gtk-recent-manager-class-changed :: <C-function-pointer>;
  constant slot gtk-recent-manager-class-_gtk-recent1 :: <C-void*>;
  constant slot gtk-recent-manager-class-_gtk-recent2 :: <C-void*>;
  constant slot gtk-recent-manager-class-_gtk-recent3 :: <C-void*>;
  constant slot gtk-recent-manager-class-_gtk-recent4 :: <C-void*>;
  pointer-type-name: <GtkRecentManagerClass>;
end C-struct;

define constant $gtk-recent-manager-error-not-found = 0;
define constant $gtk-recent-manager-error-invalid-uri = 1;
define constant $gtk-recent-manager-error-invalid-encoding = 2;
define constant $gtk-recent-manager-error-not-registered = 3;
define constant $gtk-recent-manager-error-read = 4;
define constant $gtk-recent-manager-error-write = 5;
define constant $gtk-recent-manager-error-unknown = 6;
define constant <GtkRecentManagerError> = <C-int>;
define C-pointer-type <GtkRecentManagerError*> => <GtkRecentManagerError>;

define C-struct <_GtkRecentManagerPrivate>
  pointer-type-name: <GtkRecentManagerPrivate>;
end C-struct;

define constant $gtk-recent-sort-none = 0;
define constant $gtk-recent-sort-mru = 1;
define constant $gtk-recent-sort-lru = 2;
define constant $gtk-recent-sort-custom = 3;
define constant <GtkRecentSortType> = <C-int>;
define C-pointer-type <GtkRecentSortType*> => <GtkRecentSortType>;

define constant $gtk-region-even = 1;
define constant $gtk-region-odd = 2;
define constant $gtk-region-first = 4;
define constant $gtk-region-last = 8;
define constant $gtk-region-only = 16;
define constant $gtk-region-sorted = 32;
define constant <GtkRegionFlags> = <C-int>;
define C-pointer-type <GtkRegionFlags*> => <GtkRegionFlags>;

define constant $gtk-relief-normal = 0;
define constant $gtk-relief-half = 1;
define constant $gtk-relief-none = 2;
define constant <GtkReliefStyle> = <C-int>;
define C-pointer-type <GtkReliefStyle*> => <GtkReliefStyle>;

define C-struct <_GtkRequestedSize>
  slot gtk-requested-size-data :: <C-void*>;
  slot gtk-requested-size-minimum-size :: <C-signed-int>;
  slot gtk-requested-size-natural-size :: <C-signed-int>;
  pointer-type-name: <GtkRequestedSize>;
end C-struct;

define C-struct <_GtkRequisition>
  slot gtk-requisition-width :: <C-signed-int>;
  slot gtk-requisition-height :: <C-signed-int>;
  pointer-type-name: <GtkRequisition>;
end C-struct;

define C-function gtk-requisition-new
  result res :: <GtkRequisition>;
  c-name: "gtk_requisition_new";
end;

define C-function gtk-requisition-copy
  input parameter self :: <GtkRequisition>;
  result res :: <GtkRequisition>;
  c-name: "gtk_requisition_copy";
end;

define C-function gtk-requisition-free
  input parameter self :: <GtkRequisition>;
  c-name: "gtk_requisition_free";
end;

define constant $gtk-resize-parent = 0;
define constant $gtk-resize-queue = 1;
define constant $gtk-resize-immediate = 2;
define constant <GtkResizeMode> = <C-int>;
define C-pointer-type <GtkResizeMode*> => <GtkResizeMode>;

define constant $gtk-response-none = -1;
define constant $gtk-response-reject = -2;
define constant $gtk-response-accept = -3;
define constant $gtk-response-delete-event = -4;
define constant $gtk-response-ok = -5;
define constant $gtk-response-cancel = -6;
define constant $gtk-response-close = -7;
define constant $gtk-response-yes = -8;
define constant $gtk-response-no = -9;
define constant $gtk-response-apply = -10;
define constant $gtk-response-help = -11;
define constant <GtkResponseType> = <C-int>;
define C-pointer-type <GtkResponseType*> => <GtkResponseType>;

define constant $stock-about = "gtk-about";

define constant $stock-add = "gtk-add";

define constant $stock-apply = "gtk-apply";

define constant $stock-bold = "gtk-bold";

define constant $stock-cancel = "gtk-cancel";

define constant $stock-caps-lock-warning = "gtk-caps-lock-warning";

define constant $stock-cdrom = "gtk-cdrom";

define constant $stock-clear = "gtk-clear";

define constant $stock-close = "gtk-close";

define constant $stock-color-picker = "gtk-color-picker";

define constant $stock-connect = "gtk-connect";

define constant $stock-convert = "gtk-convert";

define constant $stock-copy = "gtk-copy";

define constant $stock-cut = "gtk-cut";

define constant $stock-delete = "gtk-delete";

define constant $stock-dialog-authentication = "gtk-dialog-authentication";

define constant $stock-dialog-error = "gtk-dialog-error";

define constant $stock-dialog-info = "gtk-dialog-info";

define constant $stock-dialog-question = "gtk-dialog-question";

define constant $stock-dialog-warning = "gtk-dialog-warning";

define constant $stock-directory = "gtk-directory";

define constant $stock-discard = "gtk-discard";

define constant $stock-disconnect = "gtk-disconnect";

define constant $stock-dnd = "gtk-dnd";

define constant $stock-dnd-multiple = "gtk-dnd-multiple";

define constant $stock-edit = "gtk-edit";

define constant $stock-execute = "gtk-execute";

define constant $stock-file = "gtk-file";

define constant $stock-find = "gtk-find";

define constant $stock-find-and-replace = "gtk-find-and-replace";

define constant $stock-floppy = "gtk-floppy";

define constant $stock-fullscreen = "gtk-fullscreen";

define constant $stock-goto-bottom = "gtk-goto-bottom";

define constant $stock-goto-first = "gtk-goto-first";

define constant $stock-goto-last = "gtk-goto-last";

define constant $stock-goto-top = "gtk-goto-top";

define constant $stock-go-back = "gtk-go-back";

define constant $stock-go-down = "gtk-go-down";

define constant $stock-go-forward = "gtk-go-forward";

define constant $stock-go-up = "gtk-go-up";

define constant $stock-harddisk = "gtk-harddisk";

define constant $stock-help = "gtk-help";

define constant $stock-home = "gtk-home";

define constant $stock-indent = "gtk-indent";

define constant $stock-index = "gtk-index";

define constant $stock-info = "gtk-info";

define constant $stock-italic = "gtk-italic";

define constant $stock-jump-to = "gtk-jump-to";

define constant $stock-justify-center = "gtk-justify-center";

define constant $stock-justify-fill = "gtk-justify-fill";

define constant $stock-justify-left = "gtk-justify-left";

define constant $stock-justify-right = "gtk-justify-right";

define constant $stock-leave-fullscreen = "gtk-leave-fullscreen";

define constant $stock-media-forward = "gtk-media-forward";

define constant $stock-media-next = "gtk-media-next";

define constant $stock-media-pause = "gtk-media-pause";

define constant $stock-media-play = "gtk-media-play";

define constant $stock-media-previous = "gtk-media-previous";

define constant $stock-media-record = "gtk-media-record";

define constant $stock-media-rewind = "gtk-media-rewind";

define constant $stock-media-stop = "gtk-media-stop";

define constant $stock-missing-image = "gtk-missing-image";

define constant $stock-network = "gtk-network";

define constant $stock-new = "gtk-new";

define constant $stock-no = "gtk-no";

define constant $stock-ok = "gtk-ok";

define constant $stock-open = "gtk-open";

define constant $stock-orientation-landscape = "gtk-orientation-landscape";

define constant $stock-orientation-portrait = "gtk-orientation-portrait";

define constant $stock-orientation-reverse-landscape = "gtk-orientation-reverse-landscape";

define constant $stock-orientation-reverse-portrait = "gtk-orientation-reverse-portrait";

define constant $stock-page-setup = "gtk-page-setup";

define constant $stock-paste = "gtk-paste";

define constant $stock-preferences = "gtk-preferences";

define constant $stock-print = "gtk-print";

define constant $stock-print-error = "gtk-print-error";

define constant $stock-print-paused = "gtk-print-paused";

define constant $stock-print-preview = "gtk-print-preview";

define constant $stock-print-report = "gtk-print-report";

define constant $stock-print-warning = "gtk-print-warning";

define constant $stock-properties = "gtk-properties";

define constant $stock-quit = "gtk-quit";

define constant $stock-redo = "gtk-redo";

define constant $stock-refresh = "gtk-refresh";

define constant $stock-remove = "gtk-remove";

define constant $stock-revert-to-saved = "gtk-revert-to-saved";

define constant $stock-save = "gtk-save";

define constant $stock-save-as = "gtk-save-as";

define constant $stock-select-all = "gtk-select-all";

define constant $stock-select-color = "gtk-select-color";

define constant $stock-select-font = "gtk-select-font";

define constant $stock-sort-ascending = "gtk-sort-ascending";

define constant $stock-sort-descending = "gtk-sort-descending";

define constant $stock-spell-check = "gtk-spell-check";

define constant $stock-stop = "gtk-stop";

define constant $stock-strikethrough = "gtk-strikethrough";

define constant $stock-undelete = "gtk-undelete";

define constant $stock-underline = "gtk-underline";

define constant $stock-undo = "gtk-undo";

define constant $stock-unindent = "gtk-unindent";

define constant $stock-yes = "gtk-yes";

define constant $stock-zoom-100 = "gtk-zoom-100";

define constant $stock-zoom-fit = "gtk-zoom-fit";

define constant $stock-zoom-in = "gtk-zoom-in";

define constant $stock-zoom-out = "gtk-zoom-out";

define constant $style-class-accelerator = "accelerator";

define constant $style-class-arrow = "arrow";

define constant $style-class-background = "background";

define constant $style-class-bottom = "bottom";

define constant $style-class-button = "button";

define constant $style-class-calendar = "calendar";

define constant $style-class-cell = "cell";

define constant $style-class-check = "check";

define constant $style-class-combobox-entry = "combobox-entry";

define constant $style-class-default = "default";

define constant $style-class-dnd = "dnd";

define constant $style-class-dock = "dock";

define constant $style-class-entry = "entry";

define constant $style-class-error = "error";

define constant $style-class-expander = "expander";

define constant $style-class-frame = "frame";

define constant $style-class-grip = "grip";

define constant $style-class-header = "header";

define constant $style-class-highlight = "highlight";

define constant $style-class-horizontal = "horizontal";

define constant $style-class-image = "image";

define constant $style-class-info = "info";

define constant $style-class-inline-toolbar = "inline-toolbar";

define constant $style-class-left = "left";

define constant $style-class-linked = "linked";

define constant $style-class-mark = "mark";

define constant $style-class-menu = "menu";

define constant $style-class-menubar = "menubar";

define constant $style-class-menuitem = "menuitem";

define constant $style-class-notebook = "notebook";

define constant $style-class-pane-separator = "pane-separator";

define constant $style-class-primary-toolbar = "primary-toolbar";

define constant $style-class-progressbar = "progressbar";

define constant $style-class-pulse = "pulse";

define constant $style-class-question = "question";

define constant $style-class-radio = "radio";

define constant $style-class-raised = "raised";

define constant $style-class-right = "right";

define constant $style-class-rubberband = "rubberband";

define constant $style-class-scale = "scale";

define constant $style-class-scale-has-marks-above = "scale-has-marks-above";

define constant $style-class-scale-has-marks-below = "scale-has-marks-below";

define constant $style-class-scrollbar = "scrollbar";

define constant $style-class-scrollbars-junction = "scrollbars-junction";

define constant $style-class-separator = "separator";

define constant $style-class-sidebar = "sidebar";

define constant $style-class-slider = "slider";

define constant $style-class-spinbutton = "spinbutton";

define constant $style-class-spinner = "spinner";

define constant $style-class-toolbar = "toolbar";

define constant $style-class-tooltip = "tooltip";

define constant $style-class-top = "top";

define constant $style-class-trough = "trough";

define constant $style-class-vertical = "vertical";

define constant $style-class-view = "view";

define constant $style-class-warning = "warning";

define constant $style-property-background-color = "background-color";

define constant $style-property-background-image = "background-image";

define constant $style-property-border-color = "border-color";

define constant $style-property-border-radius = "border-radius";

define constant $style-property-border-style = "border-style";

define constant $style-property-border-width = "border-width";

define constant $style-property-color = "color";

define constant $style-property-font = "font";

define constant $style-property-margin = "margin";

define constant $style-property-padding = "padding";

define constant $style-provider-priority-application = 600;

define constant $style-provider-priority-fallback = 1;

define constant $style-provider-priority-settings = 400;

define constant $style-provider-priority-theme = 200;

define constant $style-provider-priority-user = 800;

define constant $style-region-column = "column";

define constant $style-region-column-header = "column-header";

define constant $style-region-row = "row";

define constant $style-region-tab = "tab";

define open C-subtype <GtkScale> (<GtkRange>)
  constant slot gtk-scale-range :: <GtkRange>;
  constant slot gtk-scale-priv :: <GtkScalePrivate>;
end C-subtype;

define C-pointer-type <GtkScale*> => <GtkScale>;

define C-function gtk-scale-new
  input parameter orientation_ :: <GtkOrientation>;
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_new";
end;

define C-function gtk-scale-new-with-range
  input parameter orientation_ :: <GtkOrientation>;
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  input parameter step_ :: <C-double>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_new_with_range";
end;

define C-function gtk-scale-add-mark
  input parameter self :: <GtkScale>;
  input parameter value_ :: <C-double>;
  input parameter position_ :: <GtkPositionType>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_scale_add_mark";
end;

define C-function gtk-scale-clear-marks
  input parameter self :: <GtkScale>;
  c-name: "gtk_scale_clear_marks";
end;

define C-function gtk-scale-get-digits
  input parameter self :: <GtkScale>;
  result res :: <C-signed-int>;
  c-name: "gtk_scale_get_digits";
end;

define C-function gtk-scale-get-draw-value
  input parameter self :: <GtkScale>;
  result res :: <C-boolean>;
  c-name: "gtk_scale_get_draw_value";
end;

define C-function gtk-scale-get-has-origin
  input parameter self :: <GtkScale>;
  result res :: <C-boolean>;
  c-name: "gtk_scale_get_has_origin";
end;

define C-function gtk-scale-get-layout
  input parameter self :: <GtkScale>;
  result res :: <PangoLayout>;
  c-name: "gtk_scale_get_layout";
end;

define C-function gtk-scale-get-layout-offsets
  input parameter self :: <GtkScale>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gtk_scale_get_layout_offsets";
end;

define C-function gtk-scale-get-value-pos
  input parameter self :: <GtkScale>;
  result res :: <GtkPositionType>;
  c-name: "gtk_scale_get_value_pos";
end;

define C-function gtk-scale-set-digits
  input parameter self :: <GtkScale>;
  input parameter digits_ :: <C-signed-int>;
  c-name: "gtk_scale_set_digits";
end;

define C-function gtk-scale-set-draw-value
  input parameter self :: <GtkScale>;
  input parameter draw_value_ :: <C-boolean>;
  c-name: "gtk_scale_set_draw_value";
end;

define C-function gtk-scale-set-has-origin
  input parameter self :: <GtkScale>;
  input parameter has_origin_ :: <C-boolean>;
  c-name: "gtk_scale_set_has_origin";
end;

define C-function gtk-scale-set-value-pos
  input parameter self :: <GtkScale>;
  input parameter pos_ :: <GtkPositionType>;
  c-name: "gtk_scale_set_value_pos";
end;

define open C-subtype <GtkScaleButton> (<GtkButton>)
  constant slot gtk-scale-button-parent :: <GtkButton>;
  constant slot gtk-scale-button-priv :: <GtkScaleButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkScaleButton*> => <GtkScaleButton>;

define C-function gtk-scale-button-new
  input parameter size_ :: <C-signed-int>;
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  input parameter step_ :: <C-double>;
  input parameter icons_ :: <C-string*>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_button_new";
end;

define C-function gtk-scale-button-get-adjustment
  input parameter self :: <GtkScaleButton>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_scale_button_get_adjustment";
end;

define C-function gtk-scale-button-get-minus-button
  input parameter self :: <GtkScaleButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_button_get_minus_button";
end;

define C-function gtk-scale-button-get-plus-button
  input parameter self :: <GtkScaleButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_button_get_plus_button";
end;

define C-function gtk-scale-button-get-popup
  input parameter self :: <GtkScaleButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_scale_button_get_popup";
end;

define C-function gtk-scale-button-get-value
  input parameter self :: <GtkScaleButton>;
  result res :: <C-double>;
  c-name: "gtk_scale_button_get_value";
end;

define C-function gtk-scale-button-set-adjustment
  input parameter self :: <GtkScaleButton>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_scale_button_set_adjustment";
end;

define C-function gtk-scale-button-set-icons
  input parameter self :: <GtkScaleButton>;
  input parameter icons_ :: <C-string*>;
  c-name: "gtk_scale_button_set_icons";
end;

define C-function gtk-scale-button-set-value
  input parameter self :: <GtkScaleButton>;
  input parameter value_ :: <C-double>;
  c-name: "gtk_scale_button_set_value";
end;

define C-struct <_GtkScaleButtonClass>
  constant slot gtk-scale-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-scale-button-class-value-changed :: <C-function-pointer>;
  constant slot gtk-scale-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-scale-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-scale-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-scale-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkScaleButtonClass>;
end C-struct;

define C-struct <_GtkScaleButtonPrivate>
  pointer-type-name: <GtkScaleButtonPrivate>;
end C-struct;

define C-struct <_GtkScaleClass>
  constant slot gtk-scale-class-parent-class :: <GtkRangeClass>;
  constant slot gtk-scale-class-format-value :: <C-function-pointer>;
  constant slot gtk-scale-class-draw-value :: <C-function-pointer>;
  constant slot gtk-scale-class-get-layout-offsets :: <C-function-pointer>;
  constant slot gtk-scale-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-scale-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-scale-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-scale-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkScaleClass>;
end C-struct;

define C-struct <_GtkScalePrivate>
  pointer-type-name: <GtkScalePrivate>;
end C-struct;

define constant $gtk-scroll-steps = 0;
define constant $gtk-scroll-pages = 1;
define constant $gtk-scroll-ends = 2;
define constant $gtk-scroll-horizontal-steps = 3;
define constant $gtk-scroll-horizontal-pages = 4;
define constant $gtk-scroll-horizontal-ends = 5;
define constant <GtkScrollStep> = <C-int>;
define C-pointer-type <GtkScrollStep*> => <GtkScrollStep>;

define constant $gtk-scroll-none = 0;
define constant $gtk-scroll-jump = 1;
define constant $gtk-scroll-step-backward = 2;
define constant $gtk-scroll-step-forward = 3;
define constant $gtk-scroll-page-backward = 4;
define constant $gtk-scroll-page-forward = 5;
define constant $gtk-scroll-step-up = 6;
define constant $gtk-scroll-step-down = 7;
define constant $gtk-scroll-page-up = 8;
define constant $gtk-scroll-page-down = 9;
define constant $gtk-scroll-step-left = 10;
define constant $gtk-scroll-step-right = 11;
define constant $gtk-scroll-page-left = 12;
define constant $gtk-scroll-page-right = 13;
define constant $gtk-scroll-start = 14;
define constant $gtk-scroll-end = 15;
define constant <GtkScrollType> = <C-int>;
define C-pointer-type <GtkScrollType*> => <GtkScrollType>;

// Interface
define open C-subtype <GtkScrollable> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkScrollable*> => <GtkScrollable>;

define C-function gtk-scrollable-get-hadjustment
  input parameter self :: <GtkScrollable>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_scrollable_get_hadjustment";
end;

define C-function gtk-scrollable-get-hscroll-policy
  input parameter self :: <GtkScrollable>;
  result res :: <GtkScrollablePolicy>;
  c-name: "gtk_scrollable_get_hscroll_policy";
end;

define C-function gtk-scrollable-get-vadjustment
  input parameter self :: <GtkScrollable>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_scrollable_get_vadjustment";
end;

define C-function gtk-scrollable-get-vscroll-policy
  input parameter self :: <GtkScrollable>;
  result res :: <GtkScrollablePolicy>;
  c-name: "gtk_scrollable_get_vscroll_policy";
end;

define C-function gtk-scrollable-set-hadjustment
  input parameter self :: <GtkScrollable>;
  input parameter hadjustment_ :: <GtkAdjustment>;
  c-name: "gtk_scrollable_set_hadjustment";
end;

define C-function gtk-scrollable-set-hscroll-policy
  input parameter self :: <GtkScrollable>;
  input parameter policy_ :: <GtkScrollablePolicy>;
  c-name: "gtk_scrollable_set_hscroll_policy";
end;

define C-function gtk-scrollable-set-vadjustment
  input parameter self :: <GtkScrollable>;
  input parameter vadjustment_ :: <GtkAdjustment>;
  c-name: "gtk_scrollable_set_vadjustment";
end;

define C-function gtk-scrollable-set-vscroll-policy
  input parameter self :: <GtkScrollable>;
  input parameter policy_ :: <GtkScrollablePolicy>;
  c-name: "gtk_scrollable_set_vscroll_policy";
end;

define C-struct <_GtkScrollableInterface>
  constant slot gtk-scrollable-interface-base-iface :: <GTypeInterface>;
  pointer-type-name: <GtkScrollableInterface>;
end C-struct;

define constant $gtk-scroll-minimum = 0;
define constant $gtk-scroll-natural = 1;
define constant <GtkScrollablePolicy> = <C-int>;
define C-pointer-type <GtkScrollablePolicy*> => <GtkScrollablePolicy>;

define open C-subtype <GtkScrollbar> (<GtkRange>)
  constant slot gtk-scrollbar-range :: <GtkRange>;
end C-subtype;

define C-pointer-type <GtkScrollbar*> => <GtkScrollbar>;

define C-function gtk-scrollbar-new
  input parameter orientation_ :: <GtkOrientation>;
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_scrollbar_new";
end;

define C-struct <_GtkScrollbarClass>
  constant slot gtk-scrollbar-class-parent-class :: <GtkRangeClass>;
  constant slot gtk-scrollbar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-scrollbar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-scrollbar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-scrollbar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkScrollbarClass>;
end C-struct;

define open C-subtype <GtkScrolledWindow> (<GtkBin>)
  constant slot gtk-scrolled-window-container :: <GtkBin>;
  constant slot gtk-scrolled-window-priv :: <GtkScrolledWindowPrivate>;
end C-subtype;

define C-pointer-type <GtkScrolledWindow*> => <GtkScrolledWindow>;

define C-function gtk-scrolled-window-new
  input parameter hadjustment_ :: <GtkAdjustment>;
  input parameter vadjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_scrolled_window_new";
end;

define C-function gtk-scrolled-window-add-with-viewport
  input parameter self :: <GtkScrolledWindow>;
  input parameter child_ :: <GtkWidget>;
  c-name: "gtk_scrolled_window_add_with_viewport";
end;

define C-function gtk-scrolled-window-get-capture-button-press
  input parameter self :: <GtkScrolledWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_scrolled_window_get_capture_button_press";
end;

define C-function gtk-scrolled-window-get-hadjustment
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_scrolled_window_get_hadjustment";
end;

define C-function gtk-scrolled-window-get-hscrollbar
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_scrolled_window_get_hscrollbar";
end;

define C-function gtk-scrolled-window-get-kinetic-scrolling
  input parameter self :: <GtkScrolledWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_scrolled_window_get_kinetic_scrolling";
end;

define C-function gtk-scrolled-window-get-min-content-height
  input parameter self :: <GtkScrolledWindow>;
  result res :: <C-signed-int>;
  c-name: "gtk_scrolled_window_get_min_content_height";
end;

define C-function gtk-scrolled-window-get-min-content-width
  input parameter self :: <GtkScrolledWindow>;
  result res :: <C-signed-int>;
  c-name: "gtk_scrolled_window_get_min_content_width";
end;

define C-function gtk-scrolled-window-get-placement
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkCornerType>;
  c-name: "gtk_scrolled_window_get_placement";
end;

define C-function gtk-scrolled-window-get-policy
  input parameter self :: <GtkScrolledWindow>;
  output parameter hscrollbar_policy_ :: <GtkPolicyType*>;
  output parameter vscrollbar_policy_ :: <GtkPolicyType*>;
  c-name: "gtk_scrolled_window_get_policy";
end;

define C-function gtk-scrolled-window-get-shadow-type
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkShadowType>;
  c-name: "gtk_scrolled_window_get_shadow_type";
end;

define C-function gtk-scrolled-window-get-vadjustment
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_scrolled_window_get_vadjustment";
end;

define C-function gtk-scrolled-window-get-vscrollbar
  input parameter self :: <GtkScrolledWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_scrolled_window_get_vscrollbar";
end;

define C-function gtk-scrolled-window-set-capture-button-press
  input parameter self :: <GtkScrolledWindow>;
  input parameter capture_button_press_ :: <C-boolean>;
  c-name: "gtk_scrolled_window_set_capture_button_press";
end;

define C-function gtk-scrolled-window-set-hadjustment
  input parameter self :: <GtkScrolledWindow>;
  input parameter hadjustment_ :: <GtkAdjustment>;
  c-name: "gtk_scrolled_window_set_hadjustment";
end;

define C-function gtk-scrolled-window-set-kinetic-scrolling
  input parameter self :: <GtkScrolledWindow>;
  input parameter kinetic_scrolling_ :: <C-boolean>;
  c-name: "gtk_scrolled_window_set_kinetic_scrolling";
end;

define C-function gtk-scrolled-window-set-min-content-height
  input parameter self :: <GtkScrolledWindow>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_scrolled_window_set_min_content_height";
end;

define C-function gtk-scrolled-window-set-min-content-width
  input parameter self :: <GtkScrolledWindow>;
  input parameter width_ :: <C-signed-int>;
  c-name: "gtk_scrolled_window_set_min_content_width";
end;

define C-function gtk-scrolled-window-set-placement
  input parameter self :: <GtkScrolledWindow>;
  input parameter window_placement_ :: <GtkCornerType>;
  c-name: "gtk_scrolled_window_set_placement";
end;

define C-function gtk-scrolled-window-set-policy
  input parameter self :: <GtkScrolledWindow>;
  input parameter hscrollbar_policy_ :: <GtkPolicyType>;
  input parameter vscrollbar_policy_ :: <GtkPolicyType>;
  c-name: "gtk_scrolled_window_set_policy";
end;

define C-function gtk-scrolled-window-set-shadow-type
  input parameter self :: <GtkScrolledWindow>;
  input parameter type_ :: <GtkShadowType>;
  c-name: "gtk_scrolled_window_set_shadow_type";
end;

define C-function gtk-scrolled-window-set-vadjustment
  input parameter self :: <GtkScrolledWindow>;
  input parameter vadjustment_ :: <GtkAdjustment>;
  c-name: "gtk_scrolled_window_set_vadjustment";
end;

define C-function gtk-scrolled-window-unset-placement
  input parameter self :: <GtkScrolledWindow>;
  c-name: "gtk_scrolled_window_unset_placement";
end;

define C-struct <_GtkScrolledWindowClass>
  constant slot gtk-scrolled-window-class-parent-class :: <GtkBinClass>;
  constant slot gtk-scrolled-window-class-scrollbar-spacing :: <C-signed-int>;
  constant slot gtk-scrolled-window-class-scroll-child :: <C-function-pointer>;
  constant slot gtk-scrolled-window-class-move-focus-out :: <C-function-pointer>;
  constant slot gtk-scrolled-window-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-scrolled-window-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-scrolled-window-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-scrolled-window-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkScrolledWindowClass>;
end C-struct;

define C-struct <_GtkScrolledWindowPrivate>
  pointer-type-name: <GtkScrolledWindowPrivate>;
end C-struct;

define C-struct <_GtkSelectionData>
  pointer-type-name: <GtkSelectionData>;
end C-struct;

define C-function gtk-selection-data-copy
  input parameter self :: <GtkSelectionData>;
  result res :: <GtkSelectionData>;
  c-name: "gtk_selection_data_copy";
end;

define C-function gtk-selection-data-free
  input parameter self :: <GtkSelectionData>;
  c-name: "gtk_selection_data_free";
end;

define C-function gtk-selection-data-get-data-type
  input parameter self :: <GtkSelectionData>;
  result res :: <GdkAtom>;
  c-name: "gtk_selection_data_get_data_type";
end;

define C-function gtk-selection-data-get-data-with-length
  input parameter self :: <GtkSelectionData>;
  output parameter length_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*>;
  c-name: "gtk_selection_data_get_data_with_length";
end;

define C-function gtk-selection-data-get-display
  input parameter self :: <GtkSelectionData>;
  result res :: <GdkDisplay>;
  c-name: "gtk_selection_data_get_display";
end;

define C-function gtk-selection-data-get-format
  input parameter self :: <GtkSelectionData>;
  result res :: <C-signed-int>;
  c-name: "gtk_selection_data_get_format";
end;

define C-function gtk-selection-data-get-length
  input parameter self :: <GtkSelectionData>;
  result res :: <C-signed-int>;
  c-name: "gtk_selection_data_get_length";
end;

define C-function gtk-selection-data-get-pixbuf
  input parameter self :: <GtkSelectionData>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_selection_data_get_pixbuf";
end;

define C-function gtk-selection-data-get-selection
  input parameter self :: <GtkSelectionData>;
  result res :: <GdkAtom>;
  c-name: "gtk_selection_data_get_selection";
end;

define C-function gtk-selection-data-get-target
  input parameter self :: <GtkSelectionData>;
  result res :: <GdkAtom>;
  c-name: "gtk_selection_data_get_target";
end;

define C-function gtk-selection-data-get-targets
  input parameter self :: <GtkSelectionData>;
  output parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_atoms_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_get_targets";
end;

define C-function gtk-selection-data-get-text
  input parameter self :: <GtkSelectionData>;
  result res :: <C-string>;
  c-name: "gtk_selection_data_get_text";
end;

define C-function gtk-selection-data-get-uris
  input parameter self :: <GtkSelectionData>;
  result res :: <C-string*>;
  c-name: "gtk_selection_data_get_uris";
end;

define C-function gtk-selection-data-set
  input parameter self :: <GtkSelectionData>;
  input parameter type_ :: <GdkAtom>;
  input parameter format_ :: <C-signed-int>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-int>;
  c-name: "gtk_selection_data_set";
end;

define C-function gtk-selection-data-set-pixbuf
  input parameter self :: <GtkSelectionData>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_set_pixbuf";
end;

define C-function gtk-selection-data-set-text
  input parameter self :: <GtkSelectionData>;
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_set_text";
end;

define C-function gtk-selection-data-set-uris
  input parameter self :: <GtkSelectionData>;
  input parameter uris_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_set_uris";
end;

define C-function gtk-selection-data-targets-include-image
  input parameter self :: <GtkSelectionData>;
  input parameter writable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_targets_include_image";
end;

define C-function gtk-selection-data-targets-include-rich-text
  input parameter self :: <GtkSelectionData>;
  input parameter buffer_ :: <GtkTextBuffer>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_targets_include_rich_text";
end;

define C-function gtk-selection-data-targets-include-text
  input parameter self :: <GtkSelectionData>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_targets_include_text";
end;

define C-function gtk-selection-data-targets-include-uri
  input parameter self :: <GtkSelectionData>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_data_targets_include_uri";
end;

define constant $gtk-selection-none = 0;
define constant $gtk-selection-single = 1;
define constant $gtk-selection-browse = 2;
define constant $gtk-selection-multiple = 3;
define constant <GtkSelectionMode> = <C-int>;
define C-pointer-type <GtkSelectionMode*> => <GtkSelectionMode>;

define constant $gtk-sensitivity-auto = 0;
define constant $gtk-sensitivity-on = 1;
define constant $gtk-sensitivity-off = 2;
define constant <GtkSensitivityType> = <C-int>;
define C-pointer-type <GtkSensitivityType*> => <GtkSensitivityType>;

define open C-subtype <GtkSeparator> (<GtkWidget>)
  constant slot gtk-separator-widget :: <GtkWidget>;
  constant slot gtk-separator-priv :: <GtkSeparatorPrivate>;
end C-subtype;

define C-pointer-type <GtkSeparator*> => <GtkSeparator>;

define C-function gtk-separator-new
  input parameter orientation_ :: <GtkOrientation>;
  result res :: <GtkWidget>;
  c-name: "gtk_separator_new";
end;

define C-struct <_GtkSeparatorClass>
  constant slot gtk-separator-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-separator-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-separator-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-separator-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-separator-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSeparatorClass>;
end C-struct;

define open C-subtype <GtkSeparatorMenuItem> (<GtkMenuItem>)
  constant slot gtk-separator-menu-item-menu-item :: <GtkMenuItem>;
end C-subtype;

define C-pointer-type <GtkSeparatorMenuItem*> => <GtkSeparatorMenuItem>;

define C-function gtk-separator-menu-item-new
  result res :: <GtkWidget>;
  c-name: "gtk_separator_menu_item_new";
end;

define C-struct <_GtkSeparatorMenuItemClass>
  constant slot gtk-separator-menu-item-class-parent-class :: <GtkMenuItemClass>;
  constant slot gtk-separator-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-separator-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-separator-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-separator-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSeparatorMenuItemClass>;
end C-struct;

define C-struct <_GtkSeparatorPrivate>
  pointer-type-name: <GtkSeparatorPrivate>;
end C-struct;

define open C-subtype <GtkSeparatorToolItem> (<GtkToolItem>)
  constant slot gtk-separator-tool-item-parent :: <GtkToolItem>;
  constant slot gtk-separator-tool-item-priv :: <GtkSeparatorToolItemPrivate>;
end C-subtype;

define C-pointer-type <GtkSeparatorToolItem*> => <GtkSeparatorToolItem>;

define C-function gtk-separator-tool-item-new
  result res :: <GtkToolItem>;
  c-name: "gtk_separator_tool_item_new";
end;

define C-function gtk-separator-tool-item-get-draw
  input parameter self :: <GtkSeparatorToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_separator_tool_item_get_draw";
end;

define C-function gtk-separator-tool-item-set-draw
  input parameter self :: <GtkSeparatorToolItem>;
  input parameter draw_ :: <C-boolean>;
  c-name: "gtk_separator_tool_item_set_draw";
end;

define C-struct <_GtkSeparatorToolItemClass>
  constant slot gtk-separator-tool-item-class-parent-class :: <GtkToolItemClass>;
  constant slot gtk-separator-tool-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-separator-tool-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-separator-tool-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-separator-tool-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSeparatorToolItemClass>;
end C-struct;

define C-struct <_GtkSeparatorToolItemPrivate>
  pointer-type-name: <GtkSeparatorToolItemPrivate>;
end C-struct;

define open C-subtype <GtkSettings> (<GObject>)
  constant slot gtk-settings-parent-instance :: <GObject>;
  constant slot gtk-settings-priv :: <GtkSettingsPrivate>;
end C-subtype;

define C-pointer-type <GtkSettings*> => <GtkSettings>;

define C-function gtk-settings-get-default
  result res :: <GtkSettings>;
  c-name: "gtk_settings_get_default";
end;

define C-function gtk-settings-get-for-screen
  input parameter screen_ :: <GdkScreen>;
  result res :: <GtkSettings>;
  c-name: "gtk_settings_get_for_screen";
end;

define C-function gtk-settings-install-property
  input parameter pspec_ :: <GParamSpec>;
  c-name: "gtk_settings_install_property";
end;

define C-function gtk-settings-install-property-parser
  input parameter pspec_ :: <GParamSpec>;
  input parameter parser_ :: <C-function-pointer>;
  c-name: "gtk_settings_install_property_parser";
end;

define C-function gtk-settings-set-double-property
  input parameter self :: <GtkSettings>;
  input parameter name_ :: <C-string>;
  input parameter v_double_ :: <C-double>;
  input parameter origin_ :: <C-string>;
  c-name: "gtk_settings_set_double_property";
end;

define C-function gtk-settings-set-long-property
  input parameter self :: <GtkSettings>;
  input parameter name_ :: <C-string>;
  input parameter v_long_ :: <C-signed-long>;
  input parameter origin_ :: <C-string>;
  c-name: "gtk_settings_set_long_property";
end;

define C-function gtk-settings-set-property-value
  input parameter self :: <GtkSettings>;
  input parameter name_ :: <C-string>;
  input parameter svalue_ :: <GtkSettingsValue>;
  c-name: "gtk_settings_set_property_value";
end;

define C-function gtk-settings-set-string-property
  input parameter self :: <GtkSettings>;
  input parameter name_ :: <C-string>;
  input parameter v_string_ :: <C-string>;
  input parameter origin_ :: <C-string>;
  c-name: "gtk_settings_set_string_property";
end;

define C-struct <_GtkSettingsClass>
  constant slot gtk-settings-class-parent-class :: <GObjectClass>;
  constant slot gtk-settings-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-settings-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-settings-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-settings-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSettingsClass>;
end C-struct;

define C-struct <_GtkSettingsPrivate>
  pointer-type-name: <GtkSettingsPrivate>;
end C-struct;

define C-struct <_GtkSettingsValue>
  slot gtk-settings-value-origin :: <C-string>;
  slot gtk-settings-value-value :: <GValue>;
  pointer-type-name: <GtkSettingsValue>;
end C-struct;

define constant $gtk-shadow-none = 0;
define constant $gtk-shadow-in = 1;
define constant $gtk-shadow-out = 2;
define constant $gtk-shadow-etched-in = 3;
define constant $gtk-shadow-etched-out = 4;
define constant <GtkShadowType> = <C-int>;
define C-pointer-type <GtkShadowType*> => <GtkShadowType>;

define open C-subtype <GtkSizeGroup> (<GObject>)
  constant slot gtk-size-group-parent-instance :: <GObject>;
  constant slot gtk-size-group-priv :: <GtkSizeGroupPrivate>;
end C-subtype;

define C-pointer-type <GtkSizeGroup*> => <GtkSizeGroup>;

define C-function gtk-size-group-new
  input parameter mode_ :: <GtkSizeGroupMode>;
  result res :: <GtkSizeGroup>;
  c-name: "gtk_size_group_new";
end;

define C-function gtk-size-group-add-widget
  input parameter self :: <GtkSizeGroup>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_size_group_add_widget";
end;

define C-function gtk-size-group-get-ignore-hidden
  input parameter self :: <GtkSizeGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_size_group_get_ignore_hidden";
end;

define C-function gtk-size-group-get-mode
  input parameter self :: <GtkSizeGroup>;
  result res :: <GtkSizeGroupMode>;
  c-name: "gtk_size_group_get_mode";
end;

define C-function gtk-size-group-get-widgets
  input parameter self :: <GtkSizeGroup>;
  result res :: <GSList>;
  c-name: "gtk_size_group_get_widgets";
end;

define C-function gtk-size-group-remove-widget
  input parameter self :: <GtkSizeGroup>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_size_group_remove_widget";
end;

define C-function gtk-size-group-set-ignore-hidden
  input parameter self :: <GtkSizeGroup>;
  input parameter ignore_hidden_ :: <C-boolean>;
  c-name: "gtk_size_group_set_ignore_hidden";
end;

define C-function gtk-size-group-set-mode
  input parameter self :: <GtkSizeGroup>;
  input parameter mode_ :: <GtkSizeGroupMode>;
  c-name: "gtk_size_group_set_mode";
end;

define C-struct <_GtkSizeGroupClass>
  constant slot gtk-size-group-class-parent-class :: <GObjectClass>;
  constant slot gtk-size-group-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-size-group-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-size-group-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-size-group-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSizeGroupClass>;
end C-struct;

define constant $gtk-size-group-none = 0;
define constant $gtk-size-group-horizontal = 1;
define constant $gtk-size-group-vertical = 2;
define constant $gtk-size-group-both = 3;
define constant <GtkSizeGroupMode> = <C-int>;
define C-pointer-type <GtkSizeGroupMode*> => <GtkSizeGroupMode>;

define C-struct <_GtkSizeGroupPrivate>
  pointer-type-name: <GtkSizeGroupPrivate>;
end C-struct;

define constant $gtk-size-request-height-for-width = 0;
define constant $gtk-size-request-width-for-height = 1;
define constant $gtk-size-request-constant-size = 2;
define constant <GtkSizeRequestMode> = <C-int>;
define C-pointer-type <GtkSizeRequestMode*> => <GtkSizeRequestMode>;

define open C-subtype <GtkSocket> (<GtkContainer>)
  constant slot gtk-socket-container :: <GtkContainer>;
  constant slot gtk-socket-priv :: <GtkSocketPrivate>;
end C-subtype;

define C-pointer-type <GtkSocket*> => <GtkSocket>;

define C-function gtk-socket-new
  result res :: <GtkWidget>;
  c-name: "gtk_socket_new";
end;

define C-function gtk-socket-add-id
  input parameter self :: <GtkSocket>;
  input parameter window_ :: <C-unsigned-long>;
  c-name: "gtk_socket_add_id";
end;

define C-function gtk-socket-get-id
  input parameter self :: <GtkSocket>;
  result res :: <C-unsigned-long>;
  c-name: "gtk_socket_get_id";
end;

define C-function gtk-socket-get-plug-window
  input parameter self :: <GtkSocket>;
  result res :: <GdkWindow>;
  c-name: "gtk_socket_get_plug_window";
end;

define C-struct <_GtkSocketClass>
  constant slot gtk-socket-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-socket-class-plug-added :: <C-function-pointer>;
  constant slot gtk-socket-class-plug-removed :: <C-function-pointer>;
  constant slot gtk-socket-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-socket-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-socket-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-socket-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSocketClass>;
end C-struct;

define C-struct <_GtkSocketPrivate>
  pointer-type-name: <GtkSocketPrivate>;
end C-struct;

define constant $gtk-sort-ascending = 0;
define constant $gtk-sort-descending = 1;
define constant <GtkSortType> = <C-int>;
define C-pointer-type <GtkSortType*> => <GtkSortType>;

define open C-subtype <GtkSpinButton> (<GtkEntry>)
  constant slot gtk-spin-button-entry :: <GtkEntry>;
  constant slot gtk-spin-button-priv :: <GtkSpinButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkSpinButton*> => <GtkSpinButton>;

define C-function gtk-spin-button-new
  input parameter adjustment_ :: <GtkAdjustment>;
  input parameter climb_rate_ :: <C-double>;
  input parameter digits_ :: <C-unsigned-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_spin_button_new";
end;

define C-function gtk-spin-button-new-with-range
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  input parameter step_ :: <C-double>;
  result res :: <GtkWidget>;
  c-name: "gtk_spin_button_new_with_range";
end;

define C-function gtk-spin-button-configure
  input parameter self :: <GtkSpinButton>;
  input parameter adjustment_ :: <GtkAdjustment>;
  input parameter climb_rate_ :: <C-double>;
  input parameter digits_ :: <C-unsigned-int>;
  c-name: "gtk_spin_button_configure";
end;

define C-function gtk-spin-button-get-adjustment
  input parameter self :: <GtkSpinButton>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_spin_button_get_adjustment";
end;

define C-function gtk-spin-button-get-digits
  input parameter self :: <GtkSpinButton>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_spin_button_get_digits";
end;

define C-function gtk-spin-button-get-increments
  input parameter self :: <GtkSpinButton>;
  output parameter step_ :: <C-double*>;
  output parameter page_ :: <C-double*>;
  c-name: "gtk_spin_button_get_increments";
end;

define C-function gtk-spin-button-get-numeric
  input parameter self :: <GtkSpinButton>;
  result res :: <C-boolean>;
  c-name: "gtk_spin_button_get_numeric";
end;

define C-function gtk-spin-button-get-range
  input parameter self :: <GtkSpinButton>;
  output parameter min_ :: <C-double*>;
  output parameter max_ :: <C-double*>;
  c-name: "gtk_spin_button_get_range";
end;

define C-function gtk-spin-button-get-snap-to-ticks
  input parameter self :: <GtkSpinButton>;
  result res :: <C-boolean>;
  c-name: "gtk_spin_button_get_snap_to_ticks";
end;

define C-function gtk-spin-button-get-update-policy
  input parameter self :: <GtkSpinButton>;
  result res :: <GtkSpinButtonUpdatePolicy>;
  c-name: "gtk_spin_button_get_update_policy";
end;

define C-function gtk-spin-button-get-value
  input parameter self :: <GtkSpinButton>;
  result res :: <C-double>;
  c-name: "gtk_spin_button_get_value";
end;

define C-function gtk-spin-button-get-value-as-int
  input parameter self :: <GtkSpinButton>;
  result res :: <C-signed-int>;
  c-name: "gtk_spin_button_get_value_as_int";
end;

define C-function gtk-spin-button-get-wrap
  input parameter self :: <GtkSpinButton>;
  result res :: <C-boolean>;
  c-name: "gtk_spin_button_get_wrap";
end;

define C-function gtk-spin-button-set-adjustment
  input parameter self :: <GtkSpinButton>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_spin_button_set_adjustment";
end;

define C-function gtk-spin-button-set-digits
  input parameter self :: <GtkSpinButton>;
  input parameter digits_ :: <C-unsigned-int>;
  c-name: "gtk_spin_button_set_digits";
end;

define C-function gtk-spin-button-set-increments
  input parameter self :: <GtkSpinButton>;
  input parameter step_ :: <C-double>;
  input parameter page_ :: <C-double>;
  c-name: "gtk_spin_button_set_increments";
end;

define C-function gtk-spin-button-set-numeric
  input parameter self :: <GtkSpinButton>;
  input parameter numeric_ :: <C-boolean>;
  c-name: "gtk_spin_button_set_numeric";
end;

define C-function gtk-spin-button-set-range
  input parameter self :: <GtkSpinButton>;
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  c-name: "gtk_spin_button_set_range";
end;

define C-function gtk-spin-button-set-snap-to-ticks
  input parameter self :: <GtkSpinButton>;
  input parameter snap_to_ticks_ :: <C-boolean>;
  c-name: "gtk_spin_button_set_snap_to_ticks";
end;

define C-function gtk-spin-button-set-update-policy
  input parameter self :: <GtkSpinButton>;
  input parameter policy_ :: <GtkSpinButtonUpdatePolicy>;
  c-name: "gtk_spin_button_set_update_policy";
end;

define C-function gtk-spin-button-set-value
  input parameter self :: <GtkSpinButton>;
  input parameter value_ :: <C-double>;
  c-name: "gtk_spin_button_set_value";
end;

define C-function gtk-spin-button-set-wrap
  input parameter self :: <GtkSpinButton>;
  input parameter wrap_ :: <C-boolean>;
  c-name: "gtk_spin_button_set_wrap";
end;

define C-function gtk-spin-button-spin
  input parameter self :: <GtkSpinButton>;
  input parameter direction_ :: <GtkSpinType>;
  input parameter increment_ :: <C-double>;
  c-name: "gtk_spin_button_spin";
end;

define C-function gtk-spin-button-update
  input parameter self :: <GtkSpinButton>;
  c-name: "gtk_spin_button_update";
end;

define C-struct <_GtkSpinButtonClass>
  constant slot gtk-spin-button-class-parent-class :: <GtkEntryClass>;
  constant slot gtk-spin-button-class-input :: <C-function-pointer>;
  constant slot gtk-spin-button-class-output :: <C-function-pointer>;
  constant slot gtk-spin-button-class-value-changed :: <C-function-pointer>;
  constant slot gtk-spin-button-class-change-value :: <C-function-pointer>;
  constant slot gtk-spin-button-class-wrapped :: <C-function-pointer>;
  constant slot gtk-spin-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-spin-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-spin-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-spin-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSpinButtonClass>;
end C-struct;

define C-struct <_GtkSpinButtonPrivate>
  pointer-type-name: <GtkSpinButtonPrivate>;
end C-struct;

define constant $gtk-update-always = 0;
define constant $gtk-update-if-valid = 1;
define constant <GtkSpinButtonUpdatePolicy> = <C-int>;
define C-pointer-type <GtkSpinButtonUpdatePolicy*> => <GtkSpinButtonUpdatePolicy>;

define constant $gtk-spin-step-forward = 0;
define constant $gtk-spin-step-backward = 1;
define constant $gtk-spin-page-forward = 2;
define constant $gtk-spin-page-backward = 3;
define constant $gtk-spin-home = 4;
define constant $gtk-spin-end = 5;
define constant $gtk-spin-user-defined = 6;
define constant <GtkSpinType> = <C-int>;
define C-pointer-type <GtkSpinType*> => <GtkSpinType>;

define open C-subtype <GtkSpinner> (<GtkWidget>)
  constant slot gtk-spinner-parent :: <GtkWidget>;
  constant slot gtk-spinner-priv :: <GtkSpinnerPrivate>;
end C-subtype;

define C-pointer-type <GtkSpinner*> => <GtkSpinner>;

define C-function gtk-spinner-new
  result res :: <GtkWidget>;
  c-name: "gtk_spinner_new";
end;

define C-function gtk-spinner-start
  input parameter self :: <GtkSpinner>;
  c-name: "gtk_spinner_start";
end;

define C-function gtk-spinner-stop
  input parameter self :: <GtkSpinner>;
  c-name: "gtk_spinner_stop";
end;

define C-struct <_GtkSpinnerClass>
  constant slot gtk-spinner-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-spinner-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-spinner-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-spinner-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-spinner-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkSpinnerClass>;
end C-struct;

define C-struct <_GtkSpinnerPrivate>
  pointer-type-name: <GtkSpinnerPrivate>;
end C-struct;

define constant $gtk-state-flag-normal = 0;
define constant $gtk-state-flag-active = 1;
define constant $gtk-state-flag-prelight = 2;
define constant $gtk-state-flag-selected = 4;
define constant $gtk-state-flag-insensitive = 8;
define constant $gtk-state-flag-inconsistent = 16;
define constant $gtk-state-flag-focused = 32;
define constant $gtk-state-flag-backdrop = 64;
define constant <GtkStateFlags> = <C-int>;
define C-pointer-type <GtkStateFlags*> => <GtkStateFlags>;

define constant $gtk-state-normal = 0;
define constant $gtk-state-active = 1;
define constant $gtk-state-prelight = 2;
define constant $gtk-state-selected = 3;
define constant $gtk-state-insensitive = 4;
define constant $gtk-state-inconsistent = 5;
define constant $gtk-state-focused = 6;
define constant <GtkStateType> = <C-int>;
define C-pointer-type <GtkStateType*> => <GtkStateType>;

define open C-subtype <GtkStatusIcon> (<GObject>)
  constant slot gtk-status-icon-parent-instance :: <GObject>;
  constant slot gtk-status-icon-priv :: <GtkStatusIconPrivate>;
end C-subtype;

define C-pointer-type <GtkStatusIcon*> => <GtkStatusIcon>;

define C-function gtk-status-icon-new
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new";
end;

define C-function gtk-status-icon-new-from-file
  input parameter filename_ :: <C-string>;
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new_from_file";
end;

define C-function gtk-status-icon-new-from-gicon
  input parameter icon_ :: <GIcon>;
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new_from_gicon";
end;

define C-function gtk-status-icon-new-from-icon-name
  input parameter icon_name_ :: <C-string>;
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new_from_icon_name";
end;

define C-function gtk-status-icon-new-from-pixbuf
  input parameter pixbuf_ :: <GdkPixbuf>;
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new_from_pixbuf";
end;

define C-function gtk-status-icon-new-from-stock
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_new_from_stock";
end;

define C-function gtk-status-icon-position-menu
  input parameter menu_ :: <GtkMenu>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  output parameter push_in_ :: <C-int*>;
  input parameter user_data_ :: <GtkStatusIcon>;
  c-name: "gtk_status_icon_position_menu";
end;

define C-function gtk-status-icon-get-geometry
  input parameter self :: <GtkStatusIcon>;
  output parameter screen_ :: <GdkScreen*>;
  output parameter area_ :: <cairoRectangleInt>;
  output parameter orientation_ :: <GtkOrientation*>;
  result res :: <C-boolean>;
  c-name: "gtk_status_icon_get_geometry";
end;

define C-function gtk-status-icon-get-gicon
  input parameter self :: <GtkStatusIcon>;
  result res :: <GIcon>;
  c-name: "gtk_status_icon_get_gicon";
end;

define C-function gtk-status-icon-get-has-tooltip
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-boolean>;
  c-name: "gtk_status_icon_get_has_tooltip";
end;

define C-function gtk-status-icon-get-icon-name
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-string>;
  c-name: "gtk_status_icon_get_icon_name";
end;

define C-function gtk-status-icon-get-pixbuf
  input parameter self :: <GtkStatusIcon>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_status_icon_get_pixbuf";
end;

define C-function gtk-status-icon-get-screen
  input parameter self :: <GtkStatusIcon>;
  result res :: <GdkScreen>;
  c-name: "gtk_status_icon_get_screen";
end;

define C-function gtk-status-icon-get-size
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-signed-int>;
  c-name: "gtk_status_icon_get_size";
end;

define C-function gtk-status-icon-get-stock
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-string>;
  c-name: "gtk_status_icon_get_stock";
end;

define C-function gtk-status-icon-get-storage-type
  input parameter self :: <GtkStatusIcon>;
  result res :: <GtkImageType>;
  c-name: "gtk_status_icon_get_storage_type";
end;

define C-function gtk-status-icon-get-title
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-string>;
  c-name: "gtk_status_icon_get_title";
end;

define C-function gtk-status-icon-get-tooltip-markup
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-string>;
  c-name: "gtk_status_icon_get_tooltip_markup";
end;

define C-function gtk-status-icon-get-tooltip-text
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-string>;
  c-name: "gtk_status_icon_get_tooltip_text";
end;

define C-function gtk-status-icon-get-visible
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-boolean>;
  c-name: "gtk_status_icon_get_visible";
end;

define C-function gtk-status-icon-get-x11-window-id
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_status_icon_get_x11_window_id";
end;

define C-function gtk-status-icon-is-embedded
  input parameter self :: <GtkStatusIcon>;
  result res :: <C-boolean>;
  c-name: "gtk_status_icon_is_embedded";
end;

define C-function gtk-status-icon-set-from-file
  input parameter self :: <GtkStatusIcon>;
  input parameter filename_ :: <C-string>;
  c-name: "gtk_status_icon_set_from_file";
end;

define C-function gtk-status-icon-set-from-gicon
  input parameter self :: <GtkStatusIcon>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_status_icon_set_from_gicon";
end;

define C-function gtk-status-icon-set-from-icon-name
  input parameter self :: <GtkStatusIcon>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_status_icon_set_from_icon_name";
end;

define C-function gtk-status-icon-set-from-pixbuf
  input parameter self :: <GtkStatusIcon>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_status_icon_set_from_pixbuf";
end;

define C-function gtk-status-icon-set-from-stock
  input parameter self :: <GtkStatusIcon>;
  input parameter stock_id_ :: <C-string>;
  c-name: "gtk_status_icon_set_from_stock";
end;

define C-function gtk-status-icon-set-has-tooltip
  input parameter self :: <GtkStatusIcon>;
  input parameter has_tooltip_ :: <C-boolean>;
  c-name: "gtk_status_icon_set_has_tooltip";
end;

define C-function gtk-status-icon-set-name
  input parameter self :: <GtkStatusIcon>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_status_icon_set_name";
end;

define C-function gtk-status-icon-set-screen
  input parameter self :: <GtkStatusIcon>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_status_icon_set_screen";
end;

define C-function gtk-status-icon-set-title
  input parameter self :: <GtkStatusIcon>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_status_icon_set_title";
end;

define C-function gtk-status-icon-set-tooltip-markup
  input parameter self :: <GtkStatusIcon>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_status_icon_set_tooltip_markup";
end;

define C-function gtk-status-icon-set-tooltip-text
  input parameter self :: <GtkStatusIcon>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_status_icon_set_tooltip_text";
end;

define C-function gtk-status-icon-set-visible
  input parameter self :: <GtkStatusIcon>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_status_icon_set_visible";
end;

define C-struct <_GtkStatusIconClass>
  constant slot gtk-status-icon-class-parent-class :: <GObjectClass>;
  constant slot gtk-status-icon-class-activate :: <C-function-pointer>;
  constant slot gtk-status-icon-class-popup-menu :: <C-function-pointer>;
  constant slot gtk-status-icon-class-size-changed :: <C-function-pointer>;
  constant slot gtk-status-icon-class-button-press-event :: <C-function-pointer>;
  constant slot gtk-status-icon-class-button-release-event :: <C-function-pointer>;
  constant slot gtk-status-icon-class-scroll-event :: <C-function-pointer>;
  constant slot gtk-status-icon-class-query-tooltip :: <C-function-pointer>;
  constant slot gtk-status-icon-class-__gtk-reserved1 :: <C-void*>;
  constant slot gtk-status-icon-class-__gtk-reserved2 :: <C-void*>;
  constant slot gtk-status-icon-class-__gtk-reserved3 :: <C-void*>;
  constant slot gtk-status-icon-class-__gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkStatusIconClass>;
end C-struct;

define C-struct <_GtkStatusIconPrivate>
  pointer-type-name: <GtkStatusIconPrivate>;
end C-struct;

define open C-subtype <GtkStatusbar> (<GtkBox>)
  constant slot gtk-statusbar-parent-widget :: <GtkBox>;
  constant slot gtk-statusbar-priv :: <GtkStatusbarPrivate>;
end C-subtype;

define C-pointer-type <GtkStatusbar*> => <GtkStatusbar>;

define C-function gtk-statusbar-new
  result res :: <GtkWidget>;
  c-name: "gtk_statusbar_new";
end;

define C-function gtk-statusbar-get-context-id
  input parameter self :: <GtkStatusbar>;
  input parameter context_description_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_statusbar_get_context_id";
end;

define C-function gtk-statusbar-get-message-area
  input parameter self :: <GtkStatusbar>;
  result res :: <GtkWidget>;
  c-name: "gtk_statusbar_get_message_area";
end;

define C-function gtk-statusbar-pop
  input parameter self :: <GtkStatusbar>;
  input parameter context_id_ :: <C-unsigned-int>;
  c-name: "gtk_statusbar_pop";
end;

define C-function gtk-statusbar-push
  input parameter self :: <GtkStatusbar>;
  input parameter context_id_ :: <C-unsigned-int>;
  input parameter text_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_statusbar_push";
end;

define C-function gtk-statusbar-remove
  input parameter self :: <GtkStatusbar>;
  input parameter context_id_ :: <C-unsigned-int>;
  input parameter message_id_ :: <C-unsigned-int>;
  c-name: "gtk_statusbar_remove";
end;

define C-function gtk-statusbar-remove-all
  input parameter self :: <GtkStatusbar>;
  input parameter context_id_ :: <C-unsigned-int>;
  c-name: "gtk_statusbar_remove_all";
end;

define C-struct <_GtkStatusbarClass>
  constant slot gtk-statusbar-class-parent-class :: <GtkBoxClass>;
  constant slot gtk-statusbar-class-reserved :: <C-void*>;
  constant slot gtk-statusbar-class-text-pushed :: <C-function-pointer>;
  constant slot gtk-statusbar-class-text-popped :: <C-function-pointer>;
  constant slot gtk-statusbar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-statusbar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-statusbar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-statusbar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkStatusbarClass>;
end C-struct;

define C-struct <_GtkStatusbarPrivate>
  pointer-type-name: <GtkStatusbarPrivate>;
end C-struct;

define C-struct <_GtkStockItem>
  slot gtk-stock-item-stock-id :: <C-string>;
  slot gtk-stock-item-label :: <C-string>;
  slot gtk-stock-item-modifier :: <GdkModifierType>;
  slot gtk-stock-item-keyval :: <C-unsigned-int>;
  slot gtk-stock-item-translation-domain :: <C-string>;
  pointer-type-name: <GtkStockItem>;
end C-struct;

define C-function gtk-stock-item-free
  input parameter self :: <GtkStockItem>;
  c-name: "gtk_stock_item_free";
end;

define open C-subtype <GtkStyle> (<GObject>)
  constant slot gtk-style-parent-instance :: <GObject>;
  constant slot gtk-style-fg :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-bg :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-light :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-dark :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-mid :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-text :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-base :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-text-aa :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-black :: <GdkColor>;
  constant slot gtk-style-white :: <GdkColor>;
  constant slot gtk-style-font-desc :: <PangoFontDescription>;
  constant slot gtk-style-xthickness :: <C-signed-int>;
  constant slot gtk-style-ythickness :: <C-signed-int>;
  constant slot gtk-style-background :: <C-unsigned-char*> /* Not supported */;
  constant slot gtk-style-attach-count :: <C-signed-int>;
  constant slot gtk-style-visual :: <GdkVisual>;
  constant slot gtk-style-private-font-desc :: <PangoFontDescription>;
  constant slot gtk-style-rc-style :: <GtkRcStyle>;
  constant slot gtk-style-styles :: <GSList>;
  constant slot gtk-style-property-cache :: <GArray>;
  constant slot gtk-style-icon-factories :: <GSList>;
end C-subtype;

define C-pointer-type <GtkStyle*> => <GtkStyle>;

define C-function gtk-style-new
  result res :: <GtkStyle>;
  c-name: "gtk_style_new";
end;

define C-function gtk-style-apply-default-background
  input parameter self :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter window_ :: <GdkWindow>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_style_apply_default_background";
end;

define C-function gtk-style-copy
  input parameter self :: <GtkStyle>;
  result res :: <GtkStyle>;
  c-name: "gtk_style_copy";
end;

define C-function gtk-style-detach
  input parameter self :: <GtkStyle>;
  c-name: "gtk_style_detach";
end;

define C-function gtk-style-get-style-property
  input parameter self :: <GtkStyle>;
  input parameter widget_type_ :: <C-long>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_style_get_style_property";
end;

define C-function gtk-style-has-context
  input parameter self :: <GtkStyle>;
  result res :: <C-boolean>;
  c-name: "gtk_style_has_context";
end;

define C-function gtk-style-lookup-color
  input parameter self :: <GtkStyle>;
  input parameter color_name_ :: <C-string>;
  output parameter color_ :: <GdkColor>;
  result res :: <C-boolean>;
  c-name: "gtk_style_lookup_color";
end;

define C-function gtk-style-lookup-icon-set
  input parameter self :: <GtkStyle>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkIconSet>;
  c-name: "gtk_style_lookup_icon_set";
end;

define C-function gtk-style-render-icon
  input parameter self :: <GtkStyle>;
  input parameter source_ :: <GtkIconSource>;
  input parameter direction_ :: <GtkTextDirection>;
  input parameter state_ :: <GtkStateType>;
  input parameter size_ :: <C-signed-int>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_style_render_icon";
end;

define C-function gtk-style-set-background
  input parameter self :: <GtkStyle>;
  input parameter window_ :: <GdkWindow>;
  input parameter state_type_ :: <GtkStateType>;
  c-name: "gtk_style_set_background";
end;

define C-struct <_GtkStyleClass>
  constant slot gtk-style-class-parent-class :: <GObjectClass>;
  constant slot gtk-style-class-realize :: <C-function-pointer>;
  constant slot gtk-style-class-unrealize :: <C-function-pointer>;
  constant slot gtk-style-class-copy :: <C-function-pointer>;
  constant slot gtk-style-class-clone :: <C-void*>;
  constant slot gtk-style-class-init-from-rc :: <C-function-pointer>;
  constant slot gtk-style-class-set-background :: <C-function-pointer>;
  constant slot gtk-style-class-render-icon :: <C-function-pointer>;
  constant slot gtk-style-class-draw-hline :: <C-function-pointer>;
  constant slot gtk-style-class-draw-vline :: <C-function-pointer>;
  constant slot gtk-style-class-draw-shadow :: <C-function-pointer>;
  constant slot gtk-style-class-draw-arrow :: <C-function-pointer>;
  constant slot gtk-style-class-draw-diamond :: <C-function-pointer>;
  constant slot gtk-style-class-draw-box :: <C-function-pointer>;
  constant slot gtk-style-class-draw-flat-box :: <C-function-pointer>;
  constant slot gtk-style-class-draw-check :: <C-function-pointer>;
  constant slot gtk-style-class-draw-option :: <C-function-pointer>;
  constant slot gtk-style-class-draw-tab :: <C-function-pointer>;
  constant slot gtk-style-class-draw-shadow-gap :: <C-function-pointer>;
  constant slot gtk-style-class-draw-box-gap :: <C-function-pointer>;
  constant slot gtk-style-class-draw-extension :: <C-function-pointer>;
  constant slot gtk-style-class-draw-focus :: <C-function-pointer>;
  constant slot gtk-style-class-draw-slider :: <C-function-pointer>;
  constant slot gtk-style-class-draw-handle :: <C-function-pointer>;
  constant slot gtk-style-class-draw-expander :: <C-function-pointer>;
  constant slot gtk-style-class-draw-layout :: <C-function-pointer>;
  constant slot gtk-style-class-draw-resize-grip :: <C-function-pointer>;
  constant slot gtk-style-class-draw-spinner :: <C-function-pointer>;
  constant slot gtk-style-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved8 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved9 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved10 :: <C-void*>;
  constant slot gtk-style-class-_gtk-reserved11 :: <C-void*>;
  pointer-type-name: <GtkStyleClass>;
end C-struct;

define open C-subtype <GtkStyleContext> (<GObject>)
  constant slot gtk-style-context-parent-object :: <GObject>;
  constant slot gtk-style-context-priv :: <GtkStyleContextPrivate>;
end C-subtype;

define C-pointer-type <GtkStyleContext*> => <GtkStyleContext>;

define C-function gtk-style-context-new
  result res :: <GtkStyleContext>;
  c-name: "gtk_style_context_new";
end;

define C-function gtk-style-context-add-provider-for-screen
  input parameter screen_ :: <GdkScreen>;
  input parameter provider_ :: <GtkStyleProvider>;
  input parameter priority_ :: <C-unsigned-int>;
  c-name: "gtk_style_context_add_provider_for_screen";
end;

define C-function gtk-style-context-remove-provider-for-screen
  input parameter screen_ :: <GdkScreen>;
  input parameter provider_ :: <GtkStyleProvider>;
  c-name: "gtk_style_context_remove_provider_for_screen";
end;

define C-function gtk-style-context-reset-widgets
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_style_context_reset_widgets";
end;

define C-function gtk-style-context-add-class
  input parameter self :: <GtkStyleContext>;
  input parameter class_name_ :: <C-string>;
  c-name: "gtk_style_context_add_class";
end;

define C-function gtk-style-context-add-provider
  input parameter self :: <GtkStyleContext>;
  input parameter provider_ :: <GtkStyleProvider>;
  input parameter priority_ :: <C-unsigned-int>;
  c-name: "gtk_style_context_add_provider";
end;

define C-function gtk-style-context-add-region
  input parameter self :: <GtkStyleContext>;
  input parameter region_name_ :: <C-string>;
  input parameter flags_ :: <GtkRegionFlags>;
  c-name: "gtk_style_context_add_region";
end;

define C-function gtk-style-context-cancel-animations
  input parameter self :: <GtkStyleContext>;
  input parameter region_id_ :: <C-void*>;
  c-name: "gtk_style_context_cancel_animations";
end;

define C-function gtk-style-context-get-background-color
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_style_context_get_background_color";
end;

define C-function gtk-style-context-get-border
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter border_ :: <GtkBorder>;
  c-name: "gtk_style_context_get_border";
end;

define C-function gtk-style-context-get-border-color
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_style_context_get_border_color";
end;

define C-function gtk-style-context-get-color
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_style_context_get_color";
end;

define C-function gtk-style-context-get-direction
  input parameter self :: <GtkStyleContext>;
  result res :: <GtkTextDirection>;
  c-name: "gtk_style_context_get_direction";
end;

define C-function gtk-style-context-get-font
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  result res :: <PangoFontDescription>;
  c-name: "gtk_style_context_get_font";
end;

define C-function gtk-style-context-get-junction-sides
  input parameter self :: <GtkStyleContext>;
  result res :: <GtkJunctionSides>;
  c-name: "gtk_style_context_get_junction_sides";
end;

define C-function gtk-style-context-get-margin
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter margin_ :: <GtkBorder>;
  c-name: "gtk_style_context_get_margin";
end;

define C-function gtk-style-context-get-padding
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter padding_ :: <GtkBorder>;
  c-name: "gtk_style_context_get_padding";
end;

define C-function gtk-style-context-get-parent
  input parameter self :: <GtkStyleContext>;
  result res :: <GtkStyleContext>;
  c-name: "gtk_style_context_get_parent";
end;

define C-function gtk-style-context-get-path
  input parameter self :: <GtkStyleContext>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_style_context_get_path";
end;

define C-function gtk-style-context-get-property
  input parameter self :: <GtkStyleContext>;
  input parameter property_ :: <C-string>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter value_ :: <GValue>;
  c-name: "gtk_style_context_get_property";
end;

define C-function gtk-style-context-get-screen
  input parameter self :: <GtkStyleContext>;
  result res :: <GdkScreen>;
  c-name: "gtk_style_context_get_screen";
end;

define C-function gtk-style-context-get-section
  input parameter self :: <GtkStyleContext>;
  input parameter property_ :: <C-string>;
  result res :: <GtkCssSection>;
  c-name: "gtk_style_context_get_section";
end;

define C-function gtk-style-context-get-state
  input parameter self :: <GtkStyleContext>;
  result res :: <GtkStateFlags>;
  c-name: "gtk_style_context_get_state";
end;

define C-function gtk-style-context-get-style-property
  input parameter self :: <GtkStyleContext>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_style_context_get_style_property";
end;

define C-function gtk-style-context-has-class
  input parameter self :: <GtkStyleContext>;
  input parameter class_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_style_context_has_class";
end;

define C-function gtk-style-context-has-region
  input parameter self :: <GtkStyleContext>;
  input parameter region_name_ :: <C-string>;
  output parameter flags_return_ :: <GtkRegionFlags*>;
  result res :: <C-boolean>;
  c-name: "gtk_style_context_has_region";
end;

define C-function gtk-style-context-invalidate
  input parameter self :: <GtkStyleContext>;
  c-name: "gtk_style_context_invalidate";
end;

define C-function gtk-style-context-list-classes
  input parameter self :: <GtkStyleContext>;
  result res :: <GList>;
  c-name: "gtk_style_context_list_classes";
end;

define C-function gtk-style-context-list-regions
  input parameter self :: <GtkStyleContext>;
  result res :: <GList>;
  c-name: "gtk_style_context_list_regions";
end;

define C-function gtk-style-context-lookup-color
  input parameter self :: <GtkStyleContext>;
  input parameter color_name_ :: <C-string>;
  output parameter color_ :: <GdkRGBA>;
  result res :: <C-boolean>;
  c-name: "gtk_style_context_lookup_color";
end;

define C-function gtk-style-context-lookup-icon-set
  input parameter self :: <GtkStyleContext>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkIconSet>;
  c-name: "gtk_style_context_lookup_icon_set";
end;

define C-function gtk-style-context-notify-state-change
  input parameter self :: <GtkStyleContext>;
  input parameter window_ :: <GdkWindow>;
  input parameter region_id_ :: <C-void*>;
  input parameter state_ :: <GtkStateType>;
  input parameter state_value_ :: <C-boolean>;
  c-name: "gtk_style_context_notify_state_change";
end;

define C-function gtk-style-context-pop-animatable-region
  input parameter self :: <GtkStyleContext>;
  c-name: "gtk_style_context_pop_animatable_region";
end;

define C-function gtk-style-context-push-animatable-region
  input parameter self :: <GtkStyleContext>;
  input parameter region_id_ :: <C-void*>;
  c-name: "gtk_style_context_push_animatable_region";
end;

define C-function gtk-style-context-remove-class
  input parameter self :: <GtkStyleContext>;
  input parameter class_name_ :: <C-string>;
  c-name: "gtk_style_context_remove_class";
end;

define C-function gtk-style-context-remove-provider
  input parameter self :: <GtkStyleContext>;
  input parameter provider_ :: <GtkStyleProvider>;
  c-name: "gtk_style_context_remove_provider";
end;

define C-function gtk-style-context-remove-region
  input parameter self :: <GtkStyleContext>;
  input parameter region_name_ :: <C-string>;
  c-name: "gtk_style_context_remove_region";
end;

define C-function gtk-style-context-restore
  input parameter self :: <GtkStyleContext>;
  c-name: "gtk_style_context_restore";
end;

define C-function gtk-style-context-save
  input parameter self :: <GtkStyleContext>;
  c-name: "gtk_style_context_save";
end;

define C-function gtk-style-context-scroll-animations
  input parameter self :: <GtkStyleContext>;
  input parameter window_ :: <GdkWindow>;
  input parameter dx_ :: <C-signed-int>;
  input parameter dy_ :: <C-signed-int>;
  c-name: "gtk_style_context_scroll_animations";
end;

define C-function gtk-style-context-set-background
  input parameter self :: <GtkStyleContext>;
  input parameter window_ :: <GdkWindow>;
  c-name: "gtk_style_context_set_background";
end;

define C-function gtk-style-context-set-direction
  input parameter self :: <GtkStyleContext>;
  input parameter direction_ :: <GtkTextDirection>;
  c-name: "gtk_style_context_set_direction";
end;

define C-function gtk-style-context-set-junction-sides
  input parameter self :: <GtkStyleContext>;
  input parameter sides_ :: <GtkJunctionSides>;
  c-name: "gtk_style_context_set_junction_sides";
end;

define C-function gtk-style-context-set-parent
  input parameter self :: <GtkStyleContext>;
  input parameter parent_ :: <GtkStyleContext>;
  c-name: "gtk_style_context_set_parent";
end;

define C-function gtk-style-context-set-path
  input parameter self :: <GtkStyleContext>;
  input parameter path_ :: <GtkWidgetPath>;
  c-name: "gtk_style_context_set_path";
end;

define C-function gtk-style-context-set-screen
  input parameter self :: <GtkStyleContext>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_style_context_set_screen";
end;

define C-function gtk-style-context-set-state
  input parameter self :: <GtkStyleContext>;
  input parameter flags_ :: <GtkStateFlags>;
  c-name: "gtk_style_context_set_state";
end;

define C-function gtk-style-context-state-is-running
  input parameter self :: <GtkStyleContext>;
  input parameter state_ :: <GtkStateType>;
  output parameter progress_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gtk_style_context_state_is_running";
end;

define C-struct <_GtkStyleContextClass>
  constant slot gtk-style-context-class-parent-class :: <GObjectClass>;
  constant slot gtk-style-context-class-changed :: <C-function-pointer>;
  constant slot gtk-style-context-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-style-context-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-style-context-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-style-context-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkStyleContextClass>;
end C-struct;

define C-struct <_GtkStyleContextPrivate>
  pointer-type-name: <GtkStyleContextPrivate>;
end C-struct;

define open C-subtype <GtkStyleProperties> (<GObject>)
  constant slot gtk-style-properties-parent-object :: <GObject>;
  constant slot gtk-style-properties-priv :: <GtkStylePropertiesPrivate>;
end C-subtype;

define C-pointer-type <GtkStyleProperties*> => <GtkStyleProperties>;

define C-function gtk-style-properties-new
  result res :: <GtkStyleProperties>;
  c-name: "gtk_style_properties_new";
end;

define C-function gtk-style-properties-clear
  input parameter self :: <GtkStyleProperties>;
  c-name: "gtk_style_properties_clear";
end;

define C-function gtk-style-properties-get-property
  input parameter self :: <GtkStyleProperties>;
  input parameter property_ :: <C-string>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_style_properties_get_property";
end;

define C-function gtk-style-properties-lookup-color
  input parameter self :: <GtkStyleProperties>;
  input parameter name_ :: <C-string>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_style_properties_lookup_color";
end;

define C-function gtk-style-properties-map-color
  input parameter self :: <GtkStyleProperties>;
  input parameter name_ :: <C-string>;
  input parameter color_ :: <GtkSymbolicColor>;
  c-name: "gtk_style_properties_map_color";
end;

define C-function gtk-style-properties-merge
  input parameter self :: <GtkStyleProperties>;
  input parameter props_to_merge_ :: <GtkStyleProperties>;
  input parameter replace_ :: <C-boolean>;
  c-name: "gtk_style_properties_merge";
end;

define C-function gtk-style-properties-set-property
  input parameter self :: <GtkStyleProperties>;
  input parameter property_ :: <C-string>;
  input parameter state_ :: <GtkStateFlags>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_style_properties_set_property";
end;

define C-function gtk-style-properties-unset-property
  input parameter self :: <GtkStyleProperties>;
  input parameter property_ :: <C-string>;
  input parameter state_ :: <GtkStateFlags>;
  c-name: "gtk_style_properties_unset_property";
end;

define C-struct <_GtkStylePropertiesClass>
  constant slot gtk-style-properties-class-parent-class :: <GObjectClass>;
  constant slot gtk-style-properties-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-style-properties-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-style-properties-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-style-properties-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkStylePropertiesClass>;
end C-struct;

define C-struct <_GtkStylePropertiesPrivate>
  pointer-type-name: <GtkStylePropertiesPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkStyleProvider> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkStyleProvider*> => <GtkStyleProvider>;

define C-function gtk-style-provider-get-icon-factory
  input parameter self :: <GtkStyleProvider>;
  input parameter path_ :: <GtkWidgetPath>;
  result res :: <GtkIconFactory>;
  c-name: "gtk_style_provider_get_icon_factory";
end;

define C-function gtk-style-provider-get-style
  input parameter self :: <GtkStyleProvider>;
  input parameter path_ :: <GtkWidgetPath>;
  result res :: <GtkStyleProperties>;
  c-name: "gtk_style_provider_get_style";
end;

define C-function gtk-style-provider-get-style-property
  input parameter self :: <GtkStyleProvider>;
  input parameter path_ :: <GtkWidgetPath>;
  input parameter state_ :: <GtkStateFlags>;
  input parameter pspec_ :: <GParamSpec>;
  output parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_style_provider_get_style_property";
end;

define C-struct <_GtkStyleProviderIface>
  constant slot gtk-style-provider-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-style-provider-iface-get-style :: <C-function-pointer>;
  constant slot gtk-style-provider-iface-get-style-property :: <C-function-pointer>;
  constant slot gtk-style-provider-iface-get-icon-factory :: <C-function-pointer>;
  pointer-type-name: <GtkStyleProviderIface>;
end C-struct;

define open C-subtype <GtkSwitch> (<GtkWidget>)
  constant slot gtk-switch-parent-instance :: <GtkWidget>;
  constant slot gtk-switch-priv :: <GtkSwitchPrivate>;
end C-subtype;

define C-pointer-type <GtkSwitch*> => <GtkSwitch>;

define C-function gtk-switch-new
  result res :: <GtkWidget>;
  c-name: "gtk_switch_new";
end;

define C-function gtk-switch-get-active
  input parameter self :: <GtkSwitch>;
  result res :: <C-boolean>;
  c-name: "gtk_switch_get_active";
end;

define C-function gtk-switch-set-active
  input parameter self :: <GtkSwitch>;
  input parameter is_active_ :: <C-boolean>;
  c-name: "gtk_switch_set_active";
end;

define C-struct <_GtkSwitchClass>
  constant slot gtk-switch-class-parent-class :: <GtkWidgetClass>;
  constant slot gtk-switch-class-activate :: <C-function-pointer>;
  constant slot gtk-switch-class-_switch-padding-1 :: <C-void*>;
  constant slot gtk-switch-class-_switch-padding-2 :: <C-void*>;
  constant slot gtk-switch-class-_switch-padding-3 :: <C-void*>;
  constant slot gtk-switch-class-_switch-padding-4 :: <C-void*>;
  constant slot gtk-switch-class-_switch-padding-5 :: <C-void*>;
  constant slot gtk-switch-class-_switch-padding-6 :: <C-void*>;
  pointer-type-name: <GtkSwitchClass>;
end C-struct;

define C-struct <_GtkSwitchPrivate>
  pointer-type-name: <GtkSwitchPrivate>;
end C-struct;

define C-struct <_GtkSymbolicColor>
  pointer-type-name: <GtkSymbolicColor>;
end C-struct;

define C-function gtk-symbolic-color-new-alpha
  input parameter color_ :: <GtkSymbolicColor>;
  input parameter factor_ :: <C-double>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_alpha";
end;

define C-function gtk-symbolic-color-new-literal
  input parameter color_ :: <GdkRGBA>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_literal";
end;

define C-function gtk-symbolic-color-new-mix
  input parameter color1_ :: <GtkSymbolicColor>;
  input parameter color2_ :: <GtkSymbolicColor>;
  input parameter factor_ :: <C-double>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_mix";
end;

define C-function gtk-symbolic-color-new-name
  input parameter name_ :: <C-string>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_name";
end;

define C-function gtk-symbolic-color-new-shade
  input parameter color_ :: <GtkSymbolicColor>;
  input parameter factor_ :: <C-double>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_shade";
end;

define C-function gtk-symbolic-color-new-win32
  input parameter theme_class_ :: <C-string>;
  input parameter id_ :: <C-signed-int>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_new_win32";
end;

define C-function gtk-symbolic-color-ref
  input parameter self :: <GtkSymbolicColor>;
  result res :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_ref";
end;

define C-function gtk-symbolic-color-resolve
  input parameter self :: <GtkSymbolicColor>;
  input parameter props_ :: <GtkStyleProperties>;
  output parameter resolved_color_ :: <GdkRGBA>;
  result res :: <C-boolean>;
  c-name: "gtk_symbolic_color_resolve";
end;

define C-function gtk-symbolic-color-to-string
  input parameter self :: <GtkSymbolicColor>;
  result res :: <C-string>;
  c-name: "gtk_symbolic_color_to_string";
end;

define C-function gtk-symbolic-color-unref
  input parameter self :: <GtkSymbolicColor>;
  c-name: "gtk_symbolic_color_unref";
end;

define constant $text-view-priority-validate = 5;

define open C-subtype <GtkTable> (<GtkContainer>)
  constant slot gtk-table-container :: <GtkContainer>;
  constant slot gtk-table-priv :: <GtkTablePrivate>;
end C-subtype;

define C-pointer-type <GtkTable*> => <GtkTable>;

define C-function gtk-table-new
  input parameter rows_ :: <C-unsigned-int>;
  input parameter columns_ :: <C-unsigned-int>;
  input parameter homogeneous_ :: <C-boolean>;
  result res :: <GtkWidget>;
  c-name: "gtk_table_new";
end;

define C-function gtk-table-attach
  input parameter self :: <GtkTable>;
  input parameter child_ :: <GtkWidget>;
  input parameter left_attach_ :: <C-unsigned-int>;
  input parameter right_attach_ :: <C-unsigned-int>;
  input parameter top_attach_ :: <C-unsigned-int>;
  input parameter bottom_attach_ :: <C-unsigned-int>;
  input parameter xoptions_ :: <GtkAttachOptions>;
  input parameter yoptions_ :: <GtkAttachOptions>;
  input parameter xpadding_ :: <C-unsigned-int>;
  input parameter ypadding_ :: <C-unsigned-int>;
  c-name: "gtk_table_attach";
end;

define C-function gtk-table-attach-defaults
  input parameter self :: <GtkTable>;
  input parameter widget_ :: <GtkWidget>;
  input parameter left_attach_ :: <C-unsigned-int>;
  input parameter right_attach_ :: <C-unsigned-int>;
  input parameter top_attach_ :: <C-unsigned-int>;
  input parameter bottom_attach_ :: <C-unsigned-int>;
  c-name: "gtk_table_attach_defaults";
end;

define C-function gtk-table-get-col-spacing
  input parameter self :: <GtkTable>;
  input parameter column_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_table_get_col_spacing";
end;

define C-function gtk-table-get-default-col-spacing
  input parameter self :: <GtkTable>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_table_get_default_col_spacing";
end;

define C-function gtk-table-get-default-row-spacing
  input parameter self :: <GtkTable>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_table_get_default_row_spacing";
end;

define C-function gtk-table-get-homogeneous
  input parameter self :: <GtkTable>;
  result res :: <C-boolean>;
  c-name: "gtk_table_get_homogeneous";
end;

define C-function gtk-table-get-row-spacing
  input parameter self :: <GtkTable>;
  input parameter row_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_table_get_row_spacing";
end;

define C-function gtk-table-get-size
  input parameter self :: <GtkTable>;
  output parameter rows_ :: <C-unsigned-int*>;
  output parameter columns_ :: <C-unsigned-int*>;
  c-name: "gtk_table_get_size";
end;

define C-function gtk-table-resize
  input parameter self :: <GtkTable>;
  input parameter rows_ :: <C-unsigned-int>;
  input parameter columns_ :: <C-unsigned-int>;
  c-name: "gtk_table_resize";
end;

define C-function gtk-table-set-col-spacing
  input parameter self :: <GtkTable>;
  input parameter column_ :: <C-unsigned-int>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_table_set_col_spacing";
end;

define C-function gtk-table-set-col-spacings
  input parameter self :: <GtkTable>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_table_set_col_spacings";
end;

define C-function gtk-table-set-homogeneous
  input parameter self :: <GtkTable>;
  input parameter homogeneous_ :: <C-boolean>;
  c-name: "gtk_table_set_homogeneous";
end;

define C-function gtk-table-set-row-spacing
  input parameter self :: <GtkTable>;
  input parameter row_ :: <C-unsigned-int>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_table_set_row_spacing";
end;

define C-function gtk-table-set-row-spacings
  input parameter self :: <GtkTable>;
  input parameter spacing_ :: <C-unsigned-int>;
  c-name: "gtk_table_set_row_spacings";
end;

define C-struct <_GtkTableChild>
  slot gtk-table-child-widget :: <GtkWidget>;
  slot gtk-table-child-left-attach :: <C-unsigned-short>;
  slot gtk-table-child-right-attach :: <C-unsigned-short>;
  slot gtk-table-child-top-attach :: <C-unsigned-short>;
  slot gtk-table-child-bottom-attach :: <C-unsigned-short>;
  slot gtk-table-child-xpadding :: <C-unsigned-short>;
  slot gtk-table-child-ypadding :: <C-unsigned-short>;
  slot gtk-table-child-xexpand :: <C-unsigned-int>;
  slot gtk-table-child-yexpand :: <C-unsigned-int>;
  slot gtk-table-child-xshrink :: <C-unsigned-int>;
  slot gtk-table-child-yshrink :: <C-unsigned-int>;
  slot gtk-table-child-xfill :: <C-unsigned-int>;
  slot gtk-table-child-yfill :: <C-unsigned-int>;
  pointer-type-name: <GtkTableChild>;
end C-struct;

define C-struct <_GtkTableClass>
  constant slot gtk-table-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-table-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-table-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-table-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-table-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTableClass>;
end C-struct;

define C-struct <_GtkTablePrivate>
  pointer-type-name: <GtkTablePrivate>;
end C-struct;

define C-struct <_GtkTableRowCol>
  slot gtk-table-row-col-requisition :: <C-unsigned-short>;
  slot gtk-table-row-col-allocation :: <C-unsigned-short>;
  slot gtk-table-row-col-spacing :: <C-unsigned-short>;
  slot gtk-table-row-col-need-expand :: <C-unsigned-int>;
  slot gtk-table-row-col-need-shrink :: <C-unsigned-int>;
  slot gtk-table-row-col-expand :: <C-unsigned-int>;
  slot gtk-table-row-col-shrink :: <C-unsigned-int>;
  slot gtk-table-row-col-empty :: <C-unsigned-int>;
  pointer-type-name: <GtkTableRowCol>;
end C-struct;

define C-struct <_GtkTargetEntry>
  slot gtk-target-entry-target :: <C-string>;
  slot gtk-target-entry-flags :: <C-unsigned-int>;
  slot gtk-target-entry-info :: <C-unsigned-int>;
  pointer-type-name: <GtkTargetEntry>;
end C-struct;

define C-function gtk-target-entry-new
  input parameter target_ :: <C-string>;
  input parameter flags_ :: <C-unsigned-int>;
  input parameter info_ :: <C-unsigned-int>;
  result res :: <GtkTargetEntry>;
  c-name: "gtk_target_entry_new";
end;

define C-function gtk-target-entry-copy
  input parameter self :: <GtkTargetEntry>;
  result res :: <GtkTargetEntry>;
  c-name: "gtk_target_entry_copy";
end;

define C-function gtk-target-entry-free
  input parameter self :: <GtkTargetEntry>;
  c-name: "gtk_target_entry_free";
end;

define constant $gtk-target-same-app = 1;
define constant $gtk-target-same-widget = 2;
define constant $gtk-target-other-app = 4;
define constant $gtk-target-other-widget = 8;
define constant <GtkTargetFlags> = <C-int>;
define C-pointer-type <GtkTargetFlags*> => <GtkTargetFlags>;

define C-struct <_GtkTargetList>
  pointer-type-name: <GtkTargetList>;
end C-struct;

define C-function gtk-target-list-new
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter ntargets_ :: <C-unsigned-int>;
  result res :: <GtkTargetList>;
  c-name: "gtk_target_list_new";
end;

define C-function gtk-target-list-add
  input parameter self :: <GtkTargetList>;
  input parameter target_ :: <GdkAtom>;
  input parameter flags_ :: <C-unsigned-int>;
  input parameter info_ :: <C-unsigned-int>;
  c-name: "gtk_target_list_add";
end;

define C-function gtk-target-list-add-image-targets
  input parameter self :: <GtkTargetList>;
  input parameter info_ :: <C-unsigned-int>;
  input parameter writable_ :: <C-boolean>;
  c-name: "gtk_target_list_add_image_targets";
end;

define C-function gtk-target-list-add-rich-text-targets
  input parameter self :: <GtkTargetList>;
  input parameter info_ :: <C-unsigned-int>;
  input parameter deserializable_ :: <C-boolean>;
  input parameter buffer_ :: <GtkTextBuffer>;
  c-name: "gtk_target_list_add_rich_text_targets";
end;

define C-function gtk-target-list-add-table
  input parameter self :: <GtkTargetList>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter ntargets_ :: <C-unsigned-int>;
  c-name: "gtk_target_list_add_table";
end;

define C-function gtk-target-list-add-text-targets
  input parameter self :: <GtkTargetList>;
  input parameter info_ :: <C-unsigned-int>;
  c-name: "gtk_target_list_add_text_targets";
end;

define C-function gtk-target-list-add-uri-targets
  input parameter self :: <GtkTargetList>;
  input parameter info_ :: <C-unsigned-int>;
  c-name: "gtk_target_list_add_uri_targets";
end;

define C-function gtk-target-list-find
  input parameter self :: <GtkTargetList>;
  input parameter target_ :: <GdkAtom>;
  input parameter info_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_target_list_find";
end;

define C-function gtk-target-list-ref
  input parameter self :: <GtkTargetList>;
  result res :: <GtkTargetList>;
  c-name: "gtk_target_list_ref";
end;

define C-function gtk-target-list-remove
  input parameter self :: <GtkTargetList>;
  input parameter target_ :: <GdkAtom>;
  c-name: "gtk_target_list_remove";
end;

define C-function gtk-target-list-unref
  input parameter self :: <GtkTargetList>;
  c-name: "gtk_target_list_unref";
end;

define open C-subtype <GtkTearoffMenuItem> (<GtkMenuItem>)
  constant slot gtk-tearoff-menu-item-menu-item :: <GtkMenuItem>;
  constant slot gtk-tearoff-menu-item-priv :: <GtkTearoffMenuItemPrivate>;
end C-subtype;

define C-pointer-type <GtkTearoffMenuItem*> => <GtkTearoffMenuItem>;

define C-function gtk-tearoff-menu-item-new
  result res :: <GtkWidget>;
  c-name: "gtk_tearoff_menu_item_new";
end;

define C-struct <_GtkTearoffMenuItemClass>
  constant slot gtk-tearoff-menu-item-class-parent-class :: <GtkMenuItemClass>;
  constant slot gtk-tearoff-menu-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tearoff-menu-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tearoff-menu-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tearoff-menu-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTearoffMenuItemClass>;
end C-struct;

define C-struct <_GtkTearoffMenuItemPrivate>
  pointer-type-name: <GtkTearoffMenuItemPrivate>;
end C-struct;

define C-struct <_GtkTextAppearance>
  slot gtk-text-appearance-bg-color :: <GdkColor>;
  slot gtk-text-appearance-fg-color :: <GdkColor>;
  slot gtk-text-appearance-rise :: <C-signed-int>;
  slot gtk-text-appearance-underline :: <C-unsigned-int>;
  slot gtk-text-appearance-strikethrough :: <C-unsigned-int>;
  slot gtk-text-appearance-draw-bg :: <C-unsigned-int>;
  slot gtk-text-appearance-inside-selection :: <C-unsigned-int>;
  slot gtk-text-appearance-is-text :: <C-unsigned-int>;
  slot gtk-text-appearance-rgba :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GtkTextAppearance>;
end C-struct;

define C-struct <_GtkTextAttributes>
  constant slot gtk-text-attributes-refcount :: <C-unsigned-int>;
  slot gtk-text-attributes-appearance :: <GtkTextAppearance>;
  slot gtk-text-attributes-justification :: <GtkJustification>;
  slot gtk-text-attributes-direction :: <GtkTextDirection>;
  slot gtk-text-attributes-font :: <PangoFontDescription>;
  slot gtk-text-attributes-font-scale :: <C-double>;
  slot gtk-text-attributes-left-margin :: <C-signed-int>;
  slot gtk-text-attributes-right-margin :: <C-signed-int>;
  slot gtk-text-attributes-indent :: <C-signed-int>;
  slot gtk-text-attributes-pixels-above-lines :: <C-signed-int>;
  slot gtk-text-attributes-pixels-below-lines :: <C-signed-int>;
  slot gtk-text-attributes-pixels-inside-wrap :: <C-signed-int>;
  slot gtk-text-attributes-tabs :: <PangoTabArray>;
  slot gtk-text-attributes-wrap-mode :: <GtkWrapMode>;
  slot gtk-text-attributes-language :: <PangoLanguage>;
  constant slot gtk-text-attributes-pg-bg-color :: <GdkColor>;
  slot gtk-text-attributes-invisible :: <C-unsigned-int>;
  slot gtk-text-attributes-bg-full-height :: <C-unsigned-int>;
  slot gtk-text-attributes-editable :: <C-unsigned-int>;
  constant slot gtk-text-attributes-pg-bg-rgba :: <GdkRGBA>;
  constant slot gtk-text-attributes-padding :: <C-unsigned-int*>;
  pointer-type-name: <GtkTextAttributes>;
end C-struct;

define C-function gtk-text-attributes-new
  result res :: <GtkTextAttributes>;
  c-name: "gtk_text_attributes_new";
end;

define C-function gtk-text-attributes-copy
  input parameter self :: <GtkTextAttributes>;
  result res :: <GtkTextAttributes>;
  c-name: "gtk_text_attributes_copy";
end;

define C-function gtk-text-attributes-copy-values
  input parameter self :: <GtkTextAttributes>;
  input parameter dest_ :: <GtkTextAttributes>;
  c-name: "gtk_text_attributes_copy_values";
end;

define C-function gtk-text-attributes-ref
  input parameter self :: <GtkTextAttributes>;
  result res :: <GtkTextAttributes>;
  c-name: "gtk_text_attributes_ref";
end;

define C-function gtk-text-attributes-unref
  input parameter self :: <GtkTextAttributes>;
  c-name: "gtk_text_attributes_unref";
end;

define C-struct <_GtkTextBTree>
  pointer-type-name: <GtkTextBTree>;
end C-struct;

define open C-subtype <GtkTextBuffer> (<GObject>)
  constant slot gtk-text-buffer-parent-instance :: <GObject>;
  constant slot gtk-text-buffer-priv :: <GtkTextBufferPrivate>;
end C-subtype;

define C-pointer-type <GtkTextBuffer*> => <GtkTextBuffer>;

define C-function gtk-text-buffer-new
  input parameter table_ :: <GtkTextTagTable>;
  result res :: <GtkTextBuffer>;
  c-name: "gtk_text_buffer_new";
end;

define C-function gtk-text-buffer-add-mark
  input parameter self :: <GtkTextBuffer>;
  input parameter mark_ :: <GtkTextMark>;
  input parameter where_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_add_mark";
end;

define C-function gtk-text-buffer-add-selection-clipboard
  input parameter self :: <GtkTextBuffer>;
  input parameter clipboard_ :: <GtkClipboard>;
  c-name: "gtk_text_buffer_add_selection_clipboard";
end;

define C-function gtk-text-buffer-apply-tag
  input parameter self :: <GtkTextBuffer>;
  input parameter tag_ :: <GtkTextTag>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_apply_tag";
end;

define C-function gtk-text-buffer-apply-tag-by-name
  input parameter self :: <GtkTextBuffer>;
  input parameter name_ :: <C-string>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_apply_tag_by_name";
end;

define C-function gtk-text-buffer-backspace
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter interactive_ :: <C-boolean>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_backspace";
end;

define C-function gtk-text-buffer-begin-user-action
  input parameter self :: <GtkTextBuffer>;
  c-name: "gtk_text_buffer_begin_user_action";
end;

define C-function gtk-text-buffer-copy-clipboard
  input parameter self :: <GtkTextBuffer>;
  input parameter clipboard_ :: <GtkClipboard>;
  c-name: "gtk_text_buffer_copy_clipboard";
end;

define C-function gtk-text-buffer-create-child-anchor
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <GtkTextChildAnchor>;
  c-name: "gtk_text_buffer_create_child_anchor";
end;

define C-function gtk-text-buffer-create-mark
  input parameter self :: <GtkTextBuffer>;
  input parameter mark_name_ :: <C-string>;
  input parameter where_ :: <GtkTextIter>;
  input parameter left_gravity_ :: <C-boolean>;
  result res :: <GtkTextMark>;
  c-name: "gtk_text_buffer_create_mark";
end;

define C-function gtk-text-buffer-cut-clipboard
  input parameter self :: <GtkTextBuffer>;
  input parameter clipboard_ :: <GtkClipboard>;
  input parameter default_editable_ :: <C-boolean>;
  c-name: "gtk_text_buffer_cut_clipboard";
end;

define C-function gtk-text-buffer-delete
  input parameter self :: <GtkTextBuffer>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_delete";
end;

define C-function gtk-text-buffer-delete-interactive
  input parameter self :: <GtkTextBuffer>;
  input parameter start_iter_ :: <GtkTextIter>;
  input parameter end_iter_ :: <GtkTextIter>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_delete_interactive";
end;

define C-function gtk-text-buffer-delete-mark
  input parameter self :: <GtkTextBuffer>;
  input parameter mark_ :: <GtkTextMark>;
  c-name: "gtk_text_buffer_delete_mark";
end;

define C-function gtk-text-buffer-delete-mark-by-name
  input parameter self :: <GtkTextBuffer>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_text_buffer_delete_mark_by_name";
end;

define C-function gtk-text-buffer-delete-selection
  input parameter self :: <GtkTextBuffer>;
  input parameter interactive_ :: <C-boolean>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_delete_selection";
end;

define C-function gtk-text-buffer-deserialize
  input parameter self :: <GtkTextBuffer>;
  input parameter content_buffer_ :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_deserialize";
end;

define C-function gtk-text-buffer-deserialize-get-can-create-tags
  input parameter self :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_deserialize_get_can_create_tags";
end;

define C-function gtk-text-buffer-deserialize-set-can-create-tags
  input parameter self :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  input parameter can_create_tags_ :: <C-boolean>;
  c-name: "gtk_text_buffer_deserialize_set_can_create_tags";
end;

define C-function gtk-text-buffer-end-user-action
  input parameter self :: <GtkTextBuffer>;
  c-name: "gtk_text_buffer_end_user_action";
end;

define C-function gtk-text-buffer-get-bounds
  input parameter self :: <GtkTextBuffer>;
  output parameter start_ :: <GtkTextIter>;
  output parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_get_bounds";
end;

define C-function gtk-text-buffer-get-char-count
  input parameter self :: <GtkTextBuffer>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_char_count";
end;

define C-function gtk-text-buffer-get-copy-target-list
  input parameter self :: <GtkTextBuffer>;
  result res :: <GtkTargetList>;
  c-name: "gtk_text_buffer_get_copy_target_list";
end;

define C-function gtk-text-buffer-get-deserialize-formats
  input parameter self :: <GtkTextBuffer>;
  output parameter n_formats_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_text_buffer_get_deserialize_formats";
end;

define C-function gtk-text-buffer-get-end-iter
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_get_end_iter";
end;

define C-function gtk-text-buffer-get-has-selection
  input parameter self :: <GtkTextBuffer>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_get_has_selection";
end;

define C-function gtk-text-buffer-get-insert
  input parameter self :: <GtkTextBuffer>;
  result res :: <GtkTextMark>;
  c-name: "gtk_text_buffer_get_insert";
end;

define C-function gtk-text-buffer-get-iter-at-child-anchor
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter anchor_ :: <GtkTextChildAnchor>;
  c-name: "gtk_text_buffer_get_iter_at_child_anchor";
end;

define C-function gtk-text-buffer-get-iter-at-line
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter line_number_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_iter_at_line";
end;

define C-function gtk-text-buffer-get-iter-at-line-index
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter line_number_ :: <C-signed-int>;
  input parameter byte_index_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_iter_at_line_index";
end;

define C-function gtk-text-buffer-get-iter-at-line-offset
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter line_number_ :: <C-signed-int>;
  input parameter char_offset_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_iter_at_line_offset";
end;

define C-function gtk-text-buffer-get-iter-at-mark
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter mark_ :: <GtkTextMark>;
  c-name: "gtk_text_buffer_get_iter_at_mark";
end;

define C-function gtk-text-buffer-get-iter-at-offset
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter char_offset_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_iter_at_offset";
end;

define C-function gtk-text-buffer-get-line-count
  input parameter self :: <GtkTextBuffer>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_buffer_get_line_count";
end;

define C-function gtk-text-buffer-get-mark
  input parameter self :: <GtkTextBuffer>;
  input parameter name_ :: <C-string>;
  result res :: <GtkTextMark>;
  c-name: "gtk_text_buffer_get_mark";
end;

define C-function gtk-text-buffer-get-modified
  input parameter self :: <GtkTextBuffer>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_get_modified";
end;

define C-function gtk-text-buffer-get-paste-target-list
  input parameter self :: <GtkTextBuffer>;
  result res :: <GtkTargetList>;
  c-name: "gtk_text_buffer_get_paste_target_list";
end;

define C-function gtk-text-buffer-get-selection-bound
  input parameter self :: <GtkTextBuffer>;
  result res :: <GtkTextMark>;
  c-name: "gtk_text_buffer_get_selection_bound";
end;

define C-function gtk-text-buffer-get-selection-bounds
  input parameter self :: <GtkTextBuffer>;
  output parameter start_ :: <GtkTextIter>;
  output parameter end_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_get_selection_bounds";
end;

define C-function gtk-text-buffer-get-serialize-formats
  input parameter self :: <GtkTextBuffer>;
  output parameter n_formats_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_text_buffer_get_serialize_formats";
end;

define C-function gtk-text-buffer-get-slice
  input parameter self :: <GtkTextBuffer>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  input parameter include_hidden_chars_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "gtk_text_buffer_get_slice";
end;

define C-function gtk-text-buffer-get-start-iter
  input parameter self :: <GtkTextBuffer>;
  output parameter iter_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_get_start_iter";
end;

define C-function gtk-text-buffer-get-tag-table
  input parameter self :: <GtkTextBuffer>;
  result res :: <GtkTextTagTable>;
  c-name: "gtk_text_buffer_get_tag_table";
end;

define C-function gtk-text-buffer-get-text
  input parameter self :: <GtkTextBuffer>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  input parameter include_hidden_chars_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "gtk_text_buffer_get_text";
end;

define C-function gtk-text-buffer-insert
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_insert";
end;

define C-function gtk-text-buffer-insert-at-cursor
  input parameter self :: <GtkTextBuffer>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_insert_at_cursor";
end;

define C-function gtk-text-buffer-insert-child-anchor
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter anchor_ :: <GtkTextChildAnchor>;
  c-name: "gtk_text_buffer_insert_child_anchor";
end;

define C-function gtk-text-buffer-insert-interactive
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_insert_interactive";
end;

define C-function gtk-text-buffer-insert-interactive-at-cursor
  input parameter self :: <GtkTextBuffer>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_insert_interactive_at_cursor";
end;

define C-function gtk-text-buffer-insert-pixbuf
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_text_buffer_insert_pixbuf";
end;

define C-function gtk-text-buffer-insert-range
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_insert_range";
end;

define C-function gtk-text-buffer-insert-range-interactive
  input parameter self :: <GtkTextBuffer>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  input parameter default_editable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_buffer_insert_range_interactive";
end;

define C-function gtk-text-buffer-move-mark
  input parameter self :: <GtkTextBuffer>;
  input parameter mark_ :: <GtkTextMark>;
  input parameter where_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_move_mark";
end;

define C-function gtk-text-buffer-move-mark-by-name
  input parameter self :: <GtkTextBuffer>;
  input parameter name_ :: <C-string>;
  input parameter where_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_move_mark_by_name";
end;

define C-function gtk-text-buffer-paste-clipboard
  input parameter self :: <GtkTextBuffer>;
  input parameter clipboard_ :: <GtkClipboard>;
  input parameter override_location_ :: <GtkTextIter>;
  input parameter default_editable_ :: <C-boolean>;
  c-name: "gtk_text_buffer_paste_clipboard";
end;

define C-function gtk-text-buffer-place-cursor
  input parameter self :: <GtkTextBuffer>;
  input parameter where_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_place_cursor";
end;

define C-function gtk-text-buffer-register-deserialize-format
  input parameter self :: <GtkTextBuffer>;
  input parameter mime_type_ :: <C-string>;
  input parameter function_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_destroy_ :: <C-function-pointer>;
  result res :: <GdkAtom>;
  c-name: "gtk_text_buffer_register_deserialize_format";
end;

define C-function gtk-text-buffer-register-deserialize-tagset
  input parameter self :: <GtkTextBuffer>;
  input parameter tagset_name_ :: <C-string>;
  result res :: <GdkAtom>;
  c-name: "gtk_text_buffer_register_deserialize_tagset";
end;

define C-function gtk-text-buffer-register-serialize-format
  input parameter self :: <GtkTextBuffer>;
  input parameter mime_type_ :: <C-string>;
  input parameter function_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_destroy_ :: <C-function-pointer>;
  result res :: <GdkAtom>;
  c-name: "gtk_text_buffer_register_serialize_format";
end;

define C-function gtk-text-buffer-register-serialize-tagset
  input parameter self :: <GtkTextBuffer>;
  input parameter tagset_name_ :: <C-string>;
  result res :: <GdkAtom>;
  c-name: "gtk_text_buffer_register_serialize_tagset";
end;

define C-function gtk-text-buffer-remove-all-tags
  input parameter self :: <GtkTextBuffer>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_remove_all_tags";
end;

define C-function gtk-text-buffer-remove-selection-clipboard
  input parameter self :: <GtkTextBuffer>;
  input parameter clipboard_ :: <GtkClipboard>;
  c-name: "gtk_text_buffer_remove_selection_clipboard";
end;

define C-function gtk-text-buffer-remove-tag
  input parameter self :: <GtkTextBuffer>;
  input parameter tag_ :: <GtkTextTag>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_remove_tag";
end;

define C-function gtk-text-buffer-remove-tag-by-name
  input parameter self :: <GtkTextBuffer>;
  input parameter name_ :: <C-string>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_remove_tag_by_name";
end;

define C-function gtk-text-buffer-select-range
  input parameter self :: <GtkTextBuffer>;
  input parameter ins_ :: <GtkTextIter>;
  input parameter bound_ :: <GtkTextIter>;
  c-name: "gtk_text_buffer_select_range";
end;

define C-function gtk-text-buffer-serialize
  input parameter self :: <GtkTextBuffer>;
  input parameter content_buffer_ :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "gtk_text_buffer_serialize";
end;

define C-function gtk-text-buffer-set-modified
  input parameter self :: <GtkTextBuffer>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_text_buffer_set_modified";
end;

define C-function gtk-text-buffer-set-text
  input parameter self :: <GtkTextBuffer>;
  input parameter text_ :: <C-string>;
  input parameter len_ :: <C-signed-int>;
  c-name: "gtk_text_buffer_set_text";
end;

define C-function gtk-text-buffer-unregister-deserialize-format
  input parameter self :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  c-name: "gtk_text_buffer_unregister_deserialize_format";
end;

define C-function gtk-text-buffer-unregister-serialize-format
  input parameter self :: <GtkTextBuffer>;
  input parameter format_ :: <GdkAtom>;
  c-name: "gtk_text_buffer_unregister_serialize_format";
end;

define C-struct <_GtkTextBufferClass>
  constant slot gtk-text-buffer-class-parent-class :: <GObjectClass>;
  constant slot gtk-text-buffer-class-insert-text :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-insert-pixbuf :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-insert-child-anchor :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-delete-range :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-changed :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-modified-changed :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-mark-set :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-mark-deleted :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-apply-tag :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-remove-tag :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-begin-user-action :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-end-user-action :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-paste-done :: <C-function-pointer>;
  constant slot gtk-text-buffer-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-buffer-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-buffer-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-buffer-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTextBufferClass>;
end C-struct;

define C-struct <_GtkTextBufferPrivate>
  pointer-type-name: <GtkTextBufferPrivate>;
end C-struct;

define constant $gtk-text-buffer-target-info-buffer-contents = -1;
define constant $gtk-text-buffer-target-info-rich-text = -2;
define constant $gtk-text-buffer-target-info-text = -3;
define constant <GtkTextBufferTargetInfo> = <C-int>;
define C-pointer-type <GtkTextBufferTargetInfo*> => <GtkTextBufferTargetInfo>;

define open C-subtype <GtkTextChildAnchor> (<GObject>)
  constant slot gtk-text-child-anchor-parent-instance :: <GObject>;
  constant slot gtk-text-child-anchor-segment :: <C-void*>;
end C-subtype;

define C-pointer-type <GtkTextChildAnchor*> => <GtkTextChildAnchor>;

define C-function gtk-text-child-anchor-new
  result res :: <GtkTextChildAnchor>;
  c-name: "gtk_text_child_anchor_new";
end;

define C-function gtk-text-child-anchor-get-deleted
  input parameter self :: <GtkTextChildAnchor>;
  result res :: <C-boolean>;
  c-name: "gtk_text_child_anchor_get_deleted";
end;

define C-function gtk-text-child-anchor-get-widgets
  input parameter self :: <GtkTextChildAnchor>;
  result res :: <GList>;
  c-name: "gtk_text_child_anchor_get_widgets";
end;

define C-struct <_GtkTextChildAnchorClass>
  constant slot gtk-text-child-anchor-class-parent-class :: <GObjectClass>;
  constant slot gtk-text-child-anchor-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-child-anchor-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-child-anchor-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-child-anchor-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTextChildAnchorClass>;
end C-struct;

define constant $gtk-text-dir-none = 0;
define constant $gtk-text-dir-ltr = 1;
define constant $gtk-text-dir-rtl = 2;
define constant <GtkTextDirection> = <C-int>;
define C-pointer-type <GtkTextDirection*> => <GtkTextDirection>;

define C-struct <_GtkTextIter>
  constant slot gtk-text-iter-dummy1 :: <C-void*>;
  constant slot gtk-text-iter-dummy2 :: <C-void*>;
  constant slot gtk-text-iter-dummy3 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy4 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy5 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy6 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy7 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy8 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy9 :: <C-void*>;
  constant slot gtk-text-iter-dummy10 :: <C-void*>;
  constant slot gtk-text-iter-dummy11 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy12 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy13 :: <C-signed-int>;
  constant slot gtk-text-iter-dummy14 :: <C-void*>;
  pointer-type-name: <GtkTextIter>;
end C-struct;

define C-function gtk-text-iter-assign
  input parameter self :: <GtkTextIter>;
  input parameter other_ :: <GtkTextIter>;
  c-name: "gtk_text_iter_assign";
end;

define C-function gtk-text-iter-backward-char
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_char";
end;

define C-function gtk-text-iter-backward-chars
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_chars";
end;

define C-function gtk-text-iter-backward-cursor-position
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_cursor_position";
end;

define C-function gtk-text-iter-backward-cursor-positions
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_cursor_positions";
end;

define C-function gtk-text-iter-backward-find-char
  input parameter self :: <GtkTextIter>;
  input parameter pred_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter limit_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_find_char";
end;

define C-function gtk-text-iter-backward-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_line";
end;

define C-function gtk-text-iter-backward-lines
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_lines";
end;

define C-function gtk-text-iter-backward-search
  input parameter self :: <GtkTextIter>;
  input parameter str_ :: <C-string>;
  input parameter flags_ :: <GtkTextSearchFlags>;
  output parameter match_start_ :: <GtkTextIter>;
  output parameter match_end_ :: <GtkTextIter>;
  input parameter limit_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_search";
end;

define C-function gtk-text-iter-backward-sentence-start
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_sentence_start";
end;

define C-function gtk-text-iter-backward-sentence-starts
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_sentence_starts";
end;

define C-function gtk-text-iter-backward-to-tag-toggle
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_to_tag_toggle";
end;

define C-function gtk-text-iter-backward-visible-cursor-position
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_cursor_position";
end;

define C-function gtk-text-iter-backward-visible-cursor-positions
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_cursor_positions";
end;

define C-function gtk-text-iter-backward-visible-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_line";
end;

define C-function gtk-text-iter-backward-visible-lines
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_lines";
end;

define C-function gtk-text-iter-backward-visible-word-start
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_word_start";
end;

define C-function gtk-text-iter-backward-visible-word-starts
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_visible_word_starts";
end;

define C-function gtk-text-iter-backward-word-start
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_word_start";
end;

define C-function gtk-text-iter-backward-word-starts
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_backward_word_starts";
end;

define C-function gtk-text-iter-begins-tag
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_begins_tag";
end;

define C-function gtk-text-iter-can-insert
  input parameter self :: <GtkTextIter>;
  input parameter default_editability_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_can_insert";
end;

define C-function gtk-text-iter-compare
  input parameter self :: <GtkTextIter>;
  input parameter rhs_ :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_compare";
end;

define C-function gtk-text-iter-copy
  input parameter self :: <GtkTextIter>;
  result res :: <GtkTextIter>;
  c-name: "gtk_text_iter_copy";
end;

define C-function gtk-text-iter-editable
  input parameter self :: <GtkTextIter>;
  input parameter default_setting_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_editable";
end;

define C-function gtk-text-iter-ends-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_ends_line";
end;

define C-function gtk-text-iter-ends-sentence
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_ends_sentence";
end;

define C-function gtk-text-iter-ends-tag
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_ends_tag";
end;

define C-function gtk-text-iter-ends-word
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_ends_word";
end;

define C-function gtk-text-iter-equal
  input parameter self :: <GtkTextIter>;
  input parameter rhs_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_equal";
end;

define C-function gtk-text-iter-forward-char
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_char";
end;

define C-function gtk-text-iter-forward-chars
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_chars";
end;

define C-function gtk-text-iter-forward-cursor-position
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_cursor_position";
end;

define C-function gtk-text-iter-forward-cursor-positions
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_cursor_positions";
end;

define C-function gtk-text-iter-forward-find-char
  input parameter self :: <GtkTextIter>;
  input parameter pred_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter limit_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_find_char";
end;

define C-function gtk-text-iter-forward-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_line";
end;

define C-function gtk-text-iter-forward-lines
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_lines";
end;

define C-function gtk-text-iter-forward-search
  input parameter self :: <GtkTextIter>;
  input parameter str_ :: <C-string>;
  input parameter flags_ :: <GtkTextSearchFlags>;
  output parameter match_start_ :: <GtkTextIter>;
  output parameter match_end_ :: <GtkTextIter>;
  input parameter limit_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_search";
end;

define C-function gtk-text-iter-forward-sentence-end
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_sentence_end";
end;

define C-function gtk-text-iter-forward-sentence-ends
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_sentence_ends";
end;

define C-function gtk-text-iter-forward-to-end
  input parameter self :: <GtkTextIter>;
  c-name: "gtk_text_iter_forward_to_end";
end;

define C-function gtk-text-iter-forward-to-line-end
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_to_line_end";
end;

define C-function gtk-text-iter-forward-to-tag-toggle
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_to_tag_toggle";
end;

define C-function gtk-text-iter-forward-visible-cursor-position
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_cursor_position";
end;

define C-function gtk-text-iter-forward-visible-cursor-positions
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_cursor_positions";
end;

define C-function gtk-text-iter-forward-visible-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_line";
end;

define C-function gtk-text-iter-forward-visible-lines
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_lines";
end;

define C-function gtk-text-iter-forward-visible-word-end
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_word_end";
end;

define C-function gtk-text-iter-forward-visible-word-ends
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_visible_word_ends";
end;

define C-function gtk-text-iter-forward-word-end
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_word_end";
end;

define C-function gtk-text-iter-forward-word-ends
  input parameter self :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_forward_word_ends";
end;

define C-function gtk-text-iter-free
  input parameter self :: <GtkTextIter>;
  c-name: "gtk_text_iter_free";
end;

define C-function gtk-text-iter-get-attributes
  input parameter self :: <GtkTextIter>;
  output parameter values_ :: <GtkTextAttributes>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_get_attributes";
end;

define C-function gtk-text-iter-get-buffer
  input parameter self :: <GtkTextIter>;
  result res :: <GtkTextBuffer>;
  c-name: "gtk_text_iter_get_buffer";
end;

define C-function gtk-text-iter-get-bytes-in-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_bytes_in_line";
end;

define C-function gtk-text-iter-get-char
  input parameter self :: <GtkTextIter>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_text_iter_get_char";
end;

define C-function gtk-text-iter-get-chars-in-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_chars_in_line";
end;

define C-function gtk-text-iter-get-child-anchor
  input parameter self :: <GtkTextIter>;
  result res :: <GtkTextChildAnchor>;
  c-name: "gtk_text_iter_get_child_anchor";
end;

define C-function gtk-text-iter-get-language
  input parameter self :: <GtkTextIter>;
  result res :: <PangoLanguage>;
  c-name: "gtk_text_iter_get_language";
end;

define C-function gtk-text-iter-get-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_line";
end;

define C-function gtk-text-iter-get-line-index
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_line_index";
end;

define C-function gtk-text-iter-get-line-offset
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_line_offset";
end;

define C-function gtk-text-iter-get-marks
  input parameter self :: <GtkTextIter>;
  result res :: <GSList>;
  c-name: "gtk_text_iter_get_marks";
end;

define C-function gtk-text-iter-get-offset
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_offset";
end;

define C-function gtk-text-iter-get-pixbuf
  input parameter self :: <GtkTextIter>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_text_iter_get_pixbuf";
end;

define C-function gtk-text-iter-get-slice
  input parameter self :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  result res :: <C-string>;
  c-name: "gtk_text_iter_get_slice";
end;

define C-function gtk-text-iter-get-tags
  input parameter self :: <GtkTextIter>;
  result res :: <GSList>;
  c-name: "gtk_text_iter_get_tags";
end;

define C-function gtk-text-iter-get-text
  input parameter self :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  result res :: <C-string>;
  c-name: "gtk_text_iter_get_text";
end;

define C-function gtk-text-iter-get-toggled-tags
  input parameter self :: <GtkTextIter>;
  input parameter toggled_on_ :: <C-boolean>;
  result res :: <GSList>;
  c-name: "gtk_text_iter_get_toggled_tags";
end;

define C-function gtk-text-iter-get-visible-line-index
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_visible_line_index";
end;

define C-function gtk-text-iter-get-visible-line-offset
  input parameter self :: <GtkTextIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_iter_get_visible_line_offset";
end;

define C-function gtk-text-iter-get-visible-slice
  input parameter self :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  result res :: <C-string>;
  c-name: "gtk_text_iter_get_visible_slice";
end;

define C-function gtk-text-iter-get-visible-text
  input parameter self :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  result res :: <C-string>;
  c-name: "gtk_text_iter_get_visible_text";
end;

define C-function gtk-text-iter-has-tag
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_has_tag";
end;

define C-function gtk-text-iter-in-range
  input parameter self :: <GtkTextIter>;
  input parameter start_ :: <GtkTextIter>;
  input parameter end_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_in_range";
end;

define C-function gtk-text-iter-inside-sentence
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_inside_sentence";
end;

define C-function gtk-text-iter-inside-word
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_inside_word";
end;

define C-function gtk-text-iter-is-cursor-position
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_is_cursor_position";
end;

define C-function gtk-text-iter-is-end
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_is_end";
end;

define C-function gtk-text-iter-is-start
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_is_start";
end;

define C-function gtk-text-iter-order
  input parameter self :: <GtkTextIter>;
  input parameter second_ :: <GtkTextIter>;
  c-name: "gtk_text_iter_order";
end;

define C-function gtk-text-iter-set-line
  input parameter self :: <GtkTextIter>;
  input parameter line_number_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_line";
end;

define C-function gtk-text-iter-set-line-index
  input parameter self :: <GtkTextIter>;
  input parameter byte_on_line_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_line_index";
end;

define C-function gtk-text-iter-set-line-offset
  input parameter self :: <GtkTextIter>;
  input parameter char_on_line_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_line_offset";
end;

define C-function gtk-text-iter-set-offset
  input parameter self :: <GtkTextIter>;
  input parameter char_offset_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_offset";
end;

define C-function gtk-text-iter-set-visible-line-index
  input parameter self :: <GtkTextIter>;
  input parameter byte_on_line_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_visible_line_index";
end;

define C-function gtk-text-iter-set-visible-line-offset
  input parameter self :: <GtkTextIter>;
  input parameter char_on_line_ :: <C-signed-int>;
  c-name: "gtk_text_iter_set_visible_line_offset";
end;

define C-function gtk-text-iter-starts-line
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_starts_line";
end;

define C-function gtk-text-iter-starts-sentence
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_starts_sentence";
end;

define C-function gtk-text-iter-starts-word
  input parameter self :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_starts_word";
end;

define C-function gtk-text-iter-toggles-tag
  input parameter self :: <GtkTextIter>;
  input parameter tag_ :: <GtkTextTag>;
  result res :: <C-boolean>;
  c-name: "gtk_text_iter_toggles_tag";
end;

define open C-subtype <GtkTextMark> (<GObject>)
  constant slot gtk-text-mark-parent-instance :: <GObject>;
  constant slot gtk-text-mark-segment :: <C-void*>;
end C-subtype;

define C-pointer-type <GtkTextMark*> => <GtkTextMark>;

define C-function gtk-text-mark-new
  input parameter name_ :: <C-string>;
  input parameter left_gravity_ :: <C-boolean>;
  result res :: <GtkTextMark>;
  c-name: "gtk_text_mark_new";
end;

define C-function gtk-text-mark-get-buffer
  input parameter self :: <GtkTextMark>;
  result res :: <GtkTextBuffer>;
  c-name: "gtk_text_mark_get_buffer";
end;

define C-function gtk-text-mark-get-deleted
  input parameter self :: <GtkTextMark>;
  result res :: <C-boolean>;
  c-name: "gtk_text_mark_get_deleted";
end;

define C-function gtk-text-mark-get-left-gravity
  input parameter self :: <GtkTextMark>;
  result res :: <C-boolean>;
  c-name: "gtk_text_mark_get_left_gravity";
end;

define C-function gtk-text-mark-get-name
  input parameter self :: <GtkTextMark>;
  result res :: <C-string>;
  c-name: "gtk_text_mark_get_name";
end;

define C-function gtk-text-mark-get-visible
  input parameter self :: <GtkTextMark>;
  result res :: <C-boolean>;
  c-name: "gtk_text_mark_get_visible";
end;

define C-function gtk-text-mark-set-visible
  input parameter self :: <GtkTextMark>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_text_mark_set_visible";
end;

define C-struct <_GtkTextMarkClass>
  constant slot gtk-text-mark-class-parent-class :: <GObjectClass>;
  constant slot gtk-text-mark-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-mark-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-mark-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-mark-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTextMarkClass>;
end C-struct;

define constant $gtk-text-search-visible-only = 1;
define constant $gtk-text-search-text-only = 2;
define constant $gtk-text-search-case-insensitive = 4;
define constant <GtkTextSearchFlags> = <C-int>;
define C-pointer-type <GtkTextSearchFlags*> => <GtkTextSearchFlags>;

define open C-subtype <GtkTextTag> (<GObject>)
  constant slot gtk-text-tag-parent-instance :: <GObject>;
  constant slot gtk-text-tag-priv :: <GtkTextTagPrivate>;
end C-subtype;

define C-pointer-type <GtkTextTag*> => <GtkTextTag>;

define C-function gtk-text-tag-new
  input parameter name_ :: <C-string>;
  result res :: <GtkTextTag>;
  c-name: "gtk_text_tag_new";
end;

define C-function gtk-text-tag-event
  input parameter self :: <GtkTextTag>;
  input parameter event_object_ :: <GObject>;
  input parameter event_ :: <GdkEvent>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_tag_event";
end;

define C-function gtk-text-tag-get-priority
  input parameter self :: <GtkTextTag>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_tag_get_priority";
end;

define C-function gtk-text-tag-set-priority
  input parameter self :: <GtkTextTag>;
  input parameter priority_ :: <C-signed-int>;
  c-name: "gtk_text_tag_set_priority";
end;

define C-struct <_GtkTextTagClass>
  constant slot gtk-text-tag-class-parent-class :: <GObjectClass>;
  constant slot gtk-text-tag-class-event :: <C-function-pointer>;
  constant slot gtk-text-tag-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-tag-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-tag-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-tag-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTextTagClass>;
end C-struct;

define C-struct <_GtkTextTagPrivate>
  pointer-type-name: <GtkTextTagPrivate>;
end C-struct;

define open C-subtype <GtkTextTagTable> (<GObject>)
  constant slot gtk-text-tag-table-parent-instance :: <GObject>;
  constant slot gtk-text-tag-table-priv :: <GtkTextTagTablePrivate>;
end C-subtype;

define C-pointer-type <GtkTextTagTable*> => <GtkTextTagTable>;

define C-function gtk-text-tag-table-new
  result res :: <GtkTextTagTable>;
  c-name: "gtk_text_tag_table_new";
end;

define C-function gtk-text-tag-table-add
  input parameter self :: <GtkTextTagTable>;
  input parameter tag_ :: <GtkTextTag>;
  c-name: "gtk_text_tag_table_add";
end;

define C-function gtk-text-tag-table-foreach
  input parameter self :: <GtkTextTagTable>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_text_tag_table_foreach";
end;

define C-function gtk-text-tag-table-get-size
  input parameter self :: <GtkTextTagTable>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_tag_table_get_size";
end;

define C-function gtk-text-tag-table-lookup
  input parameter self :: <GtkTextTagTable>;
  input parameter name_ :: <C-string>;
  result res :: <GtkTextTag>;
  c-name: "gtk_text_tag_table_lookup";
end;

define C-function gtk-text-tag-table-remove
  input parameter self :: <GtkTextTagTable>;
  input parameter tag_ :: <GtkTextTag>;
  c-name: "gtk_text_tag_table_remove";
end;

define C-struct <_GtkTextTagTableClass>
  constant slot gtk-text-tag-table-class-parent-class :: <GObjectClass>;
  constant slot gtk-text-tag-table-class-tag-changed :: <C-function-pointer>;
  constant slot gtk-text-tag-table-class-tag-added :: <C-function-pointer>;
  constant slot gtk-text-tag-table-class-tag-removed :: <C-function-pointer>;
  constant slot gtk-text-tag-table-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-tag-table-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-tag-table-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-tag-table-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTextTagTableClass>;
end C-struct;

define C-struct <_GtkTextTagTablePrivate>
  pointer-type-name: <GtkTextTagTablePrivate>;
end C-struct;

define open C-subtype <GtkTextView> (<GtkContainer>)
  constant slot gtk-text-view-parent-instance :: <GtkContainer>;
  constant slot gtk-text-view-priv :: <GtkTextViewPrivate>;
end C-subtype;

define C-pointer-type <GtkTextView*> => <GtkTextView>;

define C-function gtk-text-view-new
  result res :: <GtkWidget>;
  c-name: "gtk_text_view_new";
end;

define C-function gtk-text-view-new-with-buffer
  input parameter buffer_ :: <GtkTextBuffer>;
  result res :: <GtkWidget>;
  c-name: "gtk_text_view_new_with_buffer";
end;

define C-function gtk-text-view-add-child-at-anchor
  input parameter self :: <GtkTextView>;
  input parameter child_ :: <GtkWidget>;
  input parameter anchor_ :: <GtkTextChildAnchor>;
  c-name: "gtk_text_view_add_child_at_anchor";
end;

define C-function gtk-text-view-add-child-in-window
  input parameter self :: <GtkTextView>;
  input parameter child_ :: <GtkWidget>;
  input parameter which_window_ :: <GtkTextWindowType>;
  input parameter xpos_ :: <C-signed-int>;
  input parameter ypos_ :: <C-signed-int>;
  c-name: "gtk_text_view_add_child_in_window";
end;

define C-function gtk-text-view-backward-display-line
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_backward_display_line";
end;

define C-function gtk-text-view-backward-display-line-start
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_backward_display_line_start";
end;

define C-function gtk-text-view-buffer-to-window-coords
  input parameter self :: <GtkTextView>;
  input parameter win_ :: <GtkTextWindowType>;
  input parameter buffer_x_ :: <C-signed-int>;
  input parameter buffer_y_ :: <C-signed-int>;
  output parameter window_x_ :: <C-signed-int*>;
  output parameter window_y_ :: <C-signed-int*>;
  c-name: "gtk_text_view_buffer_to_window_coords";
end;

define C-function gtk-text-view-forward-display-line
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_forward_display_line";
end;

define C-function gtk-text-view-forward-display-line-end
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_forward_display_line_end";
end;

define C-function gtk-text-view-get-accepts-tab
  input parameter self :: <GtkTextView>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_get_accepts_tab";
end;

define C-function gtk-text-view-get-border-window-size
  input parameter self :: <GtkTextView>;
  input parameter type_ :: <GtkTextWindowType>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_border_window_size";
end;

define C-function gtk-text-view-get-buffer
  input parameter self :: <GtkTextView>;
  result res :: <GtkTextBuffer>;
  c-name: "gtk_text_view_get_buffer";
end;

define C-function gtk-text-view-get-cursor-locations
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  output parameter strong_ :: <cairoRectangleInt>;
  output parameter weak_ :: <cairoRectangleInt>;
  c-name: "gtk_text_view_get_cursor_locations";
end;

define C-function gtk-text-view-get-cursor-visible
  input parameter self :: <GtkTextView>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_get_cursor_visible";
end;

define C-function gtk-text-view-get-default-attributes
  input parameter self :: <GtkTextView>;
  result res :: <GtkTextAttributes>;
  c-name: "gtk_text_view_get_default_attributes";
end;

define C-function gtk-text-view-get-editable
  input parameter self :: <GtkTextView>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_get_editable";
end;

define C-function gtk-text-view-get-hadjustment
  input parameter self :: <GtkTextView>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_text_view_get_hadjustment";
end;

define C-function gtk-text-view-get-indent
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_indent";
end;

define C-function gtk-text-view-get-iter-at-location
  input parameter self :: <GtkTextView>;
  output parameter iter_ :: <GtkTextIter>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_text_view_get_iter_at_location";
end;

define C-function gtk-text-view-get-iter-at-position
  input parameter self :: <GtkTextView>;
  output parameter iter_ :: <GtkTextIter>;
  output parameter trailing_ :: <C-signed-int*>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_text_view_get_iter_at_position";
end;

define C-function gtk-text-view-get-iter-location
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  output parameter location_ :: <cairoRectangleInt>;
  c-name: "gtk_text_view_get_iter_location";
end;

define C-function gtk-text-view-get-justification
  input parameter self :: <GtkTextView>;
  result res :: <GtkJustification>;
  c-name: "gtk_text_view_get_justification";
end;

define C-function gtk-text-view-get-left-margin
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_left_margin";
end;

define C-function gtk-text-view-get-line-at-y
  input parameter self :: <GtkTextView>;
  output parameter target_iter_ :: <GtkTextIter>;
  input parameter y_ :: <C-signed-int>;
  output parameter line_top_ :: <C-signed-int*>;
  c-name: "gtk_text_view_get_line_at_y";
end;

define C-function gtk-text-view-get-line-yrange
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  output parameter y_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_text_view_get_line_yrange";
end;

define C-function gtk-text-view-get-overwrite
  input parameter self :: <GtkTextView>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_get_overwrite";
end;

define C-function gtk-text-view-get-pixels-above-lines
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_pixels_above_lines";
end;

define C-function gtk-text-view-get-pixels-below-lines
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_pixels_below_lines";
end;

define C-function gtk-text-view-get-pixels-inside-wrap
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_pixels_inside_wrap";
end;

define C-function gtk-text-view-get-right-margin
  input parameter self :: <GtkTextView>;
  result res :: <C-signed-int>;
  c-name: "gtk_text_view_get_right_margin";
end;

define C-function gtk-text-view-get-tabs
  input parameter self :: <GtkTextView>;
  result res :: <PangoTabArray>;
  c-name: "gtk_text_view_get_tabs";
end;

define C-function gtk-text-view-get-vadjustment
  input parameter self :: <GtkTextView>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_text_view_get_vadjustment";
end;

define C-function gtk-text-view-get-visible-rect
  input parameter self :: <GtkTextView>;
  output parameter visible_rect_ :: <cairoRectangleInt>;
  c-name: "gtk_text_view_get_visible_rect";
end;

define C-function gtk-text-view-get-window
  input parameter self :: <GtkTextView>;
  input parameter win_ :: <GtkTextWindowType>;
  result res :: <GdkWindow>;
  c-name: "gtk_text_view_get_window";
end;

define C-function gtk-text-view-get-window-type
  input parameter self :: <GtkTextView>;
  input parameter window_ :: <GdkWindow>;
  result res :: <GtkTextWindowType>;
  c-name: "gtk_text_view_get_window_type";
end;

define C-function gtk-text-view-get-wrap-mode
  input parameter self :: <GtkTextView>;
  result res :: <GtkWrapMode>;
  c-name: "gtk_text_view_get_wrap_mode";
end;

define C-function gtk-text-view-im-context-filter-keypress
  input parameter self :: <GtkTextView>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_im_context_filter_keypress";
end;

define C-function gtk-text-view-move-child
  input parameter self :: <GtkTextView>;
  input parameter child_ :: <GtkWidget>;
  input parameter xpos_ :: <C-signed-int>;
  input parameter ypos_ :: <C-signed-int>;
  c-name: "gtk_text_view_move_child";
end;

define C-function gtk-text-view-move-mark-onscreen
  input parameter self :: <GtkTextView>;
  input parameter mark_ :: <GtkTextMark>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_move_mark_onscreen";
end;

define C-function gtk-text-view-move-visually
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter count_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_move_visually";
end;

define C-function gtk-text-view-place-cursor-onscreen
  input parameter self :: <GtkTextView>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_place_cursor_onscreen";
end;

define C-function gtk-text-view-reset-im-context
  input parameter self :: <GtkTextView>;
  c-name: "gtk_text_view_reset_im_context";
end;

define C-function gtk-text-view-scroll-mark-onscreen
  input parameter self :: <GtkTextView>;
  input parameter mark_ :: <GtkTextMark>;
  c-name: "gtk_text_view_scroll_mark_onscreen";
end;

define C-function gtk-text-view-scroll-to-iter
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  input parameter within_margin_ :: <C-double>;
  input parameter use_align_ :: <C-boolean>;
  input parameter xalign_ :: <C-double>;
  input parameter yalign_ :: <C-double>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_scroll_to_iter";
end;

define C-function gtk-text-view-scroll-to-mark
  input parameter self :: <GtkTextView>;
  input parameter mark_ :: <GtkTextMark>;
  input parameter within_margin_ :: <C-double>;
  input parameter use_align_ :: <C-boolean>;
  input parameter xalign_ :: <C-double>;
  input parameter yalign_ :: <C-double>;
  c-name: "gtk_text_view_scroll_to_mark";
end;

define C-function gtk-text-view-set-accepts-tab
  input parameter self :: <GtkTextView>;
  input parameter accepts_tab_ :: <C-boolean>;
  c-name: "gtk_text_view_set_accepts_tab";
end;

define C-function gtk-text-view-set-border-window-size
  input parameter self :: <GtkTextView>;
  input parameter type_ :: <GtkTextWindowType>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_border_window_size";
end;

define C-function gtk-text-view-set-buffer
  input parameter self :: <GtkTextView>;
  input parameter buffer_ :: <GtkTextBuffer>;
  c-name: "gtk_text_view_set_buffer";
end;

define C-function gtk-text-view-set-cursor-visible
  input parameter self :: <GtkTextView>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_text_view_set_cursor_visible";
end;

define C-function gtk-text-view-set-editable
  input parameter self :: <GtkTextView>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_text_view_set_editable";
end;

define C-function gtk-text-view-set-indent
  input parameter self :: <GtkTextView>;
  input parameter indent_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_indent";
end;

define C-function gtk-text-view-set-justification
  input parameter self :: <GtkTextView>;
  input parameter justification_ :: <GtkJustification>;
  c-name: "gtk_text_view_set_justification";
end;

define C-function gtk-text-view-set-left-margin
  input parameter self :: <GtkTextView>;
  input parameter left_margin_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_left_margin";
end;

define C-function gtk-text-view-set-overwrite
  input parameter self :: <GtkTextView>;
  input parameter overwrite_ :: <C-boolean>;
  c-name: "gtk_text_view_set_overwrite";
end;

define C-function gtk-text-view-set-pixels-above-lines
  input parameter self :: <GtkTextView>;
  input parameter pixels_above_lines_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_pixels_above_lines";
end;

define C-function gtk-text-view-set-pixels-below-lines
  input parameter self :: <GtkTextView>;
  input parameter pixels_below_lines_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_pixels_below_lines";
end;

define C-function gtk-text-view-set-pixels-inside-wrap
  input parameter self :: <GtkTextView>;
  input parameter pixels_inside_wrap_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_pixels_inside_wrap";
end;

define C-function gtk-text-view-set-right-margin
  input parameter self :: <GtkTextView>;
  input parameter right_margin_ :: <C-signed-int>;
  c-name: "gtk_text_view_set_right_margin";
end;

define C-function gtk-text-view-set-tabs
  input parameter self :: <GtkTextView>;
  input parameter tabs_ :: <PangoTabArray>;
  c-name: "gtk_text_view_set_tabs";
end;

define C-function gtk-text-view-set-wrap-mode
  input parameter self :: <GtkTextView>;
  input parameter wrap_mode_ :: <GtkWrapMode>;
  c-name: "gtk_text_view_set_wrap_mode";
end;

define C-function gtk-text-view-starts-display-line
  input parameter self :: <GtkTextView>;
  input parameter iter_ :: <GtkTextIter>;
  result res :: <C-boolean>;
  c-name: "gtk_text_view_starts_display_line";
end;

define C-function gtk-text-view-window-to-buffer-coords
  input parameter self :: <GtkTextView>;
  input parameter win_ :: <GtkTextWindowType>;
  input parameter window_x_ :: <C-signed-int>;
  input parameter window_y_ :: <C-signed-int>;
  output parameter buffer_x_ :: <C-signed-int*>;
  output parameter buffer_y_ :: <C-signed-int*>;
  c-name: "gtk_text_view_window_to_buffer_coords";
end;

define C-struct <_GtkTextViewClass>
  constant slot gtk-text-view-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-text-view-class-populate-popup :: <C-function-pointer>;
  constant slot gtk-text-view-class-move-cursor :: <C-function-pointer>;
  constant slot gtk-text-view-class-set-anchor :: <C-function-pointer>;
  constant slot gtk-text-view-class-insert-at-cursor :: <C-function-pointer>;
  constant slot gtk-text-view-class-delete-from-cursor :: <C-function-pointer>;
  constant slot gtk-text-view-class-backspace :: <C-function-pointer>;
  constant slot gtk-text-view-class-cut-clipboard :: <C-function-pointer>;
  constant slot gtk-text-view-class-copy-clipboard :: <C-function-pointer>;
  constant slot gtk-text-view-class-paste-clipboard :: <C-function-pointer>;
  constant slot gtk-text-view-class-toggle-overwrite :: <C-function-pointer>;
  constant slot gtk-text-view-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-text-view-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkTextViewClass>;
end C-struct;

define C-struct <_GtkTextViewPrivate>
  pointer-type-name: <GtkTextViewPrivate>;
end C-struct;

define constant $gtk-text-window-private = 0;
define constant $gtk-text-window-widget = 1;
define constant $gtk-text-window-text = 2;
define constant $gtk-text-window-left = 3;
define constant $gtk-text-window-right = 4;
define constant $gtk-text-window-top = 5;
define constant $gtk-text-window-bottom = 6;
define constant <GtkTextWindowType> = <C-int>;
define C-pointer-type <GtkTextWindowType*> => <GtkTextWindowType>;

define C-struct <_GtkThemeEngine>
  pointer-type-name: <GtkThemeEngine>;
end C-struct;

define open C-subtype <GtkThemingEngine> (<GObject>)
  constant slot gtk-theming-engine-parent-object :: <GObject>;
  constant slot gtk-theming-engine-priv :: <GtkThemingEnginePrivate>;
end C-subtype;

define C-pointer-type <GtkThemingEngine*> => <GtkThemingEngine>;

define C-function gtk-theming-engine-load
  input parameter name_ :: <C-string>;
  result res :: <GtkThemingEngine>;
  c-name: "gtk_theming_engine_load";
end;

define C-function gtk-theming-engine-get-background-color
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_theming_engine_get_background_color";
end;

define C-function gtk-theming-engine-get-border
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter border_ :: <GtkBorder>;
  c-name: "gtk_theming_engine_get_border";
end;

define C-function gtk-theming-engine-get-border-color
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_theming_engine_get_border_color";
end;

define C-function gtk-theming-engine-get-color
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter color_ :: <GdkRGBA>;
  c-name: "gtk_theming_engine_get_color";
end;

define C-function gtk-theming-engine-get-direction
  input parameter self :: <GtkThemingEngine>;
  result res :: <GtkTextDirection>;
  c-name: "gtk_theming_engine_get_direction";
end;

define C-function gtk-theming-engine-get-font
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  result res :: <PangoFontDescription>;
  c-name: "gtk_theming_engine_get_font";
end;

define C-function gtk-theming-engine-get-junction-sides
  input parameter self :: <GtkThemingEngine>;
  result res :: <GtkJunctionSides>;
  c-name: "gtk_theming_engine_get_junction_sides";
end;

define C-function gtk-theming-engine-get-margin
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter margin_ :: <GtkBorder>;
  c-name: "gtk_theming_engine_get_margin";
end;

define C-function gtk-theming-engine-get-padding
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter padding_ :: <GtkBorder>;
  c-name: "gtk_theming_engine_get_padding";
end;

define C-function gtk-theming-engine-get-path
  input parameter self :: <GtkThemingEngine>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_theming_engine_get_path";
end;

define C-function gtk-theming-engine-get-property
  input parameter self :: <GtkThemingEngine>;
  input parameter property_ :: <C-string>;
  input parameter state_ :: <GtkStateFlags>;
  output parameter value_ :: <GValue>;
  c-name: "gtk_theming_engine_get_property";
end;

define C-function gtk-theming-engine-get-screen
  input parameter self :: <GtkThemingEngine>;
  result res :: <GdkScreen>;
  c-name: "gtk_theming_engine_get_screen";
end;

define C-function gtk-theming-engine-get-state
  input parameter self :: <GtkThemingEngine>;
  result res :: <GtkStateFlags>;
  c-name: "gtk_theming_engine_get_state";
end;

define C-function gtk-theming-engine-get-style-property
  input parameter self :: <GtkThemingEngine>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_theming_engine_get_style_property";
end;

define C-function gtk-theming-engine-has-class
  input parameter self :: <GtkThemingEngine>;
  input parameter style_class_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_theming_engine_has_class";
end;

define C-function gtk-theming-engine-has-region
  input parameter self :: <GtkThemingEngine>;
  input parameter style_region_ :: <C-string>;
  output parameter flags_ :: <GtkRegionFlags*>;
  result res :: <C-boolean>;
  c-name: "gtk_theming_engine_has_region";
end;

define C-function gtk-theming-engine-lookup-color
  input parameter self :: <GtkThemingEngine>;
  input parameter color_name_ :: <C-string>;
  output parameter color_ :: <GdkRGBA>;
  result res :: <C-boolean>;
  c-name: "gtk_theming_engine_lookup_color";
end;

define C-function gtk-theming-engine-state-is-running
  input parameter self :: <GtkThemingEngine>;
  input parameter state_ :: <GtkStateType>;
  output parameter progress_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gtk_theming_engine_state_is_running";
end;

define C-struct <_GtkThemingEngineClass>
  constant slot gtk-theming-engine-class-parent-class :: <GObjectClass>;
  constant slot gtk-theming-engine-class-render-line :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-background :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-frame :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-frame-gap :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-extension :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-check :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-option :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-arrow :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-expander :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-focus :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-layout :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-slider :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-handle :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-activity :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-render-icon-pixbuf :: <C-void*>;
  constant slot gtk-theming-engine-class-render-icon :: <C-function-pointer>;
  constant slot gtk-theming-engine-class-padding :: <C-void*>;
  pointer-type-name: <GtkThemingEngineClass>;
end C-struct;

define C-struct <_GtkThemingEnginePrivate>
  pointer-type-name: <GtkThemingEnginePrivate>;
end C-struct;

define open C-subtype <GtkToggleAction> (<GtkAction>)
  constant slot gtk-toggle-action-parent :: <GtkAction>;
  constant slot gtk-toggle-action-private-data :: <GtkToggleActionPrivate>;
end C-subtype;

define C-pointer-type <GtkToggleAction*> => <GtkToggleAction>;

define C-function gtk-toggle-action-new
  input parameter name_ :: <C-string>;
  input parameter label_ :: <C-string>;
  input parameter tooltip_ :: <C-string>;
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkToggleAction>;
  c-name: "gtk_toggle_action_new";
end;

define C-function gtk-toggle-action-get-active
  input parameter self :: <GtkToggleAction>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_action_get_active";
end;

define C-function gtk-toggle-action-get-draw-as-radio
  input parameter self :: <GtkToggleAction>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_action_get_draw_as_radio";
end;

define C-function gtk-toggle-action-set-active
  input parameter self :: <GtkToggleAction>;
  input parameter is_active_ :: <C-boolean>;
  c-name: "gtk_toggle_action_set_active";
end;

define C-function gtk-toggle-action-set-draw-as-radio
  input parameter self :: <GtkToggleAction>;
  input parameter draw_as_radio_ :: <C-boolean>;
  c-name: "gtk_toggle_action_set_draw_as_radio";
end;

define C-function gtk-toggle-action-toggled
  input parameter self :: <GtkToggleAction>;
  c-name: "gtk_toggle_action_toggled";
end;

define C-struct <_GtkToggleActionClass>
  constant slot gtk-toggle-action-class-parent-class :: <GtkActionClass>;
  constant slot gtk-toggle-action-class-toggled :: <C-function-pointer>;
  constant slot gtk-toggle-action-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-toggle-action-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-toggle-action-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-toggle-action-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToggleActionClass>;
end C-struct;

define C-struct <_GtkToggleActionEntry>
  slot gtk-toggle-action-entry-name :: <C-string>;
  slot gtk-toggle-action-entry-stock-id :: <C-string>;
  slot gtk-toggle-action-entry-label :: <C-string>;
  slot gtk-toggle-action-entry-accelerator :: <C-string>;
  slot gtk-toggle-action-entry-tooltip :: <C-string>;
  slot gtk-toggle-action-entry-callback :: <C-function-pointer>;
  slot gtk-toggle-action-entry-is-active :: <C-boolean>;
  pointer-type-name: <GtkToggleActionEntry>;
end C-struct;

define C-struct <_GtkToggleActionPrivate>
  pointer-type-name: <GtkToggleActionPrivate>;
end C-struct;

define open C-subtype <GtkToggleButton> (<GtkButton>)
  constant slot gtk-toggle-button-button :: <GtkButton>;
  constant slot gtk-toggle-button-priv :: <GtkToggleButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkToggleButton*> => <GtkToggleButton>;

define C-function gtk-toggle-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_toggle_button_new";
end;

define C-function gtk-toggle-button-new-with-label
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_toggle_button_new_with_label";
end;

define C-function gtk-toggle-button-new-with-mnemonic
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_toggle_button_new_with_mnemonic";
end;

define C-function gtk-toggle-button-get-active
  input parameter self :: <GtkToggleButton>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_button_get_active";
end;

define C-function gtk-toggle-button-get-inconsistent
  input parameter self :: <GtkToggleButton>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_button_get_inconsistent";
end;

define C-function gtk-toggle-button-get-mode
  input parameter self :: <GtkToggleButton>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_button_get_mode";
end;

define C-function gtk-toggle-button-set-active
  input parameter self :: <GtkToggleButton>;
  input parameter is_active_ :: <C-boolean>;
  c-name: "gtk_toggle_button_set_active";
end;

define C-function gtk-toggle-button-set-inconsistent
  input parameter self :: <GtkToggleButton>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_toggle_button_set_inconsistent";
end;

define C-function gtk-toggle-button-set-mode
  input parameter self :: <GtkToggleButton>;
  input parameter draw_indicator_ :: <C-boolean>;
  c-name: "gtk_toggle_button_set_mode";
end;

define C-function gtk-toggle-button-toggled
  input parameter self :: <GtkToggleButton>;
  c-name: "gtk_toggle_button_toggled";
end;

define C-struct <_GtkToggleButtonClass>
  constant slot gtk-toggle-button-class-parent-class :: <GtkButtonClass>;
  constant slot gtk-toggle-button-class-toggled :: <C-function-pointer>;
  constant slot gtk-toggle-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-toggle-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-toggle-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-toggle-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToggleButtonClass>;
end C-struct;

define C-struct <_GtkToggleButtonPrivate>
  pointer-type-name: <GtkToggleButtonPrivate>;
end C-struct;

define open C-subtype <GtkToggleToolButton> (<GtkToolButton>)
  constant slot gtk-toggle-tool-button-parent :: <GtkToolButton>;
  constant slot gtk-toggle-tool-button-priv :: <GtkToggleToolButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkToggleToolButton*> => <GtkToggleToolButton>;

define C-function gtk-toggle-tool-button-new
  result res :: <GtkToolItem>;
  c-name: "gtk_toggle_tool_button_new";
end;

define C-function gtk-toggle-tool-button-new-from-stock
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_toggle_tool_button_new_from_stock";
end;

define C-function gtk-toggle-tool-button-get-active
  input parameter self :: <GtkToggleToolButton>;
  result res :: <C-boolean>;
  c-name: "gtk_toggle_tool_button_get_active";
end;

define C-function gtk-toggle-tool-button-set-active
  input parameter self :: <GtkToggleToolButton>;
  input parameter is_active_ :: <C-boolean>;
  c-name: "gtk_toggle_tool_button_set_active";
end;

define C-struct <_GtkToggleToolButtonClass>
  constant slot gtk-toggle-tool-button-class-parent-class :: <GtkToolButtonClass>;
  constant slot gtk-toggle-tool-button-class-toggled :: <C-function-pointer>;
  constant slot gtk-toggle-tool-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-toggle-tool-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-toggle-tool-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-toggle-tool-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToggleToolButtonClass>;
end C-struct;

define C-struct <_GtkToggleToolButtonPrivate>
  pointer-type-name: <GtkToggleToolButtonPrivate>;
end C-struct;

define open C-subtype <GtkToolButton> (<GtkToolItem>)
  constant slot gtk-tool-button-parent :: <GtkToolItem>;
  constant slot gtk-tool-button-priv :: <GtkToolButtonPrivate>;
end C-subtype;

define C-pointer-type <GtkToolButton*> => <GtkToolButton>;

define C-function gtk-tool-button-new
  input parameter icon_widget_ :: <GtkWidget>;
  input parameter label_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_button_new";
end;

define C-function gtk-tool-button-new-from-stock
  input parameter stock_id_ :: <C-string>;
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_button_new_from_stock";
end;

define C-function gtk-tool-button-get-icon-name
  input parameter self :: <GtkToolButton>;
  result res :: <C-string>;
  c-name: "gtk_tool_button_get_icon_name";
end;

define C-function gtk-tool-button-get-icon-widget
  input parameter self :: <GtkToolButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_button_get_icon_widget";
end;

define C-function gtk-tool-button-get-label
  input parameter self :: <GtkToolButton>;
  result res :: <C-string>;
  c-name: "gtk_tool_button_get_label";
end;

define C-function gtk-tool-button-get-label-widget
  input parameter self :: <GtkToolButton>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_button_get_label_widget";
end;

define C-function gtk-tool-button-get-stock-id
  input parameter self :: <GtkToolButton>;
  result res :: <C-string>;
  c-name: "gtk_tool_button_get_stock_id";
end;

define C-function gtk-tool-button-get-use-underline
  input parameter self :: <GtkToolButton>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_button_get_use_underline";
end;

define C-function gtk-tool-button-set-icon-name
  input parameter self :: <GtkToolButton>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_tool_button_set_icon_name";
end;

define C-function gtk-tool-button-set-icon-widget
  input parameter self :: <GtkToolButton>;
  input parameter icon_widget_ :: <GtkWidget>;
  c-name: "gtk_tool_button_set_icon_widget";
end;

define C-function gtk-tool-button-set-label
  input parameter self :: <GtkToolButton>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_tool_button_set_label";
end;

define C-function gtk-tool-button-set-label-widget
  input parameter self :: <GtkToolButton>;
  input parameter label_widget_ :: <GtkWidget>;
  c-name: "gtk_tool_button_set_label_widget";
end;

define C-function gtk-tool-button-set-stock-id
  input parameter self :: <GtkToolButton>;
  input parameter stock_id_ :: <C-string>;
  c-name: "gtk_tool_button_set_stock_id";
end;

define C-function gtk-tool-button-set-use-underline
  input parameter self :: <GtkToolButton>;
  input parameter use_underline_ :: <C-boolean>;
  c-name: "gtk_tool_button_set_use_underline";
end;

define C-struct <_GtkToolButtonClass>
  constant slot gtk-tool-button-class-parent-class :: <GtkToolItemClass>;
  constant slot gtk-tool-button-class-button-type :: <C-long>;
  constant slot gtk-tool-button-class-clicked :: <C-function-pointer>;
  constant slot gtk-tool-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tool-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tool-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tool-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToolButtonClass>;
end C-struct;

define C-struct <_GtkToolButtonPrivate>
  pointer-type-name: <GtkToolButtonPrivate>;
end C-struct;

define open C-subtype <GtkToolItem> (<GtkBin>)
  constant slot gtk-tool-item-parent :: <GtkBin>;
  constant slot gtk-tool-item-priv :: <GtkToolItemPrivate>;
end C-subtype;

define C-pointer-type <GtkToolItem*> => <GtkToolItem>;

define C-function gtk-tool-item-new
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_item_new";
end;

define C-function gtk-tool-item-get-ellipsize-mode
  input parameter self :: <GtkToolItem>;
  result res :: <PangoEllipsizeMode>;
  c-name: "gtk_tool_item_get_ellipsize_mode";
end;

define C-function gtk-tool-item-get-expand
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_expand";
end;

define C-function gtk-tool-item-get-homogeneous
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_homogeneous";
end;

define C-function gtk-tool-item-get-icon-size
  input parameter self :: <GtkToolItem>;
  result res :: <C-signed-int>;
  c-name: "gtk_tool_item_get_icon_size";
end;

define C-function gtk-tool-item-get-is-important
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_is_important";
end;

define C-function gtk-tool-item-get-orientation
  input parameter self :: <GtkToolItem>;
  result res :: <GtkOrientation>;
  c-name: "gtk_tool_item_get_orientation";
end;

define C-function gtk-tool-item-get-proxy-menu-item
  input parameter self :: <GtkToolItem>;
  input parameter menu_item_id_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_item_get_proxy_menu_item";
end;

define C-function gtk-tool-item-get-relief-style
  input parameter self :: <GtkToolItem>;
  result res :: <GtkReliefStyle>;
  c-name: "gtk_tool_item_get_relief_style";
end;

define C-function gtk-tool-item-get-text-alignment
  input parameter self :: <GtkToolItem>;
  result res :: <C-float>;
  c-name: "gtk_tool_item_get_text_alignment";
end;

define C-function gtk-tool-item-get-text-orientation
  input parameter self :: <GtkToolItem>;
  result res :: <GtkOrientation>;
  c-name: "gtk_tool_item_get_text_orientation";
end;

define C-function gtk-tool-item-get-text-size-group
  input parameter self :: <GtkToolItem>;
  result res :: <GtkSizeGroup>;
  c-name: "gtk_tool_item_get_text_size_group";
end;

define C-function gtk-tool-item-get-toolbar-style
  input parameter self :: <GtkToolItem>;
  result res :: <GtkToolbarStyle>;
  c-name: "gtk_tool_item_get_toolbar_style";
end;

define C-function gtk-tool-item-get-use-drag-window
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_use_drag_window";
end;

define C-function gtk-tool-item-get-visible-horizontal
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_visible_horizontal";
end;

define C-function gtk-tool-item-get-visible-vertical
  input parameter self :: <GtkToolItem>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_get_visible_vertical";
end;

define C-function gtk-tool-item-rebuild-menu
  input parameter self :: <GtkToolItem>;
  c-name: "gtk_tool_item_rebuild_menu";
end;

define C-function gtk-tool-item-retrieve-proxy-menu-item
  input parameter self :: <GtkToolItem>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_item_retrieve_proxy_menu_item";
end;

define C-function gtk-tool-item-set-expand
  input parameter self :: <GtkToolItem>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_expand";
end;

define C-function gtk-tool-item-set-homogeneous
  input parameter self :: <GtkToolItem>;
  input parameter homogeneous_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_homogeneous";
end;

define C-function gtk-tool-item-set-is-important
  input parameter self :: <GtkToolItem>;
  input parameter is_important_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_is_important";
end;

define C-function gtk-tool-item-set-proxy-menu-item
  input parameter self :: <GtkToolItem>;
  input parameter menu_item_id_ :: <C-string>;
  input parameter menu_item_ :: <GtkWidget>;
  c-name: "gtk_tool_item_set_proxy_menu_item";
end;

define C-function gtk-tool-item-set-tooltip-markup
  input parameter self :: <GtkToolItem>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_tool_item_set_tooltip_markup";
end;

define C-function gtk-tool-item-set-tooltip-text
  input parameter self :: <GtkToolItem>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_tool_item_set_tooltip_text";
end;

define C-function gtk-tool-item-set-use-drag-window
  input parameter self :: <GtkToolItem>;
  input parameter use_drag_window_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_use_drag_window";
end;

define C-function gtk-tool-item-set-visible-horizontal
  input parameter self :: <GtkToolItem>;
  input parameter visible_horizontal_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_visible_horizontal";
end;

define C-function gtk-tool-item-set-visible-vertical
  input parameter self :: <GtkToolItem>;
  input parameter visible_vertical_ :: <C-boolean>;
  c-name: "gtk_tool_item_set_visible_vertical";
end;

define C-function gtk-tool-item-toolbar-reconfigured
  input parameter self :: <GtkToolItem>;
  c-name: "gtk_tool_item_toolbar_reconfigured";
end;

define C-struct <_GtkToolItemClass>
  constant slot gtk-tool-item-class-parent-class :: <GtkBinClass>;
  constant slot gtk-tool-item-class-create-menu-proxy :: <C-function-pointer>;
  constant slot gtk-tool-item-class-toolbar-reconfigured :: <C-function-pointer>;
  constant slot gtk-tool-item-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tool-item-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tool-item-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tool-item-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToolItemClass>;
end C-struct;

define open C-subtype <GtkToolItemGroup> (<GtkContainer>)
  constant slot gtk-tool-item-group-parent-instance :: <GtkContainer>;
  constant slot gtk-tool-item-group-priv :: <GtkToolItemGroupPrivate>;
end C-subtype;

define C-pointer-type <GtkToolItemGroup*> => <GtkToolItemGroup>;

define C-function gtk-tool-item-group-new
  input parameter label_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_item_group_new";
end;

define C-function gtk-tool-item-group-get-collapsed
  input parameter self :: <GtkToolItemGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_item_group_get_collapsed";
end;

define C-function gtk-tool-item-group-get-drop-item
  input parameter self :: <GtkToolItemGroup>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_item_group_get_drop_item";
end;

define C-function gtk-tool-item-group-get-ellipsize
  input parameter self :: <GtkToolItemGroup>;
  result res :: <PangoEllipsizeMode>;
  c-name: "gtk_tool_item_group_get_ellipsize";
end;

define C-function gtk-tool-item-group-get-header-relief
  input parameter self :: <GtkToolItemGroup>;
  result res :: <GtkReliefStyle>;
  c-name: "gtk_tool_item_group_get_header_relief";
end;

define C-function gtk-tool-item-group-get-item-position
  input parameter self :: <GtkToolItemGroup>;
  input parameter item_ :: <GtkToolItem>;
  result res :: <C-signed-int>;
  c-name: "gtk_tool_item_group_get_item_position";
end;

define C-function gtk-tool-item-group-get-label
  input parameter self :: <GtkToolItemGroup>;
  result res :: <C-string>;
  c-name: "gtk_tool_item_group_get_label";
end;

define C-function gtk-tool-item-group-get-label-widget
  input parameter self :: <GtkToolItemGroup>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_item_group_get_label_widget";
end;

define C-function gtk-tool-item-group-get-n-items
  input parameter self :: <GtkToolItemGroup>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_tool_item_group_get_n_items";
end;

define C-function gtk-tool-item-group-get-nth-item
  input parameter self :: <GtkToolItemGroup>;
  input parameter index_ :: <C-unsigned-int>;
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_item_group_get_nth_item";
end;

define C-function gtk-tool-item-group-insert
  input parameter self :: <GtkToolItemGroup>;
  input parameter item_ :: <GtkToolItem>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_tool_item_group_insert";
end;

define C-function gtk-tool-item-group-set-collapsed
  input parameter self :: <GtkToolItemGroup>;
  input parameter collapsed_ :: <C-boolean>;
  c-name: "gtk_tool_item_group_set_collapsed";
end;

define C-function gtk-tool-item-group-set-ellipsize
  input parameter self :: <GtkToolItemGroup>;
  input parameter ellipsize_ :: <PangoEllipsizeMode>;
  c-name: "gtk_tool_item_group_set_ellipsize";
end;

define C-function gtk-tool-item-group-set-header-relief
  input parameter self :: <GtkToolItemGroup>;
  input parameter style_ :: <GtkReliefStyle>;
  c-name: "gtk_tool_item_group_set_header_relief";
end;

define C-function gtk-tool-item-group-set-item-position
  input parameter self :: <GtkToolItemGroup>;
  input parameter item_ :: <GtkToolItem>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_tool_item_group_set_item_position";
end;

define C-function gtk-tool-item-group-set-label
  input parameter self :: <GtkToolItemGroup>;
  input parameter label_ :: <C-string>;
  c-name: "gtk_tool_item_group_set_label";
end;

define C-function gtk-tool-item-group-set-label-widget
  input parameter self :: <GtkToolItemGroup>;
  input parameter label_widget_ :: <GtkWidget>;
  c-name: "gtk_tool_item_group_set_label_widget";
end;

define C-struct <_GtkToolItemGroupClass>
  constant slot gtk-tool-item-group-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-tool-item-group-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tool-item-group-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tool-item-group-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tool-item-group-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToolItemGroupClass>;
end C-struct;

define C-struct <_GtkToolItemGroupPrivate>
  pointer-type-name: <GtkToolItemGroupPrivate>;
end C-struct;

define C-struct <_GtkToolItemPrivate>
  pointer-type-name: <GtkToolItemPrivate>;
end C-struct;

define open C-subtype <GtkToolPalette> (<GtkContainer>)
  constant slot gtk-tool-palette-parent-instance :: <GtkContainer>;
  constant slot gtk-tool-palette-priv :: <GtkToolPalettePrivate>;
end C-subtype;

define C-pointer-type <GtkToolPalette*> => <GtkToolPalette>;

define C-function gtk-tool-palette-new
  result res :: <GtkWidget>;
  c-name: "gtk_tool_palette_new";
end;

define C-function gtk-tool-palette-get-drag-target-group
  result res :: <GtkTargetEntry>;
  c-name: "gtk_tool_palette_get_drag_target_group";
end;

define C-function gtk-tool-palette-get-drag-target-item
  result res :: <GtkTargetEntry>;
  c-name: "gtk_tool_palette_get_drag_target_item";
end;

define C-function gtk-tool-palette-add-drag-dest
  input parameter self :: <GtkToolPalette>;
  input parameter widget_ :: <GtkWidget>;
  input parameter flags_ :: <GtkDestDefaults>;
  input parameter targets_ :: <GtkToolPaletteDragTargets>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_tool_palette_add_drag_dest";
end;

define C-function gtk-tool-palette-get-drag-item
  input parameter self :: <GtkToolPalette>;
  input parameter selection_ :: <GtkSelectionData>;
  result res :: <GtkWidget>;
  c-name: "gtk_tool_palette_get_drag_item";
end;

define C-function gtk-tool-palette-get-drop-group
  input parameter self :: <GtkToolPalette>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <GtkToolItemGroup>;
  c-name: "gtk_tool_palette_get_drop_group";
end;

define C-function gtk-tool-palette-get-drop-item
  input parameter self :: <GtkToolPalette>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <GtkToolItem>;
  c-name: "gtk_tool_palette_get_drop_item";
end;

define C-function gtk-tool-palette-get-exclusive
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_palette_get_exclusive";
end;

define C-function gtk-tool-palette-get-expand
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  result res :: <C-boolean>;
  c-name: "gtk_tool_palette_get_expand";
end;

define C-function gtk-tool-palette-get-group-position
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  result res :: <C-signed-int>;
  c-name: "gtk_tool_palette_get_group_position";
end;

define C-function gtk-tool-palette-get-hadjustment
  input parameter self :: <GtkToolPalette>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_tool_palette_get_hadjustment";
end;

define C-function gtk-tool-palette-get-icon-size
  input parameter self :: <GtkToolPalette>;
  result res :: <C-signed-int>;
  c-name: "gtk_tool_palette_get_icon_size";
end;

define C-function gtk-tool-palette-get-style
  input parameter self :: <GtkToolPalette>;
  result res :: <GtkToolbarStyle>;
  c-name: "gtk_tool_palette_get_style";
end;

define C-function gtk-tool-palette-get-vadjustment
  input parameter self :: <GtkToolPalette>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_tool_palette_get_vadjustment";
end;

define C-function gtk-tool-palette-set-drag-source
  input parameter self :: <GtkToolPalette>;
  input parameter targets_ :: <GtkToolPaletteDragTargets>;
  c-name: "gtk_tool_palette_set_drag_source";
end;

define C-function gtk-tool-palette-set-exclusive
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  input parameter exclusive_ :: <C-boolean>;
  c-name: "gtk_tool_palette_set_exclusive";
end;

define C-function gtk-tool-palette-set-expand
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tool_palette_set_expand";
end;

define C-function gtk-tool-palette-set-group-position
  input parameter self :: <GtkToolPalette>;
  input parameter group_ :: <GtkToolItemGroup>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_tool_palette_set_group_position";
end;

define C-function gtk-tool-palette-set-icon-size
  input parameter self :: <GtkToolPalette>;
  input parameter icon_size_ :: <C-signed-int>;
  c-name: "gtk_tool_palette_set_icon_size";
end;

define C-function gtk-tool-palette-set-style
  input parameter self :: <GtkToolPalette>;
  input parameter style_ :: <GtkToolbarStyle>;
  c-name: "gtk_tool_palette_set_style";
end;

define C-function gtk-tool-palette-unset-icon-size
  input parameter self :: <GtkToolPalette>;
  c-name: "gtk_tool_palette_unset_icon_size";
end;

define C-function gtk-tool-palette-unset-style
  input parameter self :: <GtkToolPalette>;
  c-name: "gtk_tool_palette_unset_style";
end;

define C-struct <_GtkToolPaletteClass>
  constant slot gtk-tool-palette-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-tool-palette-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tool-palette-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tool-palette-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tool-palette-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToolPaletteClass>;
end C-struct;

define constant $gtk-tool-palette-drag-items = 1;
define constant $gtk-tool-palette-drag-groups = 2;
define constant <GtkToolPaletteDragTargets> = <C-int>;
define C-pointer-type <GtkToolPaletteDragTargets*> => <GtkToolPaletteDragTargets>;

define C-struct <_GtkToolPalettePrivate>
  pointer-type-name: <GtkToolPalettePrivate>;
end C-struct;

// Interface
define open C-subtype <GtkToolShell> (<GtkWidget>)
end C-subtype;

define C-pointer-type <GtkToolShell*> => <GtkToolShell>;

define C-function gtk-tool-shell-get-ellipsize-mode
  input parameter self :: <GtkToolShell>;
  result res :: <PangoEllipsizeMode>;
  c-name: "gtk_tool_shell_get_ellipsize_mode";
end;

define C-function gtk-tool-shell-get-icon-size
  input parameter self :: <GtkToolShell>;
  result res :: <C-signed-int>;
  c-name: "gtk_tool_shell_get_icon_size";
end;

define C-function gtk-tool-shell-get-orientation
  input parameter self :: <GtkToolShell>;
  result res :: <GtkOrientation>;
  c-name: "gtk_tool_shell_get_orientation";
end;

define C-function gtk-tool-shell-get-relief-style
  input parameter self :: <GtkToolShell>;
  result res :: <GtkReliefStyle>;
  c-name: "gtk_tool_shell_get_relief_style";
end;

define C-function gtk-tool-shell-get-style
  input parameter self :: <GtkToolShell>;
  result res :: <GtkToolbarStyle>;
  c-name: "gtk_tool_shell_get_style";
end;

define C-function gtk-tool-shell-get-text-alignment
  input parameter self :: <GtkToolShell>;
  result res :: <C-float>;
  c-name: "gtk_tool_shell_get_text_alignment";
end;

define C-function gtk-tool-shell-get-text-orientation
  input parameter self :: <GtkToolShell>;
  result res :: <GtkOrientation>;
  c-name: "gtk_tool_shell_get_text_orientation";
end;

define C-function gtk-tool-shell-get-text-size-group
  input parameter self :: <GtkToolShell>;
  result res :: <GtkSizeGroup>;
  c-name: "gtk_tool_shell_get_text_size_group";
end;

define C-function gtk-tool-shell-rebuild-menu
  input parameter self :: <GtkToolShell>;
  c-name: "gtk_tool_shell_rebuild_menu";
end;

define C-struct <_GtkToolShellIface>
  constant slot gtk-tool-shell-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-tool-shell-iface-get-icon-size :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-orientation :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-style :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-relief-style :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-rebuild-menu :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-text-orientation :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-text-alignment :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-ellipsize-mode :: <C-function-pointer>;
  constant slot gtk-tool-shell-iface-get-text-size-group :: <C-function-pointer>;
  pointer-type-name: <GtkToolShellIface>;
end C-struct;

define open C-subtype <GtkToolbar> (<GtkContainer>)
  constant slot gtk-toolbar-container :: <GtkContainer>;
  constant slot gtk-toolbar-priv :: <GtkToolbarPrivate>;
end C-subtype;

define C-pointer-type <GtkToolbar*> => <GtkToolbar>;

define C-function gtk-toolbar-new
  result res :: <GtkWidget>;
  c-name: "gtk_toolbar_new";
end;

define C-function gtk-toolbar-get-drop-index
  input parameter self :: <GtkToolbar>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_toolbar_get_drop_index";
end;

define C-function gtk-toolbar-get-icon-size
  input parameter self :: <GtkToolbar>;
  result res :: <C-signed-int>;
  c-name: "gtk_toolbar_get_icon_size";
end;

define C-function gtk-toolbar-get-item-index
  input parameter self :: <GtkToolbar>;
  input parameter item_ :: <GtkToolItem>;
  result res :: <C-signed-int>;
  c-name: "gtk_toolbar_get_item_index";
end;

define C-function gtk-toolbar-get-n-items
  input parameter self :: <GtkToolbar>;
  result res :: <C-signed-int>;
  c-name: "gtk_toolbar_get_n_items";
end;

define C-function gtk-toolbar-get-nth-item
  input parameter self :: <GtkToolbar>;
  input parameter n_ :: <C-signed-int>;
  result res :: <GtkToolItem>;
  c-name: "gtk_toolbar_get_nth_item";
end;

define C-function gtk-toolbar-get-relief-style
  input parameter self :: <GtkToolbar>;
  result res :: <GtkReliefStyle>;
  c-name: "gtk_toolbar_get_relief_style";
end;

define C-function gtk-toolbar-get-show-arrow
  input parameter self :: <GtkToolbar>;
  result res :: <C-boolean>;
  c-name: "gtk_toolbar_get_show_arrow";
end;

define C-function gtk-toolbar-get-style
  input parameter self :: <GtkToolbar>;
  result res :: <GtkToolbarStyle>;
  c-name: "gtk_toolbar_get_style";
end;

define C-function gtk-toolbar-insert
  input parameter self :: <GtkToolbar>;
  input parameter item_ :: <GtkToolItem>;
  input parameter pos_ :: <C-signed-int>;
  c-name: "gtk_toolbar_insert";
end;

define C-function gtk-toolbar-set-drop-highlight-item
  input parameter self :: <GtkToolbar>;
  input parameter tool_item_ :: <GtkToolItem>;
  input parameter index__ :: <C-signed-int>;
  c-name: "gtk_toolbar_set_drop_highlight_item";
end;

define C-function gtk-toolbar-set-icon-size
  input parameter self :: <GtkToolbar>;
  input parameter icon_size_ :: <C-signed-int>;
  c-name: "gtk_toolbar_set_icon_size";
end;

define C-function gtk-toolbar-set-show-arrow
  input parameter self :: <GtkToolbar>;
  input parameter show_arrow_ :: <C-boolean>;
  c-name: "gtk_toolbar_set_show_arrow";
end;

define C-function gtk-toolbar-set-style
  input parameter self :: <GtkToolbar>;
  input parameter style_ :: <GtkToolbarStyle>;
  c-name: "gtk_toolbar_set_style";
end;

define C-function gtk-toolbar-unset-icon-size
  input parameter self :: <GtkToolbar>;
  c-name: "gtk_toolbar_unset_icon_size";
end;

define C-function gtk-toolbar-unset-style
  input parameter self :: <GtkToolbar>;
  c-name: "gtk_toolbar_unset_style";
end;

define C-struct <_GtkToolbarClass>
  constant slot gtk-toolbar-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-toolbar-class-orientation-changed :: <C-function-pointer>;
  constant slot gtk-toolbar-class-style-changed :: <C-function-pointer>;
  constant slot gtk-toolbar-class-popup-context-menu :: <C-function-pointer>;
  constant slot gtk-toolbar-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-toolbar-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-toolbar-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-toolbar-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkToolbarClass>;
end C-struct;

define C-struct <_GtkToolbarPrivate>
  pointer-type-name: <GtkToolbarPrivate>;
end C-struct;

define constant $gtk-toolbar-space-empty = 0;
define constant $gtk-toolbar-space-line = 1;
define constant <GtkToolbarSpaceStyle> = <C-int>;
define C-pointer-type <GtkToolbarSpaceStyle*> => <GtkToolbarSpaceStyle>;

define constant $gtk-toolbar-icons = 0;
define constant $gtk-toolbar-text = 1;
define constant $gtk-toolbar-both = 2;
define constant $gtk-toolbar-both-horiz = 3;
define constant <GtkToolbarStyle> = <C-int>;
define C-pointer-type <GtkToolbarStyle*> => <GtkToolbarStyle>;

define open C-subtype <GtkTooltip> (<GObject>)
end C-subtype;

define C-pointer-type <GtkTooltip*> => <GtkTooltip>;

define C-function gtk-tooltip-trigger-tooltip-query
  input parameter display_ :: <GdkDisplay>;
  c-name: "gtk_tooltip_trigger_tooltip_query";
end;

define C-function gtk-tooltip-set-custom
  input parameter self :: <GtkTooltip>;
  input parameter custom_widget_ :: <GtkWidget>;
  c-name: "gtk_tooltip_set_custom";
end;

define C-function gtk-tooltip-set-icon
  input parameter self :: <GtkTooltip>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_tooltip_set_icon";
end;

define C-function gtk-tooltip-set-icon-from-gicon
  input parameter self :: <GtkTooltip>;
  input parameter gicon_ :: <GIcon>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_tooltip_set_icon_from_gicon";
end;

define C-function gtk-tooltip-set-icon-from-icon-name
  input parameter self :: <GtkTooltip>;
  input parameter icon_name_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_tooltip_set_icon_from_icon_name";
end;

define C-function gtk-tooltip-set-icon-from-stock
  input parameter self :: <GtkTooltip>;
  input parameter stock_id_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  c-name: "gtk_tooltip_set_icon_from_stock";
end;

define C-function gtk-tooltip-set-markup
  input parameter self :: <GtkTooltip>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_tooltip_set_markup";
end;

define C-function gtk-tooltip-set-text
  input parameter self :: <GtkTooltip>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_tooltip_set_text";
end;

define C-function gtk-tooltip-set-tip-area
  input parameter self :: <GtkTooltip>;
  input parameter rect_ :: <cairoRectangleInt>;
  c-name: "gtk_tooltip_set_tip_area";
end;

// Interface
define open C-subtype <GtkTreeDragDest> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkTreeDragDest*> => <GtkTreeDragDest>;

define C-function gtk-tree-drag-dest-drag-data-received
  input parameter self :: <GtkTreeDragDest>;
  input parameter dest_ :: <GtkTreePath>;
  input parameter selection_data_ :: <GtkSelectionData>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_drag_dest_drag_data_received";
end;

define C-function gtk-tree-drag-dest-row-drop-possible
  input parameter self :: <GtkTreeDragDest>;
  input parameter dest_path_ :: <GtkTreePath>;
  input parameter selection_data_ :: <GtkSelectionData>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_drag_dest_row_drop_possible";
end;

define C-struct <_GtkTreeDragDestIface>
  constant slot gtk-tree-drag-dest-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-tree-drag-dest-iface-drag-data-received :: <C-function-pointer>;
  constant slot gtk-tree-drag-dest-iface-row-drop-possible :: <C-function-pointer>;
  pointer-type-name: <GtkTreeDragDestIface>;
end C-struct;

// Interface
define open C-subtype <GtkTreeDragSource> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkTreeDragSource*> => <GtkTreeDragSource>;

define C-function gtk-tree-drag-source-drag-data-delete
  input parameter self :: <GtkTreeDragSource>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_drag_source_drag_data_delete";
end;

define C-function gtk-tree-drag-source-drag-data-get
  input parameter self :: <GtkTreeDragSource>;
  input parameter path_ :: <GtkTreePath>;
  output parameter selection_data_ :: <GtkSelectionData>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_drag_source_drag_data_get";
end;

define C-function gtk-tree-drag-source-row-draggable
  input parameter self :: <GtkTreeDragSource>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_drag_source_row_draggable";
end;

define C-struct <_GtkTreeDragSourceIface>
  constant slot gtk-tree-drag-source-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-tree-drag-source-iface-row-draggable :: <C-function-pointer>;
  constant slot gtk-tree-drag-source-iface-drag-data-get :: <C-function-pointer>;
  constant slot gtk-tree-drag-source-iface-drag-data-delete :: <C-function-pointer>;
  pointer-type-name: <GtkTreeDragSourceIface>;
end C-struct;

define C-struct <_GtkTreeIter>
  slot gtk-tree-iter-stamp :: <C-signed-int>;
  slot gtk-tree-iter-user-data :: <C-void*>;
  slot gtk-tree-iter-user-data2 :: <C-void*>;
  slot gtk-tree-iter-user-data3 :: <C-void*>;
  pointer-type-name: <GtkTreeIter>;
end C-struct;

define C-function gtk-tree-iter-copy
  input parameter self :: <GtkTreeIter>;
  result res :: <GtkTreeIter>;
  c-name: "gtk_tree_iter_copy";
end;

define C-function gtk-tree-iter-free
  input parameter self :: <GtkTreeIter>;
  c-name: "gtk_tree_iter_free";
end;

// Interface
define open C-subtype <GtkTreeModel> (<C-void*>)
end C-subtype;

define C-pointer-type <GtkTreeModel*> => <GtkTreeModel>;

define C-function gtk-tree-model-filter-new
  input parameter self :: <GtkTreeModel>;
  input parameter root_ :: <GtkTreePath>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_model_filter_new";
end;

define C-function gtk-tree-model-foreach
  input parameter self :: <GtkTreeModel>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gtk_tree_model_foreach";
end;

define C-function gtk-tree-model-get-column-type
  input parameter self :: <GtkTreeModel>;
  input parameter index__ :: <C-signed-int>;
  result res :: <C-long>;
  c-name: "gtk_tree_model_get_column_type";
end;

define C-function gtk-tree-model-get-flags
  input parameter self :: <GtkTreeModel>;
  result res :: <GtkTreeModelFlags>;
  c-name: "gtk_tree_model_get_flags";
end;

define C-function gtk-tree-model-get-iter
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_get_iter";
end;

define C-function gtk-tree-model-get-iter-first
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_get_iter_first";
end;

define C-function gtk-tree-model-get-iter-from-string
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter path_string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_get_iter_from_string";
end;

define C-function gtk-tree-model-get-n-columns
  input parameter self :: <GtkTreeModel>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_model_get_n_columns";
end;

define C-function gtk-tree-model-get-path
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_model_get_path";
end;

define C-function gtk-tree-model-get-string-from-iter
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-string>;
  c-name: "gtk_tree_model_get_string_from_iter";
end;

define C-function gtk-tree-model-get-value
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter column_ :: <C-signed-int>;
  output parameter value_ :: <GValue>;
  c-name: "gtk_tree_model_get_value";
end;

define C-function gtk-tree-model-iter-children
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_children";
end;

define C-function gtk-tree-model-iter-has-child
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_has_child";
end;

define C-function gtk-tree-model-iter-n-children
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_model_iter_n_children";
end;

define C-function gtk-tree-model-iter-next
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_next";
end;

define C-function gtk-tree-model-iter-nth-child
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  input parameter n_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_nth_child";
end;

define C-function gtk-tree-model-iter-parent
  input parameter self :: <GtkTreeModel>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter child_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_parent";
end;

define C-function gtk-tree-model-iter-previous
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_iter_previous";
end;

define C-function gtk-tree-model-ref-node
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_ref_node";
end;

define C-function gtk-tree-model-row-changed
  input parameter self :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_row_changed";
end;

define C-function gtk-tree-model-row-deleted
  input parameter self :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_model_row_deleted";
end;

define C-function gtk-tree-model-row-has-child-toggled
  input parameter self :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_row_has_child_toggled";
end;

define C-function gtk-tree-model-row-inserted
  input parameter self :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_row_inserted";
end;

define C-function gtk-tree-model-sort-new-with-model
  input parameter self :: <GtkTreeModel>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_model_sort_new_with_model";
end;

define C-function gtk-tree-model-unref-node
  input parameter self :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_unref_node";
end;

define open C-subtype <GtkTreeModelFilter> (<GObject>)
  constant slot gtk-tree-model-filter-parent :: <GObject>;
  constant slot gtk-tree-model-filter-priv :: <GtkTreeModelFilterPrivate>;
end C-subtype;

define C-pointer-type <GtkTreeModelFilter*> => <GtkTreeModelFilter>;

define C-function gtk-tree-model-filter-clear-cache
  input parameter self :: <GtkTreeModelFilter>;
  c-name: "gtk_tree_model_filter_clear_cache";
end;

define C-function gtk-tree-model-filter-convert-child-iter-to-iter
  input parameter self :: <GtkTreeModelFilter>;
  output parameter filter_iter_ :: <GtkTreeIter>;
  input parameter child_iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_filter_convert_child_iter_to_iter";
end;

define C-function gtk-tree-model-filter-convert-child-path-to-path
  input parameter self :: <GtkTreeModelFilter>;
  input parameter child_path_ :: <GtkTreePath>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_model_filter_convert_child_path_to_path";
end;

define C-function gtk-tree-model-filter-convert-iter-to-child-iter
  input parameter self :: <GtkTreeModelFilter>;
  output parameter child_iter_ :: <GtkTreeIter>;
  input parameter filter_iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_filter_convert_iter_to_child_iter";
end;

define C-function gtk-tree-model-filter-convert-path-to-child-path
  input parameter self :: <GtkTreeModelFilter>;
  input parameter filter_path_ :: <GtkTreePath>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_model_filter_convert_path_to_child_path";
end;

define C-function gtk-tree-model-filter-get-model
  input parameter self :: <GtkTreeModelFilter>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_model_filter_get_model";
end;

define C-function gtk-tree-model-filter-refilter
  input parameter self :: <GtkTreeModelFilter>;
  c-name: "gtk_tree_model_filter_refilter";
end;

define C-function gtk-tree-model-filter-set-modify-func
  input parameter self :: <GtkTreeModelFilter>;
  input parameter n_columns_ :: <C-signed-int>;
  input parameter types_ :: <C-long*>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_model_filter_set_modify_func";
end;

define C-function gtk-tree-model-filter-set-visible-column
  input parameter self :: <GtkTreeModelFilter>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_tree_model_filter_set_visible_column";
end;

define C-function gtk-tree-model-filter-set-visible-func
  input parameter self :: <GtkTreeModelFilter>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_model_filter_set_visible_func";
end;

define C-struct <_GtkTreeModelFilterClass>
  constant slot gtk-tree-model-filter-class-parent-class :: <GObjectClass>;
  constant slot gtk-tree-model-filter-class-visible :: <C-function-pointer>;
  constant slot gtk-tree-model-filter-class-modify :: <C-function-pointer>;
  constant slot gtk-tree-model-filter-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-model-filter-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-model-filter-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-model-filter-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTreeModelFilterClass>;
end C-struct;

define C-struct <_GtkTreeModelFilterPrivate>
  pointer-type-name: <GtkTreeModelFilterPrivate>;
end C-struct;

define constant $gtk-tree-model-iters-persist = 1;
define constant $gtk-tree-model-list-only = 2;
define constant <GtkTreeModelFlags> = <C-int>;
define C-pointer-type <GtkTreeModelFlags*> => <GtkTreeModelFlags>;

define C-struct <_GtkTreeModelIface>
  constant slot gtk-tree-model-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-tree-model-iface-row-changed :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-row-inserted :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-row-has-child-toggled :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-row-deleted :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-rows-reordered :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-flags :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-n-columns :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-column-type :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-iter :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-path :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-get-value :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-next :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-previous :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-children :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-has-child :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-n-children :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-nth-child :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-iter-parent :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-ref-node :: <C-function-pointer>;
  constant slot gtk-tree-model-iface-unref-node :: <C-function-pointer>;
  pointer-type-name: <GtkTreeModelIface>;
end C-struct;

define open C-subtype <GtkTreeModelSort> (<GObject>)
  constant slot gtk-tree-model-sort-parent :: <GObject>;
  constant slot gtk-tree-model-sort-priv :: <GtkTreeModelSortPrivate>;
end C-subtype;

define C-pointer-type <GtkTreeModelSort*> => <GtkTreeModelSort>;

define C-function gtk-tree-model-sort-clear-cache
  input parameter self :: <GtkTreeModelSort>;
  c-name: "gtk_tree_model_sort_clear_cache";
end;

define C-function gtk-tree-model-sort-convert-child-iter-to-iter
  input parameter self :: <GtkTreeModelSort>;
  output parameter sort_iter_ :: <GtkTreeIter>;
  input parameter child_iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_sort_convert_child_iter_to_iter";
end;

define C-function gtk-tree-model-sort-convert-child-path-to-path
  input parameter self :: <GtkTreeModelSort>;
  input parameter child_path_ :: <GtkTreePath>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_model_sort_convert_child_path_to_path";
end;

define C-function gtk-tree-model-sort-convert-iter-to-child-iter
  input parameter self :: <GtkTreeModelSort>;
  output parameter child_iter_ :: <GtkTreeIter>;
  input parameter sorted_iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_model_sort_convert_iter_to_child_iter";
end;

define C-function gtk-tree-model-sort-convert-path-to-child-path
  input parameter self :: <GtkTreeModelSort>;
  input parameter sorted_path_ :: <GtkTreePath>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_model_sort_convert_path_to_child_path";
end;

define C-function gtk-tree-model-sort-get-model
  input parameter self :: <GtkTreeModelSort>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_model_sort_get_model";
end;

define C-function gtk-tree-model-sort-iter-is-valid
  input parameter self :: <GtkTreeModelSort>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_model_sort_iter_is_valid";
end;

define C-function gtk-tree-model-sort-reset-default-sort-func
  input parameter self :: <GtkTreeModelSort>;
  c-name: "gtk_tree_model_sort_reset_default_sort_func";
end;

define C-struct <_GtkTreeModelSortClass>
  constant slot gtk-tree-model-sort-class-parent-class :: <GObjectClass>;
  constant slot gtk-tree-model-sort-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-model-sort-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-model-sort-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-model-sort-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTreeModelSortClass>;
end C-struct;

define C-struct <_GtkTreeModelSortPrivate>
  pointer-type-name: <GtkTreeModelSortPrivate>;
end C-struct;

define C-struct <_GtkTreePath>
  pointer-type-name: <GtkTreePath>;
end C-struct;

define C-function gtk-tree-path-new
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_path_new";
end;

define C-function gtk-tree-path-new-first
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_path_new_first";
end;

define C-function gtk-tree-path-new-from-string
  input parameter path_ :: <C-string>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_path_new_from_string";
end;

define C-function gtk-tree-path-append-index
  input parameter self :: <GtkTreePath>;
  input parameter index__ :: <C-signed-int>;
  c-name: "gtk_tree_path_append_index";
end;

define C-function gtk-tree-path-compare
  input parameter self :: <GtkTreePath>;
  input parameter b_ :: <GtkTreePath>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_path_compare";
end;

define C-function gtk-tree-path-copy
  input parameter self :: <GtkTreePath>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_path_copy";
end;

define C-function gtk-tree-path-down
  input parameter self :: <GtkTreePath>;
  c-name: "gtk_tree_path_down";
end;

define C-function gtk-tree-path-free
  input parameter self :: <GtkTreePath>;
  c-name: "gtk_tree_path_free";
end;

define C-function gtk-tree-path-get-depth
  input parameter self :: <GtkTreePath>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_path_get_depth";
end;

define C-function gtk-tree-path-get-indices-with-depth
  input parameter self :: <GtkTreePath>;
  output parameter depth_ :: <C-signed-int*>;
  result res :: <C-signed-int*>;
  c-name: "gtk_tree_path_get_indices_with_depth";
end;

define C-function gtk-tree-path-is-ancestor
  input parameter self :: <GtkTreePath>;
  input parameter descendant_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_path_is_ancestor";
end;

define C-function gtk-tree-path-is-descendant
  input parameter self :: <GtkTreePath>;
  input parameter ancestor_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_path_is_descendant";
end;

define C-function gtk-tree-path-next
  input parameter self :: <GtkTreePath>;
  c-name: "gtk_tree_path_next";
end;

define C-function gtk-tree-path-prepend-index
  input parameter self :: <GtkTreePath>;
  input parameter index__ :: <C-signed-int>;
  c-name: "gtk_tree_path_prepend_index";
end;

define C-function gtk-tree-path-prev
  input parameter self :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_path_prev";
end;

define C-function gtk-tree-path-to-string
  input parameter self :: <GtkTreePath>;
  result res :: <C-string>;
  c-name: "gtk_tree_path_to_string";
end;

define C-function gtk-tree-path-up
  input parameter self :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_path_up";
end;

define C-struct <_GtkTreeRowReference>
  pointer-type-name: <GtkTreeRowReference>;
end C-struct;

define C-function gtk-tree-row-reference-new
  input parameter model_ :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <GtkTreeRowReference>;
  c-name: "gtk_tree_row_reference_new";
end;

define C-function gtk-tree-row-reference-new-proxy
  input parameter proxy_ :: <GObject>;
  input parameter model_ :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <GtkTreeRowReference>;
  c-name: "gtk_tree_row_reference_new_proxy";
end;

define C-function gtk-tree-row-reference-copy
  input parameter self :: <GtkTreeRowReference>;
  result res :: <GtkTreeRowReference>;
  c-name: "gtk_tree_row_reference_copy";
end;

define C-function gtk-tree-row-reference-free
  input parameter self :: <GtkTreeRowReference>;
  c-name: "gtk_tree_row_reference_free";
end;

define C-function gtk-tree-row-reference-get-model
  input parameter self :: <GtkTreeRowReference>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_row_reference_get_model";
end;

define C-function gtk-tree-row-reference-get-path
  input parameter self :: <GtkTreeRowReference>;
  result res :: <GtkTreePath>;
  c-name: "gtk_tree_row_reference_get_path";
end;

define C-function gtk-tree-row-reference-valid
  input parameter self :: <GtkTreeRowReference>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_row_reference_valid";
end;

define C-function gtk-tree-row-reference-deleted
  input parameter proxy_ :: <GObject>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_row_reference_deleted";
end;

define C-function gtk-tree-row-reference-inserted
  input parameter proxy_ :: <GObject>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_row_reference_inserted";
end;

define open C-subtype <GtkTreeSelection> (<GObject>)
  constant slot gtk-tree-selection-parent :: <GObject>;
  constant slot gtk-tree-selection-priv :: <GtkTreeSelectionPrivate>;
end C-subtype;

define C-pointer-type <GtkTreeSelection*> => <GtkTreeSelection>;

define C-function gtk-tree-selection-count-selected-rows
  input parameter self :: <GtkTreeSelection>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_selection_count_selected_rows";
end;

define C-function gtk-tree-selection-get-mode
  input parameter self :: <GtkTreeSelection>;
  result res :: <GtkSelectionMode>;
  c-name: "gtk_tree_selection_get_mode";
end;

define C-function gtk-tree-selection-get-selected
  input parameter self :: <GtkTreeSelection>;
  output parameter model_ :: <GtkTreeModel*>;
  output parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_selection_get_selected";
end;

define C-function gtk-tree-selection-get-selected-rows
  input parameter self :: <GtkTreeSelection>;
  output parameter model_ :: <GtkTreeModel*>;
  result res :: <GList>;
  c-name: "gtk_tree_selection_get_selected_rows";
end;

define C-function gtk-tree-selection-get-tree-view
  input parameter self :: <GtkTreeSelection>;
  result res :: <GtkTreeView>;
  c-name: "gtk_tree_selection_get_tree_view";
end;

define C-function gtk-tree-selection-iter-is-selected
  input parameter self :: <GtkTreeSelection>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_selection_iter_is_selected";
end;

define C-function gtk-tree-selection-path-is-selected
  input parameter self :: <GtkTreeSelection>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_selection_path_is_selected";
end;

define C-function gtk-tree-selection-select-all
  input parameter self :: <GtkTreeSelection>;
  c-name: "gtk_tree_selection_select_all";
end;

define C-function gtk-tree-selection-select-iter
  input parameter self :: <GtkTreeSelection>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_selection_select_iter";
end;

define C-function gtk-tree-selection-select-path
  input parameter self :: <GtkTreeSelection>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_selection_select_path";
end;

define C-function gtk-tree-selection-select-range
  input parameter self :: <GtkTreeSelection>;
  input parameter start_path_ :: <GtkTreePath>;
  input parameter end_path_ :: <GtkTreePath>;
  c-name: "gtk_tree_selection_select_range";
end;

define C-function gtk-tree-selection-selected-foreach
  input parameter self :: <GtkTreeSelection>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_tree_selection_selected_foreach";
end;

define C-function gtk-tree-selection-set-mode
  input parameter self :: <GtkTreeSelection>;
  input parameter type_ :: <GtkSelectionMode>;
  c-name: "gtk_tree_selection_set_mode";
end;

define C-function gtk-tree-selection-set-select-function
  input parameter self :: <GtkTreeSelection>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_selection_set_select_function";
end;

define C-function gtk-tree-selection-unselect-all
  input parameter self :: <GtkTreeSelection>;
  c-name: "gtk_tree_selection_unselect_all";
end;

define C-function gtk-tree-selection-unselect-iter
  input parameter self :: <GtkTreeSelection>;
  input parameter iter_ :: <GtkTreeIter>;
  c-name: "gtk_tree_selection_unselect_iter";
end;

define C-function gtk-tree-selection-unselect-path
  input parameter self :: <GtkTreeSelection>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_selection_unselect_path";
end;

define C-function gtk-tree-selection-unselect-range
  input parameter self :: <GtkTreeSelection>;
  input parameter start_path_ :: <GtkTreePath>;
  input parameter end_path_ :: <GtkTreePath>;
  c-name: "gtk_tree_selection_unselect_range";
end;

define C-struct <_GtkTreeSelectionClass>
  constant slot gtk-tree-selection-class-parent-class :: <GObjectClass>;
  constant slot gtk-tree-selection-class-changed :: <C-function-pointer>;
  constant slot gtk-tree-selection-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-selection-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-selection-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-selection-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTreeSelectionClass>;
end C-struct;

define C-struct <_GtkTreeSelectionPrivate>
  pointer-type-name: <GtkTreeSelectionPrivate>;
end C-struct;

// Interface
define open C-subtype <GtkTreeSortable> (<GtkTreeModel>)
end C-subtype;

define C-pointer-type <GtkTreeSortable*> => <GtkTreeSortable>;

define C-function gtk-tree-sortable-get-sort-column-id
  input parameter self :: <GtkTreeSortable>;
  output parameter sort_column_id_ :: <C-signed-int*>;
  output parameter order_ :: <GtkSortType*>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_sortable_get_sort_column_id";
end;

define C-function gtk-tree-sortable-has-default-sort-func
  input parameter self :: <GtkTreeSortable>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_sortable_has_default_sort_func";
end;

define C-function gtk-tree-sortable-set-default-sort-func
  input parameter self :: <GtkTreeSortable>;
  input parameter sort_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_sortable_set_default_sort_func";
end;

define C-function gtk-tree-sortable-set-sort-column-id
  input parameter self :: <GtkTreeSortable>;
  input parameter sort_column_id_ :: <C-signed-int>;
  input parameter order_ :: <GtkSortType>;
  c-name: "gtk_tree_sortable_set_sort_column_id";
end;

define C-function gtk-tree-sortable-set-sort-func
  input parameter self :: <GtkTreeSortable>;
  input parameter sort_column_id_ :: <C-signed-int>;
  input parameter sort_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_sortable_set_sort_func";
end;

define C-function gtk-tree-sortable-sort-column-changed
  input parameter self :: <GtkTreeSortable>;
  c-name: "gtk_tree_sortable_sort_column_changed";
end;

define C-struct <_GtkTreeSortableIface>
  constant slot gtk-tree-sortable-iface-g-iface :: <GTypeInterface>;
  constant slot gtk-tree-sortable-iface-sort-column-changed :: <C-function-pointer>;
  constant slot gtk-tree-sortable-iface-get-sort-column-id :: <C-function-pointer>;
  constant slot gtk-tree-sortable-iface-set-sort-column-id :: <C-function-pointer>;
  constant slot gtk-tree-sortable-iface-set-sort-func :: <C-function-pointer>;
  constant slot gtk-tree-sortable-iface-set-default-sort-func :: <C-function-pointer>;
  constant slot gtk-tree-sortable-iface-has-default-sort-func :: <C-function-pointer>;
  pointer-type-name: <GtkTreeSortableIface>;
end C-struct;

define open C-subtype <GtkTreeStore> (<GObject>)
  constant slot gtk-tree-store-parent :: <GObject>;
  constant slot gtk-tree-store-priv :: <GtkTreeStorePrivate>;
end C-subtype;

define C-pointer-type <GtkTreeStore*> => <GtkTreeStore>;

define C-function gtk-tree-store-newv
  input parameter n_columns_ :: <C-signed-int>;
  input parameter types_ :: <C-long*>;
  result res :: <GtkTreeStore>;
  c-name: "gtk_tree_store_newv";
end;

define C-function gtk-tree-store-append
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_append";
end;

define C-function gtk-tree-store-clear
  input parameter self :: <GtkTreeStore>;
  c-name: "gtk_tree_store_clear";
end;

define C-function gtk-tree-store-insert
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  input parameter position_ :: <C-signed-int>;
  c-name: "gtk_tree_store_insert";
end;

define C-function gtk-tree-store-insert-after
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  input parameter sibling_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_insert_after";
end;

define C-function gtk-tree-store-insert-before
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  input parameter sibling_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_insert_before";
end;

define C-function gtk-tree-store-insert-with-valuesv
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  input parameter position_ :: <C-signed-int>;
  input parameter columns_ :: <C-signed-int*>;
  input parameter values_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_values_ :: <C-signed-int>;
  c-name: "gtk_tree_store_insert_with_valuesv";
end;

define C-function gtk-tree-store-is-ancestor
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter descendant_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_store_is_ancestor";
end;

define C-function gtk-tree-store-iter-depth
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_store_iter_depth";
end;

define C-function gtk-tree-store-iter-is-valid
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_store_iter_is_valid";
end;

define C-function gtk-tree-store-move-after
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_move_after";
end;

define C-function gtk-tree-store-move-before
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter position_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_move_before";
end;

define C-function gtk-tree-store-prepend
  input parameter self :: <GtkTreeStore>;
  output parameter iter_ :: <GtkTreeIter>;
  input parameter parent_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_prepend";
end;

define C-function gtk-tree-store-remove
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_store_remove";
end;

define C-function gtk-tree-store-set-column-types
  input parameter self :: <GtkTreeStore>;
  input parameter n_columns_ :: <C-signed-int>;
  input parameter types_ :: <C-long*>;
  c-name: "gtk_tree_store_set_column_types";
end;

define C-function gtk-tree-store-set-value
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter column_ :: <C-signed-int>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_tree_store_set_value";
end;

define C-function gtk-tree-store-set-valuesv
  input parameter self :: <GtkTreeStore>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter columns_ :: <C-signed-int*>;
  input parameter values_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_values_ :: <C-signed-int>;
  c-name: "gtk_tree_store_set_valuesv";
end;

define C-function gtk-tree-store-swap
  input parameter self :: <GtkTreeStore>;
  input parameter a_ :: <GtkTreeIter>;
  input parameter b_ :: <GtkTreeIter>;
  c-name: "gtk_tree_store_swap";
end;

define C-struct <_GtkTreeStoreClass>
  constant slot gtk-tree-store-class-parent-class :: <GObjectClass>;
  constant slot gtk-tree-store-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-store-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-store-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-store-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTreeStoreClass>;
end C-struct;

define C-struct <_GtkTreeStorePrivate>
  pointer-type-name: <GtkTreeStorePrivate>;
end C-struct;

define open C-subtype <GtkTreeView> (<GtkContainer>)
  constant slot gtk-tree-view-parent :: <GtkContainer>;
  constant slot gtk-tree-view-priv :: <GtkTreeViewPrivate>;
end C-subtype;

define C-pointer-type <GtkTreeView*> => <GtkTreeView>;

define C-function gtk-tree-view-new
  result res :: <GtkWidget>;
  c-name: "gtk_tree_view_new";
end;

define C-function gtk-tree-view-new-with-model
  input parameter model_ :: <GtkTreeModel>;
  result res :: <GtkWidget>;
  c-name: "gtk_tree_view_new_with_model";
end;

define C-function gtk-tree-view-append-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_append_column";
end;

define C-function gtk-tree-view-collapse-all
  input parameter self :: <GtkTreeView>;
  c-name: "gtk_tree_view_collapse_all";
end;

define C-function gtk-tree-view-collapse-row
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_collapse_row";
end;

define C-function gtk-tree-view-columns-autosize
  input parameter self :: <GtkTreeView>;
  c-name: "gtk_tree_view_columns_autosize";
end;

define C-function gtk-tree-view-convert-bin-window-to-tree-coords
  input parameter self :: <GtkTreeView>;
  input parameter bx_ :: <C-signed-int>;
  input parameter by_ :: <C-signed-int>;
  output parameter tx_ :: <C-signed-int*>;
  output parameter ty_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_bin_window_to_tree_coords";
end;

define C-function gtk-tree-view-convert-bin-window-to-widget-coords
  input parameter self :: <GtkTreeView>;
  input parameter bx_ :: <C-signed-int>;
  input parameter by_ :: <C-signed-int>;
  output parameter wx_ :: <C-signed-int*>;
  output parameter wy_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_bin_window_to_widget_coords";
end;

define C-function gtk-tree-view-convert-tree-to-bin-window-coords
  input parameter self :: <GtkTreeView>;
  input parameter tx_ :: <C-signed-int>;
  input parameter ty_ :: <C-signed-int>;
  output parameter bx_ :: <C-signed-int*>;
  output parameter by_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_tree_to_bin_window_coords";
end;

define C-function gtk-tree-view-convert-tree-to-widget-coords
  input parameter self :: <GtkTreeView>;
  input parameter tx_ :: <C-signed-int>;
  input parameter ty_ :: <C-signed-int>;
  output parameter wx_ :: <C-signed-int*>;
  output parameter wy_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_tree_to_widget_coords";
end;

define C-function gtk-tree-view-convert-widget-to-bin-window-coords
  input parameter self :: <GtkTreeView>;
  input parameter wx_ :: <C-signed-int>;
  input parameter wy_ :: <C-signed-int>;
  output parameter bx_ :: <C-signed-int*>;
  output parameter by_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_widget_to_bin_window_coords";
end;

define C-function gtk-tree-view-convert-widget-to-tree-coords
  input parameter self :: <GtkTreeView>;
  input parameter wx_ :: <C-signed-int>;
  input parameter wy_ :: <C-signed-int>;
  output parameter tx_ :: <C-signed-int*>;
  output parameter ty_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_convert_widget_to_tree_coords";
end;

define C-function gtk-tree-view-create-row-drag-icon
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <cairoSurface>;
  c-name: "gtk_tree_view_create_row_drag_icon";
end;

define C-function gtk-tree-view-enable-model-drag-dest
  input parameter self :: <GtkTreeView>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_tree_view_enable_model_drag_dest";
end;

define C-function gtk-tree-view-enable-model-drag-source
  input parameter self :: <GtkTreeView>;
  input parameter start_button_mask_ :: <GdkModifierType>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_tree_view_enable_model_drag_source";
end;

define C-function gtk-tree-view-expand-all
  input parameter self :: <GtkTreeView>;
  c-name: "gtk_tree_view_expand_all";
end;

define C-function gtk-tree-view-expand-row
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter open_all_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_expand_row";
end;

define C-function gtk-tree-view-expand-to-path
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_view_expand_to_path";
end;

define C-function gtk-tree-view-get-background-area
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter column_ :: <GtkTreeViewColumn>;
  output parameter rect_ :: <cairoRectangleInt>;
  c-name: "gtk_tree_view_get_background_area";
end;

define C-function gtk-tree-view-get-bin-window
  input parameter self :: <GtkTreeView>;
  result res :: <GdkWindow>;
  c-name: "gtk_tree_view_get_bin_window";
end;

define C-function gtk-tree-view-get-cell-area
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter column_ :: <GtkTreeViewColumn>;
  output parameter rect_ :: <cairoRectangleInt>;
  c-name: "gtk_tree_view_get_cell_area";
end;

define C-function gtk-tree-view-get-column
  input parameter self :: <GtkTreeView>;
  input parameter n_ :: <C-signed-int>;
  result res :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_get_column";
end;

define C-function gtk-tree-view-get-columns
  input parameter self :: <GtkTreeView>;
  result res :: <GList>;
  c-name: "gtk_tree_view_get_columns";
end;

define C-function gtk-tree-view-get-cursor
  input parameter self :: <GtkTreeView>;
  output parameter path_ :: <GtkTreePath>;
  output parameter focus_column_ :: <GtkTreeViewColumn*>;
  c-name: "gtk_tree_view_get_cursor";
end;

define C-function gtk-tree-view-get-dest-row-at-pos
  input parameter self :: <GtkTreeView>;
  input parameter drag_x_ :: <C-signed-int>;
  input parameter drag_y_ :: <C-signed-int>;
  output parameter path_ :: <GtkTreePath>;
  output parameter pos_ :: <GtkTreeViewDropPosition*>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_dest_row_at_pos";
end;

define C-function gtk-tree-view-get-drag-dest-row
  input parameter self :: <GtkTreeView>;
  output parameter path_ :: <GtkTreePath>;
  output parameter pos_ :: <GtkTreeViewDropPosition*>;
  c-name: "gtk_tree_view_get_drag_dest_row";
end;

define C-function gtk-tree-view-get-enable-search
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_enable_search";
end;

define C-function gtk-tree-view-get-enable-tree-lines
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_enable_tree_lines";
end;

define C-function gtk-tree-view-get-expander-column
  input parameter self :: <GtkTreeView>;
  result res :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_get_expander_column";
end;

define C-function gtk-tree-view-get-fixed-height-mode
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_fixed_height_mode";
end;

define C-function gtk-tree-view-get-grid-lines
  input parameter self :: <GtkTreeView>;
  result res :: <GtkTreeViewGridLines>;
  c-name: "gtk_tree_view_get_grid_lines";
end;

define C-function gtk-tree-view-get-hadjustment
  input parameter self :: <GtkTreeView>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_tree_view_get_hadjustment";
end;

define C-function gtk-tree-view-get-headers-clickable
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_headers_clickable";
end;

define C-function gtk-tree-view-get-headers-visible
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_headers_visible";
end;

define C-function gtk-tree-view-get-hover-expand
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_hover_expand";
end;

define C-function gtk-tree-view-get-hover-selection
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_hover_selection";
end;

define C-function gtk-tree-view-get-level-indentation
  input parameter self :: <GtkTreeView>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_get_level_indentation";
end;

define C-function gtk-tree-view-get-model
  input parameter self :: <GtkTreeView>;
  result res :: <GtkTreeModel>;
  c-name: "gtk_tree_view_get_model";
end;

define C-function gtk-tree-view-get-n-columns
  input parameter self :: <GtkTreeView>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_tree_view_get_n_columns";
end;

define C-function gtk-tree-view-get-path-at-pos
  input parameter self :: <GtkTreeView>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter path_ :: <GtkTreePath>;
  output parameter column_ :: <GtkTreeViewColumn*>;
  output parameter cell_x_ :: <C-signed-int*>;
  output parameter cell_y_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_path_at_pos";
end;

define C-function gtk-tree-view-get-reorderable
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_reorderable";
end;

define C-function gtk-tree-view-get-rubber-banding
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_rubber_banding";
end;

define C-function gtk-tree-view-get-rules-hint
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_rules_hint";
end;

define C-function gtk-tree-view-get-search-column
  input parameter self :: <GtkTreeView>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_get_search_column";
end;

define C-function gtk-tree-view-get-search-entry
  input parameter self :: <GtkTreeView>;
  result res :: <GtkEntry>;
  c-name: "gtk_tree_view_get_search_entry";
end;

define C-function gtk-tree-view-get-selection
  input parameter self :: <GtkTreeView>;
  result res :: <GtkTreeSelection>;
  c-name: "gtk_tree_view_get_selection";
end;

define C-function gtk-tree-view-get-show-expanders
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_show_expanders";
end;

define C-function gtk-tree-view-get-tooltip-column
  input parameter self :: <GtkTreeView>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_get_tooltip_column";
end;

define C-function gtk-tree-view-get-tooltip-context
  input parameter self :: <GtkTreeView>;
  input output parameter x_ :: <C-signed-int*>;
  input output parameter y_ :: <C-signed-int*>;
  input parameter keyboard_tip_ :: <C-boolean>;
  output parameter model_ :: <GtkTreeModel*>;
  output parameter path_ :: <GtkTreePath>;
  output parameter iter_ :: <GtkTreeIter>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_tooltip_context";
end;

define C-function gtk-tree-view-get-vadjustment
  input parameter self :: <GtkTreeView>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_tree_view_get_vadjustment";
end;

define C-function gtk-tree-view-get-visible-range
  input parameter self :: <GtkTreeView>;
  output parameter start_path_ :: <GtkTreePath>;
  output parameter end_path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_get_visible_range";
end;

define C-function gtk-tree-view-get-visible-rect
  input parameter self :: <GtkTreeView>;
  output parameter visible_rect_ :: <cairoRectangleInt>;
  c-name: "gtk_tree_view_get_visible_rect";
end;

define C-function gtk-tree-view-insert-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <GtkTreeViewColumn>;
  input parameter position_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_insert_column";
end;

define C-function gtk-tree-view-insert-column-with-data-func
  input parameter self :: <GtkTreeView>;
  input parameter position_ :: <C-signed-int>;
  input parameter title_ :: <C-string>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter dnotify_ :: <C-function-pointer>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_insert_column_with_data_func";
end;

define C-function gtk-tree-view-is-blank-at-pos
  input parameter self :: <GtkTreeView>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter path_ :: <GtkTreePath>;
  output parameter column_ :: <GtkTreeViewColumn*>;
  output parameter cell_x_ :: <C-signed-int*>;
  output parameter cell_y_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_is_blank_at_pos";
end;

define C-function gtk-tree-view-is-rubber-banding-active
  input parameter self :: <GtkTreeView>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_is_rubber_banding_active";
end;

define C-function gtk-tree-view-map-expanded-rows
  input parameter self :: <GtkTreeView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_tree_view_map_expanded_rows";
end;

define C-function gtk-tree-view-move-column-after
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <GtkTreeViewColumn>;
  input parameter base_column_ :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_move_column_after";
end;

define C-function gtk-tree-view-remove-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_remove_column";
end;

define C-function gtk-tree-view-row-activated
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter column_ :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_row_activated";
end;

define C-function gtk-tree-view-row-expanded
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_row_expanded";
end;

define C-function gtk-tree-view-scroll-to-cell
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter column_ :: <GtkTreeViewColumn>;
  input parameter use_align_ :: <C-boolean>;
  input parameter row_align_ :: <C-float>;
  input parameter col_align_ :: <C-float>;
  c-name: "gtk_tree_view_scroll_to_cell";
end;

define C-function gtk-tree-view-scroll-to-point
  input parameter self :: <GtkTreeView>;
  input parameter tree_x_ :: <C-signed-int>;
  input parameter tree_y_ :: <C-signed-int>;
  c-name: "gtk_tree_view_scroll_to_point";
end;

define C-function gtk-tree-view-set-column-drag-function
  input parameter self :: <GtkTreeView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_set_column_drag_function";
end;

define C-function gtk-tree-view-set-cursor
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter focus_column_ :: <GtkTreeViewColumn>;
  input parameter start_editing_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_cursor";
end;

define C-function gtk-tree-view-set-cursor-on-cell
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter focus_column_ :: <GtkTreeViewColumn>;
  input parameter focus_cell_ :: <GtkCellRenderer>;
  input parameter start_editing_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_cursor_on_cell";
end;

define C-function gtk-tree-view-set-destroy-count-func
  input parameter self :: <GtkTreeView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_set_destroy_count_func";
end;

define C-function gtk-tree-view-set-drag-dest-row
  input parameter self :: <GtkTreeView>;
  input parameter path_ :: <GtkTreePath>;
  input parameter pos_ :: <GtkTreeViewDropPosition>;
  c-name: "gtk_tree_view_set_drag_dest_row";
end;

define C-function gtk-tree-view-set-enable-search
  input parameter self :: <GtkTreeView>;
  input parameter enable_search_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_enable_search";
end;

define C-function gtk-tree-view-set-enable-tree-lines
  input parameter self :: <GtkTreeView>;
  input parameter enabled_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_enable_tree_lines";
end;

define C-function gtk-tree-view-set-expander-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_set_expander_column";
end;

define C-function gtk-tree-view-set-fixed-height-mode
  input parameter self :: <GtkTreeView>;
  input parameter enable_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_fixed_height_mode";
end;

define C-function gtk-tree-view-set-grid-lines
  input parameter self :: <GtkTreeView>;
  input parameter grid_lines_ :: <GtkTreeViewGridLines>;
  c-name: "gtk_tree_view_set_grid_lines";
end;

define C-function gtk-tree-view-set-hadjustment
  input parameter self :: <GtkTreeView>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_tree_view_set_hadjustment";
end;

define C-function gtk-tree-view-set-headers-clickable
  input parameter self :: <GtkTreeView>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_headers_clickable";
end;

define C-function gtk-tree-view-set-headers-visible
  input parameter self :: <GtkTreeView>;
  input parameter headers_visible_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_headers_visible";
end;

define C-function gtk-tree-view-set-hover-expand
  input parameter self :: <GtkTreeView>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_hover_expand";
end;

define C-function gtk-tree-view-set-hover-selection
  input parameter self :: <GtkTreeView>;
  input parameter hover_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_hover_selection";
end;

define C-function gtk-tree-view-set-level-indentation
  input parameter self :: <GtkTreeView>;
  input parameter indentation_ :: <C-signed-int>;
  c-name: "gtk_tree_view_set_level_indentation";
end;

define C-function gtk-tree-view-set-model
  input parameter self :: <GtkTreeView>;
  input parameter model_ :: <GtkTreeModel>;
  c-name: "gtk_tree_view_set_model";
end;

define C-function gtk-tree-view-set-reorderable
  input parameter self :: <GtkTreeView>;
  input parameter reorderable_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_reorderable";
end;

define C-function gtk-tree-view-set-row-separator-func
  input parameter self :: <GtkTreeView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_set_row_separator_func";
end;

define C-function gtk-tree-view-set-rubber-banding
  input parameter self :: <GtkTreeView>;
  input parameter enable_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_rubber_banding";
end;

define C-function gtk-tree-view-set-rules-hint
  input parameter self :: <GtkTreeView>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_rules_hint";
end;

define C-function gtk-tree-view-set-search-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_tree_view_set_search_column";
end;

define C-function gtk-tree-view-set-search-entry
  input parameter self :: <GtkTreeView>;
  input parameter entry_ :: <GtkEntry>;
  c-name: "gtk_tree_view_set_search_entry";
end;

define C-function gtk-tree-view-set-search-equal-func
  input parameter self :: <GtkTreeView>;
  input parameter search_equal_func_ :: <C-function-pointer>;
  input parameter search_user_data_ :: <C-void*>;
  input parameter search_destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_set_search_equal_func";
end;

define C-function gtk-tree-view-set-search-position-func
  input parameter self :: <GtkTreeView>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_set_search_position_func";
end;

define C-function gtk-tree-view-set-show-expanders
  input parameter self :: <GtkTreeView>;
  input parameter enabled_ :: <C-boolean>;
  c-name: "gtk_tree_view_set_show_expanders";
end;

define C-function gtk-tree-view-set-tooltip-cell
  input parameter self :: <GtkTreeView>;
  input parameter tooltip_ :: <GtkTooltip>;
  input parameter path_ :: <GtkTreePath>;
  input parameter column_ :: <GtkTreeViewColumn>;
  input parameter cell_ :: <GtkCellRenderer>;
  c-name: "gtk_tree_view_set_tooltip_cell";
end;

define C-function gtk-tree-view-set-tooltip-column
  input parameter self :: <GtkTreeView>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_tree_view_set_tooltip_column";
end;

define C-function gtk-tree-view-set-tooltip-row
  input parameter self :: <GtkTreeView>;
  input parameter tooltip_ :: <GtkTooltip>;
  input parameter path_ :: <GtkTreePath>;
  c-name: "gtk_tree_view_set_tooltip_row";
end;

define C-function gtk-tree-view-set-vadjustment
  input parameter self :: <GtkTreeView>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_tree_view_set_vadjustment";
end;

define C-function gtk-tree-view-unset-rows-drag-dest
  input parameter self :: <GtkTreeView>;
  c-name: "gtk_tree_view_unset_rows_drag_dest";
end;

define C-function gtk-tree-view-unset-rows-drag-source
  input parameter self :: <GtkTreeView>;
  c-name: "gtk_tree_view_unset_rows_drag_source";
end;

define C-struct <_GtkTreeViewClass>
  constant slot gtk-tree-view-class-parent-class :: <GtkContainerClass>;
  constant slot gtk-tree-view-class-row-activated :: <C-function-pointer>;
  constant slot gtk-tree-view-class-test-expand-row :: <C-function-pointer>;
  constant slot gtk-tree-view-class-test-collapse-row :: <C-function-pointer>;
  constant slot gtk-tree-view-class-row-expanded :: <C-function-pointer>;
  constant slot gtk-tree-view-class-row-collapsed :: <C-function-pointer>;
  constant slot gtk-tree-view-class-columns-changed :: <C-function-pointer>;
  constant slot gtk-tree-view-class-cursor-changed :: <C-function-pointer>;
  constant slot gtk-tree-view-class-move-cursor :: <C-function-pointer>;
  constant slot gtk-tree-view-class-select-all :: <C-function-pointer>;
  constant slot gtk-tree-view-class-unselect-all :: <C-function-pointer>;
  constant slot gtk-tree-view-class-select-cursor-row :: <C-function-pointer>;
  constant slot gtk-tree-view-class-toggle-cursor-row :: <C-function-pointer>;
  constant slot gtk-tree-view-class-expand-collapse-cursor-row :: <C-function-pointer>;
  constant slot gtk-tree-view-class-select-cursor-parent :: <C-function-pointer>;
  constant slot gtk-tree-view-class-start-interactive-search :: <C-function-pointer>;
  constant slot gtk-tree-view-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved7 :: <C-void*>;
  constant slot gtk-tree-view-class-_gtk-reserved8 :: <C-void*>;
  pointer-type-name: <GtkTreeViewClass>;
end C-struct;

define open C-subtype <GtkTreeViewColumn> (<GInitiallyUnowned>)
  constant slot gtk-tree-view-column-parent-instance :: <GInitiallyUnowned>;
  constant slot gtk-tree-view-column-priv :: <GtkTreeViewColumnPrivate>;
end C-subtype;

define C-pointer-type <GtkTreeViewColumn*> => <GtkTreeViewColumn>;

define C-function gtk-tree-view-column-new
  result res :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_column_new";
end;

define C-function gtk-tree-view-column-new-with-area
  input parameter area_ :: <GtkCellArea>;
  result res :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_column_new_with_area";
end;

define C-function gtk-tree-view-column-add-attribute
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_renderer_ :: <GtkCellRenderer>;
  input parameter attribute_ :: <C-string>;
  input parameter column_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_add_attribute";
end;

define C-function gtk-tree-view-column-cell-get-position
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_renderer_ :: <GtkCellRenderer>;
  output parameter x_offset_ :: <C-signed-int*>;
  output parameter width_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_cell_get_position";
end;

define C-function gtk-tree-view-column-cell-get-size
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_area_ :: <cairoRectangleInt>;
  output parameter x_offset_ :: <C-signed-int*>;
  output parameter y_offset_ :: <C-signed-int*>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_tree_view_column_cell_get_size";
end;

define C-function gtk-tree-view-column-cell-is-visible
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_cell_is_visible";
end;

define C-function gtk-tree-view-column-cell-set-cell-data
  input parameter self :: <GtkTreeViewColumn>;
  input parameter tree_model_ :: <GtkTreeModel>;
  input parameter iter_ :: <GtkTreeIter>;
  input parameter is_expander_ :: <C-boolean>;
  input parameter is_expanded_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_cell_set_cell_data";
end;

define C-function gtk-tree-view-column-clear
  input parameter self :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_column_clear";
end;

define C-function gtk-tree-view-column-clear-attributes
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_renderer_ :: <GtkCellRenderer>;
  c-name: "gtk_tree_view_column_clear_attributes";
end;

define C-function gtk-tree-view-column-clicked
  input parameter self :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_column_clicked";
end;

define C-function gtk-tree-view-column-focus-cell
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_ :: <GtkCellRenderer>;
  c-name: "gtk_tree_view_column_focus_cell";
end;

define C-function gtk-tree-view-column-get-alignment
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-float>;
  c-name: "gtk_tree_view_column_get_alignment";
end;

define C-function gtk-tree-view-column-get-button
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <GtkWidget>;
  c-name: "gtk_tree_view_column_get_button";
end;

define C-function gtk-tree-view-column-get-clickable
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_clickable";
end;

define C-function gtk-tree-view-column-get-expand
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_expand";
end;

define C-function gtk-tree-view-column-get-fixed-width
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_fixed_width";
end;

define C-function gtk-tree-view-column-get-max-width
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_max_width";
end;

define C-function gtk-tree-view-column-get-min-width
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_min_width";
end;

define C-function gtk-tree-view-column-get-reorderable
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_reorderable";
end;

define C-function gtk-tree-view-column-get-resizable
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_resizable";
end;

define C-function gtk-tree-view-column-get-sizing
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <GtkTreeViewColumnSizing>;
  c-name: "gtk_tree_view_column_get_sizing";
end;

define C-function gtk-tree-view-column-get-sort-column-id
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_sort_column_id";
end;

define C-function gtk-tree-view-column-get-sort-indicator
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_sort_indicator";
end;

define C-function gtk-tree-view-column-get-sort-order
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <GtkSortType>;
  c-name: "gtk_tree_view_column_get_sort_order";
end;

define C-function gtk-tree-view-column-get-spacing
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_spacing";
end;

define C-function gtk-tree-view-column-get-title
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-string>;
  c-name: "gtk_tree_view_column_get_title";
end;

define C-function gtk-tree-view-column-get-tree-view
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <GtkWidget>;
  c-name: "gtk_tree_view_column_get_tree_view";
end;

define C-function gtk-tree-view-column-get-visible
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_view_column_get_visible";
end;

define C-function gtk-tree-view-column-get-widget
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <GtkWidget>;
  c-name: "gtk_tree_view_column_get_widget";
end;

define C-function gtk-tree-view-column-get-width
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_width";
end;

define C-function gtk-tree-view-column-get-x-offset
  input parameter self :: <GtkTreeViewColumn>;
  result res :: <C-signed-int>;
  c-name: "gtk_tree_view_column_get_x_offset";
end;

define C-function gtk-tree-view-column-pack-end
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_pack_end";
end;

define C-function gtk-tree-view-column-pack-start
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_ :: <GtkCellRenderer>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_pack_start";
end;

define C-function gtk-tree-view-column-queue-resize
  input parameter self :: <GtkTreeViewColumn>;
  c-name: "gtk_tree_view_column_queue_resize";
end;

define C-function gtk-tree-view-column-set-alignment
  input parameter self :: <GtkTreeViewColumn>;
  input parameter xalign_ :: <C-float>;
  c-name: "gtk_tree_view_column_set_alignment";
end;

define C-function gtk-tree-view-column-set-cell-data-func
  input parameter self :: <GtkTreeViewColumn>;
  input parameter cell_renderer_ :: <GtkCellRenderer>;
  input parameter func_ :: <C-function-pointer>;
  input parameter func_data_ :: <C-void*>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "gtk_tree_view_column_set_cell_data_func";
end;

define C-function gtk-tree-view-column-set-clickable
  input parameter self :: <GtkTreeViewColumn>;
  input parameter clickable_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_clickable";
end;

define C-function gtk-tree-view-column-set-expand
  input parameter self :: <GtkTreeViewColumn>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_expand";
end;

define C-function gtk-tree-view-column-set-fixed-width
  input parameter self :: <GtkTreeViewColumn>;
  input parameter fixed_width_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_set_fixed_width";
end;

define C-function gtk-tree-view-column-set-max-width
  input parameter self :: <GtkTreeViewColumn>;
  input parameter max_width_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_set_max_width";
end;

define C-function gtk-tree-view-column-set-min-width
  input parameter self :: <GtkTreeViewColumn>;
  input parameter min_width_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_set_min_width";
end;

define C-function gtk-tree-view-column-set-reorderable
  input parameter self :: <GtkTreeViewColumn>;
  input parameter reorderable_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_reorderable";
end;

define C-function gtk-tree-view-column-set-resizable
  input parameter self :: <GtkTreeViewColumn>;
  input parameter resizable_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_resizable";
end;

define C-function gtk-tree-view-column-set-sizing
  input parameter self :: <GtkTreeViewColumn>;
  input parameter type_ :: <GtkTreeViewColumnSizing>;
  c-name: "gtk_tree_view_column_set_sizing";
end;

define C-function gtk-tree-view-column-set-sort-column-id
  input parameter self :: <GtkTreeViewColumn>;
  input parameter sort_column_id_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_set_sort_column_id";
end;

define C-function gtk-tree-view-column-set-sort-indicator
  input parameter self :: <GtkTreeViewColumn>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_sort_indicator";
end;

define C-function gtk-tree-view-column-set-sort-order
  input parameter self :: <GtkTreeViewColumn>;
  input parameter order_ :: <GtkSortType>;
  c-name: "gtk_tree_view_column_set_sort_order";
end;

define C-function gtk-tree-view-column-set-spacing
  input parameter self :: <GtkTreeViewColumn>;
  input parameter spacing_ :: <C-signed-int>;
  c-name: "gtk_tree_view_column_set_spacing";
end;

define C-function gtk-tree-view-column-set-title
  input parameter self :: <GtkTreeViewColumn>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_tree_view_column_set_title";
end;

define C-function gtk-tree-view-column-set-visible
  input parameter self :: <GtkTreeViewColumn>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_tree_view_column_set_visible";
end;

define C-function gtk-tree-view-column-set-widget
  input parameter self :: <GtkTreeViewColumn>;
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_tree_view_column_set_widget";
end;

define C-struct <_GtkTreeViewColumnClass>
  constant slot gtk-tree-view-column-class-parent-class :: <GInitiallyUnownedClass>;
  constant slot gtk-tree-view-column-class-clicked :: <C-function-pointer>;
  constant slot gtk-tree-view-column-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-tree-view-column-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-tree-view-column-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-tree-view-column-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkTreeViewColumnClass>;
end C-struct;

define C-struct <_GtkTreeViewColumnPrivate>
  pointer-type-name: <GtkTreeViewColumnPrivate>;
end C-struct;

define constant $gtk-tree-view-column-grow-only = 0;
define constant $gtk-tree-view-column-autosize = 1;
define constant $gtk-tree-view-column-fixed = 2;
define constant <GtkTreeViewColumnSizing> = <C-int>;
define C-pointer-type <GtkTreeViewColumnSizing*> => <GtkTreeViewColumnSizing>;

define constant $gtk-tree-view-drop-before = 0;
define constant $gtk-tree-view-drop-after = 1;
define constant $gtk-tree-view-drop-into-or-before = 2;
define constant $gtk-tree-view-drop-into-or-after = 3;
define constant <GtkTreeViewDropPosition> = <C-int>;
define C-pointer-type <GtkTreeViewDropPosition*> => <GtkTreeViewDropPosition>;

define constant $gtk-tree-view-grid-lines-none = 0;
define constant $gtk-tree-view-grid-lines-horizontal = 1;
define constant $gtk-tree-view-grid-lines-vertical = 2;
define constant $gtk-tree-view-grid-lines-both = 3;
define constant <GtkTreeViewGridLines> = <C-int>;
define C-pointer-type <GtkTreeViewGridLines*> => <GtkTreeViewGridLines>;

define C-struct <_GtkTreeViewPrivate>
  pointer-type-name: <GtkTreeViewPrivate>;
end C-struct;

define open C-subtype <GtkUIManager> (<GObject>)
  constant slot gtk-ui-manager-parent :: <GObject>;
  constant slot gtk-ui-manager-private-data :: <GtkUIManagerPrivate>;
end C-subtype;

define C-pointer-type <GtkUIManager*> => <GtkUIManager>;

define C-function gtk-ui-manager-new
  result res :: <GtkUIManager>;
  c-name: "gtk_ui_manager_new";
end;

define C-function gtk-ui-manager-add-ui
  input parameter self :: <GtkUIManager>;
  input parameter merge_id_ :: <C-unsigned-int>;
  input parameter path_ :: <C-string>;
  input parameter name_ :: <C-string>;
  input parameter action_ :: <C-string>;
  input parameter type_ :: <GtkUIManagerItemType>;
  input parameter top_ :: <C-boolean>;
  c-name: "gtk_ui_manager_add_ui";
end;

define C-function gtk-ui-manager-add-ui-from-file
  input parameter self :: <GtkUIManager>;
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_ui_manager_add_ui_from_file";
end;

define C-function gtk-ui-manager-add-ui-from-resource
  input parameter self :: <GtkUIManager>;
  input parameter resource_path_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_ui_manager_add_ui_from_resource";
end;

define C-function gtk-ui-manager-add-ui-from-string
  input parameter self :: <GtkUIManager>;
  input parameter buffer_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_ui_manager_add_ui_from_string";
end;

define C-function gtk-ui-manager-ensure-update
  input parameter self :: <GtkUIManager>;
  c-name: "gtk_ui_manager_ensure_update";
end;

define C-function gtk-ui-manager-get-accel-group
  input parameter self :: <GtkUIManager>;
  result res :: <GtkAccelGroup>;
  c-name: "gtk_ui_manager_get_accel_group";
end;

define C-function gtk-ui-manager-get-action
  input parameter self :: <GtkUIManager>;
  input parameter path_ :: <C-string>;
  result res :: <GtkAction>;
  c-name: "gtk_ui_manager_get_action";
end;

define C-function gtk-ui-manager-get-action-groups
  input parameter self :: <GtkUIManager>;
  result res :: <GList>;
  c-name: "gtk_ui_manager_get_action_groups";
end;

define C-function gtk-ui-manager-get-add-tearoffs
  input parameter self :: <GtkUIManager>;
  result res :: <C-boolean>;
  c-name: "gtk_ui_manager_get_add_tearoffs";
end;

define C-function gtk-ui-manager-get-toplevels
  input parameter self :: <GtkUIManager>;
  input parameter types_ :: <GtkUIManagerItemType>;
  result res :: <GSList>;
  c-name: "gtk_ui_manager_get_toplevels";
end;

define C-function gtk-ui-manager-get-ui
  input parameter self :: <GtkUIManager>;
  result res :: <C-string>;
  c-name: "gtk_ui_manager_get_ui";
end;

define C-function gtk-ui-manager-get-widget
  input parameter self :: <GtkUIManager>;
  input parameter path_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_ui_manager_get_widget";
end;

define C-function gtk-ui-manager-insert-action-group
  input parameter self :: <GtkUIManager>;
  input parameter action_group_ :: <GtkActionGroup>;
  input parameter pos_ :: <C-signed-int>;
  c-name: "gtk_ui_manager_insert_action_group";
end;

define C-function gtk-ui-manager-new-merge-id
  input parameter self :: <GtkUIManager>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_ui_manager_new_merge_id";
end;

define C-function gtk-ui-manager-remove-action-group
  input parameter self :: <GtkUIManager>;
  input parameter action_group_ :: <GtkActionGroup>;
  c-name: "gtk_ui_manager_remove_action_group";
end;

define C-function gtk-ui-manager-remove-ui
  input parameter self :: <GtkUIManager>;
  input parameter merge_id_ :: <C-unsigned-int>;
  c-name: "gtk_ui_manager_remove_ui";
end;

define C-function gtk-ui-manager-set-add-tearoffs
  input parameter self :: <GtkUIManager>;
  input parameter add_tearoffs_ :: <C-boolean>;
  c-name: "gtk_ui_manager_set_add_tearoffs";
end;

define C-struct <_GtkUIManagerClass>
  constant slot gtk-ui-manager-class-parent-class :: <GObjectClass>;
  constant slot gtk-ui-manager-class-add-widget :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-actions-changed :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-connect-proxy :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-disconnect-proxy :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-pre-activate :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-post-activate :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-get-widget :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-get-action :: <C-function-pointer>;
  constant slot gtk-ui-manager-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-ui-manager-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-ui-manager-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-ui-manager-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkUIManagerClass>;
end C-struct;

define constant $gtk-ui-manager-auto = 0;
define constant $gtk-ui-manager-menubar = 1;
define constant $gtk-ui-manager-menu = 2;
define constant $gtk-ui-manager-toolbar = 4;
define constant $gtk-ui-manager-placeholder = 8;
define constant $gtk-ui-manager-popup = 16;
define constant $gtk-ui-manager-menuitem = 32;
define constant $gtk-ui-manager-toolitem = 64;
define constant $gtk-ui-manager-separator = 128;
define constant $gtk-ui-manager-accelerator = 256;
define constant $gtk-ui-manager-popup-with-accels = 512;
define constant <GtkUIManagerItemType> = <C-int>;
define C-pointer-type <GtkUIManagerItemType*> => <GtkUIManagerItemType>;

define C-struct <_GtkUIManagerPrivate>
  pointer-type-name: <GtkUIManagerPrivate>;
end C-struct;

define constant $gtk-unit-pixel = 0;
define constant $gtk-unit-points = 1;
define constant $gtk-unit-inch = 2;
define constant $gtk-unit-mm = 3;
define constant <GtkUnit> = <C-int>;
define C-pointer-type <GtkUnit*> => <GtkUnit>;

define open C-subtype <GtkVBox> (<GtkBox>)
  constant slot gtk-v-box-box :: <GtkBox>;
end C-subtype;

define C-pointer-type <GtkVBox*> => <GtkVBox>;

define C-function gtk-vbox-new
  input parameter homogeneous_ :: <C-boolean>;
  input parameter spacing_ :: <C-signed-int>;
  result res :: <GtkWidget>;
  c-name: "gtk_vbox_new";
end;

define C-struct <_GtkVBoxClass>
  constant slot gtk-v-box-class-parent-class :: <GtkBoxClass>;
  pointer-type-name: <GtkVBoxClass>;
end C-struct;

define open C-subtype <GtkVButtonBox> (<GtkButtonBox>)
  constant slot gtk-v-button-box-button-box :: <GtkButtonBox>;
end C-subtype;

define C-pointer-type <GtkVButtonBox*> => <GtkVButtonBox>;

define C-function gtk-vbutton-box-new
  result res :: <GtkWidget>;
  c-name: "gtk_vbutton_box_new";
end;

define C-struct <_GtkVButtonBoxClass>
  constant slot gtk-v-button-box-class-parent-class :: <GtkButtonBoxClass>;
  pointer-type-name: <GtkVButtonBoxClass>;
end C-struct;

define open C-subtype <GtkVPaned> (<GtkPaned>)
  constant slot gtk-v-paned-paned :: <GtkPaned>;
end C-subtype;

define C-pointer-type <GtkVPaned*> => <GtkVPaned>;

define C-function gtk-vpaned-new
  result res :: <GtkWidget>;
  c-name: "gtk_vpaned_new";
end;

define C-struct <_GtkVPanedClass>
  constant slot gtk-v-paned-class-parent-class :: <GtkPanedClass>;
  pointer-type-name: <GtkVPanedClass>;
end C-struct;

define open C-subtype <GtkVScale> (<GtkScale>)
  constant slot gtk-v-scale-scale :: <GtkScale>;
end C-subtype;

define C-pointer-type <GtkVScale*> => <GtkVScale>;

define C-function gtk-vscale-new
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_vscale_new";
end;

define C-function gtk-vscale-new-with-range
  input parameter min_ :: <C-double>;
  input parameter max_ :: <C-double>;
  input parameter step_ :: <C-double>;
  result res :: <GtkWidget>;
  c-name: "gtk_vscale_new_with_range";
end;

define C-struct <_GtkVScaleClass>
  constant slot gtk-v-scale-class-parent-class :: <GtkScaleClass>;
  pointer-type-name: <GtkVScaleClass>;
end C-struct;

define open C-subtype <GtkVScrollbar> (<GtkScrollbar>)
  constant slot gtk-v-scrollbar-scrollbar :: <GtkScrollbar>;
end C-subtype;

define C-pointer-type <GtkVScrollbar*> => <GtkVScrollbar>;

define C-function gtk-vscrollbar-new
  input parameter adjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_vscrollbar_new";
end;

define C-struct <_GtkVScrollbarClass>
  constant slot gtk-v-scrollbar-class-parent-class :: <GtkScrollbarClass>;
  pointer-type-name: <GtkVScrollbarClass>;
end C-struct;

define open C-subtype <GtkVSeparator> (<GtkSeparator>)
  constant slot gtk-v-separator-separator :: <GtkSeparator>;
end C-subtype;

define C-pointer-type <GtkVSeparator*> => <GtkVSeparator>;

define C-function gtk-vseparator-new
  result res :: <GtkWidget>;
  c-name: "gtk_vseparator_new";
end;

define C-struct <_GtkVSeparatorClass>
  constant slot gtk-v-separator-class-parent-class :: <GtkSeparatorClass>;
  pointer-type-name: <GtkVSeparatorClass>;
end C-struct;

define open C-subtype <GtkViewport> (<GtkBin>)
  constant slot gtk-viewport-bin :: <GtkBin>;
  constant slot gtk-viewport-priv :: <GtkViewportPrivate>;
end C-subtype;

define C-pointer-type <GtkViewport*> => <GtkViewport>;

define C-function gtk-viewport-new
  input parameter hadjustment_ :: <GtkAdjustment>;
  input parameter vadjustment_ :: <GtkAdjustment>;
  result res :: <GtkWidget>;
  c-name: "gtk_viewport_new";
end;

define C-function gtk-viewport-get-bin-window
  input parameter self :: <GtkViewport>;
  result res :: <GdkWindow>;
  c-name: "gtk_viewport_get_bin_window";
end;

define C-function gtk-viewport-get-hadjustment
  input parameter self :: <GtkViewport>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_viewport_get_hadjustment";
end;

define C-function gtk-viewport-get-shadow-type
  input parameter self :: <GtkViewport>;
  result res :: <GtkShadowType>;
  c-name: "gtk_viewport_get_shadow_type";
end;

define C-function gtk-viewport-get-vadjustment
  input parameter self :: <GtkViewport>;
  result res :: <GtkAdjustment>;
  c-name: "gtk_viewport_get_vadjustment";
end;

define C-function gtk-viewport-get-view-window
  input parameter self :: <GtkViewport>;
  result res :: <GdkWindow>;
  c-name: "gtk_viewport_get_view_window";
end;

define C-function gtk-viewport-set-hadjustment
  input parameter self :: <GtkViewport>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_viewport_set_hadjustment";
end;

define C-function gtk-viewport-set-shadow-type
  input parameter self :: <GtkViewport>;
  input parameter type_ :: <GtkShadowType>;
  c-name: "gtk_viewport_set_shadow_type";
end;

define C-function gtk-viewport-set-vadjustment
  input parameter self :: <GtkViewport>;
  input parameter adjustment_ :: <GtkAdjustment>;
  c-name: "gtk_viewport_set_vadjustment";
end;

define C-struct <_GtkViewportClass>
  constant slot gtk-viewport-class-parent-class :: <GtkBinClass>;
  constant slot gtk-viewport-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-viewport-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-viewport-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-viewport-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkViewportClass>;
end C-struct;

define C-struct <_GtkViewportPrivate>
  pointer-type-name: <GtkViewportPrivate>;
end C-struct;

define open C-subtype <GtkVolumeButton> (<GtkScaleButton>)
  constant slot gtk-volume-button-parent :: <GtkScaleButton>;
end C-subtype;

define C-pointer-type <GtkVolumeButton*> => <GtkVolumeButton>;

define C-function gtk-volume-button-new
  result res :: <GtkWidget>;
  c-name: "gtk_volume_button_new";
end;

define C-struct <_GtkVolumeButtonClass>
  constant slot gtk-volume-button-class-parent-class :: <GtkScaleButtonClass>;
  constant slot gtk-volume-button-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-volume-button-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-volume-button-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-volume-button-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkVolumeButtonClass>;
end C-struct;

define open C-subtype <GtkWidget> (<GInitiallyUnowned>)
  constant slot gtk-widget-parent-instance :: <GInitiallyUnowned>;
  constant slot gtk-widget-priv :: <GtkWidgetPrivate>;
end C-subtype;

define C-pointer-type <GtkWidget*> => <GtkWidget>;

define C-function gtk-widget-get-default-direction
  result res :: <GtkTextDirection>;
  c-name: "gtk_widget_get_default_direction";
end;

define C-function gtk-widget-get-default-style
  result res :: <GtkStyle>;
  c-name: "gtk_widget_get_default_style";
end;

define C-function gtk-widget-pop-composite-child
  c-name: "gtk_widget_pop_composite_child";
end;

define C-function gtk-widget-push-composite-child
  c-name: "gtk_widget_push_composite_child";
end;

define C-function gtk-widget-set-default-direction
  input parameter dir_ :: <GtkTextDirection>;
  c-name: "gtk_widget_set_default_direction";
end;

define C-function gtk-widget-activate
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_activate";
end;

define C-function gtk-widget-add-accelerator
  input parameter self :: <GtkWidget>;
  input parameter accel_signal_ :: <C-string>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  input parameter accel_flags_ :: <GtkAccelFlags>;
  c-name: "gtk_widget_add_accelerator";
end;

define C-function gtk-widget-add-device-events
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  input parameter events_ :: <GdkEventMask>;
  c-name: "gtk_widget_add_device_events";
end;

define C-function gtk-widget-add-events
  input parameter self :: <GtkWidget>;
  input parameter events_ :: <C-signed-int>;
  c-name: "gtk_widget_add_events";
end;

define C-function gtk-widget-add-mnemonic-label
  input parameter self :: <GtkWidget>;
  input parameter label_ :: <GtkWidget>;
  c-name: "gtk_widget_add_mnemonic_label";
end;

define C-function gtk-widget-can-activate-accel
  input parameter self :: <GtkWidget>;
  input parameter signal_id_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_can_activate_accel";
end;

define C-function gtk-widget-child-focus
  input parameter self :: <GtkWidget>;
  input parameter direction_ :: <GtkDirectionType>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_child_focus";
end;

define C-function gtk-widget-child-notify
  input parameter self :: <GtkWidget>;
  input parameter child_property_ :: <C-string>;
  c-name: "gtk_widget_child_notify";
end;

define C-function gtk-widget-class-path
  input parameter self :: <GtkWidget>;
  output parameter path_length_ :: <C-unsigned-int*>;
  output parameter path_ :: <C-string>;
  output parameter path_reversed_ :: <C-string>;
  c-name: "gtk_widget_class_path";
end;

define C-function gtk-widget-compute-expand
  input parameter self :: <GtkWidget>;
  input parameter orientation_ :: <GtkOrientation>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_compute_expand";
end;

define C-function gtk-widget-create-pango-context
  input parameter self :: <GtkWidget>;
  result res :: <PangoContext>;
  c-name: "gtk_widget_create_pango_context";
end;

define C-function gtk-widget-create-pango-layout
  input parameter self :: <GtkWidget>;
  input parameter text_ :: <C-string>;
  result res :: <PangoLayout>;
  c-name: "gtk_widget_create_pango_layout";
end;

define C-function gtk-widget-destroy
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_destroy";
end;

define C-function gtk-widget-destroyed
  input parameter self :: <GtkWidget>;
  input output parameter widget_pointer_ :: <GtkWidget*>;
  c-name: "gtk_widget_destroyed";
end;

define C-function gtk-widget-device-is-shadowed
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_device_is_shadowed";
end;

define C-function gtk-drag-begin
  input parameter self :: <GtkWidget>;
  input parameter targets_ :: <GtkTargetList>;
  input parameter actions_ :: <GdkDragAction>;
  input parameter button_ :: <C-signed-int>;
  input parameter event_ :: <GdkEvent>;
  result res :: <GdkDragContext>;
  c-name: "gtk_drag_begin";
end;

define C-function gtk-drag-check-threshold
  input parameter self :: <GtkWidget>;
  input parameter start_x_ :: <C-signed-int>;
  input parameter start_y_ :: <C-signed-int>;
  input parameter current_x_ :: <C-signed-int>;
  input parameter current_y_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_drag_check_threshold";
end;

define C-function gtk-drag-dest-add-image-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_dest_add_image_targets";
end;

define C-function gtk-drag-dest-add-text-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_dest_add_text_targets";
end;

define C-function gtk-drag-dest-add-uri-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_dest_add_uri_targets";
end;

define C-function gtk-drag-dest-find-target
  input parameter self :: <GtkWidget>;
  input parameter context_ :: <GdkDragContext>;
  input parameter target_list_ :: <GtkTargetList>;
  result res :: <GdkAtom>;
  c-name: "gtk_drag_dest_find_target";
end;

define C-function gtk-drag-dest-get-target-list
  input parameter self :: <GtkWidget>;
  result res :: <GtkTargetList>;
  c-name: "gtk_drag_dest_get_target_list";
end;

define C-function gtk-drag-dest-get-track-motion
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_drag_dest_get_track_motion";
end;

define C-function gtk-drag-dest-set
  input parameter self :: <GtkWidget>;
  input parameter flags_ :: <GtkDestDefaults>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_drag_dest_set";
end;

define C-function gtk-drag-dest-set-proxy
  input parameter self :: <GtkWidget>;
  input parameter proxy_window_ :: <GdkWindow>;
  input parameter protocol_ :: <GdkDragProtocol>;
  input parameter use_coordinates_ :: <C-boolean>;
  c-name: "gtk_drag_dest_set_proxy";
end;

define C-function gtk-drag-dest-set-target-list
  input parameter self :: <GtkWidget>;
  input parameter target_list_ :: <GtkTargetList>;
  c-name: "gtk_drag_dest_set_target_list";
end;

define C-function gtk-drag-dest-set-track-motion
  input parameter self :: <GtkWidget>;
  input parameter track_motion_ :: <C-boolean>;
  c-name: "gtk_drag_dest_set_track_motion";
end;

define C-function gtk-drag-dest-unset
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_dest_unset";
end;

define C-function gtk-drag-get-data
  input parameter self :: <GtkWidget>;
  input parameter context_ :: <GdkDragContext>;
  input parameter target_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gtk_drag_get_data";
end;

define C-function gtk-drag-highlight
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_highlight";
end;

define C-function gtk-drag-source-add-image-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_source_add_image_targets";
end;

define C-function gtk-drag-source-add-text-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_source_add_text_targets";
end;

define C-function gtk-drag-source-add-uri-targets
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_source_add_uri_targets";
end;

define C-function gtk-drag-source-get-target-list
  input parameter self :: <GtkWidget>;
  result res :: <GtkTargetList>;
  c-name: "gtk_drag_source_get_target_list";
end;

define C-function gtk-drag-source-set
  input parameter self :: <GtkWidget>;
  input parameter start_button_mask_ :: <GdkModifierType>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter actions_ :: <GdkDragAction>;
  c-name: "gtk_drag_source_set";
end;

define C-function gtk-drag-source-set-icon-gicon
  input parameter self :: <GtkWidget>;
  input parameter icon_ :: <GIcon>;
  c-name: "gtk_drag_source_set_icon_gicon";
end;

define C-function gtk-drag-source-set-icon-name
  input parameter self :: <GtkWidget>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gtk_drag_source_set_icon_name";
end;

define C-function gtk-drag-source-set-icon-pixbuf
  input parameter self :: <GtkWidget>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gtk_drag_source_set_icon_pixbuf";
end;

define C-function gtk-drag-source-set-icon-stock
  input parameter self :: <GtkWidget>;
  input parameter stock_id_ :: <C-string>;
  c-name: "gtk_drag_source_set_icon_stock";
end;

define C-function gtk-drag-source-set-target-list
  input parameter self :: <GtkWidget>;
  input parameter target_list_ :: <GtkTargetList>;
  c-name: "gtk_drag_source_set_target_list";
end;

define C-function gtk-drag-source-unset
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_source_unset";
end;

define C-function gtk-drag-unhighlight
  input parameter self :: <GtkWidget>;
  c-name: "gtk_drag_unhighlight";
end;

define C-function gtk-widget-draw
  input parameter self :: <GtkWidget>;
  input parameter cr_ :: <cairoContext>;
  c-name: "gtk_widget_draw";
end;

define C-function gtk-widget-ensure-style
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_ensure_style";
end;

define C-function gtk-widget-error-bell
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_error_bell";
end;

define C-function gtk-widget-event
  input parameter self :: <GtkWidget>;
  input parameter event_ :: <GdkEvent>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_event";
end;

define C-function gtk-widget-freeze-child-notify
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_freeze_child_notify";
end;

define C-function gtk-widget-get-accessible
  input parameter self :: <GtkWidget>;
  result res :: <AtkObject>;
  c-name: "gtk_widget_get_accessible";
end;

define C-function gtk-widget-get-allocated-height
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_allocated_height";
end;

define C-function gtk-widget-get-allocated-width
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_allocated_width";
end;

define C-function gtk-widget-get-allocation
  input parameter self :: <GtkWidget>;
  output parameter allocation_ :: <cairoRectangleInt>;
  c-name: "gtk_widget_get_allocation";
end;

define C-function gtk-widget-get-ancestor
  input parameter self :: <GtkWidget>;
  input parameter widget_type_ :: <C-long>;
  result res :: <GtkWidget>;
  c-name: "gtk_widget_get_ancestor";
end;

define C-function gtk-widget-get-app-paintable
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_app_paintable";
end;

define C-function gtk-widget-get-can-default
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_can_default";
end;

define C-function gtk-widget-get-can-focus
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_can_focus";
end;

define C-function gtk-widget-get-child-requisition
  input parameter self :: <GtkWidget>;
  output parameter requisition_ :: <GtkRequisition>;
  c-name: "gtk_widget_get_child_requisition";
end;

define C-function gtk-widget-get-child-visible
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_child_visible";
end;

define C-function gtk-widget-get-clipboard
  input parameter self :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  result res :: <GtkClipboard>;
  c-name: "gtk_widget_get_clipboard";
end;

define C-function gtk-widget-get-composite-name
  input parameter self :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_widget_get_composite_name";
end;

define C-function gtk-widget-get-device-enabled
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_device_enabled";
end;

define C-function gtk-widget-get-device-events
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  result res :: <GdkEventMask>;
  c-name: "gtk_widget_get_device_events";
end;

define C-function gtk-widget-get-direction
  input parameter self :: <GtkWidget>;
  result res :: <GtkTextDirection>;
  c-name: "gtk_widget_get_direction";
end;

define C-function gtk-widget-get-display
  input parameter self :: <GtkWidget>;
  result res :: <GdkDisplay>;
  c-name: "gtk_widget_get_display";
end;

define C-function gtk-widget-get-double-buffered
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_double_buffered";
end;

define C-function gtk-widget-get-events
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_events";
end;

define C-function gtk-widget-get-halign
  input parameter self :: <GtkWidget>;
  result res :: <GtkAlign>;
  c-name: "gtk_widget_get_halign";
end;

define C-function gtk-widget-get-has-tooltip
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_has_tooltip";
end;

define C-function gtk-widget-get-has-window
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_has_window";
end;

define C-function gtk-widget-get-hexpand
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_hexpand";
end;

define C-function gtk-widget-get-hexpand-set
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_hexpand_set";
end;

define C-function gtk-widget-get-mapped
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_mapped";
end;

define C-function gtk-widget-get-margin-bottom
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_margin_bottom";
end;

define C-function gtk-widget-get-margin-left
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_margin_left";
end;

define C-function gtk-widget-get-margin-right
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_margin_right";
end;

define C-function gtk-widget-get-margin-top
  input parameter self :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_get_margin_top";
end;

define C-function gtk-widget-get-modifier-mask
  input parameter self :: <GtkWidget>;
  input parameter intent_ :: <GdkModifierIntent>;
  result res :: <GdkModifierType>;
  c-name: "gtk_widget_get_modifier_mask";
end;

define C-function gtk-widget-get-modifier-style
  input parameter self :: <GtkWidget>;
  result res :: <GtkRcStyle>;
  c-name: "gtk_widget_get_modifier_style";
end;

define C-function gtk-widget-get-name
  input parameter self :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_widget_get_name";
end;

define C-function gtk-widget-get-no-show-all
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_no_show_all";
end;

define C-function gtk-widget-get-pango-context
  input parameter self :: <GtkWidget>;
  result res :: <PangoContext>;
  c-name: "gtk_widget_get_pango_context";
end;

define C-function gtk-widget-get-parent
  input parameter self :: <GtkWidget>;
  result res :: <GtkWidget>;
  c-name: "gtk_widget_get_parent";
end;

define C-function gtk-widget-get-parent-window
  input parameter self :: <GtkWidget>;
  result res :: <GdkWindow>;
  c-name: "gtk_widget_get_parent_window";
end;

define C-function gtk-widget-get-path
  input parameter self :: <GtkWidget>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_widget_get_path";
end;

define C-function gtk-widget-get-pointer
  input parameter self :: <GtkWidget>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_pointer";
end;

define C-function gtk-widget-get-preferred-height
  input parameter self :: <GtkWidget>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_preferred_height";
end;

define C-function gtk-widget-get-preferred-height-for-width
  input parameter self :: <GtkWidget>;
  input parameter width_ :: <C-signed-int>;
  output parameter minimum_height_ :: <C-signed-int*>;
  output parameter natural_height_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_preferred_height_for_width";
end;

define C-function gtk-widget-get-preferred-size
  input parameter self :: <GtkWidget>;
  output parameter minimum_size_ :: <GtkRequisition>;
  output parameter natural_size_ :: <GtkRequisition>;
  c-name: "gtk_widget_get_preferred_size";
end;

define C-function gtk-widget-get-preferred-width
  input parameter self :: <GtkWidget>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_preferred_width";
end;

define C-function gtk-widget-get-preferred-width-for-height
  input parameter self :: <GtkWidget>;
  input parameter height_ :: <C-signed-int>;
  output parameter minimum_width_ :: <C-signed-int*>;
  output parameter natural_width_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_preferred_width_for_height";
end;

define C-function gtk-widget-get-realized
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_realized";
end;

define C-function gtk-widget-get-receives-default
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_receives_default";
end;

define C-function gtk-widget-get-request-mode
  input parameter self :: <GtkWidget>;
  result res :: <GtkSizeRequestMode>;
  c-name: "gtk_widget_get_request_mode";
end;

define C-function gtk-widget-get-requisition
  input parameter self :: <GtkWidget>;
  output parameter requisition_ :: <GtkRequisition>;
  c-name: "gtk_widget_get_requisition";
end;

define C-function gtk-widget-get-root-window
  input parameter self :: <GtkWidget>;
  result res :: <GdkWindow>;
  c-name: "gtk_widget_get_root_window";
end;

define C-function gtk-widget-get-screen
  input parameter self :: <GtkWidget>;
  result res :: <GdkScreen>;
  c-name: "gtk_widget_get_screen";
end;

define C-function gtk-widget-get-sensitive
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_sensitive";
end;

define C-function gtk-widget-get-settings
  input parameter self :: <GtkWidget>;
  result res :: <GtkSettings>;
  c-name: "gtk_widget_get_settings";
end;

define C-function gtk-widget-get-size-request
  input parameter self :: <GtkWidget>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_widget_get_size_request";
end;

define C-function gtk-widget-get-state
  input parameter self :: <GtkWidget>;
  result res :: <GtkStateType>;
  c-name: "gtk_widget_get_state";
end;

define C-function gtk-widget-get-state-flags
  input parameter self :: <GtkWidget>;
  result res :: <GtkStateFlags>;
  c-name: "gtk_widget_get_state_flags";
end;

define C-function gtk-widget-get-style
  input parameter self :: <GtkWidget>;
  result res :: <GtkStyle>;
  c-name: "gtk_widget_get_style";
end;

define C-function gtk-widget-get-style-context
  input parameter self :: <GtkWidget>;
  result res :: <GtkStyleContext>;
  c-name: "gtk_widget_get_style_context";
end;

define C-function gtk-widget-get-support-multidevice
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_support_multidevice";
end;

define C-function gtk-widget-get-tooltip-markup
  input parameter self :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_widget_get_tooltip_markup";
end;

define C-function gtk-widget-get-tooltip-text
  input parameter self :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_widget_get_tooltip_text";
end;

define C-function gtk-widget-get-tooltip-window
  input parameter self :: <GtkWidget>;
  result res :: <GtkWindow>;
  c-name: "gtk_widget_get_tooltip_window";
end;

define C-function gtk-widget-get-toplevel
  input parameter self :: <GtkWidget>;
  result res :: <GtkWidget>;
  c-name: "gtk_widget_get_toplevel";
end;

define C-function gtk-widget-get-valign
  input parameter self :: <GtkWidget>;
  result res :: <GtkAlign>;
  c-name: "gtk_widget_get_valign";
end;

define C-function gtk-widget-get-vexpand
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_vexpand";
end;

define C-function gtk-widget-get-vexpand-set
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_vexpand_set";
end;

define C-function gtk-widget-get-visible
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_get_visible";
end;

define C-function gtk-widget-get-visual
  input parameter self :: <GtkWidget>;
  result res :: <GdkVisual>;
  c-name: "gtk_widget_get_visual";
end;

define C-function gtk-widget-get-window
  input parameter self :: <GtkWidget>;
  result res :: <GdkWindow>;
  c-name: "gtk_widget_get_window";
end;

define C-function gtk-grab-add
  input parameter self :: <GtkWidget>;
  c-name: "gtk_grab_add";
end;

define C-function gtk-widget-grab-default
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_grab_default";
end;

define C-function gtk-widget-grab-focus
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_grab_focus";
end;

define C-function gtk-grab-remove
  input parameter self :: <GtkWidget>;
  c-name: "gtk_grab_remove";
end;

define C-function gtk-widget-has-default
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_default";
end;

define C-function gtk-widget-has-focus
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_focus";
end;

define C-function gtk-widget-has-grab
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_grab";
end;

define C-function gtk-widget-has-rc-style
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_rc_style";
end;

define C-function gtk-widget-has-screen
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_screen";
end;

define C-function gtk-widget-has-visible-focus
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_has_visible_focus";
end;

define C-function gtk-widget-hide
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_hide";
end;

define C-function gtk-widget-hide-on-delete
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_hide_on_delete";
end;

define C-function gtk-widget-in-destruction
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_in_destruction";
end;

define C-function gtk-widget-input-shape-combine-region
  input parameter self :: <GtkWidget>;
  input parameter region_ :: <cairoRegion>;
  c-name: "gtk_widget_input_shape_combine_region";
end;

define C-function gtk-widget-intersect
  input parameter self :: <GtkWidget>;
  input parameter area_ :: <cairoRectangleInt>;
  input parameter intersection_ :: <cairoRectangleInt>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_intersect";
end;

define C-function gtk-widget-is-ancestor
  input parameter self :: <GtkWidget>;
  input parameter ancestor_ :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_ancestor";
end;

define C-function gtk-widget-is-composited
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_composited";
end;

define C-function gtk-widget-is-drawable
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_drawable";
end;

define C-function gtk-widget-is-focus
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_focus";
end;

define C-function gtk-widget-is-sensitive
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_sensitive";
end;

define C-function gtk-widget-is-toplevel
  input parameter self :: <GtkWidget>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_is_toplevel";
end;

define C-function gtk-widget-keynav-failed
  input parameter self :: <GtkWidget>;
  input parameter direction_ :: <GtkDirectionType>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_keynav_failed";
end;

define C-function gtk-widget-list-accel-closures
  input parameter self :: <GtkWidget>;
  result res :: <GList>;
  c-name: "gtk_widget_list_accel_closures";
end;

define C-function gtk-widget-list-mnemonic-labels
  input parameter self :: <GtkWidget>;
  result res :: <GList>;
  c-name: "gtk_widget_list_mnemonic_labels";
end;

define C-function gtk-widget-map
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_map";
end;

define C-function gtk-widget-mnemonic-activate
  input parameter self :: <GtkWidget>;
  input parameter group_cycling_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_mnemonic_activate";
end;

define C-function gtk-widget-modify-base
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateType>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_widget_modify_base";
end;

define C-function gtk-widget-modify-bg
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateType>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_widget_modify_bg";
end;

define C-function gtk-widget-modify-cursor
  input parameter self :: <GtkWidget>;
  input parameter primary_ :: <GdkColor>;
  input parameter secondary_ :: <GdkColor>;
  c-name: "gtk_widget_modify_cursor";
end;

define C-function gtk-widget-modify-fg
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateType>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_widget_modify_fg";
end;

define C-function gtk-widget-modify-font
  input parameter self :: <GtkWidget>;
  input parameter font_desc_ :: <PangoFontDescription>;
  c-name: "gtk_widget_modify_font";
end;

define C-function gtk-widget-modify-style
  input parameter self :: <GtkWidget>;
  input parameter style_ :: <GtkRcStyle>;
  c-name: "gtk_widget_modify_style";
end;

define C-function gtk-widget-modify-text
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateType>;
  input parameter color_ :: <GdkColor>;
  c-name: "gtk_widget_modify_text";
end;

define C-function gtk-widget-override-background-color
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateFlags>;
  input parameter color_ :: <GdkRGBA>;
  c-name: "gtk_widget_override_background_color";
end;

define C-function gtk-widget-override-color
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateFlags>;
  input parameter color_ :: <GdkRGBA>;
  c-name: "gtk_widget_override_color";
end;

define C-function gtk-widget-override-cursor
  input parameter self :: <GtkWidget>;
  input parameter cursor_ :: <GdkRGBA>;
  input parameter secondary_cursor_ :: <GdkRGBA>;
  c-name: "gtk_widget_override_cursor";
end;

define C-function gtk-widget-override-font
  input parameter self :: <GtkWidget>;
  input parameter font_desc_ :: <PangoFontDescription>;
  c-name: "gtk_widget_override_font";
end;

define C-function gtk-widget-override-symbolic-color
  input parameter self :: <GtkWidget>;
  input parameter name_ :: <C-string>;
  input parameter color_ :: <GdkRGBA>;
  c-name: "gtk_widget_override_symbolic_color";
end;

define C-function gtk-widget-path
  input parameter self :: <GtkWidget>;
  output parameter path_length_ :: <C-unsigned-int*>;
  output parameter path_ :: <C-string>;
  output parameter path_reversed_ :: <C-string>;
  c-name: "gtk_widget_path";
end;

define C-function gtk-widget-queue-compute-expand
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_queue_compute_expand";
end;

define C-function gtk-widget-queue-draw
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_queue_draw";
end;

define C-function gtk-widget-queue-draw-area
  input parameter self :: <GtkWidget>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_widget_queue_draw_area";
end;

define C-function gtk-widget-queue-draw-region
  input parameter self :: <GtkWidget>;
  input parameter region_ :: <cairoRegion>;
  c-name: "gtk_widget_queue_draw_region";
end;

define C-function gtk-widget-queue-resize
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_queue_resize";
end;

define C-function gtk-widget-queue-resize-no-redraw
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_queue_resize_no_redraw";
end;

define C-function gtk-widget-realize
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_realize";
end;

define C-function gtk-widget-region-intersect
  input parameter self :: <GtkWidget>;
  input parameter region_ :: <cairoRegion>;
  result res :: <cairoRegion>;
  c-name: "gtk_widget_region_intersect";
end;

define C-function gtk-widget-remove-accelerator
  input parameter self :: <GtkWidget>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_remove_accelerator";
end;

define C-function gtk-widget-remove-mnemonic-label
  input parameter self :: <GtkWidget>;
  input parameter label_ :: <GtkWidget>;
  c-name: "gtk_widget_remove_mnemonic_label";
end;

define C-function gtk-widget-render-icon
  input parameter self :: <GtkWidget>;
  input parameter stock_id_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  input parameter detail_ :: <C-string>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_widget_render_icon";
end;

define C-function gtk-widget-render-icon-pixbuf
  input parameter self :: <GtkWidget>;
  input parameter stock_id_ :: <C-string>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_widget_render_icon_pixbuf";
end;

define C-function gtk-widget-reparent
  input parameter self :: <GtkWidget>;
  input parameter new_parent_ :: <GtkWidget>;
  c-name: "gtk_widget_reparent";
end;

define C-function gtk-widget-reset-rc-styles
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_reset_rc_styles";
end;

define C-function gtk-widget-reset-style
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_reset_style";
end;

define C-function gtk-widget-send-expose
  input parameter self :: <GtkWidget>;
  input parameter event_ :: <GdkEvent>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_send_expose";
end;

define C-function gtk-widget-send-focus-change
  input parameter self :: <GtkWidget>;
  input parameter event_ :: <GdkEvent>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_send_focus_change";
end;

define C-function gtk-widget-set-accel-path
  input parameter self :: <GtkWidget>;
  input parameter accel_path_ :: <C-string>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_widget_set_accel_path";
end;

define C-function gtk-widget-set-allocation
  input parameter self :: <GtkWidget>;
  input parameter allocation_ :: <cairoRectangleInt>;
  c-name: "gtk_widget_set_allocation";
end;

define C-function gtk-widget-set-app-paintable
  input parameter self :: <GtkWidget>;
  input parameter app_paintable_ :: <C-boolean>;
  c-name: "gtk_widget_set_app_paintable";
end;

define C-function gtk-widget-set-can-default
  input parameter self :: <GtkWidget>;
  input parameter can_default_ :: <C-boolean>;
  c-name: "gtk_widget_set_can_default";
end;

define C-function gtk-widget-set-can-focus
  input parameter self :: <GtkWidget>;
  input parameter can_focus_ :: <C-boolean>;
  c-name: "gtk_widget_set_can_focus";
end;

define C-function gtk-widget-set-child-visible
  input parameter self :: <GtkWidget>;
  input parameter is_visible_ :: <C-boolean>;
  c-name: "gtk_widget_set_child_visible";
end;

define C-function gtk-widget-set-composite-name
  input parameter self :: <GtkWidget>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_set_composite_name";
end;

define C-function gtk-widget-set-device-enabled
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  input parameter enabled_ :: <C-boolean>;
  c-name: "gtk_widget_set_device_enabled";
end;

define C-function gtk-widget-set-device-events
  input parameter self :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  input parameter events_ :: <GdkEventMask>;
  c-name: "gtk_widget_set_device_events";
end;

define C-function gtk-widget-set-direction
  input parameter self :: <GtkWidget>;
  input parameter dir_ :: <GtkTextDirection>;
  c-name: "gtk_widget_set_direction";
end;

define C-function gtk-widget-set-double-buffered
  input parameter self :: <GtkWidget>;
  input parameter double_buffered_ :: <C-boolean>;
  c-name: "gtk_widget_set_double_buffered";
end;

define C-function gtk-widget-set-events
  input parameter self :: <GtkWidget>;
  input parameter events_ :: <C-signed-int>;
  c-name: "gtk_widget_set_events";
end;

define C-function gtk-widget-set-halign
  input parameter self :: <GtkWidget>;
  input parameter align_ :: <GtkAlign>;
  c-name: "gtk_widget_set_halign";
end;

define C-function gtk-widget-set-has-tooltip
  input parameter self :: <GtkWidget>;
  input parameter has_tooltip_ :: <C-boolean>;
  c-name: "gtk_widget_set_has_tooltip";
end;

define C-function gtk-widget-set-has-window
  input parameter self :: <GtkWidget>;
  input parameter has_window_ :: <C-boolean>;
  c-name: "gtk_widget_set_has_window";
end;

define C-function gtk-widget-set-hexpand
  input parameter self :: <GtkWidget>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_widget_set_hexpand";
end;

define C-function gtk-widget-set-hexpand-set
  input parameter self :: <GtkWidget>;
  input parameter set_ :: <C-boolean>;
  c-name: "gtk_widget_set_hexpand_set";
end;

define C-function gtk-widget-set-mapped
  input parameter self :: <GtkWidget>;
  input parameter mapped_ :: <C-boolean>;
  c-name: "gtk_widget_set_mapped";
end;

define C-function gtk-widget-set-margin-bottom
  input parameter self :: <GtkWidget>;
  input parameter margin_ :: <C-signed-int>;
  c-name: "gtk_widget_set_margin_bottom";
end;

define C-function gtk-widget-set-margin-left
  input parameter self :: <GtkWidget>;
  input parameter margin_ :: <C-signed-int>;
  c-name: "gtk_widget_set_margin_left";
end;

define C-function gtk-widget-set-margin-right
  input parameter self :: <GtkWidget>;
  input parameter margin_ :: <C-signed-int>;
  c-name: "gtk_widget_set_margin_right";
end;

define C-function gtk-widget-set-margin-top
  input parameter self :: <GtkWidget>;
  input parameter margin_ :: <C-signed-int>;
  c-name: "gtk_widget_set_margin_top";
end;

define C-function gtk-widget-set-name
  input parameter self :: <GtkWidget>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_set_name";
end;

define C-function gtk-widget-set-no-show-all
  input parameter self :: <GtkWidget>;
  input parameter no_show_all_ :: <C-boolean>;
  c-name: "gtk_widget_set_no_show_all";
end;

define C-function gtk-widget-set-parent
  input parameter self :: <GtkWidget>;
  input parameter parent_ :: <GtkWidget>;
  c-name: "gtk_widget_set_parent";
end;

define C-function gtk-widget-set-parent-window
  input parameter self :: <GtkWidget>;
  input parameter parent_window_ :: <GdkWindow>;
  c-name: "gtk_widget_set_parent_window";
end;

define C-function gtk-widget-set-realized
  input parameter self :: <GtkWidget>;
  input parameter realized_ :: <C-boolean>;
  c-name: "gtk_widget_set_realized";
end;

define C-function gtk-widget-set-receives-default
  input parameter self :: <GtkWidget>;
  input parameter receives_default_ :: <C-boolean>;
  c-name: "gtk_widget_set_receives_default";
end;

define C-function gtk-widget-set-redraw-on-allocate
  input parameter self :: <GtkWidget>;
  input parameter redraw_on_allocate_ :: <C-boolean>;
  c-name: "gtk_widget_set_redraw_on_allocate";
end;

define C-function gtk-widget-set-sensitive
  input parameter self :: <GtkWidget>;
  input parameter sensitive_ :: <C-boolean>;
  c-name: "gtk_widget_set_sensitive";
end;

define C-function gtk-widget-set-size-request
  input parameter self :: <GtkWidget>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_widget_set_size_request";
end;

define C-function gtk-widget-set-state
  input parameter self :: <GtkWidget>;
  input parameter state_ :: <GtkStateType>;
  c-name: "gtk_widget_set_state";
end;

define C-function gtk-widget-set-state-flags
  input parameter self :: <GtkWidget>;
  input parameter flags_ :: <GtkStateFlags>;
  input parameter clear_ :: <C-boolean>;
  c-name: "gtk_widget_set_state_flags";
end;

define C-function gtk-widget-set-style
  input parameter self :: <GtkWidget>;
  input parameter style_ :: <GtkStyle>;
  c-name: "gtk_widget_set_style";
end;

define C-function gtk-widget-set-support-multidevice
  input parameter self :: <GtkWidget>;
  input parameter support_multidevice_ :: <C-boolean>;
  c-name: "gtk_widget_set_support_multidevice";
end;

define C-function gtk-widget-set-tooltip-markup
  input parameter self :: <GtkWidget>;
  input parameter markup_ :: <C-string>;
  c-name: "gtk_widget_set_tooltip_markup";
end;

define C-function gtk-widget-set-tooltip-text
  input parameter self :: <GtkWidget>;
  input parameter text_ :: <C-string>;
  c-name: "gtk_widget_set_tooltip_text";
end;

define C-function gtk-widget-set-tooltip-window
  input parameter self :: <GtkWidget>;
  input parameter custom_window_ :: <GtkWindow>;
  c-name: "gtk_widget_set_tooltip_window";
end;

define C-function gtk-widget-set-valign
  input parameter self :: <GtkWidget>;
  input parameter align_ :: <GtkAlign>;
  c-name: "gtk_widget_set_valign";
end;

define C-function gtk-widget-set-vexpand
  input parameter self :: <GtkWidget>;
  input parameter expand_ :: <C-boolean>;
  c-name: "gtk_widget_set_vexpand";
end;

define C-function gtk-widget-set-vexpand-set
  input parameter self :: <GtkWidget>;
  input parameter set_ :: <C-boolean>;
  c-name: "gtk_widget_set_vexpand_set";
end;

define C-function gtk-widget-set-visible
  input parameter self :: <GtkWidget>;
  input parameter visible_ :: <C-boolean>;
  c-name: "gtk_widget_set_visible";
end;

define C-function gtk-widget-set-visual
  input parameter self :: <GtkWidget>;
  input parameter visual_ :: <GdkVisual>;
  c-name: "gtk_widget_set_visual";
end;

define C-function gtk-widget-set-window
  input parameter self :: <GtkWidget>;
  input parameter window_ :: <GdkWindow>;
  c-name: "gtk_widget_set_window";
end;

define C-function gtk-widget-shape-combine-region
  input parameter self :: <GtkWidget>;
  input parameter region_ :: <cairoRegion>;
  c-name: "gtk_widget_shape_combine_region";
end;

define C-function gtk-widget-show
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_show";
end;

define C-function gtk-widget-show-all
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_show_all";
end;

define C-function gtk-widget-show-now
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_show_now";
end;

define C-function gtk-widget-size-allocate
  input parameter self :: <GtkWidget>;
  input parameter allocation_ :: <cairoRectangleInt>;
  c-name: "gtk_widget_size_allocate";
end;

define C-function gtk-widget-size-request
  input parameter self :: <GtkWidget>;
  output parameter requisition_ :: <GtkRequisition>;
  c-name: "gtk_widget_size_request";
end;

define C-function gtk-widget-style-attach
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_style_attach";
end;

define C-function gtk-widget-style-get-property
  input parameter self :: <GtkWidget>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  c-name: "gtk_widget_style_get_property";
end;

define C-function gtk-widget-thaw-child-notify
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_thaw_child_notify";
end;

define C-function gtk-widget-translate-coordinates
  input parameter self :: <GtkWidget>;
  input parameter dest_widget_ :: <GtkWidget>;
  input parameter src_x_ :: <C-signed-int>;
  input parameter src_y_ :: <C-signed-int>;
  output parameter dest_x_ :: <C-signed-int*>;
  output parameter dest_y_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_translate_coordinates";
end;

define C-function gtk-widget-trigger-tooltip-query
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_trigger_tooltip_query";
end;

define C-function gtk-widget-unmap
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_unmap";
end;

define C-function gtk-widget-unparent
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_unparent";
end;

define C-function gtk-widget-unrealize
  input parameter self :: <GtkWidget>;
  c-name: "gtk_widget_unrealize";
end;

define C-function gtk-widget-unset-state-flags
  input parameter self :: <GtkWidget>;
  input parameter flags_ :: <GtkStateFlags>;
  c-name: "gtk_widget_unset_state_flags";
end;

define C-struct <_GtkWidgetAuxInfo>
  slot gtk-widget-aux-info-width :: <C-signed-int>;
  slot gtk-widget-aux-info-height :: <C-signed-int>;
  slot gtk-widget-aux-info-halign :: <C-unsigned-int>;
  slot gtk-widget-aux-info-valign :: <C-unsigned-int>;
  slot gtk-widget-aux-info-margin :: <GtkBorder>;
  pointer-type-name: <GtkWidgetAuxInfo>;
end C-struct;

define C-struct <_GtkWidgetClass>
  constant slot gtk-widget-class-parent-class :: <GInitiallyUnownedClass>;
  constant slot gtk-widget-class-activate-signal :: <C-unsigned-int>;
  constant slot gtk-widget-class-dispatch-child-properties-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-destroy :: <C-function-pointer>;
  constant slot gtk-widget-class-show :: <C-function-pointer>;
  constant slot gtk-widget-class-show-all :: <C-function-pointer>;
  constant slot gtk-widget-class-hide :: <C-function-pointer>;
  constant slot gtk-widget-class-map :: <C-function-pointer>;
  constant slot gtk-widget-class-unmap :: <C-function-pointer>;
  constant slot gtk-widget-class-realize :: <C-function-pointer>;
  constant slot gtk-widget-class-unrealize :: <C-function-pointer>;
  constant slot gtk-widget-class-size-allocate :: <C-function-pointer>;
  constant slot gtk-widget-class-state-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-state-flags-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-parent-set :: <C-function-pointer>;
  constant slot gtk-widget-class-hierarchy-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-style-set :: <C-function-pointer>;
  constant slot gtk-widget-class-direction-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-grab-notify :: <C-function-pointer>;
  constant slot gtk-widget-class-child-notify :: <C-function-pointer>;
  constant slot gtk-widget-class-draw :: <C-function-pointer>;
  constant slot gtk-widget-class-get-request-mode :: <C-function-pointer>;
  constant slot gtk-widget-class-get-preferred-height :: <C-function-pointer>;
  constant slot gtk-widget-class-get-preferred-width-for-height :: <C-function-pointer>;
  constant slot gtk-widget-class-get-preferred-width :: <C-function-pointer>;
  constant slot gtk-widget-class-get-preferred-height-for-width :: <C-function-pointer>;
  constant slot gtk-widget-class-mnemonic-activate :: <C-function-pointer>;
  constant slot gtk-widget-class-grab-focus :: <C-function-pointer>;
  constant slot gtk-widget-class-focus :: <C-function-pointer>;
  constant slot gtk-widget-class-move-focus :: <C-function-pointer>;
  constant slot gtk-widget-class-keynav-failed :: <C-function-pointer>;
  constant slot gtk-widget-class-event :: <C-function-pointer>;
  constant slot gtk-widget-class-button-press-event :: <C-function-pointer>;
  constant slot gtk-widget-class-button-release-event :: <C-function-pointer>;
  constant slot gtk-widget-class-scroll-event :: <C-function-pointer>;
  constant slot gtk-widget-class-motion-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-delete-event :: <C-function-pointer>;
  constant slot gtk-widget-class-destroy-event :: <C-function-pointer>;
  constant slot gtk-widget-class-key-press-event :: <C-function-pointer>;
  constant slot gtk-widget-class-key-release-event :: <C-function-pointer>;
  constant slot gtk-widget-class-enter-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-leave-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-configure-event :: <C-function-pointer>;
  constant slot gtk-widget-class-focus-in-event :: <C-function-pointer>;
  constant slot gtk-widget-class-focus-out-event :: <C-function-pointer>;
  constant slot gtk-widget-class-map-event :: <C-function-pointer>;
  constant slot gtk-widget-class-unmap-event :: <C-function-pointer>;
  constant slot gtk-widget-class-property-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-selection-clear-event :: <C-function-pointer>;
  constant slot gtk-widget-class-selection-request-event :: <C-function-pointer>;
  constant slot gtk-widget-class-selection-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-proximity-in-event :: <C-function-pointer>;
  constant slot gtk-widget-class-proximity-out-event :: <C-function-pointer>;
  constant slot gtk-widget-class-visibility-notify-event :: <C-function-pointer>;
  constant slot gtk-widget-class-window-state-event :: <C-function-pointer>;
  constant slot gtk-widget-class-damage-event :: <C-function-pointer>;
  constant slot gtk-widget-class-grab-broken-event :: <C-function-pointer>;
  constant slot gtk-widget-class-selection-get :: <C-function-pointer>;
  constant slot gtk-widget-class-selection-received :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-begin :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-end :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-data-get :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-data-delete :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-leave :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-motion :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-drop :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-data-received :: <C-function-pointer>;
  constant slot gtk-widget-class-drag-failed :: <C-function-pointer>;
  constant slot gtk-widget-class-popup-menu :: <C-function-pointer>;
  constant slot gtk-widget-class-show-help :: <C-function-pointer>;
  constant slot gtk-widget-class-get-accessible :: <C-function-pointer>;
  constant slot gtk-widget-class-screen-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-can-activate-accel :: <C-function-pointer>;
  constant slot gtk-widget-class-composited-changed :: <C-function-pointer>;
  constant slot gtk-widget-class-query-tooltip :: <C-function-pointer>;
  constant slot gtk-widget-class-compute-expand :: <C-function-pointer>;
  constant slot gtk-widget-class-adjust-size-request :: <C-function-pointer>;
  constant slot gtk-widget-class-adjust-size-allocation :: <C-function-pointer>;
  constant slot gtk-widget-class-style-updated :: <C-function-pointer>;
  constant slot gtk-widget-class-touch-event :: <C-function-pointer>;
  constant slot gtk-widget-class-priv :: <GtkWidgetClassPrivate>;
  constant slot gtk-widget-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-widget-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-widget-class-_gtk-reserved4 :: <C-void*>;
  constant slot gtk-widget-class-_gtk-reserved5 :: <C-void*>;
  constant slot gtk-widget-class-_gtk-reserved6 :: <C-void*>;
  constant slot gtk-widget-class-_gtk-reserved7 :: <C-void*>;
  pointer-type-name: <GtkWidgetClass>;
end C-struct;

define C-function gtk-widget-class-find-style-property
  input parameter self :: <GtkWidgetClass>;
  input parameter property_name_ :: <C-string>;
  result res :: <GParamSpec>;
  c-name: "gtk_widget_class_find_style_property";
end;

define C-function gtk-widget-class-install-style-property
  input parameter self :: <GtkWidgetClass>;
  input parameter pspec_ :: <GParamSpec>;
  c-name: "gtk_widget_class_install_style_property";
end;

define C-function gtk-widget-class-list-style-properties
  input parameter self :: <GtkWidgetClass>;
  output parameter n_properties_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_widget_class_list_style_properties";
end;

define C-function gtk-widget-class-set-accessible-role
  input parameter self :: <GtkWidgetClass>;
  input parameter role_ :: <AtkRole>;
  c-name: "gtk_widget_class_set_accessible_role";
end;

define C-function gtk-widget-class-set-accessible-type
  input parameter self :: <GtkWidgetClass>;
  input parameter type_ :: <C-long>;
  c-name: "gtk_widget_class_set_accessible_type";
end;

define C-struct <_GtkWidgetClassPrivate>
  pointer-type-name: <GtkWidgetClassPrivate>;
end C-struct;

define constant $gtk-widget-help-tooltip = 0;
define constant $gtk-widget-help-whats-this = 1;
define constant <GtkWidgetHelpType> = <C-int>;
define C-pointer-type <GtkWidgetHelpType*> => <GtkWidgetHelpType>;

define C-struct <_GtkWidgetPath>
  pointer-type-name: <GtkWidgetPath>;
end C-struct;

define C-function gtk-widget-path-new
  result res :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_new";
end;

define C-function gtk-widget-path-append-for-widget
  input parameter self :: <GtkWidgetPath>;
  input parameter widget_ :: <GtkWidget>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_path_append_for_widget";
end;

define C-function gtk-widget-path-append-type
  input parameter self :: <GtkWidgetPath>;
  input parameter type_ :: <C-long>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_path_append_type";
end;

define C-function gtk-widget-path-append-with-siblings
  input parameter self :: <GtkWidgetPath>;
  input parameter siblings_ :: <GtkWidgetPath>;
  input parameter sibling_index_ :: <C-unsigned-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_path_append_with_siblings";
end;

define C-function gtk-widget-path-copy
  input parameter self :: <GtkWidgetPath>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_copy";
end;

define C-function gtk-widget-path-free
  input parameter self :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_free";
end;

define C-function gtk-widget-path-get-object-type
  input parameter self :: <GtkWidgetPath>;
  result res :: <C-long>;
  c-name: "gtk_widget_path_get_object_type";
end;

define C-function gtk-widget-path-has-parent
  input parameter self :: <GtkWidgetPath>;
  input parameter type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_has_parent";
end;

define C-function gtk-widget-path-is-type
  input parameter self :: <GtkWidgetPath>;
  input parameter type_ :: <C-long>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_is_type";
end;

define C-function gtk-widget-path-iter-add-class
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_path_iter_add_class";
end;

define C-function gtk-widget-path-iter-add-region
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  input parameter flags_ :: <GtkRegionFlags>;
  c-name: "gtk_widget_path_iter_add_region";
end;

define C-function gtk-widget-path-iter-clear-classes
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  c-name: "gtk_widget_path_iter_clear_classes";
end;

define C-function gtk-widget-path-iter-clear-regions
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  c-name: "gtk_widget_path_iter_clear_regions";
end;

define C-function gtk-widget-path-iter-get-name
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "gtk_widget_path_iter_get_name";
end;

define C-function gtk-widget-path-iter-get-object-type
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <C-long>;
  c-name: "gtk_widget_path_iter_get_object_type";
end;

define C-function gtk-widget-path-iter-get-sibling-index
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_widget_path_iter_get_sibling_index";
end;

define C-function gtk-widget-path-iter-get-siblings
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_iter_get_siblings";
end;

define C-function gtk-widget-path-iter-has-class
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_class";
end;

define C-function gtk-widget-path-iter-has-name
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_name";
end;

define C-function gtk-widget-path-iter-has-qclass
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter qname_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_qclass";
end;

define C-function gtk-widget-path-iter-has-qname
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter qname_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_qname";
end;

define C-function gtk-widget-path-iter-has-qregion
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter qname_ :: <C-unsigned-int>;
  output parameter flags_ :: <GtkRegionFlags*>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_qregion";
end;

define C-function gtk-widget-path-iter-has-region
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  output parameter flags_ :: <GtkRegionFlags*>;
  result res :: <C-boolean>;
  c-name: "gtk_widget_path_iter_has_region";
end;

define C-function gtk-widget-path-iter-list-classes
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <GSList>;
  c-name: "gtk_widget_path_iter_list_classes";
end;

define C-function gtk-widget-path-iter-list-regions
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  result res :: <GSList>;
  c-name: "gtk_widget_path_iter_list_regions";
end;

define C-function gtk-widget-path-iter-remove-class
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_path_iter_remove_class";
end;

define C-function gtk-widget-path-iter-remove-region
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_path_iter_remove_region";
end;

define C-function gtk-widget-path-iter-set-name
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_widget_path_iter_set_name";
end;

define C-function gtk-widget-path-iter-set-object-type
  input parameter self :: <GtkWidgetPath>;
  input parameter pos_ :: <C-signed-int>;
  input parameter type_ :: <C-long>;
  c-name: "gtk_widget_path_iter_set_object_type";
end;

define C-function gtk-widget-path-length
  input parameter self :: <GtkWidgetPath>;
  result res :: <C-signed-int>;
  c-name: "gtk_widget_path_length";
end;

define C-function gtk-widget-path-prepend-type
  input parameter self :: <GtkWidgetPath>;
  input parameter type_ :: <C-long>;
  c-name: "gtk_widget_path_prepend_type";
end;

define C-function gtk-widget-path-ref
  input parameter self :: <GtkWidgetPath>;
  result res :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_ref";
end;

define C-function gtk-widget-path-to-string
  input parameter self :: <GtkWidgetPath>;
  result res :: <C-string>;
  c-name: "gtk_widget_path_to_string";
end;

define C-function gtk-widget-path-unref
  input parameter self :: <GtkWidgetPath>;
  c-name: "gtk_widget_path_unref";
end;

define C-struct <_GtkWidgetPrivate>
  pointer-type-name: <GtkWidgetPrivate>;
end C-struct;

define open C-subtype <GtkWindow> (<GtkBin>)
  constant slot gtk-window-bin :: <GtkBin>;
  constant slot gtk-window-priv :: <GtkWindowPrivate>;
end C-subtype;

define C-pointer-type <GtkWindow*> => <GtkWindow>;

define C-function gtk-window-new
  input parameter type_ :: <GtkWindowType>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_new";
end;

define C-function gtk-window-get-default-icon-list
  result res :: <GList>;
  c-name: "gtk_window_get_default_icon_list";
end;

define C-function gtk-window-get-default-icon-name
  result res :: <C-string>;
  c-name: "gtk_window_get_default_icon_name";
end;

define C-function gtk-window-list-toplevels
  result res :: <GList>;
  c-name: "gtk_window_list_toplevels";
end;

define C-function gtk-window-set-auto-startup-notification
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_auto_startup_notification";
end;

define C-function gtk-window-set-default-icon
  input parameter icon_ :: <GdkPixbuf>;
  c-name: "gtk_window_set_default_icon";
end;

define C-function gtk-window-set-default-icon-from-file
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_window_set_default_icon_from_file";
end;

define C-function gtk-window-set-default-icon-list
  input parameter list_ :: <GList>;
  c-name: "gtk_window_set_default_icon_list";
end;

define C-function gtk-window-set-default-icon-name
  input parameter name_ :: <C-string>;
  c-name: "gtk_window_set_default_icon_name";
end;

define C-function gtk-window-activate-default
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_activate_default";
end;

define C-function gtk-window-activate-focus
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_activate_focus";
end;

define C-function gtk-window-activate-key
  input parameter self :: <GtkWindow>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_window_activate_key";
end;

define C-function gtk-window-add-accel-group
  input parameter self :: <GtkWindow>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_window_add_accel_group";
end;

define C-function gtk-window-add-mnemonic
  input parameter self :: <GtkWindow>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter target_ :: <GtkWidget>;
  c-name: "gtk_window_add_mnemonic";
end;

define C-function gtk-window-begin-move-drag
  input parameter self :: <GtkWindow>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gtk_window_begin_move_drag";
end;

define C-function gtk-window-begin-resize-drag
  input parameter self :: <GtkWindow>;
  input parameter edge_ :: <GdkWindowEdge>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gtk_window_begin_resize_drag";
end;

define C-function gtk-window-deiconify
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_deiconify";
end;

define C-function gtk-window-fullscreen
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_fullscreen";
end;

define C-function gtk-window-get-accept-focus
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_accept_focus";
end;

define C-function gtk-window-get-application
  input parameter self :: <GtkWindow>;
  result res :: <GtkApplication>;
  c-name: "gtk_window_get_application";
end;

define C-function gtk-window-get-attached-to
  input parameter self :: <GtkWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_get_attached_to";
end;

define C-function gtk-window-get-decorated
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_decorated";
end;

define C-function gtk-window-get-default-size
  input parameter self :: <GtkWindow>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_window_get_default_size";
end;

define C-function gtk-window-get-default-widget
  input parameter self :: <GtkWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_get_default_widget";
end;

define C-function gtk-window-get-deletable
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_deletable";
end;

define C-function gtk-window-get-destroy-with-parent
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_destroy_with_parent";
end;

define C-function gtk-window-get-focus
  input parameter self :: <GtkWindow>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_get_focus";
end;

define C-function gtk-window-get-focus-on-map
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_focus_on_map";
end;

define C-function gtk-window-get-focus-visible
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_focus_visible";
end;

define C-function gtk-window-get-gravity
  input parameter self :: <GtkWindow>;
  result res :: <GdkGravity>;
  c-name: "gtk_window_get_gravity";
end;

define C-function gtk-window-get-group
  input parameter self :: <GtkWindow>;
  result res :: <GtkWindowGroup>;
  c-name: "gtk_window_get_group";
end;

define C-function gtk-window-get-has-resize-grip
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_has_resize_grip";
end;

define C-function gtk-window-get-hide-titlebar-when-maximized
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_hide_titlebar_when_maximized";
end;

define C-function gtk-window-get-icon
  input parameter self :: <GtkWindow>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_window_get_icon";
end;

define C-function gtk-window-get-icon-list
  input parameter self :: <GtkWindow>;
  result res :: <GList>;
  c-name: "gtk_window_get_icon_list";
end;

define C-function gtk-window-get-icon-name
  input parameter self :: <GtkWindow>;
  result res :: <C-string>;
  c-name: "gtk_window_get_icon_name";
end;

define C-function gtk-window-get-mnemonic-modifier
  input parameter self :: <GtkWindow>;
  result res :: <GdkModifierType>;
  c-name: "gtk_window_get_mnemonic_modifier";
end;

define C-function gtk-window-get-mnemonics-visible
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_mnemonics_visible";
end;

define C-function gtk-window-get-modal
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_modal";
end;

define C-function gtk-window-get-opacity
  input parameter self :: <GtkWindow>;
  result res :: <C-double>;
  c-name: "gtk_window_get_opacity";
end;

define C-function gtk-window-get-position
  input parameter self :: <GtkWindow>;
  output parameter root_x_ :: <C-signed-int*>;
  output parameter root_y_ :: <C-signed-int*>;
  c-name: "gtk_window_get_position";
end;

define C-function gtk-window-get-resizable
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_resizable";
end;

define C-function gtk-window-get-resize-grip-area
  input parameter self :: <GtkWindow>;
  output parameter rect_ :: <cairoRectangleInt>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_resize_grip_area";
end;

define C-function gtk-window-get-role
  input parameter self :: <GtkWindow>;
  result res :: <C-string>;
  c-name: "gtk_window_get_role";
end;

define C-function gtk-window-get-screen
  input parameter self :: <GtkWindow>;
  result res :: <GdkScreen>;
  c-name: "gtk_window_get_screen";
end;

define C-function gtk-window-get-size
  input parameter self :: <GtkWindow>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gtk_window_get_size";
end;

define C-function gtk-window-get-skip-pager-hint
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_skip_pager_hint";
end;

define C-function gtk-window-get-skip-taskbar-hint
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_skip_taskbar_hint";
end;

define C-function gtk-window-get-title
  input parameter self :: <GtkWindow>;
  result res :: <C-string>;
  c-name: "gtk_window_get_title";
end;

define C-function gtk-window-get-transient-for
  input parameter self :: <GtkWindow>;
  result res :: <GtkWindow>;
  c-name: "gtk_window_get_transient_for";
end;

define C-function gtk-window-get-type-hint
  input parameter self :: <GtkWindow>;
  result res :: <GdkWindowTypeHint>;
  c-name: "gtk_window_get_type_hint";
end;

define C-function gtk-window-get-urgency-hint
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_get_urgency_hint";
end;

define C-function gtk-window-get-window-type
  input parameter self :: <GtkWindow>;
  result res :: <GtkWindowType>;
  c-name: "gtk_window_get_window_type";
end;

define C-function gtk-window-has-group
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_has_group";
end;

define C-function gtk-window-has-toplevel-focus
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_has_toplevel_focus";
end;

define C-function gtk-window-iconify
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_iconify";
end;

define C-function gtk-window-is-active
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_is_active";
end;

define C-function gtk-window-maximize
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_maximize";
end;

define C-function gtk-window-mnemonic-activate
  input parameter self :: <GtkWindow>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifier_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_window_mnemonic_activate";
end;

define C-function gtk-window-move
  input parameter self :: <GtkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_window_move";
end;

define C-function gtk-window-parse-geometry
  input parameter self :: <GtkWindow>;
  input parameter geometry_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gtk_window_parse_geometry";
end;

define C-function gtk-window-present
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_present";
end;

define C-function gtk-window-present-with-time
  input parameter self :: <GtkWindow>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gtk_window_present_with_time";
end;

define C-function gtk-window-propagate-key-event
  input parameter self :: <GtkWindow>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_window_propagate_key_event";
end;

define C-function gtk-window-remove-accel-group
  input parameter self :: <GtkWindow>;
  input parameter accel_group_ :: <GtkAccelGroup>;
  c-name: "gtk_window_remove_accel_group";
end;

define C-function gtk-window-remove-mnemonic
  input parameter self :: <GtkWindow>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter target_ :: <GtkWidget>;
  c-name: "gtk_window_remove_mnemonic";
end;

define C-function gtk-window-reshow-with-initial-size
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_reshow_with_initial_size";
end;

define C-function gtk-window-resize
  input parameter self :: <GtkWindow>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_window_resize";
end;

define C-function gtk-window-resize-grip-is-visible
  input parameter self :: <GtkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_window_resize_grip_is_visible";
end;

define C-function gtk-window-resize-to-geometry
  input parameter self :: <GtkWindow>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_window_resize_to_geometry";
end;

define C-function gtk-window-set-accept-focus
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_accept_focus";
end;

define C-function gtk-window-set-application
  input parameter self :: <GtkWindow>;
  input parameter application_ :: <GtkApplication>;
  c-name: "gtk_window_set_application";
end;

define C-function gtk-window-set-attached-to
  input parameter self :: <GtkWindow>;
  input parameter attach_widget_ :: <GtkWidget>;
  c-name: "gtk_window_set_attached_to";
end;

define C-function gtk-window-set-decorated
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_decorated";
end;

define C-function gtk-window-set-default
  input parameter self :: <GtkWindow>;
  input parameter default_widget_ :: <GtkWidget>;
  c-name: "gtk_window_set_default";
end;

define C-function gtk-window-set-default-geometry
  input parameter self :: <GtkWindow>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_window_set_default_geometry";
end;

define C-function gtk-window-set-default-size
  input parameter self :: <GtkWindow>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_window_set_default_size";
end;

define C-function gtk-window-set-deletable
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_deletable";
end;

define C-function gtk-window-set-destroy-with-parent
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_destroy_with_parent";
end;

define C-function gtk-window-set-focus
  input parameter self :: <GtkWindow>;
  input parameter focus_ :: <GtkWidget>;
  c-name: "gtk_window_set_focus";
end;

define C-function gtk-window-set-focus-on-map
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_focus_on_map";
end;

define C-function gtk-window-set-focus-visible
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_focus_visible";
end;

define C-function gtk-window-set-geometry-hints
  input parameter self :: <GtkWindow>;
  input parameter geometry_widget_ :: <GtkWidget>;
  input parameter geometry_ :: <GdkGeometry>;
  input parameter geom_mask_ :: <GdkWindowHints>;
  c-name: "gtk_window_set_geometry_hints";
end;

define C-function gtk-window-set-gravity
  input parameter self :: <GtkWindow>;
  input parameter gravity_ :: <GdkGravity>;
  c-name: "gtk_window_set_gravity";
end;

define C-function gtk-window-set-has-resize-grip
  input parameter self :: <GtkWindow>;
  input parameter value_ :: <C-boolean>;
  c-name: "gtk_window_set_has_resize_grip";
end;

define C-function gtk-window-set-has-user-ref-count
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_has_user_ref_count";
end;

define C-function gtk-window-set-hide-titlebar-when-maximized
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_hide_titlebar_when_maximized";
end;

define C-function gtk-window-set-icon
  input parameter self :: <GtkWindow>;
  input parameter icon_ :: <GdkPixbuf>;
  c-name: "gtk_window_set_icon";
end;

define C-function gtk-window-set-icon-from-file
  input parameter self :: <GtkWindow>;
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_window_set_icon_from_file";
end;

define C-function gtk-window-set-icon-list
  input parameter self :: <GtkWindow>;
  input parameter list_ :: <GList>;
  c-name: "gtk_window_set_icon_list";
end;

define C-function gtk-window-set-icon-name
  input parameter self :: <GtkWindow>;
  input parameter name_ :: <C-string>;
  c-name: "gtk_window_set_icon_name";
end;

define C-function gtk-window-set-keep-above
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_keep_above";
end;

define C-function gtk-window-set-keep-below
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_keep_below";
end;

define C-function gtk-window-set-mnemonic-modifier
  input parameter self :: <GtkWindow>;
  input parameter modifier_ :: <GdkModifierType>;
  c-name: "gtk_window_set_mnemonic_modifier";
end;

define C-function gtk-window-set-mnemonics-visible
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_mnemonics_visible";
end;

define C-function gtk-window-set-modal
  input parameter self :: <GtkWindow>;
  input parameter modal_ :: <C-boolean>;
  c-name: "gtk_window_set_modal";
end;

define C-function gtk-window-set-opacity
  input parameter self :: <GtkWindow>;
  input parameter opacity_ :: <C-double>;
  c-name: "gtk_window_set_opacity";
end;

define C-function gtk-window-set-position
  input parameter self :: <GtkWindow>;
  input parameter position_ :: <GtkWindowPosition>;
  c-name: "gtk_window_set_position";
end;

define C-function gtk-window-set-resizable
  input parameter self :: <GtkWindow>;
  input parameter resizable_ :: <C-boolean>;
  c-name: "gtk_window_set_resizable";
end;

define C-function gtk-window-set-role
  input parameter self :: <GtkWindow>;
  input parameter role_ :: <C-string>;
  c-name: "gtk_window_set_role";
end;

define C-function gtk-window-set-screen
  input parameter self :: <GtkWindow>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gtk_window_set_screen";
end;

define C-function gtk-window-set-skip-pager-hint
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_skip_pager_hint";
end;

define C-function gtk-window-set-skip-taskbar-hint
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_skip_taskbar_hint";
end;

define C-function gtk-window-set-startup-id
  input parameter self :: <GtkWindow>;
  input parameter startup_id_ :: <C-string>;
  c-name: "gtk_window_set_startup_id";
end;

define C-function gtk-window-set-title
  input parameter self :: <GtkWindow>;
  input parameter title_ :: <C-string>;
  c-name: "gtk_window_set_title";
end;

define C-function gtk-window-set-transient-for
  input parameter self :: <GtkWindow>;
  input parameter parent_ :: <GtkWindow>;
  c-name: "gtk_window_set_transient_for";
end;

define C-function gtk-window-set-type-hint
  input parameter self :: <GtkWindow>;
  input parameter hint_ :: <GdkWindowTypeHint>;
  c-name: "gtk_window_set_type_hint";
end;

define C-function gtk-window-set-urgency-hint
  input parameter self :: <GtkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gtk_window_set_urgency_hint";
end;

define C-function gtk-window-set-wmclass
  input parameter self :: <GtkWindow>;
  input parameter wmclass_name_ :: <C-string>;
  input parameter wmclass_class_ :: <C-string>;
  c-name: "gtk_window_set_wmclass";
end;

define C-function gtk-window-stick
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_stick";
end;

define C-function gtk-window-unfullscreen
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_unfullscreen";
end;

define C-function gtk-window-unmaximize
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_unmaximize";
end;

define C-function gtk-window-unstick
  input parameter self :: <GtkWindow>;
  c-name: "gtk_window_unstick";
end;

define C-struct <_GtkWindowClass>
  constant slot gtk-window-class-parent-class :: <GtkBinClass>;
  constant slot gtk-window-class-set-focus :: <C-function-pointer>;
  constant slot gtk-window-class-activate-focus :: <C-function-pointer>;
  constant slot gtk-window-class-activate-default :: <C-function-pointer>;
  constant slot gtk-window-class-keys-changed :: <C-function-pointer>;
  constant slot gtk-window-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-window-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-window-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-window-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkWindowClass>;
end C-struct;

define C-struct <_GtkWindowGeometryInfo>
  pointer-type-name: <GtkWindowGeometryInfo>;
end C-struct;

define open C-subtype <GtkWindowGroup> (<GObject>)
  constant slot gtk-window-group-parent-instance :: <GObject>;
  constant slot gtk-window-group-priv :: <GtkWindowGroupPrivate>;
end C-subtype;

define C-pointer-type <GtkWindowGroup*> => <GtkWindowGroup>;

define C-function gtk-window-group-new
  result res :: <GtkWindowGroup>;
  c-name: "gtk_window_group_new";
end;

define C-function gtk-window-group-add-window
  input parameter self :: <GtkWindowGroup>;
  input parameter window_ :: <GtkWindow>;
  c-name: "gtk_window_group_add_window";
end;

define C-function gtk-window-group-get-current-device-grab
  input parameter self :: <GtkWindowGroup>;
  input parameter device_ :: <GdkDevice>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_group_get_current_device_grab";
end;

define C-function gtk-window-group-get-current-grab
  input parameter self :: <GtkWindowGroup>;
  result res :: <GtkWidget>;
  c-name: "gtk_window_group_get_current_grab";
end;

define C-function gtk-window-group-list-windows
  input parameter self :: <GtkWindowGroup>;
  result res :: <GList>;
  c-name: "gtk_window_group_list_windows";
end;

define C-function gtk-window-group-remove-window
  input parameter self :: <GtkWindowGroup>;
  input parameter window_ :: <GtkWindow>;
  c-name: "gtk_window_group_remove_window";
end;

define C-struct <_GtkWindowGroupClass>
  constant slot gtk-window-group-class-parent-class :: <GObjectClass>;
  constant slot gtk-window-group-class-_gtk-reserved1 :: <C-void*>;
  constant slot gtk-window-group-class-_gtk-reserved2 :: <C-void*>;
  constant slot gtk-window-group-class-_gtk-reserved3 :: <C-void*>;
  constant slot gtk-window-group-class-_gtk-reserved4 :: <C-void*>;
  pointer-type-name: <GtkWindowGroupClass>;
end C-struct;

define C-struct <_GtkWindowGroupPrivate>
  pointer-type-name: <GtkWindowGroupPrivate>;
end C-struct;

define constant $gtk-win-pos-none = 0;
define constant $gtk-win-pos-center = 1;
define constant $gtk-win-pos-mouse = 2;
define constant $gtk-win-pos-center-always = 3;
define constant $gtk-win-pos-center-on-parent = 4;
define constant <GtkWindowPosition> = <C-int>;
define C-pointer-type <GtkWindowPosition*> => <GtkWindowPosition>;

define C-struct <_GtkWindowPrivate>
  pointer-type-name: <GtkWindowPrivate>;
end C-struct;

define constant $gtk-window-toplevel = 0;
define constant $gtk-window-popup = 1;
define constant <GtkWindowType> = <C-int>;
define C-pointer-type <GtkWindowType*> => <GtkWindowType>;

define constant $gtk-wrap-none = 0;
define constant $gtk-wrap-char = 1;
define constant $gtk-wrap-word = 2;
define constant $gtk-wrap-word-char = 3;
define constant <GtkWrapMode> = <C-int>;
define C-pointer-type <GtkWrapMode*> => <GtkWrapMode>;

define C-struct <_Gtk_RcProperty>
  slot gtk-_rc-property-type-name :: <C-unsigned-int>;
  slot gtk-_rc-property-property-name :: <C-unsigned-int>;
  slot gtk-_rc-property-origin :: <C-string>;
  slot gtk-_rc-property-value :: <GValue>;
  pointer-type-name: <Gtk_RcProperty>;
end C-struct;

define C-function gtk-accel-groups-activate
  input parameter object_ :: <GObject>;
  input parameter accel_key_ :: <C-unsigned-int>;
  input parameter accel_mods_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_accel_groups_activate";
end;

define C-function gtk-accel-groups-from-object
  input parameter object_ :: <GObject>;
  result res :: <GSList>;
  c-name: "gtk_accel_groups_from_object";
end;

define C-function gtk-accelerator-get-default-mod-mask
  result res :: <GdkModifierType>;
  c-name: "gtk_accelerator_get_default_mod_mask";
end;

define C-function gtk-accelerator-get-label
  input parameter accelerator_key_ :: <C-unsigned-int>;
  input parameter accelerator_mods_ :: <GdkModifierType>;
  result res :: <C-string>;
  c-name: "gtk_accelerator_get_label";
end;

define C-function gtk-accelerator-get-label-with-keycode
  input parameter display_ :: <GdkDisplay>;
  input parameter accelerator_key_ :: <C-unsigned-int>;
  input parameter keycode_ :: <C-unsigned-int>;
  input parameter accelerator_mods_ :: <GdkModifierType>;
  result res :: <C-string>;
  c-name: "gtk_accelerator_get_label_with_keycode";
end;

define C-function gtk-accelerator-name
  input parameter accelerator_key_ :: <C-unsigned-int>;
  input parameter accelerator_mods_ :: <GdkModifierType>;
  result res :: <C-string>;
  c-name: "gtk_accelerator_name";
end;

define C-function gtk-accelerator-name-with-keycode
  input parameter display_ :: <GdkDisplay>;
  input parameter accelerator_key_ :: <C-unsigned-int>;
  input parameter keycode_ :: <C-unsigned-int>;
  input parameter accelerator_mods_ :: <GdkModifierType>;
  result res :: <C-string>;
  c-name: "gtk_accelerator_name_with_keycode";
end;

define C-function gtk-accelerator-parse
  input parameter accelerator_ :: <C-string>;
  output parameter accelerator_key_ :: <C-unsigned-int*>;
  output parameter accelerator_mods_ :: <GdkModifierType*>;
  c-name: "gtk_accelerator_parse";
end;

define C-function gtk-accelerator-parse-with-keycode
  input parameter accelerator_ :: <C-string>;
  output parameter accelerator_key_ :: <C-unsigned-int*>;
  output parameter accelerator_codes_ :: <C-unsigned-int*>;
  output parameter accelerator_mods_ :: <GdkModifierType*>;
  c-name: "gtk_accelerator_parse_with_keycode";
end;

define C-function gtk-accelerator-set-default-mod-mask
  input parameter default_mod_mask_ :: <GdkModifierType>;
  c-name: "gtk_accelerator_set_default_mod_mask";
end;

define C-function gtk-accelerator-valid
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_accelerator_valid";
end;

define C-function gtk-alternative-dialog-button-order
  input parameter screen_ :: <GdkScreen>;
  result res :: <C-boolean>;
  c-name: "gtk_alternative_dialog_button_order";
end;

define C-function gtk-bindings-activate
  input parameter object_ :: <GObject>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_bindings_activate";
end;

define C-function gtk-bindings-activate-event
  input parameter object_ :: <GObject>;
  input parameter event_ :: <GdkEventKey>;
  result res :: <C-boolean>;
  c-name: "gtk_bindings_activate_event";
end;

define C-function gtk-builder-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_builder_error_quark";
end;

define C-function gtk-cairo-should-draw-window
  input parameter cr_ :: <cairoContext>;
  input parameter window_ :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gtk_cairo_should_draw_window";
end;

define C-function gtk-cairo-transform-to-window
  input parameter cr_ :: <cairoContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter window_ :: <GdkWindow>;
  c-name: "gtk_cairo_transform_to_window";
end;

define C-function gtk-check-version
  input parameter required_major_ :: <C-unsigned-int>;
  input parameter required_minor_ :: <C-unsigned-int>;
  input parameter required_micro_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "gtk_check_version";
end;

define C-function gtk-css-provider-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_css_provider_error_quark";
end;

define C-function gtk-device-grab-add
  input parameter widget_ :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  input parameter block_others_ :: <C-boolean>;
  c-name: "gtk_device_grab_add";
end;

define C-function gtk-device-grab-remove
  input parameter widget_ :: <GtkWidget>;
  input parameter device_ :: <GdkDevice>;
  c-name: "gtk_device_grab_remove";
end;

define C-function gtk-disable-setlocale
  c-name: "gtk_disable_setlocale";
end;

define C-function gtk-distribute-natural-allocation
  input parameter extra_space_ :: <C-signed-int>;
  input parameter n_requested_sizes_ :: <C-unsigned-int>;
  input parameter sizes_ :: <GtkRequestedSize>;
  result res :: <C-signed-int>;
  c-name: "gtk_distribute_natural_allocation";
end;

define C-function gtk-drag-finish
  input parameter context_ :: <GdkDragContext>;
  input parameter success_ :: <C-boolean>;
  input parameter del_ :: <C-boolean>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gtk_drag_finish";
end;

define C-function gtk-drag-get-source-widget
  input parameter context_ :: <GdkDragContext>;
  result res :: <GtkWidget>;
  c-name: "gtk_drag_get_source_widget";
end;

define C-function gtk-drag-set-icon-default
  input parameter context_ :: <GdkDragContext>;
  c-name: "gtk_drag_set_icon_default";
end;

define C-function gtk-drag-set-icon-gicon
  input parameter context_ :: <GdkDragContext>;
  input parameter icon_ :: <GIcon>;
  input parameter hot_x_ :: <C-signed-int>;
  input parameter hot_y_ :: <C-signed-int>;
  c-name: "gtk_drag_set_icon_gicon";
end;

define C-function gtk-drag-set-icon-name
  input parameter context_ :: <GdkDragContext>;
  input parameter icon_name_ :: <C-string>;
  input parameter hot_x_ :: <C-signed-int>;
  input parameter hot_y_ :: <C-signed-int>;
  c-name: "gtk_drag_set_icon_name";
end;

define C-function gtk-drag-set-icon-pixbuf
  input parameter context_ :: <GdkDragContext>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  input parameter hot_x_ :: <C-signed-int>;
  input parameter hot_y_ :: <C-signed-int>;
  c-name: "gtk_drag_set_icon_pixbuf";
end;

define C-function gtk-drag-set-icon-stock
  input parameter context_ :: <GdkDragContext>;
  input parameter stock_id_ :: <C-string>;
  input parameter hot_x_ :: <C-signed-int>;
  input parameter hot_y_ :: <C-signed-int>;
  c-name: "gtk_drag_set_icon_stock";
end;

define C-function gtk-drag-set-icon-surface
  input parameter context_ :: <GdkDragContext>;
  input parameter surface_ :: <cairoSurface>;
  c-name: "gtk_drag_set_icon_surface";
end;

define C-function gtk-drag-set-icon-widget
  input parameter context_ :: <GdkDragContext>;
  input parameter widget_ :: <GtkWidget>;
  input parameter hot_x_ :: <C-signed-int>;
  input parameter hot_y_ :: <C-signed-int>;
  c-name: "gtk_drag_set_icon_widget";
end;

define C-function gtk-events-pending
  result res :: <C-boolean>;
  c-name: "gtk_events_pending";
end;

define C-function gtk-false
  result res :: <C-boolean>;
  c-name: "gtk_false";
end;

define C-function gtk-file-chooser-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_file_chooser_error_quark";
end;

define C-function gtk-get-binary-age
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_binary_age";
end;

define C-function gtk-get-current-event
  result res :: <GdkEvent>;
  c-name: "gtk_get_current_event";
end;

define C-function gtk-get-current-event-device
  result res :: <GdkDevice>;
  c-name: "gtk_get_current_event_device";
end;

define C-function gtk-get-current-event-state
  output parameter state_ :: <GdkModifierType*>;
  result res :: <C-boolean>;
  c-name: "gtk_get_current_event_state";
end;

define C-function gtk-get-current-event-time
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_current_event_time";
end;

define C-function gtk-get-debug-flags
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_debug_flags";
end;

define C-function gtk-get-default-language
  result res :: <PangoLanguage>;
  c-name: "gtk_get_default_language";
end;

define C-function gtk-get-event-widget
  input parameter event_ :: <GdkEvent>;
  result res :: <GtkWidget>;
  c-name: "gtk_get_event_widget";
end;

define C-function gtk-get-interface-age
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_interface_age";
end;

define C-function gtk-get-major-version
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_major_version";
end;

define C-function gtk-get-micro-version
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_micro_version";
end;

define C-function gtk-get-minor-version
  result res :: <C-unsigned-int>;
  c-name: "gtk_get_minor_version";
end;

define C-function gtk-grab-get-current
  result res :: <GtkWidget>;
  c-name: "gtk_grab_get_current";
end;

define C-function gtk-icon-size-from-name
  input parameter name_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_size_from_name";
end;

define C-function gtk-icon-size-get-name
  input parameter size_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "gtk_icon_size_get_name";
end;

define C-function gtk-icon-size-lookup
  input parameter size_ :: <C-signed-int>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_size_lookup";
end;

define C-function gtk-icon-size-lookup-for-settings
  input parameter settings_ :: <GtkSettings>;
  input parameter size_ :: <C-signed-int>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gtk_icon_size_lookup_for_settings";
end;

define C-function gtk-icon-size-register
  input parameter name_ :: <C-string>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gtk_icon_size_register";
end;

define C-function gtk-icon-size-register-alias
  input parameter alias_ :: <C-string>;
  input parameter target_ :: <C-signed-int>;
  c-name: "gtk_icon_size_register_alias";
end;

define C-function gtk-icon-theme-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_icon_theme_error_quark";
end;

define C-function gtk-init
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  c-name: "gtk_init";
end;

define C-function gtk-init-check
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gtk_init_check";
end;

define C-function gtk-init-with-args
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  input parameter parameter_string_ :: <C-string>;
  input parameter entries_ :: <C-unsigned-char*> /* Not supported */;
  input parameter translation_domain_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_init_with_args";
end;

define C-function gtk-main
  c-name: "gtk_main";
end;

define C-function gtk-main-do-event
  input parameter event_ :: <GdkEvent>;
  c-name: "gtk_main_do_event";
end;

define C-function gtk-main-iteration
  result res :: <C-boolean>;
  c-name: "gtk_main_iteration";
end;

define C-function gtk-main-iteration-do
  input parameter blocking_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_main_iteration_do";
end;

define C-function gtk-main-level
  result res :: <C-unsigned-int>;
  c-name: "gtk_main_level";
end;

define C-function gtk-main-quit
  c-name: "gtk_main_quit";
end;

define C-function gtk-paint-arrow
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter arrow_type_ :: <GtkArrowType>;
  input parameter fill_ :: <C-boolean>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_arrow";
end;

define C-function gtk-paint-box
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_box";
end;

define C-function gtk-paint-box-gap
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter gap_side_ :: <GtkPositionType>;
  input parameter gap_x_ :: <C-signed-int>;
  input parameter gap_width_ :: <C-signed-int>;
  c-name: "gtk_paint_box_gap";
end;

define C-function gtk-paint-check
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_check";
end;

define C-function gtk-paint-diamond
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_diamond";
end;

define C-function gtk-paint-expander
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter expander_style_ :: <GtkExpanderStyle>;
  c-name: "gtk_paint_expander";
end;

define C-function gtk-paint-extension
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter gap_side_ :: <GtkPositionType>;
  c-name: "gtk_paint_extension";
end;

define C-function gtk-paint-flat-box
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_flat_box";
end;

define C-function gtk-paint-focus
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_focus";
end;

define C-function gtk-paint-handle
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter orientation_ :: <GtkOrientation>;
  c-name: "gtk_paint_handle";
end;

define C-function gtk-paint-hline
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x1_ :: <C-signed-int>;
  input parameter x2_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gtk_paint_hline";
end;

define C-function gtk-paint-layout
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter use_text_ :: <C-boolean>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter layout_ :: <PangoLayout>;
  c-name: "gtk_paint_layout";
end;

define C-function gtk-paint-option
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_option";
end;

define C-function gtk-paint-resize-grip
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter edge_ :: <GdkWindowEdge>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_resize_grip";
end;

define C-function gtk-paint-shadow
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_shadow";
end;

define C-function gtk-paint-shadow-gap
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter gap_side_ :: <GtkPositionType>;
  input parameter gap_x_ :: <C-signed-int>;
  input parameter gap_width_ :: <C-signed-int>;
  c-name: "gtk_paint_shadow_gap";
end;

define C-function gtk-paint-slider
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter orientation_ :: <GtkOrientation>;
  c-name: "gtk_paint_slider";
end;

define C-function gtk-paint-spinner
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter step_ :: <C-unsigned-int>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_spinner";
end;

define C-function gtk-paint-tab
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter shadow_type_ :: <GtkShadowType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gtk_paint_tab";
end;

define C-function gtk-paint-vline
  input parameter style_ :: <GtkStyle>;
  input parameter cr_ :: <cairoContext>;
  input parameter state_type_ :: <GtkStateType>;
  input parameter widget_ :: <GtkWidget>;
  input parameter detail_ :: <C-string>;
  input parameter y1__ :: <C-signed-int>;
  input parameter y2__ :: <C-signed-int>;
  input parameter x_ :: <C-signed-int>;
  c-name: "gtk_paint_vline";
end;

define C-function gtk-parse-args
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gtk_parse_args";
end;

define C-function gtk-print-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_print_error_quark";
end;

define C-function gtk-print-run-page-setup-dialog
  input parameter parent_ :: <GtkWindow>;
  input parameter page_setup_ :: <GtkPageSetup>;
  input parameter settings_ :: <GtkPrintSettings>;
  result res :: <GtkPageSetup>;
  c-name: "gtk_print_run_page_setup_dialog";
end;

define C-function gtk-print-run-page-setup-dialog-async
  input parameter parent_ :: <GtkWindow>;
  input parameter page_setup_ :: <GtkPageSetup>;
  input parameter settings_ :: <GtkPrintSettings>;
  input parameter done_cb_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  c-name: "gtk_print_run_page_setup_dialog_async";
end;

define C-function gtk-propagate-event
  input parameter widget_ :: <GtkWidget>;
  input parameter event_ :: <GdkEvent>;
  c-name: "gtk_propagate_event";
end;

define C-function gtk-rc-add-default-file
  input parameter filename_ :: <C-string>;
  c-name: "gtk_rc_add_default_file";
end;

define C-function gtk-rc-get-default-files
  result res :: <C-string*>;
  c-name: "gtk_rc_get_default_files";
end;

define C-function gtk-rc-get-style
  input parameter widget_ :: <GtkWidget>;
  result res :: <GtkStyle>;
  c-name: "gtk_rc_get_style";
end;

define C-function gtk-rc-get-style-by-paths
  input parameter settings_ :: <GtkSettings>;
  input parameter widget_path_ :: <C-string>;
  input parameter class_path_ :: <C-string>;
  input parameter type_ :: <C-long>;
  result res :: <GtkStyle>;
  c-name: "gtk_rc_get_style_by_paths";
end;

define C-function gtk-rc-parse-color
  input parameter scanner_ :: <GScanner>;
  output parameter color_ :: <GdkColor>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_rc_parse_color";
end;

define C-function gtk-rc-parse-color-full
  input parameter scanner_ :: <GScanner>;
  input parameter style_ :: <GtkRcStyle>;
  output parameter color_ :: <GdkColor>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_rc_parse_color_full";
end;

define C-function gtk-rc-parse-priority
  input parameter scanner_ :: <GScanner>;
  input parameter priority_ :: <GtkPathPriorityType>;
  result res :: <C-unsigned-int>;
  c-name: "gtk_rc_parse_priority";
end;

define C-function gtk-rc-property-parse-border
  input parameter pspec_ :: <GParamSpec>;
  input parameter gstring_ :: <GString>;
  input parameter property_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_rc_property_parse_border";
end;

define C-function gtk-rc-property-parse-color
  input parameter pspec_ :: <GParamSpec>;
  input parameter gstring_ :: <GString>;
  input parameter property_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_rc_property_parse_color";
end;

define C-function gtk-rc-property-parse-enum
  input parameter pspec_ :: <GParamSpec>;
  input parameter gstring_ :: <GString>;
  input parameter property_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_rc_property_parse_enum";
end;

define C-function gtk-rc-property-parse-flags
  input parameter pspec_ :: <GParamSpec>;
  input parameter gstring_ :: <GString>;
  input parameter property_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_rc_property_parse_flags";
end;

define C-function gtk-rc-property-parse-requisition
  input parameter pspec_ :: <GParamSpec>;
  input parameter gstring_ :: <GString>;
  input parameter property_value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gtk_rc_property_parse_requisition";
end;

define C-function gtk-rc-set-default-files
  input parameter filenames_ :: <C-string*>;
  c-name: "gtk_rc_set_default_files";
end;

define C-function gtk-recent-chooser-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_recent_chooser_error_quark";
end;

define C-function gtk-recent-manager-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gtk_recent_manager_error_quark";
end;

define C-function gtk-render-activity
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_activity";
end;

define C-function gtk-render-arrow
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter angle_ :: <C-double>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter size_ :: <C-double>;
  c-name: "gtk_render_arrow";
end;

define C-function gtk-render-background
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_background";
end;

define C-function gtk-render-check
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_check";
end;

define C-function gtk-render-expander
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_expander";
end;

define C-function gtk-render-extension
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  input parameter gap_side_ :: <GtkPositionType>;
  c-name: "gtk_render_extension";
end;

define C-function gtk-render-focus
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_focus";
end;

define C-function gtk-render-frame
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_frame";
end;

define C-function gtk-render-frame-gap
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  input parameter gap_side_ :: <GtkPositionType>;
  input parameter xy0_gap_ :: <C-double>;
  input parameter xy1_gap_ :: <C-double>;
  c-name: "gtk_render_frame_gap";
end;

define C-function gtk-render-handle
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_handle";
end;

define C-function gtk-render-icon
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "gtk_render_icon";
end;

define C-function gtk-render-icon-pixbuf
  input parameter context_ :: <GtkStyleContext>;
  input parameter source_ :: <GtkIconSource>;
  input parameter size_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gtk_render_icon_pixbuf";
end;

define C-function gtk-render-insertion-cursor
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter layout_ :: <PangoLayout>;
  input parameter index_ :: <C-signed-int>;
  input parameter direction_ :: <PangoDirection>;
  c-name: "gtk_render_insertion_cursor";
end;

define C-function gtk-render-layout
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter layout_ :: <PangoLayout>;
  c-name: "gtk_render_layout";
end;

define C-function gtk-render-line
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x0_ :: <C-double>;
  input parameter y0_ :: <C-double>;
  input parameter x1_ :: <C-double>;
  input parameter y1_ :: <C-double>;
  c-name: "gtk_render_line";
end;

define C-function gtk-render-option
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  c-name: "gtk_render_option";
end;

define C-function gtk-render-slider
  input parameter context_ :: <GtkStyleContext>;
  input parameter cr_ :: <cairoContext>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  input parameter width_ :: <C-double>;
  input parameter height_ :: <C-double>;
  input parameter orientation_ :: <GtkOrientation>;
  c-name: "gtk_render_slider";
end;

define C-function gtk-rgb-to-hsv
  input parameter r_ :: <C-double>;
  input parameter g_ :: <C-double>;
  input parameter b_ :: <C-double>;
  output parameter h_ :: <C-double*>;
  output parameter s_ :: <C-double*>;
  output parameter v_ :: <C-double*>;
  c-name: "gtk_rgb_to_hsv";
end;

define C-function gtk-selection-add-target
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  input parameter target_ :: <GdkAtom>;
  input parameter info_ :: <C-unsigned-int>;
  c-name: "gtk_selection_add_target";
end;

define C-function gtk-selection-add-targets
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter ntargets_ :: <C-unsigned-int>;
  c-name: "gtk_selection_add_targets";
end;

define C-function gtk-selection-clear-targets
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  c-name: "gtk_selection_clear_targets";
end;

define C-function gtk-selection-convert
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  input parameter target_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_convert";
end;

define C-function gtk-selection-owner-set
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_owner_set";
end;

define C-function gtk-selection-owner-set-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter widget_ :: <GtkWidget>;
  input parameter selection_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gtk_selection_owner_set_for_display";
end;

define C-function gtk-selection-remove-all
  input parameter widget_ :: <GtkWidget>;
  c-name: "gtk_selection_remove_all";
end;

define C-function gtk-set-debug-flags
  input parameter flags_ :: <C-unsigned-int>;
  c-name: "gtk_set_debug_flags";
end;

define C-function gtk-show-uri
  input parameter screen_ :: <GdkScreen>;
  input parameter uri_ :: <C-string>;
  input parameter timestamp_ :: <C-unsigned-int>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "gtk_show_uri";
end;

define C-function gtk-stock-add
  input parameter items_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_items_ :: <C-unsigned-int>;
  c-name: "gtk_stock_add";
end;

define C-function gtk-stock-add-static
  input parameter items_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_items_ :: <C-unsigned-int>;
  c-name: "gtk_stock_add_static";
end;

define C-function gtk-stock-list-ids
  result res :: <GSList>;
  c-name: "gtk_stock_list_ids";
end;

define C-function gtk-stock-lookup
  input parameter stock_id_ :: <C-string>;
  output parameter item_ :: <GtkStockItem>;
  result res :: <C-boolean>;
  c-name: "gtk_stock_lookup";
end;

define C-function gtk-stock-set-translate-func
  input parameter domain_ :: <C-string>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "gtk_stock_set_translate_func";
end;

define C-function gtk-target-table-free
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  c-name: "gtk_target_table_free";
end;

define C-function gtk-target-table-new-from-list
  input parameter list_ :: <GtkTargetList>;
  output parameter n_targets_ :: <C-signed-int*>;
  result res :: <C-unsigned-char*> /* Not supported */;
  c-name: "gtk_target_table_new_from_list";
end;

define C-function gtk-targets-include-image
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter writable_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_targets_include_image";
end;

define C-function gtk-targets-include-rich-text
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  input parameter buffer_ :: <GtkTextBuffer>;
  result res :: <C-boolean>;
  c-name: "gtk_targets_include_rich_text";
end;

define C-function gtk-targets-include-text
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_targets_include_text";
end;

define C-function gtk-targets-include-uri
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "gtk_targets_include_uri";
end;

define C-function gtk-test-create-simple-window
  input parameter window_title_ :: <C-string>;
  input parameter dialog_text_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_test_create_simple_window";
end;

define C-function gtk-test-find-label
  input parameter widget_ :: <GtkWidget>;
  input parameter label_pattern_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_test_find_label";
end;

define C-function gtk-test-find-sibling
  input parameter base_widget_ :: <GtkWidget>;
  input parameter widget_type_ :: <C-long>;
  result res :: <GtkWidget>;
  c-name: "gtk_test_find_sibling";
end;

define C-function gtk-test-find-widget
  input parameter widget_ :: <GtkWidget>;
  input parameter label_pattern_ :: <C-string>;
  input parameter widget_type_ :: <C-long>;
  result res :: <GtkWidget>;
  c-name: "gtk_test_find_widget";
end;

define C-function gtk-test-list-all-types
  output parameter n_types_ :: <C-unsigned-int*>;
  result res :: <C-long*>;
  c-name: "gtk_test_list_all_types";
end;

define C-function gtk-test-register-all-types
  c-name: "gtk_test_register_all_types";
end;

define C-function gtk-test-slider-get-value
  input parameter widget_ :: <GtkWidget>;
  result res :: <C-double>;
  c-name: "gtk_test_slider_get_value";
end;

define C-function gtk-test-slider-set-perc
  input parameter widget_ :: <GtkWidget>;
  input parameter percentage_ :: <C-double>;
  c-name: "gtk_test_slider_set_perc";
end;

define C-function gtk-test-spin-button-click
  input parameter spinner_ :: <GtkSpinButton>;
  input parameter button_ :: <C-unsigned-int>;
  input parameter upwards_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gtk_test_spin_button_click";
end;

define C-function gtk-test-text-get
  input parameter widget_ :: <GtkWidget>;
  result res :: <C-string>;
  c-name: "gtk_test_text_get";
end;

define C-function gtk-test-text-set
  input parameter widget_ :: <GtkWidget>;
  input parameter string_ :: <C-string>;
  c-name: "gtk_test_text_set";
end;

define C-function gtk-test-widget-click
  input parameter widget_ :: <GtkWidget>;
  input parameter button_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_test_widget_click";
end;

define C-function gtk-test-widget-send-key
  input parameter widget_ :: <GtkWidget>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  result res :: <C-boolean>;
  c-name: "gtk_test_widget_send_key";
end;

define C-function gtk-tree-get-row-drag-data
  input parameter selection_data_ :: <GtkSelectionData>;
  output parameter tree_model_ :: <GtkTreeModel*>;
  output parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_get_row_drag_data";
end;

define C-function gtk-tree-set-row-drag-data
  input parameter selection_data_ :: <GtkSelectionData>;
  input parameter tree_model_ :: <GtkTreeModel>;
  input parameter path_ :: <GtkTreePath>;
  result res :: <C-boolean>;
  c-name: "gtk_tree_set_row_drag_data";
end;

define C-function gtk-true
  result res :: <C-boolean>;
  c-name: "gtk_true";
end;

