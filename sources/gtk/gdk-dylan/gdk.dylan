module: gdk
synopsis: generated bindings for the Gdk library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define open C-subtype <GdkAppLaunchContext> (<GAppLaunchContext>)
end C-subtype;

define C-pointer-type <GdkAppLaunchContext*> => <GdkAppLaunchContext>;

define C-function gdk-app-launch-context-new
  result res :: <GdkAppLaunchContext>;
  c-name: "gdk_app_launch_context_new";
end;

define C-function gdk-app-launch-context-set-desktop
  input parameter self :: <GdkAppLaunchContext>;
  input parameter desktop_ :: <C-signed-int>;
  c-name: "gdk_app_launch_context_set_desktop";
end;

define C-function gdk-app-launch-context-set-display
  input parameter self :: <GdkAppLaunchContext>;
  input parameter display_ :: <GdkDisplay>;
  c-name: "gdk_app_launch_context_set_display";
end;

define C-function gdk-app-launch-context-set-icon
  input parameter self :: <GdkAppLaunchContext>;
  input parameter icon_ :: <GIcon>;
  c-name: "gdk_app_launch_context_set_icon";
end;

define C-function gdk-app-launch-context-set-icon-name
  input parameter self :: <GdkAppLaunchContext>;
  input parameter icon_name_ :: <C-string>;
  c-name: "gdk_app_launch_context_set_icon_name";
end;

define C-function gdk-app-launch-context-set-screen
  input parameter self :: <GdkAppLaunchContext>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gdk_app_launch_context_set_screen";
end;

define C-function gdk-app-launch-context-set-timestamp
  input parameter self :: <GdkAppLaunchContext>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_app_launch_context_set_timestamp";
end;

define C-struct <_GdkAtom>
  pointer-type-name: <GdkAtom>;
end C-struct;

define C-function gdk-atom-name
  input parameter self :: <GdkAtom>;
  result res :: <C-string>;
  c-name: "gdk_atom_name";
end;

define C-function gdk-atom-intern
  input parameter atom_name_ :: <C-string>;
  input parameter only_if_exists_ :: <C-boolean>;
  result res :: <GdkAtom>;
  c-name: "gdk_atom_intern";
end;

define C-function gdk-atom-intern-static-string
  input parameter atom_name_ :: <C-string>;
  result res :: <GdkAtom>;
  c-name: "gdk_atom_intern_static_string";
end;

define constant $gdk-axis-ignore = 0;
define constant $gdk-axis-x = 1;
define constant $gdk-axis-y = 2;
define constant $gdk-axis-pressure = 3;
define constant $gdk-axis-xtilt = 4;
define constant $gdk-axis-ytilt = 5;
define constant $gdk-axis-wheel = 6;
define constant $gdk-axis-last = 7;
define constant <GdkAxisUse> = <C-int>;
define C-pointer-type <GdkAxisUse*> => <GdkAxisUse>;

define constant $button-middle = 2;

define constant $button-primary = 1;

define constant $button-secondary = 3;

define constant $gdk-lsb-first = 0;
define constant $gdk-msb-first = 1;
define constant <GdkByteOrder> = <C-int>;
define C-pointer-type <GdkByteOrder*> => <GdkByteOrder>;

define constant $current-time = 0;

define C-struct <_GdkColor>
  slot gdk-color-pixel :: <C-unsigned-int>;
  slot gdk-color-red :: <C-unsigned-short>;
  slot gdk-color-green :: <C-unsigned-short>;
  slot gdk-color-blue :: <C-unsigned-short>;
  pointer-type-name: <GdkColor>;
end C-struct;

define C-function gdk-color-copy
  input parameter self :: <GdkColor>;
  result res :: <GdkColor>;
  c-name: "gdk_color_copy";
end;

define C-function gdk-color-equal
  input parameter self :: <GdkColor>;
  input parameter colorb_ :: <GdkColor>;
  result res :: <C-boolean>;
  c-name: "gdk_color_equal";
end;

define C-function gdk-color-free
  input parameter self :: <GdkColor>;
  c-name: "gdk_color_free";
end;

define C-function gdk-color-hash
  input parameter self :: <GdkColor>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_color_hash";
end;

define C-function gdk-color-to-string
  input parameter self :: <GdkColor>;
  result res :: <C-string>;
  c-name: "gdk_color_to_string";
end;

define C-function gdk-color-parse
  input parameter spec_ :: <C-string>;
  output parameter color_ :: <GdkColor>;
  result res :: <C-boolean>;
  c-name: "gdk_color_parse";
end;

define constant $gdk-crossing-normal = 0;
define constant $gdk-crossing-grab = 1;
define constant $gdk-crossing-ungrab = 2;
define constant $gdk-crossing-gtk-grab = 3;
define constant $gdk-crossing-gtk-ungrab = 4;
define constant $gdk-crossing-state-changed = 5;
define constant $gdk-crossing-touch-begin = 6;
define constant $gdk-crossing-touch-end = 7;
define constant $gdk-crossing-device-switch = 8;
define constant <GdkCrossingMode> = <C-int>;
define C-pointer-type <GdkCrossingMode*> => <GdkCrossingMode>;

define open C-subtype <GdkCursor> (<GObject>)
end C-subtype;

define C-pointer-type <GdkCursor*> => <GdkCursor>;

define C-function gdk-cursor-new
  input parameter cursor_type_ :: <GdkCursorType>;
  result res :: <GdkCursor>;
  c-name: "gdk_cursor_new";
end;

define C-function gdk-cursor-new-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter cursor_type_ :: <GdkCursorType>;
  result res :: <GdkCursor>;
  c-name: "gdk_cursor_new_for_display";
end;

define C-function gdk-cursor-new-from-name
  input parameter display_ :: <GdkDisplay>;
  input parameter name_ :: <C-string>;
  result res :: <GdkCursor>;
  c-name: "gdk_cursor_new_from_name";
end;

define C-function gdk-cursor-new-from-pixbuf
  input parameter display_ :: <GdkDisplay>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <GdkCursor>;
  c-name: "gdk_cursor_new_from_pixbuf";
end;

define C-function gdk-cursor-get-cursor-type
  input parameter self :: <GdkCursor>;
  result res :: <GdkCursorType>;
  c-name: "gdk_cursor_get_cursor_type";
end;

define C-function gdk-cursor-get-display
  input parameter self :: <GdkCursor>;
  result res :: <GdkDisplay>;
  c-name: "gdk_cursor_get_display";
end;

define C-function gdk-cursor-get-image
  input parameter self :: <GdkCursor>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_cursor_get_image";
end;

define C-function gdk-cursor-ref
  input parameter self :: <GdkCursor>;
  result res :: <GdkCursor>;
  c-name: "gdk_cursor_ref";
end;

define C-function gdk-cursor-unref
  input parameter self :: <GdkCursor>;
  c-name: "gdk_cursor_unref";
end;

define constant $gdk-x-cursor = 0;
define constant $gdk-arrow = 2;
define constant $gdk-based-arrow-down = 4;
define constant $gdk-based-arrow-up = 6;
define constant $gdk-boat = 8;
define constant $gdk-bogosity = 10;
define constant $gdk-bottom-left-corner = 12;
define constant $gdk-bottom-right-corner = 14;
define constant $gdk-bottom-side = 16;
define constant $gdk-bottom-tee = 18;
define constant $gdk-box-spiral = 20;
define constant $gdk-center-ptr = 22;
define constant $gdk-circle = 24;
define constant $gdk-clock = 26;
define constant $gdk-coffee-mug = 28;
define constant $gdk-cross = 30;
define constant $gdk-cross-reverse = 32;
define constant $gdk-crosshair = 34;
define constant $gdk-diamond-cross = 36;
define constant $gdk-dot = 38;
define constant $gdk-dotbox = 40;
define constant $gdk-double-arrow = 42;
define constant $gdk-draft-large = 44;
define constant $gdk-draft-small = 46;
define constant $gdk-draped-box = 48;
define constant $gdk-exchange = 50;
define constant $gdk-fleur = 52;
define constant $gdk-gobbler = 54;
define constant $gdk-gumby = 56;
define constant $gdk-hand1 = 58;
define constant $gdk-hand2 = 60;
define constant $gdk-heart = 62;
define constant $gdk-icon = 64;
define constant $gdk-iron-cross = 66;
define constant $gdk-left-ptr = 68;
define constant $gdk-left-side = 70;
define constant $gdk-left-tee = 72;
define constant $gdk-leftbutton = 74;
define constant $gdk-ll-angle = 76;
define constant $gdk-lr-angle = 78;
define constant $gdk-man = 80;
define constant $gdk-middlebutton = 82;
define constant $gdk-mouse = 84;
define constant $gdk-pencil = 86;
define constant $gdk-pirate = 88;
define constant $gdk-plus = 90;
define constant $gdk-question-arrow = 92;
define constant $gdk-right-ptr = 94;
define constant $gdk-right-side = 96;
define constant $gdk-right-tee = 98;
define constant $gdk-rightbutton = 100;
define constant $gdk-rtl-logo = 102;
define constant $gdk-sailboat = 104;
define constant $gdk-sb-down-arrow = 106;
define constant $gdk-sb-h-double-arrow = 108;
define constant $gdk-sb-left-arrow = 110;
define constant $gdk-sb-right-arrow = 112;
define constant $gdk-sb-up-arrow = 114;
define constant $gdk-sb-v-double-arrow = 116;
define constant $gdk-shuttle = 118;
define constant $gdk-sizing = 120;
define constant $gdk-spider = 122;
define constant $gdk-spraycan = 124;
define constant $gdk-star = 126;
define constant $gdk-target = 128;
define constant $gdk-tcross = 130;
define constant $gdk-top-left-arrow = 132;
define constant $gdk-top-left-corner = 134;
define constant $gdk-top-right-corner = 136;
define constant $gdk-top-side = 138;
define constant $gdk-top-tee = 140;
define constant $gdk-trek = 142;
define constant $gdk-ul-angle = 144;
define constant $gdk-umbrella = 146;
define constant $gdk-ur-angle = 148;
define constant $gdk-watch = 150;
define constant $gdk-xterm = 152;
define constant $gdk-last-cursor = 153;
define constant $gdk-blank-cursor = -2;
define constant $gdk-cursor-is-pixmap = -1;
define constant <GdkCursorType> = <C-int>;
define C-pointer-type <GdkCursorType*> => <GdkCursorType>;

define open C-subtype <GdkDevice> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDevice*> => <GdkDevice>;

define C-function gdk-device-grab-info-libgtk-only
  input parameter display_ :: <GdkDisplay>;
  input parameter device_ :: <GdkDevice>;
  output parameter grab_window_ :: <GdkWindow*>;
  output parameter owner_events_ :: <C-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_device_grab_info_libgtk_only";
end;

define C-function gdk-device-get-associated-device
  input parameter self :: <GdkDevice>;
  result res :: <GdkDevice>;
  c-name: "gdk_device_get_associated_device";
end;

define C-function gdk-device-get-axis-use
  input parameter self :: <GdkDevice>;
  input parameter index__ :: <C-unsigned-int>;
  result res :: <GdkAxisUse>;
  c-name: "gdk_device_get_axis_use";
end;

define C-function gdk-device-get-device-type
  input parameter self :: <GdkDevice>;
  result res :: <GdkDeviceType>;
  c-name: "gdk_device_get_device_type";
end;

define C-function gdk-device-get-display
  input parameter self :: <GdkDevice>;
  result res :: <GdkDisplay>;
  c-name: "gdk_device_get_display";
end;

define C-function gdk-device-get-has-cursor
  input parameter self :: <GdkDevice>;
  result res :: <C-boolean>;
  c-name: "gdk_device_get_has_cursor";
end;

define C-function gdk-device-get-key
  input parameter self :: <GdkDevice>;
  input parameter index__ :: <C-unsigned-int>;
  output parameter keyval_ :: <C-unsigned-int*>;
  output parameter modifiers_ :: <GdkModifierType*>;
  result res :: <C-boolean>;
  c-name: "gdk_device_get_key";
end;

define C-function gdk-device-get-mode
  input parameter self :: <GdkDevice>;
  result res :: <GdkInputMode>;
  c-name: "gdk_device_get_mode";
end;

define C-function gdk-device-get-n-axes
  input parameter self :: <GdkDevice>;
  result res :: <C-signed-int>;
  c-name: "gdk_device_get_n_axes";
end;

define C-function gdk-device-get-n-keys
  input parameter self :: <GdkDevice>;
  result res :: <C-signed-int>;
  c-name: "gdk_device_get_n_keys";
end;

define C-function gdk-device-get-name
  input parameter self :: <GdkDevice>;
  result res :: <C-string>;
  c-name: "gdk_device_get_name";
end;

define C-function gdk-device-get-position
  input parameter self :: <GdkDevice>;
  output parameter screen_ :: <GdkScreen*>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gdk_device_get_position";
end;

define C-function gdk-device-get-source
  input parameter self :: <GdkDevice>;
  result res :: <GdkInputSource>;
  c-name: "gdk_device_get_source";
end;

define C-function gdk-device-get-window-at-position
  input parameter self :: <GdkDevice>;
  output parameter win_x_ :: <C-signed-int*>;
  output parameter win_y_ :: <C-signed-int*>;
  result res :: <GdkWindow>;
  c-name: "gdk_device_get_window_at_position";
end;

define C-function gdk-device-grab
  input parameter self :: <GdkDevice>;
  input parameter window_ :: <GdkWindow>;
  input parameter grab_ownership_ :: <GdkGrabOwnership>;
  input parameter owner_events_ :: <C-boolean>;
  input parameter event_mask_ :: <GdkEventMask>;
  input parameter cursor_ :: <GdkCursor>;
  input parameter time__ :: <C-unsigned-int>;
  result res :: <GdkGrabStatus>;
  c-name: "gdk_device_grab";
end;

define C-function gdk-device-list-axes
  input parameter self :: <GdkDevice>;
  result res :: <GList>;
  c-name: "gdk_device_list_axes";
end;

define C-function gdk-device-list-slave-devices
  input parameter self :: <GdkDevice>;
  result res :: <GList>;
  c-name: "gdk_device_list_slave_devices";
end;

define C-function gdk-device-set-axis-use
  input parameter self :: <GdkDevice>;
  input parameter index__ :: <C-unsigned-int>;
  input parameter use_ :: <GdkAxisUse>;
  c-name: "gdk_device_set_axis_use";
end;

define C-function gdk-device-set-key
  input parameter self :: <GdkDevice>;
  input parameter index__ :: <C-unsigned-int>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  c-name: "gdk_device_set_key";
end;

define C-function gdk-device-set-mode
  input parameter self :: <GdkDevice>;
  input parameter mode_ :: <GdkInputMode>;
  result res :: <C-boolean>;
  c-name: "gdk_device_set_mode";
end;

define C-function gdk-device-ungrab
  input parameter self :: <GdkDevice>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_device_ungrab";
end;

define C-function gdk-device-warp
  input parameter self :: <GdkDevice>;
  input parameter screen_ :: <GdkScreen>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gdk_device_warp";
end;

define open C-subtype <GdkDeviceManager> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDeviceManager*> => <GdkDeviceManager>;

define C-function gdk-device-manager-get-client-pointer
  input parameter self :: <GdkDeviceManager>;
  result res :: <GdkDevice>;
  c-name: "gdk_device_manager_get_client_pointer";
end;

define C-function gdk-device-manager-get-display
  input parameter self :: <GdkDeviceManager>;
  result res :: <GdkDisplay>;
  c-name: "gdk_device_manager_get_display";
end;

define C-function gdk-device-manager-list-devices
  input parameter self :: <GdkDeviceManager>;
  input parameter type_ :: <GdkDeviceType>;
  result res :: <GList>;
  c-name: "gdk_device_manager_list_devices";
end;

define constant $gdk-device-type-master = 0;
define constant $gdk-device-type-slave = 1;
define constant $gdk-device-type-floating = 2;
define constant <GdkDeviceType> = <C-int>;
define C-pointer-type <GdkDeviceType*> => <GdkDeviceType>;

define open C-subtype <GdkDisplay> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDisplay*> => <GdkDisplay>;

define C-function gdk-display-get-default
  result res :: <GdkDisplay>;
  c-name: "gdk_display_get_default";
end;

define C-function gdk-display-open
  input parameter display_name_ :: <C-string>;
  result res :: <GdkDisplay>;
  c-name: "gdk_display_open";
end;

define C-function gdk-display-open-default-libgtk-only
  result res :: <GdkDisplay>;
  c-name: "gdk_display_open_default_libgtk_only";
end;

define C-function gdk-display-beep
  input parameter self :: <GdkDisplay>;
  c-name: "gdk_display_beep";
end;

define C-function gdk-display-close
  input parameter self :: <GdkDisplay>;
  c-name: "gdk_display_close";
end;

define C-function gdk-display-device-is-grabbed
  input parameter self :: <GdkDisplay>;
  input parameter device_ :: <GdkDevice>;
  result res :: <C-boolean>;
  c-name: "gdk_display_device_is_grabbed";
end;

define C-function gdk-display-flush
  input parameter self :: <GdkDisplay>;
  c-name: "gdk_display_flush";
end;

define C-function gdk-display-get-app-launch-context
  input parameter self :: <GdkDisplay>;
  result res :: <GdkAppLaunchContext>;
  c-name: "gdk_display_get_app_launch_context";
end;

define C-function gdk-display-get-default-cursor-size
  input parameter self :: <GdkDisplay>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_display_get_default_cursor_size";
end;

define C-function gdk-display-get-default-group
  input parameter self :: <GdkDisplay>;
  result res :: <GdkWindow>;
  c-name: "gdk_display_get_default_group";
end;

define C-function gdk-display-get-default-screen
  input parameter self :: <GdkDisplay>;
  result res :: <GdkScreen>;
  c-name: "gdk_display_get_default_screen";
end;

define C-function gdk-display-get-device-manager
  input parameter self :: <GdkDisplay>;
  result res :: <GdkDeviceManager>;
  c-name: "gdk_display_get_device_manager";
end;

define C-function gdk-display-get-event
  input parameter self :: <GdkDisplay>;
  result res :: <GdkEvent>;
  c-name: "gdk_display_get_event";
end;

define C-function gdk-display-get-maximal-cursor-size
  input parameter self :: <GdkDisplay>;
  output parameter width_ :: <C-unsigned-int*>;
  output parameter height_ :: <C-unsigned-int*>;
  c-name: "gdk_display_get_maximal_cursor_size";
end;

define C-function gdk-display-get-n-screens
  input parameter self :: <GdkDisplay>;
  result res :: <C-signed-int>;
  c-name: "gdk_display_get_n_screens";
end;

define C-function gdk-display-get-name
  input parameter self :: <GdkDisplay>;
  result res :: <C-string>;
  c-name: "gdk_display_get_name";
end;

define C-function gdk-display-get-pointer
  input parameter self :: <GdkDisplay>;
  output parameter screen_ :: <GdkScreen*>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  output parameter mask_ :: <GdkModifierType*>;
  c-name: "gdk_display_get_pointer";
end;

define C-function gdk-display-get-screen
  input parameter self :: <GdkDisplay>;
  input parameter screen_num_ :: <C-signed-int>;
  result res :: <GdkScreen>;
  c-name: "gdk_display_get_screen";
end;

define C-function gdk-display-get-window-at-pointer
  input parameter self :: <GdkDisplay>;
  output parameter win_x_ :: <C-signed-int*>;
  output parameter win_y_ :: <C-signed-int*>;
  result res :: <GdkWindow>;
  c-name: "gdk_display_get_window_at_pointer";
end;

define C-function gdk-display-has-pending
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_has_pending";
end;

define C-function gdk-display-is-closed
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_is_closed";
end;

define C-function gdk-display-keyboard-ungrab
  input parameter self :: <GdkDisplay>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_display_keyboard_ungrab";
end;

define C-function gdk-display-list-devices
  input parameter self :: <GdkDisplay>;
  result res :: <GList>;
  c-name: "gdk_display_list_devices";
end;

define C-function gdk-display-notify-startup-complete
  input parameter self :: <GdkDisplay>;
  input parameter startup_id_ :: <C-string>;
  c-name: "gdk_display_notify_startup_complete";
end;

define C-function gdk-display-peek-event
  input parameter self :: <GdkDisplay>;
  result res :: <GdkEvent>;
  c-name: "gdk_display_peek_event";
end;

define C-function gdk-display-pointer-is-grabbed
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_pointer_is_grabbed";
end;

define C-function gdk-display-pointer-ungrab
  input parameter self :: <GdkDisplay>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_display_pointer_ungrab";
end;

define C-function gdk-display-put-event
  input parameter self :: <GdkDisplay>;
  input parameter event_ :: <GdkEvent>;
  c-name: "gdk_display_put_event";
end;

define C-function gdk-display-request-selection-notification
  input parameter self :: <GdkDisplay>;
  input parameter selection_ :: <GdkAtom>;
  result res :: <C-boolean>;
  c-name: "gdk_display_request_selection_notification";
end;

define C-function gdk-display-set-double-click-distance
  input parameter self :: <GdkDisplay>;
  input parameter distance_ :: <C-unsigned-int>;
  c-name: "gdk_display_set_double_click_distance";
end;

define C-function gdk-display-set-double-click-time
  input parameter self :: <GdkDisplay>;
  input parameter msec_ :: <C-unsigned-int>;
  c-name: "gdk_display_set_double_click_time";
end;

define C-function gdk-display-store-clipboard
  input parameter self :: <GdkDisplay>;
  input parameter clipboard_window_ :: <GdkWindow>;
  input parameter time__ :: <C-unsigned-int>;
  input parameter targets_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_targets_ :: <C-signed-int>;
  c-name: "gdk_display_store_clipboard";
end;

define C-function gdk-display-supports-clipboard-persistence
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_clipboard_persistence";
end;

define C-function gdk-display-supports-composite
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_composite";
end;

define C-function gdk-display-supports-cursor-alpha
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_cursor_alpha";
end;

define C-function gdk-display-supports-cursor-color
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_cursor_color";
end;

define C-function gdk-display-supports-input-shapes
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_input_shapes";
end;

define C-function gdk-display-supports-selection-notification
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_selection_notification";
end;

define C-function gdk-display-supports-shapes
  input parameter self :: <GdkDisplay>;
  result res :: <C-boolean>;
  c-name: "gdk_display_supports_shapes";
end;

define C-function gdk-display-sync
  input parameter self :: <GdkDisplay>;
  c-name: "gdk_display_sync";
end;

define C-function gdk-display-warp-pointer
  input parameter self :: <GdkDisplay>;
  input parameter screen_ :: <GdkScreen>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gdk_display_warp_pointer";
end;

define open C-subtype <GdkDisplayManager> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDisplayManager*> => <GdkDisplayManager>;

define C-function gdk-display-manager-get
  result res :: <GdkDisplayManager>;
  c-name: "gdk_display_manager_get";
end;

define C-function gdk-display-manager-get-default-display
  input parameter self :: <GdkDisplayManager>;
  result res :: <GdkDisplay>;
  c-name: "gdk_display_manager_get_default_display";
end;

define C-function gdk-display-manager-list-displays
  input parameter self :: <GdkDisplayManager>;
  result res :: <GSList>;
  c-name: "gdk_display_manager_list_displays";
end;

define C-function gdk-display-manager-open-display
  input parameter self :: <GdkDisplayManager>;
  input parameter name_ :: <C-string>;
  result res :: <GdkDisplay>;
  c-name: "gdk_display_manager_open_display";
end;

define C-function gdk-display-manager-set-default-display
  input parameter self :: <GdkDisplayManager>;
  input parameter display_ :: <GdkDisplay>;
  c-name: "gdk_display_manager_set_default_display";
end;

define constant $gdk-action-default = 1;
define constant $gdk-action-copy = 2;
define constant $gdk-action-move = 4;
define constant $gdk-action-link = 8;
define constant $gdk-action-private = 16;
define constant $gdk-action-ask = 32;
define constant <GdkDragAction> = <C-int>;
define C-pointer-type <GdkDragAction*> => <GdkDragAction>;

define open C-subtype <GdkDragContext> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDragContext*> => <GdkDragContext>;

define C-function gdk-drag-context-get-actions
  input parameter self :: <GdkDragContext>;
  result res :: <GdkDragAction>;
  c-name: "gdk_drag_context_get_actions";
end;

define C-function gdk-drag-context-get-dest-window
  input parameter self :: <GdkDragContext>;
  result res :: <GdkWindow>;
  c-name: "gdk_drag_context_get_dest_window";
end;

define C-function gdk-drag-context-get-device
  input parameter self :: <GdkDragContext>;
  result res :: <GdkDevice>;
  c-name: "gdk_drag_context_get_device";
end;

define C-function gdk-drag-context-get-protocol
  input parameter self :: <GdkDragContext>;
  result res :: <GdkDragProtocol>;
  c-name: "gdk_drag_context_get_protocol";
end;

define C-function gdk-drag-context-get-selected-action
  input parameter self :: <GdkDragContext>;
  result res :: <GdkDragAction>;
  c-name: "gdk_drag_context_get_selected_action";
end;

define C-function gdk-drag-context-get-source-window
  input parameter self :: <GdkDragContext>;
  result res :: <GdkWindow>;
  c-name: "gdk_drag_context_get_source_window";
end;

define C-function gdk-drag-context-get-suggested-action
  input parameter self :: <GdkDragContext>;
  result res :: <GdkDragAction>;
  c-name: "gdk_drag_context_get_suggested_action";
end;

define C-function gdk-drag-context-list-targets
  input parameter self :: <GdkDragContext>;
  result res :: <GList>;
  c-name: "gdk_drag_context_list_targets";
end;

define C-function gdk-drag-context-set-device
  input parameter self :: <GdkDragContext>;
  input parameter device_ :: <GdkDevice>;
  c-name: "gdk_drag_context_set_device";
end;

define constant $gdk-drag-proto-none = 0;
define constant $gdk-drag-proto-motif = 1;
define constant $gdk-drag-proto-xdnd = 2;
define constant $gdk-drag-proto-rootwin = 3;
define constant $gdk-drag-proto-win32-dropfiles = 4;
define constant $gdk-drag-proto-ole2 = 5;
define constant $gdk-drag-proto-local = 6;
define constant <GdkDragProtocol> = <C-int>;
define C-pointer-type <GdkDragProtocol*> => <GdkDragProtocol>;

define C-union <_GdkEvent>
  slot gdk-event-type :: <GdkEventType>;
  slot gdk-event-any :: <GdkEventAny>;
  slot gdk-event-expose :: <GdkEventExpose>;
  slot gdk-event-visibility :: <GdkEventVisibility>;
  slot gdk-event-motion :: <GdkEventMotion>;
  slot gdk-event-button :: <GdkEventButton>;
  slot gdk-event-touch :: <GdkEventTouch>;
  slot gdk-event-scroll :: <GdkEventScroll>;
  slot gdk-event-key :: <GdkEventKey>;
  slot gdk-event-crossing :: <GdkEventCrossing>;
  slot gdk-event-focus-change :: <GdkEventFocus>;
  slot gdk-event-configure :: <GdkEventConfigure>;
  slot gdk-event-property :: <GdkEventProperty>;
  slot gdk-event-selection :: <GdkEventSelection>;
  slot gdk-event-owner-change :: <GdkEventOwnerChange>;
  slot gdk-event-proximity :: <GdkEventProximity>;
  slot gdk-event-dnd :: <GdkEventDND>;
  slot gdk-event-window-state :: <GdkEventWindowState>;
  slot gdk-event-setting :: <GdkEventSetting>;
  slot gdk-event-grab-broken :: <GdkEventGrabBroken>;
  pointer-type-name: <GdkEvent>;
end C-union;

define C-function gdk-event-new
  input parameter type_ :: <GdkEventType>;
  result res :: <GdkEvent>;
  c-name: "gdk_event_new";
end;

define C-function gdk-events-get-angle
  input parameter self :: <GdkEvent>;
  input parameter event2_ :: <GdkEvent>;
  output parameter angle_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_events_get_angle";
end;

define C-function gdk-events-get-center
  input parameter self :: <GdkEvent>;
  input parameter event2_ :: <GdkEvent>;
  output parameter x_ :: <C-double*>;
  output parameter y_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_events_get_center";
end;

define C-function gdk-events-get-distance
  input parameter self :: <GdkEvent>;
  input parameter event2_ :: <GdkEvent>;
  output parameter distance_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_events_get_distance";
end;

define C-function gdk-event-copy
  input parameter self :: <GdkEvent>;
  result res :: <GdkEvent>;
  c-name: "gdk_event_copy";
end;

define C-function gdk-event-free
  input parameter self :: <GdkEvent>;
  c-name: "gdk_event_free";
end;

define C-function gdk-event-get-axis
  input parameter self :: <GdkEvent>;
  input parameter axis_use_ :: <GdkAxisUse>;
  output parameter value_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_axis";
end;

define C-function gdk-event-get-button
  input parameter self :: <GdkEvent>;
  output parameter button_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_button";
end;

define C-function gdk-event-get-click-count
  input parameter self :: <GdkEvent>;
  output parameter click_count_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_click_count";
end;

define C-function gdk-event-get-coords
  input parameter self :: <GdkEvent>;
  output parameter x_win_ :: <C-double*>;
  output parameter y_win_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_coords";
end;

define C-function gdk-event-get-device
  input parameter self :: <GdkEvent>;
  result res :: <GdkDevice>;
  c-name: "gdk_event_get_device";
end;

define C-function gdk-event-get-keycode
  input parameter self :: <GdkEvent>;
  output parameter keycode_ :: <C-unsigned-short*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_keycode";
end;

define C-function gdk-event-get-keyval
  input parameter self :: <GdkEvent>;
  output parameter keyval_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_keyval";
end;

define C-function gdk-event-get-root-coords
  input parameter self :: <GdkEvent>;
  output parameter x_root_ :: <C-double*>;
  output parameter y_root_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_root_coords";
end;

define C-function gdk-event-get-screen
  input parameter self :: <GdkEvent>;
  result res :: <GdkScreen>;
  c-name: "gdk_event_get_screen";
end;

define C-function gdk-event-get-scroll-deltas
  input parameter self :: <GdkEvent>;
  input parameter delta_x_ :: <C-double*>;
  input parameter delta_y_ :: <C-double*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_scroll_deltas";
end;

define C-function gdk-event-get-scroll-direction
  input parameter self :: <GdkEvent>;
  output parameter direction_ :: <GdkScrollDirection*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_scroll_direction";
end;

define C-function gdk-event-get-source-device
  input parameter self :: <GdkEvent>;
  result res :: <GdkDevice>;
  c-name: "gdk_event_get_source_device";
end;

define C-function gdk-event-get-state
  input parameter self :: <GdkEvent>;
  output parameter state_ :: <GdkModifierType*>;
  result res :: <C-boolean>;
  c-name: "gdk_event_get_state";
end;

define C-function gdk-event-get-time
  input parameter self :: <GdkEvent>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_event_get_time";
end;

define C-function gdk-event-put
  input parameter self :: <GdkEvent>;
  c-name: "gdk_event_put";
end;

define C-function gdk-event-set-device
  input parameter self :: <GdkEvent>;
  input parameter device_ :: <GdkDevice>;
  c-name: "gdk_event_set_device";
end;

define C-function gdk-event-set-screen
  input parameter self :: <GdkEvent>;
  input parameter screen_ :: <GdkScreen>;
  c-name: "gdk_event_set_screen";
end;

define C-function gdk-event-set-source-device
  input parameter self :: <GdkEvent>;
  input parameter device_ :: <GdkDevice>;
  c-name: "gdk_event_set_source_device";
end;

define C-function gdk-event-triggers-context-menu
  input parameter self :: <GdkEvent>;
  result res :: <C-boolean>;
  c-name: "gdk_event_triggers_context_menu";
end;

define C-function gdk-event-get
  result res :: <GdkEvent>;
  c-name: "gdk_event_get";
end;

define C-function gdk-event-handler-set
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "gdk_event_handler_set";
end;

define C-function gdk-event-peek
  result res :: <GdkEvent>;
  c-name: "gdk_event_peek";
end;

define C-function gdk-event-request-motions
  input parameter event_ :: <GdkEventMotion>;
  c-name: "gdk_event_request_motions";
end;

define C-struct <_GdkEventAny>
  slot gdk-event-any-type :: <GdkEventType>;
  slot gdk-event-any-window :: <GdkWindow>;
  slot gdk-event-any-send-event :: <C-signed-char>;
  pointer-type-name: <GdkEventAny>;
end C-struct;

define C-struct <_GdkEventButton>
  slot gdk-event-button-type :: <GdkEventType>;
  slot gdk-event-button-window :: <GdkWindow>;
  slot gdk-event-button-send-event :: <C-signed-char>;
  slot gdk-event-button-time :: <C-unsigned-int>;
  slot gdk-event-button-x :: <C-double>;
  slot gdk-event-button-y :: <C-double>;
  slot gdk-event-button-axes :: <C-double*>;
  slot gdk-event-button-state :: <GdkModifierType>;
  slot gdk-event-button-button :: <C-unsigned-int>;
  slot gdk-event-button-device :: <GdkDevice>;
  slot gdk-event-button-x-root :: <C-double>;
  slot gdk-event-button-y-root :: <C-double>;
  pointer-type-name: <GdkEventButton>;
end C-struct;

define C-struct <_GdkEventConfigure>
  slot gdk-event-configure-type :: <GdkEventType>;
  slot gdk-event-configure-window :: <GdkWindow>;
  slot gdk-event-configure-send-event :: <C-signed-char>;
  slot gdk-event-configure-x :: <C-signed-int>;
  slot gdk-event-configure-y :: <C-signed-int>;
  slot gdk-event-configure-width :: <C-signed-int>;
  slot gdk-event-configure-height :: <C-signed-int>;
  pointer-type-name: <GdkEventConfigure>;
end C-struct;

define C-struct <_GdkEventCrossing>
  slot gdk-event-crossing-type :: <GdkEventType>;
  slot gdk-event-crossing-window :: <GdkWindow>;
  slot gdk-event-crossing-send-event :: <C-signed-char>;
  slot gdk-event-crossing-subwindow :: <GdkWindow>;
  slot gdk-event-crossing-time :: <C-unsigned-int>;
  slot gdk-event-crossing-x :: <C-double>;
  slot gdk-event-crossing-y :: <C-double>;
  slot gdk-event-crossing-x-root :: <C-double>;
  slot gdk-event-crossing-y-root :: <C-double>;
  slot gdk-event-crossing-mode :: <GdkCrossingMode>;
  slot gdk-event-crossing-detail :: <GdkNotifyType>;
  slot gdk-event-crossing-focus :: <C-boolean>;
  slot gdk-event-crossing-state :: <GdkModifierType>;
  pointer-type-name: <GdkEventCrossing>;
end C-struct;

define C-struct <_GdkEventDND>
  slot gdk-event-dnd-type :: <GdkEventType>;
  slot gdk-event-dnd-window :: <GdkWindow>;
  slot gdk-event-dnd-send-event :: <C-signed-char>;
  slot gdk-event-dnd-context :: <GdkDragContext>;
  slot gdk-event-dnd-time :: <C-unsigned-int>;
  slot gdk-event-dnd-x-root :: <C-signed-short>;
  slot gdk-event-dnd-y-root :: <C-signed-short>;
  pointer-type-name: <GdkEventDND>;
end C-struct;

define C-struct <_GdkEventExpose>
  slot gdk-event-expose-type :: <GdkEventType>;
  slot gdk-event-expose-window :: <GdkWindow>;
  slot gdk-event-expose-send-event :: <C-signed-char>;
  slot gdk-event-expose-area :: <cairoRectangleInt>;
  slot gdk-event-expose-region :: <cairoRegion>;
  slot gdk-event-expose-count :: <C-signed-int>;
  pointer-type-name: <GdkEventExpose>;
end C-struct;

define C-struct <_GdkEventFocus>
  slot gdk-event-focus-type :: <GdkEventType>;
  slot gdk-event-focus-window :: <GdkWindow>;
  slot gdk-event-focus-send-event :: <C-signed-char>;
  slot gdk-event-focus-in :: <C-signed-short>;
  pointer-type-name: <GdkEventFocus>;
end C-struct;

define C-struct <_GdkEventGrabBroken>
  slot gdk-event-grab-broken-type :: <GdkEventType>;
  slot gdk-event-grab-broken-window :: <GdkWindow>;
  slot gdk-event-grab-broken-send-event :: <C-signed-char>;
  slot gdk-event-grab-broken-keyboard :: <C-boolean>;
  slot gdk-event-grab-broken-implicit :: <C-boolean>;
  slot gdk-event-grab-broken-grab-window :: <GdkWindow>;
  pointer-type-name: <GdkEventGrabBroken>;
end C-struct;

define C-struct <_GdkEventKey>
  slot gdk-event-key-type :: <GdkEventType>;
  slot gdk-event-key-window :: <GdkWindow>;
  slot gdk-event-key-send-event :: <C-signed-char>;
  slot gdk-event-key-time :: <C-unsigned-int>;
  slot gdk-event-key-state :: <GdkModifierType>;
  slot gdk-event-key-keyval :: <C-unsigned-int>;
  slot gdk-event-key-length :: <C-signed-int>;
  slot gdk-event-key-string :: <C-string>;
  slot gdk-event-key-hardware-keycode :: <C-unsigned-short>;
  slot gdk-event-key-group :: <C-unsigned-char>;
  slot gdk-event-key-is-modifier :: <C-unsigned-int>;
  pointer-type-name: <GdkEventKey>;
end C-struct;

define constant $gdk-exposure-mask = 2;
define constant $gdk-pointer-motion-mask = 4;
define constant $gdk-pointer-motion-hint-mask = 8;
define constant $gdk-button-motion-mask = 16;
define constant $gdk-button1-motion-mask = 32;
define constant $gdk-button2-motion-mask = 64;
define constant $gdk-button3-motion-mask = 128;
define constant $gdk-button-press-mask = 256;
define constant $gdk-button-release-mask = 512;
define constant $gdk-key-press-mask = 1024;
define constant $gdk-key-release-mask = 2048;
define constant $gdk-enter-notify-mask = 4096;
define constant $gdk-leave-notify-mask = 8192;
define constant $gdk-focus-change-mask = 16384;
define constant $gdk-structure-mask = 32768;
define constant $gdk-property-change-mask = 65536;
define constant $gdk-visibility-notify-mask = 131072;
define constant $gdk-proximity-in-mask = 262144;
define constant $gdk-proximity-out-mask = 524288;
define constant $gdk-substructure-mask = 1048576;
define constant $gdk-scroll-mask = 2097152;
define constant $gdk-touch-mask = 4194304;
define constant $gdk-smooth-scroll-mask = 8388608;
define constant $gdk-all-events-mask = 16777214;
define constant <GdkEventMask> = <C-int>;
define C-pointer-type <GdkEventMask*> => <GdkEventMask>;

define C-struct <_GdkEventMotion>
  slot gdk-event-motion-type :: <GdkEventType>;
  slot gdk-event-motion-window :: <GdkWindow>;
  slot gdk-event-motion-send-event :: <C-signed-char>;
  slot gdk-event-motion-time :: <C-unsigned-int>;
  slot gdk-event-motion-x :: <C-double>;
  slot gdk-event-motion-y :: <C-double>;
  slot gdk-event-motion-axes :: <C-double*>;
  slot gdk-event-motion-state :: <GdkModifierType>;
  slot gdk-event-motion-is-hint :: <C-signed-short>;
  slot gdk-event-motion-device :: <GdkDevice>;
  slot gdk-event-motion-x-root :: <C-double>;
  slot gdk-event-motion-y-root :: <C-double>;
  pointer-type-name: <GdkEventMotion>;
end C-struct;

define C-struct <_GdkEventOwnerChange>
  slot gdk-event-owner-change-type :: <GdkEventType>;
  slot gdk-event-owner-change-window :: <GdkWindow>;
  slot gdk-event-owner-change-send-event :: <C-signed-char>;
  slot gdk-event-owner-change-owner :: <GdkWindow>;
  slot gdk-event-owner-change-reason :: <GdkOwnerChange>;
  slot gdk-event-owner-change-selection :: <GdkAtom>;
  slot gdk-event-owner-change-time :: <C-unsigned-int>;
  slot gdk-event-owner-change-selection-time :: <C-unsigned-int>;
  pointer-type-name: <GdkEventOwnerChange>;
end C-struct;

define C-struct <_GdkEventProperty>
  slot gdk-event-property-type :: <GdkEventType>;
  slot gdk-event-property-window :: <GdkWindow>;
  slot gdk-event-property-send-event :: <C-signed-char>;
  slot gdk-event-property-atom :: <GdkAtom>;
  slot gdk-event-property-time :: <C-unsigned-int>;
  slot gdk-event-property-state :: <C-unsigned-int>;
  pointer-type-name: <GdkEventProperty>;
end C-struct;

define C-struct <_GdkEventProximity>
  slot gdk-event-proximity-type :: <GdkEventType>;
  slot gdk-event-proximity-window :: <GdkWindow>;
  slot gdk-event-proximity-send-event :: <C-signed-char>;
  slot gdk-event-proximity-time :: <C-unsigned-int>;
  slot gdk-event-proximity-device :: <GdkDevice>;
  pointer-type-name: <GdkEventProximity>;
end C-struct;

define C-struct <_GdkEventScroll>
  slot gdk-event-scroll-type :: <GdkEventType>;
  slot gdk-event-scroll-window :: <GdkWindow>;
  slot gdk-event-scroll-send-event :: <C-signed-char>;
  slot gdk-event-scroll-time :: <C-unsigned-int>;
  slot gdk-event-scroll-x :: <C-double>;
  slot gdk-event-scroll-y :: <C-double>;
  slot gdk-event-scroll-state :: <GdkModifierType>;
  slot gdk-event-scroll-direction :: <GdkScrollDirection>;
  slot gdk-event-scroll-device :: <GdkDevice>;
  slot gdk-event-scroll-x-root :: <C-double>;
  slot gdk-event-scroll-y-root :: <C-double>;
  slot gdk-event-scroll-delta-x :: <C-double>;
  slot gdk-event-scroll-delta-y :: <C-double>;
  pointer-type-name: <GdkEventScroll>;
end C-struct;

define C-struct <_GdkEventSelection>
  slot gdk-event-selection-type :: <GdkEventType>;
  slot gdk-event-selection-window :: <GdkWindow>;
  slot gdk-event-selection-send-event :: <C-signed-char>;
  slot gdk-event-selection-selection :: <GdkAtom>;
  slot gdk-event-selection-target :: <GdkAtom>;
  slot gdk-event-selection-property :: <GdkAtom>;
  slot gdk-event-selection-time :: <C-unsigned-int>;
  slot gdk-event-selection-requestor :: <GdkWindow>;
  pointer-type-name: <GdkEventSelection>;
end C-struct;

define C-struct <_GdkEventSequence>
  pointer-type-name: <GdkEventSequence>;
end C-struct;

define C-struct <_GdkEventSetting>
  slot gdk-event-setting-type :: <GdkEventType>;
  slot gdk-event-setting-window :: <GdkWindow>;
  slot gdk-event-setting-send-event :: <C-signed-char>;
  slot gdk-event-setting-action :: <GdkSettingAction>;
  slot gdk-event-setting-name :: <C-string>;
  pointer-type-name: <GdkEventSetting>;
end C-struct;

define C-struct <_GdkEventTouch>
  slot gdk-event-touch-type :: <GdkEventType>;
  slot gdk-event-touch-window :: <GdkWindow>;
  slot gdk-event-touch-send-event :: <C-signed-char>;
  slot gdk-event-touch-time :: <C-unsigned-int>;
  slot gdk-event-touch-x :: <C-double>;
  slot gdk-event-touch-y :: <C-double>;
  slot gdk-event-touch-axes :: <C-double*>;
  slot gdk-event-touch-state :: <GdkModifierType>;
  slot gdk-event-touch-sequence :: <GdkEventSequence>;
  slot gdk-event-touch-emulating-pointer :: <C-boolean>;
  slot gdk-event-touch-device :: <GdkDevice>;
  slot gdk-event-touch-x-root :: <C-double>;
  slot gdk-event-touch-y-root :: <C-double>;
  pointer-type-name: <GdkEventTouch>;
end C-struct;

define constant $gdk-nothing = -1;
define constant $gdk-delete = 0;
define constant $gdk-destroy = 1;
define constant $gdk-expose = 2;
define constant $gdk-motion-notify = 3;
define constant $gdk-button-press = 4;
define constant $gdk-2button-press = 5;
define constant $gdk-3button-press = 6;
define constant $gdk-button-release = 7;
define constant $gdk-key-press = 8;
define constant $gdk-key-release = 9;
define constant $gdk-enter-notify = 10;
define constant $gdk-leave-notify = 11;
define constant $gdk-focus-change = 12;
define constant $gdk-configure = 13;
define constant $gdk-map = 14;
define constant $gdk-unmap = 15;
define constant $gdk-property-notify = 16;
define constant $gdk-selection-clear = 17;
define constant $gdk-selection-request = 18;
define constant $gdk-selection-notify = 19;
define constant $gdk-proximity-in = 20;
define constant $gdk-proximity-out = 21;
define constant $gdk-drag-enter = 22;
define constant $gdk-drag-leave = 23;
define constant $gdk-drag-motion = 24;
define constant $gdk-drag-status = 25;
define constant $gdk-drop-start = 26;
define constant $gdk-drop-finished = 27;
define constant $gdk-client-event = 28;
define constant $gdk-visibility-notify = 29;
define constant $gdk-scroll = 31;
define constant $gdk-window-state = 32;
define constant $gdk-setting = 33;
define constant $gdk-owner-change = 34;
define constant $gdk-grab-broken = 35;
define constant $gdk-damage = 36;
define constant $gdk-touch-begin = 37;
define constant $gdk-touch-update = 38;
define constant $gdk-touch-end = 39;
define constant $gdk-touch-cancel = 40;
define constant $gdk-event-last = 41;
define constant <GdkEventType> = <C-int>;
define C-pointer-type <GdkEventType*> => <GdkEventType>;

define C-struct <_GdkEventVisibility>
  slot gdk-event-visibility-type :: <GdkEventType>;
  slot gdk-event-visibility-window :: <GdkWindow>;
  slot gdk-event-visibility-send-event :: <C-signed-char>;
  slot gdk-event-visibility-state :: <GdkVisibilityState>;
  pointer-type-name: <GdkEventVisibility>;
end C-struct;

define C-struct <_GdkEventWindowState>
  slot gdk-event-window-state-type :: <GdkEventType>;
  slot gdk-event-window-state-window :: <GdkWindow>;
  slot gdk-event-window-state-send-event :: <C-signed-char>;
  slot gdk-event-window-state-changed-mask :: <GdkWindowState>;
  slot gdk-event-window-state-new-window-state :: <GdkWindowState>;
  pointer-type-name: <GdkEventWindowState>;
end C-struct;

define constant $gdk-filter-continue = 0;
define constant $gdk-filter-translate = 1;
define constant $gdk-filter-remove = 2;
define constant <GdkFilterReturn> = <C-int>;
define C-pointer-type <GdkFilterReturn*> => <GdkFilterReturn>;

define C-struct <_GdkGeometry>
  slot gdk-geometry-min-width :: <C-signed-int>;
  slot gdk-geometry-min-height :: <C-signed-int>;
  slot gdk-geometry-max-width :: <C-signed-int>;
  slot gdk-geometry-max-height :: <C-signed-int>;
  slot gdk-geometry-base-width :: <C-signed-int>;
  slot gdk-geometry-base-height :: <C-signed-int>;
  slot gdk-geometry-width-inc :: <C-signed-int>;
  slot gdk-geometry-height-inc :: <C-signed-int>;
  slot gdk-geometry-min-aspect :: <C-double>;
  slot gdk-geometry-max-aspect :: <C-double>;
  slot gdk-geometry-win-gravity :: <GdkGravity>;
  pointer-type-name: <GdkGeometry>;
end C-struct;

define constant $gdk-ownership-none = 0;
define constant $gdk-ownership-window = 1;
define constant $gdk-ownership-application = 2;
define constant <GdkGrabOwnership> = <C-int>;
define C-pointer-type <GdkGrabOwnership*> => <GdkGrabOwnership>;

define constant $gdk-grab-success = 0;
define constant $gdk-grab-already-grabbed = 1;
define constant $gdk-grab-invalid-time = 2;
define constant $gdk-grab-not-viewable = 3;
define constant $gdk-grab-frozen = 4;
define constant <GdkGrabStatus> = <C-int>;
define C-pointer-type <GdkGrabStatus*> => <GdkGrabStatus>;

define constant $gdk-gravity-north-west = 1;
define constant $gdk-gravity-north = 2;
define constant $gdk-gravity-north-east = 3;
define constant $gdk-gravity-west = 4;
define constant $gdk-gravity-center = 5;
define constant $gdk-gravity-east = 6;
define constant $gdk-gravity-south-west = 7;
define constant $gdk-gravity-south = 8;
define constant $gdk-gravity-south-east = 9;
define constant $gdk-gravity-static = 10;
define constant <GdkGravity> = <C-int>;
define C-pointer-type <GdkGravity*> => <GdkGravity>;

define constant $gdk-mode-disabled = 0;
define constant $gdk-mode-screen = 1;
define constant $gdk-mode-window = 2;
define constant <GdkInputMode> = <C-int>;
define C-pointer-type <GdkInputMode*> => <GdkInputMode>;

define constant $gdk-source-mouse = 0;
define constant $gdk-source-pen = 1;
define constant $gdk-source-eraser = 2;
define constant $gdk-source-cursor = 3;
define constant $gdk-source-keyboard = 4;
define constant $gdk-source-touchscreen = 5;
define constant $gdk-source-touchpad = 6;
define constant <GdkInputSource> = <C-int>;
define C-pointer-type <GdkInputSource*> => <GdkInputSource>;

define constant $key-0 = 48;

define constant $key-1 = 49;

define constant $key-2 = 50;

define constant $key-3 = 51;

define constant $key-3270-alt-cursor = 64784;

define constant $key-3270-attn = 64782;

define constant $key-3270-back-tab = 64773;

define constant $key-3270-change-screen = 64793;

define constant $key-3270-copy = 64789;

define constant $key-3270-cursor-blink = 64783;

define constant $key-3270-cursor-select = 64796;

define constant $key-3270-delete-word = 64794;

define constant $key-3270-duplicate = 64769;

define constant $key-3270-enter = 64798;

define constant $key-3270-erase-eof = 64774;

define constant $key-3270-erase-input = 64775;

define constant $key-3270-ex-select = 64795;

define constant $key-3270-field-mark = 64770;

define constant $key-3270-ident = 64787;

define constant $key-3270-jump = 64786;

define constant $key-3270-key-click = 64785;

define constant $key-3270-left2 = 64772;

define constant $key-3270-pa1 = 64778;

define constant $key-3270-pa2 = 64779;

define constant $key-3270-pa3 = 64780;

define constant $key-3270-play = 64790;

define constant $key-3270-print-screen = 64797;

define constant $key-3270-quit = 64777;

define constant $key-3270-record = 64792;

define constant $key-3270-reset = 64776;

define constant $key-3270-right2 = 64771;

define constant $key-3270-rule = 64788;

define constant $key-3270-setup = 64791;

define constant $key-3270-test = 64781;

define constant $key-4 = 52;

define constant $key-5 = 53;

define constant $key-6 = 54;

define constant $key-7 = 55;

define constant $key-8 = 56;

define constant $key-9 = 57;

define constant $key-a = 65;

define constant $key-ae = 198;

define constant $key-aacute = 193;

define constant $key-abelowdot = 16785056;

define constant $key-abreve = 451;

define constant $key-abreveacute = 16785070;

define constant $key-abrevebelowdot = 16785078;

define constant $key-abrevegrave = 16785072;

define constant $key-abrevehook = 16785074;

define constant $key-abrevetilde = 16785076;

define constant $key-access-x-enable = 65136;

define constant $key-access-x-feedback-enable = 65137;

define constant $key-acircumflex = 194;

define constant $key-acircumflexacute = 16785060;

define constant $key-acircumflexbelowdot = 16785068;

define constant $key-acircumflexgrave = 16785062;

define constant $key-acircumflexhook = 16785064;

define constant $key-acircumflextilde = 16785066;

define constant $key-add-favorite = 269025081;

define constant $key-adiaeresis = 196;

define constant $key-agrave = 192;

define constant $key-ahook = 16785058;

define constant $key-alt-l = 65513;

define constant $key-alt-r = 65514;

define constant $key-amacron = 960;

define constant $key-aogonek = 417;

define constant $key-application-left = 269025104;

define constant $key-application-right = 269025105;

define constant $key-arabic-0 = 16778848;

define constant $key-arabic-1 = 16778849;

define constant $key-arabic-2 = 16778850;

define constant $key-arabic-3 = 16778851;

define constant $key-arabic-4 = 16778852;

define constant $key-arabic-5 = 16778853;

define constant $key-arabic-6 = 16778854;

define constant $key-arabic-7 = 16778855;

define constant $key-arabic-8 = 16778856;

define constant $key-arabic-9 = 16778857;

define constant $key-arabic-ain = 1497;

define constant $key-arabic-alef = 1479;

define constant $key-arabic-alefmaksura = 1513;

define constant $key-arabic-beh = 1480;

define constant $key-arabic-comma = 1452;

define constant $key-arabic-dad = 1494;

define constant $key-arabic-dal = 1487;

define constant $key-arabic-damma = 1519;

define constant $key-arabic-dammatan = 1516;

define constant $key-arabic-ddal = 16778888;

define constant $key-arabic-farsi-yeh = 16778956;

define constant $key-arabic-fatha = 1518;

define constant $key-arabic-fathatan = 1515;

define constant $key-arabic-feh = 1505;

define constant $key-arabic-fullstop = 16778964;

define constant $key-arabic-gaf = 16778927;

define constant $key-arabic-ghain = 1498;

define constant $key-arabic-ha = 1511;

define constant $key-arabic-hah = 1485;

define constant $key-arabic-hamza = 1473;

define constant $key-arabic-hamza-above = 16778836;

define constant $key-arabic-hamza-below = 16778837;

define constant $key-arabic-hamzaonalef = 1475;

define constant $key-arabic-hamzaonwaw = 1476;

define constant $key-arabic-hamzaonyeh = 1478;

define constant $key-arabic-hamzaunderalef = 1477;

define constant $key-arabic-heh = 1511;

define constant $key-arabic-heh-doachashmee = 16778942;

define constant $key-arabic-heh-goal = 16778945;

define constant $key-arabic-jeem = 1484;

define constant $key-arabic-jeh = 16778904;

define constant $key-arabic-kaf = 1507;

define constant $key-arabic-kasra = 1520;

define constant $key-arabic-kasratan = 1517;

define constant $key-arabic-keheh = 16778921;

define constant $key-arabic-khah = 1486;

define constant $key-arabic-lam = 1508;

define constant $key-arabic-madda-above = 16778835;

define constant $key-arabic-maddaonalef = 1474;

define constant $key-arabic-meem = 1509;

define constant $key-arabic-noon = 1510;

define constant $key-arabic-noon-ghunna = 16778938;

define constant $key-arabic-peh = 16778878;

define constant $key-arabic-percent = 16778858;

define constant $key-arabic-qaf = 1506;

define constant $key-arabic-question-mark = 1471;

define constant $key-arabic-ra = 1489;

define constant $key-arabic-rreh = 16778897;

define constant $key-arabic-sad = 1493;

define constant $key-arabic-seen = 1491;

define constant $key-arabic-semicolon = 1467;

define constant $key-arabic-shadda = 1521;

define constant $key-arabic-sheen = 1492;

define constant $key-arabic-sukun = 1522;

define constant $key-arabic-superscript-alef = 16778864;

define constant $key-arabic-switch = 65406;

define constant $key-arabic-tah = 1495;

define constant $key-arabic-tatweel = 1504;

define constant $key-arabic-tcheh = 16778886;

define constant $key-arabic-teh = 1482;

define constant $key-arabic-tehmarbuta = 1481;

define constant $key-arabic-thal = 1488;

define constant $key-arabic-theh = 1483;

define constant $key-arabic-tteh = 16778873;

define constant $key-arabic-veh = 16778916;

define constant $key-arabic-waw = 1512;

define constant $key-arabic-yeh = 1514;

define constant $key-arabic-yeh-baree = 16778962;

define constant $key-arabic-zah = 1496;

define constant $key-arabic-zain = 1490;

define constant $key-aring = 197;

define constant $key-armenian-at = 16778552;

define constant $key-armenian-ayb = 16778545;

define constant $key-armenian-ben = 16778546;

define constant $key-armenian-cha = 16778569;

define constant $key-armenian-da = 16778548;

define constant $key-armenian-dza = 16778561;

define constant $key-armenian-e = 16778551;

define constant $key-armenian-fe = 16778582;

define constant $key-armenian-ghat = 16778562;

define constant $key-armenian-gim = 16778547;

define constant $key-armenian-hi = 16778565;

define constant $key-armenian-ho = 16778560;

define constant $key-armenian-ini = 16778555;

define constant $key-armenian-je = 16778571;

define constant $key-armenian-ke = 16778580;

define constant $key-armenian-ken = 16778559;

define constant $key-armenian-khe = 16778557;

define constant $key-armenian-lyun = 16778556;

define constant $key-armenian-men = 16778564;

define constant $key-armenian-nu = 16778566;

define constant $key-armenian-o = 16778581;

define constant $key-armenian-pe = 16778570;

define constant $key-armenian-pyur = 16778579;

define constant $key-armenian-ra = 16778572;

define constant $key-armenian-re = 16778576;

define constant $key-armenian-se = 16778573;

define constant $key-armenian-sha = 16778567;

define constant $key-armenian-tche = 16778563;

define constant $key-armenian-to = 16778553;

define constant $key-armenian-tsa = 16778558;

define constant $key-armenian-tso = 16778577;

define constant $key-armenian-tyun = 16778575;

define constant $key-armenian-vev = 16778574;

define constant $key-armenian-vo = 16778568;

define constant $key-armenian-vyun = 16778578;

define constant $key-armenian-yech = 16778549;

define constant $key-armenian-za = 16778550;

define constant $key-armenian-zhe = 16778554;

define constant $key-armenian-accent = 16778587;

define constant $key-armenian-amanak = 16778588;

define constant $key-armenian-apostrophe = 16778586;

define constant $key-armenian-but = 16778589;

define constant $key-armenian-exclam = 16778588;

define constant $key-armenian-full-stop = 16778633;

define constant $key-armenian-hyphen = 16778634;

define constant $key-armenian-ligature-ew = 16778631;

define constant $key-armenian-paruyk = 16778590;

define constant $key-armenian-question = 16778590;

define constant $key-armenian-separation-mark = 16778589;

define constant $key-armenian-shesht = 16778587;

define constant $key-armenian-verjaket = 16778633;

define constant $key-armenian-yentamna = 16778634;

define constant $key-atilde = 195;

define constant $key-audible-bell-enable = 65146;

define constant $key-audio-cycle-track = 269025179;

define constant $key-audio-forward = 269025175;

define constant $key-audio-lower-volume = 269025041;

define constant $key-audio-media = 269025074;

define constant $key-audio-mute = 269025042;

define constant $key-audio-next = 269025047;

define constant $key-audio-pause = 269025073;

define constant $key-audio-play = 269025044;

define constant $key-audio-prev = 269025046;

define constant $key-audio-raise-volume = 269025043;

define constant $key-audio-random-play = 269025177;

define constant $key-audio-record = 269025052;

define constant $key-audio-repeat = 269025176;

define constant $key-audio-rewind = 269025086;

define constant $key-audio-stop = 269025045;

define constant $key-away = 269025165;

define constant $key-b = 66;

define constant $key-babovedot = 16784898;

define constant $key-back = 269025062;

define constant $key-back-forward = 269025087;

define constant $key-back-space = 65288;

define constant $key-battery = 269025171;

define constant $key-begin = 65368;

define constant $key-blue = 269025190;

define constant $key-bluetooth = 269025172;

define constant $key-book = 269025106;

define constant $key-bounce-keys-enable = 65140;

define constant $key-break = 65387;

define constant $key-brightness-adjust = 269025083;

define constant $key-byelorussian-shortu = 1726;

define constant $key-c = 67;

define constant $key-cd = 269025107;

define constant $key-ch = 65186;

define constant $key-c-h = 65189;

define constant $key-cabovedot = 709;

define constant $key-cacute = 454;

define constant $key-calculator = 269025053;

define constant $key-calendar = 269025056;

define constant $key-cancel = 65385;

define constant $key-caps-lock = 65509;

define constant $key-ccaron = 456;

define constant $key-ccedilla = 199;

define constant $key-ccircumflex = 710;

define constant $key-clear = 65291;

define constant $key-clear-grab = 269024801;

define constant $key-close = 269025110;

define constant $key-codeinput = 65335;

define constant $key-colon-sign = 16785569;

define constant $key-community = 269025085;

define constant $key-contrast-adjust = 269025058;

define constant $key-control-l = 65507;

define constant $key-control-r = 65508;

define constant $key-copy = 269025111;

define constant $key-cruzeiro-sign = 16785570;

define constant $key-cut = 269025112;

define constant $key-cycle-angle = 269025180;

define constant $key-cyrillic-a = 1761;

define constant $key-cyrillic-be = 1762;

define constant $key-cyrillic-che = 1790;

define constant $key-cyrillic-che-descender = 16778422;

define constant $key-cyrillic-che-vertstroke = 16778424;

define constant $key-cyrillic-de = 1764;

define constant $key-cyrillic-dzhe = 1727;

define constant $key-cyrillic-e = 1788;

define constant $key-cyrillic-ef = 1766;

define constant $key-cyrillic-el = 1772;

define constant $key-cyrillic-em = 1773;

define constant $key-cyrillic-en = 1774;

define constant $key-cyrillic-en-descender = 16778402;

define constant $key-cyrillic-er = 1778;

define constant $key-cyrillic-es = 1779;

define constant $key-cyrillic-ghe = 1767;

define constant $key-cyrillic-ghe-bar = 16778386;

define constant $key-cyrillic-ha = 1768;

define constant $key-cyrillic-hardsign = 1791;

define constant $key-cyrillic-ha-descender = 16778418;

define constant $key-cyrillic-i = 1769;

define constant $key-cyrillic-ie = 1765;

define constant $key-cyrillic-io = 1715;

define constant $key-cyrillic-i-macron = 16778466;

define constant $key-cyrillic-je = 1720;

define constant $key-cyrillic-ka = 1771;

define constant $key-cyrillic-ka-descender = 16778394;

define constant $key-cyrillic-ka-vertstroke = 16778396;

define constant $key-cyrillic-lje = 1721;

define constant $key-cyrillic-nje = 1722;

define constant $key-cyrillic-o = 1775;

define constant $key-cyrillic-o-bar = 16778472;

define constant $key-cyrillic-pe = 1776;

define constant $key-cyrillic-schwa = 16778456;

define constant $key-cyrillic-sha = 1787;

define constant $key-cyrillic-shcha = 1789;

define constant $key-cyrillic-shha = 16778426;

define constant $key-cyrillic-shorti = 1770;

define constant $key-cyrillic-softsign = 1784;

define constant $key-cyrillic-te = 1780;

define constant $key-cyrillic-tse = 1763;

define constant $key-cyrillic-u = 1781;

define constant $key-cyrillic-u-macron = 16778478;

define constant $key-cyrillic-u-straight = 16778414;

define constant $key-cyrillic-u-straight-bar = 16778416;

define constant $key-cyrillic-ve = 1783;

define constant $key-cyrillic-ya = 1777;

define constant $key-cyrillic-yeru = 1785;

define constant $key-cyrillic-yu = 1760;

define constant $key-cyrillic-ze = 1786;

define constant $key-cyrillic-zhe = 1782;

define constant $key-cyrillic-zhe-descender = 16778390;

define constant $key-d = 68;

define constant $key-dos = 269025114;

define constant $key-dabovedot = 16784906;

define constant $key-dcaron = 463;

define constant $key-delete = 65535;

define constant $key-display = 269025113;

define constant $key-documents = 269025115;

define constant $key-dong-sign = 16785579;

define constant $key-down = 65364;

define constant $key-dstroke = 464;

define constant $key-e = 69;

define constant $key-eng = 957;

define constant $key-eth = 208;

define constant $key-ezh = 16777655;

define constant $key-eabovedot = 972;

define constant $key-eacute = 201;

define constant $key-ebelowdot = 16785080;

define constant $key-ecaron = 460;

define constant $key-ecircumflex = 202;

define constant $key-ecircumflexacute = 16785086;

define constant $key-ecircumflexbelowdot = 16785094;

define constant $key-ecircumflexgrave = 16785088;

define constant $key-ecircumflexhook = 16785090;

define constant $key-ecircumflextilde = 16785092;

define constant $key-ecu-sign = 16785568;

define constant $key-ediaeresis = 203;

define constant $key-egrave = 200;

define constant $key-ehook = 16785082;

define constant $key-eisu-shift = 65327;

define constant $key-eisu-toggle = 65328;

define constant $key-eject = 269025068;

define constant $key-emacron = 938;

define constant $key-end = 65367;

define constant $key-eogonek = 458;

define constant $key-escape = 65307;

define constant $key-etilde = 16785084;

define constant $key-euro-sign = 8364;

define constant $key-excel = 269025116;

define constant $key-execute = 65378;

define constant $key-explorer = 269025117;

define constant $key-f = 70;

define constant $key-f1 = 65470;

define constant $key-f10 = 65479;

define constant $key-f11 = 65480;

define constant $key-f12 = 65481;

define constant $key-f13 = 65482;

define constant $key-f14 = 65483;

define constant $key-f15 = 65484;

define constant $key-f16 = 65485;

define constant $key-f17 = 65486;

define constant $key-f18 = 65487;

define constant $key-f19 = 65488;

define constant $key-f2 = 65471;

define constant $key-f20 = 65489;

define constant $key-f21 = 65490;

define constant $key-f22 = 65491;

define constant $key-f23 = 65492;

define constant $key-f24 = 65493;

define constant $key-f25 = 65494;

define constant $key-f26 = 65495;

define constant $key-f27 = 65496;

define constant $key-f28 = 65497;

define constant $key-f29 = 65498;

define constant $key-f3 = 65472;

define constant $key-f30 = 65499;

define constant $key-f31 = 65500;

define constant $key-f32 = 65501;

define constant $key-f33 = 65502;

define constant $key-f34 = 65503;

define constant $key-f35 = 65504;

define constant $key-f4 = 65473;

define constant $key-f5 = 65474;

define constant $key-f6 = 65475;

define constant $key-f7 = 65476;

define constant $key-f8 = 65477;

define constant $key-f9 = 65478;

define constant $key-f-franc-sign = 16785571;

define constant $key-fabovedot = 16784926;

define constant $key-farsi-0 = 16778992;

define constant $key-farsi-1 = 16778993;

define constant $key-farsi-2 = 16778994;

define constant $key-farsi-3 = 16778995;

define constant $key-farsi-4 = 16778996;

define constant $key-farsi-5 = 16778997;

define constant $key-farsi-6 = 16778998;

define constant $key-farsi-7 = 16778999;

define constant $key-farsi-8 = 16779000;

define constant $key-farsi-9 = 16779001;

define constant $key-farsi-yeh = 16778956;

define constant $key-favorites = 269025072;

define constant $key-finance = 269025084;

define constant $key-find = 65384;

define constant $key-first-virtual-screen = 65232;

define constant $key-forward = 269025063;

define constant $key-frame-back = 269025181;

define constant $key-frame-forward = 269025182;

define constant $key-g = 71;

define constant $key-gabovedot = 725;

define constant $key-game = 269025118;

define constant $key-gbreve = 683;

define constant $key-gcaron = 16777702;

define constant $key-gcedilla = 939;

define constant $key-gcircumflex = 728;

define constant $key-georgian-an = 16781520;

define constant $key-georgian-ban = 16781521;

define constant $key-georgian-can = 16781546;

define constant $key-georgian-char = 16781549;

define constant $key-georgian-chin = 16781545;

define constant $key-georgian-cil = 16781548;

define constant $key-georgian-don = 16781523;

define constant $key-georgian-en = 16781524;

define constant $key-georgian-fi = 16781558;

define constant $key-georgian-gan = 16781522;

define constant $key-georgian-ghan = 16781542;

define constant $key-georgian-hae = 16781552;

define constant $key-georgian-har = 16781556;

define constant $key-georgian-he = 16781553;

define constant $key-georgian-hie = 16781554;

define constant $key-georgian-hoe = 16781557;

define constant $key-georgian-in = 16781528;

define constant $key-georgian-jhan = 16781551;

define constant $key-georgian-jil = 16781547;

define constant $key-georgian-kan = 16781529;

define constant $key-georgian-khar = 16781541;

define constant $key-georgian-las = 16781530;

define constant $key-georgian-man = 16781531;

define constant $key-georgian-nar = 16781532;

define constant $key-georgian-on = 16781533;

define constant $key-georgian-par = 16781534;

define constant $key-georgian-phar = 16781540;

define constant $key-georgian-qar = 16781543;

define constant $key-georgian-rae = 16781536;

define constant $key-georgian-san = 16781537;

define constant $key-georgian-shin = 16781544;

define constant $key-georgian-tan = 16781527;

define constant $key-georgian-tar = 16781538;

define constant $key-georgian-un = 16781539;

define constant $key-georgian-vin = 16781525;

define constant $key-georgian-we = 16781555;

define constant $key-georgian-xan = 16781550;

define constant $key-georgian-zen = 16781526;

define constant $key-georgian-zhar = 16781535;

define constant $key-go = 269025119;

define constant $key-greek-alpha = 1985;

define constant $key-greek-alph-aaccent = 1953;

define constant $key-greek-beta = 1986;

define constant $key-greek-chi = 2007;

define constant $key-greek-delta = 1988;

define constant $key-greek-epsilon = 1989;

define constant $key-greek-epsilo-naccent = 1954;

define constant $key-greek-eta = 1991;

define constant $key-greek-et-aaccent = 1955;

define constant $key-greek-gamma = 1987;

define constant $key-greek-iota = 1993;

define constant $key-greek-iot-aaccent = 1956;

define constant $key-greek-iot-adiaeresis = 1957;

define constant $key-greek-iot-adieresis = 1957;

define constant $key-greek-kappa = 1994;

define constant $key-greek-lambda = 1995;

define constant $key-greek-lamda = 1995;

define constant $key-greek-mu = 1996;

define constant $key-greek-nu = 1997;

define constant $key-greek-omega = 2009;

define constant $key-greek-omeg-aaccent = 1963;

define constant $key-greek-omicron = 1999;

define constant $key-greek-omicro-naccent = 1959;

define constant $key-greek-phi = 2006;

define constant $key-greek-pi = 2000;

define constant $key-greek-psi = 2008;

define constant $key-greek-rho = 2001;

define constant $key-greek-sigma = 2002;

define constant $key-greek-tau = 2004;

define constant $key-greek-theta = 1992;

define constant $key-greek-upsilon = 2005;

define constant $key-greek-upsilo-naccent = 1960;

define constant $key-greek-upsilo-ndieresis = 1961;

define constant $key-greek-xi = 1998;

define constant $key-greek-zeta = 1990;

define constant $key-greek-accentdieresis = 1966;

define constant $key-greek-alphaaccent = 1969;

define constant $key-greek-epsilonaccent = 1970;

define constant $key-greek-etaaccent = 1971;

define constant $key-greek-finalsmallsigma = 2035;

define constant $key-greek-horizbar = 1967;

define constant $key-greek-iotaaccent = 1972;

define constant $key-greek-iotaaccentdieresis = 1974;

define constant $key-greek-iotadieresis = 1973;

define constant $key-greek-omegaaccent = 1979;

define constant $key-greek-omicronaccent = 1975;

define constant $key-greek-switch = 65406;

define constant $key-greek-upsilonaccent = 1976;

define constant $key-greek-upsilonaccentdieresis = 1978;

define constant $key-greek-upsilondieresis = 1977;

define constant $key-green = 269025188;

define constant $key-h = 72;

define constant $key-hangul = 65329;

define constant $key-hangul-a = 3775;

define constant $key-hangul-ae = 3776;

define constant $key-hangul-arae-a = 3830;

define constant $key-hangul-arae-ae = 3831;

define constant $key-hangul-banja = 65337;

define constant $key-hangul-cieuc = 3770;

define constant $key-hangul-codeinput = 65335;

define constant $key-hangul-dikeud = 3751;

define constant $key-hangul-e = 3780;

define constant $key-hangul-eo = 3779;

define constant $key-hangul-eu = 3793;

define constant $key-hangul-end = 65331;

define constant $key-hangul-hanja = 65332;

define constant $key-hangul-hieuh = 3774;

define constant $key-hangul-i = 3795;

define constant $key-hangul-ieung = 3767;

define constant $key-hangul-j-cieuc = 3818;

define constant $key-hangul-j-dikeud = 3802;

define constant $key-hangul-j-hieuh = 3822;

define constant $key-hangul-j-ieung = 3816;

define constant $key-hangul-j-jieuj = 3817;

define constant $key-hangul-j-khieuq = 3819;

define constant $key-hangul-j-kiyeog = 3796;

define constant $key-hangul-j-kiyeog-sios = 3798;

define constant $key-hangul-j-kkogji-dalrin-ieung = 3833;

define constant $key-hangul-j-mieum = 3811;

define constant $key-hangul-j-nieun = 3799;

define constant $key-hangul-j-nieun-hieuh = 3801;

define constant $key-hangul-j-nieun-jieuj = 3800;

define constant $key-hangul-j-pan-sios = 3832;

define constant $key-hangul-j-phieuf = 3821;

define constant $key-hangul-j-pieub = 3812;

define constant $key-hangul-j-pieub-sios = 3813;

define constant $key-hangul-j-rieul = 3803;

define constant $key-hangul-j-rieul-hieuh = 3810;

define constant $key-hangul-j-rieul-kiyeog = 3804;

define constant $key-hangul-j-rieul-mieum = 3805;

define constant $key-hangul-j-rieul-phieuf = 3809;

define constant $key-hangul-j-rieul-pieub = 3806;

define constant $key-hangul-j-rieul-sios = 3807;

define constant $key-hangul-j-rieul-tieut = 3808;

define constant $key-hangul-j-sios = 3814;

define constant $key-hangul-j-ssang-kiyeog = 3797;

define constant $key-hangul-j-ssang-sios = 3815;

define constant $key-hangul-j-tieut = 3820;

define constant $key-hangul-j-yeorin-hieuh = 3834;

define constant $key-hangul-jamo = 65333;

define constant $key-hangul-jeonja = 65336;

define constant $key-hangul-jieuj = 3768;

define constant $key-hangul-khieuq = 3771;

define constant $key-hangul-kiyeog = 3745;

define constant $key-hangul-kiyeog-sios = 3747;

define constant $key-hangul-kkogji-dalrin-ieung = 3827;

define constant $key-hangul-mieum = 3761;

define constant $key-hangul-multiple-candidate = 65341;

define constant $key-hangul-nieun = 3748;

define constant $key-hangul-nieun-hieuh = 3750;

define constant $key-hangul-nieun-jieuj = 3749;

define constant $key-hangul-o = 3783;

define constant $key-hangul-oe = 3786;

define constant $key-hangul-pan-sios = 3826;

define constant $key-hangul-phieuf = 3773;

define constant $key-hangul-pieub = 3762;

define constant $key-hangul-pieub-sios = 3764;

define constant $key-hangul-post-hanja = 65339;

define constant $key-hangul-pre-hanja = 65338;

define constant $key-hangul-previous-candidate = 65342;

define constant $key-hangul-rieul = 3753;

define constant $key-hangul-rieul-hieuh = 3760;

define constant $key-hangul-rieul-kiyeog = 3754;

define constant $key-hangul-rieul-mieum = 3755;

define constant $key-hangul-rieul-phieuf = 3759;

define constant $key-hangul-rieul-pieub = 3756;

define constant $key-hangul-rieul-sios = 3757;

define constant $key-hangul-rieul-tieut = 3758;

define constant $key-hangul-rieul-yeorin-hieuh = 3823;

define constant $key-hangul-romaja = 65334;

define constant $key-hangul-single-candidate = 65340;

define constant $key-hangul-sios = 3765;

define constant $key-hangul-special = 65343;

define constant $key-hangul-ssang-dikeud = 3752;

define constant $key-hangul-ssang-jieuj = 3769;

define constant $key-hangul-ssang-kiyeog = 3746;

define constant $key-hangul-ssang-pieub = 3763;

define constant $key-hangul-ssang-sios = 3766;

define constant $key-hangul-start = 65330;

define constant $key-hangul-sunkyeongeum-mieum = 3824;

define constant $key-hangul-sunkyeongeum-phieuf = 3828;

define constant $key-hangul-sunkyeongeum-pieub = 3825;

define constant $key-hangul-tieut = 3772;

define constant $key-hangul-u = 3788;

define constant $key-hangul-wa = 3784;

define constant $key-hangul-wae = 3785;

define constant $key-hangul-we = 3790;

define constant $key-hangul-weo = 3789;

define constant $key-hangul-wi = 3791;

define constant $key-hangul-ya = 3777;

define constant $key-hangul-yae = 3778;

define constant $key-hangul-ye = 3782;

define constant $key-hangul-yeo = 3781;

define constant $key-hangul-yi = 3794;

define constant $key-hangul-yo = 3787;

define constant $key-hangul-yu = 3792;

define constant $key-hangul-yeorin-hieuh = 3829;

define constant $key-hangul-switch = 65406;

define constant $key-hankaku = 65321;

define constant $key-hcircumflex = 678;

define constant $key-hebrew-switch = 65406;

define constant $key-help = 65386;

define constant $key-henkan = 65315;

define constant $key-henkan-mode = 65315;

define constant $key-hibernate = 269025192;

define constant $key-hiragana = 65317;

define constant $key-hiragana-katakana = 65319;

define constant $key-history = 269025079;

define constant $key-home = 65360;

define constant $key-home-page = 269025048;

define constant $key-hot-links = 269025082;

define constant $key-hstroke = 673;

define constant $key-hyper-l = 65517;

define constant $key-hyper-r = 65518;

define constant $key-i = 73;

define constant $key-iso-center-object = 65075;

define constant $key-iso-continuous-underline = 65072;

define constant $key-iso-discontinuous-underline = 65073;

define constant $key-iso-emphasize = 65074;

define constant $key-iso-enter = 65076;

define constant $key-iso-fast-cursor-down = 65071;

define constant $key-iso-fast-cursor-left = 65068;

define constant $key-iso-fast-cursor-right = 65069;

define constant $key-iso-fast-cursor-up = 65070;

define constant $key-iso-first-group = 65036;

define constant $key-iso-first-group-lock = 65037;

define constant $key-iso-group-latch = 65030;

define constant $key-iso-group-lock = 65031;

define constant $key-iso-group-shift = 65406;

define constant $key-iso-last-group = 65038;

define constant $key-iso-last-group-lock = 65039;

define constant $key-iso-left-tab = 65056;

define constant $key-iso-level2-latch = 65026;

define constant $key-iso-level3-latch = 65028;

define constant $key-iso-level3-lock = 65029;

define constant $key-iso-level3-shift = 65027;

define constant $key-iso-level5-latch = 65042;

define constant $key-iso-level5-lock = 65043;

define constant $key-iso-level5-shift = 65041;

define constant $key-iso-lock = 65025;

define constant $key-iso-move-line-down = 65058;

define constant $key-iso-move-line-up = 65057;

define constant $key-iso-next-group = 65032;

define constant $key-iso-next-group-lock = 65033;

define constant $key-iso-partial-line-down = 65060;

define constant $key-iso-partial-line-up = 65059;

define constant $key-iso-partial-space-left = 65061;

define constant $key-iso-partial-space-right = 65062;

define constant $key-iso-prev-group = 65034;

define constant $key-iso-prev-group-lock = 65035;

define constant $key-iso-release-both-margins = 65067;

define constant $key-iso-release-margin-left = 65065;

define constant $key-iso-release-margin-right = 65066;

define constant $key-iso-set-margin-left = 65063;

define constant $key-iso-set-margin-right = 65064;

define constant $key-iabovedot = 681;

define constant $key-iacute = 205;

define constant $key-ibelowdot = 16785098;

define constant $key-ibreve = 16777516;

define constant $key-icircumflex = 206;

define constant $key-idiaeresis = 207;

define constant $key-igrave = 204;

define constant $key-ihook = 16785096;

define constant $key-imacron = 975;

define constant $key-insert = 65379;

define constant $key-iogonek = 967;

define constant $key-itilde = 933;

define constant $key-j = 74;

define constant $key-jcircumflex = 684;

define constant $key-k = 75;

define constant $key-kp-0 = 65456;

define constant $key-kp-1 = 65457;

define constant $key-kp-2 = 65458;

define constant $key-kp-3 = 65459;

define constant $key-kp-4 = 65460;

define constant $key-kp-5 = 65461;

define constant $key-kp-6 = 65462;

define constant $key-kp-7 = 65463;

define constant $key-kp-8 = 65464;

define constant $key-kp-9 = 65465;

define constant $key-kp-add = 65451;

define constant $key-kp-begin = 65437;

define constant $key-kp-decimal = 65454;

define constant $key-kp-delete = 65439;

define constant $key-kp-divide = 65455;

define constant $key-kp-down = 65433;

define constant $key-kp-end = 65436;

define constant $key-kp-enter = 65421;

define constant $key-kp-equal = 65469;

define constant $key-kp-f1 = 65425;

define constant $key-kp-f2 = 65426;

define constant $key-kp-f3 = 65427;

define constant $key-kp-f4 = 65428;

define constant $key-kp-home = 65429;

define constant $key-kp-insert = 65438;

define constant $key-kp-left = 65430;

define constant $key-kp-multiply = 65450;

define constant $key-kp-next = 65435;

define constant $key-kp-page-down = 65435;

define constant $key-kp-page-up = 65434;

define constant $key-kp-prior = 65434;

define constant $key-kp-right = 65432;

define constant $key-kp-separator = 65452;

define constant $key-kp-space = 65408;

define constant $key-kp-subtract = 65453;

define constant $key-kp-tab = 65417;

define constant $key-kp-up = 65431;

define constant $key-kana-lock = 65325;

define constant $key-kana-shift = 65326;

define constant $key-kanji = 65313;

define constant $key-kanji-bangou = 65335;

define constant $key-katakana = 65318;

define constant $key-kbd-brightness-down = 269025030;

define constant $key-kbd-brightness-up = 269025029;

define constant $key-kbd-light-on-off = 269025028;

define constant $key-kcedilla = 979;

define constant $key-korean-won = 3839;

define constant $key-l = 76;

define constant $key-l1 = 65480;

define constant $key-l10 = 65489;

define constant $key-l2 = 65481;

define constant $key-l3 = 65482;

define constant $key-l4 = 65483;

define constant $key-l5 = 65484;

define constant $key-l6 = 65485;

define constant $key-l7 = 65486;

define constant $key-l8 = 65487;

define constant $key-l9 = 65488;

define constant $key-lacute = 453;

define constant $key-last-virtual-screen = 65236;

define constant $key-launch0 = 269025088;

define constant $key-launch1 = 269025089;

define constant $key-launch2 = 269025090;

define constant $key-launch3 = 269025091;

define constant $key-launch4 = 269025092;

define constant $key-launch5 = 269025093;

define constant $key-launch6 = 269025094;

define constant $key-launch7 = 269025095;

define constant $key-launch8 = 269025096;

define constant $key-launch9 = 269025097;

define constant $key-launch-a = 269025098;

define constant $key-launch-b = 269025099;

define constant $key-launch-c = 269025100;

define constant $key-launch-d = 269025101;

define constant $key-launch-e = 269025102;

define constant $key-launch-f = 269025103;

define constant $key-lbelowdot = 16784950;

define constant $key-lcaron = 421;

define constant $key-lcedilla = 934;

define constant $key-left = 65361;

define constant $key-light-bulb = 269025077;

define constant $key-linefeed = 65290;

define constant $key-lira-sign = 16785572;

define constant $key-log-grab-info = 269024805;

define constant $key-log-off = 269025121;

define constant $key-log-window-tree = 269024804;

define constant $key-lstroke = 419;

define constant $key-m = 77;

define constant $key-mabovedot = 16784960;

define constant $key-macedonia-dse = 1717;

define constant $key-macedonia-gje = 1714;

define constant $key-macedonia-kje = 1724;

define constant $key-mae-koho = 65342;

define constant $key-mail = 269025049;

define constant $key-mail-forward = 269025168;

define constant $key-market = 269025122;

define constant $key-massyo = 65324;

define constant $key-meeting = 269025123;

define constant $key-memo = 269025054;

define constant $key-menu = 65383;

define constant $key-menu-kb = 269025125;

define constant $key-menu-pb = 269025126;

define constant $key-messenger = 269025166;

define constant $key-meta-l = 65511;

define constant $key-meta-r = 65512;

define constant $key-mill-sign = 16785573;

define constant $key-mode-lock = 269025025;

define constant $key-mode-switch = 65406;

define constant $key-mon-brightness-down = 269025027;

define constant $key-mon-brightness-up = 269025026;

define constant $key-mouse-keys-accel-enable = 65143;

define constant $key-mouse-keys-enable = 65142;

define constant $key-muhenkan = 65314;

define constant $key-multi-key = 65312;

define constant $key-multiple-candidate = 65341;

define constant $key-music = 269025170;

define constant $key-my-computer = 269025075;

define constant $key-my-sites = 269025127;

define constant $key-n = 78;

define constant $key-nacute = 465;

define constant $key-naira-sign = 16785574;

define constant $key-ncaron = 466;

define constant $key-ncedilla = 977;

define constant $key-new = 269025128;

define constant $key-new-sheqel-sign = 16785578;

define constant $key-news = 269025129;

define constant $key-next = 65366;

define constant $key-next-v-mode = 269024802;

define constant $key-next-virtual-screen = 65234;

define constant $key-ntilde = 209;

define constant $key-num-lock = 65407;

define constant $key-o = 79;

define constant $key-oe = 5052;

define constant $key-oacute = 211;

define constant $key-obarred = 16777631;

define constant $key-obelowdot = 16785100;

define constant $key-ocaron = 16777681;

define constant $key-ocircumflex = 212;

define constant $key-ocircumflexacute = 16785104;

define constant $key-ocircumflexbelowdot = 16785112;

define constant $key-ocircumflexgrave = 16785106;

define constant $key-ocircumflexhook = 16785108;

define constant $key-ocircumflextilde = 16785110;

define constant $key-odiaeresis = 214;

define constant $key-odoubleacute = 469;

define constant $key-office-home = 269025130;

define constant $key-ograve = 210;

define constant $key-ohook = 16785102;

define constant $key-ohorn = 16777632;

define constant $key-ohornacute = 16785114;

define constant $key-ohornbelowdot = 16785122;

define constant $key-ohorngrave = 16785116;

define constant $key-ohornhook = 16785118;

define constant $key-ohorntilde = 16785120;

define constant $key-omacron = 978;

define constant $key-ooblique = 216;

define constant $key-open = 269025131;

define constant $key-open-url = 269025080;

define constant $key-option = 269025132;

define constant $key-oslash = 216;

define constant $key-otilde = 213;

define constant $key-overlay1-enable = 65144;

define constant $key-overlay2-enable = 65145;

define constant $key-p = 80;

define constant $key-pabovedot = 16784982;

define constant $key-page-down = 65366;

define constant $key-page-up = 65365;

define constant $key-paste = 269025133;

define constant $key-pause = 65299;

define constant $key-peseta-sign = 16785575;

define constant $key-phone = 269025134;

define constant $key-pictures = 269025169;

define constant $key-pointer-accelerate = 65274;

define constant $key-pointer-button1 = 65257;

define constant $key-pointer-button2 = 65258;

define constant $key-pointer-button3 = 65259;

define constant $key-pointer-button4 = 65260;

define constant $key-pointer-button5 = 65261;

define constant $key-pointer-button-dflt = 65256;

define constant $key-pointer-dbl-click1 = 65263;

define constant $key-pointer-dbl-click2 = 65264;

define constant $key-pointer-dbl-click3 = 65265;

define constant $key-pointer-dbl-click4 = 65266;

define constant $key-pointer-dbl-click5 = 65267;

define constant $key-pointer-dbl-click-dflt = 65262;

define constant $key-pointer-dflt-btn-next = 65275;

define constant $key-pointer-dflt-btn-prev = 65276;

define constant $key-pointer-down = 65251;

define constant $key-pointer-down-left = 65254;

define constant $key-pointer-down-right = 65255;

define constant $key-pointer-drag1 = 65269;

define constant $key-pointer-drag2 = 65270;

define constant $key-pointer-drag3 = 65271;

define constant $key-pointer-drag4 = 65272;

define constant $key-pointer-drag5 = 65277;

define constant $key-pointer-drag-dflt = 65268;

define constant $key-pointer-enable-keys = 65273;

define constant $key-pointer-left = 65248;

define constant $key-pointer-right = 65249;

define constant $key-pointer-up = 65250;

define constant $key-pointer-up-left = 65252;

define constant $key-pointer-up-right = 65253;

define constant $key-power-down = 269025057;

define constant $key-power-off = 269025066;

define constant $key-prev-v-mode = 269024803;

define constant $key-prev-virtual-screen = 65233;

define constant $key-previous-candidate = 65342;

define constant $key-print = 65377;

define constant $key-prior = 65365;

define constant $key-q = 81;

define constant $key-r = 82;

define constant $key-r1 = 65490;

define constant $key-r10 = 65499;

define constant $key-r11 = 65500;

define constant $key-r12 = 65501;

define constant $key-r13 = 65502;

define constant $key-r14 = 65503;

define constant $key-r15 = 65504;

define constant $key-r2 = 65491;

define constant $key-r3 = 65492;

define constant $key-r4 = 65493;

define constant $key-r5 = 65494;

define constant $key-r6 = 65495;

define constant $key-r7 = 65496;

define constant $key-r8 = 65497;

define constant $key-r9 = 65498;

define constant $key-racute = 448;

define constant $key-rcaron = 472;

define constant $key-rcedilla = 931;

define constant $key-red = 269025187;

define constant $key-redo = 65382;

define constant $key-refresh = 269025065;

define constant $key-reload = 269025139;

define constant $key-repeat-keys-enable = 65138;

define constant $key-reply = 269025138;

define constant $key-return = 65293;

define constant $key-right = 65363;

define constant $key-rocker-down = 269025060;

define constant $key-rocker-enter = 269025061;

define constant $key-rocker-up = 269025059;

define constant $key-romaji = 65316;

define constant $key-rotate-windows = 269025140;

define constant $key-rotation-kb = 269025142;

define constant $key-rotation-pb = 269025141;

define constant $key-rupee-sign = 16785576;

define constant $key-s = 83;

define constant $key-schwa = 16777615;

define constant $key-sabovedot = 16784992;

define constant $key-sacute = 422;

define constant $key-save = 269025143;

define constant $key-scaron = 425;

define constant $key-scedilla = 426;

define constant $key-scircumflex = 734;

define constant $key-screen-saver = 269025069;

define constant $key-scroll-click = 269025146;

define constant $key-scroll-down = 269025145;

define constant $key-scroll-up = 269025144;

define constant $key-scroll-lock = 65300;

define constant $key-search = 269025051;

define constant $key-select = 65376;

define constant $key-select-button = 269025184;

define constant $key-send = 269025147;

define constant $key-serbian-dje = 1713;

define constant $key-serbian-dze = 1727;

define constant $key-serbian-je = 1720;

define constant $key-serbian-lje = 1721;

define constant $key-serbian-nje = 1722;

define constant $key-serbian-tshe = 1723;

define constant $key-shift-l = 65505;

define constant $key-shift-lock = 65510;

define constant $key-shift-r = 65506;

define constant $key-shop = 269025078;

define constant $key-single-candidate = 65340;

define constant $key-sinh-a = 16780677;

define constant $key-sinh-aa = 16780678;

define constant $key-sinh-aa2 = 16780751;

define constant $key-sinh-ae = 16780679;

define constant $key-sinh-ae2 = 16780752;

define constant $key-sinh-aee = 16780680;

define constant $key-sinh-aee2 = 16780753;

define constant $key-sinh-ai = 16780691;

define constant $key-sinh-ai2 = 16780763;

define constant $key-sinh-al = 16780746;

define constant $key-sinh-au = 16780694;

define constant $key-sinh-au2 = 16780766;

define constant $key-sinh-ba = 16780726;

define constant $key-sinh-bha = 16780727;

define constant $key-sinh-ca = 16780704;

define constant $key-sinh-cha = 16780705;

define constant $key-sinh-dda = 16780713;

define constant $key-sinh-ddha = 16780714;

define constant $key-sinh-dha = 16780719;

define constant $key-sinh-dhha = 16780720;

define constant $key-sinh-e = 16780689;

define constant $key-sinh-e2 = 16780761;

define constant $key-sinh-ee = 16780690;

define constant $key-sinh-ee2 = 16780762;

define constant $key-sinh-fa = 16780742;

define constant $key-sinh-ga = 16780700;

define constant $key-sinh-gha = 16780701;

define constant $key-sinh-h2 = 16780675;

define constant $key-sinh-ha = 16780740;

define constant $key-sinh-i = 16780681;

define constant $key-sinh-i2 = 16780754;

define constant $key-sinh-ii = 16780682;

define constant $key-sinh-ii2 = 16780755;

define constant $key-sinh-ja = 16780706;

define constant $key-sinh-jha = 16780707;

define constant $key-sinh-jnya = 16780709;

define constant $key-sinh-ka = 16780698;

define constant $key-sinh-kha = 16780699;

define constant $key-sinh-kunddaliya = 16780788;

define constant $key-sinh-la = 16780733;

define constant $key-sinh-lla = 16780741;

define constant $key-sinh-lu = 16780687;

define constant $key-sinh-lu2 = 16780767;

define constant $key-sinh-luu = 16780688;

define constant $key-sinh-luu2 = 16780787;

define constant $key-sinh-ma = 16780728;

define constant $key-sinh-mba = 16780729;

define constant $key-sinh-na = 16780721;

define constant $key-sinh-ndda = 16780716;

define constant $key-sinh-ndha = 16780723;

define constant $key-sinh-ng = 16780674;

define constant $key-sinh-ng2 = 16780702;

define constant $key-sinh-nga = 16780703;

define constant $key-sinh-nja = 16780710;

define constant $key-sinh-nna = 16780715;

define constant $key-sinh-nya = 16780708;

define constant $key-sinh-o = 16780692;

define constant $key-sinh-o2 = 16780764;

define constant $key-sinh-oo = 16780693;

define constant $key-sinh-oo2 = 16780765;

define constant $key-sinh-pa = 16780724;

define constant $key-sinh-pha = 16780725;

define constant $key-sinh-ra = 16780731;

define constant $key-sinh-ri = 16780685;

define constant $key-sinh-rii = 16780686;

define constant $key-sinh-ru2 = 16780760;

define constant $key-sinh-ruu2 = 16780786;

define constant $key-sinh-sa = 16780739;

define constant $key-sinh-sha = 16780737;

define constant $key-sinh-ssha = 16780738;

define constant $key-sinh-tha = 16780717;

define constant $key-sinh-thha = 16780718;

define constant $key-sinh-tta = 16780711;

define constant $key-sinh-ttha = 16780712;

define constant $key-sinh-u = 16780683;

define constant $key-sinh-u2 = 16780756;

define constant $key-sinh-uu = 16780684;

define constant $key-sinh-uu2 = 16780758;

define constant $key-sinh-va = 16780736;

define constant $key-sinh-ya = 16780730;

define constant $key-sleep = 269025071;

define constant $key-slow-keys-enable = 65139;

define constant $key-spell = 269025148;

define constant $key-split-screen = 269025149;

define constant $key-standby = 269025040;

define constant $key-start = 269025050;

define constant $key-sticky-keys-enable = 65141;

define constant $key-stop = 269025064;

define constant $key-subtitle = 269025178;

define constant $key-super-l = 65515;

define constant $key-super-r = 65516;

define constant $key-support = 269025150;

define constant $key-suspend = 269025191;

define constant $key-switch-vt-1 = 269024769;

define constant $key-switch-vt-10 = 269024778;

define constant $key-switch-vt-11 = 269024779;

define constant $key-switch-vt-12 = 269024780;

define constant $key-switch-vt-2 = 269024770;

define constant $key-switch-vt-3 = 269024771;

define constant $key-switch-vt-4 = 269024772;

define constant $key-switch-vt-5 = 269024773;

define constant $key-switch-vt-6 = 269024774;

define constant $key-switch-vt-7 = 269024775;

define constant $key-switch-vt-8 = 269024776;

define constant $key-switch-vt-9 = 269024777;

define constant $key-sys-req = 65301;

define constant $key-t = 84;

define constant $key-thorn = 222;

define constant $key-tab = 65289;

define constant $key-tabovedot = 16785002;

define constant $key-task-pane = 269025151;

define constant $key-tcaron = 427;

define constant $key-tcedilla = 478;

define constant $key-terminal = 269025152;

define constant $key-terminate-server = 65237;

define constant $key-thai-baht = 3551;

define constant $key-thai-bobaimai = 3514;

define constant $key-thai-chochan = 3496;

define constant $key-thai-chochang = 3498;

define constant $key-thai-choching = 3497;

define constant $key-thai-chochoe = 3500;

define constant $key-thai-dochada = 3502;

define constant $key-thai-dodek = 3508;

define constant $key-thai-fofa = 3517;

define constant $key-thai-fofan = 3519;

define constant $key-thai-hohip = 3531;

define constant $key-thai-honokhuk = 3534;

define constant $key-thai-khokhai = 3490;

define constant $key-thai-khokhon = 3493;

define constant $key-thai-khokhuat = 3491;

define constant $key-thai-khokhwai = 3492;

define constant $key-thai-khorakhang = 3494;

define constant $key-thai-kokai = 3489;

define constant $key-thai-lakkhangyao = 3557;

define constant $key-thai-lekchet = 3575;

define constant $key-thai-lekha = 3573;

define constant $key-thai-lekhok = 3574;

define constant $key-thai-lekkao = 3577;

define constant $key-thai-leknung = 3569;

define constant $key-thai-lekpaet = 3576;

define constant $key-thai-leksam = 3571;

define constant $key-thai-leksi = 3572;

define constant $key-thai-leksong = 3570;

define constant $key-thai-leksun = 3568;

define constant $key-thai-lochula = 3532;

define constant $key-thai-loling = 3525;

define constant $key-thai-lu = 3526;

define constant $key-thai-maichattawa = 3563;

define constant $key-thai-maiek = 3560;

define constant $key-thai-maihanakat = 3537;

define constant $key-thai-maihanakat-maitho = 3550;

define constant $key-thai-maitaikhu = 3559;

define constant $key-thai-maitho = 3561;

define constant $key-thai-maitri = 3562;

define constant $key-thai-maiyamok = 3558;

define constant $key-thai-moma = 3521;

define constant $key-thai-ngongu = 3495;

define constant $key-thai-nikhahit = 3565;

define constant $key-thai-nonen = 3507;

define constant $key-thai-nonu = 3513;

define constant $key-thai-oang = 3533;

define constant $key-thai-paiyannoi = 3535;

define constant $key-thai-phinthu = 3546;

define constant $key-thai-phophan = 3518;

define constant $key-thai-phophung = 3516;

define constant $key-thai-phosamphao = 3520;

define constant $key-thai-popla = 3515;

define constant $key-thai-rorua = 3523;

define constant $key-thai-ru = 3524;

define constant $key-thai-saraa = 3536;

define constant $key-thai-saraaa = 3538;

define constant $key-thai-saraae = 3553;

define constant $key-thai-saraaimaimalai = 3556;

define constant $key-thai-saraaimaimuan = 3555;

define constant $key-thai-saraam = 3539;

define constant $key-thai-sarae = 3552;

define constant $key-thai-sarai = 3540;

define constant $key-thai-saraii = 3541;

define constant $key-thai-sarao = 3554;

define constant $key-thai-sarau = 3544;

define constant $key-thai-saraue = 3542;

define constant $key-thai-sarauee = 3543;

define constant $key-thai-sarauu = 3545;

define constant $key-thai-sorusi = 3529;

define constant $key-thai-sosala = 3528;

define constant $key-thai-soso = 3499;

define constant $key-thai-sosua = 3530;

define constant $key-thai-thanthakhat = 3564;

define constant $key-thai-thonangmontho = 3505;

define constant $key-thai-thophuthao = 3506;

define constant $key-thai-thothahan = 3511;

define constant $key-thai-thothan = 3504;

define constant $key-thai-thothong = 3512;

define constant $key-thai-thothung = 3510;

define constant $key-thai-topatak = 3503;

define constant $key-thai-totao = 3509;

define constant $key-thai-wowaen = 3527;

define constant $key-thai-yoyak = 3522;

define constant $key-thai-yoying = 3501;

define constant $key-time = 269025183;

define constant $key-to-do-list = 269025055;

define constant $key-tools = 269025153;

define constant $key-top-menu = 269025186;

define constant $key-touchpad-off = 269025201;

define constant $key-touchpad-on = 269025200;

define constant $key-touchpad-toggle = 269025193;

define constant $key-touroku = 65323;

define constant $key-travel = 269025154;

define constant $key-tslash = 940;

define constant $key-u = 85;

define constant $key-uwb = 269025174;

define constant $key-uacute = 218;

define constant $key-ubelowdot = 16785124;

define constant $key-ubreve = 733;

define constant $key-ucircumflex = 219;

define constant $key-udiaeresis = 220;

define constant $key-udoubleacute = 475;

define constant $key-ugrave = 217;

define constant $key-uhook = 16785126;

define constant $key-uhorn = 16777647;

define constant $key-uhornacute = 16785128;

define constant $key-uhornbelowdot = 16785136;

define constant $key-uhorngrave = 16785130;

define constant $key-uhornhook = 16785132;

define constant $key-uhorntilde = 16785134;

define constant $key-ukrainian-ghe-with-upturn = 1725;

define constant $key-ukrainian-i = 1718;

define constant $key-ukrainian-ie = 1716;

define constant $key-ukrainian-yi = 1719;

define constant $key-ukranian-i = 1718;

define constant $key-ukranian-je = 1716;

define constant $key-ukranian-yi = 1719;

define constant $key-umacron = 990;

define constant $key-undo = 65381;

define constant $key-ungrab = 269024800;

define constant $key-uogonek = 985;

define constant $key-up = 65362;

define constant $key-uring = 473;

define constant $key-user1kb = 269025157;

define constant $key-user2kb = 269025158;

define constant $key-user-pb = 269025156;

define constant $key-utilde = 989;

define constant $key-v = 86;

define constant $key-vendor-home = 269025076;

define constant $key-video = 269025159;

define constant $key-view = 269025185;

define constant $key-void-symbol = 16777215;

define constant $key-w = 87;

define constant $key-wlan = 269025173;

define constant $key-www = 269025070;

define constant $key-wacute = 16785026;

define constant $key-wake-up = 269025067;

define constant $key-wcircumflex = 16777588;

define constant $key-wdiaeresis = 16785028;

define constant $key-web-cam = 269025167;

define constant $key-wgrave = 16785024;

define constant $key-wheel-button = 269025160;

define constant $key-window-clear = 269025109;

define constant $key-won-sign = 16785577;

define constant $key-word = 269025161;

define constant $key-x = 88;

define constant $key-xabovedot = 16785034;

define constant $key-xfer = 269025162;

define constant $key-y = 89;

define constant $key-yacute = 221;

define constant $key-ybelowdot = 16785140;

define constant $key-ycircumflex = 16777590;

define constant $key-ydiaeresis = 5054;

define constant $key-yellow = 269025189;

define constant $key-ygrave = 16785138;

define constant $key-yhook = 16785142;

define constant $key-ytilde = 16785144;

define constant $key-z = 90;

define constant $key-zabovedot = 431;

define constant $key-zacute = 428;

define constant $key-zcaron = 430;

define constant $key-zen-koho = 65341;

define constant $key-zenkaku = 65320;

define constant $key-zenkaku-hankaku = 65322;

define constant $key-zoom-in = 269025163;

define constant $key-zoom-out = 269025164;

define constant $key-zstroke = 16777653;

define constant $key-abovedot = 511;

define constant $key-acute = 180;

define constant $key-ampersand = 38;

define constant $key-apostrophe = 39;

define constant $key-approxeq = 16785992;

define constant $key-approximate = 2248;

define constant $key-asciicircum = 94;

define constant $key-asciitilde = 126;

define constant $key-asterisk = 42;

define constant $key-at = 64;

define constant $key-backslash = 92;

define constant $key-ballotcross = 2804;

define constant $key-bar = 124;

define constant $key-because = 16785973;

define constant $key-blank = 2527;

define constant $key-botintegral = 2213;

define constant $key-botleftparens = 2220;

define constant $key-botleftsqbracket = 2216;

define constant $key-botleftsummation = 2226;

define constant $key-botrightparens = 2222;

define constant $key-botrightsqbracket = 2218;

define constant $key-botrightsummation = 2230;

define constant $key-bott = 2550;

define constant $key-botvertsummationconnector = 2228;

define constant $key-braceleft = 123;

define constant $key-braceright = 125;

define constant $key-bracketleft = 91;

define constant $key-bracketright = 93;

define constant $key-braille-blank = 16787456;

define constant $key-braille-dot-1 = 65521;

define constant $key-braille-dot-10 = 65530;

define constant $key-braille-dot-2 = 65522;

define constant $key-braille-dot-3 = 65523;

define constant $key-braille-dot-4 = 65524;

define constant $key-braille-dot-5 = 65525;

define constant $key-braille-dot-6 = 65526;

define constant $key-braille-dot-7 = 65527;

define constant $key-braille-dot-8 = 65528;

define constant $key-braille-dot-9 = 65529;

define constant $key-braille-dots-1 = 16787457;

define constant $key-braille-dots-12 = 16787459;

define constant $key-braille-dots-123 = 16787463;

define constant $key-braille-dots-1234 = 16787471;

define constant $key-braille-dots-12345 = 16787487;

define constant $key-braille-dots-123456 = 16787519;

define constant $key-braille-dots-1234567 = 16787583;

define constant $key-braille-dots-12345678 = 16787711;

define constant $key-braille-dots-1234568 = 16787647;

define constant $key-braille-dots-123457 = 16787551;

define constant $key-braille-dots-1234578 = 16787679;

define constant $key-braille-dots-123458 = 16787615;

define constant $key-braille-dots-12346 = 16787503;

define constant $key-braille-dots-123467 = 16787567;

define constant $key-braille-dots-1234678 = 16787695;

define constant $key-braille-dots-123468 = 16787631;

define constant $key-braille-dots-12347 = 16787535;

define constant $key-braille-dots-123478 = 16787663;

define constant $key-braille-dots-12348 = 16787599;

define constant $key-braille-dots-1235 = 16787479;

define constant $key-braille-dots-12356 = 16787511;

define constant $key-braille-dots-123567 = 16787575;

define constant $key-braille-dots-1235678 = 16787703;

define constant $key-braille-dots-123568 = 16787639;

define constant $key-braille-dots-12357 = 16787543;

define constant $key-braille-dots-123578 = 16787671;

define constant $key-braille-dots-12358 = 16787607;

define constant $key-braille-dots-1236 = 16787495;

define constant $key-braille-dots-12367 = 16787559;

define constant $key-braille-dots-123678 = 16787687;

define constant $key-braille-dots-12368 = 16787623;

define constant $key-braille-dots-1237 = 16787527;

define constant $key-braille-dots-12378 = 16787655;

define constant $key-braille-dots-1238 = 16787591;

define constant $key-braille-dots-124 = 16787467;

define constant $key-braille-dots-1245 = 16787483;

define constant $key-braille-dots-12456 = 16787515;

define constant $key-braille-dots-124567 = 16787579;

define constant $key-braille-dots-1245678 = 16787707;

define constant $key-braille-dots-124568 = 16787643;

define constant $key-braille-dots-12457 = 16787547;

define constant $key-braille-dots-124578 = 16787675;

define constant $key-braille-dots-12458 = 16787611;

define constant $key-braille-dots-1246 = 16787499;

define constant $key-braille-dots-12467 = 16787563;

define constant $key-braille-dots-124678 = 16787691;

define constant $key-braille-dots-12468 = 16787627;

define constant $key-braille-dots-1247 = 16787531;

define constant $key-braille-dots-12478 = 16787659;

define constant $key-braille-dots-1248 = 16787595;

define constant $key-braille-dots-125 = 16787475;

define constant $key-braille-dots-1256 = 16787507;

define constant $key-braille-dots-12567 = 16787571;

define constant $key-braille-dots-125678 = 16787699;

define constant $key-braille-dots-12568 = 16787635;

define constant $key-braille-dots-1257 = 16787539;

define constant $key-braille-dots-12578 = 16787667;

define constant $key-braille-dots-1258 = 16787603;

define constant $key-braille-dots-126 = 16787491;

define constant $key-braille-dots-1267 = 16787555;

define constant $key-braille-dots-12678 = 16787683;

define constant $key-braille-dots-1268 = 16787619;

define constant $key-braille-dots-127 = 16787523;

define constant $key-braille-dots-1278 = 16787651;

define constant $key-braille-dots-128 = 16787587;

define constant $key-braille-dots-13 = 16787461;

define constant $key-braille-dots-134 = 16787469;

define constant $key-braille-dots-1345 = 16787485;

define constant $key-braille-dots-13456 = 16787517;

define constant $key-braille-dots-134567 = 16787581;

define constant $key-braille-dots-1345678 = 16787709;

define constant $key-braille-dots-134568 = 16787645;

define constant $key-braille-dots-13457 = 16787549;

define constant $key-braille-dots-134578 = 16787677;

define constant $key-braille-dots-13458 = 16787613;

define constant $key-braille-dots-1346 = 16787501;

define constant $key-braille-dots-13467 = 16787565;

define constant $key-braille-dots-134678 = 16787693;

define constant $key-braille-dots-13468 = 16787629;

define constant $key-braille-dots-1347 = 16787533;

define constant $key-braille-dots-13478 = 16787661;

define constant $key-braille-dots-1348 = 16787597;

define constant $key-braille-dots-135 = 16787477;

define constant $key-braille-dots-1356 = 16787509;

define constant $key-braille-dots-13567 = 16787573;

define constant $key-braille-dots-135678 = 16787701;

define constant $key-braille-dots-13568 = 16787637;

define constant $key-braille-dots-1357 = 16787541;

define constant $key-braille-dots-13578 = 16787669;

define constant $key-braille-dots-1358 = 16787605;

define constant $key-braille-dots-136 = 16787493;

define constant $key-braille-dots-1367 = 16787557;

define constant $key-braille-dots-13678 = 16787685;

define constant $key-braille-dots-1368 = 16787621;

define constant $key-braille-dots-137 = 16787525;

define constant $key-braille-dots-1378 = 16787653;

define constant $key-braille-dots-138 = 16787589;

define constant $key-braille-dots-14 = 16787465;

define constant $key-braille-dots-145 = 16787481;

define constant $key-braille-dots-1456 = 16787513;

define constant $key-braille-dots-14567 = 16787577;

define constant $key-braille-dots-145678 = 16787705;

define constant $key-braille-dots-14568 = 16787641;

define constant $key-braille-dots-1457 = 16787545;

define constant $key-braille-dots-14578 = 16787673;

define constant $key-braille-dots-1458 = 16787609;

define constant $key-braille-dots-146 = 16787497;

define constant $key-braille-dots-1467 = 16787561;

define constant $key-braille-dots-14678 = 16787689;

define constant $key-braille-dots-1468 = 16787625;

define constant $key-braille-dots-147 = 16787529;

define constant $key-braille-dots-1478 = 16787657;

define constant $key-braille-dots-148 = 16787593;

define constant $key-braille-dots-15 = 16787473;

define constant $key-braille-dots-156 = 16787505;

define constant $key-braille-dots-1567 = 16787569;

define constant $key-braille-dots-15678 = 16787697;

define constant $key-braille-dots-1568 = 16787633;

define constant $key-braille-dots-157 = 16787537;

define constant $key-braille-dots-1578 = 16787665;

define constant $key-braille-dots-158 = 16787601;

define constant $key-braille-dots-16 = 16787489;

define constant $key-braille-dots-167 = 16787553;

define constant $key-braille-dots-1678 = 16787681;

define constant $key-braille-dots-168 = 16787617;

define constant $key-braille-dots-17 = 16787521;

define constant $key-braille-dots-178 = 16787649;

define constant $key-braille-dots-18 = 16787585;

define constant $key-braille-dots-2 = 16787458;

define constant $key-braille-dots-23 = 16787462;

define constant $key-braille-dots-234 = 16787470;

define constant $key-braille-dots-2345 = 16787486;

define constant $key-braille-dots-23456 = 16787518;

define constant $key-braille-dots-234567 = 16787582;

define constant $key-braille-dots-2345678 = 16787710;

define constant $key-braille-dots-234568 = 16787646;

define constant $key-braille-dots-23457 = 16787550;

define constant $key-braille-dots-234578 = 16787678;

define constant $key-braille-dots-23458 = 16787614;

define constant $key-braille-dots-2346 = 16787502;

define constant $key-braille-dots-23467 = 16787566;

define constant $key-braille-dots-234678 = 16787694;

define constant $key-braille-dots-23468 = 16787630;

define constant $key-braille-dots-2347 = 16787534;

define constant $key-braille-dots-23478 = 16787662;

define constant $key-braille-dots-2348 = 16787598;

define constant $key-braille-dots-235 = 16787478;

define constant $key-braille-dots-2356 = 16787510;

define constant $key-braille-dots-23567 = 16787574;

define constant $key-braille-dots-235678 = 16787702;

define constant $key-braille-dots-23568 = 16787638;

define constant $key-braille-dots-2357 = 16787542;

define constant $key-braille-dots-23578 = 16787670;

define constant $key-braille-dots-2358 = 16787606;

define constant $key-braille-dots-236 = 16787494;

define constant $key-braille-dots-2367 = 16787558;

define constant $key-braille-dots-23678 = 16787686;

define constant $key-braille-dots-2368 = 16787622;

define constant $key-braille-dots-237 = 16787526;

define constant $key-braille-dots-2378 = 16787654;

define constant $key-braille-dots-238 = 16787590;

define constant $key-braille-dots-24 = 16787466;

define constant $key-braille-dots-245 = 16787482;

define constant $key-braille-dots-2456 = 16787514;

define constant $key-braille-dots-24567 = 16787578;

define constant $key-braille-dots-245678 = 16787706;

define constant $key-braille-dots-24568 = 16787642;

define constant $key-braille-dots-2457 = 16787546;

define constant $key-braille-dots-24578 = 16787674;

define constant $key-braille-dots-2458 = 16787610;

define constant $key-braille-dots-246 = 16787498;

define constant $key-braille-dots-2467 = 16787562;

define constant $key-braille-dots-24678 = 16787690;

define constant $key-braille-dots-2468 = 16787626;

define constant $key-braille-dots-247 = 16787530;

define constant $key-braille-dots-2478 = 16787658;

define constant $key-braille-dots-248 = 16787594;

define constant $key-braille-dots-25 = 16787474;

define constant $key-braille-dots-256 = 16787506;

define constant $key-braille-dots-2567 = 16787570;

define constant $key-braille-dots-25678 = 16787698;

define constant $key-braille-dots-2568 = 16787634;

define constant $key-braille-dots-257 = 16787538;

define constant $key-braille-dots-2578 = 16787666;

define constant $key-braille-dots-258 = 16787602;

define constant $key-braille-dots-26 = 16787490;

define constant $key-braille-dots-267 = 16787554;

define constant $key-braille-dots-2678 = 16787682;

define constant $key-braille-dots-268 = 16787618;

define constant $key-braille-dots-27 = 16787522;

define constant $key-braille-dots-278 = 16787650;

define constant $key-braille-dots-28 = 16787586;

define constant $key-braille-dots-3 = 16787460;

define constant $key-braille-dots-34 = 16787468;

define constant $key-braille-dots-345 = 16787484;

define constant $key-braille-dots-3456 = 16787516;

define constant $key-braille-dots-34567 = 16787580;

define constant $key-braille-dots-345678 = 16787708;

define constant $key-braille-dots-34568 = 16787644;

define constant $key-braille-dots-3457 = 16787548;

define constant $key-braille-dots-34578 = 16787676;

define constant $key-braille-dots-3458 = 16787612;

define constant $key-braille-dots-346 = 16787500;

define constant $key-braille-dots-3467 = 16787564;

define constant $key-braille-dots-34678 = 16787692;

define constant $key-braille-dots-3468 = 16787628;

define constant $key-braille-dots-347 = 16787532;

define constant $key-braille-dots-3478 = 16787660;

define constant $key-braille-dots-348 = 16787596;

define constant $key-braille-dots-35 = 16787476;

define constant $key-braille-dots-356 = 16787508;

define constant $key-braille-dots-3567 = 16787572;

define constant $key-braille-dots-35678 = 16787700;

define constant $key-braille-dots-3568 = 16787636;

define constant $key-braille-dots-357 = 16787540;

define constant $key-braille-dots-3578 = 16787668;

define constant $key-braille-dots-358 = 16787604;

define constant $key-braille-dots-36 = 16787492;

define constant $key-braille-dots-367 = 16787556;

define constant $key-braille-dots-3678 = 16787684;

define constant $key-braille-dots-368 = 16787620;

define constant $key-braille-dots-37 = 16787524;

define constant $key-braille-dots-378 = 16787652;

define constant $key-braille-dots-38 = 16787588;

define constant $key-braille-dots-4 = 16787464;

define constant $key-braille-dots-45 = 16787480;

define constant $key-braille-dots-456 = 16787512;

define constant $key-braille-dots-4567 = 16787576;

define constant $key-braille-dots-45678 = 16787704;

define constant $key-braille-dots-4568 = 16787640;

define constant $key-braille-dots-457 = 16787544;

define constant $key-braille-dots-4578 = 16787672;

define constant $key-braille-dots-458 = 16787608;

define constant $key-braille-dots-46 = 16787496;

define constant $key-braille-dots-467 = 16787560;

define constant $key-braille-dots-4678 = 16787688;

define constant $key-braille-dots-468 = 16787624;

define constant $key-braille-dots-47 = 16787528;

define constant $key-braille-dots-478 = 16787656;

define constant $key-braille-dots-48 = 16787592;

define constant $key-braille-dots-5 = 16787472;

define constant $key-braille-dots-56 = 16787504;

define constant $key-braille-dots-567 = 16787568;

define constant $key-braille-dots-5678 = 16787696;

define constant $key-braille-dots-568 = 16787632;

define constant $key-braille-dots-57 = 16787536;

define constant $key-braille-dots-578 = 16787664;

define constant $key-braille-dots-58 = 16787600;

define constant $key-braille-dots-6 = 16787488;

define constant $key-braille-dots-67 = 16787552;

define constant $key-braille-dots-678 = 16787680;

define constant $key-braille-dots-68 = 16787616;

define constant $key-braille-dots-7 = 16787520;

define constant $key-braille-dots-78 = 16787648;

define constant $key-braille-dots-8 = 16787584;

define constant $key-breve = 418;

define constant $key-brokenbar = 166;

define constant $key-careof = 2744;

define constant $key-caret = 2812;

define constant $key-caron = 439;

define constant $key-cedilla = 184;

define constant $key-cent = 162;

define constant $key-checkerboard = 2529;

define constant $key-checkmark = 2803;

define constant $key-circle = 3023;

define constant $key-club = 2796;

define constant $key-colon = 58;

define constant $key-comma = 44;

define constant $key-containsas = 16785931;

define constant $key-copyright = 169;

define constant $key-cr = 2532;

define constant $key-crossinglines = 2542;

define constant $key-cuberoot = 16785947;

define constant $key-currency = 164;

define constant $key-cursor = 2815;

define constant $key-dagger = 2801;

define constant $key-dead-a = 65153;

define constant $key-dead-e = 65155;

define constant $key-dead-i = 65157;

define constant $key-dead-o = 65159;

define constant $key-dead-u = 65161;

define constant $key-dead-abovecomma = 65124;

define constant $key-dead-abovedot = 65110;

define constant $key-dead-abovereversedcomma = 65125;

define constant $key-dead-abovering = 65112;

define constant $key-dead-acute = 65105;

define constant $key-dead-belowbreve = 65131;

define constant $key-dead-belowcircumflex = 65129;

define constant $key-dead-belowcomma = 65134;

define constant $key-dead-belowdiaeresis = 65132;

define constant $key-dead-belowdot = 65120;

define constant $key-dead-belowmacron = 65128;

define constant $key-dead-belowring = 65127;

define constant $key-dead-belowtilde = 65130;

define constant $key-dead-breve = 65109;

define constant $key-dead-capital-schwa = 65163;

define constant $key-dead-caron = 65114;

define constant $key-dead-cedilla = 65115;

define constant $key-dead-circumflex = 65106;

define constant $key-dead-currency = 65135;

define constant $key-dead-dasia = 65125;

define constant $key-dead-diaeresis = 65111;

define constant $key-dead-doubleacute = 65113;

define constant $key-dead-doublegrave = 65126;

define constant $key-dead-grave = 65104;

define constant $key-dead-greek = 65164;

define constant $key-dead-hook = 65121;

define constant $key-dead-horn = 65122;

define constant $key-dead-invertedbreve = 65133;

define constant $key-dead-iota = 65117;

define constant $key-dead-macron = 65108;

define constant $key-dead-ogonek = 65116;

define constant $key-dead-perispomeni = 65107;

define constant $key-dead-psili = 65124;

define constant $key-dead-semivoiced-sound = 65119;

define constant $key-dead-small-schwa = 65162;

define constant $key-dead-stroke = 65123;

define constant $key-dead-tilde = 65107;

define constant $key-dead-voiced-sound = 65118;

define constant $key-decimalpoint = 2749;

define constant $key-degree = 176;

define constant $key-diaeresis = 168;

define constant $key-diamond = 2797;

define constant $key-digitspace = 2725;

define constant $key-dintegral = 16785964;

define constant $key-division = 247;

define constant $key-dollar = 36;

define constant $key-doubbaselinedot = 2735;

define constant $key-doubleacute = 445;

define constant $key-doubledagger = 2802;

define constant $key-doublelowquotemark = 2814;

define constant $key-downarrow = 2302;

define constant $key-downcaret = 2984;

define constant $key-downshoe = 3030;

define constant $key-downstile = 3012;

define constant $key-downtack = 3010;

define constant $key-eightsubscript = 16785544;

define constant $key-eightsuperior = 16785528;

define constant $key-elementof = 16785928;

define constant $key-ellipsis = 2734;

define constant $key-em3space = 2723;

define constant $key-em4space = 2724;

define constant $key-emdash = 2729;

define constant $key-emfilledcircle = 2782;

define constant $key-emfilledrect = 2783;

define constant $key-emopencircle = 2766;

define constant $key-emopenrectangle = 2767;

define constant $key-emptyset = 16785925;

define constant $key-emspace = 2721;

define constant $key-endash = 2730;

define constant $key-enfilledcircbullet = 2790;

define constant $key-enfilledsqbullet = 2791;

define constant $key-enopencircbullet = 2784;

define constant $key-enopensquarebullet = 2785;

define constant $key-enspace = 2722;

define constant $key-equal = 61;

define constant $key-exclam = 33;

define constant $key-exclamdown = 161;

define constant $key-femalesymbol = 2808;

define constant $key-ff = 2531;

define constant $key-figdash = 2747;

define constant $key-filledlefttribullet = 2780;

define constant $key-filledrectbullet = 2779;

define constant $key-filledrighttribullet = 2781;

define constant $key-filledtribulletdown = 2793;

define constant $key-filledtribulletup = 2792;

define constant $key-fiveeighths = 2757;

define constant $key-fivesixths = 2743;

define constant $key-fivesubscript = 16785541;

define constant $key-fivesuperior = 16785525;

define constant $key-fourfifths = 2741;

define constant $key-foursubscript = 16785540;

define constant $key-foursuperior = 16785524;

define constant $key-fourthroot = 16785948;

define constant $key-function = 2294;

define constant $key-grave = 96;

define constant $key-greater = 62;

define constant $key-greaterthanequal = 2238;

define constant $key-guillemotleft = 171;

define constant $key-guillemotright = 187;

define constant $key-hairspace = 2728;

define constant $key-heart = 2798;

define constant $key-hebrew-aleph = 3296;

define constant $key-hebrew-ayin = 3314;

define constant $key-hebrew-bet = 3297;

define constant $key-hebrew-beth = 3297;

define constant $key-hebrew-chet = 3303;

define constant $key-hebrew-dalet = 3299;

define constant $key-hebrew-daleth = 3299;

define constant $key-hebrew-doublelowline = 3295;

define constant $key-hebrew-finalkaph = 3306;

define constant $key-hebrew-finalmem = 3309;

define constant $key-hebrew-finalnun = 3311;

define constant $key-hebrew-finalpe = 3315;

define constant $key-hebrew-finalzade = 3317;

define constant $key-hebrew-finalzadi = 3317;

define constant $key-hebrew-gimel = 3298;

define constant $key-hebrew-gimmel = 3298;

define constant $key-hebrew-he = 3300;

define constant $key-hebrew-het = 3303;

define constant $key-hebrew-kaph = 3307;

define constant $key-hebrew-kuf = 3319;

define constant $key-hebrew-lamed = 3308;

define constant $key-hebrew-mem = 3310;

define constant $key-hebrew-nun = 3312;

define constant $key-hebrew-pe = 3316;

define constant $key-hebrew-qoph = 3319;

define constant $key-hebrew-resh = 3320;

define constant $key-hebrew-samech = 3313;

define constant $key-hebrew-samekh = 3313;

define constant $key-hebrew-shin = 3321;

define constant $key-hebrew-taf = 3322;

define constant $key-hebrew-taw = 3322;

define constant $key-hebrew-tet = 3304;

define constant $key-hebrew-teth = 3304;

define constant $key-hebrew-waw = 3301;

define constant $key-hebrew-yod = 3305;

define constant $key-hebrew-zade = 3318;

define constant $key-hebrew-zadi = 3318;

define constant $key-hebrew-zain = 3302;

define constant $key-hebrew-zayin = 3302;

define constant $key-hexagram = 2778;

define constant $key-horizconnector = 2211;

define constant $key-horizlinescan1 = 2543;

define constant $key-horizlinescan3 = 2544;

define constant $key-horizlinescan5 = 2545;

define constant $key-horizlinescan7 = 2546;

define constant $key-horizlinescan9 = 2547;

define constant $key-ht = 2530;

define constant $key-hyphen = 173;

define constant $key-i-touch = 269025120;

define constant $key-identical = 2255;

define constant $key-idotless = 697;

define constant $key-ifonlyif = 2253;

define constant $key-implies = 2254;

define constant $key-includedin = 2266;

define constant $key-includes = 2267;

define constant $key-infinity = 2242;

define constant $key-integral = 2239;

define constant $key-intersection = 2268;

define constant $key-jot = 3018;

define constant $key-kana-a = 1201;

define constant $key-kana-chi = 1217;

define constant $key-kana-e = 1204;

define constant $key-kana-fu = 1228;

define constant $key-kana-ha = 1226;

define constant $key-kana-he = 1229;

define constant $key-kana-hi = 1227;

define constant $key-kana-ho = 1230;

define constant $key-kana-hu = 1228;

define constant $key-kana-i = 1202;

define constant $key-kana-ka = 1206;

define constant $key-kana-ke = 1209;

define constant $key-kana-ki = 1207;

define constant $key-kana-ko = 1210;

define constant $key-kana-ku = 1208;

define constant $key-kana-ma = 1231;

define constant $key-kana-me = 1234;

define constant $key-kana-mi = 1232;

define constant $key-kana-mo = 1235;

define constant $key-kana-mu = 1233;

define constant $key-kana-n = 1245;

define constant $key-kana-na = 1221;

define constant $key-kana-ne = 1224;

define constant $key-kana-ni = 1222;

define constant $key-kana-no = 1225;

define constant $key-kana-nu = 1223;

define constant $key-kana-o = 1205;

define constant $key-kana-ra = 1239;

define constant $key-kana-re = 1242;

define constant $key-kana-ri = 1240;

define constant $key-kana-ro = 1243;

define constant $key-kana-ru = 1241;

define constant $key-kana-sa = 1211;

define constant $key-kana-se = 1214;

define constant $key-kana-shi = 1212;

define constant $key-kana-so = 1215;

define constant $key-kana-su = 1213;

define constant $key-kana-ta = 1216;

define constant $key-kana-te = 1219;

define constant $key-kana-ti = 1217;

define constant $key-kana-to = 1220;

define constant $key-kana-tsu = 1218;

define constant $key-kana-tu = 1218;

define constant $key-kana-u = 1203;

define constant $key-kana-wa = 1244;

define constant $key-kana-wo = 1190;

define constant $key-kana-ya = 1236;

define constant $key-kana-yo = 1238;

define constant $key-kana-yu = 1237;

define constant $key-kana-closingbracket = 1187;

define constant $key-kana-comma = 1188;

define constant $key-kana-conjunctive = 1189;

define constant $key-kana-fullstop = 1185;

define constant $key-kana-middledot = 1189;

define constant $key-kana-openingbracket = 1186;

define constant $key-kana-switch = 65406;

define constant $key-kappa = 930;

define constant $key-kra = 930;

define constant $key-latincross = 2777;

define constant $key-leftanglebracket = 2748;

define constant $key-leftarrow = 2299;

define constant $key-leftcaret = 2979;

define constant $key-leftdoublequotemark = 2770;

define constant $key-leftmiddlecurlybrace = 2223;

define constant $key-leftopentriangle = 2764;

define constant $key-leftpointer = 2794;

define constant $key-leftradical = 2209;

define constant $key-leftshoe = 3034;

define constant $key-leftsinglequotemark = 2768;

define constant $key-leftt = 2548;

define constant $key-lefttack = 3036;

define constant $key-less = 60;

define constant $key-lessthanequal = 2236;

define constant $key-lf = 2533;

define constant $key-logicaland = 2270;

define constant $key-logicalor = 2271;

define constant $key-lowleftcorner = 2541;

define constant $key-lowrightcorner = 2538;

define constant $key-macron = 175;

define constant $key-malesymbol = 2807;

define constant $key-maltesecross = 2800;

define constant $key-marker = 2751;

define constant $key-masculine = 186;

define constant $key-minus = 45;

define constant $key-minutes = 2774;

define constant $key-mu = 181;

define constant $key-multiply = 215;

define constant $key-musicalflat = 2806;

define constant $key-musicalsharp = 2805;

define constant $key-nabla = 2245;

define constant $key-ninesubscript = 16785545;

define constant $key-ninesuperior = 16785529;

define constant $key-nl = 2536;

define constant $key-nobreakspace = 160;

define constant $key-notapproxeq = 16785991;

define constant $key-notelementof = 16785929;

define constant $key-notequal = 2237;

define constant $key-notidentical = 16786018;

define constant $key-notsign = 172;

define constant $key-numbersign = 35;

define constant $key-numerosign = 1712;

define constant $key-ogonek = 434;

define constant $key-oneeighth = 2755;

define constant $key-onefifth = 2738;

define constant $key-onehalf = 189;

define constant $key-onequarter = 188;

define constant $key-onesixth = 2742;

define constant $key-onesubscript = 16785537;

define constant $key-onesuperior = 185;

define constant $key-onethird = 2736;

define constant $key-openrectbullet = 2786;

define constant $key-openstar = 2789;

define constant $key-opentribulletdown = 2788;

define constant $key-opentribulletup = 2787;

define constant $key-ordfeminine = 170;

define constant $key-overbar = 3008;

define constant $key-overline = 1150;

define constant $key-paragraph = 182;

define constant $key-parenleft = 40;

define constant $key-parenright = 41;

define constant $key-partdifferential = 16785922;

define constant $key-partialderivative = 2287;

define constant $key-percent = 37;

define constant $key-period = 46;

define constant $key-periodcentered = 183;

define constant $key-permille = 2773;

define constant $key-phonographcopyright = 2811;

define constant $key-plus = 43;

define constant $key-plusminus = 177;

define constant $key-prescription = 2772;

define constant $key-prolongedsound = 1200;

define constant $key-punctspace = 2726;

define constant $key-quad = 3020;

define constant $key-question = 63;

define constant $key-questiondown = 191;

define constant $key-quotedbl = 34;

define constant $key-quoteleft = 96;

define constant $key-quoteright = 39;

define constant $key-radical = 2262;

define constant $key-registered = 174;

define constant $key-rightanglebracket = 2750;

define constant $key-rightarrow = 2301;

define constant $key-rightcaret = 2982;

define constant $key-rightdoublequotemark = 2771;

define constant $key-rightmiddlecurlybrace = 2224;

define constant $key-rightmiddlesummation = 2231;

define constant $key-rightopentriangle = 2765;

define constant $key-rightpointer = 2795;

define constant $key-rightshoe = 3032;

define constant $key-rightsinglequotemark = 2769;

define constant $key-rightt = 2549;

define constant $key-righttack = 3068;

define constant $key-script-switch = 65406;

define constant $key-seconds = 2775;

define constant $key-section = 167;

define constant $key-semicolon = 59;

define constant $key-semivoicedsound = 1247;

define constant $key-seveneighths = 2758;

define constant $key-sevensubscript = 16785543;

define constant $key-sevensuperior = 16785527;

define constant $key-signaturemark = 2762;

define constant $key-signifblank = 2732;

define constant $key-similarequal = 2249;

define constant $key-singlelowquotemark = 2813;

define constant $key-sixsubscript = 16785542;

define constant $key-sixsuperior = 16785526;

define constant $key-slash = 47;

define constant $key-soliddiamond = 2528;

define constant $key-space = 32;

define constant $key-squareroot = 16785946;

define constant $key-ssharp = 223;

define constant $key-sterling = 163;

define constant $key-stricteq = 16786019;

define constant $key-telephone = 2809;

define constant $key-telephonerecorder = 2810;

define constant $key-therefore = 2240;

define constant $key-thinspace = 2727;

define constant $key-threeeighths = 2756;

define constant $key-threefifths = 2740;

define constant $key-threequarters = 190;

define constant $key-threesubscript = 16785539;

define constant $key-threesuperior = 179;

define constant $key-tintegral = 16785965;

define constant $key-topintegral = 2212;

define constant $key-topleftparens = 2219;

define constant $key-topleftradical = 2210;

define constant $key-topleftsqbracket = 2215;

define constant $key-topleftsummation = 2225;

define constant $key-toprightparens = 2221;

define constant $key-toprightsqbracket = 2217;

define constant $key-toprightsummation = 2229;

define constant $key-topt = 2551;

define constant $key-topvertsummationconnector = 2227;

define constant $key-trademark = 2761;

define constant $key-trademarkincircle = 2763;

define constant $key-twofifths = 2739;

define constant $key-twosubscript = 16785538;

define constant $key-twosuperior = 178;

define constant $key-twothirds = 2737;

define constant $key-underbar = 3014;

define constant $key-underscore = 95;

define constant $key-union = 2269;

define constant $key-uparrow = 2300;

define constant $key-upcaret = 2985;

define constant $key-upleftcorner = 2540;

define constant $key-uprightcorner = 2539;

define constant $key-upshoe = 3011;

define constant $key-upstile = 3027;

define constant $key-uptack = 3022;

define constant $key-variation = 2241;

define constant $key-vertbar = 2552;

define constant $key-vertconnector = 2214;

define constant $key-voicedsound = 1246;

define constant $key-vt = 2537;

define constant $key-yen = 165;

define constant $key-zerosubscript = 16785536;

define constant $key-zerosuperior = 16785520;

define open C-subtype <GdkKeymap> (<GObject>)
end C-subtype;

define C-pointer-type <GdkKeymap*> => <GdkKeymap>;

define C-function gdk-keymap-get-default
  result res :: <GdkKeymap>;
  c-name: "gdk_keymap_get_default";
end;

define C-function gdk-keymap-get-for-display
  input parameter display_ :: <GdkDisplay>;
  result res :: <GdkKeymap>;
  c-name: "gdk_keymap_get_for_display";
end;

define C-function gdk-keymap-add-virtual-modifiers
  input parameter self :: <GdkKeymap>;
  output parameter state_ :: <GdkModifierType*>;
  c-name: "gdk_keymap_add_virtual_modifiers";
end;

define C-function gdk-keymap-get-caps-lock-state
  input parameter self :: <GdkKeymap>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_get_caps_lock_state";
end;

define C-function gdk-keymap-get-direction
  input parameter self :: <GdkKeymap>;
  result res :: <PangoDirection>;
  c-name: "gdk_keymap_get_direction";
end;

define C-function gdk-keymap-get-entries-for-keycode
  input parameter self :: <GdkKeymap>;
  input parameter hardware_keycode_ :: <C-unsigned-int>;
  output parameter keys_ :: <C-unsigned-char*> /* Not supported */;
  output parameter keyvals_ :: <C-unsigned-int*>;
  output parameter n_entries_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_get_entries_for_keycode";
end;

define C-function gdk-keymap-get-entries-for-keyval
  input parameter self :: <GdkKeymap>;
  input parameter keyval_ :: <C-unsigned-int>;
  output parameter keys_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_keys_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_get_entries_for_keyval";
end;

define C-function gdk-keymap-get-modifier-mask
  input parameter self :: <GdkKeymap>;
  input parameter intent_ :: <GdkModifierIntent>;
  result res :: <GdkModifierType>;
  c-name: "gdk_keymap_get_modifier_mask";
end;

define C-function gdk-keymap-get-modifier-state
  input parameter self :: <GdkKeymap>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keymap_get_modifier_state";
end;

define C-function gdk-keymap-get-num-lock-state
  input parameter self :: <GdkKeymap>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_get_num_lock_state";
end;

define C-function gdk-keymap-have-bidi-layouts
  input parameter self :: <GdkKeymap>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_have_bidi_layouts";
end;

define C-function gdk-keymap-lookup-key
  input parameter self :: <GdkKeymap>;
  input parameter key_ :: <GdkKeymapKey>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keymap_lookup_key";
end;

define C-function gdk-keymap-map-virtual-modifiers
  input parameter self :: <GdkKeymap>;
  output parameter state_ :: <GdkModifierType*>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_map_virtual_modifiers";
end;

define C-function gdk-keymap-translate-keyboard-state
  input parameter self :: <GdkKeymap>;
  input parameter hardware_keycode_ :: <C-unsigned-int>;
  input parameter state_ :: <GdkModifierType>;
  input parameter group_ :: <C-signed-int>;
  output parameter keyval_ :: <C-unsigned-int*>;
  output parameter effective_group_ :: <C-signed-int*>;
  output parameter level_ :: <C-signed-int*>;
  output parameter consumed_modifiers_ :: <GdkModifierType*>;
  result res :: <C-boolean>;
  c-name: "gdk_keymap_translate_keyboard_state";
end;

define C-struct <_GdkKeymapKey>
  slot gdk-keymap-key-keycode :: <C-unsigned-int>;
  slot gdk-keymap-key-group :: <C-signed-int>;
  slot gdk-keymap-key-level :: <C-signed-int>;
  pointer-type-name: <GdkKeymapKey>;
end C-struct;

define constant $max-timecoord-axes = 128;

define constant $gdk-modifier-intent-primary-accelerator = 0;
define constant $gdk-modifier-intent-context-menu = 1;
define constant $gdk-modifier-intent-extend-selection = 2;
define constant $gdk-modifier-intent-modify-selection = 3;
define constant $gdk-modifier-intent-no-text-input = 4;
define constant $gdk-modifier-intent-shift-group = 5;
define constant <GdkModifierIntent> = <C-int>;
define C-pointer-type <GdkModifierIntent*> => <GdkModifierIntent>;

define constant $gdk-shift-mask = 1;
define constant $gdk-lock-mask = 2;
define constant $gdk-control-mask = 4;
define constant $gdk-mod1-mask = 8;
define constant $gdk-mod2-mask = 16;
define constant $gdk-mod3-mask = 32;
define constant $gdk-mod4-mask = 64;
define constant $gdk-mod5-mask = 128;
define constant $gdk-button1-mask = 256;
define constant $gdk-button2-mask = 512;
define constant $gdk-button3-mask = 1024;
define constant $gdk-button4-mask = 2048;
define constant $gdk-button5-mask = 4096;
define constant $gdk-modifier-reserved-13-mask = 8192;
define constant $gdk-modifier-reserved-14-mask = 16384;
define constant $gdk-modifier-reserved-15-mask = 32768;
define constant $gdk-modifier-reserved-16-mask = 65536;
define constant $gdk-modifier-reserved-17-mask = 131072;
define constant $gdk-modifier-reserved-18-mask = 262144;
define constant $gdk-modifier-reserved-19-mask = 524288;
define constant $gdk-modifier-reserved-20-mask = 1048576;
define constant $gdk-modifier-reserved-21-mask = 2097152;
define constant $gdk-modifier-reserved-22-mask = 4194304;
define constant $gdk-modifier-reserved-23-mask = 8388608;
define constant $gdk-modifier-reserved-24-mask = 16777216;
define constant $gdk-modifier-reserved-25-mask = 33554432;
define constant $gdk-super-mask = 67108864;
define constant $gdk-hyper-mask = 134217728;
define constant $gdk-meta-mask = 268435456;
define constant $gdk-modifier-reserved-29-mask = 536870912;
define constant $gdk-release-mask = 1073741824;
define constant $gdk-modifier-mask = 1543512063;
define constant <GdkModifierType> = <C-int>;
define C-pointer-type <GdkModifierType*> => <GdkModifierType>;

define constant $gdk-notify-ancestor = 0;
define constant $gdk-notify-virtual = 1;
define constant $gdk-notify-inferior = 2;
define constant $gdk-notify-nonlinear = 3;
define constant $gdk-notify-nonlinear-virtual = 4;
define constant $gdk-notify-unknown = 5;
define constant <GdkNotifyType> = <C-int>;
define C-pointer-type <GdkNotifyType*> => <GdkNotifyType>;

define constant $gdk-owner-change-new-owner = 0;
define constant $gdk-owner-change-destroy = 1;
define constant $gdk-owner-change-close = 2;
define constant <GdkOwnerChange> = <C-int>;
define C-pointer-type <GdkOwnerChange*> => <GdkOwnerChange>;

define constant $parent-relative = 1;

define constant $priority-redraw = 20;

define C-struct <_GdkPoint>
  slot gdk-point-x :: <C-signed-int>;
  slot gdk-point-y :: <C-signed-int>;
  pointer-type-name: <GdkPoint>;
end C-struct;

define constant $gdk-prop-mode-replace = 0;
define constant $gdk-prop-mode-prepend = 1;
define constant $gdk-prop-mode-append = 2;
define constant <GdkPropMode> = <C-int>;
define C-pointer-type <GdkPropMode*> => <GdkPropMode>;

define constant $gdk-property-new-value = 0;
define constant $gdk-property-delete = 1;
define constant <GdkPropertyState> = <C-int>;
define C-pointer-type <GdkPropertyState*> => <GdkPropertyState>;

define C-struct <_GdkRGBA>
  slot gdk-rgba-red :: <C-double>;
  slot gdk-rgba-green :: <C-double>;
  slot gdk-rgba-blue :: <C-double>;
  slot gdk-rgba-alpha :: <C-double>;
  pointer-type-name: <GdkRGBA>;
end C-struct;

define C-function gdk-rgba-copy
  input parameter self :: <GdkRGBA>;
  result res :: <GdkRGBA>;
  c-name: "gdk_rgba_copy";
end;

define C-function gdk-rgba-equal
  input parameter self :: <GdkRGBA>;
  input parameter p2_ :: <GdkRGBA>;
  result res :: <C-boolean>;
  c-name: "gdk_rgba_equal";
end;

define C-function gdk-rgba-free
  input parameter self :: <GdkRGBA>;
  c-name: "gdk_rgba_free";
end;

define C-function gdk-rgba-hash
  input parameter self :: <GdkRGBA>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_rgba_hash";
end;

define C-function gdk-rgba-parse
  input parameter self :: <GdkRGBA>;
  input parameter spec_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "gdk_rgba_parse";
end;

define C-function gdk-rgba-to-string
  input parameter self :: <GdkRGBA>;
  result res :: <C-string>;
  c-name: "gdk_rgba_to_string";
end;

define open C-subtype <GdkScreen> (<GObject>)
end C-subtype;

define C-pointer-type <GdkScreen*> => <GdkScreen>;

define C-function gdk-screen-get-default
  result res :: <GdkScreen>;
  c-name: "gdk_screen_get_default";
end;

define C-function gdk-screen-height
  result res :: <C-signed-int>;
  c-name: "gdk_screen_height";
end;

define C-function gdk-screen-height-mm
  result res :: <C-signed-int>;
  c-name: "gdk_screen_height_mm";
end;

define C-function gdk-screen-width
  result res :: <C-signed-int>;
  c-name: "gdk_screen_width";
end;

define C-function gdk-screen-width-mm
  result res :: <C-signed-int>;
  c-name: "gdk_screen_width_mm";
end;

define C-function gdk-screen-get-active-window
  input parameter self :: <GdkScreen>;
  result res :: <GdkWindow>;
  c-name: "gdk_screen_get_active_window";
end;

define C-function gdk-screen-get-display
  input parameter self :: <GdkScreen>;
  result res :: <GdkDisplay>;
  c-name: "gdk_screen_get_display";
end;

define C-function gdk-screen-get-font-options
  input parameter self :: <GdkScreen>;
  result res :: <cairoFontOptions>;
  c-name: "gdk_screen_get_font_options";
end;

define C-function gdk-screen-get-height
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_height";
end;

define C-function gdk-screen-get-height-mm
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_height_mm";
end;

define C-function gdk-screen-get-monitor-at-point
  input parameter self :: <GdkScreen>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_monitor_at_point";
end;

define C-function gdk-screen-get-monitor-at-window
  input parameter self :: <GdkScreen>;
  input parameter window_ :: <GdkWindow>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_monitor_at_window";
end;

define C-function gdk-screen-get-monitor-geometry
  input parameter self :: <GdkScreen>;
  input parameter monitor_num_ :: <C-signed-int>;
  output parameter dest_ :: <cairoRectangleInt>;
  c-name: "gdk_screen_get_monitor_geometry";
end;

define C-function gdk-screen-get-monitor-height-mm
  input parameter self :: <GdkScreen>;
  input parameter monitor_num_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_monitor_height_mm";
end;

define C-function gdk-screen-get-monitor-plug-name
  input parameter self :: <GdkScreen>;
  input parameter monitor_num_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "gdk_screen_get_monitor_plug_name";
end;

define C-function gdk-screen-get-monitor-width-mm
  input parameter self :: <GdkScreen>;
  input parameter monitor_num_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_monitor_width_mm";
end;

define C-function gdk-screen-get-monitor-workarea
  input parameter self :: <GdkScreen>;
  input parameter monitor_num_ :: <C-signed-int>;
  output parameter dest_ :: <cairoRectangleInt>;
  c-name: "gdk_screen_get_monitor_workarea";
end;

define C-function gdk-screen-get-n-monitors
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_n_monitors";
end;

define C-function gdk-screen-get-number
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_number";
end;

define C-function gdk-screen-get-primary-monitor
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_primary_monitor";
end;

define C-function gdk-screen-get-resolution
  input parameter self :: <GdkScreen>;
  result res :: <C-double>;
  c-name: "gdk_screen_get_resolution";
end;

define C-function gdk-screen-get-rgba-visual
  input parameter self :: <GdkScreen>;
  result res :: <GdkVisual>;
  c-name: "gdk_screen_get_rgba_visual";
end;

define C-function gdk-screen-get-root-window
  input parameter self :: <GdkScreen>;
  result res :: <GdkWindow>;
  c-name: "gdk_screen_get_root_window";
end;

define C-function gdk-screen-get-setting
  input parameter self :: <GdkScreen>;
  input parameter name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gdk_screen_get_setting";
end;

define C-function gdk-screen-get-system-visual
  input parameter self :: <GdkScreen>;
  result res :: <GdkVisual>;
  c-name: "gdk_screen_get_system_visual";
end;

define C-function gdk-screen-get-toplevel-windows
  input parameter self :: <GdkScreen>;
  result res :: <GList>;
  c-name: "gdk_screen_get_toplevel_windows";
end;

define C-function gdk-screen-get-width
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_width";
end;

define C-function gdk-screen-get-width-mm
  input parameter self :: <GdkScreen>;
  result res :: <C-signed-int>;
  c-name: "gdk_screen_get_width_mm";
end;

define C-function gdk-screen-get-window-stack
  input parameter self :: <GdkScreen>;
  result res :: <GList>;
  c-name: "gdk_screen_get_window_stack";
end;

define C-function gdk-screen-is-composited
  input parameter self :: <GdkScreen>;
  result res :: <C-boolean>;
  c-name: "gdk_screen_is_composited";
end;

define C-function gdk-screen-list-visuals
  input parameter self :: <GdkScreen>;
  result res :: <GList>;
  c-name: "gdk_screen_list_visuals";
end;

define C-function gdk-screen-make-display-name
  input parameter self :: <GdkScreen>;
  result res :: <C-string>;
  c-name: "gdk_screen_make_display_name";
end;

define C-function gdk-screen-set-font-options
  input parameter self :: <GdkScreen>;
  input parameter options_ :: <cairoFontOptions>;
  c-name: "gdk_screen_set_font_options";
end;

define C-function gdk-screen-set-resolution
  input parameter self :: <GdkScreen>;
  input parameter dpi_ :: <C-double>;
  c-name: "gdk_screen_set_resolution";
end;

define constant $gdk-scroll-up = 0;
define constant $gdk-scroll-down = 1;
define constant $gdk-scroll-left = 2;
define constant $gdk-scroll-right = 3;
define constant $gdk-scroll-smooth = 4;
define constant <GdkScrollDirection> = <C-int>;
define C-pointer-type <GdkScrollDirection*> => <GdkScrollDirection>;

define constant $gdk-setting-action-new = 0;
define constant $gdk-setting-action-changed = 1;
define constant $gdk-setting-action-deleted = 2;
define constant <GdkSettingAction> = <C-int>;
define C-pointer-type <GdkSettingAction*> => <GdkSettingAction>;

define constant $gdk-ok = 0;
define constant $gdk-error = -1;
define constant $gdk-error-param = -2;
define constant $gdk-error-file = -3;
define constant $gdk-error-mem = -4;
define constant <GdkStatus> = <C-int>;
define C-pointer-type <GdkStatus*> => <GdkStatus>;

define C-struct <_GdkTimeCoord>
  slot gdk-time-coord-time :: <C-unsigned-int>;
  slot gdk-time-coord-axes :: <C-double*>;
  pointer-type-name: <GdkTimeCoord>;
end C-struct;

define constant $gdk-visibility-unobscured = 0;
define constant $gdk-visibility-partial = 1;
define constant $gdk-visibility-fully-obscured = 2;
define constant <GdkVisibilityState> = <C-int>;
define C-pointer-type <GdkVisibilityState*> => <GdkVisibilityState>;

define open C-subtype <GdkVisual> (<GObject>)
end C-subtype;

define C-pointer-type <GdkVisual*> => <GdkVisual>;

define C-function gdk-visual-get-best
  result res :: <GdkVisual>;
  c-name: "gdk_visual_get_best";
end;

define C-function gdk-visual-get-best-depth
  result res :: <C-signed-int>;
  c-name: "gdk_visual_get_best_depth";
end;

define C-function gdk-visual-get-best-type
  result res :: <GdkVisualType>;
  c-name: "gdk_visual_get_best_type";
end;

define C-function gdk-visual-get-best-with-both
  input parameter depth_ :: <C-signed-int>;
  input parameter visual_type_ :: <GdkVisualType>;
  result res :: <GdkVisual>;
  c-name: "gdk_visual_get_best_with_both";
end;

define C-function gdk-visual-get-best-with-depth
  input parameter depth_ :: <C-signed-int>;
  result res :: <GdkVisual>;
  c-name: "gdk_visual_get_best_with_depth";
end;

define C-function gdk-visual-get-best-with-type
  input parameter visual_type_ :: <GdkVisualType>;
  result res :: <GdkVisual>;
  c-name: "gdk_visual_get_best_with_type";
end;

define C-function gdk-visual-get-system
  result res :: <GdkVisual>;
  c-name: "gdk_visual_get_system";
end;

define C-function gdk-visual-get-bits-per-rgb
  input parameter self :: <GdkVisual>;
  result res :: <C-signed-int>;
  c-name: "gdk_visual_get_bits_per_rgb";
end;

define C-function gdk-visual-get-blue-pixel-details
  input parameter self :: <GdkVisual>;
  output parameter mask_ :: <C-unsigned-int*>;
  output parameter shift_ :: <C-signed-int*>;
  output parameter precision_ :: <C-signed-int*>;
  c-name: "gdk_visual_get_blue_pixel_details";
end;

define C-function gdk-visual-get-byte-order
  input parameter self :: <GdkVisual>;
  result res :: <GdkByteOrder>;
  c-name: "gdk_visual_get_byte_order";
end;

define C-function gdk-visual-get-colormap-size
  input parameter self :: <GdkVisual>;
  result res :: <C-signed-int>;
  c-name: "gdk_visual_get_colormap_size";
end;

define C-function gdk-visual-get-depth
  input parameter self :: <GdkVisual>;
  result res :: <C-signed-int>;
  c-name: "gdk_visual_get_depth";
end;

define C-function gdk-visual-get-green-pixel-details
  input parameter self :: <GdkVisual>;
  output parameter mask_ :: <C-unsigned-int*>;
  output parameter shift_ :: <C-signed-int*>;
  output parameter precision_ :: <C-signed-int*>;
  c-name: "gdk_visual_get_green_pixel_details";
end;

define C-function gdk-visual-get-red-pixel-details
  input parameter self :: <GdkVisual>;
  output parameter mask_ :: <C-unsigned-int*>;
  output parameter shift_ :: <C-signed-int*>;
  output parameter precision_ :: <C-signed-int*>;
  c-name: "gdk_visual_get_red_pixel_details";
end;

define C-function gdk-visual-get-screen
  input parameter self :: <GdkVisual>;
  result res :: <GdkScreen>;
  c-name: "gdk_visual_get_screen";
end;

define C-function gdk-visual-get-visual-type
  input parameter self :: <GdkVisual>;
  result res :: <GdkVisualType>;
  c-name: "gdk_visual_get_visual_type";
end;

define constant $gdk-visual-static-gray = 0;
define constant $gdk-visual-grayscale = 1;
define constant $gdk-visual-static-color = 2;
define constant $gdk-visual-pseudo-color = 3;
define constant $gdk-visual-true-color = 4;
define constant $gdk-visual-direct-color = 5;
define constant <GdkVisualType> = <C-int>;
define C-pointer-type <GdkVisualType*> => <GdkVisualType>;

define constant $gdk-decor-all = 1;
define constant $gdk-decor-border = 2;
define constant $gdk-decor-resizeh = 4;
define constant $gdk-decor-title = 8;
define constant $gdk-decor-menu = 16;
define constant $gdk-decor-minimize = 32;
define constant $gdk-decor-maximize = 64;
define constant <GdkWMDecoration> = <C-int>;
define C-pointer-type <GdkWMDecoration*> => <GdkWMDecoration>;

define constant $gdk-func-all = 1;
define constant $gdk-func-resize = 2;
define constant $gdk-func-move = 4;
define constant $gdk-func-minimize = 8;
define constant $gdk-func-maximize = 16;
define constant $gdk-func-close = 32;
define constant <GdkWMFunction> = <C-int>;
define C-pointer-type <GdkWMFunction*> => <GdkWMFunction>;

define open C-subtype <GdkWindow> (<GObject>)
end C-subtype;

define C-pointer-type <GdkWindow*> => <GdkWindow>;

define C-function gdk-window-new
  input parameter parent_ :: <GdkWindow>;
  input parameter attributes_ :: <GdkWindowAttr>;
  input parameter attributes_mask_ :: <C-signed-int>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_new";
end;

define C-function gdk-window-at-pointer
  output parameter win_x_ :: <C-signed-int*>;
  output parameter win_y_ :: <C-signed-int*>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_at_pointer";
end;

define C-function gdk-window-constrain-size
  input parameter geometry_ :: <GdkGeometry>;
  input parameter flags_ :: <C-unsigned-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  output parameter new_width_ :: <C-signed-int*>;
  output parameter new_height_ :: <C-signed-int*>;
  c-name: "gdk_window_constrain_size";
end;

define C-function gdk-window-process-all-updates
  c-name: "gdk_window_process_all_updates";
end;

define C-function gdk-window-set-debug-updates
  input parameter setting_ :: <C-boolean>;
  c-name: "gdk_window_set_debug_updates";
end;

define C-function gdk-window-beep
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_beep";
end;

define C-function gdk-window-begin-move-drag
  input parameter self :: <GdkWindow>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_window_begin_move_drag";
end;

define C-function gdk-window-begin-move-drag-for-device
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_window_begin_move_drag_for_device";
end;

define C-function gdk-window-begin-paint-rect
  input parameter self :: <GdkWindow>;
  input parameter rectangle_ :: <cairoRectangleInt>;
  c-name: "gdk_window_begin_paint_rect";
end;

define C-function gdk-window-begin-paint-region
  input parameter self :: <GdkWindow>;
  input parameter region_ :: <cairoRegion>;
  c-name: "gdk_window_begin_paint_region";
end;

define C-function gdk-window-begin-resize-drag
  input parameter self :: <GdkWindow>;
  input parameter edge_ :: <GdkWindowEdge>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_window_begin_resize_drag";
end;

define C-function gdk-window-begin-resize-drag-for-device
  input parameter self :: <GdkWindow>;
  input parameter edge_ :: <GdkWindowEdge>;
  input parameter device_ :: <GdkDevice>;
  input parameter button_ :: <C-signed-int>;
  input parameter root_x_ :: <C-signed-int>;
  input parameter root_y_ :: <C-signed-int>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_window_begin_resize_drag_for_device";
end;

define C-function gdk-window-configure-finished
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_configure_finished";
end;

define C-function gdk-window-coords-from-parent
  input parameter self :: <GdkWindow>;
  input parameter parent_x_ :: <C-double>;
  input parameter parent_y_ :: <C-double>;
  output parameter x_ :: <C-double*>;
  output parameter y_ :: <C-double*>;
  c-name: "gdk_window_coords_from_parent";
end;

define C-function gdk-window-coords-to-parent
  input parameter self :: <GdkWindow>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  output parameter parent_x_ :: <C-double*>;
  output parameter parent_y_ :: <C-double*>;
  c-name: "gdk_window_coords_to_parent";
end;

define C-function gdk-window-create-similar-surface
  input parameter self :: <GdkWindow>;
  input parameter content_ :: <cairoContent>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <cairoSurface>;
  c-name: "gdk_window_create_similar_surface";
end;

define C-function gdk-window-deiconify
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_deiconify";
end;

define C-function gdk-window-destroy
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_destroy";
end;

define C-function gdk-window-destroy-notify
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_destroy_notify";
end;

define C-function gdk-window-enable-synchronized-configure
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_enable_synchronized_configure";
end;

define C-function gdk-window-end-paint
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_end_paint";
end;

define C-function gdk-window-ensure-native
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_ensure_native";
end;

define C-function gdk-window-flush
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_flush";
end;

define C-function gdk-window-focus
  input parameter self :: <GdkWindow>;
  input parameter timestamp_ :: <C-unsigned-int>;
  c-name: "gdk_window_focus";
end;

define C-function gdk-window-freeze-toplevel-updates-libgtk-only
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_freeze_toplevel_updates_libgtk_only";
end;

define C-function gdk-window-freeze-updates
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_freeze_updates";
end;

define C-function gdk-window-fullscreen
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_fullscreen";
end;

define C-function gdk-window-geometry-changed
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_geometry_changed";
end;

define C-function gdk-window-get-accept-focus
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_accept_focus";
end;

define C-function gdk-window-get-background-pattern
  input parameter self :: <GdkWindow>;
  result res :: <cairoPattern>;
  c-name: "gdk_window_get_background_pattern";
end;

define C-function gdk-window-get-children
  input parameter self :: <GdkWindow>;
  result res :: <GList>;
  c-name: "gdk_window_get_children";
end;

define C-function gdk-window-get-clip-region
  input parameter self :: <GdkWindow>;
  result res :: <cairoRegion>;
  c-name: "gdk_window_get_clip_region";
end;

define C-function gdk-window-get-composited
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_composited";
end;

define C-function gdk-window-get-cursor
  input parameter self :: <GdkWindow>;
  result res :: <GdkCursor>;
  c-name: "gdk_window_get_cursor";
end;

define C-function gdk-window-get-decorations
  input parameter self :: <GdkWindow>;
  output parameter decorations_ :: <GdkWMDecoration*>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_decorations";
end;

define C-function gdk-window-get-device-cursor
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  result res :: <GdkCursor>;
  c-name: "gdk_window_get_device_cursor";
end;

define C-function gdk-window-get-device-events
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  result res :: <GdkEventMask>;
  c-name: "gdk_window_get_device_events";
end;

define C-function gdk-window-get-device-position
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  output parameter mask_ :: <GdkModifierType*>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_device_position";
end;

define C-function gdk-window-get-display
  input parameter self :: <GdkWindow>;
  result res :: <GdkDisplay>;
  c-name: "gdk_window_get_display";
end;

define C-function gdk-window-get-drag-protocol
  input parameter self :: <GdkWindow>;
  output parameter target_ :: <GdkWindow*>;
  result res :: <GdkDragProtocol>;
  c-name: "gdk_window_get_drag_protocol";
end;

define C-function gdk-window-get-effective-parent
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_effective_parent";
end;

define C-function gdk-window-get-effective-toplevel
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_effective_toplevel";
end;

define C-function gdk-window-get-events
  input parameter self :: <GdkWindow>;
  result res :: <GdkEventMask>;
  c-name: "gdk_window_get_events";
end;

define C-function gdk-window-get-focus-on-map
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_focus_on_map";
end;

define C-function gdk-window-get-frame-extents
  input parameter self :: <GdkWindow>;
  output parameter rect_ :: <cairoRectangleInt>;
  c-name: "gdk_window_get_frame_extents";
end;

define C-function gdk-window-get-geometry
  input parameter self :: <GdkWindow>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  c-name: "gdk_window_get_geometry";
end;

define C-function gdk-window-get-group
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_group";
end;

define C-function gdk-window-get-height
  input parameter self :: <GdkWindow>;
  result res :: <C-signed-int>;
  c-name: "gdk_window_get_height";
end;

define C-function gdk-window-get-modal-hint
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_modal_hint";
end;

define C-function gdk-window-get-origin
  input parameter self :: <GdkWindow>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  result res :: <C-signed-int>;
  c-name: "gdk_window_get_origin";
end;

define C-function gdk-window-get-parent
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_parent";
end;

define C-function gdk-window-get-pointer
  input parameter self :: <GdkWindow>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  output parameter mask_ :: <GdkModifierType*>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_pointer";
end;

define C-function gdk-window-get-position
  input parameter self :: <GdkWindow>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gdk_window_get_position";
end;

define C-function gdk-window-get-root-coords
  input parameter self :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  output parameter root_x_ :: <C-signed-int*>;
  output parameter root_y_ :: <C-signed-int*>;
  c-name: "gdk_window_get_root_coords";
end;

define C-function gdk-window-get-root-origin
  input parameter self :: <GdkWindow>;
  output parameter x_ :: <C-signed-int*>;
  output parameter y_ :: <C-signed-int*>;
  c-name: "gdk_window_get_root_origin";
end;

define C-function gdk-window-get-screen
  input parameter self :: <GdkWindow>;
  result res :: <GdkScreen>;
  c-name: "gdk_window_get_screen";
end;

define C-function gdk-window-get-source-events
  input parameter self :: <GdkWindow>;
  input parameter source_ :: <GdkInputSource>;
  result res :: <GdkEventMask>;
  c-name: "gdk_window_get_source_events";
end;

define C-function gdk-window-get-state
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindowState>;
  c-name: "gdk_window_get_state";
end;

define C-function gdk-window-get-support-multidevice
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_get_support_multidevice";
end;

define C-function gdk-window-get-toplevel
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_window_get_toplevel";
end;

define C-function gdk-window-get-type-hint
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindowTypeHint>;
  c-name: "gdk_window_get_type_hint";
end;

define C-function gdk-window-get-update-area
  input parameter self :: <GdkWindow>;
  result res :: <cairoRegion>;
  c-name: "gdk_window_get_update_area";
end;

define C-function gdk-window-get-user-data
  input parameter self :: <GdkWindow>;
  output parameter data_ :: <C-void**>;
  c-name: "gdk_window_get_user_data";
end;

define C-function gdk-window-get-visible-region
  input parameter self :: <GdkWindow>;
  result res :: <cairoRegion>;
  c-name: "gdk_window_get_visible_region";
end;

define C-function gdk-window-get-visual
  input parameter self :: <GdkWindow>;
  result res :: <GdkVisual>;
  c-name: "gdk_window_get_visual";
end;

define C-function gdk-window-get-width
  input parameter self :: <GdkWindow>;
  result res :: <C-signed-int>;
  c-name: "gdk_window_get_width";
end;

define C-function gdk-window-get-window-type
  input parameter self :: <GdkWindow>;
  result res :: <GdkWindowType>;
  c-name: "gdk_window_get_window_type";
end;

define C-function gdk-window-has-native
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_has_native";
end;

define C-function gdk-window-hide
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_hide";
end;

define C-function gdk-window-iconify
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_iconify";
end;

define C-function gdk-window-input-shape-combine-region
  input parameter self :: <GdkWindow>;
  input parameter shape_region_ :: <cairoRegion>;
  input parameter offset_x_ :: <C-signed-int>;
  input parameter offset_y_ :: <C-signed-int>;
  c-name: "gdk_window_input_shape_combine_region";
end;

define C-function gdk-window-invalidate-maybe-recurse
  input parameter self :: <GdkWindow>;
  input parameter region_ :: <cairoRegion>;
  input parameter child_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gdk_window_invalidate_maybe_recurse";
end;

define C-function gdk-window-invalidate-rect
  input parameter self :: <GdkWindow>;
  input parameter rect_ :: <cairoRectangleInt>;
  input parameter invalidate_children_ :: <C-boolean>;
  c-name: "gdk_window_invalidate_rect";
end;

define C-function gdk-window-invalidate-region
  input parameter self :: <GdkWindow>;
  input parameter region_ :: <cairoRegion>;
  input parameter invalidate_children_ :: <C-boolean>;
  c-name: "gdk_window_invalidate_region";
end;

define C-function gdk-window-is-destroyed
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_is_destroyed";
end;

define C-function gdk-window-is-input-only
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_is_input_only";
end;

define C-function gdk-window-is-shaped
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_is_shaped";
end;

define C-function gdk-window-is-viewable
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_is_viewable";
end;

define C-function gdk-window-is-visible
  input parameter self :: <GdkWindow>;
  result res :: <C-boolean>;
  c-name: "gdk_window_is_visible";
end;

define C-function gdk-window-lower
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_lower";
end;

define C-function gdk-window-maximize
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_maximize";
end;

define C-function gdk-window-merge-child-input-shapes
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_merge_child_input_shapes";
end;

define C-function gdk-window-merge-child-shapes
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_merge_child_shapes";
end;

define C-function gdk-window-move
  input parameter self :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gdk_window_move";
end;

define C-function gdk-window-move-region
  input parameter self :: <GdkWindow>;
  input parameter region_ :: <cairoRegion>;
  input parameter dx_ :: <C-signed-int>;
  input parameter dy_ :: <C-signed-int>;
  c-name: "gdk_window_move_region";
end;

define C-function gdk-window-move-resize
  input parameter self :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gdk_window_move_resize";
end;

define C-function gdk-window-peek-children
  input parameter self :: <GdkWindow>;
  result res :: <GList>;
  c-name: "gdk_window_peek_children";
end;

define C-function gdk-window-process-updates
  input parameter self :: <GdkWindow>;
  input parameter update_children_ :: <C-boolean>;
  c-name: "gdk_window_process_updates";
end;

define C-function gdk-window-raise
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_raise";
end;

define C-function gdk-window-register-dnd
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_register_dnd";
end;

define C-function gdk-window-reparent
  input parameter self :: <GdkWindow>;
  input parameter new_parent_ :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  c-name: "gdk_window_reparent";
end;

define C-function gdk-window-resize
  input parameter self :: <GdkWindow>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gdk_window_resize";
end;

define C-function gdk-window-restack
  input parameter self :: <GdkWindow>;
  input parameter sibling_ :: <GdkWindow>;
  input parameter above_ :: <C-boolean>;
  c-name: "gdk_window_restack";
end;

define C-function gdk-window-scroll
  input parameter self :: <GdkWindow>;
  input parameter dx_ :: <C-signed-int>;
  input parameter dy_ :: <C-signed-int>;
  c-name: "gdk_window_scroll";
end;

define C-function gdk-window-set-accept-focus
  input parameter self :: <GdkWindow>;
  input parameter accept_focus_ :: <C-boolean>;
  c-name: "gdk_window_set_accept_focus";
end;

define C-function gdk-window-set-background
  input parameter self :: <GdkWindow>;
  input parameter color_ :: <GdkColor>;
  c-name: "gdk_window_set_background";
end;

define C-function gdk-window-set-background-pattern
  input parameter self :: <GdkWindow>;
  input parameter pattern_ :: <cairoPattern>;
  c-name: "gdk_window_set_background_pattern";
end;

define C-function gdk-window-set-background-rgba
  input parameter self :: <GdkWindow>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gdk_window_set_background_rgba";
end;

define C-function gdk-window-set-child-input-shapes
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_set_child_input_shapes";
end;

define C-function gdk-window-set-child-shapes
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_set_child_shapes";
end;

define C-function gdk-window-set-composited
  input parameter self :: <GdkWindow>;
  input parameter composited_ :: <C-boolean>;
  c-name: "gdk_window_set_composited";
end;

define C-function gdk-window-set-cursor
  input parameter self :: <GdkWindow>;
  input parameter cursor_ :: <GdkCursor>;
  c-name: "gdk_window_set_cursor";
end;

define C-function gdk-window-set-decorations
  input parameter self :: <GdkWindow>;
  input parameter decorations_ :: <GdkWMDecoration>;
  c-name: "gdk_window_set_decorations";
end;

define C-function gdk-window-set-device-cursor
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  input parameter cursor_ :: <GdkCursor>;
  c-name: "gdk_window_set_device_cursor";
end;

define C-function gdk-window-set-device-events
  input parameter self :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  input parameter event_mask_ :: <GdkEventMask>;
  c-name: "gdk_window_set_device_events";
end;

define C-function gdk-window-set-events
  input parameter self :: <GdkWindow>;
  input parameter event_mask_ :: <GdkEventMask>;
  c-name: "gdk_window_set_events";
end;

define C-function gdk-window-set-focus-on-map
  input parameter self :: <GdkWindow>;
  input parameter focus_on_map_ :: <C-boolean>;
  c-name: "gdk_window_set_focus_on_map";
end;

define C-function gdk-window-set-functions
  input parameter self :: <GdkWindow>;
  input parameter functions_ :: <GdkWMFunction>;
  c-name: "gdk_window_set_functions";
end;

define C-function gdk-window-set-geometry-hints
  input parameter self :: <GdkWindow>;
  input parameter geometry_ :: <GdkGeometry>;
  input parameter geom_mask_ :: <GdkWindowHints>;
  c-name: "gdk_window_set_geometry_hints";
end;

define C-function gdk-window-set-group
  input parameter self :: <GdkWindow>;
  input parameter leader_ :: <GdkWindow>;
  c-name: "gdk_window_set_group";
end;

define C-function gdk-window-set-icon-list
  input parameter self :: <GdkWindow>;
  input parameter pixbufs_ :: <GList>;
  c-name: "gdk_window_set_icon_list";
end;

define C-function gdk-window-set-icon-name
  input parameter self :: <GdkWindow>;
  input parameter name_ :: <C-string>;
  c-name: "gdk_window_set_icon_name";
end;

define C-function gdk-window-set-keep-above
  input parameter self :: <GdkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gdk_window_set_keep_above";
end;

define C-function gdk-window-set-keep-below
  input parameter self :: <GdkWindow>;
  input parameter setting_ :: <C-boolean>;
  c-name: "gdk_window_set_keep_below";
end;

define C-function gdk-window-set-modal-hint
  input parameter self :: <GdkWindow>;
  input parameter modal_ :: <C-boolean>;
  c-name: "gdk_window_set_modal_hint";
end;

define C-function gdk-window-set-opacity
  input parameter self :: <GdkWindow>;
  input parameter opacity_ :: <C-double>;
  c-name: "gdk_window_set_opacity";
end;

define C-function gdk-window-set-override-redirect
  input parameter self :: <GdkWindow>;
  input parameter override_redirect_ :: <C-boolean>;
  c-name: "gdk_window_set_override_redirect";
end;

define C-function gdk-window-set-role
  input parameter self :: <GdkWindow>;
  input parameter role_ :: <C-string>;
  c-name: "gdk_window_set_role";
end;

define C-function gdk-window-set-skip-pager-hint
  input parameter self :: <GdkWindow>;
  input parameter skips_pager_ :: <C-boolean>;
  c-name: "gdk_window_set_skip_pager_hint";
end;

define C-function gdk-window-set-skip-taskbar-hint
  input parameter self :: <GdkWindow>;
  input parameter skips_taskbar_ :: <C-boolean>;
  c-name: "gdk_window_set_skip_taskbar_hint";
end;

define C-function gdk-window-set-source-events
  input parameter self :: <GdkWindow>;
  input parameter source_ :: <GdkInputSource>;
  input parameter event_mask_ :: <GdkEventMask>;
  c-name: "gdk_window_set_source_events";
end;

define C-function gdk-window-set-startup-id
  input parameter self :: <GdkWindow>;
  input parameter startup_id_ :: <C-string>;
  c-name: "gdk_window_set_startup_id";
end;

define C-function gdk-window-set-static-gravities
  input parameter self :: <GdkWindow>;
  input parameter use_static_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gdk_window_set_static_gravities";
end;

define C-function gdk-window-set-support-multidevice
  input parameter self :: <GdkWindow>;
  input parameter support_multidevice_ :: <C-boolean>;
  c-name: "gdk_window_set_support_multidevice";
end;

define C-function gdk-window-set-title
  input parameter self :: <GdkWindow>;
  input parameter title_ :: <C-string>;
  c-name: "gdk_window_set_title";
end;

define C-function gdk-window-set-transient-for
  input parameter self :: <GdkWindow>;
  input parameter parent_ :: <GdkWindow>;
  c-name: "gdk_window_set_transient_for";
end;

define C-function gdk-window-set-type-hint
  input parameter self :: <GdkWindow>;
  input parameter hint_ :: <GdkWindowTypeHint>;
  c-name: "gdk_window_set_type_hint";
end;

define C-function gdk-window-set-urgency-hint
  input parameter self :: <GdkWindow>;
  input parameter urgent_ :: <C-boolean>;
  c-name: "gdk_window_set_urgency_hint";
end;

define C-function gdk-window-set-user-data
  input parameter self :: <GdkWindow>;
  input parameter user_data_ :: <GObject>;
  c-name: "gdk_window_set_user_data";
end;

define C-function gdk-window-shape-combine-region
  input parameter self :: <GdkWindow>;
  input parameter shape_region_ :: <cairoRegion>;
  input parameter offset_x_ :: <C-signed-int>;
  input parameter offset_y_ :: <C-signed-int>;
  c-name: "gdk_window_shape_combine_region";
end;

define C-function gdk-window-show
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_show";
end;

define C-function gdk-window-show-unraised
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_show_unraised";
end;

define C-function gdk-window-stick
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_stick";
end;

define C-function gdk-window-thaw-toplevel-updates-libgtk-only
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_thaw_toplevel_updates_libgtk_only";
end;

define C-function gdk-window-thaw-updates
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_thaw_updates";
end;

define C-function gdk-window-unfullscreen
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_unfullscreen";
end;

define C-function gdk-window-unmaximize
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_unmaximize";
end;

define C-function gdk-window-unstick
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_unstick";
end;

define C-function gdk-window-withdraw
  input parameter self :: <GdkWindow>;
  c-name: "gdk_window_withdraw";
end;

define C-struct <_GdkWindowAttr>
  slot gdk-window-attr-title :: <C-string>;
  slot gdk-window-attr-event-mask :: <C-signed-int>;
  slot gdk-window-attr-x :: <C-signed-int>;
  slot gdk-window-attr-y :: <C-signed-int>;
  slot gdk-window-attr-width :: <C-signed-int>;
  slot gdk-window-attr-height :: <C-signed-int>;
  slot gdk-window-attr-wclass :: <GdkWindowWindowClass>;
  slot gdk-window-attr-visual :: <GdkVisual>;
  slot gdk-window-attr-window-type :: <GdkWindowType>;
  slot gdk-window-attr-cursor :: <GdkCursor>;
  slot gdk-window-attr-wmclass-name :: <C-string>;
  slot gdk-window-attr-wmclass-class :: <C-string>;
  slot gdk-window-attr-override-redirect :: <C-boolean>;
  slot gdk-window-attr-type-hint :: <GdkWindowTypeHint>;
  pointer-type-name: <GdkWindowAttr>;
end C-struct;

define constant $gdk-wa-title = 2;
define constant $gdk-wa-x = 4;
define constant $gdk-wa-y = 8;
define constant $gdk-wa-cursor = 16;
define constant $gdk-wa-visual = 32;
define constant $gdk-wa-wmclass = 64;
define constant $gdk-wa-noredir = 128;
define constant $gdk-wa-type-hint = 256;
define constant <GdkWindowAttributesType> = <C-int>;
define C-pointer-type <GdkWindowAttributesType*> => <GdkWindowAttributesType>;

define C-struct <_GdkWindowClass>
  constant slot gdk-window-class-parent-class :: <GObjectClass>;
  constant slot gdk-window-class-pick-embedded-child :: <C-void*>;
  constant slot gdk-window-class-to-embedder :: <C-function-pointer>;
  constant slot gdk-window-class-from-embedder :: <C-function-pointer>;
  constant slot gdk-window-class-create-surface :: <C-function-pointer>;
  constant slot gdk-window-class-_gdk-reserved1 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved2 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved3 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved4 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved5 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved6 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved7 :: <C-void*>;
  constant slot gdk-window-class-_gdk-reserved8 :: <C-void*>;
  pointer-type-name: <GdkWindowClass>;
end C-struct;

define constant $gdk-window-edge-north-west = 0;
define constant $gdk-window-edge-north = 1;
define constant $gdk-window-edge-north-east = 2;
define constant $gdk-window-edge-west = 3;
define constant $gdk-window-edge-east = 4;
define constant $gdk-window-edge-south-west = 5;
define constant $gdk-window-edge-south = 6;
define constant $gdk-window-edge-south-east = 7;
define constant <GdkWindowEdge> = <C-int>;
define C-pointer-type <GdkWindowEdge*> => <GdkWindowEdge>;

define constant $gdk-hint-pos = 1;
define constant $gdk-hint-min-size = 2;
define constant $gdk-hint-max-size = 4;
define constant $gdk-hint-base-size = 8;
define constant $gdk-hint-aspect = 16;
define constant $gdk-hint-resize-inc = 32;
define constant $gdk-hint-win-gravity = 64;
define constant $gdk-hint-user-pos = 128;
define constant $gdk-hint-user-size = 256;
define constant <GdkWindowHints> = <C-int>;
define C-pointer-type <GdkWindowHints*> => <GdkWindowHints>;

define C-struct <_GdkWindowRedirect>
  pointer-type-name: <GdkWindowRedirect>;
end C-struct;

define constant $gdk-window-state-withdrawn = 1;
define constant $gdk-window-state-iconified = 2;
define constant $gdk-window-state-maximized = 4;
define constant $gdk-window-state-sticky = 8;
define constant $gdk-window-state-fullscreen = 16;
define constant $gdk-window-state-above = 32;
define constant $gdk-window-state-below = 64;
define constant $gdk-window-state-focused = 128;
define constant <GdkWindowState> = <C-int>;
define C-pointer-type <GdkWindowState*> => <GdkWindowState>;

define constant $gdk-window-root = 0;
define constant $gdk-window-toplevel = 1;
define constant $gdk-window-child = 2;
define constant $gdk-window-temp = 3;
define constant $gdk-window-foreign = 4;
define constant $gdk-window-offscreen = 5;
define constant <GdkWindowType> = <C-int>;
define C-pointer-type <GdkWindowType*> => <GdkWindowType>;

define constant $gdk-window-type-hint-normal = 0;
define constant $gdk-window-type-hint-dialog = 1;
define constant $gdk-window-type-hint-menu = 2;
define constant $gdk-window-type-hint-toolbar = 3;
define constant $gdk-window-type-hint-splashscreen = 4;
define constant $gdk-window-type-hint-utility = 5;
define constant $gdk-window-type-hint-dock = 6;
define constant $gdk-window-type-hint-desktop = 7;
define constant $gdk-window-type-hint-dropdown-menu = 8;
define constant $gdk-window-type-hint-popup-menu = 9;
define constant $gdk-window-type-hint-tooltip = 10;
define constant $gdk-window-type-hint-notification = 11;
define constant $gdk-window-type-hint-combo = 12;
define constant $gdk-window-type-hint-dnd = 13;
define constant <GdkWindowTypeHint> = <C-int>;
define C-pointer-type <GdkWindowTypeHint*> => <GdkWindowTypeHint>;

define constant $gdk-input-output = 0;
define constant $gdk-input-only = 1;
define constant <GdkWindowWindowClass> = <C-int>;
define C-pointer-type <GdkWindowWindowClass*> => <GdkWindowWindowClass>;

define C-function gdk-add-option-entries-libgtk-only
  input parameter group_ :: <GOptionGroup>;
  c-name: "gdk_add_option_entries_libgtk_only";
end;

define C-function gdk-beep
  c-name: "gdk_beep";
end;

define C-function gdk-cairo-create
  input parameter window_ :: <GdkWindow>;
  result res :: <cairoContext>;
  c-name: "gdk_cairo_create";
end;

define C-function gdk-cairo-get-clip-rectangle
  input parameter cr_ :: <cairoContext>;
  output parameter rect_ :: <cairoRectangleInt>;
  result res :: <C-boolean>;
  c-name: "gdk_cairo_get_clip_rectangle";
end;

define C-function gdk-cairo-rectangle
  input parameter cr_ :: <cairoContext>;
  input parameter rectangle_ :: <cairoRectangleInt>;
  c-name: "gdk_cairo_rectangle";
end;

define C-function gdk-cairo-region
  input parameter cr_ :: <cairoContext>;
  input parameter region_ :: <cairoRegion>;
  c-name: "gdk_cairo_region";
end;

define C-function gdk-cairo-region-create-from-surface
  input parameter surface_ :: <cairoSurface>;
  result res :: <cairoRegion>;
  c-name: "gdk_cairo_region_create_from_surface";
end;

define C-function gdk-cairo-set-source-pixbuf
  input parameter cr_ :: <cairoContext>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  input parameter pixbuf_x_ :: <C-double>;
  input parameter pixbuf_y_ :: <C-double>;
  c-name: "gdk_cairo_set_source_pixbuf";
end;

define C-function gdk-cairo-set-source-rgba
  input parameter cr_ :: <cairoContext>;
  input parameter rgba_ :: <GdkRGBA>;
  c-name: "gdk_cairo_set_source_rgba";
end;

define C-function gdk-cairo-set-source-window
  input parameter cr_ :: <cairoContext>;
  input parameter window_ :: <GdkWindow>;
  input parameter x_ :: <C-double>;
  input parameter y_ :: <C-double>;
  c-name: "gdk_cairo_set_source_window";
end;

define C-function gdk-disable-multidevice
  c-name: "gdk_disable_multidevice";
end;

define C-function gdk-drag-abort
  input parameter context_ :: <GdkDragContext>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_drag_abort";
end;

define C-function gdk-drag-begin
  input parameter window_ :: <GdkWindow>;
  input parameter targets_ :: <GList>;
  result res :: <GdkDragContext>;
  c-name: "gdk_drag_begin";
end;

define C-function gdk-drag-begin-for-device
  input parameter window_ :: <GdkWindow>;
  input parameter device_ :: <GdkDevice>;
  input parameter targets_ :: <GList>;
  result res :: <GdkDragContext>;
  c-name: "gdk_drag_begin_for_device";
end;

define C-function gdk-drag-drop
  input parameter context_ :: <GdkDragContext>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_drag_drop";
end;

define C-function gdk-drag-drop-succeeded
  input parameter context_ :: <GdkDragContext>;
  result res :: <C-boolean>;
  c-name: "gdk_drag_drop_succeeded";
end;

define C-function gdk-drag-find-window-for-screen
  input parameter context_ :: <GdkDragContext>;
  input parameter drag_window_ :: <GdkWindow>;
  input parameter screen_ :: <GdkScreen>;
  input parameter x_root_ :: <C-signed-int>;
  input parameter y_root_ :: <C-signed-int>;
  output parameter dest_window_ :: <GdkWindow*>;
  output parameter protocol_ :: <GdkDragProtocol*>;
  c-name: "gdk_drag_find_window_for_screen";
end;

define C-function gdk-drag-get-selection
  input parameter context_ :: <GdkDragContext>;
  result res :: <GdkAtom>;
  c-name: "gdk_drag_get_selection";
end;

define C-function gdk-drag-motion
  input parameter context_ :: <GdkDragContext>;
  input parameter dest_window_ :: <GdkWindow>;
  input parameter protocol_ :: <GdkDragProtocol>;
  input parameter x_root_ :: <C-signed-int>;
  input parameter y_root_ :: <C-signed-int>;
  input parameter suggested_action_ :: <GdkDragAction>;
  input parameter possible_actions_ :: <GdkDragAction>;
  input parameter time__ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gdk_drag_motion";
end;

define C-function gdk-drag-status
  input parameter context_ :: <GdkDragContext>;
  input parameter action_ :: <GdkDragAction>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_drag_status";
end;

define C-function gdk-drop-finish
  input parameter context_ :: <GdkDragContext>;
  input parameter success_ :: <C-boolean>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_drop_finish";
end;

define C-function gdk-drop-reply
  input parameter context_ :: <GdkDragContext>;
  input parameter accepted_ :: <C-boolean>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_drop_reply";
end;

define C-function gdk-error-trap-pop
  result res :: <C-signed-int>;
  c-name: "gdk_error_trap_pop";
end;

define C-function gdk-error-trap-pop-ignored
  c-name: "gdk_error_trap_pop_ignored";
end;

define C-function gdk-error-trap-push
  c-name: "gdk_error_trap_push";
end;

define C-function gdk-events-pending
  result res :: <C-boolean>;
  c-name: "gdk_events_pending";
end;

define C-function gdk-flush
  c-name: "gdk_flush";
end;

define C-function gdk-get-default-root-window
  result res :: <GdkWindow>;
  c-name: "gdk_get_default_root_window";
end;

define C-function gdk-get-display
  result res :: <C-string>;
  c-name: "gdk_get_display";
end;

define C-function gdk-get-display-arg-name
  result res :: <C-string>;
  c-name: "gdk_get_display_arg_name";
end;

define C-function gdk-get-program-class
  result res :: <C-string>;
  c-name: "gdk_get_program_class";
end;

define C-function gdk-get-show-events
  result res :: <C-boolean>;
  c-name: "gdk_get_show_events";
end;

define C-function gdk-init
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  c-name: "gdk_init";
end;

define C-function gdk-init-check
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gdk_init_check";
end;

define C-function gdk-keyval-convert-case
  input parameter symbol_ :: <C-unsigned-int>;
  output parameter lower_ :: <C-unsigned-int*>;
  output parameter upper_ :: <C-unsigned-int*>;
  c-name: "gdk_keyval_convert_case";
end;

define C-function gdk-keyval-from-name
  input parameter keyval_name_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keyval_from_name";
end;

define C-function gdk-keyval-is-lower
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gdk_keyval_is_lower";
end;

define C-function gdk-keyval-is-upper
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "gdk_keyval_is_upper";
end;

define C-function gdk-keyval-name
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "gdk_keyval_name";
end;

define C-function gdk-keyval-to-lower
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keyval_to_lower";
end;

define C-function gdk-keyval-to-unicode
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keyval_to_unicode";
end;

define C-function gdk-keyval-to-upper
  input parameter keyval_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_keyval_to_upper";
end;

define C-function gdk-list-visuals
  result res :: <GList>;
  c-name: "gdk_list_visuals";
end;

define C-function gdk-notify-startup-complete
  c-name: "gdk_notify_startup_complete";
end;

define C-function gdk-notify-startup-complete-with-id
  input parameter startup_id_ :: <C-string>;
  c-name: "gdk_notify_startup_complete_with_id";
end;

define C-function gdk-offscreen-window-get-embedder
  input parameter window_ :: <GdkWindow>;
  result res :: <GdkWindow>;
  c-name: "gdk_offscreen_window_get_embedder";
end;

define C-function gdk-offscreen-window-get-surface
  input parameter window_ :: <GdkWindow>;
  result res :: <cairoSurface>;
  c-name: "gdk_offscreen_window_get_surface";
end;

define C-function gdk-offscreen-window-set-embedder
  input parameter window_ :: <GdkWindow>;
  input parameter embedder_ :: <GdkWindow>;
  c-name: "gdk_offscreen_window_set_embedder";
end;

define C-function gdk-pango-context-get
  result res :: <PangoContext>;
  c-name: "gdk_pango_context_get";
end;

define C-function gdk-pango-context-get-for-screen
  input parameter screen_ :: <GdkScreen>;
  result res :: <PangoContext>;
  c-name: "gdk_pango_context_get_for_screen";
end;

define C-function gdk-parse-args
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  c-name: "gdk_parse_args";
end;

define C-function gdk-pixbuf-get-from-surface
  input parameter surface_ :: <cairoSurface>;
  input parameter src_x_ :: <C-signed-int>;
  input parameter src_y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_get_from_surface";
end;

define C-function gdk-pixbuf-get-from-window
  input parameter window_ :: <GdkWindow>;
  input parameter src_x_ :: <C-signed-int>;
  input parameter src_y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_get_from_window";
end;

define C-function gdk-pre-parse-libgtk-only
  c-name: "gdk_pre_parse_libgtk_only";
end;

define C-function gdk-property-delete
  input parameter window_ :: <GdkWindow>;
  input parameter property_ :: <GdkAtom>;
  c-name: "gdk_property_delete";
end;

define C-function gdk-property-get
  input parameter window_ :: <GdkWindow>;
  input parameter property_ :: <GdkAtom>;
  input parameter type_ :: <GdkAtom>;
  input parameter offset_ :: <C-unsigned-long>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter pdelete_ :: <C-signed-int>;
  output parameter actual_property_type_ :: <GdkAtom>;
  output parameter actual_format_ :: <C-signed-int*>;
  output parameter actual_length_ :: <C-signed-int*>;
  output parameter data_ :: <C-unsigned-char*>;
  result res :: <C-boolean>;
  c-name: "gdk_property_get";
end;

define C-function gdk-query-depths
  output parameter depths_ :: <C-signed-int*>;
  output parameter count_ :: <C-signed-int*>;
  c-name: "gdk_query_depths";
end;

define C-function gdk-query-visual-types
  output parameter visual_types_ :: <C-unsigned-char*> /* Not supported */;
  output parameter count_ :: <C-signed-int*>;
  c-name: "gdk_query_visual_types";
end;

define C-function gdk-rectangle-get-type
  result res :: <C-long>;
  c-name: "gdk_rectangle_get_type";
end;

define C-function gdk-rectangle-intersect
  input parameter src1_ :: <cairoRectangleInt>;
  input parameter src2_ :: <cairoRectangleInt>;
  output parameter dest_ :: <cairoRectangleInt>;
  result res :: <C-boolean>;
  c-name: "gdk_rectangle_intersect";
end;

define C-function gdk-rectangle-union
  input parameter src1_ :: <cairoRectangleInt>;
  input parameter src2_ :: <cairoRectangleInt>;
  output parameter dest_ :: <cairoRectangleInt>;
  c-name: "gdk_rectangle_union";
end;

define C-function gdk-selection-convert
  input parameter requestor_ :: <GdkWindow>;
  input parameter selection_ :: <GdkAtom>;
  input parameter target_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_selection_convert";
end;

define C-function gdk-selection-owner-get
  input parameter selection_ :: <GdkAtom>;
  result res :: <GdkWindow>;
  c-name: "gdk_selection_owner_get";
end;

define C-function gdk-selection-owner-get-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter selection_ :: <GdkAtom>;
  result res :: <GdkWindow>;
  c-name: "gdk_selection_owner_get_for_display";
end;

define C-function gdk-selection-owner-set
  input parameter owner_ :: <GdkWindow>;
  input parameter selection_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  input parameter send_event_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gdk_selection_owner_set";
end;

define C-function gdk-selection-owner-set-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter owner_ :: <GdkWindow>;
  input parameter selection_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  input parameter send_event_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "gdk_selection_owner_set_for_display";
end;

define C-function gdk-selection-send-notify
  input parameter requestor_ :: <GdkWindow>;
  input parameter selection_ :: <GdkAtom>;
  input parameter target_ :: <GdkAtom>;
  input parameter property_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_selection_send_notify";
end;

define C-function gdk-selection-send-notify-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter requestor_ :: <GdkWindow>;
  input parameter selection_ :: <GdkAtom>;
  input parameter target_ :: <GdkAtom>;
  input parameter property_ :: <GdkAtom>;
  input parameter time__ :: <C-unsigned-int>;
  c-name: "gdk_selection_send_notify_for_display";
end;

define C-function gdk-set-double-click-time
  input parameter msec_ :: <C-unsigned-int>;
  c-name: "gdk_set_double_click_time";
end;

define C-function gdk-set-program-class
  input parameter program_class_ :: <C-string>;
  c-name: "gdk_set_program_class";
end;

define C-function gdk-set-show-events
  input parameter show_events_ :: <C-boolean>;
  c-name: "gdk_set_show_events";
end;

define C-function gdk-setting-get
  input parameter name_ :: <C-string>;
  input parameter value_ :: <GValue>;
  result res :: <C-boolean>;
  c-name: "gdk_setting_get";
end;

define C-function gdk-synthesize-window-state
  input parameter window_ :: <GdkWindow>;
  input parameter unset_flags_ :: <GdkWindowState>;
  input parameter set_flags_ :: <GdkWindowState>;
  c-name: "gdk_synthesize_window_state";
end;

define C-function gdk-test-render-sync
  input parameter window_ :: <GdkWindow>;
  c-name: "gdk_test_render_sync";
end;

define C-function gdk-test-simulate-button
  input parameter window_ :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter button_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  input parameter button_pressrelease_ :: <GdkEventType>;
  result res :: <C-boolean>;
  c-name: "gdk_test_simulate_button";
end;

define C-function gdk-test-simulate-key
  input parameter window_ :: <GdkWindow>;
  input parameter x_ :: <C-signed-int>;
  input parameter y_ :: <C-signed-int>;
  input parameter keyval_ :: <C-unsigned-int>;
  input parameter modifiers_ :: <GdkModifierType>;
  input parameter key_pressrelease_ :: <GdkEventType>;
  result res :: <C-boolean>;
  c-name: "gdk_test_simulate_key";
end;

define C-function gdk-text-property-to-utf8-list-for-display
  input parameter display_ :: <GdkDisplay>;
  input parameter encoding_ :: <GdkAtom>;
  input parameter format_ :: <C-signed-int>;
  input parameter text_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-int>;
  output parameter list_ :: <C-string*>;
  result res :: <C-signed-int>;
  c-name: "gdk_text_property_to_utf8_list_for_display";
end;

define C-function gdk-threads-add-idle-full
  input parameter priority_ :: <C-signed-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_threads_add_idle_full";
end;

define C-function gdk-threads-add-timeout-full
  input parameter priority_ :: <C-signed-int>;
  input parameter interval_ :: <C-unsigned-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_threads_add_timeout_full";
end;

define C-function gdk-threads-add-timeout-seconds-full
  input parameter priority_ :: <C-signed-int>;
  input parameter interval_ :: <C-unsigned-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_threads_add_timeout_seconds_full";
end;

define C-function gdk-threads-enter
  c-name: "gdk_threads_enter";
end;

define C-function gdk-threads-init
  c-name: "gdk_threads_init";
end;

define C-function gdk-threads-leave
  c-name: "gdk_threads_leave";
end;

define C-function gdk-unicode-to-keyval
  input parameter wc_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "gdk_unicode_to_keyval";
end;

define C-function gdk-utf8-to-string-target
  input parameter str_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gdk_utf8_to_string_target";
end;

