module: gdk
synopsis: generated bindings for the Gdk library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define open C-subtype <GdkAppLaunchContext> (<GAppLaunchContext>)
end C-subtype;

define C-pointer-type <GdkAppLaunchContext*> => <GdkAppLaunchContext>;

define property-getter applaunchcontext-display :: <GdkDisplay> on <GdkAppLaunchContext> end;
define property-setter applaunchcontext-display :: <GdkDisplay> on <GdkAppLaunchContext> end;
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

define constant $GDK-AXIS-IGNORE = 0;
define constant $GDK-AXIS-X = 1;
define constant $GDK-AXIS-Y = 2;
define constant $GDK-AXIS-PRESSURE = 3;
define constant $GDK-AXIS-XTILT = 4;
define constant $GDK-AXIS-YTILT = 5;
define constant $GDK-AXIS-WHEEL = 6;
define constant $GDK-AXIS-LAST = 7;
define constant <GdkAxisUse> = <C-int>;
define C-pointer-type <GdkAxisUse*> => <GdkAxisUse>;

define constant $BUTTON-MIDDLE = 2;

define constant $BUTTON-PRIMARY = 1;

define constant $BUTTON-SECONDARY = 3;

define constant $GDK-LSB-FIRST = 0;
define constant $GDK-MSB-FIRST = 1;
define constant <GdkByteOrder> = <C-int>;
define C-pointer-type <GdkByteOrder*> => <GdkByteOrder>;

define constant $CURRENT-TIME = 0;

define C-struct <_GdkColor>
  slot gdkcolor-pixel :: <C-unsigned-int>;
  slot gdkcolor-red :: <C-unsigned-short>;
  slot gdkcolor-green :: <C-unsigned-short>;
  slot gdkcolor-blue :: <C-unsigned-short>;
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

define constant $GDK-CROSSING-NORMAL = 0;
define constant $GDK-CROSSING-GRAB = 1;
define constant $GDK-CROSSING-UNGRAB = 2;
define constant $GDK-CROSSING-GTK-GRAB = 3;
define constant $GDK-CROSSING-GTK-UNGRAB = 4;
define constant $GDK-CROSSING-STATE-CHANGED = 5;
define constant $GDK-CROSSING-TOUCH-BEGIN = 6;
define constant $GDK-CROSSING-TOUCH-END = 7;
define constant $GDK-CROSSING-DEVICE-SWITCH = 8;
define constant <GdkCrossingMode> = <C-int>;
define C-pointer-type <GdkCrossingMode*> => <GdkCrossingMode>;

define open C-subtype <GdkCursor> (<GObject>)
end C-subtype;

define C-pointer-type <GdkCursor*> => <GdkCursor>;

define property-getter cursor-cursor-type :: <GdkCursorType> on <GdkCursor> end;
define property-setter cursor-cursor-type :: <GdkCursorType> on <GdkCursor> end;
define property-getter cursor-display :: <GdkDisplay> on <GdkCursor> end;
define property-setter cursor-display :: <GdkDisplay> on <GdkCursor> end;
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

define constant $GDK-X-CURSOR = 0;
define constant $GDK-ARROW = 2;
define constant $GDK-BASED-ARROW-DOWN = 4;
define constant $GDK-BASED-ARROW-UP = 6;
define constant $GDK-BOAT = 8;
define constant $GDK-BOGOSITY = 10;
define constant $GDK-BOTTOM-LEFT-CORNER = 12;
define constant $GDK-BOTTOM-RIGHT-CORNER = 14;
define constant $GDK-BOTTOM-SIDE = 16;
define constant $GDK-BOTTOM-TEE = 18;
define constant $GDK-BOX-SPIRAL = 20;
define constant $GDK-CENTER-PTR = 22;
define constant $GDK-CIRCLE = 24;
define constant $GDK-CLOCK = 26;
define constant $GDK-COFFEE-MUG = 28;
define constant $GDK-CROSS = 30;
define constant $GDK-CROSS-REVERSE = 32;
define constant $GDK-CROSSHAIR = 34;
define constant $GDK-DIAMOND-CROSS = 36;
define constant $GDK-DOT = 38;
define constant $GDK-DOTBOX = 40;
define constant $GDK-DOUBLE-ARROW = 42;
define constant $GDK-DRAFT-LARGE = 44;
define constant $GDK-DRAFT-SMALL = 46;
define constant $GDK-DRAPED-BOX = 48;
define constant $GDK-EXCHANGE = 50;
define constant $GDK-FLEUR = 52;
define constant $GDK-GOBBLER = 54;
define constant $GDK-GUMBY = 56;
define constant $GDK-HAND1 = 58;
define constant $GDK-HAND2 = 60;
define constant $GDK-HEART = 62;
define constant $GDK-ICON = 64;
define constant $GDK-IRON-CROSS = 66;
define constant $GDK-LEFT-PTR = 68;
define constant $GDK-LEFT-SIDE = 70;
define constant $GDK-LEFT-TEE = 72;
define constant $GDK-LEFTBUTTON = 74;
define constant $GDK-LL-ANGLE = 76;
define constant $GDK-LR-ANGLE = 78;
define constant $GDK-MAN = 80;
define constant $GDK-MIDDLEBUTTON = 82;
define constant $GDK-MOUSE = 84;
define constant $GDK-PENCIL = 86;
define constant $GDK-PIRATE = 88;
define constant $GDK-PLUS = 90;
define constant $GDK-QUESTION-ARROW = 92;
define constant $GDK-RIGHT-PTR = 94;
define constant $GDK-RIGHT-SIDE = 96;
define constant $GDK-RIGHT-TEE = 98;
define constant $GDK-RIGHTBUTTON = 100;
define constant $GDK-RTL-LOGO = 102;
define constant $GDK-SAILBOAT = 104;
define constant $GDK-SB-DOWN-ARROW = 106;
define constant $GDK-SB-H-DOUBLE-ARROW = 108;
define constant $GDK-SB-LEFT-ARROW = 110;
define constant $GDK-SB-RIGHT-ARROW = 112;
define constant $GDK-SB-UP-ARROW = 114;
define constant $GDK-SB-V-DOUBLE-ARROW = 116;
define constant $GDK-SHUTTLE = 118;
define constant $GDK-SIZING = 120;
define constant $GDK-SPIDER = 122;
define constant $GDK-SPRAYCAN = 124;
define constant $GDK-STAR = 126;
define constant $GDK-TARGET = 128;
define constant $GDK-TCROSS = 130;
define constant $GDK-TOP-LEFT-ARROW = 132;
define constant $GDK-TOP-LEFT-CORNER = 134;
define constant $GDK-TOP-RIGHT-CORNER = 136;
define constant $GDK-TOP-SIDE = 138;
define constant $GDK-TOP-TEE = 140;
define constant $GDK-TREK = 142;
define constant $GDK-UL-ANGLE = 144;
define constant $GDK-UMBRELLA = 146;
define constant $GDK-UR-ANGLE = 148;
define constant $GDK-WATCH = 150;
define constant $GDK-XTERM = 152;
define constant $GDK-LAST-CURSOR = 153;
define constant $GDK-BLANK-CURSOR = -2;
define constant $GDK-CURSOR-IS-PIXMAP = -1;
define constant <GdkCursorType> = <C-int>;
define C-pointer-type <GdkCursorType*> => <GdkCursorType>;

define open C-subtype <GdkDevice> (<GObject>)
end C-subtype;

define C-pointer-type <GdkDevice*> => <GdkDevice>;

define property-getter device-associated-device :: <GdkDevice> on <GdkDevice> end;
define property-getter device-device-manager :: <GdkDeviceManager> on <GdkDevice> end;
define property-setter device-device-manager :: <GdkDeviceManager> on <GdkDevice> end;
define property-getter device-display :: <GdkDisplay> on <GdkDevice> end;
define property-setter device-display :: <GdkDisplay> on <GdkDevice> end;
define property-getter device-has-cursor :: <C-boolean> on <GdkDevice> end;
define property-setter device-has-cursor :: <C-boolean> on <GdkDevice> end;
define property-getter device-input-mode :: <GdkInputMode> on <GdkDevice> end;
define property-setter device-input-mode :: <GdkInputMode> on <GdkDevice> end;
define property-getter device-input-source :: <GdkInputSource> on <GdkDevice> end;
define property-setter device-input-source :: <GdkInputSource> on <GdkDevice> end;
define property-getter device-n-axes :: <C-unsigned-int> on <GdkDevice> end;
define property-getter device-name :: <C-string> on <GdkDevice> end;
define property-setter device-name :: <C-string> on <GdkDevice> end;
define property-getter device-type :: <GdkDeviceType> on <GdkDevice> end;
define property-setter device-type :: <GdkDeviceType> on <GdkDevice> end;
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

define property-getter devicemanager-display :: <GdkDisplay> on <GdkDeviceManager> end;
define property-setter devicemanager-display :: <GdkDisplay> on <GdkDeviceManager> end;
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

define constant $GDK-DEVICE-TYPE-MASTER = 0;
define constant $GDK-DEVICE-TYPE-SLAVE = 1;
define constant $GDK-DEVICE-TYPE-FLOATING = 2;
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

define property-getter displaymanager-default-display :: <GdkDisplay> on <GdkDisplayManager> end;
define property-setter displaymanager-default-display :: <GdkDisplay> on <GdkDisplayManager> end;
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

define constant $GDK-ACTION-DEFAULT = 1;
define constant $GDK-ACTION-COPY = 2;
define constant $GDK-ACTION-MOVE = 4;
define constant $GDK-ACTION-LINK = 8;
define constant $GDK-ACTION-PRIVATE = 16;
define constant $GDK-ACTION-ASK = 32;
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

define constant $GDK-DRAG-PROTO-NONE = 0;
define constant $GDK-DRAG-PROTO-MOTIF = 1;
define constant $GDK-DRAG-PROTO-XDND = 2;
define constant $GDK-DRAG-PROTO-ROOTWIN = 3;
define constant $GDK-DRAG-PROTO-WIN32-DROPFILES = 4;
define constant $GDK-DRAG-PROTO-OLE2 = 5;
define constant $GDK-DRAG-PROTO-LOCAL = 6;
define constant <GdkDragProtocol> = <C-int>;
define C-pointer-type <GdkDragProtocol*> => <GdkDragProtocol>;

define C-union <_GdkEvent>
  slot gdkevent-type :: <GdkEventType>;
  slot gdkevent-any :: <GdkEventAny>;
  slot gdkevent-expose :: <GdkEventExpose>;
  slot gdkevent-visibility :: <GdkEventVisibility>;
  slot gdkevent-motion :: <GdkEventMotion>;
  slot gdkevent-button :: <GdkEventButton>;
  slot gdkevent-touch :: <GdkEventTouch>;
  slot gdkevent-scroll :: <GdkEventScroll>;
  slot gdkevent-key :: <GdkEventKey>;
  slot gdkevent-crossing :: <GdkEventCrossing>;
  slot gdkevent-focus-change :: <GdkEventFocus>;
  slot gdkevent-configure :: <GdkEventConfigure>;
  slot gdkevent-property :: <GdkEventProperty>;
  slot gdkevent-selection :: <GdkEventSelection>;
  slot gdkevent-owner-change :: <GdkEventOwnerChange>;
  slot gdkevent-proximity :: <GdkEventProximity>;
  slot gdkevent-dnd :: <GdkEventDND>;
  slot gdkevent-window-state :: <GdkEventWindowState>;
  slot gdkevent-setting :: <GdkEventSetting>;
  slot gdkevent-grab-broken :: <GdkEventGrabBroken>;
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
  slot gdkeventany-type :: <GdkEventType>;
  slot gdkeventany-window :: <GdkWindow>;
  slot gdkeventany-send-event :: <C-signed-char>;
  pointer-type-name: <GdkEventAny>;
end C-struct;

define C-struct <_GdkEventButton>
  slot gdkeventbutton-type :: <GdkEventType>;
  slot gdkeventbutton-window :: <GdkWindow>;
  slot gdkeventbutton-send-event :: <C-signed-char>;
  slot gdkeventbutton-time :: <C-unsigned-int>;
  slot gdkeventbutton-x :: <C-double>;
  slot gdkeventbutton-y :: <C-double>;
  slot gdkeventbutton-axes :: <C-double*>;
  slot gdkeventbutton-state :: <GdkModifierType>;
  slot gdkeventbutton-button :: <C-unsigned-int>;
  slot gdkeventbutton-device :: <GdkDevice>;
  slot gdkeventbutton-x-root :: <C-double>;
  slot gdkeventbutton-y-root :: <C-double>;
  pointer-type-name: <GdkEventButton>;
end C-struct;

define C-struct <_GdkEventConfigure>
  slot gdkeventconfigure-type :: <GdkEventType>;
  slot gdkeventconfigure-window :: <GdkWindow>;
  slot gdkeventconfigure-send-event :: <C-signed-char>;
  slot gdkeventconfigure-x :: <C-signed-int>;
  slot gdkeventconfigure-y :: <C-signed-int>;
  slot gdkeventconfigure-width :: <C-signed-int>;
  slot gdkeventconfigure-height :: <C-signed-int>;
  pointer-type-name: <GdkEventConfigure>;
end C-struct;

define C-struct <_GdkEventCrossing>
  slot gdkeventcrossing-type :: <GdkEventType>;
  slot gdkeventcrossing-window :: <GdkWindow>;
  slot gdkeventcrossing-send-event :: <C-signed-char>;
  slot gdkeventcrossing-subwindow :: <GdkWindow>;
  slot gdkeventcrossing-time :: <C-unsigned-int>;
  slot gdkeventcrossing-x :: <C-double>;
  slot gdkeventcrossing-y :: <C-double>;
  slot gdkeventcrossing-x-root :: <C-double>;
  slot gdkeventcrossing-y-root :: <C-double>;
  slot gdkeventcrossing-mode :: <GdkCrossingMode>;
  slot gdkeventcrossing-detail :: <GdkNotifyType>;
  slot gdkeventcrossing-focus :: <C-boolean>;
  slot gdkeventcrossing-state :: <GdkModifierType>;
  pointer-type-name: <GdkEventCrossing>;
end C-struct;

define C-struct <_GdkEventDND>
  slot gdkeventdnd-type :: <GdkEventType>;
  slot gdkeventdnd-window :: <GdkWindow>;
  slot gdkeventdnd-send-event :: <C-signed-char>;
  slot gdkeventdnd-context :: <GdkDragContext>;
  slot gdkeventdnd-time :: <C-unsigned-int>;
  slot gdkeventdnd-x-root :: <C-signed-short>;
  slot gdkeventdnd-y-root :: <C-signed-short>;
  pointer-type-name: <GdkEventDND>;
end C-struct;

define C-struct <_GdkEventExpose>
  slot gdkeventexpose-type :: <GdkEventType>;
  slot gdkeventexpose-window :: <GdkWindow>;
  slot gdkeventexpose-send-event :: <C-signed-char>;
  slot gdkeventexpose-area :: <cairoRectangleInt>;
  slot gdkeventexpose-region :: <cairoRegion>;
  slot gdkeventexpose-count :: <C-signed-int>;
  pointer-type-name: <GdkEventExpose>;
end C-struct;

define C-struct <_GdkEventFocus>
  slot gdkeventfocus-type :: <GdkEventType>;
  slot gdkeventfocus-window :: <GdkWindow>;
  slot gdkeventfocus-send-event :: <C-signed-char>;
  slot gdkeventfocus-in :: <C-signed-short>;
  pointer-type-name: <GdkEventFocus>;
end C-struct;

define C-struct <_GdkEventGrabBroken>
  slot gdkeventgrabbroken-type :: <GdkEventType>;
  slot gdkeventgrabbroken-window :: <GdkWindow>;
  slot gdkeventgrabbroken-send-event :: <C-signed-char>;
  slot gdkeventgrabbroken-keyboard :: <C-boolean>;
  slot gdkeventgrabbroken-implicit :: <C-boolean>;
  slot gdkeventgrabbroken-grab-window :: <GdkWindow>;
  pointer-type-name: <GdkEventGrabBroken>;
end C-struct;

define C-struct <_GdkEventKey>
  slot gdkeventkey-type :: <GdkEventType>;
  slot gdkeventkey-window :: <GdkWindow>;
  slot gdkeventkey-send-event :: <C-signed-char>;
  slot gdkeventkey-time :: <C-unsigned-int>;
  slot gdkeventkey-state :: <GdkModifierType>;
  slot gdkeventkey-keyval :: <C-unsigned-int>;
  slot gdkeventkey-length :: <C-signed-int>;
  slot gdkeventkey-string :: <C-string>;
  slot gdkeventkey-hardware-keycode :: <C-unsigned-short>;
  slot gdkeventkey-group :: <C-unsigned-char>;
  slot gdkeventkey-is-modifier :: <C-unsigned-int>;
  pointer-type-name: <GdkEventKey>;
end C-struct;

define constant $GDK-EXPOSURE-MASK = 2;
define constant $GDK-POINTER-MOTION-MASK = 4;
define constant $GDK-POINTER-MOTION-HINT-MASK = 8;
define constant $GDK-BUTTON-MOTION-MASK = 16;
define constant $GDK-BUTTON1-MOTION-MASK = 32;
define constant $GDK-BUTTON2-MOTION-MASK = 64;
define constant $GDK-BUTTON3-MOTION-MASK = 128;
define constant $GDK-BUTTON-PRESS-MASK = 256;
define constant $GDK-BUTTON-RELEASE-MASK = 512;
define constant $GDK-KEY-PRESS-MASK = 1024;
define constant $GDK-KEY-RELEASE-MASK = 2048;
define constant $GDK-ENTER-NOTIFY-MASK = 4096;
define constant $GDK-LEAVE-NOTIFY-MASK = 8192;
define constant $GDK-FOCUS-CHANGE-MASK = 16384;
define constant $GDK-STRUCTURE-MASK = 32768;
define constant $GDK-PROPERTY-CHANGE-MASK = 65536;
define constant $GDK-VISIBILITY-NOTIFY-MASK = 131072;
define constant $GDK-PROXIMITY-IN-MASK = 262144;
define constant $GDK-PROXIMITY-OUT-MASK = 524288;
define constant $GDK-SUBSTRUCTURE-MASK = 1048576;
define constant $GDK-SCROLL-MASK = 2097152;
define constant $GDK-TOUCH-MASK = 4194304;
define constant $GDK-SMOOTH-SCROLL-MASK = 8388608;
define constant $GDK-ALL-EVENTS-MASK = 16777214;
define constant <GdkEventMask> = <C-int>;
define C-pointer-type <GdkEventMask*> => <GdkEventMask>;

define C-struct <_GdkEventMotion>
  slot gdkeventmotion-type :: <GdkEventType>;
  slot gdkeventmotion-window :: <GdkWindow>;
  slot gdkeventmotion-send-event :: <C-signed-char>;
  slot gdkeventmotion-time :: <C-unsigned-int>;
  slot gdkeventmotion-x :: <C-double>;
  slot gdkeventmotion-y :: <C-double>;
  slot gdkeventmotion-axes :: <C-double*>;
  slot gdkeventmotion-state :: <GdkModifierType>;
  slot gdkeventmotion-is-hint :: <C-signed-short>;
  slot gdkeventmotion-device :: <GdkDevice>;
  slot gdkeventmotion-x-root :: <C-double>;
  slot gdkeventmotion-y-root :: <C-double>;
  pointer-type-name: <GdkEventMotion>;
end C-struct;

define C-struct <_GdkEventOwnerChange>
  slot gdkeventownerchange-type :: <GdkEventType>;
  slot gdkeventownerchange-window :: <GdkWindow>;
  slot gdkeventownerchange-send-event :: <C-signed-char>;
  slot gdkeventownerchange-owner :: <GdkWindow>;
  slot gdkeventownerchange-reason :: <GdkOwnerChange>;
  slot gdkeventownerchange-selection :: <GdkAtom>;
  slot gdkeventownerchange-time :: <C-unsigned-int>;
  slot gdkeventownerchange-selection-time :: <C-unsigned-int>;
  pointer-type-name: <GdkEventOwnerChange>;
end C-struct;

define C-struct <_GdkEventProperty>
  slot gdkeventproperty-type :: <GdkEventType>;
  slot gdkeventproperty-window :: <GdkWindow>;
  slot gdkeventproperty-send-event :: <C-signed-char>;
  slot gdkeventproperty-atom :: <GdkAtom>;
  slot gdkeventproperty-time :: <C-unsigned-int>;
  slot gdkeventproperty-state :: <C-unsigned-int>;
  pointer-type-name: <GdkEventProperty>;
end C-struct;

define C-struct <_GdkEventProximity>
  slot gdkeventproximity-type :: <GdkEventType>;
  slot gdkeventproximity-window :: <GdkWindow>;
  slot gdkeventproximity-send-event :: <C-signed-char>;
  slot gdkeventproximity-time :: <C-unsigned-int>;
  slot gdkeventproximity-device :: <GdkDevice>;
  pointer-type-name: <GdkEventProximity>;
end C-struct;

define C-struct <_GdkEventScroll>
  slot gdkeventscroll-type :: <GdkEventType>;
  slot gdkeventscroll-window :: <GdkWindow>;
  slot gdkeventscroll-send-event :: <C-signed-char>;
  slot gdkeventscroll-time :: <C-unsigned-int>;
  slot gdkeventscroll-x :: <C-double>;
  slot gdkeventscroll-y :: <C-double>;
  slot gdkeventscroll-state :: <GdkModifierType>;
  slot gdkeventscroll-direction :: <GdkScrollDirection>;
  slot gdkeventscroll-device :: <GdkDevice>;
  slot gdkeventscroll-x-root :: <C-double>;
  slot gdkeventscroll-y-root :: <C-double>;
  slot gdkeventscroll-delta-x :: <C-double>;
  slot gdkeventscroll-delta-y :: <C-double>;
  pointer-type-name: <GdkEventScroll>;
end C-struct;

define C-struct <_GdkEventSelection>
  slot gdkeventselection-type :: <GdkEventType>;
  slot gdkeventselection-window :: <GdkWindow>;
  slot gdkeventselection-send-event :: <C-signed-char>;
  slot gdkeventselection-selection :: <GdkAtom>;
  slot gdkeventselection-target :: <GdkAtom>;
  slot gdkeventselection-property :: <GdkAtom>;
  slot gdkeventselection-time :: <C-unsigned-int>;
  slot gdkeventselection-requestor :: <GdkWindow>;
  pointer-type-name: <GdkEventSelection>;
end C-struct;

define C-struct <_GdkEventSequence>
  pointer-type-name: <GdkEventSequence>;
end C-struct;

define C-struct <_GdkEventSetting>
  slot gdkeventsetting-type :: <GdkEventType>;
  slot gdkeventsetting-window :: <GdkWindow>;
  slot gdkeventsetting-send-event :: <C-signed-char>;
  slot gdkeventsetting-action :: <GdkSettingAction>;
  slot gdkeventsetting-name :: <C-string>;
  pointer-type-name: <GdkEventSetting>;
end C-struct;

define C-struct <_GdkEventTouch>
  slot gdkeventtouch-type :: <GdkEventType>;
  slot gdkeventtouch-window :: <GdkWindow>;
  slot gdkeventtouch-send-event :: <C-signed-char>;
  slot gdkeventtouch-time :: <C-unsigned-int>;
  slot gdkeventtouch-x :: <C-double>;
  slot gdkeventtouch-y :: <C-double>;
  slot gdkeventtouch-axes :: <C-double*>;
  slot gdkeventtouch-state :: <GdkModifierType>;
  slot gdkeventtouch-sequence :: <GdkEventSequence>;
  slot gdkeventtouch-emulating-pointer :: <C-boolean>;
  slot gdkeventtouch-device :: <GdkDevice>;
  slot gdkeventtouch-x-root :: <C-double>;
  slot gdkeventtouch-y-root :: <C-double>;
  pointer-type-name: <GdkEventTouch>;
end C-struct;

define constant $GDK-NOTHING = -1;
define constant $GDK-DELETE = 0;
define constant $GDK-DESTROY = 1;
define constant $GDK-EXPOSE = 2;
define constant $GDK-MOTION-NOTIFY = 3;
define constant $GDK-BUTTON-PRESS = 4;
define constant $GDK-2BUTTON-PRESS = 5;
define constant $GDK-3BUTTON-PRESS = 6;
define constant $GDK-BUTTON-RELEASE = 7;
define constant $GDK-KEY-PRESS = 8;
define constant $GDK-KEY-RELEASE = 9;
define constant $GDK-ENTER-NOTIFY = 10;
define constant $GDK-LEAVE-NOTIFY = 11;
define constant $GDK-FOCUS-CHANGE = 12;
define constant $GDK-CONFIGURE = 13;
define constant $GDK-MAP = 14;
define constant $GDK-UNMAP = 15;
define constant $GDK-PROPERTY-NOTIFY = 16;
define constant $GDK-SELECTION-CLEAR = 17;
define constant $GDK-SELECTION-REQUEST = 18;
define constant $GDK-SELECTION-NOTIFY = 19;
define constant $GDK-PROXIMITY-IN = 20;
define constant $GDK-PROXIMITY-OUT = 21;
define constant $GDK-DRAG-ENTER = 22;
define constant $GDK-DRAG-LEAVE = 23;
define constant $GDK-DRAG-MOTION = 24;
define constant $GDK-DRAG-STATUS = 25;
define constant $GDK-DROP-START = 26;
define constant $GDK-DROP-FINISHED = 27;
define constant $GDK-CLIENT-EVENT = 28;
define constant $GDK-VISIBILITY-NOTIFY = 29;
define constant $GDK-SCROLL = 31;
define constant $GDK-WINDOW-STATE = 32;
define constant $GDK-SETTING = 33;
define constant $GDK-OWNER-CHANGE = 34;
define constant $GDK-GRAB-BROKEN = 35;
define constant $GDK-DAMAGE = 36;
define constant $GDK-TOUCH-BEGIN = 37;
define constant $GDK-TOUCH-UPDATE = 38;
define constant $GDK-TOUCH-END = 39;
define constant $GDK-TOUCH-CANCEL = 40;
define constant $GDK-EVENT-LAST = 41;
define constant <GdkEventType> = <C-int>;
define C-pointer-type <GdkEventType*> => <GdkEventType>;

define C-struct <_GdkEventVisibility>
  slot gdkeventvisibility-type :: <GdkEventType>;
  slot gdkeventvisibility-window :: <GdkWindow>;
  slot gdkeventvisibility-send-event :: <C-signed-char>;
  slot gdkeventvisibility-state :: <GdkVisibilityState>;
  pointer-type-name: <GdkEventVisibility>;
end C-struct;

define C-struct <_GdkEventWindowState>
  slot gdkeventwindowstate-type :: <GdkEventType>;
  slot gdkeventwindowstate-window :: <GdkWindow>;
  slot gdkeventwindowstate-send-event :: <C-signed-char>;
  slot gdkeventwindowstate-changed-mask :: <GdkWindowState>;
  slot gdkeventwindowstate-new-window-state :: <GdkWindowState>;
  pointer-type-name: <GdkEventWindowState>;
end C-struct;

define constant $GDK-FILTER-CONTINUE = 0;
define constant $GDK-FILTER-TRANSLATE = 1;
define constant $GDK-FILTER-REMOVE = 2;
define constant <GdkFilterReturn> = <C-int>;
define C-pointer-type <GdkFilterReturn*> => <GdkFilterReturn>;

define C-struct <_GdkGeometry>
  slot gdkgeometry-min-width :: <C-signed-int>;
  slot gdkgeometry-min-height :: <C-signed-int>;
  slot gdkgeometry-max-width :: <C-signed-int>;
  slot gdkgeometry-max-height :: <C-signed-int>;
  slot gdkgeometry-base-width :: <C-signed-int>;
  slot gdkgeometry-base-height :: <C-signed-int>;
  slot gdkgeometry-width-inc :: <C-signed-int>;
  slot gdkgeometry-height-inc :: <C-signed-int>;
  slot gdkgeometry-min-aspect :: <C-double>;
  slot gdkgeometry-max-aspect :: <C-double>;
  slot gdkgeometry-win-gravity :: <GdkGravity>;
  pointer-type-name: <GdkGeometry>;
end C-struct;

define constant $GDK-OWNERSHIP-NONE = 0;
define constant $GDK-OWNERSHIP-WINDOW = 1;
define constant $GDK-OWNERSHIP-APPLICATION = 2;
define constant <GdkGrabOwnership> = <C-int>;
define C-pointer-type <GdkGrabOwnership*> => <GdkGrabOwnership>;

define constant $GDK-GRAB-SUCCESS = 0;
define constant $GDK-GRAB-ALREADY-GRABBED = 1;
define constant $GDK-GRAB-INVALID-TIME = 2;
define constant $GDK-GRAB-NOT-VIEWABLE = 3;
define constant $GDK-GRAB-FROZEN = 4;
define constant <GdkGrabStatus> = <C-int>;
define C-pointer-type <GdkGrabStatus*> => <GdkGrabStatus>;

define constant $GDK-GRAVITY-NORTH-WEST = 1;
define constant $GDK-GRAVITY-NORTH = 2;
define constant $GDK-GRAVITY-NORTH-EAST = 3;
define constant $GDK-GRAVITY-WEST = 4;
define constant $GDK-GRAVITY-CENTER = 5;
define constant $GDK-GRAVITY-EAST = 6;
define constant $GDK-GRAVITY-SOUTH-WEST = 7;
define constant $GDK-GRAVITY-SOUTH = 8;
define constant $GDK-GRAVITY-SOUTH-EAST = 9;
define constant $GDK-GRAVITY-STATIC = 10;
define constant <GdkGravity> = <C-int>;
define C-pointer-type <GdkGravity*> => <GdkGravity>;

define constant $GDK-MODE-DISABLED = 0;
define constant $GDK-MODE-SCREEN = 1;
define constant $GDK-MODE-WINDOW = 2;
define constant <GdkInputMode> = <C-int>;
define C-pointer-type <GdkInputMode*> => <GdkInputMode>;

define constant $GDK-SOURCE-MOUSE = 0;
define constant $GDK-SOURCE-PEN = 1;
define constant $GDK-SOURCE-ERASER = 2;
define constant $GDK-SOURCE-CURSOR = 3;
define constant $GDK-SOURCE-KEYBOARD = 4;
define constant $GDK-SOURCE-TOUCHSCREEN = 5;
define constant $GDK-SOURCE-TOUCHPAD = 6;
define constant <GdkInputSource> = <C-int>;
define C-pointer-type <GdkInputSource*> => <GdkInputSource>;

define constant $KEY-0 = 48;

define constant $KEY-1 = 49;

define constant $KEY-2 = 50;

define constant $KEY-3 = 51;

define constant $KEY-3270-AltCursor = 64784;

define constant $KEY-3270-Attn = 64782;

define constant $KEY-3270-BackTab = 64773;

define constant $KEY-3270-ChangeScreen = 64793;

define constant $KEY-3270-Copy = 64789;

define constant $KEY-3270-CursorBlink = 64783;

define constant $KEY-3270-CursorSelect = 64796;

define constant $KEY-3270-DeleteWord = 64794;

define constant $KEY-3270-Duplicate = 64769;

define constant $KEY-3270-Enter = 64798;

define constant $KEY-3270-EraseEOF = 64774;

define constant $KEY-3270-EraseInput = 64775;

define constant $KEY-3270-ExSelect = 64795;

define constant $KEY-3270-FieldMark = 64770;

define constant $KEY-3270-Ident = 64787;

define constant $KEY-3270-Jump = 64786;

define constant $KEY-3270-KeyClick = 64785;

define constant $KEY-3270-Left2 = 64772;

define constant $KEY-3270-PA1 = 64778;

define constant $KEY-3270-PA2 = 64779;

define constant $KEY-3270-PA3 = 64780;

define constant $KEY-3270-Play = 64790;

define constant $KEY-3270-PrintScreen = 64797;

define constant $KEY-3270-Quit = 64777;

define constant $KEY-3270-Record = 64792;

define constant $KEY-3270-Reset = 64776;

define constant $KEY-3270-Right2 = 64771;

define constant $KEY-3270-Rule = 64788;

define constant $KEY-3270-Setup = 64791;

define constant $KEY-3270-Test = 64781;

define constant $KEY-4 = 52;

define constant $KEY-5 = 53;

define constant $KEY-6 = 54;

define constant $KEY-7 = 55;

define constant $KEY-8 = 56;

define constant $KEY-9 = 57;

define constant $KEY-A = 65;

define constant $KEY-AE = 198;

define constant $KEY-Aacute = 193;

define constant $KEY-Abelowdot = 16785056;

define constant $KEY-Abreve = 451;

define constant $KEY-Abreveacute = 16785070;

define constant $KEY-Abrevebelowdot = 16785078;

define constant $KEY-Abrevegrave = 16785072;

define constant $KEY-Abrevehook = 16785074;

define constant $KEY-Abrevetilde = 16785076;

define constant $KEY-AccessX-Enable = 65136;

define constant $KEY-AccessX-Feedback-Enable = 65137;

define constant $KEY-Acircumflex = 194;

define constant $KEY-Acircumflexacute = 16785060;

define constant $KEY-Acircumflexbelowdot = 16785068;

define constant $KEY-Acircumflexgrave = 16785062;

define constant $KEY-Acircumflexhook = 16785064;

define constant $KEY-Acircumflextilde = 16785066;

define constant $KEY-AddFavorite = 269025081;

define constant $KEY-Adiaeresis = 196;

define constant $KEY-Agrave = 192;

define constant $KEY-Ahook = 16785058;

define constant $KEY-Alt-L = 65513;

define constant $KEY-Alt-R = 65514;

define constant $KEY-Amacron = 960;

define constant $KEY-Aogonek = 417;

define constant $KEY-ApplicationLeft = 269025104;

define constant $KEY-ApplicationRight = 269025105;

define constant $KEY-Arabic-0 = 16778848;

define constant $KEY-Arabic-1 = 16778849;

define constant $KEY-Arabic-2 = 16778850;

define constant $KEY-Arabic-3 = 16778851;

define constant $KEY-Arabic-4 = 16778852;

define constant $KEY-Arabic-5 = 16778853;

define constant $KEY-Arabic-6 = 16778854;

define constant $KEY-Arabic-7 = 16778855;

define constant $KEY-Arabic-8 = 16778856;

define constant $KEY-Arabic-9 = 16778857;

define constant $KEY-Arabic-ain = 1497;

define constant $KEY-Arabic-alef = 1479;

define constant $KEY-Arabic-alefmaksura = 1513;

define constant $KEY-Arabic-beh = 1480;

define constant $KEY-Arabic-comma = 1452;

define constant $KEY-Arabic-dad = 1494;

define constant $KEY-Arabic-dal = 1487;

define constant $KEY-Arabic-damma = 1519;

define constant $KEY-Arabic-dammatan = 1516;

define constant $KEY-Arabic-ddal = 16778888;

define constant $KEY-Arabic-farsi-yeh = 16778956;

define constant $KEY-Arabic-fatha = 1518;

define constant $KEY-Arabic-fathatan = 1515;

define constant $KEY-Arabic-feh = 1505;

define constant $KEY-Arabic-fullstop = 16778964;

define constant $KEY-Arabic-gaf = 16778927;

define constant $KEY-Arabic-ghain = 1498;

define constant $KEY-Arabic-ha = 1511;

define constant $KEY-Arabic-hah = 1485;

define constant $KEY-Arabic-hamza = 1473;

define constant $KEY-Arabic-hamza-above = 16778836;

define constant $KEY-Arabic-hamza-below = 16778837;

define constant $KEY-Arabic-hamzaonalef = 1475;

define constant $KEY-Arabic-hamzaonwaw = 1476;

define constant $KEY-Arabic-hamzaonyeh = 1478;

define constant $KEY-Arabic-hamzaunderalef = 1477;

define constant $KEY-Arabic-heh = 1511;

define constant $KEY-Arabic-heh-doachashmee = 16778942;

define constant $KEY-Arabic-heh-goal = 16778945;

define constant $KEY-Arabic-jeem = 1484;

define constant $KEY-Arabic-jeh = 16778904;

define constant $KEY-Arabic-kaf = 1507;

define constant $KEY-Arabic-kasra = 1520;

define constant $KEY-Arabic-kasratan = 1517;

define constant $KEY-Arabic-keheh = 16778921;

define constant $KEY-Arabic-khah = 1486;

define constant $KEY-Arabic-lam = 1508;

define constant $KEY-Arabic-madda-above = 16778835;

define constant $KEY-Arabic-maddaonalef = 1474;

define constant $KEY-Arabic-meem = 1509;

define constant $KEY-Arabic-noon = 1510;

define constant $KEY-Arabic-noon-ghunna = 16778938;

define constant $KEY-Arabic-peh = 16778878;

define constant $KEY-Arabic-percent = 16778858;

define constant $KEY-Arabic-qaf = 1506;

define constant $KEY-Arabic-question-mark = 1471;

define constant $KEY-Arabic-ra = 1489;

define constant $KEY-Arabic-rreh = 16778897;

define constant $KEY-Arabic-sad = 1493;

define constant $KEY-Arabic-seen = 1491;

define constant $KEY-Arabic-semicolon = 1467;

define constant $KEY-Arabic-shadda = 1521;

define constant $KEY-Arabic-sheen = 1492;

define constant $KEY-Arabic-sukun = 1522;

define constant $KEY-Arabic-superscript-alef = 16778864;

define constant $KEY-Arabic-switch = 65406;

define constant $KEY-Arabic-tah = 1495;

define constant $KEY-Arabic-tatweel = 1504;

define constant $KEY-Arabic-tcheh = 16778886;

define constant $KEY-Arabic-teh = 1482;

define constant $KEY-Arabic-tehmarbuta = 1481;

define constant $KEY-Arabic-thal = 1488;

define constant $KEY-Arabic-theh = 1483;

define constant $KEY-Arabic-tteh = 16778873;

define constant $KEY-Arabic-veh = 16778916;

define constant $KEY-Arabic-waw = 1512;

define constant $KEY-Arabic-yeh = 1514;

define constant $KEY-Arabic-yeh-baree = 16778962;

define constant $KEY-Arabic-zah = 1496;

define constant $KEY-Arabic-zain = 1490;

define constant $KEY-Aring = 197;

define constant $KEY-Armenian-AT = 16778552;

define constant $KEY-Armenian-AYB = 16778545;

define constant $KEY-Armenian-BEN = 16778546;

define constant $KEY-Armenian-CHA = 16778569;

define constant $KEY-Armenian-DA = 16778548;

define constant $KEY-Armenian-DZA = 16778561;

define constant $KEY-Armenian-E = 16778551;

define constant $KEY-Armenian-FE = 16778582;

define constant $KEY-Armenian-GHAT = 16778562;

define constant $KEY-Armenian-GIM = 16778547;

define constant $KEY-Armenian-HI = 16778565;

define constant $KEY-Armenian-HO = 16778560;

define constant $KEY-Armenian-INI = 16778555;

define constant $KEY-Armenian-JE = 16778571;

define constant $KEY-Armenian-KE = 16778580;

define constant $KEY-Armenian-KEN = 16778559;

define constant $KEY-Armenian-KHE = 16778557;

define constant $KEY-Armenian-LYUN = 16778556;

define constant $KEY-Armenian-MEN = 16778564;

define constant $KEY-Armenian-NU = 16778566;

define constant $KEY-Armenian-O = 16778581;

define constant $KEY-Armenian-PE = 16778570;

define constant $KEY-Armenian-PYUR = 16778579;

define constant $KEY-Armenian-RA = 16778572;

define constant $KEY-Armenian-RE = 16778576;

define constant $KEY-Armenian-SE = 16778573;

define constant $KEY-Armenian-SHA = 16778567;

define constant $KEY-Armenian-TCHE = 16778563;

define constant $KEY-Armenian-TO = 16778553;

define constant $KEY-Armenian-TSA = 16778558;

define constant $KEY-Armenian-TSO = 16778577;

define constant $KEY-Armenian-TYUN = 16778575;

define constant $KEY-Armenian-VEV = 16778574;

define constant $KEY-Armenian-VO = 16778568;

define constant $KEY-Armenian-VYUN = 16778578;

define constant $KEY-Armenian-YECH = 16778549;

define constant $KEY-Armenian-ZA = 16778550;

define constant $KEY-Armenian-ZHE = 16778554;

define constant $KEY-Armenian-accent = 16778587;

define constant $KEY-Armenian-amanak = 16778588;

define constant $KEY-Armenian-apostrophe = 16778586;

define constant $KEY-Armenian-but = 16778589;

define constant $KEY-Armenian-exclam = 16778588;

define constant $KEY-Armenian-full-stop = 16778633;

define constant $KEY-Armenian-hyphen = 16778634;

define constant $KEY-Armenian-ligature-ew = 16778631;

define constant $KEY-Armenian-paruyk = 16778590;

define constant $KEY-Armenian-question = 16778590;

define constant $KEY-Armenian-separation-mark = 16778589;

define constant $KEY-Armenian-shesht = 16778587;

define constant $KEY-Armenian-verjaket = 16778633;

define constant $KEY-Armenian-yentamna = 16778634;

define constant $KEY-Atilde = 195;

define constant $KEY-AudibleBell-Enable = 65146;

define constant $KEY-AudioCycleTrack = 269025179;

define constant $KEY-AudioForward = 269025175;

define constant $KEY-AudioLowerVolume = 269025041;

define constant $KEY-AudioMedia = 269025074;

define constant $KEY-AudioMute = 269025042;

define constant $KEY-AudioNext = 269025047;

define constant $KEY-AudioPause = 269025073;

define constant $KEY-AudioPlay = 269025044;

define constant $KEY-AudioPrev = 269025046;

define constant $KEY-AudioRaiseVolume = 269025043;

define constant $KEY-AudioRandomPlay = 269025177;

define constant $KEY-AudioRecord = 269025052;

define constant $KEY-AudioRepeat = 269025176;

define constant $KEY-AudioRewind = 269025086;

define constant $KEY-AudioStop = 269025045;

define constant $KEY-Away = 269025165;

define constant $KEY-B = 66;

define constant $KEY-Babovedot = 16784898;

define constant $KEY-Back = 269025062;

define constant $KEY-BackForward = 269025087;

define constant $KEY-BackSpace = 65288;

define constant $KEY-Battery = 269025171;

define constant $KEY-Begin = 65368;

define constant $KEY-Blue = 269025190;

define constant $KEY-Bluetooth = 269025172;

define constant $KEY-Book = 269025106;

define constant $KEY-BounceKeys-Enable = 65140;

define constant $KEY-Break = 65387;

define constant $KEY-BrightnessAdjust = 269025083;

define constant $KEY-Byelorussian-SHORTU = 1726;

define constant $KEY-C = 67;

define constant $KEY-CD = 269025107;

define constant $KEY-CH = 65186;

define constant $KEY-C-H = 65189;

define constant $KEY-Cabovedot = 709;

define constant $KEY-Cacute = 454;

define constant $KEY-Calculator = 269025053;

define constant $KEY-Calendar = 269025056;

define constant $KEY-Cancel = 65385;

define constant $KEY-Caps-Lock = 65509;

define constant $KEY-Ccaron = 456;

define constant $KEY-Ccedilla = 199;

define constant $KEY-Ccircumflex = 710;

define constant $KEY-Clear = 65291;

define constant $KEY-ClearGrab = 269024801;

define constant $KEY-Close = 269025110;

define constant $KEY-Codeinput = 65335;

define constant $KEY-ColonSign = 16785569;

define constant $KEY-Community = 269025085;

define constant $KEY-ContrastAdjust = 269025058;

define constant $KEY-Control-L = 65507;

define constant $KEY-Control-R = 65508;

define constant $KEY-Copy = 269025111;

define constant $KEY-CruzeiroSign = 16785570;

define constant $KEY-Cut = 269025112;

define constant $KEY-CycleAngle = 269025180;

define constant $KEY-Cyrillic-A = 1761;

define constant $KEY-Cyrillic-BE = 1762;

define constant $KEY-Cyrillic-CHE = 1790;

define constant $KEY-Cyrillic-CHE-descender = 16778422;

define constant $KEY-Cyrillic-CHE-vertstroke = 16778424;

define constant $KEY-Cyrillic-DE = 1764;

define constant $KEY-Cyrillic-DZHE = 1727;

define constant $KEY-Cyrillic-E = 1788;

define constant $KEY-Cyrillic-EF = 1766;

define constant $KEY-Cyrillic-EL = 1772;

define constant $KEY-Cyrillic-EM = 1773;

define constant $KEY-Cyrillic-EN = 1774;

define constant $KEY-Cyrillic-EN-descender = 16778402;

define constant $KEY-Cyrillic-ER = 1778;

define constant $KEY-Cyrillic-ES = 1779;

define constant $KEY-Cyrillic-GHE = 1767;

define constant $KEY-Cyrillic-GHE-bar = 16778386;

define constant $KEY-Cyrillic-HA = 1768;

define constant $KEY-Cyrillic-HARDSIGN = 1791;

define constant $KEY-Cyrillic-HA-descender = 16778418;

define constant $KEY-Cyrillic-I = 1769;

define constant $KEY-Cyrillic-IE = 1765;

define constant $KEY-Cyrillic-IO = 1715;

define constant $KEY-Cyrillic-I-macron = 16778466;

define constant $KEY-Cyrillic-JE = 1720;

define constant $KEY-Cyrillic-KA = 1771;

define constant $KEY-Cyrillic-KA-descender = 16778394;

define constant $KEY-Cyrillic-KA-vertstroke = 16778396;

define constant $KEY-Cyrillic-LJE = 1721;

define constant $KEY-Cyrillic-NJE = 1722;

define constant $KEY-Cyrillic-O = 1775;

define constant $KEY-Cyrillic-O-bar = 16778472;

define constant $KEY-Cyrillic-PE = 1776;

define constant $KEY-Cyrillic-SCHWA = 16778456;

define constant $KEY-Cyrillic-SHA = 1787;

define constant $KEY-Cyrillic-SHCHA = 1789;

define constant $KEY-Cyrillic-SHHA = 16778426;

define constant $KEY-Cyrillic-SHORTI = 1770;

define constant $KEY-Cyrillic-SOFTSIGN = 1784;

define constant $KEY-Cyrillic-TE = 1780;

define constant $KEY-Cyrillic-TSE = 1763;

define constant $KEY-Cyrillic-U = 1781;

define constant $KEY-Cyrillic-U-macron = 16778478;

define constant $KEY-Cyrillic-U-straight = 16778414;

define constant $KEY-Cyrillic-U-straight-bar = 16778416;

define constant $KEY-Cyrillic-VE = 1783;

define constant $KEY-Cyrillic-YA = 1777;

define constant $KEY-Cyrillic-YERU = 1785;

define constant $KEY-Cyrillic-YU = 1760;

define constant $KEY-Cyrillic-ZE = 1786;

define constant $KEY-Cyrillic-ZHE = 1782;

define constant $KEY-Cyrillic-ZHE-descender = 16778390;

define constant $KEY-D = 68;

define constant $KEY-DOS = 269025114;

define constant $KEY-Dabovedot = 16784906;

define constant $KEY-Dcaron = 463;

define constant $KEY-Delete = 65535;

define constant $KEY-Display = 269025113;

define constant $KEY-Documents = 269025115;

define constant $KEY-DongSign = 16785579;

define constant $KEY-Down = 65364;

define constant $KEY-Dstroke = 464;

define constant $KEY-E = 69;

define constant $KEY-ENG = 957;

define constant $KEY-ETH = 208;

define constant $KEY-EZH = 16777655;

define constant $KEY-Eabovedot = 972;

define constant $KEY-Eacute = 201;

define constant $KEY-Ebelowdot = 16785080;

define constant $KEY-Ecaron = 460;

define constant $KEY-Ecircumflex = 202;

define constant $KEY-Ecircumflexacute = 16785086;

define constant $KEY-Ecircumflexbelowdot = 16785094;

define constant $KEY-Ecircumflexgrave = 16785088;

define constant $KEY-Ecircumflexhook = 16785090;

define constant $KEY-Ecircumflextilde = 16785092;

define constant $KEY-EcuSign = 16785568;

define constant $KEY-Ediaeresis = 203;

define constant $KEY-Egrave = 200;

define constant $KEY-Ehook = 16785082;

define constant $KEY-Eisu-Shift = 65327;

define constant $KEY-Eisu-toggle = 65328;

define constant $KEY-Eject = 269025068;

define constant $KEY-Emacron = 938;

define constant $KEY-End = 65367;

define constant $KEY-Eogonek = 458;

define constant $KEY-Escape = 65307;

define constant $KEY-Etilde = 16785084;

define constant $KEY-EuroSign = 8364;

define constant $KEY-Excel = 269025116;

define constant $KEY-Execute = 65378;

define constant $KEY-Explorer = 269025117;

define constant $KEY-F = 70;

define constant $KEY-F1 = 65470;

define constant $KEY-F10 = 65479;

define constant $KEY-F11 = 65480;

define constant $KEY-F12 = 65481;

define constant $KEY-F13 = 65482;

define constant $KEY-F14 = 65483;

define constant $KEY-F15 = 65484;

define constant $KEY-F16 = 65485;

define constant $KEY-F17 = 65486;

define constant $KEY-F18 = 65487;

define constant $KEY-F19 = 65488;

define constant $KEY-F2 = 65471;

define constant $KEY-F20 = 65489;

define constant $KEY-F21 = 65490;

define constant $KEY-F22 = 65491;

define constant $KEY-F23 = 65492;

define constant $KEY-F24 = 65493;

define constant $KEY-F25 = 65494;

define constant $KEY-F26 = 65495;

define constant $KEY-F27 = 65496;

define constant $KEY-F28 = 65497;

define constant $KEY-F29 = 65498;

define constant $KEY-F3 = 65472;

define constant $KEY-F30 = 65499;

define constant $KEY-F31 = 65500;

define constant $KEY-F32 = 65501;

define constant $KEY-F33 = 65502;

define constant $KEY-F34 = 65503;

define constant $KEY-F35 = 65504;

define constant $KEY-F4 = 65473;

define constant $KEY-F5 = 65474;

define constant $KEY-F6 = 65475;

define constant $KEY-F7 = 65476;

define constant $KEY-F8 = 65477;

define constant $KEY-F9 = 65478;

define constant $KEY-FFrancSign = 16785571;

define constant $KEY-Fabovedot = 16784926;

define constant $KEY-Farsi-0 = 16778992;

define constant $KEY-Farsi-1 = 16778993;

define constant $KEY-Farsi-2 = 16778994;

define constant $KEY-Farsi-3 = 16778995;

define constant $KEY-Farsi-4 = 16778996;

define constant $KEY-Farsi-5 = 16778997;

define constant $KEY-Farsi-6 = 16778998;

define constant $KEY-Farsi-7 = 16778999;

define constant $KEY-Farsi-8 = 16779000;

define constant $KEY-Farsi-9 = 16779001;

define constant $KEY-Farsi-yeh = 16778956;

define constant $KEY-Favorites = 269025072;

define constant $KEY-Finance = 269025084;

define constant $KEY-Find = 65384;

define constant $KEY-First-Virtual-Screen = 65232;

define constant $KEY-Forward = 269025063;

define constant $KEY-FrameBack = 269025181;

define constant $KEY-FrameForward = 269025182;

define constant $KEY-G = 71;

define constant $KEY-Gabovedot = 725;

define constant $KEY-Game = 269025118;

define constant $KEY-Gbreve = 683;

define constant $KEY-Gcaron = 16777702;

define constant $KEY-Gcedilla = 939;

define constant $KEY-Gcircumflex = 728;

define constant $KEY-Georgian-an = 16781520;

define constant $KEY-Georgian-ban = 16781521;

define constant $KEY-Georgian-can = 16781546;

define constant $KEY-Georgian-char = 16781549;

define constant $KEY-Georgian-chin = 16781545;

define constant $KEY-Georgian-cil = 16781548;

define constant $KEY-Georgian-don = 16781523;

define constant $KEY-Georgian-en = 16781524;

define constant $KEY-Georgian-fi = 16781558;

define constant $KEY-Georgian-gan = 16781522;

define constant $KEY-Georgian-ghan = 16781542;

define constant $KEY-Georgian-hae = 16781552;

define constant $KEY-Georgian-har = 16781556;

define constant $KEY-Georgian-he = 16781553;

define constant $KEY-Georgian-hie = 16781554;

define constant $KEY-Georgian-hoe = 16781557;

define constant $KEY-Georgian-in = 16781528;

define constant $KEY-Georgian-jhan = 16781551;

define constant $KEY-Georgian-jil = 16781547;

define constant $KEY-Georgian-kan = 16781529;

define constant $KEY-Georgian-khar = 16781541;

define constant $KEY-Georgian-las = 16781530;

define constant $KEY-Georgian-man = 16781531;

define constant $KEY-Georgian-nar = 16781532;

define constant $KEY-Georgian-on = 16781533;

define constant $KEY-Georgian-par = 16781534;

define constant $KEY-Georgian-phar = 16781540;

define constant $KEY-Georgian-qar = 16781543;

define constant $KEY-Georgian-rae = 16781536;

define constant $KEY-Georgian-san = 16781537;

define constant $KEY-Georgian-shin = 16781544;

define constant $KEY-Georgian-tan = 16781527;

define constant $KEY-Georgian-tar = 16781538;

define constant $KEY-Georgian-un = 16781539;

define constant $KEY-Georgian-vin = 16781525;

define constant $KEY-Georgian-we = 16781555;

define constant $KEY-Georgian-xan = 16781550;

define constant $KEY-Georgian-zen = 16781526;

define constant $KEY-Georgian-zhar = 16781535;

define constant $KEY-Go = 269025119;

define constant $KEY-Greek-ALPHA = 1985;

define constant $KEY-Greek-ALPHAaccent = 1953;

define constant $KEY-Greek-BETA = 1986;

define constant $KEY-Greek-CHI = 2007;

define constant $KEY-Greek-DELTA = 1988;

define constant $KEY-Greek-EPSILON = 1989;

define constant $KEY-Greek-EPSILONaccent = 1954;

define constant $KEY-Greek-ETA = 1991;

define constant $KEY-Greek-ETAaccent = 1955;

define constant $KEY-Greek-GAMMA = 1987;

define constant $KEY-Greek-IOTA = 1993;

define constant $KEY-Greek-IOTAaccent = 1956;

define constant $KEY-Greek-IOTAdiaeresis = 1957;

define constant $KEY-Greek-IOTAdieresis = 1957;

define constant $KEY-Greek-KAPPA = 1994;

define constant $KEY-Greek-LAMBDA = 1995;

define constant $KEY-Greek-LAMDA = 1995;

define constant $KEY-Greek-MU = 1996;

define constant $KEY-Greek-NU = 1997;

define constant $KEY-Greek-OMEGA = 2009;

define constant $KEY-Greek-OMEGAaccent = 1963;

define constant $KEY-Greek-OMICRON = 1999;

define constant $KEY-Greek-OMICRONaccent = 1959;

define constant $KEY-Greek-PHI = 2006;

define constant $KEY-Greek-PI = 2000;

define constant $KEY-Greek-PSI = 2008;

define constant $KEY-Greek-RHO = 2001;

define constant $KEY-Greek-SIGMA = 2002;

define constant $KEY-Greek-TAU = 2004;

define constant $KEY-Greek-THETA = 1992;

define constant $KEY-Greek-UPSILON = 2005;

define constant $KEY-Greek-UPSILONaccent = 1960;

define constant $KEY-Greek-UPSILONdieresis = 1961;

define constant $KEY-Greek-XI = 1998;

define constant $KEY-Greek-ZETA = 1990;

define constant $KEY-Greek-accentdieresis = 1966;

define constant $KEY-Greek-finalsmallsigma = 2035;

define constant $KEY-Greek-horizbar = 1967;

define constant $KEY-Greek-iotaaccentdieresis = 1974;

define constant $KEY-Greek-switch = 65406;

define constant $KEY-Greek-upsilonaccentdieresis = 1978;

define constant $KEY-Green = 269025188;

define constant $KEY-H = 72;

define constant $KEY-Hangul = 65329;

define constant $KEY-Hangul-A = 3775;

define constant $KEY-Hangul-AE = 3776;

define constant $KEY-Hangul-AraeA = 3830;

define constant $KEY-Hangul-AraeAE = 3831;

define constant $KEY-Hangul-Banja = 65337;

define constant $KEY-Hangul-Cieuc = 3770;

define constant $KEY-Hangul-Codeinput = 65335;

define constant $KEY-Hangul-Dikeud = 3751;

define constant $KEY-Hangul-E = 3780;

define constant $KEY-Hangul-EO = 3779;

define constant $KEY-Hangul-EU = 3793;

define constant $KEY-Hangul-End = 65331;

define constant $KEY-Hangul-Hanja = 65332;

define constant $KEY-Hangul-Hieuh = 3774;

define constant $KEY-Hangul-I = 3795;

define constant $KEY-Hangul-Ieung = 3767;

define constant $KEY-Hangul-J-Cieuc = 3818;

define constant $KEY-Hangul-J-Dikeud = 3802;

define constant $KEY-Hangul-J-Hieuh = 3822;

define constant $KEY-Hangul-J-Ieung = 3816;

define constant $KEY-Hangul-J-Jieuj = 3817;

define constant $KEY-Hangul-J-Khieuq = 3819;

define constant $KEY-Hangul-J-Kiyeog = 3796;

define constant $KEY-Hangul-J-KiyeogSios = 3798;

define constant $KEY-Hangul-J-KkogjiDalrinIeung = 3833;

define constant $KEY-Hangul-J-Mieum = 3811;

define constant $KEY-Hangul-J-Nieun = 3799;

define constant $KEY-Hangul-J-NieunHieuh = 3801;

define constant $KEY-Hangul-J-NieunJieuj = 3800;

define constant $KEY-Hangul-J-PanSios = 3832;

define constant $KEY-Hangul-J-Phieuf = 3821;

define constant $KEY-Hangul-J-Pieub = 3812;

define constant $KEY-Hangul-J-PieubSios = 3813;

define constant $KEY-Hangul-J-Rieul = 3803;

define constant $KEY-Hangul-J-RieulHieuh = 3810;

define constant $KEY-Hangul-J-RieulKiyeog = 3804;

define constant $KEY-Hangul-J-RieulMieum = 3805;

define constant $KEY-Hangul-J-RieulPhieuf = 3809;

define constant $KEY-Hangul-J-RieulPieub = 3806;

define constant $KEY-Hangul-J-RieulSios = 3807;

define constant $KEY-Hangul-J-RieulTieut = 3808;

define constant $KEY-Hangul-J-Sios = 3814;

define constant $KEY-Hangul-J-SsangKiyeog = 3797;

define constant $KEY-Hangul-J-SsangSios = 3815;

define constant $KEY-Hangul-J-Tieut = 3820;

define constant $KEY-Hangul-J-YeorinHieuh = 3834;

define constant $KEY-Hangul-Jamo = 65333;

define constant $KEY-Hangul-Jeonja = 65336;

define constant $KEY-Hangul-Jieuj = 3768;

define constant $KEY-Hangul-Khieuq = 3771;

define constant $KEY-Hangul-Kiyeog = 3745;

define constant $KEY-Hangul-KiyeogSios = 3747;

define constant $KEY-Hangul-KkogjiDalrinIeung = 3827;

define constant $KEY-Hangul-Mieum = 3761;

define constant $KEY-Hangul-MultipleCandidate = 65341;

define constant $KEY-Hangul-Nieun = 3748;

define constant $KEY-Hangul-NieunHieuh = 3750;

define constant $KEY-Hangul-NieunJieuj = 3749;

define constant $KEY-Hangul-O = 3783;

define constant $KEY-Hangul-OE = 3786;

define constant $KEY-Hangul-PanSios = 3826;

define constant $KEY-Hangul-Phieuf = 3773;

define constant $KEY-Hangul-Pieub = 3762;

define constant $KEY-Hangul-PieubSios = 3764;

define constant $KEY-Hangul-PostHanja = 65339;

define constant $KEY-Hangul-PreHanja = 65338;

define constant $KEY-Hangul-PreviousCandidate = 65342;

define constant $KEY-Hangul-Rieul = 3753;

define constant $KEY-Hangul-RieulHieuh = 3760;

define constant $KEY-Hangul-RieulKiyeog = 3754;

define constant $KEY-Hangul-RieulMieum = 3755;

define constant $KEY-Hangul-RieulPhieuf = 3759;

define constant $KEY-Hangul-RieulPieub = 3756;

define constant $KEY-Hangul-RieulSios = 3757;

define constant $KEY-Hangul-RieulTieut = 3758;

define constant $KEY-Hangul-RieulYeorinHieuh = 3823;

define constant $KEY-Hangul-Romaja = 65334;

define constant $KEY-Hangul-SingleCandidate = 65340;

define constant $KEY-Hangul-Sios = 3765;

define constant $KEY-Hangul-Special = 65343;

define constant $KEY-Hangul-SsangDikeud = 3752;

define constant $KEY-Hangul-SsangJieuj = 3769;

define constant $KEY-Hangul-SsangKiyeog = 3746;

define constant $KEY-Hangul-SsangPieub = 3763;

define constant $KEY-Hangul-SsangSios = 3766;

define constant $KEY-Hangul-Start = 65330;

define constant $KEY-Hangul-SunkyeongeumMieum = 3824;

define constant $KEY-Hangul-SunkyeongeumPhieuf = 3828;

define constant $KEY-Hangul-SunkyeongeumPieub = 3825;

define constant $KEY-Hangul-Tieut = 3772;

define constant $KEY-Hangul-U = 3788;

define constant $KEY-Hangul-WA = 3784;

define constant $KEY-Hangul-WAE = 3785;

define constant $KEY-Hangul-WE = 3790;

define constant $KEY-Hangul-WEO = 3789;

define constant $KEY-Hangul-WI = 3791;

define constant $KEY-Hangul-YA = 3777;

define constant $KEY-Hangul-YAE = 3778;

define constant $KEY-Hangul-YE = 3782;

define constant $KEY-Hangul-YEO = 3781;

define constant $KEY-Hangul-YI = 3794;

define constant $KEY-Hangul-YO = 3787;

define constant $KEY-Hangul-YU = 3792;

define constant $KEY-Hangul-YeorinHieuh = 3829;

define constant $KEY-Hangul-switch = 65406;

define constant $KEY-Hankaku = 65321;

define constant $KEY-Hcircumflex = 678;

define constant $KEY-Hebrew-switch = 65406;

define constant $KEY-Help = 65386;

define constant $KEY-Henkan = 65315;

define constant $KEY-Henkan-Mode = 65315;

define constant $KEY-Hibernate = 269025192;

define constant $KEY-Hiragana = 65317;

define constant $KEY-Hiragana-Katakana = 65319;

define constant $KEY-History = 269025079;

define constant $KEY-Home = 65360;

define constant $KEY-HomePage = 269025048;

define constant $KEY-HotLinks = 269025082;

define constant $KEY-Hstroke = 673;

define constant $KEY-Hyper-L = 65517;

define constant $KEY-Hyper-R = 65518;

define constant $KEY-I = 73;

define constant $KEY-ISO-Center-Object = 65075;

define constant $KEY-ISO-Continuous-Underline = 65072;

define constant $KEY-ISO-Discontinuous-Underline = 65073;

define constant $KEY-ISO-Emphasize = 65074;

define constant $KEY-ISO-Enter = 65076;

define constant $KEY-ISO-Fast-Cursor-Down = 65071;

define constant $KEY-ISO-Fast-Cursor-Left = 65068;

define constant $KEY-ISO-Fast-Cursor-Right = 65069;

define constant $KEY-ISO-Fast-Cursor-Up = 65070;

define constant $KEY-ISO-First-Group = 65036;

define constant $KEY-ISO-First-Group-Lock = 65037;

define constant $KEY-ISO-Group-Latch = 65030;

define constant $KEY-ISO-Group-Lock = 65031;

define constant $KEY-ISO-Group-Shift = 65406;

define constant $KEY-ISO-Last-Group = 65038;

define constant $KEY-ISO-Last-Group-Lock = 65039;

define constant $KEY-ISO-Left-Tab = 65056;

define constant $KEY-ISO-Level2-Latch = 65026;

define constant $KEY-ISO-Level3-Latch = 65028;

define constant $KEY-ISO-Level3-Lock = 65029;

define constant $KEY-ISO-Level3-Shift = 65027;

define constant $KEY-ISO-Level5-Latch = 65042;

define constant $KEY-ISO-Level5-Lock = 65043;

define constant $KEY-ISO-Level5-Shift = 65041;

define constant $KEY-ISO-Lock = 65025;

define constant $KEY-ISO-Move-Line-Down = 65058;

define constant $KEY-ISO-Move-Line-Up = 65057;

define constant $KEY-ISO-Next-Group = 65032;

define constant $KEY-ISO-Next-Group-Lock = 65033;

define constant $KEY-ISO-Partial-Line-Down = 65060;

define constant $KEY-ISO-Partial-Line-Up = 65059;

define constant $KEY-ISO-Partial-Space-Left = 65061;

define constant $KEY-ISO-Partial-Space-Right = 65062;

define constant $KEY-ISO-Prev-Group = 65034;

define constant $KEY-ISO-Prev-Group-Lock = 65035;

define constant $KEY-ISO-Release-Both-Margins = 65067;

define constant $KEY-ISO-Release-Margin-Left = 65065;

define constant $KEY-ISO-Release-Margin-Right = 65066;

define constant $KEY-ISO-Set-Margin-Left = 65063;

define constant $KEY-ISO-Set-Margin-Right = 65064;

define constant $KEY-Iabovedot = 681;

define constant $KEY-Iacute = 205;

define constant $KEY-Ibelowdot = 16785098;

define constant $KEY-Ibreve = 16777516;

define constant $KEY-Icircumflex = 206;

define constant $KEY-Idiaeresis = 207;

define constant $KEY-Igrave = 204;

define constant $KEY-Ihook = 16785096;

define constant $KEY-Imacron = 975;

define constant $KEY-Insert = 65379;

define constant $KEY-Iogonek = 967;

define constant $KEY-Itilde = 933;

define constant $KEY-J = 74;

define constant $KEY-Jcircumflex = 684;

define constant $KEY-K = 75;

define constant $KEY-KP-0 = 65456;

define constant $KEY-KP-1 = 65457;

define constant $KEY-KP-2 = 65458;

define constant $KEY-KP-3 = 65459;

define constant $KEY-KP-4 = 65460;

define constant $KEY-KP-5 = 65461;

define constant $KEY-KP-6 = 65462;

define constant $KEY-KP-7 = 65463;

define constant $KEY-KP-8 = 65464;

define constant $KEY-KP-9 = 65465;

define constant $KEY-KP-Add = 65451;

define constant $KEY-KP-Begin = 65437;

define constant $KEY-KP-Decimal = 65454;

define constant $KEY-KP-Delete = 65439;

define constant $KEY-KP-Divide = 65455;

define constant $KEY-KP-Down = 65433;

define constant $KEY-KP-End = 65436;

define constant $KEY-KP-Enter = 65421;

define constant $KEY-KP-Equal = 65469;

define constant $KEY-KP-F1 = 65425;

define constant $KEY-KP-F2 = 65426;

define constant $KEY-KP-F3 = 65427;

define constant $KEY-KP-F4 = 65428;

define constant $KEY-KP-Home = 65429;

define constant $KEY-KP-Insert = 65438;

define constant $KEY-KP-Left = 65430;

define constant $KEY-KP-Multiply = 65450;

define constant $KEY-KP-Next = 65435;

define constant $KEY-KP-Page-Down = 65435;

define constant $KEY-KP-Page-Up = 65434;

define constant $KEY-KP-Prior = 65434;

define constant $KEY-KP-Right = 65432;

define constant $KEY-KP-Separator = 65452;

define constant $KEY-KP-Space = 65408;

define constant $KEY-KP-Subtract = 65453;

define constant $KEY-KP-Tab = 65417;

define constant $KEY-KP-Up = 65431;

define constant $KEY-Kana-Lock = 65325;

define constant $KEY-Kana-Shift = 65326;

define constant $KEY-Kanji = 65313;

define constant $KEY-Kanji-Bangou = 65335;

define constant $KEY-Katakana = 65318;

define constant $KEY-KbdBrightnessDown = 269025030;

define constant $KEY-KbdBrightnessUp = 269025029;

define constant $KEY-KbdLightOnOff = 269025028;

define constant $KEY-Kcedilla = 979;

define constant $KEY-Korean-Won = 3839;

define constant $KEY-L = 76;

define constant $KEY-L1 = 65480;

define constant $KEY-L10 = 65489;

define constant $KEY-L2 = 65481;

define constant $KEY-L3 = 65482;

define constant $KEY-L4 = 65483;

define constant $KEY-L5 = 65484;

define constant $KEY-L6 = 65485;

define constant $KEY-L7 = 65486;

define constant $KEY-L8 = 65487;

define constant $KEY-L9 = 65488;

define constant $KEY-Lacute = 453;

define constant $KEY-Last-Virtual-Screen = 65236;

define constant $KEY-Launch0 = 269025088;

define constant $KEY-Launch1 = 269025089;

define constant $KEY-Launch2 = 269025090;

define constant $KEY-Launch3 = 269025091;

define constant $KEY-Launch4 = 269025092;

define constant $KEY-Launch5 = 269025093;

define constant $KEY-Launch6 = 269025094;

define constant $KEY-Launch7 = 269025095;

define constant $KEY-Launch8 = 269025096;

define constant $KEY-Launch9 = 269025097;

define constant $KEY-LaunchA = 269025098;

define constant $KEY-LaunchB = 269025099;

define constant $KEY-LaunchC = 269025100;

define constant $KEY-LaunchD = 269025101;

define constant $KEY-LaunchE = 269025102;

define constant $KEY-LaunchF = 269025103;

define constant $KEY-Lbelowdot = 16784950;

define constant $KEY-Lcaron = 421;

define constant $KEY-Lcedilla = 934;

define constant $KEY-Left = 65361;

define constant $KEY-LightBulb = 269025077;

define constant $KEY-Linefeed = 65290;

define constant $KEY-LiraSign = 16785572;

define constant $KEY-LogGrabInfo = 269024805;

define constant $KEY-LogOff = 269025121;

define constant $KEY-LogWindowTree = 269024804;

define constant $KEY-Lstroke = 419;

define constant $KEY-M = 77;

define constant $KEY-Mabovedot = 16784960;

define constant $KEY-Macedonia-DSE = 1717;

define constant $KEY-Macedonia-GJE = 1714;

define constant $KEY-Macedonia-KJE = 1724;

define constant $KEY-Mae-Koho = 65342;

define constant $KEY-Mail = 269025049;

define constant $KEY-MailForward = 269025168;

define constant $KEY-Market = 269025122;

define constant $KEY-Massyo = 65324;

define constant $KEY-Meeting = 269025123;

define constant $KEY-Memo = 269025054;

define constant $KEY-Menu = 65383;

define constant $KEY-MenuKB = 269025125;

define constant $KEY-MenuPB = 269025126;

define constant $KEY-Messenger = 269025166;

define constant $KEY-Meta-L = 65511;

define constant $KEY-Meta-R = 65512;

define constant $KEY-MillSign = 16785573;

define constant $KEY-ModeLock = 269025025;

define constant $KEY-Mode-switch = 65406;

define constant $KEY-MonBrightnessDown = 269025027;

define constant $KEY-MonBrightnessUp = 269025026;

define constant $KEY-MouseKeys-Accel-Enable = 65143;

define constant $KEY-MouseKeys-Enable = 65142;

define constant $KEY-Muhenkan = 65314;

define constant $KEY-Multi-key = 65312;

define constant $KEY-MultipleCandidate = 65341;

define constant $KEY-Music = 269025170;

define constant $KEY-MyComputer = 269025075;

define constant $KEY-MySites = 269025127;

define constant $KEY-N = 78;

define constant $KEY-Nacute = 465;

define constant $KEY-NairaSign = 16785574;

define constant $KEY-Ncaron = 466;

define constant $KEY-Ncedilla = 977;

define constant $KEY-New = 269025128;

define constant $KEY-NewSheqelSign = 16785578;

define constant $KEY-News = 269025129;

define constant $KEY-Next = 65366;

define constant $KEY-Next-VMode = 269024802;

define constant $KEY-Next-Virtual-Screen = 65234;

define constant $KEY-Ntilde = 209;

define constant $KEY-Num-Lock = 65407;

define constant $KEY-O = 79;

define constant $KEY-OE = 5052;

define constant $KEY-Oacute = 211;

define constant $KEY-Obarred = 16777631;

define constant $KEY-Obelowdot = 16785100;

define constant $KEY-Ocaron = 16777681;

define constant $KEY-Ocircumflex = 212;

define constant $KEY-Ocircumflexacute = 16785104;

define constant $KEY-Ocircumflexbelowdot = 16785112;

define constant $KEY-Ocircumflexgrave = 16785106;

define constant $KEY-Ocircumflexhook = 16785108;

define constant $KEY-Ocircumflextilde = 16785110;

define constant $KEY-Odiaeresis = 214;

define constant $KEY-Odoubleacute = 469;

define constant $KEY-OfficeHome = 269025130;

define constant $KEY-Ograve = 210;

define constant $KEY-Ohook = 16785102;

define constant $KEY-Ohorn = 16777632;

define constant $KEY-Ohornacute = 16785114;

define constant $KEY-Ohornbelowdot = 16785122;

define constant $KEY-Ohorngrave = 16785116;

define constant $KEY-Ohornhook = 16785118;

define constant $KEY-Ohorntilde = 16785120;

define constant $KEY-Omacron = 978;

define constant $KEY-Ooblique = 216;

define constant $KEY-Open = 269025131;

define constant $KEY-OpenURL = 269025080;

define constant $KEY-Option = 269025132;

define constant $KEY-Oslash = 216;

define constant $KEY-Otilde = 213;

define constant $KEY-Overlay1-Enable = 65144;

define constant $KEY-Overlay2-Enable = 65145;

define constant $KEY-P = 80;

define constant $KEY-Pabovedot = 16784982;

define constant $KEY-Page-Down = 65366;

define constant $KEY-Page-Up = 65365;

define constant $KEY-Paste = 269025133;

define constant $KEY-Pause = 65299;

define constant $KEY-PesetaSign = 16785575;

define constant $KEY-Phone = 269025134;

define constant $KEY-Pictures = 269025169;

define constant $KEY-Pointer-Accelerate = 65274;

define constant $KEY-Pointer-Button1 = 65257;

define constant $KEY-Pointer-Button2 = 65258;

define constant $KEY-Pointer-Button3 = 65259;

define constant $KEY-Pointer-Button4 = 65260;

define constant $KEY-Pointer-Button5 = 65261;

define constant $KEY-Pointer-Button-Dflt = 65256;

define constant $KEY-Pointer-DblClick1 = 65263;

define constant $KEY-Pointer-DblClick2 = 65264;

define constant $KEY-Pointer-DblClick3 = 65265;

define constant $KEY-Pointer-DblClick4 = 65266;

define constant $KEY-Pointer-DblClick5 = 65267;

define constant $KEY-Pointer-DblClick-Dflt = 65262;

define constant $KEY-Pointer-DfltBtnNext = 65275;

define constant $KEY-Pointer-DfltBtnPrev = 65276;

define constant $KEY-Pointer-Down = 65251;

define constant $KEY-Pointer-DownLeft = 65254;

define constant $KEY-Pointer-DownRight = 65255;

define constant $KEY-Pointer-Drag1 = 65269;

define constant $KEY-Pointer-Drag2 = 65270;

define constant $KEY-Pointer-Drag3 = 65271;

define constant $KEY-Pointer-Drag4 = 65272;

define constant $KEY-Pointer-Drag5 = 65277;

define constant $KEY-Pointer-Drag-Dflt = 65268;

define constant $KEY-Pointer-EnableKeys = 65273;

define constant $KEY-Pointer-Left = 65248;

define constant $KEY-Pointer-Right = 65249;

define constant $KEY-Pointer-Up = 65250;

define constant $KEY-Pointer-UpLeft = 65252;

define constant $KEY-Pointer-UpRight = 65253;

define constant $KEY-PowerDown = 269025057;

define constant $KEY-PowerOff = 269025066;

define constant $KEY-Prev-VMode = 269024803;

define constant $KEY-Prev-Virtual-Screen = 65233;

define constant $KEY-PreviousCandidate = 65342;

define constant $KEY-Print = 65377;

define constant $KEY-Prior = 65365;

define constant $KEY-Q = 81;

define constant $KEY-R = 82;

define constant $KEY-R1 = 65490;

define constant $KEY-R10 = 65499;

define constant $KEY-R11 = 65500;

define constant $KEY-R12 = 65501;

define constant $KEY-R13 = 65502;

define constant $KEY-R14 = 65503;

define constant $KEY-R15 = 65504;

define constant $KEY-R2 = 65491;

define constant $KEY-R3 = 65492;

define constant $KEY-R4 = 65493;

define constant $KEY-R5 = 65494;

define constant $KEY-R6 = 65495;

define constant $KEY-R7 = 65496;

define constant $KEY-R8 = 65497;

define constant $KEY-R9 = 65498;

define constant $KEY-Racute = 448;

define constant $KEY-Rcaron = 472;

define constant $KEY-Rcedilla = 931;

define constant $KEY-Red = 269025187;

define constant $KEY-Redo = 65382;

define constant $KEY-Refresh = 269025065;

define constant $KEY-Reload = 269025139;

define constant $KEY-RepeatKeys-Enable = 65138;

define constant $KEY-Reply = 269025138;

define constant $KEY-Return = 65293;

define constant $KEY-Right = 65363;

define constant $KEY-RockerDown = 269025060;

define constant $KEY-RockerEnter = 269025061;

define constant $KEY-RockerUp = 269025059;

define constant $KEY-Romaji = 65316;

define constant $KEY-RotateWindows = 269025140;

define constant $KEY-RotationKB = 269025142;

define constant $KEY-RotationPB = 269025141;

define constant $KEY-RupeeSign = 16785576;

define constant $KEY-S = 83;

define constant $KEY-SCHWA = 16777615;

define constant $KEY-Sabovedot = 16784992;

define constant $KEY-Sacute = 422;

define constant $KEY-Save = 269025143;

define constant $KEY-Scaron = 425;

define constant $KEY-Scedilla = 426;

define constant $KEY-Scircumflex = 734;

define constant $KEY-ScreenSaver = 269025069;

define constant $KEY-ScrollClick = 269025146;

define constant $KEY-ScrollDown = 269025145;

define constant $KEY-ScrollUp = 269025144;

define constant $KEY-Scroll-Lock = 65300;

define constant $KEY-Search = 269025051;

define constant $KEY-Select = 65376;

define constant $KEY-SelectButton = 269025184;

define constant $KEY-Send = 269025147;

define constant $KEY-Serbian-DJE = 1713;

define constant $KEY-Serbian-DZE = 1727;

define constant $KEY-Serbian-JE = 1720;

define constant $KEY-Serbian-LJE = 1721;

define constant $KEY-Serbian-NJE = 1722;

define constant $KEY-Serbian-TSHE = 1723;

define constant $KEY-Shift-L = 65505;

define constant $KEY-Shift-Lock = 65510;

define constant $KEY-Shift-R = 65506;

define constant $KEY-Shop = 269025078;

define constant $KEY-SingleCandidate = 65340;

define constant $KEY-Sinh-a = 16780677;

define constant $KEY-Sinh-aa = 16780678;

define constant $KEY-Sinh-aa2 = 16780751;

define constant $KEY-Sinh-ae = 16780679;

define constant $KEY-Sinh-ae2 = 16780752;

define constant $KEY-Sinh-aee = 16780680;

define constant $KEY-Sinh-aee2 = 16780753;

define constant $KEY-Sinh-ai = 16780691;

define constant $KEY-Sinh-ai2 = 16780763;

define constant $KEY-Sinh-al = 16780746;

define constant $KEY-Sinh-au = 16780694;

define constant $KEY-Sinh-au2 = 16780766;

define constant $KEY-Sinh-ba = 16780726;

define constant $KEY-Sinh-bha = 16780727;

define constant $KEY-Sinh-ca = 16780704;

define constant $KEY-Sinh-cha = 16780705;

define constant $KEY-Sinh-dda = 16780713;

define constant $KEY-Sinh-ddha = 16780714;

define constant $KEY-Sinh-dha = 16780719;

define constant $KEY-Sinh-dhha = 16780720;

define constant $KEY-Sinh-e = 16780689;

define constant $KEY-Sinh-e2 = 16780761;

define constant $KEY-Sinh-ee = 16780690;

define constant $KEY-Sinh-ee2 = 16780762;

define constant $KEY-Sinh-fa = 16780742;

define constant $KEY-Sinh-ga = 16780700;

define constant $KEY-Sinh-gha = 16780701;

define constant $KEY-Sinh-h2 = 16780675;

define constant $KEY-Sinh-ha = 16780740;

define constant $KEY-Sinh-i = 16780681;

define constant $KEY-Sinh-i2 = 16780754;

define constant $KEY-Sinh-ii = 16780682;

define constant $KEY-Sinh-ii2 = 16780755;

define constant $KEY-Sinh-ja = 16780706;

define constant $KEY-Sinh-jha = 16780707;

define constant $KEY-Sinh-jnya = 16780709;

define constant $KEY-Sinh-ka = 16780698;

define constant $KEY-Sinh-kha = 16780699;

define constant $KEY-Sinh-kunddaliya = 16780788;

define constant $KEY-Sinh-la = 16780733;

define constant $KEY-Sinh-lla = 16780741;

define constant $KEY-Sinh-lu = 16780687;

define constant $KEY-Sinh-lu2 = 16780767;

define constant $KEY-Sinh-luu = 16780688;

define constant $KEY-Sinh-luu2 = 16780787;

define constant $KEY-Sinh-ma = 16780728;

define constant $KEY-Sinh-mba = 16780729;

define constant $KEY-Sinh-na = 16780721;

define constant $KEY-Sinh-ndda = 16780716;

define constant $KEY-Sinh-ndha = 16780723;

define constant $KEY-Sinh-ng = 16780674;

define constant $KEY-Sinh-ng2 = 16780702;

define constant $KEY-Sinh-nga = 16780703;

define constant $KEY-Sinh-nja = 16780710;

define constant $KEY-Sinh-nna = 16780715;

define constant $KEY-Sinh-nya = 16780708;

define constant $KEY-Sinh-o = 16780692;

define constant $KEY-Sinh-o2 = 16780764;

define constant $KEY-Sinh-oo = 16780693;

define constant $KEY-Sinh-oo2 = 16780765;

define constant $KEY-Sinh-pa = 16780724;

define constant $KEY-Sinh-pha = 16780725;

define constant $KEY-Sinh-ra = 16780731;

define constant $KEY-Sinh-ri = 16780685;

define constant $KEY-Sinh-rii = 16780686;

define constant $KEY-Sinh-ru2 = 16780760;

define constant $KEY-Sinh-ruu2 = 16780786;

define constant $KEY-Sinh-sa = 16780739;

define constant $KEY-Sinh-sha = 16780737;

define constant $KEY-Sinh-ssha = 16780738;

define constant $KEY-Sinh-tha = 16780717;

define constant $KEY-Sinh-thha = 16780718;

define constant $KEY-Sinh-tta = 16780711;

define constant $KEY-Sinh-ttha = 16780712;

define constant $KEY-Sinh-u = 16780683;

define constant $KEY-Sinh-u2 = 16780756;

define constant $KEY-Sinh-uu = 16780684;

define constant $KEY-Sinh-uu2 = 16780758;

define constant $KEY-Sinh-va = 16780736;

define constant $KEY-Sinh-ya = 16780730;

define constant $KEY-Sleep = 269025071;

define constant $KEY-SlowKeys-Enable = 65139;

define constant $KEY-Spell = 269025148;

define constant $KEY-SplitScreen = 269025149;

define constant $KEY-Standby = 269025040;

define constant $KEY-Start = 269025050;

define constant $KEY-StickyKeys-Enable = 65141;

define constant $KEY-Stop = 269025064;

define constant $KEY-Subtitle = 269025178;

define constant $KEY-Super-L = 65515;

define constant $KEY-Super-R = 65516;

define constant $KEY-Support = 269025150;

define constant $KEY-Suspend = 269025191;

define constant $KEY-Switch-VT-1 = 269024769;

define constant $KEY-Switch-VT-10 = 269024778;

define constant $KEY-Switch-VT-11 = 269024779;

define constant $KEY-Switch-VT-12 = 269024780;

define constant $KEY-Switch-VT-2 = 269024770;

define constant $KEY-Switch-VT-3 = 269024771;

define constant $KEY-Switch-VT-4 = 269024772;

define constant $KEY-Switch-VT-5 = 269024773;

define constant $KEY-Switch-VT-6 = 269024774;

define constant $KEY-Switch-VT-7 = 269024775;

define constant $KEY-Switch-VT-8 = 269024776;

define constant $KEY-Switch-VT-9 = 269024777;

define constant $KEY-Sys-Req = 65301;

define constant $KEY-T = 84;

define constant $KEY-THORN = 222;

define constant $KEY-Tab = 65289;

define constant $KEY-Tabovedot = 16785002;

define constant $KEY-TaskPane = 269025151;

define constant $KEY-Tcaron = 427;

define constant $KEY-Tcedilla = 478;

define constant $KEY-Terminal = 269025152;

define constant $KEY-Terminate-Server = 65237;

define constant $KEY-Thai-baht = 3551;

define constant $KEY-Thai-bobaimai = 3514;

define constant $KEY-Thai-chochan = 3496;

define constant $KEY-Thai-chochang = 3498;

define constant $KEY-Thai-choching = 3497;

define constant $KEY-Thai-chochoe = 3500;

define constant $KEY-Thai-dochada = 3502;

define constant $KEY-Thai-dodek = 3508;

define constant $KEY-Thai-fofa = 3517;

define constant $KEY-Thai-fofan = 3519;

define constant $KEY-Thai-hohip = 3531;

define constant $KEY-Thai-honokhuk = 3534;

define constant $KEY-Thai-khokhai = 3490;

define constant $KEY-Thai-khokhon = 3493;

define constant $KEY-Thai-khokhuat = 3491;

define constant $KEY-Thai-khokhwai = 3492;

define constant $KEY-Thai-khorakhang = 3494;

define constant $KEY-Thai-kokai = 3489;

define constant $KEY-Thai-lakkhangyao = 3557;

define constant $KEY-Thai-lekchet = 3575;

define constant $KEY-Thai-lekha = 3573;

define constant $KEY-Thai-lekhok = 3574;

define constant $KEY-Thai-lekkao = 3577;

define constant $KEY-Thai-leknung = 3569;

define constant $KEY-Thai-lekpaet = 3576;

define constant $KEY-Thai-leksam = 3571;

define constant $KEY-Thai-leksi = 3572;

define constant $KEY-Thai-leksong = 3570;

define constant $KEY-Thai-leksun = 3568;

define constant $KEY-Thai-lochula = 3532;

define constant $KEY-Thai-loling = 3525;

define constant $KEY-Thai-lu = 3526;

define constant $KEY-Thai-maichattawa = 3563;

define constant $KEY-Thai-maiek = 3560;

define constant $KEY-Thai-maihanakat = 3537;

define constant $KEY-Thai-maihanakat-maitho = 3550;

define constant $KEY-Thai-maitaikhu = 3559;

define constant $KEY-Thai-maitho = 3561;

define constant $KEY-Thai-maitri = 3562;

define constant $KEY-Thai-maiyamok = 3558;

define constant $KEY-Thai-moma = 3521;

define constant $KEY-Thai-ngongu = 3495;

define constant $KEY-Thai-nikhahit = 3565;

define constant $KEY-Thai-nonen = 3507;

define constant $KEY-Thai-nonu = 3513;

define constant $KEY-Thai-oang = 3533;

define constant $KEY-Thai-paiyannoi = 3535;

define constant $KEY-Thai-phinthu = 3546;

define constant $KEY-Thai-phophan = 3518;

define constant $KEY-Thai-phophung = 3516;

define constant $KEY-Thai-phosamphao = 3520;

define constant $KEY-Thai-popla = 3515;

define constant $KEY-Thai-rorua = 3523;

define constant $KEY-Thai-ru = 3524;

define constant $KEY-Thai-saraa = 3536;

define constant $KEY-Thai-saraaa = 3538;

define constant $KEY-Thai-saraae = 3553;

define constant $KEY-Thai-saraaimaimalai = 3556;

define constant $KEY-Thai-saraaimaimuan = 3555;

define constant $KEY-Thai-saraam = 3539;

define constant $KEY-Thai-sarae = 3552;

define constant $KEY-Thai-sarai = 3540;

define constant $KEY-Thai-saraii = 3541;

define constant $KEY-Thai-sarao = 3554;

define constant $KEY-Thai-sarau = 3544;

define constant $KEY-Thai-saraue = 3542;

define constant $KEY-Thai-sarauee = 3543;

define constant $KEY-Thai-sarauu = 3545;

define constant $KEY-Thai-sorusi = 3529;

define constant $KEY-Thai-sosala = 3528;

define constant $KEY-Thai-soso = 3499;

define constant $KEY-Thai-sosua = 3530;

define constant $KEY-Thai-thanthakhat = 3564;

define constant $KEY-Thai-thonangmontho = 3505;

define constant $KEY-Thai-thophuthao = 3506;

define constant $KEY-Thai-thothahan = 3511;

define constant $KEY-Thai-thothan = 3504;

define constant $KEY-Thai-thothong = 3512;

define constant $KEY-Thai-thothung = 3510;

define constant $KEY-Thai-topatak = 3503;

define constant $KEY-Thai-totao = 3509;

define constant $KEY-Thai-wowaen = 3527;

define constant $KEY-Thai-yoyak = 3522;

define constant $KEY-Thai-yoying = 3501;

define constant $KEY-Time = 269025183;

define constant $KEY-ToDoList = 269025055;

define constant $KEY-Tools = 269025153;

define constant $KEY-TopMenu = 269025186;

define constant $KEY-TouchpadOff = 269025201;

define constant $KEY-TouchpadOn = 269025200;

define constant $KEY-TouchpadToggle = 269025193;

define constant $KEY-Touroku = 65323;

define constant $KEY-Travel = 269025154;

define constant $KEY-Tslash = 940;

define constant $KEY-U = 85;

define constant $KEY-UWB = 269025174;

define constant $KEY-Uacute = 218;

define constant $KEY-Ubelowdot = 16785124;

define constant $KEY-Ubreve = 733;

define constant $KEY-Ucircumflex = 219;

define constant $KEY-Udiaeresis = 220;

define constant $KEY-Udoubleacute = 475;

define constant $KEY-Ugrave = 217;

define constant $KEY-Uhook = 16785126;

define constant $KEY-Uhorn = 16777647;

define constant $KEY-Uhornacute = 16785128;

define constant $KEY-Uhornbelowdot = 16785136;

define constant $KEY-Uhorngrave = 16785130;

define constant $KEY-Uhornhook = 16785132;

define constant $KEY-Uhorntilde = 16785134;

define constant $KEY-Ukrainian-GHE-WITH-UPTURN = 1725;

define constant $KEY-Ukrainian-I = 1718;

define constant $KEY-Ukrainian-IE = 1716;

define constant $KEY-Ukrainian-YI = 1719;

define constant $KEY-Ukranian-I = 1718;

define constant $KEY-Ukranian-JE = 1716;

define constant $KEY-Ukranian-YI = 1719;

define constant $KEY-Umacron = 990;

define constant $KEY-Undo = 65381;

define constant $KEY-Ungrab = 269024800;

define constant $KEY-Uogonek = 985;

define constant $KEY-Up = 65362;

define constant $KEY-Uring = 473;

define constant $KEY-User1KB = 269025157;

define constant $KEY-User2KB = 269025158;

define constant $KEY-UserPB = 269025156;

define constant $KEY-Utilde = 989;

define constant $KEY-V = 86;

define constant $KEY-VendorHome = 269025076;

define constant $KEY-Video = 269025159;

define constant $KEY-View = 269025185;

define constant $KEY-VoidSymbol = 16777215;

define constant $KEY-W = 87;

define constant $KEY-WLAN = 269025173;

define constant $KEY-WWW = 269025070;

define constant $KEY-Wacute = 16785026;

define constant $KEY-WakeUp = 269025067;

define constant $KEY-Wcircumflex = 16777588;

define constant $KEY-Wdiaeresis = 16785028;

define constant $KEY-WebCam = 269025167;

define constant $KEY-Wgrave = 16785024;

define constant $KEY-WheelButton = 269025160;

define constant $KEY-WindowClear = 269025109;

define constant $KEY-WonSign = 16785577;

define constant $KEY-Word = 269025161;

define constant $KEY-X = 88;

define constant $KEY-Xabovedot = 16785034;

define constant $KEY-Xfer = 269025162;

define constant $KEY-Y = 89;

define constant $KEY-Yacute = 221;

define constant $KEY-Ybelowdot = 16785140;

define constant $KEY-Ycircumflex = 16777590;

define constant $KEY-Ydiaeresis = 5054;

define constant $KEY-Yellow = 269025189;

define constant $KEY-Ygrave = 16785138;

define constant $KEY-Yhook = 16785142;

define constant $KEY-Ytilde = 16785144;

define constant $KEY-Z = 90;

define constant $KEY-Zabovedot = 431;

define constant $KEY-Zacute = 428;

define constant $KEY-Zcaron = 430;

define constant $KEY-Zen-Koho = 65341;

define constant $KEY-Zenkaku = 65320;

define constant $KEY-Zenkaku-Hankaku = 65322;

define constant $KEY-ZoomIn = 269025163;

define constant $KEY-ZoomOut = 269025164;

define constant $KEY-Zstroke = 16777653;

define constant $KEY-abovedot = 511;

define constant $KEY-acute = 180;

define constant $KEY-ampersand = 38;

define constant $KEY-apostrophe = 39;

define constant $KEY-approxeq = 16785992;

define constant $KEY-approximate = 2248;

define constant $KEY-asciicircum = 94;

define constant $KEY-asciitilde = 126;

define constant $KEY-asterisk = 42;

define constant $KEY-at = 64;

define constant $KEY-backslash = 92;

define constant $KEY-ballotcross = 2804;

define constant $KEY-bar = 124;

define constant $KEY-because = 16785973;

define constant $KEY-blank = 2527;

define constant $KEY-botintegral = 2213;

define constant $KEY-botleftparens = 2220;

define constant $KEY-botleftsqbracket = 2216;

define constant $KEY-botleftsummation = 2226;

define constant $KEY-botrightparens = 2222;

define constant $KEY-botrightsqbracket = 2218;

define constant $KEY-botrightsummation = 2230;

define constant $KEY-bott = 2550;

define constant $KEY-botvertsummationconnector = 2228;

define constant $KEY-braceleft = 123;

define constant $KEY-braceright = 125;

define constant $KEY-bracketleft = 91;

define constant $KEY-bracketright = 93;

define constant $KEY-braille-blank = 16787456;

define constant $KEY-braille-dot-1 = 65521;

define constant $KEY-braille-dot-10 = 65530;

define constant $KEY-braille-dot-2 = 65522;

define constant $KEY-braille-dot-3 = 65523;

define constant $KEY-braille-dot-4 = 65524;

define constant $KEY-braille-dot-5 = 65525;

define constant $KEY-braille-dot-6 = 65526;

define constant $KEY-braille-dot-7 = 65527;

define constant $KEY-braille-dot-8 = 65528;

define constant $KEY-braille-dot-9 = 65529;

define constant $KEY-braille-dots-1 = 16787457;

define constant $KEY-braille-dots-12 = 16787459;

define constant $KEY-braille-dots-123 = 16787463;

define constant $KEY-braille-dots-1234 = 16787471;

define constant $KEY-braille-dots-12345 = 16787487;

define constant $KEY-braille-dots-123456 = 16787519;

define constant $KEY-braille-dots-1234567 = 16787583;

define constant $KEY-braille-dots-12345678 = 16787711;

define constant $KEY-braille-dots-1234568 = 16787647;

define constant $KEY-braille-dots-123457 = 16787551;

define constant $KEY-braille-dots-1234578 = 16787679;

define constant $KEY-braille-dots-123458 = 16787615;

define constant $KEY-braille-dots-12346 = 16787503;

define constant $KEY-braille-dots-123467 = 16787567;

define constant $KEY-braille-dots-1234678 = 16787695;

define constant $KEY-braille-dots-123468 = 16787631;

define constant $KEY-braille-dots-12347 = 16787535;

define constant $KEY-braille-dots-123478 = 16787663;

define constant $KEY-braille-dots-12348 = 16787599;

define constant $KEY-braille-dots-1235 = 16787479;

define constant $KEY-braille-dots-12356 = 16787511;

define constant $KEY-braille-dots-123567 = 16787575;

define constant $KEY-braille-dots-1235678 = 16787703;

define constant $KEY-braille-dots-123568 = 16787639;

define constant $KEY-braille-dots-12357 = 16787543;

define constant $KEY-braille-dots-123578 = 16787671;

define constant $KEY-braille-dots-12358 = 16787607;

define constant $KEY-braille-dots-1236 = 16787495;

define constant $KEY-braille-dots-12367 = 16787559;

define constant $KEY-braille-dots-123678 = 16787687;

define constant $KEY-braille-dots-12368 = 16787623;

define constant $KEY-braille-dots-1237 = 16787527;

define constant $KEY-braille-dots-12378 = 16787655;

define constant $KEY-braille-dots-1238 = 16787591;

define constant $KEY-braille-dots-124 = 16787467;

define constant $KEY-braille-dots-1245 = 16787483;

define constant $KEY-braille-dots-12456 = 16787515;

define constant $KEY-braille-dots-124567 = 16787579;

define constant $KEY-braille-dots-1245678 = 16787707;

define constant $KEY-braille-dots-124568 = 16787643;

define constant $KEY-braille-dots-12457 = 16787547;

define constant $KEY-braille-dots-124578 = 16787675;

define constant $KEY-braille-dots-12458 = 16787611;

define constant $KEY-braille-dots-1246 = 16787499;

define constant $KEY-braille-dots-12467 = 16787563;

define constant $KEY-braille-dots-124678 = 16787691;

define constant $KEY-braille-dots-12468 = 16787627;

define constant $KEY-braille-dots-1247 = 16787531;

define constant $KEY-braille-dots-12478 = 16787659;

define constant $KEY-braille-dots-1248 = 16787595;

define constant $KEY-braille-dots-125 = 16787475;

define constant $KEY-braille-dots-1256 = 16787507;

define constant $KEY-braille-dots-12567 = 16787571;

define constant $KEY-braille-dots-125678 = 16787699;

define constant $KEY-braille-dots-12568 = 16787635;

define constant $KEY-braille-dots-1257 = 16787539;

define constant $KEY-braille-dots-12578 = 16787667;

define constant $KEY-braille-dots-1258 = 16787603;

define constant $KEY-braille-dots-126 = 16787491;

define constant $KEY-braille-dots-1267 = 16787555;

define constant $KEY-braille-dots-12678 = 16787683;

define constant $KEY-braille-dots-1268 = 16787619;

define constant $KEY-braille-dots-127 = 16787523;

define constant $KEY-braille-dots-1278 = 16787651;

define constant $KEY-braille-dots-128 = 16787587;

define constant $KEY-braille-dots-13 = 16787461;

define constant $KEY-braille-dots-134 = 16787469;

define constant $KEY-braille-dots-1345 = 16787485;

define constant $KEY-braille-dots-13456 = 16787517;

define constant $KEY-braille-dots-134567 = 16787581;

define constant $KEY-braille-dots-1345678 = 16787709;

define constant $KEY-braille-dots-134568 = 16787645;

define constant $KEY-braille-dots-13457 = 16787549;

define constant $KEY-braille-dots-134578 = 16787677;

define constant $KEY-braille-dots-13458 = 16787613;

define constant $KEY-braille-dots-1346 = 16787501;

define constant $KEY-braille-dots-13467 = 16787565;

define constant $KEY-braille-dots-134678 = 16787693;

define constant $KEY-braille-dots-13468 = 16787629;

define constant $KEY-braille-dots-1347 = 16787533;

define constant $KEY-braille-dots-13478 = 16787661;

define constant $KEY-braille-dots-1348 = 16787597;

define constant $KEY-braille-dots-135 = 16787477;

define constant $KEY-braille-dots-1356 = 16787509;

define constant $KEY-braille-dots-13567 = 16787573;

define constant $KEY-braille-dots-135678 = 16787701;

define constant $KEY-braille-dots-13568 = 16787637;

define constant $KEY-braille-dots-1357 = 16787541;

define constant $KEY-braille-dots-13578 = 16787669;

define constant $KEY-braille-dots-1358 = 16787605;

define constant $KEY-braille-dots-136 = 16787493;

define constant $KEY-braille-dots-1367 = 16787557;

define constant $KEY-braille-dots-13678 = 16787685;

define constant $KEY-braille-dots-1368 = 16787621;

define constant $KEY-braille-dots-137 = 16787525;

define constant $KEY-braille-dots-1378 = 16787653;

define constant $KEY-braille-dots-138 = 16787589;

define constant $KEY-braille-dots-14 = 16787465;

define constant $KEY-braille-dots-145 = 16787481;

define constant $KEY-braille-dots-1456 = 16787513;

define constant $KEY-braille-dots-14567 = 16787577;

define constant $KEY-braille-dots-145678 = 16787705;

define constant $KEY-braille-dots-14568 = 16787641;

define constant $KEY-braille-dots-1457 = 16787545;

define constant $KEY-braille-dots-14578 = 16787673;

define constant $KEY-braille-dots-1458 = 16787609;

define constant $KEY-braille-dots-146 = 16787497;

define constant $KEY-braille-dots-1467 = 16787561;

define constant $KEY-braille-dots-14678 = 16787689;

define constant $KEY-braille-dots-1468 = 16787625;

define constant $KEY-braille-dots-147 = 16787529;

define constant $KEY-braille-dots-1478 = 16787657;

define constant $KEY-braille-dots-148 = 16787593;

define constant $KEY-braille-dots-15 = 16787473;

define constant $KEY-braille-dots-156 = 16787505;

define constant $KEY-braille-dots-1567 = 16787569;

define constant $KEY-braille-dots-15678 = 16787697;

define constant $KEY-braille-dots-1568 = 16787633;

define constant $KEY-braille-dots-157 = 16787537;

define constant $KEY-braille-dots-1578 = 16787665;

define constant $KEY-braille-dots-158 = 16787601;

define constant $KEY-braille-dots-16 = 16787489;

define constant $KEY-braille-dots-167 = 16787553;

define constant $KEY-braille-dots-1678 = 16787681;

define constant $KEY-braille-dots-168 = 16787617;

define constant $KEY-braille-dots-17 = 16787521;

define constant $KEY-braille-dots-178 = 16787649;

define constant $KEY-braille-dots-18 = 16787585;

define constant $KEY-braille-dots-2 = 16787458;

define constant $KEY-braille-dots-23 = 16787462;

define constant $KEY-braille-dots-234 = 16787470;

define constant $KEY-braille-dots-2345 = 16787486;

define constant $KEY-braille-dots-23456 = 16787518;

define constant $KEY-braille-dots-234567 = 16787582;

define constant $KEY-braille-dots-2345678 = 16787710;

define constant $KEY-braille-dots-234568 = 16787646;

define constant $KEY-braille-dots-23457 = 16787550;

define constant $KEY-braille-dots-234578 = 16787678;

define constant $KEY-braille-dots-23458 = 16787614;

define constant $KEY-braille-dots-2346 = 16787502;

define constant $KEY-braille-dots-23467 = 16787566;

define constant $KEY-braille-dots-234678 = 16787694;

define constant $KEY-braille-dots-23468 = 16787630;

define constant $KEY-braille-dots-2347 = 16787534;

define constant $KEY-braille-dots-23478 = 16787662;

define constant $KEY-braille-dots-2348 = 16787598;

define constant $KEY-braille-dots-235 = 16787478;

define constant $KEY-braille-dots-2356 = 16787510;

define constant $KEY-braille-dots-23567 = 16787574;

define constant $KEY-braille-dots-235678 = 16787702;

define constant $KEY-braille-dots-23568 = 16787638;

define constant $KEY-braille-dots-2357 = 16787542;

define constant $KEY-braille-dots-23578 = 16787670;

define constant $KEY-braille-dots-2358 = 16787606;

define constant $KEY-braille-dots-236 = 16787494;

define constant $KEY-braille-dots-2367 = 16787558;

define constant $KEY-braille-dots-23678 = 16787686;

define constant $KEY-braille-dots-2368 = 16787622;

define constant $KEY-braille-dots-237 = 16787526;

define constant $KEY-braille-dots-2378 = 16787654;

define constant $KEY-braille-dots-238 = 16787590;

define constant $KEY-braille-dots-24 = 16787466;

define constant $KEY-braille-dots-245 = 16787482;

define constant $KEY-braille-dots-2456 = 16787514;

define constant $KEY-braille-dots-24567 = 16787578;

define constant $KEY-braille-dots-245678 = 16787706;

define constant $KEY-braille-dots-24568 = 16787642;

define constant $KEY-braille-dots-2457 = 16787546;

define constant $KEY-braille-dots-24578 = 16787674;

define constant $KEY-braille-dots-2458 = 16787610;

define constant $KEY-braille-dots-246 = 16787498;

define constant $KEY-braille-dots-2467 = 16787562;

define constant $KEY-braille-dots-24678 = 16787690;

define constant $KEY-braille-dots-2468 = 16787626;

define constant $KEY-braille-dots-247 = 16787530;

define constant $KEY-braille-dots-2478 = 16787658;

define constant $KEY-braille-dots-248 = 16787594;

define constant $KEY-braille-dots-25 = 16787474;

define constant $KEY-braille-dots-256 = 16787506;

define constant $KEY-braille-dots-2567 = 16787570;

define constant $KEY-braille-dots-25678 = 16787698;

define constant $KEY-braille-dots-2568 = 16787634;

define constant $KEY-braille-dots-257 = 16787538;

define constant $KEY-braille-dots-2578 = 16787666;

define constant $KEY-braille-dots-258 = 16787602;

define constant $KEY-braille-dots-26 = 16787490;

define constant $KEY-braille-dots-267 = 16787554;

define constant $KEY-braille-dots-2678 = 16787682;

define constant $KEY-braille-dots-268 = 16787618;

define constant $KEY-braille-dots-27 = 16787522;

define constant $KEY-braille-dots-278 = 16787650;

define constant $KEY-braille-dots-28 = 16787586;

define constant $KEY-braille-dots-3 = 16787460;

define constant $KEY-braille-dots-34 = 16787468;

define constant $KEY-braille-dots-345 = 16787484;

define constant $KEY-braille-dots-3456 = 16787516;

define constant $KEY-braille-dots-34567 = 16787580;

define constant $KEY-braille-dots-345678 = 16787708;

define constant $KEY-braille-dots-34568 = 16787644;

define constant $KEY-braille-dots-3457 = 16787548;

define constant $KEY-braille-dots-34578 = 16787676;

define constant $KEY-braille-dots-3458 = 16787612;

define constant $KEY-braille-dots-346 = 16787500;

define constant $KEY-braille-dots-3467 = 16787564;

define constant $KEY-braille-dots-34678 = 16787692;

define constant $KEY-braille-dots-3468 = 16787628;

define constant $KEY-braille-dots-347 = 16787532;

define constant $KEY-braille-dots-3478 = 16787660;

define constant $KEY-braille-dots-348 = 16787596;

define constant $KEY-braille-dots-35 = 16787476;

define constant $KEY-braille-dots-356 = 16787508;

define constant $KEY-braille-dots-3567 = 16787572;

define constant $KEY-braille-dots-35678 = 16787700;

define constant $KEY-braille-dots-3568 = 16787636;

define constant $KEY-braille-dots-357 = 16787540;

define constant $KEY-braille-dots-3578 = 16787668;

define constant $KEY-braille-dots-358 = 16787604;

define constant $KEY-braille-dots-36 = 16787492;

define constant $KEY-braille-dots-367 = 16787556;

define constant $KEY-braille-dots-3678 = 16787684;

define constant $KEY-braille-dots-368 = 16787620;

define constant $KEY-braille-dots-37 = 16787524;

define constant $KEY-braille-dots-378 = 16787652;

define constant $KEY-braille-dots-38 = 16787588;

define constant $KEY-braille-dots-4 = 16787464;

define constant $KEY-braille-dots-45 = 16787480;

define constant $KEY-braille-dots-456 = 16787512;

define constant $KEY-braille-dots-4567 = 16787576;

define constant $KEY-braille-dots-45678 = 16787704;

define constant $KEY-braille-dots-4568 = 16787640;

define constant $KEY-braille-dots-457 = 16787544;

define constant $KEY-braille-dots-4578 = 16787672;

define constant $KEY-braille-dots-458 = 16787608;

define constant $KEY-braille-dots-46 = 16787496;

define constant $KEY-braille-dots-467 = 16787560;

define constant $KEY-braille-dots-4678 = 16787688;

define constant $KEY-braille-dots-468 = 16787624;

define constant $KEY-braille-dots-47 = 16787528;

define constant $KEY-braille-dots-478 = 16787656;

define constant $KEY-braille-dots-48 = 16787592;

define constant $KEY-braille-dots-5 = 16787472;

define constant $KEY-braille-dots-56 = 16787504;

define constant $KEY-braille-dots-567 = 16787568;

define constant $KEY-braille-dots-5678 = 16787696;

define constant $KEY-braille-dots-568 = 16787632;

define constant $KEY-braille-dots-57 = 16787536;

define constant $KEY-braille-dots-578 = 16787664;

define constant $KEY-braille-dots-58 = 16787600;

define constant $KEY-braille-dots-6 = 16787488;

define constant $KEY-braille-dots-67 = 16787552;

define constant $KEY-braille-dots-678 = 16787680;

define constant $KEY-braille-dots-68 = 16787616;

define constant $KEY-braille-dots-7 = 16787520;

define constant $KEY-braille-dots-78 = 16787648;

define constant $KEY-braille-dots-8 = 16787584;

define constant $KEY-breve = 418;

define constant $KEY-brokenbar = 166;

define constant $KEY-careof = 2744;

define constant $KEY-caret = 2812;

define constant $KEY-caron = 439;

define constant $KEY-cedilla = 184;

define constant $KEY-cent = 162;

define constant $KEY-checkerboard = 2529;

define constant $KEY-checkmark = 2803;

define constant $KEY-circle = 3023;

define constant $KEY-club = 2796;

define constant $KEY-colon = 58;

define constant $KEY-comma = 44;

define constant $KEY-containsas = 16785931;

define constant $KEY-copyright = 169;

define constant $KEY-cr = 2532;

define constant $KEY-crossinglines = 2542;

define constant $KEY-cuberoot = 16785947;

define constant $KEY-currency = 164;

define constant $KEY-cursor = 2815;

define constant $KEY-dagger = 2801;

define constant $KEY-dead-A = 65153;

define constant $KEY-dead-E = 65155;

define constant $KEY-dead-I = 65157;

define constant $KEY-dead-O = 65159;

define constant $KEY-dead-U = 65161;

define constant $KEY-dead-abovecomma = 65124;

define constant $KEY-dead-abovedot = 65110;

define constant $KEY-dead-abovereversedcomma = 65125;

define constant $KEY-dead-abovering = 65112;

define constant $KEY-dead-acute = 65105;

define constant $KEY-dead-belowbreve = 65131;

define constant $KEY-dead-belowcircumflex = 65129;

define constant $KEY-dead-belowcomma = 65134;

define constant $KEY-dead-belowdiaeresis = 65132;

define constant $KEY-dead-belowdot = 65120;

define constant $KEY-dead-belowmacron = 65128;

define constant $KEY-dead-belowring = 65127;

define constant $KEY-dead-belowtilde = 65130;

define constant $KEY-dead-breve = 65109;

define constant $KEY-dead-capital-schwa = 65163;

define constant $KEY-dead-caron = 65114;

define constant $KEY-dead-cedilla = 65115;

define constant $KEY-dead-circumflex = 65106;

define constant $KEY-dead-currency = 65135;

define constant $KEY-dead-dasia = 65125;

define constant $KEY-dead-diaeresis = 65111;

define constant $KEY-dead-doubleacute = 65113;

define constant $KEY-dead-doublegrave = 65126;

define constant $KEY-dead-grave = 65104;

define constant $KEY-dead-greek = 65164;

define constant $KEY-dead-hook = 65121;

define constant $KEY-dead-horn = 65122;

define constant $KEY-dead-invertedbreve = 65133;

define constant $KEY-dead-iota = 65117;

define constant $KEY-dead-macron = 65108;

define constant $KEY-dead-ogonek = 65116;

define constant $KEY-dead-perispomeni = 65107;

define constant $KEY-dead-psili = 65124;

define constant $KEY-dead-semivoiced-sound = 65119;

define constant $KEY-dead-small-schwa = 65162;

define constant $KEY-dead-stroke = 65123;

define constant $KEY-dead-tilde = 65107;

define constant $KEY-dead-voiced-sound = 65118;

define constant $KEY-decimalpoint = 2749;

define constant $KEY-degree = 176;

define constant $KEY-diaeresis = 168;

define constant $KEY-diamond = 2797;

define constant $KEY-digitspace = 2725;

define constant $KEY-dintegral = 16785964;

define constant $KEY-division = 247;

define constant $KEY-dollar = 36;

define constant $KEY-doubbaselinedot = 2735;

define constant $KEY-doubleacute = 445;

define constant $KEY-doubledagger = 2802;

define constant $KEY-doublelowquotemark = 2814;

define constant $KEY-downarrow = 2302;

define constant $KEY-downcaret = 2984;

define constant $KEY-downshoe = 3030;

define constant $KEY-downstile = 3012;

define constant $KEY-downtack = 3010;

define constant $KEY-eightsubscript = 16785544;

define constant $KEY-eightsuperior = 16785528;

define constant $KEY-elementof = 16785928;

define constant $KEY-ellipsis = 2734;

define constant $KEY-em3space = 2723;

define constant $KEY-em4space = 2724;

define constant $KEY-emdash = 2729;

define constant $KEY-emfilledcircle = 2782;

define constant $KEY-emfilledrect = 2783;

define constant $KEY-emopencircle = 2766;

define constant $KEY-emopenrectangle = 2767;

define constant $KEY-emptyset = 16785925;

define constant $KEY-emspace = 2721;

define constant $KEY-endash = 2730;

define constant $KEY-enfilledcircbullet = 2790;

define constant $KEY-enfilledsqbullet = 2791;

define constant $KEY-enopencircbullet = 2784;

define constant $KEY-enopensquarebullet = 2785;

define constant $KEY-enspace = 2722;

define constant $KEY-equal = 61;

define constant $KEY-exclam = 33;

define constant $KEY-exclamdown = 161;

define constant $KEY-femalesymbol = 2808;

define constant $KEY-ff = 2531;

define constant $KEY-figdash = 2747;

define constant $KEY-filledlefttribullet = 2780;

define constant $KEY-filledrectbullet = 2779;

define constant $KEY-filledrighttribullet = 2781;

define constant $KEY-filledtribulletdown = 2793;

define constant $KEY-filledtribulletup = 2792;

define constant $KEY-fiveeighths = 2757;

define constant $KEY-fivesixths = 2743;

define constant $KEY-fivesubscript = 16785541;

define constant $KEY-fivesuperior = 16785525;

define constant $KEY-fourfifths = 2741;

define constant $KEY-foursubscript = 16785540;

define constant $KEY-foursuperior = 16785524;

define constant $KEY-fourthroot = 16785948;

define constant $KEY-function = 2294;

define constant $KEY-grave = 96;

define constant $KEY-greater = 62;

define constant $KEY-greaterthanequal = 2238;

define constant $KEY-guillemotleft = 171;

define constant $KEY-guillemotright = 187;

define constant $KEY-hairspace = 2728;

define constant $KEY-heart = 2798;

define constant $KEY-hebrew-aleph = 3296;

define constant $KEY-hebrew-ayin = 3314;

define constant $KEY-hebrew-bet = 3297;

define constant $KEY-hebrew-beth = 3297;

define constant $KEY-hebrew-chet = 3303;

define constant $KEY-hebrew-dalet = 3299;

define constant $KEY-hebrew-daleth = 3299;

define constant $KEY-hebrew-doublelowline = 3295;

define constant $KEY-hebrew-finalkaph = 3306;

define constant $KEY-hebrew-finalmem = 3309;

define constant $KEY-hebrew-finalnun = 3311;

define constant $KEY-hebrew-finalpe = 3315;

define constant $KEY-hebrew-finalzade = 3317;

define constant $KEY-hebrew-finalzadi = 3317;

define constant $KEY-hebrew-gimel = 3298;

define constant $KEY-hebrew-gimmel = 3298;

define constant $KEY-hebrew-he = 3300;

define constant $KEY-hebrew-het = 3303;

define constant $KEY-hebrew-kaph = 3307;

define constant $KEY-hebrew-kuf = 3319;

define constant $KEY-hebrew-lamed = 3308;

define constant $KEY-hebrew-mem = 3310;

define constant $KEY-hebrew-nun = 3312;

define constant $KEY-hebrew-pe = 3316;

define constant $KEY-hebrew-qoph = 3319;

define constant $KEY-hebrew-resh = 3320;

define constant $KEY-hebrew-samech = 3313;

define constant $KEY-hebrew-samekh = 3313;

define constant $KEY-hebrew-shin = 3321;

define constant $KEY-hebrew-taf = 3322;

define constant $KEY-hebrew-taw = 3322;

define constant $KEY-hebrew-tet = 3304;

define constant $KEY-hebrew-teth = 3304;

define constant $KEY-hebrew-waw = 3301;

define constant $KEY-hebrew-yod = 3305;

define constant $KEY-hebrew-zade = 3318;

define constant $KEY-hebrew-zadi = 3318;

define constant $KEY-hebrew-zain = 3302;

define constant $KEY-hebrew-zayin = 3302;

define constant $KEY-hexagram = 2778;

define constant $KEY-horizconnector = 2211;

define constant $KEY-horizlinescan1 = 2543;

define constant $KEY-horizlinescan3 = 2544;

define constant $KEY-horizlinescan5 = 2545;

define constant $KEY-horizlinescan7 = 2546;

define constant $KEY-horizlinescan9 = 2547;

define constant $KEY-ht = 2530;

define constant $KEY-hyphen = 173;

define constant $KEY-iTouch = 269025120;

define constant $KEY-identical = 2255;

define constant $KEY-idotless = 697;

define constant $KEY-ifonlyif = 2253;

define constant $KEY-implies = 2254;

define constant $KEY-includedin = 2266;

define constant $KEY-includes = 2267;

define constant $KEY-infinity = 2242;

define constant $KEY-integral = 2239;

define constant $KEY-intersection = 2268;

define constant $KEY-jot = 3018;

define constant $KEY-kana-A = 1201;

define constant $KEY-kana-CHI = 1217;

define constant $KEY-kana-E = 1204;

define constant $KEY-kana-FU = 1228;

define constant $KEY-kana-HA = 1226;

define constant $KEY-kana-HE = 1229;

define constant $KEY-kana-HI = 1227;

define constant $KEY-kana-HO = 1230;

define constant $KEY-kana-HU = 1228;

define constant $KEY-kana-I = 1202;

define constant $KEY-kana-KA = 1206;

define constant $KEY-kana-KE = 1209;

define constant $KEY-kana-KI = 1207;

define constant $KEY-kana-KO = 1210;

define constant $KEY-kana-KU = 1208;

define constant $KEY-kana-MA = 1231;

define constant $KEY-kana-ME = 1234;

define constant $KEY-kana-MI = 1232;

define constant $KEY-kana-MO = 1235;

define constant $KEY-kana-MU = 1233;

define constant $KEY-kana-N = 1245;

define constant $KEY-kana-NA = 1221;

define constant $KEY-kana-NE = 1224;

define constant $KEY-kana-NI = 1222;

define constant $KEY-kana-NO = 1225;

define constant $KEY-kana-NU = 1223;

define constant $KEY-kana-O = 1205;

define constant $KEY-kana-RA = 1239;

define constant $KEY-kana-RE = 1242;

define constant $KEY-kana-RI = 1240;

define constant $KEY-kana-RO = 1243;

define constant $KEY-kana-RU = 1241;

define constant $KEY-kana-SA = 1211;

define constant $KEY-kana-SE = 1214;

define constant $KEY-kana-SHI = 1212;

define constant $KEY-kana-SO = 1215;

define constant $KEY-kana-SU = 1213;

define constant $KEY-kana-TA = 1216;

define constant $KEY-kana-TE = 1219;

define constant $KEY-kana-TI = 1217;

define constant $KEY-kana-TO = 1220;

define constant $KEY-kana-TSU = 1218;

define constant $KEY-kana-TU = 1218;

define constant $KEY-kana-U = 1203;

define constant $KEY-kana-WA = 1244;

define constant $KEY-kana-WO = 1190;

define constant $KEY-kana-YA = 1236;

define constant $KEY-kana-YO = 1238;

define constant $KEY-kana-YU = 1237;

define constant $KEY-kana-closingbracket = 1187;

define constant $KEY-kana-comma = 1188;

define constant $KEY-kana-conjunctive = 1189;

define constant $KEY-kana-fullstop = 1185;

define constant $KEY-kana-middledot = 1189;

define constant $KEY-kana-openingbracket = 1186;

define constant $KEY-kana-switch = 65406;

define constant $KEY-kappa = 930;

define constant $KEY-kra = 930;

define constant $KEY-latincross = 2777;

define constant $KEY-leftanglebracket = 2748;

define constant $KEY-leftarrow = 2299;

define constant $KEY-leftcaret = 2979;

define constant $KEY-leftdoublequotemark = 2770;

define constant $KEY-leftmiddlecurlybrace = 2223;

define constant $KEY-leftopentriangle = 2764;

define constant $KEY-leftpointer = 2794;

define constant $KEY-leftradical = 2209;

define constant $KEY-leftshoe = 3034;

define constant $KEY-leftsinglequotemark = 2768;

define constant $KEY-leftt = 2548;

define constant $KEY-lefttack = 3036;

define constant $KEY-less = 60;

define constant $KEY-lessthanequal = 2236;

define constant $KEY-lf = 2533;

define constant $KEY-logicaland = 2270;

define constant $KEY-logicalor = 2271;

define constant $KEY-lowleftcorner = 2541;

define constant $KEY-lowrightcorner = 2538;

define constant $KEY-macron = 175;

define constant $KEY-malesymbol = 2807;

define constant $KEY-maltesecross = 2800;

define constant $KEY-marker = 2751;

define constant $KEY-masculine = 186;

define constant $KEY-minus = 45;

define constant $KEY-minutes = 2774;

define constant $KEY-mu = 181;

define constant $KEY-multiply = 215;

define constant $KEY-musicalflat = 2806;

define constant $KEY-musicalsharp = 2805;

define constant $KEY-nabla = 2245;

define constant $KEY-ninesubscript = 16785545;

define constant $KEY-ninesuperior = 16785529;

define constant $KEY-nl = 2536;

define constant $KEY-nobreakspace = 160;

define constant $KEY-notapproxeq = 16785991;

define constant $KEY-notelementof = 16785929;

define constant $KEY-notequal = 2237;

define constant $KEY-notidentical = 16786018;

define constant $KEY-notsign = 172;

define constant $KEY-numbersign = 35;

define constant $KEY-numerosign = 1712;

define constant $KEY-ogonek = 434;

define constant $KEY-oneeighth = 2755;

define constant $KEY-onefifth = 2738;

define constant $KEY-onehalf = 189;

define constant $KEY-onequarter = 188;

define constant $KEY-onesixth = 2742;

define constant $KEY-onesubscript = 16785537;

define constant $KEY-onesuperior = 185;

define constant $KEY-onethird = 2736;

define constant $KEY-openrectbullet = 2786;

define constant $KEY-openstar = 2789;

define constant $KEY-opentribulletdown = 2788;

define constant $KEY-opentribulletup = 2787;

define constant $KEY-ordfeminine = 170;

define constant $KEY-overbar = 3008;

define constant $KEY-overline = 1150;

define constant $KEY-paragraph = 182;

define constant $KEY-parenleft = 40;

define constant $KEY-parenright = 41;

define constant $KEY-partdifferential = 16785922;

define constant $KEY-partialderivative = 2287;

define constant $KEY-percent = 37;

define constant $KEY-period = 46;

define constant $KEY-periodcentered = 183;

define constant $KEY-permille = 2773;

define constant $KEY-phonographcopyright = 2811;

define constant $KEY-plus = 43;

define constant $KEY-plusminus = 177;

define constant $KEY-prescription = 2772;

define constant $KEY-prolongedsound = 1200;

define constant $KEY-punctspace = 2726;

define constant $KEY-quad = 3020;

define constant $KEY-question = 63;

define constant $KEY-questiondown = 191;

define constant $KEY-quotedbl = 34;

define constant $KEY-quoteleft = 96;

define constant $KEY-quoteright = 39;

define constant $KEY-radical = 2262;

define constant $KEY-registered = 174;

define constant $KEY-rightanglebracket = 2750;

define constant $KEY-rightarrow = 2301;

define constant $KEY-rightcaret = 2982;

define constant $KEY-rightdoublequotemark = 2771;

define constant $KEY-rightmiddlecurlybrace = 2224;

define constant $KEY-rightmiddlesummation = 2231;

define constant $KEY-rightopentriangle = 2765;

define constant $KEY-rightpointer = 2795;

define constant $KEY-rightshoe = 3032;

define constant $KEY-rightsinglequotemark = 2769;

define constant $KEY-rightt = 2549;

define constant $KEY-righttack = 3068;

define constant $KEY-script-switch = 65406;

define constant $KEY-seconds = 2775;

define constant $KEY-section = 167;

define constant $KEY-semicolon = 59;

define constant $KEY-semivoicedsound = 1247;

define constant $KEY-seveneighths = 2758;

define constant $KEY-sevensubscript = 16785543;

define constant $KEY-sevensuperior = 16785527;

define constant $KEY-signaturemark = 2762;

define constant $KEY-signifblank = 2732;

define constant $KEY-similarequal = 2249;

define constant $KEY-singlelowquotemark = 2813;

define constant $KEY-sixsubscript = 16785542;

define constant $KEY-sixsuperior = 16785526;

define constant $KEY-slash = 47;

define constant $KEY-soliddiamond = 2528;

define constant $KEY-space = 32;

define constant $KEY-squareroot = 16785946;

define constant $KEY-ssharp = 223;

define constant $KEY-sterling = 163;

define constant $KEY-stricteq = 16786019;

define constant $KEY-telephone = 2809;

define constant $KEY-telephonerecorder = 2810;

define constant $KEY-therefore = 2240;

define constant $KEY-thinspace = 2727;

define constant $KEY-threeeighths = 2756;

define constant $KEY-threefifths = 2740;

define constant $KEY-threequarters = 190;

define constant $KEY-threesubscript = 16785539;

define constant $KEY-threesuperior = 179;

define constant $KEY-tintegral = 16785965;

define constant $KEY-topintegral = 2212;

define constant $KEY-topleftparens = 2219;

define constant $KEY-topleftradical = 2210;

define constant $KEY-topleftsqbracket = 2215;

define constant $KEY-topleftsummation = 2225;

define constant $KEY-toprightparens = 2221;

define constant $KEY-toprightsqbracket = 2217;

define constant $KEY-toprightsummation = 2229;

define constant $KEY-topt = 2551;

define constant $KEY-topvertsummationconnector = 2227;

define constant $KEY-trademark = 2761;

define constant $KEY-trademarkincircle = 2763;

define constant $KEY-twofifths = 2739;

define constant $KEY-twosubscript = 16785538;

define constant $KEY-twosuperior = 178;

define constant $KEY-twothirds = 2737;

define constant $KEY-underbar = 3014;

define constant $KEY-underscore = 95;

define constant $KEY-union = 2269;

define constant $KEY-uparrow = 2300;

define constant $KEY-upcaret = 2985;

define constant $KEY-upleftcorner = 2540;

define constant $KEY-uprightcorner = 2539;

define constant $KEY-upshoe = 3011;

define constant $KEY-upstile = 3027;

define constant $KEY-uptack = 3022;

define constant $KEY-variation = 2241;

define constant $KEY-vertbar = 2552;

define constant $KEY-vertconnector = 2214;

define constant $KEY-voicedsound = 1246;

define constant $KEY-vt = 2537;

define constant $KEY-yen = 165;

define constant $KEY-zerosubscript = 16785536;

define constant $KEY-zerosuperior = 16785520;

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
  slot gdkkeymapkey-keycode :: <C-unsigned-int>;
  slot gdkkeymapkey-group :: <C-signed-int>;
  slot gdkkeymapkey-level :: <C-signed-int>;
  pointer-type-name: <GdkKeymapKey>;
end C-struct;

define constant $MAX-TIMECOORD-AXES = 128;

define constant $GDK-MODIFIER-INTENT-PRIMARY-ACCELERATOR = 0;
define constant $GDK-MODIFIER-INTENT-CONTEXT-MENU = 1;
define constant $GDK-MODIFIER-INTENT-EXTEND-SELECTION = 2;
define constant $GDK-MODIFIER-INTENT-MODIFY-SELECTION = 3;
define constant $GDK-MODIFIER-INTENT-NO-TEXT-INPUT = 4;
define constant $GDK-MODIFIER-INTENT-SHIFT-GROUP = 5;
define constant <GdkModifierIntent> = <C-int>;
define C-pointer-type <GdkModifierIntent*> => <GdkModifierIntent>;

define constant $GDK-SHIFT-MASK = 1;
define constant $GDK-LOCK-MASK = 2;
define constant $GDK-CONTROL-MASK = 4;
define constant $GDK-MOD1-MASK = 8;
define constant $GDK-MOD2-MASK = 16;
define constant $GDK-MOD3-MASK = 32;
define constant $GDK-MOD4-MASK = 64;
define constant $GDK-MOD5-MASK = 128;
define constant $GDK-BUTTON1-MASK = 256;
define constant $GDK-BUTTON2-MASK = 512;
define constant $GDK-BUTTON3-MASK = 1024;
define constant $GDK-BUTTON4-MASK = 2048;
define constant $GDK-BUTTON5-MASK = 4096;
define constant $GDK-MODIFIER-RESERVED-13-MASK = 8192;
define constant $GDK-MODIFIER-RESERVED-14-MASK = 16384;
define constant $GDK-MODIFIER-RESERVED-15-MASK = 32768;
define constant $GDK-MODIFIER-RESERVED-16-MASK = 65536;
define constant $GDK-MODIFIER-RESERVED-17-MASK = 131072;
define constant $GDK-MODIFIER-RESERVED-18-MASK = 262144;
define constant $GDK-MODIFIER-RESERVED-19-MASK = 524288;
define constant $GDK-MODIFIER-RESERVED-20-MASK = 1048576;
define constant $GDK-MODIFIER-RESERVED-21-MASK = 2097152;
define constant $GDK-MODIFIER-RESERVED-22-MASK = 4194304;
define constant $GDK-MODIFIER-RESERVED-23-MASK = 8388608;
define constant $GDK-MODIFIER-RESERVED-24-MASK = 16777216;
define constant $GDK-MODIFIER-RESERVED-25-MASK = 33554432;
define constant $GDK-SUPER-MASK = 67108864;
define constant $GDK-HYPER-MASK = 134217728;
define constant $GDK-META-MASK = 268435456;
define constant $GDK-MODIFIER-RESERVED-29-MASK = 536870912;
define constant $GDK-RELEASE-MASK = 1073741824;
define constant $GDK-MODIFIER-MASK = 1543512063;
define constant <GdkModifierType> = <C-int>;
define C-pointer-type <GdkModifierType*> => <GdkModifierType>;

define constant $GDK-NOTIFY-ANCESTOR = 0;
define constant $GDK-NOTIFY-VIRTUAL = 1;
define constant $GDK-NOTIFY-INFERIOR = 2;
define constant $GDK-NOTIFY-NONLINEAR = 3;
define constant $GDK-NOTIFY-NONLINEAR-VIRTUAL = 4;
define constant $GDK-NOTIFY-UNKNOWN = 5;
define constant <GdkNotifyType> = <C-int>;
define C-pointer-type <GdkNotifyType*> => <GdkNotifyType>;

define constant $GDK-OWNER-CHANGE-NEW-OWNER = 0;
define constant $GDK-OWNER-CHANGE-DESTROY = 1;
define constant $GDK-OWNER-CHANGE-CLOSE = 2;
define constant <GdkOwnerChange> = <C-int>;
define C-pointer-type <GdkOwnerChange*> => <GdkOwnerChange>;

define constant $PARENT-RELATIVE = 1;

define constant $PRIORITY-REDRAW = 20;

define C-struct <_GdkPoint>
  slot gdkpoint-x :: <C-signed-int>;
  slot gdkpoint-y :: <C-signed-int>;
  pointer-type-name: <GdkPoint>;
end C-struct;

define constant $GDK-PROP-MODE-REPLACE = 0;
define constant $GDK-PROP-MODE-PREPEND = 1;
define constant $GDK-PROP-MODE-APPEND = 2;
define constant <GdkPropMode> = <C-int>;
define C-pointer-type <GdkPropMode*> => <GdkPropMode>;

define constant $GDK-PROPERTY-NEW-VALUE = 0;
define constant $GDK-PROPERTY-DELETE = 1;
define constant <GdkPropertyState> = <C-int>;
define C-pointer-type <GdkPropertyState*> => <GdkPropertyState>;

define C-struct <_GdkRGBA>
  slot gdkrgba-red :: <C-double>;
  slot gdkrgba-green :: <C-double>;
  slot gdkrgba-blue :: <C-double>;
  slot gdkrgba-alpha :: <C-double>;
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

define property-getter screen-font-options :: <C-void*> on <GdkScreen> end;
define property-setter screen-font-options :: <C-void*> on <GdkScreen> end;
define property-getter screen-resolution :: <C-double> on <GdkScreen> end;
define property-setter screen-resolution :: <C-double> on <GdkScreen> end;
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

define constant $GDK-SCROLL-UP = 0;
define constant $GDK-SCROLL-DOWN = 1;
define constant $GDK-SCROLL-LEFT = 2;
define constant $GDK-SCROLL-RIGHT = 3;
define constant $GDK-SCROLL-SMOOTH = 4;
define constant <GdkScrollDirection> = <C-int>;
define C-pointer-type <GdkScrollDirection*> => <GdkScrollDirection>;

define constant $GDK-SETTING-ACTION-NEW = 0;
define constant $GDK-SETTING-ACTION-CHANGED = 1;
define constant $GDK-SETTING-ACTION-DELETED = 2;
define constant <GdkSettingAction> = <C-int>;
define C-pointer-type <GdkSettingAction*> => <GdkSettingAction>;

define constant $GDK-OK = 0;
define constant $GDK-ERROR = -1;
define constant $GDK-ERROR-PARAM = -2;
define constant $GDK-ERROR-FILE = -3;
define constant $GDK-ERROR-MEM = -4;
define constant <GdkStatus> = <C-int>;
define C-pointer-type <GdkStatus*> => <GdkStatus>;

define C-struct <_GdkTimeCoord>
  slot gdktimecoord-time :: <C-unsigned-int>;
  slot gdktimecoord-axes :: <C-double*>;
  pointer-type-name: <GdkTimeCoord>;
end C-struct;

define constant $GDK-VISIBILITY-UNOBSCURED = 0;
define constant $GDK-VISIBILITY-PARTIAL = 1;
define constant $GDK-VISIBILITY-FULLY-OBSCURED = 2;
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

define constant $GDK-VISUAL-STATIC-GRAY = 0;
define constant $GDK-VISUAL-GRAYSCALE = 1;
define constant $GDK-VISUAL-STATIC-COLOR = 2;
define constant $GDK-VISUAL-PSEUDO-COLOR = 3;
define constant $GDK-VISUAL-TRUE-COLOR = 4;
define constant $GDK-VISUAL-DIRECT-COLOR = 5;
define constant <GdkVisualType> = <C-int>;
define C-pointer-type <GdkVisualType*> => <GdkVisualType>;

define constant $GDK-DECOR-ALL = 1;
define constant $GDK-DECOR-BORDER = 2;
define constant $GDK-DECOR-RESIZEH = 4;
define constant $GDK-DECOR-TITLE = 8;
define constant $GDK-DECOR-MENU = 16;
define constant $GDK-DECOR-MINIMIZE = 32;
define constant $GDK-DECOR-MAXIMIZE = 64;
define constant <GdkWMDecoration> = <C-int>;
define C-pointer-type <GdkWMDecoration*> => <GdkWMDecoration>;

define constant $GDK-FUNC-ALL = 1;
define constant $GDK-FUNC-RESIZE = 2;
define constant $GDK-FUNC-MOVE = 4;
define constant $GDK-FUNC-MINIMIZE = 8;
define constant $GDK-FUNC-MAXIMIZE = 16;
define constant $GDK-FUNC-CLOSE = 32;
define constant <GdkWMFunction> = <C-int>;
define C-pointer-type <GdkWMFunction*> => <GdkWMFunction>;

define open C-subtype <GdkWindow> (<GObject>)
end C-subtype;

define C-pointer-type <GdkWindow*> => <GdkWindow>;

define property-getter window-cursor :: <GdkCursor> on <GdkWindow> end;
define property-setter window-cursor :: <GdkCursor> on <GdkWindow> end;
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
  slot gdkwindowattr-title :: <C-string>;
  slot gdkwindowattr-event-mask :: <C-signed-int>;
  slot gdkwindowattr-x :: <C-signed-int>;
  slot gdkwindowattr-y :: <C-signed-int>;
  slot gdkwindowattr-width :: <C-signed-int>;
  slot gdkwindowattr-height :: <C-signed-int>;
  slot gdkwindowattr-wclass :: <GdkWindowWindowClass>;
  slot gdkwindowattr-visual :: <GdkVisual>;
  slot gdkwindowattr-window-type :: <GdkWindowType>;
  slot gdkwindowattr-cursor :: <GdkCursor>;
  slot gdkwindowattr-wmclass-name :: <C-string>;
  slot gdkwindowattr-wmclass-class :: <C-string>;
  slot gdkwindowattr-override-redirect :: <C-boolean>;
  slot gdkwindowattr-type-hint :: <GdkWindowTypeHint>;
  pointer-type-name: <GdkWindowAttr>;
end C-struct;

define constant $GDK-WA-TITLE = 2;
define constant $GDK-WA-X = 4;
define constant $GDK-WA-Y = 8;
define constant $GDK-WA-CURSOR = 16;
define constant $GDK-WA-VISUAL = 32;
define constant $GDK-WA-WMCLASS = 64;
define constant $GDK-WA-NOREDIR = 128;
define constant $GDK-WA-TYPE-HINT = 256;
define constant <GdkWindowAttributesType> = <C-int>;
define C-pointer-type <GdkWindowAttributesType*> => <GdkWindowAttributesType>;

define C-struct <_GdkWindowClass>
  constant slot gdkwindowclass-parent-class :: <GObjectClass>;
  constant slot gdkwindowclass-pick-embedded-child :: <C-void*>;
  constant slot gdkwindowclass-to-embedder :: <C-function-pointer>;
  constant slot gdkwindowclass-from-embedder :: <C-function-pointer>;
  constant slot gdkwindowclass-create-surface :: <C-function-pointer>;
  constant slot gdkwindowclass--gdk-reserved1 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved2 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved3 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved4 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved5 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved6 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved7 :: <C-void*>;
  constant slot gdkwindowclass--gdk-reserved8 :: <C-void*>;
  pointer-type-name: <GdkWindowClass>;
end C-struct;

define constant $GDK-WINDOW-EDGE-NORTH-WEST = 0;
define constant $GDK-WINDOW-EDGE-NORTH = 1;
define constant $GDK-WINDOW-EDGE-NORTH-EAST = 2;
define constant $GDK-WINDOW-EDGE-WEST = 3;
define constant $GDK-WINDOW-EDGE-EAST = 4;
define constant $GDK-WINDOW-EDGE-SOUTH-WEST = 5;
define constant $GDK-WINDOW-EDGE-SOUTH = 6;
define constant $GDK-WINDOW-EDGE-SOUTH-EAST = 7;
define constant <GdkWindowEdge> = <C-int>;
define C-pointer-type <GdkWindowEdge*> => <GdkWindowEdge>;

define constant $GDK-HINT-POS = 1;
define constant $GDK-HINT-MIN-SIZE = 2;
define constant $GDK-HINT-MAX-SIZE = 4;
define constant $GDK-HINT-BASE-SIZE = 8;
define constant $GDK-HINT-ASPECT = 16;
define constant $GDK-HINT-RESIZE-INC = 32;
define constant $GDK-HINT-WIN-GRAVITY = 64;
define constant $GDK-HINT-USER-POS = 128;
define constant $GDK-HINT-USER-SIZE = 256;
define constant <GdkWindowHints> = <C-int>;
define C-pointer-type <GdkWindowHints*> => <GdkWindowHints>;

define C-struct <_GdkWindowRedirect>
  pointer-type-name: <GdkWindowRedirect>;
end C-struct;

define constant $GDK-WINDOW-STATE-WITHDRAWN = 1;
define constant $GDK-WINDOW-STATE-ICONIFIED = 2;
define constant $GDK-WINDOW-STATE-MAXIMIZED = 4;
define constant $GDK-WINDOW-STATE-STICKY = 8;
define constant $GDK-WINDOW-STATE-FULLSCREEN = 16;
define constant $GDK-WINDOW-STATE-ABOVE = 32;
define constant $GDK-WINDOW-STATE-BELOW = 64;
define constant $GDK-WINDOW-STATE-FOCUSED = 128;
define constant <GdkWindowState> = <C-int>;
define C-pointer-type <GdkWindowState*> => <GdkWindowState>;

define constant $GDK-WINDOW-ROOT = 0;
define constant $GDK-WINDOW-TOPLEVEL = 1;
define constant $GDK-WINDOW-CHILD = 2;
define constant $GDK-WINDOW-TEMP = 3;
define constant $GDK-WINDOW-FOREIGN = 4;
define constant $GDK-WINDOW-OFFSCREEN = 5;
define constant <GdkWindowType> = <C-int>;
define C-pointer-type <GdkWindowType*> => <GdkWindowType>;

define constant $GDK-WINDOW-TYPE-HINT-NORMAL = 0;
define constant $GDK-WINDOW-TYPE-HINT-DIALOG = 1;
define constant $GDK-WINDOW-TYPE-HINT-MENU = 2;
define constant $GDK-WINDOW-TYPE-HINT-TOOLBAR = 3;
define constant $GDK-WINDOW-TYPE-HINT-SPLASHSCREEN = 4;
define constant $GDK-WINDOW-TYPE-HINT-UTILITY = 5;
define constant $GDK-WINDOW-TYPE-HINT-DOCK = 6;
define constant $GDK-WINDOW-TYPE-HINT-DESKTOP = 7;
define constant $GDK-WINDOW-TYPE-HINT-DROPDOWN-MENU = 8;
define constant $GDK-WINDOW-TYPE-HINT-POPUP-MENU = 9;
define constant $GDK-WINDOW-TYPE-HINT-TOOLTIP = 10;
define constant $GDK-WINDOW-TYPE-HINT-NOTIFICATION = 11;
define constant $GDK-WINDOW-TYPE-HINT-COMBO = 12;
define constant $GDK-WINDOW-TYPE-HINT-DND = 13;
define constant <GdkWindowTypeHint> = <C-int>;
define C-pointer-type <GdkWindowTypeHint*> => <GdkWindowTypeHint>;

define constant $GDK-INPUT-OUTPUT = 0;
define constant $GDK-INPUT-ONLY = 1;
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

