module: gio
synopsis: generated bindings for the Gio library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

// Interface
define open C-subtype <GAction> (<C-void*>)
end C-subtype;

define C-pointer-type <GAction*> => <GAction>;

define C-function g-action-activate
  input parameter self :: <GAction>;
  input parameter parameter_ :: <GVariant>;
  c-name: "g_action_activate";
end;

define C-function g-action-change-state
  input parameter self :: <GAction>;
  input parameter value_ :: <GVariant>;
  c-name: "g_action_change_state";
end;

define C-function g-action-get-enabled
  input parameter self :: <GAction>;
  result res :: <C-boolean>;
  c-name: "g_action_get_enabled";
end;

define C-function g-action-get-name
  input parameter self :: <GAction>;
  result res :: <C-string>;
  c-name: "g_action_get_name";
end;

define C-function g-action-get-parameter-type
  input parameter self :: <GAction>;
  result res :: <GVariantType>;
  c-name: "g_action_get_parameter_type";
end;

define C-function g-action-get-state
  input parameter self :: <GAction>;
  result res :: <GVariant>;
  c-name: "g_action_get_state";
end;

define C-function g-action-get-state-hint
  input parameter self :: <GAction>;
  result res :: <GVariant>;
  c-name: "g_action_get_state_hint";
end;

define C-function g-action-get-state-type
  input parameter self :: <GAction>;
  result res :: <GVariantType>;
  c-name: "g_action_get_state_type";
end;

define C-struct <_GActionEntry>
  slot gactionentry-name :: <C-string>;
  constant slot gactionentry-activate :: <C-function-pointer>;
  slot gactionentry-parameter-type :: <C-string>;
  slot gactionentry-state :: <C-string>;
  constant slot gactionentry-change-state :: <C-function-pointer>;
  constant slot gactionentry-padding :: <C-unsigned-long*>;
  pointer-type-name: <GActionEntry>;
end C-struct;

// Interface
define open C-subtype <GActionGroup> (<C-void*>)
end C-subtype;

define C-pointer-type <GActionGroup*> => <GActionGroup>;

define C-function g-action-group-action-added
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  c-name: "g_action_group_action_added";
end;

define C-function g-action-group-action-enabled-changed
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter enabled_ :: <C-boolean>;
  c-name: "g_action_group_action_enabled_changed";
end;

define C-function g-action-group-action-removed
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  c-name: "g_action_group_action_removed";
end;

define C-function g-action-group-action-state-changed
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter state_ :: <GVariant>;
  c-name: "g_action_group_action_state_changed";
end;

define C-function g-action-group-activate-action
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter parameter_ :: <GVariant>;
  c-name: "g_action_group_activate_action";
end;

define C-function g-action-group-change-action-state
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  c-name: "g_action_group_change_action_state";
end;

define C-function g-action-group-get-action-enabled
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_action_group_get_action_enabled";
end;

define C-function g-action-group-get-action-parameter-type
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GVariantType>;
  c-name: "g_action_group_get_action_parameter_type";
end;

define C-function g-action-group-get-action-state
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_action_group_get_action_state";
end;

define C-function g-action-group-get-action-state-hint
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_action_group_get_action_state_hint";
end;

define C-function g-action-group-get-action-state-type
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GVariantType>;
  c-name: "g_action_group_get_action_state_type";
end;

define C-function g-action-group-has-action
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_action_group_has_action";
end;

define C-function g-action-group-list-actions
  input parameter self :: <GActionGroup>;
  result res :: <C-string*>;
  c-name: "g_action_group_list_actions";
end;

define C-function g-action-group-query-action
  input parameter self :: <GActionGroup>;
  input parameter action_name_ :: <C-string>;
  output parameter enabled_ :: <C-int*>;
  output parameter parameter_type_ :: <GVariantType>;
  output parameter state_type_ :: <GVariantType>;
  output parameter state_hint_ :: <GVariant>;
  output parameter state_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_action_group_query_action";
end;

define C-struct <_GActionGroupInterface>
  constant slot gactiongroupinterface-g-iface :: <GTypeInterface>;
  constant slot gactiongroupinterface-has-action :: <C-function-pointer>;
  constant slot gactiongroupinterface-list-actions :: <C-function-pointer>;
  constant slot gactiongroupinterface-get-action-enabled :: <C-function-pointer>;
  constant slot gactiongroupinterface-get-action-parameter-type :: <C-function-pointer>;
  constant slot gactiongroupinterface-get-action-state-type :: <C-function-pointer>;
  constant slot gactiongroupinterface-get-action-state-hint :: <C-function-pointer>;
  constant slot gactiongroupinterface-get-action-state :: <C-function-pointer>;
  constant slot gactiongroupinterface-change-action-state :: <C-function-pointer>;
  constant slot gactiongroupinterface-activate-action :: <C-function-pointer>;
  constant slot gactiongroupinterface-action-added :: <C-function-pointer>;
  constant slot gactiongroupinterface-action-removed :: <C-function-pointer>;
  constant slot gactiongroupinterface-action-enabled-changed :: <C-function-pointer>;
  constant slot gactiongroupinterface-action-state-changed :: <C-function-pointer>;
  constant slot gactiongroupinterface-query-action :: <C-function-pointer>;
  pointer-type-name: <GActionGroupInterface>;
end C-struct;

define C-struct <_GActionInterface>
  constant slot gactioninterface-g-iface :: <GTypeInterface>;
  constant slot gactioninterface-get-name :: <C-function-pointer>;
  constant slot gactioninterface-get-parameter-type :: <C-function-pointer>;
  constant slot gactioninterface-get-state-type :: <C-function-pointer>;
  constant slot gactioninterface-get-state-hint :: <C-function-pointer>;
  constant slot gactioninterface-get-enabled :: <C-function-pointer>;
  constant slot gactioninterface-get-state :: <C-function-pointer>;
  constant slot gactioninterface-change-state :: <C-function-pointer>;
  constant slot gactioninterface-activate :: <C-function-pointer>;
  pointer-type-name: <GActionInterface>;
end C-struct;

// Interface
define open C-subtype <GActionMap> (<GActionGroup>)
end C-subtype;

define C-pointer-type <GActionMap*> => <GActionMap>;

define C-function g-action-map-add-action
  input parameter self :: <GActionMap>;
  input parameter action_ :: <GAction>;
  c-name: "g_action_map_add_action";
end;

define C-function g-action-map-add-action-entries
  input parameter self :: <GActionMap>;
  input parameter entries_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_entries_ :: <C-signed-int>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_action_map_add_action_entries";
end;

define C-function g-action-map-lookup-action
  input parameter self :: <GActionMap>;
  input parameter action_name_ :: <C-string>;
  result res :: <GAction>;
  c-name: "g_action_map_lookup_action";
end;

define C-function g-action-map-remove-action
  input parameter self :: <GActionMap>;
  input parameter action_name_ :: <C-string>;
  c-name: "g_action_map_remove_action";
end;

define C-struct <_GActionMapInterface>
  constant slot gactionmapinterface-g-iface :: <GTypeInterface>;
  constant slot gactionmapinterface-lookup-action :: <C-function-pointer>;
  constant slot gactionmapinterface-add-action :: <C-function-pointer>;
  constant slot gactionmapinterface-remove-action :: <C-function-pointer>;
  pointer-type-name: <GActionMapInterface>;
end C-struct;

// Interface
define open C-subtype <GAppInfo> (<C-void*>)
end C-subtype;

define C-pointer-type <GAppInfo*> => <GAppInfo>;

define C-function g-app-info-create-from-commandline
  input parameter commandline_ :: <C-string>;
  input parameter application_name_ :: <C-string>;
  input parameter flags_ :: <GAppInfoCreateFlags>;
  result res :: <GAppInfo>;
  c-name: "g_app_info_create_from_commandline";
end;

define C-function g-app-info-get-all
  result res :: <GList>;
  c-name: "g_app_info_get_all";
end;

define C-function g-app-info-get-all-for-type
  input parameter content_type_ :: <C-string>;
  result res :: <GList>;
  c-name: "g_app_info_get_all_for_type";
end;

define C-function g-app-info-get-default-for-type
  input parameter content_type_ :: <C-string>;
  input parameter must_support_uris_ :: <C-boolean>;
  result res :: <GAppInfo>;
  c-name: "g_app_info_get_default_for_type";
end;

define C-function g-app-info-get-default-for-uri-scheme
  input parameter uri_scheme_ :: <C-string>;
  result res :: <GAppInfo>;
  c-name: "g_app_info_get_default_for_uri_scheme";
end;

define C-function g-app-info-get-fallback-for-type
  input parameter content_type_ :: <C-string>;
  result res :: <GList>;
  c-name: "g_app_info_get_fallback_for_type";
end;

define C-function g-app-info-get-recommended-for-type
  input parameter content_type_ :: <C-string>;
  result res :: <GList>;
  c-name: "g_app_info_get_recommended_for_type";
end;

define C-function g-app-info-launch-default-for-uri
  input parameter uri_ :: <C-string>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  result res :: <C-boolean>;
  c-name: "g_app_info_launch_default_for_uri";
end;

define C-function g-app-info-reset-type-associations
  input parameter content_type_ :: <C-string>;
  c-name: "g_app_info_reset_type_associations";
end;

define C-function g-app-info-add-supports-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_app_info_add_supports_type";
end;

define C-function g-app-info-can-delete
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_can_delete";
end;

define C-function g-app-info-can-remove-supports-type
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_can_remove_supports_type";
end;

define C-function g-app-info-delete
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_delete";
end;

define C-function g-app-info-dup
  input parameter self :: <GAppInfo>;
  result res :: <GAppInfo>;
  c-name: "g_app_info_dup";
end;

define C-function g-app-info-equal
  input parameter self :: <GAppInfo>;
  input parameter appinfo2_ :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_equal";
end;

define C-function g-app-info-get-commandline
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_commandline";
end;

define C-function g-app-info-get-description
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_description";
end;

define C-function g-app-info-get-display-name
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_display_name";
end;

define C-function g-app-info-get-executable
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_executable";
end;

define C-function g-app-info-get-icon
  input parameter self :: <GAppInfo>;
  result res :: <GIcon>;
  c-name: "g_app_info_get_icon";
end;

define C-function g-app-info-get-id
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_id";
end;

define C-function g-app-info-get-name
  input parameter self :: <GAppInfo>;
  result res :: <C-string>;
  c-name: "g_app_info_get_name";
end;

define C-function g-app-info-get-supported-types
  input parameter self :: <GAppInfo>;
  result res :: <C-string*>;
  c-name: "g_app_info_get_supported_types";
end;

define C-function g-app-info-launch
  input parameter self :: <GAppInfo>;
  input parameter files_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  result res :: <C-boolean>;
  c-name: "g_app_info_launch";
end;

define C-function g-app-info-launch-uris
  input parameter self :: <GAppInfo>;
  input parameter uris_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  result res :: <C-boolean>;
  c-name: "g_app_info_launch_uris";
end;

define C-function g-app-info-remove-supports-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_app_info_remove_supports_type";
end;

define C-function g-app-info-set-as-default-for-extension
  input parameter self :: <GAppInfo>;
  input parameter extension_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_app_info_set_as_default_for_extension";
end;

define C-function g-app-info-set-as-default-for-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_app_info_set_as_default_for_type";
end;

define C-function g-app-info-set-as-last-used-for-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_app_info_set_as_last_used_for_type";
end;

define C-function g-app-info-should-show
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_should_show";
end;

define C-function g-app-info-supports-files
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_supports_files";
end;

define C-function g-app-info-supports-uris
  input parameter self :: <GAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_app_info_supports_uris";
end;

define constant $G-APP-INFO-CREATE-NONE = 0;
define constant $G-APP-INFO-CREATE-NEEDS-TERMINAL = 1;
define constant $G-APP-INFO-CREATE-SUPPORTS-URIS = 2;
define constant $G-APP-INFO-CREATE-SUPPORTS-STARTUP-NOTIFICATION = 4;
define constant <GAppInfoCreateFlags> = <C-int>;
define C-pointer-type <GAppInfoCreateFlags*> => <GAppInfoCreateFlags>;

define C-struct <_GAppInfoIface>
  constant slot gappinfoiface-g-iface :: <GTypeInterface>;
  constant slot gappinfoiface-dup :: <C-function-pointer>;
  constant slot gappinfoiface-equal :: <C-function-pointer>;
  constant slot gappinfoiface-get-id :: <C-function-pointer>;
  constant slot gappinfoiface-get-name :: <C-function-pointer>;
  constant slot gappinfoiface-get-description :: <C-function-pointer>;
  constant slot gappinfoiface-get-executable :: <C-function-pointer>;
  constant slot gappinfoiface-get-icon :: <C-function-pointer>;
  constant slot gappinfoiface-launch :: <C-function-pointer>;
  constant slot gappinfoiface-supports-uris :: <C-function-pointer>;
  constant slot gappinfoiface-supports-files :: <C-function-pointer>;
  constant slot gappinfoiface-launch-uris :: <C-function-pointer>;
  constant slot gappinfoiface-should-show :: <C-function-pointer>;
  constant slot gappinfoiface-set-as-default-for-type :: <C-function-pointer>;
  constant slot gappinfoiface-set-as-default-for-extension :: <C-function-pointer>;
  constant slot gappinfoiface-add-supports-type :: <C-function-pointer>;
  constant slot gappinfoiface-can-remove-supports-type :: <C-function-pointer>;
  constant slot gappinfoiface-remove-supports-type :: <C-function-pointer>;
  constant slot gappinfoiface-can-delete :: <C-function-pointer>;
  constant slot gappinfoiface-do-delete :: <C-function-pointer>;
  constant slot gappinfoiface-get-commandline :: <C-function-pointer>;
  constant slot gappinfoiface-get-display-name :: <C-function-pointer>;
  constant slot gappinfoiface-set-as-last-used-for-type :: <C-function-pointer>;
  constant slot gappinfoiface-get-supported-types :: <C-function-pointer>;
  pointer-type-name: <GAppInfoIface>;
end C-struct;

define open C-subtype <GAppLaunchContext> (<GObject>)
  constant slot gapplaunchcontext-parent-instance :: <GObject>;
  constant slot gapplaunchcontext-priv :: <GAppLaunchContextPrivate>;
end C-subtype;

define C-pointer-type <GAppLaunchContext*> => <GAppLaunchContext>;

define C-function g-app-launch-context-new
  result res :: <GAppLaunchContext>;
  c-name: "g_app_launch_context_new";
end;

define C-function g-app-launch-context-get-display
  input parameter self :: <GAppLaunchContext>;
  input parameter info_ :: <GAppInfo>;
  input parameter files_ :: <GList>;
  result res :: <C-string>;
  c-name: "g_app_launch_context_get_display";
end;

define C-function g-app-launch-context-get-environment
  input parameter self :: <GAppLaunchContext>;
  result res :: <C-string*>;
  c-name: "g_app_launch_context_get_environment";
end;

define C-function g-app-launch-context-get-startup-notify-id
  input parameter self :: <GAppLaunchContext>;
  input parameter info_ :: <GAppInfo>;
  input parameter files_ :: <GList>;
  result res :: <C-string>;
  c-name: "g_app_launch_context_get_startup_notify_id";
end;

define C-function g-app-launch-context-launch-failed
  input parameter self :: <GAppLaunchContext>;
  input parameter startup_notify_id_ :: <C-string>;
  c-name: "g_app_launch_context_launch_failed";
end;

define C-function g-app-launch-context-setenv
  input parameter self :: <GAppLaunchContext>;
  input parameter variable_ :: <C-string>;
  input parameter value_ :: <C-string>;
  c-name: "g_app_launch_context_setenv";
end;

define C-function g-app-launch-context-unsetenv
  input parameter self :: <GAppLaunchContext>;
  input parameter variable_ :: <C-string>;
  c-name: "g_app_launch_context_unsetenv";
end;

define C-struct <_GAppLaunchContextClass>
  constant slot gapplaunchcontextclass-parent-class :: <GObjectClass>;
  constant slot gapplaunchcontextclass-get-display :: <C-function-pointer>;
  constant slot gapplaunchcontextclass-get-startup-notify-id :: <C-function-pointer>;
  constant slot gapplaunchcontextclass-launch-failed :: <C-function-pointer>;
  constant slot gapplaunchcontextclass-launched :: <C-function-pointer>;
  constant slot gapplaunchcontextclass--g-reserved1 :: <C-void*>;
  constant slot gapplaunchcontextclass--g-reserved2 :: <C-void*>;
  constant slot gapplaunchcontextclass--g-reserved3 :: <C-void*>;
  constant slot gapplaunchcontextclass--g-reserved4 :: <C-void*>;
  pointer-type-name: <GAppLaunchContextClass>;
end C-struct;

define C-struct <_GAppLaunchContextPrivate>
  pointer-type-name: <GAppLaunchContextPrivate>;
end C-struct;

define open C-subtype <GApplication> (<GObject>)
  constant slot gapplication-parent-instance :: <GObject>;
  constant slot gapplication-priv :: <GApplicationPrivate>;
end C-subtype;

define C-pointer-type <GApplication*> => <GApplication>;

define property-setter application-action-group :: <GActionGroup> on <GApplication> end;
define property-getter application-application-id :: <C-string> on <GApplication> end;
define property-setter application-application-id :: <C-string> on <GApplication> end;
define property-getter application-flags :: <GApplicationFlags> on <GApplication> end;
define property-setter application-flags :: <GApplicationFlags> on <GApplication> end;
define property-getter application-inactivity-timeout :: <C-unsigned-int> on <GApplication> end;
define property-setter application-inactivity-timeout :: <C-unsigned-int> on <GApplication> end;
define property-getter application-is-registered :: <C-boolean> on <GApplication> end;
define property-getter application-is-remote :: <C-boolean> on <GApplication> end;
define C-function g-application-new
  input parameter application_id_ :: <C-string>;
  input parameter flags_ :: <GApplicationFlags>;
  result res :: <GApplication>;
  c-name: "g_application_new";
end;

define C-function g-application-get-default
  result res :: <GApplication>;
  c-name: "g_application_get_default";
end;

define C-function g-application-id-is-valid
  input parameter application_id_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_application_id_is_valid";
end;

define C-function g-application-activate
  input parameter self :: <GApplication>;
  c-name: "g_application_activate";
end;

define C-function g-application-get-application-id
  input parameter self :: <GApplication>;
  result res :: <C-string>;
  c-name: "g_application_get_application_id";
end;

define C-function g-application-get-dbus-connection
  input parameter self :: <GApplication>;
  result res :: <GDBusConnection>;
  c-name: "g_application_get_dbus_connection";
end;

define C-function g-application-get-dbus-object-path
  input parameter self :: <GApplication>;
  result res :: <C-string>;
  c-name: "g_application_get_dbus_object_path";
end;

define C-function g-application-get-flags
  input parameter self :: <GApplication>;
  result res :: <GApplicationFlags>;
  c-name: "g_application_get_flags";
end;

define C-function g-application-get-inactivity-timeout
  input parameter self :: <GApplication>;
  result res :: <C-unsigned-int>;
  c-name: "g_application_get_inactivity_timeout";
end;

define C-function g-application-get-is-registered
  input parameter self :: <GApplication>;
  result res :: <C-boolean>;
  c-name: "g_application_get_is_registered";
end;

define C-function g-application-get-is-remote
  input parameter self :: <GApplication>;
  result res :: <C-boolean>;
  c-name: "g_application_get_is_remote";
end;

define C-function g-application-hold
  input parameter self :: <GApplication>;
  c-name: "g_application_hold";
end;

define C-function g-application-open
  input parameter self :: <GApplication>;
  input parameter files_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_files_ :: <C-signed-int>;
  input parameter hint_ :: <C-string>;
  c-name: "g_application_open";
end;

define C-function g-application-quit
  input parameter self :: <GApplication>;
  c-name: "g_application_quit";
end;

define C-function g-application-register
  input parameter self :: <GApplication>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_application_register";
end;

define C-function g-application-release
  input parameter self :: <GApplication>;
  c-name: "g_application_release";
end;

define C-function g-application-run
  input parameter self :: <GApplication>;
  input parameter argc_ :: <C-signed-int>;
  input parameter argv_ :: <C-string*>;
  result res :: <C-signed-int>;
  c-name: "g_application_run";
end;

define C-function g-application-set-action-group
  input parameter self :: <GApplication>;
  input parameter action_group_ :: <GActionGroup>;
  c-name: "g_application_set_action_group";
end;

define C-function g-application-set-application-id
  input parameter self :: <GApplication>;
  input parameter application_id_ :: <C-string>;
  c-name: "g_application_set_application_id";
end;

define C-function g-application-set-default
  input parameter self :: <GApplication>;
  c-name: "g_application_set_default";
end;

define C-function g-application-set-flags
  input parameter self :: <GApplication>;
  input parameter flags_ :: <GApplicationFlags>;
  c-name: "g_application_set_flags";
end;

define C-function g-application-set-inactivity-timeout
  input parameter self :: <GApplication>;
  input parameter inactivity_timeout_ :: <C-unsigned-int>;
  c-name: "g_application_set_inactivity_timeout";
end;

define C-struct <_GApplicationClass>
  constant slot gapplicationclass-parent-class :: <GObjectClass>;
  constant slot gapplicationclass-startup :: <C-function-pointer>;
  constant slot gapplicationclass-activate :: <C-function-pointer>;
  constant slot gapplicationclass-open :: <C-function-pointer>;
  constant slot gapplicationclass-command-line :: <C-function-pointer>;
  constant slot gapplicationclass-local-command-line :: <C-function-pointer>;
  constant slot gapplicationclass-before-emit :: <C-function-pointer>;
  constant slot gapplicationclass-after-emit :: <C-function-pointer>;
  constant slot gapplicationclass-add-platform-data :: <C-function-pointer>;
  constant slot gapplicationclass-quit-mainloop :: <C-function-pointer>;
  constant slot gapplicationclass-run-mainloop :: <C-function-pointer>;
  constant slot gapplicationclass-shutdown :: <C-function-pointer>;
  constant slot gapplicationclass-dbus-register :: <C-function-pointer>;
  constant slot gapplicationclass-dbus-unregister :: <C-function-pointer>;
  constant slot gapplicationclass-padding :: <C-void*>;
  pointer-type-name: <GApplicationClass>;
end C-struct;

define open C-subtype <GApplicationCommandLine> (<GObject>)
  constant slot gapplicationcommandline-parent-instance :: <GObject>;
  constant slot gapplicationcommandline-priv :: <GApplicationCommandLinePrivate>;
end C-subtype;

define C-pointer-type <GApplicationCommandLine*> => <GApplicationCommandLine>;

define property-setter applicationcommandline-arguments :: <GVariant> on <GApplicationCommandLine> end;
define property-getter applicationcommandline-is-remote :: <C-boolean> on <GApplicationCommandLine> end;
define property-setter applicationcommandline-platform-data :: <GVariant> on <GApplicationCommandLine> end;
define C-function g-application-command-line-create-file-for-arg
  input parameter self :: <GApplicationCommandLine>;
  input parameter arg_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_application_command_line_create_file_for_arg";
end;

define C-function g-application-command-line-get-arguments
  input parameter self :: <GApplicationCommandLine>;
  output parameter argc_ :: <C-signed-int*>;
  result res :: <C-string*>;
  c-name: "g_application_command_line_get_arguments";
end;

define C-function g-application-command-line-get-cwd
  input parameter self :: <GApplicationCommandLine>;
  result res :: <C-string>;
  c-name: "g_application_command_line_get_cwd";
end;

define C-function g-application-command-line-get-environ
  input parameter self :: <GApplicationCommandLine>;
  result res :: <C-string*>;
  c-name: "g_application_command_line_get_environ";
end;

define C-function g-application-command-line-get-exit-status
  input parameter self :: <GApplicationCommandLine>;
  result res :: <C-signed-int>;
  c-name: "g_application_command_line_get_exit_status";
end;

define C-function g-application-command-line-get-is-remote
  input parameter self :: <GApplicationCommandLine>;
  result res :: <C-boolean>;
  c-name: "g_application_command_line_get_is_remote";
end;

define C-function g-application-command-line-get-platform-data
  input parameter self :: <GApplicationCommandLine>;
  result res :: <GVariant>;
  c-name: "g_application_command_line_get_platform_data";
end;

define C-function g-application-command-line-get-stdin
  input parameter self :: <GApplicationCommandLine>;
  result res :: <GInputStream>;
  c-name: "g_application_command_line_get_stdin";
end;

define C-function g-application-command-line-getenv
  input parameter self :: <GApplicationCommandLine>;
  input parameter name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_application_command_line_getenv";
end;

define C-function g-application-command-line-set-exit-status
  input parameter self :: <GApplicationCommandLine>;
  input parameter exit_status_ :: <C-signed-int>;
  c-name: "g_application_command_line_set_exit_status";
end;

define C-struct <_GApplicationCommandLineClass>
  constant slot gapplicationcommandlineclass-parent-class :: <GObjectClass>;
  constant slot gapplicationcommandlineclass-print-literal :: <C-function-pointer>;
  constant slot gapplicationcommandlineclass-printerr-literal :: <C-function-pointer>;
  constant slot gapplicationcommandlineclass-get-stdin :: <C-function-pointer>;
  constant slot gapplicationcommandlineclass-padding :: <C-void*>;
  pointer-type-name: <GApplicationCommandLineClass>;
end C-struct;

define C-struct <_GApplicationCommandLinePrivate>
  pointer-type-name: <GApplicationCommandLinePrivate>;
end C-struct;

define constant $G-APPLICATION-FLAGS-NONE = 0;
define constant $G-APPLICATION-IS-SERVICE = 1;
define constant $G-APPLICATION-IS-LAUNCHER = 2;
define constant $G-APPLICATION-HANDLES-OPEN = 4;
define constant $G-APPLICATION-HANDLES-COMMAND-LINE = 8;
define constant $G-APPLICATION-SEND-ENVIRONMENT = 16;
define constant $G-APPLICATION-NON-UNIQUE = 32;
define constant <GApplicationFlags> = <C-int>;
define C-pointer-type <GApplicationFlags*> => <GApplicationFlags>;

define C-struct <_GApplicationPrivate>
  pointer-type-name: <GApplicationPrivate>;
end C-struct;

define constant $G-ASK-PASSWORD-NEED-PASSWORD = 1;
define constant $G-ASK-PASSWORD-NEED-USERNAME = 2;
define constant $G-ASK-PASSWORD-NEED-DOMAIN = 4;
define constant $G-ASK-PASSWORD-SAVING-SUPPORTED = 8;
define constant $G-ASK-PASSWORD-ANONYMOUS-SUPPORTED = 16;
define constant <GAskPasswordFlags> = <C-int>;
define C-pointer-type <GAskPasswordFlags*> => <GAskPasswordFlags>;

// Interface
define open C-subtype <GAsyncInitable> (<C-void*>)
end C-subtype;

define C-pointer-type <GAsyncInitable*> => <GAsyncInitable>;

define C-function g-async-initable-newv-async
  input parameter object_type_ :: <C-long>;
  input parameter n_parameters_ :: <C-unsigned-int>;
  input parameter parameters_ :: <GParameter>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_async_initable_newv_async";
end;

define C-function g-async-initable-init-async
  input parameter self :: <GAsyncInitable>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_async_initable_init_async";
end;

define C-function g-async-initable-init-finish
  input parameter self :: <GAsyncInitable>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_async_initable_init_finish";
end;

define C-function g-async-initable-new-finish
  input parameter self :: <GAsyncInitable>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GObject>;
  c-name: "g_async_initable_new_finish";
end;

define C-struct <_GAsyncInitableIface>
  constant slot gasyncinitableiface-g-iface :: <GTypeInterface>;
  constant slot gasyncinitableiface-init-async :: <C-function-pointer>;
  constant slot gasyncinitableiface-init-finish :: <C-function-pointer>;
  pointer-type-name: <GAsyncInitableIface>;
end C-struct;

// Interface
define open C-subtype <GAsyncResult> (<C-void*>)
end C-subtype;

define C-pointer-type <GAsyncResult*> => <GAsyncResult>;

define C-function g-async-result-get-source-object
  input parameter self :: <GAsyncResult>;
  result res :: <GObject>;
  c-name: "g_async_result_get_source_object";
end;

define C-function g-async-result-get-user-data
  input parameter self :: <GAsyncResult>;
  result res :: <C-void*>;
  c-name: "g_async_result_get_user_data";
end;

define C-function g-async-result-is-tagged
  input parameter self :: <GAsyncResult>;
  input parameter source_tag_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_async_result_is_tagged";
end;

define C-function g-async-result-legacy-propagate-error
  input parameter self :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_async_result_legacy_propagate_error";
end;

define C-struct <_GAsyncResultIface>
  constant slot gasyncresultiface-g-iface :: <GTypeInterface>;
  constant slot gasyncresultiface-get-user-data :: <C-function-pointer>;
  constant slot gasyncresultiface-get-source-object :: <C-function-pointer>;
  constant slot gasyncresultiface-is-tagged :: <C-function-pointer>;
  pointer-type-name: <GAsyncResultIface>;
end C-struct;

define open C-subtype <GBufferedInputStream> (<GFilterInputStream>)
  constant slot gbufferedinputstream-parent-instance :: <GFilterInputStream>;
  constant slot gbufferedinputstream-priv :: <GBufferedInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GBufferedInputStream*> => <GBufferedInputStream>;

define property-getter bufferedinputstream-buffer-size :: <C-unsigned-int> on <GBufferedInputStream> end;
define property-setter bufferedinputstream-buffer-size :: <C-unsigned-int> on <GBufferedInputStream> end;
define C-function g-buffered-input-stream-new
  input parameter base_stream_ :: <GInputStream>;
  result res :: <GInputStream>;
  c-name: "g_buffered_input_stream_new";
end;

define C-function g-buffered-input-stream-new-sized
  input parameter base_stream_ :: <GInputStream>;
  input parameter size_ :: <C-unsigned-long>;
  result res :: <GInputStream>;
  c-name: "g_buffered_input_stream_new_sized";
end;

define C-function g-buffered-input-stream-fill
  input parameter self :: <GBufferedInputStream>;
  input parameter count_ :: <C-signed-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_buffered_input_stream_fill";
end;

define C-function g-buffered-input-stream-fill-async
  input parameter self :: <GBufferedInputStream>;
  input parameter count_ :: <C-signed-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_buffered_input_stream_fill_async";
end;

define C-function g-buffered-input-stream-fill-finish
  input parameter self :: <GBufferedInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_buffered_input_stream_fill_finish";
end;

define C-function g-buffered-input-stream-get-available
  input parameter self :: <GBufferedInputStream>;
  result res :: <C-unsigned-long>;
  c-name: "g_buffered_input_stream_get_available";
end;

define C-function g-buffered-input-stream-get-buffer-size
  input parameter self :: <GBufferedInputStream>;
  result res :: <C-unsigned-long>;
  c-name: "g_buffered_input_stream_get_buffer_size";
end;

define C-function g-buffered-input-stream-peek
  input parameter self :: <GBufferedInputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter offset_ :: <C-unsigned-long>;
  input parameter count_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_buffered_input_stream_peek";
end;

define C-function g-buffered-input-stream-peek-buffer
  input parameter self :: <GBufferedInputStream>;
  output parameter count_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_buffered_input_stream_peek_buffer";
end;

define C-function g-buffered-input-stream-read-byte
  input parameter self :: <GBufferedInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-int>;
  c-name: "g_buffered_input_stream_read_byte";
end;

define C-function g-buffered-input-stream-set-buffer-size
  input parameter self :: <GBufferedInputStream>;
  input parameter size_ :: <C-unsigned-long>;
  c-name: "g_buffered_input_stream_set_buffer_size";
end;

define C-struct <_GBufferedInputStreamClass>
  constant slot gbufferedinputstreamclass-parent-class :: <GFilterInputStreamClass>;
  constant slot gbufferedinputstreamclass-fill :: <C-function-pointer>;
  constant slot gbufferedinputstreamclass-fill-async :: <C-function-pointer>;
  constant slot gbufferedinputstreamclass-fill-finish :: <C-function-pointer>;
  constant slot gbufferedinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gbufferedinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gbufferedinputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gbufferedinputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gbufferedinputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GBufferedInputStreamClass>;
end C-struct;

define C-struct <_GBufferedInputStreamPrivate>
  pointer-type-name: <GBufferedInputStreamPrivate>;
end C-struct;

define open C-subtype <GBufferedOutputStream> (<GFilterOutputStream>)
  constant slot gbufferedoutputstream-parent-instance :: <GFilterOutputStream>;
  constant slot gbufferedoutputstream-priv :: <GBufferedOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GBufferedOutputStream*> => <GBufferedOutputStream>;

define property-getter bufferedoutputstream-auto-grow :: <C-boolean> on <GBufferedOutputStream> end;
define property-setter bufferedoutputstream-auto-grow :: <C-boolean> on <GBufferedOutputStream> end;
define property-getter bufferedoutputstream-buffer-size :: <C-unsigned-int> on <GBufferedOutputStream> end;
define property-setter bufferedoutputstream-buffer-size :: <C-unsigned-int> on <GBufferedOutputStream> end;
define C-function g-buffered-output-stream-new
  input parameter base_stream_ :: <GOutputStream>;
  result res :: <GOutputStream>;
  c-name: "g_buffered_output_stream_new";
end;

define C-function g-buffered-output-stream-new-sized
  input parameter base_stream_ :: <GOutputStream>;
  input parameter size_ :: <C-unsigned-long>;
  result res :: <GOutputStream>;
  c-name: "g_buffered_output_stream_new_sized";
end;

define C-function g-buffered-output-stream-get-auto-grow
  input parameter self :: <GBufferedOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_buffered_output_stream_get_auto_grow";
end;

define C-function g-buffered-output-stream-get-buffer-size
  input parameter self :: <GBufferedOutputStream>;
  result res :: <C-unsigned-long>;
  c-name: "g_buffered_output_stream_get_buffer_size";
end;

define C-function g-buffered-output-stream-set-auto-grow
  input parameter self :: <GBufferedOutputStream>;
  input parameter auto_grow_ :: <C-boolean>;
  c-name: "g_buffered_output_stream_set_auto_grow";
end;

define C-function g-buffered-output-stream-set-buffer-size
  input parameter self :: <GBufferedOutputStream>;
  input parameter size_ :: <C-unsigned-long>;
  c-name: "g_buffered_output_stream_set_buffer_size";
end;

define C-struct <_GBufferedOutputStreamClass>
  constant slot gbufferedoutputstreamclass-parent-class :: <GFilterOutputStreamClass>;
  constant slot gbufferedoutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gbufferedoutputstreamclass--g-reserved2 :: <C-void*>;
  pointer-type-name: <GBufferedOutputStreamClass>;
end C-struct;

define C-struct <_GBufferedOutputStreamPrivate>
  pointer-type-name: <GBufferedOutputStreamPrivate>;
end C-struct;

define constant $G-BUS-NAME-OWNER-FLAGS-NONE = 0;
define constant $G-BUS-NAME-OWNER-FLAGS-ALLOW-REPLACEMENT = 1;
define constant $G-BUS-NAME-OWNER-FLAGS-REPLACE = 2;
define constant <GBusNameOwnerFlags> = <C-int>;
define C-pointer-type <GBusNameOwnerFlags*> => <GBusNameOwnerFlags>;

define constant $G-BUS-NAME-WATCHER-FLAGS-NONE = 0;
define constant $G-BUS-NAME-WATCHER-FLAGS-AUTO-START = 1;
define constant <GBusNameWatcherFlags> = <C-int>;
define C-pointer-type <GBusNameWatcherFlags*> => <GBusNameWatcherFlags>;

define constant $G-BUS-TYPE-STARTER = -1;
define constant $G-BUS-TYPE-NONE = 0;
define constant $G-BUS-TYPE-SYSTEM = 1;
define constant $G-BUS-TYPE-SESSION = 2;
define constant <GBusType> = <C-int>;
define C-pointer-type <GBusType*> => <GBusType>;

define open C-subtype <GCancellable> (<GObject>)
  constant slot gcancellable-parent-instance :: <GObject>;
  constant slot gcancellable-priv :: <GCancellablePrivate>;
end C-subtype;

define C-pointer-type <GCancellable*> => <GCancellable>;

define C-function g-cancellable-new
  result res :: <GCancellable>;
  c-name: "g_cancellable_new";
end;

define C-function g-cancellable-get-current
  result res :: <GCancellable>;
  c-name: "g_cancellable_get_current";
end;

define C-function g-cancellable-cancel
  input parameter self :: <GCancellable>;
  c-name: "g_cancellable_cancel";
end;

define C-function g-cancellable-connect
  input parameter self :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter data_destroy_func_ :: <C-function-pointer>;
  result res :: <C-unsigned-long>;
  c-name: "g_cancellable_connect";
end;

define C-function g-cancellable-disconnect
  input parameter self :: <GCancellable>;
  input parameter handler_id_ :: <C-unsigned-long>;
  c-name: "g_cancellable_disconnect";
end;

define C-function g-cancellable-get-fd
  input parameter self :: <GCancellable>;
  result res :: <C-signed-int>;
  c-name: "g_cancellable_get_fd";
end;

define C-function g-cancellable-is-cancelled
  input parameter self :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_cancellable_is_cancelled";
end;

define C-function g-cancellable-make-pollfd
  input parameter self :: <GCancellable>;
  input parameter pollfd_ :: <GPollFD>;
  result res :: <C-boolean>;
  c-name: "g_cancellable_make_pollfd";
end;

define C-function g-cancellable-pop-current
  input parameter self :: <GCancellable>;
  c-name: "g_cancellable_pop_current";
end;

define C-function g-cancellable-push-current
  input parameter self :: <GCancellable>;
  c-name: "g_cancellable_push_current";
end;

define C-function g-cancellable-release-fd
  input parameter self :: <GCancellable>;
  c-name: "g_cancellable_release_fd";
end;

define C-function g-cancellable-reset
  input parameter self :: <GCancellable>;
  c-name: "g_cancellable_reset";
end;

define C-function g-cancellable-set-error-if-cancelled
  input parameter self :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_cancellable_set_error_if_cancelled";
end;

define C-struct <_GCancellableClass>
  constant slot gcancellableclass-parent-class :: <GObjectClass>;
  constant slot gcancellableclass-cancelled :: <C-function-pointer>;
  constant slot gcancellableclass--g-reserved1 :: <C-void*>;
  constant slot gcancellableclass--g-reserved2 :: <C-void*>;
  constant slot gcancellableclass--g-reserved3 :: <C-void*>;
  constant slot gcancellableclass--g-reserved4 :: <C-void*>;
  constant slot gcancellableclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GCancellableClass>;
end C-struct;

define C-struct <_GCancellablePrivate>
  pointer-type-name: <GCancellablePrivate>;
end C-struct;

define open C-subtype <GCharsetConverter> (<GObject>)
end C-subtype;

define C-pointer-type <GCharsetConverter*> => <GCharsetConverter>;

define property-getter charsetconverter-from-charset :: <C-string> on <GCharsetConverter> end;
define property-setter charsetconverter-from-charset :: <C-string> on <GCharsetConverter> end;
define property-getter charsetconverter-to-charset :: <C-string> on <GCharsetConverter> end;
define property-setter charsetconverter-to-charset :: <C-string> on <GCharsetConverter> end;
define property-getter charsetconverter-use-fallback :: <C-boolean> on <GCharsetConverter> end;
define property-setter charsetconverter-use-fallback :: <C-boolean> on <GCharsetConverter> end;
define C-function g-charset-converter-new
  input parameter to_charset_ :: <C-string>;
  input parameter from_charset_ :: <C-string>;
  result res :: <GCharsetConverter>;
  c-name: "g_charset_converter_new";
end;

define C-function g-charset-converter-get-num-fallbacks
  input parameter self :: <GCharsetConverter>;
  result res :: <C-unsigned-int>;
  c-name: "g_charset_converter_get_num_fallbacks";
end;

define C-function g-charset-converter-get-use-fallback
  input parameter self :: <GCharsetConverter>;
  result res :: <C-boolean>;
  c-name: "g_charset_converter_get_use_fallback";
end;

define C-function g-charset-converter-set-use-fallback
  input parameter self :: <GCharsetConverter>;
  input parameter use_fallback_ :: <C-boolean>;
  c-name: "g_charset_converter_set_use_fallback";
end;

define C-struct <_GCharsetConverterClass>
  constant slot gcharsetconverterclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GCharsetConverterClass>;
end C-struct;

// Interface
define open C-subtype <GConverter> (<C-void*>)
end C-subtype;

define C-pointer-type <GConverter*> => <GConverter>;

define C-function g-converter-convert
  input parameter self :: <GConverter>;
  input parameter inbuf_ :: <C-unsigned-char*>;
  input parameter inbuf_size_ :: <C-unsigned-long>;
  input parameter outbuf_ :: <C-void*>;
  input parameter outbuf_size_ :: <C-unsigned-long>;
  input parameter flags_ :: <GConverterFlags>;
  output parameter bytes_read_ :: <C-unsigned-long*>;
  output parameter bytes_written_ :: <C-unsigned-long*>;
  result res :: <GConverterResult>;
  c-name: "g_converter_convert";
end;

define C-function g-converter-reset
  input parameter self :: <GConverter>;
  c-name: "g_converter_reset";
end;

define constant $G-CONVERTER-NO-FLAGS = 0;
define constant $G-CONVERTER-INPUT-AT-END = 1;
define constant $G-CONVERTER-FLUSH = 2;
define constant <GConverterFlags> = <C-int>;
define C-pointer-type <GConverterFlags*> => <GConverterFlags>;

define C-struct <_GConverterIface>
  constant slot gconverteriface-g-iface :: <GTypeInterface>;
  constant slot gconverteriface-convert :: <C-function-pointer>;
  constant slot gconverteriface-reset :: <C-function-pointer>;
  pointer-type-name: <GConverterIface>;
end C-struct;

define open C-subtype <GConverterInputStream> (<GFilterInputStream>)
  constant slot gconverterinputstream-parent-instance :: <GFilterInputStream>;
  constant slot gconverterinputstream-priv :: <GConverterInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GConverterInputStream*> => <GConverterInputStream>;

define property-getter converterinputstream-converter :: <GConverter> on <GConverterInputStream> end;
define property-setter converterinputstream-converter :: <GConverter> on <GConverterInputStream> end;
define C-function g-converter-input-stream-new
  input parameter base_stream_ :: <GInputStream>;
  input parameter converter_ :: <GConverter>;
  result res :: <GInputStream>;
  c-name: "g_converter_input_stream_new";
end;

define C-function g-converter-input-stream-get-converter
  input parameter self :: <GConverterInputStream>;
  result res :: <GConverter>;
  c-name: "g_converter_input_stream_get_converter";
end;

define C-struct <_GConverterInputStreamClass>
  constant slot gconverterinputstreamclass-parent-class :: <GFilterInputStreamClass>;
  constant slot gconverterinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gconverterinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gconverterinputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gconverterinputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gconverterinputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GConverterInputStreamClass>;
end C-struct;

define C-struct <_GConverterInputStreamPrivate>
  pointer-type-name: <GConverterInputStreamPrivate>;
end C-struct;

define open C-subtype <GConverterOutputStream> (<GFilterOutputStream>)
  constant slot gconverteroutputstream-parent-instance :: <GFilterOutputStream>;
  constant slot gconverteroutputstream-priv :: <GConverterOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GConverterOutputStream*> => <GConverterOutputStream>;

define property-getter converteroutputstream-converter :: <GConverter> on <GConverterOutputStream> end;
define property-setter converteroutputstream-converter :: <GConverter> on <GConverterOutputStream> end;
define C-function g-converter-output-stream-new
  input parameter base_stream_ :: <GOutputStream>;
  input parameter converter_ :: <GConverter>;
  result res :: <GOutputStream>;
  c-name: "g_converter_output_stream_new";
end;

define C-function g-converter-output-stream-get-converter
  input parameter self :: <GConverterOutputStream>;
  result res :: <GConverter>;
  c-name: "g_converter_output_stream_get_converter";
end;

define C-struct <_GConverterOutputStreamClass>
  constant slot gconverteroutputstreamclass-parent-class :: <GFilterOutputStreamClass>;
  constant slot gconverteroutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gconverteroutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gconverteroutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gconverteroutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gconverteroutputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GConverterOutputStreamClass>;
end C-struct;

define C-struct <_GConverterOutputStreamPrivate>
  pointer-type-name: <GConverterOutputStreamPrivate>;
end C-struct;

define constant $G-CONVERTER-ERROR = 0;
define constant $G-CONVERTER-CONVERTED = 1;
define constant $G-CONVERTER-FINISHED = 2;
define constant $G-CONVERTER-FLUSHED = 3;
define constant <GConverterResult> = <C-int>;
define C-pointer-type <GConverterResult*> => <GConverterResult>;

define open C-subtype <GCredentials> (<GObject>)
end C-subtype;

define C-pointer-type <GCredentials*> => <GCredentials>;

define C-function g-credentials-new
  result res :: <GCredentials>;
  c-name: "g_credentials_new";
end;

define C-function g-credentials-get-unix-pid
  input parameter self :: <GCredentials>;
  result res :: <C-signed-int>;
  c-name: "g_credentials_get_unix_pid";
end;

define C-function g-credentials-get-unix-user
  input parameter self :: <GCredentials>;
  result res :: <C-unsigned-int>;
  c-name: "g_credentials_get_unix_user";
end;

define C-function g-credentials-is-same-user
  input parameter self :: <GCredentials>;
  input parameter other_credentials_ :: <GCredentials>;
  result res :: <C-boolean>;
  c-name: "g_credentials_is_same_user";
end;

define C-function g-credentials-set-native
  input parameter self :: <GCredentials>;
  input parameter native_type_ :: <GCredentialsType>;
  input parameter native_ :: <C-void*>;
  c-name: "g_credentials_set_native";
end;

define C-function g-credentials-set-unix-user
  input parameter self :: <GCredentials>;
  input parameter uid_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_credentials_set_unix_user";
end;

define C-function g-credentials-to-string
  input parameter self :: <GCredentials>;
  result res :: <C-string>;
  c-name: "g_credentials_to_string";
end;

define C-struct <_GCredentialsClass>
  pointer-type-name: <GCredentialsClass>;
end C-struct;

define constant $G-CREDENTIALS-TYPE-INVALID = 0;
define constant $G-CREDENTIALS-TYPE-LINUX-UCRED = 1;
define constant $G-CREDENTIALS-TYPE-FREEBSD-CMSGCRED = 2;
define constant $G-CREDENTIALS-TYPE-OPENBSD-SOCKPEERCRED = 3;
define constant <GCredentialsType> = <C-int>;
define C-pointer-type <GCredentialsType*> => <GCredentialsType>;

define open C-subtype <GDBusActionGroup> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusActionGroup*> => <GDBusActionGroup>;

define C-function g-dbus-action-group-get
  input parameter connection_ :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusActionGroup>;
  c-name: "g_dbus_action_group_get";
end;

define C-struct <_GDBusAnnotationInfo>
  slot gdbusannotationinfo-ref-count :: <C-signed-int>;
  slot gdbusannotationinfo-key :: <C-string>;
  slot gdbusannotationinfo-value :: <C-string>;
  slot gdbusannotationinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusAnnotationInfo>;
end C-struct;

define C-function g-dbus-annotation-info-ref
  input parameter self :: <GDBusAnnotationInfo>;
  result res :: <GDBusAnnotationInfo>;
  c-name: "g_dbus_annotation_info_ref";
end;

define C-function g-dbus-annotation-info-unref
  input parameter self :: <GDBusAnnotationInfo>;
  c-name: "g_dbus_annotation_info_unref";
end;

define C-function g-dbus-annotation-info-lookup
  input parameter annotations_ :: <C-unsigned-char*> /* Not supported */;
  input parameter name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_dbus_annotation_info_lookup";
end;

define C-struct <_GDBusArgInfo>
  slot gdbusarginfo-ref-count :: <C-signed-int>;
  slot gdbusarginfo-name :: <C-string>;
  slot gdbusarginfo-signature :: <C-string>;
  slot gdbusarginfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusArgInfo>;
end C-struct;

define C-function g-dbus-arg-info-ref
  input parameter self :: <GDBusArgInfo>;
  result res :: <GDBusArgInfo>;
  c-name: "g_dbus_arg_info_ref";
end;

define C-function g-dbus-arg-info-unref
  input parameter self :: <GDBusArgInfo>;
  c-name: "g_dbus_arg_info_unref";
end;

define open C-subtype <GDBusAuthObserver> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusAuthObserver*> => <GDBusAuthObserver>;

define C-function g-dbus-auth-observer-new
  result res :: <GDBusAuthObserver>;
  c-name: "g_dbus_auth_observer_new";
end;

define C-function g-dbus-auth-observer-allow-mechanism
  input parameter self :: <GDBusAuthObserver>;
  input parameter mechanism_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_auth_observer_allow_mechanism";
end;

define C-function g-dbus-auth-observer-authorize-authenticated-peer
  input parameter self :: <GDBusAuthObserver>;
  input parameter stream_ :: <GIOStream>;
  input parameter credentials_ :: <GCredentials>;
  result res :: <C-boolean>;
  c-name: "g_dbus_auth_observer_authorize_authenticated_peer";
end;

define constant $G-DBUS-CALL-FLAGS-NONE = 0;
define constant $G-DBUS-CALL-FLAGS-NO-AUTO-START = 1;
define constant <GDBusCallFlags> = <C-int>;
define C-pointer-type <GDBusCallFlags*> => <GDBusCallFlags>;

define constant $G-DBUS-CAPABILITY-FLAGS-NONE = 0;
define constant $G-DBUS-CAPABILITY-FLAGS-UNIX-FD-PASSING = 1;
define constant <GDBusCapabilityFlags> = <C-int>;
define C-pointer-type <GDBusCapabilityFlags*> => <GDBusCapabilityFlags>;

define open C-subtype <GDBusConnection> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusConnection*> => <GDBusConnection>;

define property-setter dbusconnection-address :: <C-string> on <GDBusConnection> end;
define property-setter dbusconnection-authentication-observer :: <GDBusAuthObserver> on <GDBusConnection> end;
define property-getter dbusconnection-capabilities :: <GDBusCapabilityFlags> on <GDBusConnection> end;
define property-getter dbusconnection-closed :: <C-boolean> on <GDBusConnection> end;
define property-getter dbusconnection-exit-on-close :: <C-boolean> on <GDBusConnection> end;
define property-setter dbusconnection-exit-on-close :: <C-boolean> on <GDBusConnection> end;
define property-setter dbusconnection-flags :: <GDBusConnectionFlags> on <GDBusConnection> end;
define property-getter dbusconnection-guid :: <C-string> on <GDBusConnection> end;
define property-setter dbusconnection-guid :: <C-string> on <GDBusConnection> end;
define property-getter dbusconnection-stream :: <GIOStream> on <GDBusConnection> end;
define property-setter dbusconnection-stream :: <GIOStream> on <GDBusConnection> end;
define property-getter dbusconnection-unique-name :: <C-string> on <GDBusConnection> end;
define C-function g-dbus-connection-new-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_finish";
end;

define C-function g-dbus-connection-new-for-address-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_for_address_finish";
end;

define C-function g-dbus-connection-new-for-address-sync
  input parameter address_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_for_address_sync";
end;

define C-function g-dbus-connection-new-sync
  input parameter stream_ :: <GIOStream>;
  input parameter guid_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_sync";
end;

define C-function g-dbus-connection-new
  input parameter stream_ :: <GIOStream>;
  input parameter guid_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_new";
end;

define C-function g-dbus-connection-new-for-address
  input parameter address_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_new_for_address";
end;

define C-function g-dbus-connection-add-filter
  input parameter self :: <GDBusConnection>;
  input parameter filter_function_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_free_func_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_add_filter";
end;

define C-function g-dbus-connection-call
  input parameter self :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter reply_type_ :: <GVariantType>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_call";
end;

define C-function g-dbus-connection-call-finish
  input parameter self :: <GDBusConnection>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GVariant>;
  c-name: "g_dbus_connection_call_finish";
end;

define C-function g-dbus-connection-call-sync
  input parameter self :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter reply_type_ :: <GVariantType>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GVariant>;
  c-name: "g_dbus_connection_call_sync";
end;

define C-function g-dbus-connection-call-with-unix-fd-list
  input parameter self :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter reply_type_ :: <GVariantType>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter fd_list_ :: <GUnixFDList>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_call_with_unix_fd_list";
end;

define C-function g-dbus-connection-call-with-unix-fd-list-finish
  input parameter self :: <GDBusConnection>;
  output parameter out_fd_list_ :: <GUnixFDList*>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GVariant>;
  c-name: "g_dbus_connection_call_with_unix_fd_list_finish";
end;

define C-function g-dbus-connection-call-with-unix-fd-list-sync
  input parameter self :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter reply_type_ :: <GVariantType>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter fd_list_ :: <GUnixFDList>;
  output parameter out_fd_list_ :: <GUnixFDList*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GVariant>;
  c-name: "g_dbus_connection_call_with_unix_fd_list_sync";
end;

define C-function g-dbus-connection-close
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_close";
end;

define C-function g-dbus-connection-close-finish
  input parameter self :: <GDBusConnection>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_close_finish";
end;

define C-function g-dbus-connection-close-sync
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_close_sync";
end;

define C-function g-dbus-connection-emit-signal
  input parameter self :: <GDBusConnection>;
  input parameter destination_bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter signal_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_emit_signal";
end;

define C-function g-dbus-connection-export-action-group
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter action_group_ :: <GActionGroup>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_export_action_group";
end;

define C-function g-dbus-connection-export-menu-model
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter menu_ :: <GMenuModel>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_export_menu_model";
end;

define C-function g-dbus-connection-flush
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_flush";
end;

define C-function g-dbus-connection-flush-finish
  input parameter self :: <GDBusConnection>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_flush_finish";
end;

define C-function g-dbus-connection-flush-sync
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_flush_sync";
end;

define C-function g-dbus-connection-get-capabilities
  input parameter self :: <GDBusConnection>;
  result res :: <GDBusCapabilityFlags>;
  c-name: "g_dbus_connection_get_capabilities";
end;

define C-function g-dbus-connection-get-exit-on-close
  input parameter self :: <GDBusConnection>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_get_exit_on_close";
end;

define C-function g-dbus-connection-get-guid
  input parameter self :: <GDBusConnection>;
  result res :: <C-string>;
  c-name: "g_dbus_connection_get_guid";
end;

define C-function g-dbus-connection-get-last-serial
  input parameter self :: <GDBusConnection>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_get_last_serial";
end;

define C-function g-dbus-connection-get-peer-credentials
  input parameter self :: <GDBusConnection>;
  result res :: <GCredentials>;
  c-name: "g_dbus_connection_get_peer_credentials";
end;

define C-function g-dbus-connection-get-stream
  input parameter self :: <GDBusConnection>;
  result res :: <GIOStream>;
  c-name: "g_dbus_connection_get_stream";
end;

define C-function g-dbus-connection-get-unique-name
  input parameter self :: <GDBusConnection>;
  result res :: <C-string>;
  c-name: "g_dbus_connection_get_unique_name";
end;

define C-function g-dbus-connection-is-closed
  input parameter self :: <GDBusConnection>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_is_closed";
end;

define C-function g-dbus-connection-register-object
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_info_ :: <GDBusInterfaceInfo>;
  input parameter vtable_ :: <GDBusInterfaceVTable>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_free_func_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_register_object";
end;

define C-function g-dbus-connection-register-subtree
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter vtable_ :: <GDBusSubtreeVTable>;
  input parameter flags_ :: <GDBusSubtreeFlags>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_free_func_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_register_subtree";
end;

define C-function g-dbus-connection-remove-filter
  input parameter self :: <GDBusConnection>;
  input parameter filter_id_ :: <C-unsigned-int>;
  c-name: "g_dbus_connection_remove_filter";
end;

define C-function g-dbus-connection-send-message
  input parameter self :: <GDBusConnection>;
  input parameter message_ :: <GDBusMessage>;
  input parameter flags_ :: <GDBusSendMessageFlags>;
  output parameter out_serial_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_send_message";
end;

define C-function g-dbus-connection-send-message-with-reply
  input parameter self :: <GDBusConnection>;
  input parameter message_ :: <GDBusMessage>;
  input parameter flags_ :: <GDBusSendMessageFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  output parameter out_serial_ :: <C-unsigned-int*>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_connection_send_message_with_reply";
end;

define C-function g-dbus-connection-send-message-with-reply-finish
  input parameter self :: <GDBusConnection>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_connection_send_message_with_reply_finish";
end;

define C-function g-dbus-connection-send-message-with-reply-sync
  input parameter self :: <GDBusConnection>;
  input parameter message_ :: <GDBusMessage>;
  input parameter flags_ :: <GDBusSendMessageFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  output parameter out_serial_ :: <C-unsigned-int*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_connection_send_message_with_reply_sync";
end;

define C-function g-dbus-connection-set-exit-on-close
  input parameter self :: <GDBusConnection>;
  input parameter exit_on_close_ :: <C-boolean>;
  c-name: "g_dbus_connection_set_exit_on_close";
end;

define C-function g-dbus-connection-signal-subscribe
  input parameter self :: <GDBusConnection>;
  input parameter sender_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter member_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter arg0_ :: <C-string>;
  input parameter flags_ :: <GDBusSignalFlags>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter user_data_free_func_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_signal_subscribe";
end;

define C-function g-dbus-connection-signal-unsubscribe
  input parameter self :: <GDBusConnection>;
  input parameter subscription_id_ :: <C-unsigned-int>;
  c-name: "g_dbus_connection_signal_unsubscribe";
end;

define C-function g-dbus-connection-start-message-processing
  input parameter self :: <GDBusConnection>;
  c-name: "g_dbus_connection_start_message_processing";
end;

define C-function g-dbus-connection-unexport-action-group
  input parameter self :: <GDBusConnection>;
  input parameter export_id_ :: <C-unsigned-int>;
  c-name: "g_dbus_connection_unexport_action_group";
end;

define C-function g-dbus-connection-unexport-menu-model
  input parameter self :: <GDBusConnection>;
  input parameter export_id_ :: <C-unsigned-int>;
  c-name: "g_dbus_connection_unexport_menu_model";
end;

define C-function g-dbus-connection-unregister-object
  input parameter self :: <GDBusConnection>;
  input parameter registration_id_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_unregister_object";
end;

define C-function g-dbus-connection-unregister-subtree
  input parameter self :: <GDBusConnection>;
  input parameter registration_id_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_unregister_subtree";
end;

define constant $G-DBUS-CONNECTION-FLAGS-NONE = 0;
define constant $G-DBUS-CONNECTION-FLAGS-AUTHENTICATION-CLIENT = 1;
define constant $G-DBUS-CONNECTION-FLAGS-AUTHENTICATION-SERVER = 2;
define constant $G-DBUS-CONNECTION-FLAGS-AUTHENTICATION-ALLOW-ANONYMOUS = 4;
define constant $G-DBUS-CONNECTION-FLAGS-MESSAGE-BUS-CONNECTION = 8;
define constant $G-DBUS-CONNECTION-FLAGS-DELAY-MESSAGE-PROCESSING = 16;
define constant <GDBusConnectionFlags> = <C-int>;
define C-pointer-type <GDBusConnectionFlags*> => <GDBusConnectionFlags>;

define constant $G-DBUS-ERROR-FAILED = 0;
define constant $G-DBUS-ERROR-NO-MEMORY = 1;
define constant $G-DBUS-ERROR-SERVICE-UNKNOWN = 2;
define constant $G-DBUS-ERROR-NAME-HAS-NO-OWNER = 3;
define constant $G-DBUS-ERROR-NO-REPLY = 4;
define constant $G-DBUS-ERROR-IO-ERROR = 5;
define constant $G-DBUS-ERROR-BAD-ADDRESS = 6;
define constant $G-DBUS-ERROR-NOT-SUPPORTED = 7;
define constant $G-DBUS-ERROR-LIMITS-EXCEEDED = 8;
define constant $G-DBUS-ERROR-ACCESS-DENIED = 9;
define constant $G-DBUS-ERROR-AUTH-FAILED = 10;
define constant $G-DBUS-ERROR-NO-SERVER = 11;
define constant $G-DBUS-ERROR-TIMEOUT = 12;
define constant $G-DBUS-ERROR-NO-NETWORK = 13;
define constant $G-DBUS-ERROR-ADDRESS-IN-USE = 14;
define constant $G-DBUS-ERROR-DISCONNECTED = 15;
define constant $G-DBUS-ERROR-INVALID-ARGS = 16;
define constant $G-DBUS-ERROR-FILE-NOT-FOUND = 17;
define constant $G-DBUS-ERROR-FILE-EXISTS = 18;
define constant $G-DBUS-ERROR-UNKNOWN-METHOD = 19;
define constant $G-DBUS-ERROR-TIMED-OUT = 20;
define constant $G-DBUS-ERROR-MATCH-RULE-NOT-FOUND = 21;
define constant $G-DBUS-ERROR-MATCH-RULE-INVALID = 22;
define constant $G-DBUS-ERROR-SPAWN-EXEC-FAILED = 23;
define constant $G-DBUS-ERROR-SPAWN-FORK-FAILED = 24;
define constant $G-DBUS-ERROR-SPAWN-CHILD-EXITED = 25;
define constant $G-DBUS-ERROR-SPAWN-CHILD-SIGNALED = 26;
define constant $G-DBUS-ERROR-SPAWN-FAILED = 27;
define constant $G-DBUS-ERROR-SPAWN-SETUP-FAILED = 28;
define constant $G-DBUS-ERROR-SPAWN-CONFIG-INVALID = 29;
define constant $G-DBUS-ERROR-SPAWN-SERVICE-INVALID = 30;
define constant $G-DBUS-ERROR-SPAWN-SERVICE-NOT-FOUND = 31;
define constant $G-DBUS-ERROR-SPAWN-PERMISSIONS-INVALID = 32;
define constant $G-DBUS-ERROR-SPAWN-FILE-INVALID = 33;
define constant $G-DBUS-ERROR-SPAWN-NO-MEMORY = 34;
define constant $G-DBUS-ERROR-UNIX-PROCESS-ID-UNKNOWN = 35;
define constant $G-DBUS-ERROR-INVALID-SIGNATURE = 36;
define constant $G-DBUS-ERROR-INVALID-FILE-CONTENT = 37;
define constant $G-DBUS-ERROR-SELINUX-SECURITY-CONTEXT-UNKNOWN = 38;
define constant $G-DBUS-ERROR-ADT-AUDIT-DATA-UNKNOWN = 39;
define constant $G-DBUS-ERROR-OBJECT-PATH-IN-USE = 40;
define constant <GDBusError> = <C-int>;
define C-pointer-type <GDBusError*> => <GDBusError>;

define C-struct <_GDBusErrorEntry>
  slot gdbuserrorentry-error-code :: <C-signed-int>;
  slot gdbuserrorentry-dbus-error-name :: <C-string>;
  pointer-type-name: <GDBusErrorEntry>;
end C-struct;

// Interface
define open C-subtype <GDBusInterface> (<C-void*>)
end C-subtype;

define C-pointer-type <GDBusInterface*> => <GDBusInterface>;

define C-function g-dbus-interface-dup-object
  input parameter self :: <GDBusInterface>;
  result res :: <GDBusObject>;
  c-name: "g_dbus_interface_dup_object";
end;

define C-function g-dbus-interface-get-info
  input parameter self :: <GDBusInterface>;
  result res :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_get_info";
end;

define C-function g-dbus-interface-set-object
  input parameter self :: <GDBusInterface>;
  input parameter object_ :: <GDBusObject>;
  c-name: "g_dbus_interface_set_object";
end;

define C-struct <_GDBusInterfaceIface>
  constant slot gdbusinterfaceiface-parent-iface :: <GTypeInterface>;
  constant slot gdbusinterfaceiface-get-info :: <C-function-pointer>;
  constant slot gdbusinterfaceiface-get-object :: <C-function-pointer>;
  constant slot gdbusinterfaceiface-set-object :: <C-function-pointer>;
  constant slot gdbusinterfaceiface-dup-object :: <C-function-pointer>;
  pointer-type-name: <GDBusInterfaceIface>;
end C-struct;

define C-struct <_GDBusInterfaceInfo>
  slot gdbusinterfaceinfo-ref-count :: <C-signed-int>;
  slot gdbusinterfaceinfo-name :: <C-string>;
  slot gdbusinterfaceinfo-methods :: <C-unsigned-char*> /* Not supported */;
  slot gdbusinterfaceinfo-signals :: <C-unsigned-char*> /* Not supported */;
  slot gdbusinterfaceinfo-properties :: <C-unsigned-char*> /* Not supported */;
  slot gdbusinterfaceinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusInterfaceInfo>;
end C-struct;

define C-function g-dbus-interface-info-cache-build
  input parameter self :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_info_cache_build";
end;

define C-function g-dbus-interface-info-cache-release
  input parameter self :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_info_cache_release";
end;

define C-function g-dbus-interface-info-generate-xml
  input parameter self :: <GDBusInterfaceInfo>;
  input parameter indent_ :: <C-unsigned-int>;
  output parameter string_builder_ :: <GString>;
  c-name: "g_dbus_interface_info_generate_xml";
end;

define C-function g-dbus-interface-info-lookup-method
  input parameter self :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  result res :: <GDBusMethodInfo>;
  c-name: "g_dbus_interface_info_lookup_method";
end;

define C-function g-dbus-interface-info-lookup-property
  input parameter self :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  result res :: <GDBusPropertyInfo>;
  c-name: "g_dbus_interface_info_lookup_property";
end;

define C-function g-dbus-interface-info-lookup-signal
  input parameter self :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  result res :: <GDBusSignalInfo>;
  c-name: "g_dbus_interface_info_lookup_signal";
end;

define C-function g-dbus-interface-info-ref
  input parameter self :: <GDBusInterfaceInfo>;
  result res :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_info_ref";
end;

define C-function g-dbus-interface-info-unref
  input parameter self :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_info_unref";
end;

define open C-subtype <GDBusInterfaceSkeleton> (<GObject>)
  constant slot gdbusinterfaceskeleton-parent-instance :: <GObject>;
  constant slot gdbusinterfaceskeleton-priv :: <GDBusInterfaceSkeletonPrivate>;
end C-subtype;

define C-pointer-type <GDBusInterfaceSkeleton*> => <GDBusInterfaceSkeleton>;

define property-getter dbusinterfaceskeleton-g-flags :: <GDBusInterfaceSkeletonFlags> on <GDBusInterfaceSkeleton> end;
define property-setter dbusinterfaceskeleton-g-flags :: <GDBusInterfaceSkeletonFlags> on <GDBusInterfaceSkeleton> end;
define C-function g-dbus-interface-skeleton-export
  input parameter self :: <GDBusInterfaceSkeleton>;
  input parameter connection_ :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_interface_skeleton_export";
end;

define C-function g-dbus-interface-skeleton-flush
  input parameter self :: <GDBusInterfaceSkeleton>;
  c-name: "g_dbus_interface_skeleton_flush";
end;

define C-function g-dbus-interface-skeleton-get-connection
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_interface_skeleton_get_connection";
end;

define C-function g-dbus-interface-skeleton-get-connections
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <GList>;
  c-name: "g_dbus_interface_skeleton_get_connections";
end;

define C-function g-dbus-interface-skeleton-get-flags
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <GDBusInterfaceSkeletonFlags>;
  c-name: "g_dbus_interface_skeleton_get_flags";
end;

define C-function g-dbus-interface-skeleton-get-info
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_interface_skeleton_get_info";
end;

define C-function g-dbus-interface-skeleton-get-object-path
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <C-string>;
  c-name: "g_dbus_interface_skeleton_get_object_path";
end;

define C-function g-dbus-interface-skeleton-get-properties
  input parameter self :: <GDBusInterfaceSkeleton>;
  result res :: <GVariant>;
  c-name: "g_dbus_interface_skeleton_get_properties";
end;

define C-function g-dbus-interface-skeleton-has-connection
  input parameter self :: <GDBusInterfaceSkeleton>;
  input parameter connection_ :: <GDBusConnection>;
  result res :: <C-boolean>;
  c-name: "g_dbus_interface_skeleton_has_connection";
end;

define C-function g-dbus-interface-skeleton-set-flags
  input parameter self :: <GDBusInterfaceSkeleton>;
  input parameter flags_ :: <GDBusInterfaceSkeletonFlags>;
  c-name: "g_dbus_interface_skeleton_set_flags";
end;

define C-function g-dbus-interface-skeleton-unexport
  input parameter self :: <GDBusInterfaceSkeleton>;
  c-name: "g_dbus_interface_skeleton_unexport";
end;

define C-function g-dbus-interface-skeleton-unexport-from-connection
  input parameter self :: <GDBusInterfaceSkeleton>;
  input parameter connection_ :: <GDBusConnection>;
  c-name: "g_dbus_interface_skeleton_unexport_from_connection";
end;

define C-struct <_GDBusInterfaceSkeletonClass>
  constant slot gdbusinterfaceskeletonclass-parent-class :: <GObjectClass>;
  constant slot gdbusinterfaceskeletonclass-get-info :: <C-function-pointer>;
  constant slot gdbusinterfaceskeletonclass-get-vtable :: <C-void*>;
  constant slot gdbusinterfaceskeletonclass-get-properties :: <C-function-pointer>;
  constant slot gdbusinterfaceskeletonclass-flush :: <C-function-pointer>;
  constant slot gdbusinterfaceskeletonclass-vfunc-padding :: <C-void*>;
  constant slot gdbusinterfaceskeletonclass-g-authorize-method :: <C-function-pointer>;
  constant slot gdbusinterfaceskeletonclass-signal-padding :: <C-void*>;
  pointer-type-name: <GDBusInterfaceSkeletonClass>;
end C-struct;

define constant $G-DBUS-INTERFACE-SKELETON-FLAGS-NONE = 0;
define constant $G-DBUS-INTERFACE-SKELETON-FLAGS-HANDLE-METHOD-INVOCATIONS-IN-THREAD = 1;
define constant <GDBusInterfaceSkeletonFlags> = <C-int>;
define C-pointer-type <GDBusInterfaceSkeletonFlags*> => <GDBusInterfaceSkeletonFlags>;

define C-struct <_GDBusInterfaceSkeletonPrivate>
  pointer-type-name: <GDBusInterfaceSkeletonPrivate>;
end C-struct;

define C-struct <_GDBusInterfaceVTable>
  slot gdbusinterfacevtable-method-call :: <C-function-pointer>;
  slot gdbusinterfacevtable-get-property :: <C-function-pointer>;
  slot gdbusinterfacevtable-set-property :: <C-function-pointer>;
  constant slot gdbusinterfacevtable-padding :: <C-void*>;
  pointer-type-name: <GDBusInterfaceVTable>;
end C-struct;

define open C-subtype <GDBusMenuModel> (<GMenuModel>)
end C-subtype;

define C-pointer-type <GDBusMenuModel*> => <GDBusMenuModel>;

define C-function g-dbus-menu-model-get
  input parameter connection_ :: <GDBusConnection>;
  input parameter bus_name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusMenuModel>;
  c-name: "g_dbus_menu_model_get";
end;

define open C-subtype <GDBusMessage> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusMessage*> => <GDBusMessage>;

define property-getter dbusmessage-locked :: <C-boolean> on <GDBusMessage> end;
define C-function g-dbus-message-new
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new";
end;

define C-function g-dbus-message-new-from-blob
  input parameter blob_ :: <C-unsigned-char*>;
  input parameter blob_len_ :: <C-unsigned-long>;
  input parameter capabilities_ :: <GDBusCapabilityFlags>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new_from_blob";
end;

define C-function g-dbus-message-new-method-call
  input parameter name_ :: <C-string>;
  input parameter path_ :: <C-string>;
  input parameter interface__ :: <C-string>;
  input parameter method_ :: <C-string>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new_method_call";
end;

define C-function g-dbus-message-new-signal
  input parameter path_ :: <C-string>;
  input parameter interface__ :: <C-string>;
  input parameter signal_ :: <C-string>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new_signal";
end;

define C-function g-dbus-message-bytes-needed
  input parameter blob_ :: <C-unsigned-char*>;
  input parameter blob_len_ :: <C-unsigned-long>;
  result res :: <C-signed-long>;
  c-name: "g_dbus_message_bytes_needed";
end;

define C-function g-dbus-message-copy
  input parameter self :: <GDBusMessage>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_copy";
end;

define C-function g-dbus-message-get-arg0
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_arg0";
end;

define C-function g-dbus-message-get-body
  input parameter self :: <GDBusMessage>;
  result res :: <GVariant>;
  c-name: "g_dbus_message_get_body";
end;

define C-function g-dbus-message-get-byte-order
  input parameter self :: <GDBusMessage>;
  result res :: <GDBusMessageByteOrder>;
  c-name: "g_dbus_message_get_byte_order";
end;

define C-function g-dbus-message-get-destination
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_destination";
end;

define C-function g-dbus-message-get-error-name
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_error_name";
end;

define C-function g-dbus-message-get-flags
  input parameter self :: <GDBusMessage>;
  result res :: <GDBusMessageFlags>;
  c-name: "g_dbus_message_get_flags";
end;

define C-function g-dbus-message-get-header
  input parameter self :: <GDBusMessage>;
  input parameter header_field_ :: <GDBusMessageHeaderField>;
  result res :: <GVariant>;
  c-name: "g_dbus_message_get_header";
end;

define C-function g-dbus-message-get-header-fields
  input parameter self :: <GDBusMessage>;
  result res :: <C-unsigned-char*>;
  c-name: "g_dbus_message_get_header_fields";
end;

define C-function g-dbus-message-get-interface
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_interface";
end;

define C-function g-dbus-message-get-locked
  input parameter self :: <GDBusMessage>;
  result res :: <C-boolean>;
  c-name: "g_dbus_message_get_locked";
end;

define C-function g-dbus-message-get-member
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_member";
end;

define C-function g-dbus-message-get-message-type
  input parameter self :: <GDBusMessage>;
  result res :: <GDBusMessageType>;
  c-name: "g_dbus_message_get_message_type";
end;

define C-function g-dbus-message-get-num-unix-fds
  input parameter self :: <GDBusMessage>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_message_get_num_unix_fds";
end;

define C-function g-dbus-message-get-path
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_path";
end;

define C-function g-dbus-message-get-reply-serial
  input parameter self :: <GDBusMessage>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_message_get_reply_serial";
end;

define C-function g-dbus-message-get-sender
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_sender";
end;

define C-function g-dbus-message-get-serial
  input parameter self :: <GDBusMessage>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_message_get_serial";
end;

define C-function g-dbus-message-get-signature
  input parameter self :: <GDBusMessage>;
  result res :: <C-string>;
  c-name: "g_dbus_message_get_signature";
end;

define C-function g-dbus-message-get-unix-fd-list
  input parameter self :: <GDBusMessage>;
  result res :: <GUnixFDList>;
  c-name: "g_dbus_message_get_unix_fd_list";
end;

define C-function g-dbus-message-lock
  input parameter self :: <GDBusMessage>;
  c-name: "g_dbus_message_lock";
end;

define C-function g-dbus-message-new-method-error-literal
  input parameter self :: <GDBusMessage>;
  input parameter error_name_ :: <C-string>;
  input parameter error_message_ :: <C-string>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new_method_error_literal";
end;

define C-function g-dbus-message-new-method-reply
  input parameter self :: <GDBusMessage>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new_method_reply";
end;

define C-function g-dbus-message-print
  input parameter self :: <GDBusMessage>;
  input parameter indent_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "g_dbus_message_print";
end;

define C-function g-dbus-message-set-body
  input parameter self :: <GDBusMessage>;
  input parameter body_ :: <GVariant>;
  c-name: "g_dbus_message_set_body";
end;

define C-function g-dbus-message-set-byte-order
  input parameter self :: <GDBusMessage>;
  input parameter byte_order_ :: <GDBusMessageByteOrder>;
  c-name: "g_dbus_message_set_byte_order";
end;

define C-function g-dbus-message-set-destination
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_destination";
end;

define C-function g-dbus-message-set-error-name
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_error_name";
end;

define C-function g-dbus-message-set-flags
  input parameter self :: <GDBusMessage>;
  input parameter flags_ :: <GDBusMessageFlags>;
  c-name: "g_dbus_message_set_flags";
end;

define C-function g-dbus-message-set-header
  input parameter self :: <GDBusMessage>;
  input parameter header_field_ :: <GDBusMessageHeaderField>;
  input parameter value_ :: <GVariant>;
  c-name: "g_dbus_message_set_header";
end;

define C-function g-dbus-message-set-interface
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_interface";
end;

define C-function g-dbus-message-set-member
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_member";
end;

define C-function g-dbus-message-set-message-type
  input parameter self :: <GDBusMessage>;
  input parameter type_ :: <GDBusMessageType>;
  c-name: "g_dbus_message_set_message_type";
end;

define C-function g-dbus-message-set-num-unix-fds
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-unsigned-int>;
  c-name: "g_dbus_message_set_num_unix_fds";
end;

define C-function g-dbus-message-set-path
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_path";
end;

define C-function g-dbus-message-set-reply-serial
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-unsigned-int>;
  c-name: "g_dbus_message_set_reply_serial";
end;

define C-function g-dbus-message-set-sender
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_sender";
end;

define C-function g-dbus-message-set-serial
  input parameter self :: <GDBusMessage>;
  input parameter serial_ :: <C-unsigned-int>;
  c-name: "g_dbus_message_set_serial";
end;

define C-function g-dbus-message-set-signature
  input parameter self :: <GDBusMessage>;
  input parameter value_ :: <C-string>;
  c-name: "g_dbus_message_set_signature";
end;

define C-function g-dbus-message-set-unix-fd-list
  input parameter self :: <GDBusMessage>;
  input parameter fd_list_ :: <GUnixFDList>;
  c-name: "g_dbus_message_set_unix_fd_list";
end;

define C-function g-dbus-message-to-blob
  input parameter self :: <GDBusMessage>;
  output parameter out_size_ :: <C-unsigned-long*>;
  input parameter capabilities_ :: <GDBusCapabilityFlags>;
  result res :: <C-unsigned-char*>;
  c-name: "g_dbus_message_to_blob";
end;

define C-function g-dbus-message-to-gerror
  input parameter self :: <GDBusMessage>;
  result res :: <C-boolean>;
  c-name: "g_dbus_message_to_gerror";
end;

define constant $G-DBUS-MESSAGE-BYTE-ORDER-BIG-ENDIAN = 66;
define constant $G-DBUS-MESSAGE-BYTE-ORDER-LITTLE-ENDIAN = 108;
define constant <GDBusMessageByteOrder> = <C-int>;
define C-pointer-type <GDBusMessageByteOrder*> => <GDBusMessageByteOrder>;

define constant $G-DBUS-MESSAGE-FLAGS-NONE = 0;
define constant $G-DBUS-MESSAGE-FLAGS-NO-REPLY-EXPECTED = 1;
define constant $G-DBUS-MESSAGE-FLAGS-NO-AUTO-START = 2;
define constant <GDBusMessageFlags> = <C-int>;
define C-pointer-type <GDBusMessageFlags*> => <GDBusMessageFlags>;

define constant $G-DBUS-MESSAGE-HEADER-FIELD-INVALID = 0;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-PATH = 1;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-INTERFACE = 2;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-MEMBER = 3;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-ERROR-NAME = 4;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-REPLY-SERIAL = 5;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-DESTINATION = 6;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-SENDER = 7;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-SIGNATURE = 8;
define constant $G-DBUS-MESSAGE-HEADER-FIELD-NUM-UNIX-FDS = 9;
define constant <GDBusMessageHeaderField> = <C-int>;
define C-pointer-type <GDBusMessageHeaderField*> => <GDBusMessageHeaderField>;

define constant $G-DBUS-MESSAGE-TYPE-INVALID = 0;
define constant $G-DBUS-MESSAGE-TYPE-METHOD-CALL = 1;
define constant $G-DBUS-MESSAGE-TYPE-METHOD-RETURN = 2;
define constant $G-DBUS-MESSAGE-TYPE-ERROR = 3;
define constant $G-DBUS-MESSAGE-TYPE-SIGNAL = 4;
define constant <GDBusMessageType> = <C-int>;
define C-pointer-type <GDBusMessageType*> => <GDBusMessageType>;

define C-struct <_GDBusMethodInfo>
  slot gdbusmethodinfo-ref-count :: <C-signed-int>;
  slot gdbusmethodinfo-name :: <C-string>;
  slot gdbusmethodinfo-in-args :: <C-unsigned-char*> /* Not supported */;
  slot gdbusmethodinfo-out-args :: <C-unsigned-char*> /* Not supported */;
  slot gdbusmethodinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusMethodInfo>;
end C-struct;

define C-function g-dbus-method-info-ref
  input parameter self :: <GDBusMethodInfo>;
  result res :: <GDBusMethodInfo>;
  c-name: "g_dbus_method_info_ref";
end;

define C-function g-dbus-method-info-unref
  input parameter self :: <GDBusMethodInfo>;
  c-name: "g_dbus_method_info_unref";
end;

define open C-subtype <GDBusMethodInvocation> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusMethodInvocation*> => <GDBusMethodInvocation>;

define C-function g-dbus-method-invocation-get-connection
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_method_invocation_get_connection";
end;

define C-function g-dbus-method-invocation-get-interface-name
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <C-string>;
  c-name: "g_dbus_method_invocation_get_interface_name";
end;

define C-function g-dbus-method-invocation-get-message
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <GDBusMessage>;
  c-name: "g_dbus_method_invocation_get_message";
end;

define C-function g-dbus-method-invocation-get-method-info
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <GDBusMethodInfo>;
  c-name: "g_dbus_method_invocation_get_method_info";
end;

define C-function g-dbus-method-invocation-get-method-name
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <C-string>;
  c-name: "g_dbus_method_invocation_get_method_name";
end;

define C-function g-dbus-method-invocation-get-object-path
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <C-string>;
  c-name: "g_dbus_method_invocation_get_object_path";
end;

define C-function g-dbus-method-invocation-get-parameters
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <GVariant>;
  c-name: "g_dbus_method_invocation_get_parameters";
end;

define C-function g-dbus-method-invocation-get-sender
  input parameter self :: <GDBusMethodInvocation>;
  result res :: <C-string>;
  c-name: "g_dbus_method_invocation_get_sender";
end;

define C-function g-dbus-method-invocation-return-dbus-error
  input parameter self :: <GDBusMethodInvocation>;
  input parameter error_name_ :: <C-string>;
  input parameter error_message_ :: <C-string>;
  c-name: "g_dbus_method_invocation_return_dbus_error";
end;

define C-function g-dbus-method-invocation-return-error-literal
  input parameter self :: <GDBusMethodInvocation>;
  input parameter domain_ :: <C-unsigned-int>;
  input parameter code_ :: <C-signed-int>;
  input parameter message_ :: <C-string>;
  c-name: "g_dbus_method_invocation_return_error_literal";
end;

define C-function g-dbus-method-invocation-return-gerror
  input parameter self :: <GDBusMethodInvocation>;
  input parameter error_ :: <GError>;
  c-name: "g_dbus_method_invocation_return_gerror";
end;

define C-function g-dbus-method-invocation-return-value
  input parameter self :: <GDBusMethodInvocation>;
  input parameter parameters_ :: <GVariant>;
  c-name: "g_dbus_method_invocation_return_value";
end;

define C-function g-dbus-method-invocation-return-value-with-unix-fd-list
  input parameter self :: <GDBusMethodInvocation>;
  input parameter parameters_ :: <GVariant>;
  input parameter fd_list_ :: <GUnixFDList>;
  c-name: "g_dbus_method_invocation_return_value_with_unix_fd_list";
end;

define C-struct <_GDBusNodeInfo>
  slot gdbusnodeinfo-ref-count :: <C-signed-int>;
  slot gdbusnodeinfo-path :: <C-string>;
  slot gdbusnodeinfo-interfaces :: <C-unsigned-char*> /* Not supported */;
  slot gdbusnodeinfo-nodes :: <C-unsigned-char*> /* Not supported */;
  slot gdbusnodeinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusNodeInfo>;
end C-struct;

define C-function g-dbus-node-info-new-for-xml
  input parameter xml_data_ :: <C-string>;
  result res :: <GDBusNodeInfo>;
  c-name: "g_dbus_node_info_new_for_xml";
end;

define C-function g-dbus-node-info-generate-xml
  input parameter self :: <GDBusNodeInfo>;
  input parameter indent_ :: <C-unsigned-int>;
  output parameter string_builder_ :: <GString>;
  c-name: "g_dbus_node_info_generate_xml";
end;

define C-function g-dbus-node-info-lookup-interface
  input parameter self :: <GDBusNodeInfo>;
  input parameter name_ :: <C-string>;
  result res :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_node_info_lookup_interface";
end;

define C-function g-dbus-node-info-ref
  input parameter self :: <GDBusNodeInfo>;
  result res :: <GDBusNodeInfo>;
  c-name: "g_dbus_node_info_ref";
end;

define C-function g-dbus-node-info-unref
  input parameter self :: <GDBusNodeInfo>;
  c-name: "g_dbus_node_info_unref";
end;

// Interface
define open C-subtype <GDBusObject> (<C-void*>)
end C-subtype;

define C-pointer-type <GDBusObject*> => <GDBusObject>;

define C-function g-dbus-object-get-interface
  input parameter self :: <GDBusObject>;
  input parameter interface_name_ :: <C-string>;
  result res :: <GDBusInterface>;
  c-name: "g_dbus_object_get_interface";
end;

define C-function g-dbus-object-get-interfaces
  input parameter self :: <GDBusObject>;
  result res :: <GList>;
  c-name: "g_dbus_object_get_interfaces";
end;

define C-function g-dbus-object-get-object-path
  input parameter self :: <GDBusObject>;
  result res :: <C-string>;
  c-name: "g_dbus_object_get_object_path";
end;

define C-struct <_GDBusObjectIface>
  constant slot gdbusobjectiface-parent-iface :: <GTypeInterface>;
  constant slot gdbusobjectiface-get-object-path :: <C-function-pointer>;
  constant slot gdbusobjectiface-get-interfaces :: <C-function-pointer>;
  constant slot gdbusobjectiface-get-interface :: <C-function-pointer>;
  constant slot gdbusobjectiface-interface-added :: <C-function-pointer>;
  constant slot gdbusobjectiface-interface-removed :: <C-function-pointer>;
  pointer-type-name: <GDBusObjectIface>;
end C-struct;

// Interface
define open C-subtype <GDBusObjectManager> (<C-void*>)
end C-subtype;

define C-pointer-type <GDBusObjectManager*> => <GDBusObjectManager>;

define C-function g-dbus-object-manager-get-interface
  input parameter self :: <GDBusObjectManager>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  result res :: <GDBusInterface>;
  c-name: "g_dbus_object_manager_get_interface";
end;

define C-function g-dbus-object-manager-get-object
  input parameter self :: <GDBusObjectManager>;
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusObject>;
  c-name: "g_dbus_object_manager_get_object";
end;

define C-function g-dbus-object-manager-get-object-path
  input parameter self :: <GDBusObjectManager>;
  result res :: <C-string>;
  c-name: "g_dbus_object_manager_get_object_path";
end;

define C-function g-dbus-object-manager-get-objects
  input parameter self :: <GDBusObjectManager>;
  result res :: <GList>;
  c-name: "g_dbus_object_manager_get_objects";
end;

define open C-subtype <GDBusObjectManagerClient> (<GObject>)
  constant slot gdbusobjectmanagerclient-parent-instance :: <GObject>;
  constant slot gdbusobjectmanagerclient-priv :: <GDBusObjectManagerClientPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectManagerClient*> => <GDBusObjectManagerClient>;

define property-setter dbusobjectmanagerclient-bus-type :: <GBusType> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-connection :: <GDBusConnection> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-connection :: <GDBusConnection> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-flags :: <GDBusObjectManagerClientFlags> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-flags :: <GDBusObjectManagerClientFlags> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-get-proxy-type-destroy-notify :: <C-void*> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-get-proxy-type-destroy-notify :: <C-void*> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-get-proxy-type-func :: <C-void*> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-get-proxy-type-func :: <C-void*> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-get-proxy-type-user-data :: <C-void*> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-get-proxy-type-user-data :: <C-void*> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-name :: <C-string> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-name :: <C-string> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-name-owner :: <C-string> on <GDBusObjectManagerClient> end;
define property-getter dbusobjectmanagerclient-object-path :: <C-string> on <GDBusObjectManagerClient> end;
define property-setter dbusobjectmanagerclient-object-path :: <C-string> on <GDBusObjectManagerClient> end;
define C-function g-dbus-object-manager-client-new-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusObjectManagerClient>;
  c-name: "g_dbus_object_manager_client_new_finish";
end;

define C-function g-dbus-object-manager-client-new-for-bus-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusObjectManagerClient>;
  c-name: "g_dbus_object_manager_client_new_for_bus_finish";
end;

define C-function g-dbus-object-manager-client-new-for-bus-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter flags_ :: <GDBusObjectManagerClientFlags>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter get_proxy_type_func_ :: <C-function-pointer>;
  input parameter get_proxy_type_user_data_ :: <C-void*>;
  input parameter get_proxy_type_destroy_notify_ :: <C-function-pointer>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusObjectManagerClient>;
  c-name: "g_dbus_object_manager_client_new_for_bus_sync";
end;

define C-function g-dbus-object-manager-client-new-sync
  input parameter connection_ :: <GDBusConnection>;
  input parameter flags_ :: <GDBusObjectManagerClientFlags>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter get_proxy_type_func_ :: <C-function-pointer>;
  input parameter get_proxy_type_user_data_ :: <C-void*>;
  input parameter get_proxy_type_destroy_notify_ :: <C-function-pointer>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusObjectManagerClient>;
  c-name: "g_dbus_object_manager_client_new_sync";
end;

define C-function g-dbus-object-manager-client-new
  input parameter connection_ :: <GDBusConnection>;
  input parameter flags_ :: <GDBusObjectManagerClientFlags>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter get_proxy_type_func_ :: <C-function-pointer>;
  input parameter get_proxy_type_user_data_ :: <C-void*>;
  input parameter get_proxy_type_destroy_notify_ :: <C-function-pointer>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_object_manager_client_new";
end;

define C-function g-dbus-object-manager-client-new-for-bus
  input parameter bus_type_ :: <GBusType>;
  input parameter flags_ :: <GDBusObjectManagerClientFlags>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter get_proxy_type_func_ :: <C-function-pointer>;
  input parameter get_proxy_type_user_data_ :: <C-void*>;
  input parameter get_proxy_type_destroy_notify_ :: <C-function-pointer>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_object_manager_client_new_for_bus";
end;

define C-function g-dbus-object-manager-client-get-connection
  input parameter self :: <GDBusObjectManagerClient>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_object_manager_client_get_connection";
end;

define C-function g-dbus-object-manager-client-get-flags
  input parameter self :: <GDBusObjectManagerClient>;
  result res :: <GDBusObjectManagerClientFlags>;
  c-name: "g_dbus_object_manager_client_get_flags";
end;

define C-function g-dbus-object-manager-client-get-name
  input parameter self :: <GDBusObjectManagerClient>;
  result res :: <C-string>;
  c-name: "g_dbus_object_manager_client_get_name";
end;

define C-function g-dbus-object-manager-client-get-name-owner
  input parameter self :: <GDBusObjectManagerClient>;
  result res :: <C-string>;
  c-name: "g_dbus_object_manager_client_get_name_owner";
end;

define C-struct <_GDBusObjectManagerClientClass>
  constant slot gdbusobjectmanagerclientclass-parent-class :: <GObjectClass>;
  constant slot gdbusobjectmanagerclientclass-interface-proxy-signal :: <C-function-pointer>;
  constant slot gdbusobjectmanagerclientclass-interface-proxy-properties-changed :: <C-function-pointer>;
  constant slot gdbusobjectmanagerclientclass-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectManagerClientClass>;
end C-struct;

define constant $G-DBUS-OBJECT-MANAGER-CLIENT-FLAGS-NONE = 0;
define constant $G-DBUS-OBJECT-MANAGER-CLIENT-FLAGS-DO-NOT-AUTO-START = 1;
define constant <GDBusObjectManagerClientFlags> = <C-int>;
define C-pointer-type <GDBusObjectManagerClientFlags*> => <GDBusObjectManagerClientFlags>;

define C-struct <_GDBusObjectManagerClientPrivate>
  pointer-type-name: <GDBusObjectManagerClientPrivate>;
end C-struct;

define C-struct <_GDBusObjectManagerIface>
  constant slot gdbusobjectmanageriface-parent-iface :: <GTypeInterface>;
  constant slot gdbusobjectmanageriface-get-object-path :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-get-objects :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-get-object :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-get-interface :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-object-added :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-object-removed :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-interface-added :: <C-function-pointer>;
  constant slot gdbusobjectmanageriface-interface-removed :: <C-function-pointer>;
  pointer-type-name: <GDBusObjectManagerIface>;
end C-struct;

define open C-subtype <GDBusObjectManagerServer> (<GObject>)
  constant slot gdbusobjectmanagerserver-parent-instance :: <GObject>;
  constant slot gdbusobjectmanagerserver-priv :: <GDBusObjectManagerServerPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectManagerServer*> => <GDBusObjectManagerServer>;

define property-getter dbusobjectmanagerserver-connection :: <GDBusConnection> on <GDBusObjectManagerServer> end;
define property-setter dbusobjectmanagerserver-connection :: <GDBusConnection> on <GDBusObjectManagerServer> end;
define property-getter dbusobjectmanagerserver-object-path :: <C-string> on <GDBusObjectManagerServer> end;
define property-setter dbusobjectmanagerserver-object-path :: <C-string> on <GDBusObjectManagerServer> end;
define C-function g-dbus-object-manager-server-new
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusObjectManagerServer>;
  c-name: "g_dbus_object_manager_server_new";
end;

define C-function g-dbus-object-manager-server-export
  input parameter self :: <GDBusObjectManagerServer>;
  input parameter object_ :: <GDBusObjectSkeleton>;
  c-name: "g_dbus_object_manager_server_export";
end;

define C-function g-dbus-object-manager-server-export-uniquely
  input parameter self :: <GDBusObjectManagerServer>;
  input parameter object_ :: <GDBusObjectSkeleton>;
  c-name: "g_dbus_object_manager_server_export_uniquely";
end;

define C-function g-dbus-object-manager-server-get-connection
  input parameter self :: <GDBusObjectManagerServer>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_object_manager_server_get_connection";
end;

define C-function g-dbus-object-manager-server-is-exported
  input parameter self :: <GDBusObjectManagerServer>;
  input parameter object_ :: <GDBusObjectSkeleton>;
  result res :: <C-boolean>;
  c-name: "g_dbus_object_manager_server_is_exported";
end;

define C-function g-dbus-object-manager-server-set-connection
  input parameter self :: <GDBusObjectManagerServer>;
  input parameter connection_ :: <GDBusConnection>;
  c-name: "g_dbus_object_manager_server_set_connection";
end;

define C-function g-dbus-object-manager-server-unexport
  input parameter self :: <GDBusObjectManagerServer>;
  input parameter object_path_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_object_manager_server_unexport";
end;

define C-struct <_GDBusObjectManagerServerClass>
  constant slot gdbusobjectmanagerserverclass-parent-class :: <GObjectClass>;
  constant slot gdbusobjectmanagerserverclass-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectManagerServerClass>;
end C-struct;

define C-struct <_GDBusObjectManagerServerPrivate>
  pointer-type-name: <GDBusObjectManagerServerPrivate>;
end C-struct;

define open C-subtype <GDBusObjectProxy> (<GObject>)
  constant slot gdbusobjectproxy-parent-instance :: <GObject>;
  constant slot gdbusobjectproxy-priv :: <GDBusObjectProxyPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectProxy*> => <GDBusObjectProxy>;

define property-getter dbusobjectproxy-g-connection :: <GDBusConnection> on <GDBusObjectProxy> end;
define property-setter dbusobjectproxy-g-connection :: <GDBusConnection> on <GDBusObjectProxy> end;
define property-getter dbusobjectproxy-g-object-path :: <C-string> on <GDBusObjectProxy> end;
define property-setter dbusobjectproxy-g-object-path :: <C-string> on <GDBusObjectProxy> end;
define C-function g-dbus-object-proxy-new
  input parameter connection_ :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusObjectProxy>;
  c-name: "g_dbus_object_proxy_new";
end;

define C-function g-dbus-object-proxy-get-connection
  input parameter self :: <GDBusObjectProxy>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_object_proxy_get_connection";
end;

define C-struct <_GDBusObjectProxyClass>
  constant slot gdbusobjectproxyclass-parent-class :: <GObjectClass>;
  constant slot gdbusobjectproxyclass-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectProxyClass>;
end C-struct;

define C-struct <_GDBusObjectProxyPrivate>
  pointer-type-name: <GDBusObjectProxyPrivate>;
end C-struct;

define open C-subtype <GDBusObjectSkeleton> (<GObject>)
  constant slot gdbusobjectskeleton-parent-instance :: <GObject>;
  constant slot gdbusobjectskeleton-priv :: <GDBusObjectSkeletonPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectSkeleton*> => <GDBusObjectSkeleton>;

define property-getter dbusobjectskeleton-g-object-path :: <C-string> on <GDBusObjectSkeleton> end;
define property-setter dbusobjectskeleton-g-object-path :: <C-string> on <GDBusObjectSkeleton> end;
define C-function g-dbus-object-skeleton-new
  input parameter object_path_ :: <C-string>;
  result res :: <GDBusObjectSkeleton>;
  c-name: "g_dbus_object_skeleton_new";
end;

define C-function g-dbus-object-skeleton-add-interface
  input parameter self :: <GDBusObjectSkeleton>;
  input parameter interface__ :: <GDBusInterfaceSkeleton>;
  c-name: "g_dbus_object_skeleton_add_interface";
end;

define C-function g-dbus-object-skeleton-flush
  input parameter self :: <GDBusObjectSkeleton>;
  c-name: "g_dbus_object_skeleton_flush";
end;

define C-function g-dbus-object-skeleton-remove-interface
  input parameter self :: <GDBusObjectSkeleton>;
  input parameter interface__ :: <GDBusInterfaceSkeleton>;
  c-name: "g_dbus_object_skeleton_remove_interface";
end;

define C-function g-dbus-object-skeleton-remove-interface-by-name
  input parameter self :: <GDBusObjectSkeleton>;
  input parameter interface_name_ :: <C-string>;
  c-name: "g_dbus_object_skeleton_remove_interface_by_name";
end;

define C-function g-dbus-object-skeleton-set-object-path
  input parameter self :: <GDBusObjectSkeleton>;
  input parameter object_path_ :: <C-string>;
  c-name: "g_dbus_object_skeleton_set_object_path";
end;

define C-struct <_GDBusObjectSkeletonClass>
  constant slot gdbusobjectskeletonclass-parent-class :: <GObjectClass>;
  constant slot gdbusobjectskeletonclass-authorize-method :: <C-function-pointer>;
  constant slot gdbusobjectskeletonclass-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectSkeletonClass>;
end C-struct;

define C-struct <_GDBusObjectSkeletonPrivate>
  pointer-type-name: <GDBusObjectSkeletonPrivate>;
end C-struct;

define C-struct <_GDBusPropertyInfo>
  slot gdbuspropertyinfo-ref-count :: <C-signed-int>;
  slot gdbuspropertyinfo-name :: <C-string>;
  slot gdbuspropertyinfo-signature :: <C-string>;
  slot gdbuspropertyinfo-flags :: <GDBusPropertyInfoFlags>;
  slot gdbuspropertyinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusPropertyInfo>;
end C-struct;

define C-function g-dbus-property-info-ref
  input parameter self :: <GDBusPropertyInfo>;
  result res :: <GDBusPropertyInfo>;
  c-name: "g_dbus_property_info_ref";
end;

define C-function g-dbus-property-info-unref
  input parameter self :: <GDBusPropertyInfo>;
  c-name: "g_dbus_property_info_unref";
end;

define constant $G-DBUS-PROPERTY-INFO-FLAGS-NONE = 0;
define constant $G-DBUS-PROPERTY-INFO-FLAGS-READABLE = 1;
define constant $G-DBUS-PROPERTY-INFO-FLAGS-WRITABLE = 2;
define constant <GDBusPropertyInfoFlags> = <C-int>;
define C-pointer-type <GDBusPropertyInfoFlags*> => <GDBusPropertyInfoFlags>;

define open C-subtype <GDBusProxy> (<GObject>)
  constant slot gdbusproxy-parent-instance :: <GObject>;
  constant slot gdbusproxy-priv :: <GDBusProxyPrivate>;
end C-subtype;

define C-pointer-type <GDBusProxy*> => <GDBusProxy>;

define property-setter dbusproxy-g-bus-type :: <GBusType> on <GDBusProxy> end;
define property-getter dbusproxy-g-connection :: <GDBusConnection> on <GDBusProxy> end;
define property-setter dbusproxy-g-connection :: <GDBusConnection> on <GDBusProxy> end;
define property-getter dbusproxy-g-default-timeout :: <C-signed-int> on <GDBusProxy> end;
define property-setter dbusproxy-g-default-timeout :: <C-signed-int> on <GDBusProxy> end;
define property-getter dbusproxy-g-flags :: <GDBusProxyFlags> on <GDBusProxy> end;
define property-setter dbusproxy-g-flags :: <GDBusProxyFlags> on <GDBusProxy> end;
define property-getter dbusproxy-g-interface-info :: <GDBusInterfaceInfo> on <GDBusProxy> end;
define property-setter dbusproxy-g-interface-info :: <GDBusInterfaceInfo> on <GDBusProxy> end;
define property-getter dbusproxy-g-interface-name :: <C-string> on <GDBusProxy> end;
define property-setter dbusproxy-g-interface-name :: <C-string> on <GDBusProxy> end;
define property-getter dbusproxy-g-name :: <C-string> on <GDBusProxy> end;
define property-setter dbusproxy-g-name :: <C-string> on <GDBusProxy> end;
define property-getter dbusproxy-g-name-owner :: <C-string> on <GDBusProxy> end;
define property-getter dbusproxy-g-object-path :: <C-string> on <GDBusProxy> end;
define property-setter dbusproxy-g-object-path :: <C-string> on <GDBusProxy> end;
define C-function g-dbus-proxy-new-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusProxy>;
  c-name: "g_dbus_proxy_new_finish";
end;

define C-function g-dbus-proxy-new-for-bus-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusProxy>;
  c-name: "g_dbus_proxy_new_for_bus_finish";
end;

define C-function g-dbus-proxy-new-for-bus-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter flags_ :: <GDBusProxyFlags>;
  input parameter info_ :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusProxy>;
  c-name: "g_dbus_proxy_new_for_bus_sync";
end;

define C-function g-dbus-proxy-new-sync
  input parameter connection_ :: <GDBusConnection>;
  input parameter flags_ :: <GDBusProxyFlags>;
  input parameter info_ :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusProxy>;
  c-name: "g_dbus_proxy_new_sync";
end;

define C-function g-dbus-proxy-new
  input parameter connection_ :: <GDBusConnection>;
  input parameter flags_ :: <GDBusProxyFlags>;
  input parameter info_ :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_proxy_new";
end;

define C-function g-dbus-proxy-new-for-bus
  input parameter bus_type_ :: <GBusType>;
  input parameter flags_ :: <GDBusProxyFlags>;
  input parameter info_ :: <GDBusInterfaceInfo>;
  input parameter name_ :: <C-string>;
  input parameter object_path_ :: <C-string>;
  input parameter interface_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_proxy_new_for_bus";
end;

define C-function g-dbus-proxy-call
  input parameter self :: <GDBusProxy>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_proxy_call";
end;

define C-function g-dbus-proxy-call-finish
  input parameter self :: <GDBusProxy>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GVariant>;
  c-name: "g_dbus_proxy_call_finish";
end;

define C-function g-dbus-proxy-call-sync
  input parameter self :: <GDBusProxy>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GVariant>;
  c-name: "g_dbus_proxy_call_sync";
end;

define C-function g-dbus-proxy-call-with-unix-fd-list
  input parameter self :: <GDBusProxy>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter fd_list_ :: <GUnixFDList>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_proxy_call_with_unix_fd_list";
end;

define C-function g-dbus-proxy-call-with-unix-fd-list-finish
  input parameter self :: <GDBusProxy>;
  output parameter out_fd_list_ :: <GUnixFDList*>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GVariant>;
  c-name: "g_dbus_proxy_call_with_unix_fd_list_finish";
end;

define C-function g-dbus-proxy-call-with-unix-fd-list-sync
  input parameter self :: <GDBusProxy>;
  input parameter method_name_ :: <C-string>;
  input parameter parameters_ :: <GVariant>;
  input parameter flags_ :: <GDBusCallFlags>;
  input parameter timeout_msec_ :: <C-signed-int>;
  input parameter fd_list_ :: <GUnixFDList>;
  output parameter out_fd_list_ :: <GUnixFDList*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GVariant>;
  c-name: "g_dbus_proxy_call_with_unix_fd_list_sync";
end;

define C-function g-dbus-proxy-get-cached-property
  input parameter self :: <GDBusProxy>;
  input parameter property_name_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_dbus_proxy_get_cached_property";
end;

define C-function g-dbus-proxy-get-cached-property-names
  input parameter self :: <GDBusProxy>;
  result res :: <C-string*>;
  c-name: "g_dbus_proxy_get_cached_property_names";
end;

define C-function g-dbus-proxy-get-connection
  input parameter self :: <GDBusProxy>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_proxy_get_connection";
end;

define C-function g-dbus-proxy-get-default-timeout
  input parameter self :: <GDBusProxy>;
  result res :: <C-signed-int>;
  c-name: "g_dbus_proxy_get_default_timeout";
end;

define C-function g-dbus-proxy-get-flags
  input parameter self :: <GDBusProxy>;
  result res :: <GDBusProxyFlags>;
  c-name: "g_dbus_proxy_get_flags";
end;

define C-function g-dbus-proxy-get-interface-info
  input parameter self :: <GDBusProxy>;
  result res :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_proxy_get_interface_info";
end;

define C-function g-dbus-proxy-get-interface-name
  input parameter self :: <GDBusProxy>;
  result res :: <C-string>;
  c-name: "g_dbus_proxy_get_interface_name";
end;

define C-function g-dbus-proxy-get-name
  input parameter self :: <GDBusProxy>;
  result res :: <C-string>;
  c-name: "g_dbus_proxy_get_name";
end;

define C-function g-dbus-proxy-get-name-owner
  input parameter self :: <GDBusProxy>;
  result res :: <C-string>;
  c-name: "g_dbus_proxy_get_name_owner";
end;

define C-function g-dbus-proxy-get-object-path
  input parameter self :: <GDBusProxy>;
  result res :: <C-string>;
  c-name: "g_dbus_proxy_get_object_path";
end;

define C-function g-dbus-proxy-set-cached-property
  input parameter self :: <GDBusProxy>;
  input parameter property_name_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  c-name: "g_dbus_proxy_set_cached_property";
end;

define C-function g-dbus-proxy-set-default-timeout
  input parameter self :: <GDBusProxy>;
  input parameter timeout_msec_ :: <C-signed-int>;
  c-name: "g_dbus_proxy_set_default_timeout";
end;

define C-function g-dbus-proxy-set-interface-info
  input parameter self :: <GDBusProxy>;
  input parameter info_ :: <GDBusInterfaceInfo>;
  c-name: "g_dbus_proxy_set_interface_info";
end;

define C-struct <_GDBusProxyClass>
  constant slot gdbusproxyclass-parent-class :: <GObjectClass>;
  constant slot gdbusproxyclass-g-properties-changed :: <C-function-pointer>;
  constant slot gdbusproxyclass-g-signal :: <C-function-pointer>;
  constant slot gdbusproxyclass-padding :: <C-void*>;
  pointer-type-name: <GDBusProxyClass>;
end C-struct;

define constant $G-DBUS-PROXY-FLAGS-NONE = 0;
define constant $G-DBUS-PROXY-FLAGS-DO-NOT-LOAD-PROPERTIES = 1;
define constant $G-DBUS-PROXY-FLAGS-DO-NOT-CONNECT-SIGNALS = 2;
define constant $G-DBUS-PROXY-FLAGS-DO-NOT-AUTO-START = 4;
define constant $G-DBUS-PROXY-FLAGS-GET-INVALIDATED-PROPERTIES = 8;
define constant <GDBusProxyFlags> = <C-int>;
define C-pointer-type <GDBusProxyFlags*> => <GDBusProxyFlags>;

define C-struct <_GDBusProxyPrivate>
  pointer-type-name: <GDBusProxyPrivate>;
end C-struct;

define constant $G-DBUS-SEND-MESSAGE-FLAGS-NONE = 0;
define constant $G-DBUS-SEND-MESSAGE-FLAGS-PRESERVE-SERIAL = 1;
define constant <GDBusSendMessageFlags> = <C-int>;
define C-pointer-type <GDBusSendMessageFlags*> => <GDBusSendMessageFlags>;

define open C-subtype <GDBusServer> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusServer*> => <GDBusServer>;

define property-getter dbusserver-active :: <C-boolean> on <GDBusServer> end;
define property-getter dbusserver-address :: <C-string> on <GDBusServer> end;
define property-setter dbusserver-address :: <C-string> on <GDBusServer> end;
define property-getter dbusserver-authentication-observer :: <GDBusAuthObserver> on <GDBusServer> end;
define property-setter dbusserver-authentication-observer :: <GDBusAuthObserver> on <GDBusServer> end;
define property-getter dbusserver-client-address :: <C-string> on <GDBusServer> end;
define property-getter dbusserver-flags :: <GDBusServerFlags> on <GDBusServer> end;
define property-setter dbusserver-flags :: <GDBusServerFlags> on <GDBusServer> end;
define property-getter dbusserver-guid :: <C-string> on <GDBusServer> end;
define property-setter dbusserver-guid :: <C-string> on <GDBusServer> end;
define C-function g-dbus-server-new-sync
  input parameter address_ :: <C-string>;
  input parameter flags_ :: <GDBusServerFlags>;
  input parameter guid_ :: <C-string>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusServer>;
  c-name: "g_dbus_server_new_sync";
end;

define C-function g-dbus-server-get-client-address
  input parameter self :: <GDBusServer>;
  result res :: <C-string>;
  c-name: "g_dbus_server_get_client_address";
end;

define C-function g-dbus-server-get-flags
  input parameter self :: <GDBusServer>;
  result res :: <GDBusServerFlags>;
  c-name: "g_dbus_server_get_flags";
end;

define C-function g-dbus-server-get-guid
  input parameter self :: <GDBusServer>;
  result res :: <C-string>;
  c-name: "g_dbus_server_get_guid";
end;

define C-function g-dbus-server-is-active
  input parameter self :: <GDBusServer>;
  result res :: <C-boolean>;
  c-name: "g_dbus_server_is_active";
end;

define C-function g-dbus-server-start
  input parameter self :: <GDBusServer>;
  c-name: "g_dbus_server_start";
end;

define C-function g-dbus-server-stop
  input parameter self :: <GDBusServer>;
  c-name: "g_dbus_server_stop";
end;

define constant $G-DBUS-SERVER-FLAGS-NONE = 0;
define constant $G-DBUS-SERVER-FLAGS-RUN-IN-THREAD = 1;
define constant $G-DBUS-SERVER-FLAGS-AUTHENTICATION-ALLOW-ANONYMOUS = 2;
define constant <GDBusServerFlags> = <C-int>;
define C-pointer-type <GDBusServerFlags*> => <GDBusServerFlags>;

define constant $G-DBUS-SIGNAL-FLAGS-NONE = 0;
define constant $G-DBUS-SIGNAL-FLAGS-NO-MATCH-RULE = 1;
define constant <GDBusSignalFlags> = <C-int>;
define C-pointer-type <GDBusSignalFlags*> => <GDBusSignalFlags>;

define C-struct <_GDBusSignalInfo>
  slot gdbussignalinfo-ref-count :: <C-signed-int>;
  slot gdbussignalinfo-name :: <C-string>;
  slot gdbussignalinfo-args :: <C-unsigned-char*> /* Not supported */;
  slot gdbussignalinfo-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusSignalInfo>;
end C-struct;

define C-function g-dbus-signal-info-ref
  input parameter self :: <GDBusSignalInfo>;
  result res :: <GDBusSignalInfo>;
  c-name: "g_dbus_signal_info_ref";
end;

define C-function g-dbus-signal-info-unref
  input parameter self :: <GDBusSignalInfo>;
  c-name: "g_dbus_signal_info_unref";
end;

define constant $G-DBUS-SUBTREE-FLAGS-NONE = 0;
define constant $G-DBUS-SUBTREE-FLAGS-DISPATCH-TO-UNENUMERATED-NODES = 1;
define constant <GDBusSubtreeFlags> = <C-int>;
define C-pointer-type <GDBusSubtreeFlags*> => <GDBusSubtreeFlags>;

define C-struct <_GDBusSubtreeVTable>
  slot gdbussubtreevtable-enumerate :: <C-void*>;
  slot gdbussubtreevtable-introspect :: <C-function-pointer>;
  slot gdbussubtreevtable-dispatch :: <C-function-pointer>;
  constant slot gdbussubtreevtable-padding :: <C-void*>;
  pointer-type-name: <GDBusSubtreeVTable>;
end C-struct;

define constant $DESKTOP-APP-INFO-LOOKUP-EXTENSION-POINT-NAME = "gio-desktop-app-info-lookup";

define open C-subtype <GDataInputStream> (<GBufferedInputStream>)
  constant slot gdatainputstream-parent-instance :: <GBufferedInputStream>;
  constant slot gdatainputstream-priv :: <GDataInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GDataInputStream*> => <GDataInputStream>;

define property-getter datainputstream-byte-order :: <GDataStreamByteOrder> on <GDataInputStream> end;
define property-setter datainputstream-byte-order :: <GDataStreamByteOrder> on <GDataInputStream> end;
define property-getter datainputstream-newline-type :: <GDataStreamNewlineType> on <GDataInputStream> end;
define property-setter datainputstream-newline-type :: <GDataStreamNewlineType> on <GDataInputStream> end;
define C-function g-data-input-stream-new
  input parameter base_stream_ :: <GInputStream>;
  result res :: <GDataInputStream>;
  c-name: "g_data_input_stream_new";
end;

define C-function g-data-input-stream-get-byte-order
  input parameter self :: <GDataInputStream>;
  result res :: <GDataStreamByteOrder>;
  c-name: "g_data_input_stream_get_byte_order";
end;

define C-function g-data-input-stream-get-newline-type
  input parameter self :: <GDataInputStream>;
  result res :: <GDataStreamNewlineType>;
  c-name: "g_data_input_stream_get_newline_type";
end;

define C-function g-data-input-stream-read-byte
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-unsigned-char>;
  c-name: "g_data_input_stream_read_byte";
end;

define C-function g-data-input-stream-read-int16
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-short>;
  c-name: "g_data_input_stream_read_int16";
end;

define C-function g-data-input-stream-read-int32
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-int>;
  c-name: "g_data_input_stream_read_int32";
end;

define C-function g-data-input-stream-read-int64
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_data_input_stream_read_int64";
end;

define C-function g-data-input-stream-read-line
  input parameter self :: <GDataInputStream>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-unsigned-char*>;
  c-name: "g_data_input_stream_read_line";
end;

define C-function g-data-input-stream-read-line-async
  input parameter self :: <GDataInputStream>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_data_input_stream_read_line_async";
end;

define C-function g-data-input-stream-read-line-finish
  input parameter self :: <GDataInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_data_input_stream_read_line_finish";
end;

define C-function g-data-input-stream-read-line-finish-utf8
  input parameter self :: <GDataInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_line_finish_utf8";
end;

define C-function g-data-input-stream-read-line-utf8
  input parameter self :: <GDataInputStream>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_line_utf8";
end;

define C-function g-data-input-stream-read-uint16
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-unsigned-short>;
  c-name: "g_data_input_stream_read_uint16";
end;

define C-function g-data-input-stream-read-uint32
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-unsigned-int>;
  c-name: "g_data_input_stream_read_uint32";
end;

define C-function g-data-input-stream-read-uint64
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-unsigned-long>;
  c-name: "g_data_input_stream_read_uint64";
end;

define C-function g-data-input-stream-read-until
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_until";
end;

define C-function g-data-input-stream-read-until-async
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_data_input_stream_read_until_async";
end;

define C-function g-data-input-stream-read-until-finish
  input parameter self :: <GDataInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_until_finish";
end;

define C-function g-data-input-stream-read-upto
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  input parameter stop_chars_len_ :: <C-signed-long>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_upto";
end;

define C-function g-data-input-stream-read-upto-async
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  input parameter stop_chars_len_ :: <C-signed-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_data_input_stream_read_upto_async";
end;

define C-function g-data-input-stream-read-upto-finish
  input parameter self :: <GDataInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_upto_finish";
end;

define C-function g-data-input-stream-set-byte-order
  input parameter self :: <GDataInputStream>;
  input parameter order_ :: <GDataStreamByteOrder>;
  c-name: "g_data_input_stream_set_byte_order";
end;

define C-function g-data-input-stream-set-newline-type
  input parameter self :: <GDataInputStream>;
  input parameter type_ :: <GDataStreamNewlineType>;
  c-name: "g_data_input_stream_set_newline_type";
end;

define C-struct <_GDataInputStreamClass>
  constant slot gdatainputstreamclass-parent-class :: <GBufferedInputStreamClass>;
  constant slot gdatainputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gdatainputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gdatainputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gdatainputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gdatainputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GDataInputStreamClass>;
end C-struct;

define C-struct <_GDataInputStreamPrivate>
  pointer-type-name: <GDataInputStreamPrivate>;
end C-struct;

define open C-subtype <GDataOutputStream> (<GFilterOutputStream>)
  constant slot gdataoutputstream-parent-instance :: <GFilterOutputStream>;
  constant slot gdataoutputstream-priv :: <GDataOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GDataOutputStream*> => <GDataOutputStream>;

define property-getter dataoutputstream-byte-order :: <GDataStreamByteOrder> on <GDataOutputStream> end;
define property-setter dataoutputstream-byte-order :: <GDataStreamByteOrder> on <GDataOutputStream> end;
define C-function g-data-output-stream-new
  input parameter base_stream_ :: <GOutputStream>;
  result res :: <GDataOutputStream>;
  c-name: "g_data_output_stream_new";
end;

define C-function g-data-output-stream-get-byte-order
  input parameter self :: <GDataOutputStream>;
  result res :: <GDataStreamByteOrder>;
  c-name: "g_data_output_stream_get_byte_order";
end;

define C-function g-data-output-stream-put-byte
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-char>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_byte";
end;

define C-function g-data-output-stream-put-int16
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-short>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int16";
end;

define C-function g-data-output-stream-put-int32
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int32";
end;

define C-function g-data-output-stream-put-int64
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int64";
end;

define C-function g-data-output-stream-put-string
  input parameter self :: <GDataOutputStream>;
  input parameter str_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_string";
end;

define C-function g-data-output-stream-put-uint16
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint16";
end;

define C-function g-data-output-stream-put-uint32
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint32";
end;

define C-function g-data-output-stream-put-uint64
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint64";
end;

define C-function g-data-output-stream-set-byte-order
  input parameter self :: <GDataOutputStream>;
  input parameter order_ :: <GDataStreamByteOrder>;
  c-name: "g_data_output_stream_set_byte_order";
end;

define C-struct <_GDataOutputStreamClass>
  constant slot gdataoutputstreamclass-parent-class :: <GFilterOutputStreamClass>;
  constant slot gdataoutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gdataoutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gdataoutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gdataoutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gdataoutputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GDataOutputStreamClass>;
end C-struct;

define C-struct <_GDataOutputStreamPrivate>
  pointer-type-name: <GDataOutputStreamPrivate>;
end C-struct;

define constant $G-DATA-STREAM-BYTE-ORDER-BIG-ENDIAN = 0;
define constant $G-DATA-STREAM-BYTE-ORDER-LITTLE-ENDIAN = 1;
define constant $G-DATA-STREAM-BYTE-ORDER-HOST-ENDIAN = 2;
define constant <GDataStreamByteOrder> = <C-int>;
define C-pointer-type <GDataStreamByteOrder*> => <GDataStreamByteOrder>;

define constant $G-DATA-STREAM-NEWLINE-TYPE-LF = 0;
define constant $G-DATA-STREAM-NEWLINE-TYPE-CR = 1;
define constant $G-DATA-STREAM-NEWLINE-TYPE-CR-LF = 2;
define constant $G-DATA-STREAM-NEWLINE-TYPE-ANY = 3;
define constant <GDataStreamNewlineType> = <C-int>;
define C-pointer-type <GDataStreamNewlineType*> => <GDataStreamNewlineType>;

define open C-subtype <GDesktopAppInfo> (<GObject>)
end C-subtype;

define C-pointer-type <GDesktopAppInfo*> => <GDesktopAppInfo>;

define property-getter desktopappinfo-filename :: <C-string> on <GDesktopAppInfo> end;
define property-setter desktopappinfo-filename :: <C-string> on <GDesktopAppInfo> end;
define C-function g-desktop-app-info-new
  input parameter desktop_id_ :: <C-string>;
  result res :: <GDesktopAppInfo>;
  c-name: "g_desktop_app_info_new";
end;

define C-function g-desktop-app-info-new-from-filename
  input parameter filename_ :: <C-string>;
  result res :: <GDesktopAppInfo>;
  c-name: "g_desktop_app_info_new_from_filename";
end;

define C-function g-desktop-app-info-new-from-keyfile
  input parameter key_file_ :: <GKeyFile>;
  result res :: <GDesktopAppInfo>;
  c-name: "g_desktop_app_info_new_from_keyfile";
end;

define C-function g-desktop-app-info-set-desktop-env
  input parameter desktop_env_ :: <C-string>;
  c-name: "g_desktop_app_info_set_desktop_env";
end;

define C-function g-desktop-app-info-get-boolean
  input parameter self :: <GDesktopAppInfo>;
  input parameter key_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_get_boolean";
end;

define C-function g-desktop-app-info-get-categories
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-string>;
  c-name: "g_desktop_app_info_get_categories";
end;

define C-function g-desktop-app-info-get-filename
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-string>;
  c-name: "g_desktop_app_info_get_filename";
end;

define C-function g-desktop-app-info-get-generic-name
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-string>;
  c-name: "g_desktop_app_info_get_generic_name";
end;

define C-function g-desktop-app-info-get-is-hidden
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_get_is_hidden";
end;

define C-function g-desktop-app-info-get-keywords
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-string*>;
  c-name: "g_desktop_app_info_get_keywords";
end;

define C-function g-desktop-app-info-get-nodisplay
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_get_nodisplay";
end;

define C-function g-desktop-app-info-get-show-in
  input parameter self :: <GDesktopAppInfo>;
  input parameter desktop_env_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_get_show_in";
end;

define C-function g-desktop-app-info-get-startup-wm-class
  input parameter self :: <GDesktopAppInfo>;
  result res :: <C-string>;
  c-name: "g_desktop_app_info_get_startup_wm_class";
end;

define C-function g-desktop-app-info-get-string
  input parameter self :: <GDesktopAppInfo>;
  input parameter key_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_desktop_app_info_get_string";
end;

define C-function g-desktop-app-info-has-key
  input parameter self :: <GDesktopAppInfo>;
  input parameter key_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_has_key";
end;

define C-function g-desktop-app-info-launch-uris-as-manager
  input parameter self :: <GDesktopAppInfo>;
  input parameter uris_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  input parameter spawn_flags_ :: <GSpawnFlags>;
  input parameter user_setup_ :: <C-function-pointer>;
  input parameter user_setup_data_ :: <C-void*>;
  input parameter pid_callback_ :: <C-function-pointer>;
  input parameter pid_callback_data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_launch_uris_as_manager";
end;

define C-struct <_GDesktopAppInfoClass>
  constant slot gdesktopappinfoclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GDesktopAppInfoClass>;
end C-struct;

// Interface
define open C-subtype <GDesktopAppInfoLookup> (<C-void*>)
end C-subtype;

define C-pointer-type <GDesktopAppInfoLookup*> => <GDesktopAppInfoLookup>;

define C-function g-desktop-app-info-lookup-get-default-for-uri-scheme
  input parameter self :: <GDesktopAppInfoLookup>;
  input parameter uri_scheme_ :: <C-string>;
  result res :: <GAppInfo>;
  c-name: "g_desktop_app_info_lookup_get_default_for_uri_scheme";
end;

define C-struct <_GDesktopAppInfoLookupIface>
  constant slot gdesktopappinfolookupiface-g-iface :: <GTypeInterface>;
  constant slot gdesktopappinfolookupiface-get-default-for-uri-scheme :: <C-function-pointer>;
  pointer-type-name: <GDesktopAppInfoLookupIface>;
end C-struct;

// Interface
define open C-subtype <GDrive> (<C-void*>)
end C-subtype;

define C-pointer-type <GDrive*> => <GDrive>;

define C-function g-drive-can-eject
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_can_eject";
end;

define C-function g-drive-can-poll-for-media
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_can_poll_for_media";
end;

define C-function g-drive-can-start
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_can_start";
end;

define C-function g-drive-can-start-degraded
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_can_start_degraded";
end;

define C-function g-drive-can-stop
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_can_stop";
end;

define C-function g-drive-eject
  input parameter self :: <GDrive>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_drive_eject";
end;

define C-function g-drive-eject-finish
  input parameter self :: <GDrive>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_drive_eject_finish";
end;

define C-function g-drive-eject-with-operation
  input parameter self :: <GDrive>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_drive_eject_with_operation";
end;

define C-function g-drive-eject-with-operation-finish
  input parameter self :: <GDrive>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_drive_eject_with_operation_finish";
end;

define C-function g-drive-enumerate-identifiers
  input parameter self :: <GDrive>;
  result res :: <C-string*>;
  c-name: "g_drive_enumerate_identifiers";
end;

define C-function g-drive-get-icon
  input parameter self :: <GDrive>;
  result res :: <GIcon>;
  c-name: "g_drive_get_icon";
end;

define C-function g-drive-get-identifier
  input parameter self :: <GDrive>;
  input parameter kind_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_drive_get_identifier";
end;

define C-function g-drive-get-name
  input parameter self :: <GDrive>;
  result res :: <C-string>;
  c-name: "g_drive_get_name";
end;

define C-function g-drive-get-sort-key
  input parameter self :: <GDrive>;
  result res :: <C-string>;
  c-name: "g_drive_get_sort_key";
end;

define C-function g-drive-get-start-stop-type
  input parameter self :: <GDrive>;
  result res :: <GDriveStartStopType>;
  c-name: "g_drive_get_start_stop_type";
end;

define C-function g-drive-get-symbolic-icon
  input parameter self :: <GDrive>;
  result res :: <GIcon>;
  c-name: "g_drive_get_symbolic_icon";
end;

define C-function g-drive-get-volumes
  input parameter self :: <GDrive>;
  result res :: <GList>;
  c-name: "g_drive_get_volumes";
end;

define C-function g-drive-has-media
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_has_media";
end;

define C-function g-drive-has-volumes
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_has_volumes";
end;

define C-function g-drive-is-media-check-automatic
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_is_media_check_automatic";
end;

define C-function g-drive-is-media-removable
  input parameter self :: <GDrive>;
  result res :: <C-boolean>;
  c-name: "g_drive_is_media_removable";
end;

define C-function g-drive-poll-for-media
  input parameter self :: <GDrive>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_drive_poll_for_media";
end;

define C-function g-drive-poll-for-media-finish
  input parameter self :: <GDrive>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_drive_poll_for_media_finish";
end;

define C-function g-drive-start
  input parameter self :: <GDrive>;
  input parameter flags_ :: <GDriveStartFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_drive_start";
end;

define C-function g-drive-start-finish
  input parameter self :: <GDrive>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_drive_start_finish";
end;

define C-function g-drive-stop
  input parameter self :: <GDrive>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_drive_stop";
end;

define C-function g-drive-stop-finish
  input parameter self :: <GDrive>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_drive_stop_finish";
end;

define C-struct <_GDriveIface>
  constant slot gdriveiface-g-iface :: <GTypeInterface>;
  constant slot gdriveiface-changed :: <C-function-pointer>;
  constant slot gdriveiface-disconnected :: <C-function-pointer>;
  constant slot gdriveiface-eject-button :: <C-function-pointer>;
  constant slot gdriveiface-get-name :: <C-function-pointer>;
  constant slot gdriveiface-get-icon :: <C-function-pointer>;
  constant slot gdriveiface-has-volumes :: <C-function-pointer>;
  constant slot gdriveiface-get-volumes :: <C-function-pointer>;
  constant slot gdriveiface-is-media-removable :: <C-function-pointer>;
  constant slot gdriveiface-has-media :: <C-function-pointer>;
  constant slot gdriveiface-is-media-check-automatic :: <C-function-pointer>;
  constant slot gdriveiface-can-eject :: <C-function-pointer>;
  constant slot gdriveiface-can-poll-for-media :: <C-function-pointer>;
  constant slot gdriveiface-eject :: <C-function-pointer>;
  constant slot gdriveiface-eject-finish :: <C-function-pointer>;
  constant slot gdriveiface-poll-for-media :: <C-function-pointer>;
  constant slot gdriveiface-poll-for-media-finish :: <C-function-pointer>;
  constant slot gdriveiface-get-identifier :: <C-function-pointer>;
  constant slot gdriveiface-enumerate-identifiers :: <C-function-pointer>;
  constant slot gdriveiface-get-start-stop-type :: <C-function-pointer>;
  constant slot gdriveiface-can-start :: <C-function-pointer>;
  constant slot gdriveiface-can-start-degraded :: <C-function-pointer>;
  constant slot gdriveiface-start :: <C-function-pointer>;
  constant slot gdriveiface-start-finish :: <C-function-pointer>;
  constant slot gdriveiface-can-stop :: <C-function-pointer>;
  constant slot gdriveiface-stop :: <C-function-pointer>;
  constant slot gdriveiface-stop-finish :: <C-function-pointer>;
  constant slot gdriveiface-stop-button :: <C-function-pointer>;
  constant slot gdriveiface-eject-with-operation :: <C-function-pointer>;
  constant slot gdriveiface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot gdriveiface-get-sort-key :: <C-function-pointer>;
  constant slot gdriveiface-get-symbolic-icon :: <C-function-pointer>;
  pointer-type-name: <GDriveIface>;
end C-struct;

define constant $G-DRIVE-START-NONE = 0;
define constant <GDriveStartFlags> = <C-int>;
define C-pointer-type <GDriveStartFlags*> => <GDriveStartFlags>;

define constant $G-DRIVE-START-STOP-TYPE-UNKNOWN = 0;
define constant $G-DRIVE-START-STOP-TYPE-SHUTDOWN = 1;
define constant $G-DRIVE-START-STOP-TYPE-NETWORK = 2;
define constant $G-DRIVE-START-STOP-TYPE-MULTIDISK = 3;
define constant $G-DRIVE-START-STOP-TYPE-PASSWORD = 4;
define constant <GDriveStartStopType> = <C-int>;
define C-pointer-type <GDriveStartStopType*> => <GDriveStartStopType>;

define open C-subtype <GEmblem> (<GObject>)
end C-subtype;

define C-pointer-type <GEmblem*> => <GEmblem>;

define property-getter emblem-icon :: <GObject> on <GEmblem> end;
define property-setter emblem-icon :: <GObject> on <GEmblem> end;
define property-getter emblem-origin :: <GEmblemOrigin> on <GEmblem> end;
define property-setter emblem-origin :: <GEmblemOrigin> on <GEmblem> end;
define C-function g-emblem-new
  input parameter icon_ :: <GIcon>;
  result res :: <GEmblem>;
  c-name: "g_emblem_new";
end;

define C-function g-emblem-new-with-origin
  input parameter icon_ :: <GIcon>;
  input parameter origin_ :: <GEmblemOrigin>;
  result res :: <GEmblem>;
  c-name: "g_emblem_new_with_origin";
end;

define C-function g-emblem-get-icon
  input parameter self :: <GEmblem>;
  result res :: <GIcon>;
  c-name: "g_emblem_get_icon";
end;

define C-function g-emblem-get-origin
  input parameter self :: <GEmblem>;
  result res :: <GEmblemOrigin>;
  c-name: "g_emblem_get_origin";
end;

define C-struct <_GEmblemClass>
  pointer-type-name: <GEmblemClass>;
end C-struct;

define constant $G-EMBLEM-ORIGIN-UNKNOWN = 0;
define constant $G-EMBLEM-ORIGIN-DEVICE = 1;
define constant $G-EMBLEM-ORIGIN-LIVEMETADATA = 2;
define constant $G-EMBLEM-ORIGIN-TAG = 3;
define constant <GEmblemOrigin> = <C-int>;
define C-pointer-type <GEmblemOrigin*> => <GEmblemOrigin>;

define open C-subtype <GEmblemedIcon> (<GObject>)
  constant slot gemblemedicon-parent-instance :: <GObject>;
  constant slot gemblemedicon-priv :: <GEmblemedIconPrivate>;
end C-subtype;

define C-pointer-type <GEmblemedIcon*> => <GEmblemedIcon>;

define property-getter emblemedicon-gicon :: <GIcon> on <GEmblemedIcon> end;
define property-setter emblemedicon-gicon :: <GIcon> on <GEmblemedIcon> end;
define C-function g-emblemed-icon-new
  input parameter icon_ :: <GIcon>;
  input parameter emblem_ :: <GEmblem>;
  result res :: <GEmblemedIcon>;
  c-name: "g_emblemed_icon_new";
end;

define C-function g-emblemed-icon-add-emblem
  input parameter self :: <GEmblemedIcon>;
  input parameter emblem_ :: <GEmblem>;
  c-name: "g_emblemed_icon_add_emblem";
end;

define C-function g-emblemed-icon-clear-emblems
  input parameter self :: <GEmblemedIcon>;
  c-name: "g_emblemed_icon_clear_emblems";
end;

define C-function g-emblemed-icon-get-emblems
  input parameter self :: <GEmblemedIcon>;
  result res :: <GList>;
  c-name: "g_emblemed_icon_get_emblems";
end;

define C-function g-emblemed-icon-get-icon
  input parameter self :: <GEmblemedIcon>;
  result res :: <GIcon>;
  c-name: "g_emblemed_icon_get_icon";
end;

define C-struct <_GEmblemedIconClass>
  constant slot gemblemediconclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GEmblemedIconClass>;
end C-struct;

define C-struct <_GEmblemedIconPrivate>
  pointer-type-name: <GEmblemedIconPrivate>;
end C-struct;

define constant $FILE-ATTRIBUTE-ACCESS-CAN-DELETE = "access::can-delete";

define constant $FILE-ATTRIBUTE-ACCESS-CAN-EXECUTE = "access::can-execute";

define constant $FILE-ATTRIBUTE-ACCESS-CAN-READ = "access::can-read";

define constant $FILE-ATTRIBUTE-ACCESS-CAN-RENAME = "access::can-rename";

define constant $FILE-ATTRIBUTE-ACCESS-CAN-TRASH = "access::can-trash";

define constant $FILE-ATTRIBUTE-ACCESS-CAN-WRITE = "access::can-write";

define constant $FILE-ATTRIBUTE-DOS-IS-ARCHIVE = "dos::is-archive";

define constant $FILE-ATTRIBUTE-DOS-IS-SYSTEM = "dos::is-system";

define constant $FILE-ATTRIBUTE-ETAG-VALUE = "etag::value";

define constant $FILE-ATTRIBUTE-FILESYSTEM-FREE = "filesystem::free";

define constant $FILE-ATTRIBUTE-FILESYSTEM-READONLY = "filesystem::readonly";

define constant $FILE-ATTRIBUTE-FILESYSTEM-SIZE = "filesystem::size";

define constant $FILE-ATTRIBUTE-FILESYSTEM-TYPE = "filesystem::type";

define constant $FILE-ATTRIBUTE-FILESYSTEM-USED = "filesystem::used";

define constant $FILE-ATTRIBUTE-FILESYSTEM-USE-PREVIEW = "filesystem::use-preview";

define constant $FILE-ATTRIBUTE-GVFS-BACKEND = "gvfs::backend";

define constant $FILE-ATTRIBUTE-ID-FILE = "id::file";

define constant $FILE-ATTRIBUTE-ID-FILESYSTEM = "id::filesystem";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-EJECT = "mountable::can-eject";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-MOUNT = "mountable::can-mount";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-POLL = "mountable::can-poll";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-START = "mountable::can-start";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-START-DEGRADED = "mountable::can-start-degraded";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-STOP = "mountable::can-stop";

define constant $FILE-ATTRIBUTE-MOUNTABLE-CAN-UNMOUNT = "mountable::can-unmount";

define constant $FILE-ATTRIBUTE-MOUNTABLE-HAL-UDI = "mountable::hal-udi";

define constant $FILE-ATTRIBUTE-MOUNTABLE-IS-MEDIA-CHECK-AUTOMATIC = "mountable::is-media-check-automatic";

define constant $FILE-ATTRIBUTE-MOUNTABLE-START-STOP-TYPE = "mountable::start-stop-type";

define constant $FILE-ATTRIBUTE-MOUNTABLE-UNIX-DEVICE = "mountable::unix-device";

define constant $FILE-ATTRIBUTE-MOUNTABLE-UNIX-DEVICE-FILE = "mountable::unix-device-file";

define constant $FILE-ATTRIBUTE-OWNER-GROUP = "owner::group";

define constant $FILE-ATTRIBUTE-OWNER-USER = "owner::user";

define constant $FILE-ATTRIBUTE-OWNER-USER-REAL = "owner::user-real";

define constant $FILE-ATTRIBUTE-PREVIEW-ICON = "preview::icon";

define constant $FILE-ATTRIBUTE-SELINUX-CONTEXT = "selinux::context";

define constant $FILE-ATTRIBUTE-STANDARD-ALLOCATED-SIZE = "standard::allocated-size";

define constant $FILE-ATTRIBUTE-STANDARD-CONTENT-TYPE = "standard::content-type";

define constant $FILE-ATTRIBUTE-STANDARD-COPY-NAME = "standard::copy-name";

define constant $FILE-ATTRIBUTE-STANDARD-DESCRIPTION = "standard::description";

define constant $FILE-ATTRIBUTE-STANDARD-DISPLAY-NAME = "standard::display-name";

define constant $FILE-ATTRIBUTE-STANDARD-EDIT-NAME = "standard::edit-name";

define constant $FILE-ATTRIBUTE-STANDARD-FAST-CONTENT-TYPE = "standard::fast-content-type";

define constant $FILE-ATTRIBUTE-STANDARD-ICON = "standard::icon";

define constant $FILE-ATTRIBUTE-STANDARD-IS-BACKUP = "standard::is-backup";

define constant $FILE-ATTRIBUTE-STANDARD-IS-HIDDEN = "standard::is-hidden";

define constant $FILE-ATTRIBUTE-STANDARD-IS-SYMLINK = "standard::is-symlink";

define constant $FILE-ATTRIBUTE-STANDARD-IS-VIRTUAL = "standard::is-virtual";

define constant $FILE-ATTRIBUTE-STANDARD-NAME = "standard::name";

define constant $FILE-ATTRIBUTE-STANDARD-SIZE = "standard::size";

define constant $FILE-ATTRIBUTE-STANDARD-SORT-ORDER = "standard::sort-order";

define constant $FILE-ATTRIBUTE-STANDARD-SYMBOLIC-ICON = "standard::symbolic-icon";

define constant $FILE-ATTRIBUTE-STANDARD-SYMLINK-TARGET = "standard::symlink-target";

define constant $FILE-ATTRIBUTE-STANDARD-TARGET-URI = "standard::target-uri";

define constant $FILE-ATTRIBUTE-STANDARD-TYPE = "standard::type";

define constant $FILE-ATTRIBUTE-THUMBNAILING-FAILED = "thumbnail::failed";

define constant $FILE-ATTRIBUTE-THUMBNAIL-PATH = "thumbnail::path";

define constant $FILE-ATTRIBUTE-TIME-ACCESS = "time::access";

define constant $FILE-ATTRIBUTE-TIME-ACCESS-USEC = "time::access-usec";

define constant $FILE-ATTRIBUTE-TIME-CHANGED = "time::changed";

define constant $FILE-ATTRIBUTE-TIME-CHANGED-USEC = "time::changed-usec";

define constant $FILE-ATTRIBUTE-TIME-CREATED = "time::created";

define constant $FILE-ATTRIBUTE-TIME-CREATED-USEC = "time::created-usec";

define constant $FILE-ATTRIBUTE-TIME-MODIFIED = "time::modified";

define constant $FILE-ATTRIBUTE-TIME-MODIFIED-USEC = "time::modified-usec";

define constant $FILE-ATTRIBUTE-TRASH-DELETION-DATE = "trash::deletion-date";

define constant $FILE-ATTRIBUTE-TRASH-ITEM-COUNT = "trash::item-count";

define constant $FILE-ATTRIBUTE-TRASH-ORIG-PATH = "trash::orig-path";

define constant $FILE-ATTRIBUTE-UNIX-BLOCKS = "unix::blocks";

define constant $FILE-ATTRIBUTE-UNIX-BLOCK-SIZE = "unix::block-size";

define constant $FILE-ATTRIBUTE-UNIX-DEVICE = "unix::device";

define constant $FILE-ATTRIBUTE-UNIX-GID = "unix::gid";

define constant $FILE-ATTRIBUTE-UNIX-INODE = "unix::inode";

define constant $FILE-ATTRIBUTE-UNIX-IS-MOUNTPOINT = "unix::is-mountpoint";

define constant $FILE-ATTRIBUTE-UNIX-MODE = "unix::mode";

define constant $FILE-ATTRIBUTE-UNIX-NLINK = "unix::nlink";

define constant $FILE-ATTRIBUTE-UNIX-RDEV = "unix::rdev";

define constant $FILE-ATTRIBUTE-UNIX-UID = "unix::uid";

// Interface
define open C-subtype <GFile> (<C-void*>)
end C-subtype;

define C-pointer-type <GFile*> => <GFile>;

define C-function g-file-new-for-commandline-arg
  input parameter arg_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_new_for_commandline_arg";
end;

define C-function g-file-new-for-commandline-arg-and-cwd
  input parameter arg_ :: <C-string>;
  input parameter cwd_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_new_for_commandline_arg_and_cwd";
end;

define C-function g-file-new-for-path
  input parameter path_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_new_for_path";
end;

define C-function g-file-new-for-uri
  input parameter uri_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_new_for_uri";
end;

define C-function g-file-new-tmp
  input parameter tmpl_ :: <C-string>;
  output parameter iostream_ :: <GFileIOStream*>;
  result res :: <GFile>;
  c-name: "g_file_new_tmp";
end;

define C-function g-file-parse-name
  input parameter parse_name_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_parse_name";
end;

define C-function g-file-append-to
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_append_to";
end;

define C-function g-file-append-to-async
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_append_to_async";
end;

define C-function g-file-append-to-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_append_to_finish";
end;

define C-function g-file-copy
  input parameter self :: <GFile>;
  input parameter destination_ :: <GFile>;
  input parameter flags_ :: <GFileCopyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter progress_callback_ :: <C-function-pointer>;
  input parameter progress_callback_data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_file_copy";
end;

define C-function g-file-copy-attributes
  input parameter self :: <GFile>;
  input parameter destination_ :: <GFile>;
  input parameter flags_ :: <GFileCopyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_copy_attributes";
end;

define C-function g-file-copy-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_copy_finish";
end;

define C-function g-file-create
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_create";
end;

define C-function g-file-create-async
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_create_async";
end;

define C-function g-file-create-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_create_finish";
end;

define C-function g-file-create-readwrite
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileIOStream>;
  c-name: "g_file_create_readwrite";
end;

define C-function g-file-create-readwrite-async
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_create_readwrite_async";
end;

define C-function g-file-create-readwrite-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileIOStream>;
  c-name: "g_file_create_readwrite_finish";
end;

define C-function g-file-delete
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_delete";
end;

define C-function g-file-delete-async
  input parameter self :: <GFile>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_delete_async";
end;

define C-function g-file-delete-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_delete_finish";
end;

define C-function g-file-dup
  input parameter self :: <GFile>;
  result res :: <GFile>;
  c-name: "g_file_dup";
end;

define C-function g-file-eject-mountable
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_eject_mountable";
end;

define C-function g-file-eject-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_eject_mountable_finish";
end;

define C-function g-file-eject-mountable-with-operation
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_eject_mountable_with_operation";
end;

define C-function g-file-eject-mountable-with-operation-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_eject_mountable_with_operation_finish";
end;

define C-function g-file-enumerate-children
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileEnumerator>;
  c-name: "g_file_enumerate_children";
end;

define C-function g-file-enumerate-children-async
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_enumerate_children_async";
end;

define C-function g-file-enumerate-children-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileEnumerator>;
  c-name: "g_file_enumerate_children_finish";
end;

define C-function g-file-equal
  input parameter self :: <GFile>;
  input parameter file2_ :: <GFile>;
  result res :: <C-boolean>;
  c-name: "g_file_equal";
end;

define C-function g-file-find-enclosing-mount
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GMount>;
  c-name: "g_file_find_enclosing_mount";
end;

define C-function g-file-find-enclosing-mount-async
  input parameter self :: <GFile>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_find_enclosing_mount_async";
end;

define C-function g-file-find-enclosing-mount-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GMount>;
  c-name: "g_file_find_enclosing_mount_finish";
end;

define C-function g-file-get-basename
  input parameter self :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_basename";
end;

define C-function g-file-get-child
  input parameter self :: <GFile>;
  input parameter name_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_get_child";
end;

define C-function g-file-get-child-for-display-name
  input parameter self :: <GFile>;
  input parameter display_name_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_get_child_for_display_name";
end;

define C-function g-file-get-parent
  input parameter self :: <GFile>;
  result res :: <GFile>;
  c-name: "g_file_get_parent";
end;

define C-function g-file-get-parse-name
  input parameter self :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_parse_name";
end;

define C-function g-file-get-path
  input parameter self :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_path";
end;

define C-function g-file-get-relative-path
  input parameter self :: <GFile>;
  input parameter descendant_ :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_relative_path";
end;

define C-function g-file-get-uri
  input parameter self :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_uri";
end;

define C-function g-file-get-uri-scheme
  input parameter self :: <GFile>;
  result res :: <C-string>;
  c-name: "g_file_get_uri_scheme";
end;

define C-function g-file-has-parent
  input parameter self :: <GFile>;
  input parameter parent_ :: <GFile>;
  result res :: <C-boolean>;
  c-name: "g_file_has_parent";
end;

define C-function g-file-has-prefix
  input parameter self :: <GFile>;
  input parameter prefix_ :: <GFile>;
  result res :: <C-boolean>;
  c-name: "g_file_has_prefix";
end;

define C-function g-file-has-uri-scheme
  input parameter self :: <GFile>;
  input parameter uri_scheme_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_has_uri_scheme";
end;

define C-function g-file-hash
  input parameter self :: <GFile>;
  result res :: <C-unsigned-int>;
  c-name: "g_file_hash";
end;

define C-function g-file-is-native
  input parameter self :: <GFile>;
  result res :: <C-boolean>;
  c-name: "g_file_is_native";
end;

define C-function g-file-load-contents
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter contents_ :: <C-unsigned-char*>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter etag_out_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_load_contents";
end;

define C-function g-file-load-contents-async
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_load_contents_async";
end;

define C-function g-file-load-contents-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter contents_ :: <C-unsigned-char*>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter etag_out_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_load_contents_finish";
end;

define C-function g-file-load-partial-contents-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter contents_ :: <C-unsigned-char*>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter etag_out_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_load_partial_contents_finish";
end;

define C-function g-file-make-directory
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_make_directory";
end;

define C-function g-file-make-directory-with-parents
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_make_directory_with_parents";
end;

define C-function g-file-make-symbolic-link
  input parameter self :: <GFile>;
  input parameter symlink_value_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_make_symbolic_link";
end;

define C-function g-file-monitor
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileMonitor>;
  c-name: "g_file_monitor";
end;

define C-function g-file-monitor-directory
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileMonitor>;
  c-name: "g_file_monitor_directory";
end;

define C-function g-file-monitor-file
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileMonitor>;
  c-name: "g_file_monitor_file";
end;

define C-function g-file-mount-enclosing-volume
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountMountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_mount_enclosing_volume";
end;

define C-function g-file-mount-enclosing-volume-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_mount_enclosing_volume_finish";
end;

define C-function g-file-mount-mountable
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountMountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_mount_mountable";
end;

define C-function g-file-mount-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GFile>;
  c-name: "g_file_mount_mountable_finish";
end;

define C-function g-file-move
  input parameter self :: <GFile>;
  input parameter destination_ :: <GFile>;
  input parameter flags_ :: <GFileCopyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter progress_callback_ :: <C-function-pointer>;
  input parameter progress_callback_data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_file_move";
end;

define C-function g-file-open-readwrite
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileIOStream>;
  c-name: "g_file_open_readwrite";
end;

define C-function g-file-open-readwrite-async
  input parameter self :: <GFile>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_open_readwrite_async";
end;

define C-function g-file-open-readwrite-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileIOStream>;
  c-name: "g_file_open_readwrite_finish";
end;

define C-function g-file-poll-mountable
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_poll_mountable";
end;

define C-function g-file-poll-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_poll_mountable_finish";
end;

define C-function g-file-query-default-handler
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GAppInfo>;
  c-name: "g_file_query_default_handler";
end;

define C-function g-file-query-exists
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_query_exists";
end;

define C-function g-file-query-file-type
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileType>;
  c-name: "g_file_query_file_type";
end;

define C-function g-file-query-filesystem-info
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_filesystem_info";
end;

define C-function g-file-query-filesystem-info-async
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_query_filesystem_info_async";
end;

define C-function g-file-query-filesystem-info-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_filesystem_info_finish";
end;

define C-function g-file-query-info
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_info";
end;

define C-function g-file-query-info-async
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_query_info_async";
end;

define C-function g-file-query-info-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_info_finish";
end;

define C-function g-file-query-settable-attributes
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_query_settable_attributes";
end;

define C-function g-file-query-writable-namespaces
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_query_writable_namespaces";
end;

define C-function g-file-read
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInputStream>;
  c-name: "g_file_read";
end;

define C-function g-file-read-async
  input parameter self :: <GFile>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_read_async";
end;

define C-function g-file-read-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileInputStream>;
  c-name: "g_file_read_finish";
end;

define C-function g-file-replace
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_replace";
end;

define C-function g-file-replace-async
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_replace_async";
end;

define C-function g-file-replace-contents
  input parameter self :: <GFile>;
  input parameter contents_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  output parameter new_etag_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_replace_contents";
end;

define C-function g-file-replace-contents-async
  input parameter self :: <GFile>;
  input parameter contents_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_replace_contents_async";
end;

define C-function g-file-replace-contents-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter new_etag_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_replace_contents_finish";
end;

define C-function g-file-replace-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_replace_finish";
end;

define C-function g-file-replace-readwrite
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileIOStream>;
  c-name: "g_file_replace_readwrite";
end;

define C-function g-file-replace-readwrite-async
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_replace_readwrite_async";
end;

define C-function g-file-replace-readwrite-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFileIOStream>;
  c-name: "g_file_replace_readwrite_finish";
end;

define C-function g-file-resolve-relative-path
  input parameter self :: <GFile>;
  input parameter relative_path_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_resolve_relative_path";
end;

define C-function g-file-set-attribute
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter type_ :: <GFileAttributeType>;
  input parameter value_p_ :: <C-void*>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute";
end;

define C-function g-file-set-attribute-byte-string
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_byte_string";
end;

define C-function g-file-set-attribute-int32
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_int32";
end;

define C-function g-file-set-attribute-int64
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-signed-long>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_int64";
end;

define C-function g-file-set-attribute-string
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_string";
end;

define C-function g-file-set-attribute-uint32
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-unsigned-int>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_uint32";
end;

define C-function g-file-set-attribute-uint64
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-unsigned-long>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_uint64";
end;

define C-function g-file-set-attributes-async
  input parameter self :: <GFile>;
  input parameter info_ :: <GFileInfo>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_set_attributes_async";
end;

define C-function g-file-set-attributes-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  output parameter info_ :: <GFileInfo*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attributes_finish";
end;

define C-function g-file-set-attributes-from-info
  input parameter self :: <GFile>;
  input parameter info_ :: <GFileInfo>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attributes_from_info";
end;

define C-function g-file-set-display-name
  input parameter self :: <GFile>;
  input parameter display_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFile>;
  c-name: "g_file_set_display_name";
end;

define C-function g-file-set-display-name-async
  input parameter self :: <GFile>;
  input parameter display_name_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_set_display_name_async";
end;

define C-function g-file-set-display-name-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  result res :: <GFile>;
  c-name: "g_file_set_display_name_finish";
end;

define C-function g-file-start-mountable
  input parameter self :: <GFile>;
  input parameter flags_ :: <GDriveStartFlags>;
  input parameter start_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_start_mountable";
end;

define C-function g-file-start-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_start_mountable_finish";
end;

define C-function g-file-stop-mountable
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_stop_mountable";
end;

define C-function g-file-stop-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_stop_mountable_finish";
end;

define C-function g-file-supports-thread-contexts
  input parameter self :: <GFile>;
  result res :: <C-boolean>;
  c-name: "g_file_supports_thread_contexts";
end;

define C-function g-file-trash
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_trash";
end;

define C-function g-file-unmount-mountable
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_unmount_mountable";
end;

define C-function g-file-unmount-mountable-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_unmount_mountable_finish";
end;

define C-function g-file-unmount-mountable-with-operation
  input parameter self :: <GFile>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_unmount_mountable_with_operation";
end;

define C-function g-file-unmount-mountable-with-operation-finish
  input parameter self :: <GFile>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_unmount_mountable_with_operation_finish";
end;

define C-struct <_GFileAttributeInfo>
  slot gfileattributeinfo-name :: <C-string>;
  slot gfileattributeinfo-type :: <GFileAttributeType>;
  slot gfileattributeinfo-flags :: <GFileAttributeInfoFlags>;
  pointer-type-name: <GFileAttributeInfo>;
end C-struct;

define constant $G-FILE-ATTRIBUTE-INFO-NONE = 0;
define constant $G-FILE-ATTRIBUTE-INFO-COPY-WITH-FILE = 1;
define constant $G-FILE-ATTRIBUTE-INFO-COPY-WHEN-MOVED = 2;
define constant <GFileAttributeInfoFlags> = <C-int>;
define C-pointer-type <GFileAttributeInfoFlags*> => <GFileAttributeInfoFlags>;

define C-struct <_GFileAttributeInfoList>
  slot gfileattributeinfolist-infos :: <GFileAttributeInfo>;
  slot gfileattributeinfolist-n-infos :: <C-signed-int>;
  pointer-type-name: <GFileAttributeInfoList>;
end C-struct;

define C-function g-file-attribute-info-list-new
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_attribute_info_list_new";
end;

define C-function g-file-attribute-info-list-add
  input parameter self :: <GFileAttributeInfoList>;
  input parameter name_ :: <C-string>;
  input parameter type_ :: <GFileAttributeType>;
  input parameter flags_ :: <GFileAttributeInfoFlags>;
  c-name: "g_file_attribute_info_list_add";
end;

define C-function g-file-attribute-info-list-dup
  input parameter self :: <GFileAttributeInfoList>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_attribute_info_list_dup";
end;

define C-function g-file-attribute-info-list-lookup
  input parameter self :: <GFileAttributeInfoList>;
  input parameter name_ :: <C-string>;
  result res :: <GFileAttributeInfo>;
  c-name: "g_file_attribute_info_list_lookup";
end;

define C-function g-file-attribute-info-list-ref
  input parameter self :: <GFileAttributeInfoList>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_attribute_info_list_ref";
end;

define C-function g-file-attribute-info-list-unref
  input parameter self :: <GFileAttributeInfoList>;
  c-name: "g_file_attribute_info_list_unref";
end;

define C-struct <_GFileAttributeMatcher>
  pointer-type-name: <GFileAttributeMatcher>;
end C-struct;

define C-function g-file-attribute-matcher-new
  input parameter attributes_ :: <C-string>;
  result res :: <GFileAttributeMatcher>;
  c-name: "g_file_attribute_matcher_new";
end;

define C-function g-file-attribute-matcher-enumerate-namespace
  input parameter self :: <GFileAttributeMatcher>;
  input parameter ns_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_attribute_matcher_enumerate_namespace";
end;

define C-function g-file-attribute-matcher-enumerate-next
  input parameter self :: <GFileAttributeMatcher>;
  result res :: <C-string>;
  c-name: "g_file_attribute_matcher_enumerate_next";
end;

define C-function g-file-attribute-matcher-matches
  input parameter self :: <GFileAttributeMatcher>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_attribute_matcher_matches";
end;

define C-function g-file-attribute-matcher-matches-only
  input parameter self :: <GFileAttributeMatcher>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_attribute_matcher_matches_only";
end;

define C-function g-file-attribute-matcher-ref
  input parameter self :: <GFileAttributeMatcher>;
  result res :: <GFileAttributeMatcher>;
  c-name: "g_file_attribute_matcher_ref";
end;

define C-function g-file-attribute-matcher-subtract
  input parameter self :: <GFileAttributeMatcher>;
  input parameter subtract_ :: <GFileAttributeMatcher>;
  result res :: <GFileAttributeMatcher>;
  c-name: "g_file_attribute_matcher_subtract";
end;

define C-function g-file-attribute-matcher-to-string
  input parameter self :: <GFileAttributeMatcher>;
  result res :: <C-string>;
  c-name: "g_file_attribute_matcher_to_string";
end;

define C-function g-file-attribute-matcher-unref
  input parameter self :: <GFileAttributeMatcher>;
  c-name: "g_file_attribute_matcher_unref";
end;

define constant $G-FILE-ATTRIBUTE-STATUS-UNSET = 0;
define constant $G-FILE-ATTRIBUTE-STATUS-SET = 1;
define constant $G-FILE-ATTRIBUTE-STATUS-ERROR-SETTING = 2;
define constant <GFileAttributeStatus> = <C-int>;
define C-pointer-type <GFileAttributeStatus*> => <GFileAttributeStatus>;

define constant $G-FILE-ATTRIBUTE-TYPE-INVALID = 0;
define constant $G-FILE-ATTRIBUTE-TYPE-STRING = 1;
define constant $G-FILE-ATTRIBUTE-TYPE-BYTE-STRING = 2;
define constant $G-FILE-ATTRIBUTE-TYPE-BOOLEAN = 3;
define constant $G-FILE-ATTRIBUTE-TYPE-UINT32 = 4;
define constant $G-FILE-ATTRIBUTE-TYPE-INT32 = 5;
define constant $G-FILE-ATTRIBUTE-TYPE-UINT64 = 6;
define constant $G-FILE-ATTRIBUTE-TYPE-INT64 = 7;
define constant $G-FILE-ATTRIBUTE-TYPE-OBJECT = 8;
define constant $G-FILE-ATTRIBUTE-TYPE-STRINGV = 9;
define constant <GFileAttributeType> = <C-int>;
define C-pointer-type <GFileAttributeType*> => <GFileAttributeType>;

define constant $G-FILE-COPY-NONE = 0;
define constant $G-FILE-COPY-OVERWRITE = 1;
define constant $G-FILE-COPY-BACKUP = 2;
define constant $G-FILE-COPY-NOFOLLOW-SYMLINKS = 4;
define constant $G-FILE-COPY-ALL-METADATA = 8;
define constant $G-FILE-COPY-NO-FALLBACK-FOR-MOVE = 16;
define constant $G-FILE-COPY-TARGET-DEFAULT-PERMS = 32;
define constant <GFileCopyFlags> = <C-int>;
define C-pointer-type <GFileCopyFlags*> => <GFileCopyFlags>;

define constant $G-FILE-CREATE-NONE = 0;
define constant $G-FILE-CREATE-PRIVATE = 1;
define constant $G-FILE-CREATE-REPLACE-DESTINATION = 2;
define constant <GFileCreateFlags> = <C-int>;
define C-pointer-type <GFileCreateFlags*> => <GFileCreateFlags>;

// Interface
define open C-subtype <GFileDescriptorBased> (<C-void*>)
end C-subtype;

define C-pointer-type <GFileDescriptorBased*> => <GFileDescriptorBased>;

define C-function g-file-descriptor-based-get-fd
  input parameter self :: <GFileDescriptorBased>;
  result res :: <C-signed-int>;
  c-name: "g_file_descriptor_based_get_fd";
end;

define C-struct <_GFileDescriptorBasedIface>
  constant slot gfiledescriptorbasediface-g-iface :: <GTypeInterface>;
  constant slot gfiledescriptorbasediface-get-fd :: <C-function-pointer>;
  pointer-type-name: <GFileDescriptorBasedIface>;
end C-struct;

define open C-subtype <GFileEnumerator> (<GObject>)
  constant slot gfileenumerator-parent-instance :: <GObject>;
  constant slot gfileenumerator-priv :: <GFileEnumeratorPrivate>;
end C-subtype;

define C-pointer-type <GFileEnumerator*> => <GFileEnumerator>;

define property-setter fileenumerator-container :: <GFile> on <GFileEnumerator> end;
define C-function g-file-enumerator-close
  input parameter self :: <GFileEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_file_enumerator_close";
end;

define C-function g-file-enumerator-close-async
  input parameter self :: <GFileEnumerator>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_enumerator_close_async";
end;

define C-function g-file-enumerator-close-finish
  input parameter self :: <GFileEnumerator>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_file_enumerator_close_finish";
end;

define C-function g-file-enumerator-get-child
  input parameter self :: <GFileEnumerator>;
  input parameter info_ :: <GFileInfo>;
  result res :: <GFile>;
  c-name: "g_file_enumerator_get_child";
end;

define C-function g-file-enumerator-get-container
  input parameter self :: <GFileEnumerator>;
  result res :: <GFile>;
  c-name: "g_file_enumerator_get_container";
end;

define C-function g-file-enumerator-has-pending
  input parameter self :: <GFileEnumerator>;
  result res :: <C-boolean>;
  c-name: "g_file_enumerator_has_pending";
end;

define C-function g-file-enumerator-is-closed
  input parameter self :: <GFileEnumerator>;
  result res :: <C-boolean>;
  c-name: "g_file_enumerator_is_closed";
end;

define C-function g-file-enumerator-next-file
  input parameter self :: <GFileEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_enumerator_next_file";
end;

define C-function g-file-enumerator-next-files-async
  input parameter self :: <GFileEnumerator>;
  input parameter num_files_ :: <C-signed-int>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_enumerator_next_files_async";
end;

define C-function g-file-enumerator-next-files-finish
  input parameter self :: <GFileEnumerator>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GList>;
  c-name: "g_file_enumerator_next_files_finish";
end;

define C-function g-file-enumerator-set-pending
  input parameter self :: <GFileEnumerator>;
  input parameter pending_ :: <C-boolean>;
  c-name: "g_file_enumerator_set_pending";
end;

define C-struct <_GFileEnumeratorClass>
  constant slot gfileenumeratorclass-parent-class :: <GObjectClass>;
  constant slot gfileenumeratorclass-next-file :: <C-function-pointer>;
  constant slot gfileenumeratorclass-close-fn :: <C-function-pointer>;
  constant slot gfileenumeratorclass-next-files-async :: <C-function-pointer>;
  constant slot gfileenumeratorclass-next-files-finish :: <C-function-pointer>;
  constant slot gfileenumeratorclass-close-async :: <C-function-pointer>;
  constant slot gfileenumeratorclass-close-finish :: <C-function-pointer>;
  constant slot gfileenumeratorclass--g-reserved1 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved2 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved3 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved4 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved5 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved6 :: <C-void*>;
  constant slot gfileenumeratorclass--g-reserved7 :: <C-void*>;
  pointer-type-name: <GFileEnumeratorClass>;
end C-struct;

define C-struct <_GFileEnumeratorPrivate>
  pointer-type-name: <GFileEnumeratorPrivate>;
end C-struct;

define open C-subtype <GFileIOStream> (<GIOStream>)
  constant slot gfileiostream-parent-instance :: <GIOStream>;
  constant slot gfileiostream-priv :: <GFileIOStreamPrivate>;
end C-subtype;

define C-pointer-type <GFileIOStream*> => <GFileIOStream>;

define C-function g-file-io-stream-get-etag
  input parameter self :: <GFileIOStream>;
  result res :: <C-string>;
  c-name: "g_file_io_stream_get_etag";
end;

define C-function g-file-io-stream-query-info
  input parameter self :: <GFileIOStream>;
  input parameter attributes_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_io_stream_query_info";
end;

define C-function g-file-io-stream-query-info-async
  input parameter self :: <GFileIOStream>;
  input parameter attributes_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_io_stream_query_info_async";
end;

define C-function g-file-io-stream-query-info-finish
  input parameter self :: <GFileIOStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GFileInfo>;
  c-name: "g_file_io_stream_query_info_finish";
end;

define C-struct <_GFileIOStreamClass>
  constant slot gfileiostreamclass-parent-class :: <GIOStreamClass>;
  constant slot gfileiostreamclass-tell :: <C-function-pointer>;
  constant slot gfileiostreamclass-can-seek :: <C-function-pointer>;
  constant slot gfileiostreamclass-seek :: <C-function-pointer>;
  constant slot gfileiostreamclass-can-truncate :: <C-function-pointer>;
  constant slot gfileiostreamclass-truncate-fn :: <C-function-pointer>;
  constant slot gfileiostreamclass-query-info :: <C-function-pointer>;
  constant slot gfileiostreamclass-query-info-async :: <C-function-pointer>;
  constant slot gfileiostreamclass-query-info-finish :: <C-function-pointer>;
  constant slot gfileiostreamclass-get-etag :: <C-function-pointer>;
  constant slot gfileiostreamclass--g-reserved1 :: <C-void*>;
  constant slot gfileiostreamclass--g-reserved2 :: <C-void*>;
  constant slot gfileiostreamclass--g-reserved3 :: <C-void*>;
  constant slot gfileiostreamclass--g-reserved4 :: <C-void*>;
  constant slot gfileiostreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileIOStreamClass>;
end C-struct;

define C-struct <_GFileIOStreamPrivate>
  pointer-type-name: <GFileIOStreamPrivate>;
end C-struct;

define open C-subtype <GFileIcon> (<GObject>)
end C-subtype;

define C-pointer-type <GFileIcon*> => <GFileIcon>;

define property-getter fileicon-file :: <GFile> on <GFileIcon> end;
define property-setter fileicon-file :: <GFile> on <GFileIcon> end;
define C-function g-file-icon-new
  input parameter file_ :: <GFile>;
  result res :: <GFileIcon>;
  c-name: "g_file_icon_new";
end;

define C-function g-file-icon-get-file
  input parameter self :: <GFileIcon>;
  result res :: <GFile>;
  c-name: "g_file_icon_get_file";
end;

define C-struct <_GFileIconClass>
  pointer-type-name: <GFileIconClass>;
end C-struct;

define C-struct <_GFileIface>
  constant slot gfileiface-g-iface :: <GTypeInterface>;
  constant slot gfileiface-dup :: <C-function-pointer>;
  constant slot gfileiface-hash :: <C-function-pointer>;
  constant slot gfileiface-equal :: <C-function-pointer>;
  constant slot gfileiface-is-native :: <C-function-pointer>;
  constant slot gfileiface-has-uri-scheme :: <C-function-pointer>;
  constant slot gfileiface-get-uri-scheme :: <C-function-pointer>;
  constant slot gfileiface-get-basename :: <C-function-pointer>;
  constant slot gfileiface-get-path :: <C-function-pointer>;
  constant slot gfileiface-get-uri :: <C-function-pointer>;
  constant slot gfileiface-get-parse-name :: <C-function-pointer>;
  constant slot gfileiface-get-parent :: <C-function-pointer>;
  constant slot gfileiface-prefix-matches :: <C-function-pointer>;
  constant slot gfileiface-get-relative-path :: <C-function-pointer>;
  constant slot gfileiface-resolve-relative-path :: <C-function-pointer>;
  constant slot gfileiface-get-child-for-display-name :: <C-function-pointer>;
  constant slot gfileiface-enumerate-children :: <C-function-pointer>;
  constant slot gfileiface-enumerate-children-async :: <C-function-pointer>;
  constant slot gfileiface-enumerate-children-finish :: <C-function-pointer>;
  constant slot gfileiface-query-info :: <C-function-pointer>;
  constant slot gfileiface-query-info-async :: <C-function-pointer>;
  constant slot gfileiface-query-info-finish :: <C-function-pointer>;
  constant slot gfileiface-query-filesystem-info :: <C-function-pointer>;
  constant slot gfileiface-query-filesystem-info-async :: <C-function-pointer>;
  constant slot gfileiface-query-filesystem-info-finish :: <C-function-pointer>;
  constant slot gfileiface-find-enclosing-mount :: <C-function-pointer>;
  constant slot gfileiface-find-enclosing-mount-async :: <C-function-pointer>;
  constant slot gfileiface-find-enclosing-mount-finish :: <C-function-pointer>;
  constant slot gfileiface-set-display-name :: <C-function-pointer>;
  constant slot gfileiface-set-display-name-async :: <C-function-pointer>;
  constant slot gfileiface-set-display-name-finish :: <C-function-pointer>;
  constant slot gfileiface-query-settable-attributes :: <C-function-pointer>;
  constant slot gfileiface--query-settable-attributes-async :: <C-void*>;
  constant slot gfileiface--query-settable-attributes-finish :: <C-void*>;
  constant slot gfileiface-query-writable-namespaces :: <C-function-pointer>;
  constant slot gfileiface--query-writable-namespaces-async :: <C-void*>;
  constant slot gfileiface--query-writable-namespaces-finish :: <C-void*>;
  constant slot gfileiface-set-attribute :: <C-function-pointer>;
  constant slot gfileiface-set-attributes-from-info :: <C-function-pointer>;
  constant slot gfileiface-set-attributes-async :: <C-function-pointer>;
  constant slot gfileiface-set-attributes-finish :: <C-function-pointer>;
  constant slot gfileiface-read-fn :: <C-function-pointer>;
  constant slot gfileiface-read-async :: <C-function-pointer>;
  constant slot gfileiface-read-finish :: <C-function-pointer>;
  constant slot gfileiface-append-to :: <C-function-pointer>;
  constant slot gfileiface-append-to-async :: <C-function-pointer>;
  constant slot gfileiface-append-to-finish :: <C-function-pointer>;
  constant slot gfileiface-create :: <C-function-pointer>;
  constant slot gfileiface-create-async :: <C-function-pointer>;
  constant slot gfileiface-create-finish :: <C-function-pointer>;
  constant slot gfileiface-replace :: <C-function-pointer>;
  constant slot gfileiface-replace-async :: <C-function-pointer>;
  constant slot gfileiface-replace-finish :: <C-function-pointer>;
  constant slot gfileiface-delete-file :: <C-function-pointer>;
  constant slot gfileiface-delete-file-async :: <C-function-pointer>;
  constant slot gfileiface-delete-file-finish :: <C-function-pointer>;
  constant slot gfileiface-trash :: <C-function-pointer>;
  constant slot gfileiface--trash-async :: <C-void*>;
  constant slot gfileiface--trash-finish :: <C-void*>;
  constant slot gfileiface-make-directory :: <C-function-pointer>;
  constant slot gfileiface--make-directory-async :: <C-void*>;
  constant slot gfileiface--make-directory-finish :: <C-void*>;
  constant slot gfileiface-make-symbolic-link :: <C-function-pointer>;
  constant slot gfileiface--make-symbolic-link-async :: <C-void*>;
  constant slot gfileiface--make-symbolic-link-finish :: <C-void*>;
  constant slot gfileiface-copy :: <C-function-pointer>;
  constant slot gfileiface-copy-async :: <C-void*>;
  constant slot gfileiface-copy-finish :: <C-function-pointer>;
  constant slot gfileiface-move :: <C-function-pointer>;
  constant slot gfileiface--move-async :: <C-void*>;
  constant slot gfileiface--move-finish :: <C-void*>;
  constant slot gfileiface-mount-mountable :: <C-function-pointer>;
  constant slot gfileiface-mount-mountable-finish :: <C-function-pointer>;
  constant slot gfileiface-unmount-mountable :: <C-function-pointer>;
  constant slot gfileiface-unmount-mountable-finish :: <C-function-pointer>;
  constant slot gfileiface-eject-mountable :: <C-function-pointer>;
  constant slot gfileiface-eject-mountable-finish :: <C-function-pointer>;
  constant slot gfileiface-mount-enclosing-volume :: <C-function-pointer>;
  constant slot gfileiface-mount-enclosing-volume-finish :: <C-function-pointer>;
  constant slot gfileiface-monitor-dir :: <C-function-pointer>;
  constant slot gfileiface-monitor-file :: <C-function-pointer>;
  constant slot gfileiface-open-readwrite :: <C-function-pointer>;
  constant slot gfileiface-open-readwrite-async :: <C-function-pointer>;
  constant slot gfileiface-open-readwrite-finish :: <C-function-pointer>;
  constant slot gfileiface-create-readwrite :: <C-function-pointer>;
  constant slot gfileiface-create-readwrite-async :: <C-function-pointer>;
  constant slot gfileiface-create-readwrite-finish :: <C-function-pointer>;
  constant slot gfileiface-replace-readwrite :: <C-function-pointer>;
  constant slot gfileiface-replace-readwrite-async :: <C-function-pointer>;
  constant slot gfileiface-replace-readwrite-finish :: <C-function-pointer>;
  constant slot gfileiface-start-mountable :: <C-function-pointer>;
  constant slot gfileiface-start-mountable-finish :: <C-function-pointer>;
  constant slot gfileiface-stop-mountable :: <C-function-pointer>;
  constant slot gfileiface-stop-mountable-finish :: <C-function-pointer>;
  constant slot gfileiface-supports-thread-contexts :: <C-boolean>;
  constant slot gfileiface-unmount-mountable-with-operation :: <C-function-pointer>;
  constant slot gfileiface-unmount-mountable-with-operation-finish :: <C-function-pointer>;
  constant slot gfileiface-eject-mountable-with-operation :: <C-function-pointer>;
  constant slot gfileiface-eject-mountable-with-operation-finish :: <C-function-pointer>;
  constant slot gfileiface-poll-mountable :: <C-function-pointer>;
  constant slot gfileiface-poll-mountable-finish :: <C-function-pointer>;
  pointer-type-name: <GFileIface>;
end C-struct;

define open C-subtype <GFileInfo> (<GObject>)
end C-subtype;

define C-pointer-type <GFileInfo*> => <GFileInfo>;

define C-function g-file-info-new
  result res :: <GFileInfo>;
  c-name: "g_file_info_new";
end;

define C-function g-file-info-clear-status
  input parameter self :: <GFileInfo>;
  c-name: "g_file_info_clear_status";
end;

define C-function g-file-info-copy-into
  input parameter self :: <GFileInfo>;
  input parameter dest_info_ :: <GFileInfo>;
  c-name: "g_file_info_copy_into";
end;

define C-function g-file-info-dup
  input parameter self :: <GFileInfo>;
  result res :: <GFileInfo>;
  c-name: "g_file_info_dup";
end;

define C-function g-file-info-get-attribute-as-string
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_file_info_get_attribute_as_string";
end;

define C-function g-file-info-get-attribute-boolean
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_info_get_attribute_boolean";
end;

define C-function g-file-info-get-attribute-byte-string
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_file_info_get_attribute_byte_string";
end;

define C-function g-file-info-get-attribute-data
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  output parameter type_ :: <GFileAttributeType*>;
  output parameter value_pp_ :: <C-void**>;
  output parameter status_ :: <GFileAttributeStatus*>;
  result res :: <C-boolean>;
  c-name: "g_file_info_get_attribute_data";
end;

define C-function g-file-info-get-attribute-int32
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_file_info_get_attribute_int32";
end;

define C-function g-file-info-get-attribute-int64
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-signed-long>;
  c-name: "g_file_info_get_attribute_int64";
end;

define C-function g-file-info-get-attribute-object
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <GObject>;
  c-name: "g_file_info_get_attribute_object";
end;

define C-function g-file-info-get-attribute-status
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <GFileAttributeStatus>;
  c-name: "g_file_info_get_attribute_status";
end;

define C-function g-file-info-get-attribute-string
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_file_info_get_attribute_string";
end;

define C-function g-file-info-get-attribute-stringv
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_file_info_get_attribute_stringv";
end;

define C-function g-file-info-get-attribute-type
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <GFileAttributeType>;
  c-name: "g_file_info_get_attribute_type";
end;

define C-function g-file-info-get-attribute-uint32
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_file_info_get_attribute_uint32";
end;

define C-function g-file-info-get-attribute-uint64
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-unsigned-long>;
  c-name: "g_file_info_get_attribute_uint64";
end;

define C-function g-file-info-get-content-type
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_content_type";
end;

define C-function g-file-info-get-deletion-date
  input parameter self :: <GFileInfo>;
  result res :: <GDateTime>;
  c-name: "g_file_info_get_deletion_date";
end;

define C-function g-file-info-get-display-name
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_display_name";
end;

define C-function g-file-info-get-edit-name
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_edit_name";
end;

define C-function g-file-info-get-etag
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_etag";
end;

define C-function g-file-info-get-file-type
  input parameter self :: <GFileInfo>;
  result res :: <GFileType>;
  c-name: "g_file_info_get_file_type";
end;

define C-function g-file-info-get-icon
  input parameter self :: <GFileInfo>;
  result res :: <GIcon>;
  c-name: "g_file_info_get_icon";
end;

define C-function g-file-info-get-is-backup
  input parameter self :: <GFileInfo>;
  result res :: <C-boolean>;
  c-name: "g_file_info_get_is_backup";
end;

define C-function g-file-info-get-is-hidden
  input parameter self :: <GFileInfo>;
  result res :: <C-boolean>;
  c-name: "g_file_info_get_is_hidden";
end;

define C-function g-file-info-get-is-symlink
  input parameter self :: <GFileInfo>;
  result res :: <C-boolean>;
  c-name: "g_file_info_get_is_symlink";
end;

define C-function g-file-info-get-modification-time
  input parameter self :: <GFileInfo>;
  output parameter result_ :: <GTimeVal>;
  c-name: "g_file_info_get_modification_time";
end;

define C-function g-file-info-get-name
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_name";
end;

define C-function g-file-info-get-size
  input parameter self :: <GFileInfo>;
  result res :: <C-signed-long>;
  c-name: "g_file_info_get_size";
end;

define C-function g-file-info-get-sort-order
  input parameter self :: <GFileInfo>;
  result res :: <C-signed-int>;
  c-name: "g_file_info_get_sort_order";
end;

define C-function g-file-info-get-symbolic-icon
  input parameter self :: <GFileInfo>;
  result res :: <GIcon>;
  c-name: "g_file_info_get_symbolic_icon";
end;

define C-function g-file-info-get-symlink-target
  input parameter self :: <GFileInfo>;
  result res :: <C-string>;
  c-name: "g_file_info_get_symlink_target";
end;

define C-function g-file-info-has-attribute
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_info_has_attribute";
end;

define C-function g-file-info-has-namespace
  input parameter self :: <GFileInfo>;
  input parameter name_space_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_file_info_has_namespace";
end;

define C-function g-file-info-list-attributes
  input parameter self :: <GFileInfo>;
  input parameter name_space_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_file_info_list_attributes";
end;

define C-function g-file-info-remove-attribute
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  c-name: "g_file_info_remove_attribute";
end;

define C-function g-file-info-set-attribute
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter type_ :: <GFileAttributeType>;
  input parameter value_p_ :: <C-void*>;
  c-name: "g_file_info_set_attribute";
end;

define C-function g-file-info-set-attribute-boolean
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-boolean>;
  c-name: "g_file_info_set_attribute_boolean";
end;

define C-function g-file-info-set-attribute-byte-string
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-string>;
  c-name: "g_file_info_set_attribute_byte_string";
end;

define C-function g-file-info-set-attribute-int32
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-signed-int>;
  c-name: "g_file_info_set_attribute_int32";
end;

define C-function g-file-info-set-attribute-int64
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-signed-long>;
  c-name: "g_file_info_set_attribute_int64";
end;

define C-function g-file-info-set-attribute-mask
  input parameter self :: <GFileInfo>;
  input parameter mask_ :: <GFileAttributeMatcher>;
  c-name: "g_file_info_set_attribute_mask";
end;

define C-function g-file-info-set-attribute-object
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <GObject>;
  c-name: "g_file_info_set_attribute_object";
end;

define C-function g-file-info-set-attribute-status
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter status_ :: <GFileAttributeStatus>;
  result res :: <C-boolean>;
  c-name: "g_file_info_set_attribute_status";
end;

define C-function g-file-info-set-attribute-string
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-string>;
  c-name: "g_file_info_set_attribute_string";
end;

define C-function g-file-info-set-attribute-stringv
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-string*>;
  c-name: "g_file_info_set_attribute_stringv";
end;

define C-function g-file-info-set-attribute-uint32
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-unsigned-int>;
  c-name: "g_file_info_set_attribute_uint32";
end;

define C-function g-file-info-set-attribute-uint64
  input parameter self :: <GFileInfo>;
  input parameter attribute_ :: <C-string>;
  input parameter attr_value_ :: <C-unsigned-long>;
  c-name: "g_file_info_set_attribute_uint64";
end;

define C-function g-file-info-set-content-type
  input parameter self :: <GFileInfo>;
  input parameter content_type_ :: <C-string>;
  c-name: "g_file_info_set_content_type";
end;

define C-function g-file-info-set-display-name
  input parameter self :: <GFileInfo>;
  input parameter display_name_ :: <C-string>;
  c-name: "g_file_info_set_display_name";
end;

define C-function g-file-info-set-edit-name
  input parameter self :: <GFileInfo>;
  input parameter edit_name_ :: <C-string>;
  c-name: "g_file_info_set_edit_name";
end;

define C-function g-file-info-set-file-type
  input parameter self :: <GFileInfo>;
  input parameter type_ :: <GFileType>;
  c-name: "g_file_info_set_file_type";
end;

define C-function g-file-info-set-icon
  input parameter self :: <GFileInfo>;
  input parameter icon_ :: <GIcon>;
  c-name: "g_file_info_set_icon";
end;

define C-function g-file-info-set-is-hidden
  input parameter self :: <GFileInfo>;
  input parameter is_hidden_ :: <C-boolean>;
  c-name: "g_file_info_set_is_hidden";
end;

define C-function g-file-info-set-is-symlink
  input parameter self :: <GFileInfo>;
  input parameter is_symlink_ :: <C-boolean>;
  c-name: "g_file_info_set_is_symlink";
end;

define C-function g-file-info-set-modification-time
  input parameter self :: <GFileInfo>;
  input parameter mtime_ :: <GTimeVal>;
  c-name: "g_file_info_set_modification_time";
end;

define C-function g-file-info-set-name
  input parameter self :: <GFileInfo>;
  input parameter name_ :: <C-string>;
  c-name: "g_file_info_set_name";
end;

define C-function g-file-info-set-size
  input parameter self :: <GFileInfo>;
  input parameter size_ :: <C-signed-long>;
  c-name: "g_file_info_set_size";
end;

define C-function g-file-info-set-sort-order
  input parameter self :: <GFileInfo>;
  input parameter sort_order_ :: <C-signed-int>;
  c-name: "g_file_info_set_sort_order";
end;

define C-function g-file-info-set-symbolic-icon
  input parameter self :: <GFileInfo>;
  input parameter icon_ :: <GIcon>;
  c-name: "g_file_info_set_symbolic_icon";
end;

define C-function g-file-info-set-symlink-target
  input parameter self :: <GFileInfo>;
  input parameter symlink_target_ :: <C-string>;
  c-name: "g_file_info_set_symlink_target";
end;

define C-function g-file-info-unset-attribute-mask
  input parameter self :: <GFileInfo>;
  c-name: "g_file_info_unset_attribute_mask";
end;

define C-struct <_GFileInfoClass>
  pointer-type-name: <GFileInfoClass>;
end C-struct;

define open C-subtype <GFileInputStream> (<GInputStream>)
  constant slot gfileinputstream-parent-instance :: <GInputStream>;
  constant slot gfileinputstream-priv :: <GFileInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GFileInputStream*> => <GFileInputStream>;

define C-function g-file-input-stream-query-info
  input parameter self :: <GFileInputStream>;
  input parameter attributes_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_input_stream_query_info";
end;

define C-function g-file-input-stream-query-info-async
  input parameter self :: <GFileInputStream>;
  input parameter attributes_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_input_stream_query_info_async";
end;

define C-function g-file-input-stream-query-info-finish
  input parameter self :: <GFileInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GFileInfo>;
  c-name: "g_file_input_stream_query_info_finish";
end;

define C-struct <_GFileInputStreamClass>
  constant slot gfileinputstreamclass-parent-class :: <GInputStreamClass>;
  constant slot gfileinputstreamclass-tell :: <C-function-pointer>;
  constant slot gfileinputstreamclass-can-seek :: <C-function-pointer>;
  constant slot gfileinputstreamclass-seek :: <C-function-pointer>;
  constant slot gfileinputstreamclass-query-info :: <C-function-pointer>;
  constant slot gfileinputstreamclass-query-info-async :: <C-function-pointer>;
  constant slot gfileinputstreamclass-query-info-finish :: <C-function-pointer>;
  constant slot gfileinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gfileinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gfileinputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gfileinputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gfileinputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileInputStreamClass>;
end C-struct;

define C-struct <_GFileInputStreamPrivate>
  pointer-type-name: <GFileInputStreamPrivate>;
end C-struct;

define open C-subtype <GFileMonitor> (<GObject>)
  constant slot gfilemonitor-parent-instance :: <GObject>;
  constant slot gfilemonitor-priv :: <GFileMonitorPrivate>;
end C-subtype;

define C-pointer-type <GFileMonitor*> => <GFileMonitor>;

define property-getter filemonitor-cancelled :: <C-boolean> on <GFileMonitor> end;
define property-getter filemonitor-rate-limit :: <C-signed-int> on <GFileMonitor> end;
define property-setter filemonitor-rate-limit :: <C-signed-int> on <GFileMonitor> end;
define C-function g-file-monitor-cancel
  input parameter self :: <GFileMonitor>;
  result res :: <C-boolean>;
  c-name: "g_file_monitor_cancel";
end;

define C-function g-file-monitor-emit-event
  input parameter self :: <GFileMonitor>;
  input parameter child_ :: <GFile>;
  input parameter other_file_ :: <GFile>;
  input parameter event_type_ :: <GFileMonitorEvent>;
  c-name: "g_file_monitor_emit_event";
end;

define C-function g-file-monitor-is-cancelled
  input parameter self :: <GFileMonitor>;
  result res :: <C-boolean>;
  c-name: "g_file_monitor_is_cancelled";
end;

define C-function g-file-monitor-set-rate-limit
  input parameter self :: <GFileMonitor>;
  input parameter limit_msecs_ :: <C-signed-int>;
  c-name: "g_file_monitor_set_rate_limit";
end;

define C-struct <_GFileMonitorClass>
  constant slot gfilemonitorclass-parent-class :: <GObjectClass>;
  constant slot gfilemonitorclass-changed :: <C-function-pointer>;
  constant slot gfilemonitorclass-cancel :: <C-function-pointer>;
  constant slot gfilemonitorclass--g-reserved1 :: <C-void*>;
  constant slot gfilemonitorclass--g-reserved2 :: <C-void*>;
  constant slot gfilemonitorclass--g-reserved3 :: <C-void*>;
  constant slot gfilemonitorclass--g-reserved4 :: <C-void*>;
  constant slot gfilemonitorclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileMonitorClass>;
end C-struct;

define constant $G-FILE-MONITOR-EVENT-CHANGED = 0;
define constant $G-FILE-MONITOR-EVENT-CHANGES-DONE-HINT = 1;
define constant $G-FILE-MONITOR-EVENT-DELETED = 2;
define constant $G-FILE-MONITOR-EVENT-CREATED = 3;
define constant $G-FILE-MONITOR-EVENT-ATTRIBUTE-CHANGED = 4;
define constant $G-FILE-MONITOR-EVENT-PRE-UNMOUNT = 5;
define constant $G-FILE-MONITOR-EVENT-UNMOUNTED = 6;
define constant $G-FILE-MONITOR-EVENT-MOVED = 7;
define constant <GFileMonitorEvent> = <C-int>;
define C-pointer-type <GFileMonitorEvent*> => <GFileMonitorEvent>;

define constant $G-FILE-MONITOR-NONE = 0;
define constant $G-FILE-MONITOR-WATCH-MOUNTS = 1;
define constant $G-FILE-MONITOR-SEND-MOVED = 2;
define constant $G-FILE-MONITOR-WATCH-HARD-LINKS = 4;
define constant <GFileMonitorFlags> = <C-int>;
define C-pointer-type <GFileMonitorFlags*> => <GFileMonitorFlags>;

define C-struct <_GFileMonitorPrivate>
  pointer-type-name: <GFileMonitorPrivate>;
end C-struct;

define open C-subtype <GFileOutputStream> (<GOutputStream>)
  constant slot gfileoutputstream-parent-instance :: <GOutputStream>;
  constant slot gfileoutputstream-priv :: <GFileOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GFileOutputStream*> => <GFileOutputStream>;

define C-function g-file-output-stream-get-etag
  input parameter self :: <GFileOutputStream>;
  result res :: <C-string>;
  c-name: "g_file_output_stream_get_etag";
end;

define C-function g-file-output-stream-query-info
  input parameter self :: <GFileOutputStream>;
  input parameter attributes_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GFileInfo>;
  c-name: "g_file_output_stream_query_info";
end;

define C-function g-file-output-stream-query-info-async
  input parameter self :: <GFileOutputStream>;
  input parameter attributes_ :: <C-string>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_file_output_stream_query_info_async";
end;

define C-function g-file-output-stream-query-info-finish
  input parameter self :: <GFileOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GFileInfo>;
  c-name: "g_file_output_stream_query_info_finish";
end;

define C-struct <_GFileOutputStreamClass>
  constant slot gfileoutputstreamclass-parent-class :: <GOutputStreamClass>;
  constant slot gfileoutputstreamclass-tell :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-can-seek :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-seek :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-can-truncate :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-truncate-fn :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-query-info :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-query-info-async :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-query-info-finish :: <C-function-pointer>;
  constant slot gfileoutputstreamclass-get-etag :: <C-function-pointer>;
  constant slot gfileoutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gfileoutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gfileoutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gfileoutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gfileoutputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileOutputStreamClass>;
end C-struct;

define C-struct <_GFileOutputStreamPrivate>
  pointer-type-name: <GFileOutputStreamPrivate>;
end C-struct;

define constant $G-FILE-QUERY-INFO-NONE = 0;
define constant $G-FILE-QUERY-INFO-NOFOLLOW-SYMLINKS = 1;
define constant <GFileQueryInfoFlags> = <C-int>;
define C-pointer-type <GFileQueryInfoFlags*> => <GFileQueryInfoFlags>;

define constant $G-FILE-TYPE-UNKNOWN = 0;
define constant $G-FILE-TYPE-REGULAR = 1;
define constant $G-FILE-TYPE-DIRECTORY = 2;
define constant $G-FILE-TYPE-SYMBOLIC-LINK = 3;
define constant $G-FILE-TYPE-SPECIAL = 4;
define constant $G-FILE-TYPE-SHORTCUT = 5;
define constant $G-FILE-TYPE-MOUNTABLE = 6;
define constant <GFileType> = <C-int>;
define C-pointer-type <GFileType*> => <GFileType>;

define open C-subtype <GFilenameCompleter> (<GObject>)
end C-subtype;

define C-pointer-type <GFilenameCompleter*> => <GFilenameCompleter>;

define C-function g-filename-completer-new
  result res :: <GFilenameCompleter>;
  c-name: "g_filename_completer_new";
end;

define C-function g-filename-completer-get-completion-suffix
  input parameter self :: <GFilenameCompleter>;
  input parameter initial_text_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_filename_completer_get_completion_suffix";
end;

define C-function g-filename-completer-get-completions
  input parameter self :: <GFilenameCompleter>;
  input parameter initial_text_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_filename_completer_get_completions";
end;

define C-function g-filename-completer-set-dirs-only
  input parameter self :: <GFilenameCompleter>;
  input parameter dirs_only_ :: <C-boolean>;
  c-name: "g_filename_completer_set_dirs_only";
end;

define C-struct <_GFilenameCompleterClass>
  constant slot gfilenamecompleterclass-parent-class :: <GObjectClass>;
  constant slot gfilenamecompleterclass-got-completion-data :: <C-function-pointer>;
  constant slot gfilenamecompleterclass--g-reserved1 :: <C-void*>;
  constant slot gfilenamecompleterclass--g-reserved2 :: <C-void*>;
  constant slot gfilenamecompleterclass--g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilenameCompleterClass>;
end C-struct;

define constant $G-FILESYSTEM-PREVIEW-TYPE-IF-ALWAYS = 0;
define constant $G-FILESYSTEM-PREVIEW-TYPE-IF-LOCAL = 1;
define constant $G-FILESYSTEM-PREVIEW-TYPE-NEVER = 2;
define constant <GFilesystemPreviewType> = <C-int>;
define C-pointer-type <GFilesystemPreviewType*> => <GFilesystemPreviewType>;

define open C-subtype <GFilterInputStream> (<GInputStream>)
  constant slot gfilterinputstream-parent-instance :: <GInputStream>;
  constant slot gfilterinputstream-base-stream :: <GInputStream>;
end C-subtype;

define C-pointer-type <GFilterInputStream*> => <GFilterInputStream>;

define property-getter filterinputstream-base-stream :: <GInputStream> on <GFilterInputStream> end;
define property-setter filterinputstream-base-stream :: <GInputStream> on <GFilterInputStream> end;
define property-getter filterinputstream-close-base-stream :: <C-boolean> on <GFilterInputStream> end;
define property-setter filterinputstream-close-base-stream :: <C-boolean> on <GFilterInputStream> end;
define C-function g-filter-input-stream-get-base-stream
  input parameter self :: <GFilterInputStream>;
  result res :: <GInputStream>;
  c-name: "g_filter_input_stream_get_base_stream";
end;

define C-function g-filter-input-stream-get-close-base-stream
  input parameter self :: <GFilterInputStream>;
  result res :: <C-boolean>;
  c-name: "g_filter_input_stream_get_close_base_stream";
end;

define C-function g-filter-input-stream-set-close-base-stream
  input parameter self :: <GFilterInputStream>;
  input parameter close_base_ :: <C-boolean>;
  c-name: "g_filter_input_stream_set_close_base_stream";
end;

define C-struct <_GFilterInputStreamClass>
  constant slot gfilterinputstreamclass-parent-class :: <GInputStreamClass>;
  constant slot gfilterinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gfilterinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gfilterinputstreamclass--g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilterInputStreamClass>;
end C-struct;

define open C-subtype <GFilterOutputStream> (<GOutputStream>)
  constant slot gfilteroutputstream-parent-instance :: <GOutputStream>;
  constant slot gfilteroutputstream-base-stream :: <GOutputStream>;
end C-subtype;

define C-pointer-type <GFilterOutputStream*> => <GFilterOutputStream>;

define property-getter filteroutputstream-base-stream :: <GOutputStream> on <GFilterOutputStream> end;
define property-setter filteroutputstream-base-stream :: <GOutputStream> on <GFilterOutputStream> end;
define property-getter filteroutputstream-close-base-stream :: <C-boolean> on <GFilterOutputStream> end;
define property-setter filteroutputstream-close-base-stream :: <C-boolean> on <GFilterOutputStream> end;
define C-function g-filter-output-stream-get-base-stream
  input parameter self :: <GFilterOutputStream>;
  result res :: <GOutputStream>;
  c-name: "g_filter_output_stream_get_base_stream";
end;

define C-function g-filter-output-stream-get-close-base-stream
  input parameter self :: <GFilterOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_filter_output_stream_get_close_base_stream";
end;

define C-function g-filter-output-stream-set-close-base-stream
  input parameter self :: <GFilterOutputStream>;
  input parameter close_base_ :: <C-boolean>;
  c-name: "g_filter_output_stream_set_close_base_stream";
end;

define C-struct <_GFilterOutputStreamClass>
  constant slot gfilteroutputstreamclass-parent-class :: <GOutputStreamClass>;
  constant slot gfilteroutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gfilteroutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gfilteroutputstreamclass--g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilterOutputStreamClass>;
end C-struct;

define constant $G-IO-ERROR-FAILED = 0;
define constant $G-IO-ERROR-NOT-FOUND = 1;
define constant $G-IO-ERROR-EXISTS = 2;
define constant $G-IO-ERROR-IS-DIRECTORY = 3;
define constant $G-IO-ERROR-NOT-DIRECTORY = 4;
define constant $G-IO-ERROR-NOT-EMPTY = 5;
define constant $G-IO-ERROR-NOT-REGULAR-FILE = 6;
define constant $G-IO-ERROR-NOT-SYMBOLIC-LINK = 7;
define constant $G-IO-ERROR-NOT-MOUNTABLE-FILE = 8;
define constant $G-IO-ERROR-FILENAME-TOO-LONG = 9;
define constant $G-IO-ERROR-INVALID-FILENAME = 10;
define constant $G-IO-ERROR-TOO-MANY-LINKS = 11;
define constant $G-IO-ERROR-NO-SPACE = 12;
define constant $G-IO-ERROR-INVALID-ARGUMENT = 13;
define constant $G-IO-ERROR-PERMISSION-DENIED = 14;
define constant $G-IO-ERROR-NOT-SUPPORTED = 15;
define constant $G-IO-ERROR-NOT-MOUNTED = 16;
define constant $G-IO-ERROR-ALREADY-MOUNTED = 17;
define constant $G-IO-ERROR-CLOSED = 18;
define constant $G-IO-ERROR-CANCELLED = 19;
define constant $G-IO-ERROR-PENDING = 20;
define constant $G-IO-ERROR-READ-ONLY = 21;
define constant $G-IO-ERROR-CANT-CREATE-BACKUP = 22;
define constant $G-IO-ERROR-WRONG-ETAG = 23;
define constant $G-IO-ERROR-TIMED-OUT = 24;
define constant $G-IO-ERROR-WOULD-RECURSE = 25;
define constant $G-IO-ERROR-BUSY = 26;
define constant $G-IO-ERROR-WOULD-BLOCK = 27;
define constant $G-IO-ERROR-HOST-NOT-FOUND = 28;
define constant $G-IO-ERROR-WOULD-MERGE = 29;
define constant $G-IO-ERROR-FAILED-HANDLED = 30;
define constant $G-IO-ERROR-TOO-MANY-OPEN-FILES = 31;
define constant $G-IO-ERROR-NOT-INITIALIZED = 32;
define constant $G-IO-ERROR-ADDRESS-IN-USE = 33;
define constant $G-IO-ERROR-PARTIAL-INPUT = 34;
define constant $G-IO-ERROR-INVALID-DATA = 35;
define constant $G-IO-ERROR-DBUS-ERROR = 36;
define constant $G-IO-ERROR-HOST-UNREACHABLE = 37;
define constant $G-IO-ERROR-NETWORK-UNREACHABLE = 38;
define constant $G-IO-ERROR-CONNECTION-REFUSED = 39;
define constant $G-IO-ERROR-PROXY-FAILED = 40;
define constant $G-IO-ERROR-PROXY-AUTH-FAILED = 41;
define constant $G-IO-ERROR-PROXY-NEED-AUTH = 42;
define constant $G-IO-ERROR-PROXY-NOT-ALLOWED = 43;
define constant $G-IO-ERROR-BROKEN-PIPE = 44;
define constant <GIOErrorEnum> = <C-int>;
define C-pointer-type <GIOErrorEnum*> => <GIOErrorEnum>;

define C-struct <_GIOExtension>
  pointer-type-name: <GIOExtension>;
end C-struct;

define C-function g-io-extension-get-name
  input parameter self :: <GIOExtension>;
  result res :: <C-string>;
  c-name: "g_io_extension_get_name";
end;

define C-function g-io-extension-get-priority
  input parameter self :: <GIOExtension>;
  result res :: <C-signed-int>;
  c-name: "g_io_extension_get_priority";
end;

define C-function g-io-extension-get-type
  input parameter self :: <GIOExtension>;
  result res :: <C-long>;
  c-name: "g_io_extension_get_type";
end;

define C-struct <_GIOExtensionPoint>
  pointer-type-name: <GIOExtensionPoint>;
end C-struct;

define C-function g-io-extension-point-get-extension-by-name
  input parameter self :: <GIOExtensionPoint>;
  input parameter name_ :: <C-string>;
  result res :: <GIOExtension>;
  c-name: "g_io_extension_point_get_extension_by_name";
end;

define C-function g-io-extension-point-get-extensions
  input parameter self :: <GIOExtensionPoint>;
  result res :: <GList>;
  c-name: "g_io_extension_point_get_extensions";
end;

define C-function g-io-extension-point-get-required-type
  input parameter self :: <GIOExtensionPoint>;
  result res :: <C-long>;
  c-name: "g_io_extension_point_get_required_type";
end;

define C-function g-io-extension-point-set-required-type
  input parameter self :: <GIOExtensionPoint>;
  input parameter type_ :: <C-long>;
  c-name: "g_io_extension_point_set_required_type";
end;

define C-function g-io-extension-point-implement
  input parameter extension_point_name_ :: <C-string>;
  input parameter type_ :: <C-long>;
  input parameter extension_name_ :: <C-string>;
  input parameter priority_ :: <C-signed-int>;
  result res :: <GIOExtension>;
  c-name: "g_io_extension_point_implement";
end;

define C-function g-io-extension-point-lookup
  input parameter name_ :: <C-string>;
  result res :: <GIOExtensionPoint>;
  c-name: "g_io_extension_point_lookup";
end;

define C-function g-io-extension-point-register
  input parameter name_ :: <C-string>;
  result res :: <GIOExtensionPoint>;
  c-name: "g_io_extension_point_register";
end;

define open C-subtype <GIOModule> (<GTypeModule>)
end C-subtype;

define C-pointer-type <GIOModule*> => <GIOModule>;

define C-function g-io-module-new
  input parameter filename_ :: <C-string>;
  result res :: <GIOModule>;
  c-name: "g_io_module_new";
end;

define C-struct <_GIOModuleClass>
  pointer-type-name: <GIOModuleClass>;
end C-struct;

define C-struct <_GIOModuleScope>
  pointer-type-name: <GIOModuleScope>;
end C-struct;

define C-function g-io-module-scope-block
  input parameter self :: <GIOModuleScope>;
  input parameter basename_ :: <C-string>;
  c-name: "g_io_module_scope_block";
end;

define C-function g-io-module-scope-free
  input parameter self :: <GIOModuleScope>;
  c-name: "g_io_module_scope_free";
end;

define constant $G-IO-MODULE-SCOPE-NONE = 0;
define constant $G-IO-MODULE-SCOPE-BLOCK-DUPLICATES = 1;
define constant <GIOModuleScopeFlags> = <C-int>;
define C-pointer-type <GIOModuleScopeFlags*> => <GIOModuleScopeFlags>;

define C-struct <_GIOSchedulerJob>
  pointer-type-name: <GIOSchedulerJob>;
end C-struct;

define C-function g-io-scheduler-job-send-to-mainloop
  input parameter self :: <GIOSchedulerJob>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-boolean>;
  c-name: "g_io_scheduler_job_send_to_mainloop";
end;

define C-function g-io-scheduler-job-send-to-mainloop-async
  input parameter self :: <GIOSchedulerJob>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "g_io_scheduler_job_send_to_mainloop_async";
end;

define open C-subtype <GIOStream> (<GObject>)
  constant slot giostream-parent-instance :: <GObject>;
  constant slot giostream-priv :: <GIOStreamPrivate>;
end C-subtype;

define C-pointer-type <GIOStream*> => <GIOStream>;

define property-getter iostream-closed :: <C-boolean> on <GIOStream> end;
define property-getter iostream-input-stream :: <GInputStream> on <GIOStream> end;
define property-getter iostream-output-stream :: <GOutputStream> on <GIOStream> end;
define C-function g-io-stream-splice-finish
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_splice_finish";
end;

define C-function g-io-stream-clear-pending
  input parameter self :: <GIOStream>;
  c-name: "g_io_stream_clear_pending";
end;

define C-function g-io-stream-close
  input parameter self :: <GIOStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_close";
end;

define C-function g-io-stream-close-async
  input parameter self :: <GIOStream>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_io_stream_close_async";
end;

define C-function g-io-stream-close-finish
  input parameter self :: <GIOStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_close_finish";
end;

define C-function g-io-stream-get-input-stream
  input parameter self :: <GIOStream>;
  result res :: <GInputStream>;
  c-name: "g_io_stream_get_input_stream";
end;

define C-function g-io-stream-get-output-stream
  input parameter self :: <GIOStream>;
  result res :: <GOutputStream>;
  c-name: "g_io_stream_get_output_stream";
end;

define C-function g-io-stream-has-pending
  input parameter self :: <GIOStream>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_has_pending";
end;

define C-function g-io-stream-is-closed
  input parameter self :: <GIOStream>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_is_closed";
end;

define C-function g-io-stream-set-pending
  input parameter self :: <GIOStream>;
  result res :: <C-boolean>;
  c-name: "g_io_stream_set_pending";
end;

define C-function g-io-stream-splice-async
  input parameter self :: <GIOStream>;
  input parameter stream2_ :: <GIOStream>;
  input parameter flags_ :: <GIOStreamSpliceFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_io_stream_splice_async";
end;

define C-struct <_GIOStreamAdapter>
  pointer-type-name: <GIOStreamAdapter>;
end C-struct;

define C-struct <_GIOStreamClass>
  constant slot giostreamclass-parent-class :: <GObjectClass>;
  constant slot giostreamclass-get-input-stream :: <C-function-pointer>;
  constant slot giostreamclass-get-output-stream :: <C-function-pointer>;
  constant slot giostreamclass-close-fn :: <C-function-pointer>;
  constant slot giostreamclass-close-async :: <C-function-pointer>;
  constant slot giostreamclass-close-finish :: <C-function-pointer>;
  constant slot giostreamclass--g-reserved1 :: <C-void*>;
  constant slot giostreamclass--g-reserved2 :: <C-void*>;
  constant slot giostreamclass--g-reserved3 :: <C-void*>;
  constant slot giostreamclass--g-reserved4 :: <C-void*>;
  constant slot giostreamclass--g-reserved5 :: <C-void*>;
  constant slot giostreamclass--g-reserved6 :: <C-void*>;
  constant slot giostreamclass--g-reserved7 :: <C-void*>;
  constant slot giostreamclass--g-reserved8 :: <C-void*>;
  constant slot giostreamclass--g-reserved9 :: <C-void*>;
  constant slot giostreamclass--g-reserved10 :: <C-void*>;
  pointer-type-name: <GIOStreamClass>;
end C-struct;

define C-struct <_GIOStreamPrivate>
  pointer-type-name: <GIOStreamPrivate>;
end C-struct;

define constant $G-IO-STREAM-SPLICE-NONE = 0;
define constant $G-IO-STREAM-SPLICE-CLOSE-STREAM1 = 1;
define constant $G-IO-STREAM-SPLICE-CLOSE-STREAM2 = 2;
define constant $G-IO-STREAM-SPLICE-WAIT-FOR-BOTH = 4;
define constant <GIOStreamSpliceFlags> = <C-int>;
define C-pointer-type <GIOStreamSpliceFlags*> => <GIOStreamSpliceFlags>;

// Interface
define open C-subtype <GIcon> (<C-void*>)
end C-subtype;

define C-pointer-type <GIcon*> => <GIcon>;

define C-function g-icon-hash
  input parameter icon_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_icon_hash";
end;

define C-function g-icon-new-for-string
  input parameter str_ :: <C-string>;
  result res :: <GIcon>;
  c-name: "g_icon_new_for_string";
end;

define C-function g-icon-equal
  input parameter self :: <GIcon>;
  input parameter icon2_ :: <GIcon>;
  result res :: <C-boolean>;
  c-name: "g_icon_equal";
end;

define C-function g-icon-to-string
  input parameter self :: <GIcon>;
  result res :: <C-string>;
  c-name: "g_icon_to_string";
end;

define C-struct <_GIconIface>
  constant slot giconiface-g-iface :: <GTypeInterface>;
  constant slot giconiface-hash :: <C-function-pointer>;
  constant slot giconiface-equal :: <C-function-pointer>;
  constant slot giconiface-to-tokens :: <C-void*>;
  constant slot giconiface-from-tokens :: <C-void*>;
  pointer-type-name: <GIconIface>;
end C-struct;

define open C-subtype <GInetAddress> (<GObject>)
  constant slot ginetaddress-parent-instance :: <GObject>;
  constant slot ginetaddress-priv :: <GInetAddressPrivate>;
end C-subtype;

define C-pointer-type <GInetAddress*> => <GInetAddress>;

define property-getter inetaddress-bytes :: <C-void*> on <GInetAddress> end;
define property-setter inetaddress-bytes :: <C-void*> on <GInetAddress> end;
define property-getter inetaddress-family :: <GSocketFamily> on <GInetAddress> end;
define property-setter inetaddress-family :: <GSocketFamily> on <GInetAddress> end;
define property-getter inetaddress-is-any :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-link-local :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-loopback :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-mc-global :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-mc-link-local :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-mc-node-local :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-mc-org-local :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-mc-site-local :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-multicast :: <C-boolean> on <GInetAddress> end;
define property-getter inetaddress-is-site-local :: <C-boolean> on <GInetAddress> end;
define C-function g-inet-address-new-any
  input parameter family_ :: <GSocketFamily>;
  result res :: <GInetAddress>;
  c-name: "g_inet_address_new_any";
end;

define C-function g-inet-address-new-from-bytes
  input parameter bytes_ :: <C-unsigned-char*>;
  input parameter family_ :: <GSocketFamily>;
  result res :: <GInetAddress>;
  c-name: "g_inet_address_new_from_bytes";
end;

define C-function g-inet-address-new-from-string
  input parameter string_ :: <C-string>;
  result res :: <GInetAddress>;
  c-name: "g_inet_address_new_from_string";
end;

define C-function g-inet-address-new-loopback
  input parameter family_ :: <GSocketFamily>;
  result res :: <GInetAddress>;
  c-name: "g_inet_address_new_loopback";
end;

define C-function g-inet-address-equal
  input parameter self :: <GInetAddress>;
  input parameter other_address_ :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_equal";
end;

define C-function g-inet-address-get-family
  input parameter self :: <GInetAddress>;
  result res :: <GSocketFamily>;
  c-name: "g_inet_address_get_family";
end;

define C-function g-inet-address-get-is-any
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_any";
end;

define C-function g-inet-address-get-is-link-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_link_local";
end;

define C-function g-inet-address-get-is-loopback
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_loopback";
end;

define C-function g-inet-address-get-is-mc-global
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_mc_global";
end;

define C-function g-inet-address-get-is-mc-link-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_mc_link_local";
end;

define C-function g-inet-address-get-is-mc-node-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_mc_node_local";
end;

define C-function g-inet-address-get-is-mc-org-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_mc_org_local";
end;

define C-function g-inet-address-get-is-mc-site-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_mc_site_local";
end;

define C-function g-inet-address-get-is-multicast
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_multicast";
end;

define C-function g-inet-address-get-is-site-local
  input parameter self :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_get_is_site_local";
end;

define C-function g-inet-address-get-native-size
  input parameter self :: <GInetAddress>;
  result res :: <C-unsigned-long>;
  c-name: "g_inet_address_get_native_size";
end;

define C-function g-inet-address-to-string
  input parameter self :: <GInetAddress>;
  result res :: <C-string>;
  c-name: "g_inet_address_to_string";
end;

define C-struct <_GInetAddressClass>
  constant slot ginetaddressclass-parent-class :: <GObjectClass>;
  constant slot ginetaddressclass-to-string :: <C-function-pointer>;
  constant slot ginetaddressclass-to-bytes :: <C-function-pointer>;
  pointer-type-name: <GInetAddressClass>;
end C-struct;

define open C-subtype <GInetAddressMask> (<GObject>)
  constant slot ginetaddressmask-parent-instance :: <GObject>;
  constant slot ginetaddressmask-priv :: <GInetAddressMaskPrivate>;
end C-subtype;

define C-pointer-type <GInetAddressMask*> => <GInetAddressMask>;

define property-getter inetaddressmask-address :: <GInetAddress> on <GInetAddressMask> end;
define property-setter inetaddressmask-address :: <GInetAddress> on <GInetAddressMask> end;
define property-getter inetaddressmask-family :: <GSocketFamily> on <GInetAddressMask> end;
define property-getter inetaddressmask-length :: <C-unsigned-int> on <GInetAddressMask> end;
define property-setter inetaddressmask-length :: <C-unsigned-int> on <GInetAddressMask> end;
define C-function g-inet-address-mask-new
  input parameter addr_ :: <GInetAddress>;
  input parameter length_ :: <C-unsigned-int>;
  result res :: <GInetAddressMask>;
  c-name: "g_inet_address_mask_new";
end;

define C-function g-inet-address-mask-new-from-string
  input parameter mask_string_ :: <C-string>;
  result res :: <GInetAddressMask>;
  c-name: "g_inet_address_mask_new_from_string";
end;

define C-function g-inet-address-mask-equal
  input parameter self :: <GInetAddressMask>;
  input parameter mask2_ :: <GInetAddressMask>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_mask_equal";
end;

define C-function g-inet-address-mask-get-address
  input parameter self :: <GInetAddressMask>;
  result res :: <GInetAddress>;
  c-name: "g_inet_address_mask_get_address";
end;

define C-function g-inet-address-mask-get-family
  input parameter self :: <GInetAddressMask>;
  result res :: <GSocketFamily>;
  c-name: "g_inet_address_mask_get_family";
end;

define C-function g-inet-address-mask-get-length
  input parameter self :: <GInetAddressMask>;
  result res :: <C-unsigned-int>;
  c-name: "g_inet_address_mask_get_length";
end;

define C-function g-inet-address-mask-matches
  input parameter self :: <GInetAddressMask>;
  input parameter address_ :: <GInetAddress>;
  result res :: <C-boolean>;
  c-name: "g_inet_address_mask_matches";
end;

define C-function g-inet-address-mask-to-string
  input parameter self :: <GInetAddressMask>;
  result res :: <C-string>;
  c-name: "g_inet_address_mask_to_string";
end;

define C-struct <_GInetAddressMaskClass>
  constant slot ginetaddressmaskclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GInetAddressMaskClass>;
end C-struct;

define C-struct <_GInetAddressMaskPrivate>
  pointer-type-name: <GInetAddressMaskPrivate>;
end C-struct;

define C-struct <_GInetAddressPrivate>
  pointer-type-name: <GInetAddressPrivate>;
end C-struct;

define open C-subtype <GInetSocketAddress> (<GSocketAddress>)
  constant slot ginetsocketaddress-parent-instance :: <GSocketAddress>;
  constant slot ginetsocketaddress-priv :: <GInetSocketAddressPrivate>;
end C-subtype;

define C-pointer-type <GInetSocketAddress*> => <GInetSocketAddress>;

define property-getter inetsocketaddress-address :: <GInetAddress> on <GInetSocketAddress> end;
define property-setter inetsocketaddress-address :: <GInetAddress> on <GInetSocketAddress> end;
define property-getter inetsocketaddress-flowinfo :: <C-unsigned-int> on <GInetSocketAddress> end;
define property-setter inetsocketaddress-flowinfo :: <C-unsigned-int> on <GInetSocketAddress> end;
define property-getter inetsocketaddress-port :: <C-unsigned-int> on <GInetSocketAddress> end;
define property-setter inetsocketaddress-port :: <C-unsigned-int> on <GInetSocketAddress> end;
define property-getter inetsocketaddress-scope-id :: <C-unsigned-int> on <GInetSocketAddress> end;
define property-setter inetsocketaddress-scope-id :: <C-unsigned-int> on <GInetSocketAddress> end;
define C-function g-inet-socket-address-new
  input parameter address_ :: <GInetAddress>;
  input parameter port_ :: <C-unsigned-short>;
  result res :: <GSocketAddress>;
  c-name: "g_inet_socket_address_new";
end;

define C-function g-inet-socket-address-get-address
  input parameter self :: <GInetSocketAddress>;
  result res :: <GInetAddress>;
  c-name: "g_inet_socket_address_get_address";
end;

define C-function g-inet-socket-address-get-flowinfo
  input parameter self :: <GInetSocketAddress>;
  result res :: <C-unsigned-int>;
  c-name: "g_inet_socket_address_get_flowinfo";
end;

define C-function g-inet-socket-address-get-port
  input parameter self :: <GInetSocketAddress>;
  result res :: <C-unsigned-short>;
  c-name: "g_inet_socket_address_get_port";
end;

define C-function g-inet-socket-address-get-scope-id
  input parameter self :: <GInetSocketAddress>;
  result res :: <C-unsigned-int>;
  c-name: "g_inet_socket_address_get_scope_id";
end;

define C-struct <_GInetSocketAddressClass>
  constant slot ginetsocketaddressclass-parent-class :: <GSocketAddressClass>;
  pointer-type-name: <GInetSocketAddressClass>;
end C-struct;

define C-struct <_GInetSocketAddressPrivate>
  pointer-type-name: <GInetSocketAddressPrivate>;
end C-struct;

// Interface
define open C-subtype <GInitable> (<C-void*>)
end C-subtype;

define C-pointer-type <GInitable*> => <GInitable>;

define C-function g-initable-newv
  input parameter object_type_ :: <C-long>;
  input parameter n_parameters_ :: <C-unsigned-int>;
  input parameter parameters_ :: <C-unsigned-char*> /* Not supported */;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GObject>;
  c-name: "g_initable_newv";
end;

define C-function g-initable-init
  input parameter self :: <GInitable>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_initable_init";
end;

define C-struct <_GInitableIface>
  constant slot ginitableiface-g-iface :: <GTypeInterface>;
  constant slot ginitableiface-init :: <C-function-pointer>;
  pointer-type-name: <GInitableIface>;
end C-struct;

define open C-subtype <GInputStream> (<GObject>)
  constant slot ginputstream-parent-instance :: <GObject>;
  constant slot ginputstream-priv :: <GInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GInputStream*> => <GInputStream>;

define C-function g-input-stream-clear-pending
  input parameter self :: <GInputStream>;
  c-name: "g_input_stream_clear_pending";
end;

define C-function g-input-stream-close
  input parameter self :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_close";
end;

define C-function g-input-stream-close-async
  input parameter self :: <GInputStream>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_input_stream_close_async";
end;

define C-function g-input-stream-close-finish
  input parameter self :: <GInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_close_finish";
end;

define C-function g-input-stream-has-pending
  input parameter self :: <GInputStream>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_has_pending";
end;

define C-function g-input-stream-is-closed
  input parameter self :: <GInputStream>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_is_closed";
end;

define C-function g-input-stream-read
  input parameter self :: <GInputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_read";
end;

define C-function g-input-stream-read-all
  input parameter self :: <GInputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  output parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_read_all";
end;

define C-function g-input-stream-read-async
  input parameter self :: <GInputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_input_stream_read_async";
end;

define C-function g-input-stream-read-bytes
  input parameter self :: <GInputStream>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GBytes>;
  c-name: "g_input_stream_read_bytes";
end;

define C-function g-input-stream-read-bytes-async
  input parameter self :: <GInputStream>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_input_stream_read_bytes_async";
end;

define C-function g-input-stream-read-bytes-finish
  input parameter self :: <GInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GBytes>;
  c-name: "g_input_stream_read_bytes_finish";
end;

define C-function g-input-stream-read-finish
  input parameter self :: <GInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_read_finish";
end;

define C-function g-input-stream-set-pending
  input parameter self :: <GInputStream>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_set_pending";
end;

define C-function g-input-stream-skip
  input parameter self :: <GInputStream>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_skip";
end;

define C-function g-input-stream-skip-async
  input parameter self :: <GInputStream>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_input_stream_skip_async";
end;

define C-function g-input-stream-skip-finish
  input parameter self :: <GInputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_skip_finish";
end;

define C-struct <_GInputStreamClass>
  constant slot ginputstreamclass-parent-class :: <GObjectClass>;
  constant slot ginputstreamclass-read-fn :: <C-function-pointer>;
  constant slot ginputstreamclass-skip :: <C-function-pointer>;
  constant slot ginputstreamclass-close-fn :: <C-function-pointer>;
  constant slot ginputstreamclass-read-async :: <C-function-pointer>;
  constant slot ginputstreamclass-read-finish :: <C-function-pointer>;
  constant slot ginputstreamclass-skip-async :: <C-function-pointer>;
  constant slot ginputstreamclass-skip-finish :: <C-function-pointer>;
  constant slot ginputstreamclass-close-async :: <C-function-pointer>;
  constant slot ginputstreamclass-close-finish :: <C-function-pointer>;
  constant slot ginputstreamclass--g-reserved1 :: <C-void*>;
  constant slot ginputstreamclass--g-reserved2 :: <C-void*>;
  constant slot ginputstreamclass--g-reserved3 :: <C-void*>;
  constant slot ginputstreamclass--g-reserved4 :: <C-void*>;
  constant slot ginputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GInputStreamClass>;
end C-struct;

define C-struct <_GInputStreamPrivate>
  pointer-type-name: <GInputStreamPrivate>;
end C-struct;

define C-struct <_GInputVector>
  slot ginputvector-buffer :: <C-void*>;
  slot ginputvector-size :: <C-unsigned-long>;
  pointer-type-name: <GInputVector>;
end C-struct;

// Interface
define open C-subtype <GLoadableIcon> (<GIcon>)
end C-subtype;

define C-pointer-type <GLoadableIcon*> => <GLoadableIcon>;

define C-function g-loadable-icon-load
  input parameter self :: <GLoadableIcon>;
  input parameter size_ :: <C-signed-int>;
  output parameter type_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GInputStream>;
  c-name: "g_loadable_icon_load";
end;

define C-function g-loadable-icon-load-async
  input parameter self :: <GLoadableIcon>;
  input parameter size_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_loadable_icon_load_async";
end;

define C-function g-loadable-icon-load-finish
  input parameter self :: <GLoadableIcon>;
  input parameter res_ :: <GAsyncResult>;
  input parameter type_ :: <C-string>;
  result res :: <GInputStream>;
  c-name: "g_loadable_icon_load_finish";
end;

define C-struct <_GLoadableIconIface>
  constant slot gloadableiconiface-g-iface :: <GTypeInterface>;
  constant slot gloadableiconiface-load :: <C-function-pointer>;
  constant slot gloadableiconiface-load-async :: <C-function-pointer>;
  constant slot gloadableiconiface-load-finish :: <C-function-pointer>;
  pointer-type-name: <GLoadableIconIface>;
end C-struct;

define constant $MENU-ATTRIBUTE-ACTION = "action";

define constant $MENU-ATTRIBUTE-ACTION-NAMESPACE = "action-namespace";

define constant $MENU-ATTRIBUTE-LABEL = "label";

define constant $MENU-ATTRIBUTE-TARGET = "target";

define constant $MENU-LINK-SECTION = "section";

define constant $MENU-LINK-SUBMENU = "submenu";

define open C-subtype <GMemoryInputStream> (<GInputStream>)
  constant slot gmemoryinputstream-parent-instance :: <GInputStream>;
  constant slot gmemoryinputstream-priv :: <GMemoryInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GMemoryInputStream*> => <GMemoryInputStream>;

define C-function g-memory-input-stream-new
  result res :: <GInputStream>;
  c-name: "g_memory_input_stream_new";
end;

define C-function g-memory-input-stream-new-from-bytes
  input parameter bytes_ :: <GBytes>;
  result res :: <GInputStream>;
  c-name: "g_memory_input_stream_new_from_bytes";
end;

define C-function g-memory-input-stream-new-from-data
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-signed-long>;
  input parameter destroy_ :: <C-function-pointer>;
  result res :: <GInputStream>;
  c-name: "g_memory_input_stream_new_from_data";
end;

define C-function g-memory-input-stream-add-bytes
  input parameter self :: <GMemoryInputStream>;
  input parameter bytes_ :: <GBytes>;
  c-name: "g_memory_input_stream_add_bytes";
end;

define C-function g-memory-input-stream-add-data
  input parameter self :: <GMemoryInputStream>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-signed-long>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "g_memory_input_stream_add_data";
end;

define C-struct <_GMemoryInputStreamClass>
  constant slot gmemoryinputstreamclass-parent-class :: <GInputStreamClass>;
  constant slot gmemoryinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gmemoryinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gmemoryinputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gmemoryinputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gmemoryinputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GMemoryInputStreamClass>;
end C-struct;

define C-struct <_GMemoryInputStreamPrivate>
  pointer-type-name: <GMemoryInputStreamPrivate>;
end C-struct;

define open C-subtype <GMemoryOutputStream> (<GOutputStream>)
  constant slot gmemoryoutputstream-parent-instance :: <GOutputStream>;
  constant slot gmemoryoutputstream-priv :: <GMemoryOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GMemoryOutputStream*> => <GMemoryOutputStream>;

define property-getter memoryoutputstream-data :: <C-void*> on <GMemoryOutputStream> end;
define property-setter memoryoutputstream-data :: <C-void*> on <GMemoryOutputStream> end;
define property-getter memoryoutputstream-data-size :: <C-unsigned-long> on <GMemoryOutputStream> end;
define property-getter memoryoutputstream-size :: <C-unsigned-long> on <GMemoryOutputStream> end;
define property-setter memoryoutputstream-size :: <C-unsigned-long> on <GMemoryOutputStream> end;
define C-function g-memory-output-stream-new-resizable
  result res :: <GOutputStream>;
  c-name: "g_memory_output_stream_new_resizable";
end;

define C-function g-memory-output-stream-get-data
  input parameter self :: <GMemoryOutputStream>;
  result res :: <C-void*>;
  c-name: "g_memory_output_stream_get_data";
end;

define C-function g-memory-output-stream-get-data-size
  input parameter self :: <GMemoryOutputStream>;
  result res :: <C-unsigned-long>;
  c-name: "g_memory_output_stream_get_data_size";
end;

define C-function g-memory-output-stream-get-size
  input parameter self :: <GMemoryOutputStream>;
  result res :: <C-unsigned-long>;
  c-name: "g_memory_output_stream_get_size";
end;

define C-function g-memory-output-stream-steal-as-bytes
  input parameter self :: <GMemoryOutputStream>;
  result res :: <GBytes>;
  c-name: "g_memory_output_stream_steal_as_bytes";
end;

define C-function g-memory-output-stream-steal-data
  input parameter self :: <GMemoryOutputStream>;
  result res :: <C-void*>;
  c-name: "g_memory_output_stream_steal_data";
end;

define C-struct <_GMemoryOutputStreamClass>
  constant slot gmemoryoutputstreamclass-parent-class :: <GOutputStreamClass>;
  constant slot gmemoryoutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gmemoryoutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gmemoryoutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gmemoryoutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gmemoryoutputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GMemoryOutputStreamClass>;
end C-struct;

define C-struct <_GMemoryOutputStreamPrivate>
  pointer-type-name: <GMemoryOutputStreamPrivate>;
end C-struct;

define open C-subtype <GMenu> (<GMenuModel>)
end C-subtype;

define C-pointer-type <GMenu*> => <GMenu>;

define C-function g-menu-new
  result res :: <GMenu>;
  c-name: "g_menu_new";
end;

define C-function g-menu-append
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter detailed_action_ :: <C-string>;
  c-name: "g_menu_append";
end;

define C-function g-menu-append-item
  input parameter self :: <GMenu>;
  input parameter item_ :: <GMenuItem>;
  c-name: "g_menu_append_item";
end;

define C-function g-menu-append-section
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter section_ :: <GMenuModel>;
  c-name: "g_menu_append_section";
end;

define C-function g-menu-append-submenu
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter submenu_ :: <GMenuModel>;
  c-name: "g_menu_append_submenu";
end;

define C-function g-menu-freeze
  input parameter self :: <GMenu>;
  c-name: "g_menu_freeze";
end;

define C-function g-menu-insert
  input parameter self :: <GMenu>;
  input parameter position_ :: <C-signed-int>;
  input parameter label_ :: <C-string>;
  input parameter detailed_action_ :: <C-string>;
  c-name: "g_menu_insert";
end;

define C-function g-menu-insert-item
  input parameter self :: <GMenu>;
  input parameter position_ :: <C-signed-int>;
  input parameter item_ :: <GMenuItem>;
  c-name: "g_menu_insert_item";
end;

define C-function g-menu-insert-section
  input parameter self :: <GMenu>;
  input parameter position_ :: <C-signed-int>;
  input parameter label_ :: <C-string>;
  input parameter section_ :: <GMenuModel>;
  c-name: "g_menu_insert_section";
end;

define C-function g-menu-insert-submenu
  input parameter self :: <GMenu>;
  input parameter position_ :: <C-signed-int>;
  input parameter label_ :: <C-string>;
  input parameter submenu_ :: <GMenuModel>;
  c-name: "g_menu_insert_submenu";
end;

define C-function g-menu-prepend
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter detailed_action_ :: <C-string>;
  c-name: "g_menu_prepend";
end;

define C-function g-menu-prepend-item
  input parameter self :: <GMenu>;
  input parameter item_ :: <GMenuItem>;
  c-name: "g_menu_prepend_item";
end;

define C-function g-menu-prepend-section
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter section_ :: <GMenuModel>;
  c-name: "g_menu_prepend_section";
end;

define C-function g-menu-prepend-submenu
  input parameter self :: <GMenu>;
  input parameter label_ :: <C-string>;
  input parameter submenu_ :: <GMenuModel>;
  c-name: "g_menu_prepend_submenu";
end;

define C-function g-menu-remove
  input parameter self :: <GMenu>;
  input parameter position_ :: <C-signed-int>;
  c-name: "g_menu_remove";
end;

define open C-subtype <GMenuAttributeIter> (<GObject>)
  constant slot gmenuattributeiter-parent-instance :: <GObject>;
  constant slot gmenuattributeiter-priv :: <GMenuAttributeIterPrivate>;
end C-subtype;

define C-pointer-type <GMenuAttributeIter*> => <GMenuAttributeIter>;

define C-function g-menu-attribute-iter-get-name
  input parameter self :: <GMenuAttributeIter>;
  result res :: <C-string>;
  c-name: "g_menu_attribute_iter_get_name";
end;

define C-function g-menu-attribute-iter-get-next
  input parameter self :: <GMenuAttributeIter>;
  output parameter out_name_ :: <C-string>;
  output parameter value_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_menu_attribute_iter_get_next";
end;

define C-function g-menu-attribute-iter-get-value
  input parameter self :: <GMenuAttributeIter>;
  result res :: <GVariant>;
  c-name: "g_menu_attribute_iter_get_value";
end;

define C-function g-menu-attribute-iter-next
  input parameter self :: <GMenuAttributeIter>;
  result res :: <C-boolean>;
  c-name: "g_menu_attribute_iter_next";
end;

define C-struct <_GMenuAttributeIterClass>
  constant slot gmenuattributeiterclass-parent-class :: <GObjectClass>;
  constant slot gmenuattributeiterclass-get-next :: <C-function-pointer>;
  pointer-type-name: <GMenuAttributeIterClass>;
end C-struct;

define C-struct <_GMenuAttributeIterPrivate>
  pointer-type-name: <GMenuAttributeIterPrivate>;
end C-struct;

define open C-subtype <GMenuItem> (<GObject>)
end C-subtype;

define C-pointer-type <GMenuItem*> => <GMenuItem>;

define C-function g-menu-item-new
  input parameter label_ :: <C-string>;
  input parameter detailed_action_ :: <C-string>;
  result res :: <GMenuItem>;
  c-name: "g_menu_item_new";
end;

define C-function g-menu-item-new-from-model
  input parameter model_ :: <GMenuModel>;
  input parameter item_index_ :: <C-signed-int>;
  result res :: <GMenuItem>;
  c-name: "g_menu_item_new_from_model";
end;

define C-function g-menu-item-new-section
  input parameter label_ :: <C-string>;
  input parameter section_ :: <GMenuModel>;
  result res :: <GMenuItem>;
  c-name: "g_menu_item_new_section";
end;

define C-function g-menu-item-new-submenu
  input parameter label_ :: <C-string>;
  input parameter submenu_ :: <GMenuModel>;
  result res :: <GMenuItem>;
  c-name: "g_menu_item_new_submenu";
end;

define C-function g-menu-item-get-attribute-value
  input parameter self :: <GMenuItem>;
  input parameter attribute_ :: <C-string>;
  input parameter expected_type_ :: <GVariantType>;
  result res :: <GVariant>;
  c-name: "g_menu_item_get_attribute_value";
end;

define C-function g-menu-item-get-link
  input parameter self :: <GMenuItem>;
  input parameter link_ :: <C-string>;
  result res :: <GMenuModel>;
  c-name: "g_menu_item_get_link";
end;

define C-function g-menu-item-set-action-and-target-value
  input parameter self :: <GMenuItem>;
  input parameter action_ :: <C-string>;
  input parameter target_value_ :: <GVariant>;
  c-name: "g_menu_item_set_action_and_target_value";
end;

define C-function g-menu-item-set-attribute-value
  input parameter self :: <GMenuItem>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  c-name: "g_menu_item_set_attribute_value";
end;

define C-function g-menu-item-set-detailed-action
  input parameter self :: <GMenuItem>;
  input parameter detailed_action_ :: <C-string>;
  c-name: "g_menu_item_set_detailed_action";
end;

define C-function g-menu-item-set-label
  input parameter self :: <GMenuItem>;
  input parameter label_ :: <C-string>;
  c-name: "g_menu_item_set_label";
end;

define C-function g-menu-item-set-link
  input parameter self :: <GMenuItem>;
  input parameter link_ :: <C-string>;
  input parameter model_ :: <GMenuModel>;
  c-name: "g_menu_item_set_link";
end;

define C-function g-menu-item-set-section
  input parameter self :: <GMenuItem>;
  input parameter section_ :: <GMenuModel>;
  c-name: "g_menu_item_set_section";
end;

define C-function g-menu-item-set-submenu
  input parameter self :: <GMenuItem>;
  input parameter submenu_ :: <GMenuModel>;
  c-name: "g_menu_item_set_submenu";
end;

define open C-subtype <GMenuLinkIter> (<GObject>)
  constant slot gmenulinkiter-parent-instance :: <GObject>;
  constant slot gmenulinkiter-priv :: <GMenuLinkIterPrivate>;
end C-subtype;

define C-pointer-type <GMenuLinkIter*> => <GMenuLinkIter>;

define C-function g-menu-link-iter-get-name
  input parameter self :: <GMenuLinkIter>;
  result res :: <C-string>;
  c-name: "g_menu_link_iter_get_name";
end;

define C-function g-menu-link-iter-get-next
  input parameter self :: <GMenuLinkIter>;
  output parameter out_link_ :: <C-string>;
  output parameter value_ :: <GMenuModel*>;
  result res :: <C-boolean>;
  c-name: "g_menu_link_iter_get_next";
end;

define C-function g-menu-link-iter-get-value
  input parameter self :: <GMenuLinkIter>;
  result res :: <GMenuModel>;
  c-name: "g_menu_link_iter_get_value";
end;

define C-function g-menu-link-iter-next
  input parameter self :: <GMenuLinkIter>;
  result res :: <C-boolean>;
  c-name: "g_menu_link_iter_next";
end;

define C-struct <_GMenuLinkIterClass>
  constant slot gmenulinkiterclass-parent-class :: <GObjectClass>;
  constant slot gmenulinkiterclass-get-next :: <C-function-pointer>;
  pointer-type-name: <GMenuLinkIterClass>;
end C-struct;

define C-struct <_GMenuLinkIterPrivate>
  pointer-type-name: <GMenuLinkIterPrivate>;
end C-struct;

define open C-subtype <GMenuModel> (<GObject>)
  constant slot gmenumodel-parent-instance :: <GObject>;
  constant slot gmenumodel-priv :: <GMenuModelPrivate>;
end C-subtype;

define C-pointer-type <GMenuModel*> => <GMenuModel>;

define C-function g-menu-model-get-item-attribute-value
  input parameter self :: <GMenuModel>;
  input parameter item_index_ :: <C-signed-int>;
  input parameter attribute_ :: <C-string>;
  input parameter expected_type_ :: <GVariantType>;
  result res :: <GVariant>;
  c-name: "g_menu_model_get_item_attribute_value";
end;

define C-function g-menu-model-get-item-link
  input parameter self :: <GMenuModel>;
  input parameter item_index_ :: <C-signed-int>;
  input parameter link_ :: <C-string>;
  result res :: <GMenuModel>;
  c-name: "g_menu_model_get_item_link";
end;

define C-function g-menu-model-get-n-items
  input parameter self :: <GMenuModel>;
  result res :: <C-signed-int>;
  c-name: "g_menu_model_get_n_items";
end;

define C-function g-menu-model-is-mutable
  input parameter self :: <GMenuModel>;
  result res :: <C-boolean>;
  c-name: "g_menu_model_is_mutable";
end;

define C-function g-menu-model-items-changed
  input parameter self :: <GMenuModel>;
  input parameter position_ :: <C-signed-int>;
  input parameter removed_ :: <C-signed-int>;
  input parameter added_ :: <C-signed-int>;
  c-name: "g_menu_model_items_changed";
end;

define C-function g-menu-model-iterate-item-attributes
  input parameter self :: <GMenuModel>;
  input parameter item_index_ :: <C-signed-int>;
  result res :: <GMenuAttributeIter>;
  c-name: "g_menu_model_iterate_item_attributes";
end;

define C-function g-menu-model-iterate-item-links
  input parameter self :: <GMenuModel>;
  input parameter item_index_ :: <C-signed-int>;
  result res :: <GMenuLinkIter>;
  c-name: "g_menu_model_iterate_item_links";
end;

define C-struct <_GMenuModelClass>
  constant slot gmenumodelclass-parent-class :: <GObjectClass>;
  constant slot gmenumodelclass-is-mutable :: <C-function-pointer>;
  constant slot gmenumodelclass-get-n-items :: <C-function-pointer>;
  constant slot gmenumodelclass-get-item-attributes :: <C-function-pointer>;
  constant slot gmenumodelclass-iterate-item-attributes :: <C-function-pointer>;
  constant slot gmenumodelclass-get-item-attribute-value :: <C-function-pointer>;
  constant slot gmenumodelclass-get-item-links :: <C-function-pointer>;
  constant slot gmenumodelclass-iterate-item-links :: <C-function-pointer>;
  constant slot gmenumodelclass-get-item-link :: <C-function-pointer>;
  pointer-type-name: <GMenuModelClass>;
end C-struct;

define C-struct <_GMenuModelPrivate>
  pointer-type-name: <GMenuModelPrivate>;
end C-struct;

// Interface
define open C-subtype <GMount> (<C-void*>)
end C-subtype;

define C-pointer-type <GMount*> => <GMount>;

define C-function g-mount-can-eject
  input parameter self :: <GMount>;
  result res :: <C-boolean>;
  c-name: "g_mount_can_eject";
end;

define C-function g-mount-can-unmount
  input parameter self :: <GMount>;
  result res :: <C-boolean>;
  c-name: "g_mount_can_unmount";
end;

define C-function g-mount-eject
  input parameter self :: <GMount>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_eject";
end;

define C-function g-mount-eject-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_mount_eject_finish";
end;

define C-function g-mount-eject-with-operation
  input parameter self :: <GMount>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_eject_with_operation";
end;

define C-function g-mount-eject-with-operation-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_mount_eject_with_operation_finish";
end;

define C-function g-mount-get-default-location
  input parameter self :: <GMount>;
  result res :: <GFile>;
  c-name: "g_mount_get_default_location";
end;

define C-function g-mount-get-drive
  input parameter self :: <GMount>;
  result res :: <GDrive>;
  c-name: "g_mount_get_drive";
end;

define C-function g-mount-get-icon
  input parameter self :: <GMount>;
  result res :: <GIcon>;
  c-name: "g_mount_get_icon";
end;

define C-function g-mount-get-name
  input parameter self :: <GMount>;
  result res :: <C-string>;
  c-name: "g_mount_get_name";
end;

define C-function g-mount-get-root
  input parameter self :: <GMount>;
  result res :: <GFile>;
  c-name: "g_mount_get_root";
end;

define C-function g-mount-get-sort-key
  input parameter self :: <GMount>;
  result res :: <C-string>;
  c-name: "g_mount_get_sort_key";
end;

define C-function g-mount-get-symbolic-icon
  input parameter self :: <GMount>;
  result res :: <GIcon>;
  c-name: "g_mount_get_symbolic_icon";
end;

define C-function g-mount-get-uuid
  input parameter self :: <GMount>;
  result res :: <C-string>;
  c-name: "g_mount_get_uuid";
end;

define C-function g-mount-get-volume
  input parameter self :: <GMount>;
  result res :: <GVolume>;
  c-name: "g_mount_get_volume";
end;

define C-function g-mount-guess-content-type
  input parameter self :: <GMount>;
  input parameter force_rescan_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_guess_content_type";
end;

define C-function g-mount-guess-content-type-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-string*>;
  c-name: "g_mount_guess_content_type_finish";
end;

define C-function g-mount-guess-content-type-sync
  input parameter self :: <GMount>;
  input parameter force_rescan_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string*>;
  c-name: "g_mount_guess_content_type_sync";
end;

define C-function g-mount-is-shadowed
  input parameter self :: <GMount>;
  result res :: <C-boolean>;
  c-name: "g_mount_is_shadowed";
end;

define C-function g-mount-remount
  input parameter self :: <GMount>;
  input parameter flags_ :: <GMountMountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_remount";
end;

define C-function g-mount-remount-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_mount_remount_finish";
end;

define C-function g-mount-shadow
  input parameter self :: <GMount>;
  c-name: "g_mount_shadow";
end;

define C-function g-mount-unmount
  input parameter self :: <GMount>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_unmount";
end;

define C-function g-mount-unmount-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_mount_unmount_finish";
end;

define C-function g-mount-unmount-with-operation
  input parameter self :: <GMount>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_mount_unmount_with_operation";
end;

define C-function g-mount-unmount-with-operation-finish
  input parameter self :: <GMount>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_mount_unmount_with_operation_finish";
end;

define C-function g-mount-unshadow
  input parameter self :: <GMount>;
  c-name: "g_mount_unshadow";
end;

define C-struct <_GMountIface>
  constant slot gmountiface-g-iface :: <GTypeInterface>;
  constant slot gmountiface-changed :: <C-function-pointer>;
  constant slot gmountiface-unmounted :: <C-function-pointer>;
  constant slot gmountiface-get-root :: <C-function-pointer>;
  constant slot gmountiface-get-name :: <C-function-pointer>;
  constant slot gmountiface-get-icon :: <C-function-pointer>;
  constant slot gmountiface-get-uuid :: <C-function-pointer>;
  constant slot gmountiface-get-volume :: <C-function-pointer>;
  constant slot gmountiface-get-drive :: <C-function-pointer>;
  constant slot gmountiface-can-unmount :: <C-function-pointer>;
  constant slot gmountiface-can-eject :: <C-function-pointer>;
  constant slot gmountiface-unmount :: <C-function-pointer>;
  constant slot gmountiface-unmount-finish :: <C-function-pointer>;
  constant slot gmountiface-eject :: <C-function-pointer>;
  constant slot gmountiface-eject-finish :: <C-function-pointer>;
  constant slot gmountiface-remount :: <C-function-pointer>;
  constant slot gmountiface-remount-finish :: <C-function-pointer>;
  constant slot gmountiface-guess-content-type :: <C-function-pointer>;
  constant slot gmountiface-guess-content-type-finish :: <C-function-pointer>;
  constant slot gmountiface-guess-content-type-sync :: <C-function-pointer>;
  constant slot gmountiface-pre-unmount :: <C-function-pointer>;
  constant slot gmountiface-unmount-with-operation :: <C-function-pointer>;
  constant slot gmountiface-unmount-with-operation-finish :: <C-function-pointer>;
  constant slot gmountiface-eject-with-operation :: <C-function-pointer>;
  constant slot gmountiface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot gmountiface-get-default-location :: <C-function-pointer>;
  constant slot gmountiface-get-sort-key :: <C-function-pointer>;
  constant slot gmountiface-get-symbolic-icon :: <C-function-pointer>;
  pointer-type-name: <GMountIface>;
end C-struct;

define constant $G-MOUNT-MOUNT-NONE = 0;
define constant <GMountMountFlags> = <C-int>;
define C-pointer-type <GMountMountFlags*> => <GMountMountFlags>;

define open C-subtype <GMountOperation> (<GObject>)
  constant slot gmountoperation-parent-instance :: <GObject>;
  constant slot gmountoperation-priv :: <GMountOperationPrivate>;
end C-subtype;

define C-pointer-type <GMountOperation*> => <GMountOperation>;

define property-getter mountoperation-anonymous :: <C-boolean> on <GMountOperation> end;
define property-setter mountoperation-anonymous :: <C-boolean> on <GMountOperation> end;
define property-getter mountoperation-choice :: <C-signed-int> on <GMountOperation> end;
define property-setter mountoperation-choice :: <C-signed-int> on <GMountOperation> end;
define property-getter mountoperation-domain :: <C-string> on <GMountOperation> end;
define property-setter mountoperation-domain :: <C-string> on <GMountOperation> end;
define property-getter mountoperation-password :: <C-string> on <GMountOperation> end;
define property-setter mountoperation-password :: <C-string> on <GMountOperation> end;
define property-getter mountoperation-password-save :: <GPasswordSave> on <GMountOperation> end;
define property-setter mountoperation-password-save :: <GPasswordSave> on <GMountOperation> end;
define property-getter mountoperation-username :: <C-string> on <GMountOperation> end;
define property-setter mountoperation-username :: <C-string> on <GMountOperation> end;
define C-function g-mount-operation-new
  result res :: <GMountOperation>;
  c-name: "g_mount_operation_new";
end;

define C-function g-mount-operation-get-anonymous
  input parameter self :: <GMountOperation>;
  result res :: <C-boolean>;
  c-name: "g_mount_operation_get_anonymous";
end;

define C-function g-mount-operation-get-choice
  input parameter self :: <GMountOperation>;
  result res :: <C-signed-int>;
  c-name: "g_mount_operation_get_choice";
end;

define C-function g-mount-operation-get-domain
  input parameter self :: <GMountOperation>;
  result res :: <C-string>;
  c-name: "g_mount_operation_get_domain";
end;

define C-function g-mount-operation-get-password
  input parameter self :: <GMountOperation>;
  result res :: <C-string>;
  c-name: "g_mount_operation_get_password";
end;

define C-function g-mount-operation-get-password-save
  input parameter self :: <GMountOperation>;
  result res :: <GPasswordSave>;
  c-name: "g_mount_operation_get_password_save";
end;

define C-function g-mount-operation-get-username
  input parameter self :: <GMountOperation>;
  result res :: <C-string>;
  c-name: "g_mount_operation_get_username";
end;

define C-function g-mount-operation-reply
  input parameter self :: <GMountOperation>;
  input parameter result_ :: <GMountOperationResult>;
  c-name: "g_mount_operation_reply";
end;

define C-function g-mount-operation-set-anonymous
  input parameter self :: <GMountOperation>;
  input parameter anonymous_ :: <C-boolean>;
  c-name: "g_mount_operation_set_anonymous";
end;

define C-function g-mount-operation-set-choice
  input parameter self :: <GMountOperation>;
  input parameter choice_ :: <C-signed-int>;
  c-name: "g_mount_operation_set_choice";
end;

define C-function g-mount-operation-set-domain
  input parameter self :: <GMountOperation>;
  input parameter domain_ :: <C-string>;
  c-name: "g_mount_operation_set_domain";
end;

define C-function g-mount-operation-set-password
  input parameter self :: <GMountOperation>;
  input parameter password_ :: <C-string>;
  c-name: "g_mount_operation_set_password";
end;

define C-function g-mount-operation-set-password-save
  input parameter self :: <GMountOperation>;
  input parameter save_ :: <GPasswordSave>;
  c-name: "g_mount_operation_set_password_save";
end;

define C-function g-mount-operation-set-username
  input parameter self :: <GMountOperation>;
  input parameter username_ :: <C-string>;
  c-name: "g_mount_operation_set_username";
end;

define C-struct <_GMountOperationClass>
  constant slot gmountoperationclass-parent-class :: <GObjectClass>;
  constant slot gmountoperationclass-ask-password :: <C-function-pointer>;
  constant slot gmountoperationclass-ask-question :: <C-function-pointer>;
  constant slot gmountoperationclass-reply :: <C-function-pointer>;
  constant slot gmountoperationclass-aborted :: <C-function-pointer>;
  constant slot gmountoperationclass-show-processes :: <C-void*>;
  constant slot gmountoperationclass-show-unmount-progress :: <C-function-pointer>;
  constant slot gmountoperationclass--g-reserved1 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved2 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved3 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved4 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved5 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved6 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved7 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved8 :: <C-void*>;
  constant slot gmountoperationclass--g-reserved9 :: <C-void*>;
  pointer-type-name: <GMountOperationClass>;
end C-struct;

define C-struct <_GMountOperationPrivate>
  pointer-type-name: <GMountOperationPrivate>;
end C-struct;

define constant $G-MOUNT-OPERATION-HANDLED = 0;
define constant $G-MOUNT-OPERATION-ABORTED = 1;
define constant $G-MOUNT-OPERATION-UNHANDLED = 2;
define constant <GMountOperationResult> = <C-int>;
define C-pointer-type <GMountOperationResult*> => <GMountOperationResult>;

define constant $G-MOUNT-UNMOUNT-NONE = 0;
define constant $G-MOUNT-UNMOUNT-FORCE = 1;
define constant <GMountUnmountFlags> = <C-int>;
define C-pointer-type <GMountUnmountFlags*> => <GMountUnmountFlags>;

define constant $NATIVE-VOLUME-MONITOR-EXTENSION-POINT-NAME = "gio-native-volume-monitor";

define constant $NETWORK-MONITOR-EXTENSION-POINT-NAME = "gio-network-monitor";

define open C-subtype <GNativeVolumeMonitor> (<GVolumeMonitor>)
  constant slot gnativevolumemonitor-parent-instance :: <GVolumeMonitor>;
end C-subtype;

define C-pointer-type <GNativeVolumeMonitor*> => <GNativeVolumeMonitor>;

define C-struct <_GNativeVolumeMonitorClass>
  constant slot gnativevolumemonitorclass-parent-class :: <GVolumeMonitorClass>;
  constant slot gnativevolumemonitorclass-get-mount-for-mount-path :: <C-void*>;
  pointer-type-name: <GNativeVolumeMonitorClass>;
end C-struct;

define open C-subtype <GNetworkAddress> (<GObject>)
  constant slot gnetworkaddress-parent-instance :: <GObject>;
  constant slot gnetworkaddress-priv :: <GNetworkAddressPrivate>;
end C-subtype;

define C-pointer-type <GNetworkAddress*> => <GNetworkAddress>;

define property-getter networkaddress-hostname :: <C-string> on <GNetworkAddress> end;
define property-setter networkaddress-hostname :: <C-string> on <GNetworkAddress> end;
define property-getter networkaddress-port :: <C-unsigned-int> on <GNetworkAddress> end;
define property-setter networkaddress-port :: <C-unsigned-int> on <GNetworkAddress> end;
define property-getter networkaddress-scheme :: <C-string> on <GNetworkAddress> end;
define property-setter networkaddress-scheme :: <C-string> on <GNetworkAddress> end;
define C-function g-network-address-new
  input parameter hostname_ :: <C-string>;
  input parameter port_ :: <C-unsigned-short>;
  result res :: <GNetworkAddress>;
  c-name: "g_network_address_new";
end;

define C-function g-network-address-parse
  input parameter host_and_port_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  result res :: <GSocketConnectable>;
  c-name: "g_network_address_parse";
end;

define C-function g-network-address-parse-uri
  input parameter uri_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  result res :: <GSocketConnectable>;
  c-name: "g_network_address_parse_uri";
end;

define C-function g-network-address-get-hostname
  input parameter self :: <GNetworkAddress>;
  result res :: <C-string>;
  c-name: "g_network_address_get_hostname";
end;

define C-function g-network-address-get-port
  input parameter self :: <GNetworkAddress>;
  result res :: <C-unsigned-short>;
  c-name: "g_network_address_get_port";
end;

define C-function g-network-address-get-scheme
  input parameter self :: <GNetworkAddress>;
  result res :: <C-string>;
  c-name: "g_network_address_get_scheme";
end;

define C-struct <_GNetworkAddressClass>
  constant slot gnetworkaddressclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GNetworkAddressClass>;
end C-struct;

define C-struct <_GNetworkAddressPrivate>
  pointer-type-name: <GNetworkAddressPrivate>;
end C-struct;

// Interface
define open C-subtype <GNetworkMonitor> (<GInitable>)
end C-subtype;

define C-pointer-type <GNetworkMonitor*> => <GNetworkMonitor>;

define C-function g-network-monitor-get-default
  result res :: <GNetworkMonitor>;
  c-name: "g_network_monitor_get_default";
end;

define C-function g-network-monitor-can-reach
  input parameter self :: <GNetworkMonitor>;
  input parameter connectable_ :: <GSocketConnectable>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_network_monitor_can_reach";
end;

define C-function g-network-monitor-can-reach-async
  input parameter self :: <GNetworkMonitor>;
  input parameter connectable_ :: <GSocketConnectable>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_network_monitor_can_reach_async";
end;

define C-function g-network-monitor-can-reach-finish
  input parameter self :: <GNetworkMonitor>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_network_monitor_can_reach_finish";
end;

define C-function g-network-monitor-get-network-available
  input parameter self :: <GNetworkMonitor>;
  result res :: <C-boolean>;
  c-name: "g_network_monitor_get_network_available";
end;

define C-struct <_GNetworkMonitorInterface>
  constant slot gnetworkmonitorinterface-g-iface :: <GTypeInterface>;
  constant slot gnetworkmonitorinterface-network-changed :: <C-function-pointer>;
  constant slot gnetworkmonitorinterface-can-reach :: <C-function-pointer>;
  constant slot gnetworkmonitorinterface-can-reach-async :: <C-function-pointer>;
  constant slot gnetworkmonitorinterface-can-reach-finish :: <C-function-pointer>;
  pointer-type-name: <GNetworkMonitorInterface>;
end C-struct;

define open C-subtype <GNetworkService> (<GObject>)
  constant slot gnetworkservice-parent-instance :: <GObject>;
  constant slot gnetworkservice-priv :: <GNetworkServicePrivate>;
end C-subtype;

define C-pointer-type <GNetworkService*> => <GNetworkService>;

define property-getter networkservice-domain :: <C-string> on <GNetworkService> end;
define property-setter networkservice-domain :: <C-string> on <GNetworkService> end;
define property-getter networkservice-protocol :: <C-string> on <GNetworkService> end;
define property-setter networkservice-protocol :: <C-string> on <GNetworkService> end;
define property-getter networkservice-scheme :: <C-string> on <GNetworkService> end;
define property-setter networkservice-scheme :: <C-string> on <GNetworkService> end;
define property-getter networkservice-service :: <C-string> on <GNetworkService> end;
define property-setter networkservice-service :: <C-string> on <GNetworkService> end;
define C-function g-network-service-new
  input parameter service_ :: <C-string>;
  input parameter protocol_ :: <C-string>;
  input parameter domain_ :: <C-string>;
  result res :: <GNetworkService>;
  c-name: "g_network_service_new";
end;

define C-function g-network-service-get-domain
  input parameter self :: <GNetworkService>;
  result res :: <C-string>;
  c-name: "g_network_service_get_domain";
end;

define C-function g-network-service-get-protocol
  input parameter self :: <GNetworkService>;
  result res :: <C-string>;
  c-name: "g_network_service_get_protocol";
end;

define C-function g-network-service-get-scheme
  input parameter self :: <GNetworkService>;
  result res :: <C-string>;
  c-name: "g_network_service_get_scheme";
end;

define C-function g-network-service-get-service
  input parameter self :: <GNetworkService>;
  result res :: <C-string>;
  c-name: "g_network_service_get_service";
end;

define C-function g-network-service-set-scheme
  input parameter self :: <GNetworkService>;
  input parameter scheme_ :: <C-string>;
  c-name: "g_network_service_set_scheme";
end;

define C-struct <_GNetworkServiceClass>
  constant slot gnetworkserviceclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GNetworkServiceClass>;
end C-struct;

define C-struct <_GNetworkServicePrivate>
  pointer-type-name: <GNetworkServicePrivate>;
end C-struct;

define open C-subtype <GOutputStream> (<GObject>)
  constant slot goutputstream-parent-instance :: <GObject>;
  constant slot goutputstream-priv :: <GOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GOutputStream*> => <GOutputStream>;

define C-function g-output-stream-clear-pending
  input parameter self :: <GOutputStream>;
  c-name: "g_output_stream_clear_pending";
end;

define C-function g-output-stream-close
  input parameter self :: <GOutputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_close";
end;

define C-function g-output-stream-close-async
  input parameter self :: <GOutputStream>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_output_stream_close_async";
end;

define C-function g-output-stream-close-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_close_finish";
end;

define C-function g-output-stream-flush
  input parameter self :: <GOutputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_flush";
end;

define C-function g-output-stream-flush-async
  input parameter self :: <GOutputStream>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_output_stream_flush_async";
end;

define C-function g-output-stream-flush-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_flush_finish";
end;

define C-function g-output-stream-has-pending
  input parameter self :: <GOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_has_pending";
end;

define C-function g-output-stream-is-closed
  input parameter self :: <GOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_is_closed";
end;

define C-function g-output-stream-is-closing
  input parameter self :: <GOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_is_closing";
end;

define C-function g-output-stream-set-pending
  input parameter self :: <GOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_set_pending";
end;

define C-function g-output-stream-splice
  input parameter self :: <GOutputStream>;
  input parameter source_ :: <GInputStream>;
  input parameter flags_ :: <GOutputStreamSpliceFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_splice";
end;

define C-function g-output-stream-splice-async
  input parameter self :: <GOutputStream>;
  input parameter source_ :: <GInputStream>;
  input parameter flags_ :: <GOutputStreamSpliceFlags>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_output_stream_splice_async";
end;

define C-function g-output-stream-splice-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_splice_finish";
end;

define C-function g-output-stream-write
  input parameter self :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write";
end;

define C-function g-output-stream-write-all
  input parameter self :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  output parameter bytes_written_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_write_all";
end;

define C-function g-output-stream-write-async
  input parameter self :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_output_stream_write_async";
end;

define C-function g-output-stream-write-bytes
  input parameter self :: <GOutputStream>;
  input parameter bytes_ :: <GBytes>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write_bytes";
end;

define C-function g-output-stream-write-bytes-async
  input parameter self :: <GOutputStream>;
  input parameter bytes_ :: <GBytes>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_output_stream_write_bytes_async";
end;

define C-function g-output-stream-write-bytes-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write_bytes_finish";
end;

define C-function g-output-stream-write-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write_finish";
end;

define C-struct <_GOutputStreamClass>
  constant slot goutputstreamclass-parent-class :: <GObjectClass>;
  constant slot goutputstreamclass-write-fn :: <C-function-pointer>;
  constant slot goutputstreamclass-splice :: <C-function-pointer>;
  constant slot goutputstreamclass-flush :: <C-function-pointer>;
  constant slot goutputstreamclass-close-fn :: <C-function-pointer>;
  constant slot goutputstreamclass-write-async :: <C-function-pointer>;
  constant slot goutputstreamclass-write-finish :: <C-function-pointer>;
  constant slot goutputstreamclass-splice-async :: <C-function-pointer>;
  constant slot goutputstreamclass-splice-finish :: <C-function-pointer>;
  constant slot goutputstreamclass-flush-async :: <C-function-pointer>;
  constant slot goutputstreamclass-flush-finish :: <C-function-pointer>;
  constant slot goutputstreamclass-close-async :: <C-function-pointer>;
  constant slot goutputstreamclass-close-finish :: <C-function-pointer>;
  constant slot goutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved5 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved6 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved7 :: <C-void*>;
  constant slot goutputstreamclass--g-reserved8 :: <C-void*>;
  pointer-type-name: <GOutputStreamClass>;
end C-struct;

define C-struct <_GOutputStreamPrivate>
  pointer-type-name: <GOutputStreamPrivate>;
end C-struct;

define constant $G-OUTPUT-STREAM-SPLICE-NONE = 0;
define constant $G-OUTPUT-STREAM-SPLICE-CLOSE-SOURCE = 1;
define constant $G-OUTPUT-STREAM-SPLICE-CLOSE-TARGET = 2;
define constant <GOutputStreamSpliceFlags> = <C-int>;
define C-pointer-type <GOutputStreamSpliceFlags*> => <GOutputStreamSpliceFlags>;

define C-struct <_GOutputVector>
  slot goutputvector-buffer :: <C-void*>;
  slot goutputvector-size :: <C-unsigned-long>;
  pointer-type-name: <GOutputVector>;
end C-struct;

define constant $PROXY-EXTENSION-POINT-NAME = "gio-proxy";

define constant $PROXY-RESOLVER-EXTENSION-POINT-NAME = "gio-proxy-resolver";

define constant $G-PASSWORD-SAVE-NEVER = 0;
define constant $G-PASSWORD-SAVE-FOR-SESSION = 1;
define constant $G-PASSWORD-SAVE-PERMANENTLY = 2;
define constant <GPasswordSave> = <C-int>;
define C-pointer-type <GPasswordSave*> => <GPasswordSave>;

define open C-subtype <GPermission> (<GObject>)
  constant slot gpermission-parent-instance :: <GObject>;
  constant slot gpermission-priv :: <GPermissionPrivate>;
end C-subtype;

define C-pointer-type <GPermission*> => <GPermission>;

define property-getter permission-allowed :: <C-boolean> on <GPermission> end;
define property-getter permission-can-acquire :: <C-boolean> on <GPermission> end;
define property-getter permission-can-release :: <C-boolean> on <GPermission> end;
define C-function g-permission-acquire
  input parameter self :: <GPermission>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_permission_acquire";
end;

define C-function g-permission-acquire-async
  input parameter self :: <GPermission>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_permission_acquire_async";
end;

define C-function g-permission-acquire-finish
  input parameter self :: <GPermission>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_permission_acquire_finish";
end;

define C-function g-permission-get-allowed
  input parameter self :: <GPermission>;
  result res :: <C-boolean>;
  c-name: "g_permission_get_allowed";
end;

define C-function g-permission-get-can-acquire
  input parameter self :: <GPermission>;
  result res :: <C-boolean>;
  c-name: "g_permission_get_can_acquire";
end;

define C-function g-permission-get-can-release
  input parameter self :: <GPermission>;
  result res :: <C-boolean>;
  c-name: "g_permission_get_can_release";
end;

define C-function g-permission-impl-update
  input parameter self :: <GPermission>;
  input parameter allowed_ :: <C-boolean>;
  input parameter can_acquire_ :: <C-boolean>;
  input parameter can_release_ :: <C-boolean>;
  c-name: "g_permission_impl_update";
end;

define C-function g-permission-release
  input parameter self :: <GPermission>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_permission_release";
end;

define C-function g-permission-release-async
  input parameter self :: <GPermission>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_permission_release_async";
end;

define C-function g-permission-release-finish
  input parameter self :: <GPermission>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_permission_release_finish";
end;

define C-struct <_GPermissionClass>
  constant slot gpermissionclass-parent-class :: <GObjectClass>;
  constant slot gpermissionclass-acquire :: <C-function-pointer>;
  constant slot gpermissionclass-acquire-async :: <C-function-pointer>;
  constant slot gpermissionclass-acquire-finish :: <C-function-pointer>;
  constant slot gpermissionclass-release :: <C-function-pointer>;
  constant slot gpermissionclass-release-async :: <C-function-pointer>;
  constant slot gpermissionclass-release-finish :: <C-function-pointer>;
  constant slot gpermissionclass-reserved :: <C-void*>;
  pointer-type-name: <GPermissionClass>;
end C-struct;

define C-struct <_GPermissionPrivate>
  pointer-type-name: <GPermissionPrivate>;
end C-struct;

// Interface
define open C-subtype <GPollableInputStream> (<GInputStream>)
end C-subtype;

define C-pointer-type <GPollableInputStream*> => <GPollableInputStream>;

define C-function g-pollable-input-stream-can-poll
  input parameter self :: <GPollableInputStream>;
  result res :: <C-boolean>;
  c-name: "g_pollable_input_stream_can_poll";
end;

define C-function g-pollable-input-stream-create-source
  input parameter self :: <GPollableInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSource>;
  c-name: "g_pollable_input_stream_create_source";
end;

define C-function g-pollable-input-stream-is-readable
  input parameter self :: <GPollableInputStream>;
  result res :: <C-boolean>;
  c-name: "g_pollable_input_stream_is_readable";
end;

define C-function g-pollable-input-stream-read-nonblocking
  input parameter self :: <GPollableInputStream>;
  input parameter buffer_ :: <C-void*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_input_stream_read_nonblocking";
end;

define C-struct <_GPollableInputStreamInterface>
  constant slot gpollableinputstreaminterface-g-iface :: <GTypeInterface>;
  constant slot gpollableinputstreaminterface-can-poll :: <C-function-pointer>;
  constant slot gpollableinputstreaminterface-is-readable :: <C-function-pointer>;
  constant slot gpollableinputstreaminterface-create-source :: <C-function-pointer>;
  constant slot gpollableinputstreaminterface-read-nonblocking :: <C-function-pointer>;
  pointer-type-name: <GPollableInputStreamInterface>;
end C-struct;

// Interface
define open C-subtype <GPollableOutputStream> (<GOutputStream>)
end C-subtype;

define C-pointer-type <GPollableOutputStream*> => <GPollableOutputStream>;

define C-function g-pollable-output-stream-can-poll
  input parameter self :: <GPollableOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_pollable_output_stream_can_poll";
end;

define C-function g-pollable-output-stream-create-source
  input parameter self :: <GPollableOutputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSource>;
  c-name: "g_pollable_output_stream_create_source";
end;

define C-function g-pollable-output-stream-is-writable
  input parameter self :: <GPollableOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_pollable_output_stream_is_writable";
end;

define C-function g-pollable-output-stream-write-nonblocking
  input parameter self :: <GPollableOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_output_stream_write_nonblocking";
end;

define C-struct <_GPollableOutputStreamInterface>
  constant slot gpollableoutputstreaminterface-g-iface :: <GTypeInterface>;
  constant slot gpollableoutputstreaminterface-can-poll :: <C-function-pointer>;
  constant slot gpollableoutputstreaminterface-is-writable :: <C-function-pointer>;
  constant slot gpollableoutputstreaminterface-create-source :: <C-function-pointer>;
  constant slot gpollableoutputstreaminterface-write-nonblocking :: <C-function-pointer>;
  pointer-type-name: <GPollableOutputStreamInterface>;
end C-struct;

// Interface
define open C-subtype <GProxy> (<C-void*>)
end C-subtype;

define C-pointer-type <GProxy*> => <GProxy>;

define C-function g-proxy-get-default-for-protocol
  input parameter protocol_ :: <C-string>;
  result res :: <GProxy>;
  c-name: "g_proxy_get_default_for_protocol";
end;

define C-function g-proxy-connect
  input parameter self :: <GProxy>;
  input parameter connection_ :: <GIOStream>;
  input parameter proxy_address_ :: <GProxyAddress>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GIOStream>;
  c-name: "g_proxy_connect";
end;

define C-function g-proxy-connect-async
  input parameter self :: <GProxy>;
  input parameter connection_ :: <GIOStream>;
  input parameter proxy_address_ :: <GProxyAddress>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_proxy_connect_async";
end;

define C-function g-proxy-connect-finish
  input parameter self :: <GProxy>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GIOStream>;
  c-name: "g_proxy_connect_finish";
end;

define C-function g-proxy-supports-hostname
  input parameter self :: <GProxy>;
  result res :: <C-boolean>;
  c-name: "g_proxy_supports_hostname";
end;

define open C-subtype <GProxyAddress> (<GInetSocketAddress>)
  constant slot gproxyaddress-parent-instance :: <GInetSocketAddress>;
  constant slot gproxyaddress-priv :: <GProxyAddressPrivate>;
end C-subtype;

define C-pointer-type <GProxyAddress*> => <GProxyAddress>;

define property-getter proxyaddress-destination-hostname :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-destination-hostname :: <C-string> on <GProxyAddress> end;
define property-getter proxyaddress-destination-port :: <C-unsigned-int> on <GProxyAddress> end;
define property-setter proxyaddress-destination-port :: <C-unsigned-int> on <GProxyAddress> end;
define property-getter proxyaddress-destination-protocol :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-destination-protocol :: <C-string> on <GProxyAddress> end;
define property-getter proxyaddress-password :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-password :: <C-string> on <GProxyAddress> end;
define property-getter proxyaddress-protocol :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-protocol :: <C-string> on <GProxyAddress> end;
define property-getter proxyaddress-uri :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-uri :: <C-string> on <GProxyAddress> end;
define property-getter proxyaddress-username :: <C-string> on <GProxyAddress> end;
define property-setter proxyaddress-username :: <C-string> on <GProxyAddress> end;
define C-function g-proxy-address-new
  input parameter inetaddr_ :: <GInetAddress>;
  input parameter port_ :: <C-unsigned-short>;
  input parameter protocol_ :: <C-string>;
  input parameter dest_hostname_ :: <C-string>;
  input parameter dest_port_ :: <C-unsigned-short>;
  input parameter username_ :: <C-string>;
  input parameter password_ :: <C-string>;
  result res :: <GSocketAddress>;
  c-name: "g_proxy_address_new";
end;

define C-function g-proxy-address-get-destination-hostname
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_destination_hostname";
end;

define C-function g-proxy-address-get-destination-port
  input parameter self :: <GProxyAddress>;
  result res :: <C-unsigned-short>;
  c-name: "g_proxy_address_get_destination_port";
end;

define C-function g-proxy-address-get-destination-protocol
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_destination_protocol";
end;

define C-function g-proxy-address-get-password
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_password";
end;

define C-function g-proxy-address-get-protocol
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_protocol";
end;

define C-function g-proxy-address-get-uri
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_uri";
end;

define C-function g-proxy-address-get-username
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_username";
end;

define C-struct <_GProxyAddressClass>
  constant slot gproxyaddressclass-parent-class :: <GInetSocketAddressClass>;
  pointer-type-name: <GProxyAddressClass>;
end C-struct;

define open C-subtype <GProxyAddressEnumerator> (<GSocketAddressEnumerator>)
  constant slot gproxyaddressenumerator-parent-instance :: <GSocketAddressEnumerator>;
  constant slot gproxyaddressenumerator-priv :: <GProxyAddressEnumeratorPrivate>;
end C-subtype;

define C-pointer-type <GProxyAddressEnumerator*> => <GProxyAddressEnumerator>;

define property-getter proxyaddressenumerator-connectable :: <GSocketConnectable> on <GProxyAddressEnumerator> end;
define property-setter proxyaddressenumerator-connectable :: <GSocketConnectable> on <GProxyAddressEnumerator> end;
define property-getter proxyaddressenumerator-proxy-resolver :: <GProxyResolver> on <GProxyAddressEnumerator> end;
define property-setter proxyaddressenumerator-proxy-resolver :: <GProxyResolver> on <GProxyAddressEnumerator> end;
define property-getter proxyaddressenumerator-uri :: <C-string> on <GProxyAddressEnumerator> end;
define property-setter proxyaddressenumerator-uri :: <C-string> on <GProxyAddressEnumerator> end;
define C-struct <_GProxyAddressEnumeratorClass>
  constant slot gproxyaddressenumeratorclass-parent-class :: <GSocketAddressEnumeratorClass>;
  constant slot gproxyaddressenumeratorclass--g-reserved1 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved2 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved3 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved4 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved5 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved6 :: <C-void*>;
  constant slot gproxyaddressenumeratorclass--g-reserved7 :: <C-void*>;
  pointer-type-name: <GProxyAddressEnumeratorClass>;
end C-struct;

define C-struct <_GProxyAddressEnumeratorPrivate>
  pointer-type-name: <GProxyAddressEnumeratorPrivate>;
end C-struct;

define C-struct <_GProxyAddressPrivate>
  pointer-type-name: <GProxyAddressPrivate>;
end C-struct;

define C-struct <_GProxyInterface>
  constant slot gproxyinterface-g-iface :: <GTypeInterface>;
  constant slot gproxyinterface-connect :: <C-function-pointer>;
  constant slot gproxyinterface-connect-async :: <C-function-pointer>;
  constant slot gproxyinterface-connect-finish :: <C-function-pointer>;
  constant slot gproxyinterface-supports-hostname :: <C-function-pointer>;
  pointer-type-name: <GProxyInterface>;
end C-struct;

// Interface
define open C-subtype <GProxyResolver> (<C-void*>)
end C-subtype;

define C-pointer-type <GProxyResolver*> => <GProxyResolver>;

define C-function g-proxy-resolver-get-default
  result res :: <GProxyResolver>;
  c-name: "g_proxy_resolver_get_default";
end;

define C-function g-proxy-resolver-is-supported
  input parameter self :: <GProxyResolver>;
  result res :: <C-boolean>;
  c-name: "g_proxy_resolver_is_supported";
end;

define C-function g-proxy-resolver-lookup
  input parameter self :: <GProxyResolver>;
  input parameter uri_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string*>;
  c-name: "g_proxy_resolver_lookup";
end;

define C-function g-proxy-resolver-lookup-async
  input parameter self :: <GProxyResolver>;
  input parameter uri_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_proxy_resolver_lookup_async";
end;

define C-function g-proxy-resolver-lookup-finish
  input parameter self :: <GProxyResolver>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-string*>;
  c-name: "g_proxy_resolver_lookup_finish";
end;

define C-struct <_GProxyResolverInterface>
  constant slot gproxyresolverinterface-g-iface :: <GTypeInterface>;
  constant slot gproxyresolverinterface-is-supported :: <C-function-pointer>;
  constant slot gproxyresolverinterface-lookup :: <C-function-pointer>;
  constant slot gproxyresolverinterface-lookup-async :: <C-function-pointer>;
  constant slot gproxyresolverinterface-lookup-finish :: <C-function-pointer>;
  pointer-type-name: <GProxyResolverInterface>;
end C-struct;

// Interface
define open C-subtype <GRemoteActionGroup> (<GActionGroup>)
end C-subtype;

define C-pointer-type <GRemoteActionGroup*> => <GRemoteActionGroup>;

define C-function g-remote-action-group-activate-action-full
  input parameter self :: <GRemoteActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter parameter_ :: <GVariant>;
  input parameter platform_data_ :: <GVariant>;
  c-name: "g_remote_action_group_activate_action_full";
end;

define C-function g-remote-action-group-change-action-state-full
  input parameter self :: <GRemoteActionGroup>;
  input parameter action_name_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  input parameter platform_data_ :: <GVariant>;
  c-name: "g_remote_action_group_change_action_state_full";
end;

define C-struct <_GRemoteActionGroupInterface>
  constant slot gremoteactiongroupinterface-g-iface :: <GTypeInterface>;
  constant slot gremoteactiongroupinterface-activate-action-full :: <C-function-pointer>;
  constant slot gremoteactiongroupinterface-change-action-state-full :: <C-function-pointer>;
  pointer-type-name: <GRemoteActionGroupInterface>;
end C-struct;

define open C-subtype <GResolver> (<GObject>)
  constant slot gresolver-parent-instance :: <GObject>;
  constant slot gresolver-priv :: <GResolverPrivate>;
end C-subtype;

define C-pointer-type <GResolver*> => <GResolver>;

define C-function g-resolver-get-default
  result res :: <GResolver>;
  c-name: "g_resolver_get_default";
end;

define C-function g-resolver-lookup-by-address
  input parameter self :: <GResolver>;
  input parameter address_ :: <GInetAddress>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string>;
  c-name: "g_resolver_lookup_by_address";
end;

define C-function g-resolver-lookup-by-address-async
  input parameter self :: <GResolver>;
  input parameter address_ :: <GInetAddress>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_resolver_lookup_by_address_async";
end;

define C-function g-resolver-lookup-by-address-finish
  input parameter self :: <GResolver>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-string>;
  c-name: "g_resolver_lookup_by_address_finish";
end;

define C-function g-resolver-lookup-by-name
  input parameter self :: <GResolver>;
  input parameter hostname_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_by_name";
end;

define C-function g-resolver-lookup-by-name-async
  input parameter self :: <GResolver>;
  input parameter hostname_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_resolver_lookup_by_name_async";
end;

define C-function g-resolver-lookup-by-name-finish
  input parameter self :: <GResolver>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_by_name_finish";
end;

define C-function g-resolver-lookup-records
  input parameter self :: <GResolver>;
  input parameter rrname_ :: <C-string>;
  input parameter record_type_ :: <GResolverRecordType>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_records";
end;

define C-function g-resolver-lookup-records-async
  input parameter self :: <GResolver>;
  input parameter rrname_ :: <C-string>;
  input parameter record_type_ :: <GResolverRecordType>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_resolver_lookup_records_async";
end;

define C-function g-resolver-lookup-records-finish
  input parameter self :: <GResolver>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_records_finish";
end;

define C-function g-resolver-lookup-service
  input parameter self :: <GResolver>;
  input parameter service_ :: <C-string>;
  input parameter protocol_ :: <C-string>;
  input parameter domain_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_service";
end;

define C-function g-resolver-lookup-service-async
  input parameter self :: <GResolver>;
  input parameter service_ :: <C-string>;
  input parameter protocol_ :: <C-string>;
  input parameter domain_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_resolver_lookup_service_async";
end;

define C-function g-resolver-lookup-service-finish
  input parameter self :: <GResolver>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_service_finish";
end;

define C-function g-resolver-set-default
  input parameter self :: <GResolver>;
  c-name: "g_resolver_set_default";
end;

define C-struct <_GResolverClass>
  constant slot gresolverclass-parent-class :: <GObjectClass>;
  constant slot gresolverclass-reload :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-name :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-name-async :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-name-finish :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-address :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-address-async :: <C-function-pointer>;
  constant slot gresolverclass-lookup-by-address-finish :: <C-function-pointer>;
  constant slot gresolverclass-lookup-service :: <C-void*>;
  constant slot gresolverclass-lookup-service-async :: <C-function-pointer>;
  constant slot gresolverclass-lookup-service-finish :: <C-function-pointer>;
  constant slot gresolverclass-lookup-records :: <C-function-pointer>;
  constant slot gresolverclass-lookup-records-async :: <C-function-pointer>;
  constant slot gresolverclass-lookup-records-finish :: <C-function-pointer>;
  constant slot gresolverclass--g-reserved4 :: <C-void*>;
  constant slot gresolverclass--g-reserved5 :: <C-void*>;
  constant slot gresolverclass--g-reserved6 :: <C-void*>;
  pointer-type-name: <GResolverClass>;
end C-struct;

define constant $G-RESOLVER-ERROR-NOT-FOUND = 0;
define constant $G-RESOLVER-ERROR-TEMPORARY-FAILURE = 1;
define constant $G-RESOLVER-ERROR-INTERNAL = 2;
define constant <GResolverError> = <C-int>;
define C-pointer-type <GResolverError*> => <GResolverError>;

define C-struct <_GResolverPrivate>
  pointer-type-name: <GResolverPrivate>;
end C-struct;

define constant $G-RESOLVER-RECORD-SRV = 1;
define constant $G-RESOLVER-RECORD-MX = 2;
define constant $G-RESOLVER-RECORD-TXT = 3;
define constant $G-RESOLVER-RECORD-SOA = 4;
define constant $G-RESOLVER-RECORD-NS = 5;
define constant <GResolverRecordType> = <C-int>;
define C-pointer-type <GResolverRecordType*> => <GResolverRecordType>;

define C-struct <_GResource>
  pointer-type-name: <GResource>;
end C-struct;

define C-function g-resource-new-from-data
  input parameter data_ :: <GBytes>;
  result res :: <GResource>;
  c-name: "g_resource_new_from_data";
end;

define C-function g-resources-register
  input parameter self :: <GResource>;
  c-name: "g_resources_register";
end;

define C-function g-resources-unregister
  input parameter self :: <GResource>;
  c-name: "g_resources_unregister";
end;

define C-function g-resource-enumerate-children
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <C-string*>;
  c-name: "g_resource_enumerate_children";
end;

define C-function g-resource-get-info
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter size_ :: <C-unsigned-long*>;
  output parameter flags_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_resource_get_info";
end;

define C-function g-resource-lookup-data
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <GBytes>;
  c-name: "g_resource_lookup_data";
end;

define C-function g-resource-open-stream
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <GInputStream>;
  c-name: "g_resource_open_stream";
end;

define C-function g-resource-ref
  input parameter self :: <GResource>;
  result res :: <GResource>;
  c-name: "g_resource_ref";
end;

define C-function g-resource-unref
  input parameter self :: <GResource>;
  c-name: "g_resource_unref";
end;

define C-function g-resource-load
  input parameter filename_ :: <C-string>;
  result res :: <GResource>;
  c-name: "g_resource_load";
end;

define constant $G-RESOURCE-ERROR-NOT-FOUND = 0;
define constant $G-RESOURCE-ERROR-INTERNAL = 1;
define constant <GResourceError> = <C-int>;
define C-pointer-type <GResourceError*> => <GResourceError>;

define constant $G-RESOURCE-FLAGS-NONE = 0;
define constant $G-RESOURCE-FLAGS-COMPRESSED = 1;
define constant <GResourceFlags> = <C-int>;
define C-pointer-type <GResourceFlags*> => <GResourceFlags>;

define constant $G-RESOURCE-LOOKUP-FLAGS-NONE = 0;
define constant <GResourceLookupFlags> = <C-int>;
define C-pointer-type <GResourceLookupFlags*> => <GResourceLookupFlags>;

// Interface
define open C-subtype <GSeekable> (<C-void*>)
end C-subtype;

define C-pointer-type <GSeekable*> => <GSeekable>;

define C-function g-seekable-can-seek
  input parameter self :: <GSeekable>;
  result res :: <C-boolean>;
  c-name: "g_seekable_can_seek";
end;

define C-function g-seekable-can-truncate
  input parameter self :: <GSeekable>;
  result res :: <C-boolean>;
  c-name: "g_seekable_can_truncate";
end;

define C-function g-seekable-seek
  input parameter self :: <GSeekable>;
  input parameter offset_ :: <C-signed-long>;
  input parameter type_ :: <GSeekType>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_seekable_seek";
end;

define C-function g-seekable-tell
  input parameter self :: <GSeekable>;
  result res :: <C-signed-long>;
  c-name: "g_seekable_tell";
end;

define C-function g-seekable-truncate
  input parameter self :: <GSeekable>;
  input parameter offset_ :: <C-signed-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_seekable_truncate";
end;

define C-struct <_GSeekableIface>
  constant slot gseekableiface-g-iface :: <GTypeInterface>;
  constant slot gseekableiface-tell :: <C-function-pointer>;
  constant slot gseekableiface-can-seek :: <C-function-pointer>;
  constant slot gseekableiface-seek :: <C-function-pointer>;
  constant slot gseekableiface-can-truncate :: <C-function-pointer>;
  constant slot gseekableiface-truncate-fn :: <C-function-pointer>;
  pointer-type-name: <GSeekableIface>;
end C-struct;

define open C-subtype <GSettings> (<GObject>)
  constant slot gsettings-parent-instance :: <GObject>;
  constant slot gsettings-priv :: <GSettingsPrivate>;
end C-subtype;

define C-pointer-type <GSettings*> => <GSettings>;

define property-getter settings-delay-apply :: <C-boolean> on <GSettings> end;
define property-getter settings-has-unapplied :: <C-boolean> on <GSettings> end;
define property-getter settings-path :: <C-string> on <GSettings> end;
define property-setter settings-path :: <C-string> on <GSettings> end;
define property-getter settings-schema :: <C-string> on <GSettings> end;
define property-setter settings-schema :: <C-string> on <GSettings> end;
define property-getter settings-schema-id :: <C-string> on <GSettings> end;
define property-setter settings-schema-id :: <C-string> on <GSettings> end;
define property-getter settings-settings-schema :: <GSettingsSchema> on <GSettings> end;
define property-setter settings-settings-schema :: <GSettingsSchema> on <GSettings> end;
define C-function g-settings-new
  input parameter schema_id_ :: <C-string>;
  result res :: <GSettings>;
  c-name: "g_settings_new";
end;

define C-function g-settings-new-full
  input parameter schema_ :: <GSettingsSchema>;
  input parameter backend_ :: <GSettingsBackend>;
  input parameter path_ :: <C-string>;
  result res :: <GSettings>;
  c-name: "g_settings_new_full";
end;

define C-function g-settings-new-with-backend
  input parameter schema_id_ :: <C-string>;
  input parameter backend_ :: <GSettingsBackend>;
  result res :: <GSettings>;
  c-name: "g_settings_new_with_backend";
end;

define C-function g-settings-new-with-backend-and-path
  input parameter schema_id_ :: <C-string>;
  input parameter backend_ :: <GSettingsBackend>;
  input parameter path_ :: <C-string>;
  result res :: <GSettings>;
  c-name: "g_settings_new_with_backend_and_path";
end;

define C-function g-settings-new-with-path
  input parameter schema_id_ :: <C-string>;
  input parameter path_ :: <C-string>;
  result res :: <GSettings>;
  c-name: "g_settings_new_with_path";
end;

define C-function g-settings-list-relocatable-schemas
  result res :: <C-string*>;
  c-name: "g_settings_list_relocatable_schemas";
end;

define C-function g-settings-list-schemas
  result res :: <C-string*>;
  c-name: "g_settings_list_schemas";
end;

define C-function g-settings-sync
  c-name: "g_settings_sync";
end;

define C-function g-settings-unbind
  input parameter object_ :: <C-void*>;
  input parameter property_ :: <C-string>;
  c-name: "g_settings_unbind";
end;

define C-function g-settings-apply
  input parameter self :: <GSettings>;
  c-name: "g_settings_apply";
end;

define C-function g-settings-bind
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter object_ :: <GObject>;
  input parameter property_ :: <C-string>;
  input parameter flags_ :: <GSettingsBindFlags>;
  c-name: "g_settings_bind";
end;

define C-function g-settings-bind-writable
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter object_ :: <GObject>;
  input parameter property_ :: <C-string>;
  input parameter inverted_ :: <C-boolean>;
  c-name: "g_settings_bind_writable";
end;

define C-function g-settings-create-action
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <GAction>;
  c-name: "g_settings_create_action";
end;

define C-function g-settings-delay
  input parameter self :: <GSettings>;
  c-name: "g_settings_delay";
end;

define C-function g-settings-get-boolean
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_settings_get_boolean";
end;

define C-function g-settings-get-child
  input parameter self :: <GSettings>;
  input parameter name_ :: <C-string>;
  result res :: <GSettings>;
  c-name: "g_settings_get_child";
end;

define C-function g-settings-get-double
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-double>;
  c-name: "g_settings_get_double";
end;

define C-function g-settings-get-enum
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_settings_get_enum";
end;

define C-function g-settings-get-flags
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_settings_get_flags";
end;

define C-function g-settings-get-has-unapplied
  input parameter self :: <GSettings>;
  result res :: <C-boolean>;
  c-name: "g_settings_get_has_unapplied";
end;

define C-function g-settings-get-int
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_settings_get_int";
end;

define C-function g-settings-get-mapped
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter mapping_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  result res :: <C-void*>;
  c-name: "g_settings_get_mapped";
end;

define C-function g-settings-get-range
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_settings_get_range";
end;

define C-function g-settings-get-string
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_settings_get_string";
end;

define C-function g-settings-get-strv
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_settings_get_strv";
end;

define C-function g-settings-get-uint
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_settings_get_uint";
end;

define C-function g-settings-get-value
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_settings_get_value";
end;

define C-function g-settings-is-writable
  input parameter self :: <GSettings>;
  input parameter name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_settings_is_writable";
end;

define C-function g-settings-list-children
  input parameter self :: <GSettings>;
  result res :: <C-string*>;
  c-name: "g_settings_list_children";
end;

define C-function g-settings-list-keys
  input parameter self :: <GSettings>;
  result res :: <C-string*>;
  c-name: "g_settings_list_keys";
end;

define C-function g-settings-range-check
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_settings_range_check";
end;

define C-function g-settings-reset
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  c-name: "g_settings_reset";
end;

define C-function g-settings-revert
  input parameter self :: <GSettings>;
  c-name: "g_settings_revert";
end;

define C-function g-settings-set-boolean
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_boolean";
end;

define C-function g-settings-set-double
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-double>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_double";
end;

define C-function g-settings-set-enum
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_enum";
end;

define C-function g-settings-set-flags
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_flags";
end;

define C-function g-settings-set-int
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_int";
end;

define C-function g-settings-set-string
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_string";
end;

define C-function g-settings-set-strv
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_strv";
end;

define C-function g-settings-set-uint
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_uint";
end;

define C-function g-settings-set-value
  input parameter self :: <GSettings>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_settings_set_value";
end;

define C-struct <_GSettingsBackend>
  pointer-type-name: <GSettingsBackend>;
end C-struct;

define constant $G-SETTINGS-BIND-DEFAULT = 0;
define constant $G-SETTINGS-BIND-GET = 1;
define constant $G-SETTINGS-BIND-SET = 2;
define constant $G-SETTINGS-BIND-NO-SENSITIVITY = 4;
define constant $G-SETTINGS-BIND-GET-NO-CHANGES = 8;
define constant $G-SETTINGS-BIND-INVERT-BOOLEAN = 16;
define constant <GSettingsBindFlags> = <C-int>;
define C-pointer-type <GSettingsBindFlags*> => <GSettingsBindFlags>;

define C-struct <_GSettingsClass>
  constant slot gsettingsclass-parent-class :: <GObjectClass>;
  constant slot gsettingsclass-writable-changed :: <C-function-pointer>;
  constant slot gsettingsclass-changed :: <C-function-pointer>;
  constant slot gsettingsclass-writable-change-event :: <C-function-pointer>;
  constant slot gsettingsclass-change-event :: <C-function-pointer>;
  constant slot gsettingsclass-padding :: <C-void*>;
  pointer-type-name: <GSettingsClass>;
end C-struct;

define C-struct <_GSettingsPrivate>
  pointer-type-name: <GSettingsPrivate>;
end C-struct;

define C-struct <_GSettingsSchema>
  pointer-type-name: <GSettingsSchema>;
end C-struct;

define C-function g-settings-schema-get-id
  input parameter self :: <GSettingsSchema>;
  result res :: <C-string>;
  c-name: "g_settings_schema_get_id";
end;

define C-function g-settings-schema-get-path
  input parameter self :: <GSettingsSchema>;
  result res :: <C-string>;
  c-name: "g_settings_schema_get_path";
end;

define C-function g-settings-schema-ref
  input parameter self :: <GSettingsSchema>;
  result res :: <GSettingsSchema>;
  c-name: "g_settings_schema_ref";
end;

define C-function g-settings-schema-unref
  input parameter self :: <GSettingsSchema>;
  c-name: "g_settings_schema_unref";
end;

define C-struct <_GSettingsSchemaSource>
  pointer-type-name: <GSettingsSchemaSource>;
end C-struct;

define C-function g-settings-schema-source-new-from-directory
  input parameter directory_ :: <C-string>;
  input parameter parent_ :: <GSettingsSchemaSource>;
  input parameter trusted_ :: <C-boolean>;
  result res :: <GSettingsSchemaSource>;
  c-name: "g_settings_schema_source_new_from_directory";
end;

define C-function g-settings-schema-source-lookup
  input parameter self :: <GSettingsSchemaSource>;
  input parameter schema_id_ :: <C-string>;
  input parameter recursive_ :: <C-boolean>;
  result res :: <GSettingsSchema>;
  c-name: "g_settings_schema_source_lookup";
end;

define C-function g-settings-schema-source-ref
  input parameter self :: <GSettingsSchemaSource>;
  result res :: <GSettingsSchemaSource>;
  c-name: "g_settings_schema_source_ref";
end;

define C-function g-settings-schema-source-unref
  input parameter self :: <GSettingsSchemaSource>;
  c-name: "g_settings_schema_source_unref";
end;

define C-function g-settings-schema-source-get-default
  result res :: <GSettingsSchemaSource>;
  c-name: "g_settings_schema_source_get_default";
end;

define open C-subtype <GSimpleAction> (<GObject>)
end C-subtype;

define C-pointer-type <GSimpleAction*> => <GSimpleAction>;

define property-getter simpleaction-enabled :: <C-boolean> on <GSimpleAction> end;
define property-setter simpleaction-enabled :: <C-boolean> on <GSimpleAction> end;
define property-getter simpleaction-name :: <C-string> on <GSimpleAction> end;
define property-setter simpleaction-name :: <C-string> on <GSimpleAction> end;
define property-getter simpleaction-parameter-type :: <GVariantType> on <GSimpleAction> end;
define property-setter simpleaction-parameter-type :: <GVariantType> on <GSimpleAction> end;
define property-getter simpleaction-state :: <GVariant> on <GSimpleAction> end;
define property-setter simpleaction-state :: <GVariant> on <GSimpleAction> end;
define property-getter simpleaction-state-type :: <GVariantType> on <GSimpleAction> end;
define C-function g-simple-action-new
  input parameter name_ :: <C-string>;
  input parameter parameter_type_ :: <GVariantType>;
  result res :: <GSimpleAction>;
  c-name: "g_simple_action_new";
end;

define C-function g-simple-action-new-stateful
  input parameter name_ :: <C-string>;
  input parameter parameter_type_ :: <GVariantType>;
  input parameter state_ :: <GVariant>;
  result res :: <GSimpleAction>;
  c-name: "g_simple_action_new_stateful";
end;

define C-function g-simple-action-set-enabled
  input parameter self :: <GSimpleAction>;
  input parameter enabled_ :: <C-boolean>;
  c-name: "g_simple_action_set_enabled";
end;

define C-function g-simple-action-set-state
  input parameter self :: <GSimpleAction>;
  input parameter value_ :: <GVariant>;
  c-name: "g_simple_action_set_state";
end;

define open C-subtype <GSimpleActionGroup> (<GObject>)
  constant slot gsimpleactiongroup-parent-instance :: <GObject>;
  constant slot gsimpleactiongroup-priv :: <GSimpleActionGroupPrivate>;
end C-subtype;

define C-pointer-type <GSimpleActionGroup*> => <GSimpleActionGroup>;

define C-function g-simple-action-group-new
  result res :: <GSimpleActionGroup>;
  c-name: "g_simple_action_group_new";
end;

define C-function g-simple-action-group-add-entries
  input parameter self :: <GSimpleActionGroup>;
  input parameter entries_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_entries_ :: <C-signed-int>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_simple_action_group_add_entries";
end;

define C-function g-simple-action-group-insert
  input parameter self :: <GSimpleActionGroup>;
  input parameter action_ :: <GAction>;
  c-name: "g_simple_action_group_insert";
end;

define C-function g-simple-action-group-lookup
  input parameter self :: <GSimpleActionGroup>;
  input parameter action_name_ :: <C-string>;
  result res :: <GAction>;
  c-name: "g_simple_action_group_lookup";
end;

define C-function g-simple-action-group-remove
  input parameter self :: <GSimpleActionGroup>;
  input parameter action_name_ :: <C-string>;
  c-name: "g_simple_action_group_remove";
end;

define C-struct <_GSimpleActionGroupClass>
  constant slot gsimpleactiongroupclass-parent-class :: <GObjectClass>;
  constant slot gsimpleactiongroupclass-padding :: <C-void*>;
  pointer-type-name: <GSimpleActionGroupClass>;
end C-struct;

define C-struct <_GSimpleActionGroupPrivate>
  pointer-type-name: <GSimpleActionGroupPrivate>;
end C-struct;

define open C-subtype <GSimpleAsyncResult> (<GObject>)
end C-subtype;

define C-pointer-type <GSimpleAsyncResult*> => <GSimpleAsyncResult>;

define C-function g-simple-async-result-new
  input parameter source_object_ :: <GObject>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter source_tag_ :: <C-void*>;
  result res :: <GSimpleAsyncResult>;
  c-name: "g_simple_async_result_new";
end;

define C-function g-simple-async-result-new-from-error
  input parameter source_object_ :: <GObject>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter error_ :: <GError>;
  result res :: <GSimpleAsyncResult>;
  c-name: "g_simple_async_result_new_from_error";
end;

define C-function g-simple-async-result-is-valid
  input parameter result_ :: <GAsyncResult>;
  input parameter source_ :: <GObject>;
  input parameter source_tag_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_simple_async_result_is_valid";
end;

define C-function g-simple-async-result-complete
  input parameter self :: <GSimpleAsyncResult>;
  c-name: "g_simple_async_result_complete";
end;

define C-function g-simple-async-result-complete-in-idle
  input parameter self :: <GSimpleAsyncResult>;
  c-name: "g_simple_async_result_complete_in_idle";
end;

define C-function g-simple-async-result-get-op-res-gboolean
  input parameter self :: <GSimpleAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_simple_async_result_get_op_res_gboolean";
end;

define C-function g-simple-async-result-get-op-res-gssize
  input parameter self :: <GSimpleAsyncResult>;
  result res :: <C-signed-long>;
  c-name: "g_simple_async_result_get_op_res_gssize";
end;

define C-function g-simple-async-result-propagate-error
  input parameter self :: <GSimpleAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_simple_async_result_propagate_error";
end;

define C-function g-simple-async-result-set-check-cancellable
  input parameter self :: <GSimpleAsyncResult>;
  input parameter check_cancellable_ :: <GCancellable>;
  c-name: "g_simple_async_result_set_check_cancellable";
end;

define C-function g-simple-async-result-set-from-error
  input parameter self :: <GSimpleAsyncResult>;
  input parameter error_ :: <GError>;
  c-name: "g_simple_async_result_set_from_error";
end;

define C-function g-simple-async-result-set-handle-cancellation
  input parameter self :: <GSimpleAsyncResult>;
  input parameter handle_cancellation_ :: <C-boolean>;
  c-name: "g_simple_async_result_set_handle_cancellation";
end;

define C-function g-simple-async-result-set-op-res-gboolean
  input parameter self :: <GSimpleAsyncResult>;
  input parameter op_res_ :: <C-boolean>;
  c-name: "g_simple_async_result_set_op_res_gboolean";
end;

define C-function g-simple-async-result-set-op-res-gssize
  input parameter self :: <GSimpleAsyncResult>;
  input parameter op_res_ :: <C-signed-long>;
  c-name: "g_simple_async_result_set_op_res_gssize";
end;

define C-struct <_GSimpleAsyncResultClass>
  pointer-type-name: <GSimpleAsyncResultClass>;
end C-struct;

define open C-subtype <GSimplePermission> (<GPermission>)
end C-subtype;

define C-pointer-type <GSimplePermission*> => <GSimplePermission>;

define C-function g-simple-permission-new
  input parameter allowed_ :: <C-boolean>;
  result res :: <GPermission>;
  c-name: "g_simple_permission_new";
end;

define open C-subtype <GSimpleProxyResolver> (<GObject>)
  constant slot gsimpleproxyresolver-parent-instance :: <GObject>;
  constant slot gsimpleproxyresolver-priv :: <GSimpleProxyResolverPrivate>;
end C-subtype;

define C-pointer-type <GSimpleProxyResolver*> => <GSimpleProxyResolver>;

define property-getter simpleproxyresolver-default-proxy :: <C-string> on <GSimpleProxyResolver> end;
define property-setter simpleproxyresolver-default-proxy :: <C-string> on <GSimpleProxyResolver> end;
define property-getter simpleproxyresolver-ignore-hosts :: <C-string*> on <GSimpleProxyResolver> end;
define property-setter simpleproxyresolver-ignore-hosts :: <C-string*> on <GSimpleProxyResolver> end;
define C-function g-simple-proxy-resolver-set-default-proxy
  input parameter self :: <GSimpleProxyResolver>;
  input parameter default_proxy_ :: <C-string>;
  c-name: "g_simple_proxy_resolver_set_default_proxy";
end;

define C-function g-simple-proxy-resolver-set-ignore-hosts
  input parameter self :: <GSimpleProxyResolver>;
  input parameter ignore_hosts_ :: <C-string>;
  c-name: "g_simple_proxy_resolver_set_ignore_hosts";
end;

define C-function g-simple-proxy-resolver-set-uri-proxy
  input parameter self :: <GSimpleProxyResolver>;
  input parameter uri_scheme_ :: <C-string>;
  input parameter proxy_ :: <C-string>;
  c-name: "g_simple_proxy_resolver_set_uri_proxy";
end;

define C-struct <_GSimpleProxyResolverClass>
  constant slot gsimpleproxyresolverclass-parent-class :: <GObjectClass>;
  constant slot gsimpleproxyresolverclass--g-reserved1 :: <C-void*>;
  constant slot gsimpleproxyresolverclass--g-reserved2 :: <C-void*>;
  constant slot gsimpleproxyresolverclass--g-reserved3 :: <C-void*>;
  constant slot gsimpleproxyresolverclass--g-reserved4 :: <C-void*>;
  constant slot gsimpleproxyresolverclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GSimpleProxyResolverClass>;
end C-struct;

define C-struct <_GSimpleProxyResolverPrivate>
  pointer-type-name: <GSimpleProxyResolverPrivate>;
end C-struct;

define open C-subtype <GSocket> (<GObject>)
  constant slot gsocket-parent-instance :: <GObject>;
  constant slot gsocket-priv :: <GSocketPrivate>;
end C-subtype;

define C-pointer-type <GSocket*> => <GSocket>;

define property-getter socket-blocking :: <C-boolean> on <GSocket> end;
define property-setter socket-blocking :: <C-boolean> on <GSocket> end;
define property-getter socket-broadcast :: <C-boolean> on <GSocket> end;
define property-setter socket-broadcast :: <C-boolean> on <GSocket> end;
define property-getter socket-family :: <GSocketFamily> on <GSocket> end;
define property-setter socket-family :: <GSocketFamily> on <GSocket> end;
define property-getter socket-fd :: <C-signed-int> on <GSocket> end;
define property-setter socket-fd :: <C-signed-int> on <GSocket> end;
define property-getter socket-keepalive :: <C-boolean> on <GSocket> end;
define property-setter socket-keepalive :: <C-boolean> on <GSocket> end;
define property-getter socket-listen-backlog :: <C-signed-int> on <GSocket> end;
define property-setter socket-listen-backlog :: <C-signed-int> on <GSocket> end;
define property-getter socket-local-address :: <GSocketAddress> on <GSocket> end;
define property-getter socket-multicast-loopback :: <C-boolean> on <GSocket> end;
define property-setter socket-multicast-loopback :: <C-boolean> on <GSocket> end;
define property-getter socket-multicast-ttl :: <C-unsigned-int> on <GSocket> end;
define property-setter socket-multicast-ttl :: <C-unsigned-int> on <GSocket> end;
define property-getter socket-protocol :: <GSocketProtocol> on <GSocket> end;
define property-setter socket-protocol :: <GSocketProtocol> on <GSocket> end;
define property-getter socket-remote-address :: <GSocketAddress> on <GSocket> end;
define property-getter socket-timeout :: <C-unsigned-int> on <GSocket> end;
define property-setter socket-timeout :: <C-unsigned-int> on <GSocket> end;
define property-getter socket-ttl :: <C-unsigned-int> on <GSocket> end;
define property-setter socket-ttl :: <C-unsigned-int> on <GSocket> end;
define property-getter socket-type :: <GSocketType> on <GSocket> end;
define property-setter socket-type :: <GSocketType> on <GSocket> end;
define C-function g-socket-new
  input parameter family_ :: <GSocketFamily>;
  input parameter type_ :: <GSocketType>;
  input parameter protocol_ :: <GSocketProtocol>;
  result res :: <GSocket>;
  c-name: "g_socket_new";
end;

define C-function g-socket-new-from-fd
  input parameter fd_ :: <C-signed-int>;
  result res :: <GSocket>;
  c-name: "g_socket_new_from_fd";
end;

define C-function g-socket-accept
  input parameter self :: <GSocket>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocket>;
  c-name: "g_socket_accept";
end;

define C-function g-socket-bind
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter allow_reuse_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_socket_bind";
end;

define C-function g-socket-check-connect-result
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_check_connect_result";
end;

define C-function g-socket-close
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_close";
end;

define C-function g-socket-condition-check
  input parameter self :: <GSocket>;
  input parameter condition_ :: <GIOCondition>;
  result res :: <GIOCondition>;
  c-name: "g_socket_condition_check";
end;

define C-function g-socket-condition-timed-wait
  input parameter self :: <GSocket>;
  input parameter condition_ :: <GIOCondition>;
  input parameter timeout_ :: <C-signed-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_socket_condition_timed_wait";
end;

define C-function g-socket-condition-wait
  input parameter self :: <GSocket>;
  input parameter condition_ :: <GIOCondition>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_socket_condition_wait";
end;

define C-function g-socket-connect
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_socket_connect";
end;

define C-function g-socket-connection-factory-create-connection
  input parameter self :: <GSocket>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_connection_factory_create_connection";
end;

define C-function g-socket-get-available-bytes
  input parameter self :: <GSocket>;
  result res :: <C-signed-long>;
  c-name: "g_socket_get_available_bytes";
end;

define C-function g-socket-get-blocking
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_get_blocking";
end;

define C-function g-socket-get-broadcast
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_get_broadcast";
end;

define C-function g-socket-get-credentials
  input parameter self :: <GSocket>;
  result res :: <GCredentials>;
  c-name: "g_socket_get_credentials";
end;

define C-function g-socket-get-family
  input parameter self :: <GSocket>;
  result res :: <GSocketFamily>;
  c-name: "g_socket_get_family";
end;

define C-function g-socket-get-fd
  input parameter self :: <GSocket>;
  result res :: <C-signed-int>;
  c-name: "g_socket_get_fd";
end;

define C-function g-socket-get-keepalive
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_get_keepalive";
end;

define C-function g-socket-get-listen-backlog
  input parameter self :: <GSocket>;
  result res :: <C-signed-int>;
  c-name: "g_socket_get_listen_backlog";
end;

define C-function g-socket-get-local-address
  input parameter self :: <GSocket>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_get_local_address";
end;

define C-function g-socket-get-multicast-loopback
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_get_multicast_loopback";
end;

define C-function g-socket-get-multicast-ttl
  input parameter self :: <GSocket>;
  result res :: <C-unsigned-int>;
  c-name: "g_socket_get_multicast_ttl";
end;

define C-function g-socket-get-option
  input parameter self :: <GSocket>;
  input parameter level_ :: <C-signed-int>;
  input parameter optname_ :: <C-signed-int>;
  output parameter value_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "g_socket_get_option";
end;

define C-function g-socket-get-protocol
  input parameter self :: <GSocket>;
  result res :: <GSocketProtocol>;
  c-name: "g_socket_get_protocol";
end;

define C-function g-socket-get-remote-address
  input parameter self :: <GSocket>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_get_remote_address";
end;

define C-function g-socket-get-socket-type
  input parameter self :: <GSocket>;
  result res :: <GSocketType>;
  c-name: "g_socket_get_socket_type";
end;

define C-function g-socket-get-timeout
  input parameter self :: <GSocket>;
  result res :: <C-unsigned-int>;
  c-name: "g_socket_get_timeout";
end;

define C-function g-socket-get-ttl
  input parameter self :: <GSocket>;
  result res :: <C-unsigned-int>;
  c-name: "g_socket_get_ttl";
end;

define C-function g-socket-is-closed
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_is_closed";
end;

define C-function g-socket-is-connected
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_is_connected";
end;

define C-function g-socket-join-multicast-group
  input parameter self :: <GSocket>;
  input parameter group_ :: <GInetAddress>;
  input parameter source_specific_ :: <C-boolean>;
  input parameter iface_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_socket_join_multicast_group";
end;

define C-function g-socket-leave-multicast-group
  input parameter self :: <GSocket>;
  input parameter group_ :: <GInetAddress>;
  input parameter source_specific_ :: <C-boolean>;
  input parameter iface_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_socket_leave_multicast_group";
end;

define C-function g-socket-listen
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_listen";
end;

define C-function g-socket-receive
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive";
end;

define C-function g-socket-receive-from
  input parameter self :: <GSocket>;
  output parameter address_ :: <GSocketAddress*>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive_from";
end;

define C-function g-socket-receive-message
  input parameter self :: <GSocket>;
  output parameter address_ :: <GSocketAddress*>;
  input parameter vectors_ :: <C-unsigned-char*> /* Not supported */;
  input parameter num_vectors_ :: <C-signed-int>;
  input parameter messages_ :: <C-unsigned-char*> /* Not supported */;
  input parameter num_messages_ :: <C-signed-int*>;
  input parameter flags_ :: <C-signed-int*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive_message";
end;

define C-function g-socket-receive-with-blocking
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive_with_blocking";
end;

define C-function g-socket-send
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send";
end;

define C-function g-socket-send-message
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter vectors_ :: <C-unsigned-char*> /* Not supported */;
  input parameter num_vectors_ :: <C-signed-int>;
  input parameter messages_ :: <C-unsigned-char*> /* Not supported */;
  input parameter num_messages_ :: <C-signed-int>;
  input parameter flags_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send_message";
end;

define C-function g-socket-send-to
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send_to";
end;

define C-function g-socket-send-with-blocking
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send_with_blocking";
end;

define C-function g-socket-set-blocking
  input parameter self :: <GSocket>;
  input parameter blocking_ :: <C-boolean>;
  c-name: "g_socket_set_blocking";
end;

define C-function g-socket-set-broadcast
  input parameter self :: <GSocket>;
  input parameter broadcast_ :: <C-boolean>;
  c-name: "g_socket_set_broadcast";
end;

define C-function g-socket-set-keepalive
  input parameter self :: <GSocket>;
  input parameter keepalive_ :: <C-boolean>;
  c-name: "g_socket_set_keepalive";
end;

define C-function g-socket-set-listen-backlog
  input parameter self :: <GSocket>;
  input parameter backlog_ :: <C-signed-int>;
  c-name: "g_socket_set_listen_backlog";
end;

define C-function g-socket-set-multicast-loopback
  input parameter self :: <GSocket>;
  input parameter loopback_ :: <C-boolean>;
  c-name: "g_socket_set_multicast_loopback";
end;

define C-function g-socket-set-multicast-ttl
  input parameter self :: <GSocket>;
  input parameter ttl_ :: <C-unsigned-int>;
  c-name: "g_socket_set_multicast_ttl";
end;

define C-function g-socket-set-option
  input parameter self :: <GSocket>;
  input parameter level_ :: <C-signed-int>;
  input parameter optname_ :: <C-signed-int>;
  input parameter value_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_socket_set_option";
end;

define C-function g-socket-set-timeout
  input parameter self :: <GSocket>;
  input parameter timeout_ :: <C-unsigned-int>;
  c-name: "g_socket_set_timeout";
end;

define C-function g-socket-set-ttl
  input parameter self :: <GSocket>;
  input parameter ttl_ :: <C-unsigned-int>;
  c-name: "g_socket_set_ttl";
end;

define C-function g-socket-shutdown
  input parameter self :: <GSocket>;
  input parameter shutdown_read_ :: <C-boolean>;
  input parameter shutdown_write_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_socket_shutdown";
end;

define C-function g-socket-speaks-ipv4
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_speaks_ipv4";
end;

define open C-subtype <GSocketAddress> (<GObject>)
  constant slot gsocketaddress-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GSocketAddress*> => <GSocketAddress>;

define property-getter socketaddress-family :: <GSocketFamily> on <GSocketAddress> end;
define C-function g-socket-address-new-from-native
  input parameter native_ :: <C-void*>;
  input parameter len_ :: <C-unsigned-long>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_address_new_from_native";
end;

define C-function g-socket-address-get-family
  input parameter self :: <GSocketAddress>;
  result res :: <GSocketFamily>;
  c-name: "g_socket_address_get_family";
end;

define C-function g-socket-address-get-native-size
  input parameter self :: <GSocketAddress>;
  result res :: <C-signed-long>;
  c-name: "g_socket_address_get_native_size";
end;

define C-function g-socket-address-to-native
  input parameter self :: <GSocketAddress>;
  input parameter dest_ :: <C-void*>;
  input parameter destlen_ :: <C-unsigned-long>;
  result res :: <C-boolean>;
  c-name: "g_socket_address_to_native";
end;

define C-struct <_GSocketAddressClass>
  constant slot gsocketaddressclass-parent-class :: <GObjectClass>;
  constant slot gsocketaddressclass-get-family :: <C-function-pointer>;
  constant slot gsocketaddressclass-get-native-size :: <C-function-pointer>;
  constant slot gsocketaddressclass-to-native :: <C-function-pointer>;
  pointer-type-name: <GSocketAddressClass>;
end C-struct;

define open C-subtype <GSocketAddressEnumerator> (<GObject>)
  constant slot gsocketaddressenumerator-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GSocketAddressEnumerator*> => <GSocketAddressEnumerator>;

define C-function g-socket-address-enumerator-next
  input parameter self :: <GSocketAddressEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_address_enumerator_next";
end;

define C-function g-socket-address-enumerator-next-async
  input parameter self :: <GSocketAddressEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_address_enumerator_next_async";
end;

define C-function g-socket-address-enumerator-next-finish
  input parameter self :: <GSocketAddressEnumerator>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_address_enumerator_next_finish";
end;

define C-struct <_GSocketAddressEnumeratorClass>
  constant slot gsocketaddressenumeratorclass-parent-class :: <GObjectClass>;
  constant slot gsocketaddressenumeratorclass-next :: <C-function-pointer>;
  constant slot gsocketaddressenumeratorclass-next-async :: <C-function-pointer>;
  constant slot gsocketaddressenumeratorclass-next-finish :: <C-function-pointer>;
  pointer-type-name: <GSocketAddressEnumeratorClass>;
end C-struct;

define C-struct <_GSocketClass>
  constant slot gsocketclass-parent-class :: <GObjectClass>;
  constant slot gsocketclass--g-reserved1 :: <C-void*>;
  constant slot gsocketclass--g-reserved2 :: <C-void*>;
  constant slot gsocketclass--g-reserved3 :: <C-void*>;
  constant slot gsocketclass--g-reserved4 :: <C-void*>;
  constant slot gsocketclass--g-reserved5 :: <C-void*>;
  constant slot gsocketclass--g-reserved6 :: <C-void*>;
  constant slot gsocketclass--g-reserved7 :: <C-void*>;
  constant slot gsocketclass--g-reserved8 :: <C-void*>;
  constant slot gsocketclass--g-reserved9 :: <C-void*>;
  constant slot gsocketclass--g-reserved10 :: <C-void*>;
  pointer-type-name: <GSocketClass>;
end C-struct;

define open C-subtype <GSocketClient> (<GObject>)
  constant slot gsocketclient-parent-instance :: <GObject>;
  constant slot gsocketclient-priv :: <GSocketClientPrivate>;
end C-subtype;

define C-pointer-type <GSocketClient*> => <GSocketClient>;

define property-getter socketclient-enable-proxy :: <C-boolean> on <GSocketClient> end;
define property-setter socketclient-enable-proxy :: <C-boolean> on <GSocketClient> end;
define property-getter socketclient-family :: <GSocketFamily> on <GSocketClient> end;
define property-setter socketclient-family :: <GSocketFamily> on <GSocketClient> end;
define property-getter socketclient-local-address :: <GSocketAddress> on <GSocketClient> end;
define property-setter socketclient-local-address :: <GSocketAddress> on <GSocketClient> end;
define property-getter socketclient-protocol :: <GSocketProtocol> on <GSocketClient> end;
define property-setter socketclient-protocol :: <GSocketProtocol> on <GSocketClient> end;
define property-getter socketclient-proxy-resolver :: <GProxyResolver> on <GSocketClient> end;
define property-setter socketclient-proxy-resolver :: <GProxyResolver> on <GSocketClient> end;
define property-getter socketclient-timeout :: <C-unsigned-int> on <GSocketClient> end;
define property-setter socketclient-timeout :: <C-unsigned-int> on <GSocketClient> end;
define property-getter socketclient-tls :: <C-boolean> on <GSocketClient> end;
define property-setter socketclient-tls :: <C-boolean> on <GSocketClient> end;
define property-getter socketclient-tls-validation-flags :: <GTlsCertificateFlags> on <GSocketClient> end;
define property-setter socketclient-tls-validation-flags :: <GTlsCertificateFlags> on <GSocketClient> end;
define property-getter socketclient-type :: <GSocketType> on <GSocketClient> end;
define property-setter socketclient-type :: <GSocketType> on <GSocketClient> end;
define C-function g-socket-client-new
  result res :: <GSocketClient>;
  c-name: "g_socket_client_new";
end;

define C-function g-socket-client-add-application-proxy
  input parameter self :: <GSocketClient>;
  input parameter protocol_ :: <C-string>;
  c-name: "g_socket_client_add_application_proxy";
end;

define C-function g-socket-client-connect
  input parameter self :: <GSocketClient>;
  input parameter connectable_ :: <GSocketConnectable>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect";
end;

define C-function g-socket-client-connect-async
  input parameter self :: <GSocketClient>;
  input parameter connectable_ :: <GSocketConnectable>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_client_connect_async";
end;

define C-function g-socket-client-connect-finish
  input parameter self :: <GSocketClient>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_finish";
end;

define C-function g-socket-client-connect-to-host
  input parameter self :: <GSocketClient>;
  input parameter host_and_port_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_host";
end;

define C-function g-socket-client-connect-to-host-async
  input parameter self :: <GSocketClient>;
  input parameter host_and_port_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_client_connect_to_host_async";
end;

define C-function g-socket-client-connect-to-host-finish
  input parameter self :: <GSocketClient>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_host_finish";
end;

define C-function g-socket-client-connect-to-service
  input parameter self :: <GSocketClient>;
  input parameter domain_ :: <C-string>;
  input parameter service_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_service";
end;

define C-function g-socket-client-connect-to-service-async
  input parameter self :: <GSocketClient>;
  input parameter domain_ :: <C-string>;
  input parameter service_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_client_connect_to_service_async";
end;

define C-function g-socket-client-connect-to-service-finish
  input parameter self :: <GSocketClient>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_service_finish";
end;

define C-function g-socket-client-connect-to-uri
  input parameter self :: <GSocketClient>;
  input parameter uri_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_uri";
end;

define C-function g-socket-client-connect-to-uri-async
  input parameter self :: <GSocketClient>;
  input parameter uri_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_client_connect_to_uri_async";
end;

define C-function g-socket-client-connect-to-uri-finish
  input parameter self :: <GSocketClient>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_uri_finish";
end;

define C-function g-socket-client-get-enable-proxy
  input parameter self :: <GSocketClient>;
  result res :: <C-boolean>;
  c-name: "g_socket_client_get_enable_proxy";
end;

define C-function g-socket-client-get-family
  input parameter self :: <GSocketClient>;
  result res :: <GSocketFamily>;
  c-name: "g_socket_client_get_family";
end;

define C-function g-socket-client-get-local-address
  input parameter self :: <GSocketClient>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_client_get_local_address";
end;

define C-function g-socket-client-get-protocol
  input parameter self :: <GSocketClient>;
  result res :: <GSocketProtocol>;
  c-name: "g_socket_client_get_protocol";
end;

define C-function g-socket-client-get-proxy-resolver
  input parameter self :: <GSocketClient>;
  result res :: <GProxyResolver>;
  c-name: "g_socket_client_get_proxy_resolver";
end;

define C-function g-socket-client-get-socket-type
  input parameter self :: <GSocketClient>;
  result res :: <GSocketType>;
  c-name: "g_socket_client_get_socket_type";
end;

define C-function g-socket-client-get-timeout
  input parameter self :: <GSocketClient>;
  result res :: <C-unsigned-int>;
  c-name: "g_socket_client_get_timeout";
end;

define C-function g-socket-client-get-tls
  input parameter self :: <GSocketClient>;
  result res :: <C-boolean>;
  c-name: "g_socket_client_get_tls";
end;

define C-function g-socket-client-get-tls-validation-flags
  input parameter self :: <GSocketClient>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_socket_client_get_tls_validation_flags";
end;

define C-function g-socket-client-set-enable-proxy
  input parameter self :: <GSocketClient>;
  input parameter enable_ :: <C-boolean>;
  c-name: "g_socket_client_set_enable_proxy";
end;

define C-function g-socket-client-set-family
  input parameter self :: <GSocketClient>;
  input parameter family_ :: <GSocketFamily>;
  c-name: "g_socket_client_set_family";
end;

define C-function g-socket-client-set-local-address
  input parameter self :: <GSocketClient>;
  input parameter address_ :: <GSocketAddress>;
  c-name: "g_socket_client_set_local_address";
end;

define C-function g-socket-client-set-protocol
  input parameter self :: <GSocketClient>;
  input parameter protocol_ :: <GSocketProtocol>;
  c-name: "g_socket_client_set_protocol";
end;

define C-function g-socket-client-set-proxy-resolver
  input parameter self :: <GSocketClient>;
  input parameter proxy_resolver_ :: <GProxyResolver>;
  c-name: "g_socket_client_set_proxy_resolver";
end;

define C-function g-socket-client-set-socket-type
  input parameter self :: <GSocketClient>;
  input parameter type_ :: <GSocketType>;
  c-name: "g_socket_client_set_socket_type";
end;

define C-function g-socket-client-set-timeout
  input parameter self :: <GSocketClient>;
  input parameter timeout_ :: <C-unsigned-int>;
  c-name: "g_socket_client_set_timeout";
end;

define C-function g-socket-client-set-tls
  input parameter self :: <GSocketClient>;
  input parameter tls_ :: <C-boolean>;
  c-name: "g_socket_client_set_tls";
end;

define C-function g-socket-client-set-tls-validation-flags
  input parameter self :: <GSocketClient>;
  input parameter flags_ :: <GTlsCertificateFlags>;
  c-name: "g_socket_client_set_tls_validation_flags";
end;

define C-struct <_GSocketClientClass>
  constant slot gsocketclientclass-parent-class :: <GObjectClass>;
  constant slot gsocketclientclass-event :: <C-function-pointer>;
  constant slot gsocketclientclass--g-reserved1 :: <C-void*>;
  constant slot gsocketclientclass--g-reserved2 :: <C-void*>;
  constant slot gsocketclientclass--g-reserved3 :: <C-void*>;
  constant slot gsocketclientclass--g-reserved4 :: <C-void*>;
  pointer-type-name: <GSocketClientClass>;
end C-struct;

define constant $G-SOCKET-CLIENT-RESOLVING = 0;
define constant $G-SOCKET-CLIENT-RESOLVED = 1;
define constant $G-SOCKET-CLIENT-CONNECTING = 2;
define constant $G-SOCKET-CLIENT-CONNECTED = 3;
define constant $G-SOCKET-CLIENT-PROXY-NEGOTIATING = 4;
define constant $G-SOCKET-CLIENT-PROXY-NEGOTIATED = 5;
define constant $G-SOCKET-CLIENT-TLS-HANDSHAKING = 6;
define constant $G-SOCKET-CLIENT-TLS-HANDSHAKED = 7;
define constant $G-SOCKET-CLIENT-COMPLETE = 8;
define constant <GSocketClientEvent> = <C-int>;
define C-pointer-type <GSocketClientEvent*> => <GSocketClientEvent>;

define C-struct <_GSocketClientPrivate>
  pointer-type-name: <GSocketClientPrivate>;
end C-struct;

// Interface
define open C-subtype <GSocketConnectable> (<C-void*>)
end C-subtype;

define C-pointer-type <GSocketConnectable*> => <GSocketConnectable>;

define C-function g-socket-connectable-enumerate
  input parameter self :: <GSocketConnectable>;
  result res :: <GSocketAddressEnumerator>;
  c-name: "g_socket_connectable_enumerate";
end;

define C-function g-socket-connectable-proxy-enumerate
  input parameter self :: <GSocketConnectable>;
  result res :: <GSocketAddressEnumerator>;
  c-name: "g_socket_connectable_proxy_enumerate";
end;

define C-struct <_GSocketConnectableIface>
  constant slot gsocketconnectableiface-g-iface :: <GTypeInterface>;
  constant slot gsocketconnectableiface-enumerate :: <C-function-pointer>;
  constant slot gsocketconnectableiface-proxy-enumerate :: <C-function-pointer>;
  pointer-type-name: <GSocketConnectableIface>;
end C-struct;

define open C-subtype <GSocketConnection> (<GIOStream>)
  constant slot gsocketconnection-parent-instance :: <GIOStream>;
  constant slot gsocketconnection-priv :: <GSocketConnectionPrivate>;
end C-subtype;

define C-pointer-type <GSocketConnection*> => <GSocketConnection>;

define property-getter socketconnection-socket :: <GSocket> on <GSocketConnection> end;
define property-setter socketconnection-socket :: <GSocket> on <GSocketConnection> end;
define C-function g-socket-connection-factory-lookup-type
  input parameter family_ :: <GSocketFamily>;
  input parameter type_ :: <GSocketType>;
  input parameter protocol_id_ :: <C-signed-int>;
  result res :: <C-long>;
  c-name: "g_socket_connection_factory_lookup_type";
end;

define C-function g-socket-connection-factory-register-type
  input parameter g_type_ :: <C-long>;
  input parameter family_ :: <GSocketFamily>;
  input parameter type_ :: <GSocketType>;
  input parameter protocol_ :: <C-signed-int>;
  c-name: "g_socket_connection_factory_register_type";
end;

define C-function g-socket-connection-connect
  input parameter self :: <GSocketConnection>;
  input parameter address_ :: <GSocketAddress>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_socket_connection_connect";
end;

define C-function g-socket-connection-connect-async
  input parameter self :: <GSocketConnection>;
  input parameter address_ :: <GSocketAddress>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_connection_connect_async";
end;

define C-function g-socket-connection-connect-finish
  input parameter self :: <GSocketConnection>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_socket_connection_connect_finish";
end;

define C-function g-socket-connection-get-local-address
  input parameter self :: <GSocketConnection>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_connection_get_local_address";
end;

define C-function g-socket-connection-get-remote-address
  input parameter self :: <GSocketConnection>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_connection_get_remote_address";
end;

define C-function g-socket-connection-get-socket
  input parameter self :: <GSocketConnection>;
  result res :: <GSocket>;
  c-name: "g_socket_connection_get_socket";
end;

define C-function g-socket-connection-is-connected
  input parameter self :: <GSocketConnection>;
  result res :: <C-boolean>;
  c-name: "g_socket_connection_is_connected";
end;

define C-struct <_GSocketConnectionClass>
  constant slot gsocketconnectionclass-parent-class :: <GIOStreamClass>;
  constant slot gsocketconnectionclass--g-reserved1 :: <C-void*>;
  constant slot gsocketconnectionclass--g-reserved2 :: <C-void*>;
  constant slot gsocketconnectionclass--g-reserved3 :: <C-void*>;
  constant slot gsocketconnectionclass--g-reserved4 :: <C-void*>;
  constant slot gsocketconnectionclass--g-reserved5 :: <C-void*>;
  constant slot gsocketconnectionclass--g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketConnectionClass>;
end C-struct;

define C-struct <_GSocketConnectionPrivate>
  pointer-type-name: <GSocketConnectionPrivate>;
end C-struct;

define open C-subtype <GSocketControlMessage> (<GObject>)
  constant slot gsocketcontrolmessage-parent-instance :: <GObject>;
  constant slot gsocketcontrolmessage-priv :: <GSocketControlMessagePrivate>;
end C-subtype;

define C-pointer-type <GSocketControlMessage*> => <GSocketControlMessage>;

define C-function g-socket-control-message-deserialize
  input parameter level_ :: <C-signed-int>;
  input parameter type_ :: <C-signed-int>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter data_ :: <C-unsigned-char*>;
  result res :: <GSocketControlMessage>;
  c-name: "g_socket_control_message_deserialize";
end;

define C-function g-socket-control-message-get-level
  input parameter self :: <GSocketControlMessage>;
  result res :: <C-signed-int>;
  c-name: "g_socket_control_message_get_level";
end;

define C-function g-socket-control-message-get-msg-type
  input parameter self :: <GSocketControlMessage>;
  result res :: <C-signed-int>;
  c-name: "g_socket_control_message_get_msg_type";
end;

define C-function g-socket-control-message-get-size
  input parameter self :: <GSocketControlMessage>;
  result res :: <C-unsigned-long>;
  c-name: "g_socket_control_message_get_size";
end;

define C-function g-socket-control-message-serialize
  input parameter self :: <GSocketControlMessage>;
  input parameter data_ :: <C-void*>;
  c-name: "g_socket_control_message_serialize";
end;

define C-struct <_GSocketControlMessageClass>
  constant slot gsocketcontrolmessageclass-parent-class :: <GObjectClass>;
  constant slot gsocketcontrolmessageclass-get-size :: <C-function-pointer>;
  constant slot gsocketcontrolmessageclass-get-level :: <C-function-pointer>;
  constant slot gsocketcontrolmessageclass-get-type :: <C-function-pointer>;
  constant slot gsocketcontrolmessageclass-serialize :: <C-function-pointer>;
  constant slot gsocketcontrolmessageclass-deserialize :: <C-void*>;
  constant slot gsocketcontrolmessageclass--g-reserved1 :: <C-void*>;
  constant slot gsocketcontrolmessageclass--g-reserved2 :: <C-void*>;
  constant slot gsocketcontrolmessageclass--g-reserved3 :: <C-void*>;
  constant slot gsocketcontrolmessageclass--g-reserved4 :: <C-void*>;
  constant slot gsocketcontrolmessageclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GSocketControlMessageClass>;
end C-struct;

define C-struct <_GSocketControlMessagePrivate>
  pointer-type-name: <GSocketControlMessagePrivate>;
end C-struct;

define constant $G-SOCKET-FAMILY-INVALID = 0;
define constant $G-SOCKET-FAMILY-UNIX = 1;
define constant $G-SOCKET-FAMILY-IPV4 = 2;
define constant $G-SOCKET-FAMILY-IPV6 = 10;
define constant <GSocketFamily> = <C-int>;
define C-pointer-type <GSocketFamily*> => <GSocketFamily>;

define open C-subtype <GSocketListener> (<GObject>)
  constant slot gsocketlistener-parent-instance :: <GObject>;
  constant slot gsocketlistener-priv :: <GSocketListenerPrivate>;
end C-subtype;

define C-pointer-type <GSocketListener*> => <GSocketListener>;

define property-getter socketlistener-listen-backlog :: <C-signed-int> on <GSocketListener> end;
define property-setter socketlistener-listen-backlog :: <C-signed-int> on <GSocketListener> end;
define C-function g-socket-listener-new
  result res :: <GSocketListener>;
  c-name: "g_socket_listener_new";
end;

define C-function g-socket-listener-accept
  input parameter self :: <GSocketListener>;
  output parameter source_object_ :: <GObject*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_listener_accept";
end;

define C-function g-socket-listener-accept-async
  input parameter self :: <GSocketListener>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_listener_accept_async";
end;

define C-function g-socket-listener-accept-finish
  input parameter self :: <GSocketListener>;
  input parameter result_ :: <GAsyncResult>;
  output parameter source_object_ :: <GObject*>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_listener_accept_finish";
end;

define C-function g-socket-listener-accept-socket
  input parameter self :: <GSocketListener>;
  output parameter source_object_ :: <GObject*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSocket>;
  c-name: "g_socket_listener_accept_socket";
end;

define C-function g-socket-listener-accept-socket-async
  input parameter self :: <GSocketListener>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_socket_listener_accept_socket_async";
end;

define C-function g-socket-listener-accept-socket-finish
  input parameter self :: <GSocketListener>;
  input parameter result_ :: <GAsyncResult>;
  output parameter source_object_ :: <GObject*>;
  result res :: <GSocket>;
  c-name: "g_socket_listener_accept_socket_finish";
end;

define C-function g-socket-listener-add-address
  input parameter self :: <GSocketListener>;
  input parameter address_ :: <GSocketAddress>;
  input parameter type_ :: <GSocketType>;
  input parameter protocol_ :: <GSocketProtocol>;
  input parameter source_object_ :: <GObject>;
  output parameter effective_address_ :: <GSocketAddress*>;
  result res :: <C-boolean>;
  c-name: "g_socket_listener_add_address";
end;

define C-function g-socket-listener-add-any-inet-port
  input parameter self :: <GSocketListener>;
  input parameter source_object_ :: <GObject>;
  result res :: <C-unsigned-short>;
  c-name: "g_socket_listener_add_any_inet_port";
end;

define C-function g-socket-listener-add-inet-port
  input parameter self :: <GSocketListener>;
  input parameter port_ :: <C-unsigned-short>;
  input parameter source_object_ :: <GObject>;
  result res :: <C-boolean>;
  c-name: "g_socket_listener_add_inet_port";
end;

define C-function g-socket-listener-add-socket
  input parameter self :: <GSocketListener>;
  input parameter socket_ :: <GSocket>;
  input parameter source_object_ :: <GObject>;
  result res :: <C-boolean>;
  c-name: "g_socket_listener_add_socket";
end;

define C-function g-socket-listener-close
  input parameter self :: <GSocketListener>;
  c-name: "g_socket_listener_close";
end;

define C-function g-socket-listener-set-backlog
  input parameter self :: <GSocketListener>;
  input parameter listen_backlog_ :: <C-signed-int>;
  c-name: "g_socket_listener_set_backlog";
end;

define C-struct <_GSocketListenerClass>
  constant slot gsocketlistenerclass-parent-class :: <GObjectClass>;
  constant slot gsocketlistenerclass-changed :: <C-function-pointer>;
  constant slot gsocketlistenerclass--g-reserved1 :: <C-void*>;
  constant slot gsocketlistenerclass--g-reserved2 :: <C-void*>;
  constant slot gsocketlistenerclass--g-reserved3 :: <C-void*>;
  constant slot gsocketlistenerclass--g-reserved4 :: <C-void*>;
  constant slot gsocketlistenerclass--g-reserved5 :: <C-void*>;
  constant slot gsocketlistenerclass--g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketListenerClass>;
end C-struct;

define C-struct <_GSocketListenerPrivate>
  pointer-type-name: <GSocketListenerPrivate>;
end C-struct;

define constant $G-SOCKET-MSG-NONE = 0;
define constant $G-SOCKET-MSG-OOB = 1;
define constant $G-SOCKET-MSG-PEEK = 2;
define constant $G-SOCKET-MSG-DONTROUTE = 4;
define constant <GSocketMsgFlags> = <C-int>;
define C-pointer-type <GSocketMsgFlags*> => <GSocketMsgFlags>;

define C-struct <_GSocketPrivate>
  pointer-type-name: <GSocketPrivate>;
end C-struct;

define constant $G-SOCKET-PROTOCOL-UNKNOWN = -1;
define constant $G-SOCKET-PROTOCOL-DEFAULT = 0;
define constant $G-SOCKET-PROTOCOL-TCP = 6;
define constant $G-SOCKET-PROTOCOL-UDP = 17;
define constant $G-SOCKET-PROTOCOL-SCTP = 132;
define constant <GSocketProtocol> = <C-int>;
define C-pointer-type <GSocketProtocol*> => <GSocketProtocol>;

define open C-subtype <GSocketService> (<GSocketListener>)
  constant slot gsocketservice-parent-instance :: <GSocketListener>;
  constant slot gsocketservice-priv :: <GSocketServicePrivate>;
end C-subtype;

define C-pointer-type <GSocketService*> => <GSocketService>;

define C-function g-socket-service-new
  result res :: <GSocketService>;
  c-name: "g_socket_service_new";
end;

define C-function g-socket-service-is-active
  input parameter self :: <GSocketService>;
  result res :: <C-boolean>;
  c-name: "g_socket_service_is_active";
end;

define C-function g-socket-service-start
  input parameter self :: <GSocketService>;
  c-name: "g_socket_service_start";
end;

define C-function g-socket-service-stop
  input parameter self :: <GSocketService>;
  c-name: "g_socket_service_stop";
end;

define C-struct <_GSocketServiceClass>
  constant slot gsocketserviceclass-parent-class :: <GSocketListenerClass>;
  constant slot gsocketserviceclass-incoming :: <C-function-pointer>;
  constant slot gsocketserviceclass--g-reserved1 :: <C-void*>;
  constant slot gsocketserviceclass--g-reserved2 :: <C-void*>;
  constant slot gsocketserviceclass--g-reserved3 :: <C-void*>;
  constant slot gsocketserviceclass--g-reserved4 :: <C-void*>;
  constant slot gsocketserviceclass--g-reserved5 :: <C-void*>;
  constant slot gsocketserviceclass--g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketServiceClass>;
end C-struct;

define C-struct <_GSocketServicePrivate>
  pointer-type-name: <GSocketServicePrivate>;
end C-struct;

define constant $G-SOCKET-TYPE-INVALID = 0;
define constant $G-SOCKET-TYPE-STREAM = 1;
define constant $G-SOCKET-TYPE-DATAGRAM = 2;
define constant $G-SOCKET-TYPE-SEQPACKET = 3;
define constant <GSocketType> = <C-int>;
define C-pointer-type <GSocketType*> => <GSocketType>;

define C-struct <_GSrvTarget>
  pointer-type-name: <GSrvTarget>;
end C-struct;

define C-function g-srv-target-new
  input parameter hostname_ :: <C-string>;
  input parameter port_ :: <C-unsigned-short>;
  input parameter priority_ :: <C-unsigned-short>;
  input parameter weight_ :: <C-unsigned-short>;
  result res :: <GSrvTarget>;
  c-name: "g_srv_target_new";
end;

define C-function g-srv-target-copy
  input parameter self :: <GSrvTarget>;
  result res :: <GSrvTarget>;
  c-name: "g_srv_target_copy";
end;

define C-function g-srv-target-free
  input parameter self :: <GSrvTarget>;
  c-name: "g_srv_target_free";
end;

define C-function g-srv-target-get-hostname
  input parameter self :: <GSrvTarget>;
  result res :: <C-string>;
  c-name: "g_srv_target_get_hostname";
end;

define C-function g-srv-target-get-port
  input parameter self :: <GSrvTarget>;
  result res :: <C-unsigned-short>;
  c-name: "g_srv_target_get_port";
end;

define C-function g-srv-target-get-priority
  input parameter self :: <GSrvTarget>;
  result res :: <C-unsigned-short>;
  c-name: "g_srv_target_get_priority";
end;

define C-function g-srv-target-get-weight
  input parameter self :: <GSrvTarget>;
  result res :: <C-unsigned-short>;
  c-name: "g_srv_target_get_weight";
end;

define C-struct <_GStaticResource>
  slot gstaticresource-data :: <C-unsigned-char*>;
  slot gstaticresource-data-len :: <C-unsigned-long>;
  slot gstaticresource-resource :: <GResource>;
  slot gstaticresource-next :: <GStaticResource>;
  slot gstaticresource-padding :: <C-void*>;
  pointer-type-name: <GStaticResource>;
end C-struct;

define C-function g-static-resource-fini
  input parameter self :: <GStaticResource>;
  c-name: "g_static_resource_fini";
end;

define C-function g-static-resource-get-resource
  input parameter self :: <GStaticResource>;
  result res :: <GResource>;
  c-name: "g_static_resource_get_resource";
end;

define C-function g-static-resource-init
  input parameter self :: <GStaticResource>;
  c-name: "g_static_resource_init";
end;

define constant $TLS-BACKEND-EXTENSION-POINT-NAME = "gio-tls-backend";

define constant $TLS-DATABASE-PURPOSE-AUTHENTICATE-CLIENT = "1.3.6.1.5.5.7.3.2";

define constant $TLS-DATABASE-PURPOSE-AUTHENTICATE-SERVER = "1.3.6.1.5.5.7.3.1";

define open C-subtype <GTask> (<GObject>)
end C-subtype;

define C-pointer-type <GTask*> => <GTask>;

define C-function g-task-new
  input parameter source_object_ :: <GObject>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  result res :: <GTask>;
  c-name: "g_task_new";
end;

define C-function g-task-is-valid
  input parameter result_ :: <GAsyncResult>;
  input parameter source_object_ :: <GObject>;
  result res :: <C-boolean>;
  c-name: "g_task_is_valid";
end;

define C-function g-task-report-error
  input parameter source_object_ :: <GObject>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter callback_data_ :: <C-void*>;
  input parameter source_tag_ :: <C-void*>;
  input parameter error_ :: <GError>;
  c-name: "g_task_report_error";
end;

define C-function g-task-get-cancellable
  input parameter self :: <GTask>;
  result res :: <GCancellable>;
  c-name: "g_task_get_cancellable";
end;

define C-function g-task-get-check-cancellable
  input parameter self :: <GTask>;
  result res :: <C-boolean>;
  c-name: "g_task_get_check_cancellable";
end;

define C-function g-task-get-context
  input parameter self :: <GTask>;
  result res :: <GMainContext>;
  c-name: "g_task_get_context";
end;

define C-function g-task-get-priority
  input parameter self :: <GTask>;
  result res :: <C-signed-int>;
  c-name: "g_task_get_priority";
end;

define C-function g-task-get-return-on-cancel
  input parameter self :: <GTask>;
  result res :: <C-boolean>;
  c-name: "g_task_get_return_on_cancel";
end;

define C-function g-task-get-source-object
  input parameter self :: <GTask>;
  result res :: <GObject>;
  c-name: "g_task_get_source_object";
end;

define C-function g-task-get-source-tag
  input parameter self :: <GTask>;
  result res :: <C-void*>;
  c-name: "g_task_get_source_tag";
end;

define C-function g-task-get-task-data
  input parameter self :: <GTask>;
  result res :: <C-void*>;
  c-name: "g_task_get_task_data";
end;

define C-function g-task-had-error
  input parameter self :: <GTask>;
  result res :: <C-boolean>;
  c-name: "g_task_had_error";
end;

define C-function g-task-propagate-boolean
  input parameter self :: <GTask>;
  result res :: <C-boolean>;
  c-name: "g_task_propagate_boolean";
end;

define C-function g-task-propagate-int
  input parameter self :: <GTask>;
  result res :: <C-signed-long>;
  c-name: "g_task_propagate_int";
end;

define C-function g-task-propagate-pointer
  input parameter self :: <GTask>;
  result res :: <C-void*>;
  c-name: "g_task_propagate_pointer";
end;

define C-function g-task-return-boolean
  input parameter self :: <GTask>;
  input parameter result_ :: <C-boolean>;
  c-name: "g_task_return_boolean";
end;

define C-function g-task-return-error
  input parameter self :: <GTask>;
  input parameter error_ :: <GError>;
  c-name: "g_task_return_error";
end;

define C-function g-task-return-error-if-cancelled
  input parameter self :: <GTask>;
  result res :: <C-boolean>;
  c-name: "g_task_return_error_if_cancelled";
end;

define C-function g-task-return-int
  input parameter self :: <GTask>;
  input parameter result_ :: <C-signed-long>;
  c-name: "g_task_return_int";
end;

define C-function g-task-return-pointer
  input parameter self :: <GTask>;
  input parameter result_ :: <C-void*>;
  input parameter result_destroy_ :: <C-function-pointer>;
  c-name: "g_task_return_pointer";
end;

define C-function g-task-set-check-cancellable
  input parameter self :: <GTask>;
  input parameter check_cancellable_ :: <C-boolean>;
  c-name: "g_task_set_check_cancellable";
end;

define C-function g-task-set-priority
  input parameter self :: <GTask>;
  input parameter priority_ :: <C-signed-int>;
  c-name: "g_task_set_priority";
end;

define C-function g-task-set-return-on-cancel
  input parameter self :: <GTask>;
  input parameter return_on_cancel_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_task_set_return_on_cancel";
end;

define C-function g-task-set-source-tag
  input parameter self :: <GTask>;
  input parameter source_tag_ :: <C-void*>;
  c-name: "g_task_set_source_tag";
end;

define C-function g-task-set-task-data
  input parameter self :: <GTask>;
  input parameter task_data_ :: <C-void*>;
  input parameter task_data_destroy_ :: <C-function-pointer>;
  c-name: "g_task_set_task_data";
end;

define C-struct <_GTaskClass>
  pointer-type-name: <GTaskClass>;
end C-struct;

define open C-subtype <GTcpConnection> (<GSocketConnection>)
  constant slot gtcpconnection-parent-instance :: <GSocketConnection>;
  constant slot gtcpconnection-priv :: <GTcpConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTcpConnection*> => <GTcpConnection>;

define property-getter tcpconnection-graceful-disconnect :: <C-boolean> on <GTcpConnection> end;
define property-setter tcpconnection-graceful-disconnect :: <C-boolean> on <GTcpConnection> end;
define C-function g-tcp-connection-get-graceful-disconnect
  input parameter self :: <GTcpConnection>;
  result res :: <C-boolean>;
  c-name: "g_tcp_connection_get_graceful_disconnect";
end;

define C-function g-tcp-connection-set-graceful-disconnect
  input parameter self :: <GTcpConnection>;
  input parameter graceful_disconnect_ :: <C-boolean>;
  c-name: "g_tcp_connection_set_graceful_disconnect";
end;

define C-struct <_GTcpConnectionClass>
  constant slot gtcpconnectionclass-parent-class :: <GSocketConnectionClass>;
  pointer-type-name: <GTcpConnectionClass>;
end C-struct;

define C-struct <_GTcpConnectionPrivate>
  pointer-type-name: <GTcpConnectionPrivate>;
end C-struct;

define open C-subtype <GTcpWrapperConnection> (<GTcpConnection>)
  constant slot gtcpwrapperconnection-parent-instance :: <GTcpConnection>;
  constant slot gtcpwrapperconnection-priv :: <GTcpWrapperConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTcpWrapperConnection*> => <GTcpWrapperConnection>;

define property-getter tcpwrapperconnection-base-io-stream :: <GIOStream> on <GTcpWrapperConnection> end;
define property-setter tcpwrapperconnection-base-io-stream :: <GIOStream> on <GTcpWrapperConnection> end;
define C-function g-tcp-wrapper-connection-new
  input parameter base_io_stream_ :: <GIOStream>;
  input parameter socket_ :: <GSocket>;
  result res :: <GSocketConnection>;
  c-name: "g_tcp_wrapper_connection_new";
end;

define C-function g-tcp-wrapper-connection-get-base-io-stream
  input parameter self :: <GTcpWrapperConnection>;
  result res :: <GIOStream>;
  c-name: "g_tcp_wrapper_connection_get_base_io_stream";
end;

define C-struct <_GTcpWrapperConnectionClass>
  constant slot gtcpwrapperconnectionclass-parent-class :: <GTcpConnectionClass>;
  pointer-type-name: <GTcpWrapperConnectionClass>;
end C-struct;

define C-struct <_GTcpWrapperConnectionPrivate>
  pointer-type-name: <GTcpWrapperConnectionPrivate>;
end C-struct;

define open C-subtype <GTestDBus> (<GObject>)
end C-subtype;

define C-pointer-type <GTestDBus*> => <GTestDBus>;

define property-getter testdbus-flags :: <GTestDBusFlags> on <GTestDBus> end;
define property-setter testdbus-flags :: <GTestDBusFlags> on <GTestDBus> end;
define C-function g-test-dbus-new
  input parameter flags_ :: <GTestDBusFlags>;
  result res :: <GTestDBus>;
  c-name: "g_test_dbus_new";
end;

define C-function g-test-dbus-unset
  c-name: "g_test_dbus_unset";
end;

define C-function g-test-dbus-add-service-dir
  input parameter self :: <GTestDBus>;
  input parameter path_ :: <C-string>;
  c-name: "g_test_dbus_add_service_dir";
end;

define C-function g-test-dbus-down
  input parameter self :: <GTestDBus>;
  c-name: "g_test_dbus_down";
end;

define C-function g-test-dbus-get-bus-address
  input parameter self :: <GTestDBus>;
  result res :: <C-string>;
  c-name: "g_test_dbus_get_bus_address";
end;

define C-function g-test-dbus-get-flags
  input parameter self :: <GTestDBus>;
  result res :: <GTestDBusFlags>;
  c-name: "g_test_dbus_get_flags";
end;

define C-function g-test-dbus-stop
  input parameter self :: <GTestDBus>;
  c-name: "g_test_dbus_stop";
end;

define C-function g-test-dbus-up
  input parameter self :: <GTestDBus>;
  c-name: "g_test_dbus_up";
end;

define constant $G-TEST-DBUS-NONE = 0;
define constant <GTestDBusFlags> = <C-int>;
define C-pointer-type <GTestDBusFlags*> => <GTestDBusFlags>;

define open C-subtype <GThemedIcon> (<GObject>)
end C-subtype;

define C-pointer-type <GThemedIcon*> => <GThemedIcon>;

define property-setter themedicon-name :: <C-string> on <GThemedIcon> end;
define property-getter themedicon-names :: <C-string*> on <GThemedIcon> end;
define property-setter themedicon-names :: <C-string*> on <GThemedIcon> end;
define property-getter themedicon-use-default-fallbacks :: <C-boolean> on <GThemedIcon> end;
define property-setter themedicon-use-default-fallbacks :: <C-boolean> on <GThemedIcon> end;
define C-function g-themed-icon-new
  input parameter iconname_ :: <C-string>;
  result res :: <GThemedIcon>;
  c-name: "g_themed_icon_new";
end;

define C-function g-themed-icon-new-from-names
  input parameter iconnames_ :: <C-string*>;
  input parameter len_ :: <C-signed-int>;
  result res :: <GThemedIcon>;
  c-name: "g_themed_icon_new_from_names";
end;

define C-function g-themed-icon-new-with-default-fallbacks
  input parameter iconname_ :: <C-string>;
  result res :: <GThemedIcon>;
  c-name: "g_themed_icon_new_with_default_fallbacks";
end;

define C-function g-themed-icon-append-name
  input parameter self :: <GThemedIcon>;
  input parameter iconname_ :: <C-string>;
  c-name: "g_themed_icon_append_name";
end;

define C-function g-themed-icon-get-names
  input parameter self :: <GThemedIcon>;
  result res :: <C-string*>;
  c-name: "g_themed_icon_get_names";
end;

define C-function g-themed-icon-prepend-name
  input parameter self :: <GThemedIcon>;
  input parameter iconname_ :: <C-string>;
  c-name: "g_themed_icon_prepend_name";
end;

define C-struct <_GThemedIconClass>
  pointer-type-name: <GThemedIconClass>;
end C-struct;

define open C-subtype <GThreadedSocketService> (<GSocketService>)
  constant slot gthreadedsocketservice-parent-instance :: <GSocketService>;
  constant slot gthreadedsocketservice-priv :: <GThreadedSocketServicePrivate>;
end C-subtype;

define C-pointer-type <GThreadedSocketService*> => <GThreadedSocketService>;

define property-getter threadedsocketservice-max-threads :: <C-signed-int> on <GThreadedSocketService> end;
define property-setter threadedsocketservice-max-threads :: <C-signed-int> on <GThreadedSocketService> end;
define C-function g-threaded-socket-service-new
  input parameter max_threads_ :: <C-signed-int>;
  result res :: <GSocketService>;
  c-name: "g_threaded_socket_service_new";
end;

define C-struct <_GThreadedSocketServiceClass>
  constant slot gthreadedsocketserviceclass-parent-class :: <GSocketServiceClass>;
  constant slot gthreadedsocketserviceclass-run :: <C-function-pointer>;
  constant slot gthreadedsocketserviceclass--g-reserved1 :: <C-void*>;
  constant slot gthreadedsocketserviceclass--g-reserved2 :: <C-void*>;
  constant slot gthreadedsocketserviceclass--g-reserved3 :: <C-void*>;
  constant slot gthreadedsocketserviceclass--g-reserved4 :: <C-void*>;
  constant slot gthreadedsocketserviceclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GThreadedSocketServiceClass>;
end C-struct;

define C-struct <_GThreadedSocketServicePrivate>
  pointer-type-name: <GThreadedSocketServicePrivate>;
end C-struct;

define constant $G-TLS-AUTHENTICATION-NONE = 0;
define constant $G-TLS-AUTHENTICATION-REQUESTED = 1;
define constant $G-TLS-AUTHENTICATION-REQUIRED = 2;
define constant <GTlsAuthenticationMode> = <C-int>;
define C-pointer-type <GTlsAuthenticationMode*> => <GTlsAuthenticationMode>;

// Interface
define open C-subtype <GTlsBackend> (<C-void*>)
end C-subtype;

define C-pointer-type <GTlsBackend*> => <GTlsBackend>;

define C-function g-tls-backend-get-default
  result res :: <GTlsBackend>;
  c-name: "g_tls_backend_get_default";
end;

define C-function g-tls-backend-get-certificate-type
  input parameter self :: <GTlsBackend>;
  result res :: <C-long>;
  c-name: "g_tls_backend_get_certificate_type";
end;

define C-function g-tls-backend-get-client-connection-type
  input parameter self :: <GTlsBackend>;
  result res :: <C-long>;
  c-name: "g_tls_backend_get_client_connection_type";
end;

define C-function g-tls-backend-get-default-database
  input parameter self :: <GTlsBackend>;
  result res :: <GTlsDatabase>;
  c-name: "g_tls_backend_get_default_database";
end;

define C-function g-tls-backend-get-file-database-type
  input parameter self :: <GTlsBackend>;
  result res :: <C-long>;
  c-name: "g_tls_backend_get_file_database_type";
end;

define C-function g-tls-backend-get-server-connection-type
  input parameter self :: <GTlsBackend>;
  result res :: <C-long>;
  c-name: "g_tls_backend_get_server_connection_type";
end;

define C-function g-tls-backend-supports-tls
  input parameter self :: <GTlsBackend>;
  result res :: <C-boolean>;
  c-name: "g_tls_backend_supports_tls";
end;

define C-struct <_GTlsBackendInterface>
  constant slot gtlsbackendinterface-g-iface :: <GTypeInterface>;
  constant slot gtlsbackendinterface-supports-tls :: <C-function-pointer>;
  constant slot gtlsbackendinterface-get-certificate-type :: <C-function-pointer>;
  constant slot gtlsbackendinterface-get-client-connection-type :: <C-function-pointer>;
  constant slot gtlsbackendinterface-get-server-connection-type :: <C-function-pointer>;
  constant slot gtlsbackendinterface-get-file-database-type :: <C-function-pointer>;
  constant slot gtlsbackendinterface-get-default-database :: <C-function-pointer>;
  pointer-type-name: <GTlsBackendInterface>;
end C-struct;

define open C-subtype <GTlsCertificate> (<GObject>)
  constant slot gtlscertificate-parent-instance :: <GObject>;
  constant slot gtlscertificate-priv :: <GTlsCertificatePrivate>;
end C-subtype;

define C-pointer-type <GTlsCertificate*> => <GTlsCertificate>;

define property-getter tlscertificate-certificate :: <GByteArray> on <GTlsCertificate> end;
define property-setter tlscertificate-certificate :: <GByteArray> on <GTlsCertificate> end;
define property-getter tlscertificate-certificate-pem :: <C-string> on <GTlsCertificate> end;
define property-setter tlscertificate-certificate-pem :: <C-string> on <GTlsCertificate> end;
define property-getter tlscertificate-issuer :: <GTlsCertificate> on <GTlsCertificate> end;
define property-setter tlscertificate-issuer :: <GTlsCertificate> on <GTlsCertificate> end;
define property-setter tlscertificate-private-key :: <GByteArray> on <GTlsCertificate> end;
define property-setter tlscertificate-private-key-pem :: <C-string> on <GTlsCertificate> end;
define C-function g-tls-certificate-new-from-file
  input parameter file_ :: <C-string>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_file";
end;

define C-function g-tls-certificate-new-from-files
  input parameter cert_file_ :: <C-string>;
  input parameter key_file_ :: <C-string>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_files";
end;

define C-function g-tls-certificate-new-from-pem
  input parameter data_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_pem";
end;

define C-function g-tls-certificate-list-new-from-file
  input parameter file_ :: <C-string>;
  result res :: <GList>;
  c-name: "g_tls_certificate_list_new_from_file";
end;

define C-function g-tls-certificate-get-issuer
  input parameter self :: <GTlsCertificate>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_get_issuer";
end;

define C-function g-tls-certificate-is-same
  input parameter self :: <GTlsCertificate>;
  input parameter cert_two_ :: <GTlsCertificate>;
  result res :: <C-boolean>;
  c-name: "g_tls_certificate_is_same";
end;

define C-function g-tls-certificate-verify
  input parameter self :: <GTlsCertificate>;
  input parameter identity_ :: <GSocketConnectable>;
  input parameter trusted_ca_ :: <GTlsCertificate>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_certificate_verify";
end;

define C-struct <_GTlsCertificateClass>
  constant slot gtlscertificateclass-parent-class :: <GObjectClass>;
  constant slot gtlscertificateclass-verify :: <C-function-pointer>;
  constant slot gtlscertificateclass-padding :: <C-void*>;
  pointer-type-name: <GTlsCertificateClass>;
end C-struct;

define constant $G-TLS-CERTIFICATE-UNKNOWN-CA = 1;
define constant $G-TLS-CERTIFICATE-BAD-IDENTITY = 2;
define constant $G-TLS-CERTIFICATE-NOT-ACTIVATED = 4;
define constant $G-TLS-CERTIFICATE-EXPIRED = 8;
define constant $G-TLS-CERTIFICATE-REVOKED = 16;
define constant $G-TLS-CERTIFICATE-INSECURE = 32;
define constant $G-TLS-CERTIFICATE-GENERIC-ERROR = 64;
define constant $G-TLS-CERTIFICATE-VALIDATE-ALL = 127;
define constant <GTlsCertificateFlags> = <C-int>;
define C-pointer-type <GTlsCertificateFlags*> => <GTlsCertificateFlags>;

define C-struct <_GTlsCertificatePrivate>
  pointer-type-name: <GTlsCertificatePrivate>;
end C-struct;

// Interface
define open C-subtype <GTlsClientConnection> (<GTlsConnection>)
end C-subtype;

define C-pointer-type <GTlsClientConnection*> => <GTlsClientConnection>;

define C-function g-tls-client-connection-new
  input parameter base_io_stream_ :: <GIOStream>;
  input parameter server_identity_ :: <GSocketConnectable>;
  result res :: <GTlsClientConnection>;
  c-name: "g_tls_client_connection_new";
end;

define C-function g-tls-client-connection-get-accepted-cas
  input parameter self :: <GTlsClientConnection>;
  result res :: <GList>;
  c-name: "g_tls_client_connection_get_accepted_cas";
end;

define C-function g-tls-client-connection-get-server-identity
  input parameter self :: <GTlsClientConnection>;
  result res :: <GSocketConnectable>;
  c-name: "g_tls_client_connection_get_server_identity";
end;

define C-function g-tls-client-connection-get-use-ssl3
  input parameter self :: <GTlsClientConnection>;
  result res :: <C-boolean>;
  c-name: "g_tls_client_connection_get_use_ssl3";
end;

define C-function g-tls-client-connection-get-validation-flags
  input parameter self :: <GTlsClientConnection>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_client_connection_get_validation_flags";
end;

define C-function g-tls-client-connection-set-server-identity
  input parameter self :: <GTlsClientConnection>;
  input parameter identity_ :: <GSocketConnectable>;
  c-name: "g_tls_client_connection_set_server_identity";
end;

define C-function g-tls-client-connection-set-use-ssl3
  input parameter self :: <GTlsClientConnection>;
  input parameter use_ssl3_ :: <C-boolean>;
  c-name: "g_tls_client_connection_set_use_ssl3";
end;

define C-function g-tls-client-connection-set-validation-flags
  input parameter self :: <GTlsClientConnection>;
  input parameter flags_ :: <GTlsCertificateFlags>;
  c-name: "g_tls_client_connection_set_validation_flags";
end;

define C-struct <_GTlsClientConnectionInterface>
  constant slot gtlsclientconnectioninterface-g-iface :: <GTypeInterface>;
  pointer-type-name: <GTlsClientConnectionInterface>;
end C-struct;

define open C-subtype <GTlsConnection> (<GIOStream>)
  constant slot gtlsconnection-parent-instance :: <GIOStream>;
  constant slot gtlsconnection-priv :: <GTlsConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTlsConnection*> => <GTlsConnection>;

define property-getter tlsconnection-base-io-stream :: <GIOStream> on <GTlsConnection> end;
define property-setter tlsconnection-base-io-stream :: <GIOStream> on <GTlsConnection> end;
define property-getter tlsconnection-certificate :: <GTlsCertificate> on <GTlsConnection> end;
define property-setter tlsconnection-certificate :: <GTlsCertificate> on <GTlsConnection> end;
define property-getter tlsconnection-database :: <GTlsDatabase> on <GTlsConnection> end;
define property-setter tlsconnection-database :: <GTlsDatabase> on <GTlsConnection> end;
define property-getter tlsconnection-interaction :: <GTlsInteraction> on <GTlsConnection> end;
define property-setter tlsconnection-interaction :: <GTlsInteraction> on <GTlsConnection> end;
define property-getter tlsconnection-peer-certificate :: <GTlsCertificate> on <GTlsConnection> end;
define property-getter tlsconnection-peer-certificate-errors :: <GTlsCertificateFlags> on <GTlsConnection> end;
define property-getter tlsconnection-rehandshake-mode :: <GTlsRehandshakeMode> on <GTlsConnection> end;
define property-setter tlsconnection-rehandshake-mode :: <GTlsRehandshakeMode> on <GTlsConnection> end;
define property-getter tlsconnection-require-close-notify :: <C-boolean> on <GTlsConnection> end;
define property-setter tlsconnection-require-close-notify :: <C-boolean> on <GTlsConnection> end;
define property-getter tlsconnection-use-system-certdb :: <C-boolean> on <GTlsConnection> end;
define property-setter tlsconnection-use-system-certdb :: <C-boolean> on <GTlsConnection> end;
define C-function g-tls-connection-emit-accept-certificate
  input parameter self :: <GTlsConnection>;
  input parameter peer_cert_ :: <GTlsCertificate>;
  input parameter errors_ :: <GTlsCertificateFlags>;
  result res :: <C-boolean>;
  c-name: "g_tls_connection_emit_accept_certificate";
end;

define C-function g-tls-connection-get-certificate
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_connection_get_certificate";
end;

define C-function g-tls-connection-get-database
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsDatabase>;
  c-name: "g_tls_connection_get_database";
end;

define C-function g-tls-connection-get-interaction
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsInteraction>;
  c-name: "g_tls_connection_get_interaction";
end;

define C-function g-tls-connection-get-peer-certificate
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_connection_get_peer_certificate";
end;

define C-function g-tls-connection-get-peer-certificate-errors
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_connection_get_peer_certificate_errors";
end;

define C-function g-tls-connection-get-rehandshake-mode
  input parameter self :: <GTlsConnection>;
  result res :: <GTlsRehandshakeMode>;
  c-name: "g_tls_connection_get_rehandshake_mode";
end;

define C-function g-tls-connection-get-require-close-notify
  input parameter self :: <GTlsConnection>;
  result res :: <C-boolean>;
  c-name: "g_tls_connection_get_require_close_notify";
end;

define C-function g-tls-connection-get-use-system-certdb
  input parameter self :: <GTlsConnection>;
  result res :: <C-boolean>;
  c-name: "g_tls_connection_get_use_system_certdb";
end;

define C-function g-tls-connection-handshake
  input parameter self :: <GTlsConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_tls_connection_handshake";
end;

define C-function g-tls-connection-handshake-async
  input parameter self :: <GTlsConnection>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_connection_handshake_async";
end;

define C-function g-tls-connection-handshake-finish
  input parameter self :: <GTlsConnection>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_tls_connection_handshake_finish";
end;

define C-function g-tls-connection-set-certificate
  input parameter self :: <GTlsConnection>;
  input parameter certificate_ :: <GTlsCertificate>;
  c-name: "g_tls_connection_set_certificate";
end;

define C-function g-tls-connection-set-database
  input parameter self :: <GTlsConnection>;
  input parameter database_ :: <GTlsDatabase>;
  c-name: "g_tls_connection_set_database";
end;

define C-function g-tls-connection-set-interaction
  input parameter self :: <GTlsConnection>;
  input parameter interaction_ :: <GTlsInteraction>;
  c-name: "g_tls_connection_set_interaction";
end;

define C-function g-tls-connection-set-rehandshake-mode
  input parameter self :: <GTlsConnection>;
  input parameter mode_ :: <GTlsRehandshakeMode>;
  c-name: "g_tls_connection_set_rehandshake_mode";
end;

define C-function g-tls-connection-set-require-close-notify
  input parameter self :: <GTlsConnection>;
  input parameter require_close_notify_ :: <C-boolean>;
  c-name: "g_tls_connection_set_require_close_notify";
end;

define C-function g-tls-connection-set-use-system-certdb
  input parameter self :: <GTlsConnection>;
  input parameter use_system_certdb_ :: <C-boolean>;
  c-name: "g_tls_connection_set_use_system_certdb";
end;

define C-struct <_GTlsConnectionClass>
  constant slot gtlsconnectionclass-parent-class :: <GIOStreamClass>;
  constant slot gtlsconnectionclass-accept-certificate :: <C-function-pointer>;
  constant slot gtlsconnectionclass-handshake :: <C-function-pointer>;
  constant slot gtlsconnectionclass-handshake-async :: <C-function-pointer>;
  constant slot gtlsconnectionclass-handshake-finish :: <C-function-pointer>;
  constant slot gtlsconnectionclass-padding :: <C-void*>;
  pointer-type-name: <GTlsConnectionClass>;
end C-struct;

define C-struct <_GTlsConnectionPrivate>
  pointer-type-name: <GTlsConnectionPrivate>;
end C-struct;

define open C-subtype <GTlsDatabase> (<GObject>)
  constant slot gtlsdatabase-parent-instance :: <GObject>;
  constant slot gtlsdatabase-priv :: <GTlsDatabasePrivate>;
end C-subtype;

define C-pointer-type <GTlsDatabase*> => <GTlsDatabase>;

define C-function g-tls-database-create-certificate-handle
  input parameter self :: <GTlsDatabase>;
  input parameter certificate_ :: <GTlsCertificate>;
  result res :: <C-string>;
  c-name: "g_tls_database_create_certificate_handle";
end;

define C-function g-tls-database-lookup-certificate-for-handle
  input parameter self :: <GTlsDatabase>;
  input parameter handle_ :: <C-string>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_for_handle";
end;

define C-function g-tls-database-lookup-certificate-for-handle-async
  input parameter self :: <GTlsDatabase>;
  input parameter handle_ :: <C-string>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_database_lookup_certificate_for_handle_async";
end;

define C-function g-tls-database-lookup-certificate-for-handle-finish
  input parameter self :: <GTlsDatabase>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_for_handle_finish";
end;

define C-function g-tls-database-lookup-certificate-issuer
  input parameter self :: <GTlsDatabase>;
  input parameter certificate_ :: <GTlsCertificate>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_issuer";
end;

define C-function g-tls-database-lookup-certificate-issuer-async
  input parameter self :: <GTlsDatabase>;
  input parameter certificate_ :: <GTlsCertificate>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_database_lookup_certificate_issuer_async";
end;

define C-function g-tls-database-lookup-certificate-issuer-finish
  input parameter self :: <GTlsDatabase>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_issuer_finish";
end;

define C-function g-tls-database-lookup-certificates-issued-by
  input parameter self :: <GTlsDatabase>;
  input parameter issuer_raw_dn_ :: <GByteArray>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GList>;
  c-name: "g_tls_database_lookup_certificates_issued_by";
end;

define C-function g-tls-database-lookup-certificates-issued-by-async
  input parameter self :: <GTlsDatabase>;
  input parameter issuer_raw_dn_ :: <GByteArray>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_database_lookup_certificates_issued_by_async";
end;

define C-function g-tls-database-lookup-certificates-issued-by-finish
  input parameter self :: <GTlsDatabase>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GList>;
  c-name: "g_tls_database_lookup_certificates_issued_by_finish";
end;

define C-function g-tls-database-verify-chain
  input parameter self :: <GTlsDatabase>;
  input parameter chain_ :: <GTlsCertificate>;
  input parameter purpose_ :: <C-string>;
  input parameter identity_ :: <GSocketConnectable>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseVerifyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_database_verify_chain";
end;

define C-function g-tls-database-verify-chain-async
  input parameter self :: <GTlsDatabase>;
  input parameter chain_ :: <GTlsCertificate>;
  input parameter purpose_ :: <C-string>;
  input parameter identity_ :: <GSocketConnectable>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseVerifyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_database_verify_chain_async";
end;

define C-function g-tls-database-verify-chain-finish
  input parameter self :: <GTlsDatabase>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_database_verify_chain_finish";
end;

define C-struct <_GTlsDatabaseClass>
  constant slot gtlsdatabaseclass-parent-class :: <GObjectClass>;
  constant slot gtlsdatabaseclass-verify-chain :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-verify-chain-async :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-verify-chain-finish :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-create-certificate-handle :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-for-handle :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-for-handle-async :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-for-handle-finish :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-issuer :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-issuer-async :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificate-issuer-finish :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificates-issued-by :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificates-issued-by-async :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-lookup-certificates-issued-by-finish :: <C-function-pointer>;
  constant slot gtlsdatabaseclass-padding :: <C-void*>;
  pointer-type-name: <GTlsDatabaseClass>;
end C-struct;

define constant $G-TLS-DATABASE-LOOKUP-NONE = 0;
define constant $G-TLS-DATABASE-LOOKUP-KEYPAIR = 1;
define constant <GTlsDatabaseLookupFlags> = <C-int>;
define C-pointer-type <GTlsDatabaseLookupFlags*> => <GTlsDatabaseLookupFlags>;

define C-struct <_GTlsDatabasePrivate>
  pointer-type-name: <GTlsDatabasePrivate>;
end C-struct;

define constant $G-TLS-DATABASE-VERIFY-NONE = 0;
define constant <GTlsDatabaseVerifyFlags> = <C-int>;
define C-pointer-type <GTlsDatabaseVerifyFlags*> => <GTlsDatabaseVerifyFlags>;

define constant $G-TLS-ERROR-UNAVAILABLE = 0;
define constant $G-TLS-ERROR-MISC = 1;
define constant $G-TLS-ERROR-BAD-CERTIFICATE = 2;
define constant $G-TLS-ERROR-NOT-TLS = 3;
define constant $G-TLS-ERROR-HANDSHAKE = 4;
define constant $G-TLS-ERROR-CERTIFICATE-REQUIRED = 5;
define constant $G-TLS-ERROR-EOF = 6;
define constant <GTlsError> = <C-int>;
define C-pointer-type <GTlsError*> => <GTlsError>;

// Interface
define open C-subtype <GTlsFileDatabase> (<GTlsDatabase>)
end C-subtype;

define C-pointer-type <GTlsFileDatabase*> => <GTlsFileDatabase>;

define C-function g-tls-file-database-new
  input parameter anchors_ :: <C-string>;
  result res :: <GTlsFileDatabase>;
  c-name: "g_tls_file_database_new";
end;

define C-struct <_GTlsFileDatabaseInterface>
  constant slot gtlsfiledatabaseinterface-g-iface :: <GTypeInterface>;
  constant slot gtlsfiledatabaseinterface-padding :: <C-void*>;
  pointer-type-name: <GTlsFileDatabaseInterface>;
end C-struct;

define open C-subtype <GTlsInteraction> (<GObject>)
  constant slot gtlsinteraction-parent-instance :: <GObject>;
  constant slot gtlsinteraction-priv :: <GTlsInteractionPrivate>;
end C-subtype;

define C-pointer-type <GTlsInteraction*> => <GTlsInteraction>;

define C-function g-tls-interaction-ask-password
  input parameter self :: <GTlsInteraction>;
  input parameter password_ :: <GTlsPassword>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GTlsInteractionResult>;
  c-name: "g_tls_interaction_ask_password";
end;

define C-function g-tls-interaction-ask-password-async
  input parameter self :: <GTlsInteraction>;
  input parameter password_ :: <GTlsPassword>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_tls_interaction_ask_password_async";
end;

define C-function g-tls-interaction-ask-password-finish
  input parameter self :: <GTlsInteraction>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GTlsInteractionResult>;
  c-name: "g_tls_interaction_ask_password_finish";
end;

define C-function g-tls-interaction-invoke-ask-password
  input parameter self :: <GTlsInteraction>;
  input parameter password_ :: <GTlsPassword>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GTlsInteractionResult>;
  c-name: "g_tls_interaction_invoke_ask_password";
end;

define C-struct <_GTlsInteractionClass>
  constant slot gtlsinteractionclass-parent-class :: <GObjectClass>;
  constant slot gtlsinteractionclass-ask-password :: <C-function-pointer>;
  constant slot gtlsinteractionclass-ask-password-async :: <C-function-pointer>;
  constant slot gtlsinteractionclass-ask-password-finish :: <C-function-pointer>;
  constant slot gtlsinteractionclass-padding :: <C-void*>;
  pointer-type-name: <GTlsInteractionClass>;
end C-struct;

define C-struct <_GTlsInteractionPrivate>
  pointer-type-name: <GTlsInteractionPrivate>;
end C-struct;

define constant $G-TLS-INTERACTION-UNHANDLED = 0;
define constant $G-TLS-INTERACTION-HANDLED = 1;
define constant $G-TLS-INTERACTION-FAILED = 2;
define constant <GTlsInteractionResult> = <C-int>;
define C-pointer-type <GTlsInteractionResult*> => <GTlsInteractionResult>;

define open C-subtype <GTlsPassword> (<GObject>)
  constant slot gtlspassword-parent-instance :: <GObject>;
  constant slot gtlspassword-priv :: <GTlsPasswordPrivate>;
end C-subtype;

define C-pointer-type <GTlsPassword*> => <GTlsPassword>;

define property-getter tlspassword-description :: <C-string> on <GTlsPassword> end;
define property-setter tlspassword-description :: <C-string> on <GTlsPassword> end;
define property-getter tlspassword-flags :: <GTlsPasswordFlags> on <GTlsPassword> end;
define property-setter tlspassword-flags :: <GTlsPasswordFlags> on <GTlsPassword> end;
define property-getter tlspassword-warning :: <C-string> on <GTlsPassword> end;
define property-setter tlspassword-warning :: <C-string> on <GTlsPassword> end;
define C-function g-tls-password-new
  input parameter flags_ :: <GTlsPasswordFlags>;
  input parameter description_ :: <C-string>;
  result res :: <GTlsPassword>;
  c-name: "g_tls_password_new";
end;

define C-function g-tls-password-get-description
  input parameter self :: <GTlsPassword>;
  result res :: <C-string>;
  c-name: "g_tls_password_get_description";
end;

define C-function g-tls-password-get-flags
  input parameter self :: <GTlsPassword>;
  result res :: <GTlsPasswordFlags>;
  c-name: "g_tls_password_get_flags";
end;

define C-function g-tls-password-get-value
  input parameter self :: <GTlsPassword>;
  input parameter length_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_tls_password_get_value";
end;

define C-function g-tls-password-get-warning
  input parameter self :: <GTlsPassword>;
  result res :: <C-string>;
  c-name: "g_tls_password_get_warning";
end;

define C-function g-tls-password-set-description
  input parameter self :: <GTlsPassword>;
  input parameter description_ :: <C-string>;
  c-name: "g_tls_password_set_description";
end;

define C-function g-tls-password-set-flags
  input parameter self :: <GTlsPassword>;
  input parameter flags_ :: <GTlsPasswordFlags>;
  c-name: "g_tls_password_set_flags";
end;

define C-function g-tls-password-set-value
  input parameter self :: <GTlsPassword>;
  input parameter value_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  c-name: "g_tls_password_set_value";
end;

define C-function g-tls-password-set-value-full
  input parameter self :: <GTlsPassword>;
  input parameter value_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "g_tls_password_set_value_full";
end;

define C-function g-tls-password-set-warning
  input parameter self :: <GTlsPassword>;
  input parameter warning_ :: <C-string>;
  c-name: "g_tls_password_set_warning";
end;

define C-struct <_GTlsPasswordClass>
  constant slot gtlspasswordclass-parent-class :: <GObjectClass>;
  constant slot gtlspasswordclass-get-value :: <C-function-pointer>;
  constant slot gtlspasswordclass-set-value :: <C-function-pointer>;
  constant slot gtlspasswordclass-get-default-warning :: <C-function-pointer>;
  constant slot gtlspasswordclass-padding :: <C-void*>;
  pointer-type-name: <GTlsPasswordClass>;
end C-struct;

define constant $G-TLS-PASSWORD-NONE = 0;
define constant $G-TLS-PASSWORD-RETRY = 2;
define constant $G-TLS-PASSWORD-MANY-TRIES = 4;
define constant $G-TLS-PASSWORD-FINAL-TRY = 8;
define constant <GTlsPasswordFlags> = <C-int>;
define C-pointer-type <GTlsPasswordFlags*> => <GTlsPasswordFlags>;

define C-struct <_GTlsPasswordPrivate>
  pointer-type-name: <GTlsPasswordPrivate>;
end C-struct;

define constant $G-TLS-REHANDSHAKE-NEVER = 0;
define constant $G-TLS-REHANDSHAKE-SAFELY = 1;
define constant $G-TLS-REHANDSHAKE-UNSAFELY = 2;
define constant <GTlsRehandshakeMode> = <C-int>;
define C-pointer-type <GTlsRehandshakeMode*> => <GTlsRehandshakeMode>;

// Interface
define open C-subtype <GTlsServerConnection> (<GTlsConnection>)
end C-subtype;

define C-pointer-type <GTlsServerConnection*> => <GTlsServerConnection>;

define C-function g-tls-server-connection-new
  input parameter base_io_stream_ :: <GIOStream>;
  input parameter certificate_ :: <GTlsCertificate>;
  result res :: <GTlsServerConnection>;
  c-name: "g_tls_server_connection_new";
end;

define C-struct <_GTlsServerConnectionInterface>
  constant slot gtlsserverconnectioninterface-g-iface :: <GTypeInterface>;
  pointer-type-name: <GTlsServerConnectionInterface>;
end C-struct;

define open C-subtype <GUnixConnection> (<GSocketConnection>)
  constant slot gunixconnection-parent-instance :: <GSocketConnection>;
  constant slot gunixconnection-priv :: <GUnixConnectionPrivate>;
end C-subtype;

define C-pointer-type <GUnixConnection*> => <GUnixConnection>;

define C-function g-unix-connection-receive-credentials
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GCredentials>;
  c-name: "g_unix_connection_receive_credentials";
end;

define C-function g-unix-connection-receive-credentials-async
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_unix_connection_receive_credentials_async";
end;

define C-function g-unix-connection-receive-credentials-finish
  input parameter self :: <GUnixConnection>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <GCredentials>;
  c-name: "g_unix_connection_receive_credentials_finish";
end;

define C-function g-unix-connection-receive-fd
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-int>;
  c-name: "g_unix_connection_receive_fd";
end;

define C-function g-unix-connection-send-credentials
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_unix_connection_send_credentials";
end;

define C-function g-unix-connection-send-credentials-async
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_unix_connection_send_credentials_async";
end;

define C-function g-unix-connection-send-credentials-finish
  input parameter self :: <GUnixConnection>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_unix_connection_send_credentials_finish";
end;

define C-function g-unix-connection-send-fd
  input parameter self :: <GUnixConnection>;
  input parameter fd_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_unix_connection_send_fd";
end;

define C-struct <_GUnixConnectionClass>
  constant slot gunixconnectionclass-parent-class :: <GSocketConnectionClass>;
  pointer-type-name: <GUnixConnectionClass>;
end C-struct;

define C-struct <_GUnixConnectionPrivate>
  pointer-type-name: <GUnixConnectionPrivate>;
end C-struct;

define open C-subtype <GUnixCredentialsMessage> (<GSocketControlMessage>)
  constant slot gunixcredentialsmessage-parent-instance :: <GSocketControlMessage>;
  constant slot gunixcredentialsmessage-priv :: <GUnixCredentialsMessagePrivate>;
end C-subtype;

define C-pointer-type <GUnixCredentialsMessage*> => <GUnixCredentialsMessage>;

define property-getter unixcredentialsmessage-credentials :: <GCredentials> on <GUnixCredentialsMessage> end;
define property-setter unixcredentialsmessage-credentials :: <GCredentials> on <GUnixCredentialsMessage> end;
define C-function g-unix-credentials-message-new
  result res :: <GSocketControlMessage>;
  c-name: "g_unix_credentials_message_new";
end;

define C-function g-unix-credentials-message-new-with-credentials
  input parameter credentials_ :: <GCredentials>;
  result res :: <GSocketControlMessage>;
  c-name: "g_unix_credentials_message_new_with_credentials";
end;

define C-function g-unix-credentials-message-is-supported
  result res :: <C-boolean>;
  c-name: "g_unix_credentials_message_is_supported";
end;

define C-function g-unix-credentials-message-get-credentials
  input parameter self :: <GUnixCredentialsMessage>;
  result res :: <GCredentials>;
  c-name: "g_unix_credentials_message_get_credentials";
end;

define C-struct <_GUnixCredentialsMessageClass>
  constant slot gunixcredentialsmessageclass-parent-class :: <GSocketControlMessageClass>;
  constant slot gunixcredentialsmessageclass--g-reserved1 :: <C-void*>;
  constant slot gunixcredentialsmessageclass--g-reserved2 :: <C-void*>;
  pointer-type-name: <GUnixCredentialsMessageClass>;
end C-struct;

define C-struct <_GUnixCredentialsMessagePrivate>
  pointer-type-name: <GUnixCredentialsMessagePrivate>;
end C-struct;

define open C-subtype <GUnixFDList> (<GObject>)
  constant slot gunixfdlist-parent-instance :: <GObject>;
  constant slot gunixfdlist-priv :: <GUnixFDListPrivate>;
end C-subtype;

define C-pointer-type <GUnixFDList*> => <GUnixFDList>;

define C-function g-unix-fd-list-new
  result res :: <GUnixFDList>;
  c-name: "g_unix_fd_list_new";
end;

define C-function g-unix-fd-list-new-from-array
  input parameter fds_ :: <C-signed-int*>;
  input parameter n_fds_ :: <C-signed-int>;
  result res :: <GUnixFDList>;
  c-name: "g_unix_fd_list_new_from_array";
end;

define C-function g-unix-fd-list-append
  input parameter self :: <GUnixFDList>;
  input parameter fd_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_unix_fd_list_append";
end;

define C-function g-unix-fd-list-get
  input parameter self :: <GUnixFDList>;
  input parameter index__ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_unix_fd_list_get";
end;

define C-function g-unix-fd-list-get-length
  input parameter self :: <GUnixFDList>;
  result res :: <C-signed-int>;
  c-name: "g_unix_fd_list_get_length";
end;

define C-function g-unix-fd-list-peek-fds
  input parameter self :: <GUnixFDList>;
  output parameter length_ :: <C-signed-int*>;
  result res :: <C-signed-int*>;
  c-name: "g_unix_fd_list_peek_fds";
end;

define C-function g-unix-fd-list-steal-fds
  input parameter self :: <GUnixFDList>;
  output parameter length_ :: <C-signed-int*>;
  result res :: <C-signed-int*>;
  c-name: "g_unix_fd_list_steal_fds";
end;

define C-struct <_GUnixFDListClass>
  constant slot gunixfdlistclass-parent-class :: <GObjectClass>;
  constant slot gunixfdlistclass--g-reserved1 :: <C-void*>;
  constant slot gunixfdlistclass--g-reserved2 :: <C-void*>;
  constant slot gunixfdlistclass--g-reserved3 :: <C-void*>;
  constant slot gunixfdlistclass--g-reserved4 :: <C-void*>;
  constant slot gunixfdlistclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GUnixFDListClass>;
end C-struct;

define C-struct <_GUnixFDListPrivate>
  pointer-type-name: <GUnixFDListPrivate>;
end C-struct;

define open C-subtype <GUnixFDMessage> (<GSocketControlMessage>)
  constant slot gunixfdmessage-parent-instance :: <GSocketControlMessage>;
  constant slot gunixfdmessage-priv :: <GUnixFDMessagePrivate>;
end C-subtype;

define C-pointer-type <GUnixFDMessage*> => <GUnixFDMessage>;

define property-getter unixfdmessage-fd-list :: <GUnixFDList> on <GUnixFDMessage> end;
define property-setter unixfdmessage-fd-list :: <GUnixFDList> on <GUnixFDMessage> end;
define C-function g-unix-fd-message-new
  result res :: <GSocketControlMessage>;
  c-name: "g_unix_fd_message_new";
end;

define C-function g-unix-fd-message-new-with-fd-list
  input parameter fd_list_ :: <GUnixFDList>;
  result res :: <GSocketControlMessage>;
  c-name: "g_unix_fd_message_new_with_fd_list";
end;

define C-function g-unix-fd-message-append-fd
  input parameter self :: <GUnixFDMessage>;
  input parameter fd_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_unix_fd_message_append_fd";
end;

define C-function g-unix-fd-message-get-fd-list
  input parameter self :: <GUnixFDMessage>;
  result res :: <GUnixFDList>;
  c-name: "g_unix_fd_message_get_fd_list";
end;

define C-function g-unix-fd-message-steal-fds
  input parameter self :: <GUnixFDMessage>;
  output parameter length_ :: <C-signed-int*>;
  result res :: <C-signed-int*>;
  c-name: "g_unix_fd_message_steal_fds";
end;

define C-struct <_GUnixFDMessageClass>
  constant slot gunixfdmessageclass-parent-class :: <GSocketControlMessageClass>;
  constant slot gunixfdmessageclass--g-reserved1 :: <C-void*>;
  constant slot gunixfdmessageclass--g-reserved2 :: <C-void*>;
  pointer-type-name: <GUnixFDMessageClass>;
end C-struct;

define C-struct <_GUnixFDMessagePrivate>
  pointer-type-name: <GUnixFDMessagePrivate>;
end C-struct;

define open C-subtype <GUnixInputStream> (<GInputStream>)
  constant slot gunixinputstream-parent-instance :: <GInputStream>;
  constant slot gunixinputstream-priv :: <GUnixInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GUnixInputStream*> => <GUnixInputStream>;

define property-getter unixinputstream-close-fd :: <C-boolean> on <GUnixInputStream> end;
define property-setter unixinputstream-close-fd :: <C-boolean> on <GUnixInputStream> end;
define property-getter unixinputstream-fd :: <C-signed-int> on <GUnixInputStream> end;
define property-setter unixinputstream-fd :: <C-signed-int> on <GUnixInputStream> end;
define C-function g-unix-input-stream-new
  input parameter fd_ :: <C-signed-int>;
  input parameter close_fd_ :: <C-boolean>;
  result res :: <GInputStream>;
  c-name: "g_unix_input_stream_new";
end;

define C-function g-unix-input-stream-get-close-fd
  input parameter self :: <GUnixInputStream>;
  result res :: <C-boolean>;
  c-name: "g_unix_input_stream_get_close_fd";
end;

define C-function g-unix-input-stream-get-fd
  input parameter self :: <GUnixInputStream>;
  result res :: <C-signed-int>;
  c-name: "g_unix_input_stream_get_fd";
end;

define C-function g-unix-input-stream-set-close-fd
  input parameter self :: <GUnixInputStream>;
  input parameter close_fd_ :: <C-boolean>;
  c-name: "g_unix_input_stream_set_close_fd";
end;

define C-struct <_GUnixInputStreamClass>
  constant slot gunixinputstreamclass-parent-class :: <GInputStreamClass>;
  constant slot gunixinputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gunixinputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gunixinputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gunixinputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gunixinputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GUnixInputStreamClass>;
end C-struct;

define C-struct <_GUnixInputStreamPrivate>
  pointer-type-name: <GUnixInputStreamPrivate>;
end C-struct;

define C-struct <_GUnixMountEntry>
  pointer-type-name: <GUnixMountEntry>;
end C-struct;

define open C-subtype <GUnixMountMonitor> (<GObject>)
end C-subtype;

define C-pointer-type <GUnixMountMonitor*> => <GUnixMountMonitor>;

define C-function g-unix-mount-monitor-new
  result res :: <GUnixMountMonitor>;
  c-name: "g_unix_mount_monitor_new";
end;

define C-function g-unix-mount-monitor-set-rate-limit
  input parameter self :: <GUnixMountMonitor>;
  input parameter limit_msec_ :: <C-signed-int>;
  c-name: "g_unix_mount_monitor_set_rate_limit";
end;

define C-struct <_GUnixMountMonitorClass>
  pointer-type-name: <GUnixMountMonitorClass>;
end C-struct;

define C-struct <_GUnixMountPoint>
  pointer-type-name: <GUnixMountPoint>;
end C-struct;

define C-function g-unix-mount-point-compare
  input parameter self :: <GUnixMountPoint>;
  input parameter mount2_ :: <GUnixMountPoint>;
  result res :: <C-signed-int>;
  c-name: "g_unix_mount_point_compare";
end;

define C-function g-unix-mount-point-free
  input parameter self :: <GUnixMountPoint>;
  c-name: "g_unix_mount_point_free";
end;

define C-function g-unix-mount-point-get-device-path
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-string>;
  c-name: "g_unix_mount_point_get_device_path";
end;

define C-function g-unix-mount-point-get-fs-type
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-string>;
  c-name: "g_unix_mount_point_get_fs_type";
end;

define C-function g-unix-mount-point-get-mount-path
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-string>;
  c-name: "g_unix_mount_point_get_mount_path";
end;

define C-function g-unix-mount-point-get-options
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-string>;
  c-name: "g_unix_mount_point_get_options";
end;

define C-function g-unix-mount-point-guess-can-eject
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_point_guess_can_eject";
end;

define C-function g-unix-mount-point-guess-icon
  input parameter self :: <GUnixMountPoint>;
  result res :: <GIcon>;
  c-name: "g_unix_mount_point_guess_icon";
end;

define C-function g-unix-mount-point-guess-name
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-string>;
  c-name: "g_unix_mount_point_guess_name";
end;

define C-function g-unix-mount-point-guess-symbolic-icon
  input parameter self :: <GUnixMountPoint>;
  result res :: <GIcon>;
  c-name: "g_unix_mount_point_guess_symbolic_icon";
end;

define C-function g-unix-mount-point-is-loopback
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_point_is_loopback";
end;

define C-function g-unix-mount-point-is-readonly
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_point_is_readonly";
end;

define C-function g-unix-mount-point-is-user-mountable
  input parameter self :: <GUnixMountPoint>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_point_is_user_mountable";
end;

define open C-subtype <GUnixOutputStream> (<GOutputStream>)
  constant slot gunixoutputstream-parent-instance :: <GOutputStream>;
  constant slot gunixoutputstream-priv :: <GUnixOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GUnixOutputStream*> => <GUnixOutputStream>;

define property-getter unixoutputstream-close-fd :: <C-boolean> on <GUnixOutputStream> end;
define property-setter unixoutputstream-close-fd :: <C-boolean> on <GUnixOutputStream> end;
define property-getter unixoutputstream-fd :: <C-signed-int> on <GUnixOutputStream> end;
define property-setter unixoutputstream-fd :: <C-signed-int> on <GUnixOutputStream> end;
define C-function g-unix-output-stream-new
  input parameter fd_ :: <C-signed-int>;
  input parameter close_fd_ :: <C-boolean>;
  result res :: <GOutputStream>;
  c-name: "g_unix_output_stream_new";
end;

define C-function g-unix-output-stream-get-close-fd
  input parameter self :: <GUnixOutputStream>;
  result res :: <C-boolean>;
  c-name: "g_unix_output_stream_get_close_fd";
end;

define C-function g-unix-output-stream-get-fd
  input parameter self :: <GUnixOutputStream>;
  result res :: <C-signed-int>;
  c-name: "g_unix_output_stream_get_fd";
end;

define C-function g-unix-output-stream-set-close-fd
  input parameter self :: <GUnixOutputStream>;
  input parameter close_fd_ :: <C-boolean>;
  c-name: "g_unix_output_stream_set_close_fd";
end;

define C-struct <_GUnixOutputStreamClass>
  constant slot gunixoutputstreamclass-parent-class :: <GOutputStreamClass>;
  constant slot gunixoutputstreamclass--g-reserved1 :: <C-void*>;
  constant slot gunixoutputstreamclass--g-reserved2 :: <C-void*>;
  constant slot gunixoutputstreamclass--g-reserved3 :: <C-void*>;
  constant slot gunixoutputstreamclass--g-reserved4 :: <C-void*>;
  constant slot gunixoutputstreamclass--g-reserved5 :: <C-void*>;
  pointer-type-name: <GUnixOutputStreamClass>;
end C-struct;

define C-struct <_GUnixOutputStreamPrivate>
  pointer-type-name: <GUnixOutputStreamPrivate>;
end C-struct;

define open C-subtype <GUnixSocketAddress> (<GSocketAddress>)
  constant slot gunixsocketaddress-parent-instance :: <GSocketAddress>;
  constant slot gunixsocketaddress-priv :: <GUnixSocketAddressPrivate>;
end C-subtype;

define C-pointer-type <GUnixSocketAddress*> => <GUnixSocketAddress>;

define property-getter unixsocketaddress-abstract :: <C-boolean> on <GUnixSocketAddress> end;
define property-setter unixsocketaddress-abstract :: <C-boolean> on <GUnixSocketAddress> end;
define property-getter unixsocketaddress-address-type :: <GUnixSocketAddressType> on <GUnixSocketAddress> end;
define property-setter unixsocketaddress-address-type :: <GUnixSocketAddressType> on <GUnixSocketAddress> end;
define property-getter unixsocketaddress-path :: <C-string> on <GUnixSocketAddress> end;
define property-setter unixsocketaddress-path :: <C-string> on <GUnixSocketAddress> end;
define property-getter unixsocketaddress-path-as-array :: <GByteArray> on <GUnixSocketAddress> end;
define property-setter unixsocketaddress-path-as-array :: <GByteArray> on <GUnixSocketAddress> end;
define C-function g-unix-socket-address-new
  input parameter path_ :: <C-string>;
  result res :: <GSocketAddress>;
  c-name: "g_unix_socket_address_new";
end;

define C-function g-unix-socket-address-new-abstract
  input parameter path_ :: <C-signed-char*>;
  input parameter path_len_ :: <C-signed-int>;
  result res :: <GSocketAddress>;
  c-name: "g_unix_socket_address_new_abstract";
end;

define C-function g-unix-socket-address-new-with-type
  input parameter path_ :: <C-signed-char*>;
  input parameter path_len_ :: <C-signed-int>;
  input parameter type_ :: <GUnixSocketAddressType>;
  result res :: <GSocketAddress>;
  c-name: "g_unix_socket_address_new_with_type";
end;

define C-function g-unix-socket-address-abstract-names-supported
  result res :: <C-boolean>;
  c-name: "g_unix_socket_address_abstract_names_supported";
end;

define C-function g-unix-socket-address-get-address-type
  input parameter self :: <GUnixSocketAddress>;
  result res :: <GUnixSocketAddressType>;
  c-name: "g_unix_socket_address_get_address_type";
end;

define C-function g-unix-socket-address-get-is-abstract
  input parameter self :: <GUnixSocketAddress>;
  result res :: <C-boolean>;
  c-name: "g_unix_socket_address_get_is_abstract";
end;

define C-function g-unix-socket-address-get-path
  input parameter self :: <GUnixSocketAddress>;
  result res :: <C-string>;
  c-name: "g_unix_socket_address_get_path";
end;

define C-function g-unix-socket-address-get-path-len
  input parameter self :: <GUnixSocketAddress>;
  result res :: <C-unsigned-long>;
  c-name: "g_unix_socket_address_get_path_len";
end;

define C-struct <_GUnixSocketAddressClass>
  constant slot gunixsocketaddressclass-parent-class :: <GSocketAddressClass>;
  pointer-type-name: <GUnixSocketAddressClass>;
end C-struct;

define C-struct <_GUnixSocketAddressPrivate>
  pointer-type-name: <GUnixSocketAddressPrivate>;
end C-struct;

define constant $G-UNIX-SOCKET-ADDRESS-INVALID = 0;
define constant $G-UNIX-SOCKET-ADDRESS-ANONYMOUS = 1;
define constant $G-UNIX-SOCKET-ADDRESS-PATH = 2;
define constant $G-UNIX-SOCKET-ADDRESS-ABSTRACT = 3;
define constant $G-UNIX-SOCKET-ADDRESS-ABSTRACT-PADDED = 4;
define constant <GUnixSocketAddressType> = <C-int>;
define C-pointer-type <GUnixSocketAddressType*> => <GUnixSocketAddressType>;

define constant $VFS-EXTENSION-POINT-NAME = "gio-vfs";

define constant $VOLUME-IDENTIFIER-KIND-CLASS = "class";

define constant $VOLUME-IDENTIFIER-KIND-HAL-UDI = "hal-udi";

define constant $VOLUME-IDENTIFIER-KIND-LABEL = "label";

define constant $VOLUME-IDENTIFIER-KIND-NFS-MOUNT = "nfs-mount";

define constant $VOLUME-IDENTIFIER-KIND-UNIX-DEVICE = "unix-device";

define constant $VOLUME-IDENTIFIER-KIND-UUID = "uuid";

define constant $VOLUME-MONITOR-EXTENSION-POINT-NAME = "gio-volume-monitor";

define open C-subtype <GVfs> (<GObject>)
  constant slot gvfs-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GVfs*> => <GVfs>;

define C-function g-vfs-get-default
  result res :: <GVfs>;
  c-name: "g_vfs_get_default";
end;

define C-function g-vfs-get-local
  result res :: <GVfs>;
  c-name: "g_vfs_get_local";
end;

define C-function g-vfs-get-file-for-path
  input parameter self :: <GVfs>;
  input parameter path_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_vfs_get_file_for_path";
end;

define C-function g-vfs-get-file-for-uri
  input parameter self :: <GVfs>;
  input parameter uri_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_vfs_get_file_for_uri";
end;

define C-function g-vfs-get-supported-uri-schemes
  input parameter self :: <GVfs>;
  result res :: <C-string*>;
  c-name: "g_vfs_get_supported_uri_schemes";
end;

define C-function g-vfs-is-active
  input parameter self :: <GVfs>;
  result res :: <C-boolean>;
  c-name: "g_vfs_is_active";
end;

define C-function g-vfs-parse-name
  input parameter self :: <GVfs>;
  input parameter parse_name_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_vfs_parse_name";
end;

define C-struct <_GVfsClass>
  constant slot gvfsclass-parent-class :: <GObjectClass>;
  constant slot gvfsclass-is-active :: <C-function-pointer>;
  constant slot gvfsclass-get-file-for-path :: <C-function-pointer>;
  constant slot gvfsclass-get-file-for-uri :: <C-function-pointer>;
  constant slot gvfsclass-get-supported-uri-schemes :: <C-function-pointer>;
  constant slot gvfsclass-parse-name :: <C-function-pointer>;
  constant slot gvfsclass-local-file-add-info :: <C-function-pointer>;
  constant slot gvfsclass-add-writable-namespaces :: <C-function-pointer>;
  constant slot gvfsclass-local-file-set-attributes :: <C-function-pointer>;
  constant slot gvfsclass-local-file-removed :: <C-function-pointer>;
  constant slot gvfsclass-local-file-moved :: <C-function-pointer>;
  constant slot gvfsclass--g-reserved1 :: <C-void*>;
  constant slot gvfsclass--g-reserved2 :: <C-void*>;
  constant slot gvfsclass--g-reserved3 :: <C-void*>;
  constant slot gvfsclass--g-reserved4 :: <C-void*>;
  constant slot gvfsclass--g-reserved5 :: <C-void*>;
  constant slot gvfsclass--g-reserved6 :: <C-void*>;
  constant slot gvfsclass--g-reserved7 :: <C-void*>;
  pointer-type-name: <GVfsClass>;
end C-struct;

// Interface
define open C-subtype <GVolume> (<C-void*>)
end C-subtype;

define C-pointer-type <GVolume*> => <GVolume>;

define C-function g-volume-can-eject
  input parameter self :: <GVolume>;
  result res :: <C-boolean>;
  c-name: "g_volume_can_eject";
end;

define C-function g-volume-can-mount
  input parameter self :: <GVolume>;
  result res :: <C-boolean>;
  c-name: "g_volume_can_mount";
end;

define C-function g-volume-eject
  input parameter self :: <GVolume>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_volume_eject";
end;

define C-function g-volume-eject-finish
  input parameter self :: <GVolume>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_volume_eject_finish";
end;

define C-function g-volume-eject-with-operation
  input parameter self :: <GVolume>;
  input parameter flags_ :: <GMountUnmountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_volume_eject_with_operation";
end;

define C-function g-volume-eject-with-operation-finish
  input parameter self :: <GVolume>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_volume_eject_with_operation_finish";
end;

define C-function g-volume-enumerate-identifiers
  input parameter self :: <GVolume>;
  result res :: <C-string*>;
  c-name: "g_volume_enumerate_identifiers";
end;

define C-function g-volume-get-activation-root
  input parameter self :: <GVolume>;
  result res :: <GFile>;
  c-name: "g_volume_get_activation_root";
end;

define C-function g-volume-get-drive
  input parameter self :: <GVolume>;
  result res :: <GDrive>;
  c-name: "g_volume_get_drive";
end;

define C-function g-volume-get-icon
  input parameter self :: <GVolume>;
  result res :: <GIcon>;
  c-name: "g_volume_get_icon";
end;

define C-function g-volume-get-identifier
  input parameter self :: <GVolume>;
  input parameter kind_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_volume_get_identifier";
end;

define C-function g-volume-get-mount
  input parameter self :: <GVolume>;
  result res :: <GMount>;
  c-name: "g_volume_get_mount";
end;

define C-function g-volume-get-name
  input parameter self :: <GVolume>;
  result res :: <C-string>;
  c-name: "g_volume_get_name";
end;

define C-function g-volume-get-sort-key
  input parameter self :: <GVolume>;
  result res :: <C-string>;
  c-name: "g_volume_get_sort_key";
end;

define C-function g-volume-get-symbolic-icon
  input parameter self :: <GVolume>;
  result res :: <GIcon>;
  c-name: "g_volume_get_symbolic_icon";
end;

define C-function g-volume-get-uuid
  input parameter self :: <GVolume>;
  result res :: <C-string>;
  c-name: "g_volume_get_uuid";
end;

define C-function g-volume-mount
  input parameter self :: <GVolume>;
  input parameter flags_ :: <GMountMountFlags>;
  input parameter mount_operation_ :: <GMountOperation>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_volume_mount";
end;

define C-function g-volume-mount-finish
  input parameter self :: <GVolume>;
  input parameter result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "g_volume_mount_finish";
end;

define C-function g-volume-should-automount
  input parameter self :: <GVolume>;
  result res :: <C-boolean>;
  c-name: "g_volume_should_automount";
end;

define C-struct <_GVolumeIface>
  constant slot gvolumeiface-g-iface :: <GTypeInterface>;
  constant slot gvolumeiface-changed :: <C-function-pointer>;
  constant slot gvolumeiface-removed :: <C-function-pointer>;
  constant slot gvolumeiface-get-name :: <C-function-pointer>;
  constant slot gvolumeiface-get-icon :: <C-function-pointer>;
  constant slot gvolumeiface-get-uuid :: <C-function-pointer>;
  constant slot gvolumeiface-get-drive :: <C-function-pointer>;
  constant slot gvolumeiface-get-mount :: <C-function-pointer>;
  constant slot gvolumeiface-can-mount :: <C-function-pointer>;
  constant slot gvolumeiface-can-eject :: <C-function-pointer>;
  constant slot gvolumeiface-mount-fn :: <C-function-pointer>;
  constant slot gvolumeiface-mount-finish :: <C-function-pointer>;
  constant slot gvolumeiface-eject :: <C-function-pointer>;
  constant slot gvolumeiface-eject-finish :: <C-function-pointer>;
  constant slot gvolumeiface-get-identifier :: <C-function-pointer>;
  constant slot gvolumeiface-enumerate-identifiers :: <C-function-pointer>;
  constant slot gvolumeiface-should-automount :: <C-function-pointer>;
  constant slot gvolumeiface-get-activation-root :: <C-function-pointer>;
  constant slot gvolumeiface-eject-with-operation :: <C-function-pointer>;
  constant slot gvolumeiface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot gvolumeiface-get-sort-key :: <C-function-pointer>;
  constant slot gvolumeiface-get-symbolic-icon :: <C-function-pointer>;
  pointer-type-name: <GVolumeIface>;
end C-struct;

define open C-subtype <GVolumeMonitor> (<GObject>)
  constant slot gvolumemonitor-parent-instance :: <GObject>;
  constant slot gvolumemonitor-priv :: <C-void*>;
end C-subtype;

define C-pointer-type <GVolumeMonitor*> => <GVolumeMonitor>;

define C-function g-volume-monitor-adopt-orphan-mount
  input parameter mount_ :: <GMount>;
  result res :: <GVolume>;
  c-name: "g_volume_monitor_adopt_orphan_mount";
end;

define C-function g-volume-monitor-get
  result res :: <GVolumeMonitor>;
  c-name: "g_volume_monitor_get";
end;

define C-function g-volume-monitor-get-connected-drives
  input parameter self :: <GVolumeMonitor>;
  result res :: <GList>;
  c-name: "g_volume_monitor_get_connected_drives";
end;

define C-function g-volume-monitor-get-mount-for-uuid
  input parameter self :: <GVolumeMonitor>;
  input parameter uuid_ :: <C-string>;
  result res :: <GMount>;
  c-name: "g_volume_monitor_get_mount_for_uuid";
end;

define C-function g-volume-monitor-get-mounts
  input parameter self :: <GVolumeMonitor>;
  result res :: <GList>;
  c-name: "g_volume_monitor_get_mounts";
end;

define C-function g-volume-monitor-get-volume-for-uuid
  input parameter self :: <GVolumeMonitor>;
  input parameter uuid_ :: <C-string>;
  result res :: <GVolume>;
  c-name: "g_volume_monitor_get_volume_for_uuid";
end;

define C-function g-volume-monitor-get-volumes
  input parameter self :: <GVolumeMonitor>;
  result res :: <GList>;
  c-name: "g_volume_monitor_get_volumes";
end;

define C-struct <_GVolumeMonitorClass>
  constant slot gvolumemonitorclass-parent-class :: <GObjectClass>;
  constant slot gvolumemonitorclass-volume-added :: <C-function-pointer>;
  constant slot gvolumemonitorclass-volume-removed :: <C-function-pointer>;
  constant slot gvolumemonitorclass-volume-changed :: <C-function-pointer>;
  constant slot gvolumemonitorclass-mount-added :: <C-function-pointer>;
  constant slot gvolumemonitorclass-mount-removed :: <C-function-pointer>;
  constant slot gvolumemonitorclass-mount-pre-unmount :: <C-function-pointer>;
  constant slot gvolumemonitorclass-mount-changed :: <C-function-pointer>;
  constant slot gvolumemonitorclass-drive-connected :: <C-function-pointer>;
  constant slot gvolumemonitorclass-drive-disconnected :: <C-function-pointer>;
  constant slot gvolumemonitorclass-drive-changed :: <C-function-pointer>;
  constant slot gvolumemonitorclass-is-supported :: <C-function-pointer>;
  constant slot gvolumemonitorclass-get-connected-drives :: <C-function-pointer>;
  constant slot gvolumemonitorclass-get-volumes :: <C-function-pointer>;
  constant slot gvolumemonitorclass-get-mounts :: <C-function-pointer>;
  constant slot gvolumemonitorclass-get-volume-for-uuid :: <C-function-pointer>;
  constant slot gvolumemonitorclass-get-mount-for-uuid :: <C-function-pointer>;
  constant slot gvolumemonitorclass-adopt-orphan-mount :: <C-void*>;
  constant slot gvolumemonitorclass-drive-eject-button :: <C-function-pointer>;
  constant slot gvolumemonitorclass-drive-stop-button :: <C-function-pointer>;
  constant slot gvolumemonitorclass--g-reserved1 :: <C-void*>;
  constant slot gvolumemonitorclass--g-reserved2 :: <C-void*>;
  constant slot gvolumemonitorclass--g-reserved3 :: <C-void*>;
  constant slot gvolumemonitorclass--g-reserved4 :: <C-void*>;
  constant slot gvolumemonitorclass--g-reserved5 :: <C-void*>;
  constant slot gvolumemonitorclass--g-reserved6 :: <C-void*>;
  pointer-type-name: <GVolumeMonitorClass>;
end C-struct;

define open C-subtype <GZlibCompressor> (<GObject>)
end C-subtype;

define C-pointer-type <GZlibCompressor*> => <GZlibCompressor>;

define property-getter zlibcompressor-file-info :: <GFileInfo> on <GZlibCompressor> end;
define property-setter zlibcompressor-file-info :: <GFileInfo> on <GZlibCompressor> end;
define property-getter zlibcompressor-format :: <GZlibCompressorFormat> on <GZlibCompressor> end;
define property-setter zlibcompressor-format :: <GZlibCompressorFormat> on <GZlibCompressor> end;
define property-getter zlibcompressor-level :: <C-signed-int> on <GZlibCompressor> end;
define property-setter zlibcompressor-level :: <C-signed-int> on <GZlibCompressor> end;
define C-function g-zlib-compressor-new
  input parameter format_ :: <GZlibCompressorFormat>;
  input parameter level_ :: <C-signed-int>;
  result res :: <GZlibCompressor>;
  c-name: "g_zlib_compressor_new";
end;

define C-function g-zlib-compressor-get-file-info
  input parameter self :: <GZlibCompressor>;
  result res :: <GFileInfo>;
  c-name: "g_zlib_compressor_get_file_info";
end;

define C-function g-zlib-compressor-set-file-info
  input parameter self :: <GZlibCompressor>;
  input parameter file_info_ :: <GFileInfo>;
  c-name: "g_zlib_compressor_set_file_info";
end;

define C-struct <_GZlibCompressorClass>
  constant slot gzlibcompressorclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GZlibCompressorClass>;
end C-struct;

define constant $G-ZLIB-COMPRESSOR-FORMAT-ZLIB = 0;
define constant $G-ZLIB-COMPRESSOR-FORMAT-GZIP = 1;
define constant $G-ZLIB-COMPRESSOR-FORMAT-RAW = 2;
define constant <GZlibCompressorFormat> = <C-int>;
define C-pointer-type <GZlibCompressorFormat*> => <GZlibCompressorFormat>;

define open C-subtype <GZlibDecompressor> (<GObject>)
end C-subtype;

define C-pointer-type <GZlibDecompressor*> => <GZlibDecompressor>;

define property-getter zlibdecompressor-file-info :: <GFileInfo> on <GZlibDecompressor> end;
define property-getter zlibdecompressor-format :: <GZlibCompressorFormat> on <GZlibDecompressor> end;
define property-setter zlibdecompressor-format :: <GZlibCompressorFormat> on <GZlibDecompressor> end;
define C-function g-zlib-decompressor-new
  input parameter format_ :: <GZlibCompressorFormat>;
  result res :: <GZlibDecompressor>;
  c-name: "g_zlib_decompressor_new";
end;

define C-function g-zlib-decompressor-get-file-info
  input parameter self :: <GZlibDecompressor>;
  result res :: <GFileInfo>;
  c-name: "g_zlib_decompressor_get_file_info";
end;

define C-struct <_GZlibDecompressorClass>
  constant slot gzlibdecompressorclass-parent-class :: <GObjectClass>;
  pointer-type-name: <GZlibDecompressorClass>;
end C-struct;

define C-function g-bus-get
  input parameter bus_type_ :: <GBusType>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_bus_get";
end;

define C-function g-bus-get-finish
  input parameter res_ :: <GAsyncResult>;
  result res :: <GDBusConnection>;
  c-name: "g_bus_get_finish";
end;

define C-function g-bus-get-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GDBusConnection>;
  c-name: "g_bus_get_sync";
end;

define C-function g-bus-own-name-on-connection-with-closures
  input parameter connection_ :: <GDBusConnection>;
  input parameter name_ :: <C-string>;
  input parameter flags_ :: <GBusNameOwnerFlags>;
  input parameter name_acquired_closure_ :: <GClosure>;
  input parameter name_lost_closure_ :: <GClosure>;
  result res :: <C-unsigned-int>;
  c-name: "g_bus_own_name_on_connection_with_closures";
end;

define C-function g-bus-own-name-with-closures
  input parameter bus_type_ :: <GBusType>;
  input parameter name_ :: <C-string>;
  input parameter flags_ :: <GBusNameOwnerFlags>;
  input parameter bus_acquired_closure_ :: <GClosure>;
  input parameter name_acquired_closure_ :: <GClosure>;
  input parameter name_lost_closure_ :: <GClosure>;
  result res :: <C-unsigned-int>;
  c-name: "g_bus_own_name_with_closures";
end;

define C-function g-bus-unown-name
  input parameter owner_id_ :: <C-unsigned-int>;
  c-name: "g_bus_unown_name";
end;

define C-function g-bus-unwatch-name
  input parameter watcher_id_ :: <C-unsigned-int>;
  c-name: "g_bus_unwatch_name";
end;

define C-function g-bus-watch-name-on-connection-with-closures
  input parameter connection_ :: <GDBusConnection>;
  input parameter name_ :: <C-string>;
  input parameter flags_ :: <GBusNameWatcherFlags>;
  input parameter name_appeared_closure_ :: <GClosure>;
  input parameter name_vanished_closure_ :: <GClosure>;
  result res :: <C-unsigned-int>;
  c-name: "g_bus_watch_name_on_connection_with_closures";
end;

define C-function g-bus-watch-name-with-closures
  input parameter bus_type_ :: <GBusType>;
  input parameter name_ :: <C-string>;
  input parameter flags_ :: <GBusNameWatcherFlags>;
  input parameter name_appeared_closure_ :: <GClosure>;
  input parameter name_vanished_closure_ :: <GClosure>;
  result res :: <C-unsigned-int>;
  c-name: "g_bus_watch_name_with_closures";
end;

define C-function g-content-type-can-be-executable
  input parameter type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_content_type_can_be_executable";
end;

define C-function g-content-type-equals
  input parameter type1_ :: <C-string>;
  input parameter type2_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_content_type_equals";
end;

define C-function g-content-type-from-mime-type
  input parameter mime_type_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_content_type_from_mime_type";
end;

define C-function g-content-type-get-description
  input parameter type_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_content_type_get_description";
end;

define C-function g-content-type-get-generic-icon-name
  input parameter type_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_content_type_get_generic_icon_name";
end;

define C-function g-content-type-get-icon
  input parameter type_ :: <C-string>;
  result res :: <GIcon>;
  c-name: "g_content_type_get_icon";
end;

define C-function g-content-type-get-mime-type
  input parameter type_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_content_type_get_mime_type";
end;

define C-function g-content-type-get-symbolic-icon
  input parameter type_ :: <C-string>;
  result res :: <GIcon>;
  c-name: "g_content_type_get_symbolic_icon";
end;

define C-function g-content-type-guess
  input parameter filename_ :: <C-string>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter data_size_ :: <C-unsigned-long>;
  output parameter result_uncertain_ :: <C-int*>;
  result res :: <C-string>;
  c-name: "g_content_type_guess";
end;

define C-function g-content-type-guess-for-tree
  input parameter root_ :: <GFile>;
  result res :: <C-string*>;
  c-name: "g_content_type_guess_for_tree";
end;

define C-function g-content-type-is-a
  input parameter type_ :: <C-string>;
  input parameter supertype_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_content_type_is_a";
end;

define C-function g-content-type-is-unknown
  input parameter type_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_content_type_is_unknown";
end;

define C-function g-content-types-get-registered
  result res :: <GList>;
  c-name: "g_content_types_get_registered";
end;

define C-function g-dbus-address-escape-value
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_dbus_address_escape_value";
end;

define C-function g-dbus-address-get-for-bus-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-string>;
  c-name: "g_dbus_address_get_for_bus_sync";
end;

define C-function g-dbus-address-get-stream
  input parameter address_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_dbus_address_get_stream";
end;

define C-function g-dbus-address-get-stream-finish
  input parameter res_ :: <GAsyncResult>;
  input parameter out_guid_ :: <C-string>;
  result res :: <GIOStream>;
  c-name: "g_dbus_address_get_stream_finish";
end;

define C-function g-dbus-address-get-stream-sync
  input parameter address_ :: <C-string>;
  input parameter out_guid_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GIOStream>;
  c-name: "g_dbus_address_get_stream_sync";
end;

define C-function g-dbus-error-encode-gerror
  input parameter error_ :: <GError>;
  result res :: <C-string>;
  c-name: "g_dbus_error_encode_gerror";
end;

define C-function g-dbus-error-get-remote-error
  input parameter error_ :: <GError>;
  result res :: <C-string>;
  c-name: "g_dbus_error_get_remote_error";
end;

define C-function g-dbus-error-is-remote-error
  input parameter error_ :: <GError>;
  result res :: <C-boolean>;
  c-name: "g_dbus_error_is_remote_error";
end;

define C-function g-dbus-error-new-for-dbus-error
  input parameter dbus_error_name_ :: <C-string>;
  input parameter dbus_error_message_ :: <C-string>;
  result res :: <GError>;
  c-name: "g_dbus_error_new_for_dbus_error";
end;

define C-function g-dbus-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_error_quark";
end;

define C-function g-dbus-error-register-error
  input parameter error_domain_ :: <C-unsigned-int>;
  input parameter error_code_ :: <C-signed-int>;
  input parameter dbus_error_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_error_register_error";
end;

define C-function g-dbus-error-register-error-domain
  input parameter error_domain_quark_name_ :: <C-string>;
  input parameter quark_volatile_ :: <C-unsigned-long*>;
  input parameter entries_ :: <GDBusErrorEntry>;
  input parameter num_entries_ :: <C-unsigned-int>;
  c-name: "g_dbus_error_register_error_domain";
end;

define C-function g-dbus-error-strip-remote-error
  input parameter error_ :: <GError>;
  result res :: <C-boolean>;
  c-name: "g_dbus_error_strip_remote_error";
end;

define C-function g-dbus-error-unregister-error
  input parameter error_domain_ :: <C-unsigned-int>;
  input parameter error_code_ :: <C-signed-int>;
  input parameter dbus_error_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_error_unregister_error";
end;

define C-function g-dbus-generate-guid
  result res :: <C-string>;
  c-name: "g_dbus_generate_guid";
end;

define C-function g-dbus-gvalue-to-gvariant
  input parameter gvalue_ :: <GValue>;
  input parameter type_ :: <GVariantType>;
  result res :: <GVariant>;
  c-name: "g_dbus_gvalue_to_gvariant";
end;

define C-function g-dbus-gvariant-to-gvalue
  input parameter value_ :: <GVariant>;
  output parameter out_gvalue_ :: <GValue>;
  c-name: "g_dbus_gvariant_to_gvalue";
end;

define C-function g-dbus-is-address
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_address";
end;

define C-function g-dbus-is-guid
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_guid";
end;

define C-function g-dbus-is-interface-name
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_interface_name";
end;

define C-function g-dbus-is-member-name
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_member_name";
end;

define C-function g-dbus-is-name
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_name";
end;

define C-function g-dbus-is-supported-address
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_supported_address";
end;

define C-function g-dbus-is-unique-name
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_dbus_is_unique_name";
end;

define C-function g-io-error-from-errno
  input parameter err_no_ :: <C-signed-int>;
  result res :: <GIOErrorEnum>;
  c-name: "g_io_error_from_errno";
end;

define C-function g-io-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_io_error_quark";
end;

define C-function g-io-modules-load-all-in-directory
  input parameter dirname_ :: <C-string>;
  result res :: <GList>;
  c-name: "g_io_modules_load_all_in_directory";
end;

define C-function g-io-modules-load-all-in-directory-with-scope
  input parameter dirname_ :: <C-string>;
  input parameter scope_ :: <GIOModuleScope>;
  result res :: <GList>;
  c-name: "g_io_modules_load_all_in_directory_with_scope";
end;

define C-function g-io-modules-scan-all-in-directory
  input parameter dirname_ :: <C-string>;
  c-name: "g_io_modules_scan_all_in_directory";
end;

define C-function g-io-modules-scan-all-in-directory-with-scope
  input parameter dirname_ :: <C-string>;
  input parameter scope_ :: <GIOModuleScope>;
  c-name: "g_io_modules_scan_all_in_directory_with_scope";
end;

define C-function g-networking-init
  c-name: "g_networking_init";
end;

define C-function g-pollable-source-new
  input parameter pollable_stream_ :: <GObject>;
  result res :: <GSource>;
  c-name: "g_pollable_source_new";
end;

define C-function g-pollable-source-new-full
  input parameter pollable_stream_ :: <GObject>;
  input parameter child_source_ :: <GSource>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GSource>;
  c-name: "g_pollable_source_new_full";
end;

define C-function g-pollable-stream-read
  input parameter stream_ :: <GInputStream>;
  input parameter buffer_ :: <C-void*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_stream_read";
end;

define C-function g-pollable-stream-write
  input parameter stream_ :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_stream_write";
end;

define C-function g-pollable-stream-write-all
  input parameter stream_ :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  output parameter bytes_written_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <C-boolean>;
  c-name: "g_pollable_stream_write_all";
end;

define C-function g-resolver-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_resolver_error_quark";
end;

define C-function g-resource-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_resource_error_quark";
end;

define C-function g-resources-enumerate-children
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <C-string*>;
  c-name: "g_resources_enumerate_children";
end;

define C-function g-resources-get-info
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter size_ :: <C-unsigned-long*>;
  output parameter flags_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_resources_get_info";
end;

define C-function g-resources-lookup-data
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <GBytes>;
  c-name: "g_resources_lookup_data";
end;

define C-function g-resources-open-stream
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  result res :: <GInputStream>;
  c-name: "g_resources_open_stream";
end;

define C-function g-simple-async-report-gerror-in-idle
  input parameter object_ :: <GObject>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter error_ :: <GError>;
  c-name: "g_simple_async_report_gerror_in_idle";
end;

define C-function g-tls-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_tls_error_quark";
end;

define C-function g-unix-is-mount-path-system-internal
  input parameter mount_path_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_unix_is_mount_path_system_internal";
end;

define C-function g-unix-mount-compare
  input parameter mount1_ :: <GUnixMountEntry>;
  input parameter mount2_ :: <GUnixMountEntry>;
  result res :: <C-signed-int>;
  c-name: "g_unix_mount_compare";
end;

define C-function g-unix-mount-free
  input parameter mount_entry_ :: <GUnixMountEntry>;
  c-name: "g_unix_mount_free";
end;

define C-function g-unix-mount-get-device-path
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-string>;
  c-name: "g_unix_mount_get_device_path";
end;

define C-function g-unix-mount-get-fs-type
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-string>;
  c-name: "g_unix_mount_get_fs_type";
end;

define C-function g-unix-mount-get-mount-path
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-string>;
  c-name: "g_unix_mount_get_mount_path";
end;

define C-function g-unix-mount-guess-can-eject
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_guess_can_eject";
end;

define C-function g-unix-mount-guess-icon
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <GIcon>;
  c-name: "g_unix_mount_guess_icon";
end;

define C-function g-unix-mount-guess-name
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-string>;
  c-name: "g_unix_mount_guess_name";
end;

define C-function g-unix-mount-guess-should-display
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_guess_should_display";
end;

define C-function g-unix-mount-guess-symbolic-icon
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <GIcon>;
  c-name: "g_unix_mount_guess_symbolic_icon";
end;

define C-function g-unix-mount-is-readonly
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_is_readonly";
end;

define C-function g-unix-mount-is-system-internal
  input parameter mount_entry_ :: <GUnixMountEntry>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_is_system_internal";
end;

define C-function g-unix-mount-points-changed-since
  input parameter time_ :: <C-unsigned-long>;
  result res :: <C-boolean>;
  c-name: "g_unix_mount_points_changed_since";
end;

define C-function g-unix-mounts-changed-since
  input parameter time_ :: <C-unsigned-long>;
  result res :: <C-boolean>;
  c-name: "g_unix_mounts_changed_since";
end;

