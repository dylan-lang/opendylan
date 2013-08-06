module: gio
synopsis: generated bindings for the Gio library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;
ignore(<C-void**>);

define C-pointer-type <GError*> => <GError>;
ignore(<GError*>);

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
  slot g-action-entry-name :: <C-string>;
  constant slot g-action-entry-activate :: <C-function-pointer>;
  slot g-action-entry-parameter-type :: <C-string>;
  slot g-action-entry-state :: <C-string>;
  constant slot g-action-entry-change-state :: <C-function-pointer>;
  constant slot g-action-entry-padding :: <C-unsigned-long*>;
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
  constant slot g-action-group-interface-g-iface :: <GTypeInterface>;
  constant slot g-action-group-interface-has-action :: <C-function-pointer>;
  constant slot g-action-group-interface-list-actions :: <C-function-pointer>;
  constant slot g-action-group-interface-get-action-enabled :: <C-function-pointer>;
  constant slot g-action-group-interface-get-action-parameter-type :: <C-function-pointer>;
  constant slot g-action-group-interface-get-action-state-type :: <C-function-pointer>;
  constant slot g-action-group-interface-get-action-state-hint :: <C-function-pointer>;
  constant slot g-action-group-interface-get-action-state :: <C-function-pointer>;
  constant slot g-action-group-interface-change-action-state :: <C-function-pointer>;
  constant slot g-action-group-interface-activate-action :: <C-function-pointer>;
  constant slot g-action-group-interface-action-added :: <C-function-pointer>;
  constant slot g-action-group-interface-action-removed :: <C-function-pointer>;
  constant slot g-action-group-interface-action-enabled-changed :: <C-function-pointer>;
  constant slot g-action-group-interface-action-state-changed :: <C-function-pointer>;
  constant slot g-action-group-interface-query-action :: <C-function-pointer>;
  pointer-type-name: <GActionGroupInterface>;
end C-struct;

define C-struct <_GActionInterface>
  constant slot g-action-interface-g-iface :: <GTypeInterface>;
  constant slot g-action-interface-get-name :: <C-function-pointer>;
  constant slot g-action-interface-get-parameter-type :: <C-function-pointer>;
  constant slot g-action-interface-get-state-type :: <C-function-pointer>;
  constant slot g-action-interface-get-state-hint :: <C-function-pointer>;
  constant slot g-action-interface-get-enabled :: <C-function-pointer>;
  constant slot g-action-interface-get-state :: <C-function-pointer>;
  constant slot g-action-interface-change-state :: <C-function-pointer>;
  constant slot g-action-interface-activate :: <C-function-pointer>;
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
  input parameter entries_ :: <GActionEntry>;
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
  constant slot g-action-map-interface-g-iface :: <GTypeInterface>;
  constant slot g-action-map-interface-lookup-action :: <C-function-pointer>;
  constant slot g-action-map-interface-add-action :: <C-function-pointer>;
  constant slot g-action-map-interface-remove-action :: <C-function-pointer>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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

define C-function g-app-info-launch
  input parameter self :: <GAppInfo>;
  input parameter files_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_app_info_launch";
end;

define C-function g-app-info-launch-uris
  input parameter self :: <GAppInfo>;
  input parameter uris_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_app_info_launch_uris";
end;

define C-function g-app-info-remove-supports-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_app_info_remove_supports_type";
end;

define C-function g-app-info-set-as-default-for-extension
  input parameter self :: <GAppInfo>;
  input parameter extension_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_app_info_set_as_default_for_extension";
end;

define C-function g-app-info-set-as-default-for-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_app_info_set_as_default_for_type";
end;

define C-function g-app-info-set-as-last-used-for-type
  input parameter self :: <GAppInfo>;
  input parameter content_type_ :: <C-string>;
  output parameter error_ :: <GError*>;
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

define constant $g-app-info-create-none = 0;
define constant $g-app-info-create-needs-terminal = 1;
define constant $g-app-info-create-supports-uris = 2;
define constant $g-app-info-create-supports-startup-notification = 4;
define constant <GAppInfoCreateFlags> = <C-int>;
define C-pointer-type <GAppInfoCreateFlags*> => <GAppInfoCreateFlags>;

define C-struct <_GAppInfoIface>
  constant slot g-app-info-iface-g-iface :: <GTypeInterface>;
  constant slot g-app-info-iface-dup :: <C-function-pointer>;
  constant slot g-app-info-iface-equal :: <C-function-pointer>;
  constant slot g-app-info-iface-get-id :: <C-function-pointer>;
  constant slot g-app-info-iface-get-name :: <C-function-pointer>;
  constant slot g-app-info-iface-get-description :: <C-function-pointer>;
  constant slot g-app-info-iface-get-executable :: <C-function-pointer>;
  constant slot g-app-info-iface-get-icon :: <C-function-pointer>;
  constant slot g-app-info-iface-launch :: <C-function-pointer>;
  constant slot g-app-info-iface-supports-uris :: <C-function-pointer>;
  constant slot g-app-info-iface-supports-files :: <C-function-pointer>;
  constant slot g-app-info-iface-launch-uris :: <C-function-pointer>;
  constant slot g-app-info-iface-should-show :: <C-function-pointer>;
  constant slot g-app-info-iface-set-as-default-for-type :: <C-function-pointer>;
  constant slot g-app-info-iface-set-as-default-for-extension :: <C-function-pointer>;
  constant slot g-app-info-iface-add-supports-type :: <C-function-pointer>;
  constant slot g-app-info-iface-can-remove-supports-type :: <C-function-pointer>;
  constant slot g-app-info-iface-remove-supports-type :: <C-function-pointer>;
  constant slot g-app-info-iface-can-delete :: <C-function-pointer>;
  constant slot g-app-info-iface-do-delete :: <C-function-pointer>;
  constant slot g-app-info-iface-get-commandline :: <C-function-pointer>;
  constant slot g-app-info-iface-get-display-name :: <C-function-pointer>;
  constant slot g-app-info-iface-set-as-last-used-for-type :: <C-function-pointer>;
  pointer-type-name: <GAppInfoIface>;
end C-struct;

define open C-subtype <GAppLaunchContext> (<GObject>)
  constant slot g-app-launch-context-parent-instance :: <GObject>;
  constant slot g-app-launch-context-priv :: <GAppLaunchContextPrivate>;
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
  constant slot g-app-launch-context-class-parent-class :: <GObjectClass>;
  constant slot g-app-launch-context-class-get-display :: <C-function-pointer>;
  constant slot g-app-launch-context-class-get-startup-notify-id :: <C-function-pointer>;
  constant slot g-app-launch-context-class-launch-failed :: <C-function-pointer>;
  constant slot g-app-launch-context-class-_g-reserved1 :: <C-void*>;
  constant slot g-app-launch-context-class-_g-reserved2 :: <C-void*>;
  constant slot g-app-launch-context-class-_g-reserved3 :: <C-void*>;
  constant slot g-app-launch-context-class-_g-reserved4 :: <C-void*>;
  constant slot g-app-launch-context-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GAppLaunchContextClass>;
end C-struct;

define C-struct <_GAppLaunchContextPrivate>
  pointer-type-name: <GAppLaunchContextPrivate>;
end C-struct;

define open C-subtype <GApplication> (<GObject>)
  constant slot g-application-parent-instance :: <GObject>;
  constant slot g-application-priv :: <GApplicationPrivate>;
end C-subtype;

define C-pointer-type <GApplication*> => <GApplication>;

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
  output parameter error_ :: <GError*>;
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
  constant slot g-application-class-parent-class :: <GObjectClass>;
  constant slot g-application-class-startup :: <C-function-pointer>;
  constant slot g-application-class-activate :: <C-function-pointer>;
  constant slot g-application-class-open :: <C-function-pointer>;
  constant slot g-application-class-command-line :: <C-function-pointer>;
  constant slot g-application-class-local-command-line :: <C-function-pointer>;
  constant slot g-application-class-before-emit :: <C-function-pointer>;
  constant slot g-application-class-after-emit :: <C-function-pointer>;
  constant slot g-application-class-add-platform-data :: <C-function-pointer>;
  constant slot g-application-class-quit-mainloop :: <C-function-pointer>;
  constant slot g-application-class-run-mainloop :: <C-function-pointer>;
  constant slot g-application-class-shutdown :: <C-function-pointer>;
  constant slot g-application-class-padding :: <C-void*>;
  pointer-type-name: <GApplicationClass>;
end C-struct;

define open C-subtype <GApplicationCommandLine> (<GObject>)
  constant slot g-application-command-line-parent-instance :: <GObject>;
  constant slot g-application-command-line-priv :: <GApplicationCommandLinePrivate>;
end C-subtype;

define C-pointer-type <GApplicationCommandLine*> => <GApplicationCommandLine>;

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
  constant slot g-application-command-line-class-parent-class :: <GObjectClass>;
  constant slot g-application-command-line-class-print-literal :: <C-function-pointer>;
  constant slot g-application-command-line-class-printerr-literal :: <C-function-pointer>;
  constant slot g-application-command-line-class-padding :: <C-void*>;
  pointer-type-name: <GApplicationCommandLineClass>;
end C-struct;

define C-struct <_GApplicationCommandLinePrivate>
  pointer-type-name: <GApplicationCommandLinePrivate>;
end C-struct;

define constant $g-application-flags-none = 0;
define constant $g-application-is-service = 1;
define constant $g-application-is-launcher = 2;
define constant $g-application-handles-open = 4;
define constant $g-application-handles-command-line = 8;
define constant $g-application-send-environment = 16;
define constant $g-application-non-unique = 32;
define constant <GApplicationFlags> = <C-int>;
define C-pointer-type <GApplicationFlags*> => <GApplicationFlags>;

define C-struct <_GApplicationPrivate>
  pointer-type-name: <GApplicationPrivate>;
end C-struct;

define constant $g-ask-password-need-password = 1;
define constant $g-ask-password-need-username = 2;
define constant $g-ask-password-need-domain = 4;
define constant $g-ask-password-saving-supported = 8;
define constant $g-ask-password-anonymous-supported = 16;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_async_initable_init_finish";
end;

define C-function g-async-initable-new-finish
  input parameter self :: <GAsyncInitable>;
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GObject>;
  c-name: "g_async_initable_new_finish";
end;

define C-struct <_GAsyncInitableIface>
  constant slot g-async-initable-iface-g-iface :: <GTypeInterface>;
  constant slot g-async-initable-iface-init-async :: <C-function-pointer>;
  constant slot g-async-initable-iface-init-finish :: <C-function-pointer>;
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

define C-struct <_GAsyncResultIface>
  constant slot g-async-result-iface-g-iface :: <GTypeInterface>;
  constant slot g-async-result-iface-get-user-data :: <C-function-pointer>;
  constant slot g-async-result-iface-get-source-object :: <C-function-pointer>;
  pointer-type-name: <GAsyncResultIface>;
end C-struct;

define open C-subtype <GBufferedInputStream> (<GFilterInputStream>)
  constant slot g-buffered-input-stream-parent-instance :: <GFilterInputStream>;
  constant slot g-buffered-input-stream-priv :: <GBufferedInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GBufferedInputStream*> => <GBufferedInputStream>;

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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_buffered_input_stream_read_byte";
end;

define C-function g-buffered-input-stream-set-buffer-size
  input parameter self :: <GBufferedInputStream>;
  input parameter size_ :: <C-unsigned-long>;
  c-name: "g_buffered_input_stream_set_buffer_size";
end;

define C-struct <_GBufferedInputStreamClass>
  constant slot g-buffered-input-stream-class-parent-class :: <GFilterInputStreamClass>;
  constant slot g-buffered-input-stream-class-fill :: <C-function-pointer>;
  constant slot g-buffered-input-stream-class-fill-async :: <C-function-pointer>;
  constant slot g-buffered-input-stream-class-fill-finish :: <C-function-pointer>;
  constant slot g-buffered-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-buffered-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-buffered-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-buffered-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-buffered-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GBufferedInputStreamClass>;
end C-struct;

define C-struct <_GBufferedInputStreamPrivate>
  pointer-type-name: <GBufferedInputStreamPrivate>;
end C-struct;

define open C-subtype <GBufferedOutputStream> (<GFilterOutputStream>)
  constant slot g-buffered-output-stream-parent-instance :: <GFilterOutputStream>;
  constant slot g-buffered-output-stream-priv :: <GBufferedOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GBufferedOutputStream*> => <GBufferedOutputStream>;

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
  constant slot g-buffered-output-stream-class-parent-class :: <GFilterOutputStreamClass>;
  constant slot g-buffered-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-buffered-output-stream-class-_g-reserved2 :: <C-void*>;
  pointer-type-name: <GBufferedOutputStreamClass>;
end C-struct;

define C-struct <_GBufferedOutputStreamPrivate>
  pointer-type-name: <GBufferedOutputStreamPrivate>;
end C-struct;

define constant $g-bus-name-owner-flags-none = 0;
define constant $g-bus-name-owner-flags-allow-replacement = 1;
define constant $g-bus-name-owner-flags-replace = 2;
define constant <GBusNameOwnerFlags> = <C-int>;
define C-pointer-type <GBusNameOwnerFlags*> => <GBusNameOwnerFlags>;

define constant $g-bus-name-watcher-flags-none = 0;
define constant $g-bus-name-watcher-flags-auto-start = 1;
define constant <GBusNameWatcherFlags> = <C-int>;
define C-pointer-type <GBusNameWatcherFlags*> => <GBusNameWatcherFlags>;

define constant $g-bus-type-starter = -1;
define constant $g-bus-type-none = 0;
define constant $g-bus-type-system = 1;
define constant $g-bus-type-session = 2;
define constant <GBusType> = <C-int>;
define C-pointer-type <GBusType*> => <GBusType>;

define open C-subtype <GCancellable> (<GObject>)
  constant slot g-cancellable-parent-instance :: <GObject>;
  constant slot g-cancellable-priv :: <GCancellablePrivate>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_cancellable_set_error_if_cancelled";
end;

define C-struct <_GCancellableClass>
  constant slot g-cancellable-class-parent-class :: <GObjectClass>;
  constant slot g-cancellable-class-cancelled :: <C-function-pointer>;
  constant slot g-cancellable-class-_g-reserved1 :: <C-void*>;
  constant slot g-cancellable-class-_g-reserved2 :: <C-void*>;
  constant slot g-cancellable-class-_g-reserved3 :: <C-void*>;
  constant slot g-cancellable-class-_g-reserved4 :: <C-void*>;
  constant slot g-cancellable-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GCancellableClass>;
end C-struct;

define C-struct <_GCancellablePrivate>
  pointer-type-name: <GCancellablePrivate>;
end C-struct;

define open C-subtype <GCharsetConverter> (<GObject>)
end C-subtype;

define C-pointer-type <GCharsetConverter*> => <GCharsetConverter>;

define C-function g-charset-converter-new
  input parameter to_charset_ :: <C-string>;
  input parameter from_charset_ :: <C-string>;
  output parameter error_ :: <GError*>;
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
  constant slot g-charset-converter-class-parent-class :: <GObjectClass>;
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
  output parameter error_ :: <GError*>;
  result res :: <GConverterResult>;
  c-name: "g_converter_convert";
end;

define C-function g-converter-reset
  input parameter self :: <GConverter>;
  c-name: "g_converter_reset";
end;

define constant $g-converter-no-flags = 0;
define constant $g-converter-input-at-end = 1;
define constant $g-converter-flush = 2;
define constant <GConverterFlags> = <C-int>;
define C-pointer-type <GConverterFlags*> => <GConverterFlags>;

define C-struct <_GConverterIface>
  constant slot g-converter-iface-g-iface :: <GTypeInterface>;
  constant slot g-converter-iface-convert :: <C-function-pointer>;
  constant slot g-converter-iface-reset :: <C-function-pointer>;
  pointer-type-name: <GConverterIface>;
end C-struct;

define open C-subtype <GConverterInputStream> (<GFilterInputStream>)
  constant slot g-converter-input-stream-parent-instance :: <GFilterInputStream>;
  constant slot g-converter-input-stream-priv :: <GConverterInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GConverterInputStream*> => <GConverterInputStream>;

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
  constant slot g-converter-input-stream-class-parent-class :: <GFilterInputStreamClass>;
  constant slot g-converter-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-converter-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-converter-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-converter-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-converter-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GConverterInputStreamClass>;
end C-struct;

define C-struct <_GConverterInputStreamPrivate>
  pointer-type-name: <GConverterInputStreamPrivate>;
end C-struct;

define open C-subtype <GConverterOutputStream> (<GFilterOutputStream>)
  constant slot g-converter-output-stream-parent-instance :: <GFilterOutputStream>;
  constant slot g-converter-output-stream-priv :: <GConverterOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GConverterOutputStream*> => <GConverterOutputStream>;

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
  constant slot g-converter-output-stream-class-parent-class :: <GFilterOutputStreamClass>;
  constant slot g-converter-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-converter-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-converter-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-converter-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-converter-output-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GConverterOutputStreamClass>;
end C-struct;

define C-struct <_GConverterOutputStreamPrivate>
  pointer-type-name: <GConverterOutputStreamPrivate>;
end C-struct;

define constant $g-converter-error = 0;
define constant $g-converter-converted = 1;
define constant $g-converter-finished = 2;
define constant $g-converter-flushed = 3;
define constant <GConverterResult> = <C-int>;
define C-pointer-type <GConverterResult*> => <GConverterResult>;

define open C-subtype <GCredentials> (<GObject>)
end C-subtype;

define C-pointer-type <GCredentials*> => <GCredentials>;

define C-function g-credentials-new
  result res :: <GCredentials>;
  c-name: "g_credentials_new";
end;

define C-function g-credentials-get-unix-user
  input parameter self :: <GCredentials>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "g_credentials_get_unix_user";
end;

define C-function g-credentials-is-same-user
  input parameter self :: <GCredentials>;
  input parameter other_credentials_ :: <GCredentials>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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

define constant $g-credentials-type-invalid = 0;
define constant $g-credentials-type-linux-ucred = 1;
define constant $g-credentials-type-freebsd-cmsgcred = 2;
define constant $g-credentials-type-openbsd-sockpeercred = 3;
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
  slot g-d-bus-annotation-info-ref-count :: <C-signed-int>;
  slot g-d-bus-annotation-info-key :: <C-string>;
  slot g-d-bus-annotation-info-value :: <C-string>;
  slot g-d-bus-annotation-info-annotations :: <C-unsigned-char*> /* Not supported */;
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
  slot g-d-bus-arg-info-ref-count :: <C-signed-int>;
  slot g-d-bus-arg-info-name :: <C-string>;
  slot g-d-bus-arg-info-signature :: <C-string>;
  slot g-d-bus-arg-info-annotations :: <C-unsigned-char*> /* Not supported */;
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

define C-function g-dbus-auth-observer-authorize-authenticated-peer
  input parameter self :: <GDBusAuthObserver>;
  input parameter stream_ :: <GIOStream>;
  input parameter credentials_ :: <GCredentials>;
  result res :: <C-boolean>;
  c-name: "g_dbus_auth_observer_authorize_authenticated_peer";
end;

define constant $g-dbus-call-flags-none = 0;
define constant $g-dbus-call-flags-no-auto-start = 1;
define constant <GDBusCallFlags> = <C-int>;
define C-pointer-type <GDBusCallFlags*> => <GDBusCallFlags>;

define constant $g-dbus-capability-flags-none = 0;
define constant $g-dbus-capability-flags-unix-fd-passing = 1;
define constant <GDBusCapabilityFlags> = <C-int>;
define C-pointer-type <GDBusCapabilityFlags*> => <GDBusCapabilityFlags>;

define open C-subtype <GDBusConnection> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusConnection*> => <GDBusConnection>;

define C-function g-dbus-connection-new-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_finish";
end;

define C-function g-dbus-connection-new-for-address-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_for_address_finish";
end;

define C-function g-dbus-connection-new-for-address-sync
  input parameter address_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GDBusConnection>;
  c-name: "g_dbus_connection_new_for_address_sync";
end;

define C-function g-dbus-connection-new-sync
  input parameter stream_ :: <GIOStream>;
  input parameter guid_ :: <C-string>;
  input parameter flags_ :: <GDBusConnectionFlags>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_close_finish";
end;

define C-function g-dbus-connection-close-sync
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_emit_signal";
end;

define C-function g-dbus-connection-export-action-group
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter action_group_ :: <GActionGroup>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "g_dbus_connection_export_action_group";
end;

define C-function g-dbus-connection-export-menu-model
  input parameter self :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  input parameter menu_ :: <GMenuModel>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_dbus_connection_flush_finish";
end;

define C-function g-dbus-connection-flush-sync
  input parameter self :: <GDBusConnection>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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

define constant $g-dbus-connection-flags-none = 0;
define constant $g-dbus-connection-flags-authentication-client = 1;
define constant $g-dbus-connection-flags-authentication-server = 2;
define constant $g-dbus-connection-flags-authentication-allow-anonymous = 4;
define constant $g-dbus-connection-flags-message-bus-connection = 8;
define constant $g-dbus-connection-flags-delay-message-processing = 16;
define constant <GDBusConnectionFlags> = <C-int>;
define C-pointer-type <GDBusConnectionFlags*> => <GDBusConnectionFlags>;

define constant $g-dbus-error-failed = 0;
define constant $g-dbus-error-no-memory = 1;
define constant $g-dbus-error-service-unknown = 2;
define constant $g-dbus-error-name-has-no-owner = 3;
define constant $g-dbus-error-no-reply = 4;
define constant $g-dbus-error-io-error = 5;
define constant $g-dbus-error-bad-address = 6;
define constant $g-dbus-error-not-supported = 7;
define constant $g-dbus-error-limits-exceeded = 8;
define constant $g-dbus-error-access-denied = 9;
define constant $g-dbus-error-auth-failed = 10;
define constant $g-dbus-error-no-server = 11;
define constant $g-dbus-error-timeout = 12;
define constant $g-dbus-error-no-network = 13;
define constant $g-dbus-error-address-in-use = 14;
define constant $g-dbus-error-disconnected = 15;
define constant $g-dbus-error-invalid-args = 16;
define constant $g-dbus-error-file-not-found = 17;
define constant $g-dbus-error-file-exists = 18;
define constant $g-dbus-error-unknown-method = 19;
define constant $g-dbus-error-timed-out = 20;
define constant $g-dbus-error-match-rule-not-found = 21;
define constant $g-dbus-error-match-rule-invalid = 22;
define constant $g-dbus-error-spawn-exec-failed = 23;
define constant $g-dbus-error-spawn-fork-failed = 24;
define constant $g-dbus-error-spawn-child-exited = 25;
define constant $g-dbus-error-spawn-child-signaled = 26;
define constant $g-dbus-error-spawn-failed = 27;
define constant $g-dbus-error-spawn-setup-failed = 28;
define constant $g-dbus-error-spawn-config-invalid = 29;
define constant $g-dbus-error-spawn-service-invalid = 30;
define constant $g-dbus-error-spawn-service-not-found = 31;
define constant $g-dbus-error-spawn-permissions-invalid = 32;
define constant $g-dbus-error-spawn-file-invalid = 33;
define constant $g-dbus-error-spawn-no-memory = 34;
define constant $g-dbus-error-unix-process-id-unknown = 35;
define constant $g-dbus-error-invalid-signature = 36;
define constant $g-dbus-error-invalid-file-content = 37;
define constant $g-dbus-error-selinux-security-context-unknown = 38;
define constant $g-dbus-error-adt-audit-data-unknown = 39;
define constant $g-dbus-error-object-path-in-use = 40;
define constant <GDBusError> = <C-int>;
define C-pointer-type <GDBusError*> => <GDBusError>;

define C-struct <_GDBusErrorEntry>
  slot g-d-bus-error-entry-error-code :: <C-signed-int>;
  slot g-d-bus-error-entry-dbus-error-name :: <C-string>;
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
  constant slot g-d-bus-interface-iface-parent-iface :: <GTypeInterface>;
  constant slot g-d-bus-interface-iface-get-info :: <C-function-pointer>;
  constant slot g-d-bus-interface-iface-get-object :: <C-function-pointer>;
  constant slot g-d-bus-interface-iface-set-object :: <C-function-pointer>;
  constant slot g-d-bus-interface-iface-dup-object :: <C-function-pointer>;
  pointer-type-name: <GDBusInterfaceIface>;
end C-struct;

define C-struct <_GDBusInterfaceInfo>
  slot g-d-bus-interface-info-ref-count :: <C-signed-int>;
  slot g-d-bus-interface-info-name :: <C-string>;
  slot g-d-bus-interface-info-methods :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-interface-info-signals :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-interface-info-properties :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-interface-info-annotations :: <C-unsigned-char*> /* Not supported */;
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
  constant slot g-d-bus-interface-skeleton-parent-instance :: <GObject>;
  constant slot g-d-bus-interface-skeleton-priv :: <GDBusInterfaceSkeletonPrivate>;
end C-subtype;

define C-pointer-type <GDBusInterfaceSkeleton*> => <GDBusInterfaceSkeleton>;

define C-function g-dbus-interface-skeleton-export
  input parameter self :: <GDBusInterfaceSkeleton>;
  input parameter connection_ :: <GDBusConnection>;
  input parameter object_path_ :: <C-string>;
  output parameter error_ :: <GError*>;
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
  constant slot g-d-bus-interface-skeleton-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-interface-skeleton-class-get-info :: <C-function-pointer>;
  constant slot g-d-bus-interface-skeleton-class-get-vtable :: <C-void*>;
  constant slot g-d-bus-interface-skeleton-class-get-properties :: <C-function-pointer>;
  constant slot g-d-bus-interface-skeleton-class-flush :: <C-function-pointer>;
  constant slot g-d-bus-interface-skeleton-class-vfunc-padding :: <C-void*>;
  constant slot g-d-bus-interface-skeleton-class-g-authorize-method :: <C-function-pointer>;
  constant slot g-d-bus-interface-skeleton-class-signal-padding :: <C-void*>;
  pointer-type-name: <GDBusInterfaceSkeletonClass>;
end C-struct;

define constant $g-dbus-interface-skeleton-flags-none = 0;
define constant $g-dbus-interface-skeleton-flags-handle-method-invocations-in-thread = 1;
define constant <GDBusInterfaceSkeletonFlags> = <C-int>;
define C-pointer-type <GDBusInterfaceSkeletonFlags*> => <GDBusInterfaceSkeletonFlags>;

define C-struct <_GDBusInterfaceSkeletonPrivate>
  pointer-type-name: <GDBusInterfaceSkeletonPrivate>;
end C-struct;

define C-struct <_GDBusInterfaceVTable>
  slot g-d-bus-interface-v-table-method-call :: <C-function-pointer>;
  slot g-d-bus-interface-v-table-get-property :: <C-function-pointer>;
  slot g-d-bus-interface-v-table-set-property :: <C-function-pointer>;
  constant slot g-d-bus-interface-v-table-padding :: <C-void*>;
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

define C-function g-dbus-message-new
  result res :: <GDBusMessage>;
  c-name: "g_dbus_message_new";
end;

define C-function g-dbus-message-new-from-blob
  input parameter blob_ :: <C-unsigned-char*>;
  input parameter blob_len_ :: <C-unsigned-long>;
  input parameter capabilities_ :: <GDBusCapabilityFlags>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_dbus_message_bytes_needed";
end;

define C-function g-dbus-message-copy
  input parameter self :: <GDBusMessage>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_dbus_message_to_blob";
end;

define C-function g-dbus-message-to-gerror
  input parameter self :: <GDBusMessage>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_dbus_message_to_gerror";
end;

define constant $g-dbus-message-byte-order-big-endian = 66;
define constant $g-dbus-message-byte-order-little-endian = 108;
define constant <GDBusMessageByteOrder> = <C-int>;
define C-pointer-type <GDBusMessageByteOrder*> => <GDBusMessageByteOrder>;

define constant $g-dbus-message-flags-none = 0;
define constant $g-dbus-message-flags-no-reply-expected = 1;
define constant $g-dbus-message-flags-no-auto-start = 2;
define constant <GDBusMessageFlags> = <C-int>;
define C-pointer-type <GDBusMessageFlags*> => <GDBusMessageFlags>;

define constant $g-dbus-message-header-field-invalid = 0;
define constant $g-dbus-message-header-field-path = 1;
define constant $g-dbus-message-header-field-interface = 2;
define constant $g-dbus-message-header-field-member = 3;
define constant $g-dbus-message-header-field-error-name = 4;
define constant $g-dbus-message-header-field-reply-serial = 5;
define constant $g-dbus-message-header-field-destination = 6;
define constant $g-dbus-message-header-field-sender = 7;
define constant $g-dbus-message-header-field-signature = 8;
define constant $g-dbus-message-header-field-num-unix-fds = 9;
define constant <GDBusMessageHeaderField> = <C-int>;
define C-pointer-type <GDBusMessageHeaderField*> => <GDBusMessageHeaderField>;

define constant $g-dbus-message-type-invalid = 0;
define constant $g-dbus-message-type-method-call = 1;
define constant $g-dbus-message-type-method-return = 2;
define constant $g-dbus-message-type-error = 3;
define constant $g-dbus-message-type-signal = 4;
define constant <GDBusMessageType> = <C-int>;
define C-pointer-type <GDBusMessageType*> => <GDBusMessageType>;

define C-struct <_GDBusMethodInfo>
  slot g-d-bus-method-info-ref-count :: <C-signed-int>;
  slot g-d-bus-method-info-name :: <C-string>;
  slot g-d-bus-method-info-in-args :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-method-info-out-args :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-method-info-annotations :: <C-unsigned-char*> /* Not supported */;
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
  slot g-d-bus-node-info-ref-count :: <C-signed-int>;
  slot g-d-bus-node-info-path :: <C-string>;
  slot g-d-bus-node-info-interfaces :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-node-info-nodes :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-node-info-annotations :: <C-unsigned-char*> /* Not supported */;
  pointer-type-name: <GDBusNodeInfo>;
end C-struct;

define C-function g-dbus-node-info-new-for-xml
  input parameter xml_data_ :: <C-string>;
  output parameter error_ :: <GError*>;
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
  constant slot g-d-bus-object-iface-parent-iface :: <GTypeInterface>;
  constant slot g-d-bus-object-iface-get-object-path :: <C-function-pointer>;
  constant slot g-d-bus-object-iface-get-interfaces :: <C-function-pointer>;
  constant slot g-d-bus-object-iface-get-interface :: <C-function-pointer>;
  constant slot g-d-bus-object-iface-interface-added :: <C-function-pointer>;
  constant slot g-d-bus-object-iface-interface-removed :: <C-function-pointer>;
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
  constant slot g-d-bus-object-manager-client-parent-instance :: <GObject>;
  constant slot g-d-bus-object-manager-client-priv :: <GDBusObjectManagerClientPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectManagerClient*> => <GDBusObjectManagerClient>;

define C-function g-dbus-object-manager-client-new-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GDBusObjectManagerClient>;
  c-name: "g_dbus_object_manager_client_new_finish";
end;

define C-function g-dbus-object-manager-client-new-for-bus-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-d-bus-object-manager-client-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-object-manager-client-class-interface-proxy-signal :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-client-class-interface-proxy-properties-changed :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-client-class-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectManagerClientClass>;
end C-struct;

define constant $g-dbus-object-manager-client-flags-none = 0;
define constant $g-dbus-object-manager-client-flags-do-not-auto-start = 1;
define constant <GDBusObjectManagerClientFlags> = <C-int>;
define C-pointer-type <GDBusObjectManagerClientFlags*> => <GDBusObjectManagerClientFlags>;

define C-struct <_GDBusObjectManagerClientPrivate>
  pointer-type-name: <GDBusObjectManagerClientPrivate>;
end C-struct;

define C-struct <_GDBusObjectManagerIface>
  constant slot g-d-bus-object-manager-iface-parent-iface :: <GTypeInterface>;
  constant slot g-d-bus-object-manager-iface-get-object-path :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-get-objects :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-get-object :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-get-interface :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-object-added :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-object-removed :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-interface-added :: <C-function-pointer>;
  constant slot g-d-bus-object-manager-iface-interface-removed :: <C-function-pointer>;
  pointer-type-name: <GDBusObjectManagerIface>;
end C-struct;

define open C-subtype <GDBusObjectManagerServer> (<GObject>)
  constant slot g-d-bus-object-manager-server-parent-instance :: <GObject>;
  constant slot g-d-bus-object-manager-server-priv :: <GDBusObjectManagerServerPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectManagerServer*> => <GDBusObjectManagerServer>;

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
  constant slot g-d-bus-object-manager-server-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-object-manager-server-class-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectManagerServerClass>;
end C-struct;

define C-struct <_GDBusObjectManagerServerPrivate>
  pointer-type-name: <GDBusObjectManagerServerPrivate>;
end C-struct;

define open C-subtype <GDBusObjectProxy> (<GObject>)
  constant slot g-d-bus-object-proxy-parent-instance :: <GObject>;
  constant slot g-d-bus-object-proxy-priv :: <GDBusObjectProxyPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectProxy*> => <GDBusObjectProxy>;

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
  constant slot g-d-bus-object-proxy-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-object-proxy-class-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectProxyClass>;
end C-struct;

define C-struct <_GDBusObjectProxyPrivate>
  pointer-type-name: <GDBusObjectProxyPrivate>;
end C-struct;

define open C-subtype <GDBusObjectSkeleton> (<GObject>)
  constant slot g-d-bus-object-skeleton-parent-instance :: <GObject>;
  constant slot g-d-bus-object-skeleton-priv :: <GDBusObjectSkeletonPrivate>;
end C-subtype;

define C-pointer-type <GDBusObjectSkeleton*> => <GDBusObjectSkeleton>;

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
  constant slot g-d-bus-object-skeleton-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-object-skeleton-class-authorize-method :: <C-function-pointer>;
  constant slot g-d-bus-object-skeleton-class-padding :: <C-void*>;
  pointer-type-name: <GDBusObjectSkeletonClass>;
end C-struct;

define C-struct <_GDBusObjectSkeletonPrivate>
  pointer-type-name: <GDBusObjectSkeletonPrivate>;
end C-struct;

define C-struct <_GDBusPropertyInfo>
  slot g-d-bus-property-info-ref-count :: <C-signed-int>;
  slot g-d-bus-property-info-name :: <C-string>;
  slot g-d-bus-property-info-signature :: <C-string>;
  slot g-d-bus-property-info-flags :: <GDBusPropertyInfoFlags>;
  slot g-d-bus-property-info-annotations :: <C-unsigned-char*> /* Not supported */;
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

define constant $g-dbus-property-info-flags-none = 0;
define constant $g-dbus-property-info-flags-readable = 1;
define constant $g-dbus-property-info-flags-writable = 2;
define constant <GDBusPropertyInfoFlags> = <C-int>;
define C-pointer-type <GDBusPropertyInfoFlags*> => <GDBusPropertyInfoFlags>;

define open C-subtype <GDBusProxy> (<GObject>)
  constant slot g-d-bus-proxy-parent-instance :: <GObject>;
  constant slot g-d-bus-proxy-priv :: <GDBusProxyPrivate>;
end C-subtype;

define C-pointer-type <GDBusProxy*> => <GDBusProxy>;

define C-function g-dbus-proxy-new-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GDBusProxy>;
  c-name: "g_dbus_proxy_new_finish";
end;

define C-function g-dbus-proxy-new-for-bus-finish
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-d-bus-proxy-class-parent-class :: <GObjectClass>;
  constant slot g-d-bus-proxy-class-g-properties-changed :: <C-function-pointer>;
  constant slot g-d-bus-proxy-class-g-signal :: <C-function-pointer>;
  constant slot g-d-bus-proxy-class-padding :: <C-void*>;
  pointer-type-name: <GDBusProxyClass>;
end C-struct;

define constant $g-dbus-proxy-flags-none = 0;
define constant $g-dbus-proxy-flags-do-not-load-properties = 1;
define constant $g-dbus-proxy-flags-do-not-connect-signals = 2;
define constant $g-dbus-proxy-flags-do-not-auto-start = 4;
define constant $g-dbus-proxy-flags-get-invalidated-properties = 8;
define constant <GDBusProxyFlags> = <C-int>;
define C-pointer-type <GDBusProxyFlags*> => <GDBusProxyFlags>;

define C-struct <_GDBusProxyPrivate>
  pointer-type-name: <GDBusProxyPrivate>;
end C-struct;

define constant $g-dbus-send-message-flags-none = 0;
define constant $g-dbus-send-message-flags-preserve-serial = 1;
define constant <GDBusSendMessageFlags> = <C-int>;
define C-pointer-type <GDBusSendMessageFlags*> => <GDBusSendMessageFlags>;

define open C-subtype <GDBusServer> (<GObject>)
end C-subtype;

define C-pointer-type <GDBusServer*> => <GDBusServer>;

define C-function g-dbus-server-new-sync
  input parameter address_ :: <C-string>;
  input parameter flags_ :: <GDBusServerFlags>;
  input parameter guid_ :: <C-string>;
  input parameter observer_ :: <GDBusAuthObserver>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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

define constant $g-dbus-server-flags-none = 0;
define constant $g-dbus-server-flags-run-in-thread = 1;
define constant $g-dbus-server-flags-authentication-allow-anonymous = 2;
define constant <GDBusServerFlags> = <C-int>;
define C-pointer-type <GDBusServerFlags*> => <GDBusServerFlags>;

define constant $g-dbus-signal-flags-none = 0;
define constant $g-dbus-signal-flags-no-match-rule = 1;
define constant <GDBusSignalFlags> = <C-int>;
define C-pointer-type <GDBusSignalFlags*> => <GDBusSignalFlags>;

define C-struct <_GDBusSignalInfo>
  slot g-d-bus-signal-info-ref-count :: <C-signed-int>;
  slot g-d-bus-signal-info-name :: <C-string>;
  slot g-d-bus-signal-info-args :: <C-unsigned-char*> /* Not supported */;
  slot g-d-bus-signal-info-annotations :: <C-unsigned-char*> /* Not supported */;
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

define constant $g-dbus-subtree-flags-none = 0;
define constant $g-dbus-subtree-flags-dispatch-to-unenumerated-nodes = 1;
define constant <GDBusSubtreeFlags> = <C-int>;
define C-pointer-type <GDBusSubtreeFlags*> => <GDBusSubtreeFlags>;

define C-struct <_GDBusSubtreeVTable>
  slot g-d-bus-subtree-v-table-enumerate :: <C-void*>;
  slot g-d-bus-subtree-v-table-introspect :: <C-function-pointer>;
  slot g-d-bus-subtree-v-table-dispatch :: <C-function-pointer>;
  constant slot g-d-bus-subtree-v-table-padding :: <C-void*>;
  pointer-type-name: <GDBusSubtreeVTable>;
end C-struct;

define constant $desktop-app-info-lookup-extension-point-name = "gio-desktop-app-info-lookup";

define open C-subtype <GDataInputStream> (<GBufferedInputStream>)
  constant slot g-data-input-stream-parent-instance :: <GBufferedInputStream>;
  constant slot g-data-input-stream-priv :: <GDataInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GDataInputStream*> => <GDataInputStream>;

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
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-char>;
  c-name: "g_data_input_stream_read_byte";
end;

define C-function g-data-input-stream-read-int16
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-short>;
  c-name: "g_data_input_stream_read_int16";
end;

define C-function g-data-input-stream-read-int32
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_data_input_stream_read_int32";
end;

define C-function g-data-input-stream-read-int64
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_data_input_stream_read_int64";
end;

define C-function g-data-input-stream-read-line
  input parameter self :: <GDataInputStream>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_data_input_stream_read_line_finish";
end;

define C-function g-data-input-stream-read-line-finish-utf8
  input parameter self :: <GDataInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_line_finish_utf8";
end;

define C-function g-data-input-stream-read-line-utf8
  input parameter self :: <GDataInputStream>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_line_utf8";
end;

define C-function g-data-input-stream-read-uint16
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-short>;
  c-name: "g_data_input_stream_read_uint16";
end;

define C-function g-data-input-stream-read-uint32
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int>;
  c-name: "g_data_input_stream_read_uint32";
end;

define C-function g-data-input-stream-read-uint64
  input parameter self :: <GDataInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-long>;
  c-name: "g_data_input_stream_read_uint64";
end;

define C-function g-data-input-stream-read-until
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_data_input_stream_read_until_finish";
end;

define C-function g-data-input-stream-read-upto
  input parameter self :: <GDataInputStream>;
  input parameter stop_chars_ :: <C-string>;
  input parameter stop_chars_len_ :: <C-signed-long>;
  output parameter length_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-data-input-stream-class-parent-class :: <GBufferedInputStreamClass>;
  constant slot g-data-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-data-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-data-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-data-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-data-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GDataInputStreamClass>;
end C-struct;

define C-struct <_GDataInputStreamPrivate>
  pointer-type-name: <GDataInputStreamPrivate>;
end C-struct;

define open C-subtype <GDataOutputStream> (<GFilterOutputStream>)
  constant slot g-data-output-stream-parent-instance :: <GFilterOutputStream>;
  constant slot g-data-output-stream-priv :: <GDataOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GDataOutputStream*> => <GDataOutputStream>;

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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_byte";
end;

define C-function g-data-output-stream-put-int16
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-short>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int16";
end;

define C-function g-data-output-stream-put-int32
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int32";
end;

define C-function g-data-output-stream-put-int64
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-signed-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_int64";
end;

define C-function g-data-output-stream-put-string
  input parameter self :: <GDataOutputStream>;
  input parameter str_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_string";
end;

define C-function g-data-output-stream-put-uint16
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint16";
end;

define C-function g-data-output-stream-put-uint32
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-int>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint32";
end;

define C-function g-data-output-stream-put-uint64
  input parameter self :: <GDataOutputStream>;
  input parameter data_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_data_output_stream_put_uint64";
end;

define C-function g-data-output-stream-set-byte-order
  input parameter self :: <GDataOutputStream>;
  input parameter order_ :: <GDataStreamByteOrder>;
  c-name: "g_data_output_stream_set_byte_order";
end;

define C-struct <_GDataOutputStreamClass>
  constant slot g-data-output-stream-class-parent-class :: <GFilterOutputStreamClass>;
  constant slot g-data-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-data-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-data-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-data-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-data-output-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GDataOutputStreamClass>;
end C-struct;

define C-struct <_GDataOutputStreamPrivate>
  pointer-type-name: <GDataOutputStreamPrivate>;
end C-struct;

define constant $g-data-stream-byte-order-big-endian = 0;
define constant $g-data-stream-byte-order-little-endian = 1;
define constant $g-data-stream-byte-order-host-endian = 2;
define constant <GDataStreamByteOrder> = <C-int>;
define C-pointer-type <GDataStreamByteOrder*> => <GDataStreamByteOrder>;

define constant $g-data-stream-newline-type-lf = 0;
define constant $g-data-stream-newline-type-cr = 1;
define constant $g-data-stream-newline-type-cr-lf = 2;
define constant $g-data-stream-newline-type-any = 3;
define constant <GDataStreamNewlineType> = <C-int>;
define C-pointer-type <GDataStreamNewlineType*> => <GDataStreamNewlineType>;

define open C-subtype <GDesktopAppInfo> (<GObject>)
end C-subtype;

define C-pointer-type <GDesktopAppInfo*> => <GDesktopAppInfo>;

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

define C-function g-desktop-app-info-launch-uris-as-manager
  input parameter self :: <GDesktopAppInfo>;
  input parameter uris_ :: <GList>;
  input parameter launch_context_ :: <GAppLaunchContext>;
  input parameter spawn_flags_ :: <GSpawnFlags>;
  input parameter user_setup_ :: <C-function-pointer>;
  input parameter user_setup_data_ :: <C-void*>;
  input parameter pid_callback_ :: <C-function-pointer>;
  input parameter pid_callback_data_ :: <C-void*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_desktop_app_info_launch_uris_as_manager";
end;

define C-struct <_GDesktopAppInfoClass>
  constant slot g-desktop-app-info-class-parent-class :: <GObjectClass>;
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
  constant slot g-desktop-app-info-lookup-iface-g-iface :: <GTypeInterface>;
  constant slot g-desktop-app-info-lookup-iface-get-default-for-uri-scheme :: <C-function-pointer>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_drive_stop_finish";
end;

define C-struct <_GDriveIface>
  constant slot g-drive-iface-g-iface :: <GTypeInterface>;
  constant slot g-drive-iface-changed :: <C-function-pointer>;
  constant slot g-drive-iface-disconnected :: <C-function-pointer>;
  constant slot g-drive-iface-eject-button :: <C-function-pointer>;
  constant slot g-drive-iface-get-name :: <C-function-pointer>;
  constant slot g-drive-iface-get-icon :: <C-function-pointer>;
  constant slot g-drive-iface-has-volumes :: <C-function-pointer>;
  constant slot g-drive-iface-get-volumes :: <C-function-pointer>;
  constant slot g-drive-iface-is-media-removable :: <C-function-pointer>;
  constant slot g-drive-iface-has-media :: <C-function-pointer>;
  constant slot g-drive-iface-is-media-check-automatic :: <C-function-pointer>;
  constant slot g-drive-iface-can-eject :: <C-function-pointer>;
  constant slot g-drive-iface-can-poll-for-media :: <C-function-pointer>;
  constant slot g-drive-iface-eject :: <C-function-pointer>;
  constant slot g-drive-iface-eject-finish :: <C-function-pointer>;
  constant slot g-drive-iface-poll-for-media :: <C-function-pointer>;
  constant slot g-drive-iface-poll-for-media-finish :: <C-function-pointer>;
  constant slot g-drive-iface-get-identifier :: <C-function-pointer>;
  constant slot g-drive-iface-enumerate-identifiers :: <C-function-pointer>;
  constant slot g-drive-iface-get-start-stop-type :: <C-function-pointer>;
  constant slot g-drive-iface-can-start :: <C-function-pointer>;
  constant slot g-drive-iface-can-start-degraded :: <C-function-pointer>;
  constant slot g-drive-iface-start :: <C-function-pointer>;
  constant slot g-drive-iface-start-finish :: <C-function-pointer>;
  constant slot g-drive-iface-can-stop :: <C-function-pointer>;
  constant slot g-drive-iface-stop :: <C-function-pointer>;
  constant slot g-drive-iface-stop-finish :: <C-function-pointer>;
  constant slot g-drive-iface-stop-button :: <C-function-pointer>;
  constant slot g-drive-iface-eject-with-operation :: <C-function-pointer>;
  constant slot g-drive-iface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot g-drive-iface-get-sort-key :: <C-function-pointer>;
  pointer-type-name: <GDriveIface>;
end C-struct;

define constant $g-drive-start-none = 0;
define constant <GDriveStartFlags> = <C-int>;
define C-pointer-type <GDriveStartFlags*> => <GDriveStartFlags>;

define constant $g-drive-start-stop-type-unknown = 0;
define constant $g-drive-start-stop-type-shutdown = 1;
define constant $g-drive-start-stop-type-network = 2;
define constant $g-drive-start-stop-type-multidisk = 3;
define constant $g-drive-start-stop-type-password = 4;
define constant <GDriveStartStopType> = <C-int>;
define C-pointer-type <GDriveStartStopType*> => <GDriveStartStopType>;

define open C-subtype <GEmblem> (<GObject>)
end C-subtype;

define C-pointer-type <GEmblem*> => <GEmblem>;

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

define constant $g-emblem-origin-unknown = 0;
define constant $g-emblem-origin-device = 1;
define constant $g-emblem-origin-livemetadata = 2;
define constant $g-emblem-origin-tag = 3;
define constant <GEmblemOrigin> = <C-int>;
define C-pointer-type <GEmblemOrigin*> => <GEmblemOrigin>;

define open C-subtype <GEmblemedIcon> (<GObject>)
  constant slot g-emblemed-icon-parent-instance :: <GObject>;
  constant slot g-emblemed-icon-priv :: <GEmblemedIconPrivate>;
end C-subtype;

define C-pointer-type <GEmblemedIcon*> => <GEmblemedIcon>;

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
  constant slot g-emblemed-icon-class-parent-class :: <GObjectClass>;
  pointer-type-name: <GEmblemedIconClass>;
end C-struct;

define C-struct <_GEmblemedIconPrivate>
  pointer-type-name: <GEmblemedIconPrivate>;
end C-struct;

define constant $file-attribute-access-can-delete = "access::can-delete";

define constant $file-attribute-access-can-execute = "access::can-execute";

define constant $file-attribute-access-can-read = "access::can-read";

define constant $file-attribute-access-can-rename = "access::can-rename";

define constant $file-attribute-access-can-trash = "access::can-trash";

define constant $file-attribute-access-can-write = "access::can-write";

define constant $file-attribute-dos-is-archive = "dos::is-archive";

define constant $file-attribute-dos-is-system = "dos::is-system";

define constant $file-attribute-etag-value = "etag::value";

define constant $file-attribute-filesystem-free = "filesystem::free";

define constant $file-attribute-filesystem-readonly = "filesystem::readonly";

define constant $file-attribute-filesystem-size = "filesystem::size";

define constant $file-attribute-filesystem-type = "filesystem::type";

define constant $file-attribute-filesystem-used = "filesystem::used";

define constant $file-attribute-filesystem-use-preview = "filesystem::use-preview";

define constant $file-attribute-gvfs-backend = "gvfs::backend";

define constant $file-attribute-id-file = "id::file";

define constant $file-attribute-id-filesystem = "id::filesystem";

define constant $file-attribute-mountable-can-eject = "mountable::can-eject";

define constant $file-attribute-mountable-can-mount = "mountable::can-mount";

define constant $file-attribute-mountable-can-poll = "mountable::can-poll";

define constant $file-attribute-mountable-can-start = "mountable::can-start";

define constant $file-attribute-mountable-can-start-degraded = "mountable::can-start-degraded";

define constant $file-attribute-mountable-can-stop = "mountable::can-stop";

define constant $file-attribute-mountable-can-unmount = "mountable::can-unmount";

define constant $file-attribute-mountable-hal-udi = "mountable::hal-udi";

define constant $file-attribute-mountable-is-media-check-automatic = "mountable::is-media-check-automatic";

define constant $file-attribute-mountable-start-stop-type = "mountable::start-stop-type";

define constant $file-attribute-mountable-unix-device = "mountable::unix-device";

define constant $file-attribute-mountable-unix-device-file = "mountable::unix-device-file";

define constant $file-attribute-owner-group = "owner::group";

define constant $file-attribute-owner-user = "owner::user";

define constant $file-attribute-owner-user-real = "owner::user-real";

define constant $file-attribute-preview-icon = "preview::icon";

define constant $file-attribute-selinux-context = "selinux::context";

define constant $file-attribute-standard-allocated-size = "standard::allocated-size";

define constant $file-attribute-standard-content-type = "standard::content-type";

define constant $file-attribute-standard-copy-name = "standard::copy-name";

define constant $file-attribute-standard-description = "standard::description";

define constant $file-attribute-standard-display-name = "standard::display-name";

define constant $file-attribute-standard-edit-name = "standard::edit-name";

define constant $file-attribute-standard-fast-content-type = "standard::fast-content-type";

define constant $file-attribute-standard-icon = "standard::icon";

define constant $file-attribute-standard-is-backup = "standard::is-backup";

define constant $file-attribute-standard-is-hidden = "standard::is-hidden";

define constant $file-attribute-standard-is-symlink = "standard::is-symlink";

define constant $file-attribute-standard-is-virtual = "standard::is-virtual";

define constant $file-attribute-standard-name = "standard::name";

define constant $file-attribute-standard-size = "standard::size";

define constant $file-attribute-standard-sort-order = "standard::sort-order";

define constant $file-attribute-standard-symlink-target = "standard::symlink-target";

define constant $file-attribute-standard-target-uri = "standard::target-uri";

define constant $file-attribute-standard-type = "standard::type";

define constant $file-attribute-thumbnailing-failed = "thumbnail::failed";

define constant $file-attribute-thumbnail-path = "thumbnail::path";

define constant $file-attribute-time-access = "time::access";

define constant $file-attribute-time-access-usec = "time::access-usec";

define constant $file-attribute-time-changed = "time::changed";

define constant $file-attribute-time-changed-usec = "time::changed-usec";

define constant $file-attribute-time-created = "time::created";

define constant $file-attribute-time-created-usec = "time::created-usec";

define constant $file-attribute-time-modified = "time::modified";

define constant $file-attribute-time-modified-usec = "time::modified-usec";

define constant $file-attribute-trash-deletion-date = "trash::deletion-date";

define constant $file-attribute-trash-item-count = "trash::item-count";

define constant $file-attribute-trash-orig-path = "trash::orig-path";

define constant $file-attribute-unix-blocks = "unix::blocks";

define constant $file-attribute-unix-block-size = "unix::block-size";

define constant $file-attribute-unix-device = "unix::device";

define constant $file-attribute-unix-gid = "unix::gid";

define constant $file-attribute-unix-inode = "unix::inode";

define constant $file-attribute-unix-is-mountpoint = "unix::is-mountpoint";

define constant $file-attribute-unix-mode = "unix::mode";

define constant $file-attribute-unix-nlink = "unix::nlink";

define constant $file-attribute-unix-rdev = "unix::rdev";

define constant $file-attribute-unix-uid = "unix::uid";

// Interface
define open C-subtype <GFile> (<C-void*>)
end C-subtype;

define C-pointer-type <GFile*> => <GFile>;

define C-function g-file-new-for-commandline-arg
  input parameter arg_ :: <C-string>;
  result res :: <GFile>;
  c-name: "g_file_new_for_commandline_arg";
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_copy";
end;

define C-function g-file-copy-attributes
  input parameter self :: <GFile>;
  input parameter destination_ :: <GFile>;
  input parameter flags_ :: <GFileCopyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_copy_attributes";
end;

define C-function g-file-copy-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_copy_finish";
end;

define C-function g-file-create
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_create_finish";
end;

define C-function g-file-create-readwrite
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileIOStream>;
  c-name: "g_file_create_readwrite_finish";
end;

define C-function g-file-delete
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_delete";
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_eject_mountable_with_operation_finish";
end;

define C-function g-file-enumerate-children
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_load_contents_finish";
end;

define C-function g-file-load-partial-contents-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter contents_ :: <C-unsigned-char*>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter etag_out_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_load_partial_contents_finish";
end;

define C-function g-file-make-directory
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_make_directory";
end;

define C-function g-file-make-directory-with-parents
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_make_directory_with_parents";
end;

define C-function g-file-make-symbolic-link
  input parameter self :: <GFile>;
  input parameter symlink_value_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_make_symbolic_link";
end;

define C-function g-file-monitor
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GFileMonitor>;
  c-name: "g_file_monitor";
end;

define C-function g-file-monitor-directory
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GFileMonitor>;
  c-name: "g_file_monitor_directory";
end;

define C-function g-file-monitor-file
  input parameter self :: <GFile>;
  input parameter flags_ :: <GFileMonitorFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_move";
end;

define C-function g-file-open-readwrite
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_poll_mountable_finish";
end;

define C-function g-file-query-default-handler
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_filesystem_info_finish";
end;

define C-function g-file-query-info
  input parameter self :: <GFile>;
  input parameter attributes_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInfo>;
  c-name: "g_file_query_info_finish";
end;

define C-function g-file-query-settable-attributes
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_query_settable_attributes";
end;

define C-function g-file-query-writable-namespaces
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GFileAttributeInfoList>;
  c-name: "g_file_query_writable_namespaces";
end;

define C-function g-file-read
  input parameter self :: <GFile>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInputStream>;
  c-name: "g_file_read_finish";
end;

define C-function g-file-replace
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_replace_contents_finish";
end;

define C-function g-file-replace-finish
  input parameter self :: <GFile>;
  input parameter res_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <GFileOutputStream>;
  c-name: "g_file_replace_finish";
end;

define C-function g-file-replace-readwrite
  input parameter self :: <GFile>;
  input parameter etag_ :: <C-string>;
  input parameter make_backup_ :: <C-boolean>;
  input parameter flags_ :: <GFileCreateFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute";
end;

define C-function g-file-set-attribute-byte-string
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_byte_string";
end;

define C-function g-file-set-attribute-int32
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_int32";
end;

define C-function g-file-set-attribute-int64
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-signed-long>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_int64";
end;

define C-function g-file-set-attribute-string
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_string";
end;

define C-function g-file-set-attribute-uint32
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-unsigned-int>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attribute_uint32";
end;

define C-function g-file-set-attribute-uint64
  input parameter self :: <GFile>;
  input parameter attribute_ :: <C-string>;
  input parameter value_ :: <C-unsigned-long>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attributes_finish";
end;

define C-function g-file-set-attributes-from-info
  input parameter self :: <GFile>;
  input parameter info_ :: <GFileInfo>;
  input parameter flags_ :: <GFileQueryInfoFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_attributes_from_info";
end;

define C-function g-file-set-display-name
  input parameter self :: <GFile>;
  input parameter display_name_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_unmount_mountable_with_operation_finish";
end;

define C-struct <_GFileAttributeInfo>
  slot g-file-attribute-info-name :: <C-string>;
  slot g-file-attribute-info-type :: <GFileAttributeType>;
  slot g-file-attribute-info-flags :: <GFileAttributeInfoFlags>;
  pointer-type-name: <GFileAttributeInfo>;
end C-struct;

define constant $g-file-attribute-info-none = 0;
define constant $g-file-attribute-info-copy-with-file = 1;
define constant $g-file-attribute-info-copy-when-moved = 2;
define constant <GFileAttributeInfoFlags> = <C-int>;
define C-pointer-type <GFileAttributeInfoFlags*> => <GFileAttributeInfoFlags>;

define C-struct <_GFileAttributeInfoList>
  slot g-file-attribute-info-list-infos :: <GFileAttributeInfo>;
  slot g-file-attribute-info-list-n-infos :: <C-signed-int>;
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

define constant $g-file-attribute-status-unset = 0;
define constant $g-file-attribute-status-set = 1;
define constant $g-file-attribute-status-error-setting = 2;
define constant <GFileAttributeStatus> = <C-int>;
define C-pointer-type <GFileAttributeStatus*> => <GFileAttributeStatus>;

define constant $g-file-attribute-type-invalid = 0;
define constant $g-file-attribute-type-string = 1;
define constant $g-file-attribute-type-byte-string = 2;
define constant $g-file-attribute-type-boolean = 3;
define constant $g-file-attribute-type-uint32 = 4;
define constant $g-file-attribute-type-int32 = 5;
define constant $g-file-attribute-type-uint64 = 6;
define constant $g-file-attribute-type-int64 = 7;
define constant $g-file-attribute-type-object = 8;
define constant $g-file-attribute-type-stringv = 9;
define constant <GFileAttributeType> = <C-int>;
define C-pointer-type <GFileAttributeType*> => <GFileAttributeType>;

define constant $g-file-copy-none = 0;
define constant $g-file-copy-overwrite = 1;
define constant $g-file-copy-backup = 2;
define constant $g-file-copy-nofollow-symlinks = 4;
define constant $g-file-copy-all-metadata = 8;
define constant $g-file-copy-no-fallback-for-move = 16;
define constant $g-file-copy-target-default-perms = 32;
define constant <GFileCopyFlags> = <C-int>;
define C-pointer-type <GFileCopyFlags*> => <GFileCopyFlags>;

define constant $g-file-create-none = 0;
define constant $g-file-create-private = 1;
define constant $g-file-create-replace-destination = 2;
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
  constant slot g-file-descriptor-based-iface-g-iface :: <GTypeInterface>;
  constant slot g-file-descriptor-based-iface-get-fd :: <C-function-pointer>;
  pointer-type-name: <GFileDescriptorBasedIface>;
end C-struct;

define open C-subtype <GFileEnumerator> (<GObject>)
  constant slot g-file-enumerator-parent-instance :: <GObject>;
  constant slot g-file-enumerator-priv :: <GFileEnumeratorPrivate>;
end C-subtype;

define C-pointer-type <GFileEnumerator*> => <GFileEnumerator>;

define C-function g-file-enumerator-close
  input parameter self :: <GFileEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_enumerator_close_finish";
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GList>;
  c-name: "g_file_enumerator_next_files_finish";
end;

define C-function g-file-enumerator-set-pending
  input parameter self :: <GFileEnumerator>;
  input parameter pending_ :: <C-boolean>;
  c-name: "g_file_enumerator_set_pending";
end;

define C-struct <_GFileEnumeratorClass>
  constant slot g-file-enumerator-class-parent-class :: <GObjectClass>;
  constant slot g-file-enumerator-class-next-file :: <C-function-pointer>;
  constant slot g-file-enumerator-class-close-fn :: <C-function-pointer>;
  constant slot g-file-enumerator-class-next-files-async :: <C-function-pointer>;
  constant slot g-file-enumerator-class-next-files-finish :: <C-function-pointer>;
  constant slot g-file-enumerator-class-close-async :: <C-function-pointer>;
  constant slot g-file-enumerator-class-close-finish :: <C-function-pointer>;
  constant slot g-file-enumerator-class-_g-reserved1 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved2 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved3 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved4 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved5 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved6 :: <C-void*>;
  constant slot g-file-enumerator-class-_g-reserved7 :: <C-void*>;
  pointer-type-name: <GFileEnumeratorClass>;
end C-struct;

define C-struct <_GFileEnumeratorPrivate>
  pointer-type-name: <GFileEnumeratorPrivate>;
end C-struct;

define open C-subtype <GFileIOStream> (<GIOStream>)
  constant slot g-file-io-stream-parent-instance :: <GIOStream>;
  constant slot g-file-io-stream-priv :: <GFileIOStreamPrivate>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInfo>;
  c-name: "g_file_io_stream_query_info_finish";
end;

define C-struct <_GFileIOStreamClass>
  constant slot g-file-io-stream-class-parent-class :: <GIOStreamClass>;
  constant slot g-file-io-stream-class-tell :: <C-function-pointer>;
  constant slot g-file-io-stream-class-can-seek :: <C-function-pointer>;
  constant slot g-file-io-stream-class-seek :: <C-function-pointer>;
  constant slot g-file-io-stream-class-can-truncate :: <C-function-pointer>;
  constant slot g-file-io-stream-class-truncate-fn :: <C-function-pointer>;
  constant slot g-file-io-stream-class-query-info :: <C-function-pointer>;
  constant slot g-file-io-stream-class-query-info-async :: <C-function-pointer>;
  constant slot g-file-io-stream-class-query-info-finish :: <C-function-pointer>;
  constant slot g-file-io-stream-class-get-etag :: <C-function-pointer>;
  constant slot g-file-io-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-file-io-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-file-io-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-file-io-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-file-io-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileIOStreamClass>;
end C-struct;

define C-struct <_GFileIOStreamPrivate>
  pointer-type-name: <GFileIOStreamPrivate>;
end C-struct;

define open C-subtype <GFileIcon> (<GObject>)
end C-subtype;

define C-pointer-type <GFileIcon*> => <GFileIcon>;

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
  constant slot g-file-iface-g-iface :: <GTypeInterface>;
  constant slot g-file-iface-dup :: <C-function-pointer>;
  constant slot g-file-iface-hash :: <C-function-pointer>;
  constant slot g-file-iface-equal :: <C-function-pointer>;
  constant slot g-file-iface-is-native :: <C-function-pointer>;
  constant slot g-file-iface-has-uri-scheme :: <C-function-pointer>;
  constant slot g-file-iface-get-uri-scheme :: <C-function-pointer>;
  constant slot g-file-iface-get-basename :: <C-function-pointer>;
  constant slot g-file-iface-get-path :: <C-function-pointer>;
  constant slot g-file-iface-get-uri :: <C-function-pointer>;
  constant slot g-file-iface-get-parse-name :: <C-function-pointer>;
  constant slot g-file-iface-get-parent :: <C-function-pointer>;
  constant slot g-file-iface-prefix-matches :: <C-function-pointer>;
  constant slot g-file-iface-get-relative-path :: <C-function-pointer>;
  constant slot g-file-iface-resolve-relative-path :: <C-function-pointer>;
  constant slot g-file-iface-get-child-for-display-name :: <C-function-pointer>;
  constant slot g-file-iface-enumerate-children :: <C-function-pointer>;
  constant slot g-file-iface-enumerate-children-async :: <C-function-pointer>;
  constant slot g-file-iface-enumerate-children-finish :: <C-function-pointer>;
  constant slot g-file-iface-query-info :: <C-function-pointer>;
  constant slot g-file-iface-query-info-async :: <C-function-pointer>;
  constant slot g-file-iface-query-info-finish :: <C-function-pointer>;
  constant slot g-file-iface-query-filesystem-info :: <C-function-pointer>;
  constant slot g-file-iface-query-filesystem-info-async :: <C-function-pointer>;
  constant slot g-file-iface-query-filesystem-info-finish :: <C-function-pointer>;
  constant slot g-file-iface-find-enclosing-mount :: <C-function-pointer>;
  constant slot g-file-iface-find-enclosing-mount-async :: <C-function-pointer>;
  constant slot g-file-iface-find-enclosing-mount-finish :: <C-function-pointer>;
  constant slot g-file-iface-set-display-name :: <C-function-pointer>;
  constant slot g-file-iface-set-display-name-async :: <C-function-pointer>;
  constant slot g-file-iface-set-display-name-finish :: <C-function-pointer>;
  constant slot g-file-iface-query-settable-attributes :: <C-function-pointer>;
  constant slot g-file-iface-_query-settable-attributes-async :: <C-void*>;
  constant slot g-file-iface-_query-settable-attributes-finish :: <C-void*>;
  constant slot g-file-iface-query-writable-namespaces :: <C-function-pointer>;
  constant slot g-file-iface-_query-writable-namespaces-async :: <C-void*>;
  constant slot g-file-iface-_query-writable-namespaces-finish :: <C-void*>;
  constant slot g-file-iface-set-attribute :: <C-function-pointer>;
  constant slot g-file-iface-set-attributes-from-info :: <C-function-pointer>;
  constant slot g-file-iface-set-attributes-async :: <C-function-pointer>;
  constant slot g-file-iface-set-attributes-finish :: <C-function-pointer>;
  constant slot g-file-iface-read-fn :: <C-function-pointer>;
  constant slot g-file-iface-read-async :: <C-function-pointer>;
  constant slot g-file-iface-read-finish :: <C-function-pointer>;
  constant slot g-file-iface-append-to :: <C-function-pointer>;
  constant slot g-file-iface-append-to-async :: <C-function-pointer>;
  constant slot g-file-iface-append-to-finish :: <C-function-pointer>;
  constant slot g-file-iface-create :: <C-function-pointer>;
  constant slot g-file-iface-create-async :: <C-function-pointer>;
  constant slot g-file-iface-create-finish :: <C-function-pointer>;
  constant slot g-file-iface-replace :: <C-function-pointer>;
  constant slot g-file-iface-replace-async :: <C-function-pointer>;
  constant slot g-file-iface-replace-finish :: <C-function-pointer>;
  constant slot g-file-iface-delete-file :: <C-function-pointer>;
  constant slot g-file-iface-_delete-file-async :: <C-void*>;
  constant slot g-file-iface-_delete-file-finish :: <C-void*>;
  constant slot g-file-iface-trash :: <C-function-pointer>;
  constant slot g-file-iface-_trash-async :: <C-void*>;
  constant slot g-file-iface-_trash-finish :: <C-void*>;
  constant slot g-file-iface-make-directory :: <C-function-pointer>;
  constant slot g-file-iface-_make-directory-async :: <C-void*>;
  constant slot g-file-iface-_make-directory-finish :: <C-void*>;
  constant slot g-file-iface-make-symbolic-link :: <C-function-pointer>;
  constant slot g-file-iface-_make-symbolic-link-async :: <C-void*>;
  constant slot g-file-iface-_make-symbolic-link-finish :: <C-void*>;
  constant slot g-file-iface-copy :: <C-function-pointer>;
  constant slot g-file-iface-copy-async :: <C-void*>;
  constant slot g-file-iface-copy-finish :: <C-function-pointer>;
  constant slot g-file-iface-move :: <C-function-pointer>;
  constant slot g-file-iface-_move-async :: <C-void*>;
  constant slot g-file-iface-_move-finish :: <C-void*>;
  constant slot g-file-iface-mount-mountable :: <C-function-pointer>;
  constant slot g-file-iface-mount-mountable-finish :: <C-function-pointer>;
  constant slot g-file-iface-unmount-mountable :: <C-function-pointer>;
  constant slot g-file-iface-unmount-mountable-finish :: <C-function-pointer>;
  constant slot g-file-iface-eject-mountable :: <C-function-pointer>;
  constant slot g-file-iface-eject-mountable-finish :: <C-function-pointer>;
  constant slot g-file-iface-mount-enclosing-volume :: <C-function-pointer>;
  constant slot g-file-iface-mount-enclosing-volume-finish :: <C-function-pointer>;
  constant slot g-file-iface-monitor-dir :: <C-function-pointer>;
  constant slot g-file-iface-monitor-file :: <C-function-pointer>;
  constant slot g-file-iface-open-readwrite :: <C-function-pointer>;
  constant slot g-file-iface-open-readwrite-async :: <C-function-pointer>;
  constant slot g-file-iface-open-readwrite-finish :: <C-function-pointer>;
  constant slot g-file-iface-create-readwrite :: <C-function-pointer>;
  constant slot g-file-iface-create-readwrite-async :: <C-function-pointer>;
  constant slot g-file-iface-create-readwrite-finish :: <C-function-pointer>;
  constant slot g-file-iface-replace-readwrite :: <C-function-pointer>;
  constant slot g-file-iface-replace-readwrite-async :: <C-function-pointer>;
  constant slot g-file-iface-replace-readwrite-finish :: <C-function-pointer>;
  constant slot g-file-iface-start-mountable :: <C-function-pointer>;
  constant slot g-file-iface-start-mountable-finish :: <C-function-pointer>;
  constant slot g-file-iface-stop-mountable :: <C-function-pointer>;
  constant slot g-file-iface-stop-mountable-finish :: <C-function-pointer>;
  constant slot g-file-iface-supports-thread-contexts :: <C-boolean>;
  constant slot g-file-iface-unmount-mountable-with-operation :: <C-function-pointer>;
  constant slot g-file-iface-unmount-mountable-with-operation-finish :: <C-function-pointer>;
  constant slot g-file-iface-eject-mountable-with-operation :: <C-function-pointer>;
  constant slot g-file-iface-eject-mountable-with-operation-finish :: <C-function-pointer>;
  constant slot g-file-iface-poll-mountable :: <C-function-pointer>;
  constant slot g-file-iface-poll-mountable-finish :: <C-function-pointer>;
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
  constant slot g-file-input-stream-parent-instance :: <GInputStream>;
  constant slot g-file-input-stream-priv :: <GFileInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GFileInputStream*> => <GFileInputStream>;

define C-function g-file-input-stream-query-info
  input parameter self :: <GFileInputStream>;
  input parameter attributes_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInfo>;
  c-name: "g_file_input_stream_query_info_finish";
end;

define C-struct <_GFileInputStreamClass>
  constant slot g-file-input-stream-class-parent-class :: <GInputStreamClass>;
  constant slot g-file-input-stream-class-tell :: <C-function-pointer>;
  constant slot g-file-input-stream-class-can-seek :: <C-function-pointer>;
  constant slot g-file-input-stream-class-seek :: <C-function-pointer>;
  constant slot g-file-input-stream-class-query-info :: <C-function-pointer>;
  constant slot g-file-input-stream-class-query-info-async :: <C-function-pointer>;
  constant slot g-file-input-stream-class-query-info-finish :: <C-function-pointer>;
  constant slot g-file-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-file-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-file-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-file-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-file-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileInputStreamClass>;
end C-struct;

define C-struct <_GFileInputStreamPrivate>
  pointer-type-name: <GFileInputStreamPrivate>;
end C-struct;

define open C-subtype <GFileMonitor> (<GObject>)
  constant slot g-file-monitor-parent-instance :: <GObject>;
  constant slot g-file-monitor-priv :: <GFileMonitorPrivate>;
end C-subtype;

define C-pointer-type <GFileMonitor*> => <GFileMonitor>;

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
  constant slot g-file-monitor-class-parent-class :: <GObjectClass>;
  constant slot g-file-monitor-class-changed :: <C-function-pointer>;
  constant slot g-file-monitor-class-cancel :: <C-function-pointer>;
  constant slot g-file-monitor-class-_g-reserved1 :: <C-void*>;
  constant slot g-file-monitor-class-_g-reserved2 :: <C-void*>;
  constant slot g-file-monitor-class-_g-reserved3 :: <C-void*>;
  constant slot g-file-monitor-class-_g-reserved4 :: <C-void*>;
  constant slot g-file-monitor-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileMonitorClass>;
end C-struct;

define constant $g-file-monitor-event-changed = 0;
define constant $g-file-monitor-event-changes-done-hint = 1;
define constant $g-file-monitor-event-deleted = 2;
define constant $g-file-monitor-event-created = 3;
define constant $g-file-monitor-event-attribute-changed = 4;
define constant $g-file-monitor-event-pre-unmount = 5;
define constant $g-file-monitor-event-unmounted = 6;
define constant $g-file-monitor-event-moved = 7;
define constant <GFileMonitorEvent> = <C-int>;
define C-pointer-type <GFileMonitorEvent*> => <GFileMonitorEvent>;

define constant $g-file-monitor-none = 0;
define constant $g-file-monitor-watch-mounts = 1;
define constant $g-file-monitor-send-moved = 2;
define constant <GFileMonitorFlags> = <C-int>;
define C-pointer-type <GFileMonitorFlags*> => <GFileMonitorFlags>;

define C-struct <_GFileMonitorPrivate>
  pointer-type-name: <GFileMonitorPrivate>;
end C-struct;

define open C-subtype <GFileOutputStream> (<GOutputStream>)
  constant slot g-file-output-stream-parent-instance :: <GOutputStream>;
  constant slot g-file-output-stream-priv :: <GFileOutputStreamPrivate>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GFileInfo>;
  c-name: "g_file_output_stream_query_info_finish";
end;

define C-struct <_GFileOutputStreamClass>
  constant slot g-file-output-stream-class-parent-class :: <GOutputStreamClass>;
  constant slot g-file-output-stream-class-tell :: <C-function-pointer>;
  constant slot g-file-output-stream-class-can-seek :: <C-function-pointer>;
  constant slot g-file-output-stream-class-seek :: <C-function-pointer>;
  constant slot g-file-output-stream-class-can-truncate :: <C-function-pointer>;
  constant slot g-file-output-stream-class-truncate-fn :: <C-function-pointer>;
  constant slot g-file-output-stream-class-query-info :: <C-function-pointer>;
  constant slot g-file-output-stream-class-query-info-async :: <C-function-pointer>;
  constant slot g-file-output-stream-class-query-info-finish :: <C-function-pointer>;
  constant slot g-file-output-stream-class-get-etag :: <C-function-pointer>;
  constant slot g-file-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-file-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-file-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-file-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-file-output-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GFileOutputStreamClass>;
end C-struct;

define C-struct <_GFileOutputStreamPrivate>
  pointer-type-name: <GFileOutputStreamPrivate>;
end C-struct;

define constant $g-file-query-info-none = 0;
define constant $g-file-query-info-nofollow-symlinks = 1;
define constant <GFileQueryInfoFlags> = <C-int>;
define C-pointer-type <GFileQueryInfoFlags*> => <GFileQueryInfoFlags>;

define constant $g-file-type-unknown = 0;
define constant $g-file-type-regular = 1;
define constant $g-file-type-directory = 2;
define constant $g-file-type-symbolic-link = 3;
define constant $g-file-type-special = 4;
define constant $g-file-type-shortcut = 5;
define constant $g-file-type-mountable = 6;
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
  constant slot g-filename-completer-class-parent-class :: <GObjectClass>;
  constant slot g-filename-completer-class-got-completion-data :: <C-function-pointer>;
  constant slot g-filename-completer-class-_g-reserved1 :: <C-void*>;
  constant slot g-filename-completer-class-_g-reserved2 :: <C-void*>;
  constant slot g-filename-completer-class-_g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilenameCompleterClass>;
end C-struct;

define constant $g-filesystem-preview-type-if-always = 0;
define constant $g-filesystem-preview-type-if-local = 1;
define constant $g-filesystem-preview-type-never = 2;
define constant <GFilesystemPreviewType> = <C-int>;
define C-pointer-type <GFilesystemPreviewType*> => <GFilesystemPreviewType>;

define open C-subtype <GFilterInputStream> (<GInputStream>)
  constant slot g-filter-input-stream-parent-instance :: <GInputStream>;
  constant slot g-filter-input-stream-base-stream :: <GInputStream>;
end C-subtype;

define C-pointer-type <GFilterInputStream*> => <GFilterInputStream>;

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
  constant slot g-filter-input-stream-class-parent-class :: <GInputStreamClass>;
  constant slot g-filter-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-filter-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-filter-input-stream-class-_g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilterInputStreamClass>;
end C-struct;

define open C-subtype <GFilterOutputStream> (<GOutputStream>)
  constant slot g-filter-output-stream-parent-instance :: <GOutputStream>;
  constant slot g-filter-output-stream-base-stream :: <GOutputStream>;
end C-subtype;

define C-pointer-type <GFilterOutputStream*> => <GFilterOutputStream>;

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
  constant slot g-filter-output-stream-class-parent-class :: <GOutputStreamClass>;
  constant slot g-filter-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-filter-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-filter-output-stream-class-_g-reserved3 :: <C-void*>;
  pointer-type-name: <GFilterOutputStreamClass>;
end C-struct;

define constant $g-io-error-failed = 0;
define constant $g-io-error-not-found = 1;
define constant $g-io-error-exists = 2;
define constant $g-io-error-is-directory = 3;
define constant $g-io-error-not-directory = 4;
define constant $g-io-error-not-empty = 5;
define constant $g-io-error-not-regular-file = 6;
define constant $g-io-error-not-symbolic-link = 7;
define constant $g-io-error-not-mountable-file = 8;
define constant $g-io-error-filename-too-long = 9;
define constant $g-io-error-invalid-filename = 10;
define constant $g-io-error-too-many-links = 11;
define constant $g-io-error-no-space = 12;
define constant $g-io-error-invalid-argument = 13;
define constant $g-io-error-permission-denied = 14;
define constant $g-io-error-not-supported = 15;
define constant $g-io-error-not-mounted = 16;
define constant $g-io-error-already-mounted = 17;
define constant $g-io-error-closed = 18;
define constant $g-io-error-cancelled = 19;
define constant $g-io-error-pending = 20;
define constant $g-io-error-read-only = 21;
define constant $g-io-error-cant-create-backup = 22;
define constant $g-io-error-wrong-etag = 23;
define constant $g-io-error-timed-out = 24;
define constant $g-io-error-would-recurse = 25;
define constant $g-io-error-busy = 26;
define constant $g-io-error-would-block = 27;
define constant $g-io-error-host-not-found = 28;
define constant $g-io-error-would-merge = 29;
define constant $g-io-error-failed-handled = 30;
define constant $g-io-error-too-many-open-files = 31;
define constant $g-io-error-not-initialized = 32;
define constant $g-io-error-address-in-use = 33;
define constant $g-io-error-partial-input = 34;
define constant $g-io-error-invalid-data = 35;
define constant $g-io-error-dbus-error = 36;
define constant $g-io-error-host-unreachable = 37;
define constant $g-io-error-network-unreachable = 38;
define constant $g-io-error-connection-refused = 39;
define constant $g-io-error-proxy-failed = 40;
define constant $g-io-error-proxy-auth-failed = 41;
define constant $g-io-error-proxy-need-auth = 42;
define constant $g-io-error-proxy-not-allowed = 43;
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

define constant $g-io-module-scope-none = 0;
define constant $g-io-module-scope-block-duplicates = 1;
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
  constant slot g-io-stream-parent-instance :: <GObject>;
  constant slot g-io-stream-priv :: <GIOStreamPrivate>;
end C-subtype;

define C-pointer-type <GIOStream*> => <GIOStream>;

define C-function g-io-stream-splice-finish
  input parameter result_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-io-stream-class-parent-class :: <GObjectClass>;
  constant slot g-io-stream-class-get-input-stream :: <C-function-pointer>;
  constant slot g-io-stream-class-get-output-stream :: <C-function-pointer>;
  constant slot g-io-stream-class-close-fn :: <C-function-pointer>;
  constant slot g-io-stream-class-close-async :: <C-function-pointer>;
  constant slot g-io-stream-class-close-finish :: <C-function-pointer>;
  constant slot g-io-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved5 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved6 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved7 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved8 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved9 :: <C-void*>;
  constant slot g-io-stream-class-_g-reserved10 :: <C-void*>;
  pointer-type-name: <GIOStreamClass>;
end C-struct;

define C-struct <_GIOStreamPrivate>
  pointer-type-name: <GIOStreamPrivate>;
end C-struct;

define constant $g-io-stream-splice-none = 0;
define constant $g-io-stream-splice-close-stream1 = 1;
define constant $g-io-stream-splice-close-stream2 = 2;
define constant $g-io-stream-splice-wait-for-both = 4;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-icon-iface-g-iface :: <GTypeInterface>;
  constant slot g-icon-iface-hash :: <C-function-pointer>;
  constant slot g-icon-iface-equal :: <C-function-pointer>;
  constant slot g-icon-iface-to-tokens :: <C-function-pointer>;
  constant slot g-icon-iface-from-tokens :: <C-void*>;
  pointer-type-name: <GIconIface>;
end C-struct;

define open C-subtype <GInetAddress> (<GObject>)
  constant slot g-inet-address-parent-instance :: <GObject>;
  constant slot g-inet-address-priv :: <GInetAddressPrivate>;
end C-subtype;

define C-pointer-type <GInetAddress*> => <GInetAddress>;

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
  constant slot g-inet-address-class-parent-class :: <GObjectClass>;
  constant slot g-inet-address-class-to-string :: <C-function-pointer>;
  constant slot g-inet-address-class-to-bytes :: <C-function-pointer>;
  pointer-type-name: <GInetAddressClass>;
end C-struct;

define open C-subtype <GInetAddressMask> (<GObject>)
  constant slot g-inet-address-mask-parent-instance :: <GObject>;
  constant slot g-inet-address-mask-priv :: <GInetAddressMaskPrivate>;
end C-subtype;

define C-pointer-type <GInetAddressMask*> => <GInetAddressMask>;

define C-function g-inet-address-mask-new
  input parameter addr_ :: <GInetAddress>;
  input parameter length_ :: <C-unsigned-int>;
  output parameter error_ :: <GError*>;
  result res :: <GInetAddressMask>;
  c-name: "g_inet_address_mask_new";
end;

define C-function g-inet-address-mask-new-from-string
  input parameter mask_string_ :: <C-string>;
  output parameter error_ :: <GError*>;
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
  constant slot g-inet-address-mask-class-parent-class :: <GObjectClass>;
  pointer-type-name: <GInetAddressMaskClass>;
end C-struct;

define C-struct <_GInetAddressMaskPrivate>
  pointer-type-name: <GInetAddressMaskPrivate>;
end C-struct;

define C-struct <_GInetAddressPrivate>
  pointer-type-name: <GInetAddressPrivate>;
end C-struct;

define open C-subtype <GInetSocketAddress> (<GSocketAddress>)
  constant slot g-inet-socket-address-parent-instance :: <GSocketAddress>;
  constant slot g-inet-socket-address-priv :: <GInetSocketAddressPrivate>;
end C-subtype;

define C-pointer-type <GInetSocketAddress*> => <GInetSocketAddress>;

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
  constant slot g-inet-socket-address-class-parent-class :: <GSocketAddressClass>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-void*>;
  c-name: "g_initable_newv";
end;

define C-function g-initable-init
  input parameter self :: <GInitable>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_initable_init";
end;

define C-struct <_GInitableIface>
  constant slot g-initable-iface-g-iface :: <GTypeInterface>;
  constant slot g-initable-iface-init :: <C-function-pointer>;
  pointer-type-name: <GInitableIface>;
end C-struct;

define open C-subtype <GInputStream> (<GObject>)
  constant slot g-input-stream-parent-instance :: <GObject>;
  constant slot g-input-stream-priv :: <GInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GInputStream*> => <GInputStream>;

define C-function g-input-stream-clear-pending
  input parameter self :: <GInputStream>;
  c-name: "g_input_stream_clear_pending";
end;

define C-function g-input-stream-close
  input parameter self :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  input parameter buffer_ :: <C-void*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_read";
end;

define C-function g-input-stream-read-all
  input parameter self :: <GInputStream>;
  input parameter buffer_ :: <C-void*>;
  input parameter count_ :: <C-unsigned-long>;
  output parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_read_all";
end;

define C-function g-input-stream-read-async
  input parameter self :: <GInputStream>;
  input parameter buffer_ :: <C-void*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_input_stream_read_async";
end;

define C-function g-input-stream-read-finish
  input parameter self :: <GInputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_read_finish";
end;

define C-function g-input-stream-set-pending
  input parameter self :: <GInputStream>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_input_stream_set_pending";
end;

define C-function g-input-stream-skip
  input parameter self :: <GInputStream>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_input_stream_skip_finish";
end;

define C-struct <_GInputStreamClass>
  constant slot g-input-stream-class-parent-class :: <GObjectClass>;
  constant slot g-input-stream-class-read-fn :: <C-function-pointer>;
  constant slot g-input-stream-class-skip :: <C-function-pointer>;
  constant slot g-input-stream-class-close-fn :: <C-function-pointer>;
  constant slot g-input-stream-class-read-async :: <C-function-pointer>;
  constant slot g-input-stream-class-read-finish :: <C-function-pointer>;
  constant slot g-input-stream-class-skip-async :: <C-function-pointer>;
  constant slot g-input-stream-class-skip-finish :: <C-function-pointer>;
  constant slot g-input-stream-class-close-async :: <C-function-pointer>;
  constant slot g-input-stream-class-close-finish :: <C-function-pointer>;
  constant slot g-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GInputStreamClass>;
end C-struct;

define C-struct <_GInputStreamPrivate>
  pointer-type-name: <GInputStreamPrivate>;
end C-struct;

define C-struct <_GInputVector>
  slot g-input-vector-buffer :: <C-void*>;
  slot g-input-vector-size :: <C-unsigned-long>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GInputStream>;
  c-name: "g_loadable_icon_load_finish";
end;

define C-struct <_GLoadableIconIface>
  constant slot g-loadable-icon-iface-g-iface :: <GTypeInterface>;
  constant slot g-loadable-icon-iface-load :: <C-function-pointer>;
  constant slot g-loadable-icon-iface-load-async :: <C-function-pointer>;
  constant slot g-loadable-icon-iface-load-finish :: <C-function-pointer>;
  pointer-type-name: <GLoadableIconIface>;
end C-struct;

define constant $menu-attribute-action = "action";

define constant $menu-attribute-label = "label";

define constant $menu-attribute-target = "target";

define constant $menu-link-section = "section";

define constant $menu-link-submenu = "submenu";

define open C-subtype <GMemoryInputStream> (<GInputStream>)
  constant slot g-memory-input-stream-parent-instance :: <GInputStream>;
  constant slot g-memory-input-stream-priv :: <GMemoryInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GMemoryInputStream*> => <GMemoryInputStream>;

define C-function g-memory-input-stream-new
  result res :: <GInputStream>;
  c-name: "g_memory_input_stream_new";
end;

define C-function g-memory-input-stream-new-from-data
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-signed-long>;
  input parameter destroy_ :: <C-function-pointer>;
  result res :: <GInputStream>;
  c-name: "g_memory_input_stream_new_from_data";
end;

define C-function g-memory-input-stream-add-data
  input parameter self :: <GMemoryInputStream>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-signed-long>;
  input parameter destroy_ :: <C-function-pointer>;
  c-name: "g_memory_input_stream_add_data";
end;

define C-struct <_GMemoryInputStreamClass>
  constant slot g-memory-input-stream-class-parent-class :: <GInputStreamClass>;
  constant slot g-memory-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-memory-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-memory-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-memory-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-memory-input-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GMemoryInputStreamClass>;
end C-struct;

define C-struct <_GMemoryInputStreamPrivate>
  pointer-type-name: <GMemoryInputStreamPrivate>;
end C-struct;

define open C-subtype <GMemoryOutputStream> (<GOutputStream>)
  constant slot g-memory-output-stream-parent-instance :: <GOutputStream>;
  constant slot g-memory-output-stream-priv :: <GMemoryOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GMemoryOutputStream*> => <GMemoryOutputStream>;

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

define C-function g-memory-output-stream-steal-data
  input parameter self :: <GMemoryOutputStream>;
  result res :: <C-void*>;
  c-name: "g_memory_output_stream_steal_data";
end;

define C-struct <_GMemoryOutputStreamClass>
  constant slot g-memory-output-stream-class-parent-class :: <GOutputStreamClass>;
  constant slot g-memory-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-memory-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-memory-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-memory-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-memory-output-stream-class-_g-reserved5 :: <C-void*>;
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
  constant slot g-menu-attribute-iter-parent-instance :: <GObject>;
  constant slot g-menu-attribute-iter-priv :: <GMenuAttributeIterPrivate>;
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
  constant slot g-menu-attribute-iter-class-parent-class :: <GObjectClass>;
  constant slot g-menu-attribute-iter-class-get-next :: <C-function-pointer>;
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
  constant slot g-menu-link-iter-parent-instance :: <GObject>;
  constant slot g-menu-link-iter-priv :: <GMenuLinkIterPrivate>;
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
  constant slot g-menu-link-iter-class-parent-class :: <GObjectClass>;
  constant slot g-menu-link-iter-class-get-next :: <C-function-pointer>;
  pointer-type-name: <GMenuLinkIterClass>;
end C-struct;

define C-struct <_GMenuLinkIterPrivate>
  pointer-type-name: <GMenuLinkIterPrivate>;
end C-struct;

define open C-subtype <GMenuModel> (<GObject>)
  constant slot g-menu-model-parent-instance :: <GObject>;
  constant slot g-menu-model-priv :: <GMenuModelPrivate>;
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
  constant slot g-menu-model-class-parent-class :: <GObjectClass>;
  constant slot g-menu-model-class-is-mutable :: <C-function-pointer>;
  constant slot g-menu-model-class-get-n-items :: <C-function-pointer>;
  constant slot g-menu-model-class-get-item-attributes :: <C-function-pointer>;
  constant slot g-menu-model-class-iterate-item-attributes :: <C-function-pointer>;
  constant slot g-menu-model-class-get-item-attribute-value :: <C-function-pointer>;
  constant slot g-menu-model-class-get-item-links :: <C-function-pointer>;
  constant slot g-menu-model-class-iterate-item-links :: <C-function-pointer>;
  constant slot g-menu-model-class-get-item-link :: <C-function-pointer>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_mount_guess_content_type_finish";
end;

define C-function g-mount-guess-content-type-sync
  input parameter self :: <GMount>;
  input parameter force_rescan_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_mount_unmount_with_operation_finish";
end;

define C-function g-mount-unshadow
  input parameter self :: <GMount>;
  c-name: "g_mount_unshadow";
end;

define C-struct <_GMountIface>
  constant slot g-mount-iface-g-iface :: <GTypeInterface>;
  constant slot g-mount-iface-changed :: <C-function-pointer>;
  constant slot g-mount-iface-unmounted :: <C-function-pointer>;
  constant slot g-mount-iface-get-root :: <C-function-pointer>;
  constant slot g-mount-iface-get-name :: <C-function-pointer>;
  constant slot g-mount-iface-get-icon :: <C-function-pointer>;
  constant slot g-mount-iface-get-uuid :: <C-function-pointer>;
  constant slot g-mount-iface-get-volume :: <C-function-pointer>;
  constant slot g-mount-iface-get-drive :: <C-function-pointer>;
  constant slot g-mount-iface-can-unmount :: <C-function-pointer>;
  constant slot g-mount-iface-can-eject :: <C-function-pointer>;
  constant slot g-mount-iface-unmount :: <C-function-pointer>;
  constant slot g-mount-iface-unmount-finish :: <C-function-pointer>;
  constant slot g-mount-iface-eject :: <C-function-pointer>;
  constant slot g-mount-iface-eject-finish :: <C-function-pointer>;
  constant slot g-mount-iface-remount :: <C-function-pointer>;
  constant slot g-mount-iface-remount-finish :: <C-function-pointer>;
  constant slot g-mount-iface-guess-content-type :: <C-function-pointer>;
  constant slot g-mount-iface-guess-content-type-finish :: <C-function-pointer>;
  constant slot g-mount-iface-guess-content-type-sync :: <C-function-pointer>;
  constant slot g-mount-iface-pre-unmount :: <C-function-pointer>;
  constant slot g-mount-iface-unmount-with-operation :: <C-function-pointer>;
  constant slot g-mount-iface-unmount-with-operation-finish :: <C-function-pointer>;
  constant slot g-mount-iface-eject-with-operation :: <C-function-pointer>;
  constant slot g-mount-iface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot g-mount-iface-get-default-location :: <C-function-pointer>;
  constant slot g-mount-iface-get-sort-key :: <C-function-pointer>;
  pointer-type-name: <GMountIface>;
end C-struct;

define constant $g-mount-mount-none = 0;
define constant <GMountMountFlags> = <C-int>;
define C-pointer-type <GMountMountFlags*> => <GMountMountFlags>;

define open C-subtype <GMountOperation> (<GObject>)
  constant slot g-mount-operation-parent-instance :: <GObject>;
  constant slot g-mount-operation-priv :: <GMountOperationPrivate>;
end C-subtype;

define C-pointer-type <GMountOperation*> => <GMountOperation>;

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
  constant slot g-mount-operation-class-parent-class :: <GObjectClass>;
  constant slot g-mount-operation-class-ask-password :: <C-function-pointer>;
  constant slot g-mount-operation-class-ask-question :: <C-function-pointer>;
  constant slot g-mount-operation-class-reply :: <C-function-pointer>;
  constant slot g-mount-operation-class-aborted :: <C-function-pointer>;
  constant slot g-mount-operation-class-show-processes :: <C-function-pointer>;
  constant slot g-mount-operation-class-_g-reserved1 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved2 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved3 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved4 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved5 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved6 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved7 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved8 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved9 :: <C-void*>;
  constant slot g-mount-operation-class-_g-reserved10 :: <C-void*>;
  pointer-type-name: <GMountOperationClass>;
end C-struct;

define C-struct <_GMountOperationPrivate>
  pointer-type-name: <GMountOperationPrivate>;
end C-struct;

define constant $g-mount-operation-handled = 0;
define constant $g-mount-operation-aborted = 1;
define constant $g-mount-operation-unhandled = 2;
define constant <GMountOperationResult> = <C-int>;
define C-pointer-type <GMountOperationResult*> => <GMountOperationResult>;

define constant $g-mount-unmount-none = 0;
define constant $g-mount-unmount-force = 1;
define constant <GMountUnmountFlags> = <C-int>;
define C-pointer-type <GMountUnmountFlags*> => <GMountUnmountFlags>;

define constant $native-volume-monitor-extension-point-name = "gio-native-volume-monitor";

define constant $network-monitor-extension-point-name = "gio-network-monitor";

define open C-subtype <GNativeVolumeMonitor> (<GVolumeMonitor>)
  constant slot g-native-volume-monitor-parent-instance :: <GVolumeMonitor>;
end C-subtype;

define C-pointer-type <GNativeVolumeMonitor*> => <GNativeVolumeMonitor>;

define C-struct <_GNativeVolumeMonitorClass>
  constant slot g-native-volume-monitor-class-parent-class :: <GVolumeMonitorClass>;
  constant slot g-native-volume-monitor-class-get-mount-for-mount-path :: <C-void*>;
  pointer-type-name: <GNativeVolumeMonitorClass>;
end C-struct;

define open C-subtype <GNetworkAddress> (<GObject>)
  constant slot g-network-address-parent-instance :: <GObject>;
  constant slot g-network-address-priv :: <GNetworkAddressPrivate>;
end C-subtype;

define C-pointer-type <GNetworkAddress*> => <GNetworkAddress>;

define C-function g-network-address-new
  input parameter hostname_ :: <C-string>;
  input parameter port_ :: <C-unsigned-short>;
  result res :: <GNetworkAddress>;
  c-name: "g_network_address_new";
end;

define C-function g-network-address-parse
  input parameter host_and_port_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  output parameter error_ :: <GError*>;
  result res :: <GSocketConnectable>;
  c-name: "g_network_address_parse";
end;

define C-function g-network-address-parse-uri
  input parameter uri_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  output parameter error_ :: <GError*>;
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
  constant slot g-network-address-class-parent-class :: <GObjectClass>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_network_monitor_can_reach_finish";
end;

define C-function g-network-monitor-get-network-available
  input parameter self :: <GNetworkMonitor>;
  result res :: <C-boolean>;
  c-name: "g_network_monitor_get_network_available";
end;

define C-struct <_GNetworkMonitorInterface>
  constant slot g-network-monitor-interface-g-iface :: <GTypeInterface>;
  constant slot g-network-monitor-interface-network-changed :: <C-function-pointer>;
  constant slot g-network-monitor-interface-can-reach :: <C-function-pointer>;
  constant slot g-network-monitor-interface-can-reach-async :: <C-function-pointer>;
  constant slot g-network-monitor-interface-can-reach-finish :: <C-function-pointer>;
  pointer-type-name: <GNetworkMonitorInterface>;
end C-struct;

define open C-subtype <GNetworkService> (<GObject>)
  constant slot g-network-service-parent-instance :: <GObject>;
  constant slot g-network-service-priv :: <GNetworkServicePrivate>;
end C-subtype;

define C-pointer-type <GNetworkService*> => <GNetworkService>;

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
  constant slot g-network-service-class-parent-class :: <GObjectClass>;
  pointer-type-name: <GNetworkServiceClass>;
end C-struct;

define C-struct <_GNetworkServicePrivate>
  pointer-type-name: <GNetworkServicePrivate>;
end C-struct;

define open C-subtype <GOutputStream> (<GObject>)
  constant slot g-output-stream-parent-instance :: <GObject>;
  constant slot g-output-stream-priv :: <GOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GOutputStream*> => <GOutputStream>;

define C-function g-output-stream-clear-pending
  input parameter self :: <GOutputStream>;
  c-name: "g_output_stream_clear_pending";
end;

define C-function g-output-stream-close
  input parameter self :: <GOutputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_close_finish";
end;

define C-function g-output-stream-flush
  input parameter self :: <GOutputStream>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_output_stream_set_pending";
end;

define C-function g-output-stream-splice
  input parameter self :: <GOutputStream>;
  input parameter source_ :: <GInputStream>;
  input parameter flags_ :: <GOutputStreamSpliceFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_splice_finish";
end;

define C-function g-output-stream-write
  input parameter self :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write";
end;

define C-function g-output-stream-write-all
  input parameter self :: <GOutputStream>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  output parameter bytes_written_ :: <C-unsigned-long*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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

define C-function g-output-stream-write-finish
  input parameter self :: <GOutputStream>;
  input parameter result_ :: <GAsyncResult>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_output_stream_write_finish";
end;

define C-struct <_GOutputStreamClass>
  constant slot g-output-stream-class-parent-class :: <GObjectClass>;
  constant slot g-output-stream-class-write-fn :: <C-function-pointer>;
  constant slot g-output-stream-class-splice :: <C-function-pointer>;
  constant slot g-output-stream-class-flush :: <C-function-pointer>;
  constant slot g-output-stream-class-close-fn :: <C-function-pointer>;
  constant slot g-output-stream-class-write-async :: <C-function-pointer>;
  constant slot g-output-stream-class-write-finish :: <C-function-pointer>;
  constant slot g-output-stream-class-splice-async :: <C-function-pointer>;
  constant slot g-output-stream-class-splice-finish :: <C-function-pointer>;
  constant slot g-output-stream-class-flush-async :: <C-function-pointer>;
  constant slot g-output-stream-class-flush-finish :: <C-function-pointer>;
  constant slot g-output-stream-class-close-async :: <C-function-pointer>;
  constant slot g-output-stream-class-close-finish :: <C-function-pointer>;
  constant slot g-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved5 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved6 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved7 :: <C-void*>;
  constant slot g-output-stream-class-_g-reserved8 :: <C-void*>;
  pointer-type-name: <GOutputStreamClass>;
end C-struct;

define C-struct <_GOutputStreamPrivate>
  pointer-type-name: <GOutputStreamPrivate>;
end C-struct;

define constant $g-output-stream-splice-none = 0;
define constant $g-output-stream-splice-close-source = 1;
define constant $g-output-stream-splice-close-target = 2;
define constant <GOutputStreamSpliceFlags> = <C-int>;
define C-pointer-type <GOutputStreamSpliceFlags*> => <GOutputStreamSpliceFlags>;

define C-struct <_GOutputVector>
  slot g-output-vector-buffer :: <C-void*>;
  slot g-output-vector-size :: <C-unsigned-long>;
  pointer-type-name: <GOutputVector>;
end C-struct;

define constant $proxy-extension-point-name = "gio-proxy";

define constant $proxy-resolver-extension-point-name = "gio-proxy-resolver";

define constant $g-password-save-never = 0;
define constant $g-password-save-for-session = 1;
define constant $g-password-save-permanently = 2;
define constant <GPasswordSave> = <C-int>;
define C-pointer-type <GPasswordSave*> => <GPasswordSave>;

define open C-subtype <GPermission> (<GObject>)
  constant slot g-permission-parent-instance :: <GObject>;
  constant slot g-permission-priv :: <GPermissionPrivate>;
end C-subtype;

define C-pointer-type <GPermission*> => <GPermission>;

define C-function g-permission-acquire
  input parameter self :: <GPermission>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_permission_release_finish";
end;

define C-struct <_GPermissionClass>
  constant slot g-permission-class-parent-class :: <GObjectClass>;
  constant slot g-permission-class-acquire :: <C-function-pointer>;
  constant slot g-permission-class-acquire-async :: <C-function-pointer>;
  constant slot g-permission-class-acquire-finish :: <C-function-pointer>;
  constant slot g-permission-class-release :: <C-function-pointer>;
  constant slot g-permission-class-release-async :: <C-function-pointer>;
  constant slot g-permission-class-release-finish :: <C-function-pointer>;
  constant slot g-permission-class-reserved :: <C-void*>;
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
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_input_stream_read_nonblocking";
end;

define C-struct <_GPollableInputStreamInterface>
  constant slot g-pollable-input-stream-interface-g-iface :: <GTypeInterface>;
  constant slot g-pollable-input-stream-interface-can-poll :: <C-function-pointer>;
  constant slot g-pollable-input-stream-interface-is-readable :: <C-function-pointer>;
  constant slot g-pollable-input-stream-interface-create-source :: <C-function-pointer>;
  constant slot g-pollable-input-stream-interface-read-nonblocking :: <C-function-pointer>;
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
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_pollable_output_stream_write_nonblocking";
end;

define C-struct <_GPollableOutputStreamInterface>
  constant slot g-pollable-output-stream-interface-g-iface :: <GTypeInterface>;
  constant slot g-pollable-output-stream-interface-can-poll :: <C-function-pointer>;
  constant slot g-pollable-output-stream-interface-is-writable :: <C-function-pointer>;
  constant slot g-pollable-output-stream-interface-create-source :: <C-function-pointer>;
  constant slot g-pollable-output-stream-interface-write-nonblocking :: <C-function-pointer>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GIOStream>;
  c-name: "g_proxy_connect_finish";
end;

define C-function g-proxy-supports-hostname
  input parameter self :: <GProxy>;
  result res :: <C-boolean>;
  c-name: "g_proxy_supports_hostname";
end;

define open C-subtype <GProxyAddress> (<GInetSocketAddress>)
  constant slot g-proxy-address-parent-instance :: <GInetSocketAddress>;
  constant slot g-proxy-address-priv :: <GProxyAddressPrivate>;
end C-subtype;

define C-pointer-type <GProxyAddress*> => <GProxyAddress>;

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

define C-function g-proxy-address-get-username
  input parameter self :: <GProxyAddress>;
  result res :: <C-string>;
  c-name: "g_proxy_address_get_username";
end;

define C-struct <_GProxyAddressClass>
  constant slot g-proxy-address-class-parent-class :: <GInetSocketAddressClass>;
  pointer-type-name: <GProxyAddressClass>;
end C-struct;

define open C-subtype <GProxyAddressEnumerator> (<GSocketAddressEnumerator>)
  constant slot g-proxy-address-enumerator-parent-instance :: <GSocketAddressEnumerator>;
  constant slot g-proxy-address-enumerator-priv :: <GProxyAddressEnumeratorPrivate>;
end C-subtype;

define C-pointer-type <GProxyAddressEnumerator*> => <GProxyAddressEnumerator>;

define C-struct <_GProxyAddressEnumeratorClass>
  constant slot g-proxy-address-enumerator-class-parent-class :: <GSocketAddressEnumeratorClass>;
  constant slot g-proxy-address-enumerator-class-_g-reserved1 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved2 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved3 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved4 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved5 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved6 :: <C-void*>;
  constant slot g-proxy-address-enumerator-class-_g-reserved7 :: <C-void*>;
  pointer-type-name: <GProxyAddressEnumeratorClass>;
end C-struct;

define C-struct <_GProxyAddressEnumeratorPrivate>
  pointer-type-name: <GProxyAddressEnumeratorPrivate>;
end C-struct;

define C-struct <_GProxyAddressPrivate>
  pointer-type-name: <GProxyAddressPrivate>;
end C-struct;

define C-struct <_GProxyInterface>
  constant slot g-proxy-interface-g-iface :: <GTypeInterface>;
  constant slot g-proxy-interface-connect :: <C-function-pointer>;
  constant slot g-proxy-interface-connect-async :: <C-function-pointer>;
  constant slot g-proxy-interface-connect-finish :: <C-function-pointer>;
  constant slot g-proxy-interface-supports-hostname :: <C-function-pointer>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_proxy_resolver_lookup_finish";
end;

define C-struct <_GProxyResolverInterface>
  constant slot g-proxy-resolver-interface-g-iface :: <GTypeInterface>;
  constant slot g-proxy-resolver-interface-is-supported :: <C-function-pointer>;
  constant slot g-proxy-resolver-interface-lookup :: <C-function-pointer>;
  constant slot g-proxy-resolver-interface-lookup-async :: <C-function-pointer>;
  constant slot g-proxy-resolver-interface-lookup-finish :: <C-function-pointer>;
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
  constant slot g-remote-action-group-interface-g-iface :: <GTypeInterface>;
  constant slot g-remote-action-group-interface-activate-action-full :: <C-function-pointer>;
  constant slot g-remote-action-group-interface-change-action-state-full :: <C-function-pointer>;
  pointer-type-name: <GRemoteActionGroupInterface>;
end C-struct;

define open C-subtype <GResolver> (<GObject>)
  constant slot g-resolver-parent-instance :: <GObject>;
  constant slot g-resolver-priv :: <GResolverPrivate>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_resolver_lookup_by_address_finish";
end;

define C-function g-resolver-lookup-by-name
  input parameter self :: <GResolver>;
  input parameter hostname_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_by_name_finish";
end;

define C-function g-resolver-lookup-service
  input parameter self :: <GResolver>;
  input parameter service_ :: <C-string>;
  input parameter protocol_ :: <C-string>;
  input parameter domain_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GList>;
  c-name: "g_resolver_lookup_service_finish";
end;

define C-function g-resolver-set-default
  input parameter self :: <GResolver>;
  c-name: "g_resolver_set_default";
end;

define C-struct <_GResolverClass>
  constant slot g-resolver-class-parent-class :: <GObjectClass>;
  constant slot g-resolver-class-reload :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-name :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-name-async :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-name-finish :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-address :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-address-async :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-by-address-finish :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-service :: <C-void*>;
  constant slot g-resolver-class-lookup-service-async :: <C-function-pointer>;
  constant slot g-resolver-class-lookup-service-finish :: <C-function-pointer>;
  constant slot g-resolver-class-_g-reserved1 :: <C-void*>;
  constant slot g-resolver-class-_g-reserved2 :: <C-void*>;
  constant slot g-resolver-class-_g-reserved3 :: <C-void*>;
  constant slot g-resolver-class-_g-reserved4 :: <C-void*>;
  constant slot g-resolver-class-_g-reserved5 :: <C-void*>;
  constant slot g-resolver-class-_g-reserved6 :: <C-void*>;
  pointer-type-name: <GResolverClass>;
end C-struct;

define constant $g-resolver-error-not-found = 0;
define constant $g-resolver-error-temporary-failure = 1;
define constant $g-resolver-error-internal = 2;
define constant <GResolverError> = <C-int>;
define C-pointer-type <GResolverError*> => <GResolverError>;

define C-struct <_GResolverPrivate>
  pointer-type-name: <GResolverPrivate>;
end C-struct;

define C-struct <_GResource>
  pointer-type-name: <GResource>;
end C-struct;

define C-function g-resource-new-from-data
  input parameter data_ :: <GBytes>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_resource_enumerate_children";
end;

define C-function g-resource-get-info
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter size_ :: <C-unsigned-long*>;
  output parameter flags_ :: <C-unsigned-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_resource_get_info";
end;

define C-function g-resource-lookup-data
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter error_ :: <GError*>;
  result res :: <GBytes>;
  c-name: "g_resource_lookup_data";
end;

define C-function g-resource-open-stream
  input parameter self :: <GResource>;
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GResource>;
  c-name: "g_resource_load";
end;

define constant $g-resource-error-not-found = 0;
define constant $g-resource-error-internal = 1;
define constant <GResourceError> = <C-int>;
define C-pointer-type <GResourceError*> => <GResourceError>;

define constant $g-resource-flags-none = 0;
define constant $g-resource-flags-compressed = 1;
define constant <GResourceFlags> = <C-int>;
define C-pointer-type <GResourceFlags*> => <GResourceFlags>;

define constant $g-resource-lookup-flags-none = 0;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_seekable_truncate";
end;

define C-struct <_GSeekableIface>
  constant slot g-seekable-iface-g-iface :: <GTypeInterface>;
  constant slot g-seekable-iface-tell :: <C-function-pointer>;
  constant slot g-seekable-iface-can-seek :: <C-function-pointer>;
  constant slot g-seekable-iface-seek :: <C-function-pointer>;
  constant slot g-seekable-iface-can-truncate :: <C-function-pointer>;
  constant slot g-seekable-iface-truncate-fn :: <C-function-pointer>;
  pointer-type-name: <GSeekableIface>;
end C-struct;

define open C-subtype <GSettings> (<GObject>)
  constant slot g-settings-parent-instance :: <GObject>;
  constant slot g-settings-priv :: <GSettingsPrivate>;
end C-subtype;

define C-pointer-type <GSettings*> => <GSettings>;

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

define constant $g-settings-bind-default = 0;
define constant $g-settings-bind-get = 1;
define constant $g-settings-bind-set = 2;
define constant $g-settings-bind-no-sensitivity = 4;
define constant $g-settings-bind-get-no-changes = 8;
define constant $g-settings-bind-invert-boolean = 16;
define constant <GSettingsBindFlags> = <C-int>;
define C-pointer-type <GSettingsBindFlags*> => <GSettingsBindFlags>;

define C-struct <_GSettingsClass>
  constant slot g-settings-class-parent-class :: <GObjectClass>;
  constant slot g-settings-class-writable-changed :: <C-function-pointer>;
  constant slot g-settings-class-changed :: <C-function-pointer>;
  constant slot g-settings-class-writable-change-event :: <C-function-pointer>;
  constant slot g-settings-class-change-event :: <C-function-pointer>;
  constant slot g-settings-class-padding :: <C-void*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-simple-action-group-parent-instance :: <GObject>;
  constant slot g-simple-action-group-priv :: <GSimpleActionGroupPrivate>;
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
  constant slot g-simple-action-group-class-parent-class :: <GObjectClass>;
  constant slot g-simple-action-group-class-padding :: <C-void*>;
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
  output parameter error_ :: <GError*>;
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

define open C-subtype <GSocket> (<GObject>)
  constant slot g-socket-parent-instance :: <GObject>;
  constant slot g-socket-priv :: <GSocketPrivate>;
end C-subtype;

define C-pointer-type <GSocket*> => <GSocket>;

define C-function g-socket-new
  input parameter family_ :: <GSocketFamily>;
  input parameter type_ :: <GSocketType>;
  input parameter protocol_ :: <GSocketProtocol>;
  output parameter error_ :: <GError*>;
  result res :: <GSocket>;
  c-name: "g_socket_new";
end;

define C-function g-socket-new-from-fd
  input parameter fd_ :: <C-signed-int>;
  output parameter error_ :: <GError*>;
  result res :: <GSocket>;
  c-name: "g_socket_new_from_fd";
end;

define C-function g-socket-accept
  input parameter self :: <GSocket>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GSocket>;
  c-name: "g_socket_accept";
end;

define C-function g-socket-bind
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter allow_reuse_ :: <C-boolean>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_bind";
end;

define C-function g-socket-check-connect-result
  input parameter self :: <GSocket>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_check_connect_result";
end;

define C-function g-socket-close
  input parameter self :: <GSocket>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_condition_timed_wait";
end;

define C-function g-socket-condition-wait
  input parameter self :: <GSocket>;
  input parameter condition_ :: <GIOCondition>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_condition_wait";
end;

define C-function g-socket-connect
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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

define C-function g-socket-get-protocol
  input parameter self :: <GSocket>;
  result res :: <GSocketProtocol>;
  c-name: "g_socket_get_protocol";
end;

define C-function g-socket-get-remote-address
  input parameter self :: <GSocket>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_join_multicast_group";
end;

define C-function g-socket-leave-multicast-group
  input parameter self :: <GSocket>;
  input parameter group_ :: <GInetAddress>;
  input parameter source_specific_ :: <C-boolean>;
  input parameter iface_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_leave_multicast_group";
end;

define C-function g-socket-listen
  input parameter self :: <GSocket>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_listen";
end;

define C-function g-socket-receive
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-string>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive";
end;

define C-function g-socket-receive-from
  input parameter self :: <GSocket>;
  output parameter address_ :: <GSocketAddress*>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive_message";
end;

define C-function g-socket-receive-with-blocking
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-string>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_socket_receive_with_blocking";
end;

define C-function g-socket-send
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send_message";
end;

define C-function g-socket-send-to
  input parameter self :: <GSocket>;
  input parameter address_ :: <GSocketAddress>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_socket_send_to";
end;

define C-function g-socket-send-with-blocking
  input parameter self :: <GSocket>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter blocking_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_shutdown";
end;

define C-function g-socket-speaks-ipv4
  input parameter self :: <GSocket>;
  result res :: <C-boolean>;
  c-name: "g_socket_speaks_ipv4";
end;

define open C-subtype <GSocketAddress> (<GObject>)
  constant slot g-socket-address-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GSocketAddress*> => <GSocketAddress>;

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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_address_to_native";
end;

define C-struct <_GSocketAddressClass>
  constant slot g-socket-address-class-parent-class :: <GObjectClass>;
  constant slot g-socket-address-class-get-family :: <C-function-pointer>;
  constant slot g-socket-address-class-get-native-size :: <C-function-pointer>;
  constant slot g-socket-address-class-to-native :: <C-function-pointer>;
  pointer-type-name: <GSocketAddressClass>;
end C-struct;

define open C-subtype <GSocketAddressEnumerator> (<GObject>)
  constant slot g-socket-address-enumerator-parent-instance :: <GObject>;
end C-subtype;

define C-pointer-type <GSocketAddressEnumerator*> => <GSocketAddressEnumerator>;

define C-function g-socket-address-enumerator-next
  input parameter self :: <GSocketAddressEnumerator>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_address_enumerator_next_finish";
end;

define C-struct <_GSocketAddressEnumeratorClass>
  constant slot g-socket-address-enumerator-class-parent-class :: <GObjectClass>;
  constant slot g-socket-address-enumerator-class-next :: <C-function-pointer>;
  constant slot g-socket-address-enumerator-class-next-async :: <C-function-pointer>;
  constant slot g-socket-address-enumerator-class-next-finish :: <C-function-pointer>;
  pointer-type-name: <GSocketAddressEnumeratorClass>;
end C-struct;

define C-struct <_GSocketClass>
  constant slot g-socket-class-parent-class :: <GObjectClass>;
  constant slot g-socket-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-class-_g-reserved4 :: <C-void*>;
  constant slot g-socket-class-_g-reserved5 :: <C-void*>;
  constant slot g-socket-class-_g-reserved6 :: <C-void*>;
  constant slot g-socket-class-_g-reserved7 :: <C-void*>;
  constant slot g-socket-class-_g-reserved8 :: <C-void*>;
  constant slot g-socket-class-_g-reserved9 :: <C-void*>;
  constant slot g-socket-class-_g-reserved10 :: <C-void*>;
  pointer-type-name: <GSocketClass>;
end C-struct;

define open C-subtype <GSocketClient> (<GObject>)
  constant slot g-socket-client-parent-instance :: <GObject>;
  constant slot g-socket-client-priv :: <GSocketClientPrivate>;
end C-subtype;

define C-pointer-type <GSocketClient*> => <GSocketClient>;

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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_finish";
end;

define C-function g-socket-client-connect-to-host
  input parameter self :: <GSocketClient>;
  input parameter host_and_port_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_host_finish";
end;

define C-function g-socket-client-connect-to-service
  input parameter self :: <GSocketClient>;
  input parameter domain_ :: <C-string>;
  input parameter service_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_client_connect_to_service_finish";
end;

define C-function g-socket-client-connect-to-uri
  input parameter self :: <GSocketClient>;
  input parameter uri_ :: <C-string>;
  input parameter default_port_ :: <C-unsigned-short>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-socket-client-class-parent-class :: <GObjectClass>;
  constant slot g-socket-client-class-event :: <C-function-pointer>;
  constant slot g-socket-client-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-client-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-client-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-client-class-_g-reserved4 :: <C-void*>;
  pointer-type-name: <GSocketClientClass>;
end C-struct;

define constant $g-socket-client-resolving = 0;
define constant $g-socket-client-resolved = 1;
define constant $g-socket-client-connecting = 2;
define constant $g-socket-client-connected = 3;
define constant $g-socket-client-proxy-negotiating = 4;
define constant $g-socket-client-proxy-negotiated = 5;
define constant $g-socket-client-tls-handshaking = 6;
define constant $g-socket-client-tls-handshaked = 7;
define constant $g-socket-client-complete = 8;
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
  constant slot g-socket-connectable-iface-g-iface :: <GTypeInterface>;
  constant slot g-socket-connectable-iface-enumerate :: <C-function-pointer>;
  constant slot g-socket-connectable-iface-proxy-enumerate :: <C-function-pointer>;
  pointer-type-name: <GSocketConnectableIface>;
end C-struct;

define open C-subtype <GSocketConnection> (<GIOStream>)
  constant slot g-socket-connection-parent-instance :: <GIOStream>;
  constant slot g-socket-connection-priv :: <GSocketConnectionPrivate>;
end C-subtype;

define C-pointer-type <GSocketConnection*> => <GSocketConnection>;

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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_connection_connect_finish";
end;

define C-function g-socket-connection-get-local-address
  input parameter self :: <GSocketConnection>;
  output parameter error_ :: <GError*>;
  result res :: <GSocketAddress>;
  c-name: "g_socket_connection_get_local_address";
end;

define C-function g-socket-connection-get-remote-address
  input parameter self :: <GSocketConnection>;
  output parameter error_ :: <GError*>;
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
  constant slot g-socket-connection-class-parent-class :: <GIOStreamClass>;
  constant slot g-socket-connection-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-connection-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-connection-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-connection-class-_g-reserved4 :: <C-void*>;
  constant slot g-socket-connection-class-_g-reserved5 :: <C-void*>;
  constant slot g-socket-connection-class-_g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketConnectionClass>;
end C-struct;

define C-struct <_GSocketConnectionPrivate>
  pointer-type-name: <GSocketConnectionPrivate>;
end C-struct;

define open C-subtype <GSocketControlMessage> (<GObject>)
  constant slot g-socket-control-message-parent-instance :: <GObject>;
  constant slot g-socket-control-message-priv :: <GSocketControlMessagePrivate>;
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
  constant slot g-socket-control-message-class-parent-class :: <GObjectClass>;
  constant slot g-socket-control-message-class-get-size :: <C-function-pointer>;
  constant slot g-socket-control-message-class-get-level :: <C-function-pointer>;
  constant slot g-socket-control-message-class-get-type :: <C-function-pointer>;
  constant slot g-socket-control-message-class-serialize :: <C-function-pointer>;
  constant slot g-socket-control-message-class-deserialize :: <C-void*>;
  constant slot g-socket-control-message-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-control-message-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-control-message-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-control-message-class-_g-reserved4 :: <C-void*>;
  constant slot g-socket-control-message-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GSocketControlMessageClass>;
end C-struct;

define C-struct <_GSocketControlMessagePrivate>
  pointer-type-name: <GSocketControlMessagePrivate>;
end C-struct;

define constant $g-socket-family-invalid = 0;
define constant $g-socket-family-unix = 1;
define constant $g-socket-family-ipv4 = 2;
define constant $g-socket-family-ipv6 = 10;
define constant <GSocketFamily> = <C-int>;
define C-pointer-type <GSocketFamily*> => <GSocketFamily>;

define open C-subtype <GSocketListener> (<GObject>)
  constant slot g-socket-listener-parent-instance :: <GObject>;
  constant slot g-socket-listener-priv :: <GSocketListenerPrivate>;
end C-subtype;

define C-pointer-type <GSocketListener*> => <GSocketListener>;

define C-function g-socket-listener-new
  result res :: <GSocketListener>;
  c-name: "g_socket_listener_new";
end;

define C-function g-socket-listener-accept
  input parameter self :: <GSocketListener>;
  output parameter source_object_ :: <GObject*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GSocketConnection>;
  c-name: "g_socket_listener_accept_finish";
end;

define C-function g-socket-listener-accept-socket
  input parameter self :: <GSocketListener>;
  output parameter source_object_ :: <GObject*>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_listener_add_address";
end;

define C-function g-socket-listener-add-any-inet-port
  input parameter self :: <GSocketListener>;
  input parameter source_object_ :: <GObject>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-short>;
  c-name: "g_socket_listener_add_any_inet_port";
end;

define C-function g-socket-listener-add-inet-port
  input parameter self :: <GSocketListener>;
  input parameter port_ :: <C-unsigned-short>;
  input parameter source_object_ :: <GObject>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_socket_listener_add_inet_port";
end;

define C-function g-socket-listener-add-socket
  input parameter self :: <GSocketListener>;
  input parameter socket_ :: <GSocket>;
  input parameter source_object_ :: <GObject>;
  output parameter error_ :: <GError*>;
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
  constant slot g-socket-listener-class-parent-class :: <GObjectClass>;
  constant slot g-socket-listener-class-changed :: <C-function-pointer>;
  constant slot g-socket-listener-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-listener-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-listener-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-listener-class-_g-reserved4 :: <C-void*>;
  constant slot g-socket-listener-class-_g-reserved5 :: <C-void*>;
  constant slot g-socket-listener-class-_g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketListenerClass>;
end C-struct;

define C-struct <_GSocketListenerPrivate>
  pointer-type-name: <GSocketListenerPrivate>;
end C-struct;

define constant $g-socket-msg-none = 0;
define constant $g-socket-msg-oob = 1;
define constant $g-socket-msg-peek = 2;
define constant $g-socket-msg-dontroute = 4;
define constant <GSocketMsgFlags> = <C-int>;
define C-pointer-type <GSocketMsgFlags*> => <GSocketMsgFlags>;

define C-struct <_GSocketPrivate>
  pointer-type-name: <GSocketPrivate>;
end C-struct;

define constant $g-socket-protocol-unknown = -1;
define constant $g-socket-protocol-default = 0;
define constant $g-socket-protocol-tcp = 6;
define constant $g-socket-protocol-udp = 17;
define constant $g-socket-protocol-sctp = 132;
define constant <GSocketProtocol> = <C-int>;
define C-pointer-type <GSocketProtocol*> => <GSocketProtocol>;

define open C-subtype <GSocketService> (<GSocketListener>)
  constant slot g-socket-service-parent-instance :: <GSocketListener>;
  constant slot g-socket-service-priv :: <GSocketServicePrivate>;
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
  constant slot g-socket-service-class-parent-class :: <GSocketListenerClass>;
  constant slot g-socket-service-class-incoming :: <C-function-pointer>;
  constant slot g-socket-service-class-_g-reserved1 :: <C-void*>;
  constant slot g-socket-service-class-_g-reserved2 :: <C-void*>;
  constant slot g-socket-service-class-_g-reserved3 :: <C-void*>;
  constant slot g-socket-service-class-_g-reserved4 :: <C-void*>;
  constant slot g-socket-service-class-_g-reserved5 :: <C-void*>;
  constant slot g-socket-service-class-_g-reserved6 :: <C-void*>;
  pointer-type-name: <GSocketServiceClass>;
end C-struct;

define C-struct <_GSocketServicePrivate>
  pointer-type-name: <GSocketServicePrivate>;
end C-struct;

define constant $g-socket-type-invalid = 0;
define constant $g-socket-type-stream = 1;
define constant $g-socket-type-datagram = 2;
define constant $g-socket-type-seqpacket = 3;
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
  slot g-static-resource-data :: <C-unsigned-char*>;
  slot g-static-resource-data-len :: <C-unsigned-long>;
  slot g-static-resource-resource :: <GResource>;
  slot g-static-resource-next :: <GStaticResource>;
  slot g-static-resource-padding :: <C-void*>;
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

define constant $tls-backend-extension-point-name = "gio-tls-backend";

define constant $tls-database-purpose-authenticate-client = "1.3.6.1.5.5.7.3.2";

define constant $tls-database-purpose-authenticate-server = "1.3.6.1.5.5.7.3.1";

define open C-subtype <GTcpConnection> (<GSocketConnection>)
  constant slot g-tcp-connection-parent-instance :: <GSocketConnection>;
  constant slot g-tcp-connection-priv :: <GTcpConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTcpConnection*> => <GTcpConnection>;

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
  constant slot g-tcp-connection-class-parent-class :: <GSocketConnectionClass>;
  pointer-type-name: <GTcpConnectionClass>;
end C-struct;

define C-struct <_GTcpConnectionPrivate>
  pointer-type-name: <GTcpConnectionPrivate>;
end C-struct;

define open C-subtype <GTcpWrapperConnection> (<GTcpConnection>)
  constant slot g-tcp-wrapper-connection-parent-instance :: <GTcpConnection>;
  constant slot g-tcp-wrapper-connection-priv :: <GTcpWrapperConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTcpWrapperConnection*> => <GTcpWrapperConnection>;

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
  constant slot g-tcp-wrapper-connection-class-parent-class :: <GTcpConnectionClass>;
  pointer-type-name: <GTcpWrapperConnectionClass>;
end C-struct;

define C-struct <_GTcpWrapperConnectionPrivate>
  pointer-type-name: <GTcpWrapperConnectionPrivate>;
end C-struct;

define open C-subtype <GThemedIcon> (<GObject>)
end C-subtype;

define C-pointer-type <GThemedIcon*> => <GThemedIcon>;

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
  constant slot g-threaded-socket-service-parent-instance :: <GSocketService>;
  constant slot g-threaded-socket-service-priv :: <GThreadedSocketServicePrivate>;
end C-subtype;

define C-pointer-type <GThreadedSocketService*> => <GThreadedSocketService>;

define C-function g-threaded-socket-service-new
  input parameter max_threads_ :: <C-signed-int>;
  result res :: <GSocketService>;
  c-name: "g_threaded_socket_service_new";
end;

define C-struct <_GThreadedSocketServiceClass>
  constant slot g-threaded-socket-service-class-parent-class :: <GSocketServiceClass>;
  constant slot g-threaded-socket-service-class-run :: <C-function-pointer>;
  constant slot g-threaded-socket-service-class-_g-reserved1 :: <C-void*>;
  constant slot g-threaded-socket-service-class-_g-reserved2 :: <C-void*>;
  constant slot g-threaded-socket-service-class-_g-reserved3 :: <C-void*>;
  constant slot g-threaded-socket-service-class-_g-reserved4 :: <C-void*>;
  constant slot g-threaded-socket-service-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GThreadedSocketServiceClass>;
end C-struct;

define C-struct <_GThreadedSocketServicePrivate>
  pointer-type-name: <GThreadedSocketServicePrivate>;
end C-struct;

define constant $g-tls-authentication-none = 0;
define constant $g-tls-authentication-requested = 1;
define constant $g-tls-authentication-required = 2;
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
  constant slot g-tls-backend-interface-g-iface :: <GTypeInterface>;
  constant slot g-tls-backend-interface-supports-tls :: <C-function-pointer>;
  constant slot g-tls-backend-interface-get-certificate-type :: <C-function-pointer>;
  constant slot g-tls-backend-interface-get-client-connection-type :: <C-function-pointer>;
  constant slot g-tls-backend-interface-get-server-connection-type :: <C-function-pointer>;
  constant slot g-tls-backend-interface-get-file-database-type :: <C-function-pointer>;
  constant slot g-tls-backend-interface-get-default-database :: <C-function-pointer>;
  pointer-type-name: <GTlsBackendInterface>;
end C-struct;

define open C-subtype <GTlsCertificate> (<GObject>)
  constant slot g-tls-certificate-parent-instance :: <GObject>;
  constant slot g-tls-certificate-priv :: <GTlsCertificatePrivate>;
end C-subtype;

define C-pointer-type <GTlsCertificate*> => <GTlsCertificate>;

define C-function g-tls-certificate-new-from-file
  input parameter file_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_file";
end;

define C-function g-tls-certificate-new-from-files
  input parameter cert_file_ :: <C-string>;
  input parameter key_file_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_files";
end;

define C-function g-tls-certificate-new-from-pem
  input parameter data_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_new_from_pem";
end;

define C-function g-tls-certificate-list-new-from-file
  input parameter file_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GList>;
  c-name: "g_tls_certificate_list_new_from_file";
end;

define C-function g-tls-certificate-get-issuer
  input parameter self :: <GTlsCertificate>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_certificate_get_issuer";
end;

define C-function g-tls-certificate-verify
  input parameter self :: <GTlsCertificate>;
  input parameter identity_ :: <GSocketConnectable>;
  input parameter trusted_ca_ :: <GTlsCertificate>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_certificate_verify";
end;

define C-struct <_GTlsCertificateClass>
  constant slot g-tls-certificate-class-parent-class :: <GObjectClass>;
  constant slot g-tls-certificate-class-verify :: <C-function-pointer>;
  constant slot g-tls-certificate-class-padding :: <C-void*>;
  pointer-type-name: <GTlsCertificateClass>;
end C-struct;

define constant $g-tls-certificate-unknown-ca = 1;
define constant $g-tls-certificate-bad-identity = 2;
define constant $g-tls-certificate-not-activated = 4;
define constant $g-tls-certificate-expired = 8;
define constant $g-tls-certificate-revoked = 16;
define constant $g-tls-certificate-insecure = 32;
define constant $g-tls-certificate-generic-error = 64;
define constant $g-tls-certificate-validate-all = 127;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-tls-client-connection-interface-g-iface :: <GTypeInterface>;
  pointer-type-name: <GTlsClientConnectionInterface>;
end C-struct;

define open C-subtype <GTlsConnection> (<GIOStream>)
  constant slot g-tls-connection-parent-instance :: <GIOStream>;
  constant slot g-tls-connection-priv :: <GTlsConnectionPrivate>;
end C-subtype;

define C-pointer-type <GTlsConnection*> => <GTlsConnection>;

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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  constant slot g-tls-connection-class-parent-class :: <GIOStreamClass>;
  constant slot g-tls-connection-class-accept-certificate :: <C-function-pointer>;
  constant slot g-tls-connection-class-handshake :: <C-function-pointer>;
  constant slot g-tls-connection-class-handshake-async :: <C-function-pointer>;
  constant slot g-tls-connection-class-handshake-finish :: <C-function-pointer>;
  constant slot g-tls-connection-class-padding :: <C-void*>;
  pointer-type-name: <GTlsConnectionClass>;
end C-struct;

define C-struct <_GTlsConnectionPrivate>
  pointer-type-name: <GTlsConnectionPrivate>;
end C-struct;

define open C-subtype <GTlsDatabase> (<GObject>)
  constant slot g-tls-database-parent-instance :: <GObject>;
  constant slot g-tls-database-priv :: <GTlsDatabasePrivate>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_for_handle_finish";
end;

define C-function g-tls-database-lookup-certificate-issuer
  input parameter self :: <GTlsDatabase>;
  input parameter certificate_ :: <GTlsCertificate>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificate>;
  c-name: "g_tls_database_lookup_certificate_issuer_finish";
end;

define C-function g-tls-database-lookup-certificates-issued-by
  input parameter self :: <GTlsDatabase>;
  input parameter issuer_raw_dn_ :: <GByteArray>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseLookupFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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

define C-function g-tls-database-verify-chain
  input parameter self :: <GTlsDatabase>;
  input parameter chain_ :: <GTlsCertificate>;
  input parameter purpose_ :: <C-string>;
  input parameter identity_ :: <GSocketConnectable>;
  input parameter interaction_ :: <GTlsInteraction>;
  input parameter flags_ :: <GTlsDatabaseVerifyFlags>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GTlsCertificateFlags>;
  c-name: "g_tls_database_verify_chain_finish";
end;

define C-struct <_GTlsDatabaseClass>
  constant slot g-tls-database-class-parent-class :: <GObjectClass>;
  constant slot g-tls-database-class-verify-chain :: <C-function-pointer>;
  constant slot g-tls-database-class-verify-chain-async :: <C-function-pointer>;
  constant slot g-tls-database-class-verify-chain-finish :: <C-function-pointer>;
  constant slot g-tls-database-class-create-certificate-handle :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-for-handle :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-for-handle-async :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-for-handle-finish :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-issuer :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-issuer-async :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificate-issuer-finish :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificates-issued-by :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificates-issued-by-async :: <C-function-pointer>;
  constant slot g-tls-database-class-lookup-certificates-issued-by-finish :: <C-void*>;
  constant slot g-tls-database-class-padding :: <C-void*>;
  pointer-type-name: <GTlsDatabaseClass>;
end C-struct;

define constant $g-tls-database-lookup-none = 0;
define constant $g-tls-database-lookup-keypair = 1;
define constant <GTlsDatabaseLookupFlags> = <C-int>;
define C-pointer-type <GTlsDatabaseLookupFlags*> => <GTlsDatabaseLookupFlags>;

define C-struct <_GTlsDatabasePrivate>
  pointer-type-name: <GTlsDatabasePrivate>;
end C-struct;

define constant $g-tls-database-verify-none = 0;
define constant <GTlsDatabaseVerifyFlags> = <C-int>;
define C-pointer-type <GTlsDatabaseVerifyFlags*> => <GTlsDatabaseVerifyFlags>;

define constant $g-tls-error-unavailable = 0;
define constant $g-tls-error-misc = 1;
define constant $g-tls-error-bad-certificate = 2;
define constant $g-tls-error-not-tls = 3;
define constant $g-tls-error-handshake = 4;
define constant $g-tls-error-certificate-required = 5;
define constant $g-tls-error-eof = 6;
define constant <GTlsError> = <C-int>;
define C-pointer-type <GTlsError*> => <GTlsError>;

// Interface
define open C-subtype <GTlsFileDatabase> (<GTlsDatabase>)
end C-subtype;

define C-pointer-type <GTlsFileDatabase*> => <GTlsFileDatabase>;

define C-function g-tls-file-database-new
  input parameter anchors_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsFileDatabase>;
  c-name: "g_tls_file_database_new";
end;

define C-struct <_GTlsFileDatabaseInterface>
  constant slot g-tls-file-database-interface-g-iface :: <GTypeInterface>;
  constant slot g-tls-file-database-interface-padding :: <C-void*>;
  pointer-type-name: <GTlsFileDatabaseInterface>;
end C-struct;

define open C-subtype <GTlsInteraction> (<GObject>)
  constant slot g-tls-interaction-parent-instance :: <GObject>;
  constant slot g-tls-interaction-priv :: <GTlsInteractionPrivate>;
end C-subtype;

define C-pointer-type <GTlsInteraction*> => <GTlsInteraction>;

define C-function g-tls-interaction-ask-password
  input parameter self :: <GTlsInteraction>;
  input parameter password_ :: <GTlsPassword>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GTlsInteractionResult>;
  c-name: "g_tls_interaction_ask_password_finish";
end;

define C-function g-tls-interaction-invoke-ask-password
  input parameter self :: <GTlsInteraction>;
  input parameter password_ :: <GTlsPassword>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsInteractionResult>;
  c-name: "g_tls_interaction_invoke_ask_password";
end;

define C-struct <_GTlsInteractionClass>
  constant slot g-tls-interaction-class-parent-class :: <GObjectClass>;
  constant slot g-tls-interaction-class-ask-password :: <C-function-pointer>;
  constant slot g-tls-interaction-class-ask-password-async :: <C-function-pointer>;
  constant slot g-tls-interaction-class-ask-password-finish :: <C-function-pointer>;
  constant slot g-tls-interaction-class-padding :: <C-void*>;
  pointer-type-name: <GTlsInteractionClass>;
end C-struct;

define C-struct <_GTlsInteractionPrivate>
  pointer-type-name: <GTlsInteractionPrivate>;
end C-struct;

define constant $g-tls-interaction-unhandled = 0;
define constant $g-tls-interaction-handled = 1;
define constant $g-tls-interaction-failed = 2;
define constant <GTlsInteractionResult> = <C-int>;
define C-pointer-type <GTlsInteractionResult*> => <GTlsInteractionResult>;

define open C-subtype <GTlsPassword> (<GObject>)
  constant slot g-tls-password-parent-instance :: <GObject>;
  constant slot g-tls-password-priv :: <GTlsPasswordPrivate>;
end C-subtype;

define C-pointer-type <GTlsPassword*> => <GTlsPassword>;

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
  constant slot g-tls-password-class-parent-class :: <GObjectClass>;
  constant slot g-tls-password-class-get-value :: <C-function-pointer>;
  constant slot g-tls-password-class-set-value :: <C-function-pointer>;
  constant slot g-tls-password-class-get-default-warning :: <C-function-pointer>;
  constant slot g-tls-password-class-padding :: <C-void*>;
  pointer-type-name: <GTlsPasswordClass>;
end C-struct;

define constant $g-tls-password-none = 0;
define constant $g-tls-password-retry = 2;
define constant $g-tls-password-many-tries = 4;
define constant $g-tls-password-final-try = 8;
define constant <GTlsPasswordFlags> = <C-int>;
define C-pointer-type <GTlsPasswordFlags*> => <GTlsPasswordFlags>;

define C-struct <_GTlsPasswordPrivate>
  pointer-type-name: <GTlsPasswordPrivate>;
end C-struct;

define constant $g-tls-rehandshake-never = 0;
define constant $g-tls-rehandshake-safely = 1;
define constant $g-tls-rehandshake-unsafely = 2;
define constant <GTlsRehandshakeMode> = <C-int>;
define C-pointer-type <GTlsRehandshakeMode*> => <GTlsRehandshakeMode>;

// Interface
define open C-subtype <GTlsServerConnection> (<GTlsConnection>)
end C-subtype;

define C-pointer-type <GTlsServerConnection*> => <GTlsServerConnection>;

define C-function g-tls-server-connection-new
  input parameter base_io_stream_ :: <GIOStream>;
  input parameter certificate_ :: <GTlsCertificate>;
  output parameter error_ :: <GError*>;
  result res :: <GTlsServerConnection>;
  c-name: "g_tls_server_connection_new";
end;

define C-struct <_GTlsServerConnectionInterface>
  constant slot g-tls-server-connection-interface-g-iface :: <GTypeInterface>;
  pointer-type-name: <GTlsServerConnectionInterface>;
end C-struct;

define open C-subtype <GUnixConnection> (<GSocketConnection>)
  constant slot g-unix-connection-parent-instance :: <GSocketConnection>;
  constant slot g-unix-connection-priv :: <GUnixConnectionPrivate>;
end C-subtype;

define C-pointer-type <GUnixConnection*> => <GUnixConnection>;

define C-function g-unix-connection-receive-credentials
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GCredentials>;
  c-name: "g_unix_connection_receive_credentials_finish";
end;

define C-function g-unix-connection-receive-fd
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_unix_connection_receive_fd";
end;

define C-function g-unix-connection-send-credentials
  input parameter self :: <GUnixConnection>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_unix_connection_send_credentials_finish";
end;

define C-function g-unix-connection-send-fd
  input parameter self :: <GUnixConnection>;
  input parameter fd_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_unix_connection_send_fd";
end;

define C-struct <_GUnixConnectionClass>
  constant slot g-unix-connection-class-parent-class :: <GSocketConnectionClass>;
  pointer-type-name: <GUnixConnectionClass>;
end C-struct;

define C-struct <_GUnixConnectionPrivate>
  pointer-type-name: <GUnixConnectionPrivate>;
end C-struct;

define open C-subtype <GUnixCredentialsMessage> (<GSocketControlMessage>)
  constant slot g-unix-credentials-message-parent-instance :: <GSocketControlMessage>;
  constant slot g-unix-credentials-message-priv :: <GUnixCredentialsMessagePrivate>;
end C-subtype;

define C-pointer-type <GUnixCredentialsMessage*> => <GUnixCredentialsMessage>;

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
  constant slot g-unix-credentials-message-class-parent-class :: <GSocketControlMessageClass>;
  constant slot g-unix-credentials-message-class-_g-reserved1 :: <C-void*>;
  constant slot g-unix-credentials-message-class-_g-reserved2 :: <C-void*>;
  pointer-type-name: <GUnixCredentialsMessageClass>;
end C-struct;

define C-struct <_GUnixCredentialsMessagePrivate>
  pointer-type-name: <GUnixCredentialsMessagePrivate>;
end C-struct;

define open C-subtype <GUnixFDList> (<GObject>)
  constant slot g-unix-fd-list-parent-instance :: <GObject>;
  constant slot g-unix-fd-list-priv :: <GUnixFDListPrivate>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_unix_fd_list_append";
end;

define C-function g-unix-fd-list-get
  input parameter self :: <GUnixFDList>;
  input parameter index__ :: <C-signed-int>;
  output parameter error_ :: <GError*>;
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
  constant slot g-unix-fd-list-class-parent-class :: <GObjectClass>;
  constant slot g-unix-fd-list-class-_g-reserved1 :: <C-void*>;
  constant slot g-unix-fd-list-class-_g-reserved2 :: <C-void*>;
  constant slot g-unix-fd-list-class-_g-reserved3 :: <C-void*>;
  constant slot g-unix-fd-list-class-_g-reserved4 :: <C-void*>;
  constant slot g-unix-fd-list-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GUnixFDListClass>;
end C-struct;

define C-struct <_GUnixFDListPrivate>
  pointer-type-name: <GUnixFDListPrivate>;
end C-struct;

define open C-subtype <GUnixFDMessage> (<GSocketControlMessage>)
  constant slot g-unix-fd-message-parent-instance :: <GSocketControlMessage>;
  constant slot g-unix-fd-message-priv :: <GUnixFDMessagePrivate>;
end C-subtype;

define C-pointer-type <GUnixFDMessage*> => <GUnixFDMessage>;

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
  output parameter error_ :: <GError*>;
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
  constant slot g-unix-fd-message-class-parent-class :: <GSocketControlMessageClass>;
  constant slot g-unix-fd-message-class-_g-reserved1 :: <C-void*>;
  constant slot g-unix-fd-message-class-_g-reserved2 :: <C-void*>;
  pointer-type-name: <GUnixFDMessageClass>;
end C-struct;

define C-struct <_GUnixFDMessagePrivate>
  pointer-type-name: <GUnixFDMessagePrivate>;
end C-struct;

define open C-subtype <GUnixInputStream> (<GInputStream>)
  constant slot g-unix-input-stream-parent-instance :: <GInputStream>;
  constant slot g-unix-input-stream-priv :: <GUnixInputStreamPrivate>;
end C-subtype;

define C-pointer-type <GUnixInputStream*> => <GUnixInputStream>;

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
  constant slot g-unix-input-stream-class-parent-class :: <GInputStreamClass>;
  constant slot g-unix-input-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-unix-input-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-unix-input-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-unix-input-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-unix-input-stream-class-_g-reserved5 :: <C-void*>;
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
  constant slot g-unix-output-stream-parent-instance :: <GOutputStream>;
  constant slot g-unix-output-stream-priv :: <GUnixOutputStreamPrivate>;
end C-subtype;

define C-pointer-type <GUnixOutputStream*> => <GUnixOutputStream>;

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
  constant slot g-unix-output-stream-class-parent-class :: <GOutputStreamClass>;
  constant slot g-unix-output-stream-class-_g-reserved1 :: <C-void*>;
  constant slot g-unix-output-stream-class-_g-reserved2 :: <C-void*>;
  constant slot g-unix-output-stream-class-_g-reserved3 :: <C-void*>;
  constant slot g-unix-output-stream-class-_g-reserved4 :: <C-void*>;
  constant slot g-unix-output-stream-class-_g-reserved5 :: <C-void*>;
  pointer-type-name: <GUnixOutputStreamClass>;
end C-struct;

define C-struct <_GUnixOutputStreamPrivate>
  pointer-type-name: <GUnixOutputStreamPrivate>;
end C-struct;

define open C-subtype <GUnixSocketAddress> (<GSocketAddress>)
  constant slot g-unix-socket-address-parent-instance :: <GSocketAddress>;
  constant slot g-unix-socket-address-priv :: <GUnixSocketAddressPrivate>;
end C-subtype;

define C-pointer-type <GUnixSocketAddress*> => <GUnixSocketAddress>;

define C-function g-unix-socket-address-new
  input parameter path_ :: <C-string>;
  result res :: <GSocketAddress>;
  c-name: "g_unix_socket_address_new";
end;

define C-function g-unix-socket-address-new-abstract
  input parameter path_ :: <C-unsigned-char*>;
  input parameter path_len_ :: <C-signed-int>;
  result res :: <GSocketAddress>;
  c-name: "g_unix_socket_address_new_abstract";
end;

define C-function g-unix-socket-address-new-with-type
  input parameter path_ :: <C-unsigned-char*>;
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
  constant slot g-unix-socket-address-class-parent-class :: <GSocketAddressClass>;
  pointer-type-name: <GUnixSocketAddressClass>;
end C-struct;

define C-struct <_GUnixSocketAddressPrivate>
  pointer-type-name: <GUnixSocketAddressPrivate>;
end C-struct;

define constant $g-unix-socket-address-invalid = 0;
define constant $g-unix-socket-address-anonymous = 1;
define constant $g-unix-socket-address-path = 2;
define constant $g-unix-socket-address-abstract = 3;
define constant $g-unix-socket-address-abstract-padded = 4;
define constant <GUnixSocketAddressType> = <C-int>;
define C-pointer-type <GUnixSocketAddressType*> => <GUnixSocketAddressType>;

define constant $vfs-extension-point-name = "gio-vfs";

define constant $volume-identifier-kind-class = "class";

define constant $volume-identifier-kind-hal-udi = "hal-udi";

define constant $volume-identifier-kind-label = "label";

define constant $volume-identifier-kind-nfs-mount = "nfs-mount";

define constant $volume-identifier-kind-unix-device = "unix-device";

define constant $volume-identifier-kind-uuid = "uuid";

define constant $volume-monitor-extension-point-name = "gio-volume-monitor";

define open C-subtype <GVfs> (<GObject>)
  constant slot g-vfs-parent-instance :: <GObject>;
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
  constant slot g-vfs-class-parent-class :: <GObjectClass>;
  constant slot g-vfs-class-is-active :: <C-function-pointer>;
  constant slot g-vfs-class-get-file-for-path :: <C-function-pointer>;
  constant slot g-vfs-class-get-file-for-uri :: <C-function-pointer>;
  constant slot g-vfs-class-get-supported-uri-schemes :: <C-function-pointer>;
  constant slot g-vfs-class-parse-name :: <C-function-pointer>;
  constant slot g-vfs-class-local-file-add-info :: <C-function-pointer>;
  constant slot g-vfs-class-add-writable-namespaces :: <C-function-pointer>;
  constant slot g-vfs-class-local-file-set-attributes :: <C-function-pointer>;
  constant slot g-vfs-class-local-file-removed :: <C-function-pointer>;
  constant slot g-vfs-class-local-file-moved :: <C-function-pointer>;
  constant slot g-vfs-class-_g-reserved1 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved2 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved3 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved4 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved5 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved6 :: <C-void*>;
  constant slot g-vfs-class-_g-reserved7 :: <C-void*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_volume_mount_finish";
end;

define C-function g-volume-should-automount
  input parameter self :: <GVolume>;
  result res :: <C-boolean>;
  c-name: "g_volume_should_automount";
end;

define C-struct <_GVolumeIface>
  constant slot g-volume-iface-g-iface :: <GTypeInterface>;
  constant slot g-volume-iface-changed :: <C-function-pointer>;
  constant slot g-volume-iface-removed :: <C-function-pointer>;
  constant slot g-volume-iface-get-name :: <C-function-pointer>;
  constant slot g-volume-iface-get-icon :: <C-function-pointer>;
  constant slot g-volume-iface-get-uuid :: <C-function-pointer>;
  constant slot g-volume-iface-get-drive :: <C-function-pointer>;
  constant slot g-volume-iface-get-mount :: <C-function-pointer>;
  constant slot g-volume-iface-can-mount :: <C-function-pointer>;
  constant slot g-volume-iface-can-eject :: <C-function-pointer>;
  constant slot g-volume-iface-mount-fn :: <C-function-pointer>;
  constant slot g-volume-iface-mount-finish :: <C-function-pointer>;
  constant slot g-volume-iface-eject :: <C-function-pointer>;
  constant slot g-volume-iface-eject-finish :: <C-function-pointer>;
  constant slot g-volume-iface-get-identifier :: <C-function-pointer>;
  constant slot g-volume-iface-enumerate-identifiers :: <C-function-pointer>;
  constant slot g-volume-iface-should-automount :: <C-function-pointer>;
  constant slot g-volume-iface-get-activation-root :: <C-function-pointer>;
  constant slot g-volume-iface-eject-with-operation :: <C-function-pointer>;
  constant slot g-volume-iface-eject-with-operation-finish :: <C-function-pointer>;
  constant slot g-volume-iface-get-sort-key :: <C-function-pointer>;
  pointer-type-name: <GVolumeIface>;
end C-struct;

define open C-subtype <GVolumeMonitor> (<GObject>)
  constant slot g-volume-monitor-parent-instance :: <GObject>;
  constant slot g-volume-monitor-priv :: <C-void*>;
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
  constant slot g-volume-monitor-class-parent-class :: <GObjectClass>;
  constant slot g-volume-monitor-class-volume-added :: <C-function-pointer>;
  constant slot g-volume-monitor-class-volume-removed :: <C-function-pointer>;
  constant slot g-volume-monitor-class-volume-changed :: <C-function-pointer>;
  constant slot g-volume-monitor-class-mount-added :: <C-function-pointer>;
  constant slot g-volume-monitor-class-mount-removed :: <C-function-pointer>;
  constant slot g-volume-monitor-class-mount-pre-unmount :: <C-function-pointer>;
  constant slot g-volume-monitor-class-mount-changed :: <C-function-pointer>;
  constant slot g-volume-monitor-class-drive-connected :: <C-function-pointer>;
  constant slot g-volume-monitor-class-drive-disconnected :: <C-function-pointer>;
  constant slot g-volume-monitor-class-drive-changed :: <C-function-pointer>;
  constant slot g-volume-monitor-class-is-supported :: <C-function-pointer>;
  constant slot g-volume-monitor-class-get-connected-drives :: <C-function-pointer>;
  constant slot g-volume-monitor-class-get-volumes :: <C-function-pointer>;
  constant slot g-volume-monitor-class-get-mounts :: <C-function-pointer>;
  constant slot g-volume-monitor-class-get-volume-for-uuid :: <C-function-pointer>;
  constant slot g-volume-monitor-class-get-mount-for-uuid :: <C-function-pointer>;
  constant slot g-volume-monitor-class-adopt-orphan-mount :: <C-void*>;
  constant slot g-volume-monitor-class-drive-eject-button :: <C-function-pointer>;
  constant slot g-volume-monitor-class-drive-stop-button :: <C-function-pointer>;
  constant slot g-volume-monitor-class-_g-reserved1 :: <C-void*>;
  constant slot g-volume-monitor-class-_g-reserved2 :: <C-void*>;
  constant slot g-volume-monitor-class-_g-reserved3 :: <C-void*>;
  constant slot g-volume-monitor-class-_g-reserved4 :: <C-void*>;
  constant slot g-volume-monitor-class-_g-reserved5 :: <C-void*>;
  constant slot g-volume-monitor-class-_g-reserved6 :: <C-void*>;
  pointer-type-name: <GVolumeMonitorClass>;
end C-struct;

define open C-subtype <GZlibCompressor> (<GObject>)
end C-subtype;

define C-pointer-type <GZlibCompressor*> => <GZlibCompressor>;

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
  constant slot g-zlib-compressor-class-parent-class :: <GObjectClass>;
  pointer-type-name: <GZlibCompressorClass>;
end C-struct;

define constant $g-zlib-compressor-format-zlib = 0;
define constant $g-zlib-compressor-format-gzip = 1;
define constant $g-zlib-compressor-format-raw = 2;
define constant <GZlibCompressorFormat> = <C-int>;
define C-pointer-type <GZlibCompressorFormat*> => <GZlibCompressorFormat>;

define open C-subtype <GZlibDecompressor> (<GObject>)
end C-subtype;

define C-pointer-type <GZlibDecompressor*> => <GZlibDecompressor>;

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
  constant slot g-zlib-decompressor-class-parent-class :: <GObjectClass>;
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
  output parameter error_ :: <GError*>;
  result res :: <GDBusConnection>;
  c-name: "g_bus_get_finish";
end;

define C-function g-bus-get-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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

define C-function g-dbus-address-get-for-bus-sync
  input parameter bus_type_ :: <GBusType>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  output parameter error_ :: <GError*>;
  result res :: <GIOStream>;
  c-name: "g_dbus_address_get_stream_finish";
end;

define C-function g-dbus-address-get-stream-sync
  input parameter address_ :: <C-string>;
  input parameter out_guid_ :: <C-string>;
  input parameter cancellable_ :: <GCancellable>;
  output parameter error_ :: <GError*>;
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
  input parameter out_gvalue_ :: <GValue>;
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
  output parameter error_ :: <GError*>;
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

define C-function g-io-scheduler-cancel-all-jobs
  c-name: "g_io_scheduler_cancel_all_jobs";
end;

define C-function g-io-scheduler-push-job
  input parameter job_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  input parameter io_priority_ :: <C-signed-int>;
  input parameter cancellable_ :: <GCancellable>;
  c-name: "g_io_scheduler_push_job";
end;

define C-function g-pollable-source-new
  input parameter pollable_stream_ :: <GObject>;
  result res :: <GSource>;
  c-name: "g_pollable_source_new";
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
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_resources_enumerate_children";
end;

define C-function g-resources-get-info
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter size_ :: <C-unsigned-long*>;
  output parameter flags_ :: <C-unsigned-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_resources_get_info";
end;

define C-function g-resources-lookup-data
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter error_ :: <GError*>;
  result res :: <GBytes>;
  c-name: "g_resources_lookup_data";
end;

define C-function g-resources-open-stream
  input parameter path_ :: <C-string>;
  input parameter lookup_flags_ :: <GResourceLookupFlags>;
  output parameter error_ :: <GError*>;
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

