module: win32-common-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("win32_common");
module-namer("win32_common");

variable-namer("_L_wndproc_G__key", <wndproc>-key);

variable-namer("_L_dlgproc_G__key", <dlgproc>-key);

variable-namer("_L_lpofnhookproc_G__key", <lpofnhookproc>-key);

variable-namer("_L_lpcchookproc_G__key", <lpcchookproc>-key);

variable-namer("_L_lpfrhookproc_G__key", <lpfrhookproc>-key);

variable-namer("_L_lpcfhookproc_G__key", <lpcfhookproc>-key);

variable-namer("_L_lpprinthookproc_G__key", <lpprinthookproc>-key);

variable-namer("_L_lpsetuphookproc_G__key", <lpsetuphookproc>-key);

constant-namer("register_callback", register-callback);

constant-namer("application_instance_handle", application-instance-handle);

constant-namer("application_command_line", application-command-line);

constant-namer("application_show_window", application-show-window);

variable-namer("_D_null_handle", $null-handle);

variable-namer("_D_null_hwnd", $null-hwnd);

variable-namer("_D_null_rect", $null-rect);

variable-namer("_D_null_point", $null-point);

variable-namer("_D_null_void", $null-void);

variable-namer("_D_null_string", $null-string);

constant-namer("null_handle", null-handle);

constant-namer("null_handle_Q_", null-handle?);

constant-namer("_P_free", %free);

constant-namer("makeintresource", makeintresource);

variable-namer("_D_ffffffff", $ffffffff);

constant-namer("text", text);

constant-namer("makelangid", makelangid);

constant-namer("primarylangid", primarylangid);

constant-namer("sublangid", sublangid);

constant-namer("loword", loword);

constant-namer("hiword", hiword);

constant-namer("makelong", makelong);

constant-namer("lparam_to_xy", lparam-to-xy);

constant-namer("pointer_address", pointer-address);

constant-namer("_L_c_unicode_string_G_", <c-unicode-string>);

constant-namer("_L_large_integer_G_", <large-integer>);

constant-namer("_L_plarge_integer_G_", <plarge-integer>);

constant-namer("_L_ularge_integer_G_", <ularge-integer>);

constant-namer("_L_pularge_integer_G_", <pularge-integer>);

constant-namer("_L_handle_G_", <handle>);

constant-namer("_L_phandle_G_", <phandle>);

variable-namer("_D_application_error_mask", $application-error-mask);

variable-namer("_D_error_severity_informational", $error-severity-informational);

variable-namer("_D_error_severity_warning", $error-severity-warning);

variable-namer("_D_error_severity_error", $error-severity-error);

constant-namer("_L_luid_G_", <luid>);

constant-namer("_L_pluid_G_", <pluid>);

variable-namer("_D_unicode_null", $unicode-null);

constant-namer("_L_boolean_byte_G_", <boolean-byte>);

constant-namer("flink_value", flink-value);

constant-namer("flink_value_setter", flink-value-setter);

constant-namer("blink_value", blink-value);

constant-namer("blink_value_setter", blink-value-setter);

constant-namer("_L_list_entry_G_", <list-entry>);

constant-namer("_L_plist_entry_G_", <plist-entry>);

constant-namer("_L_restricted_pointer_G_", <restricted-pointer>);

constant-namer("_L_prlist_entry_G_", <prlist-entry>);

constant-namer("next_value", next-value);

constant-namer("next_value_setter", next-value-setter);

constant-namer("_L_single_list_entry_G_", <single-list-entry>);

constant-namer("_L_psingle_list_entry_G_", <psingle-list-entry>);

variable-namer("_D_maxlong", $maxlong);

variable-namer("_D_maxdword", $maxdword);

constant-namer("_L_wchar_G_", <wchar>);

constant-namer("_L_pwchar_G_", <pwchar>);

constant-namer("_L_lpwch_G_", <lpwch>);

constant-namer("_L_pwch_G_", <pwch>);

constant-namer("_L_lpcwch_G_", <lpcwch>);

constant-namer("_L_pcwch_G_", <pcwch>);

constant-namer("_L_nwpstr_G_", <nwpstr>);

constant-namer("_L_lpwstr_G_", <lpwstr>);

constant-namer("_L_pwstr_G_", <pwstr>);

constant-namer("_L_lpcwstr_G_", <lpcwstr>);

constant-namer("_L_pcwstr_G_", <pcwstr>);

constant-namer("_L_hwnd_G_", <hwnd>);

constant-namer("_L_hhook_G_", <hhook>);

constant-namer("_L_sphandle_G_", <sphandle>);

constant-namer("_L_lphandle_G_", <lphandle>);

constant-namer("_L_hglobal_G_", <hglobal>);

constant-namer("_L_hlocal_G_", <hlocal>);

constant-namer("_L_globalhandle_G_", <globalhandle>);

constant-namer("_L_localhandle_G_", <localhandle>);

constant-namer("_L_haccel_G_", <haccel>);

constant-namer("_L_hbitmap_G_", <hbitmap>);

constant-namer("_L_hbrush_G_", <hbrush>);

constant-namer("_L_hdc_G_", <hdc>);

constant-namer("_L_hglrc_G_", <hglrc>);

constant-namer("_L_hdesk_G_", <hdesk>);

constant-namer("_L_henhmetafile_G_", <henhmetafile>);

constant-namer("_L_hfont_G_", <hfont>);

constant-namer("_L_hicon_G_", <hicon>);

constant-namer("_L_hmenu_G_", <hmenu>);

constant-namer("_L_hmetafile_G_", <hmetafile>);

constant-namer("_L_hinstance_G_", <hinstance>);

constant-namer("_L_hmodule_G_", <hmodule>);

constant-namer("_L_hpalette_G_", <hpalette>);

constant-namer("_L_hpen_G_", <hpen>);

constant-namer("_L_hrgn_G_", <hrgn>);

constant-namer("_L_hrsrc_G_", <hrsrc>);

constant-namer("_L_hstr_G_", <hstr>);

constant-namer("_L_htask_G_", <htask>);

constant-namer("_L_hwinsta_G_", <hwinsta>);

constant-namer("_L_hkl_G_", <hkl>);

constant-namer("_L_hcursor_G_", <hcursor>);

constant-namer("left_value", left-value);

constant-namer("left_value_setter", left-value-setter);

constant-namer("top_value", top-value);

constant-namer("top_value_setter", top-value-setter);

constant-namer("right_value", right-value);

constant-namer("right_value_setter", right-value-setter);

constant-namer("bottom_value", bottom-value);

constant-namer("bottom_value_setter", bottom-value-setter);

constant-namer("_L_rect_G_", <rect>);

constant-namer("_L_prect_G_", <prect>);

constant-namer("_L_lprect_G_", <lprect>);

constant-namer("_L_lpcrect_G_", <lpcrect>);

constant-namer("_L_rectl_G_", <rectl>);

constant-namer("_L_prectl_G_", <prectl>);

constant-namer("_L_lprectl_G_", <lprectl>);

constant-namer("_L_lpcrectl_G_", <lpcrectl>);

constant-namer("x_value", x-value);

constant-namer("x_value_setter", x-value-setter);

constant-namer("y_value", y-value);

constant-namer("y_value_setter", y-value-setter);

constant-namer("_L_point_G_", <point>);

constant-namer("_L_ppoint_G_", <ppoint>);

constant-namer("_L_lppoint_G_", <lppoint>);

constant-namer("_L_pointl_G_", <pointl>);

constant-namer("_L_ppointl_G_", <ppointl>);

constant-namer("cx_value", cx-value);

constant-namer("cx_value_setter", cx-value-setter);

constant-namer("cy_value", cy-value);

constant-namer("cy_value_setter", cy-value-setter);

constant-namer("_L_size_G_", <size>);

constant-namer("_L_psize_G_", <psize>);

constant-namer("_L_lpsize_G_", <lpsize>);

constant-namer("_L_sizel_G_", <sizel>);

constant-namer("_L_psizel_G_", <psizel>);

constant-namer("_L_lpsizel_G_", <lpsizel>);

constant-namer("_L_points_G_", <points>);

constant-namer("_L_ppoints_G_", <ppoints>);

constant-namer("_L_lppoints_G_", <lppoints>);
