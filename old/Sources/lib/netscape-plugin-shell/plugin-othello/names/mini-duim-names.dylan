module: mini-duim-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("mini_duim");

module-namer("mini_duim");

constant-namer("_P_layout", %layout);

constant-namer("_P_layout_setter", %layout-setter);

constant-namer("_P_menu_bar", %menu-bar);

constant-namer("_P_menu_bar_setter", %menu-bar-setter);

constant-namer("_P_tool_bar", %tool-bar);

constant-namer("_P_tool_bar_setter", %tool-bar-setter);

constant-namer("_P_status_bar", %status-bar);

constant-namer("_P_status_bar_setter", %status-bar-setter);

constant-namer("enter_debugger", enter-debugger);

constant-namer("_L_frame_G_", <frame>);

constant-namer("frame_layout", frame-layout);

constant-namer("frame_mapped_Q_", frame-mapped?);

constant-namer("frame_mapped_Q__setter", frame-mapped?-setter);

constant-namer("frame_menu_bar", frame-menu-bar);

constant-namer("frame_tool_bar", frame-tool-bar);

constant-namer("frame_status_bar", frame-status-bar);

constant-namer("frame_title", frame-title);

constant-namer("frame_title_setter", frame-title-setter);

constant-namer("generate_panes", generate-panes);

constant-namer("start_frame", start-frame);

constant-namer("find_frame", find-frame);

constant-namer("top_level_sheet", top-level-sheet);

constant-namer("destroy_frame", destroy-frame);

constant-namer("contain", contain);

constant-namer("make_container", make-container);

constant-namer("_L_frame_manager_G_", <frame-manager>);

constant-namer("find_frame_manager", find-frame-manager);

constant-namer("attach_frame", attach-frame);

constant-namer("frame_wrapper", frame-wrapper);

constant-namer("note_title_changed", note-title-changed);

constant-namer("draw_point_T_", draw-point*);

constant-namer("draw_line_T_", draw-line*);

constant-namer("draw_rectangle_T_", draw-rectangle*);

constant-namer("draw_polygon_T_", draw-polygon*);

constant-namer("draw_ellipse_T_", draw-ellipse*);

constant-namer("draw_text_T_", draw-text*);

constant-namer("clear_box", clear-box);

constant-namer("clear_box_T_", clear-box*);

constant-namer("update_drawing_state", update-drawing-state);

constant-namer("invalidate_drawing_state", invalidate-drawing-state);

constant-namer("_L_color_G_", <color>);

constant-namer("make_rgb_color", make-rgb-color);

constant-namer("color_rgb", color-rgb);

variable-namer("_D_black", $black);

variable-namer("_D_red", $red);

variable-namer("_D_green", $green);

variable-namer("_D_blue", $blue);

variable-namer("_D_cyan", $cyan);

variable-namer("_D_magenta", $magenta);

variable-namer("_D_yellow", $yellow);

variable-namer("_D_white", $white);

constant-namer("_L_pen_G_", <pen>);

constant-namer("pen_width", pen-width);

constant-namer("pen_dashed_Q_", pen-dashed?);

constant-namer("_L_brush_G_", <brush>);

constant-namer("brush_background", brush-background);

constant-namer("brush_foreground", brush-foreground);

constant-namer("_L_text_style_G_", <text-style>);

constant-namer("make_text_style", make-text-style);

constant-namer("text_style_components", text-style-components);

constant-namer("text_style_mapping", text-style-mapping);

constant-namer("_L_top_level_sheet_G_", <top-level-sheet>);

constant-namer("_L_simple_pane_G_", <simple-pane>);

constant-namer("_L_drawing_pane_G_", <drawing-pane>);

constant-namer("_L_gadget_G_", <gadget>);

constant-namer("gadget_id", gadget-id);

constant-namer("gadget_client", gadget-client);

constant-namer("gadget_label", gadget-label);

constant-namer("gadget_label_setter", gadget-label-setter);

constant-namer("gadget_enabled_Q_", gadget-enabled?);

constant-namer("gadget_enabled_Q__setter", gadget-enabled?-setter);

constant-namer("gadget_selection_mode", gadget-selection-mode);

constant-namer("gadget_selection_mode_setter", gadget-selection-mode-setter);

constant-namer("gadget_value", gadget-value);

constant-namer("gadget_value_setter", gadget-value-setter);

constant-namer("_L_label_G_", <label>);

constant-namer("_L_push_button_G_", <push-button>);

constant-namer("_L_radio_button_G_", <radio-button>);

constant-namer("_L_check_button_G_", <check-button>);

constant-namer("_L_menu_bar_G_", <menu-bar>);

constant-namer("_L_menu_G_", <menu>);

constant-namer("_L_menu_item_G_", <menu-item>);

constant-namer("_L_menu_button_G_", <menu-button>);

constant-namer("_L_menu_separator_G_", <menu-separator>);

constant-namer("_L_event_G_", <event>);

constant-namer("_L_key_press_event_G_", <key-press-event>);

constant-namer("_L_key_release_event_G_", <key-release-event>);

constant-namer("_L_button_press_event_G_", <button-press-event>);

constant-namer("_L_button_release_event_G_", <button-release-event>);

constant-namer("_L_double_click_event_G_", <double-click-event>);

constant-namer("_L_pointer_motion_event_G_", <pointer-motion-event>);

constant-namer("_L_pointer_drag_event_G_", <pointer-drag-event>);

constant-namer("_L_window_configuration_event_G_", <window-configuration-event>);

constant-namer("_L_window_repaint_event_G_", <window-repaint-event>);

constant-namer("event_sheet", event-sheet);

constant-namer("event_modifier_state", event-modifier-state);

constant-namer("event_key_name", event-key-name);

constant-namer("event_character", event-character);

constant-namer("event_x", event-x);

constant-namer("event_y", event-y);

constant-namer("event_button", event-button);

constant-namer("event_region", event-region);

variable-namer("_D_left_button", $left-button);

variable-namer("_D_middle_button", $middle-button);

variable-namer("_D_right_button", $right-button);

variable-namer("_D_shift_key", $shift-key);

variable-namer("_D_control_key", $control-key);

variable-namer("_D_meta_key", $meta-key);

constant-namer("handle_event", handle-event);

constant-namer("handle_repaint", handle-repaint);

constant-namer("repaint_sheet", repaint-sheet);

constant-namer("port_event_loop", port-event-loop);

constant-namer("process_messages", process-messages);

constant-namer("process_next_message", process-next-message);

constant-namer("_L_mirror_G_", <mirror>);

constant-namer("_L_window_mirror_G_", <window-mirror>);

constant-namer("_L_menu_mirror_G_", <menu-mirror>);

constant-namer("_L_top_level_mirror_G_", <top-level-mirror>);

constant-namer("make_mirror", make-mirror);

constant-namer("make_win32_menu", make-win32-menu);

constant-namer("destroy_mirror", destroy-mirror);

constant-namer("_P_window_handle", %window-handle);

constant-namer("_P_dc", %dc);

constant-namer("_P_dc_setter", %dc-setter);

constant-namer("_L_sheet_G_", <sheet>);

constant-namer("make_pane", make-pane);

constant-namer("sheet_region", sheet-region);

constant-namer("sheet_transform", sheet-transform);

constant-namer("sheet_parent", sheet-parent);

constant-namer("sheet_parent_setter", sheet-parent-setter);

constant-namer("sheet_children", sheet-children);

constant-namer("sheet_children_setter", sheet-children-setter);

constant-namer("sheet_medium", sheet-medium);

constant-namer("sheet_mirror", sheet-mirror);

constant-namer("sheet_direct_mirror", sheet-direct-mirror);

constant-namer("sheet_frame", sheet-frame);

constant-namer("add_child", add-child);

constant-namer("remove_child", remove-child);

constant-namer("sheet_mapped_Q_", sheet-mapped?);

constant-namer("sheet_mapped_Q__setter", sheet-mapped?-setter);

constant-namer("destroy_sheet", destroy-sheet);

constant-namer("_L_medium_G_", <medium>);

constant-namer("medium_sheet", medium-sheet);

constant-namer("medium_drawable", medium-drawable);

constant-namer("medium_pen", medium-pen);

constant-namer("medium_brush", medium-brush);

constant-namer("medium_text_style", medium-text-style);

constant-namer("do_with_drawing_options", do-with-drawing-options);

constant-namer("beep", beep);

constant-namer("destroy_medium", destroy-medium);

constant-namer("_L_port_G_", <port>);

constant-namer("_P_port", %port);

constant-namer("find_port", find-port);

constant-namer("_L_region_G_", <region>);

constant-namer("_L_point_G_", <point>);

constant-namer("make_point", make-point);

constant-namer("point_x", point-x);

constant-namer("point_y", point-y);

constant-namer("point_position", point-position);

constant-namer("_L_bounding_box_G_", <bounding-box>);

constant-namer("make_bounding_box", make-bounding-box);

constant-namer("box_left", box-left);

constant-namer("box_top", box-top);

constant-namer("box_right", box-right);

constant-namer("box_bottom", box-bottom);

constant-namer("box_edges", box-edges);

constant-namer("box_width", box-width);

constant-namer("box_height", box-height);

constant-namer("box_size", box-size);

constant-namer("_L_transform_G_", <transform>);

constant-namer("make_translation_transform", make-translation-transform);

constant-namer("compose_transforms", compose-transforms);

constant-namer("invert_transform", invert-transform);

constant-namer("transform_position", transform-position);

constant-namer("untransform_position", untransform-position);

constant-namer("transform_box", transform-box);

constant-namer("untransform_box", untransform-box);

constant-namer("transform_region", transform-region);
