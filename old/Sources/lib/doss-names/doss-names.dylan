module: doss-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("doss");
module-namer("doss_internals");

constant-namer("_L_doss_io_manager_G_", <doss-io-manager>);

constant-namer("_L_doss_dumper_G_", <doss-dumper>);

constant-namer("_L_doss_loader_G_", <doss-loader>);

constant-namer("_L_doss_policy_G_", <doss-policy>);

constant-namer("_L_basic_doss_policy_G_", <basic-doss-policy>);

constant-namer("_L_loaded_object_proxy_G_", <loaded-object-proxy>);

constant-namer("_L_end_of_doss_stream_G_", <end-of-doss-stream>);

constant-namer("fetch_object", fetch-object);

constant-namer("fetch_next_object", fetch-next-object);

constant-namer("store_object", store-object);

constant-namer("put_reference", put-reference);

constant-namer("put_variable", put-variable);

constant-namer("put_class_description", put-class-description);

constant-namer("put_apply", put-apply);

constant-namer("put_object", put-object);

constant-namer("put_header", put-header);

constant-namer("put_footer", put-footer);

constant-namer("policy", policy);

constant-namer("policy_setter", policy-setter);

constant-namer("stream", stream);

constant-namer("stream_setter", stream-setter);

constant-namer("post_load_cleanup", post-load-cleanup);

constant-namer("doss_dumpable_slots", doss-dumpable-slots);

constant-namer("has_repeated_slots_Q_", has-repeated-slots?);

constant-namer("number_of_repeated_slots", number-of-repeated-slots);

constant-namer("doss_slot_value", doss-slot-value);

constant-namer("doss_repeated_slot_element", doss-repeated-slot-element);

constant-namer("unbound_proxy", unbound-proxy);

constant-namer("put_specially", put-specially);

constant-namer("locate_variable_via_policy", locate-variable-via-policy);

constant-namer("fixups", fixups);

constant-namer("loaded_object", loaded-object);

variable-namer("_T_debug_stream_T_", *debug-stream*);

variable-namer("_T_debug_print_T_", *debug-print*);
