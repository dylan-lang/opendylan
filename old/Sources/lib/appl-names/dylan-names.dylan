module: dylan-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

library-namer("dylan");
module-namer("internal");

/// CANONICALS - ADDED THESE BY HAND -- Nosa

constant-namer("_P_false", #f);

constant-namer("_P_true", #t);

constant-namer("_P_unbound", %unbound);

constant-namer("_P_empty_list", #());

constant-namer("_P_bs_empty", "");

constant-namer("_P_sv_empty", #[]);


/// MACHINE GENERATED

constant-namer("pointer_id_Q_", pointer-id?);

constant-namer("_P_unsupplied_Q_", %unsupplied?);

constant-namer("ignorable", ignorable);

variable-namer("_D_", $);

variable-namer("_D__D_", $$);

variable-namer("_D__D__D_", $$$);

variable-namer("last_values", last-values);

constant-namer("instance_storage_size", instance-storage-size);

variable-namer("_T_current_handlers_T_", *current-handlers*);

constant-namer("values", values);

constant-namer("apply", apply);

constant-namer("_L_object_G_", <object>);

constant-namer("make", make);

constant-namer("initialize", initialize);

constant-namer("slot_initialized_Q_", slot-initialized?);

constant-namer("_EQ__EQ_", \==);

constant-namer("_SG__EQ__EQ_", \~==);

constant-namer("_EQ_", \=);

constant-namer("_SG__EQ_", \~=);

constant-namer("object_hash", object-hash);

constant-namer("_L_", \<);

constant-namer("_G_", \>);

constant-namer("_L__EQ_", \<=);

constant-namer("_G__EQ_", \>=);

constant-namer("_L_function_G_", <function>);

constant-namer("_L_method_G_", <method>);

constant-namer("_L_generic_function_G_", <generic-function>);

constant-namer("add_method", add-method);

constant-namer("generic_function_methods", generic-function-methods);

constant-namer("function_arguments", function-arguments);

constant-namer("applicable_method_Q_", applicable-method?);

constant-namer("sorted_applicable_methods", sorted-applicable-methods);

constant-namer("find_method", find-method);

constant-namer("_L_type_G_", <type>);

constant-namer("instance_Q_", instance?);

constant-namer("subtype_Q_", subtype?);

constant-namer("_L_class_G_", <class>);

constant-namer("as", as);

constant-namer("shallow_copy", shallow-copy);

constant-namer("class_for_copy", class-for-copy);

constant-namer("object_class", object-class);

constant-namer("all_superclasses", all-superclasses);

constant-namer("direct_superclasses", direct-superclasses);

constant-namer("direct_subclasses", direct-subclasses);

constant-namer("_L_singleton_G_", <singleton>);

constant-namer("singleton", singleton);

constant-namer("limited", limited);

constant-namer("initial_state", initial-state);

constant-namer("next_state", next-state);

constant-namer("current_element", current-element);

constant-namer("copy_state", copy-state);

constant-namer("final_state", final-state);

constant-namer("previous_state", previous-state);

constant-namer("forward_iteration_protocol", forward-iteration-protocol);

constant-namer("backward_iteration_protocol", backward-iteration-protocol);

constant-namer("key_sequence", key-sequence);

constant-namer("current_key", current-key);

constant-namer("element", element);

constant-namer("element_setter", element-setter);

constant-namer("_L_collection_G_", <collection>);

constant-namer("_L_mutable_collection_G_", <mutable-collection>);

constant-namer("size", size);

constant-namer("empty_Q_", empty?);

constant-namer("do", do);

constant-namer("map", map);

constant-namer("map_as", map-as);

constant-namer("map_into", map-into);

constant-namer("any_Q_", any?);

constant-namer("every_Q_", every?);

constant-namer("reduce", reduce);

constant-namer("reduce1", reduce1);

constant-namer("member_Q_", member?);

constant-namer("find_key", find-key);

constant-namer("replace_elements_E_", replace-elements!);

constant-namer("fill_E_", fill!);

constant-namer("_L_sequence_G_", <sequence>);

constant-namer("_L_mutable_sequence_G_", <mutable-sequence>);

constant-namer("add", add);

constant-namer("add_E_", add!);

constant-namer("add_new", add-new);

constant-namer("add_new_E_", add-new!);

constant-namer("remove", remove);

constant-namer("remove_E_", remove!);

constant-namer("choose", choose);

constant-namer("choose_by", choose-by);

constant-namer("intersection", intersection);

constant-namer("union", union);

constant-namer("remove_duplicates", remove-duplicates);

constant-namer("remove_duplicates_E_", remove-duplicates!);

constant-namer("copy_sequence", copy-sequence);

constant-namer("concatenate_as", concatenate-as);

constant-namer("concatenate", concatenate);

constant-namer("replace_subsequence_E_", replace-subsequence!);

constant-namer("reverse", reverse);

constant-namer("reverse_E_", reverse!);

constant-namer("sort", sort);

constant-namer("sort_E_", sort!);

constant-namer("first", first);

constant-namer("second", second);

constant-namer("third", third);

constant-namer("last", last);

constant-namer("first_setter", first-setter);

constant-namer("second_setter", second-setter);

constant-namer("third_setter", third-setter);

constant-namer("last_setter", last-setter);

constant-namer("subsequence_position", subsequence-position);

constant-namer("_L_explicit_key_collection_G_", <explicit-key-collection>);

constant-namer("_L_mutable_explicit_key_collection_G_", <mutable-explicit-key-collection>);

constant-namer("_L_array_G_", <array>);

constant-namer("aref", aref);

constant-namer("aref_setter", aref-setter);

constant-namer("dimension", dimension);

constant-namer("dimensions", dimensions);

constant-namer("rank", rank);

constant-namer("row_major_index", row-major-index);

constant-namer("_L_deque_G_", <deque>);

constant-namer("push", push);

constant-namer("pop", pop);

constant-namer("push_last", push-last);

constant-namer("pop_last", pop-last);

constant-namer("_L_list_G_", <list>);

constant-namer("_L_empty_list_G_", <empty-list>);

constant-namer("_L_pair_G_", <pair>);

constant-namer("pair", pair);

constant-namer("head", head);

constant-namer("tail", tail);

constant-namer("head_setter", head-setter);

constant-namer("tail_setter", tail-setter);

constant-namer("list", list);

constant-namer("_L_range_G_", <range>);

constant-namer("range", range);

constant-namer("_L_string_G_", <string>);

constant-namer("_L_byte_string_G_", <byte-string>);

constant-namer("_L_unicode_string_G_", <unicode-string>);

constant-namer("as_lowercase", as-lowercase);

constant-namer("as_lowercase_E_", as-lowercase!);

constant-namer("as_uppercase", as-uppercase);

constant-namer("as_uppercase_E_", as-uppercase!);

constant-namer("_L_table_G_", <table>);

constant-namer("_L_object_table_G_", <object-table>);

constant-namer("remove_key_E_", remove-key!);

constant-namer("table_protocol", table-protocol);

constant-namer("merge_hash_codes", merge-hash-codes);

constant-namer("_D_permanent_hash_state", $permanent-hash-state);

constant-namer("key_test", key-test);

constant-namer("_L_vector_G_", <vector>);

constant-namer("_L_stretchy_vector_G_", <stretchy-vector>);

constant-namer("_L_simple_object_vector_G_", <simple-object-vector>);

constant-namer("size_setter", size-setter);

constant-namer("vector", vector);

constant-namer("_L_condition_G_", <condition>);

constant-namer("_L_simple_condition_G_", <simple-condition>);

constant-namer("_L_serious_condition_G_", <serious-condition>);

constant-namer("_L_error_G_", <error>);

constant-namer("_L_simple_error_G_", <simple-error>);

constant-namer("_L_type_error_G_", <type-error>);

constant-namer("_L_warning_G_", <warning>);

constant-namer("_L_simple_warning_G_", <simple-warning>);

constant-namer("_L_restart_G_", <restart>);

constant-namer("_L_simple_restart_G_", <simple-restart>);

constant-namer("_L_abort_G_", <abort>);

constant-namer("signal", signal);

constant-namer("error", error);

constant-namer("cerror", cerror);

constant-namer("break", break);

constant-namer("check_type", check-type);

constant-namer("abort", abort);

constant-namer("default_handler", default-handler);

constant-namer("restart_query", restart-query);

constant-namer("return_query", return-query);

constant-namer("do_handlers", do-handlers);

constant-namer("return_allowed_Q_", return-allowed?);

constant-namer("return_description", return-description);

constant-namer("_L_number_G_", <number>);

constant-namer("_L_complex_G_", <complex>);

constant-namer("_L_real_G_", <real>);

constant-namer("_L_rational_G_", <rational>);

constant-namer("_L_float_G_", <float>);

constant-namer("_L_single_float_G_", <single-float>);

constant-namer("_L_double_float_G_", <double-float>);

constant-namer("_L_extended_float_G_", <extended-float>);

constant-namer("_L_integer_G_", <integer>);

constant-namer("odd_Q_", odd?);

constant-namer("even_Q_", even?);

constant-namer("zero_Q_", zero?);

constant-namer("positive_Q_", positive?);

constant-namer("negative_Q_", negative?);

constant-namer("integral_Q_", integral?);

constant-namer("_PL_", \+);

constant-namer("_T_", \*);

constant-namer("_", \-);

constant-namer("_S_", \/);

constant-namer("negative", negative);

constant-namer("floor", floor);

constant-namer("ceiling", ceiling);

constant-namer("round", round);

constant-namer("truncate", truncate);

constant-namer("floor_S_", floor/);

constant-namer("ceiling_S_", ceiling/);

constant-namer("round_S_", round/);

constant-namer("truncate_S_", truncate/);

constant-namer("modulo", modulo);

constant-namer("remainder", remainder);

constant-namer("_CR_", \^);

constant-namer("min", min);

constant-namer("max", max);

constant-namer("abs", abs);

constant-namer("gcd", gcd);

constant-namer("lcm", lcm);

constant-namer("logior", logior);

constant-namer("logxor", logxor);

constant-namer("logand", logand);

constant-namer("lognot", lognot);

constant-namer("logbit_Q_", logbit?);

constant-namer("ash", ash);

constant-namer("_L_symbol_G_", <symbol>);

constant-namer("_L_keyword_G_", <keyword>);

constant-namer("_L_character_G_", <character>);

constant-namer("compose", compose);

constant-namer("complement", complement);

constant-namer("disjoin", disjoin);

constant-namer("conjoin", conjoin);

constant-namer("curry", curry);

constant-namer("rcurry", rcurry);

constant-namer("always", always);

constant-namer("identity", identity);

constant-namer("id_Q_", id?);

constant-namer("_L_boolean_G_", <boolean>);

constant-namer("list_T_", list*);

constant-namer("make_handler", make-handler);

constant-namer("power_of_two_ceiling", power-of-two-ceiling);

constant-namer("_L_small_integer_G_", <small-integer>);

constant-namer("_L_big_integer_G_", <big-integer>);

constant-namer("_L_machine_integer_G_", <machine-integer>);

constant-namer("_L_unsigned_machine_integer_G_", <unsigned-machine-integer>);

constant-namer("_L_subclass_G_", <subclass>);

constant-namer("subclass", subclass);

constant-namer("_L_subtype_G_", <subtype>);

constant-namer("subtype", subtype);

constant-namer("_L_union_G_", <union>);

constant-namer("_L_limited_integer_G_", <limited-integer>);

constant-namer("preceding_specializer_Q_", preceding-specializer?);

constant-namer("has_instances_Q_", has-instances?);

constant-namer("_L_lambda_G_", <lambda>);

constant-namer("_L_complex_method_G_", <complex-method>);

constant-namer("_L_slot_method_G_", <slot-method>);

constant-namer("_L_open_class_G_", <open-class>);

constant-namer("_L_sealed_class_G_", <sealed-class>);

constant-namer("_L_value_class_G_", <value-class>);

constant-namer("_L_function_class_G_", <function-class>);

constant-namer("_L_instance_slot_descriptor_G_", <instance-slot-descriptor>);

constant-namer("_L_constant_slot_descriptor_G_", <constant-slot-descriptor>);

constant-namer("_L_class_slot_descriptor_G_", <class-slot-descriptor>);

constant-namer("_L_each_subclass_slot_descriptor_G_", <each-subclass-slot-descriptor>);

constant-namer("_L_virtual_slot_descriptor_G_", <virtual-slot-descriptor>);

constant-namer("_L_repeated_slot_descriptor_G_", <repeated-slot-descriptor>);

constant-namer("slot_descriptors", slot-descriptors);

constant-namer("_L_slot_descriptor_G_", <slot-descriptor>);

constant-namer("slot_getter", slot-getter);

constant-namer("slot_setter", slot-setter);

constant-namer("init_keyword", init-keyword);

constant-namer("required_init_keyword_Q_", required-init-keyword?);

constant-namer("init_value_Q_", init-value?);

constant-namer("init_value", init-value);

constant-namer("init_function", init-function);

constant-namer("slot_type", slot-type);

constant-namer("slot_owner", slot-owner);

constant-namer("object", object);

constant-namer("specializers", specializers);

constant-namer("slot_allocation", slot-allocation);

constant-namer("slot_element", slot-element);

constant-namer("slot_element_setter", slot-element-setter);

constant-namer("byte_slot_element", byte-slot-element);

constant-namer("byte_slot_element_setter", byte-slot-element-setter);

constant-namer("class_slot_element", class-slot-element);

constant-namer("class_slot_element_setter", class-slot-element-setter);

constant-namer("slot_value", slot-value);

constant-namer("slot_value_setter", slot-value-setter);

constant-namer("class_slot_value", class-slot-value);

constant-namer("class_slot_value_setter", class-slot-value-setter);

constant-namer("specializers_setter", specializers-setter);

constant-namer("precedes_Q_", precedes?);

constant-namer("allocate_instance", allocate-instance);

constant-namer("shared_initialize", shared-initialize);

constant-namer("reinitialize", reinitialize);

constant-namer("subclass_Q_", subclass?);

constant-namer("first_index", first-index);

constant-namer("last_index", last-index);

constant-namer("data", data);

constant-namer("log", log);

constant-namer("condition_format_string", condition-format-string);

constant-namer("condition_format_arguments", condition-format-arguments);

constant-namer("_L_not_found_error_G_", <not-found-error>);

constant-namer("_L_key_not_found_error_G_", <key-not-found-error>);

constant-namer("_L_value_not_found_error_G_", <value-not-found-error>);

constant-namer("_L_invalid_index_error_G_", <invalid-index-error>);

constant-namer("_L_subscript_out_of_bounds_error_G_", <subscript-out-of-bounds-error>);

constant-namer("type_error_value", type-error-value);

constant-namer("type_error_type", type-error-type);

constant-namer("bits", bits);

constant-namer("_L_hashed_collection_G_", <hashed-collection>);

constant-namer("tally", tally);

constant-namer("tally_setter", tally-setter);

constant-namer("mask", mask);

constant-namer("mask_setter", mask-setter);

constant-namer("elements", elements);

constant-namer("elements_setter", elements-setter);

constant-namer("void_element", void-element);

constant-namer("gc_state", gc-state);

constant-namer("gc_state_setter", gc-state-setter);

constant-namer("current_gc_state", current-gc-state);

constant-namer("clear_E_", clear!);

constant-namer("rehash_E_", rehash!);

constant-namer("rehash_using_E_", rehash-using!);

constant-namer("grow_E_", grow!);

constant-namer("index_for_key", index-for-key);

constant-namer("merge_hash_ids", merge-hash-ids);

constant-namer("merge_hash_states", merge-hash-states);

constant-namer("includes_key_Q_", includes-key?);

constant-namer("_P_method_apply", %method-apply);

constant-namer("_P_small_integer_data", %small-integer-data);

constant-namer("_P_small_integer", %small-integer);

constant-namer("_P_big_integer", %big-integer);

constant-namer("_P_big_integer_data", %big-integer-data);

constant-namer("_P_machine_integer", %machine-integer);

constant-namer("_P_machine_integer_data", %machine-integer-data);

constant-namer("_P_unsigned_machine_integer", %unsigned-machine-integer);

constant-namer("_P_unsigned_machine_integer_data", %unsigned-machine-integer-data);

constant-namer("_P_integer", %integer);

constant-namer("_P_integer_data", %integer-data);

constant-namer("_P_byte_character", %byte-character);

constant-namer("_P_byte_character_data", %byte-character-data);

constant-namer("_P_single_float", %single-float);

constant-namer("_P_single_float_data", %single-float-data);

constant-namer("_P_double_float", %double-float);

constant-namer("_P_double_float_data", %double-float-data);

constant-namer("_P_vector_data", %vector-data);

constant-namer("_P_byte_string", %byte-string);

constant-namer("_P_byte_string_data", %byte-string-data);

// constant-namer("_P_interrupt_handler_name", %interrupt-handler-name);

constant-namer("_P_register_symbols", %register-symbols);

constant-namer("_L_unbound_G_", <unbound>);

constant-namer("_L_multidimensional_array_G_", <multidimensional-array>);

constant-namer("_L_island_queue_G_", <island-queue>);

variable-namer("_D_not_found", $not-found);

constant-namer("debug_name", debug-name);

constant-namer("debug_name_setter", debug-name-setter);

constant-namer("_L_byte_character_G_", <byte-character>);

constant-namer("alpha_Q_", alpha?);

constant-namer("lowercase_Q_", lowercase?);

constant-namer("uppercase_Q_", uppercase?);
