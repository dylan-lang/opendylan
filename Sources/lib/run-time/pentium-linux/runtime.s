	.global KPunboundVKi
	.global KPfalseVKi
	.global KPtrueVKi
	.global KPempty_listVKi
	.global KPempty_vectorVKi
	.global Ddirect_object_mm_wrappersVKi
	.global Ddirect_object_classesVKi
	.global KLtraceable_value_cellGVKiW
	.global KLuntraceable_value_cellGVKiW
	.global KLuntraceable_double_value_cellGVKiW
	.global KLmachine_wordGVKeW
	.global Tdispatch_profiling_enabledQTVKe
	.global Tclass_profiling_enabledQTVKe
	.global KLobjectGVKd
	.global KLobjectGVKdW
	.global KLsingle_floatGVKdW
	.global KLdouble_floatGVKdW
	.global KLsimple_object_vectorGVKdW
	.global KLbyte_stringGVKdW
	.global KLmethodGVKdW
	.global KerrorVKd
	.global Kargument_count_errorVKiI
	.global Kodd_keyword_arguments_errorVKiI
	.global Kunknown_keyword_argument_errorVKiI
	.global Ktype_check_errorVKiI
	.global Kstack_overflow_errorVKiI
	.global Kgrounded_instanceQVKeI
	.global MMCollectCount
	.global dylan__malloc__misc
	.global dylan__malloc__ambig
	.global dylan__malloc__exact
	.global dylan__free__root
	.global mps__malloc
	.global mps__free
	.global primitive_mps_park
	.global primitive_mps_release
	.global Prunning_under_dylan_debuggerQ
	.global primitive_alloc
	.global primitive_alloc_s1
	.global primitive_alloc_s2
	.global primitive_alloc_s
	.global primitive_alloc_r
	.global primitive_alloc_rf
	.global primitive_alloc_rt
	.global primitive_alloc_s_r
	.global primitive_alloc_s_rb
	.global primitive_alloc_leaf
	.global primitive_alloc_leaf_s_r
	.global primitive_alloc_leaf_s1
	.global primitive_alloc_leaf_s2
	.global primitive_alloc_leaf_s
	.global primitive_alloc_leaf_r
	.global primitive_alloc_leaf_s_rbf
	.global primitive_alloc_leaf_s_rbfz
	.global primitive_alloc_leaf_rbfz
	.global primitive_alloc_leaf_s_rb
	.global primitive_alloc_exact_awl_s_r
	.global primitive_alloc_exact_awl_rf
	.global primitive_alloc_weak_awl_s_r
	.global primitive_alloc_weak_awl_rf
	.global primitive_alloc_wrapper_s_r
	.global primitive_copy
	.global primitive_copy_r
	.global primitive_wrap_machine_word
	.global primitive_wrap_c_pointer
	.global primitive_exit_application
	.global Prunning_dylan_spy_functionQ
	.global spy_invoke_dylan_under_coded_restartVKi
	.global Kmake_simple_lockYthreads_primitivesVdylan
	.global default_tlv_vector
	.global dylan_false
	.global Ksignal_low_memoryVKe
	.global dylan_signal_low_memory
	.global Kkeyboard_break_handlerVKe
	.global dylan_keyboard_break_handler
	.global dylan_keyboard_interruptQ
	.global KPslotacc_single_q_instance_getterVKiI
	.global KPslotacc_single_q_instance_setterVKiI
	.global KPslotacc_single_q_class_getterVKiI
	.global KPslotacc_single_q_class_setterVKiI
	.global KPslotacc_repeated_instance_getterVKiI
	.global KPslotacc_repeated_instance_setterVKiI
	.global Kunbound_instance_slotVKeI
	.global Kunbound_repeated_slotVKeI
	.global Kinvalid_keyword_trapVKeI
	.global Kodd_number_of_keyword_args_trapVKeI
	.global Krepeated_slot_getter_index_out_of_range_trapVKeI
	.global Krepeated_slot_setter_index_out_of_range_trapVKeI
	.global Dinapplicable_engine_nodeVKg
	.global Dabsent_engine_nodeVKg
	.global KLsymbolGVKdW
	.global Poblist
	.global PoblistUsize
	.global PoblistUcursor
	.global Kmachine_word_overflowVKmI
	.global Kinteger_divide_by_0VKeI
	.global Kfloat_divide_by_0VKeI
	.global Kfloat_overflowVKeI
	.global Kfloat_underflowVKeI
	.global c_primitive_start_timer
	.global c_primitive_stop_timer
	.global exit
	.global system

	.section .dydat$m, "aw"
	.align 4


_dylan_object_file_data_start:

default_tlv_vector:
	.long 0
	.align 4
	.type default_tlv_vector,@object
	.size default_tlv_vector,. - default_tlv_vector

Poblist:
	.long 0
	.align 4
	.type Poblist,@object
	.size Poblist,. - Poblist


	.section .dyutr$r, "aw"
	.align 4


_dylan_untraced_data_start:

Prunning_under_dylan_debuggerQ:
	.long 0
	.align 4
	.type Prunning_under_dylan_debuggerQ,@object
	.size Prunning_under_dylan_debuggerQ,. - Prunning_under_dylan_debuggerQ

Klow_zeros_table:
	.byte 4
	.byte 0
	.byte 1
	.byte 0
	.byte 2
	.byte 0
	.byte 1
	.byte 0
	.byte 3
	.byte 0
	.byte 1
	.byte 0
	.byte 2
	.byte 0
	.byte 1
	.byte 0
	.align 4
	.type Klow_zeros_table,@object
	.size Klow_zeros_table,. - Klow_zeros_table

Khigh_zeros_table:
	.byte 4
	.byte 3
	.byte 2
	.byte 2
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.align 4
	.type Khigh_zeros_table,@object
	.size Khigh_zeros_table,. - Khigh_zeros_table

Prunning_dylan_spy_functionQ:
	.long 0
	.align 4
	.type Prunning_dylan_spy_functionQ,@object
	.size Prunning_dylan_spy_functionQ,. - Prunning_dylan_spy_functionQ

dylan_false:
	.long KPfalseVKi
	.align 4
	.type dylan_false,@object
	.size dylan_false,. - dylan_false

dylan_signal_low_memory:
	.long Ksignal_low_memoryVKe
	.align 4
	.type dylan_signal_low_memory,@object
	.size dylan_signal_low_memory,. - dylan_signal_low_memory

dylan_keyboard_break_handler:
	.long Kkeyboard_break_handlerVKe
	.align 4
	.type dylan_keyboard_break_handler,@object
	.size dylan_keyboard_break_handler,. - dylan_keyboard_break_handler

dylan_keyboard_interruptQ:
	.long 0
	.align 4
	.type dylan_keyboard_interruptQ,@object
	.size dylan_keyboard_interruptQ,. - dylan_keyboard_interruptQ

Kinitialize_engine_node_table:
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_1
	.long initialize_engine_node_table_entries_2
	.long initialize_engine_node_table_entries_3
	.long initialize_engine_node_table_entries_4
	.long initialize_engine_node_table_entries_5
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_default
	.long initialize_engine_node_table_entries_13
	.long initialize_engine_node_table_entries_14
	.long initialize_engine_node_table_entries_1
	.long initialize_engine_node_table_entries_16
	.long initialize_engine_node_table_entries_17
	.long initialize_engine_node_table_entries_18
	.long initialize_engine_node_table_entries_19
	.long initialize_engine_node_table_entries_20
	.long initialize_engine_node_table_entries_21
	.long initialize_engine_node_table_entries_22
	.long initialize_engine_node_table_entries_23
	.long initialize_engine_node_table_entries_20
	.long initialize_engine_node_table_entries_21
	.long initialize_engine_node_table_entries_20
	.long initialize_engine_node_table_entries_21
	.long initialize_engine_node_table_entries_21
	.long initialize_engine_node_table_entries_29
	.long initialize_engine_node_table_entries_21
	.long initialize_engine_node_table_entries_29
	.align 4
	.type Kinitialize_engine_node_table,@object
	.size Kinitialize_engine_node_table,. - Kinitialize_engine_node_table

PoblistUsize:
	.long 8
	.align 4
	.type PoblistUsize,@object
	.size PoblistUsize,. - PoblistUsize

PoblistUcursor:
	.long 8
	.align 4
	.type PoblistUcursor,@object
	.size PoblistUcursor,. - PoblistUcursor

Kfloating_point_classes:
	.long 0
	.long Kunsupported_float
	.long 256
	.long Knan_float
	.long 1024
	.long Knormal_float
	.long 1280
	.long Kinfinity_float
	.long 16384
	.long Kzero_float
	.long 16640
	.long Kempty_float
	.long 17408
	.long Kdenormal_float
	.align 4
	.type Kfloating_point_classes,@object
	.size Kfloating_point_classes,. - Kfloating_point_classes


	.section .dyutr$m, "aw"
	.align 4


_dylan_untraced_objs_start:

Kprimitive_error_string:
	.long KLbyte_stringGVKdW
	.long 65
	.ascii "Primitive error."
	.byte 0
	.align 4
	.type Kprimitive_error_string,@object
	.size Kprimitive_error_string,. - Kprimitive_error_string

Kunsupported_float:
	.long KLbyte_stringGVKdW
	.long 45
	.ascii "unsupported"
	.byte 0
	.align 4
	.type Kunsupported_float,@object
	.size Kunsupported_float,. - Kunsupported_float

Knan_float:
	.long KLbyte_stringGVKdW
	.long 13
	.ascii "nan"
	.byte 0
	.align 4
	.type Knan_float,@object
	.size Knan_float,. - Knan_float

Knormal_float:
	.long KLbyte_stringGVKdW
	.long 25
	.ascii "normal"
	.byte 0
	.align 4
	.type Knormal_float,@object
	.size Knormal_float,. - Knormal_float

Kinfinity_float:
	.long KLbyte_stringGVKdW
	.long 33
	.ascii "infinity"
	.byte 0
	.align 4
	.type Kinfinity_float,@object
	.size Kinfinity_float,. - Kinfinity_float

Kzero_float:
	.long KLbyte_stringGVKdW
	.long 17
	.ascii "zero"
	.byte 0
	.align 4
	.type Kzero_float,@object
	.size Kzero_float,. - Kzero_float

Kempty_float:
	.long KLbyte_stringGVKdW
	.long 21
	.ascii "empty"
	.byte 0
	.align 4
	.type Kempty_float,@object
	.size Kempty_float,. - Kempty_float

Kdenormal_float:
	.long KLbyte_stringGVKdW
	.long 33
	.ascii "denormal"
	.byte 0
	.align 4
	.type Kdenormal_float,@object
	.size Kdenormal_float,. - Kdenormal_float

	.global primitive_remove_optionals
	.global Psegment_register_load_instruction_offset
	.global Psegment_register_store_instruction_offset

	.section .text
	.align 4


	.global primitive_break
	.type primitive_break,@function
primitive_break:
	.byte	204,195


	.global primitive_error
	.type primitive_error,@function
primitive_error:
	.byte	85,137,229,184
	.long	Kprimitive_error_string
	.byte	187
	.long	KerrorVKd
	.byte	185,1,0,0,0,255,83,4,201,195


	.global primitive_debug_message
	.type primitive_debug_message,@function
primitive_debug_message:
	.byte	131,61
	.long	Prunning_under_dylan_debuggerQ
	.byte	0,116,36,85,137,229,139,125,12,139,95,4,193,251,2,137
	.byte	218,193,226,2,43,226,141,87,8,137,217,137,214,137,231,252
	.byte	243,165,83,80,204,88,201,194,8,0


	.global primitive_invoke_debugger
	.type primitive_invoke_debugger,@function
primitive_invoke_debugger:
	.byte	85,137,229,139,125,8,131,61
	.long	Prunning_under_dylan_debuggerQ
	.byte	0,117,0,139,95,4,193,251,2,137,218,193,226,2,43,226
	.byte	141,87,8,137,217,137,214,137,231,252,243,165,83,80,204,88
	.byte	201,194,4,0


	.global class_allocation_break
	.type class_allocation_break,@function
class_allocation_break:
	.byte	129,61
	.long	Tclass_profiling_enabledQTVKe
	.long	KPtrueVKi
	.byte	116,23,85,137,229,139,77,12,139,85,20,131,61
	.long	Prunning_under_dylan_debuggerQ
	.byte	0,117,0,82,81,204,88,201,195


	.global primitive_inside_debuggerQ
	.type primitive_inside_debuggerQ,@function
primitive_inside_debuggerQ:
	.byte	131,61
	.long	Prunning_under_dylan_debuggerQ
	.byte	0,116,6,184
	.long	KPtrueVKi
	.byte	195,184
	.long	KPfalseVKi
	.byte	235,248


	.global primitive_gc_state
	.type primitive_gc_state,@function
primitive_gc_state:
	.byte	85,137,229,252,232
	.long	MMCollectCount - 4 - .
	.byte	193,224,2,131,192,1,201,195


	.global primitive_allocate
	.type primitive_allocate,@function
primitive_allocate:
	.byte	85,137,229,232
	.long	primitive_error - 4 - .
	.byte	201,195


	.global primitive_allocate_filled
	.type primitive_allocate_filled,@function
primitive_allocate_filled:
	.byte	85,137,229,139,77,12,139,85,16,139,93,20,139,125,24,193
	.byte	224,2,87,83,82,81,255,117,8,80,252,232
	.long	primitive_alloc_s_r - 4 - .
	.byte	131,196,24,201,194,20,0


	.global primitive_allocate_filled_in_leaf_pool
	.type primitive_allocate_filled_in_leaf_pool,@function
primitive_allocate_filled_in_leaf_pool:
	.byte	85,137,229,139,77,12,139,85,16,139,93,20,139,125,24,193
	.byte	224,2,87,83,82,81,255,117,8,80,252,232
	.long	primitive_alloc_leaf_s_r - 4 - .
	.byte	131,196,24,201,194,20,0


	.global primitive_allocate_weak_in_awl_pool
	.type primitive_allocate_weak_in_awl_pool,@function
primitive_allocate_weak_in_awl_pool:
	.byte	85,137,229,139,77,16,139,85,20,139,93,24,139,125,28,193
	.byte	224,2,83,82,81,255,117,12,87,255,117,8,80,252,232
	.long	primitive_alloc_weak_awl_s_r - 4 - .
	.byte	131,196,28,201,194,24,0


	.global primitive_allocate_in_awl_pool
	.type primitive_allocate_in_awl_pool,@function
primitive_allocate_in_awl_pool:
	.byte	85,137,229,139,77,16,139,85,20,139,93,24,139,125,28,193
	.byte	224,2,83,82,81,255,117,12,87,255,117,8,80,252,232
	.long	primitive_alloc_exact_awl_s_r - 4 - .
	.byte	131,196,28,201,194,24,0


	.global primitive_allocate_wrapper
	.type primitive_allocate_wrapper,@function
primitive_allocate_wrapper:
	.byte	85,137,229,139,77,12,139,85,16,139,93,20,139,125,24,193
	.byte	224,2,87,83,82,81,255,117,8,80,252,232
	.long	primitive_alloc_wrapper_s_r - 4 - .
	.byte	131,196,24,201,194,20,0


	.global primitive_untraced_allocate
	.type primitive_untraced_allocate,@function
primitive_untraced_allocate:
	.byte	85,137,229,80,252,232
	.long	dylan__malloc__misc - 4 - .
	.byte	131,196,4,201,195


	.global primitive_byte_allocate
	.type primitive_byte_allocate,@function
primitive_byte_allocate:
	.byte	85,137,229,232
	.long	primitive_error - 4 - .
	.byte	201,195


	.global primitive_byte_allocate_filled
	.type primitive_byte_allocate_filled,@function
primitive_byte_allocate_filled:
	.byte	85,137,229,139,77,16,139,85,20,139,93,24,139,125,28,193
	.byte	224,2,3,69,8,131,192,3,131,224,252,87,83,82,81,255
	.byte	117,12,80,252,232
	.long	primitive_alloc_leaf_s_rb - 4 - .
	.byte	131,196,24,201,194,24,0


	.global primitive_byte_allocate_filled_terminated
	.type primitive_byte_allocate_filled_terminated,@function
primitive_byte_allocate_filled_terminated:
	.byte	85,137,229,139,77,12,139,125,20,139,85,24,139,93,28,193
	.byte	224,2,3,69,8,131,192,3,131,224,252,193,255,2,87,83
	.byte	82,81,80,252,232
	.long	primitive_alloc_leaf_rbfz - 4 - .
	.byte	131,196,20,201,194,24,0


	.global primitive_allocate_vector
	.type primitive_allocate_vector,@function
primitive_allocate_vector:
	.byte	85,137,229,137,199,193,231,2,131,199,8,104
	.long	KPunboundVKi
	.byte	106,1,80,104
	.long	KLsimple_object_vectorGVKdW
	.byte	87,252,232
	.long	primitive_alloc_rf - 4 - .
	.byte	131,196,20,201,195


	.global primitive_vector
	.type primitive_vector,@function
primitive_vector:
	.byte	141,124,36,4,131,248,0,117,17,191
	.long	KPempty_vectorVKi
	.byte	137,194,193,226,2,137,248,94,3,226,255,230,85,137,229,131
	.byte	236,4,137,69,252,137,251,139,125,252,193,231,2,131,199,8
	.byte	83,106,1,255,117,252,104
	.long	KLsimple_object_vectorGVKdW
	.byte	87,252,232
	.long	primitive_alloc_rt - 4 - .
	.byte	131,196,20,137,199,139,69,252,137,194,193,226,2,137,248,201
	.byte	235,195


	.global primitive_copy_vector
	.type primitive_copy_vector,@function
primitive_copy_vector:
	.byte	139,120,4,131,239,1,131,255,0,117,6,184
	.long	KPempty_vectorVKi
	.byte	195,85,137,229,131,199,8,80,87,252,232
	.long	primitive_copy - 4 - .
	.byte	131,196,8,201,235,235


	.global primitive_make_box
	.type primitive_make_box,@function
primitive_make_box:
	.byte	85,137,229,80,104
	.long	KLtraceable_value_cellGVKiW
	.byte	106,8,252,232
	.long	primitive_alloc_s1 - 4 - .
	.byte	131,196,12,201,195


	.global primitive_make_raw_box
	.type primitive_make_raw_box,@function
primitive_make_raw_box:
	.byte	85,137,229,137,199,104
	.long	KLuntraceable_value_cellGVKiW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,137,120,4,201,195


	.global primitive_make_single_float_box
	.type primitive_make_single_float_box,@function
primitive_make_single_float_box:
	.byte	85,137,229,131,236,4,217,93,252,104
	.long	KLuntraceable_value_cellGVKiW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,217,69,252,217,88,4,201,195


	.global primitive_make_double_float_box
	.type primitive_make_double_float_box,@function
primitive_make_double_float_box:
	.byte	85,137,229,131,236,8,221,93,248,104
	.long	KLuntraceable_double_value_cellGVKiW
	.byte	106,12,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,221,69,248,221,88,4,201,195


	.global primitive_make_closure
	.type primitive_make_closure,@function
primitive_make_closure:
	.byte	85,137,229,139,125,8,137,251,193,227,2,131,195,20,80,106
	.byte	4,87,83,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,201,194,4,0


	.global primitive_make_keyword_closure
	.type primitive_make_keyword_closure,@function
primitive_make_keyword_closure:
	.byte	85,137,229,139,125,8,137,251,193,227,2,131,195,28,80,106
	.byte	6,87,83,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,201,194,4,0


	.global primitive_make_closure_with_signature
	.type primitive_make_closure_with_signature,@function
primitive_make_closure_with_signature:
	.byte	85,137,229,139,125,12,137,251,193,227,2,131,195,20,80,106
	.byte	4,87,83,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,139,117,8,137,112,8,201,194,8,0


	.global primitive_make_keyword_closure_with_signature
	.type primitive_make_keyword_closure_with_signature,@function
primitive_make_keyword_closure_with_signature:
	.byte	85,137,229,139,125,12,137,251,193,227,2,131,195,28,80,106
	.byte	6,87,83,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,139,117,8,137,112,8,201,194,8,0


	.global primitive_initialize_closure
	.type primitive_initialize_closure,@function
primitive_initialize_closure:
	.byte	137,193,139,92,36,4,191,4,0,0,0,131,251,0,116,33
	.byte	85,137,229,131,236,4,137,250,141,121,20,137,217,141,117,12
	.byte	137,117,252,139,117,252,252,243,165,193,227,2,137,215,3,251
	.byte	201,94,3,231,255,230


	.global primitive_initialize_keyword_closure
	.type primitive_initialize_keyword_closure,@function
primitive_initialize_keyword_closure:
	.byte	137,193,139,92,36,4,191,4,0,0,0,131,251,0,116,33
	.byte	85,137,229,131,236,4,137,250,141,121,28,137,217,141,117,12
	.byte	137,117,252,139,117,252,252,243,165,193,227,2,137,215,3,251
	.byte	201,94,3,231,255,230


	.global primitive_make_closure_with_environment
	.type primitive_make_closure_with_environment,@function
primitive_make_closure_with_environment:
	.byte	85,137,229,139,125,8,193,231,2,131,199,20,80,106,4,255
	.byte	117,8,87,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,191,4,0,0,0,131,125,8,0,116,26,137,250
	.byte	141,120,20,139,77,8,141,93,12,137,222,252,243,165,139,93
	.byte	8,193,227,2,137,215,3,251,201,94,3,231,255,230


	.global primitive_make_keyword_closure_with_environment
	.type primitive_make_keyword_closure_with_environment,@function
primitive_make_keyword_closure_with_environment:
	.byte	85,137,229,139,125,8,193,231,2,131,199,28,80,106,6,255
	.byte	117,8,87,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,191,4,0,0,0,131,125,8,0,116,26,137,250
	.byte	141,120,28,139,77,8,141,93,12,137,222,252,243,165,139,93
	.byte	8,193,227,2,137,215,3,251,201,94,3,231,255,230


	.global primitive_make_closure_with_environment_signature
	.type primitive_make_closure_with_environment_signature,@function
primitive_make_closure_with_environment_signature:
	.byte	85,137,229,139,125,12,193,231,2,131,199,20,80,106,4,255
	.byte	117,12,87,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,191,8,0,0,0,131,125,12,0,116,26,137,250
	.byte	141,120,20,139,77,12,141,93,16,137,222,252,243,165,139,93
	.byte	12,193,227,2,137,215,3,251,139,117,8,137,112,8,201,94
	.byte	3,231,255,230


	.global primitive_make_keyword_closure_with_environment_signature
	.type primitive_make_keyword_closure_with_environment_signature,@function
primitive_make_keyword_closure_with_environment_signature:
	.byte	85,137,229,139,125,12,193,231,2,131,199,28,80,106,6,255
	.byte	117,12,87,252,232
	.long	primitive_copy_r - 4 - .
	.byte	131,196,16,191,8,0,0,0,131,125,12,0,116,26,137,250
	.byte	141,120,28,139,77,12,141,93,16,137,222,252,243,165,139,93
	.byte	12,193,227,2,137,215,3,251,139,117,8,137,112,8,201,94
	.byte	3,231,255,230


	.global primitive_make_method_with_signature
	.type primitive_make_method_with_signature,@function
primitive_make_method_with_signature:
	.byte	85,137,229,139,125,8,80,106,16,252,232
	.long	primitive_copy - 4 - .
	.byte	131,196,8,137,120,8,201,194,4,0


	.global primitive_make_keyword_method_with_signature
	.type primitive_make_keyword_method_with_signature,@function
primitive_make_keyword_method_with_signature:
	.byte	85,137,229,139,125,8,80,106,24,252,232
	.long	primitive_copy - 4 - .
	.byte	131,196,8,137,120,8,201,194,4,0


	.global primitive_strlen
	.type primitive_strlen,@function
primitive_strlen:
	.byte	137,199,139,24,10,219,116,36,247,195,0,255,0,0,116,25
	.byte	247,195,0,0,255,0,116,14,131,192,4,247,195,0,0,0
	.byte	255,117,223,131,232,3,131,192,1,131,192,1,43,199,195


	.global primitive_raw_as_string
	.type primitive_raw_as_string,@function
primitive_raw_as_string:
	.byte	85,137,229,131,236,8,137,69,248,232
	.long	primitive_strlen - 4 - .
	.byte	137,69,252,139,125,252,131,199,12,131,231,252,139,93,252,106
	.byte	1,83,104
	.long	KLbyte_stringGVKdW
	.byte	87,252,232
	.long	primitive_alloc_leaf_r - 4 - .
	.byte	131,196,16,141,120,8,131,69,252,1,139,77,252,139,117,248
	.byte	252,193,249,2,243,165,139,77,252,131,225,3,243,164,201,195


	.global primitive_random
	.type primitive_random,@function
primitive_random:
	.byte	85,137,229,232
	.long	primitive_error - 4 - .
	.byte	201,195


	.global primitive_arg_error
	.type primitive_arg_error,@function
primitive_arg_error:
	.byte	85,137,229,232
	.long	primitive_error - 4 - .
	.byte	201,195


	.global primitive_raw_as_single_float
	.type primitive_raw_as_single_float,@function
primitive_raw_as_single_float:
	.byte	85,137,229,131,236,4,217,93,252,104
	.long	KLsingle_floatGVKdW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,139,125,252,137,120,4,201,195


	.global primitive_raw_as_double_float
	.type primitive_raw_as_double_float,@function
primitive_raw_as_double_float:
	.byte	85,137,229,131,236,8,221,93,248,104
	.long	KLdouble_floatGVKdW
	.byte	106,12,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,139,93,248,139,125,252,137,88,4,137,120,8,201
	.byte	195


	.global primitive_wrap_machine_word
	.type primitive_wrap_machine_word,@function
primitive_wrap_machine_word:
	.byte	85,137,229,137,199,104
	.long	KLmachine_wordGVKeW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,137,120,4,201,195


	.global primitive_wrap_c_pointer
	.type primitive_wrap_c_pointer,@function
primitive_wrap_c_pointer:
	.byte	85,137,229,139,125,8,80,106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,137,120,4,201,194,4,0


	.global primitive_wrap_abstract_integer
	.type primitive_wrap_abstract_integer,@function
primitive_wrap_abstract_integer:
	.byte	61,255,255,255,31,127,14,61,0,0,0,224,124,7,193,224
	.byte	2,131,192,1,195,85,137,229,137,199,104
	.long	KLmachine_wordGVKeW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,137,120,4,201,235,228


	.global primitive_wrap_unsigned_abstract_integer
	.type primitive_wrap_unsigned_abstract_integer,@function
primitive_wrap_unsigned_abstract_integer:
	.byte	61,255,255,255,31,119,7,193,224,2,131,192,1,195,85,137
	.byte	229,137,199,104
	.long	KLmachine_wordGVKeW
	.byte	106,8,252,232
	.long	primitive_alloc_leaf - 4 - .
	.byte	131,196,8,137,120,4,201,235,228


	.global primitive_unwrap_abstract_integer
	.type primitive_unwrap_abstract_integer,@function
primitive_unwrap_abstract_integer:
	.byte	137,199,131,231,3,131,255,0,116,4,193,248,2,195,139,64
	.byte	4,235,250


	.global primitive_machine_word_count_low_zeros
	.type primitive_machine_word_count_low_zeros,@function
primitive_machine_word_count_low_zeros:
	.byte	131,248,0,117,6,184,32,0,0,0,195,51,255,60,0,117
	.byte	32,169,0,255,0,0,117,19,169,0,0,255,0,117,6,131
	.byte	199,8,193,232,8,131,199,8,193,232,8,131,199,8,193,232
	.byte	8,168,15,117,6,131,199,4,193,232,4,131,224,15,15,182
	.byte	128
	.long	Klow_zeros_table
	.byte	3,199,195


	.global primitive_machine_word_count_high_zeros
	.type primitive_machine_word_count_high_zeros,@function
primitive_machine_word_count_high_zeros:
	.byte	131,248,0,117,6,184,32,0,0,0,195,51,255,169,0,0
	.byte	0,255,117,32,169,0,0,255,0,117,19,169,0,255,0,0
	.byte	117,6,131,199,8,193,224,8,131,199,8,193,224,8,131,199
	.byte	8,193,224,8,169,0,0,0,240,117,6,131,199,4,193,224
	.byte	4,193,232,28,15,182,128
	.long	Khigh_zeros_table
	.byte	3,199,195


	.global primitive_type_check
	.type primitive_type_check,@function
primitive_type_check:
	.byte	139,124,36,4,129,255
	.long	KLobjectGVKd
	.byte	116,40,85,137,229,131,236,4,137,69,252,139,95,4,87,139
	.byte	69,252,255,211,61
	.long	KPfalseVKi
	.byte	117,11,255,117,8,139,69,252,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	139,69,252,201,194,4,0


	.global primitive_type_check_values
	.type primitive_type_check_values,@function
primitive_type_check_values:
	.byte	85,137,229,131,236,32,139,85,8,80,156,89,128,229,4,15
	.byte	132,148,0,0,0,106,1,139,52,36,137,117,240,141,116,36
	.byte	4,137,117,244,141,114,8,137,117,224,139,114,4,137,117,228
	.byte	131,109,228,1,199,69,252,0,0,0,0,199,69,236,0,0
	.byte	0,0,139,117,236,59,117,228,15,132,231,0,0,0,139,117
	.byte	236,59,117,240,15,141,219,0,0,0,139,117,244,3,117,252
	.byte	139,54,137,117,248,139,117,224,3,117,236,139,54,137,117,232
	.byte	129,125,232
	.long	KPfalseVKi
	.byte	116,38,139,125,232,129,255
	.long	KLobjectGVKd
	.byte	116,27,139,95,4,87,139,69,248,255,211,61
	.long	KPfalseVKi
	.byte	117,11,255,117,232,139,69,248,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	131,69,252,4,131,69,236,4,235,153,86,232,0,0,0,0
	.byte	139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,12,36,131,196,8,94,139,73,32,137,203,193,227
	.byte	2,43,227,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,137,231,252,243,165
	.byte	83,233,226,254,255,255,129,125,12
	.long	KPfalseVKi
	.byte	116,63,139,117,252,59,117,240,125,55,139,117,244,3,117,252
	.byte	139,54,137,117,248,139,125,12,129,255
	.long	KLobjectGVKd
	.byte	116,27,139,95,4,87,139,69,248,255,211,61
	.long	KPfalseVKi
	.byte	117,11,255,117,12,139,69,248,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	131,69,252,4,235,193,91,131,251,1,117,6,253,88,201,194
	.byte	8,0,137,218,193,250,2,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,32,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,137,209,137,230
	.byte	252,243,165,3,227,252,233,109,255,255,255


	.global primitive_type_check_rest_values
	.type primitive_type_check_rest_values,@function
primitive_type_check_rest_values:
	.byte	85,137,229,131,236,16,137,194,80,156,89,128,229,4,116,85
	.byte	106,1,139,52,36,137,117,240,141,116,36,4,137,117,244,137
	.byte	85,252,139,117,252,59,117,240,15,141,195,0,0,0,139,117
	.byte	244,3,117,252,139,54,137,117,248,139,125,8,129,255
	.long	KLobjectGVKd
	.byte	116,27,139,95,4,87,139,69,248,255,211,61
	.long	KPfalseVKi
	.byte	117,11,255,117,8,139,69,248,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	131,69,252,4,235,189,86,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,12,36,131,196,8,94,139,73,32,137,203,193,227
	.byte	2,43,227,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,137,231,252,243,165
	.byte	83,233,33,255,255,255,91,131,251,1,117,6,253,88,201,194
	.byte	4,0,137,218,193,250,2,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,32,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,137,209,137,230
	.byte	252,243,165,3,227,252,233,109,255,255,255


	.global primitive_adjust_mv
	.type primitive_adjust_mv,@function
primitive_adjust_mv:
	.byte	156,89,128,229,4,15,132,152,0,0,0,131,255,1,15,132
	.byte	142,0,0,0,232,0,0,0,0,139,52,36,131,198,14,3
	.byte	53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,126,32,131,255,0,116,76
	.byte	86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,131,195,36,137,249,184
	.long	KPfalseVKi
	.byte	137,223,252,243,171,137,3,253,195,86,232,0,0,0,0,139
	.byte	52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,59,251,116,189
	.byte	131,255,1,116,183,232,0,0,0,0,139,52,36,131,198,14
	.byte	3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,126,32,59,251,124,81,137
	.byte	249,43,203,137,223,86,232,0,0,0,0,139,52,36,131,198
	.byte	14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,131,195,36,193,231,2,3
	.byte	251,184
	.long	KPfalseVKi
	.byte	252,243,171,252,233,33,255,255,255


	.global primitive_adjust_mv_rest
	.type primitive_adjust_mv_rest,@function
primitive_adjust_mv_rest:
	.byte	156,89,128,229,4,15,132,147,0,0,0,131,255,1,15,142
	.byte	137,0,0,0,232,0,0,0,0,139,52,36,131,198,14,3
	.byte	53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,126,32,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,20,36,131,196,8,94,131,194,36,137,249,184
	.long	KPfalseVKi
	.byte	137,215,252,243,171,137,2,253,195,86,232,0,0,0,0,139
	.byte	52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,59,251,126,189
	.byte	131,255,1,116,183,137,249,43,203,86,232,0,0,0,0,139
	.byte	52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,20,36,131,196,8,94,131,194,36,193,227,2,3
	.byte	218,184
	.long	KPfalseVKi
	.byte	137,223,252,243,171,232,0,0,0,0,139,52,36,131,198,14
	.byte	3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,126,32,252,233,37,255,255
	.byte	255


	.global primitive_pad_mv
	.type primitive_pad_mv,@function
primitive_pad_mv:
	.byte	156,89,128,229,4,116,80,131,255,1,126,74,86,232,0,0
	.byte	0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,131,195,36,137,249,184
	.long	KPfalseVKi
	.byte	137,223,252,243,171,195,86,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,131,255,1,116
	.byte	188,59,251,126,184,137,249,43,203,137,223,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,131,195,36,193,231,2,3
	.byte	251,184
	.long	KPfalseVKi
	.byte	252,243,171,233,98,255,255,255


	.global primitive_set_mv_from_vector
	.type primitive_set_mv_from_vector,@function
primitive_set_mv_from_vector:
	.byte	139,80,4,193,250,2,131,250,1,117,5,139,64,8,253,195
	.byte	232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,32,131,250,0,116,78
	.byte	141,112,8,139,6,86,232,0,0,0,0,139,52,36,131,198
	.byte	14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,137,209,252,243
	.byte	165,252,233,112,255,255,255,184
	.long	KPfalseVKi
	.byte	252,233,101,255,255,255


	.global primitive_build_bind_exit_frame
	.type primitive_build_bind_exit_frame,@function
primitive_build_bind_exit_frame:
	.byte	94,131,236,56,137,68,36,52,86,232,0,0,0,0,139,52
	.byte	36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,0,137,108,36,48,137
	.byte	68,36,44,137,224,255,230


	.global primitive_build_unwind_protect_frame
	.type primitive_build_unwind_protect_frame,@function
primitive_build_unwind_protect_frame:
	.byte	95,131,236,12,137,68,36,8,86,232,0,0,0,0,139,52
	.byte	36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,0,137,108,36,4,137
	.byte	4,36,137,224,232,0,0,0,0,139,52,36,131,198,14,3
	.byte	53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,38,255,231


	.global primitive_unwind_protect_cleanup
	.type primitive_unwind_protect_cleanup,@function
primitive_unwind_protect_cleanup:
	.byte	86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,27,139,59,232,0,0
	.byte	0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,62,80,156,89,128,229,4
	.byte	116,29,106,1,83,139,123,8,255,215,91,90,131,250,1,15
	.byte	133,150,0,0,0,253,88,139,52,36,141,99,12,255,230,86
	.byte	232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,12,36,131,196,8,94,139,73,32,137,202,193,226
	.byte	2,43,226,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,137,231,252,243,165
	.byte	82,233,89,255,255,255,137,208,193,248,2,232,0,0,0,0
	.byte	139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,70,32,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,137,193,137,230
	.byte	252,243,165,3,226,252,233,221,254,255,255


	.global primitive_nlx
	.type primitive_nlx,@function
primitive_nlx:
	.byte	86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,20,36,131,196,8,94,139,18,139,72,44,137,195
	.byte	139,124,36,4,59,209,117,13,137,248,139,123,52,139,107,48
	.byte	141,99,56,255,231,141,67,4,156,89,128,229,4,15,132,201
	.byte	0,0,0,137,120,8,199,64,4,5,0,0,0,137,3,131
	.byte	250,0,15,132,229,1,0,0,137,212,139,60,36,139,68,36
	.byte	8,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,62,139,108,36,4,83,255
	.byte	208,91,86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,20,36,131,196,8,94,139,18,139,123,44,59,250
	.byte	15,133,99,255,255,255,139,3,139,80,4,193,250,2,131,250
	.byte	1,15,133,177,0,0,0,139,64,8,253,139,123,52,139,107
	.byte	48,141,99,56,255,231,86,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,12,36,131,196,8,94,139,73,32,131,249,8,126
	.byte	13,81,83,82,137,200,232
	.long	primitive_allocate_vector - 4 - .
	.byte	90,91,89,137,207,193,231,2,131,199,1,137,120,4,141,120
	.byte	8,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,252,243,165,233,159
	.byte	254,255,255,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,32,131,250,0,116,83
	.byte	141,112,8,139,6,86,232,0,0,0,0,139,52,36,131,198
	.byte	14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,137,209,252,243
	.byte	165,252,233,196,254,255,255,233
	.long	primitive_error - 4 - .
	.byte	184
	.long	KPfalseVKi
	.byte	252,233,180,254,255,255


	.global primitive_stack_allocate_vector
	.type primitive_stack_allocate_vector,@function
primitive_stack_allocate_vector:
	.byte	90,137,199,137,251,193,227,2,131,195,8,43,227,137,224,193
	.byte	231,2,131,199,1,137,120,4,199,0
	.long	KLsimple_object_vectorGVKdW
	.byte	255,226


	.global primitive_stack_allocate_vector_from_buffer
	.type primitive_stack_allocate_vector_from_buffer,@function
primitive_stack_allocate_vector_from_buffer:
	.byte	139,116,36,4,90,131,196,4,137,195,137,223,193,231,2,131
	.byte	199,8,43,231,137,224,137,223,193,231,2,131,199,1,137,120
	.byte	4,199,0
	.long	KLsimple_object_vectorGVKdW
	.byte	141,120,8,137,217,252,243,165,255,226


	.global primitive_stack_allocate_vector_from_buffer_with_offset
	.type primitive_stack_allocate_vector_from_buffer_with_offset,@function
primitive_stack_allocate_vector_from_buffer_with_offset:
	.byte	139,116,36,4,139,124,36,8,193,231,2,3,247,90,131,196
	.byte	8,137,195,137,223,193,231,2,131,199,8,43,231,137,224,137
	.byte	223,193,231,2,131,199,1,137,120,4,199,0
	.long	KLsimple_object_vectorGVKdW
	.byte	141,120,8,137,217,252,243,165,255,226


	.global primitive_heap_vector_remaining_values
	.type primitive_heap_vector_remaining_values,@function
primitive_heap_vector_remaining_values:
	.byte	232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,156,89,128,229,4
	.byte	116,25,131,255,0,117,104,137,6,184,1,0,0,0,137,247
	.byte	131,248,0,117,94,184
	.long	KPempty_vectorVKi
	.byte	195,86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,59,251,125,18
	.byte	131,255,0,117,2,137,6,43,223,193,231,2,3,247,137,216
	.byte	235,159,51,192,235,155,85,137,229,137,251,137,199,193,231,2
	.byte	131,199,8,83,106,1,80,104
	.long	KLsimple_object_vectorGVKdW
	.byte	87,252,232
	.long	primitive_alloc_rt - 4 - .
	.byte	131,196,20,201,235,132


	.global primitive_stack_vector_remaining_values
	.type primitive_stack_vector_remaining_values,@function
primitive_stack_vector_remaining_values:
	.byte	232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,131,198,36,156,89,128,229,4
	.byte	116,58,131,255,0,15,133,133,0,0,0,137,6,184,1,0
	.byte	0,0,90,137,195,137,223,193,231,2,131,199,8,43,231,137
	.byte	224,137,223,193,231,2,131,199,1,137,120,4,199,0
	.long	KLsimple_object_vectorGVKdW
	.byte	141,120,8,137,217,252,243,165,255,226,86,232,0,0,0,0
	.byte	139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,59,251,125,18
	.byte	131,255,0,117,2,137,6,43,223,193,231,2,3,247,137,216
	.byte	235,130,51,192,233,123,255,255,255


	.global primitive_mep_apply
	.type primitive_mep_apply,@function
primitive_mep_apply:
	.byte	137,195,139,84,36,4,139,68,36,8,139,72,4,193,249,2
	.byte	131,192,8,139,123,8,139,127,4,129,231,0,0,20,0,131
	.byte	255,0,117,69,131,249,0,15,132,230,0,0,0,131,249,1
	.byte	15,132,233,0,0,0,131,249,2,15,132,238,0,0,0,131
	.byte	249,3,15,132,246,0,0,0,131,233,1,94,131,196,8,137
	.byte	207,193,231,2,43,231,137,231,86,131,192,4,137,198,252,243
	.byte	165,137,215,139,64,252,255,99,12,139,123,8,139,127,4,193
	.byte	255,2,129,231,255,0,0,0,131,255,0,15,132,210,0,0
	.byte	0,95,131,196,4,137,20,36,137,206,193,230,2,43,230,87
	.byte	141,124,36,4,81,137,198,252,243,165,89,139,68,36,4,143
	.byte	4,36,139,123,8,139,127,4,193,255,2,129,231,255,0,0
	.byte	0,137,230,131,236,16,137,202,137,249,137,231,252,243,165,137
	.byte	247,193,226,2,137,209,139,115,8,139,118,4,129,230,252,3
	.byte	0,0,43,214,131,194,1,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,139,123,8,139
	.byte	127,4,193,255,2,129,231,255,0,0,0,193,231,2,3,252
	.byte	131,199,4,139,23,131,194,4,137,23,139,20,20,137,215,255
	.byte	99,12,143,68,36,4,131,196,4,137,215,255,99,12,143,68
	.byte	36,4,131,196,4,137,215,139,0,255,99,12,139,112,4,137
	.byte	116,36,8,143,4,36,137,215,139,0,255,99,12,139,112,4
	.byte	137,116,36,4,139,112,8,137,116,36,8,137,215,139,0,255
	.byte	99,12,131,232,8,199,68,36,8,4,0,0,0,143,4,36
	.byte	137,215,255,99,12


	.global primitive_mep_apply_with_optionals
	.type primitive_mep_apply_with_optionals,@function
primitive_mep_apply_with_optionals:
	.byte	137,195,139,84,36,4,139,68,36,8,139,72,4,193,249,2
	.byte	131,192,8,139,123,8,139,127,4,129,231,0,0,20,0,131
	.byte	255,0,117,57,131,249,0,116,104,131,249,1,116,111,131,249
	.byte	2,116,120,131,249,3,15,132,128,0,0,0,131,233,1,94
	.byte	131,196,8,137,207,193,231,2,43,231,137,231,86,131,192,4
	.byte	137,198,252,243,165,137,215,139,64,252,255,99,12,131,249,1
	.byte	116,111,131,249,2,116,124,137,207,193,231,2,137,124,36,8
	.byte	94,131,233,1,131,196,4,137,207,193,231,2,43,231,137,231
	.byte	86,131,192,4,137,198,252,243,165,137,215,139,64,252,255,99
	.byte	12,143,68,36,4,131,196,4,137,215,255,99,12,143,68,36
	.byte	4,131,196,4,137,215,139,0,255,99,12,139,112,4,137,116
	.byte	36,8,143,4,36,137,215,139,0,255,99,12,139,112,4,137
	.byte	116,36,4,139,112,8,137,116,36,8,137,215,139,0,255,99
	.byte	12,199,68,36,8,4,0,0,0,143,4,36,137,215,139,0
	.byte	255,99,12,139,112,4,137,116,36,4,199,68,36,8,8,0
	.byte	0,0,137,215,139,0,255,99,12


	.global primitive_apply
	.type primitive_apply,@function
primitive_apply:
	.byte	137,195,139,84,36,4,139,74,4,193,249,2,131,249,1,116
	.byte	52,131,249,2,116,59,95,137,200,193,224,2,43,224,137,200
	.byte	131,232,1,131,196,8,87,141,124,36,4,141,74,12,137,206
	.byte	137,193,252,243,165,139,74,4,193,249,2,131,194,8,139,2
	.byte	233
	.long	apply_xep - 4 - .
	.byte	139,66,8,94,137,52,36,233
	.long	apply_xep_0 - 4 - .
	.byte	139,66,8,139,114,12,137,116,36,4,233
	.long	apply_xep_1 - 4 - .


	.global primitive_remove_optionals
	.type primitive_remove_optionals,@function
primitive_remove_optionals:
	.byte	81,87,141,86,12,139,20,20,137,247,131,239,4,43,215,137
	.byte	241,141,116,36,12,141,60,22,3,249,3,241,131,239,4,131
	.byte	238,4,253,193,249,2,243,165,95,89,94,3,226,255,230


	.global primitive_start_timer
	.type primitive_start_timer,@function
primitive_start_timer:
	.byte	85,137,229,252,232
	.long	c_primitive_start_timer - 4 - .
	.byte	201,195


	.global primitive_stop_timer
	.type primitive_stop_timer,@function
primitive_stop_timer:
	.byte	85,137,229,252,232
	.long	c_primitive_stop_timer - 4 - .
	.byte	201,195


	.global primitive_exit_application
	.type primitive_exit_application,@function
primitive_exit_application:
	.byte	85,137,229,80,252,232
	.long	exit - 4 - .
	.byte	131,196,4,201,195


	.global primitive_run_application
	.type primitive_run_application,@function
primitive_run_application:
	.byte	85,137,229,80,252,232
	.long	system - 4 - .
	.byte	131,196,4,201,195


	.global call_first_dylan_function
	.type call_first_dylan_function,@function
call_first_dylan_function:
	.byte	85,137,229,156,83,86,87,139,85,12,131,250,0,116,8,141
	.byte	77,16,131,250,0,127,17,139,93,8,137,209,255,83,4,141
	.byte	101,240,95,94,91,157,93,195,139,1,141,89,4,131,250,1
	.byte	116,229,137,209,131,233,1,137,207,193,231,2,43,231,137,231
	.byte	137,222,252,243,165,235,208


	.global call_dylan_function
	.type call_dylan_function,@function
call_dylan_function:
	.byte	85,137,229,156,83,86,87,139,85,12,131,250,0,116,8,141
	.byte	77,16,131,250,0,127,17,139,93,8,137,209,255,83,4,141
	.byte	101,240,95,94,91,157,93,195,139,1,141,89,4,131,250,1
	.byte	116,229,137,209,131,233,1,137,207,193,231,2,43,231,137,231
	.byte	137,222,252,243,165,235,208


	.global spy_call_dylan_function
	.type spy_call_dylan_function,@function
spy_call_dylan_function:
	.byte	85,137,229,156,83,86,87,139,85,12,199,5
	.long	Prunning_dylan_spy_functionQ
	.byte	1,0,0,0,131,250,0,116,8,141,77,16,131,250,0,127
	.byte	27,139,93,8,137,209,255,83,4,199,5
	.long	Prunning_dylan_spy_functionQ
	.byte	0,0,0,0,141,101,240,95,94,91,157,93,195,139,1,141
	.byte	89,4,131,250,1,116,219,137,209,131,233,1,137,207,193,231
	.byte	2,43,231,137,231,137,222,252,243,165,235,198


	.global call_dylan_function_returning_all_values
	.type call_dylan_function_returning_all_values,@function
call_dylan_function_returning_all_values:
	.byte	85,137,229,156,83,86,87,139,85,12,131,250,0,116,8,141
	.byte	77,16,131,250,0,127,118,139,93,8,137,209,255,83,4,51
	.byte	210,86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,60,36,131,196,8,94,131,199,36,156,89,128,229
	.byte	4,116,73,131,250,0,15,133,151,0,0,0,137,7,186,1
	.byte	0,0,0,131,250,0,15,133,142,0,0,0,184
	.long	KPempty_vectorVKi
	.byte	141,101,240,95,94,91,157,93,195,139,1,141,89,4,131,250
	.byte	1,116,128,137,209,131,233,1,137,207,193,231,2,43,231,137
	.byte	231,137,222,252,243,165,233,104,255,255,255,86,232,0,0,0
	.byte	0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,28,36,131,196,8,94,139,91,32,59,211,125,21
	.byte	131,250,0,117,2,137,7,43,218,193,226,2,3,250,137,218
	.byte	233,112,255,255,255,51,210,233,105,255,255,255,137,209,193,225
	.byte	2,131,193,8,87,106,1,82,104
	.long	KLsimple_object_vectorGVKdW
	.byte	81,252,232
	.long	primitive_alloc_rt - 4 - .
	.byte	131,196,20,233,87,255,255,255


	.global make_dylan_vector
	.type make_dylan_vector,@function
make_dylan_vector:
	.byte	85,137,229,139,77,8,137,202,193,226,2,131,194,8,104
	.long	KPunboundVKi
	.byte	106,1,81,104
	.long	KLsimple_object_vectorGVKdW
	.byte	82,252,232
	.long	primitive_alloc_rf - 4 - .
	.byte	131,196,20,201,195


	.global get_current_teb
	.type get_current_teb,@function
get_current_teb:
	.byte	86,232,0,0,0,0,139,52,36,131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,195


	.global get_tlv_vector
	.type get_tlv_vector,@function
get_tlv_vector:
	.byte	85,137,229,86,86,232,0,0,0,0,139,52,36,131,198,14
	.byte	3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,64,4,141,101,252,94
	.byte	93,195


	.global set_tlv_vector
	.type set_tlv_vector,@function
set_tlv_vector:
	.byte	85,137,229,86,139,85,8,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,4,141,101,252,94,93
	.byte	195


	.global get_current_thread
	.type get_current_thread,@function
get_current_thread:
	.byte	85,137,229,86,86,232,0,0,0,0,139,52,36,131,198,14
	.byte	3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,64,8,141,101,252,94
	.byte	93,195


	.global set_current_thread
	.type set_current_thread,@function
set_current_thread:
	.byte	85,137,229,86,139,85,8,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,8,141,101,252,94,93
	.byte	195


	.global get_current_thread_handle
	.type get_current_thread_handle,@function
get_current_thread_handle:
	.byte	85,137,229,86,86,232,0,0,0,0,139,52,36,131,198,14
	.byte	3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,64,12,141,101,252,94
	.byte	93,195


	.global set_current_thread_handle
	.type set_current_thread_handle,@function
set_current_thread_handle:
	.byte	85,137,229,86,139,85,8,232,0,0,0,0,139,52,36,131
	.byte	198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,52,36,131,196,8,137,86,12,141,101,252,94,93
	.byte	195


	.global spy_call_interactive_function
	.type spy_call_interactive_function,@function
spy_call_interactive_function:
	.byte	85,137,229,156,83,86,87,139,125,8,186
	.long	Kmake_simple_lockYthreads_primitivesVdylan
	.byte	82,106,16,252,232
	.long	primitive_copy - 4 - .
	.byte	131,196,8,137,120,12,80,184,253,255,255,255,139,29
	.long	spy_invoke_dylan_under_coded_restartVKi
	.byte	185,2,0,0,0,255,83,4,141,101,240,95,94,91,157,93
	.byte	195


	.global spy_read_location_through_barrier
	.type spy_read_location_through_barrier,@function
spy_read_location_through_barrier:
	.byte	139,68,36,4,139,0,195


	.global spy_write_location_through_barrier
	.type spy_write_location_through_barrier,@function
spy_write_location_through_barrier:
	.byte	85,137,229,86,87,139,125,8,139,85,12,137,23,141,101,248
	.byte	95,94,93,195


	.global spy_read_thread_variable_at_offset
	.type spy_read_thread_variable_at_offset,@function
spy_read_thread_variable_at_offset:
	.byte	85,137,229,86,139,85,8,86,232,0,0,0,0,139,52,36
	.byte	131,198,14,3,53
	.long	Psegment_register_load_instruction_offset
	.byte	255,230,137,230,129,206,255,255,31,0,139,182,1,255,255,255
	.byte	86,139,116,36,4,131,198,46,255,230,101,139,53,60,0,0
	.byte	0,86,139,4,36,131,196,8,94,139,64,4,139,4,144,141
	.byte	101,252,94,93,195


	.global spy_start_debugger_transaction
	.type spy_start_debugger_transaction,@function
spy_start_debugger_transaction:
	.byte	252,233
	.long	primitive_mps_park - 4 - .


	.global spy_end_debugger_transaction
	.type spy_end_debugger_transaction,@function
spy_end_debugger_transaction:
	.byte	252,233
	.long	primitive_mps_release - 4 - .


	.global spy_fixup_imported_dylan_data
	.type spy_fixup_imported_dylan_data,@function
spy_fixup_imported_dylan_data:
	.byte	195


	.global spy_fixup_unimported_dylan_data
	.type spy_fixup_unimported_dylan_data,@function
spy_fixup_unimported_dylan_data:
	.byte	195


	.global spy_exit_application
	.type spy_exit_application,@function
spy_exit_application:
	.byte	85,137,229,106,0,252,232
	.long	exit - 4 - .
	.byte	131,196,4,201,195


	.global c_primitive_raw_as_string
	.type c_primitive_raw_as_string,@function
c_primitive_raw_as_string:
	.byte	85,137,229,156,83,86,87,139,69,8,232
	.long	primitive_raw_as_string - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global primitive_check_specializers
	.type primitive_check_specializers,@function
primitive_check_specializers:
	.byte	85,137,229,139,83,8,139,82,8,82,106,0,106,0,81,80
	.byte	83,87,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,31,137,117,244,137,69,248,137,247,129,255
	.long	KLobjectGVKd
	.byte	117,94,184
	.long	KPtrueVKi
	.byte	139,85,252,61
	.long	KPfalseVKi
	.byte	116,66,139,77,240,131,249,4,126,79,131,233,4,139,116,10
	.byte	8,129,254
	.long	KLobjectGVKd
	.byte	116,236,141,65,4,139,4,40,137,117,244,137,69,248,137,77
	.byte	240,137,247,129,255
	.long	KLobjectGVKd
	.byte	117,63,184
	.long	KPtrueVKi
	.byte	139,77,240,139,85,252,61
	.long	KPfalseVKi
	.byte	117,193,139,125,244,139,69,248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,95,4,87,255,211,235,159,95,91,88,201,139,83,8
	.byte	139,82,4,129,226,0,0,4,0,131,250,0,117,11,255,99
	.byte	12,139,95,4,87,255,211,235,190,255,99,16


	.global primitive_process_keys
	.type primitive_process_keys,@function
primitive_process_keys:
	.byte	139,83,20,80,83,87,82,139,123,8,139,127,4,129,231,0
	.byte	0,8,0,87,139,83,20,139,122,4,131,239,1,137,248,193
	.byte	232,1,141,90,12,137,207,43,248,131,199,4,59,249,119,12
	.byte	139,3,137,7,131,195,8,131,199,4,235,240,139,94,4,131
	.byte	235,1,246,195,4,116,20,131,196,4,131,196,4,95,91,88
	.byte	85,137,229,137,216,232
	.long	Kodd_keyword_arguments_errorVKiI - 4 - .
	.byte	195,141,126,8,141,52,59,139,66,4,131,232,1,131,194,8
	.byte	141,28,16,131,235,8,193,248,1,43,200,131,193,4,87,131
	.byte	238,8,139,60,36,59,247,115,12,131,196,8,131,196,4,95
	.byte	91,88,255,99,16,139,6,137,215,59,251,119,226,59,7,116
	.byte	5,131,199,8,235,243,43,250,193,255,1,139,70,4,137,4
	.byte	57,235,204


	.global primitive_process_keys_for_xep
	.type primitive_process_keys_for_xep,@function
primitive_process_keys_for_xep:
	.byte	139,83,20,80,83,87,82,139,123,8,139,127,4,129,231,0
	.byte	0,8,0,87,139,83,20,139,122,4,131,239,1,137,248,193
	.byte	232,1,141,90,12,137,207,43,248,131,199,4,59,249,119,12
	.byte	139,3,137,7,131,195,8,131,199,4,235,240,139,94,4,131
	.byte	235,1,246,195,4,116,20,131,196,4,131,196,4,95,91,88
	.byte	85,137,229,137,216,232
	.long	Kodd_keyword_arguments_errorVKiI - 4 - .
	.byte	195,141,126,8,141,52,59,139,66,4,131,232,1,131,194,8
	.byte	141,28,16,131,235,8,193,248,1,43,200,131,193,4,87,131
	.byte	238,8,139,60,36,59,247,115,12,131,196,8,131,196,4,95
	.byte	91,88,255,99,16,139,6,137,215,59,251,119,226,59,7,116
	.byte	5,131,199,8,235,243,43,250,193,255,1,139,70,4,137,4
	.byte	57,235,204


	.global primitive_process_keys_checking_args_for_xep
	.type primitive_process_keys_checking_args_for_xep,@function
primitive_process_keys_checking_args_for_xep:
	.byte	139,83,20,80,83,87,82,139,123,8,139,127,4,129,231,0
	.byte	0,8,0,87,139,83,20,139,122,4,131,239,1,137,248,193
	.byte	232,1,141,90,12,137,207,43,248,131,199,4,59,249,119,12
	.byte	139,3,137,7,131,195,8,131,199,4,235,240,139,94,4,131
	.byte	235,1,246,195,4,116,20,131,196,4,131,196,4,95,91,88
	.byte	85,137,229,137,216,232
	.long	Kodd_keyword_arguments_errorVKiI - 4 - .
	.byte	195,141,126,8,141,52,59,139,66,4,131,232,1,131,194,8
	.byte	141,28,16,131,235,8,193,248,1,43,200,131,193,4,87,131
	.byte	238,8,139,60,36,59,247,15,131,160,0,0,0,131,196,8
	.byte	131,196,4,95,91,88,85,137,229,139,83,8,139,82,4,129
	.byte	226,252,3,0,0,139,75,8,139,73,8,81,106,0,106,0
	.byte	82,80,83,87,139,113,8,129,254
	.long	KLobjectGVKd
	.byte	116,31,137,117,244,137,69,248,137,247,129,255
	.long	KLobjectGVKd
	.byte	117,115,184
	.long	KPtrueVKi
	.byte	139,77,252,61
	.long	KPfalseVKi
	.byte	116,66,139,85,240,131,250,4,126,100,131,234,4,139,116,17
	.byte	8,129,254
	.long	KLobjectGVKd
	.byte	116,236,141,66,4,139,4,40,137,117,244,137,69,248,137,85
	.byte	240,137,247,129,255
	.long	KLobjectGVKd
	.byte	117,67,184
	.long	KPtrueVKi
	.byte	139,85,240,139,77,252,61
	.long	KPfalseVKi
	.byte	117,193,139,125,244,139,69,248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,6,137,215,59,251,15,135,70,255,255,255,59,7,116
	.byte	28,131,199,8,235,239,139,95,4,87,255,211,235,138,95,91
	.byte	88,201,255,99,16,139,95,4,87,255,211,235,186,43,250,193
	.byte	255,1,139,70,4,137,4,57,233,22,255,255,255


	.global slotacc_single_q_instance_getter_xep
	.type slotacc_single_q_instance_getter_xep,@function
slotacc_single_q_instance_getter_xep:
	.byte	131,249,2,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_single_q_instance_getterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global slotacc_single_q_instance_setter_xep
	.type slotacc_single_q_instance_setter_xep,@function
slotacc_single_q_instance_setter_xep:
	.byte	131,249,3,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_single_q_instance_setterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global slotacc_single_q_class_getter_xep
	.type slotacc_single_q_class_getter_xep,@function
slotacc_single_q_class_getter_xep:
	.byte	131,249,2,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_single_q_class_getterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global slotacc_single_q_class_setter_xep
	.type slotacc_single_q_class_setter_xep,@function
slotacc_single_q_class_setter_xep:
	.byte	131,249,3,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_single_q_class_setterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global slotacc_repeated_instance_getter_xep
	.type slotacc_repeated_instance_getter_xep,@function
slotacc_repeated_instance_getter_xep:
	.byte	131,249,3,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_repeated_instance_getterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global slotacc_repeated_instance_setter_xep
	.type slotacc_repeated_instance_setter_xep,@function
slotacc_repeated_instance_setter_xep:
	.byte	131,249,4,117,10,191
	.long	KPfalseVKi
	.byte	233
	.long	KPslotacc_repeated_instance_setterVKiI - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global primitive_set_accessor_method_xep
	.type primitive_set_accessor_method_xep,@function
primitive_set_accessor_method_xep:
	.byte	139,124,36,4,193,255,2,131,255,0,116,25,131,255,1,116
	.byte	31,131,255,2,116,33,131,255,3,116,35,131,255,4,116,37
	.byte	131,255,5,116,39,191
	.long	slotacc_single_q_instance_getter_xep
	.byte	137,120,4,194,4,0,191
	.long	slotacc_single_q_instance_setter_xep
	.byte	235,243,191
	.long	slotacc_single_q_class_getter_xep
	.byte	235,236,191
	.long	slotacc_single_q_class_setter_xep
	.byte	235,229,191
	.long	slotacc_repeated_instance_getter_xep
	.byte	235,222,191
	.long	slotacc_repeated_instance_setter_xep
	.byte	235,215


	.global xep_0
	.type xep_0,@function
xep_0:
	.byte	131,249,0,117,8,191
	.long	KPfalseVKi
	.byte	255,99,12,85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_1
	.type xep_1,@function
xep_1:
	.byte	131,249,1,117,62,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,37,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	117,37,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,35,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,214,139,125,244,139,69
	.byte	248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195


	.global xep_2
	.type xep_2,@function
xep_2:
	.byte	131,249,2,117,118,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,37,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	117,93,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,91,95,91,88,201,139,83,8,139,82,8,139,114,12,129
	.byte	254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,4,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,60,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,35,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,158,139,125,244,139,69
	.byte	248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,191


	.global xep_3
	.type xep_3,@function
xep_3:
	.byte	131,249,3,15,133,182,0,0,0,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,45,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	15,133,153,0,0,0,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	15,132,150,0,0,0,95,91,88,201,139,83,8,139,82,8
	.byte	139,114,12,129,254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,4,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,119,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,94,95,91,88,201,139,83,8,139,82,8,139,114,16,129
	.byte	254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,8,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,73,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,38,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,233,95,255,255,255,139,125
	.byte	244,139,69,248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,132,139,95,4,87,137
	.byte	200,255,211,235,178


	.global xep_4
	.type xep_4,@function
xep_4:
	.byte	131,249,4,117,15,191
	.long	KPfalseVKi
	.byte	185,16,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_5
	.type xep_5,@function
xep_5:
	.byte	131,249,5,117,15,191
	.long	KPfalseVKi
	.byte	185,20,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_6
	.type xep_6,@function
xep_6:
	.byte	131,249,6,117,15,191
	.long	KPfalseVKi
	.byte	185,24,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_7
	.type xep_7,@function
xep_7:
	.byte	131,249,7,117,15,191
	.long	KPfalseVKi
	.byte	185,28,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_8
	.type xep_8,@function
xep_8:
	.byte	131,249,8,117,15,191
	.long	KPfalseVKi
	.byte	185,32,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep_9
	.type xep_9,@function
xep_9:
	.byte	131,249,9,117,15,191
	.long	KPfalseVKi
	.byte	185,36,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global xep
	.type xep,@function
xep:
	.byte	139,123,8,139,127,4,193,255,2,129,231,255,0,0,0,59
	.byte	207,117,22,191
	.long	KPfalseVKi
	.byte	139,75,8,139,73,4,129,225,252,3,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_0
	.type rest_xep_0,@function
rest_xep_0:
	.byte	94,131,249,0,127,41,137,224,131,236,12,86,193,225,2,137
	.byte	207,131,193,1,137,72,252,199,64,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,199,12,137,120,244,131,192,248,191
	.long	KPfalseVKi
	.byte	255,99,12,137,68,36,252,131,236,4,235,206


	.global rest_xep_1
	.type rest_xep_1,@function
rest_xep_1:
	.byte	131,249,1,114,109,131,236,16,141,84,36,16,137,207,139,50
	.byte	137,52,36,131,194,4,193,231,2,137,249,131,199,253,137,122
	.byte	252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,37,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	117,37,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,35,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,214,139,125,244,139,69
	.byte	248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_2
	.type rest_xep_2,@function
rest_xep_2:
	.byte	131,249,2,15,130,172,0,0,0,131,236,16,141,84,36,16
	.byte	137,207,139,50,139,74,4,137,52,36,137,76,36,4,131,194
	.byte	8,193,231,2,137,249,131,199,249,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,37,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	117,93,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,91,95,91,88,201,139,83,8,139,82,8,139,114,12,129
	.byte	254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,4,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,60,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,35,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,158,139,125,244,139,69
	.byte	248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,191


	.global rest_xep_3
	.type rest_xep_3,@function
rest_xep_3:
	.byte	131,249,3,15,130,243,0,0,0,131,236,16,141,84,36,16
	.byte	137,207,139,50,139,74,4,137,52,36,137,76,36,4,139,114
	.byte	8,137,116,36,8,131,194,12,193,231,2,137,249,131,199,245
	.byte	137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,191
	.long	KPfalseVKi
	.byte	139,83,8,139,82,8,139,114,8,129,254
	.long	KLobjectGVKd
	.byte	116,45,137,193,85,137,229,82,81,86,80,83,87,137,247,129
	.byte	255
	.long	KLobjectGVKd
	.byte	15,133,153,0,0,0,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	15,132,150,0,0,0,95,91,88,201,139,83,8,139,82,8
	.byte	139,114,12,129,254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,4,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,119,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,94,95,91,88,201,139,83,8,139,82,8,139,114,16,129
	.byte	254
	.long	KLobjectGVKd
	.byte	116,39,139,76,36,8,85,137,229,82,81,86,80,83,87,137
	.byte	247,129,255
	.long	KLobjectGVKd
	.byte	117,73,184
	.long	KPtrueVKi
	.byte	61
	.long	KPfalseVKi
	.byte	116,38,95,91,88,201,255,99,12,85,137,229,193,225,2,131
	.byte	193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,233,95,255,255,255,139,125
	.byte	244,139,69,248,87,232
	.long	Ktype_check_errorVKiI - 4 - .
	.byte	195,139,95,4,87,137,200,255,211,235,132,139,95,4,87,137
	.byte	200,255,211,235,178


	.global rest_xep_4
	.type rest_xep_4,@function
rest_xep_4:
	.byte	131,249,4,114,83,131,236,16,141,84,36,16,137,207,139,50
	.byte	139,74,4,137,52,36,137,76,36,4,139,114,8,139,74,12
	.byte	137,116,36,8,137,76,36,12,131,194,16,193,231,2,137,249
	.byte	131,199,241,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,191
	.long	KPfalseVKi
	.byte	185,16,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_5
	.type rest_xep_5,@function
rest_xep_5:
	.byte	131,249,5,114,90,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,137,116,36,16,131
	.byte	199,20,193,226,2,137,209,131,194,237,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	185,20,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_6
	.type rest_xep_6,@function
rest_xep_6:
	.byte	131,249,6,114,97,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,131,199,24,193,226,2,137,209,131,194
	.byte	233,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	185,24,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_7
	.type rest_xep_7,@function
rest_xep_7:
	.byte	131,249,7,114,104,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,137,116,36,24,131,199,28
	.byte	193,226,2,137,209,131,194,229,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	185,28,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_8
	.type rest_xep_8,@function
rest_xep_8:
	.byte	131,249,8,114,111,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,139,79,28,137,116,36,24
	.byte	137,76,36,28,131,199,32,193,226,2,137,209,131,194,225,137
	.byte	87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	185,32,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep_9
	.type rest_xep_9,@function
rest_xep_9:
	.byte	131,249,9,114,118,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,139,79,28,137,116,36,24
	.byte	137,76,36,28,139,119,32,137,116,36,32,131,199,36,193,226
	.byte	2,137,209,131,194,221,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	185,36,0,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_xep
	.type rest_xep,@function
rest_xep:
	.byte	139,123,8,139,127,4,193,255,2,129,231,255,0,0,0,59
	.byte	207,114,97,139,123,8,139,127,4,193,255,2,129,231,255,0
	.byte	0,0,137,230,131,236,16,137,202,137,249,137,231,252,243,165
	.byte	137,247,193,226,2,137,209,139,115,8,139,118,4,129,230,252
	.byte	3,0,0,43,214,131,194,1,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,191
	.long	KPfalseVKi
	.byte	139,75,8,139,73,4,129,225,252,3,0,0,233
	.long	primitive_check_specializers - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_0
	.type rest_key_xep_0,@function
rest_key_xep_0:
	.byte	139,83,20,139,82,4,193,250,3,94,131,249,0,127,71,131
	.byte	194,3,193,226,2,137,231,43,226,86,193,225,2,137,200,131
	.byte	193,1,137,79,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,3,193,226,2,3
	.byte	194,137,71,244,131,199,248,137,248,137,249,131,233,8,137,254
	.byte	191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_for_xep - 4 - .
	.byte	137,68,36,252,131,236,4,235,176


	.global rest_key_xep_1
	.type rest_key_xep_1,@function
rest_key_xep_1:
	.byte	131,249,1,114,97,139,123,20,139,127,4,193,255,3,131,199
	.byte	4,193,231,2,43,231,141,20,60,137,207,139,50,137,52,36
	.byte	131,194,4,193,231,2,137,249,131,199,253,137,122,252,199,66
	.byte	248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,123,20,139,127,4,193,255,3,131,199,4,193,231,2,3
	.byte	207,131,233,4,137,74,244,137,214,43,247,131,194,248,137,22
	.byte	137,209,131,233,8,137,214,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_2
	.type rest_key_xep_2,@function
rest_key_xep_2:
	.byte	131,249,2,114,103,139,83,20,139,82,4,193,250,3,131,194
	.byte	4,193,226,2,43,226,3,212,137,207,139,50,139,74,4,137
	.byte	52,36,137,76,36,4,131,194,8,193,231,2,137,249,131,199
	.byte	249,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,123,20,139,127,4,193,255,3,131,199,4,193,231,2,3
	.byte	207,131,233,4,137,74,244,137,214,43,247,131,194,248,137,22
	.byte	137,209,131,233,8,137,214,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_3
	.type rest_key_xep_3,@function
rest_key_xep_3:
	.byte	131,249,3,114,110,139,83,20,139,82,4,193,250,3,131,194
	.byte	4,193,226,2,43,226,3,212,137,207,139,50,139,74,4,137
	.byte	52,36,137,76,36,4,139,114,8,137,116,36,8,131,194,12
	.byte	193,231,2,137,249,131,199,245,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,123,20,139,127,4,193,255,3,131,199,4,193,231,2,3
	.byte	207,131,233,4,137,74,244,137,214,43,247,131,194,248,137,22
	.byte	137,209,131,233,8,137,214,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_4
	.type rest_key_xep_4,@function
rest_key_xep_4:
	.byte	131,249,4,114,117,139,83,20,139,82,4,193,250,3,131,194
	.byte	4,193,226,2,43,226,3,212,137,207,139,50,139,74,4,137
	.byte	52,36,137,76,36,4,139,114,8,139,74,12,137,116,36,8
	.byte	137,76,36,12,131,194,16,193,231,2,137,249,131,199,241,137
	.byte	122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,123,20,139,127,4,193,255,3,131,199,4,193,231,2,3
	.byte	207,131,233,4,137,74,244,137,214,43,247,131,194,248,137,22
	.byte	137,209,131,233,8,137,214,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_5
	.type rest_key_xep_5,@function
rest_key_xep_5:
	.byte	131,249,5,114,124,139,123,20,139,127,4,193,255,3,131,199
	.byte	4,193,231,2,43,231,3,252,137,202,139,55,139,79,4,137
	.byte	52,36,137,76,36,4,139,119,8,139,79,12,137,116,36,8
	.byte	137,76,36,12,139,119,16,137,116,36,16,131,199,20,193,226
	.byte	2,137,209,131,194,237,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_6
	.type rest_key_xep_6,@function
rest_key_xep_6:
	.byte	131,249,6,15,130,131,0,0,0,139,123,20,139,127,4,193
	.byte	255,3,131,199,4,193,231,2,43,231,3,252,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,131,199,24,193,226,2,137,209,131,194
	.byte	233,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_7
	.type rest_key_xep_7,@function
rest_key_xep_7:
	.byte	131,249,7,15,130,138,0,0,0,139,123,20,139,127,4,193
	.byte	255,3,131,199,4,193,231,2,43,231,3,252,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,137,116,36,24,131,199,28
	.byte	193,226,2,137,209,131,194,229,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_8
	.type rest_key_xep_8,@function
rest_key_xep_8:
	.byte	131,249,8,15,130,145,0,0,0,139,123,20,139,127,4,193
	.byte	255,3,131,199,4,193,231,2,43,231,3,252,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,139,79,28,137,116,36,24
	.byte	137,76,36,28,131,199,32,193,226,2,137,209,131,194,225,137
	.byte	87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep_9
	.type rest_key_xep_9,@function
rest_key_xep_9:
	.byte	131,249,9,15,130,152,0,0,0,139,123,20,139,127,4,193
	.byte	255,3,131,199,4,193,231,2,43,231,3,252,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,139,119,24,139,79,28,137,116,36,24
	.byte	137,76,36,28,139,119,32,137,116,36,32,131,199,36,193,226
	.byte	2,137,209,131,194,221,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_xep
	.type rest_key_xep,@function
rest_key_xep:
	.byte	139,123,8,139,127,4,193,255,2,129,231,255,0,0,0,59
	.byte	207,114,126,139,83,20,139,82,4,193,250,3,139,123,8,139
	.byte	127,4,193,255,2,129,231,255,0,0,0,131,194,4,193,226
	.byte	2,137,230,43,226,137,202,137,249,137,231,252,243,165,137,247
	.byte	193,226,2,137,209,139,115,8,139,118,4,129,230,252,3,0
	.byte	0,43,214,131,194,1,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	139,83,20,139,82,4,193,250,3,131,194,4,193,226,2,3
	.byte	202,131,233,4,137,79,244,137,254,43,242,131,199,248,137,62
	.byte	137,249,131,233,8,137,254,191
	.long	KPfalseVKi
	.byte	233
	.long	primitive_process_keys_checking_args_for_xep - 4 - .
	.byte	85,137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global rest_key_mep_0
	.type rest_key_mep_0,@function
rest_key_mep_0:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,4,193,225,2,43,225,141,60,12,139,55,137,52
	.byte	36,131,199,4,137,249,131,233,4,137,199,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_1
	.type rest_key_mep_1,@function
rest_key_mep_1:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,8,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,131,199,8,137,249,131,233,4
	.byte	139,124,36,4,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_2
	.type rest_key_mep_2,@function
rest_key_mep_2:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,12,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,137,116,36,8,131
	.byte	199,12,137,249,131,233,4,139,124,36,8,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_3
	.type rest_key_mep_3,@function
rest_key_mep_3:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,16,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,131,199,16,137,249,131,233,4,139,124
	.byte	36,12,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_4
	.type rest_key_mep_4,@function
rest_key_mep_4:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,20,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,137,116,36,16,131,199,20
	.byte	137,249,131,233,4,139,124,36,16,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_5
	.type rest_key_mep_5,@function
rest_key_mep_5:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,24,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,139,79,20,137,116,36,16
	.byte	137,76,36,20,131,199,24,137,249,131,233,4,139,124,36,20
	.byte	137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_6
	.type rest_key_mep_6,@function
rest_key_mep_6:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,28,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,139,79,20,137,116,36,16
	.byte	137,76,36,20,139,119,24,137,116,36,24,131,199,28,137,249
	.byte	131,233,4,139,124,36,24,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_7
	.type rest_key_mep_7,@function
rest_key_mep_7:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,32,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,139,79,20,137,116,36,16
	.byte	137,76,36,20,139,119,24,139,79,28,137,116,36,24,137,76
	.byte	36,28,131,199,32,137,249,131,233,4,139,124,36,28,137,254
	.byte	137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_8
	.type rest_key_mep_8,@function
rest_key_mep_8:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,36,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,139,79,20,137,116,36,16
	.byte	137,76,36,20,139,119,24,139,79,28,137,116,36,24,137,76
	.byte	36,28,139,119,32,137,116,36,32,131,199,36,137,249,131,233
	.byte	4,139,124,36,32,137,254,137,215,233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep_9
	.type rest_key_mep_9,@function
rest_key_mep_9:
	.byte	137,250,139,75,20,139,73,4,193,249,3,137,206,193,230,2
	.byte	1,116,36,40,193,225,2,43,225,141,60,12,139,55,139,79
	.byte	4,137,52,36,137,76,36,4,139,119,8,139,79,12,137,116
	.byte	36,8,137,76,36,12,139,119,16,139,79,20,137,116,36,16
	.byte	137,76,36,20,139,119,24,139,79,28,137,116,36,24,137,76
	.byte	36,28,139,119,32,139,79,36,137,116,36,32,137,76,36,36
	.byte	131,199,40,137,249,131,233,4,139,124,36,36,137,254,137,215
	.byte	233
	.long	primitive_process_keys - 4 - .


	.global rest_key_mep
	.type rest_key_mep,@function
rest_key_mep:
	.byte	87,83,139,123,8,139,127,4,193,255,2,129,231,255,0,0
	.byte	0,131,199,3,139,91,20,139,75,4,193,249,3,137,206,193
	.byte	230,2,137,251,193,227,2,1,52,28,193,225,2,137,230,43
	.byte	225,137,249,137,231,252,243,165,137,241,131,233,4,131,235,4
	.byte	139,28,28,91,95,137,222,233
	.long	primitive_process_keys - 4 - .


	.global new_gf_xep_0
	.type new_gf_xep_0,@function
new_gf_xep_0:
	.byte	131,249,0,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_1
	.type new_gf_xep_1,@function
new_gf_xep_1:
	.byte	131,249,1,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_2
	.type new_gf_xep_2,@function
new_gf_xep_2:
	.byte	131,249,2,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_3
	.type new_gf_xep_3,@function
new_gf_xep_3:
	.byte	131,249,3,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_4
	.type new_gf_xep_4,@function
new_gf_xep_4:
	.byte	131,249,4,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_5
	.type new_gf_xep_5,@function
new_gf_xep_5:
	.byte	131,249,5,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep_6
	.type new_gf_xep_6,@function
new_gf_xep_6:
	.byte	131,249,6,117,10,137,223,139,95,24,139,83,12,255,226,85
	.byte	137,229,193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_xep
	.type new_gf_xep,@function
new_gf_xep:
	.byte	139,123,8,139,127,4,193,255,2,129,231,255,0,0,0,59
	.byte	207,117,10,137,223,139,95,24,139,83,12,255,226,85,137,229
	.byte	193,225,2,131,193,1,81,137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_0
	.type new_gf_optional_xep_0,@function
new_gf_optional_xep_0:
	.byte	94,131,249,0,127,43,137,231,131,236,12,86,193,225,2,137
	.byte	202,131,193,1,137,79,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,194,12,137,87,244,141,71,248,137,223,139,95,24,139,83
	.byte	12,255,226,137,68,36,252,131,236,4,235,204


	.global new_gf_optional_xep_1
	.type new_gf_optional_xep_1,@function
new_gf_optional_xep_1:
	.byte	131,249,1,114,57,131,236,16,141,84,36,16,137,207,139,50
	.byte	137,52,36,131,194,4,193,231,2,137,249,131,199,253,137,122
	.byte	252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_2
	.type new_gf_optional_xep_2,@function
new_gf_optional_xep_2:
	.byte	131,249,2,114,64,131,236,16,141,84,36,16,137,207,139,50
	.byte	139,74,4,137,52,36,137,76,36,4,131,194,8,193,231,2
	.byte	137,249,131,199,249,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_3
	.type new_gf_optional_xep_3,@function
new_gf_optional_xep_3:
	.byte	131,249,3,114,71,131,236,16,141,84,36,16,137,207,139,50
	.byte	139,74,4,137,52,36,137,76,36,4,139,114,8,137,116,36
	.byte	8,131,194,12,193,231,2,137,249,131,199,245,137,122,252,199
	.byte	66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_4
	.type new_gf_optional_xep_4,@function
new_gf_optional_xep_4:
	.byte	131,249,4,114,78,131,236,16,141,84,36,16,137,207,139,50
	.byte	139,74,4,137,52,36,137,76,36,4,139,114,8,139,74,12
	.byte	137,116,36,8,137,76,36,12,131,194,16,193,231,2,137,249
	.byte	131,199,241,137,122,252,199,66,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,74,244,141,122,248,137,122,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_5
	.type new_gf_optional_xep_5,@function
new_gf_optional_xep_5:
	.byte	131,249,5,114,85,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,137,116,36,16,131
	.byte	199,20,193,226,2,137,209,131,194,237,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep_6
	.type new_gf_optional_xep_6,@function
new_gf_optional_xep_6:
	.byte	131,249,6,114,92,131,236,16,141,124,36,16,137,202,139,55
	.byte	139,79,4,137,52,36,137,76,36,4,139,119,8,139,79,12
	.byte	137,116,36,8,137,76,36,12,139,119,16,139,79,20,137,116
	.byte	36,16,137,76,36,20,131,199,24,193,226,2,137,209,131,194
	.byte	233,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global new_gf_optional_xep
	.type new_gf_optional_xep,@function
new_gf_optional_xep:
	.byte	139,123,8,139,127,4,193,255,2,129,231,255,0,0,0,59
	.byte	207,114,85,139,123,8,139,127,4,193,255,2,129,231,255,0
	.byte	0,0,137,230,131,236,16,137,202,137,249,137,231,252,243,165
	.byte	137,247,193,226,2,137,209,139,115,8,139,118,4,129,230,252
	.byte	3,0,0,43,214,131,194,1,137,87,252,199,71,248
	.long	KLsimple_object_vectorGVKdW
	.byte	131,193,12,137,79,244,141,87,248,137,87,240,137,223,139,95
	.byte	24,139,83,12,255,226,85,137,229,193,225,2,131,193,1,81
	.byte	137,216,232
	.long	Kargument_count_errorVKiI - 4 - .
	.byte	195


	.global apply_xep_0
	.type apply_xep_0,@function
apply_xep_0:
	.byte	139,80,4,193,250,2,131,250,1,126,37,131,234,1,137,215
	.byte	193,231,2,139,52,36,43,231,137,52,36,141,124,36,4,141
	.byte	112,12,137,209,252,243,165,141,74,1,139,64,8,255,99,4
	.byte	137,209,139,64,8,255,99,4


	.global apply_xep_1
	.type apply_xep_1,@function
apply_xep_1:
	.byte	139,116,36,4,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,1,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,1,87,141,124,36,4,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,1,0,0,0,139,52
	.byte	36,137,116,36,4,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,4,185,2,0,0,0,255,99,4


	.global apply_xep_2
	.type apply_xep_2,@function
apply_xep_2:
	.byte	139,116,36,8,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,2,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,2,87,141,124,36,8,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,2,0,0,0,139,52
	.byte	36,137,116,36,8,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,8,185,3,0,0,0,255,99,4


	.global apply_xep_3
	.type apply_xep_3,@function
apply_xep_3:
	.byte	139,116,36,12,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,3,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,3,87,141,124,36,12,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,3,0,0,0,139,52
	.byte	36,137,116,36,12,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,12,185,4,0,0,0,255,99,4


	.global apply_xep_4
	.type apply_xep_4,@function
apply_xep_4:
	.byte	139,116,36,16,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,4,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,4,87,141,124,36,16,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,4,0,0,0,139,52
	.byte	36,137,116,36,16,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,16,185,5,0,0,0,255,99,4


	.global apply_xep_5
	.type apply_xep_5,@function
apply_xep_5:
	.byte	139,116,36,20,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,5,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,5,87,141,124,36,20,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,5,0,0,0,139,52
	.byte	36,137,116,36,20,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,20,185,6,0,0,0,255,99,4


	.global apply_xep_6
	.type apply_xep_6,@function
apply_xep_6:
	.byte	139,116,36,24,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,6,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,6,87,141,124,36,24,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,6,0,0,0,139,52
	.byte	36,137,116,36,24,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,24,185,7,0,0,0,255,99,4


	.global apply_xep_7
	.type apply_xep_7,@function
apply_xep_7:
	.byte	139,116,36,28,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,7,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,7,87,141,124,36,28,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,7,0,0,0,139,52
	.byte	36,137,116,36,28,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,28,185,8,0,0,0,255,99,4


	.global apply_xep_8
	.type apply_xep_8,@function
apply_xep_8:
	.byte	139,116,36,32,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,8,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,8,87,141,124,36,32,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,8,0,0,0,139,52
	.byte	36,137,116,36,32,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,32,185,9,0,0,0,255,99,4


	.global apply_xep_9
	.type apply_xep_9,@function
apply_xep_9:
	.byte	139,116,36,36,139,126,4,193,255,2,131,255,0,116,58,131
	.byte	255,1,116,72,131,239,1,185,9,0,0,0,193,231,2,137
	.byte	230,43,231,137,231,252,243,165,137,247,139,55,139,78,4,193
	.byte	249,2,131,198,8,141,121,9,87,141,124,36,36,131,199,4
	.byte	90,252,243,165,137,209,255,99,4,185,9,0,0,0,139,52
	.byte	36,137,116,36,36,131,196,4,255,83,4,195,139,126,8,137
	.byte	124,36,36,185,10,0,0,0,255,99,4


	.global apply_xep
	.type apply_xep,@function
apply_xep:
	.byte	131,249,1,117,5,233
	.long	apply_xep_0 - 4 - .
	.byte	137,202,131,234,1,193,226,2,139,52,20,139,126,4,193,255
	.byte	2,131,255,0,116,62,131,255,1,116,73,131,239,1,137,209
	.byte	193,249,2,193,231,2,137,230,43,231,137,231,252,243,165,137
	.byte	241,137,215,139,49,139,78,4,193,249,2,131,198,8,137,250
	.byte	193,250,2,3,209,82,3,252,131,199,4,90,252,243,165,137
	.byte	209,255,99,4,131,233,1,139,52,36,137,52,20,131,196,4
	.byte	255,83,4,195,139,126,8,137,60,20,255,99,4


	.global primitive_engine_node_apply_with_optionals
	.type primitive_engine_node_apply_with_optionals,@function
primitive_engine_node_apply_with_optionals:
	.byte	137,195,139,84,36,4,139,68,36,8,139,72,4,193,249,2
	.byte	131,192,8,82,139,58,139,127,8,129,231,0,32,0,0,131
	.byte	255,0,116,5,139,82,20,235,235,139,122,8,139,127,4,90
	.byte	129,231,0,0,20,0,131,255,0,117,57,131,249,0,116,104
	.byte	131,249,1,116,111,131,249,2,116,120,131,249,3,15,132,128
	.byte	0,0,0,131,233,1,94,131,196,8,137,207,193,231,2,43
	.byte	231,137,231,86,131,192,4,137,198,252,243,165,137,215,139,64
	.byte	252,255,99,12,131,249,1,116,111,131,249,2,116,124,137,207
	.byte	193,231,2,137,124,36,8,94,131,233,1,131,196,4,137,207
	.byte	193,231,2,43,231,137,231,86,131,192,4,137,198,252,243,165
	.byte	137,215,139,64,252,255,99,12,143,68,36,4,131,196,4,137
	.byte	215,255,99,12,143,68,36,4,131,196,4,137,215,139,0,255
	.byte	99,12,139,112,4,137,116,36,8,143,4,36,137,215,139,0
	.byte	255,99,12,139,112,4,137,116,36,4,139,112,8,137,116,36
	.byte	8,137,215,139,0,255,99,12,199,68,36,8,4,0,0,0
	.byte	143,4,36,137,215,139,0,255,99,12,139,112,4,137,116,36
	.byte	4,199,68,36,8,8,0,0,0,137,215,139,0,255,99,12


	.global general_engine_node_1_entry
	.type general_engine_node_1_entry,@function
general_engine_node_1_entry:
	.byte	131,236,8,139,116,36,8,137,52,36,137,92,36,4,137,124
	.byte	36,8,139,91,8,255,227


	.global general_engine_node_2_entry
	.type general_engine_node_2_entry,@function
general_engine_node_2_entry:
	.byte	131,236,8,139,116,36,8,137,52,36,139,116,36,12,137,116
	.byte	36,4,137,92,36,8,137,124,36,12,139,91,8,255,227


	.global general_engine_node_3_entry
	.type general_engine_node_3_entry,@function
general_engine_node_3_entry:
	.byte	131,236,8,139,116,36,8,137,52,36,139,116,36,12,137,116
	.byte	36,4,139,116,36,16,137,116,36,8,137,92,36,12,137,124
	.byte	36,16,139,91,8,255,227


	.global general_engine_node_n_0
	.type general_engine_node_n_0,@function
general_engine_node_n_0:
	.byte	137,250,137,208,139,56,139,127,8,129,231,0,32,0,0,131
	.byte	255,0,116,5,139,64,20,235,235,95,106,1,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,224,87,85,137,229,139,123,8,82,83,255,215,201,194,8
	.byte	0


	.global general_engine_node_n
	.type general_engine_node_n,@function
general_engine_node_n:
	.byte	137,249,137,207,139,23,139,82,8,129,226,0,32,0,0,131
	.byte	250,0,116,5,139,127,20,235,235,139,127,8,139,127,4,129
	.byte	231,252,3,0,0,90,80,131,199,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,231,82,85,137,229,139,83,8,81,83,137,248,255,210,201
	.byte	139,124,36,8,131,199,7,94,3,231,255,230


	.global general_engine_node_n_optionals_0
	.type general_engine_node_n_optionals_0,@function
general_engine_node_n_optionals_0:
	.byte	85,137,229,87,131,236,4,106,5,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,137,224,139,125,252,139,83,8,87,83,255,210
	.byte	201,139,124,36,4,94,3,231,255,230


	.global general_engine_node_n_optionals
	.type general_engine_node_n_optionals,@function
general_engine_node_n_optionals:
	.byte	85,137,229,87,139,23,139,82,8,129,226,0,32,0,0,131
	.byte	250,0,116,5,139,127,20,235,235,139,87,8,139,82,4,129
	.byte	226,252,3,0,0,95,201,85,137,229,87,83,141,122,4,43
	.byte	231,131,199,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,141,117,8,141,124,36,12,137,209,193,249,2
	.byte	131,233,1,252,243,165,141,122,4,139,60,47,137,124,20,8
	.byte	137,224,82,139,93,248,139,125,252,139,83,8,87,83,255,210
	.byte	90,201,131,194,4,139,20,20,94,3,226,255,230


	.global general_engine_node_spread_optionals_0
	.type general_engine_node_spread_optionals_0,@function
general_engine_node_spread_optionals_0:
	.byte	85,137,229,87,139,125,252,139,83,8,87,83,255,210,201,139
	.byte	124,36,4,94,3,231,255,230


	.global general_engine_node_spread_optionals
	.type general_engine_node_spread_optionals,@function
general_engine_node_spread_optionals:
	.byte	85,137,229,87,139,23,139,82,8,129,226,0,32,0,0,131
	.byte	250,0,116,5,139,127,20,235,235,139,87,8,139,82,4,129
	.byte	226,252,3,0,0,95,201,85,137,229,87,83,141,122,4,139
	.byte	60,47,139,127,4,131,239,1,43,231,3,250,43,226,131,199
	.byte	1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,141,117,8,141,124,36,12,137,209,193,249,2
	.byte	131,233,1,252,243,165,141,122,4,139,60,47,139,79,4,193
	.byte	249,2,141,95,8,141,124,36,8,3,250,137,222,252,243,165
	.byte	137,224,82,139,93,248,139,125,252,139,83,8,87,83,255,210
	.byte	90,201,131,194,4,139,20,20,94,3,226,255,230


	.global boxed_instance_slot_getter_entry
	.type boxed_instance_slot_getter_entry,@function
boxed_instance_slot_getter_entry:
	.byte	139,91,8,131,195,3,139,60,24,129,255
	.long	KPunboundVKi
	.byte	116,4,137,248,253,195,193,251,2,131,235,1,193,227,2,131
	.byte	195,1,83,232
	.long	Kunbound_instance_slotVKeI - 4 - .
	.byte	235,234


	.global boxed_instance_slot_setter_entry
	.type boxed_instance_slot_setter_entry,@function
boxed_instance_slot_setter_entry:
	.byte	139,124,36,4,139,91,8,131,195,3,137,4,31,253,194,4
	.byte	0


	.global boxed_repeated_instance_slot_getter_entry
	.type boxed_repeated_instance_slot_getter_entry,@function
boxed_repeated_instance_slot_getter_entry:
	.byte	139,124,36,4,139,91,8,131,195,3,3,216,139,83,252,131
	.byte	255,0,124,26,59,250,125,22,137,250,131,226,252,139,28,19
	.byte	129,251
	.long	KPunboundVKi
	.byte	116,14,137,216,253,194,4,0,87,232
	.long	Krepeated_slot_getter_index_out_of_range_trapVKeI - 4 - .
	.byte	235,244,87,232
	.long	Kunbound_repeated_slotVKeI - 4 - .
	.byte	235,236


	.global boxed_repeated_instance_slot_setter_entry
	.type boxed_repeated_instance_slot_setter_entry,@function
boxed_repeated_instance_slot_setter_entry:
	.byte	137,194,139,68,36,4,139,124,36,8,139,91,8,131,195,3
	.byte	3,216,139,75,252,131,255,0,124,16,59,249,125,12,131,231
	.byte	252,137,20,59,137,208,253,194,4,0,87,232
	.long	Krepeated_slot_getter_index_out_of_range_trapVKeI - 4 - .
	.byte	235,244


	.global raw_byte_repeated_instance_slot_getter_entry
	.type raw_byte_repeated_instance_slot_getter_entry,@function
raw_byte_repeated_instance_slot_getter_entry:
	.byte	139,124,36,4,139,91,8,131,195,3,3,216,139,83,252,131
	.byte	255,0,124,23,59,250,125,19,137,248,193,248,2,15,182,4
	.byte	24,193,224,2,131,192,2,253,194,4,0,87,232
	.long	Krepeated_slot_getter_index_out_of_range_trapVKeI - 4 - .
	.byte	235,244


	.global raw_byte_repeated_instance_slot_setter_entry
	.type raw_byte_repeated_instance_slot_setter_entry,@function
raw_byte_repeated_instance_slot_setter_entry:
	.byte	137,194,139,68,36,4,139,124,36,8,139,91,8,131,195,3
	.byte	3,216,139,75,252,131,255,0,124,21,59,249,125,17,137,208
	.byte	193,248,2,193,255,2,136,4,59,137,208,253,194,4,0,87
	.byte	232
	.long	Krepeated_slot_getter_index_out_of_range_trapVKeI - 4 - .
	.byte	235,244


	.global single_method_entry
	.type single_method_entry,@function
single_method_entry:
	.byte	139,123,20,139,91,16,255,99,12


	.global unrestricted_keyed_single_method_entry_0
	.type unrestricted_keyed_single_method_entry_0,@function
unrestricted_keyed_single_method_entry_0:
	.byte	137,194,139,82,4,246,194,4,116,40,85,137,229,87,131,236
	.byte	4,106,5,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,137,224,139,125,252,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	201,139,124,36,4,94,3,231,255,230,139,123,20,139,91,16
	.byte	255,99,12


	.global unrestricted_keyed_single_method_entry_1
	.type unrestricted_keyed_single_method_entry_1,@function
unrestricted_keyed_single_method_entry_1:
	.byte	139,84,36,4,139,82,4,246,194,4,116,47,85,137,229,87
	.byte	131,236,8,106,9,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,137,224,139,125,252
	.byte	83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	201,139,124,36,8,94,3,231,255,230,139,123,20,139,91,16
	.byte	255,99,12


	.global unrestricted_keyed_single_method_entry_2
	.type unrestricted_keyed_single_method_entry_2,@function
unrestricted_keyed_single_method_entry_2:
	.byte	139,84,36,8,139,82,4,246,194,4,116,54,85,137,229,87
	.byte	131,236,12,106,13,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,139,125,12,137,124
	.byte	36,16,137,224,139,125,252,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	201,139,124,36,12,94,3,231,255,230,139,123,20,139,91,16
	.byte	255,99,12


	.global unrestricted_keyed_single_method_entry
	.type unrestricted_keyed_single_method_entry,@function
unrestricted_keyed_single_method_entry:
	.byte	139,83,4,193,250,6,129,226,252,3,0,0,139,20,20,139
	.byte	82,4,246,194,4,116,92,139,83,4,193,250,6,129,226,252
	.byte	3,0,0,85,137,229,87,83,141,122,4,43,231,131,199,1
	.byte	87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,141,117,8,141,124,36,12,137,209,193,249,2
	.byte	131,233,1,252,243,165,141,122,4,139,60,47,137,124,20,8
	.byte	137,224,82,139,93,248,139,125,252,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	90,201,131,194,4,139,20,20,94,3,226,255,230,139,123,20
	.byte	139,91,16,255,99,12


	.global explicit_keyed_single_method_entry_0
	.type explicit_keyed_single_method_entry_0,@function
explicit_keyed_single_method_entry_0:
	.byte	137,194,139,82,4,246,194,4,117,49,137,194,80,85,83,87
	.byte	137,208,139,91,24,139,120,4,131,239,1,3,248,59,248,126
	.byte	87,139,55,139,107,4,131,237,1,3,235,59,235,126,88,139
	.byte	85,4,59,214,116,87,131,237,4,235,240,51,246,85,137,229
	.byte	87,86,131,236,4,106,5,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,137,224,139,117,248,139,125,252,131,254,0,116
	.byte	53,139,83,24,104
	.long	KPfalseVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,4,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,176,131
	.byte	239,8,235,139,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,211


	.global explicit_keyed_single_method_entry_1
	.type explicit_keyed_single_method_entry_1,@function
explicit_keyed_single_method_entry_1:
	.byte	139,84,36,4,139,82,4,246,194,4,117,51,139,84,36,4
	.byte	80,85,83,87,137,208,139,91,24,139,120,4,131,239,1,3
	.byte	248,59,248,126,94,139,55,139,107,4,131,237,1,3,235,59
	.byte	235,126,95,139,85,4,59,214,116,94,131,237,4,235,240,51
	.byte	246,85,137,229,87,86,131,236,8,106,9,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,137,224,139,117,248
	.byte	139,125,252,131,254,0,116,53,139,83,24,104
	.long	KPfalseVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,8,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,169,131
	.byte	239,8,235,132,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,211


	.global explicit_keyed_single_method_entry_2
	.type explicit_keyed_single_method_entry_2,@function
explicit_keyed_single_method_entry_2:
	.byte	139,84,36,8,139,82,4,246,194,4,117,51,139,84,36,8
	.byte	80,85,83,87,137,208,139,91,24,139,120,4,131,239,1,3
	.byte	248,59,248,126,101,139,55,139,107,4,131,237,1,3,235,59
	.byte	235,126,102,139,85,4,59,214,116,101,131,237,4,235,240,51
	.byte	246,85,137,229,87,86,131,236,12,106,13,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,139,125,12,137,124
	.byte	36,16,137,224,139,117,248,139,125,252,131,254,0,116,56,139
	.byte	83,24,104
	.long	KPfalseVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,12,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,162,131
	.byte	239,8,233,122,255,255,255,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,208


	.global explicit_keyed_single_method_entry
	.type explicit_keyed_single_method_entry,@function
explicit_keyed_single_method_entry:
	.byte	137,222,139,91,4,193,251,6,129,227,252,3,0,0,139,28
	.byte	28,139,91,4,246,195,4,117,76,137,243,139,83,4,193,250
	.byte	6,129,226,252,3,0,0,139,20,20,80,85,83,87,137,208
	.byte	139,91,24,139,120,4,131,239,1,3,248,59,248,15,142,149
	.byte	0,0,0,139,55,139,107,4,131,237,1,3,235,59,235,15
	.byte	142,146,0,0,0,139,85,4,59,214,15,132,144,0,0,0
	.byte	131,237,4,235,232,137,243,51,246,139,83,4,193,250,6,129
	.byte	226,252,3,0,0,85,137,229,87,83,86,141,122,4,43,231
	.byte	131,199,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,141,117,8,141,124,36,12,137,209,193,249,2
	.byte	131,233,1,252,243,165,141,122,4,139,60,47,137,124,20,8
	.byte	137,224,139,117,244,82,139,93,248,139,125,252,131,254,0,116
	.byte	62,139,83,24,104
	.long	KPfalseVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	90,201,131,194,4,139,20,20,94,3,226,255,230,131,196,4
	.byte	91,93,88,139,123,20,139,91,16,255,99,12,95,91,93,88
	.byte	233,121,255,255,255,131,239,8,233,67,255,255,255,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,202


	.global implicit_keyed_single_method_entry_0
	.type implicit_keyed_single_method_entry_0,@function
implicit_keyed_single_method_entry_0:
	.byte	137,194,139,82,4,246,194,4,117,52,137,194,80,85,83,87
	.byte	137,208,139,91,16,139,83,20,139,120,4,131,239,1,3,248
	.byte	59,248,126,90,139,55,139,106,4,131,237,1,3,234,59,234
	.byte	126,91,139,93,0,59,222,116,90,131,237,8,235,240,51,246
	.byte	85,137,229,87,86,131,236,4,106,5,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,137,224,139,117,248,139,125,252,131,254,0,116
	.byte	56,139,83,16,139,82,20,104
	.long	KPtrueVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,4,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,173,131
	.byte	239,8,235,136,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,211


	.global implicit_keyed_single_method_entry_1
	.type implicit_keyed_single_method_entry_1,@function
implicit_keyed_single_method_entry_1:
	.byte	139,84,36,4,139,82,4,246,194,4,117,54,139,84,36,4
	.byte	80,85,83,87,137,208,139,91,16,139,83,20,139,120,4,131
	.byte	239,1,3,248,59,248,126,97,139,55,139,106,4,131,237,1
	.byte	3,234,59,234,126,98,139,93,0,59,222,116,97,131,237,8
	.byte	235,240,51,246,85,137,229,87,86,131,236,8,106,9,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,137,224,139,117,248
	.byte	139,125,252,131,254,0,116,56,139,83,16,139,82,20,104
	.long	KPtrueVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,8,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,166,131
	.byte	239,8,235,129,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,211


	.global implicit_keyed_single_method_entry_2
	.type implicit_keyed_single_method_entry_2,@function
implicit_keyed_single_method_entry_2:
	.byte	139,84,36,8,139,82,4,246,194,4,117,54,139,84,36,8
	.byte	80,85,83,87,137,208,139,91,16,139,83,20,139,120,4,131
	.byte	239,1,3,248,59,248,126,104,139,55,139,106,4,131,237,1
	.byte	3,234,59,234,126,105,139,93,0,59,222,116,104,131,237,8
	.byte	235,240,51,246,85,137,229,87,86,131,236,12,106,13,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,139,125,8,137,124,36,12,139,125,12,137,124
	.byte	36,16,137,224,139,117,248,139,125,252,131,254,0,116,59,139
	.byte	83,16,139,82,20,104
	.long	KPtrueVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	201,139,124,36,12,94,3,231,255,230,131,196,4,91,93,88
	.byte	139,123,20,139,91,16,255,99,12,95,91,93,88,235,159,131
	.byte	239,8,233,119,255,255,255,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,208


	.global implicit_keyed_single_method_entry
	.type implicit_keyed_single_method_entry,@function
implicit_keyed_single_method_entry:
	.byte	137,222,139,91,4,193,251,6,129,227,252,3,0,0,139,28
	.byte	28,139,91,4,246,195,4,117,79,137,243,139,83,4,193,250
	.byte	6,129,226,252,3,0,0,139,20,20,80,85,83,87,137,208
	.byte	139,91,16,139,83,20,139,120,4,131,239,1,3,248,59,248
	.byte	15,142,152,0,0,0,139,55,139,106,4,131,237,1,3,234
	.byte	59,234,15,142,149,0,0,0,139,93,0,59,222,15,132,147
	.byte	0,0,0,131,237,8,235,232,137,243,51,246,139,83,4,193
	.byte	250,6,129,226,252,3,0,0,85,137,229,87,83,86,141,122
	.byte	4,43,231,131,199,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	137,68,36,8,141,117,8,141,124,36,12,137,209,193,249,2
	.byte	131,233,1,252,243,165,141,122,4,139,60,47,137,124,20,8
	.byte	137,224,139,117,244,82,139,93,248,139,125,252,131,254,0,116
	.byte	65,139,83,16,139,82,20,104
	.long	KPtrueVKi
	.byte	82,86,83,87,232
	.long	Kinvalid_keyword_trapVKeI - 4 - .
	.byte	90,201,131,194,4,139,20,20,94,3,226,255,230,131,196,4
	.byte	91,93,88,139,123,20,139,91,16,255,99,12,95,91,93,88
	.byte	233,118,255,255,255,131,239,8,233,64,255,255,255,83,87,232
	.long	Kodd_number_of_keyword_args_trapVKeI - 4 - .
	.byte	235,202


	.global discriminate_on_argument_entry_0
	.type discriminate_on_argument_entry_0,@function
discriminate_on_argument_entry_0:
	.byte	137,193,80,87,139,83,8,83,87,137,200,255,210,137,195,95
	.byte	88,139,83,12,255,226


	.global discriminate_on_argument_entry_1
	.type discriminate_on_argument_entry_1,@function
discriminate_on_argument_entry_1:
	.byte	139,76,36,4,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry_2
	.type discriminate_on_argument_entry_2,@function
discriminate_on_argument_entry_2:
	.byte	139,76,36,8,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry_3
	.type discriminate_on_argument_entry_3,@function
discriminate_on_argument_entry_3:
	.byte	139,76,36,12,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry_4
	.type discriminate_on_argument_entry_4,@function
discriminate_on_argument_entry_4:
	.byte	139,76,36,16,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry_5
	.type discriminate_on_argument_entry_5,@function
discriminate_on_argument_entry_5:
	.byte	139,76,36,20,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry_6
	.type discriminate_on_argument_entry_6,@function
discriminate_on_argument_entry_6:
	.byte	139,76,36,24,80,87,139,83,8,83,87,137,200,255,210,137
	.byte	195,95,88,139,83,12,255,226


	.global discriminate_on_argument_entry
	.type discriminate_on_argument_entry,@function
discriminate_on_argument_entry:
	.byte	139,83,4,193,250,6,129,226,252,3,0,0,139,20,20,80
	.byte	87,139,75,8,83,87,137,208,255,209,137,195,95,88,139,83
	.byte	12,255,226


	.global if_type_discriminator_engine_0
	.type if_type_discriminator_engine_0,@function
if_type_discriminator_engine_0:
	.byte	137,194,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_1
	.type if_type_discriminator_engine_1,@function
if_type_discriminator_engine_1:
	.byte	139,84,36,4,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_2
	.type if_type_discriminator_engine_2,@function
if_type_discriminator_engine_2:
	.byte	139,84,36,8,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_3
	.type if_type_discriminator_engine_3,@function
if_type_discriminator_engine_3:
	.byte	139,84,36,12,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_4
	.type if_type_discriminator_engine_4,@function
if_type_discriminator_engine_4:
	.byte	139,84,36,16,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_5
	.type if_type_discriminator_engine_5,@function
if_type_discriminator_engine_5:
	.byte	139,84,36,20,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine_6
	.type if_type_discriminator_engine_6,@function
if_type_discriminator_engine_6:
	.byte	139,84,36,24,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global if_type_discriminator_engine
	.type if_type_discriminator_engine,@function
if_type_discriminator_engine:
	.byte	139,83,4,193,250,6,129,226,252,3,0,0,139,20,20,80
	.byte	87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,26,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,11,91,139,91,24,95,88,139,83,12,255,226,91,139,91
	.byte	20,235,243


	.global typecheck_discriminator_engine_0
	.type typecheck_discriminator_engine_0,@function
typecheck_discriminator_engine_0:
	.byte	137,194,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_1
	.type typecheck_discriminator_engine_1,@function
typecheck_discriminator_engine_1:
	.byte	139,84,36,4,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_2
	.type typecheck_discriminator_engine_2,@function
typecheck_discriminator_engine_2:
	.byte	139,84,36,8,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_3
	.type typecheck_discriminator_engine_3,@function
typecheck_discriminator_engine_3:
	.byte	139,84,36,12,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_4
	.type typecheck_discriminator_engine_4,@function
typecheck_discriminator_engine_4:
	.byte	139,84,36,16,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_5
	.type typecheck_discriminator_engine_5,@function
typecheck_discriminator_engine_5:
	.byte	139,84,36,20,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine_6
	.type typecheck_discriminator_engine_6,@function
typecheck_discriminator_engine_6:
	.byte	139,84,36,24,80,87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global typecheck_discriminator_engine
	.type typecheck_discriminator_engine,@function
typecheck_discriminator_engine:
	.byte	139,83,4,193,250,6,129,226,252,3,0,0,139,20,20,80
	.byte	87,139,123,16,83,129,255
	.long	KLobjectGVKd
	.byte	116,29,139,95,4,87,137,208,255,211,61
	.long	KPfalseVKi
	.byte	117,14,91,139,29
	.long	Dinapplicable_engine_nodeVKg
	.byte	95,88,139,83,12,255,226,91,139,91,20,235,243


	.global monomorphic_by_class_discriminator_engine_0
	.type monomorphic_by_class_discriminator_engine_0,@function
monomorphic_by_class_discriminator_engine_0:
	.byte	137,193,137,202,131,226,3,131,250,0,117,20,139,9,131,201
	.byte	1,139,83,16,59,209,117,19,139,91,20,139,83,12,255,226
	.byte	139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_1
	.type monomorphic_by_class_discriminator_engine_1,@function
monomorphic_by_class_discriminator_engine_1:
	.byte	139,76,36,4,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_2
	.type monomorphic_by_class_discriminator_engine_2,@function
monomorphic_by_class_discriminator_engine_2:
	.byte	139,76,36,8,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_3
	.type monomorphic_by_class_discriminator_engine_3,@function
monomorphic_by_class_discriminator_engine_3:
	.byte	139,76,36,12,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_4
	.type monomorphic_by_class_discriminator_engine_4,@function
monomorphic_by_class_discriminator_engine_4:
	.byte	139,76,36,16,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_5
	.type monomorphic_by_class_discriminator_engine_5,@function
monomorphic_by_class_discriminator_engine_5:
	.byte	139,76,36,20,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine_6
	.type monomorphic_by_class_discriminator_engine_6,@function
monomorphic_by_class_discriminator_engine_6:
	.byte	139,76,36,24,137,202,131,226,3,131,250,0,117,20,139,9
	.byte	131,201,1,139,83,16,59,209,117,19,139,91,20,139,83,12
	.byte	255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global monomorphic_by_class_discriminator_engine
	.type monomorphic_by_class_discriminator_engine,@function
monomorphic_by_class_discriminator_engine:
	.byte	139,75,4,193,249,6,129,225,252,3,0,0,139,12,12,137
	.byte	202,131,226,3,131,250,0,117,20,139,9,131,201,1,139,83
	.byte	16,59,209,117,19,139,91,20,139,83,12,255,226,139,53
	.long	Ddirect_object_mm_wrappersVKi
	.byte	139,12,150,235,227,139,29
	.long	Dabsent_engine_nodeVKg
	.byte	139,83,12,255,226


	.global general_engine_node_spread_entry
	.type general_engine_node_spread_entry,@function
general_engine_node_spread_entry:
	.byte	137,249,139,17,139,82,8,129,226,0,32,0,0,131,250,0
	.byte	116,5,139,73,20,235,235,139,81,8,139,82,4,193,250,2
	.byte	129,226,255,0,0,0,139,73,8,139,73,4,129,225,0,0
	.byte	20,0,131,249,0,117,22,185
	.long	general_engine_node_n
	.byte	131,250,1,125,10,131,250,0,117,5,185
	.long	general_engine_node_n_0
	.byte	255,225,185
	.long	general_engine_node_spread_optionals
	.byte	131,250,1,125,244,131,250,0,117,239,185
	.long	general_engine_node_spread_optionals_0
	.byte	235,232


	.global general_engine_node_n_entry
	.type general_engine_node_n_entry,@function
general_engine_node_n_entry:
	.byte	137,249,139,17,139,82,8,129,226,0,32,0,0,131,250,0
	.byte	116,5,139,73,20,235,235,139,81,8,139,82,4,193,250,2
	.byte	129,226,255,0,0,0,139,73,8,139,73,4,129,225,0,0
	.byte	20,0,131,249,0,117,22,185
	.long	general_engine_node_n
	.byte	131,250,1,125,10,131,250,0,117,5,185
	.long	general_engine_node_n_0
	.byte	255,225,185
	.long	general_engine_node_n_optionals
	.byte	131,250,1,125,244,131,250,0,117,239,185
	.long	general_engine_node_n_optionals_0
	.byte	235,232


	.global cache_header_entry
	.type cache_header_entry,@function
cache_header_entry:
	.byte	137,223,139,95,16,139,83,12,255,226


	.global profiling_cache_header_entry
	.type profiling_cache_header_entry,@function
profiling_cache_header_entry:
	.byte	137,223,129,61
	.long	Tdispatch_profiling_enabledQTVKe
	.long	KPfalseVKi
	.byte	116,18,186,1,0,0,0,240,15,193,83,24,131,194,1,131
	.byte	250,0,116,8,139,91,16,139,83,12,255,226,240,131,68,27
	.byte	28,1,235,240


	.global primitive_enable_cache_header_engine_node
	.type primitive_enable_cache_header_engine_node,@function
primitive_enable_cache_header_engine_node:
	.byte	191
	.long	cache_header_entry
	.byte	137,120,12,194,4,0


	.global primitive_invalidate_cache_header_engine_node
	.type primitive_invalidate_cache_header_engine_node,@function
primitive_invalidate_cache_header_engine_node:
	.byte	191
	.long	general_engine_node_n_entry
	.byte	137,120,12,194,4,0


	.global initialize_engine_node_table_entries_1
	.type initialize_engine_node_table_entries_1,@function
initialize_engine_node_table_entries_1:
	.byte	191
	.long	general_engine_node_spread_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_2
	.type initialize_engine_node_table_entries_2,@function
initialize_engine_node_table_entries_2:
	.byte	191
	.long	single_method_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_3
	.type initialize_engine_node_table_entries_3,@function
initialize_engine_node_table_entries_3:
	.byte	139,120,4,193,255,8,129,231,255,0,0,0,187
	.long	implicit_keyed_single_method_entry
	.byte	131,255,3,125,10,131,255,0,117,9,187
	.long	implicit_keyed_single_method_entry_0
	.byte	137,88,12,195,131,255,1,117,7,187
	.long	implicit_keyed_single_method_entry_1
	.byte	235,240,131,255,2,117,235,187
	.long	implicit_keyed_single_method_entry_2
	.byte	235,228


	.global initialize_engine_node_table_entries_4
	.type initialize_engine_node_table_entries_4,@function
initialize_engine_node_table_entries_4:
	.byte	139,120,4,193,255,8,129,231,255,0,0,0,187
	.long	explicit_keyed_single_method_entry
	.byte	131,255,3,125,10,131,255,0,117,9,187
	.long	explicit_keyed_single_method_entry_0
	.byte	137,88,12,195,131,255,1,117,7,187
	.long	explicit_keyed_single_method_entry_1
	.byte	235,240,131,255,2,117,235,187
	.long	explicit_keyed_single_method_entry_2
	.byte	235,228


	.global initialize_engine_node_table_entries_5
	.type initialize_engine_node_table_entries_5,@function
initialize_engine_node_table_entries_5:
	.byte	139,120,4,193,255,8,129,231,255,0,0,0,187
	.long	unrestricted_keyed_single_method_entry
	.byte	131,255,3,125,10,131,255,0,117,9,187
	.long	unrestricted_keyed_single_method_entry_0
	.byte	137,88,12,195,131,255,1,117,7,187
	.long	unrestricted_keyed_single_method_entry_1
	.byte	235,240,131,255,2,117,235,187
	.long	unrestricted_keyed_single_method_entry_2
	.byte	235,228


	.global initialize_engine_node_table_entries_13
	.type initialize_engine_node_table_entries_13,@function
initialize_engine_node_table_entries_13:
	.byte	191
	.long	profiling_cache_header_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_14
	.type initialize_engine_node_table_entries_14,@function
initialize_engine_node_table_entries_14:
	.byte	191
	.long	cache_header_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_16
	.type initialize_engine_node_table_entries_16,@function
initialize_engine_node_table_entries_16:
	.byte	191
	.long	boxed_instance_slot_getter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_17
	.type initialize_engine_node_table_entries_17,@function
initialize_engine_node_table_entries_17:
	.byte	191
	.long	boxed_instance_slot_setter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_18
	.type initialize_engine_node_table_entries_18,@function
initialize_engine_node_table_entries_18:
	.byte	191
	.long	boxed_repeated_instance_slot_getter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_19
	.type initialize_engine_node_table_entries_19,@function
initialize_engine_node_table_entries_19:
	.byte	191
	.long	boxed_repeated_instance_slot_setter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_22
	.type initialize_engine_node_table_entries_22,@function
initialize_engine_node_table_entries_22:
	.byte	191
	.long	raw_byte_repeated_instance_slot_getter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_23
	.type initialize_engine_node_table_entries_23,@function
initialize_engine_node_table_entries_23:
	.byte	191
	.long	raw_byte_repeated_instance_slot_setter_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_20
	.type initialize_engine_node_table_entries_20,@function
initialize_engine_node_table_entries_20:
	.byte	191
	.long	general_engine_node_1_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_21
	.type initialize_engine_node_table_entries_21,@function
initialize_engine_node_table_entries_21:
	.byte	191
	.long	general_engine_node_2_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_29
	.type initialize_engine_node_table_entries_29,@function
initialize_engine_node_table_entries_29:
	.byte	191
	.long	general_engine_node_3_entry
	.byte	137,120,12,195


	.global initialize_engine_node_table_entries_default
	.type initialize_engine_node_table_entries_default,@function
initialize_engine_node_table_entries_default:
	.byte	191
	.long	general_engine_node_n_entry
	.byte	137,120,12,195


	.global primitive_initialize_engine_node
	.type primitive_initialize_engine_node,@function
primitive_initialize_engine_node:
	.byte	139,120,4,129,231,252,0,0,0,139,191
	.long	Kinitialize_engine_node_table
	.byte	255,231


	.global primitive_initialize_discriminator
	.type primitive_initialize_discriminator,@function
primitive_initialize_discriminator:
	.byte	139,120,4,193,255,8,129,231,255,0,0,0,139,88,4,193
	.byte	251,2,131,227,63,131,251,33,116,39,131,251,32,116,56,131
	.byte	251,42,116,73,131,251,13,116,90,187
	.long	discriminate_on_argument_entry
	.byte	131,255,7,125,10,131,255,0,117,82,187
	.long	discriminate_on_argument_entry_0
	.byte	137,88,12,195,187
	.long	if_type_discriminator_engine
	.byte	131,255,7,125,242,131,255,0,117,70,187
	.long	if_type_discriminator_engine_0
	.byte	235,230,187
	.long	typecheck_discriminator_engine
	.byte	131,255,7,125,220,131,255,0,117,60,187
	.long	typecheck_discriminator_engine_0
	.byte	235,208,187
	.long	monomorphic_by_class_discriminator_engine
	.byte	131,255,7,125,198,131,255,0,117,50,187
	.long	monomorphic_by_class_discriminator_engine_0
	.byte	235,186,187
	.long	profiling_cache_header_entry
	.byte	235,179,131,255,1,117,43,187
	.long	discriminate_on_argument_entry_1
	.byte	235,167,131,255,1,117,46,187
	.long	if_type_discriminator_engine_1
	.byte	235,155,131,255,1,117,49,187
	.long	typecheck_discriminator_engine_1
	.byte	235,143,131,255,1,117,52,187
	.long	monomorphic_by_class_discriminator_engine_1
	.byte	235,131,131,255,2,117,55,187
	.long	discriminate_on_argument_entry_2
	.byte	233,116,255,255,255,131,255,2,117,55,187
	.long	if_type_discriminator_engine_2
	.byte	233,101,255,255,255,131,255,2,117,55,187
	.long	typecheck_discriminator_engine_2
	.byte	233,86,255,255,255,131,255,2,117,55,187
	.long	monomorphic_by_class_discriminator_engine_2
	.byte	233,71,255,255,255,131,255,3,117,55,187
	.long	discriminate_on_argument_entry_3
	.byte	233,56,255,255,255,131,255,3,117,55,187
	.long	if_type_discriminator_engine_3
	.byte	233,41,255,255,255,131,255,3,117,55,187
	.long	typecheck_discriminator_engine_3
	.byte	233,26,255,255,255,131,255,3,117,55,187
	.long	monomorphic_by_class_discriminator_engine_3
	.byte	233,11,255,255,255,131,255,4,117,55,187
	.long	discriminate_on_argument_entry_4
	.byte	233,252,254,255,255,131,255,4,117,55,187
	.long	if_type_discriminator_engine_4
	.byte	233,237,254,255,255,131,255,4,117,55,187
	.long	typecheck_discriminator_engine_4
	.byte	233,222,254,255,255,131,255,4,117,55,187
	.long	monomorphic_by_class_discriminator_engine_4
	.byte	233,207,254,255,255,131,255,5,117,55,187
	.long	discriminate_on_argument_entry_5
	.byte	233,192,254,255,255,131,255,5,117,59,187
	.long	if_type_discriminator_engine_5
	.byte	233,177,254,255,255,131,255,5,117,63,187
	.long	typecheck_discriminator_engine_5
	.byte	233,162,254,255,255,131,255,5,117,67,187
	.long	monomorphic_by_class_discriminator_engine_5
	.byte	233,147,254,255,255,131,255,6,15,133,138,254,255,255,187
	.long	discriminate_on_argument_entry_6
	.byte	233,128,254,255,255,131,255,6,15,133,119,254,255,255,187
	.long	if_type_discriminator_engine_6
	.byte	233,109,254,255,255,131,255,6,15,133,100,254,255,255,187
	.long	typecheck_discriminator_engine_6
	.byte	233,90,254,255,255,131,255,6,15,133,81,254,255,255,187
	.long	monomorphic_by_class_discriminator_engine_6
	.byte	233,71,254,255,255


	.global primitive_set_generic_function_entrypoints
	.type primitive_set_generic_function_entrypoints,@function
primitive_set_generic_function_entrypoints:
	.byte	139,120,8,139,127,4,193,255,2,129,231,255,0,0,0,139
	.byte	88,8,139,91,4,129,227,0,0,20,0,131,251,0,117,24
	.byte	187
	.long	new_gf_xep
	.byte	131,255,7,125,10,131,255,0,117,34,187
	.long	new_gf_xep_0
	.byte	137,88,4,195,187
	.long	new_gf_optional_xep
	.byte	131,255,7,125,10,131,255,0,117,22,187
	.long	new_gf_optional_xep_0
	.byte	137,88,4,235,230,131,255,1,117,19,187
	.long	new_gf_xep_1
	.byte	235,215,131,255,1,117,19,187
	.long	new_gf_optional_xep_1
	.byte	235,227,131,255,2,117,19,187
	.long	new_gf_xep_2
	.byte	235,191,131,255,2,117,19,187
	.long	new_gf_optional_xep_2
	.byte	235,203,131,255,3,117,19,187
	.long	new_gf_xep_3
	.byte	235,167,131,255,3,117,19,187
	.long	new_gf_optional_xep_3
	.byte	235,179,131,255,4,117,19,187
	.long	new_gf_xep_4
	.byte	235,143,131,255,4,117,22,187
	.long	new_gf_optional_xep_4
	.byte	235,155,131,255,5,117,22,187
	.long	new_gf_xep_5
	.byte	233,116,255,255,255,131,255,5,117,26,187
	.long	new_gf_optional_xep_5
	.byte	235,128,131,255,6,15,133,95,255,255,255,187
	.long	new_gf_xep_6
	.byte	233,85,255,255,255,131,255,6,15,133,100,255,255,255,187
	.long	new_gf_optional_xep_6
	.byte	233,90,255,255,255


	.global primitive_preboot_symbols
	.type primitive_preboot_symbols,@function
primitive_preboot_symbols:
	.byte	139,29
	.long	Poblist
	.byte	131,195,8,139,61
	.long	PoblistUcursor
	.byte	131,239,8,193,255,2,131,255,0,117,36,184
	.long	KPempty_vectorVKi
	.byte	199,5
	.long	PoblistUsize
	.byte	8,0,0,0,199,5
	.long	PoblistUcursor
	.byte	8,0,0,0,199,5
	.long	Poblist
	.byte	0,0,0,0,195,85,137,229,137,218,137,251,193,227,2,131
	.byte	195,8,82,106,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	83,252,232
	.long	primitive_alloc_rt - 4 - .
	.byte	131,196,20,199,5
	.long	PoblistUsize
	.byte	8,0,0,0,199,5
	.long	PoblistUcursor
	.byte	8,0,0,0,199,5
	.long	Poblist
	.byte	0,0,0,0,201,235,190


	.global primitive_make_symbol
	.type primitive_make_symbol,@function
primitive_make_symbol:
	.byte	85,137,229,80,104
	.long	KLsymbolGVKdW
	.byte	106,8,252,232
	.long	primitive_alloc_s1 - 4 - .
	.byte	131,196,12,201,195


	.global primitive_string_as_symbol
	.type primitive_string_as_symbol,@function
primitive_string_as_symbol:
	.byte	85,137,229,131,236,20,137,69,240,139,53
	.long	PoblistUcursor
	.byte	137,117,248,139,53
	.long	Poblist
	.byte	137,117,252,185,8,0,0,0,59,77,248,125,100,139,117,252
	.byte	139,4,49,139,112,4,137,117,236,139,117,240,139,126,4,139
	.byte	117,236,139,94,4,59,251,15,133,243,0,0,0,193,255,2
	.byte	131,239,1,131,255,0,124,62,139,117,240,15,182,84,62,8
	.byte	139,117,236,15,182,92,62,8,59,211,116,32,51,211,131,250
	.byte	32,15,133,201,0,0,0,131,203,32,131,251,97,15,140,189
	.byte	0,0,0,131,251,122,15,143,180,0,0,0,131,239,1,235
	.byte	194,184
	.long	KPfalseVKi
	.byte	61
	.long	KPfalseVKi
	.byte	15,133,157,0,0,0,139,125,240,87,104
	.long	KLsymbolGVKdW
	.byte	106,8,252,232
	.long	primitive_alloc_s1 - 4 - .
	.byte	131,196,12,137,69,240,139,61
	.long	PoblistUsize
	.byte	139,53
	.long	PoblistUcursor
	.byte	137,117,252,139,29
	.long	Poblist
	.byte	57,125,252,124,83,137,254,131,238,8,137,117,248,129,199,0
	.byte	4,0,0,141,115,8,137,117,244,137,251,131,235,8,193,251
	.byte	2,104
	.long	KPunboundVKi
	.byte	106,1,83,104
	.long	KLsimple_object_vectorGVKdW
	.byte	87,252,232
	.long	primitive_alloc_rf - 4 - .
	.byte	131,196,20,137,5
	.long	Poblist
	.byte	137,195,131,192,8,137,61
	.long	PoblistUsize
	.byte	193,125,248,2,139,77,248,139,117,244,137,199,252,243,165,139
	.byte	77,240,139,117,252,137,12,51,131,69,252,4,139,117,252,137
	.byte	53
	.long	PoblistUcursor
	.byte	139,69,240,201,195,131,193,4,233,224,254,255,255


	.global primitive_resolve_symbol
	.type primitive_resolve_symbol,@function
primitive_resolve_symbol:
	.byte	85,137,229,131,236,24,139,112,4,137,117,232,139,53
	.long	PoblistUcursor
	.byte	137,117,244,139,53
	.long	Poblist
	.byte	137,117,248,199,69,252,8,0,0,0,139,117,252,59,117,244
	.byte	125,102,139,117,248,3,117,252,139,14,139,113,4,137,117,236
	.byte	139,117,232,139,126,4,139,117,236,139,94,4,59,251,15,133
	.byte	229,0,0,0,193,255,2,131,239,1,131,255,0,124,62,139
	.byte	117,232,15,182,84,62,8,139,117,236,15,182,92,62,8,59
	.byte	211,116,32,51,211,131,250,32,15,133,187,0,0,0,131,203
	.byte	32,131,251,97,15,140,175,0,0,0,131,251,122,15,143,166
	.byte	0,0,0,131,239,1,235,194,185
	.long	KPfalseVKi
	.byte	129,249
	.long	KPfalseVKi
	.byte	15,133,140,0,0,0,139,21
	.long	PoblistUsize
	.byte	139,61
	.long	PoblistUcursor
	.byte	139,29
	.long	Poblist
	.byte	59,250,124,104,137,69,236,137,85,252,137,125,240,139,117,252
	.byte	131,238,8,137,117,248,129,69,252,0,4,0,0,141,115,8
	.byte	137,117,244,139,125,252,131,239,8,193,255,2,104
	.long	KPunboundVKi
	.byte	106,1,87,104
	.long	KLsimple_object_vectorGVKdW
	.byte	255,117,252,252,232
	.long	primitive_alloc_rf - 4 - .
	.byte	131,196,20,137,5
	.long	Poblist
	.byte	137,195,141,120,8,139,117,252,137,53
	.long	PoblistUsize
	.byte	193,125,248,2,139,77,248,139,117,244,252,243,165,139,69,236
	.byte	139,125,240,137,4,59,131,199,4,137,61
	.long	PoblistUcursor
	.byte	137,193,137,200,201,195,131,69,252,4,233,232,254,255,255


	.global dylan_stack_overflow_handler
	.type dylan_stack_overflow_handler,@function
dylan_stack_overflow_handler:
	.byte	85,137,229,156,83,86,87,184
	.long	. + 28
	.byte	232
	.long	primitive_build_unwind_protect_frame - 4 - .
	.byte	232
	.long	Kstack_overflow_errorVKiI - 4 - .
	.byte	232
	.long	primitive_unwind_protect_cleanup - 4 - .
	.byte	141,101,240,95,94,91,157,93,195,195


	.global dylan_integer_overflow_handler
	.type dylan_integer_overflow_handler,@function
dylan_integer_overflow_handler:
	.byte	85,137,229,156,83,86,87,232
	.long	Kmachine_word_overflowVKmI - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global dylan_integer_divide_0_handler
	.type dylan_integer_divide_0_handler,@function
dylan_integer_divide_0_handler:
	.byte	85,137,229,156,83,86,87,232
	.long	Kinteger_divide_by_0VKeI - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global dylan_float_divide_0_handler
	.type dylan_float_divide_0_handler,@function
dylan_float_divide_0_handler:
	.byte	85,137,229,156,83,86,87,219,226,232
	.long	Kfloat_divide_by_0VKeI - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global dylan_float_overflow_handler
	.type dylan_float_overflow_handler,@function
dylan_float_overflow_handler:
	.byte	85,137,229,156,83,86,87,219,226,232
	.long	Kfloat_overflowVKeI - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global dylan_float_underflow_handler
	.type dylan_float_underflow_handler,@function
dylan_float_underflow_handler:
	.byte	85,137,229,156,83,86,87,219,226,232
	.long	Kfloat_underflowVKeI - 4 - .
	.byte	141,101,240,95,94,91,157,93,195


	.global primitive_float_class
	.type primitive_float_class,@function
primitive_float_class:
	.byte	217,229,106,0,221,60,36,129,36,36,0,69,0,0,91,51
	.byte	192,139,184
	.long	Kfloating_point_classes
	.byte	59,223,116,5,131,192,8,235,241,131,192,4,139,128
	.long	Kfloating_point_classes
	.byte	195
