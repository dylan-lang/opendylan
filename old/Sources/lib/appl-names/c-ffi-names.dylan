module: c-ffi-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("c_ffi");
module-namer("c_ffi");

constant-namer("_L_c_value_G_", <c-value>);

constant-namer("size_of", size-of);

constant-namer("alignment_of", alignment-of);

constant-namer("pointer_type", pointer-type);

constant-namer("_L_c_signed_char_G_", <c-signed-char>);

constant-namer("_L_c_unsigned_char_G_", <c-unsigned-char>);

constant-namer("_L_c_char_G_", <c-char>);

constant-namer("_L_c_signed_short_G_", <c-signed-short>);

constant-namer("_L_c_unsigned_short_G_", <c-unsigned-short>);

constant-namer("_L_c_short_G_", <c-short>);

constant-namer("_L_c_signed_int_G_", <c-signed-int>);

constant-namer("_L_c_unsigned_int_G_", <c-unsigned-int>);

constant-namer("_L_c_int_G_", <c-int>);

constant-namer("_L_c_signed_long_G_", <c-signed-long>);

constant-namer("_L_c_unsigned_long_G_", <c-unsigned-long>);

constant-namer("_L_c_long_G_", <c-long>);

constant-namer("_L_c_float_G_", <c-float>);

constant-namer("_L_c_double_G_", <c-double>);

constant-namer("_L_c_void_G_", <c-void>);

constant-namer("_L_c_struct_G_", <c-struct>);

constant-namer("_L_c_union_G_", <c-union>);

constant-namer("_L_c_pointer_G_", <c-pointer>);

constant-namer("referenced_type", referenced-type);

constant-namer("null_pointer", null-pointer);

constant-namer("null_pointer_Q_", null-pointer?);

constant-namer("_L_c_typed_pointer_G_", <c-typed-pointer>);

constant-namer("pointer_value", pointer-value);

constant-namer("pointer_value_setter", pointer-value-setter);

constant-namer("default_allocator", default-allocator);

constant-namer("_L_c_untyped_pointer_G_", <c-untyped-pointer>);

constant-namer("canonical_type", canonical-type);

constant-namer("_L_c_array_G_", <c-array>);

//constant-namer("array_type", array-type);

constant-namer("_L_c_function_G_", <c-function>);

constant-namer("import_value", import-value);

constant-namer("export_value", export-value);

constant-namer("class_for_map", class-for-map);

constant-namer("_L_c_string_G_", <c-string>);

constant-namer("_L_c_character_G_", <c-character>);

constant-namer("_L_c_boolean_G_", <c-boolean>);

constant-namer("_L_c_dylan_object_G_", <c-dylan-object>);

constant-namer("pointer_pointer", pointer-pointer);

constant-namer("pointer_pointer_setter", pointer-pointer-setter);

constant-namer("copy_wrapper_properties", copy-wrapper-properties);

constant-namer("referenced_type_setter", referenced-type-setter);

constant-namer("pointer_type_setter", pointer-type-setter);

constant-namer("copy_wrappers_properties", copy-wrappers-properties);

constant-namer("pointer_element_count", pointer-element-count);

constant-namer("low_level_value_at", low-level-value-at);

constant-namer("low_level_value_at_setter", low-level-value-at-setter);
