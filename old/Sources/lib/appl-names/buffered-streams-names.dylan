module: buffered-streams-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


module-namer("buffered_streams");

constant-namer("_L_file_error_G_", <file-error>);

constant-namer("_L_file_exists_error_G_", <file-exists-error>);

constant-namer("_L_file_does_not_exist_error_G_", <file-does-not-exist-error>);

constant-namer("_L_invalid_file_permissions_error_G_", <invalid-file-permissions-error>);

constant-namer("_L_byte_G_", <byte>);

constant-namer("_L_buffer_index_G_", <buffer-index>);

constant-namer("_L_buffer_G_", <buffer>);

constant-namer("buffer_subsequence", buffer-subsequence);

constant-namer("copy_into_buffer_E_", copy-into-buffer!);

constant-namer("copy_from_buffer_E_", copy-from-buffer!);

// constant-namer("_L_stream_buffer_G_", <stream-buffer>);

constant-namer("next_input_buffer", next-input-buffer);

constant-namer("next_output_buffer", next-output-buffer);

constant-namer("stream_get_input_buffer", stream-get-input-buffer);

constant-namer("stream_next_input_buffer", stream-next-input-buffer);

constant-namer("stream_release_input_buffer", stream-release-input-buffer);

constant-namer("stream_get_output_buffer", stream-get-output-buffer);

constant-namer("stream_send_output_buffer", stream-send-output-buffer);

constant-namer("stream_next_output_buffer", stream-next-output-buffer);

constant-namer("stream_release_output_buffer", stream-release-output-buffer);

constant-namer("_L_typed_stream_G_", <typed-stream>);

constant-namer("sequence_type", sequence-type);

constant-namer("coerce_to_element", coerce-to-element);

constant-namer("coerce_from_element", coerce-from-element);

constant-namer("coerce_to_sequence", coerce-to-sequence);

constant-namer("coerce_from_sequence", coerce-from-sequence);

constant-namer("_L_external_stream_accessor_G_", <external-stream-accessor>);

constant-namer("_L_external_stream_G_", <external-stream>);

constant-namer("accessor", accessor);

constant-namer("accessor_open", accessor-open);

constant-namer("accessor_open_Q_", accessor-open?);

constant-namer("accessor_close", accessor-close);

constant-namer("accessor_newline_sequence", accessor-newline-sequence);

constant-namer("accessor_read_into_E_", accessor-read-into!);

constant-namer("accessor_write_from", accessor-write-from);

constant-namer("platform_accessor_class", platform-accessor-class);

variable-namer("_T_open_accessors_T_", *open-accessors*);

constant-namer("preferred_buffer_size", preferred-buffer-size);

constant-namer("file_size", file-size);

variable-namer("_T_debug_print_T_", *debug-print*);

// constant-namer("buffer", buffer);

constant-namer("buffer_size", buffer-size);

// constant-namer("buffer_offset", buffer-offset);

// constant-namer("buffer_offset_setter", buffer-offset-setter);

constant-namer("buffer_position", buffer-position);

constant-namer("buffer_position_setter", buffer-position-setter);

constant-namer("buffer_end", buffer-end);

constant-namer("buffer_end_setter", buffer-end-setter);

constant-namer("buffer_held_Q_", buffer-held?);

constant-namer("_L_buffered_stream_G_", <buffered-stream>);

constant-namer("input_buffer", input-buffer);

constant-namer("input_buffer_setter", input-buffer-setter);

constant-namer("output_buffer", output-buffer);

constant-namer("output_buffer_setter", output-buffer-setter);

constant-namer("output_buffer_dirty_Q_", output-buffer-dirty?);

constant-namer("output_buffer_dirty_Q__setter", output-buffer-dirty?-setter);

constant-namer("buffer_held_error", buffer-held-error);

constant-namer("buffer_held_Q__setter", buffer-held?-setter);
