module: simple-streams-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("simple_streams");
module-namer("simple_streams");

constant-namer("_L_stream_G_", <stream>);

constant-namer("_L_file_stream_G_", <file-stream>);

constant-namer("stream_handle", stream-handle);

constant-namer("make_stream", make-stream);

variable-namer("_T_standard_output_T_", *standard-output*);

variable-namer("_T_standard_input_T_", *standard-input*);

variable-namer("_T_stream_T_", *stream*);

constant-namer("standard_output", standard-output);

constant-namer("output", output);

constant-namer("force_output", force-output);

constant-namer("input", input);

constant-namer("undo_input", undo-input);

constant-namer("clear_input", clear-input);

constant-namer("_L_collection_stream_G_", <collection-stream>);

constant-namer("stream_collection", stream-collection);

constant-namer("stream_collection_setter", stream-collection-setter);

constant-namer("print", print);

constant-namer("format", format);
