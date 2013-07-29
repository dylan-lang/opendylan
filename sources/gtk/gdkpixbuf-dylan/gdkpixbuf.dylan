module: gdkpixbuf
synopsis: generated bindings for the GdkPixbuf library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;

define constant $GDK-COLORSPACE-RGB = 0;
define constant <GdkColorspace> = <C-int>;
define C-pointer-type <GdkColorspace*> => <GdkColorspace>;

define constant $GDK-INTERP-NEAREST = 0;
define constant $GDK-INTERP-TILES = 1;
define constant $GDK-INTERP-BILINEAR = 2;
define constant $GDK-INTERP-HYPER = 3;
define constant <GdkInterpType> = <C-int>;
define C-pointer-type <GdkInterpType*> => <GdkInterpType>;

define constant $PIXBUF-FEATURES-H = 1;

define constant $PIXBUF-MAGIC-NUMBER = 1197763408;

define constant $PIXBUF-MAJOR = 2;

define constant $PIXBUF-MICRO = 2;

define constant $PIXBUF-MINOR = 28;

define constant $PIXBUF-VERSION = "2.28.2";

define constant $PIXDATA-HEADER-LENGTH = 24;

define open C-subtype <GdkPixbuf> (<GObject>)
end C-subtype;

define C-pointer-type <GdkPixbuf*> => <GdkPixbuf>;

define property-getter pixbuf-bits-per-sample :: <C-signed-int> on <GdkPixbuf> end;
define property-setter pixbuf-bits-per-sample :: <C-signed-int> on <GdkPixbuf> end;
define property-getter pixbuf-colorspace :: <GdkColorspace> on <GdkPixbuf> end;
define property-setter pixbuf-colorspace :: <GdkColorspace> on <GdkPixbuf> end;
define property-getter pixbuf-has-alpha :: <C-boolean> on <GdkPixbuf> end;
define property-setter pixbuf-has-alpha :: <C-boolean> on <GdkPixbuf> end;
define property-getter pixbuf-height :: <C-signed-int> on <GdkPixbuf> end;
define property-setter pixbuf-height :: <C-signed-int> on <GdkPixbuf> end;
define property-getter pixbuf-n-channels :: <C-signed-int> on <GdkPixbuf> end;
define property-setter pixbuf-n-channels :: <C-signed-int> on <GdkPixbuf> end;
define property-getter pixbuf-pixels :: <C-void*> on <GdkPixbuf> end;
define property-setter pixbuf-pixels :: <C-void*> on <GdkPixbuf> end;
define property-getter pixbuf-rowstride :: <C-signed-int> on <GdkPixbuf> end;
define property-setter pixbuf-rowstride :: <C-signed-int> on <GdkPixbuf> end;
define property-getter pixbuf-width :: <C-signed-int> on <GdkPixbuf> end;
define property-setter pixbuf-width :: <C-signed-int> on <GdkPixbuf> end;
define C-function gdk-pixbuf-new
  input parameter colorspace_ :: <GdkColorspace>;
  input parameter has_alpha_ :: <C-boolean>;
  input parameter bits_per_sample_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new";
end;

define C-function gdk-pixbuf-new-from-data
  input parameter data_ :: <C-unsigned-char*>;
  input parameter colorspace_ :: <GdkColorspace>;
  input parameter has_alpha_ :: <C-boolean>;
  input parameter bits_per_sample_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter rowstride_ :: <C-signed-int>;
  input parameter destroy_fn_ :: <C-function-pointer>;
  input parameter destroy_fn_data_ :: <C-void*>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_data";
end;

define C-function gdk-pixbuf-new-from-file
  input parameter filename_ :: <C-string>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_file";
end;

define C-function gdk-pixbuf-new-from-file-at-scale
  input parameter filename_ :: <C-string>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter preserve_aspect_ratio_ :: <C-boolean>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_file_at_scale";
end;

define C-function gdk-pixbuf-new-from-file-at-size
  input parameter filename_ :: <C-string>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_file_at_size";
end;

define C-function gdk-pixbuf-new-from-inline
  input parameter data_length_ :: <C-signed-int>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter copy_pixels_ :: <C-boolean>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_inline";
end;

define C-function gdk-pixbuf-new-from-resource
  input parameter resource_path_ :: <C-string>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_resource";
end;

define C-function gdk-pixbuf-new-from-resource-at-scale
  input parameter resource_path_ :: <C-string>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter preserve_aspect_ratio_ :: <C-boolean>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_resource_at_scale";
end;

define C-function gdk-pixbuf-new-from-stream
  input parameter stream_ :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_stream";
end;

define C-function gdk-pixbuf-new-from-stream-at-scale
  input parameter stream_ :: <GInputStream>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter preserve_aspect_ratio_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_stream_at_scale";
end;

define C-function gdk-pixbuf-new-from-stream-finish
  input parameter async_result_ :: <GAsyncResult>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_stream_finish";
end;

define C-function gdk-pixbuf-new-from-xpm-data
  input parameter data_ :: <C-string*>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_from_xpm_data";
end;

define C-function gdk-pixbuf-from-pixdata
  input parameter pixdata_ :: <GdkPixdata>;
  input parameter copy_pixels_ :: <C-boolean>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_from_pixdata";
end;

define C-function gdk-pixbuf-get-file-info
  input parameter filename_ :: <C-string>;
  output parameter width_ :: <C-signed-int*>;
  output parameter height_ :: <C-signed-int*>;
  result res :: <GdkPixbufFormat>;
  c-name: "gdk_pixbuf_get_file_info";
end;

define C-function gdk-pixbuf-get-formats
  result res :: <GSList>;
  c-name: "gdk_pixbuf_get_formats";
end;

define C-function gdk-pixbuf-gettext
  input parameter msgid_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gdk_pixbuf_gettext";
end;

define C-function gdk-pixbuf-new-from-stream-async
  input parameter stream_ :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gdk_pixbuf_new_from_stream_async";
end;

define C-function gdk-pixbuf-new-from-stream-at-scale-async
  input parameter stream_ :: <GInputStream>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter preserve_aspect_ratio_ :: <C-boolean>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gdk_pixbuf_new_from_stream_at_scale_async";
end;

define C-function gdk-pixbuf-save-to-stream-finish
  input parameter async_result_ :: <GAsyncResult>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_save_to_stream_finish";
end;

define C-function gdk-pixbuf-add-alpha
  input parameter self :: <GdkPixbuf>;
  input parameter substitute_color_ :: <C-boolean>;
  input parameter r_ :: <C-unsigned-char>;
  input parameter g_ :: <C-unsigned-char>;
  input parameter b_ :: <C-unsigned-char>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_add_alpha";
end;

define C-function gdk-pixbuf-apply-embedded-orientation
  input parameter self :: <GdkPixbuf>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_apply_embedded_orientation";
end;

define C-function gdk-pixbuf-composite
  input parameter self :: <GdkPixbuf>;
  input parameter dest_ :: <GdkPixbuf>;
  input parameter dest_x_ :: <C-signed-int>;
  input parameter dest_y_ :: <C-signed-int>;
  input parameter dest_width_ :: <C-signed-int>;
  input parameter dest_height_ :: <C-signed-int>;
  input parameter offset_x_ :: <C-double>;
  input parameter offset_y_ :: <C-double>;
  input parameter scale_x_ :: <C-double>;
  input parameter scale_y_ :: <C-double>;
  input parameter interp_type_ :: <GdkInterpType>;
  input parameter overall_alpha_ :: <C-signed-int>;
  c-name: "gdk_pixbuf_composite";
end;

define C-function gdk-pixbuf-composite-color
  input parameter self :: <GdkPixbuf>;
  input parameter dest_ :: <GdkPixbuf>;
  input parameter dest_x_ :: <C-signed-int>;
  input parameter dest_y_ :: <C-signed-int>;
  input parameter dest_width_ :: <C-signed-int>;
  input parameter dest_height_ :: <C-signed-int>;
  input parameter offset_x_ :: <C-double>;
  input parameter offset_y_ :: <C-double>;
  input parameter scale_x_ :: <C-double>;
  input parameter scale_y_ :: <C-double>;
  input parameter interp_type_ :: <GdkInterpType>;
  input parameter overall_alpha_ :: <C-signed-int>;
  input parameter check_x_ :: <C-signed-int>;
  input parameter check_y_ :: <C-signed-int>;
  input parameter check_size_ :: <C-signed-int>;
  input parameter color1_ :: <C-unsigned-int>;
  input parameter color2_ :: <C-unsigned-int>;
  c-name: "gdk_pixbuf_composite_color";
end;

define C-function gdk-pixbuf-composite-color-simple
  input parameter self :: <GdkPixbuf>;
  input parameter dest_width_ :: <C-signed-int>;
  input parameter dest_height_ :: <C-signed-int>;
  input parameter interp_type_ :: <GdkInterpType>;
  input parameter overall_alpha_ :: <C-signed-int>;
  input parameter check_size_ :: <C-signed-int>;
  input parameter color1_ :: <C-unsigned-int>;
  input parameter color2_ :: <C-unsigned-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_composite_color_simple";
end;

define C-function gdk-pixbuf-copy
  input parameter self :: <GdkPixbuf>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_copy";
end;

define C-function gdk-pixbuf-copy-area
  input parameter self :: <GdkPixbuf>;
  input parameter src_x_ :: <C-signed-int>;
  input parameter src_y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter dest_pixbuf_ :: <GdkPixbuf>;
  input parameter dest_x_ :: <C-signed-int>;
  input parameter dest_y_ :: <C-signed-int>;
  c-name: "gdk_pixbuf_copy_area";
end;

define C-function gdk-pixbuf-fill
  input parameter self :: <GdkPixbuf>;
  input parameter pixel_ :: <C-unsigned-int>;
  c-name: "gdk_pixbuf_fill";
end;

define C-function gdk-pixbuf-flip
  input parameter self :: <GdkPixbuf>;
  input parameter horizontal_ :: <C-boolean>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_flip";
end;

define C-function gdk-pixbuf-get-bits-per-sample
  input parameter self :: <GdkPixbuf>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_get_bits_per_sample";
end;

define C-function gdk-pixbuf-get-byte-length
  input parameter self :: <GdkPixbuf>;
  result res :: <C-unsigned-long>;
  c-name: "gdk_pixbuf_get_byte_length";
end;

define C-function gdk-pixbuf-get-colorspace
  input parameter self :: <GdkPixbuf>;
  result res :: <GdkColorspace>;
  c-name: "gdk_pixbuf_get_colorspace";
end;

define C-function gdk-pixbuf-get-has-alpha
  input parameter self :: <GdkPixbuf>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_get_has_alpha";
end;

define C-function gdk-pixbuf-get-height
  input parameter self :: <GdkPixbuf>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_get_height";
end;

define C-function gdk-pixbuf-get-n-channels
  input parameter self :: <GdkPixbuf>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_get_n_channels";
end;

define C-function gdk-pixbuf-get-option
  input parameter self :: <GdkPixbuf>;
  input parameter key_ :: <C-string>;
  result res :: <C-string>;
  c-name: "gdk_pixbuf_get_option";
end;

define C-function gdk-pixbuf-get-pixels-with-length
  input parameter self :: <GdkPixbuf>;
  output parameter length_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*>;
  c-name: "gdk_pixbuf_get_pixels_with_length";
end;

define C-function gdk-pixbuf-get-rowstride
  input parameter self :: <GdkPixbuf>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_get_rowstride";
end;

define C-function gdk-pixbuf-get-width
  input parameter self :: <GdkPixbuf>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_get_width";
end;

define C-function gdk-pixbuf-new-subpixbuf
  input parameter self :: <GdkPixbuf>;
  input parameter src_x_ :: <C-signed-int>;
  input parameter src_y_ :: <C-signed-int>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_new_subpixbuf";
end;

define C-function gdk-pixbuf-rotate-simple
  input parameter self :: <GdkPixbuf>;
  input parameter angle_ :: <GdkPixbufRotation>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_rotate_simple";
end;

define C-function gdk-pixbuf-saturate-and-pixelate
  input parameter self :: <GdkPixbuf>;
  input parameter dest_ :: <GdkPixbuf>;
  input parameter saturation_ :: <C-float>;
  input parameter pixelate_ :: <C-boolean>;
  c-name: "gdk_pixbuf_saturate_and_pixelate";
end;

define C-function gdk-pixbuf-save-to-bufferv
  input parameter self :: <GdkPixbuf>;
  output parameter buffer_ :: <C-unsigned-char*>;
  output parameter buffer_size_ :: <C-unsigned-long*>;
  input parameter type_ :: <C-string>;
  input parameter option_keys_ :: <C-string*>;
  input parameter option_values_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_save_to_bufferv";
end;

define C-function gdk-pixbuf-save-to-callbackv
  input parameter self :: <GdkPixbuf>;
  input parameter save_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter type_ :: <C-string>;
  input parameter option_keys_ :: <C-string*>;
  input parameter option_values_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_save_to_callbackv";
end;

define C-function gdk-pixbuf-savev
  input parameter self :: <GdkPixbuf>;
  input parameter filename_ :: <C-string>;
  input parameter type_ :: <C-string>;
  input parameter option_keys_ :: <C-string*>;
  input parameter option_values_ :: <C-string*>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_savev";
end;

define C-function gdk-pixbuf-scale
  input parameter self :: <GdkPixbuf>;
  input parameter dest_ :: <GdkPixbuf>;
  input parameter dest_x_ :: <C-signed-int>;
  input parameter dest_y_ :: <C-signed-int>;
  input parameter dest_width_ :: <C-signed-int>;
  input parameter dest_height_ :: <C-signed-int>;
  input parameter offset_x_ :: <C-double>;
  input parameter offset_y_ :: <C-double>;
  input parameter scale_x_ :: <C-double>;
  input parameter scale_y_ :: <C-double>;
  input parameter interp_type_ :: <GdkInterpType>;
  c-name: "gdk_pixbuf_scale";
end;

define C-function gdk-pixbuf-scale-simple
  input parameter self :: <GdkPixbuf>;
  input parameter dest_width_ :: <C-signed-int>;
  input parameter dest_height_ :: <C-signed-int>;
  input parameter interp_type_ :: <GdkInterpType>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_scale_simple";
end;

define constant $GDK-PIXBUF-ALPHA-BILEVEL = 0;
define constant $GDK-PIXBUF-ALPHA-FULL = 1;
define constant <GdkPixbufAlphaMode> = <C-int>;
define C-pointer-type <GdkPixbufAlphaMode*> => <GdkPixbufAlphaMode>;

define open C-subtype <GdkPixbufAnimation> (<GObject>)
end C-subtype;

define C-pointer-type <GdkPixbufAnimation*> => <GdkPixbufAnimation>;

define C-function gdk-pixbuf-animation-new-from-file
  input parameter filename_ :: <C-string>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gdk_pixbuf_animation_new_from_file";
end;

define C-function gdk-pixbuf-animation-new-from-resource
  input parameter resource_path_ :: <C-string>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gdk_pixbuf_animation_new_from_resource";
end;

define C-function gdk-pixbuf-animation-new-from-stream
  input parameter stream_ :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gdk_pixbuf_animation_new_from_stream";
end;

define C-function gdk-pixbuf-animation-new-from-stream-finish
  input parameter async_result_ :: <GAsyncResult>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gdk_pixbuf_animation_new_from_stream_finish";
end;

define C-function gdk-pixbuf-animation-new-from-stream-async
  input parameter stream_ :: <GInputStream>;
  input parameter cancellable_ :: <GCancellable>;
  input parameter callback_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  c-name: "gdk_pixbuf_animation_new_from_stream_async";
end;

define C-function gdk-pixbuf-animation-get-height
  input parameter self :: <GdkPixbufAnimation>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_animation_get_height";
end;

define C-function gdk-pixbuf-animation-get-iter
  input parameter self :: <GdkPixbufAnimation>;
  input parameter start_time_ :: <GTimeVal>;
  result res :: <GdkPixbufAnimationIter>;
  c-name: "gdk_pixbuf_animation_get_iter";
end;

define C-function gdk-pixbuf-animation-get-static-image
  input parameter self :: <GdkPixbufAnimation>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_animation_get_static_image";
end;

define C-function gdk-pixbuf-animation-get-width
  input parameter self :: <GdkPixbufAnimation>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_animation_get_width";
end;

define C-function gdk-pixbuf-animation-is-static-image
  input parameter self :: <GdkPixbufAnimation>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_animation_is_static_image";
end;

define open C-subtype <GdkPixbufAnimationIter> (<GObject>)
end C-subtype;

define C-pointer-type <GdkPixbufAnimationIter*> => <GdkPixbufAnimationIter>;

define C-function gdk-pixbuf-animation-iter-advance
  input parameter self :: <GdkPixbufAnimationIter>;
  input parameter current_time_ :: <GTimeVal>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_animation_iter_advance";
end;

define C-function gdk-pixbuf-animation-iter-get-delay-time
  input parameter self :: <GdkPixbufAnimationIter>;
  result res :: <C-signed-int>;
  c-name: "gdk_pixbuf_animation_iter_get_delay_time";
end;

define C-function gdk-pixbuf-animation-iter-get-pixbuf
  input parameter self :: <GdkPixbufAnimationIter>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_animation_iter_get_pixbuf";
end;

define C-function gdk-pixbuf-animation-iter-on-currently-loading-frame
  input parameter self :: <GdkPixbufAnimationIter>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_animation_iter_on_currently_loading_frame";
end;

define constant $GDK-PIXBUF-ERROR-CORRUPT-IMAGE = 0;
define constant $GDK-PIXBUF-ERROR-INSUFFICIENT-MEMORY = 1;
define constant $GDK-PIXBUF-ERROR-BAD-OPTION = 2;
define constant $GDK-PIXBUF-ERROR-UNKNOWN-TYPE = 3;
define constant $GDK-PIXBUF-ERROR-UNSUPPORTED-OPERATION = 4;
define constant $GDK-PIXBUF-ERROR-FAILED = 5;
define constant <GdkPixbufError> = <C-int>;
define C-pointer-type <GdkPixbufError*> => <GdkPixbufError>;

define C-struct <_GdkPixbufFormat>
  pointer-type-name: <GdkPixbufFormat>;
end C-struct;

define C-function gdk-pixbuf-format-copy
  input parameter self :: <GdkPixbufFormat>;
  result res :: <GdkPixbufFormat>;
  c-name: "gdk_pixbuf_format_copy";
end;

define C-function gdk-pixbuf-format-free
  input parameter self :: <GdkPixbufFormat>;
  c-name: "gdk_pixbuf_format_free";
end;

define C-function gdk-pixbuf-format-get-description
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-string>;
  c-name: "gdk_pixbuf_format_get_description";
end;

define C-function gdk-pixbuf-format-get-extensions
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-string*>;
  c-name: "gdk_pixbuf_format_get_extensions";
end;

define C-function gdk-pixbuf-format-get-license
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-string>;
  c-name: "gdk_pixbuf_format_get_license";
end;

define C-function gdk-pixbuf-format-get-mime-types
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-string*>;
  c-name: "gdk_pixbuf_format_get_mime_types";
end;

define C-function gdk-pixbuf-format-get-name
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-string>;
  c-name: "gdk_pixbuf_format_get_name";
end;

define C-function gdk-pixbuf-format-is-disabled
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_format_is_disabled";
end;

define C-function gdk-pixbuf-format-is-scalable
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_format_is_scalable";
end;

define C-function gdk-pixbuf-format-is-writable
  input parameter self :: <GdkPixbufFormat>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_format_is_writable";
end;

define C-function gdk-pixbuf-format-set-disabled
  input parameter self :: <GdkPixbufFormat>;
  input parameter disabled_ :: <C-boolean>;
  c-name: "gdk_pixbuf_format_set_disabled";
end;

define open C-subtype <GdkPixbufLoader> (<GObject>)
  constant slot gdkpixbufloader-parent-instance :: <GObject>;
  constant slot gdkpixbufloader-priv :: <C-void*>;
end C-subtype;

define C-pointer-type <GdkPixbufLoader*> => <GdkPixbufLoader>;

define C-function gdk-pixbuf-loader-new
  result res :: <GdkPixbufLoader>;
  c-name: "gdk_pixbuf_loader_new";
end;

define C-function gdk-pixbuf-loader-new-with-mime-type
  input parameter mime_type_ :: <C-string>;
  result res :: <GdkPixbufLoader>;
  c-name: "gdk_pixbuf_loader_new_with_mime_type";
end;

define C-function gdk-pixbuf-loader-new-with-type
  input parameter image_type_ :: <C-string>;
  result res :: <GdkPixbufLoader>;
  c-name: "gdk_pixbuf_loader_new_with_type";
end;

define C-function gdk-pixbuf-loader-close
  input parameter self :: <GdkPixbufLoader>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_loader_close";
end;

define C-function gdk-pixbuf-loader-get-animation
  input parameter self :: <GdkPixbufLoader>;
  result res :: <GdkPixbufAnimation>;
  c-name: "gdk_pixbuf_loader_get_animation";
end;

define C-function gdk-pixbuf-loader-get-format
  input parameter self :: <GdkPixbufLoader>;
  result res :: <GdkPixbufFormat>;
  c-name: "gdk_pixbuf_loader_get_format";
end;

define C-function gdk-pixbuf-loader-get-pixbuf
  input parameter self :: <GdkPixbufLoader>;
  result res :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_loader_get_pixbuf";
end;

define C-function gdk-pixbuf-loader-set-size
  input parameter self :: <GdkPixbufLoader>;
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  c-name: "gdk_pixbuf_loader_set_size";
end;

define C-function gdk-pixbuf-loader-write
  input parameter self :: <GdkPixbufLoader>;
  input parameter buf_ :: <C-unsigned-char*>;
  input parameter count_ :: <C-unsigned-long>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_loader_write";
end;

define C-struct <_GdkPixbufLoaderClass>
  constant slot gdkpixbufloaderclass-parent-class :: <GObjectClass>;
  constant slot gdkpixbufloaderclass-size-prepared :: <C-function-pointer>;
  constant slot gdkpixbufloaderclass-area-prepared :: <C-function-pointer>;
  constant slot gdkpixbufloaderclass-area-updated :: <C-function-pointer>;
  constant slot gdkpixbufloaderclass-closed :: <C-function-pointer>;
  pointer-type-name: <GdkPixbufLoaderClass>;
end C-struct;

define constant $GDK-PIXBUF-ROTATE-NONE = 0;
define constant $GDK-PIXBUF-ROTATE-COUNTERCLOCKWISE = 90;
define constant $GDK-PIXBUF-ROTATE-UPSIDEDOWN = 180;
define constant $GDK-PIXBUF-ROTATE-CLOCKWISE = 270;
define constant <GdkPixbufRotation> = <C-int>;
define C-pointer-type <GdkPixbufRotation*> => <GdkPixbufRotation>;

define open C-subtype <GdkPixbufSimpleAnim> (<GdkPixbufAnimation>)
end C-subtype;

define C-pointer-type <GdkPixbufSimpleAnim*> => <GdkPixbufSimpleAnim>;

define property-getter pixbufsimpleanim-loop :: <C-boolean> on <GdkPixbufSimpleAnim> end;
define property-setter pixbufsimpleanim-loop :: <C-boolean> on <GdkPixbufSimpleAnim> end;
define C-function gdk-pixbuf-simple-anim-new
  input parameter width_ :: <C-signed-int>;
  input parameter height_ :: <C-signed-int>;
  input parameter rate_ :: <C-float>;
  result res :: <GdkPixbufSimpleAnim>;
  c-name: "gdk_pixbuf_simple_anim_new";
end;

define C-function gdk-pixbuf-simple-anim-add-frame
  input parameter self :: <GdkPixbufSimpleAnim>;
  input parameter pixbuf_ :: <GdkPixbuf>;
  c-name: "gdk_pixbuf_simple_anim_add_frame";
end;

define C-function gdk-pixbuf-simple-anim-get-loop
  input parameter self :: <GdkPixbufSimpleAnim>;
  result res :: <C-boolean>;
  c-name: "gdk_pixbuf_simple_anim_get_loop";
end;

define C-function gdk-pixbuf-simple-anim-set-loop
  input parameter self :: <GdkPixbufSimpleAnim>;
  input parameter loop_ :: <C-boolean>;
  c-name: "gdk_pixbuf_simple_anim_set_loop";
end;

define C-struct <_GdkPixbufSimpleAnimClass>
  pointer-type-name: <GdkPixbufSimpleAnimClass>;
end C-struct;

define open C-subtype <GdkPixbufSimpleAnimIter> (<GdkPixbufAnimationIter>)
end C-subtype;

define C-pointer-type <GdkPixbufSimpleAnimIter*> => <GdkPixbufSimpleAnimIter>;

define C-struct <_GdkPixdata>
  slot gdkpixdata-magic :: <C-unsigned-int>;
  slot gdkpixdata-length :: <C-signed-int>;
  slot gdkpixdata-pixdata-type :: <C-unsigned-int>;
  slot gdkpixdata-rowstride :: <C-unsigned-int>;
  slot gdkpixdata-width :: <C-unsigned-int>;
  slot gdkpixdata-height :: <C-unsigned-int>;
  slot gdkpixdata-pixel-data :: <C-unsigned-char*>;
  pointer-type-name: <GdkPixdata>;
end C-struct;

define C-function gdk-pixdata-deserialize
  input parameter self :: <GdkPixdata>;
  input parameter stream_length_ :: <C-unsigned-int>;
  input parameter stream_ :: <C-unsigned-char*>;
  result res :: <C-boolean>;
  c-name: "gdk_pixdata_deserialize";
end;

define C-function gdk-pixdata-serialize
  input parameter self :: <GdkPixdata>;
  output parameter stream_length_p_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-char*>;
  c-name: "gdk_pixdata_serialize";
end;

define C-function gdk-pixdata-to-csource
  input parameter self :: <GdkPixdata>;
  input parameter name_ :: <C-string>;
  input parameter dump_type_ :: <GdkPixdataDumpType>;
  result res :: <GString>;
  c-name: "gdk_pixdata_to_csource";
end;

define constant $GDK-PIXDATA-DUMP-PIXDATA-STREAM = 0;
define constant $GDK-PIXDATA-DUMP-PIXDATA-STRUCT = 1;
define constant $GDK-PIXDATA-DUMP-MACROS = 2;
define constant $GDK-PIXDATA-DUMP-GTYPES = 0;
define constant $GDK-PIXDATA-DUMP-CTYPES = 256;
define constant $GDK-PIXDATA-DUMP-STATIC = 512;
define constant $GDK-PIXDATA-DUMP-CONST = 1024;
define constant $GDK-PIXDATA-DUMP-RLE-DECODER = 65536;
define constant <GdkPixdataDumpType> = <C-int>;
define C-pointer-type <GdkPixdataDumpType*> => <GdkPixdataDumpType>;

define constant $GDK-PIXDATA-COLOR-TYPE-RGB = 1;
define constant $GDK-PIXDATA-COLOR-TYPE-RGBA = 2;
define constant $GDK-PIXDATA-COLOR-TYPE-MASK = 255;
define constant $GDK-PIXDATA-SAMPLE-WIDTH-8 = 65536;
define constant $GDK-PIXDATA-SAMPLE-WIDTH-MASK = 983040;
define constant $GDK-PIXDATA-ENCODING-RAW = 16777216;
define constant $GDK-PIXDATA-ENCODING-RLE = 33554432;
define constant $GDK-PIXDATA-ENCODING-MASK = 251658240;
define constant <GdkPixdataType> = <C-int>;
define C-pointer-type <GdkPixdataType*> => <GdkPixdataType>;

define C-function gdk-pixbuf-error-quark
  result res :: <C-unsigned-int>;
  c-name: "gdk_pixbuf_error_quark";
end;

