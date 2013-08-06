module: glib
synopsis: generated bindings for the GLib library
copyright: See LICENSE file in this distribution.


define C-pointer-type <C-void**> => <C-void*>;
ignore(<C-void**>);

define C-pointer-type <GError*> => <GError>;
ignore(<GError*>);

define constant $ascii-dtostr-buf-size = 39;

define C-struct <_GArray>
  slot g-array-data :: <C-string>;
  slot g-array-len :: <C-unsigned-int>;
  pointer-type-name: <GArray>;
end C-struct;

define C-function g-array-free
  input parameter array_ :: <GArray>;
  input parameter free_segment_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "g_array_free";
end;

define C-function g-array-get-element-size
  input parameter array_ :: <GArray>;
  result res :: <C-unsigned-int>;
  c-name: "g_array_get_element_size";
end;

define C-function g-array-set-clear-func
  input parameter array_ :: <GArray>;
  input parameter clear_func_ :: <C-function-pointer>;
  c-name: "g_array_set_clear_func";
end;

define C-function g-array-unref
  input parameter array_ :: <GArray>;
  c-name: "g_array_unref";
end;

define constant $g-ascii-alnum = 1;
define constant $g-ascii-alpha = 2;
define constant $g-ascii-cntrl = 4;
define constant $g-ascii-digit = 8;
define constant $g-ascii-graph = 16;
define constant $g-ascii-lower = 32;
define constant $g-ascii-print = 64;
define constant $g-ascii-punct = 128;
define constant $g-ascii-space = 256;
define constant $g-ascii-upper = 512;
define constant $g-ascii-xdigit = 1024;
define constant <GAsciiType> = <C-int>;
define C-pointer-type <GAsciiType*> => <GAsciiType>;

define C-struct <_GAsyncQueue>
  pointer-type-name: <GAsyncQueue>;
end C-struct;

define C-function g-async-queue-length
  input parameter self :: <GAsyncQueue>;
  result res :: <C-signed-int>;
  c-name: "g_async_queue_length";
end;

define C-function g-async-queue-length-unlocked
  input parameter self :: <GAsyncQueue>;
  result res :: <C-signed-int>;
  c-name: "g_async_queue_length_unlocked";
end;

define C-function g-async-queue-lock
  input parameter self :: <GAsyncQueue>;
  c-name: "g_async_queue_lock";
end;

define C-function g-async-queue-push
  input parameter self :: <GAsyncQueue>;
  input parameter data_ :: <C-void*>;
  c-name: "g_async_queue_push";
end;

define C-function g-async-queue-push-unlocked
  input parameter self :: <GAsyncQueue>;
  input parameter data_ :: <C-void*>;
  c-name: "g_async_queue_push_unlocked";
end;

define C-function g-async-queue-ref-unlocked
  input parameter self :: <GAsyncQueue>;
  c-name: "g_async_queue_ref_unlocked";
end;

define C-function g-async-queue-unlock
  input parameter self :: <GAsyncQueue>;
  c-name: "g_async_queue_unlock";
end;

define C-function g-async-queue-unref
  input parameter self :: <GAsyncQueue>;
  c-name: "g_async_queue_unref";
end;

define C-function g-async-queue-unref-and-unlock
  input parameter self :: <GAsyncQueue>;
  c-name: "g_async_queue_unref_and_unlock";
end;

define constant $big-endian = 4321;

define C-struct <_GBookmarkFile>
  pointer-type-name: <GBookmarkFile>;
end C-struct;

define C-function g-bookmark-file-add-application
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter name_ :: <C-string>;
  input parameter exec_ :: <C-string>;
  c-name: "g_bookmark_file_add_application";
end;

define C-function g-bookmark-file-add-group
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter group_ :: <C-string>;
  c-name: "g_bookmark_file_add_group";
end;

define C-function g-bookmark-file-free
  input parameter self :: <GBookmarkFile>;
  c-name: "g_bookmark_file_free";
end;

define C-function g-bookmark-file-get-added
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_bookmark_file_get_added";
end;

define C-function g-bookmark-file-get-app-info
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter name_ :: <C-string>;
  input parameter exec_ :: <C-string>;
  input parameter count_ :: <C-unsigned-int*>;
  input parameter stamp_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_get_app_info";
end;

define C-function g-bookmark-file-get-description
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_bookmark_file_get_description";
end;

define C-function g-bookmark-file-get-icon
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter href_ :: <C-string>;
  input parameter mime_type_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_get_icon";
end;

define C-function g-bookmark-file-get-is-private
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_get_is_private";
end;

define C-function g-bookmark-file-get-mime-type
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_bookmark_file_get_mime_type";
end;

define C-function g-bookmark-file-get-modified
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_bookmark_file_get_modified";
end;

define C-function g-bookmark-file-get-size
  input parameter self :: <GBookmarkFile>;
  result res :: <C-signed-int>;
  c-name: "g_bookmark_file_get_size";
end;

define C-function g-bookmark-file-get-title
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_bookmark_file_get_title";
end;

define C-function g-bookmark-file-get-visited
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_bookmark_file_get_visited";
end;

define C-function g-bookmark-file-has-application
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_has_application";
end;

define C-function g-bookmark-file-has-group
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter group_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_has_group";
end;

define C-function g-bookmark-file-has-item
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_has_item";
end;

define C-function g-bookmark-file-load-from-data
  input parameter self :: <GBookmarkFile>;
  input parameter data_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_load_from_data";
end;

define C-function g-bookmark-file-load-from-data-dirs
  input parameter self :: <GBookmarkFile>;
  input parameter file_ :: <C-string>;
  input parameter full_path_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_load_from_data_dirs";
end;

define C-function g-bookmark-file-load-from-file
  input parameter self :: <GBookmarkFile>;
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_load_from_file";
end;

define C-function g-bookmark-file-move-item
  input parameter self :: <GBookmarkFile>;
  input parameter old_uri_ :: <C-string>;
  input parameter new_uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_move_item";
end;

define C-function g-bookmark-file-remove-application
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_remove_application";
end;

define C-function g-bookmark-file-remove-group
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter group_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_remove_group";
end;

define C-function g-bookmark-file-remove-item
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_remove_item";
end;

define C-function g-bookmark-file-set-added
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter added_ :: <C-signed-long>;
  c-name: "g_bookmark_file_set_added";
end;

define C-function g-bookmark-file-set-app-info
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter name_ :: <C-string>;
  input parameter exec_ :: <C-string>;
  input parameter count_ :: <C-signed-int>;
  input parameter stamp_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_set_app_info";
end;

define C-function g-bookmark-file-set-description
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter description_ :: <C-string>;
  c-name: "g_bookmark_file_set_description";
end;

define C-function g-bookmark-file-set-groups
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter groups_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_bookmark_file_set_groups";
end;

define C-function g-bookmark-file-set-icon
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter href_ :: <C-string>;
  input parameter mime_type_ :: <C-string>;
  c-name: "g_bookmark_file_set_icon";
end;

define C-function g-bookmark-file-set-is-private
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter is_private_ :: <C-boolean>;
  c-name: "g_bookmark_file_set_is_private";
end;

define C-function g-bookmark-file-set-mime-type
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter mime_type_ :: <C-string>;
  c-name: "g_bookmark_file_set_mime_type";
end;

define C-function g-bookmark-file-set-modified
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter modified_ :: <C-signed-long>;
  c-name: "g_bookmark_file_set_modified";
end;

define C-function g-bookmark-file-set-title
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter title_ :: <C-string>;
  c-name: "g_bookmark_file_set_title";
end;

define C-function g-bookmark-file-set-visited
  input parameter self :: <GBookmarkFile>;
  input parameter uri_ :: <C-string>;
  input parameter visited_ :: <C-signed-long>;
  c-name: "g_bookmark_file_set_visited";
end;

define C-function g-bookmark-file-to-data
  input parameter self :: <GBookmarkFile>;
  input parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_bookmark_file_to_data";
end;

define C-function g-bookmark-file-to-file
  input parameter self :: <GBookmarkFile>;
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_bookmark_file_to_file";
end;

define C-function g-bookmark-file-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_bookmark_file_error_quark";
end;

define constant $g-bookmark-file-error-invalid-uri = 0;
define constant $g-bookmark-file-error-invalid-value = 1;
define constant $g-bookmark-file-error-app-not-registered = 2;
define constant $g-bookmark-file-error-uri-not-found = 3;
define constant $g-bookmark-file-error-read = 4;
define constant $g-bookmark-file-error-unknown-encoding = 5;
define constant $g-bookmark-file-error-write = 6;
define constant $g-bookmark-file-error-file-not-found = 7;
define constant <GBookmarkFileError> = <C-int>;
define C-pointer-type <GBookmarkFileError*> => <GBookmarkFileError>;

define C-struct <_GByteArray>
  slot g-byte-array-data :: <C-unsigned-char*>;
  slot g-byte-array-len :: <C-unsigned-int>;
  pointer-type-name: <GByteArray>;
end C-struct;

define C-function g-byte-array-free
  input parameter array_ :: <GByteArray>;
  input parameter free_segment_ :: <C-boolean>;
  result res :: <C-unsigned-char*>;
  c-name: "g_byte_array_free";
end;

define C-function g-byte-array-free-to-bytes
  input parameter array_ :: <GByteArray>;
  result res :: <GBytes>;
  c-name: "g_byte_array_free_to_bytes";
end;

define C-function g-byte-array-new-take
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-unsigned-long>;
  result res :: <GByteArray>;
  c-name: "g_byte_array_new_take";
end;

define C-function g-byte-array-unref
  input parameter array_ :: <GByteArray>;
  c-name: "g_byte_array_unref";
end;

define C-struct <_GBytes>
  pointer-type-name: <GBytes>;
end C-struct;

define C-function g-bytes-new
  input parameter data_ :: <C-void*>;
  input parameter size_ :: <C-unsigned-long>;
  result res :: <GBytes>;
  c-name: "g_bytes_new";
end;

define C-function g-bytes-new-static
  input parameter data_ :: <C-void*>;
  input parameter size_ :: <C-unsigned-long>;
  result res :: <GBytes>;
  c-name: "g_bytes_new_static";
end;

define C-function g-bytes-new-take
  input parameter data_ :: <C-void*>;
  input parameter size_ :: <C-unsigned-long>;
  result res :: <GBytes>;
  c-name: "g_bytes_new_take";
end;

define C-function g-bytes-new-with-free-func
  input parameter data_ :: <C-void*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter free_func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  result res :: <GBytes>;
  c-name: "g_bytes_new_with_free_func";
end;

define C-function g-bytes-compare
  input parameter self :: <GBytes>;
  input parameter bytes2_ :: <GBytes>;
  result res :: <C-signed-int>;
  c-name: "g_bytes_compare";
end;

define C-function g-bytes-equal
  input parameter self :: <GBytes>;
  input parameter bytes2_ :: <GBytes>;
  result res :: <C-boolean>;
  c-name: "g_bytes_equal";
end;

define C-function g-bytes-get-size
  input parameter self :: <GBytes>;
  result res :: <C-unsigned-long>;
  c-name: "g_bytes_get_size";
end;

define C-function g-bytes-hash
  input parameter self :: <GBytes>;
  result res :: <C-unsigned-int>;
  c-name: "g_bytes_hash";
end;

define C-function g-bytes-new-from-bytes
  input parameter self :: <GBytes>;
  input parameter offset_ :: <C-unsigned-long>;
  input parameter length_ :: <C-unsigned-long>;
  result res :: <GBytes>;
  c-name: "g_bytes_new_from_bytes";
end;

define C-function g-bytes-ref
  input parameter self :: <GBytes>;
  result res :: <GBytes>;
  c-name: "g_bytes_ref";
end;

define C-function g-bytes-unref
  input parameter self :: <GBytes>;
  c-name: "g_bytes_unref";
end;

define C-function g-bytes-unref-to-array
  input parameter self :: <GBytes>;
  result res :: <GByteArray>;
  c-name: "g_bytes_unref_to_array";
end;

define C-function g-bytes-unref-to-data
  input parameter self :: <GBytes>;
  input parameter size_ :: <C-unsigned-long*>;
  result res :: <C-void*>;
  c-name: "g_bytes_unref_to_data";
end;

define constant $can-inline = 1;

define constant $cset-a-2-z = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

define constant $cset-digits = "0123456789";

define C-struct <_GChecksum>
  pointer-type-name: <GChecksum>;
end C-struct;

define C-function g-checksum-free
  input parameter self :: <GChecksum>;
  c-name: "g_checksum_free";
end;

define C-function g-checksum-get-digest
  input parameter self :: <GChecksum>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter digest_len_ :: <C-unsigned-long*>;
  c-name: "g_checksum_get_digest";
end;

define C-function g-checksum-get-string
  input parameter self :: <GChecksum>;
  result res :: <C-string>;
  c-name: "g_checksum_get_string";
end;

define C-function g-checksum-reset
  input parameter self :: <GChecksum>;
  c-name: "g_checksum_reset";
end;

define C-function g-checksum-update
  input parameter self :: <GChecksum>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  c-name: "g_checksum_update";
end;

define C-function g-checksum-type-get-length
  input parameter checksum_type_ :: <GChecksumType>;
  result res :: <C-signed-long>;
  c-name: "g_checksum_type_get_length";
end;

define constant $g-checksum-md5 = 0;
define constant $g-checksum-sha1 = 1;
define constant $g-checksum-sha256 = 2;
define constant <GChecksumType> = <C-int>;
define C-pointer-type <GChecksumType*> => <GChecksumType>;

define C-struct <_GCond>
  constant slot g-cond-p :: <C-void*>;
  constant slot g-cond-i :: <C-unsigned-int*>;
  pointer-type-name: <GCond>;
end C-struct;

define C-function g-cond-broadcast
  input parameter self :: <GCond>;
  c-name: "g_cond_broadcast";
end;

define C-function g-cond-clear
  input parameter self :: <GCond>;
  c-name: "g_cond_clear";
end;

define C-function g-cond-init
  input parameter self :: <GCond>;
  c-name: "g_cond_init";
end;

define C-function g-cond-signal
  input parameter self :: <GCond>;
  c-name: "g_cond_signal";
end;

define C-function g-cond-wait
  input parameter self :: <GCond>;
  input parameter mutex_ :: <GMutex>;
  c-name: "g_cond_wait";
end;

define C-function g-cond-wait-until
  input parameter self :: <GCond>;
  input parameter mutex_ :: <GMutex>;
  input parameter end_time_ :: <C-signed-long>;
  result res :: <C-boolean>;
  c-name: "g_cond_wait_until";
end;

define constant $g-convert-error-no-conversion = 0;
define constant $g-convert-error-illegal-sequence = 1;
define constant $g-convert-error-failed = 2;
define constant $g-convert-error-partial-input = 3;
define constant $g-convert-error-bad-uri = 4;
define constant $g-convert-error-not-absolute-path = 5;
define constant <GConvertError> = <C-int>;
define C-pointer-type <GConvertError*> => <GConvertError>;

define constant $datalist-flags-mask = 3;

define constant $date-bad-day = 0;

define constant $date-bad-julian = 0;

define constant $date-bad-year = 0;

define constant $dir-separator = 92;

define constant $dir-separator-s = "\\";

define C-struct <_GData>
  pointer-type-name: <GData>;
end C-struct;

define C-struct <_GDate>
  slot g-date-julian-days :: <C-unsigned-int>;
  slot g-date-julian :: <C-unsigned-int>;
  slot g-date-dmy :: <C-unsigned-int>;
  slot g-date-day :: <C-unsigned-int>;
  slot g-date-month :: <C-unsigned-int>;
  slot g-date-year :: <C-unsigned-int>;
  pointer-type-name: <GDate>;
end C-struct;

define C-function g-date-new
  result res :: <GDate>;
  c-name: "g_date_new";
end;

define C-function g-date-new-dmy
  input parameter day_ :: <C-unsigned-char>;
  input parameter month_ :: <GDateMonth>;
  input parameter year_ :: <C-unsigned-short>;
  result res :: <GDate>;
  c-name: "g_date_new_dmy";
end;

define C-function g-date-new-julian
  input parameter julian_day_ :: <C-unsigned-int>;
  result res :: <GDate>;
  c-name: "g_date_new_julian";
end;

define C-function g-date-add-days
  input parameter self :: <GDate>;
  input parameter n_days_ :: <C-unsigned-int>;
  c-name: "g_date_add_days";
end;

define C-function g-date-add-months
  input parameter self :: <GDate>;
  input parameter n_months_ :: <C-unsigned-int>;
  c-name: "g_date_add_months";
end;

define C-function g-date-add-years
  input parameter self :: <GDate>;
  input parameter n_years_ :: <C-unsigned-int>;
  c-name: "g_date_add_years";
end;

define C-function g-date-clamp
  input parameter self :: <GDate>;
  input parameter min_date_ :: <GDate>;
  input parameter max_date_ :: <GDate>;
  c-name: "g_date_clamp";
end;

define C-function g-date-clear
  input parameter self :: <GDate>;
  input parameter n_dates_ :: <C-unsigned-int>;
  c-name: "g_date_clear";
end;

define C-function g-date-compare
  input parameter self :: <GDate>;
  input parameter rhs_ :: <GDate>;
  result res :: <C-signed-int>;
  c-name: "g_date_compare";
end;

define C-function g-date-days-between
  input parameter self :: <GDate>;
  input parameter date2_ :: <GDate>;
  result res :: <C-signed-int>;
  c-name: "g_date_days_between";
end;

define C-function g-date-free
  input parameter self :: <GDate>;
  c-name: "g_date_free";
end;

define C-function g-date-get-day
  input parameter self :: <GDate>;
  result res :: <C-unsigned-char>;
  c-name: "g_date_get_day";
end;

define C-function g-date-get-day-of-year
  input parameter self :: <GDate>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_get_day_of_year";
end;

define C-function g-date-get-iso8601-week-of-year
  input parameter self :: <GDate>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_get_iso8601_week_of_year";
end;

define C-function g-date-get-julian
  input parameter self :: <GDate>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_get_julian";
end;

define C-function g-date-get-monday-week-of-year
  input parameter self :: <GDate>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_get_monday_week_of_year";
end;

define C-function g-date-get-month
  input parameter self :: <GDate>;
  result res :: <GDateMonth>;
  c-name: "g_date_get_month";
end;

define C-function g-date-get-sunday-week-of-year
  input parameter self :: <GDate>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_get_sunday_week_of_year";
end;

define C-function g-date-get-weekday
  input parameter self :: <GDate>;
  result res :: <GDateWeekday>;
  c-name: "g_date_get_weekday";
end;

define C-function g-date-get-year
  input parameter self :: <GDate>;
  result res :: <C-unsigned-short>;
  c-name: "g_date_get_year";
end;

define C-function g-date-is-first-of-month
  input parameter self :: <GDate>;
  result res :: <C-boolean>;
  c-name: "g_date_is_first_of_month";
end;

define C-function g-date-is-last-of-month
  input parameter self :: <GDate>;
  result res :: <C-boolean>;
  c-name: "g_date_is_last_of_month";
end;

define C-function g-date-order
  input parameter self :: <GDate>;
  input parameter date2_ :: <GDate>;
  c-name: "g_date_order";
end;

define C-function g-date-set-day
  input parameter self :: <GDate>;
  input parameter day_ :: <C-unsigned-char>;
  c-name: "g_date_set_day";
end;

define C-function g-date-set-dmy
  input parameter self :: <GDate>;
  input parameter day_ :: <C-unsigned-char>;
  input parameter month_ :: <GDateMonth>;
  input parameter y_ :: <C-unsigned-short>;
  c-name: "g_date_set_dmy";
end;

define C-function g-date-set-julian
  input parameter self :: <GDate>;
  input parameter julian_date_ :: <C-unsigned-int>;
  c-name: "g_date_set_julian";
end;

define C-function g-date-set-month
  input parameter self :: <GDate>;
  input parameter month_ :: <GDateMonth>;
  c-name: "g_date_set_month";
end;

define C-function g-date-set-parse
  input parameter self :: <GDate>;
  input parameter str_ :: <C-string>;
  c-name: "g_date_set_parse";
end;

define C-function g-date-set-time
  input parameter self :: <GDate>;
  input parameter time__ :: <C-signed-int>;
  c-name: "g_date_set_time";
end;

define C-function g-date-set-time-t
  input parameter self :: <GDate>;
  input parameter timet_ :: <C-signed-long>;
  c-name: "g_date_set_time_t";
end;

define C-function g-date-set-time-val
  input parameter self :: <GDate>;
  input parameter timeval_ :: <GTimeVal>;
  c-name: "g_date_set_time_val";
end;

define C-function g-date-set-year
  input parameter self :: <GDate>;
  input parameter year_ :: <C-unsigned-short>;
  c-name: "g_date_set_year";
end;

define C-function g-date-subtract-days
  input parameter self :: <GDate>;
  input parameter n_days_ :: <C-unsigned-int>;
  c-name: "g_date_subtract_days";
end;

define C-function g-date-subtract-months
  input parameter self :: <GDate>;
  input parameter n_months_ :: <C-unsigned-int>;
  c-name: "g_date_subtract_months";
end;

define C-function g-date-subtract-years
  input parameter self :: <GDate>;
  input parameter n_years_ :: <C-unsigned-int>;
  c-name: "g_date_subtract_years";
end;

define C-function g-date-to-struct-tm
  input parameter self :: <GDate>;
  input parameter tm_ :: <C-void*>;
  c-name: "g_date_to_struct_tm";
end;

define C-function g-date-valid
  input parameter self :: <GDate>;
  result res :: <C-boolean>;
  c-name: "g_date_valid";
end;

define C-function g-date-get-days-in-month
  input parameter month_ :: <GDateMonth>;
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-unsigned-char>;
  c-name: "g_date_get_days_in_month";
end;

define C-function g-date-get-monday-weeks-in-year
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-unsigned-char>;
  c-name: "g_date_get_monday_weeks_in_year";
end;

define C-function g-date-get-sunday-weeks-in-year
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-unsigned-char>;
  c-name: "g_date_get_sunday_weeks_in_year";
end;

define C-function g-date-is-leap-year
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-boolean>;
  c-name: "g_date_is_leap_year";
end;

define C-function g-date-strftime
  input parameter s_ :: <C-string>;
  input parameter slen_ :: <C-unsigned-long>;
  input parameter format_ :: <C-string>;
  input parameter date_ :: <GDate>;
  result res :: <C-unsigned-long>;
  c-name: "g_date_strftime";
end;

define C-function g-date-valid-day
  input parameter day_ :: <C-unsigned-char>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_day";
end;

define C-function g-date-valid-dmy
  input parameter day_ :: <C-unsigned-char>;
  input parameter month_ :: <GDateMonth>;
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_dmy";
end;

define C-function g-date-valid-julian
  input parameter julian_date_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_julian";
end;

define C-function g-date-valid-month
  input parameter month_ :: <GDateMonth>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_month";
end;

define C-function g-date-valid-weekday
  input parameter weekday_ :: <GDateWeekday>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_weekday";
end;

define C-function g-date-valid-year
  input parameter year_ :: <C-unsigned-short>;
  result res :: <C-boolean>;
  c-name: "g_date_valid_year";
end;

define constant $g-date-day = 0;
define constant $g-date-month = 1;
define constant $g-date-year = 2;
define constant <GDateDMY> = <C-int>;
define C-pointer-type <GDateDMY*> => <GDateDMY>;

define constant $g-date-bad-month = 0;
define constant $g-date-january = 1;
define constant $g-date-february = 2;
define constant $g-date-march = 3;
define constant $g-date-april = 4;
define constant $g-date-may = 5;
define constant $g-date-june = 6;
define constant $g-date-july = 7;
define constant $g-date-august = 8;
define constant $g-date-september = 9;
define constant $g-date-october = 10;
define constant $g-date-november = 11;
define constant $g-date-december = 12;
define constant <GDateMonth> = <C-int>;
define C-pointer-type <GDateMonth*> => <GDateMonth>;

define C-struct <_GDateTime>
  pointer-type-name: <GDateTime>;
end C-struct;

define C-function g-date-time-new
  input parameter tz_ :: <GTimeZone>;
  input parameter year_ :: <C-signed-int>;
  input parameter month_ :: <C-signed-int>;
  input parameter day_ :: <C-signed-int>;
  input parameter hour_ :: <C-signed-int>;
  input parameter minute_ :: <C-signed-int>;
  input parameter seconds_ :: <C-double>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new";
end;

define C-function g-date-time-new-from-timeval-local
  input parameter tv_ :: <GTimeVal>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_from_timeval_local";
end;

define C-function g-date-time-new-from-timeval-utc
  input parameter tv_ :: <GTimeVal>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_from_timeval_utc";
end;

define C-function g-date-time-new-from-unix-local
  input parameter t_ :: <C-signed-long>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_from_unix_local";
end;

define C-function g-date-time-new-from-unix-utc
  input parameter t_ :: <C-signed-long>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_from_unix_utc";
end;

define C-function g-date-time-new-local
  input parameter year_ :: <C-signed-int>;
  input parameter month_ :: <C-signed-int>;
  input parameter day_ :: <C-signed-int>;
  input parameter hour_ :: <C-signed-int>;
  input parameter minute_ :: <C-signed-int>;
  input parameter seconds_ :: <C-double>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_local";
end;

define C-function g-date-time-new-now
  input parameter tz_ :: <GTimeZone>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_now";
end;

define C-function g-date-time-new-now-local
  result res :: <GDateTime>;
  c-name: "g_date_time_new_now_local";
end;

define C-function g-date-time-new-now-utc
  result res :: <GDateTime>;
  c-name: "g_date_time_new_now_utc";
end;

define C-function g-date-time-new-utc
  input parameter year_ :: <C-signed-int>;
  input parameter month_ :: <C-signed-int>;
  input parameter day_ :: <C-signed-int>;
  input parameter hour_ :: <C-signed-int>;
  input parameter minute_ :: <C-signed-int>;
  input parameter seconds_ :: <C-double>;
  result res :: <GDateTime>;
  c-name: "g_date_time_new_utc";
end;

define C-function g-date-time-add
  input parameter self :: <GDateTime>;
  input parameter timespan_ :: <C-signed-long>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add";
end;

define C-function g-date-time-add-days
  input parameter self :: <GDateTime>;
  input parameter days_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_days";
end;

define C-function g-date-time-add-full
  input parameter self :: <GDateTime>;
  input parameter years_ :: <C-signed-int>;
  input parameter months_ :: <C-signed-int>;
  input parameter days_ :: <C-signed-int>;
  input parameter hours_ :: <C-signed-int>;
  input parameter minutes_ :: <C-signed-int>;
  input parameter seconds_ :: <C-double>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_full";
end;

define C-function g-date-time-add-hours
  input parameter self :: <GDateTime>;
  input parameter hours_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_hours";
end;

define C-function g-date-time-add-minutes
  input parameter self :: <GDateTime>;
  input parameter minutes_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_minutes";
end;

define C-function g-date-time-add-months
  input parameter self :: <GDateTime>;
  input parameter months_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_months";
end;

define C-function g-date-time-add-seconds
  input parameter self :: <GDateTime>;
  input parameter seconds_ :: <C-double>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_seconds";
end;

define C-function g-date-time-add-weeks
  input parameter self :: <GDateTime>;
  input parameter weeks_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_weeks";
end;

define C-function g-date-time-add-years
  input parameter self :: <GDateTime>;
  input parameter years_ :: <C-signed-int>;
  result res :: <GDateTime>;
  c-name: "g_date_time_add_years";
end;

define C-function g-date-time-difference
  input parameter self :: <GDateTime>;
  input parameter begin_ :: <GDateTime>;
  result res :: <C-signed-long>;
  c-name: "g_date_time_difference";
end;

define C-function g-date-time-format
  input parameter self :: <GDateTime>;
  input parameter format_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_date_time_format";
end;

define C-function g-date-time-get-day-of-month
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_day_of_month";
end;

define C-function g-date-time-get-day-of-week
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_day_of_week";
end;

define C-function g-date-time-get-day-of-year
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_day_of_year";
end;

define C-function g-date-time-get-hour
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_hour";
end;

define C-function g-date-time-get-microsecond
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_microsecond";
end;

define C-function g-date-time-get-minute
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_minute";
end;

define C-function g-date-time-get-month
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_month";
end;

define C-function g-date-time-get-second
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_second";
end;

define C-function g-date-time-get-seconds
  input parameter self :: <GDateTime>;
  result res :: <C-double>;
  c-name: "g_date_time_get_seconds";
end;

define C-function g-date-time-get-timezone-abbreviation
  input parameter self :: <GDateTime>;
  result res :: <C-string>;
  c-name: "g_date_time_get_timezone_abbreviation";
end;

define C-function g-date-time-get-utc-offset
  input parameter self :: <GDateTime>;
  result res :: <C-signed-long>;
  c-name: "g_date_time_get_utc_offset";
end;

define C-function g-date-time-get-week-numbering-year
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_week_numbering_year";
end;

define C-function g-date-time-get-week-of-year
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_week_of_year";
end;

define C-function g-date-time-get-year
  input parameter self :: <GDateTime>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_get_year";
end;

define C-function g-date-time-get-ymd
  input parameter self :: <GDateTime>;
  output parameter year_ :: <C-signed-int*>;
  output parameter month_ :: <C-signed-int*>;
  output parameter day_ :: <C-signed-int*>;
  c-name: "g_date_time_get_ymd";
end;

define C-function g-date-time-is-daylight-savings
  input parameter self :: <GDateTime>;
  result res :: <C-boolean>;
  c-name: "g_date_time_is_daylight_savings";
end;

define C-function g-date-time-ref
  input parameter self :: <GDateTime>;
  result res :: <GDateTime>;
  c-name: "g_date_time_ref";
end;

define C-function g-date-time-to-local
  input parameter self :: <GDateTime>;
  result res :: <GDateTime>;
  c-name: "g_date_time_to_local";
end;

define C-function g-date-time-to-timeval
  input parameter self :: <GDateTime>;
  input parameter tv_ :: <GTimeVal>;
  result res :: <C-boolean>;
  c-name: "g_date_time_to_timeval";
end;

define C-function g-date-time-to-timezone
  input parameter self :: <GDateTime>;
  input parameter tz_ :: <GTimeZone>;
  result res :: <GDateTime>;
  c-name: "g_date_time_to_timezone";
end;

define C-function g-date-time-to-unix
  input parameter self :: <GDateTime>;
  result res :: <C-signed-long>;
  c-name: "g_date_time_to_unix";
end;

define C-function g-date-time-to-utc
  input parameter self :: <GDateTime>;
  result res :: <GDateTime>;
  c-name: "g_date_time_to_utc";
end;

define C-function g-date-time-unref
  input parameter self :: <GDateTime>;
  c-name: "g_date_time_unref";
end;

define C-function g-date-time-compare
  input parameter dt1_ :: <C-void*>;
  input parameter dt2_ :: <C-void*>;
  result res :: <C-signed-int>;
  c-name: "g_date_time_compare";
end;

define C-function g-date-time-equal
  input parameter dt1_ :: <C-void*>;
  input parameter dt2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_date_time_equal";
end;

define C-function g-date-time-hash
  input parameter datetime_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_date_time_hash";
end;

define constant $g-date-bad-weekday = 0;
define constant $g-date-monday = 1;
define constant $g-date-tuesday = 2;
define constant $g-date-wednesday = 3;
define constant $g-date-thursday = 4;
define constant $g-date-friday = 5;
define constant $g-date-saturday = 6;
define constant $g-date-sunday = 7;
define constant <GDateWeekday> = <C-int>;
define C-pointer-type <GDateWeekday*> => <GDateWeekday>;

define C-struct <_GDebugKey>
  slot g-debug-key-key :: <C-string>;
  slot g-debug-key-value :: <C-unsigned-int>;
  pointer-type-name: <GDebugKey>;
end C-struct;

define C-struct <_GDir>
  pointer-type-name: <GDir>;
end C-struct;

define C-function g-dir-close
  input parameter self :: <GDir>;
  c-name: "g_dir_close";
end;

define C-function g-dir-read-name
  input parameter self :: <GDir>;
  result res :: <C-string>;
  c-name: "g_dir_read_name";
end;

define C-function g-dir-rewind
  input parameter self :: <GDir>;
  c-name: "g_dir_rewind";
end;

define C-function g-dir-make-tmp
  input parameter tmpl_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_dir_make_tmp";
end;

define C-union <_GDoubleIEEE754>
  slot g-double-ieee754-v-double :: <C-double>;
  pointer-type-name: <GDoubleIEEE754>;
end C-union;

define constant $e = 2.7182820d0;

define C-struct <_GError>
  slot g-error-domain :: <C-unsigned-int>;
  slot g-error-code :: <C-signed-int>;
  slot g-error-message :: <C-string>;
  pointer-type-name: <GError>;
end C-struct;

define C-function g-error-new-literal
  input parameter domain_ :: <C-unsigned-int>;
  input parameter code_ :: <C-signed-int>;
  input parameter message_ :: <C-string>;
  result res :: <GError>;
  c-name: "g_error_new_literal";
end;

define C-function g-error-copy
  input parameter self :: <GError>;
  result res :: <GError>;
  c-name: "g_error_copy";
end;

define C-function g-error-free
  input parameter self :: <GError>;
  c-name: "g_error_free";
end;

define C-function g-error-matches
  input parameter self :: <GError>;
  input parameter domain_ :: <C-unsigned-int>;
  input parameter code_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_error_matches";
end;

define constant $g-err-unknown = 0;
define constant $g-err-unexp-eof = 1;
define constant $g-err-unexp-eof-in-string = 2;
define constant $g-err-unexp-eof-in-comment = 3;
define constant $g-err-non-digit-in-const = 4;
define constant $g-err-digit-radix = 5;
define constant $g-err-float-radix = 6;
define constant $g-err-float-malformed = 7;
define constant <GErrorType> = <C-int>;
define C-pointer-type <GErrorType*> => <GErrorType>;

define constant $g-file-error-exist = 0;
define constant $g-file-error-isdir = 1;
define constant $g-file-error-acces = 2;
define constant $g-file-error-nametoolong = 3;
define constant $g-file-error-noent = 4;
define constant $g-file-error-notdir = 5;
define constant $g-file-error-nxio = 6;
define constant $g-file-error-nodev = 7;
define constant $g-file-error-rofs = 8;
define constant $g-file-error-txtbsy = 9;
define constant $g-file-error-fault = 10;
define constant $g-file-error-loop = 11;
define constant $g-file-error-nospc = 12;
define constant $g-file-error-nomem = 13;
define constant $g-file-error-mfile = 14;
define constant $g-file-error-nfile = 15;
define constant $g-file-error-badf = 16;
define constant $g-file-error-inval = 17;
define constant $g-file-error-pipe = 18;
define constant $g-file-error-again = 19;
define constant $g-file-error-intr = 20;
define constant $g-file-error-io = 21;
define constant $g-file-error-perm = 22;
define constant $g-file-error-nosys = 23;
define constant $g-file-error-failed = 24;
define constant <GFileError> = <C-int>;
define C-pointer-type <GFileError*> => <GFileError>;

define constant $g-file-test-is-regular = 1;
define constant $g-file-test-is-symlink = 2;
define constant $g-file-test-is-dir = 4;
define constant $g-file-test-is-executable = 8;
define constant $g-file-test-exists = 16;
define constant <GFileTest> = <C-int>;
define C-pointer-type <GFileTest*> => <GFileTest>;

define C-union <_GFloatIEEE754>
  slot g-float-ieee754-v-float :: <C-float>;
  pointer-type-name: <GFloatIEEE754>;
end C-union;

define constant $g-format-size-default = 0;
define constant $g-format-size-long-format = 1;
define constant $g-format-size-iec-units = 2;
define constant <GFormatSizeFlags> = <C-int>;
define C-pointer-type <GFormatSizeFlags*> => <GFormatSizeFlags>;

define constant $gint16-format = "hi";

define constant $gint16-modifier = "h";

define constant $gint32-format = "i";

define constant $gint32-modifier = "";

define constant $gint64-format = "li";

define constant $gint64-modifier = "l";

define constant $gintptr-format = "li";

define constant $gintptr-modifier = "l";

define constant $gnuc-function = "";

define constant $gnuc-pretty-function = "";

define constant $gsize-format = "lu";

define constant $gsize-modifier = "l";

define constant $gssize-format = "li";

define constant $guint16-format = "hu";

define constant $guint32-format = "u";

define constant $guint64-format = "lu";

define constant $guintptr-format = "lu";

define constant $have-gint64 = 1;

define constant $have-gnuc-varargs = 1;

define constant $have-gnuc-visibility = 1;

define constant $have-growing-stack = 1;

define constant $have-inline = 1;

define constant $have-iso-varargs = 1;

define constant $have---inline = 1;

define constant $have---inline-- = 1;

define constant $hook-flag-user-shift = 4;

define C-struct <_GHashTable>
  pointer-type-name: <GHashTable>;
end C-struct;

define C-function g-hash-table-add
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  c-name: "g_hash_table_add";
end;

define C-function g-hash-table-contains
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_hash_table_contains";
end;

define C-function g-hash-table-destroy
  input parameter hash_table_ :: <GHashTable>;
  c-name: "g_hash_table_destroy";
end;

define C-function g-hash-table-insert
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  c-name: "g_hash_table_insert";
end;

define C-function g-hash-table-lookup-extended
  input parameter hash_table_ :: <GHashTable>;
  input parameter lookup_key_ :: <C-void*>;
  input parameter orig_key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_hash_table_lookup_extended";
end;

define C-function g-hash-table-remove
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_hash_table_remove";
end;

define C-function g-hash-table-remove-all
  input parameter hash_table_ :: <GHashTable>;
  c-name: "g_hash_table_remove_all";
end;

define C-function g-hash-table-replace
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  c-name: "g_hash_table_replace";
end;

define C-function g-hash-table-size
  input parameter hash_table_ :: <GHashTable>;
  result res :: <C-unsigned-int>;
  c-name: "g_hash_table_size";
end;

define C-function g-hash-table-steal
  input parameter hash_table_ :: <GHashTable>;
  input parameter key_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_hash_table_steal";
end;

define C-function g-hash-table-steal-all
  input parameter hash_table_ :: <GHashTable>;
  c-name: "g_hash_table_steal_all";
end;

define C-function g-hash-table-unref
  input parameter hash_table_ :: <GHashTable>;
  c-name: "g_hash_table_unref";
end;

define C-struct <_GHashTableIter>
  constant slot g-hash-table-iter-dummy1 :: <C-void*>;
  constant slot g-hash-table-iter-dummy2 :: <C-void*>;
  constant slot g-hash-table-iter-dummy3 :: <C-void*>;
  constant slot g-hash-table-iter-dummy4 :: <C-signed-int>;
  constant slot g-hash-table-iter-dummy5 :: <C-boolean>;
  constant slot g-hash-table-iter-dummy6 :: <C-void*>;
  pointer-type-name: <GHashTableIter>;
end C-struct;

define C-function g-hash-table-iter-init
  input parameter self :: <GHashTableIter>;
  input parameter hash_table_ :: <GHashTable>;
  c-name: "g_hash_table_iter_init";
end;

define C-function g-hash-table-iter-next
  input parameter self :: <GHashTableIter>;
  input parameter key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_hash_table_iter_next";
end;

define C-function g-hash-table-iter-remove
  input parameter self :: <GHashTableIter>;
  c-name: "g_hash_table_iter_remove";
end;

define C-function g-hash-table-iter-replace
  input parameter self :: <GHashTableIter>;
  input parameter value_ :: <C-void*>;
  c-name: "g_hash_table_iter_replace";
end;

define C-function g-hash-table-iter-steal
  input parameter self :: <GHashTableIter>;
  c-name: "g_hash_table_iter_steal";
end;

define C-struct <_GHmac>
  pointer-type-name: <GHmac>;
end C-struct;

define C-function g-hmac-get-digest
  input parameter self :: <GHmac>;
  input parameter buffer_ :: <C-unsigned-char*>;
  input parameter digest_len_ :: <C-unsigned-long*>;
  c-name: "g_hmac_get_digest";
end;

define C-function g-hmac-get-string
  input parameter self :: <GHmac>;
  result res :: <C-string>;
  c-name: "g_hmac_get_string";
end;

define C-function g-hmac-unref
  input parameter self :: <GHmac>;
  c-name: "g_hmac_unref";
end;

define C-function g-hmac-update
  input parameter self :: <GHmac>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  c-name: "g_hmac_update";
end;

define C-struct <_GHook>
  slot g-hook-data :: <C-void*>;
  slot g-hook-next :: <GHook>;
  slot g-hook-prev :: <GHook>;
  slot g-hook-ref-count :: <C-unsigned-int>;
  slot g-hook-hook-id :: <C-unsigned-long>;
  slot g-hook-flags :: <C-unsigned-int>;
  slot g-hook-func :: <C-void*>;
  slot g-hook-destroy :: <C-function-pointer>;
  pointer-type-name: <GHook>;
end C-struct;

define C-function g-hook-compare-ids
  input parameter self :: <GHook>;
  input parameter sibling_ :: <GHook>;
  result res :: <C-signed-int>;
  c-name: "g_hook_compare_ids";
end;

define C-function g-hook-destroy-link
  input parameter hook_list_ :: <GHookList>;
  input parameter hook_ :: <GHook>;
  c-name: "g_hook_destroy_link";
end;

define C-function g-hook-free
  input parameter hook_list_ :: <GHookList>;
  input parameter hook_ :: <GHook>;
  c-name: "g_hook_free";
end;

define C-function g-hook-insert-before
  input parameter hook_list_ :: <GHookList>;
  input parameter sibling_ :: <GHook>;
  input parameter hook_ :: <GHook>;
  c-name: "g_hook_insert_before";
end;

define C-function g-hook-prepend
  input parameter hook_list_ :: <GHookList>;
  input parameter hook_ :: <GHook>;
  c-name: "g_hook_prepend";
end;

define C-function g-hook-unref
  input parameter hook_list_ :: <GHookList>;
  input parameter hook_ :: <GHook>;
  c-name: "g_hook_unref";
end;

define constant $g-hook-flag-active = 1;
define constant $g-hook-flag-in-call = 2;
define constant $g-hook-flag-mask = 15;
define constant <GHookFlagMask> = <C-int>;
define C-pointer-type <GHookFlagMask*> => <GHookFlagMask>;

define C-struct <_GHookList>
  slot g-hook-list-seq-id :: <C-unsigned-long>;
  slot g-hook-list-hook-size :: <C-unsigned-int>;
  slot g-hook-list-is-setup :: <C-unsigned-int>;
  slot g-hook-list-hooks :: <GHook>;
  slot g-hook-list-dummy3 :: <C-void*>;
  slot g-hook-list-finalize-hook :: <C-function-pointer>;
  slot g-hook-list-dummy :: <C-void*>;
  pointer-type-name: <GHookList>;
end C-struct;

define C-function g-hook-list-clear
  input parameter self :: <GHookList>;
  c-name: "g_hook_list_clear";
end;

define C-function g-hook-list-init
  input parameter self :: <GHookList>;
  input parameter hook_size_ :: <C-unsigned-int>;
  c-name: "g_hook_list_init";
end;

define C-function g-hook-list-invoke
  input parameter self :: <GHookList>;
  input parameter may_recurse_ :: <C-boolean>;
  c-name: "g_hook_list_invoke";
end;

define C-function g-hook-list-invoke-check
  input parameter self :: <GHookList>;
  input parameter may_recurse_ :: <C-boolean>;
  c-name: "g_hook_list_invoke_check";
end;

define C-struct <_GIConv>
  pointer-type-name: <GIConv>;
end C-struct;

define C-function g-iconv
  input parameter self :: <GIConv>;
  input parameter inbuf_ :: <C-string>;
  input parameter inbytes_left_ :: <C-unsigned-long*>;
  input parameter outbuf_ :: <C-string>;
  input parameter outbytes_left_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-long>;
  c-name: "g_iconv";
end;

define C-function g-iconv-close
  input parameter self :: <GIConv>;
  result res :: <C-signed-int>;
  c-name: "g_iconv_close";
end;

define constant $ieee754-double-bias = 1023;

define constant $ieee754-float-bias = 127;

define C-struct <_GIOChannel>
  constant slot g-io-channel-ref-count :: <C-signed-int>;
  constant slot g-io-channel-funcs :: <GIOFuncs>;
  constant slot g-io-channel-encoding :: <C-string>;
  constant slot g-io-channel-read-cd :: <GIConv>;
  constant slot g-io-channel-write-cd :: <GIConv>;
  constant slot g-io-channel-line-term :: <C-string>;
  constant slot g-io-channel-line-term-len :: <C-unsigned-int>;
  constant slot g-io-channel-buf-size :: <C-unsigned-long>;
  constant slot g-io-channel-read-buf :: <GString>;
  constant slot g-io-channel-encoded-read-buf :: <GString>;
  constant slot g-io-channel-write-buf :: <GString>;
  constant slot g-io-channel-partial-write-buf :: <C-unsigned-char*>;
  constant slot g-io-channel-use-buffer :: <C-unsigned-int>;
  constant slot g-io-channel-do-encode :: <C-unsigned-int>;
  constant slot g-io-channel-close-on-unref :: <C-unsigned-int>;
  constant slot g-io-channel-is-readable :: <C-unsigned-int>;
  constant slot g-io-channel-is-writeable :: <C-unsigned-int>;
  constant slot g-io-channel-is-seekable :: <C-unsigned-int>;
  constant slot g-io-channel-reserved1 :: <C-void*>;
  constant slot g-io-channel-reserved2 :: <C-void*>;
  pointer-type-name: <GIOChannel>;
end C-struct;

define C-function g-io-channel-new-file
  input parameter filename_ :: <C-string>;
  input parameter mode_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GIOChannel>;
  c-name: "g_io_channel_new_file";
end;

define C-function g-io-channel-unix-new
  input parameter fd_ :: <C-signed-int>;
  result res :: <GIOChannel>;
  c-name: "g_io_channel_unix_new";
end;

define C-function g-io-channel-close
  input parameter self :: <GIOChannel>;
  c-name: "g_io_channel_close";
end;

define C-function g-io-channel-flush
  input parameter self :: <GIOChannel>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_flush";
end;

define C-function g-io-channel-get-buffer-condition
  input parameter self :: <GIOChannel>;
  result res :: <GIOCondition>;
  c-name: "g_io_channel_get_buffer_condition";
end;

define C-function g-io-channel-get-buffer-size
  input parameter self :: <GIOChannel>;
  result res :: <C-unsigned-long>;
  c-name: "g_io_channel_get_buffer_size";
end;

define C-function g-io-channel-get-buffered
  input parameter self :: <GIOChannel>;
  result res :: <C-boolean>;
  c-name: "g_io_channel_get_buffered";
end;

define C-function g-io-channel-get-close-on-unref
  input parameter self :: <GIOChannel>;
  result res :: <C-boolean>;
  c-name: "g_io_channel_get_close_on_unref";
end;

define C-function g-io-channel-get-encoding
  input parameter self :: <GIOChannel>;
  result res :: <C-string>;
  c-name: "g_io_channel_get_encoding";
end;

define C-function g-io-channel-get-flags
  input parameter self :: <GIOChannel>;
  result res :: <GIOFlags>;
  c-name: "g_io_channel_get_flags";
end;

define C-function g-io-channel-get-line-term
  input parameter self :: <GIOChannel>;
  input parameter length_ :: <C-signed-int*>;
  result res :: <C-string>;
  c-name: "g_io_channel_get_line_term";
end;

define C-function g-io-channel-init
  input parameter self :: <GIOChannel>;
  c-name: "g_io_channel_init";
end;

define C-function g-io-channel-read
  input parameter self :: <GIOChannel>;
  input parameter buf_ :: <C-string>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  result res :: <GIOError>;
  c-name: "g_io_channel_read";
end;

define C-function g-io-channel-read-chars
  input parameter self :: <GIOChannel>;
  input parameter buf_ :: <C-string>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_read_chars";
end;

define C-function g-io-channel-read-line
  input parameter self :: <GIOChannel>;
  input parameter str_return_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long*>;
  input parameter terminator_pos_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_read_line";
end;

define C-function g-io-channel-read-line-string
  input parameter self :: <GIOChannel>;
  input parameter buffer_ :: <GString>;
  input parameter terminator_pos_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_read_line_string";
end;

define C-function g-io-channel-read-to-end
  input parameter self :: <GIOChannel>;
  input parameter str_return_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_read_to_end";
end;

define C-function g-io-channel-read-unichar
  input parameter self :: <GIOChannel>;
  input parameter thechar_ :: <C-unsigned-int*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_read_unichar";
end;

define C-function g-io-channel-ref
  input parameter self :: <GIOChannel>;
  result res :: <GIOChannel>;
  c-name: "g_io_channel_ref";
end;

define C-function g-io-channel-seek
  input parameter self :: <GIOChannel>;
  input parameter offset_ :: <C-signed-long>;
  input parameter type_ :: <GSeekType>;
  result res :: <GIOError>;
  c-name: "g_io_channel_seek";
end;

define C-function g-io-channel-seek-position
  input parameter self :: <GIOChannel>;
  input parameter offset_ :: <C-signed-long>;
  input parameter type_ :: <GSeekType>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_seek_position";
end;

define C-function g-io-channel-set-buffer-size
  input parameter self :: <GIOChannel>;
  input parameter size_ :: <C-unsigned-long>;
  c-name: "g_io_channel_set_buffer_size";
end;

define C-function g-io-channel-set-buffered
  input parameter self :: <GIOChannel>;
  input parameter buffered_ :: <C-boolean>;
  c-name: "g_io_channel_set_buffered";
end;

define C-function g-io-channel-set-close-on-unref
  input parameter self :: <GIOChannel>;
  input parameter do_close_ :: <C-boolean>;
  c-name: "g_io_channel_set_close_on_unref";
end;

define C-function g-io-channel-set-encoding
  input parameter self :: <GIOChannel>;
  input parameter encoding_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_set_encoding";
end;

define C-function g-io-channel-set-flags
  input parameter self :: <GIOChannel>;
  input parameter flags_ :: <GIOFlags>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_set_flags";
end;

define C-function g-io-channel-set-line-term
  input parameter self :: <GIOChannel>;
  input parameter line_term_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  c-name: "g_io_channel_set_line_term";
end;

define C-function g-io-channel-shutdown
  input parameter self :: <GIOChannel>;
  input parameter flush_ :: <C-boolean>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_shutdown";
end;

define C-function g-io-channel-unix-get-fd
  input parameter self :: <GIOChannel>;
  result res :: <C-signed-int>;
  c-name: "g_io_channel_unix_get_fd";
end;

define C-function g-io-channel-unref
  input parameter self :: <GIOChannel>;
  c-name: "g_io_channel_unref";
end;

define C-function g-io-channel-write
  input parameter self :: <GIOChannel>;
  input parameter buf_ :: <C-string>;
  input parameter count_ :: <C-unsigned-long>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  result res :: <GIOError>;
  c-name: "g_io_channel_write";
end;

define C-function g-io-channel-write-chars
  input parameter self :: <GIOChannel>;
  input parameter buf_ :: <C-string>;
  input parameter count_ :: <C-signed-long>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_write_chars";
end;

define C-function g-io-channel-write-unichar
  input parameter self :: <GIOChannel>;
  input parameter thechar_ :: <C-unsigned-int>;
  output parameter error_ :: <GError*>;
  result res :: <GIOStatus>;
  c-name: "g_io_channel_write_unichar";
end;

define C-function g-io-channel-error-from-errno
  input parameter en_ :: <C-signed-int>;
  result res :: <GIOChannelError>;
  c-name: "g_io_channel_error_from_errno";
end;

define C-function g-io-channel-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_io_channel_error_quark";
end;

define constant $g-io-channel-error-fbig = 0;
define constant $g-io-channel-error-inval = 1;
define constant $g-io-channel-error-io = 2;
define constant $g-io-channel-error-isdir = 3;
define constant $g-io-channel-error-nospc = 4;
define constant $g-io-channel-error-nxio = 5;
define constant $g-io-channel-error-overflow = 6;
define constant $g-io-channel-error-pipe = 7;
define constant $g-io-channel-error-failed = 8;
define constant <GIOChannelError> = <C-int>;
define C-pointer-type <GIOChannelError*> => <GIOChannelError>;

define constant $g-io-in = 1;
define constant $g-io-out = 4;
define constant $g-io-pri = 2;
define constant $g-io-err = 8;
define constant $g-io-hup = 16;
define constant $g-io-nval = 32;
define constant <GIOCondition> = <C-int>;
define C-pointer-type <GIOCondition*> => <GIOCondition>;

define constant $g-io-error-none = 0;
define constant $g-io-error-again = 1;
define constant $g-io-error-inval = 2;
define constant $g-io-error-unknown = 3;
define constant <GIOError> = <C-int>;
define C-pointer-type <GIOError*> => <GIOError>;

define constant $g-io-flag-append = 1;
define constant $g-io-flag-nonblock = 2;
define constant $g-io-flag-is-readable = 4;
define constant $g-io-flag-is-writable = 8;
define constant $g-io-flag-is-seekable = 16;
define constant $g-io-flag-mask = 31;
define constant $g-io-flag-get-mask = 31;
define constant $g-io-flag-set-mask = 3;
define constant <GIOFlags> = <C-int>;
define C-pointer-type <GIOFlags*> => <GIOFlags>;

define C-struct <_GIOFuncs>
  constant slot g-io-funcs-io-read :: <C-function-pointer>;
  constant slot g-io-funcs-io-write :: <C-function-pointer>;
  constant slot g-io-funcs-io-seek :: <C-function-pointer>;
  constant slot g-io-funcs-io-close :: <C-function-pointer>;
  constant slot g-io-funcs-io-create-watch :: <C-function-pointer>;
  constant slot g-io-funcs-io-free :: <C-function-pointer>;
  constant slot g-io-funcs-io-set-flags :: <C-function-pointer>;
  constant slot g-io-funcs-io-get-flags :: <C-function-pointer>;
  pointer-type-name: <GIOFuncs>;
end C-struct;

define constant $g-io-status-error = 0;
define constant $g-io-status-normal = 1;
define constant $g-io-status-eof = 2;
define constant $g-io-status-again = 3;
define constant <GIOStatus> = <C-int>;
define C-pointer-type <GIOStatus*> => <GIOStatus>;

define constant $key-file-desktop-group = "Desktop Entry";

define constant $key-file-desktop-key-categories = "Categories";

define constant $key-file-desktop-key-comment = "Comment";

define constant $key-file-desktop-key-exec = "Exec";

define constant $key-file-desktop-key-fullname = "X-GNOME-FullName";

define constant $key-file-desktop-key-generic-name = "GenericName";

define constant $key-file-desktop-key-gettext-domain = "X-GNOME-Gettext-Domain";

define constant $key-file-desktop-key-hidden = "Hidden";

define constant $key-file-desktop-key-icon = "Icon";

define constant $key-file-desktop-key-keywords = "Keywords";

define constant $key-file-desktop-key-mime-type = "MimeType";

define constant $key-file-desktop-key-name = "Name";

define constant $key-file-desktop-key-not-show-in = "NotShowIn";

define constant $key-file-desktop-key-no-display = "NoDisplay";

define constant $key-file-desktop-key-only-show-in = "OnlyShowIn";

define constant $key-file-desktop-key-path = "Path";

define constant $key-file-desktop-key-startup-notify = "StartupNotify";

define constant $key-file-desktop-key-startup-wm-class = "StartupWMClass";

define constant $key-file-desktop-key-terminal = "Terminal";

define constant $key-file-desktop-key-try-exec = "TryExec";

define constant $key-file-desktop-key-type = "Type";

define constant $key-file-desktop-key-url = "URL";

define constant $key-file-desktop-key-version = "Version";

define constant $key-file-desktop-type-application = "Application";

define constant $key-file-desktop-type-directory = "Directory";

define constant $key-file-desktop-type-link = "Link";

define C-struct <_GKeyFile>
  pointer-type-name: <GKeyFile>;
end C-struct;

define C-function g-key-file-new
  result res :: <GKeyFile>;
  c-name: "g_key_file_new";
end;

define C-function g-key-file-get-boolean
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_get_boolean";
end;

define C-function g-key-file-get-boolean-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-int*>;
  c-name: "g_key_file_get_boolean_list";
end;

define C-function g-key-file-get-comment
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_key_file_get_comment";
end;

define C-function g-key-file-get-double
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-double>;
  c-name: "g_key_file_get_double";
end;

define C-function g-key-file-get-double-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-double*>;
  c-name: "g_key_file_get_double_list";
end;

define C-function g-key-file-get-groups
  input parameter self :: <GKeyFile>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_key_file_get_groups";
end;

define C-function g-key-file-get-int64
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-long>;
  c-name: "g_key_file_get_int64";
end;

define C-function g-key-file-get-integer
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_key_file_get_integer";
end;

define C-function g-key-file-get-integer-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int*>;
  c-name: "g_key_file_get_integer_list";
end;

define C-function g-key-file-get-keys
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_key_file_get_keys";
end;

define C-function g-key-file-get-locale-string
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter locale_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_key_file_get_locale_string";
end;

define C-function g-key-file-get-locale-string-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter locale_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_key_file_get_locale_string_list";
end;

define C-function g-key-file-get-start-group
  input parameter self :: <GKeyFile>;
  result res :: <C-string>;
  c-name: "g_key_file_get_start_group";
end;

define C-function g-key-file-get-string
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_key_file_get_string";
end;

define C-function g-key-file-get-string-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string*>;
  c-name: "g_key_file_get_string_list";
end;

define C-function g-key-file-get-uint64
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-long>;
  c-name: "g_key_file_get_uint64";
end;

define C-function g-key-file-get-value
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_key_file_get_value";
end;

define C-function g-key-file-has-group
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_key_file_has_group";
end;

define C-function g-key-file-load-from-data
  input parameter self :: <GKeyFile>;
  input parameter data_ :: <C-string>;
  input parameter length_ :: <C-unsigned-long>;
  input parameter flags_ :: <GKeyFileFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_load_from_data";
end;

define C-function g-key-file-load-from-data-dirs
  input parameter self :: <GKeyFile>;
  input parameter file_ :: <C-string>;
  output parameter full_path_ :: <C-string>;
  input parameter flags_ :: <GKeyFileFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_load_from_data_dirs";
end;

define C-function g-key-file-load-from-dirs
  input parameter self :: <GKeyFile>;
  input parameter file_ :: <C-string>;
  input parameter search_dirs_ :: <C-string*>;
  output parameter full_path_ :: <C-string>;
  input parameter flags_ :: <GKeyFileFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_load_from_dirs";
end;

define C-function g-key-file-load-from-file
  input parameter self :: <GKeyFile>;
  input parameter file_ :: <C-string>;
  input parameter flags_ :: <GKeyFileFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_load_from_file";
end;

define C-function g-key-file-remove-comment
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_remove_comment";
end;

define C-function g-key-file-remove-group
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_remove_group";
end;

define C-function g-key-file-remove-key
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_remove_key";
end;

define C-function g-key-file-set-boolean
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-boolean>;
  c-name: "g_key_file_set_boolean";
end;

define C-function g-key-file-set-boolean-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter list_ :: <C-int*>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_boolean_list";
end;

define C-function g-key-file-set-comment
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter comment_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_key_file_set_comment";
end;

define C-function g-key-file-set-double
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-double>;
  c-name: "g_key_file_set_double";
end;

define C-function g-key-file-set-double-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter list_ :: <C-double*>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_double_list";
end;

define C-function g-key-file-set-int64
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-signed-long>;
  c-name: "g_key_file_set_int64";
end;

define C-function g-key-file-set-integer
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-signed-int>;
  c-name: "g_key_file_set_integer";
end;

define C-function g-key-file-set-integer-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter list_ :: <C-signed-int*>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_integer_list";
end;

define C-function g-key-file-set-list-separator
  input parameter self :: <GKeyFile>;
  input parameter separator_ :: <C-unsigned-char>;
  c-name: "g_key_file_set_list_separator";
end;

define C-function g-key-file-set-locale-string
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter locale_ :: <C-string>;
  input parameter string_ :: <C-string>;
  c-name: "g_key_file_set_locale_string";
end;

define C-function g-key-file-set-locale-string-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter locale_ :: <C-string>;
  input parameter list_ :: <C-string*>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_locale_string_list";
end;

define C-function g-key-file-set-string
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter string_ :: <C-string>;
  c-name: "g_key_file_set_string";
end;

define C-function g-key-file-set-string-list
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter list_ :: <C-string*>;
  input parameter length_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_string_list";
end;

define C-function g-key-file-set-uint64
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-unsigned-long>;
  c-name: "g_key_file_set_uint64";
end;

define C-function g-key-file-set-value
  input parameter self :: <GKeyFile>;
  input parameter group_name_ :: <C-string>;
  input parameter key_ :: <C-string>;
  input parameter value_ :: <C-string>;
  c-name: "g_key_file_set_value";
end;

define C-function g-key-file-to-data
  input parameter self :: <GKeyFile>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_key_file_to_data";
end;

define C-function g-key-file-unref
  input parameter self :: <GKeyFile>;
  c-name: "g_key_file_unref";
end;

define C-function g-key-file-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_key_file_error_quark";
end;

define constant $g-key-file-error-unknown-encoding = 0;
define constant $g-key-file-error-parse = 1;
define constant $g-key-file-error-not-found = 2;
define constant $g-key-file-error-key-not-found = 3;
define constant $g-key-file-error-group-not-found = 4;
define constant $g-key-file-error-invalid-value = 5;
define constant <GKeyFileError> = <C-int>;
define C-pointer-type <GKeyFileError*> => <GKeyFileError>;

define constant $g-key-file-none = 0;
define constant $g-key-file-keep-comments = 1;
define constant $g-key-file-keep-translations = 2;
define constant <GKeyFileFlags> = <C-int>;
define C-pointer-type <GKeyFileFlags*> => <GKeyFileFlags>;

define constant $little-endian = 1234;

define constant $ln10 = 2.3025850d0;

define constant $ln2 = 0.6931470d0;

define constant $log-2-base-10 = 0.3010300d0;

define constant $log-fatal-mask = 0;

define constant $log-level-user-shift = 8;

define C-struct <_GList>
  slot g-list-data :: <C-void*>;
  slot g-list-next :: <GList>;
  slot g-list-prev :: <GList>;
  pointer-type-name: <GList>;
end C-struct;

define constant $g-log-flag-recursion = 1;
define constant $g-log-flag-fatal = 2;
define constant $g-log-level-error = 4;
define constant $g-log-level-critical = 8;
define constant $g-log-level-warning = 16;
define constant $g-log-level-message = 32;
define constant $g-log-level-info = 64;
define constant $g-log-level-debug = 128;
define constant $g-log-level-mask = -4;
define constant <GLogLevelFlags> = <C-int>;
define C-pointer-type <GLogLevelFlags*> => <GLogLevelFlags>;

define constant $g-major-version = 2;

define constant $g-micro-version = 0;

define constant $g-minor-version = 32;

define constant $module-suffix = "so";

define C-struct <_GMainContext>
  pointer-type-name: <GMainContext>;
end C-struct;

define C-function g-main-context-new
  result res :: <GMainContext>;
  c-name: "g_main_context_new";
end;

define C-function g-main-context-acquire
  input parameter self :: <GMainContext>;
  result res :: <C-boolean>;
  c-name: "g_main_context_acquire";
end;

define C-function g-main-context-add-poll
  input parameter self :: <GMainContext>;
  input parameter fd_ :: <GPollFD>;
  input parameter priority_ :: <C-signed-int>;
  c-name: "g_main_context_add_poll";
end;

define C-function g-main-context-check
  input parameter self :: <GMainContext>;
  input parameter max_priority_ :: <C-signed-int>;
  input parameter fds_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_fds_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_main_context_check";
end;

define C-function g-main-context-dispatch
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_dispatch";
end;

define C-function g-main-context-find-source-by-funcs-user-data
  input parameter self :: <GMainContext>;
  input parameter funcs_ :: <GSourceFuncs>;
  input parameter user_data_ :: <C-void*>;
  result res :: <GSource>;
  c-name: "g_main_context_find_source_by_funcs_user_data";
end;

define C-function g-main-context-find-source-by-id
  input parameter self :: <GMainContext>;
  input parameter source_id_ :: <C-unsigned-int>;
  result res :: <GSource>;
  c-name: "g_main_context_find_source_by_id";
end;

define C-function g-main-context-find-source-by-user-data
  input parameter self :: <GMainContext>;
  input parameter user_data_ :: <C-void*>;
  result res :: <GSource>;
  c-name: "g_main_context_find_source_by_user_data";
end;

define C-function g-main-context-invoke-full
  input parameter self :: <GMainContext>;
  input parameter priority_ :: <C-signed-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "g_main_context_invoke_full";
end;

define C-function g-main-context-is-owner
  input parameter self :: <GMainContext>;
  result res :: <C-boolean>;
  c-name: "g_main_context_is_owner";
end;

define C-function g-main-context-iteration
  input parameter self :: <GMainContext>;
  input parameter may_block_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_main_context_iteration";
end;

define C-function g-main-context-pending
  input parameter self :: <GMainContext>;
  result res :: <C-boolean>;
  c-name: "g_main_context_pending";
end;

define C-function g-main-context-pop-thread-default
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_pop_thread_default";
end;

define C-function g-main-context-prepare
  input parameter self :: <GMainContext>;
  input parameter priority_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "g_main_context_prepare";
end;

define C-function g-main-context-push-thread-default
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_push_thread_default";
end;

define C-function g-main-context-query
  input parameter self :: <GMainContext>;
  input parameter max_priority_ :: <C-signed-int>;
  output parameter timeout__ :: <C-signed-int*>;
  output parameter fds_ :: <C-unsigned-char*> /* Not supported */;
  output parameter n_fds_ :: <C-signed-int*>;
  result res :: <C-signed-int>;
  c-name: "g_main_context_query";
end;

define C-function g-main-context-ref
  input parameter self :: <GMainContext>;
  result res :: <GMainContext>;
  c-name: "g_main_context_ref";
end;

define C-function g-main-context-release
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_release";
end;

define C-function g-main-context-remove-poll
  input parameter self :: <GMainContext>;
  input parameter fd_ :: <GPollFD>;
  c-name: "g_main_context_remove_poll";
end;

define C-function g-main-context-unref
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_unref";
end;

define C-function g-main-context-wait
  input parameter self :: <GMainContext>;
  input parameter cond_ :: <GCond>;
  input parameter mutex_ :: <GMutex>;
  result res :: <C-boolean>;
  c-name: "g_main_context_wait";
end;

define C-function g-main-context-wakeup
  input parameter self :: <GMainContext>;
  c-name: "g_main_context_wakeup";
end;

define C-function g-main-context-default
  result res :: <GMainContext>;
  c-name: "g_main_context_default";
end;

define C-function g-main-context-get-thread-default
  result res :: <GMainContext>;
  c-name: "g_main_context_get_thread_default";
end;

define C-function g-main-context-ref-thread-default
  result res :: <GMainContext>;
  c-name: "g_main_context_ref_thread_default";
end;

define C-struct <_GMainLoop>
  pointer-type-name: <GMainLoop>;
end C-struct;

define C-function g-main-loop-new
  input parameter context_ :: <GMainContext>;
  input parameter is_running_ :: <C-boolean>;
  result res :: <GMainLoop>;
  c-name: "g_main_loop_new";
end;

define C-function g-main-loop-get-context
  input parameter self :: <GMainLoop>;
  result res :: <GMainContext>;
  c-name: "g_main_loop_get_context";
end;

define C-function g-main-loop-is-running
  input parameter self :: <GMainLoop>;
  result res :: <C-boolean>;
  c-name: "g_main_loop_is_running";
end;

define C-function g-main-loop-quit
  input parameter self :: <GMainLoop>;
  c-name: "g_main_loop_quit";
end;

define C-function g-main-loop-ref
  input parameter self :: <GMainLoop>;
  result res :: <GMainLoop>;
  c-name: "g_main_loop_ref";
end;

define C-function g-main-loop-run
  input parameter self :: <GMainLoop>;
  c-name: "g_main_loop_run";
end;

define C-function g-main-loop-unref
  input parameter self :: <GMainLoop>;
  c-name: "g_main_loop_unref";
end;

define C-struct <_GMappedFile>
  pointer-type-name: <GMappedFile>;
end C-struct;

define C-function g-mapped-file-free
  input parameter self :: <GMappedFile>;
  c-name: "g_mapped_file_free";
end;

define C-function g-mapped-file-get-contents
  input parameter self :: <GMappedFile>;
  result res :: <C-string>;
  c-name: "g_mapped_file_get_contents";
end;

define C-function g-mapped-file-get-length
  input parameter self :: <GMappedFile>;
  result res :: <C-unsigned-long>;
  c-name: "g_mapped_file_get_length";
end;

define C-function g-mapped-file-unref
  input parameter self :: <GMappedFile>;
  c-name: "g_mapped_file_unref";
end;

define constant $g-markup-collect-invalid = 0;
define constant $g-markup-collect-string = 1;
define constant $g-markup-collect-strdup = 2;
define constant $g-markup-collect-boolean = 3;
define constant $g-markup-collect-tristate = 4;
define constant $g-markup-collect-optional = 65536;
define constant <GMarkupCollectType> = <C-int>;
define C-pointer-type <GMarkupCollectType*> => <GMarkupCollectType>;

define constant $g-markup-error-bad-utf8 = 0;
define constant $g-markup-error-empty = 1;
define constant $g-markup-error-parse = 2;
define constant $g-markup-error-unknown-element = 3;
define constant $g-markup-error-unknown-attribute = 4;
define constant $g-markup-error-invalid-content = 5;
define constant $g-markup-error-missing-attribute = 6;
define constant <GMarkupError> = <C-int>;
define C-pointer-type <GMarkupError*> => <GMarkupError>;

define C-struct <_GMarkupParseContext>
  pointer-type-name: <GMarkupParseContext>;
end C-struct;

define C-function g-markup-parse-context-end-parse
  input parameter self :: <GMarkupParseContext>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_markup_parse_context_end_parse";
end;

define C-function g-markup-parse-context-free
  input parameter self :: <GMarkupParseContext>;
  c-name: "g_markup_parse_context_free";
end;

define C-function g-markup-parse-context-get-element
  input parameter self :: <GMarkupParseContext>;
  result res :: <C-string>;
  c-name: "g_markup_parse_context_get_element";
end;

define C-function g-markup-parse-context-get-position
  input parameter self :: <GMarkupParseContext>;
  input parameter line_number_ :: <C-signed-int*>;
  input parameter char_number_ :: <C-signed-int*>;
  c-name: "g_markup_parse_context_get_position";
end;

define C-function g-markup-parse-context-parse
  input parameter self :: <GMarkupParseContext>;
  input parameter text_ :: <C-string>;
  input parameter text_len_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_markup_parse_context_parse";
end;

define C-function g-markup-parse-context-push
  input parameter self :: <GMarkupParseContext>;
  input parameter parser_ :: <GMarkupParser>;
  input parameter user_data_ :: <C-void*>;
  c-name: "g_markup_parse_context_push";
end;

define constant $g-markup-do-not-use-this-unsupported-flag = 1;
define constant $g-markup-treat-cdata-as-text = 2;
define constant $g-markup-prefix-error-position = 4;
define constant <GMarkupParseFlags> = <C-int>;
define C-pointer-type <GMarkupParseFlags*> => <GMarkupParseFlags>;

define C-struct <_GMarkupParser>
  constant slot g-markup-parser-start-element :: <C-function-pointer>;
  constant slot g-markup-parser-end-element :: <C-function-pointer>;
  constant slot g-markup-parser-text :: <C-function-pointer>;
  constant slot g-markup-parser-passthrough :: <C-function-pointer>;
  constant slot g-markup-parser-error :: <C-function-pointer>;
  pointer-type-name: <GMarkupParser>;
end C-struct;

define C-struct <_GMatchInfo>
  pointer-type-name: <GMatchInfo>;
end C-struct;

define C-function g-match-info-expand-references
  input parameter self :: <GMatchInfo>;
  input parameter string_to_expand_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_match_info_expand_references";
end;

define C-function g-match-info-fetch
  input parameter self :: <GMatchInfo>;
  input parameter match_num_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_match_info_fetch";
end;

define C-function g-match-info-fetch-named
  input parameter self :: <GMatchInfo>;
  input parameter name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_match_info_fetch_named";
end;

define C-function g-match-info-fetch-named-pos
  input parameter self :: <GMatchInfo>;
  input parameter name_ :: <C-string>;
  output parameter start_pos_ :: <C-signed-int*>;
  output parameter end_pos_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "g_match_info_fetch_named_pos";
end;

define C-function g-match-info-fetch-pos
  input parameter self :: <GMatchInfo>;
  input parameter match_num_ :: <C-signed-int>;
  output parameter start_pos_ :: <C-signed-int*>;
  output parameter end_pos_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "g_match_info_fetch_pos";
end;

define C-function g-match-info-free
  input parameter self :: <GMatchInfo>;
  c-name: "g_match_info_free";
end;

define C-function g-match-info-get-match-count
  input parameter self :: <GMatchInfo>;
  result res :: <C-signed-int>;
  c-name: "g_match_info_get_match_count";
end;

define C-function g-match-info-get-regex
  input parameter self :: <GMatchInfo>;
  result res :: <GRegex>;
  c-name: "g_match_info_get_regex";
end;

define C-function g-match-info-get-string
  input parameter self :: <GMatchInfo>;
  result res :: <C-string>;
  c-name: "g_match_info_get_string";
end;

define C-function g-match-info-is-partial-match
  input parameter self :: <GMatchInfo>;
  result res :: <C-boolean>;
  c-name: "g_match_info_is_partial_match";
end;

define C-function g-match-info-matches
  input parameter self :: <GMatchInfo>;
  result res :: <C-boolean>;
  c-name: "g_match_info_matches";
end;

define C-function g-match-info-next
  input parameter self :: <GMatchInfo>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_match_info_next";
end;

define C-function g-match-info-ref
  input parameter self :: <GMatchInfo>;
  result res :: <GMatchInfo>;
  c-name: "g_match_info_ref";
end;

define C-function g-match-info-unref
  input parameter self :: <GMatchInfo>;
  c-name: "g_match_info_unref";
end;

define C-struct <_GMemVTable>
  constant slot g-mem-v-table-malloc :: <C-void*>;
  constant slot g-mem-v-table-realloc :: <C-void*>;
  constant slot g-mem-v-table-free :: <C-function-pointer>;
  constant slot g-mem-v-table-calloc :: <C-void*>;
  constant slot g-mem-v-table-try-malloc :: <C-void*>;
  constant slot g-mem-v-table-try-realloc :: <C-void*>;
  pointer-type-name: <GMemVTable>;
end C-struct;

define C-union <_GMutex>
  constant slot g-mutex-p :: <C-void*>;
  constant slot g-mutex-i :: <C-unsigned-int*>;
  pointer-type-name: <GMutex>;
end C-union;

define C-function g-mutex-clear
  input parameter self :: <GMutex>;
  c-name: "g_mutex_clear";
end;

define C-function g-mutex-init
  input parameter self :: <GMutex>;
  c-name: "g_mutex_init";
end;

define C-function g-mutex-lock
  input parameter self :: <GMutex>;
  c-name: "g_mutex_lock";
end;

define C-function g-mutex-trylock
  input parameter self :: <GMutex>;
  result res :: <C-boolean>;
  c-name: "g_mutex_trylock";
end;

define C-function g-mutex-unlock
  input parameter self :: <GMutex>;
  c-name: "g_mutex_unlock";
end;

define C-struct <_GNode>
  slot g-node-data :: <C-void*>;
  slot g-node-next :: <GNode>;
  slot g-node-prev :: <GNode>;
  slot g-node-parent :: <GNode>;
  slot g-node-children :: <GNode>;
  pointer-type-name: <GNode>;
end C-struct;

define C-function g-node-child-index
  input parameter self :: <GNode>;
  input parameter data_ :: <C-void*>;
  result res :: <C-signed-int>;
  c-name: "g_node_child_index";
end;

define C-function g-node-child-position
  input parameter self :: <GNode>;
  input parameter child_ :: <GNode>;
  result res :: <C-signed-int>;
  c-name: "g_node_child_position";
end;

define C-function g-node-depth
  input parameter self :: <GNode>;
  result res :: <C-unsigned-int>;
  c-name: "g_node_depth";
end;

define C-function g-node-destroy
  input parameter self :: <GNode>;
  c-name: "g_node_destroy";
end;

define C-function g-node-is-ancestor
  input parameter self :: <GNode>;
  input parameter descendant_ :: <GNode>;
  result res :: <C-boolean>;
  c-name: "g_node_is_ancestor";
end;

define C-function g-node-max-height
  input parameter self :: <GNode>;
  result res :: <C-unsigned-int>;
  c-name: "g_node_max_height";
end;

define C-function g-node-n-children
  input parameter self :: <GNode>;
  result res :: <C-unsigned-int>;
  c-name: "g_node_n_children";
end;

define C-function g-node-n-nodes
  input parameter self :: <GNode>;
  input parameter flags_ :: <GTraverseFlags>;
  result res :: <C-unsigned-int>;
  c-name: "g_node_n_nodes";
end;

define C-function g-node-reverse-children
  input parameter self :: <GNode>;
  c-name: "g_node_reverse_children";
end;

define C-function g-node-unlink
  input parameter self :: <GNode>;
  c-name: "g_node_unlink";
end;

define constant $g-normalize-default = 0;
define constant $g-normalize-nfd = 0;
define constant $g-normalize-default-compose = 1;
define constant $g-normalize-nfc = 1;
define constant $g-normalize-all = 2;
define constant $g-normalize-nfkd = 2;
define constant $g-normalize-all-compose = 3;
define constant $g-normalize-nfkc = 3;
define constant <GNormalizeMode> = <C-int>;
define C-pointer-type <GNormalizeMode*> => <GNormalizeMode>;

define constant $option-remaining = "";

define C-struct <_GOnce>
  slot g-once-status :: <GOnceStatus>;
  slot g-once-retval :: <C-void*>;
  pointer-type-name: <GOnce>;
end C-struct;

define C-function g-once-init-enter
  input parameter location_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_once_init_enter";
end;

define C-function g-once-init-leave
  input parameter location_ :: <C-void*>;
  input parameter result_ :: <C-unsigned-long>;
  c-name: "g_once_init_leave";
end;

define constant $g-once-status-notcalled = 0;
define constant $g-once-status-progress = 1;
define constant $g-once-status-ready = 2;
define constant <GOnceStatus> = <C-int>;
define C-pointer-type <GOnceStatus*> => <GOnceStatus>;

define constant $g-option-arg-none = 0;
define constant $g-option-arg-string = 1;
define constant $g-option-arg-int = 2;
define constant $g-option-arg-callback = 3;
define constant $g-option-arg-filename = 4;
define constant $g-option-arg-string-array = 5;
define constant $g-option-arg-filename-array = 6;
define constant $g-option-arg-double = 7;
define constant $g-option-arg-int64 = 8;
define constant <GOptionArg> = <C-int>;
define C-pointer-type <GOptionArg*> => <GOptionArg>;

define C-struct <_GOptionContext>
  pointer-type-name: <GOptionContext>;
end C-struct;

define C-function g-option-context-add-group
  input parameter self :: <GOptionContext>;
  input parameter group_ :: <GOptionGroup>;
  c-name: "g_option_context_add_group";
end;

define C-function g-option-context-add-main-entries
  input parameter self :: <GOptionContext>;
  input parameter entries_ :: <GOptionEntry>;
  input parameter translation_domain_ :: <C-string>;
  c-name: "g_option_context_add_main_entries";
end;

define C-function g-option-context-free
  input parameter self :: <GOptionContext>;
  c-name: "g_option_context_free";
end;

define C-function g-option-context-get-description
  input parameter self :: <GOptionContext>;
  result res :: <C-string>;
  c-name: "g_option_context_get_description";
end;

define C-function g-option-context-get-help
  input parameter self :: <GOptionContext>;
  input parameter main_help_ :: <C-boolean>;
  input parameter group_ :: <GOptionGroup>;
  result res :: <C-string>;
  c-name: "g_option_context_get_help";
end;

define C-function g-option-context-get-help-enabled
  input parameter self :: <GOptionContext>;
  result res :: <C-boolean>;
  c-name: "g_option_context_get_help_enabled";
end;

define C-function g-option-context-get-ignore-unknown-options
  input parameter self :: <GOptionContext>;
  result res :: <C-boolean>;
  c-name: "g_option_context_get_ignore_unknown_options";
end;

define C-function g-option-context-get-summary
  input parameter self :: <GOptionContext>;
  result res :: <C-string>;
  c-name: "g_option_context_get_summary";
end;

define C-function g-option-context-parse
  input parameter self :: <GOptionContext>;
  input output parameter argc_ :: <C-signed-int*>;
  input output parameter argv_ :: <C-string*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_option_context_parse";
end;

define C-function g-option-context-set-description
  input parameter self :: <GOptionContext>;
  input parameter description_ :: <C-string>;
  c-name: "g_option_context_set_description";
end;

define C-function g-option-context-set-help-enabled
  input parameter self :: <GOptionContext>;
  input parameter help_enabled_ :: <C-boolean>;
  c-name: "g_option_context_set_help_enabled";
end;

define C-function g-option-context-set-ignore-unknown-options
  input parameter self :: <GOptionContext>;
  input parameter ignore_unknown_ :: <C-boolean>;
  c-name: "g_option_context_set_ignore_unknown_options";
end;

define C-function g-option-context-set-main-group
  input parameter self :: <GOptionContext>;
  input parameter group_ :: <GOptionGroup>;
  c-name: "g_option_context_set_main_group";
end;

define C-function g-option-context-set-summary
  input parameter self :: <GOptionContext>;
  input parameter summary_ :: <C-string>;
  c-name: "g_option_context_set_summary";
end;

define C-function g-option-context-set-translate-func
  input parameter self :: <GOptionContext>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_notify_ :: <C-function-pointer>;
  c-name: "g_option_context_set_translate_func";
end;

define C-function g-option-context-set-translation-domain
  input parameter self :: <GOptionContext>;
  input parameter domain_ :: <C-string>;
  c-name: "g_option_context_set_translation_domain";
end;

define C-struct <_GOptionEntry>
  slot g-option-entry-long-name :: <C-string>;
  slot g-option-entry-short-name :: <C-unsigned-char>;
  slot g-option-entry-flags :: <C-signed-int>;
  slot g-option-entry-arg :: <GOptionArg>;
  slot g-option-entry-arg-data :: <C-void*>;
  slot g-option-entry-description :: <C-string>;
  slot g-option-entry-arg-description :: <C-string>;
  pointer-type-name: <GOptionEntry>;
end C-struct;

define constant $g-option-error-unknown-option = 0;
define constant $g-option-error-bad-value = 1;
define constant $g-option-error-failed = 2;
define constant <GOptionError> = <C-int>;
define C-pointer-type <GOptionError*> => <GOptionError>;

define constant $g-option-flag-hidden = 1;
define constant $g-option-flag-in-main = 2;
define constant $g-option-flag-reverse = 4;
define constant $g-option-flag-no-arg = 8;
define constant $g-option-flag-filename = 16;
define constant $g-option-flag-optional-arg = 32;
define constant $g-option-flag-noalias = 64;
define constant <GOptionFlags> = <C-int>;
define C-pointer-type <GOptionFlags*> => <GOptionFlags>;

define C-struct <_GOptionGroup>
  pointer-type-name: <GOptionGroup>;
end C-struct;

define C-function g-option-group-add-entries
  input parameter self :: <GOptionGroup>;
  input parameter entries_ :: <GOptionEntry>;
  c-name: "g_option_group_add_entries";
end;

define C-function g-option-group-free
  input parameter self :: <GOptionGroup>;
  c-name: "g_option_group_free";
end;

define C-function g-option-group-set-translate-func
  input parameter self :: <GOptionGroup>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_notify_ :: <C-function-pointer>;
  c-name: "g_option_group_set_translate_func";
end;

define C-function g-option-group-set-translation-domain
  input parameter self :: <GOptionGroup>;
  input parameter domain_ :: <C-string>;
  c-name: "g_option_group_set_translation_domain";
end;

define constant $pdp-endian = 3412;

define constant $pi = 3.1415930d0;

define constant $pi-2 = 1.5707960d0;

define constant $pi-4 = 0.7853980d0;

define constant $pollfd-format = "%#I64x";

define constant $priority-default = 0;

define constant $priority-default-idle = 200;

define constant $priority-high = -100;

define constant $priority-high-idle = 100;

define constant $priority-low = 300;

define C-struct <_GPatternSpec>
  pointer-type-name: <GPatternSpec>;
end C-struct;

define C-function g-pattern-spec-equal
  input parameter self :: <GPatternSpec>;
  input parameter pspec2_ :: <GPatternSpec>;
  result res :: <C-boolean>;
  c-name: "g_pattern_spec_equal";
end;

define C-function g-pattern-spec-free
  input parameter self :: <GPatternSpec>;
  c-name: "g_pattern_spec_free";
end;

define C-struct <_GPollFD>
  slot g-poll-fd-fd :: <C-signed-int>;
  slot g-poll-fd-events :: <C-unsigned-short>;
  slot g-poll-fd-revents :: <C-unsigned-short>;
  pointer-type-name: <GPollFD>;
end C-struct;

define C-struct <_GPrivate>
  constant slot g-private-p :: <C-void*>;
  constant slot g-private-notify :: <C-function-pointer>;
  constant slot g-private-future :: <C-void*>;
  pointer-type-name: <GPrivate>;
end C-struct;

define C-function g-private-replace
  input parameter self :: <GPrivate>;
  input parameter value_ :: <C-void*>;
  c-name: "g_private_replace";
end;

define C-function g-private-set
  input parameter self :: <GPrivate>;
  input parameter value_ :: <C-void*>;
  c-name: "g_private_set";
end;

define C-struct <_GPtrArray>
  slot g-ptr-array-pdata :: <C-void*>;
  slot g-ptr-array-len :: <C-unsigned-int>;
  pointer-type-name: <GPtrArray>;
end C-struct;

define C-function g-ptr-array-add
  input parameter array_ :: <GPtrArray>;
  input parameter data_ :: <C-void*>;
  c-name: "g_ptr_array_add";
end;

define C-function g-ptr-array-remove
  input parameter array_ :: <GPtrArray>;
  input parameter data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_ptr_array_remove";
end;

define C-function g-ptr-array-remove-fast
  input parameter array_ :: <GPtrArray>;
  input parameter data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_ptr_array_remove_fast";
end;

define C-function g-ptr-array-remove-range
  input parameter array_ :: <GPtrArray>;
  input parameter index__ :: <C-unsigned-int>;
  input parameter length_ :: <C-unsigned-int>;
  c-name: "g_ptr_array_remove_range";
end;

define C-function g-ptr-array-set-free-func
  input parameter array_ :: <GPtrArray>;
  input parameter element_free_func_ :: <C-function-pointer>;
  c-name: "g_ptr_array_set_free_func";
end;

define C-function g-ptr-array-set-size
  input parameter array_ :: <GPtrArray>;
  input parameter length_ :: <C-signed-int>;
  c-name: "g_ptr_array_set_size";
end;

define C-function g-ptr-array-unref
  input parameter array_ :: <GPtrArray>;
  c-name: "g_ptr_array_unref";
end;

define C-struct <_GQueue>
  slot g-queue-head :: <GList>;
  slot g-queue-tail :: <GList>;
  slot g-queue-length :: <C-unsigned-int>;
  pointer-type-name: <GQueue>;
end C-struct;

define C-function g-queue-clear
  input parameter self :: <GQueue>;
  c-name: "g_queue_clear";
end;

define C-function g-queue-free
  input parameter self :: <GQueue>;
  c-name: "g_queue_free";
end;

define C-function g-queue-free-full
  input parameter self :: <GQueue>;
  input parameter free_func_ :: <C-function-pointer>;
  c-name: "g_queue_free_full";
end;

define C-function g-queue-get-length
  input parameter self :: <GQueue>;
  result res :: <C-unsigned-int>;
  c-name: "g_queue_get_length";
end;

define C-function g-queue-index
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  result res :: <C-signed-int>;
  c-name: "g_queue_index";
end;

define C-function g-queue-init
  input parameter self :: <GQueue>;
  c-name: "g_queue_init";
end;

define C-function g-queue-is-empty
  input parameter self :: <GQueue>;
  result res :: <C-boolean>;
  c-name: "g_queue_is_empty";
end;

define C-function g-queue-push-head
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  c-name: "g_queue_push_head";
end;

define C-function g-queue-push-nth
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  input parameter n_ :: <C-signed-int>;
  c-name: "g_queue_push_nth";
end;

define C-function g-queue-push-tail
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  c-name: "g_queue_push_tail";
end;

define C-function g-queue-remove
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_queue_remove";
end;

define C-function g-queue-remove-all
  input parameter self :: <GQueue>;
  input parameter data_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_queue_remove_all";
end;

define C-function g-queue-reverse
  input parameter self :: <GQueue>;
  c-name: "g_queue_reverse";
end;

define C-struct <_GRWLock>
  constant slot g-rw-lock-p :: <C-void*>;
  constant slot g-rw-lock-i :: <C-unsigned-int*>;
  pointer-type-name: <GRWLock>;
end C-struct;

define C-function g-rw-lock-clear
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_clear";
end;

define C-function g-rw-lock-init
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_init";
end;

define C-function g-rw-lock-reader-lock
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_reader_lock";
end;

define C-function g-rw-lock-reader-trylock
  input parameter self :: <GRWLock>;
  result res :: <C-boolean>;
  c-name: "g_rw_lock_reader_trylock";
end;

define C-function g-rw-lock-reader-unlock
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_reader_unlock";
end;

define C-function g-rw-lock-writer-lock
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_writer_lock";
end;

define C-function g-rw-lock-writer-trylock
  input parameter self :: <GRWLock>;
  result res :: <C-boolean>;
  c-name: "g_rw_lock_writer_trylock";
end;

define C-function g-rw-lock-writer-unlock
  input parameter self :: <GRWLock>;
  c-name: "g_rw_lock_writer_unlock";
end;

define C-struct <_GRand>
  pointer-type-name: <GRand>;
end C-struct;

define C-function g-rand-double
  input parameter self :: <GRand>;
  result res :: <C-double>;
  c-name: "g_rand_double";
end;

define C-function g-rand-double-range
  input parameter self :: <GRand>;
  input parameter begin_ :: <C-double>;
  input parameter end_ :: <C-double>;
  result res :: <C-double>;
  c-name: "g_rand_double_range";
end;

define C-function g-rand-free
  input parameter self :: <GRand>;
  c-name: "g_rand_free";
end;

define C-function g-rand-int
  input parameter self :: <GRand>;
  result res :: <C-unsigned-int>;
  c-name: "g_rand_int";
end;

define C-function g-rand-int-range
  input parameter self :: <GRand>;
  input parameter begin_ :: <C-signed-int>;
  input parameter end_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_rand_int_range";
end;

define C-function g-rand-set-seed
  input parameter self :: <GRand>;
  input parameter seed_ :: <C-unsigned-int>;
  c-name: "g_rand_set_seed";
end;

define C-function g-rand-set-seed-array
  input parameter self :: <GRand>;
  input parameter seed_ :: <C-unsigned-int*>;
  input parameter seed_length_ :: <C-unsigned-int>;
  c-name: "g_rand_set_seed_array";
end;

define C-struct <_GRecMutex>
  constant slot g-rec-mutex-p :: <C-void*>;
  constant slot g-rec-mutex-i :: <C-unsigned-int*>;
  pointer-type-name: <GRecMutex>;
end C-struct;

define C-function g-rec-mutex-clear
  input parameter self :: <GRecMutex>;
  c-name: "g_rec_mutex_clear";
end;

define C-function g-rec-mutex-init
  input parameter self :: <GRecMutex>;
  c-name: "g_rec_mutex_init";
end;

define C-function g-rec-mutex-lock
  input parameter self :: <GRecMutex>;
  c-name: "g_rec_mutex_lock";
end;

define C-function g-rec-mutex-trylock
  input parameter self :: <GRecMutex>;
  result res :: <C-boolean>;
  c-name: "g_rec_mutex_trylock";
end;

define C-function g-rec-mutex-unlock
  input parameter self :: <GRecMutex>;
  c-name: "g_rec_mutex_unlock";
end;

define C-struct <_GRegex>
  pointer-type-name: <GRegex>;
end C-struct;

define C-function g-regex-new
  input parameter pattern_ :: <C-string>;
  input parameter compile_options_ :: <GRegexCompileFlags>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter error_ :: <GError*>;
  result res :: <GRegex>;
  c-name: "g_regex_new";
end;

define C-function g-regex-get-capture-count
  input parameter self :: <GRegex>;
  result res :: <C-signed-int>;
  c-name: "g_regex_get_capture_count";
end;

define C-function g-regex-get-compile-flags
  input parameter self :: <GRegex>;
  result res :: <GRegexCompileFlags>;
  c-name: "g_regex_get_compile_flags";
end;

define C-function g-regex-get-match-flags
  input parameter self :: <GRegex>;
  result res :: <GRegexMatchFlags>;
  c-name: "g_regex_get_match_flags";
end;

define C-function g-regex-get-max-backref
  input parameter self :: <GRegex>;
  result res :: <C-signed-int>;
  c-name: "g_regex_get_max_backref";
end;

define C-function g-regex-get-pattern
  input parameter self :: <GRegex>;
  result res :: <C-string>;
  c-name: "g_regex_get_pattern";
end;

define C-function g-regex-get-string-number
  input parameter self :: <GRegex>;
  input parameter name_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_regex_get_string_number";
end;

define C-function g-regex-match
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter match_info_ :: <GMatchInfo>;
  result res :: <C-boolean>;
  c-name: "g_regex_match";
end;

define C-function g-regex-match-all
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter match_info_ :: <GMatchInfo>;
  result res :: <C-boolean>;
  c-name: "g_regex_match_all";
end;

define C-function g-regex-match-all-full
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string*>;
  input parameter string_len_ :: <C-signed-long>;
  input parameter start_position_ :: <C-signed-int>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter match_info_ :: <GMatchInfo>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_regex_match_all_full";
end;

define C-function g-regex-match-full
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string*>;
  input parameter string_len_ :: <C-signed-long>;
  input parameter start_position_ :: <C-signed-int>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter match_info_ :: <GMatchInfo>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_regex_match_full";
end;

define C-function g-regex-ref
  input parameter self :: <GRegex>;
  result res :: <GRegex>;
  c-name: "g_regex_ref";
end;

define C-function g-regex-replace
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string*>;
  input parameter string_len_ :: <C-signed-long>;
  input parameter start_position_ :: <C-signed-int>;
  input parameter replacement_ :: <C-string>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_regex_replace";
end;

define C-function g-regex-replace-literal
  input parameter self :: <GRegex>;
  input parameter string_ :: <C-string*>;
  input parameter string_len_ :: <C-signed-long>;
  input parameter start_position_ :: <C-signed-int>;
  input parameter replacement_ :: <C-string>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_regex_replace_literal";
end;

define C-function g-regex-unref
  input parameter self :: <GRegex>;
  c-name: "g_regex_unref";
end;

define C-function g-regex-check-replacement
  input parameter replacement_ :: <C-string>;
  output parameter has_references_ :: <C-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_regex_check_replacement";
end;

define C-function g-regex-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_regex_error_quark";
end;

define C-function g-regex-escape-nul
  input parameter string_ :: <C-string>;
  input parameter length_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_regex_escape_nul";
end;

define C-function g-regex-escape-string
  input parameter string_ :: <C-string*>;
  input parameter length_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_regex_escape_string";
end;

define C-function g-regex-match-simple
  input parameter pattern_ :: <C-string>;
  input parameter string_ :: <C-string>;
  input parameter compile_options_ :: <GRegexCompileFlags>;
  input parameter match_options_ :: <GRegexMatchFlags>;
  result res :: <C-boolean>;
  c-name: "g_regex_match_simple";
end;

define constant $g-regex-caseless = 1;
define constant $g-regex-multiline = 2;
define constant $g-regex-dotall = 4;
define constant $g-regex-extended = 8;
define constant $g-regex-anchored = 16;
define constant $g-regex-dollar-endonly = 32;
define constant $g-regex-ungreedy = 512;
define constant $g-regex-raw = 2048;
define constant $g-regex-no-auto-capture = 4096;
define constant $g-regex-optimize = 8192;
define constant $g-regex-dupnames = 524288;
define constant $g-regex-newline-cr = 1048576;
define constant $g-regex-newline-lf = 2097152;
define constant $g-regex-newline-crlf = 3145728;
define constant <GRegexCompileFlags> = <C-int>;
define C-pointer-type <GRegexCompileFlags*> => <GRegexCompileFlags>;

define constant $g-regex-error-compile = 0;
define constant $g-regex-error-optimize = 1;
define constant $g-regex-error-replace = 2;
define constant $g-regex-error-match = 3;
define constant $g-regex-error-internal = 4;
define constant $g-regex-error-stray-backslash = 101;
define constant $g-regex-error-missing-control-char = 102;
define constant $g-regex-error-unrecognized-escape = 103;
define constant $g-regex-error-quantifiers-out-of-order = 104;
define constant $g-regex-error-quantifier-too-big = 105;
define constant $g-regex-error-unterminated-character-class = 106;
define constant $g-regex-error-invalid-escape-in-character-class = 107;
define constant $g-regex-error-range-out-of-order = 108;
define constant $g-regex-error-nothing-to-repeat = 109;
define constant $g-regex-error-unrecognized-character = 112;
define constant $g-regex-error-posix-named-class-outside-class = 113;
define constant $g-regex-error-unmatched-parenthesis = 114;
define constant $g-regex-error-inexistent-subpattern-reference = 115;
define constant $g-regex-error-unterminated-comment = 118;
define constant $g-regex-error-expression-too-large = 120;
define constant $g-regex-error-memory-error = 121;
define constant $g-regex-error-variable-length-lookbehind = 125;
define constant $g-regex-error-malformed-condition = 126;
define constant $g-regex-error-too-many-conditional-branches = 127;
define constant $g-regex-error-assertion-expected = 128;
define constant $g-regex-error-unknown-posix-class-name = 130;
define constant $g-regex-error-posix-collating-elements-not-supported = 131;
define constant $g-regex-error-hex-code-too-large = 134;
define constant $g-regex-error-invalid-condition = 135;
define constant $g-regex-error-single-byte-match-in-lookbehind = 136;
define constant $g-regex-error-infinite-loop = 140;
define constant $g-regex-error-missing-subpattern-name-terminator = 142;
define constant $g-regex-error-duplicate-subpattern-name = 143;
define constant $g-regex-error-malformed-property = 146;
define constant $g-regex-error-unknown-property = 147;
define constant $g-regex-error-subpattern-name-too-long = 148;
define constant $g-regex-error-too-many-subpatterns = 149;
define constant $g-regex-error-invalid-octal-value = 151;
define constant $g-regex-error-too-many-branches-in-define = 154;
define constant $g-regex-error-define-repetion = 155;
define constant $g-regex-error-inconsistent-newline-options = 156;
define constant $g-regex-error-missing-back-reference = 157;
define constant <GRegexError> = <C-int>;
define C-pointer-type <GRegexError*> => <GRegexError>;

define constant $g-regex-match-anchored = 16;
define constant $g-regex-match-notbol = 128;
define constant $g-regex-match-noteol = 256;
define constant $g-regex-match-notempty = 1024;
define constant $g-regex-match-partial = 32768;
define constant $g-regex-match-newline-cr = 1048576;
define constant $g-regex-match-newline-lf = 2097152;
define constant $g-regex-match-newline-crlf = 3145728;
define constant $g-regex-match-newline-any = 4194304;
define constant <GRegexMatchFlags> = <C-int>;
define C-pointer-type <GRegexMatchFlags*> => <GRegexMatchFlags>;

define constant $searchpath-separator = 59;

define constant $searchpath-separator-s = ";";

define constant $sizeof-long = 8;

define constant $sizeof-size-t = 8;

define constant $sizeof-void-p = 8;

define C-struct <_GSList>
  slot g-s-list-data :: <C-void*>;
  slot g-s-list-next :: <GSList>;
  pointer-type-name: <GSList>;
end C-struct;

define constant $sqrt2 = 1.4142140d0;

define constant $str-delimiters = "_-|> <.";

define constant $sysdef-af-inet = 2;

define constant $sysdef-af-inet6 = 10;

define constant $sysdef-af-unix = 1;

define constant $sysdef-msg-dontroute = 4;

define constant $sysdef-msg-oob = 1;

define constant $sysdef-msg-peek = 2;

define C-struct <_GScanner>
  slot g-scanner-user-data :: <C-void*>;
  slot g-scanner-max-parse-errors :: <C-unsigned-int>;
  slot g-scanner-parse-errors :: <C-unsigned-int>;
  slot g-scanner-input-name :: <C-string>;
  slot g-scanner-qdata :: <GData>;
  slot g-scanner-config :: <GScannerConfig>;
  slot g-scanner-token :: <GTokenType>;
  slot g-scanner-value :: <_GTokenValue>;
  slot g-scanner-line :: <C-unsigned-int>;
  slot g-scanner-position :: <C-unsigned-int>;
  slot g-scanner-next-token :: <GTokenType>;
  slot g-scanner-next-value :: <_GTokenValue>;
  slot g-scanner-next-line :: <C-unsigned-int>;
  slot g-scanner-next-position :: <C-unsigned-int>;
  constant slot g-scanner-symbol-table :: <GHashTable>;
  constant slot g-scanner-input-fd :: <C-signed-int>;
  constant slot g-scanner-text :: <C-string>;
  constant slot g-scanner-text-end :: <C-string>;
  constant slot g-scanner-buffer :: <C-string>;
  constant slot g-scanner-scope-id :: <C-unsigned-int>;
  slot g-scanner-msg-handler :: <C-function-pointer>;
  pointer-type-name: <GScanner>;
end C-struct;

define C-function g-scanner-cur-line
  input parameter self :: <GScanner>;
  result res :: <C-unsigned-int>;
  c-name: "g_scanner_cur_line";
end;

define C-function g-scanner-cur-position
  input parameter self :: <GScanner>;
  result res :: <C-unsigned-int>;
  c-name: "g_scanner_cur_position";
end;

define C-function g-scanner-cur-token
  input parameter self :: <GScanner>;
  result res :: <GTokenType>;
  c-name: "g_scanner_cur_token";
end;

define C-function g-scanner-destroy
  input parameter self :: <GScanner>;
  c-name: "g_scanner_destroy";
end;

define C-function g-scanner-eof
  input parameter self :: <GScanner>;
  result res :: <C-boolean>;
  c-name: "g_scanner_eof";
end;

define C-function g-scanner-get-next-token
  input parameter self :: <GScanner>;
  result res :: <GTokenType>;
  c-name: "g_scanner_get_next_token";
end;

define C-function g-scanner-input-file
  input parameter self :: <GScanner>;
  input parameter input_fd_ :: <C-signed-int>;
  c-name: "g_scanner_input_file";
end;

define C-function g-scanner-input-text
  input parameter self :: <GScanner>;
  input parameter text_ :: <C-string>;
  input parameter text_len_ :: <C-unsigned-int>;
  c-name: "g_scanner_input_text";
end;

define C-function g-scanner-peek-next-token
  input parameter self :: <GScanner>;
  result res :: <GTokenType>;
  c-name: "g_scanner_peek_next_token";
end;

define C-function g-scanner-scope-add-symbol
  input parameter self :: <GScanner>;
  input parameter scope_id_ :: <C-unsigned-int>;
  input parameter symbol_ :: <C-string>;
  input parameter value_ :: <C-void*>;
  c-name: "g_scanner_scope_add_symbol";
end;

define C-function g-scanner-scope-remove-symbol
  input parameter self :: <GScanner>;
  input parameter scope_id_ :: <C-unsigned-int>;
  input parameter symbol_ :: <C-string>;
  c-name: "g_scanner_scope_remove_symbol";
end;

define C-function g-scanner-set-scope
  input parameter self :: <GScanner>;
  input parameter scope_id_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_scanner_set_scope";
end;

define C-function g-scanner-sync-file-offset
  input parameter self :: <GScanner>;
  c-name: "g_scanner_sync_file_offset";
end;

define C-function g-scanner-unexp-token
  input parameter self :: <GScanner>;
  input parameter expected_token_ :: <GTokenType>;
  input parameter identifier_spec_ :: <C-string>;
  input parameter symbol_spec_ :: <C-string>;
  input parameter symbol_name_ :: <C-string>;
  input parameter message_ :: <C-string>;
  input parameter is_error_ :: <C-signed-int>;
  c-name: "g_scanner_unexp_token";
end;

define C-struct <_GScannerConfig>
  slot g-scanner-config-cset-skip-characters :: <C-string>;
  slot g-scanner-config-cset-identifier-first :: <C-string>;
  slot g-scanner-config-cset-identifier-nth :: <C-string>;
  slot g-scanner-config-cpair-comment-single :: <C-string>;
  slot g-scanner-config-case-sensitive :: <C-unsigned-int>;
  slot g-scanner-config-skip-comment-multi :: <C-unsigned-int>;
  slot g-scanner-config-skip-comment-single :: <C-unsigned-int>;
  slot g-scanner-config-scan-comment-multi :: <C-unsigned-int>;
  slot g-scanner-config-scan-identifier :: <C-unsigned-int>;
  slot g-scanner-config-scan-identifier-1char :: <C-unsigned-int>;
  slot g-scanner-config-scan-identifier-null :: <C-unsigned-int>;
  slot g-scanner-config-scan-symbols :: <C-unsigned-int>;
  slot g-scanner-config-scan-binary :: <C-unsigned-int>;
  slot g-scanner-config-scan-octal :: <C-unsigned-int>;
  slot g-scanner-config-scan-float :: <C-unsigned-int>;
  slot g-scanner-config-scan-hex :: <C-unsigned-int>;
  slot g-scanner-config-scan-hex-dollar :: <C-unsigned-int>;
  slot g-scanner-config-scan-string-sq :: <C-unsigned-int>;
  slot g-scanner-config-scan-string-dq :: <C-unsigned-int>;
  slot g-scanner-config-numbers-2-int :: <C-unsigned-int>;
  slot g-scanner-config-int-2-float :: <C-unsigned-int>;
  slot g-scanner-config-identifier-2-string :: <C-unsigned-int>;
  slot g-scanner-config-char-2-token :: <C-unsigned-int>;
  slot g-scanner-config-symbol-2-token :: <C-unsigned-int>;
  slot g-scanner-config-scope-0-fallback :: <C-unsigned-int>;
  slot g-scanner-config-store-int64 :: <C-unsigned-int>;
  constant slot g-scanner-config-padding-dummy :: <C-unsigned-int>;
  pointer-type-name: <GScannerConfig>;
end C-struct;

define constant $g-seek-cur = 0;
define constant $g-seek-set = 1;
define constant $g-seek-end = 2;
define constant <GSeekType> = <C-int>;
define C-pointer-type <GSeekType*> => <GSeekType>;

define C-struct <_GSequence>
  pointer-type-name: <GSequence>;
end C-struct;

define C-function g-sequence-free
  input parameter self :: <GSequence>;
  c-name: "g_sequence_free";
end;

define C-function g-sequence-get-length
  input parameter self :: <GSequence>;
  result res :: <C-signed-int>;
  c-name: "g_sequence_get_length";
end;

define C-function g-sequence-move
  input parameter src_ :: <GSequenceIter>;
  input parameter dest_ :: <GSequenceIter>;
  c-name: "g_sequence_move";
end;

define C-function g-sequence-move-range
  input parameter dest_ :: <GSequenceIter>;
  input parameter begin_ :: <GSequenceIter>;
  input parameter end_ :: <GSequenceIter>;
  c-name: "g_sequence_move_range";
end;

define C-function g-sequence-remove
  input parameter iter_ :: <GSequenceIter>;
  c-name: "g_sequence_remove";
end;

define C-function g-sequence-remove-range
  input parameter begin_ :: <GSequenceIter>;
  input parameter end_ :: <GSequenceIter>;
  c-name: "g_sequence_remove_range";
end;

define C-function g-sequence-set
  input parameter iter_ :: <GSequenceIter>;
  input parameter data_ :: <C-void*>;
  c-name: "g_sequence_set";
end;

define C-function g-sequence-swap
  input parameter a_ :: <GSequenceIter>;
  input parameter b_ :: <GSequenceIter>;
  c-name: "g_sequence_swap";
end;

define C-struct <_GSequenceIter>
  pointer-type-name: <GSequenceIter>;
end C-struct;

define C-function g-sequence-iter-compare
  input parameter self :: <GSequenceIter>;
  input parameter b_ :: <GSequenceIter>;
  result res :: <C-signed-int>;
  c-name: "g_sequence_iter_compare";
end;

define C-function g-sequence-iter-get-position
  input parameter self :: <GSequenceIter>;
  result res :: <C-signed-int>;
  c-name: "g_sequence_iter_get_position";
end;

define C-function g-sequence-iter-is-begin
  input parameter self :: <GSequenceIter>;
  result res :: <C-boolean>;
  c-name: "g_sequence_iter_is_begin";
end;

define C-function g-sequence-iter-is-end
  input parameter self :: <GSequenceIter>;
  result res :: <C-boolean>;
  c-name: "g_sequence_iter_is_end";
end;

define constant $g-shell-error-bad-quoting = 0;
define constant $g-shell-error-empty-string = 1;
define constant $g-shell-error-failed = 2;
define constant <GShellError> = <C-int>;
define C-pointer-type <GShellError*> => <GShellError>;

define constant $g-slice-config-always-malloc = 1;
define constant $g-slice-config-bypass-magazines = 2;
define constant $g-slice-config-working-set-msecs = 3;
define constant $g-slice-config-color-increment = 4;
define constant $g-slice-config-chunk-sizes = 5;
define constant $g-slice-config-contention-counter = 6;
define constant <GSliceConfig> = <C-int>;
define C-pointer-type <GSliceConfig*> => <GSliceConfig>;

define C-struct <_GSource>
  constant slot g-source-callback-data :: <C-void*>;
  constant slot g-source-callback-funcs :: <GSourceCallbackFuncs>;
  constant slot g-source-source-funcs :: <GSourceFuncs>;
  constant slot g-source-ref-count :: <C-unsigned-int>;
  constant slot g-source-context :: <GMainContext>;
  constant slot g-source-priority :: <C-signed-int>;
  constant slot g-source-flags :: <C-unsigned-int>;
  constant slot g-source-source-id :: <C-unsigned-int>;
  constant slot g-source-poll-fds :: <GSList>;
  constant slot g-source-prev :: <GSource>;
  constant slot g-source-next :: <GSource>;
  constant slot g-source-name :: <C-string>;
  constant slot g-source-priv :: <GSourcePrivate>;
  pointer-type-name: <GSource>;
end C-struct;

define C-function g-source-new
  input parameter source_funcs_ :: <GSourceFuncs>;
  input parameter struct_size_ :: <C-unsigned-int>;
  result res :: <GSource>;
  c-name: "g_source_new";
end;

define C-function g-source-add-child-source
  input parameter self :: <GSource>;
  input parameter child_source_ :: <GSource>;
  c-name: "g_source_add_child_source";
end;

define C-function g-source-add-poll
  input parameter self :: <GSource>;
  input parameter fd_ :: <GPollFD>;
  c-name: "g_source_add_poll";
end;

define C-function g-source-attach
  input parameter self :: <GSource>;
  input parameter context_ :: <GMainContext>;
  result res :: <C-unsigned-int>;
  c-name: "g_source_attach";
end;

define C-function g-source-destroy
  input parameter self :: <GSource>;
  c-name: "g_source_destroy";
end;

define C-function g-source-get-can-recurse
  input parameter self :: <GSource>;
  result res :: <C-boolean>;
  c-name: "g_source_get_can_recurse";
end;

define C-function g-source-get-context
  input parameter self :: <GSource>;
  result res :: <GMainContext>;
  c-name: "g_source_get_context";
end;

define C-function g-source-get-current-time
  input parameter self :: <GSource>;
  input parameter timeval_ :: <GTimeVal>;
  c-name: "g_source_get_current_time";
end;

define C-function g-source-get-id
  input parameter self :: <GSource>;
  result res :: <C-unsigned-int>;
  c-name: "g_source_get_id";
end;

define C-function g-source-get-name
  input parameter self :: <GSource>;
  result res :: <C-string>;
  c-name: "g_source_get_name";
end;

define C-function g-source-get-priority
  input parameter self :: <GSource>;
  result res :: <C-signed-int>;
  c-name: "g_source_get_priority";
end;

define C-function g-source-get-time
  input parameter self :: <GSource>;
  result res :: <C-signed-long>;
  c-name: "g_source_get_time";
end;

define C-function g-source-is-destroyed
  input parameter self :: <GSource>;
  result res :: <C-boolean>;
  c-name: "g_source_is_destroyed";
end;

define C-function g-source-ref
  input parameter self :: <GSource>;
  result res :: <GSource>;
  c-name: "g_source_ref";
end;

define C-function g-source-remove-child-source
  input parameter self :: <GSource>;
  input parameter child_source_ :: <GSource>;
  c-name: "g_source_remove_child_source";
end;

define C-function g-source-remove-poll
  input parameter self :: <GSource>;
  input parameter fd_ :: <GPollFD>;
  c-name: "g_source_remove_poll";
end;

define C-function g-source-set-callback
  input parameter self :: <GSource>;
  input parameter func_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  c-name: "g_source_set_callback";
end;

define C-function g-source-set-callback-indirect
  input parameter self :: <GSource>;
  input parameter callback_data_ :: <C-void*>;
  input parameter callback_funcs_ :: <GSourceCallbackFuncs>;
  c-name: "g_source_set_callback_indirect";
end;

define C-function g-source-set-can-recurse
  input parameter self :: <GSource>;
  input parameter can_recurse_ :: <C-boolean>;
  c-name: "g_source_set_can_recurse";
end;

define C-function g-source-set-funcs
  input parameter self :: <GSource>;
  input parameter funcs_ :: <GSourceFuncs>;
  c-name: "g_source_set_funcs";
end;

define C-function g-source-set-name
  input parameter self :: <GSource>;
  input parameter name_ :: <C-string>;
  c-name: "g_source_set_name";
end;

define C-function g-source-set-priority
  input parameter self :: <GSource>;
  input parameter priority_ :: <C-signed-int>;
  c-name: "g_source_set_priority";
end;

define C-function g-source-unref
  input parameter self :: <GSource>;
  c-name: "g_source_unref";
end;

define C-function g-source-remove
  input parameter tag_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_source_remove";
end;

define C-function g-source-remove-by-funcs-user-data
  input parameter funcs_ :: <GSourceFuncs>;
  input parameter user_data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_source_remove_by_funcs_user_data";
end;

define C-function g-source-remove-by-user-data
  input parameter user_data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_source_remove_by_user_data";
end;

define C-function g-source-set-name-by-id
  input parameter tag_ :: <C-unsigned-int>;
  input parameter name_ :: <C-string>;
  c-name: "g_source_set_name_by_id";
end;

define C-struct <_GSourceCallbackFuncs>
  constant slot g-source-callback-funcs-ref :: <C-function-pointer>;
  constant slot g-source-callback-funcs-unref :: <C-function-pointer>;
  constant slot g-source-callback-funcs-get :: <C-void*>;
  pointer-type-name: <GSourceCallbackFuncs>;
end C-struct;

define C-struct <_GSourceFuncs>
  constant slot g-source-funcs-prepare :: <C-function-pointer>;
  constant slot g-source-funcs-check :: <C-function-pointer>;
  constant slot g-source-funcs-dispatch :: <C-void*>;
  constant slot g-source-funcs-finalize :: <C-function-pointer>;
  constant slot g-source-funcs-closure-callback :: <C-function-pointer>;
  constant slot g-source-funcs-closure-marshal :: <C-function-pointer>;
  pointer-type-name: <GSourceFuncs>;
end C-struct;

define C-struct <_GSourcePrivate>
  pointer-type-name: <GSourcePrivate>;
end C-struct;

define constant $g-spawn-error-fork = 0;
define constant $g-spawn-error-read = 1;
define constant $g-spawn-error-chdir = 2;
define constant $g-spawn-error-acces = 3;
define constant $g-spawn-error-perm = 4;
define constant $g-spawn-error-too-big = 5;
define constant $g-spawn-error-2big = 5;
define constant $g-spawn-error-noexec = 6;
define constant $g-spawn-error-nametoolong = 7;
define constant $g-spawn-error-noent = 8;
define constant $g-spawn-error-nomem = 9;
define constant $g-spawn-error-notdir = 10;
define constant $g-spawn-error-loop = 11;
define constant $g-spawn-error-txtbusy = 12;
define constant $g-spawn-error-io = 13;
define constant $g-spawn-error-nfile = 14;
define constant $g-spawn-error-mfile = 15;
define constant $g-spawn-error-inval = 16;
define constant $g-spawn-error-isdir = 17;
define constant $g-spawn-error-libbad = 18;
define constant $g-spawn-error-failed = 19;
define constant <GSpawnError> = <C-int>;
define C-pointer-type <GSpawnError*> => <GSpawnError>;

define constant $g-spawn-leave-descriptors-open = 1;
define constant $g-spawn-do-not-reap-child = 2;
define constant $g-spawn-search-path = 4;
define constant $g-spawn-stdout-to-dev-null = 8;
define constant $g-spawn-stderr-to-dev-null = 16;
define constant $g-spawn-child-inherits-stdin = 32;
define constant $g-spawn-file-and-argv-zero = 64;
define constant <GSpawnFlags> = <C-int>;
define C-pointer-type <GSpawnFlags*> => <GSpawnFlags>;

define C-struct <_GStatBuf>
  pointer-type-name: <GStatBuf>;
end C-struct;

define C-struct <_GString>
  slot g-string-str :: <C-string>;
  slot g-string-len :: <C-unsigned-long>;
  slot g-string-allocated-len :: <C-unsigned-long>;
  pointer-type-name: <GString>;
end C-struct;

define C-function g-string-append
  input parameter self :: <GString>;
  input parameter val_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_append";
end;

define C-function g-string-append-c
  input parameter self :: <GString>;
  input parameter c_ :: <C-unsigned-char>;
  result res :: <GString>;
  c-name: "g_string_append_c";
end;

define C-function g-string-append-len
  input parameter self :: <GString>;
  input parameter val_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_append_len";
end;

define C-function g-string-append-unichar
  input parameter self :: <GString>;
  input parameter wc_ :: <C-unsigned-int>;
  result res :: <GString>;
  c-name: "g_string_append_unichar";
end;

define C-function g-string-append-uri-escaped
  input parameter self :: <GString>;
  input parameter unescaped_ :: <C-string>;
  input parameter reserved_chars_allowed_ :: <C-string>;
  input parameter allow_utf8_ :: <C-boolean>;
  result res :: <GString>;
  c-name: "g_string_append_uri_escaped";
end;

define C-function g-string-ascii-down
  input parameter self :: <GString>;
  result res :: <GString>;
  c-name: "g_string_ascii_down";
end;

define C-function g-string-ascii-up
  input parameter self :: <GString>;
  result res :: <GString>;
  c-name: "g_string_ascii_up";
end;

define C-function g-string-assign
  input parameter self :: <GString>;
  input parameter rval_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_assign";
end;

define C-function g-string-down
  input parameter self :: <GString>;
  result res :: <GString>;
  c-name: "g_string_down";
end;

define C-function g-string-equal
  input parameter self :: <GString>;
  input parameter v2_ :: <GString>;
  result res :: <C-boolean>;
  c-name: "g_string_equal";
end;

define C-function g-string-erase
  input parameter self :: <GString>;
  input parameter pos_ :: <C-signed-long>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_erase";
end;

define C-function g-string-free
  input parameter self :: <GString>;
  input parameter free_segment_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "g_string_free";
end;

define C-function g-string-hash
  input parameter self :: <GString>;
  result res :: <C-unsigned-int>;
  c-name: "g_string_hash";
end;

define C-function g-string-insert
  input parameter self :: <GString>;
  input parameter pos_ :: <C-signed-long>;
  input parameter val_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_insert";
end;

define C-function g-string-insert-c
  input parameter self :: <GString>;
  input parameter pos_ :: <C-signed-long>;
  input parameter c_ :: <C-unsigned-char>;
  result res :: <GString>;
  c-name: "g_string_insert_c";
end;

define C-function g-string-insert-len
  input parameter self :: <GString>;
  input parameter pos_ :: <C-signed-long>;
  input parameter val_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_insert_len";
end;

define C-function g-string-insert-unichar
  input parameter self :: <GString>;
  input parameter pos_ :: <C-signed-long>;
  input parameter wc_ :: <C-unsigned-int>;
  result res :: <GString>;
  c-name: "g_string_insert_unichar";
end;

define C-function g-string-overwrite
  input parameter self :: <GString>;
  input parameter pos_ :: <C-unsigned-long>;
  input parameter val_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_overwrite";
end;

define C-function g-string-overwrite-len
  input parameter self :: <GString>;
  input parameter pos_ :: <C-unsigned-long>;
  input parameter val_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_overwrite_len";
end;

define C-function g-string-prepend
  input parameter self :: <GString>;
  input parameter val_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_prepend";
end;

define C-function g-string-prepend-c
  input parameter self :: <GString>;
  input parameter c_ :: <C-unsigned-char>;
  result res :: <GString>;
  c-name: "g_string_prepend_c";
end;

define C-function g-string-prepend-len
  input parameter self :: <GString>;
  input parameter val_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_prepend_len";
end;

define C-function g-string-prepend-unichar
  input parameter self :: <GString>;
  input parameter wc_ :: <C-unsigned-int>;
  result res :: <GString>;
  c-name: "g_string_prepend_unichar";
end;

define C-function g-string-set-size
  input parameter self :: <GString>;
  input parameter len_ :: <C-unsigned-long>;
  result res :: <GString>;
  c-name: "g_string_set_size";
end;

define C-function g-string-truncate
  input parameter self :: <GString>;
  input parameter len_ :: <C-unsigned-long>;
  result res :: <GString>;
  c-name: "g_string_truncate";
end;

define C-function g-string-up
  input parameter self :: <GString>;
  result res :: <GString>;
  c-name: "g_string_up";
end;

define C-struct <_GStringChunk>
  pointer-type-name: <GStringChunk>;
end C-struct;

define C-function g-string-chunk-clear
  input parameter self :: <GStringChunk>;
  c-name: "g_string_chunk_clear";
end;

define C-function g-string-chunk-free
  input parameter self :: <GStringChunk>;
  c-name: "g_string_chunk_free";
end;

define C-function g-string-chunk-insert
  input parameter self :: <GStringChunk>;
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_string_chunk_insert";
end;

define C-function g-string-chunk-insert-const
  input parameter self :: <GStringChunk>;
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_string_chunk_insert_const";
end;

define C-function g-string-chunk-insert-len
  input parameter self :: <GStringChunk>;
  input parameter string_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_string_chunk_insert_len";
end;

define C-struct <_GTestCase>
  pointer-type-name: <GTestCase>;
end C-struct;

define C-struct <_GTestConfig>
  slot g-test-config-test-initialized :: <C-boolean>;
  slot g-test-config-test-quick :: <C-boolean>;
  slot g-test-config-test-perf :: <C-boolean>;
  slot g-test-config-test-verbose :: <C-boolean>;
  slot g-test-config-test-quiet :: <C-boolean>;
  slot g-test-config-test-undefined :: <C-boolean>;
  pointer-type-name: <GTestConfig>;
end C-struct;

define C-struct <_GTestLogBuffer>
  constant slot g-test-log-buffer-data :: <GString>;
  constant slot g-test-log-buffer-msgs :: <GSList>;
  pointer-type-name: <GTestLogBuffer>;
end C-struct;

define C-function g-test-log-buffer-free
  input parameter self :: <GTestLogBuffer>;
  c-name: "g_test_log_buffer_free";
end;

define C-function g-test-log-buffer-push
  input parameter self :: <GTestLogBuffer>;
  input parameter n_bytes_ :: <C-unsigned-int>;
  input parameter bytes_ :: <C-unsigned-char*>;
  c-name: "g_test_log_buffer_push";
end;

define C-struct <_GTestLogMsg>
  slot g-test-log-msg-log-type :: <GTestLogType>;
  slot g-test-log-msg-n-strings :: <C-unsigned-int>;
  slot g-test-log-msg-strings :: <C-string>;
  slot g-test-log-msg-n-nums :: <C-unsigned-int>;
  slot g-test-log-msg-nums :: <C-signed-long*>;
  pointer-type-name: <GTestLogMsg>;
end C-struct;

define C-function g-test-log-msg-free
  input parameter self :: <GTestLogMsg>;
  c-name: "g_test_log_msg_free";
end;

define constant $g-test-log-none = 0;
define constant $g-test-log-error = 1;
define constant $g-test-log-start-binary = 2;
define constant $g-test-log-list-case = 3;
define constant $g-test-log-skip-case = 4;
define constant $g-test-log-start-case = 5;
define constant $g-test-log-stop-case = 6;
define constant $g-test-log-min-result = 7;
define constant $g-test-log-max-result = 8;
define constant $g-test-log-message = 9;
define constant <GTestLogType> = <C-int>;
define C-pointer-type <GTestLogType*> => <GTestLogType>;

define C-struct <_GTestSuite>
  pointer-type-name: <GTestSuite>;
end C-struct;

define C-function g-test-suite-add
  input parameter self :: <GTestSuite>;
  input parameter test_case_ :: <GTestCase>;
  c-name: "g_test_suite_add";
end;

define C-function g-test-suite-add-suite
  input parameter self :: <GTestSuite>;
  input parameter nestedsuite_ :: <GTestSuite>;
  c-name: "g_test_suite_add_suite";
end;

define constant $g-test-trap-silence-stdout = 128;
define constant $g-test-trap-silence-stderr = 256;
define constant $g-test-trap-inherit-stdin = 512;
define constant <GTestTrapFlags> = <C-int>;
define C-pointer-type <GTestTrapFlags*> => <GTestTrapFlags>;

define C-struct <_GThread>
  pointer-type-name: <GThread>;
end C-struct;

define C-function g-thread-unref
  input parameter self :: <GThread>;
  c-name: "g_thread_unref";
end;

define C-function g-thread-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_thread_error_quark";
end;

define C-function g-thread-exit
  input parameter retval_ :: <C-void*>;
  c-name: "g_thread_exit";
end;

define C-function g-thread-yield
  c-name: "g_thread_yield";
end;

define constant $g-thread-error-again = 0;
define constant <GThreadError> = <C-int>;
define C-pointer-type <GThreadError*> => <GThreadError>;

define C-struct <_GThreadPool>
  slot g-thread-pool-func :: <C-function-pointer>;
  slot g-thread-pool-user-data :: <C-void*>;
  slot g-thread-pool-exclusive :: <C-boolean>;
  pointer-type-name: <GThreadPool>;
end C-struct;

define C-function g-thread-pool-free
  input parameter self :: <GThreadPool>;
  input parameter immediate_ :: <C-boolean>;
  input parameter wait__ :: <C-boolean>;
  c-name: "g_thread_pool_free";
end;

define C-function g-thread-pool-get-max-threads
  input parameter self :: <GThreadPool>;
  result res :: <C-signed-int>;
  c-name: "g_thread_pool_get_max_threads";
end;

define C-function g-thread-pool-get-num-threads
  input parameter self :: <GThreadPool>;
  result res :: <C-unsigned-int>;
  c-name: "g_thread_pool_get_num_threads";
end;

define C-function g-thread-pool-push
  input parameter self :: <GThreadPool>;
  input parameter data_ :: <C-void*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_thread_pool_push";
end;

define C-function g-thread-pool-set-max-threads
  input parameter self :: <GThreadPool>;
  input parameter max_threads_ :: <C-signed-int>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_thread_pool_set_max_threads";
end;

define C-function g-thread-pool-unprocessed
  input parameter self :: <GThreadPool>;
  result res :: <C-unsigned-int>;
  c-name: "g_thread_pool_unprocessed";
end;

define C-function g-thread-pool-get-max-idle-time
  result res :: <C-unsigned-int>;
  c-name: "g_thread_pool_get_max_idle_time";
end;

define C-function g-thread-pool-get-max-unused-threads
  result res :: <C-signed-int>;
  c-name: "g_thread_pool_get_max_unused_threads";
end;

define C-function g-thread-pool-get-num-unused-threads
  result res :: <C-unsigned-int>;
  c-name: "g_thread_pool_get_num_unused_threads";
end;

define C-function g-thread-pool-set-max-idle-time
  input parameter interval_ :: <C-unsigned-int>;
  c-name: "g_thread_pool_set_max_idle_time";
end;

define C-function g-thread-pool-set-max-unused-threads
  input parameter max_threads_ :: <C-signed-int>;
  c-name: "g_thread_pool_set_max_unused_threads";
end;

define C-function g-thread-pool-stop-unused-threads
  c-name: "g_thread_pool_stop_unused_threads";
end;

define constant $g-time-type-standard = 0;
define constant $g-time-type-daylight = 1;
define constant $g-time-type-universal = 2;
define constant <GTimeType> = <C-int>;
define C-pointer-type <GTimeType*> => <GTimeType>;

define C-struct <_GTimeVal>
  slot g-time-val-tv-sec :: <C-signed-long>;
  slot g-time-val-tv-usec :: <C-signed-long>;
  pointer-type-name: <GTimeVal>;
end C-struct;

define C-function g-time-val-add
  input parameter self :: <GTimeVal>;
  input parameter microseconds_ :: <C-signed-long>;
  c-name: "g_time_val_add";
end;

define C-function g-time-val-to-iso8601
  input parameter self :: <GTimeVal>;
  result res :: <C-string>;
  c-name: "g_time_val_to_iso8601";
end;

define C-function g-time-val-from-iso8601
  input parameter iso_date_ :: <C-string>;
  output parameter time__ :: <GTimeVal>;
  result res :: <C-boolean>;
  c-name: "g_time_val_from_iso8601";
end;

define C-struct <_GTimeZone>
  pointer-type-name: <GTimeZone>;
end C-struct;

define C-function g-time-zone-adjust-time
  input parameter self :: <GTimeZone>;
  input parameter type_ :: <GTimeType>;
  input parameter time__ :: <C-signed-long*>;
  result res :: <C-signed-int>;
  c-name: "g_time_zone_adjust_time";
end;

define C-function g-time-zone-find-interval
  input parameter self :: <GTimeZone>;
  input parameter type_ :: <GTimeType>;
  input parameter time__ :: <C-signed-long>;
  result res :: <C-signed-int>;
  c-name: "g_time_zone_find_interval";
end;

define C-function g-time-zone-get-abbreviation
  input parameter self :: <GTimeZone>;
  input parameter interval_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_time_zone_get_abbreviation";
end;

define C-function g-time-zone-get-offset
  input parameter self :: <GTimeZone>;
  input parameter interval_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_time_zone_get_offset";
end;

define C-function g-time-zone-is-dst
  input parameter self :: <GTimeZone>;
  input parameter interval_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_time_zone_is_dst";
end;

define C-function g-time-zone-unref
  input parameter self :: <GTimeZone>;
  c-name: "g_time_zone_unref";
end;

define C-struct <_GTimer>
  pointer-type-name: <GTimer>;
end C-struct;

define C-function g-timer-continue
  input parameter self :: <GTimer>;
  c-name: "g_timer_continue";
end;

define C-function g-timer-destroy
  input parameter self :: <GTimer>;
  c-name: "g_timer_destroy";
end;

define C-function g-timer-elapsed
  input parameter self :: <GTimer>;
  input parameter microseconds_ :: <C-unsigned-long*>;
  result res :: <C-double>;
  c-name: "g_timer_elapsed";
end;

define C-function g-timer-reset
  input parameter self :: <GTimer>;
  c-name: "g_timer_reset";
end;

define C-function g-timer-start
  input parameter self :: <GTimer>;
  c-name: "g_timer_start";
end;

define C-function g-timer-stop
  input parameter self :: <GTimer>;
  c-name: "g_timer_stop";
end;

define constant $g-token-eof = 0;
define constant $g-token-left-paren = 40;
define constant $g-token-right-paren = 41;
define constant $g-token-left-curly = 123;
define constant $g-token-right-curly = 125;
define constant $g-token-left-brace = 91;
define constant $g-token-right-brace = 93;
define constant $g-token-equal-sign = 61;
define constant $g-token-comma = 44;
define constant $g-token-none = 256;
define constant $g-token-error = 257;
define constant $g-token-char = 258;
define constant $g-token-binary = 259;
define constant $g-token-octal = 260;
define constant $g-token-int = 261;
define constant $g-token-hex = 262;
define constant $g-token-float = 263;
define constant $g-token-string = 264;
define constant $g-token-symbol = 265;
define constant $g-token-identifier = 266;
define constant $g-token-identifier-null = 267;
define constant $g-token-comment-single = 268;
define constant $g-token-comment-multi = 269;
define constant <GTokenType> = <C-int>;
define C-pointer-type <GTokenType*> => <GTokenType>;

define C-union <_GTokenValue>
  slot g-token-value-v-symbol :: <C-void*>;
  slot g-token-value-v-identifier :: <C-string>;
  slot g-token-value-v-binary :: <C-unsigned-long>;
  slot g-token-value-v-octal :: <C-unsigned-long>;
  slot g-token-value-v-int :: <C-unsigned-long>;
  slot g-token-value-v-int64 :: <C-unsigned-long>;
  slot g-token-value-v-float :: <C-double>;
  slot g-token-value-v-hex :: <C-unsigned-long>;
  slot g-token-value-v-string :: <C-string>;
  slot g-token-value-v-comment :: <C-string>;
  slot g-token-value-v-char :: <C-unsigned-char>;
  slot g-token-value-v-error :: <C-unsigned-int>;
  pointer-type-name: <GTokenValue>;
end C-union;

define C-struct <_GTrashStack>
  slot g-trash-stack-next :: <GTrashStack>;
  pointer-type-name: <GTrashStack>;
end C-struct;

define C-function g-trash-stack-height
  input parameter stack_p_ :: <GTrashStack>;
  result res :: <C-unsigned-int>;
  c-name: "g_trash_stack_height";
end;

define C-function g-trash-stack-push
  input parameter stack_p_ :: <GTrashStack>;
  input parameter data_p_ :: <C-void*>;
  c-name: "g_trash_stack_push";
end;

define constant $g-traverse-leaves = 1;
define constant $g-traverse-non-leaves = 2;
define constant $g-traverse-all = 3;
define constant $g-traverse-mask = 3;
define constant $g-traverse-leafs = 1;
define constant $g-traverse-non-leafs = 2;
define constant <GTraverseFlags> = <C-int>;
define C-pointer-type <GTraverseFlags*> => <GTraverseFlags>;

define constant $g-in-order = 0;
define constant $g-pre-order = 1;
define constant $g-post-order = 2;
define constant $g-level-order = 3;
define constant <GTraverseType> = <C-int>;
define C-pointer-type <GTraverseType*> => <GTraverseType>;

define C-struct <_GTree>
  pointer-type-name: <GTree>;
end C-struct;

define C-function g-tree-destroy
  input parameter self :: <GTree>;
  c-name: "g_tree_destroy";
end;

define C-function g-tree-height
  input parameter self :: <GTree>;
  result res :: <C-signed-int>;
  c-name: "g_tree_height";
end;

define C-function g-tree-insert
  input parameter self :: <GTree>;
  input parameter key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  c-name: "g_tree_insert";
end;

define C-function g-tree-lookup-extended
  input parameter self :: <GTree>;
  input parameter lookup_key_ :: <C-void*>;
  input parameter orig_key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_tree_lookup_extended";
end;

define C-function g-tree-nnodes
  input parameter self :: <GTree>;
  result res :: <C-signed-int>;
  c-name: "g_tree_nnodes";
end;

define C-function g-tree-remove
  input parameter self :: <GTree>;
  input parameter key_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_tree_remove";
end;

define C-function g-tree-replace
  input parameter self :: <GTree>;
  input parameter key_ :: <C-void*>;
  input parameter value_ :: <C-void*>;
  c-name: "g_tree_replace";
end;

define C-function g-tree-steal
  input parameter self :: <GTree>;
  input parameter key_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_tree_steal";
end;

define C-function g-tree-unref
  input parameter self :: <GTree>;
  c-name: "g_tree_unref";
end;

define constant $unichar-max-decomposition-length = 18;

define constant $uri-reserved-chars-generic-delimiters = ":/?#[]@";

define constant $uri-reserved-chars-subcomponent-delimiters = "!$&'()*+,;=";

define constant $usec-per-sec = 1000000;

define constant $g-unicode-break-mandatory = 0;
define constant $g-unicode-break-carriage-return = 1;
define constant $g-unicode-break-line-feed = 2;
define constant $g-unicode-break-combining-mark = 3;
define constant $g-unicode-break-surrogate = 4;
define constant $g-unicode-break-zero-width-space = 5;
define constant $g-unicode-break-inseparable = 6;
define constant $g-unicode-break-non-breaking-glue = 7;
define constant $g-unicode-break-contingent = 8;
define constant $g-unicode-break-space = 9;
define constant $g-unicode-break-after = 10;
define constant $g-unicode-break-before = 11;
define constant $g-unicode-break-before-and-after = 12;
define constant $g-unicode-break-hyphen = 13;
define constant $g-unicode-break-non-starter = 14;
define constant $g-unicode-break-open-punctuation = 15;
define constant $g-unicode-break-close-punctuation = 16;
define constant $g-unicode-break-quotation = 17;
define constant $g-unicode-break-exclamation = 18;
define constant $g-unicode-break-ideographic = 19;
define constant $g-unicode-break-numeric = 20;
define constant $g-unicode-break-infix-separator = 21;
define constant $g-unicode-break-symbol = 22;
define constant $g-unicode-break-alphabetic = 23;
define constant $g-unicode-break-prefix = 24;
define constant $g-unicode-break-postfix = 25;
define constant $g-unicode-break-complex-context = 26;
define constant $g-unicode-break-ambiguous = 27;
define constant $g-unicode-break-unknown = 28;
define constant $g-unicode-break-next-line = 29;
define constant $g-unicode-break-word-joiner = 30;
define constant $g-unicode-break-hangul-l-jamo = 31;
define constant $g-unicode-break-hangul-v-jamo = 32;
define constant $g-unicode-break-hangul-t-jamo = 33;
define constant $g-unicode-break-hangul-lv-syllable = 34;
define constant $g-unicode-break-hangul-lvt-syllable = 35;
define constant $g-unicode-break-close-paranthesis = 36;
define constant $g-unicode-break-conditional-japanese-starter = 37;
define constant $g-unicode-break-hebrew-letter = 38;
define constant <GUnicodeBreakType> = <C-int>;
define C-pointer-type <GUnicodeBreakType*> => <GUnicodeBreakType>;

define constant $g-unicode-script-invalid-code = -1;
define constant $g-unicode-script-common = 0;
define constant $g-unicode-script-inherited = 1;
define constant $g-unicode-script-arabic = 2;
define constant $g-unicode-script-armenian = 3;
define constant $g-unicode-script-bengali = 4;
define constant $g-unicode-script-bopomofo = 5;
define constant $g-unicode-script-cherokee = 6;
define constant $g-unicode-script-coptic = 7;
define constant $g-unicode-script-cyrillic = 8;
define constant $g-unicode-script-deseret = 9;
define constant $g-unicode-script-devanagari = 10;
define constant $g-unicode-script-ethiopic = 11;
define constant $g-unicode-script-georgian = 12;
define constant $g-unicode-script-gothic = 13;
define constant $g-unicode-script-greek = 14;
define constant $g-unicode-script-gujarati = 15;
define constant $g-unicode-script-gurmukhi = 16;
define constant $g-unicode-script-han = 17;
define constant $g-unicode-script-hangul = 18;
define constant $g-unicode-script-hebrew = 19;
define constant $g-unicode-script-hiragana = 20;
define constant $g-unicode-script-kannada = 21;
define constant $g-unicode-script-katakana = 22;
define constant $g-unicode-script-khmer = 23;
define constant $g-unicode-script-lao = 24;
define constant $g-unicode-script-latin = 25;
define constant $g-unicode-script-malayalam = 26;
define constant $g-unicode-script-mongolian = 27;
define constant $g-unicode-script-myanmar = 28;
define constant $g-unicode-script-ogham = 29;
define constant $g-unicode-script-old-italic = 30;
define constant $g-unicode-script-oriya = 31;
define constant $g-unicode-script-runic = 32;
define constant $g-unicode-script-sinhala = 33;
define constant $g-unicode-script-syriac = 34;
define constant $g-unicode-script-tamil = 35;
define constant $g-unicode-script-telugu = 36;
define constant $g-unicode-script-thaana = 37;
define constant $g-unicode-script-thai = 38;
define constant $g-unicode-script-tibetan = 39;
define constant $g-unicode-script-canadian-aboriginal = 40;
define constant $g-unicode-script-yi = 41;
define constant $g-unicode-script-tagalog = 42;
define constant $g-unicode-script-hanunoo = 43;
define constant $g-unicode-script-buhid = 44;
define constant $g-unicode-script-tagbanwa = 45;
define constant $g-unicode-script-braille = 46;
define constant $g-unicode-script-cypriot = 47;
define constant $g-unicode-script-limbu = 48;
define constant $g-unicode-script-osmanya = 49;
define constant $g-unicode-script-shavian = 50;
define constant $g-unicode-script-linear-b = 51;
define constant $g-unicode-script-tai-le = 52;
define constant $g-unicode-script-ugaritic = 53;
define constant $g-unicode-script-new-tai-lue = 54;
define constant $g-unicode-script-buginese = 55;
define constant $g-unicode-script-glagolitic = 56;
define constant $g-unicode-script-tifinagh = 57;
define constant $g-unicode-script-syloti-nagri = 58;
define constant $g-unicode-script-old-persian = 59;
define constant $g-unicode-script-kharoshthi = 60;
define constant $g-unicode-script-unknown = 61;
define constant $g-unicode-script-balinese = 62;
define constant $g-unicode-script-cuneiform = 63;
define constant $g-unicode-script-phoenician = 64;
define constant $g-unicode-script-phags-pa = 65;
define constant $g-unicode-script-nko = 66;
define constant $g-unicode-script-kayah-li = 67;
define constant $g-unicode-script-lepcha = 68;
define constant $g-unicode-script-rejang = 69;
define constant $g-unicode-script-sundanese = 70;
define constant $g-unicode-script-saurashtra = 71;
define constant $g-unicode-script-cham = 72;
define constant $g-unicode-script-ol-chiki = 73;
define constant $g-unicode-script-vai = 74;
define constant $g-unicode-script-carian = 75;
define constant $g-unicode-script-lycian = 76;
define constant $g-unicode-script-lydian = 77;
define constant $g-unicode-script-avestan = 78;
define constant $g-unicode-script-bamum = 79;
define constant $g-unicode-script-egyptian-hieroglyphs = 80;
define constant $g-unicode-script-imperial-aramaic = 81;
define constant $g-unicode-script-inscriptional-pahlavi = 82;
define constant $g-unicode-script-inscriptional-parthian = 83;
define constant $g-unicode-script-javanese = 84;
define constant $g-unicode-script-kaithi = 85;
define constant $g-unicode-script-lisu = 86;
define constant $g-unicode-script-meetei-mayek = 87;
define constant $g-unicode-script-old-south-arabian = 88;
define constant $g-unicode-script-old-turkic = 89;
define constant $g-unicode-script-samaritan = 90;
define constant $g-unicode-script-tai-tham = 91;
define constant $g-unicode-script-tai-viet = 92;
define constant $g-unicode-script-batak = 93;
define constant $g-unicode-script-brahmi = 94;
define constant $g-unicode-script-mandaic = 95;
define constant $g-unicode-script-chakma = 96;
define constant $g-unicode-script-meroitic-cursive = 97;
define constant $g-unicode-script-meroitic-hieroglyphs = 98;
define constant $g-unicode-script-miao = 99;
define constant $g-unicode-script-sharada = 100;
define constant $g-unicode-script-sora-sompeng = 101;
define constant $g-unicode-script-takri = 102;
define constant <GUnicodeScript> = <C-int>;
define C-pointer-type <GUnicodeScript*> => <GUnicodeScript>;

define constant $g-unicode-control = 0;
define constant $g-unicode-format = 1;
define constant $g-unicode-unassigned = 2;
define constant $g-unicode-private-use = 3;
define constant $g-unicode-surrogate = 4;
define constant $g-unicode-lowercase-letter = 5;
define constant $g-unicode-modifier-letter = 6;
define constant $g-unicode-other-letter = 7;
define constant $g-unicode-titlecase-letter = 8;
define constant $g-unicode-uppercase-letter = 9;
define constant $g-unicode-spacing-mark = 10;
define constant $g-unicode-enclosing-mark = 11;
define constant $g-unicode-non-spacing-mark = 12;
define constant $g-unicode-decimal-number = 13;
define constant $g-unicode-letter-number = 14;
define constant $g-unicode-other-number = 15;
define constant $g-unicode-connect-punctuation = 16;
define constant $g-unicode-dash-punctuation = 17;
define constant $g-unicode-close-punctuation = 18;
define constant $g-unicode-final-punctuation = 19;
define constant $g-unicode-initial-punctuation = 20;
define constant $g-unicode-other-punctuation = 21;
define constant $g-unicode-open-punctuation = 22;
define constant $g-unicode-currency-symbol = 23;
define constant $g-unicode-modifier-symbol = 24;
define constant $g-unicode-math-symbol = 25;
define constant $g-unicode-other-symbol = 26;
define constant $g-unicode-line-separator = 27;
define constant $g-unicode-paragraph-separator = 28;
define constant $g-unicode-space-separator = 29;
define constant <GUnicodeType> = <C-int>;
define C-pointer-type <GUnicodeType*> => <GUnicodeType>;

define constant $g-user-directory-desktop = 0;
define constant $g-user-directory-documents = 1;
define constant $g-user-directory-download = 2;
define constant $g-user-directory-music = 3;
define constant $g-user-directory-pictures = 4;
define constant $g-user-directory-public-share = 5;
define constant $g-user-directory-templates = 6;
define constant $g-user-directory-videos = 7;
define constant $g-user-n-directories = 8;
define constant <GUserDirectory> = <C-int>;
define C-pointer-type <GUserDirectory*> => <GUserDirectory>;

define constant $va-copy-as-array = 1;

define C-struct <_GVariant>
  pointer-type-name: <GVariant>;
end C-struct;

define C-function g-variant-new-array
  input parameter child_type_ :: <GVariantType>;
  input parameter children_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_children_ :: <C-unsigned-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_array";
end;

define C-function g-variant-new-boolean
  input parameter value_ :: <C-boolean>;
  result res :: <GVariant>;
  c-name: "g_variant_new_boolean";
end;

define C-function g-variant-new-byte
  input parameter value_ :: <C-unsigned-char>;
  result res :: <GVariant>;
  c-name: "g_variant_new_byte";
end;

define C-function g-variant-new-bytestring
  input parameter string_ :: <C-unsigned-char*>;
  result res :: <GVariant>;
  c-name: "g_variant_new_bytestring";
end;

define C-function g-variant-new-bytestring-array
  input parameter strv_ :: <C-string*>;
  input parameter length_ :: <C-signed-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_bytestring_array";
end;

define C-function g-variant-new-dict-entry
  input parameter key_ :: <GVariant>;
  input parameter value_ :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_new_dict_entry";
end;

define C-function g-variant-new-double
  input parameter value_ :: <C-double>;
  result res :: <GVariant>;
  c-name: "g_variant_new_double";
end;

define C-function g-variant-new-fixed-array
  input parameter element_type_ :: <GVariantType>;
  input parameter elements_ :: <C-void*>;
  input parameter n_elements_ :: <C-unsigned-long>;
  input parameter element_size_ :: <C-unsigned-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_fixed_array";
end;

define C-function g-variant-new-from-data
  input parameter type_ :: <GVariantType>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter size_ :: <C-unsigned-long>;
  input parameter trusted_ :: <C-boolean>;
  input parameter notify_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  result res :: <GVariant>;
  c-name: "g_variant_new_from_data";
end;

define C-function g-variant-new-handle
  input parameter value_ :: <C-signed-int>;
  result res :: <GVariant>;
  c-name: "g_variant_new_handle";
end;

define C-function g-variant-new-int16
  input parameter value_ :: <C-signed-short>;
  result res :: <GVariant>;
  c-name: "g_variant_new_int16";
end;

define C-function g-variant-new-int32
  input parameter value_ :: <C-signed-int>;
  result res :: <GVariant>;
  c-name: "g_variant_new_int32";
end;

define C-function g-variant-new-int64
  input parameter value_ :: <C-signed-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_int64";
end;

define C-function g-variant-new-maybe
  input parameter child_type_ :: <GVariantType>;
  input parameter child_ :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_new_maybe";
end;

define C-function g-variant-new-object-path
  input parameter object_path_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_variant_new_object_path";
end;

define C-function g-variant-new-objv
  input parameter strv_ :: <C-string*>;
  input parameter length_ :: <C-signed-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_objv";
end;

define C-function g-variant-new-signature
  input parameter signature_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_variant_new_signature";
end;

define C-function g-variant-new-string
  input parameter string_ :: <C-string>;
  result res :: <GVariant>;
  c-name: "g_variant_new_string";
end;

define C-function g-variant-new-strv
  input parameter strv_ :: <C-string*>;
  input parameter length_ :: <C-signed-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_strv";
end;

define C-function g-variant-new-tuple
  input parameter children_ :: <C-unsigned-char*> /* Not supported */;
  input parameter n_children_ :: <C-unsigned-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_tuple";
end;

define C-function g-variant-new-uint16
  input parameter value_ :: <C-unsigned-short>;
  result res :: <GVariant>;
  c-name: "g_variant_new_uint16";
end;

define C-function g-variant-new-uint32
  input parameter value_ :: <C-unsigned-int>;
  result res :: <GVariant>;
  c-name: "g_variant_new_uint32";
end;

define C-function g-variant-new-uint64
  input parameter value_ :: <C-unsigned-long>;
  result res :: <GVariant>;
  c-name: "g_variant_new_uint64";
end;

define C-function g-variant-new-variant
  input parameter value_ :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_new_variant";
end;

define C-function g-variant-byteswap
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_byteswap";
end;

define C-function g-variant-classify
  input parameter self :: <GVariant>;
  result res :: <GVariantClass>;
  c-name: "g_variant_classify";
end;

define C-function g-variant-compare
  input parameter self :: <GVariant>;
  input parameter two_ :: <GVariant>;
  result res :: <C-signed-int>;
  c-name: "g_variant_compare";
end;

define C-function g-variant-dup-bytestring
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_variant_dup_bytestring";
end;

define C-function g-variant-dup-bytestring-array
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_dup_bytestring_array";
end;

define C-function g-variant-dup-objv
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_dup_objv";
end;

define C-function g-variant-dup-string
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_variant_dup_string";
end;

define C-function g-variant-dup-strv
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_dup_strv";
end;

define C-function g-variant-equal
  input parameter self :: <GVariant>;
  input parameter two_ :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_variant_equal";
end;

define C-function g-variant-get-boolean
  input parameter self :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_variant_get_boolean";
end;

define C-function g-variant-get-byte
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-char>;
  c-name: "g_variant_get_byte";
end;

define C-function g-variant-get-bytestring
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-char*>;
  c-name: "g_variant_get_bytestring";
end;

define C-function g-variant-get-bytestring-array
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_get_bytestring_array";
end;

define C-function g-variant-get-child-value
  input parameter self :: <GVariant>;
  input parameter index__ :: <C-unsigned-long>;
  result res :: <GVariant>;
  c-name: "g_variant_get_child_value";
end;

define C-function g-variant-get-data
  input parameter self :: <GVariant>;
  result res :: <C-void*>;
  c-name: "g_variant_get_data";
end;

define C-function g-variant-get-double
  input parameter self :: <GVariant>;
  result res :: <C-double>;
  c-name: "g_variant_get_double";
end;

define C-function g-variant-get-fixed-array
  input parameter self :: <GVariant>;
  output parameter n_elements_ :: <C-unsigned-long*>;
  input parameter element_size_ :: <C-unsigned-long>;
  result res :: <C-void*>;
  c-name: "g_variant_get_fixed_array";
end;

define C-function g-variant-get-handle
  input parameter self :: <GVariant>;
  result res :: <C-signed-int>;
  c-name: "g_variant_get_handle";
end;

define C-function g-variant-get-int16
  input parameter self :: <GVariant>;
  result res :: <C-signed-short>;
  c-name: "g_variant_get_int16";
end;

define C-function g-variant-get-int32
  input parameter self :: <GVariant>;
  result res :: <C-signed-int>;
  c-name: "g_variant_get_int32";
end;

define C-function g-variant-get-int64
  input parameter self :: <GVariant>;
  result res :: <C-signed-long>;
  c-name: "g_variant_get_int64";
end;

define C-function g-variant-get-maybe
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_get_maybe";
end;

define C-function g-variant-get-normal-form
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_get_normal_form";
end;

define C-function g-variant-get-objv
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_get_objv";
end;

define C-function g-variant-get-size
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-long>;
  c-name: "g_variant_get_size";
end;

define C-function g-variant-get-string
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string>;
  c-name: "g_variant_get_string";
end;

define C-function g-variant-get-strv
  input parameter self :: <GVariant>;
  output parameter length_ :: <C-unsigned-long*>;
  result res :: <C-string*>;
  c-name: "g_variant_get_strv";
end;

define C-function g-variant-get-type
  input parameter self :: <GVariant>;
  result res :: <GVariantType>;
  c-name: "g_variant_get_type";
end;

define C-function g-variant-get-type-string
  input parameter self :: <GVariant>;
  result res :: <C-string>;
  c-name: "g_variant_get_type_string";
end;

define C-function g-variant-get-uint16
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-short>;
  c-name: "g_variant_get_uint16";
end;

define C-function g-variant-get-uint32
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-int>;
  c-name: "g_variant_get_uint32";
end;

define C-function g-variant-get-uint64
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-long>;
  c-name: "g_variant_get_uint64";
end;

define C-function g-variant-get-variant
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_get_variant";
end;

define C-function g-variant-hash
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-int>;
  c-name: "g_variant_hash";
end;

define C-function g-variant-is-container
  input parameter self :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_container";
end;

define C-function g-variant-is-floating
  input parameter self :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_floating";
end;

define C-function g-variant-is-normal-form
  input parameter self :: <GVariant>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_normal_form";
end;

define C-function g-variant-is-of-type
  input parameter self :: <GVariant>;
  input parameter type_ :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_of_type";
end;

define C-function g-variant-lookup-value
  input parameter self :: <GVariant>;
  input parameter key_ :: <C-string>;
  input parameter expected_type_ :: <GVariantType>;
  result res :: <GVariant>;
  c-name: "g_variant_lookup_value";
end;

define C-function g-variant-n-children
  input parameter self :: <GVariant>;
  result res :: <C-unsigned-long>;
  c-name: "g_variant_n_children";
end;

define C-function g-variant-print
  input parameter self :: <GVariant>;
  input parameter type_annotate_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "g_variant_print";
end;

define C-function g-variant-ref
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_ref";
end;

define C-function g-variant-ref-sink
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_ref_sink";
end;

define C-function g-variant-store
  input parameter self :: <GVariant>;
  input parameter data_ :: <C-void*>;
  c-name: "g_variant_store";
end;

define C-function g-variant-take-ref
  input parameter self :: <GVariant>;
  result res :: <GVariant>;
  c-name: "g_variant_take_ref";
end;

define C-function g-variant-unref
  input parameter self :: <GVariant>;
  c-name: "g_variant_unref";
end;

define C-function g-variant-is-object-path
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_object_path";
end;

define C-function g-variant-is-signature
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_variant_is_signature";
end;

define C-function g-variant-parse
  input parameter type_ :: <GVariantType>;
  input parameter text_ :: <C-string>;
  input parameter limit_ :: <C-string>;
  input parameter endptr_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <GVariant>;
  c-name: "g_variant_parse";
end;

define C-function g-variant-parser-get-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_variant_parser_get_error_quark";
end;

define C-struct <_GVariantBuilder>
  constant slot g-variant-builder-x :: <C-unsigned-long*>;
  pointer-type-name: <GVariantBuilder>;
end C-struct;

define C-function g-variant-builder-new
  input parameter type_ :: <GVariantType>;
  result res :: <GVariantBuilder>;
  c-name: "g_variant_builder_new";
end;

define C-function g-variant-builder-add-value
  input parameter self :: <GVariantBuilder>;
  input parameter value_ :: <GVariant>;
  c-name: "g_variant_builder_add_value";
end;

define C-function g-variant-builder-close
  input parameter self :: <GVariantBuilder>;
  c-name: "g_variant_builder_close";
end;

define C-function g-variant-builder-end
  input parameter self :: <GVariantBuilder>;
  result res :: <GVariant>;
  c-name: "g_variant_builder_end";
end;

define C-function g-variant-builder-open
  input parameter self :: <GVariantBuilder>;
  input parameter type_ :: <GVariantType>;
  c-name: "g_variant_builder_open";
end;

define C-function g-variant-builder-ref
  input parameter self :: <GVariantBuilder>;
  result res :: <GVariantBuilder>;
  c-name: "g_variant_builder_ref";
end;

define C-function g-variant-builder-unref
  input parameter self :: <GVariantBuilder>;
  c-name: "g_variant_builder_unref";
end;

define constant $g-variant-class-boolean = 98;
define constant $g-variant-class-byte = 121;
define constant $g-variant-class-int16 = 110;
define constant $g-variant-class-uint16 = 113;
define constant $g-variant-class-int32 = 105;
define constant $g-variant-class-uint32 = 117;
define constant $g-variant-class-int64 = 120;
define constant $g-variant-class-uint64 = 116;
define constant $g-variant-class-handle = 104;
define constant $g-variant-class-double = 100;
define constant $g-variant-class-string = 115;
define constant $g-variant-class-object-path = 111;
define constant $g-variant-class-signature = 103;
define constant $g-variant-class-variant = 118;
define constant $g-variant-class-maybe = 109;
define constant $g-variant-class-array = 97;
define constant $g-variant-class-tuple = 40;
define constant $g-variant-class-dict-entry = 123;
define constant <GVariantClass> = <C-int>;
define C-pointer-type <GVariantClass*> => <GVariantClass>;

define constant $g-variant-parse-error-failed = 0;
define constant $g-variant-parse-error-basic-type-expected = 1;
define constant $g-variant-parse-error-cannot-infer-type = 2;
define constant $g-variant-parse-error-definite-type-expected = 3;
define constant $g-variant-parse-error-input-not-at-end = 4;
define constant $g-variant-parse-error-invalid-character = 5;
define constant $g-variant-parse-error-invalid-format-string = 6;
define constant $g-variant-parse-error-invalid-object-path = 7;
define constant $g-variant-parse-error-invalid-signature = 8;
define constant $g-variant-parse-error-invalid-type-string = 9;
define constant $g-variant-parse-error-no-common-type = 10;
define constant $g-variant-parse-error-number-out-of-range = 11;
define constant $g-variant-parse-error-number-too-big = 12;
define constant $g-variant-parse-error-type-error = 13;
define constant $g-variant-parse-error-unexpected-token = 14;
define constant $g-variant-parse-error-unknown-keyword = 15;
define constant $g-variant-parse-error-unterminated-string-constant = 16;
define constant $g-variant-parse-error-value-expected = 17;
define constant <GVariantParseError> = <C-int>;
define C-pointer-type <GVariantParseError*> => <GVariantParseError>;

define C-struct <_GVariantType>
  pointer-type-name: <GVariantType>;
end C-struct;

define C-function g-variant-type-new
  input parameter type_string_ :: <C-string>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_new";
end;

define C-function g-variant-type-new-tuple
  input parameter items_ :: <C-unsigned-char*> /* Not supported */;
  input parameter length_ :: <C-signed-int>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_new_tuple";
end;

define C-function g-variant-type-copy
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_copy";
end;

define C-function g-variant-type-dup-string
  input parameter self :: <GVariantType>;
  result res :: <C-string>;
  c-name: "g_variant_type_dup_string";
end;

define C-function g-variant-type-element
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_element";
end;

define C-function g-variant-type-equal
  input parameter self :: <GVariantType>;
  input parameter type2_ :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_equal";
end;

define C-function g-variant-type-first
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_first";
end;

define C-function g-variant-type-free
  input parameter self :: <GVariantType>;
  c-name: "g_variant_type_free";
end;

define C-function g-variant-type-get-string-length
  input parameter self :: <GVariantType>;
  result res :: <C-unsigned-long>;
  c-name: "g_variant_type_get_string_length";
end;

define C-function g-variant-type-hash
  input parameter self :: <GVariantType>;
  result res :: <C-unsigned-int>;
  c-name: "g_variant_type_hash";
end;

define C-function g-variant-type-is-array
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_array";
end;

define C-function g-variant-type-is-basic
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_basic";
end;

define C-function g-variant-type-is-container
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_container";
end;

define C-function g-variant-type-is-definite
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_definite";
end;

define C-function g-variant-type-is-dict-entry
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_dict_entry";
end;

define C-function g-variant-type-is-maybe
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_maybe";
end;

define C-function g-variant-type-is-subtype-of
  input parameter self :: <GVariantType>;
  input parameter supertype_ :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_subtype_of";
end;

define C-function g-variant-type-is-tuple
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_tuple";
end;

define C-function g-variant-type-is-variant
  input parameter self :: <GVariantType>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_is_variant";
end;

define C-function g-variant-type-key
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_key";
end;

define C-function g-variant-type-n-items
  input parameter self :: <GVariantType>;
  result res :: <C-unsigned-long>;
  c-name: "g_variant_type_n_items";
end;

define C-function g-variant-type-new-array
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_new_array";
end;

define C-function g-variant-type-new-dict-entry
  input parameter self :: <GVariantType>;
  input parameter value_ :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_new_dict_entry";
end;

define C-function g-variant-type-new-maybe
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_new_maybe";
end;

define C-function g-variant-type-next
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_next";
end;

define C-function g-variant-type-value
  input parameter self :: <GVariantType>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_value";
end;

define C-function g-variant-type-checked-
  input parameter unknown_ :: <C-string>;
  result res :: <GVariantType>;
  c-name: "g_variant_type_checked_";
end;

define C-function g-variant-type-string-is-valid
  input parameter type_string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_string_is_valid";
end;

define C-function g-variant-type-string-scan
  input parameter string_ :: <C-string>;
  input parameter limit_ :: <C-string>;
  output parameter endptr_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_variant_type_string_scan";
end;

define constant $win32-msg-handle = 19981206;

define C-function g-access
  input parameter filename_ :: <C-string>;
  input parameter mode_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_access";
end;

define C-function g-ascii-digit-value
  input parameter c_ :: <C-unsigned-char>;
  result res :: <C-signed-int>;
  c-name: "g_ascii_digit_value";
end;

define C-function g-ascii-dtostr
  input parameter buffer_ :: <C-string>;
  input parameter buf_len_ :: <C-signed-int>;
  input parameter d_ :: <C-double>;
  result res :: <C-string>;
  c-name: "g_ascii_dtostr";
end;

define C-function g-ascii-formatd
  input parameter buffer_ :: <C-string>;
  input parameter buf_len_ :: <C-signed-int>;
  input parameter format_ :: <C-string>;
  input parameter d_ :: <C-double>;
  result res :: <C-string>;
  c-name: "g_ascii_formatd";
end;

define C-function g-ascii-strcasecmp
  input parameter s1_ :: <C-string>;
  input parameter s2_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_ascii_strcasecmp";
end;

define C-function g-ascii-strdown
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_ascii_strdown";
end;

define C-function g-ascii-strncasecmp
  input parameter s1_ :: <C-string>;
  input parameter s2_ :: <C-string>;
  input parameter n_ :: <C-unsigned-long>;
  result res :: <C-signed-int>;
  c-name: "g_ascii_strncasecmp";
end;

define C-function g-ascii-strtod
  input parameter nptr_ :: <C-string>;
  input parameter endptr_ :: <C-string>;
  result res :: <C-double>;
  c-name: "g_ascii_strtod";
end;

define C-function g-ascii-strtoll
  input parameter nptr_ :: <C-string>;
  input parameter endptr_ :: <C-string>;
  input parameter base_ :: <C-unsigned-int>;
  result res :: <C-signed-long>;
  c-name: "g_ascii_strtoll";
end;

define C-function g-ascii-strtoull
  input parameter nptr_ :: <C-string>;
  input parameter endptr_ :: <C-string>;
  input parameter base_ :: <C-unsigned-int>;
  result res :: <C-unsigned-long>;
  c-name: "g_ascii_strtoull";
end;

define C-function g-ascii-strup
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_ascii_strup";
end;

define C-function g-ascii-tolower
  input parameter c_ :: <C-unsigned-char>;
  result res :: <C-unsigned-char>;
  c-name: "g_ascii_tolower";
end;

define C-function g-ascii-toupper
  input parameter c_ :: <C-unsigned-char>;
  result res :: <C-unsigned-char>;
  c-name: "g_ascii_toupper";
end;

define C-function g-ascii-xdigit-value
  input parameter c_ :: <C-unsigned-char>;
  result res :: <C-signed-int>;
  c-name: "g_ascii_xdigit_value";
end;

define C-function g-assert-warning
  input parameter log_domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter pretty_function_ :: <C-string>;
  input parameter expression_ :: <C-string>;
  c-name: "g_assert_warning";
end;

define C-function g-assertion-message
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter message_ :: <C-string>;
  c-name: "g_assertion_message";
end;

define C-function g-assertion-message-cmpstr
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter expr_ :: <C-string>;
  input parameter arg1_ :: <C-string>;
  input parameter cmp_ :: <C-string>;
  input parameter arg2_ :: <C-string>;
  c-name: "g_assertion_message_cmpstr";
end;

define C-function g-assertion-message-error
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter expr_ :: <C-string>;
  input parameter error_ :: <GError>;
  input parameter error_domain_ :: <C-unsigned-int>;
  input parameter error_code_ :: <C-signed-int>;
  c-name: "g_assertion_message_error";
end;

define C-function g-assertion-message-expr
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter expr_ :: <C-string>;
  c-name: "g_assertion_message_expr";
end;

define C-function g-atexit
  input parameter func_ :: <C-function-pointer>;
  c-name: "g_atexit";
end;

define C-function g-atomic-int-add
  input parameter atomic_ :: <C-signed-int*>;
  input parameter val_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_atomic_int_add";
end;

define C-function g-atomic-int-and
  input parameter atomic_ :: <C-unsigned-int*>;
  input parameter val_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_atomic_int_and";
end;

define C-function g-atomic-int-compare-and-exchange
  input parameter atomic_ :: <C-signed-int*>;
  input parameter oldval_ :: <C-signed-int>;
  input parameter newval_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_atomic_int_compare_and_exchange";
end;

define C-function g-atomic-int-dec-and-test
  input parameter atomic_ :: <C-signed-int*>;
  result res :: <C-boolean>;
  c-name: "g_atomic_int_dec_and_test";
end;

define C-function g-atomic-int-get
  input parameter atomic_ :: <C-signed-int*>;
  result res :: <C-signed-int>;
  c-name: "g_atomic_int_get";
end;

define C-function g-atomic-int-inc
  input parameter atomic_ :: <C-signed-int*>;
  c-name: "g_atomic_int_inc";
end;

define C-function g-atomic-int-or
  input parameter atomic_ :: <C-unsigned-int*>;
  input parameter val_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_atomic_int_or";
end;

define C-function g-atomic-int-set
  input parameter atomic_ :: <C-signed-int*>;
  input parameter newval_ :: <C-signed-int>;
  c-name: "g_atomic_int_set";
end;

define C-function g-atomic-int-xor
  input parameter atomic_ :: <C-unsigned-int*>;
  input parameter val_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_atomic_int_xor";
end;

define C-function g-atomic-pointer-add
  input parameter atomic_ :: <C-void*>;
  input parameter val_ :: <C-signed-long>;
  result res :: <C-signed-long>;
  c-name: "g_atomic_pointer_add";
end;

define C-function g-atomic-pointer-and
  input parameter atomic_ :: <C-void*>;
  input parameter val_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_atomic_pointer_and";
end;

define C-function g-atomic-pointer-compare-and-exchange
  input parameter atomic_ :: <C-void*>;
  input parameter oldval_ :: <C-void*>;
  input parameter newval_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_atomic_pointer_compare_and_exchange";
end;

define C-function g-atomic-pointer-or
  input parameter atomic_ :: <C-void*>;
  input parameter val_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_atomic_pointer_or";
end;

define C-function g-atomic-pointer-set
  input parameter atomic_ :: <C-void*>;
  input parameter newval_ :: <C-void*>;
  c-name: "g_atomic_pointer_set";
end;

define C-function g-atomic-pointer-xor
  input parameter atomic_ :: <C-void*>;
  input parameter val_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_atomic_pointer_xor";
end;

define C-function g-base64-decode
  input parameter text_ :: <C-string>;
  output parameter out_len_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_base64_decode";
end;

define C-function g-base64-decode-inplace
  input output parameter text_ :: <C-unsigned-char*>;
  input output parameter out_len_ :: <C-unsigned-long*>;
  result res :: <C-unsigned-char*>;
  c-name: "g_base64_decode_inplace";
end;

define C-function g-base64-decode-step
  input parameter in_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-unsigned-long>;
  output parameter out_ :: <C-unsigned-char*>;
  input output parameter state_ :: <C-signed-int*>;
  input output parameter save_ :: <C-unsigned-int*>;
  result res :: <C-unsigned-long>;
  c-name: "g_base64_decode_step";
end;

define C-function g-base64-encode
  input parameter data_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_base64_encode";
end;

define C-function g-base64-encode-close
  input parameter break_lines_ :: <C-boolean>;
  output parameter out_ :: <C-unsigned-char*>;
  input output parameter state_ :: <C-signed-int*>;
  input output parameter save_ :: <C-signed-int*>;
  result res :: <C-unsigned-long>;
  c-name: "g_base64_encode_close";
end;

define C-function g-base64-encode-step
  input parameter in_ :: <C-unsigned-char*>;
  input parameter len_ :: <C-unsigned-long>;
  input parameter break_lines_ :: <C-boolean>;
  output parameter out_ :: <C-unsigned-char*>;
  input output parameter state_ :: <C-signed-int*>;
  input output parameter save_ :: <C-signed-int*>;
  result res :: <C-unsigned-long>;
  c-name: "g_base64_encode_step";
end;

define C-function g-basename
  input parameter file_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_basename";
end;

define C-function g-bit-lock
  input parameter address_ :: <C-signed-int*>;
  input parameter lock_bit_ :: <C-signed-int>;
  c-name: "g_bit_lock";
end;

define C-function g-bit-nth-lsf
  input parameter mask_ :: <C-unsigned-long>;
  input parameter nth_bit_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_bit_nth_lsf";
end;

define C-function g-bit-nth-msf
  input parameter mask_ :: <C-unsigned-long>;
  input parameter nth_bit_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_bit_nth_msf";
end;

define C-function g-bit-storage
  input parameter number_ :: <C-unsigned-long>;
  result res :: <C-unsigned-int>;
  c-name: "g_bit_storage";
end;

define C-function g-bit-trylock
  input parameter address_ :: <C-signed-int*>;
  input parameter lock_bit_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_bit_trylock";
end;

define C-function g-bit-unlock
  input parameter address_ :: <C-signed-int*>;
  input parameter lock_bit_ :: <C-signed-int>;
  c-name: "g_bit_unlock";
end;

define C-function g-build-filenamev
  input parameter args_ :: <C-string*>;
  result res :: <C-string>;
  c-name: "g_build_filenamev";
end;

define C-function g-build-pathv
  input parameter separator_ :: <C-string>;
  input parameter args_ :: <C-string*>;
  result res :: <C-string>;
  c-name: "g_build_pathv";
end;

define C-function g-chdir
  input parameter path_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_chdir";
end;

define C-function glib-check-version
  input parameter required_major_ :: <C-unsigned-int>;
  input parameter required_minor_ :: <C-unsigned-int>;
  input parameter required_micro_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "glib_check_version";
end;

define C-function g-child-watch-add-full
  input parameter priority_ :: <C-signed-int>;
  input parameter pid_ :: <C-signed-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_child_watch_add_full";
end;

define C-function g-child-watch-source-new
  input parameter pid_ :: <C-signed-int>;
  result res :: <GSource>;
  c-name: "g_child_watch_source_new";
end;

define C-function g-clear-error
  output parameter error_ :: <GError*>;
  c-name: "g_clear_error";
end;

define C-function g-compute-checksum-for-data
  input parameter checksum_type_ :: <GChecksumType>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_compute_checksum_for_data";
end;

define C-function g-compute-checksum-for-string
  input parameter checksum_type_ :: <GChecksumType>;
  input parameter str_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_compute_checksum_for_string";
end;

define C-function g-compute-hmac-for-data
  input parameter digest_type_ :: <GChecksumType>;
  input parameter key_ :: <C-unsigned-char*>;
  input parameter key_len_ :: <C-unsigned-long>;
  input parameter data_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_compute_hmac_for_data";
end;

define C-function g-compute-hmac-for-string
  input parameter digest_type_ :: <GChecksumType>;
  input parameter key_ :: <C-unsigned-char*>;
  input parameter key_len_ :: <C-unsigned-long>;
  input parameter str_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_compute_hmac_for_string";
end;

define C-function g-convert
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter to_codeset_ :: <C-string>;
  input parameter from_codeset_ :: <C-string>;
  output parameter bytes_read_ :: <C-unsigned-long*>;
  output parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_convert";
end;

define C-function g-convert-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_convert_error_quark";
end;

define C-function g-convert-with-fallback
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter to_codeset_ :: <C-string>;
  input parameter from_codeset_ :: <C-string>;
  input parameter fallback_ :: <C-string>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_convert_with_fallback";
end;

define C-function g-convert-with-iconv
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter converter_ :: <GIConv>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_convert_with_iconv";
end;

define C-function g-datalist-clear
  input parameter datalist_ :: <GData>;
  c-name: "g_datalist_clear";
end;

define C-function g-datalist-get-flags
  input parameter datalist_ :: <GData>;
  result res :: <C-unsigned-int>;
  c-name: "g_datalist_get_flags";
end;

define C-function g-datalist-id-set-data-full
  input parameter datalist_ :: <GData>;
  input parameter key_id_ :: <C-unsigned-int>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_func_ :: <C-function-pointer>;
  c-name: "g_datalist_id_set_data_full";
end;

define C-function g-datalist-init
  input parameter datalist_ :: <GData>;
  c-name: "g_datalist_init";
end;

define C-function g-datalist-set-flags
  input parameter datalist_ :: <GData>;
  input parameter flags_ :: <C-unsigned-int>;
  c-name: "g_datalist_set_flags";
end;

define C-function g-datalist-unset-flags
  input parameter datalist_ :: <GData>;
  input parameter flags_ :: <C-unsigned-int>;
  c-name: "g_datalist_unset_flags";
end;

define C-function g-dataset-destroy
  input parameter dataset_location_ :: <C-void*>;
  c-name: "g_dataset_destroy";
end;

define C-function g-dataset-id-set-data-full
  input parameter dataset_location_ :: <C-void*>;
  input parameter key_id_ :: <C-unsigned-int>;
  input parameter data_ :: <C-void*>;
  input parameter destroy_func_ :: <C-function-pointer>;
  c-name: "g_dataset_id_set_data_full";
end;

define C-function g-dcgettext
  input parameter domain_ :: <C-string>;
  input parameter msgid_ :: <C-string>;
  input parameter category_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_dcgettext";
end;

define C-function g-dgettext
  input parameter domain_ :: <C-string>;
  input parameter msgid_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_dgettext";
end;

define C-function g-direct-equal
  input parameter v1_ :: <C-void*>;
  input parameter v2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_direct_equal";
end;

define C-function g-direct-hash
  input parameter v_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_direct_hash";
end;

define C-function g-dngettext
  input parameter domain_ :: <C-string>;
  input parameter msgid_ :: <C-string>;
  input parameter msgid_plural_ :: <C-string>;
  input parameter n_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_dngettext";
end;

define C-function g-double-equal
  input parameter v1_ :: <C-void*>;
  input parameter v2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_double_equal";
end;

define C-function g-double-hash
  input parameter v_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_double_hash";
end;

define C-function g-dpgettext
  input parameter domain_ :: <C-string>;
  input parameter msgctxtid_ :: <C-string>;
  input parameter msgidoffset_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_dpgettext";
end;

define C-function g-dpgettext2
  input parameter domain_ :: <C-string>;
  input parameter context_ :: <C-string>;
  input parameter msgid_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_dpgettext2";
end;

define C-function g-environ-getenv
  input parameter envp_ :: <C-string*>;
  input parameter variable_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_environ_getenv";
end;

define C-function g-environ-setenv
  input parameter envp_ :: <C-string*>;
  input parameter variable_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter overwrite_ :: <C-boolean>;
  result res :: <C-string*>;
  c-name: "g_environ_setenv";
end;

define C-function g-environ-unsetenv
  input parameter envp_ :: <C-string*>;
  input parameter variable_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_environ_unsetenv";
end;

define C-function g-file-error-from-errno
  input parameter err_no_ :: <C-signed-int>;
  result res :: <GFileError>;
  c-name: "g_file_error_from_errno";
end;

define C-function g-file-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_file_error_quark";
end;

define C-function g-file-get-contents
  input parameter filename_ :: <C-string>;
  output parameter contents_ :: <C-unsigned-char*>;
  output parameter length_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_get_contents";
end;

define C-function g-file-open-tmp
  input parameter tmpl_ :: <C-string>;
  output parameter name_used_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-signed-int>;
  c-name: "g_file_open_tmp";
end;

define C-function g-file-read-link
  input parameter filename_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_file_read_link";
end;

define C-function g-file-set-contents
  input parameter filename_ :: <C-string>;
  input parameter contents_ :: <C-unsigned-char*>;
  input parameter length_ :: <C-signed-long>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_file_set_contents";
end;

define C-function g-file-test
  input parameter filename_ :: <C-string>;
  input parameter test_ :: <GFileTest>;
  result res :: <C-boolean>;
  c-name: "g_file_test";
end;

define C-function g-filename-display-basename
  input parameter filename_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_filename_display_basename";
end;

define C-function g-filename-display-name
  input parameter filename_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_filename_display_name";
end;

define C-function g-filename-from-uri
  input parameter uri_ :: <C-string>;
  input parameter hostname_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_filename_from_uri";
end;

define C-function g-filename-from-utf8
  input parameter utf8string_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_filename_from_utf8";
end;

define C-function g-filename-to-uri
  input parameter filename_ :: <C-string>;
  input parameter hostname_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_filename_to_uri";
end;

define C-function g-filename-to-utf8
  input parameter opsysstring_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_filename_to_utf8";
end;

define C-function g-find-program-in-path
  input parameter program_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_find_program_in_path";
end;

define C-function g-format-size
  input parameter size_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_format_size";
end;

define C-function g-format-size-for-display
  input parameter size_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_format_size_for_display";
end;

define C-function g-format-size-full
  input parameter size_ :: <C-unsigned-long>;
  input parameter flags_ :: <GFormatSizeFlags>;
  result res :: <C-string>;
  c-name: "g_format_size_full";
end;

define C-function g-free
  input parameter mem_ :: <C-void*>;
  c-name: "g_free";
end;

define C-function g-get-application-name
  result res :: <C-string>;
  c-name: "g_get_application_name";
end;

define C-function g-get-charset
  input parameter charset_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_get_charset";
end;

define C-function g-get-codeset
  result res :: <C-string>;
  c-name: "g_get_codeset";
end;

define C-function g-get-current-dir
  result res :: <C-string>;
  c-name: "g_get_current_dir";
end;

define C-function g-get-current-time
  input parameter result_ :: <GTimeVal>;
  c-name: "g_get_current_time";
end;

define C-function g-get-environ
  result res :: <C-string*>;
  c-name: "g_get_environ";
end;

define C-function g-get-filename-charsets
  input parameter charsets_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_get_filename_charsets";
end;

define C-function g-get-home-dir
  result res :: <C-string>;
  c-name: "g_get_home_dir";
end;

define C-function g-get-host-name
  result res :: <C-string>;
  c-name: "g_get_host_name";
end;

define C-function g-get-language-names
  result res :: <C-string*>;
  c-name: "g_get_language_names";
end;

define C-function g-get-locale-variants
  input parameter locale_ :: <C-string>;
  result res :: <C-string*>;
  c-name: "g_get_locale_variants";
end;

define C-function g-get-monotonic-time
  result res :: <C-signed-long>;
  c-name: "g_get_monotonic_time";
end;

define C-function g-get-prgname
  result res :: <C-string>;
  c-name: "g_get_prgname";
end;

define C-function g-get-real-name
  result res :: <C-string>;
  c-name: "g_get_real_name";
end;

define C-function g-get-real-time
  result res :: <C-signed-long>;
  c-name: "g_get_real_time";
end;

define C-function g-get-system-config-dirs
  result res :: <C-string*>;
  c-name: "g_get_system_config_dirs";
end;

define C-function g-get-system-data-dirs
  result res :: <C-string*>;
  c-name: "g_get_system_data_dirs";
end;

define C-function g-get-tmp-dir
  result res :: <C-string>;
  c-name: "g_get_tmp_dir";
end;

define C-function g-get-user-cache-dir
  result res :: <C-string>;
  c-name: "g_get_user_cache_dir";
end;

define C-function g-get-user-config-dir
  result res :: <C-string>;
  c-name: "g_get_user_config_dir";
end;

define C-function g-get-user-data-dir
  result res :: <C-string>;
  c-name: "g_get_user_data_dir";
end;

define C-function g-get-user-name
  result res :: <C-string>;
  c-name: "g_get_user_name";
end;

define C-function g-get-user-runtime-dir
  result res :: <C-string>;
  c-name: "g_get_user_runtime_dir";
end;

define C-function g-get-user-special-dir
  input parameter directory_ :: <GUserDirectory>;
  result res :: <C-string>;
  c-name: "g_get_user_special_dir";
end;

define C-function g-getenv
  input parameter variable_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_getenv";
end;

define C-function g-hostname-is-ascii-encoded
  input parameter hostname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_hostname_is_ascii_encoded";
end;

define C-function g-hostname-is-ip-address
  input parameter hostname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_hostname_is_ip_address";
end;

define C-function g-hostname-is-non-ascii
  input parameter hostname_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_hostname_is_non_ascii";
end;

define C-function g-hostname-to-ascii
  input parameter hostname_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_hostname_to_ascii";
end;

define C-function g-hostname-to-unicode
  input parameter hostname_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_hostname_to_unicode";
end;

define C-function g-idle-add-full
  input parameter priority_ :: <C-signed-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_idle_add_full";
end;

define C-function g-idle-remove-by-data
  input parameter data_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_idle_remove_by_data";
end;

define C-function g-idle-source-new
  result res :: <GSource>;
  c-name: "g_idle_source_new";
end;

define C-function g-int64-equal
  input parameter v1_ :: <C-void*>;
  input parameter v2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_int64_equal";
end;

define C-function g-int64-hash
  input parameter v_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_int64_hash";
end;

define C-function g-int-equal
  input parameter v1_ :: <C-void*>;
  input parameter v2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_int_equal";
end;

define C-function g-int-hash
  input parameter v_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_int_hash";
end;

define C-function g-intern-static-string
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_intern_static_string";
end;

define C-function g-intern-string
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_intern_string";
end;

define C-function g-io-add-watch-full
  input parameter channel_ :: <GIOChannel>;
  input parameter priority_ :: <C-signed-int>;
  input parameter condition_ :: <GIOCondition>;
  input parameter func_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_io_add_watch_full";
end;

define C-function g-io-create-watch
  input parameter channel_ :: <GIOChannel>;
  input parameter condition_ :: <GIOCondition>;
  result res :: <GSource>;
  c-name: "g_io_create_watch";
end;

define C-function g-listenv
  result res :: <C-string*>;
  c-name: "g_listenv";
end;

define C-function g-locale-from-utf8
  input parameter utf8string_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_locale_from_utf8";
end;

define C-function g-locale-to-utf8
  input parameter opsysstring_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter bytes_read_ :: <C-unsigned-long*>;
  input parameter bytes_written_ :: <C-unsigned-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_locale_to_utf8";
end;

define C-function g-log-default-handler
  input parameter log_domain_ :: <C-string>;
  input parameter log_level_ :: <GLogLevelFlags>;
  input parameter message_ :: <C-string>;
  input parameter unused_data_ :: <C-void*>;
  c-name: "g_log_default_handler";
end;

define C-function g-log-remove-handler
  input parameter log_domain_ :: <C-string>;
  input parameter handler_id_ :: <C-unsigned-int>;
  c-name: "g_log_remove_handler";
end;

define C-function g-log-set-always-fatal
  input parameter fatal_mask_ :: <GLogLevelFlags>;
  result res :: <GLogLevelFlags>;
  c-name: "g_log_set_always_fatal";
end;

define C-function g-log-set-fatal-mask
  input parameter log_domain_ :: <C-string>;
  input parameter fatal_mask_ :: <GLogLevelFlags>;
  result res :: <GLogLevelFlags>;
  c-name: "g_log_set_fatal_mask";
end;

define C-function g-main-current-source
  result res :: <GSource>;
  c-name: "g_main_current_source";
end;

define C-function g-main-depth
  result res :: <C-signed-int>;
  c-name: "g_main_depth";
end;

define C-function g-markup-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_markup_error_quark";
end;

define C-function g-markup-escape-text
  input parameter text_ :: <C-string>;
  input parameter length_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_markup_escape_text";
end;

define C-function g-mem-is-system-malloc
  result res :: <C-boolean>;
  c-name: "g_mem_is_system_malloc";
end;

define C-function g-mem-profile
  c-name: "g_mem_profile";
end;

define C-function g-mem-set-vtable
  input parameter vtable_ :: <GMemVTable>;
  c-name: "g_mem_set_vtable";
end;

define C-function g-mkdir-with-parents
  input parameter pathname_ :: <C-string>;
  input parameter mode_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_mkdir_with_parents";
end;

define C-function g-mkdtemp
  input parameter tmpl_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_mkdtemp";
end;

define C-function g-mkdtemp-full
  input parameter tmpl_ :: <C-string>;
  input parameter mode_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_mkdtemp_full";
end;

define C-function g-mkstemp
  input parameter tmpl_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_mkstemp";
end;

define C-function g-mkstemp-full
  input parameter tmpl_ :: <C-string>;
  input parameter flags_ :: <C-signed-int>;
  input parameter mode_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_mkstemp_full";
end;

define C-function g-nullify-pointer
  input parameter nullify_location_ :: <C-void*>;
  c-name: "g_nullify_pointer";
end;

define C-function g-on-error-query
  input parameter prg_name_ :: <C-string>;
  c-name: "g_on_error_query";
end;

define C-function g-on-error-stack-trace
  input parameter prg_name_ :: <C-string>;
  c-name: "g_on_error_stack_trace";
end;

define C-function g-option-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_option_error_quark";
end;

define C-function g-parse-debug-string
  input parameter string_ :: <C-string>;
  input parameter keys_ :: <C-unsigned-char*> /* Not supported */;
  input parameter nkeys_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_parse_debug_string";
end;

define C-function g-path-get-basename
  input parameter file_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_path_get_basename";
end;

define C-function g-path-get-dirname
  input parameter file_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_path_get_dirname";
end;

define C-function g-path-is-absolute
  input parameter file_name_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_path_is_absolute";
end;

define C-function g-path-skip-root
  input parameter file_name_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_path_skip_root";
end;

define C-function g-pattern-match
  input parameter pspec_ :: <GPatternSpec>;
  input parameter string_length_ :: <C-unsigned-int>;
  input parameter string_ :: <C-string>;
  input parameter string_reversed_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_pattern_match";
end;

define C-function g-pattern-match-simple
  input parameter pattern_ :: <C-string>;
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_pattern_match_simple";
end;

define C-function g-pattern-match-string
  input parameter pspec_ :: <GPatternSpec>;
  input parameter string_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_pattern_match_string";
end;

define C-function g-pointer-bit-lock
  input parameter address_ :: <C-void*>;
  input parameter lock_bit_ :: <C-signed-int>;
  c-name: "g_pointer_bit_lock";
end;

define C-function g-pointer-bit-trylock
  input parameter address_ :: <C-void*>;
  input parameter lock_bit_ :: <C-signed-int>;
  result res :: <C-boolean>;
  c-name: "g_pointer_bit_trylock";
end;

define C-function g-pointer-bit-unlock
  input parameter address_ :: <C-void*>;
  input parameter lock_bit_ :: <C-signed-int>;
  c-name: "g_pointer_bit_unlock";
end;

define C-function g-poll
  input parameter fds_ :: <GPollFD>;
  input parameter nfds_ :: <C-unsigned-int>;
  input parameter timeout_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_poll";
end;

define C-function g-propagate-error
  input parameter dest_ :: <GError>;
  input parameter src_ :: <GError>;
  c-name: "g_propagate_error";
end;

define C-function g-quark-from-static-string
  input parameter string_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_quark_from_static_string";
end;

define C-function g-quark-from-string
  input parameter string_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_quark_from_string";
end;

define C-function g-quark-to-string
  input parameter quark_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "g_quark_to_string";
end;

define C-function g-quark-try-string
  input parameter string_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_quark_try_string";
end;

define C-function g-random-double
  result res :: <C-double>;
  c-name: "g_random_double";
end;

define C-function g-random-double-range
  input parameter begin_ :: <C-double>;
  input parameter end_ :: <C-double>;
  result res :: <C-double>;
  c-name: "g_random_double_range";
end;

define C-function g-random-int
  result res :: <C-unsigned-int>;
  c-name: "g_random_int";
end;

define C-function g-random-int-range
  input parameter begin_ :: <C-signed-int>;
  input parameter end_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_random_int_range";
end;

define C-function g-random-set-seed
  input parameter seed_ :: <C-unsigned-int>;
  c-name: "g_random_set_seed";
end;

define C-function g-reload-user-special-dirs-cache
  c-name: "g_reload_user_special_dirs_cache";
end;

define C-function g-return-if-fail-warning
  input parameter log_domain_ :: <C-string>;
  input parameter pretty_function_ :: <C-string>;
  input parameter expression_ :: <C-string>;
  c-name: "g_return_if_fail_warning";
end;

define C-function g-rmdir
  input parameter filename_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_rmdir";
end;

define C-function g-set-application-name
  input parameter application_name_ :: <C-string>;
  c-name: "g_set_application_name";
end;

define C-function g-set-error-literal
  input parameter err_ :: <GError>;
  input parameter domain_ :: <C-unsigned-int>;
  input parameter code_ :: <C-signed-int>;
  input parameter message_ :: <C-string>;
  c-name: "g_set_error_literal";
end;

define C-function g-set-prgname
  input parameter prgname_ :: <C-string>;
  c-name: "g_set_prgname";
end;

define C-function g-setenv
  input parameter variable_ :: <C-string>;
  input parameter value_ :: <C-string>;
  input parameter overwrite_ :: <C-boolean>;
  result res :: <C-boolean>;
  c-name: "g_setenv";
end;

define C-function g-shell-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_shell_error_quark";
end;

define C-function g-shell-parse-argv
  input parameter command_line_ :: <C-string>;
  output parameter argcp_ :: <C-signed-int*>;
  output parameter argvp_ :: <C-string*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_shell_parse_argv";
end;

define C-function g-shell-quote
  input parameter unquoted_string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_shell_quote";
end;

define C-function g-shell-unquote
  input parameter quoted_string_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_shell_unquote";
end;

define C-function g-slice-free1
  input parameter block_size_ :: <C-unsigned-long>;
  input parameter mem_block_ :: <C-void*>;
  c-name: "g_slice_free1";
end;

define C-function g-slice-free-chain-with-offset
  input parameter block_size_ :: <C-unsigned-long>;
  input parameter mem_chain_ :: <C-void*>;
  input parameter next_offset_ :: <C-unsigned-long>;
  c-name: "g_slice_free_chain_with_offset";
end;

define C-function g-slice-get-config
  input parameter ckey_ :: <GSliceConfig>;
  result res :: <C-signed-long>;
  c-name: "g_slice_get_config";
end;

define C-function g-slice-get-config-state
  input parameter ckey_ :: <GSliceConfig>;
  input parameter address_ :: <C-signed-long>;
  input parameter n_values_ :: <C-unsigned-int*>;
  result res :: <C-signed-long*>;
  c-name: "g_slice_get_config_state";
end;

define C-function g-slice-set-config
  input parameter ckey_ :: <GSliceConfig>;
  input parameter value_ :: <C-signed-long>;
  c-name: "g_slice_set_config";
end;

define C-function g-spaced-primes-closest
  input parameter num_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_spaced_primes_closest";
end;

define C-function g-spawn-async
  input parameter working_directory_ :: <C-string>;
  input parameter argv_ :: <C-string*>;
  input parameter envp_ :: <C-string*>;
  input parameter flags_ :: <GSpawnFlags>;
  input parameter child_setup_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  output parameter child_pid_ :: <C-signed-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_spawn_async";
end;

define C-function g-spawn-async-with-pipes
  input parameter working_directory_ :: <C-string>;
  input parameter argv_ :: <C-string*>;
  input parameter envp_ :: <C-string*>;
  input parameter flags_ :: <GSpawnFlags>;
  input parameter child_setup_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  output parameter child_pid_ :: <C-signed-int*>;
  output parameter standard_input_ :: <C-signed-int*>;
  output parameter standard_output_ :: <C-signed-int*>;
  output parameter standard_error_ :: <C-signed-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_spawn_async_with_pipes";
end;

define C-function g-spawn-close-pid
  input parameter pid_ :: <C-signed-int>;
  c-name: "g_spawn_close_pid";
end;

define C-function g-spawn-command-line-async
  input parameter command_line_ :: <C-string>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_spawn_command_line_async";
end;

define C-function g-spawn-command-line-sync
  input parameter command_line_ :: <C-string>;
  output parameter standard_output_ :: <C-unsigned-char*>;
  output parameter standard_error_ :: <C-unsigned-char*>;
  output parameter exit_status_ :: <C-signed-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_spawn_command_line_sync";
end;

define C-function g-spawn-error-quark
  result res :: <C-unsigned-int>;
  c-name: "g_spawn_error_quark";
end;

define C-function g-spawn-sync
  input parameter working_directory_ :: <C-string>;
  input parameter argv_ :: <C-string*>;
  input parameter envp_ :: <C-string*>;
  input parameter flags_ :: <GSpawnFlags>;
  input parameter child_setup_ :: <C-function-pointer>;
  input parameter user_data_ :: <C-void*>;
  output parameter standard_output_ :: <C-unsigned-char*>;
  output parameter standard_error_ :: <C-unsigned-char*>;
  output parameter exit_status_ :: <C-signed-int*>;
  output parameter error_ :: <GError*>;
  result res :: <C-boolean>;
  c-name: "g_spawn_sync";
end;

define C-function g-stpcpy
  input parameter dest_ :: <C-string>;
  input parameter src_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_stpcpy";
end;

define C-function g-str-equal
  input parameter v1_ :: <C-void*>;
  input parameter v2_ :: <C-void*>;
  result res :: <C-boolean>;
  c-name: "g_str_equal";
end;

define C-function g-str-has-prefix
  input parameter str_ :: <C-string>;
  input parameter prefix_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_str_has_prefix";
end;

define C-function g-str-has-suffix
  input parameter str_ :: <C-string>;
  input parameter suffix_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_str_has_suffix";
end;

define C-function g-str-hash
  input parameter v_ :: <C-void*>;
  result res :: <C-unsigned-int>;
  c-name: "g_str_hash";
end;

define C-function g-strcanon
  input parameter string_ :: <C-string>;
  input parameter valid_chars_ :: <C-string>;
  input parameter substitutor_ :: <C-unsigned-char>;
  result res :: <C-string>;
  c-name: "g_strcanon";
end;

define C-function g-strcasecmp
  input parameter s1_ :: <C-string>;
  input parameter s2_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_strcasecmp";
end;

define C-function g-strchomp
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strchomp";
end;

define C-function g-strchug
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strchug";
end;

define C-function g-strcmp0
  input parameter str1_ :: <C-string>;
  input parameter str2_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_strcmp0";
end;

define C-function g-strcompress
  input parameter source_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strcompress";
end;

define C-function g-strdelimit
  input parameter string_ :: <C-string>;
  input parameter delimiters_ :: <C-string>;
  input parameter new_delimiter_ :: <C-unsigned-char>;
  result res :: <C-string>;
  c-name: "g_strdelimit";
end;

define C-function g-strdown
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strdown";
end;

define C-function g-strdup
  input parameter str_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strdup";
end;

define C-function g-strerror
  input parameter errnum_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_strerror";
end;

define C-function g-strescape
  input parameter source_ :: <C-string>;
  input parameter exceptions_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strescape";
end;

define C-function g-strfreev
  input parameter str_array_ :: <C-string>;
  c-name: "g_strfreev";
end;

define C-function g-string-new
  input parameter init_ :: <C-string>;
  result res :: <GString>;
  c-name: "g_string_new";
end;

define C-function g-string-new-len
  input parameter init_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <GString>;
  c-name: "g_string_new_len";
end;

define C-function g-string-sized-new
  input parameter dfl_size_ :: <C-unsigned-long>;
  result res :: <GString>;
  c-name: "g_string_sized_new";
end;

define C-function g-strip-context
  input parameter msgid_ :: <C-string>;
  input parameter msgval_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strip_context";
end;

define C-function g-strjoinv
  input parameter separator_ :: <C-string>;
  input parameter str_array_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strjoinv";
end;

define C-function g-strlcat
  input parameter dest_ :: <C-string>;
  input parameter src_ :: <C-string>;
  input parameter dest_size_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_strlcat";
end;

define C-function g-strlcpy
  input parameter dest_ :: <C-string>;
  input parameter src_ :: <C-string>;
  input parameter dest_size_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_strlcpy";
end;

define C-function g-strncasecmp
  input parameter s1_ :: <C-string>;
  input parameter s2_ :: <C-string>;
  input parameter n_ :: <C-unsigned-int>;
  result res :: <C-signed-int>;
  c-name: "g_strncasecmp";
end;

define C-function g-strndup
  input parameter str_ :: <C-string>;
  input parameter n_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_strndup";
end;

define C-function g-strnfill
  input parameter length_ :: <C-unsigned-long>;
  input parameter fill_char_ :: <C-unsigned-char>;
  result res :: <C-string>;
  c-name: "g_strnfill";
end;

define C-function g-strreverse
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strreverse";
end;

define C-function g-strrstr
  input parameter haystack_ :: <C-string>;
  input parameter needle_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strrstr";
end;

define C-function g-strrstr-len
  input parameter haystack_ :: <C-string>;
  input parameter haystack_len_ :: <C-signed-long>;
  input parameter needle_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strrstr_len";
end;

define C-function g-strsignal
  input parameter signum_ :: <C-signed-int>;
  result res :: <C-string>;
  c-name: "g_strsignal";
end;

define C-function g-strstr-len
  input parameter haystack_ :: <C-string>;
  input parameter haystack_len_ :: <C-signed-long>;
  input parameter needle_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strstr_len";
end;

define C-function g-strtod
  input parameter nptr_ :: <C-string>;
  input parameter endptr_ :: <C-string>;
  result res :: <C-double>;
  c-name: "g_strtod";
end;

define C-function g-strup
  input parameter string_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_strup";
end;

define C-function g-strv-get-type
  result res :: <C-long>;
  c-name: "g_strv_get_type";
end;

define C-function g-strv-length
  input parameter str_array_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_strv_length";
end;

define C-function g-test-bug
  input parameter bug_uri_snippet_ :: <C-string>;
  c-name: "g_test_bug";
end;

define C-function g-test-bug-base
  input parameter uri_pattern_ :: <C-string>;
  c-name: "g_test_bug_base";
end;

define C-function g-test-fail
  c-name: "g_test_fail";
end;

define C-function g-test-log-type-name
  input parameter log_type_ :: <GTestLogType>;
  result res :: <C-string>;
  c-name: "g_test_log_type_name";
end;

define C-function g-test-queue-destroy
  input parameter destroy_func_ :: <C-function-pointer>;
  input parameter destroy_data_ :: <C-void*>;
  c-name: "g_test_queue_destroy";
end;

define C-function g-test-queue-free
  input parameter gfree_pointer_ :: <C-void*>;
  c-name: "g_test_queue_free";
end;

define C-function g-test-rand-double
  result res :: <C-double>;
  c-name: "g_test_rand_double";
end;

define C-function g-test-rand-double-range
  input parameter range_start_ :: <C-double>;
  input parameter range_end_ :: <C-double>;
  result res :: <C-double>;
  c-name: "g_test_rand_double_range";
end;

define C-function g-test-rand-int
  result res :: <C-signed-int>;
  c-name: "g_test_rand_int";
end;

define C-function g-test-rand-int-range
  input parameter begin_ :: <C-signed-int>;
  input parameter end_ :: <C-signed-int>;
  result res :: <C-signed-int>;
  c-name: "g_test_rand_int_range";
end;

define C-function g-test-run
  result res :: <C-signed-int>;
  c-name: "g_test_run";
end;

define C-function g-test-run-suite
  input parameter suite_ :: <GTestSuite>;
  result res :: <C-signed-int>;
  c-name: "g_test_run_suite";
end;

define C-function g-test-timer-elapsed
  result res :: <C-double>;
  c-name: "g_test_timer_elapsed";
end;

define C-function g-test-timer-last
  result res :: <C-double>;
  c-name: "g_test_timer_last";
end;

define C-function g-test-timer-start
  c-name: "g_test_timer_start";
end;

define C-function g-test-trap-assertions
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter assertion_flags_ :: <C-unsigned-long>;
  input parameter pattern_ :: <C-string>;
  c-name: "g_test_trap_assertions";
end;

define C-function g-test-trap-fork
  input parameter usec_timeout_ :: <C-unsigned-long>;
  input parameter test_trap_flags_ :: <GTestTrapFlags>;
  result res :: <C-boolean>;
  c-name: "g_test_trap_fork";
end;

define C-function g-test-trap-has-passed
  result res :: <C-boolean>;
  c-name: "g_test_trap_has_passed";
end;

define C-function g-test-trap-reached-timeout
  result res :: <C-boolean>;
  c-name: "g_test_trap_reached_timeout";
end;

define C-function g-timeout-add-full
  input parameter priority_ :: <C-signed-int>;
  input parameter interval_ :: <C-unsigned-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_timeout_add_full";
end;

define C-function g-timeout-add-seconds-full
  input parameter priority_ :: <C-signed-int>;
  input parameter interval_ :: <C-unsigned-int>;
  input parameter function_ :: <C-function-pointer>;
  input parameter data_ :: <C-void*>;
  input parameter notify_ :: <C-function-pointer>;
  result res :: <C-unsigned-int>;
  c-name: "g_timeout_add_seconds_full";
end;

define C-function g-timeout-source-new
  input parameter interval_ :: <C-unsigned-int>;
  result res :: <GSource>;
  c-name: "g_timeout_source_new";
end;

define C-function g-timeout-source-new-seconds
  input parameter interval_ :: <C-unsigned-int>;
  result res :: <GSource>;
  c-name: "g_timeout_source_new_seconds";
end;

define C-function g-ucs4-to-utf16
  input parameter str_ :: <C-unsigned-int*>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-short*>;
  c-name: "g_ucs4_to_utf16";
end;

define C-function g-ucs4-to-utf8
  input parameter str_ :: <C-unsigned-int*>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_ucs4_to_utf8";
end;

define C-function g-unichar-break-type
  input parameter c_ :: <C-unsigned-int>;
  result res :: <GUnicodeBreakType>;
  c-name: "g_unichar_break_type";
end;

define C-function g-unichar-combining-class
  input parameter uc_ :: <C-unsigned-int>;
  result res :: <C-signed-int>;
  c-name: "g_unichar_combining_class";
end;

define C-function g-unichar-compose
  input parameter a_ :: <C-unsigned-int>;
  input parameter b_ :: <C-unsigned-int>;
  input parameter ch_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_unichar_compose";
end;

define C-function g-unichar-decompose
  input parameter ch_ :: <C-unsigned-int>;
  input parameter a_ :: <C-unsigned-int*>;
  input parameter b_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_unichar_decompose";
end;

define C-function g-unichar-digit-value
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-signed-int>;
  c-name: "g_unichar_digit_value";
end;

define C-function g-unichar-fully-decompose
  input parameter ch_ :: <C-unsigned-int>;
  input parameter compat_ :: <C-boolean>;
  input parameter result_ :: <C-unsigned-int*>;
  input parameter result_len_ :: <C-unsigned-long>;
  result res :: <C-unsigned-long>;
  c-name: "g_unichar_fully_decompose";
end;

define C-function g-unichar-get-mirror-char
  input parameter ch_ :: <C-unsigned-int>;
  input parameter mirrored_ch_ :: <C-unsigned-int*>;
  result res :: <C-boolean>;
  c-name: "g_unichar_get_mirror_char";
end;

define C-function g-unichar-get-script
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <GUnicodeScript>;
  c-name: "g_unichar_get_script";
end;

define C-function g-unichar-isalnum
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isalnum";
end;

define C-function g-unichar-isalpha
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isalpha";
end;

define C-function g-unichar-iscntrl
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_iscntrl";
end;

define C-function g-unichar-isdefined
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isdefined";
end;

define C-function g-unichar-isdigit
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isdigit";
end;

define C-function g-unichar-isgraph
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isgraph";
end;

define C-function g-unichar-islower
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_islower";
end;

define C-function g-unichar-ismark
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_ismark";
end;

define C-function g-unichar-isprint
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isprint";
end;

define C-function g-unichar-ispunct
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_ispunct";
end;

define C-function g-unichar-isspace
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isspace";
end;

define C-function g-unichar-istitle
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_istitle";
end;

define C-function g-unichar-isupper
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isupper";
end;

define C-function g-unichar-iswide
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_iswide";
end;

define C-function g-unichar-iswide-cjk
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_iswide_cjk";
end;

define C-function g-unichar-isxdigit
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_isxdigit";
end;

define C-function g-unichar-iszerowidth
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_iszerowidth";
end;

define C-function g-unichar-to-utf8
  input parameter c_ :: <C-unsigned-int>;
  input parameter outbuf_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_unichar_to_utf8";
end;

define C-function g-unichar-tolower
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_unichar_tolower";
end;

define C-function g-unichar-totitle
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_unichar_totitle";
end;

define C-function g-unichar-toupper
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-unsigned-int>;
  c-name: "g_unichar_toupper";
end;

define C-function g-unichar-type
  input parameter c_ :: <C-unsigned-int>;
  result res :: <GUnicodeType>;
  c-name: "g_unichar_type";
end;

define C-function g-unichar-validate
  input parameter ch_ :: <C-unsigned-int>;
  result res :: <C-boolean>;
  c-name: "g_unichar_validate";
end;

define C-function g-unichar-xdigit-value
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-signed-int>;
  c-name: "g_unichar_xdigit_value";
end;

define C-function g-unicode-canonical-ordering
  input parameter string_ :: <C-unsigned-int*>;
  input parameter len_ :: <C-unsigned-long>;
  c-name: "g_unicode_canonical_ordering";
end;

define C-function g-unicode-script-from-iso15924
  input parameter iso15924_ :: <C-unsigned-int>;
  result res :: <GUnicodeScript>;
  c-name: "g_unicode_script_from_iso15924";
end;

define C-function g-unicode-script-to-iso15924
  input parameter script_ :: <GUnicodeScript>;
  result res :: <C-unsigned-int>;
  c-name: "g_unicode_script_to_iso15924";
end;

define C-function g-unlink
  input parameter filename_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_unlink";
end;

define C-function g-unsetenv
  input parameter variable_ :: <C-string>;
  c-name: "g_unsetenv";
end;

define C-function g-uri-escape-string
  input parameter unescaped_ :: <C-string>;
  input parameter reserved_chars_allowed_ :: <C-string>;
  input parameter allow_utf8_ :: <C-boolean>;
  result res :: <C-string>;
  c-name: "g_uri_escape_string";
end;

define C-function g-uri-parse-scheme
  input parameter uri_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_uri_parse_scheme";
end;

define C-function g-uri-unescape-segment
  input parameter escaped_string_ :: <C-string>;
  input parameter escaped_string_end_ :: <C-string>;
  input parameter illegal_characters_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_uri_unescape_segment";
end;

define C-function g-uri-unescape-string
  input parameter escaped_string_ :: <C-string>;
  input parameter illegal_characters_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_uri_unescape_string";
end;

define C-function g-usleep
  input parameter microseconds_ :: <C-unsigned-long>;
  c-name: "g_usleep";
end;

define C-function g-utf16-to-ucs4
  input parameter str_ :: <C-unsigned-short*>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int*>;
  c-name: "g_utf16_to_ucs4";
end;

define C-function g-utf16-to-utf8
  input parameter str_ :: <C-unsigned-short*>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-string>;
  c-name: "g_utf16_to_utf8";
end;

define C-function g-utf8-casefold
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_casefold";
end;

define C-function g-utf8-collate
  input parameter str1_ :: <C-string>;
  input parameter str2_ :: <C-string>;
  result res :: <C-signed-int>;
  c-name: "g_utf8_collate";
end;

define C-function g-utf8-collate-key
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_collate_key";
end;

define C-function g-utf8-collate-key-for-filename
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_collate_key_for_filename";
end;

define C-function g-utf8-find-next-char
  input parameter p_ :: <C-string>;
  input parameter end_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_utf8_find_next_char";
end;

define C-function g-utf8-find-prev-char
  input parameter str_ :: <C-string>;
  input parameter p_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_utf8_find_prev_char";
end;

define C-function g-utf8-get-char
  input parameter p_ :: <C-string>;
  result res :: <C-unsigned-int>;
  c-name: "g_utf8_get_char";
end;

define C-function g-utf8-get-char-validated
  input parameter p_ :: <C-string>;
  input parameter max_len_ :: <C-signed-long>;
  result res :: <C-unsigned-int>;
  c-name: "g_utf8_get_char_validated";
end;

define C-function g-utf8-normalize
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter mode_ :: <GNormalizeMode>;
  result res :: <C-string>;
  c-name: "g_utf8_normalize";
end;

define C-function g-utf8-offset-to-pointer
  input parameter str_ :: <C-string>;
  input parameter offset_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_offset_to_pointer";
end;

define C-function g-utf8-pointer-to-offset
  input parameter str_ :: <C-string>;
  input parameter pos_ :: <C-string>;
  result res :: <C-signed-long>;
  c-name: "g_utf8_pointer_to_offset";
end;

define C-function g-utf8-prev-char
  input parameter p_ :: <C-string>;
  result res :: <C-string>;
  c-name: "g_utf8_prev_char";
end;

define C-function g-utf8-strchr
  input parameter p_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "g_utf8_strchr";
end;

define C-function g-utf8-strdown
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_strdown";
end;

define C-function g-utf8-strlen
  input parameter p_ :: <C-string>;
  input parameter max_ :: <C-signed-long>;
  result res :: <C-signed-long>;
  c-name: "g_utf8_strlen";
end;

define C-function g-utf8-strncpy
  input parameter dest_ :: <C-string>;
  input parameter src_ :: <C-string>;
  input parameter n_ :: <C-unsigned-long>;
  result res :: <C-string>;
  c-name: "g_utf8_strncpy";
end;

define C-function g-utf8-strrchr
  input parameter p_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter c_ :: <C-unsigned-int>;
  result res :: <C-string>;
  c-name: "g_utf8_strrchr";
end;

define C-function g-utf8-strreverse
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_strreverse";
end;

define C-function g-utf8-strup
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_strup";
end;

define C-function g-utf8-substring
  input parameter str_ :: <C-string>;
  input parameter start_pos_ :: <C-signed-long>;
  input parameter end_pos_ :: <C-signed-long>;
  result res :: <C-string>;
  c-name: "g_utf8_substring";
end;

define C-function g-utf8-to-ucs4
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-int*>;
  c-name: "g_utf8_to_ucs4";
end;

define C-function g-utf8-to-ucs4-fast
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_written_ :: <C-signed-long*>;
  result res :: <C-unsigned-int*>;
  c-name: "g_utf8_to_ucs4_fast";
end;

define C-function g-utf8-to-utf16
  input parameter str_ :: <C-string>;
  input parameter len_ :: <C-signed-long>;
  input parameter items_read_ :: <C-signed-long*>;
  input parameter items_written_ :: <C-signed-long*>;
  output parameter error_ :: <GError*>;
  result res :: <C-unsigned-short*>;
  c-name: "g_utf8_to_utf16";
end;

define C-function g-utf8-validate
  input parameter str_ :: <C-string>;
  input parameter max_len_ :: <C-signed-long>;
  output parameter end_ :: <C-string>;
  result res :: <C-boolean>;
  c-name: "g_utf8_validate";
end;

define C-function g-variant-get-gtype
  result res :: <C-long>;
  c-name: "g_variant_get_gtype";
end;

define C-function g-warn-message
  input parameter domain_ :: <C-string>;
  input parameter file_ :: <C-string>;
  input parameter line_ :: <C-signed-int>;
  input parameter func_ :: <C-string>;
  input parameter warnexpr_ :: <C-string>;
  c-name: "g_warn_message";
end;

