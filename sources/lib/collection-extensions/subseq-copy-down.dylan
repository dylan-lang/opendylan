module: subseq

/*
 * XXX: This file is temporary.
 *      It contains things not supported by GD but needed in OD.
 *      -- Ingo Albrecht, Jan 2006
 */

define sealed copy-down-method aref
    (c :: <byte-vector-subsequence>, #rest rest) => (result :: <byte>);

define sealed copy-down-method aref-setter
    (value :: <byte>, c :: <byte-vector-subsequence>, #rest rest)
 => (result :: <byte>);

define sealed copy-down-method element
    (seq :: <byte-vector-subsequence>, key :: <integer>, #key default = subseq-no-default)
 => elt :: <byte>;

define sealed copy-down-method element-setter
    (value :: <byte>, seq :: <byte-vector-subsequence>, key :: <integer>)
 => (result :: <byte>);
