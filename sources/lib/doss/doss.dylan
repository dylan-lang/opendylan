Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  DOSS abstract classes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* History:
version 0.1 verified basic algorithm - no support for doss header, closures, 
            apply & floats yet.
version 0.2 added header; changed doss-slot-value to use a getter rather than
            a slot-descriptor, dropped post-load-cleanup? (but kept
            post-load-cleanup).
*/

define constant $doss-0-1-version = as(<byte-vector>, "DOSS 0.1");
define constant $doss-0-2-version = as(<byte-vector>, "DOSS 0.2");
define constant $doss-0-3-version = as(<byte-vector>, "DOSS 0.3");

define constant $doss-version = 3;
define constant $doss-version-string = $doss-0-3-version;

define open class <doss-io-manager> (<object>)
  // Ideally a dumped stream should begin with some useful info (how many 
  // objects/bytes in the stream etc).
  // For now we provide a version string and a header size.  The version
  //  string is at most 16 bytes long.
  // Following this is the header-size stored in at most 8 bytes.
  // Following this is the remainder of the header.
  slot doss-version :: <integer> = $doss-version;
  constant slot doss-version-string :: <byte-vector> = $doss-version-string;
  constant slot header-size :: <integer> = 32;
  constant slot header-size-offset :: <integer> = 16;
end class <doss-io-manager>;

// Policy objects decide which objects to treat specially during a dump
// and provide mechanism for various special treatments. Has to be here
// because of forward reference problems
define open class <doss-policy> (<doss-io-manager>) 
end class <doss-policy>;


/// doss codes
/*
  doss reads & writes a simple byte-coded language with variable length
  "instructions".  Codes indicate object definitions or object references.
  The code space is organised thus:
    0- 63  conventional codes for references to slot objects, classes,
         variables etc
   64-127 integer codes, integer follows #bytes encoded in least 6 bits
  128-191 character codes (same encoding as integers)
  192-255 reserved (for other (tagged) immediate objects
  
  A better allocation might be to have e.g. 4 bits for byte count (recursive
  if overflow) and up to 15 embedded byte-count codes, e.g.
    obdef-codes 0:bytecount id
    obid--codes 1:bytecount id
    class-codes 2:bytecount id
    int---codes 3:bytecount int 
    char--codes 4:bytecount char
*/

// codes for #() #t & #f
define constant $empty-list-code :: <integer> = 0;
define constant $true-code       :: <integer> = 1;
define constant $false-code      :: <integer> = 2;

// next encoded integer is 2's complement integer
define constant $integer-start   :: <integer> = 64;

// next encoded integer is ascii code of a character
define constant $character-start :: <integer> = 128;

// next encoded integer is id of loaded object
define constant $object-id-code  :: <integer> = 3;

// I think one really needs an explicit unbound object
define constant $unbound-code    :: <integer> = 4;

// object id followed by object's definition
define constant $object-code     :: <integer> = 5;

// encodings of 'variable' references
define constant $class-code      :: <integer> = 6;
define constant $keyword-code    :: <integer> = 7;
define constant $symbol-code     :: <integer> = 8;
define constant $variable-code   :: <integer> = 9;
define constant $string-code     :: <integer> = 10;
define constant $apply-code      :: <integer> = 11;

// to fix the circularity of the void element in tables being used as a 
// key in object-ids
define constant $void-code       :: <integer> = 12;

// for "value" objects (symbols strings) that need ids.
define constant $val-obj-id-code :: <integer> = 13;

// run-length encoding
define constant $repeat-code     :: <integer> = 14;

// specials; pair
define constant $pair-code       :: <integer> = 15;

// the various float formats
define constant $float-code      :: <integer> = 16;
define constant $double-code     :: <integer> = 17;
define constant $extended-code   :: <integer> = 18;

define constant $footer-code     :: <integer> = 19;  

define constant $unbound-proxy = list("doss-unbound-proxy");

define method unbound-proxy (dosser :: <doss-io-manager>) => (object)
  $unbound-proxy
end method unbound-proxy;

// Table's void element can't be handled by the specials mechanisms in
// policies since these mechanisms depend on objects being keys in 
// <object-table>s and $table-void-element is the object <table> uses
// to indicate a non-key entry.
define constant $table-void-element = void-element(make(<object-table>));


// Debugging switch.
define variable *debug-print* = #f;
define variable *debug-stream* = *standard-output*;

define function my-format (#rest all-args) => ()
  if (*debug-print*)
    apply(format, *debug-stream*, all-args)
  end
end function my-format;

// eof
