Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DOM strings

//
// typedef sequence<unsigned short> DOMString;
//

define constant <DOM-string> = <string>;


/// DOM timestamps

//
// typedef unsigned long long DOMTimeStamp;
//

define constant <DOM-timestamp> = <integer>;


/// DOM keys

//
// typedef Object DOMKey;
//

define constant <DOM-key> = <integer>;


/// Exceptions

// exception DOMException {
//   unsigned short code;
// };
// 
// const unsigned short INDEX_SIZE_ERR           = 1;
// const unsigned short DOMSTRING_SIZE_ERR       = 2;
// const unsigned short HIERARCHY_REQUEST_ERR    = 3;
// const unsigned short WRONG_DOCUMENT_ERR       = 4;
// const unsigned short INVALID_CHARACTER_ERR    = 5;
// const unsigned short NO_DATA_ALLOWED_ERR      = 6;
// const unsigned short NO_MODIFICATION_ALLOWED_ERR = 7;
// const unsigned short NOT_FOUND_ERR            = 8;
// const unsigned short NOT_SUPPORTED_ERR        = 9;
// const unsigned short INUSE_ATTRIBUTE_ERR      = 10;
// const unsigned short INVALID_STATE_ERR        = 11;
// const unsigned short SYNTAX_ERR               = 12;
// const unsigned short INVALID_MODIFICATION_ERR = 13;
// const unsigned short NAMESPACE_ERR            = 14;
// const unsigned short INVALID_ACCESS_ERR       = 15;
//

define open abstract class <DOM-exception> 
    (<format-string-condition>, <error>)
  sealed each-subclass slot exception-code :: <integer> = 0,
    init-keyword: code:;
end class <DOM-exception>;

define sealed domain make (subclass(<DOM-exception>));
define sealed domain initialize (<DOM-exception>);

define constant $index-size-error           :: <integer> = 1;
define constant $string-size-error          :: <integer> = 2;
define constant $hierarchy-request-error    :: <integer> = 3;
define constant $wrong-document-error       :: <integer> = 4;
define constant $invalid-character-error    :: <integer> = 5;
define constant $no-data-allowed-error      :: <integer> = 6;
define constant $no-modification-allowed-error :: <integer> = 7;
define constant $not-found-error            :: <integer> = 8;
define constant $not-supported-error        :: <integer> = 9;
define constant $inuse-attribute-error      :: <integer> = 10;
define constant $invalid-state-error        :: <integer> = 11;
define constant $syntax-error               :: <integer> = 12;
define constant $invalid-modification-error :: <integer> = 13;
define constant $namespace-error            :: <integer> = 14;
define constant $invalid-access-error       :: <integer> = 15;

define sealed class <index-size-error> (<DOM-exception>)
  keyword code: = $index-size-error;
end class <index-size-error>;

define sealed class <string-size-error> (<DOM-exception>)
  keyword code: = $string-size-error;
end class <string-size-error>;

define sealed class <hierarchy-request-error> (<DOM-exception>)
  keyword code: = $hierarchy-request-error;
end class <hierarchy-request-error>;

define sealed class <wrong-document-error> (<DOM-exception>)
  keyword code: = $wrong-document-error;
end class <wrong-document-error>;

define sealed class <invalid-character-error> (<DOM-exception>)
  keyword code: = $invalid-character-error;
end class <invalid-character-error>;

define sealed class <no-data-allowed-error> (<DOM-exception>)
  keyword code: = $no-data-allowed-error;
end class <no-data-allowed-error>;

define sealed class <no-modification-allowed-error> (<DOM-exception>)
  keyword code: = $no-modification-allowed-error;
end class <no-modification-allowed-error>;

define sealed class <not-found-error> (<DOM-exception>)
  keyword code: = $not-found-error;
end class <not-found-error>;

define sealed class <not-supported-error> (<DOM-exception>)
  keyword code: = $not-supported-error;
end class <not-supported-error>;

define sealed class <inuse-attribute-error> (<DOM-exception>)
  keyword code: = $inuse-attribute-error;
end class <inuse-attribute-error>;

define sealed class <invalid-state-error> (<DOM-exception>)
  keyword code: = $invalid-state-error;
end class <invalid-state-error>;

define sealed class <syntax-error> (<DOM-exception>)
  keyword code: = $syntax-error;
end class <syntax-error>;

define sealed class <invalid-modification-error> (<DOM-exception>)
  keyword code: = $invalid-modification-error;
end class <invalid-modification-error>;

define sealed class <namespace-error> (<DOM-exception>)
  keyword code: = $namespace-error;
end class <namespace-error>;

define sealed class <invalid-access-error> (<DOM-exception>)
  keyword code: = $invalid-access-error;
end class <invalid-access-error>;
