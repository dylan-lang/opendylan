Module:       expat-internals
Synopsis:     Dylan FFI wrapper for Expat
Author:       Evan Williams, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Error codes

define sealed class <XML-parser-error> (<error>)
  sealed constant slot parser :: false-or(<XML-parser>) = #f,
    init-keyword: parser:;
  sealed constant slot error-code :: <integer>,
    required-init-keyword: error-code:;
  sealed slot error-string :: <string> = "";
  sealed virtual slot line-number   :: false-or(<integer>), setter: #f;
  sealed virtual slot column-number :: false-or(<integer>), setter: #f;
  sealed virtual slot byte-index    :: false-or(<integer>), setter: #f;
end class <XML-parser-error>;

define method initialize (e :: <XML-parser-error>, #key)
  next-method();
  error-string(e) := as-XML-string(XML-error-string(error-code(e)))
end method initialize;

define sealed domain make (subclass(<XML-parser-error>));
define sealed domain initialize (<XML-parser-error>);


define sealed method line-number
    (parser-error :: <XML-parser-error>) => (line :: false-or(<integer>))
  let parser = parser(parser-error);
  parser & XML_GetCurrentLineNumber(parser.%C-XML-parser)
end method line-number;

define sealed method column-number
    (parser-error :: <XML-parser-error>) => (column :: false-or(<integer>))
  let parser = parser(parser-error);
  parser & XML_GetCurrentColumnNumber(parser.%C-XML-parser)
end method column-number;

define sealed method byte-index
    (parser-error :: <XML-parser-error>) => (index :: false-or(<integer>))
  let parser = parser(parser-error);
  parser & XML_GetCurrentByteIndex(parser.%C-XML-parser)
end method byte-index;


define constant $XML-error-none                     :: <integer> = 0;
define constant $XML-error-no-memory                :: <integer> = 1;
define constant $XML-error-syntax                   :: <integer> = 2;
define constant $XML-error-no-elements              :: <integer> = 3;
define constant $XML-error-invalid-token            :: <integer> = 4;
define constant $XML-error-unclosed-token           :: <integer> = 5;
define constant $XML-error-partial-char             :: <integer> = 6;
define constant $XML-error-tag-mismatch             :: <integer> = 7;
define constant $XML-error-duplicate-attribute      :: <integer> = 8;
define constant $XML-error-junk-after-doc-element   :: <integer> = 9;
define constant $XML-error-param-entity-ref         :: <integer> = 10;
define constant $XML-error-undefined-entity         :: <integer> = 11;
define constant $XML-error-recursive-entity-ref     :: <integer> = 12;
define constant $XML-error-async-entity             :: <integer> = 13;
define constant $XML-error-bad-char-ref             :: <integer> = 14;
define constant $XML-error-binary-entity-ref        :: <integer> = 15;
define constant $XML-error-attribute-external-entity-ref :: <integer> = 16;
define constant $XML-error-misplaced-xml-pi         :: <integer> = 17;
define constant $XML-error-unknown-encoding         :: <integer> = 18;
define constant $XML-error-incorrect-encoding       :: <integer> = 19;
define constant $XML-error-unclosed-cdata-section   :: <integer> = 20;
define constant $XML-error-external-entity-handling :: <integer> = 21;


/// FFI wrapper for Expat parser object

define C-subtype <XML_parser> (<C-dylan-object>) end;


/// Utility for getting C strings into useful XML strings

//---*** This should create a Unicode string if necessary
define inline function as-XML-string
    (C-string :: <C-string>) => (string :: <byte-string>)
  as(<byte-string>, C-string)
end function as-XML-string;


/// Dylan wrapper for Expat parser

define open class <XML-parser> (<object>)
  sealed constant slot %C-XML-parser :: <XML_parser> = create-XML-parser();
  sealed slot parsing-CDATA-section? :: <boolean> = #f;
end class <XML-parser>;

define method initialize (parser :: <XML-parser>, #key)
  next-method();
  let c-parser = parser.%C-XML-parser;
  register-C-Dylan-object(parser);
  XML_SetUserData(c-parser, export-C-Dylan-object(parser));
  // Set up all the callbacks
  XML_SetElementHandler(c-parser, XML-start-element-handler-callback, XML-end-element-handler-callback);
  XML_SetCharacterDataHandler(c-parser, XML-character-data-handler-callback);
  XML_SetProcessingInstructionHandler(c-parser, XML-processing-instruction-handler-callback);
  XML_SetDefaultHandler(c-parser, XML-default-handler-callback);
  XML_SetUnparsedEntityDeclHandler(c-parser, XML-unparsed-entity-decl-handler-callback);
  XML_SetNotationDeclHandler(c-parser, XML-notation-decl-handler-callback);
end method initialize;

// Constructs a new Expat parser
// 'encoding' is the encoding specified by the external protocol or null if
// there is none specified.
define function create-XML-parser
    (#key encoding :: <C-string> = $null-string)
 => (xml-parser :: <XML_parser>)
  let parser :: <XML_parser> = XML_ParserCreate(encoding);
  when (null-pointer?(parser))
    error(make(<XML-parser-error>, error-code: $XML-ERROR-NO-MEMORY))
  end;
  parser
end function create-XML-parser;

define sealed method destroy-XML-parser
    (parser :: <XML-parser>) => ()
  XML_ParserFree(parser.%C-XML-parser);
  unregister-C-Dylan-object(parser)
end method destroy-XML-parser;


define macro with-XML-parser
  { with-XML-parser (?parser:name :: ?type:expression, #rest ?options:*) ?:body end }
    => { let ?parser :: ?type = make(?type, ?options);
         block ()
	   ?body
	 cleanup
	   destroy-XML-parser(?parser);
	 end }
end macro with-XML-parser;


/// Handlers

define constant <attributes> = <vector>; 

define open generic XML-start-element-handler
    (parser :: <XML-parser>, name :: <string>, attributes :: <attributes>)
 => (handled? :: <boolean>);

// 'attributes' is a vector of name/value pairs
define method XML-start-element-handler
    (parser :: <XML-parser>, name :: <string>, attributes :: <attributes>)
 => (handled? :: <boolean>)
  #f
end method XML-start-element-handler;


// typedef void (*XML_StartElementHandler)(void *userData,
//					   const XML_Char *name,
//					   const XML_Char **atts);
// 'atts' is vector of name/value pairs, terminated by 0
// Each name and value is 0-terminated
define method XML-start-element-handler-wrapper
    (c-parser :: <XML_parser>, c-name :: <C-string>, c-attributes :: <C-Char**>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser> = import-C-Dylan-object(c-parser);
  let attributes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let name = as-XML-string(c-name);
  let i :: <integer> = 0;
  for (attribute :: <string> = pointer-cast(<C-string>, pointer-value(c-attributes, index: i))
	 then pointer-cast(<C-string>, pointer-value(c-attributes, index: i)),
       until: null-pointer?(attribute))
    i := i + 1;				// stupid 'for' is broken
    add!(attributes, as-XML-string(attribute))
  end;
  XML-start-element-handler(parser, name, attributes)
end method XML-start-element-handler-wrapper;

define c-callable-wrapper XML-start-element-handler-callback of XML-start-element-handler-wrapper
  parameter c-parser   :: <XML_parser>;
  parameter name       :: <C-string>;
  parameter attributes :: <C-Char**>;
end;


define open generic XML-end-element-handler
    (parser :: <XML-parser>, name :: <string>)
 => (handled? :: <boolean>);

define method XML-end-element-handler
    (parser :: <XML-parser>, name :: <string>)
 => (handled? :: <boolean>)
  #f
end method XML-end-element-handler;

// typedef void (*XML_EndElementHandler)(void *userData,
//					 const XML_Char *name);
define method XML-end-element-handler-wrapper
    (c-parser :: <XML_parser>, c-name :: <C-string>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser> = import-C-Dylan-object(c-parser);
  let name = as-XML-string(c-name);
  XML-end-element-handler(parser, name)
end method XML-end-element-handler-wrapper;

define c-callable-wrapper XML-end-element-handler-callback of XML-end-element-handler-wrapper
  parameter c-parser :: <XML_parser>;
  parameter name     :: <C-string>;
end;


define open generic XML-character-data-handler
    (parser :: <XML-parser>, data :: <string>)
 => (handled? :: <boolean>);

define method XML-character-data-handler
    (parser :: <XML-parser>, data :: <string>)
 => (handled? :: <boolean>)
  #f
end method XML-character-data-handler;

// typedef void (*XML_CharacterDataHandler)(void *userData,
//					    const XML_Char *s,
//					    int len);
// Note that 's' is not 0 terminated
define method XML-character-data-handler-wrapper
    (c-parser :: <XML_parser>, c-data :: <C-Char*>, len :: <integer>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser> = import-C-Dylan-object(c-parser);
  if (len = 0)
    parsing-CDATA-section?(parser) := ~parsing-CDATA-section?(parser);
  else
    //---*** This should create a Unicode string if necessary
    let data :: <byte-string> = make(<byte-string>, size: len);
    for (i :: <integer> from 0 below len)
      data[i] := as(<character>, c-data[i])
    end;
    XML-character-data-handler(parser, data)
  end
end method XML-character-data-handler-wrapper;

define c-callable-wrapper XML-character-data-handler-callback of XML-character-data-handler-wrapper
  parameter c-parser :: <XML_parser>;
  parameter data     :: <C-Char*>;
  parameter len      :: <C-int>;
end;


define open generic XML-processing-instruction-handler
    (parser :: <XML-parser>, target :: <string>, data :: <string>)
 => (handled? :: <boolean>);

define method XML-processing-instruction-handler
    (parser :: <XML-parser>, target :: <string>, data :: <string>)
 => (handled? :: <boolean>)
  #f
end method XML-processing-instruction-handler;

// typedef void (*XML_ProcessingInstructionHandler)(void *userData,
//						    const XML_Char *target,
//						    const XML_Char *data);
// 'target' and 'data' are 0 terminated
define method XML-processing-instruction-handler-wrapper
    (c-parser :: <XML_parser>, c-target :: <C-string>, c-data :: <C-string>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser> = import-C-Dylan-object(c-parser);
  let target = as-XML-string(c-target);
  let data   = as-XML-string(c-data);
  XML-processing-instruction-handler(parser, target, data)
end method XML-processing-instruction-handler-wrapper;

define c-callable-wrapper XML-processing-instruction-handler-callback of XML-processing-instruction-handler-wrapper
  parameter c-parser :: <XML_parser>;
  parameter target   :: <C-string>;
  parameter data     :: <C-string>;
end;


// This is called for any characters in the XML document for which there is
// no applicable handler.  This includes both characters that are part of
// markup which is of a kind that is not reported (comments, markup
// declarations), or characters that are part of a construct which could be
// reported but for which no handler has been supplied. The characters are
// passed exactly as they were in the XML document except that they will be
// encoded in UTF-8.  Line boundaries are not normalized.  Note that a byte
// order mark character is not passed to the default handler.  If a default
// handler is set, internal entity references are not expanded.  There are
// no guarantees about how characters are divided between calls to the
// default handler: e.g., a comment might be split between multiple calls.
define open generic XML-default-handler
    (parser :: <XML-parser>, data :: <string>)
 => (handled? :: <boolean>);

define method XML-default-handler
    (parser :: <XML-parser>, data :: <string>)
 => (handled? :: <boolean>)
  #f
end method XML-default-handler;

// typedef void (*XML_DefaultHandler)(void *userData,
//				     const XML_Char *s,
//				     int len);
define method XML-default-handler-wrapper
    (c-parser :: <XML_parser>, c-data :: <C-Char*>, len :: <integer>)
 => (handled? :: <boolean>)
  when (len > 0)
    let parser :: <XML-parser>  = import-C-Dylan-object(c-parser);
    //---*** This should create a Unicode string if necessary
    let data   :: <byte-string> = make(<byte-string>, size: len);
    for (i :: <integer> from 0 below len)
      data[i] := as(<character>, c-data[i])
    end;
    XML-default-handler(parser, data)
  end
end method XML-default-handler-wrapper;

define c-callable-wrapper XML-default-handler-callback of XML-default-handler-wrapper
  parameter c-parser :: <XML_parser>;
  parameter data     :: <C-Char*>;
  parameter len      :: <C-int>;
end;


// This is called for a declaration of an unparsed (NDATA) entity.
// The base argument is whatever was set by XML_SetBase.  The entityName,
// systemId and notationName arguments will never be null.  The other
// arguments may be.
define open generic XML-unparsed-entity-decl-handler
    (parser :: <XML-parser>, entity-name :: <string>, base :: false-or(<string>),
     system-id :: <string>, public-id :: false-or(<string>), notation-name :: <string>)
 => (handled? :: <boolean>);

define method XML-unparsed-entity-decl-handler
    (parser :: <XML-parser>, entity-name :: <string>, base :: false-or(<string>),
     system-id :: <string>, public-id :: false-or(<string>), notation-name :: <string>)
 => (handled? :: <boolean>)
  #f
end method XML-unparsed-entity-decl-handler;

// typedef void (*XML_UnparsedEntityDeclHandler)(void *userData,
//						 const XML_Char *entityName,
//					         const XML_Char *base,
//					         const XML_Char *systemId,
//					         const XML_Char *publicId,
//					         const XML_Char *notationName);
define method XML-unparsed-entity-decl-handler-wrapper
    (c-parser :: <XML_parser>, c-entity-name :: <C-string>, c-base :: <C-string>,
     c-system-id :: <C-string>, c-public-id :: <C-string>, c-notation-name :: <C-string>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser>  = import-C-Dylan-object(c-parser);
  let entity-name   = as-XML-string(c-entity-name);
  let base          = ~null-pointer?(c-base) & as-XML-string(c-base);
  let system-id     = as-XML-string(c-system-id);
  let public-id     = ~null-pointer?(c-public-id) & as-XML-string(c-public-id);
  let notation-name = as-XML-string(c-notation-name);
  XML-unparsed-entity-decl-handler(parser, entity-name, base, system-id, public-id, notation-name)
end method XML-unparsed-entity-decl-handler-wrapper;

define c-callable-wrapper XML-unparsed-entity-decl-handler-callback of XML-unparsed-entity-decl-handler-wrapper
  parameter c-parser      :: <XML_parser>;
  parameter entity-name   :: <C-string>;
  parameter c-base        :: <C-string>;
  parameter system-id     :: <C-string>;
  parameter c-public-id   :: <C-string>;
  parameter notation-name :: <C-string>;
end;


// This is called for a declaration of notation.
// The base argument is whatever was set by XML_SetBase.  The notationName
// will never be null.  The other arguments can be.
define open generic XML-notation-decl-handler
    (parser :: <XML-parser>, notation-name :: <string>, base :: false-or(<string>),
     system-id :: false-or(<string>), public-id :: false-or(<string>))
 => (handled? :: <boolean>);

define method XML-notation-decl-handler
    (parser :: <XML-parser>, notation-name :: <string>, base :: false-or(<string>),
     system-id :: false-or(<string>), public-id :: false-or(<string>))
 => (handled? :: <boolean>)
  #f
end method XML-notation-decl-handler;

// typedef void (*XML_NotationDeclHandler)(void *userData,
//					   const XML_Char *notationName,
//					   const XML_Char *base,
//					   const XML_Char *systemId,
//					   const XML_Char *publicId);
define method XML-notation-decl-handler-wrapper
    (c-parser :: <XML_parser>, c-notation-name :: <C-string>, c-base :: <C-string>,
     c-system-id :: <C-string>, c-public-id :: <C-string>)
 => (handled? :: <boolean>)
  let parser :: <XML-parser>  = import-C-Dylan-object(c-parser);
  let notation-name = as-XML-string(c-notation-name);
  let base          = ~null-pointer?(c-base) & as-XML-string(c-base);
  let system-id     = ~null-pointer?(c-system-id) & as-XML-string(c-system-id);
  let public-id     = ~null-pointer?(c-public-id) & as-XML-string(c-public-id);
  XML-notation-decl-handler(parser, notation-name, base, system-id, public-id)
end method XML-notation-decl-handler-wrapper;

define c-callable-wrapper XML-notation-decl-handler-callback of XML-notation-decl-handler-wrapper
  parameter c-parser      :: <XML_parser>;
  parameter notation-name :: <C-string>;
  parameter c-base        :: <C-string>;
  parameter c-system-id   :: <C-string>;
  parameter c-public-id   :: <C-string>;
end;


/*---*** Do we need this?
// This is called for a reference to an external parsed general entity.
// The referenced entity is not automatically parsed.
// The application can parse it immediately or later using
// XML_ExternalEntityParserCreate.
// The parser argument is the parser parsing the entity containing the reference;
// it can be passed as the parser argument to XML_ExternalEntityParserCreate.
// The systemId argument is the system identifier as specified in the entity declaration;
// it will not be null.
// The base argument is the system identifier that should be used as the base for
// resolving systemId if systemId was relative; this is set by XML_SetBase;
// it may be null.
// The publicId argument is the public identifier as specified in the entity declaration,
// or null if none was specified; the whitespace in the public identifier
// will have been normalized as required by the XML spec.
// The openEntityNames argument is a space-separated list of the names of the entities
// that are open for the parse of this entity (including the name of the referenced
// entity); this can be passed as the openEntityNames argument to
// XML_ExternalEntityParserCreate; openEntityNames is valid only until the handler
// returns, so if the referenced entity is to be parsed later, it must be copied.
// The handler should return 0 if processing should not continue because of
// a fatal error in the handling of the external entity.
// In this case the calling parser will return an XML_ERROR_EXTERNAL_ENTITY_HANDLING
// error.
// Note that unlike other handlers the first argument is the parser, not userData.

//---*** Evan, fix this.

//typedef int (*XML_ExternalEntityRefHandler)(XML_Parser parser,
//					    const XML_Char *openEntityNames,
//					    const XML_Char *base,
//					    const XML_Char *systemId,
//					    const XML_Char *publicId);
*/

/*---*** Do we need this?
// This structure is filled in by the XML_UnknownEncodingHandler
// to provide information to the parser about encodings that are unknown
// to the parser.
// The map[b] member gives information about byte sequences
// whose first byte is b.
// If map[b] is c where c is >= 0, then b by itself encodes the Unicode scalar value c.
// If map[b] is -1, then the byte sequence is malformed.
// If map[b] is -n, where n >= 2, then b is the first byte of an n-byte
// sequence that encodes a single Unicode scalar value.
// The data member will be passed as the first argument to the convert function.
// The convert function is used to convert multibyte sequences;
// s will point to a n-byte sequence where map[(unsigned char)*s] == -n.
// The convert function must return the Unicode scalar value
// represented by this byte sequence or -1 if the byte sequence is malformed.
// The convert function may be null if the encoding is a single-byte encoding,
// that is if map[b] >= -1 for all bytes b.
// When the parser is finished with the encoding, then if release is not null,
// it will call release passing it the data member;
// once release has been called, the convert function will not be called again.
//
// Expat places certain restrictions on the encodings that are supported
// using this mechanism.
//
// 1. Every ASCII character that can appear in a well-formed XML document,
// other than the characters
//
//   $@\^`{}~
//
// must be represented by a single byte, and that byte must be the
// same byte that represents that character in ASCII.
//
// 2. No character may require more than 4 bytes to encode.
//
// 3. All characters encoded must have Unicode scalar values <= 0xFFFF,
// (ie characters that would be encoded by surrogates in UTF-16
// are  not allowed).  Note that this restriction doesn't apply to
// the built-in support for UTF-8 and UTF-16.
//
// 4. No Unicode character may be encoded by more than one distinct sequence
// of bytes.

define C-struct <XML_Encoding> 
  array slot map :: <C-int>, length: 256;
  slot data :: <C-void*>;
  slot convert :: <C-function-pointer>;  //    int (*convert)(void *data, const char *s);
  slot release :: <C-function-pointer>;  //    void (*release)(void *data);
  pointer-type-name <XML_Encoding*>;
  cname: "XML_Encoding";
end;

// This is called for an encoding that is unknown to the parser.
// The encodingHandlerData argument is that which was passed as the
// second argument to XML_SetUnknownEncodingHandler.
// The name argument gives the name of the encoding as specified in
// the encoding declaration.
// If the callback can provide information about the encoding,
// it must fill in the XML_Encoding structure, and return 1.
// Otherwise it must return 0.
// If info does not describe a suitable encoding,
// then the parser will return an XML_UNKNOWN_ENCODING error.

//---*** Evan, fix this.

//typedef int (*XML_UnknownEncodingHandler)(void *encodingHandlerData,
//					  const XML_Char *name,
//					  XML_Encoding *info);
*/


// This can be called within a handler for a start element, end element,
// processing instruction or character data.  It causes the corresponding
// markup to be passed to the default handler.
// Within the expansion of an internal entity, nothing will be passed to
// the default handler, although this usually will not happen since setting
// a default handler inhibits expansion of internal entities.
define method XML-default-current (parser :: <XML-parser>)
  XML_DefaultCurrent(parser.%C-XML-parser)
end method XML-default-current;


/*
//---*** Evan, fix this.

// Returns the last value set by XML_SetUserData or null.
// #define XML_GetUserData(parser) (*(void **)(parser))

// If this function is called, then the parser will be passed
// as the first argument to callbacks instead of userData.
// The userData will still be accessible using XML_GetUserData.

define C-function XML_UseParserAsHandlerArg
  parameter parser :: <XML_parser>;
  cname: "XML_UseParserAsHandlerArg";
end;
*/


// Sets the base to be used for resolving relative URIs in system identifiers
// in declarations.  Resolving relative identifiers is left to the application:
// this value will be passed through as the base argument to the
// XML_ExternalEntityRefHandler, XML_NotationDeclHandler
// and XML_UnparsedEntityDeclHandler. The base argument will be copied.
define method XML-set-base
    (parser :: <XML-parser>, base :: <string>)
  let error-code :: <integer> = XML_SetBase(parser.%C-XML-parser, base);
  when (error-code = 0)
    error(make(<XML-parser-error>,
	       error-code: error-code, parser: parser))
  end
end method XML-set-base;

define method XML-get-base
    (parser :: <XML-parser>) => (base :: <string>)
  XML_GetBase(parser.%C-XML-parser)
end method XML-get-base;


/// The parser top-level functions

// Parses some input.
// The last call to XML_Parse must have 'final-buffer?' true;
// 'len' may be zero for this call (or any other).
define method XML-parse
    (parser :: <XML-parser>, buffer :: <string>, final-buffer? :: <boolean>,
     #key encoding :: false-or(<string>))
  let error-code = XML_Parse(parser.%C-XML-parser, buffer, buffer.size, final-buffer?,
			     if (encoding) encoding else $null-string end);
  when (error-code = 0)
    error(make(<XML-parser-error>,
	       error-code: XML-get-error-code(parser),
	       parser: parser))
  end
end method XML-parse;

define method XML-parse-buffer
    (parser :: <XML-parser>, len :: <integer>, final-buffer? :: <boolean>)
  let error-code = XML_ParseBuffer(parser.%C-XML-parser, len, final-buffer?);
  when (error-code = 0)
    error(make(<XML-parser-error>,
	       error-code: XML-get-error-code(parser),
	       parser: parser))
  end
end method XML-parse-buffer;


/// Error handling

// Returns a string describing the error
define method XML-error-string
    (error-code :: <integer>) => (error-string :: <string>)
  XML_ErrorString(error-code)
end method XML-error-string;

define method XML-get-error-code
    (parser :: <XML-parser>) => (error-code :: <integer>)
  XML_GetErrorCode(parser.%C-XML-parser)
end method XML-get-error-code;

// These functions return information about the current parse location.
// They may be called when XML_Parse or XML_ParseBuffer return 0; in this
// case the location is the location of the character at which the error
// was detected.
// They may also be called from any other callback called to report some
// parse event; in this the location is the location of the first of the
// sequence of characters that generated the event.
//--- These could be slots in the condition?
define method XML-get-current-line-number
    (parser :: <XML-parser>) => (line :: <integer>)
  XML_GetCurrentLineNumber(parser.%C-XML-parser)
end method XML-get-current-line-number;

define method XML-get-current-column-number
    (parser :: <XML-parser>) => (column :: <integer>)
  XML_GetCurrentColumnNumber(parser.%C-XML-parser)
end method XML-get-current-column-number;

define method XML-get-current-byte-index
    (parser :: <XML-parser>) => (index :: <integer>)
  XML_GetCurrentByteIndex(parser.%C-XML-parser)
end method XML-get-current-byte-index;
