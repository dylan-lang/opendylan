Module:       expat-internals
Synopsis:     Dylan FFI wrapper for Expat
Author:       Evan Williams, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// FFI bindings

define constant $null-string = null-pointer(<C-string>); 

define C-pointer-type <C-Char**> => <C-Char*>;


// define C-subtype <XML_Char> (<C-char>) end;
// define C-subtype <XML_Char*> (<C-char*>) end;
// define C-subtype <XML_LChar> (<C-char>) end;
// define C-subtype <XML_LChar*> (<C-char*>) end;


// Creates an XML_Parser object that can parse an external general entity;
// openEntityNames is a space-separated list of the names of the entities that are open
// for the parse of this entity (including the name of this one);
// encoding is the externally specified encoding,
// or null if there is no externally specified encoding.
// This can be called at any point after the first call to an ExternalEntityRefHandler
// so longer as the parser has not yet been freed.
// The new parser is completely independent and may safely be used in a separate thread.
// The handlers and userData are initialized from the parser argument.
// Returns 0 if out of memory.  Otherwise returns a new XML_Parser object.
define C-function XML_ParserCreate
  parameter encoding :: <C-string>;
  result value :: <XML_parser>;
  c-name: "XML_ParserCreate";
end;
  
define C-function XML_ParserFree
  parameter parser :: <XML_parser>;
  c-name: "XML_ParserFree";
end C-function;


define C-function XML_ErrorString
  parameter code :: <C-int>;
  result value :: <C-string>;
  c-name: "XML_ErrorString";
end;


define C-function XML_SetElementHandler
  parameter parser :: <XML_parser>;
  parameter _start :: <C-function-pointer>;	// XML_StartElementHandler
  parameter _end   :: <C-function-pointer>;	// XML_EndElementHandler
  c-name: "XML_SetElementHandler";
end;

define C-function XML_SetCharacterDataHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_CharacterDataHandler
  c-name: "XML_SetCharacterDataHandler";
end;

define C-function XML_SetProcessingInstructionHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_ProcessingInstructionHandler
  c-name: "XML_SetProcessingInstructionHandler";
end;

define C-function XML_SetDefaultHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_DefaultHandler
  c-name: "XML_SetDefaultHandler";
end;

define C-function XML_SetUnparsedEntityDeclHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_UnparsedEntityDeclHandler
  c-name: "XML_SetUnparsedEntityDeclHandler";
end;

define C-function XML_SetNotationDeclHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_NotationDeclHandler
  c-name: "XML_SetNotationDeclHandler";
end;

define C-function XML_SetExternalEntityRefHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_ExternalEntityRefHandler
  c-name: "XML_SetExternalEntityRefHandler";
end;

define C-function XML_SetUnknownEncodingHandler
  parameter parser  :: <XML_parser>;
  parameter handler :: <C-function-pointer>;	// XML_UnknownEncodingHandler
  parameter encodingHandlerData :: <C-void*>;
  c-name: "XML_SetUnknownEncodingHandler";
end;

define C-function XML_DefaultCurrent
  parameter parser :: <XML_parser>;
  c-name: "XML_DefaultCurrent";
end;

define C-function XML_SetUserData
  parameter parser   :: <XML_parser>;
  parameter userData :: <C-void*>;
  c-name: "XML_SetUserData";
end;

define C-function XML_SetBase
  parameter parser :: <XML_parser>;
  parameter base   :: <C-string>;
  result value :: <C-int>;
  c-name: "XML_SetBase";
end;

define C-function XML_GetBase
  parameter parser :: <XML_parser>;
  result value :: <C-string>;
  c-name: "XML_GetBase";
end;

define C-function XML_Parse
  parameter parser   :: <XML_parser>;
  parameter s        :: <C-string>;
  parameter len      :: <C-int>;
  parameter isFinal  :: <C-boolean>;
  parameter encoding :: <C-string>;
  result value :: <C-int>;
  c-name: "XML_Parse";
end;

define C-function XML_GetBuffer
  parameter parser :: <XML_parser>;
  parameter len    :: <C-int>;
  result value :: <C-void*>;
  c-name: "XML_GetBuffer";
end;

define C-function XML_ParseBuffer
  parameter parser  :: <XML_parser>;
  parameter len     :: <C-int>;
  parameter isFinal :: <C-boolean>;
  result value :: <C-int>;
  c-name: "XML_ParseBuffer";
end;

define C-function XML_ExternalEntityParserCreate
  parameter parser          :: <XML_parser>;
  parameter openEntityNames :: <C-string>;
  parameter encoding        :: <C-string>;
  result value :: <XML_parser>;
  c-name: "XML_ExternalEntityParserCreate";
end;

// If XML_Parse or XML_ParseBuffer have returned 0, then XML_GetErrorCode
// returns information about the error.
define C-function XML_GetErrorCode
  parameter parser :: <XML_parser>;
  result value :: <C-int>;
  c-name: "XML_GetErrorCode";
end;

define C-function XML_GetCurrentLineNumber
  parameter parser :: <XML_parser>;
  result value :: <C-int>;
  c-name: "XML_GetCurrentLineNumber";
end;

define C-function XML_GetCurrentColumnNumber
  parameter parser :: <XML_parser>;
  result value :: <C-int>;
  c-name: "XML_GetCurrentColumnNumber";
end;

define C-function XML_GetCurrentByteIndex
  parameter parser :: <XML_parser>;
  result value :: <C-signed-long>;
  c-name: "XML_GetCurrentByteIndex";
end;
