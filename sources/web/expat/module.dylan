Module:       Dylan-User
Synopsis:     Dylan FFI wrapper for Expat
Author:       Evan Williams, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module expat
  create $XML-error-async-entity,
	 $XML-error-attribute-external-entity-ref,
	 $XML-error-bad-char-ref,
	 $XML-error-binary-entity-ref,
	 $XML-error-duplicate-attribute,
	 $XML-error-external-entity-handling,
	 $XML-error-incorrect-encoding,
	 $XML-error-invalid-token,
	 $XML-error-junk-after-doc-element,
	 $XML-error-misplaced-xml-pi,
	 $XML-error-no-elements,
	 $XML-error-no-memory,
	 $XML-error-none,
	 $XML-error-param-entity-ref,
	 $XML-error-partial-char,
	 $XML-error-recursive-entity-ref,
	 $XML-error-syntax,
	 $XML-error-tag-mismatch,
	 $XML-error-unclosed-cdata-section,
	 $XML-error-unclosed-token,
	 $XML-error-undefined-entity,
	 $XML-error-unknown-encoding;

  create <XML-parser>,
	 <XML-parser-error>,
	 create-XML-parser,
	 destroy-XML-parser,
	 parsing-CDATA-section?,
	 \with-XML-parser,
	 XML-character-data-handler,
	 XML-default-handler,
         XML-default-current,
	 XML-end-element-handler,
	 XML-error-string,
	 XML-get-base,
	 XML-get-current-byte-index,
	 XML-get-current-column-number,
	 XML-get-current-line-number,
	 XML-get-error-code,
	 XML-notation-decl-handler, 
	 XML-parse,
	 XML-parse-buffer,
	 XML-processing-instruction-handler,
	 XML-set-base,
	 XML-start-element-handler,
	 XML-unparsed-entity-decl-handler;

  create <attributes>;
end module expat;

define module expat-internals
  use functional-dylan,
    exclude: { position, position-if, count };
  use dylan-extensions,
    import: { \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error };
  use simple-format;			// for debugging
  use c-ffi;

  use expat, export: all;

  export XML_ParserCreate,
	 XML_ParserFree,
	 XML_ErrorString,
	 XML_SetElementHandler,
	 XML_SetCharacterDataHandler,
	 XML_SetProcessingInstructionHandler,
	 XML_SetDefaultHandler,
	 XML_SetUnparsedEntityDeclHandler,
	 XML_SetNotationDeclHandler,
	 XML_SetExternalEntityRefHandler,
	 XML_SetUnknownEncodingHandler,
	 XML_DefaultCurrent,
	 XML_SetUserData,
	 XML_SetBase,
	 XML_GetBase,
	 XML_Parse,
	 XML_GetBuffer,
	 XML_ParseBuffer,
	 XML_ExternalEntityParserCreate,
	 XML_GetErrorCode,
	 XML_GetCurrentLineNumber,
	 XML_GetCurrentColumnNumber,
	 XML_GetCurrentByteIndex;
end module expat-internals;
