Module:       xml-internals
Synopsis:     XML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stream-based XML parser, implemented by gluing to 'Expat'
 
define sealed method parse-xml-from-file
    (file :: <string>, #key text? = #t)
 => (xml :: <xml-document>)
  with-open-file (stream = file, direction: #"input")
    let document = create-xml-document();
    parse-xml-from-stream(stream,
                          document: document,
                          text?:    text?)
  end
end method parse-xml-from-file;
 

define thread variable *xml-document* :: false-or(<xml-document>) = #f;
 
define sealed method parse-xml-from-stream
    (stream :: <stream>, #key document, text? = #t)
 => (xml :: <xml-document>)
  let document :: <xml-document> = document | create-xml-document();
  dynamic-bind (*xml-document* = document)
    with-XML-parser (parser :: <DOM-XML-parser>, document: document)
      //--- Maybe we should read the document in smaller chunks?
      let buffer = read-to-end(stream);
      XML-parse(parser, buffer, #t)
    end
  end;
  document
end method parse-xml-from-stream;
 

/// The DOM-based XML parser and handlers
 
define sealed class <DOM-XML-parser> (<XML-parser>)
  sealed constant slot %document :: <xml-document>,
    required-init-keyword: document:;
  sealed slot %current-node :: false-or(<node>) = #f;
end class <DOM-XML-parser>;
 
define method initialize
    (parser :: <DOM-XML-parser>, #key document)
  next-method();
  parser.%current-node := document
end method initialize;
 
define sealed domain make (subclass(<DOM-XML-parser>));
define sealed domain initialize (<DOM-XML-parser>);
 

define sealed method XML-start-element-handler
    (parser :: <DOM-XML-parser>, name :: <string>, attributes :: <attributes>)
 => (handled? :: <boolean>)
  // Create the new element, and add it to the current node
  let elt = create-element(parser.%document, name);
  append-child(parser.%current-node, elt);
  // Add the attributes
  let n = size(attributes);
  for (i :: <integer> from 0 below n by 2)
    let name  = attributes[i + 0];
    let value = attributes[i + 1];
    set-attribute(elt, name, value)
  end;
  // "Push" the new element onto the parser's context
  parser.%current-node := elt;
  #t
end method XML-start-element-handler;
 
define sealed method XML-end-element-handler
    (parser :: <DOM-XML-parser>, name :: <string>)
 => (handled? :: <boolean>)
  // "Pop" the parser's context
  parser.%current-node := parent-node(parser.%current-node);
  #t
end method XML-end-element-handler;
 
define sealed method XML-character-data-handler
    (parser :: <DOM-XML-parser>, data :: <byte-string>)
 => (handled? :: <boolean>)
  let cdata? = parsing-CDATA-section?(parser);
  let last   = last-child(parser.%current-node);
  case
    // Try to merge adjacent CDATA section nodes
    cdata? & last & node-type(last) = $cdata-section-node =>
      append-data(last, data);
    // Try to merge adjacent text nodes
    ~cdata? & last & node-type(last) = $text-node =>
      append-data(last, data);
    otherwise =>
      let text = if (cdata?) create-CDATA-section(parser.%document, data)
                 else create-text-node(parser.%document, data) end;
      append-child(parser.%current-node, text);
  end;
  #t
end method XML-character-data-handler;
 
define sealed method XML-processing-instruction-handler
    (parser :: <DOM-XML-parser>, target :: <string>, data :: <string>)
 => (handled? :: <boolean>)
  let instr = create-processing-instruction(parser.%document, target, data);
  append-child(parser.%current-node, data);
  #t
end method XML-processing-instruction-handler;
 
define sealed method XML-default-handler
    (parser :: <DOM-XML-parser>, data :: <byte-string>)
 => (handled? :: <boolean>)
  //---*** What else do we need besides comment handlers?
  case
    (string-equal?(data, "<!-- ", end1: min(size(data), 5))
     & string-equal?(data, " -->", start1: max(0, size(data) - 4))) =>
      let data    = copy-sequence(data, start: 5, end: size(data) - 4);
      let comment = create-comment(parser.%document, data);
      append-child(parser.%current-node, comment);
      #t;
    otherwise =>
      #f;
  end
end method XML-default-handler;
 
define sealed method XML-unparsed-entity-decl-handler
    (parser :: <DOM-XML-parser>, entity-name :: <string>, base :: false-or(<string>),
     system-id :: <string>, public-id :: false-or(<string>), notation-name :: <string>)
 => (handled? :: <boolean>)
  //---*** Fill this in
  #t
end method XML-unparsed-entity-decl-handler;
 
define sealed method XML-notation-decl-handler
    (parser :: <DOM-XML-parser>, notation-name :: <string>, base :: false-or(<string>),
     system-id :: false-or(<string>), public-id :: false-or(<string>))
 => (handled? :: <boolean>)
  //---*** Fill this in
  #t
end method XML-notation-decl-handler;
