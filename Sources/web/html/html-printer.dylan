Module:       html-internals
Synopsis:     HTML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HTML printer

define sealed method print-html-to-file
    (html :: <node>, file :: <string>) => ()
  with-open-file (stream = file, direction: #"output")
    print-html-to-stream(html, stream)
  end
end method print-html-to-file;

define sealed method print-html-to-string
    (html :: <node>) => (string :: <string>)
  with-output-to-string (stream)
    print-html-to-stream(html, stream)
  end
end method print-html-to-string;


define sealed generic print-html-to-stream
    (html :: <node>, stream :: <stream>) => ();

define thread variable *printing-markup?* :: <boolean> = #f;

define sealed method print-html-to-stream
    (document :: <html-document>, stream :: <stream>) => ()
  when (doctype(document))
    print-html-to-stream(doctype(document), stream)
  end;
  for (child in child-nodes(document))
    print-html-to-stream(child, stream)
  end;
  new-line(stream)
end method print-html-to-stream;

define sealed method print-html-to-stream
    (doctype :: <document-type>, stream :: <stream>) => ()
  //---*** Print the document type
end method print-html-to-stream;

define sealed method print-html-to-stream
    (elt :: <html-element>, stream :: <stream>) => ()
  write(stream, "<");
  dynamic-bind (*printing-markup?* = #t)
    print-html-string(tag-name(elt), stream);
    let attributes :: <named-node-map> = attributes(elt);
    for (i :: <integer> from 0 below length(attributes))
      let attribute :: <attribute> = item(attributes, i);
      write(stream, " ");
      print-html-to-stream(attribute, stream)
    end
  end;
  write(stream, ">");
  when (has-child-nodes?(elt))
    for (child in child-nodes(elt))
      print-html-to-stream(child, stream)
    end;
    dynamic-bind (*printing-markup?* = #t)
      write(stream, "</");
      print-html-string(tag-name(elt), stream);
      write(stream, ">")
    end
  end
end method print-html-to-stream;

define sealed method print-html-to-stream
    (attr :: <attribute>, stream :: <stream>) => ()
  dynamic-bind (*printing-markup?* = #t)
    //---*** What about 'specified?' and default values?
    print-html-string(name(attr), stream);
    write(stream, "=\"");
    print-html-string(value(attr), stream);
    write(stream, "\"")
  end
end method print-html-to-stream;

define sealed method print-html-to-stream
    (text :: <text>, stream :: <stream>) => ()
  print-html-string(data(text), stream)
end method print-html-to-stream;

define sealed method print-html-to-stream
    (comment :: <comment>, stream :: <stream>) => ()
  write(stream, "<!--");
  write(stream, data(comment));
  write(stream, "-->");
  when (node-type(parent-node(comment)) = $document-node)
    new-line(stream)
  end
end method print-html-to-stream;

define sealed method print-html-to-stream
    (entity :: <entity-reference>, stream :: <stream>) => ()
  write(stream, "&");
  write(stream, node-name(entity));
  write(stream, ";")
end method print-html-to-stream;

define sealed method print-html-to-stream
    (markup :: <sgml-markup>, stream :: <xstream>) => ()
  write(stream, "<!");
  write(stream, data(markup));
  write(stream, ">")
end method print-html-to-stream;


define sealed method print-html-string
    (string :: <string>, stream :: <stream>) => ()
  without-bounds-checks
    for (i :: <integer> from 0 below size(string))
      let char :: <character> = string[i];
      let code :: <integer>   = as(<integer>, char);
      case
	*printing-markup?* & char == '"' =>
	  write(stream, "&quot;");
	*character->entity-names*[code] =>
	  write(stream, "&");
	  write(stream, *character->entity-names*[code]);
	  write(stream, ";");
	otherwise =>
	  write-element(stream, char);
      end
    end
  end
end method print-html-string;

define sealed method print-html-string
    (char :: <character>, stream :: <stream>) => ()
  let code :: <integer> = as(<integer>, char);
  case
    *printing-markup?* & char == '"' =>
      write(stream, "&quot;");
    *character->entity-names*[code] =>
      write(stream, "&");
      write(stream, *character->entity-names*[code]);
      write(stream, ";");
    otherwise =>
      write-element(stream, char);
  end
end method print-html-string;
