Module:       xml-internals
Synopsis:     XML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// XML printer

define sealed method print-xml-to-file
    (xml :: <node>, file :: <string>) => ()
  with-open-file (stream = file, direction: #"output")
    print-xml-to-stream(xml, stream)
  end
end method print-xml-to-file;

define sealed method print-xml-to-string
    (xml :: <node>) => (string :: <string>)
  with-output-to-string (stream)
    print-xml-to-stream(xml, stream)
  end
end method print-xml-to-string;


define sealed generic print-xml-to-stream
    (xml :: <node>, stream :: <stream>) => ();

define thread variable *printing-markup?* :: <boolean> = #f;

define sealed method print-xml-to-stream
    (document :: <document>, stream :: <stream>) => ()
  write(stream, "<?xml version=\"1.0\"");
  when (encoding(document))
    write(stream, " encoding=\"");
    write(stream, encoding(document));
    write(stream, "\"")
  end;
  write(stream, "?>");
  new-line(stream);
  //---*** Shouldn't doctype just be one of the children?
  when (doctype(document))
    print-xml-to-stream(doctype(document), stream)
  end;
  for (child in child-nodes(document))
    print-xml-to-stream(child, stream)
  end;
  new-line(stream)
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (fragment :: <document-fragment>, stream :: <stream>) => ()
  for (child in child-nodes(fragment))
    print-xml-to-stream(child, stream)
  end;
  when (node-type(parent-node(fragment)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (doctype :: <document-type>, stream :: <stream>) => ()
  let docelt = document-element(owner-document(doctype));
  write(stream, "<!DOCTYPE ");
  write(stream, if (docelt) node-name(docelt) else "NO-DOC-ELEMENT" end);
  when (public-id(doctype))
    write(stream, " PUBLIC '");
    write(stream, public-id(doctype));
    write(stream, "'")
  end;
  when (system-id(doctype))
    write(stream, " SYSTEM '");
    write(stream, system-id(doctype));
    write(stream, "'")
  end;
  when (internal-subset(doctype))
    new-line(stream);
    write(stream, "[");
    write(stream, internal-subset(doctype));
    write(stream, "]")
  end;
  write(stream, ">");
  when (node-type(parent-node(doctype)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (elt :: <element>, stream :: <stream>) => ()
  write(stream, "<");
  dynamic-bind (*printing-markup?* = #t)
    print-xml-string(tag-name(elt), stream);
    let attributes :: <named-node-map> = attributes(elt);
    for (i :: <integer> from 0 below length(attributes))
      let attribute :: <attribute> = item(attributes, i);
      when (specified?(attribute))
        write(stream, " ");
        print-xml-to-stream(attribute, stream)
      end
    end
  end;
  //--- Is this the right test to decide whether to use '<foo/>'?
  if (~has-child-nodes?(elt))
    write(stream, "/>");
  else
    write(stream, ">");
    for (child in child-nodes(elt))
      print-xml-to-stream(child, stream)
    end;
    write(stream, "</");
    write(stream, tag-name(elt));
    write(stream, ">")
  end
end method print-xml-to-stream;

// Note this simply won't get called if 'specified?' is false
//---*** What about default values?
define sealed method print-xml-to-stream
    (attr :: <attribute>, stream :: <stream>) => ()
  dynamic-bind (*printing-markup?* = #t)
    print-xml-string(name(attr), stream);
    write(stream, "=\"");
    print-xml-string(value(attr), stream);
    write(stream, "\"")
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (text :: <text>, stream :: <stream>) => ()
  print-xml-string(data(text), stream)
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (cdata :: <CDATA-section>, stream :: <stream>) => ()
  write(stream, "<![CDATA[");
  write(stream, data(cdata));
  write(stream, "]]>");
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (comment :: <comment>, stream :: <stream>) => ()
  write(stream, "<!-- ");
  write(stream, data(comment));
  write(stream, " -->");
  when (node-type(parent-node(comment)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (entity :: <entity>, stream :: <stream>) => ()
  write(stream, "<!ENTITY ");
  if (node-value(entity))
    write(stream, "\"");
    dynamic-bind (*printing-markup?* = #t)
      print-xml-string(node-value(entity), stream)
    end;
    write(stream, "\"")
  else
    when (public-id(entity))
      write(stream, " PUBLIC '");
      write(stream, public-id(entity));
      write(stream, "'")
    end;
    when (system-id(entity))
      write(stream, " SYSTEM '");
      write(stream, system-id(entity));
      write(stream, "'")
    end;
    when (notation-name(entity))
      write(stream, " NDATA ");
      write(stream, notation-name(entity))
    end
  end;
  write(stream, ">");
  when (node-type(parent-node(entity)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (ref :: <entity-reference>, stream :: <stream>) => ()
  write(stream, "&");
  write(stream, node-name(ref));
  write(stream, ";")
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (notation :: <notation>, stream :: <stream>) => ()
  write(stream, "<!NOTATION ");
  write(stream, node-name(notation));
  if (public-id(notation))
    write(stream, " PUBLIC '");
    write(stream, public-id(notation));
    when (system-id(notation))
      write(stream, " ");
      write(stream, system-id(notation))
    end;
  else
    write(stream, " SYSTEM ");
    write(stream, system-id(notation))
  end;
  write(stream, ">");
  when (node-type(parent-node(notation)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;

define sealed method print-xml-to-stream
    (instr :: <processing-instruction>, stream :: <stream>) => ()
  write(stream, "<?");
  write(stream, target(instr));
  when (data(instr))
    write(stream, " ");
    write(stream, data(instr))
  end;
  write(stream, "?>");
  when (node-type(parent-node(instr)) = $document-node)
    new-line(stream)
  end
end method print-xml-to-stream;


define sealed method print-xml-string
    (string :: <string>, stream :: <stream>) => ()
  without-bounds-checks
    for (i :: <integer> from 0 below size(string))
      let char :: <character> = string[i];
      case
	*printing-markup?* & char == '"' =>
	  write(stream, "&quot;");
	char == '<' =>
	  write(stream, "&lt;");
	char == '>' =>
	  write(stream, "&gt;");
	char == '&' =>
	  write(stream, "&amp;");
	otherwise =>
	  write-element(stream, char);
      end
    end
  end
end method print-xml-string;

define sealed method print-xml-string
    (char :: <character>, stream :: <stream>) => ()
  case
    *printing-markup?* & char == '"' =>
      write(stream, "&quot;");
    char == '<' =>
      write(stream, "&lt;");
    char == '>' =>
      write(stream, "&gt;");
    char == '&' =>
      write(stream, "&amp;");
    otherwise =>
      write-element(stream, char);
  end
end method print-xml-string;
