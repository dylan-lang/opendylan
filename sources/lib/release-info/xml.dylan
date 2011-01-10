Module:    release-info-internals
Synopsis:  Simple XML support
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// XML class hierarchy

define class <xml-document> (<object>)
  slot document-location :: <locator>,
    required-init-keyword: location:;
  slot document-element :: false-or(<xml-element>) = #f,
    init-keyword: element:;
end class <xml-document>;

define class <xml-node> (<object>)
end class <xml-node>;

define class <xml-element> (<xml-node>)
  constant slot node-name :: <string>,
    required-init-keyword: name:;
  constant slot node-children :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  constant slot node-attributes :: <string-table> = make(<string-table>),
    init-keyword: attributes:;
  slot node-text :: <string> = "",
    init-keyword: text:;
end class <xml-element>;

define method read-xml-document 
    (locator :: <locator>) => (page :: <xml-document>)
  let parser = make(<xml-parser>);
  make(<xml-document>, 
       location: locator,
       element: tokenize-xml(parser, locator));
end method read-xml-document;

define method node-attribute 
    (xml-element :: <xml-element>, attribute :: <string>)
 => (value :: false-or(<string>))
  element(xml-element.node-attributes, attribute, default: #f)
end method node-attribute;

define method node-attribute-setter
    (value :: <string>, xml-element :: <xml-element>, attribute :: <string>)
 => (value :: <string>)
  element(xml-element.node-attributes, attribute) := value
end method node-attribute-setter;

define method select-nodes
    (node :: <xml-element>, path :: <string>)
 => (nodes :: <vector>)
  let pos = position(path, '/');
  if (pos)
    let subnode = select-single-node(node, copy-sequence(path, end: pos));
    if (subnode)
      select-nodes(subnode, copy-sequence(path, start: pos + 1))
    else
      #[]
    end
  else
    let results = make(<stretchy-object-vector>);
    for (child in node.node-children)
      if (child.node-name = path)
        add!(results, child)
      end
    end;
    results
  end
end method select-nodes;

define method select-single-node
    (node :: <xml-element>, path :: <string>)
 => (node :: false-or(<xml-element>))
  let nodes = select-nodes(node, path);
  unless (empty?(nodes))
    nodes[0]
  end
end method select-single-node;

define method select-node-text
    (node :: <xml-element>, path :: <string>,
     #key default :: <string> = "")
 => (text :: <string>)
  let nodes = select-nodes(node, path);
  unless (empty?(nodes))
    nodes[0].node-text
  end
    | default
end method select-node-text;


/// XML error handling

define class <xml-error> (<simple-error>)
end class <xml-error>;

define function xml-error
    (message :: <string>, #rest args) => ()
  error(make(<xml-error>,
	     format-string: message,
	     format-arguments: args))
end function xml-error;


/// XML parsing

define constant $debug = #f;

define function parser-debug
    (message :: <string>, #rest args) => ()
  when ($debug)
    apply(format-out, message, args);
    format-out("\n")
  end
end function parser-debug;

define class <xml-parser> (<object>)
  constant slot parser-stack :: <deque> = make(<deque>);
  slot parser-element :: false-or(<xml-element>) = #f;
  slot parser-text :: <string> = "";
end class <xml-parser>;

define method parse-attributes
    (string :: <string>, 
     #key start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (table :: <string-table>)
  let attributes = make(<string-table>);
  let pos :: <integer> = start;
  let last-pos :: <integer> = start;
  parser-debug(">      Parsing attributes from '%s'",
               copy-sequence(string, start: start, end: _end));
  while (pos < _end)
    while (pos < _end & string[pos] = ' ')
      pos := pos + 1
    end;
    last-pos := pos;
    while (pos < _end & string[pos] ~= '=' & string[pos] ~= ' ')
      pos := pos + 1
    end;
    if (pos < _end)
      let name = copy-sequence(string, start: last-pos, end: pos);
      while (pos < _end & string[pos] = ' ')
        pos := pos + 1
      end;
      pos := pos + 1;
      parser-debug("Found %s, checking '%s'", name, copy-sequence(string, start: pos));
      while (pos < _end & string[pos] = ' ')
        pos := pos + 1
      end;
      if (string[pos] == '"')
        parser-debug("Found quote");
        pos := pos + 1;
        let pos2 = pos;
        while (pos2 < _end & string[pos2] ~= '"')
          pos2 := pos2 + 1
        end;
        if (pos2 < _end)
          parser-debug("Found closing quote");
          let value = copy-sequence(string, start: pos, end: pos2);
          attributes[name] := value;
          parser-debug("Found value '%s'", value);
          pos := pos2 + 1;
          last-pos := pos;
          if (pos < _end)
            parser-debug("Starting again at %d", pos)
          end
        else
          xml-error("Missing quote in '%s'", copy-sequence(string, start: start, end: _end))
        end
      end
    end
  end;
  attributes
end method parse-attributes;
    
define method parse-tag 
    (string :: <string>, #key start, end: _end)
 => (name :: <string>, attributes :: <string-table>)
  let string
    = copy-sequence(string, start: start | 0, end: _end | size(string));
  parser-debug(">   Parsing '%s'", string);
  let options-pos = find-key(string, curry(\=, ' '));
  if (options-pos)
    let name = copy-sequence(string, end: options-pos);
    values(name, parse-attributes(string, start: options-pos + 1, end: size(string)))
  else
    values(string, make(<string-table>))
  end
end method parse-tag;

define method parse-line
    (parser :: <xml-parser>, line :: <string>)
 => (remainder :: false-or(<string>))
  let closing-tag? = #f;
  let start = 0;
  let pos = 0;
  let _end = size(line);
  let stack = parser.parser-stack;
  parser-debug("> Processing line '%s'", line);
  while (pos < _end)
    select (line[pos])
      '<' =>
        if (pos + 1 < _end)
          if (line[pos + 1] = '/')
            parser.parser-text := copy-sequence(line, start: start, end: pos);
            closing-tag? := #t;
            pos := pos + 1
          end;
        end;
        start := pos;
      '>' =>
        let end-pos = pos;
        if (pos > 0 & line[pos - 1] = '/')
          closing-tag? := #t;
          end-pos := pos - 1;
        end;
        let (name, attributes) = parse-tag(line, start: start + 1, end: end-pos);
        if (empty?(name))
          xml-error("No name was found! (index %d in '%s')", start + 1, line);
        end;
        if (closing-tag?)
          if (end-pos ~= pos)
            let node = make(<xml-element>, name: name, attributes: attributes);
            let parent = parser.parser-element;
            add!(parent.node-children, node);
            parser-debug("++ Children now %=", parent.node-children);
          else
            let node = parser.parser-element;
            unless (name = node.node-name)
              xml-error("Mismatched closing tag '%s'", name)
            end;
            unless (empty?(attributes))
              xml-error("Found attributes '%=' for close tag '%s'", attributes, name)
            end;
            node.node-text := parser.parser-text;
            parser.parser-text := "";
            unless (empty?(stack))
              let parent = pop(stack);
              add!(parent.node-children, node);
              parser-debug("++ Children now %=", parent.node-children);
              parser.parser-element := parent;
            end
          end;
          closing-tag? := #f;
        else
          let node = make(<xml-element>, name: name, attributes: attributes);
          if (parser.parser-element)
            push(stack, parser.parser-element)
          end;
          parser.parser-element := node
        end;
        start := pos + 1;
      otherwise =>
        #f;
    end;
    pos := pos + 1;
  end;
  if (pos > 0) copy-sequence(line, start: start) end
end method parse-line;

define method tokenize-xml 
    (parser :: <xml-parser>, stream :: <stream>)
 => (element :: <xml-element>)
  let remainder = #f;
  block (return)
    until (stream-at-end?(stream))
      let line = read-line(stream, on-end-of-stream: #f);
      if (line)
        if (remainder)
          line := concatenate(remainder, " ", line)
        end;
        let (new-remainder) = parse-line(parser, line);
        remainder := new-remainder
      else
        return()
      end
    end
  end;
  parser.parser-element
end method tokenize-xml;

/*
define method tokenize-xml
    (parser :: <xml-parser>, locator :: <file-url>)
 => (element :: <xml-element>)
  let stream = make(<http-stream>, location: locator);
  block ()
    tokenize-xml(parser, stream)
  cleanup
    close(stream)
  end
end method tokenize-xml;
*/

define method tokenize-xml
    (parser :: <xml-parser>, locator :: <file-locator>)
 => (element :: <xml-element>)
  with-open-file (stream = locator, direction: #"input")
    tokenize-xml(parser, stream);
  end
end method tokenize-xml;
