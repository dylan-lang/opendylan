Module:       html-internals
Synopsis:     HTML parser and printer
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HTML parser
 
// #t means to create a DOM object
// #f means to do nothing
// A function means to call that function as a callback
define constant <action> = type-union(<boolean>, <function>);
 
define sealed method parse-html-from-file
    (file :: <string>,
     #key text-action :: <action> = #t, markup-action :: <action> = #t)
 => (html :: <html-document>)
  with-open-file (stream = file, direction: #"input")
    let document = create-html-document(file);
    parse-html-from-stream(stream,
                           document: document,
                           text-action: text-action,
                           markup-action: markup-action)
  end
end method parse-html-from-file;
 

define thread variable *html-document* :: false-or(<html-document>) = #f;
 
define sealed method parse-html-from-stream
    (stream :: <stream>,
     #key document, text-action :: <action> = #t, markup-action :: <action> = #t)
 => (html :: <html-document>)
  let document :: <html-document> = document | create-html-document("");
  dynamic-bind (*html-document* = document)
    block (end-of-html-stream)
      parse-html(make(<xstream>, inner-stream: stream),
                 context: document,
                 text-action: text-action,
                 markup-action: markup-action,
                 end-of-html-stream: end-of-html-stream)
    end
  end;
  document
end method parse-html-from-stream;
 

/// Condition classes
 
define abstract class <html-parse-error>
    (<format-string-condition>, <error>)
end class <html-parse-error>;
 
define sealed domain make (subclass(<html-parse-error>));
define sealed domain initialize (<html-parse-error>);
 
define sealed class <invalid-character> (<html-parse-error>)
  sealed constant slot invalid-character-code,
    required-init-keyword: code:;
end class <invalid-character>;
 
define sealed class <unbalanced-angle-brackets> (<html-parse-error>)
end class <unbalanced-angle-brackets>;
 
define sealed class <unbalanced-quotes> (<html-parse-error>)
end class <unbalanced-quotes>;
 
define sealed class <unexpected-end-tag> (<html-parse-error>)
end class <unexpected-end-tag>;
 
define sealed class <unexpected-attributes> (<html-parse-error>)
end class <unexpected-attributes>;
 
define sealed class <undefined-html-character> (<html-parse-error>)
end class <undefined-html-character>;
 

/// String resources
 
define constant $result-strings :: <object-deque> = make(<deque>);
 
//---*** This isn't thread-safe...
define macro with-string-buffer
  { with-string-buffer (?result:name) ?:body end }
    => { let ?result :: limited(<stretchy-vector>, of: <byte-character>)
           = if (empty?($result-strings))
               make(limited(<stretchy-vector>, of: <byte-character>))
             else
               pop($result-strings)
             end;
         block ()
           ?result.size :=0;
           ?body
         cleanup
           push($result-strings, ?result)
         end }
end macro with-string-buffer;
 

/// HTML parsing streams
 
define sealed class <xstream> (<wrapper-stream>)
  sealed slot xstream-buffer :: <list> = #();
end class <xstream>;
 
define sealed domain make (subclass(<xstream>));
define sealed domain initialize (<xstream>);
 
define sealed inline method readch
    (x :: <xstream>, #key eof-error? = #t)
 => (char :: false-or(<character>))
  if (~empty?(xstream-buffer(x)))
    pop!(xstream-buffer(x))
  else
    read-element(inner-stream(x), on-end-of-stream: #f)
  end
end method readch;
 
define sealed inline method peekch
    (x :: <xstream>, #key eof-error? = #t)
 => (char :: false-or(<character>))
  if (~empty?(xstream-buffer(x)))
    head(xstream-buffer(x))
  else
    peek(inner-stream(x), on-end-of-stream: #f)
  end
end method peekch;
 
define sealed inline method unreadch
    (x :: <xstream>, char :: <character>) => (char :: <character>)
  push!(xstream-buffer(x), char);
  char
end method unreadch;
 

/// HTML parser context management
 
define thread variable *html-context*  :: false-or(<node>) = #f;
define thread variable *html-context-stack* :: <list> = #();
 
define macro with-html-context
  { with-html-context (?context:expression) ?:body end }
    => { begin
           let with-html-context-body = method () ?body end;
           do-with-html-context(?context, with-html-context-body)
         end }
end macro with-html-context;
 
define sealed method do-with-html-context
    (context, continuation :: <function>) => (#rest values)
  block (context-thunk)
    dynamic-bind (*html-context-stack* = pair(pair(context, context-thunk), *html-context-stack*),
                  *html-context*       = context)
      continuation()
    end
  end
end method do-with-html-context;
 

define sealed method exit-html-context
    (name :: <string>, putback) => ()
  let context
    = find(*html-context-stack*, name,
           test: method (name, elt) node-name(head(elt)) = name end method);
  if (context)
    // If found, it's a non-local exit thunk...
    let thunk = tail(context);
    thunk(putback)
  else
    error(make(<unexpected-end-tag>,
               format-string: "</%s> seen but no <%s> pending",
               format-arguments: vector(name, name)))
  end
end method exit-html-context;
 
define sealed method maybe-exit-html-context
    (name :: <string>, putback) => (putback)
  local method any-context (names)
          position-if(*html-context-stack*,
                      method (elt) member?(node-name(head(elt)), names, test: \=) end method)
        end method;
  select (name by \=)
    "OPTION" =>
      let pos = any-context(#["OPTION", "SELECT"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key == "OPTION")
        exit-html-context(key, putback)
      end;
    "LI" =>
      let pos = any-context(#["LI", "UL", "OL"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key = "LI")
        exit-html-context(key, putback)
      end;
    "P" =>
      let pos = any-context(#["UL", "OL", "LI", "DL", "DT", "DD",
                              "TABLE", "TH", "TD", "P", "PRE",
                              "CENTER", "DIV"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key = "P")
        exit-html-context(key, putback)
      end;
    "DT", "DD" =>
      let pos = any-context(#["DD", "DT", "DL"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key = "DT" | key = "DD")
        exit-html-context(key, putback)
      end;
    "TD", "TH" =>
      let pos = any-context(#["TD", "TH", "TR", "TABLE", "CAPTION"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key = "TD" | key = "TH")
        exit-html-context(key, putback)
      end;
    "TR" =>
      let pos = any-context(#["TR", "TABLE", "CAPTION"]);
      let key = pos & node-name(head(*html-context-stack*[pos]));
      when (key = "TR")
        exit-html-context(key, putback)
      end;
    otherwise =>
      #f
  end;
  putback
end method maybe-exit-html-context;
 

/// The HTML parser
 
define sealed method parse-html
    (xstream :: <xstream>,
     #key context = *html-context*, end-of-html-stream :: <function>,
          text-action :: <action> = #t, markup-action :: <action> = #t)
  let putback = #f;
  // We exit this loop via the 'end-of-html-stream' thunk...
  while (#t)
    let item
      = putback
        | parse-html-content(xstream,
                             text-action: text-action, markup-action: markup-action);
    putback := #f;
    case
      instance?(item, <document-type>) =>
        doctype(*html-document*) := item;
      ~item =>
        end-of-html-stream(#f);
      //--- KMP's code had 'object-empty?', but I don't know what it's supposed to do!
      ~instance?(item, <html-element>)
      | html-empty-element-name?(node-name(item)) =>
        when (select (item by instance?)
                <character-data> => text-action == #t;
                <node>           => markup-action == #t;
                otherwise        => #f;
              end)
          append-child(context, item)
        end;
      otherwise =>
        putback
          := begin
               // Store the new item first -- we won't get another chance to do it
               when (select (item by instance?)
                      <character-data> => text-action == #t;
                      <node>           => markup-action == #t;
                      otherwise        => #f;
                     end)
                 append-child(context, item)
               end;
               with-html-context (item)
                 // This normally returns by non-local exit to 'end-of-html-stream'
                 parse-html(xstream,
                            text-action: text-action,
                            markup-action: markup-action,
                            end-of-html-stream: end-of-html-stream)
               end
             end;
    end
  end;
  context
end method parse-html;
 
define sealed method parse-html-content
    (xstream :: <xstream>,
     #key text-action :: <action> = #t, markup-action :: <action> = #t)
 => (elt :: false-or(type-union(<document-type>, <html-element>, <character-data>, <string>)))
  let c = peekch(xstream, eof-error?: #f);
  case
    ~c =>
      #f;
    c == '<' =>
      parse-html-element(xstream, markup-action: markup-action);
    otherwise =>
      if (text-action)
        let text = parse-html-text(xstream);
        if (text-action == #t)
          create-text-node(*html-document*, text)
        else
          text-action(text);
          text
        end;
      else
        skip-html-text(xstream);
        parse-html-element(xstream, markup-action: markup-action)
      end
  end
end method parse-html-content;
 

define thread variable *parsing-html-tag?* :: <boolean> = #f;
 
define sealed method parse-html-element
    (xstream :: <xstream>, #key markup-action :: <action> = #t)
 => (elt :: false-or(type-union(<document-type>, <html-element>, <character-data>)))
  block (return)
    let c = readch(xstream, eof-error?: #f);
    case
      ~c =>
        #f;
      c ~== '<' =>
        error(make(<unbalanced-angle-brackets>,
                   format-string: "Missing '<' at start of HTML element"));
      otherwise =>
        when (peekch(xstream, eof-error?: #f) == '!')
          let markup = parse-sgml-markup(xstream);
          return(markup)
        end;
        skip-html-whitespace(xstream);
        let c0 = peekch(xstream);
        let open? = (c0 ~== '/');
        unless (open?)
          readch(xstream)
        end;
        let name = parse-html-name(xstream);
        assert(name, "Missing element name within <...>");
        //--- KMP's code used to pass 'empty?: html-empty-element-name?(name)'
        //---*** The context stack should have triple of {name, elt, thunk}
        //---*** so that we don't have to create these useless elements!
        let elt = create-element(*html-document*, name);
        let c = peekch(xstream);
        assert(c ~== '=',
               "Character '%c' seen where not expected", c);
        let attributes :: <stretchy-object-vector> = make(<stretchy-vector>);
        for (tag = parse-html-name(xstream) then parse-html-name(xstream),
             until: ~tag)
          let char = peekch(xstream);
          let val  = if (char == '=')
                       readch(xstream);
                       parse-html-attribute-value(xstream)
                     else
                       #f
                     end;
          if (open?)
            when (markup-action)
              if (markup-action == #t)
                let attribute :: <attribute> = create-attribute(*html-document*, tag);
                value(attribute) := val;
                set-attribute-node(elt, attribute)
              else
                add!(attributes, tag);
                add!(attributes, val)
              end
            end
          else
            error(make(<unexpected-attributes>,
                       format-string: "Found attributes in </%s ...>",
                       format-arguments: vector(name)))
          end;
        finally
          if (open?)
            when (markup-action & markup-action ~== #t)
              markup-action(name, attributes)
            end;
            maybe-exit-html-context(name, elt)
          else
            exit-html-context(name, #f)
          end
        end;
    end
  end
end method parse-html-element;
 
define sealed method parse-html-name
    (xstream :: <xstream>)
 => (name :: false-or(<DOM-string>))
  skip-html-whitespace(xstream);
  let c0 = readch(xstream);
  dynamic-bind (*parsing-html-tag?* = #t)
    case
      c0 = '>' =>
        #f;
      otherwise =>
        with-string-buffer (result)
          for (c :: <byte-character> = c0 then readch(xstream),
               until: html-whitespace?(c) | c == '=' | c == '>')
            add!(result, c);
          finally
            if (c == '=' | c == '>')
              unreadch(xstream, c)
            else
              skip-html-whitespace(xstream)
            end;
            as-uppercase!(as(<DOM-string>, result))
          end
        end;
    end
  end
end method parse-html-name;
 

define constant $html-empty-elements :: <simple-object-vector>
  = #["META", "BR", "IMG", "HR", "INPUT"];
 
define inline method html-empty-element-name?
    (name :: <string>) => (empty? :: <boolean>)
  member?(name, $html-empty-elements, test: \=) & #t
end method html-empty-element-name?;
 

define inline method html-whitespace?
    (c :: <character>) => (whitespace? :: <boolean>)
  c == ' ' | c == '\t' | c == '\r' | c == '\n'
end method html-whitespace?;
 
define sealed method skip-html-whitespace
    (xstream :: <xstream>) => ()
  block (return)
    for (c = readch(xstream, eof-error?: #f) then readch(xstream, eof-error?: #f),
         until: ~c)
      unless (html-whitespace?(c))
        unreadch(xstream, c);
        return(#f)
      end
    end
  end
end method skip-html-whitespace;
 

define sealed method parse-html-attribute-value
    (xstream :: <xstream>)
 => (value :: false-or(<DOM-string>))
  skip-html-whitespace(xstream);
  let c0 = readch(xstream);
  dynamic-bind (*parsing-html-tag?* = #t)
    with-string-buffer (result)
      if (c0 == '"' | c0 == '\'')
        block (return)
          for (c :: <byte-character> = readch(xstream) then readch(xstream),
               until: c == c0)
            when (c == '>')
              // What the heck, it's not fatal
              signal(make(<unbalanced-quotes>,
                          format-string: "Unbalanced '\"' while parsing attribute value"));
              unreadch(xstream, c);
              return(as(<DOM-string>, result))
            end;
            // '&' is handled as quoted in strings by some browsers, but it's
            // really supposed to be escaped
            if (c == '&')
              add!(result, parse-html-entityref(xstream))
            else
              add!(result, c)
            end;
          finally
            return(as(<DOM-string>, result))
          end
        end
      else
        for (c :: <byte-character> = c0 then readch(xstream),
             until: html-whitespace?(c) | c == '>')
          if (c == '&')
            add!(result, parse-html-entityref(xstream))
          else
            add!(result, c)
          end;
        finally
          when (c == '>') unreadch(xstream, c) end;
          as(<DOM-string>, result)
        end
      end
    end
  end
end method parse-html-attribute-value;
 

define sealed method parse-html-text
    (xstream :: <xstream>)
 => (text :: <DOM-string>)
  block (return)
    with-string-buffer (result)
      for (c = readch(xstream, eof-error?: #f) then readch(xstream, eof-error?: #f),
           until: ~c)
        local method done ()
                unreadch(xstream, c);
                return(as(<DOM-string>, result))
              end method;
        let c :: <byte-character> = c;
        case
          c == '<' =>
            done();
          c == '&' =>
            if (~empty?(result))
              done()
            else
              let ref = parse-html-entityref(xstream);
              return(if (instance?(ref, <character>))
                       make(<DOM-string>, size: 1, fill: ref)
                     else
                       ref
                     end)
            end;
          otherwise =>
            add!(result, c)
        end;
      finally
        return(as(<DOM-string>, result))
      end
    end
  end
end method parse-html-text;
 
define sealed method skip-html-text
    (xstream :: <xstream>) => ()
  block (return)
    for (c = readch(xstream, eof-error?: #f) then readch(xstream, eof-error?: #f),
         until: ~c)
      case
        c == '<' =>
          unreadch(xstream, c);
          return(#f);
        c == '&' =>
          skip-html-entityref(xstream)
      end
    end
  end
end method skip-html-text;
 

define variable *unknown-entity-treatment* = #"unparse";
 
define sealed method parse-html-entityref
    (xstream :: <xstream>)
 => (ref :: type-union(<character>, <entity-reference>))
  if (peekch(xstream, eof-error?: #f) = '#')
    parse-html-charref(xstream)
  else
    let semi? = #f;
    let name
      = as(<string>,
           block (return)
             let result :: <stretchy-object-vector> = make(<stretchy-vector>);
             for (c :: <byte-character> = readch(xstream, eof-error?: #f) then readch(xstream, eof-error?: #f),
                  until: c == ';')
               add!(result, c);
               unless (c & alpha-char?(c))
                 when (c) unreadch(xstream, c) end;
                 return(result)
               end;
             finally
               semi? := #t;
               return(result)
             end
           end);
    let character = element(*entity-names->character*, name, default: #f);
    case
      character =>
        character;
      *parsing-html-tag?* | *unknown-entity-treatment* == #"unparse" =>
        when (semi?)
          unreadch(xstream, ';')
        end;
        for (i :: <integer> from size(name) - 1 to 0)
          unreadch(xstream, name[i])
        end;
        '&';
      *unknown-entity-treatment* == #"error" =>
        error(make(<undefined-html-character>,
                   format-string: "Undefined HTML character: '%='",
                   format-arguments: vector(name)));
      otherwise =>
        create-entity-reference(*html-document*, name);
    end
  end
end method parse-html-entityref;
 
define sealed method parse-html-charref
    (xstream :: <xstream>)
 => (ref :: type-union(<character>, <entity-reference>))
  readch(xstream);
  let semi? = #f;
  let name
    = as(<string>,
         block (return)
           let result :: <stretchy-object-vector> = make(<stretchy-vector>);
           for (c :: <byte-character> = readch(xstream, eof-error?: #f) then readch(xstream, eof-error?: #f),
                until: c == ';')
             add!(result, c);
             unless (c & digit-char?(c))
               when (c) unreadch(xstream, c) end;
               return(result)
             end;
           finally
             semi? := #t;
             return(result)
           end
         end);
  let code = string-to-integer(name, default: #f);
  case
    code & code >= 0 & code < 256 =>
      as(<character>, code);
    *parsing-html-tag?* | *unknown-entity-treatment* == #"unparse" =>
      when (semi?)
        unreadch(xstream, ';')
      end;
      for (i :: <integer> from size(name) - 1 to 0)
        unreadch(xstream, name[i])
      end;
      unreadch(xstream, '#');
      '&';
    *unknown-entity-treatment* == #"error" =>
      error(make(<undefined-html-character>,
                 format-string: "Undefined HTML character: '%='",
                 format-arguments: vector(name)));
    otherwise =>
      create-entity-reference(*html-document*, concatenate-as(<string>, "#", name));
  end
end method parse-html-charref;
 
define inline method skip-html-entityref
    (xstream :: <xstream>) => ()
  for (c = readch(xstream) then readch(xstream),
       until: c == ';') #f end
end method skip-html-entityref;
 

/// Random SGML markup parsing
 
define constant $SGML-markup-node :: <integer> = 100;
 
define sealed class <sgml-markup> (<character-data>)
  keyword type: = $SGML-markup-node;
end class <sgml-markup>;
 
//--- Is the comment parsing really completely correct?
define sealed method parse-sgml-markup
    (xstream :: <xstream>)
 => (markup :: false-or(type-union(<document-type>, <character-data>)))
  assert(readch(xstream) == '!',
         "Missing '!' at start of SGML-like markup");
  let stream = make(<string-stream>, direction: #"output");
  let level :: <integer> = 1;
  let markup
    = block (return)
        local method test-char(ch)
          select (ch)
            '>' =>
              dec!(level);
              when (level = 0)
                return(stream-contents(stream))
              end;
            '<' =>
              inc!(level);
            otherwise =>
              #f;
          end;
          write-element(stream, ch);
        end method;
        let ch1 = readch(xstream);
        test-char(ch1);
        let ch2 = readch(xstream);
        test-char(ch2);
        if (ch1 = '-' & ch2 = '-')
          // Process a comment
          let first-dash?  = #f;
          let second-dash? = #f;
          for (ch = readch(xstream) then readch(xstream))
            when (first-dash? & second-dash? & ch = '>')
              return(stream-contents(stream))
            end;
            if (ch = '-')
              if (~first-dash?)
                first-dash? := #t
              else
                when (~second-dash?)
                  second-dash? := #t
                end
              end
            else
              first-dash?  := #f;
              second-dash? := #f
            end;
            write-element(stream, ch);
          end
        else
          for (ch = readch(xstream) then readch(xstream))
            test-char(ch)
          end
        end;
      cleanup
        close(stream)
      end;
  case
    string-equal?(markup, "--", end1: min(2, markup.size)) =>
      assert(string-equal?(markup, "--", start1: max(size(markup) - 2, 0)),
             "Missing '-->' at the end of a comment");
      make(<comment>,
           document: *html-document*,
           name:  "#comment",			// not #"comment"!
           value: copy-sequence(markup, start: 2, end: size(markup) - 2));
    string-equal?(markup, "DOCTYPE", end1: min(7, markup.size)) =>
      make(<document-type>,
           document: *html-document*,
           name:  "#doctype",			// not #"doctype"!
           value: markup);
    otherwise =>
      make(<sgml-markup>,
           document: *html-document*,
           name:  "#markup",			// not #"markup"!
           value: markup);
  end
end method parse-sgml-markup;


/// HTML entity names
 
define variable *entity-names->character* :: <string-table>
  = make(<string-table>);
 
define variable *character->entity-names* :: <simple-object-vector>
  = make(<vector>, size: 256);
 
define function initialize-html-entity-names ()
  local method code (x)
          if (instance?(x, <character>)) as(<integer>, x) else x end
        end method;
  for (entry in #[#("lt", '<'),
                  #("gt", '>'),
                  #("amp", '&'),
                  #("quot", '"'),
                  #("nbsp", 160, 160),  // Non-breaking space
                  #("iexcl", 161, 161), // Inverted exclamation point
                  #("cent", 162, 162),  // Cent sign
                  #("pound", 163, 163), // Pounds Sterling
                  #("curren", 164, 164),// General currency
                  #("yen", 165, 165),   // Yen
                  #("brvbar", 166, 166),// Broken vertical bar
                  #("sect", 167, 167),  // Section
                  #("uml", 168, 168),   // Umlaut (dieresis)
                  #("copy", 169, 169),  // Copyright
                  #("ordf", 170, 170),  // Ordinal indicator, feminine
                  #("laquo", 171, 171), // guillemotleft
                  #("not", 172, 172),   // logical not
                  #("shy", 173, 173),   // soft hyphen
                  #("reg", 174, 174),   // registered trademark
                  #("macr", 175, 175),  // macron
                  #("deg", 176, 176),   // degree
                  #("plusmn", 177, 177),// plus-or-minus
                  #("sup2", 178, 178),  // superscript 2
                  #("sup3", 179, 179),  // superscript 3
                  #("acute", 180, 180), // acute accent
                  #("micro", 181, 181), // micro
                  #("para", 182, 182),  // para
                  #("middot", 183, 183),
                  // Vertically-centered period (dot accent)
                  #("cedil", 184, 184), // cedilla
                  #("sup1", 185, 185),  // superscript 1
                  #("ordm", 186, 186),  // ordinal indicator, masculine
                  #("raquo", 187, 187), // guillemotright
                  #("frac14", 188, 188),// fractional 1/4
                  #("frac12", 189, 189),// fractional 1/2
                  #("frac34", 190, 190),// fractional 3/4
                  #("iquest", 191, 191),// inverted question mark
                  #("Agrave", 192),     // capital A, grave accent
                  #("Aacute", 193),     // capital A, acute accent
                  #("Acirc", 194),      // capital A, circumflex accent
                  #("Atilde", 195),     // capital A, tilde
                  #("Auml", 196),       // capital A, dieresis or umlaut mark
                  #("Aring", 197),      // capital A, ring
                  #("AElig", 198),      // capital AE diphthong (ligature)
                  #("Ccedil", 199),     // capital C, cedilla
                  #("Egrave", 200),     // capital E, grave accent
                  #("Eacute", 201),     // capital E, acute accent
                  #("Ecirc", 202),      // capital E, circumflex accent
                  #("Euml", 203),       // capital E, dieresis or umlaut mark
                  #("Igrave", 204),     // capital I, grave accent
                  #("Iacute", 205),     // capital I, acute accent
                  #("Icirc", 206),      // capital I, circumflex accent
                  #("Iuml", 207),       // capital I, dieresis or umlaut mark
                  #("ETH", 208),        // capital ETH, Icelandic
                  #("Ntilde", 209),     // capital N, tilde
                  #("Ograve", 210),     // capital O, grave accent
                  #("Oacute", 211),     // capital O, acute accent
                  #("Ocirc", 212),      // capital O, circumflex accent
                  #("Otilde", 213),     // capital O, tilde
                  #("Ouml", 214),       // capital O, dieresis or umlaut mark
                  #("times", 215),      // multiply sign
                  #("Oslash", 216),     // capital O, slash
                  #("Ugrave", 217),     // capital U, grave accent
                  #("Uacute", 218),     // capital U, acute accent
                  #("Ucirc", 219),      // capital U, circumflex accent
                  #("Uuml", 220),       // capital U, dieresis or umlaut mark
                  #("Yacute", 221),     // capital Y, acute accent
                  #("THORN", 222),      // capital THORN, Icelandic
                  #("szlig", 223),      // small sharp s, German (sz ligature)
                  #("agrave", 224),     // small a, grave accent
                  #("aacute", 225),     // small a, acute accent
                  #("acirc", 226),      // small a, circumflex accent
                  #("atilde", 227),     // small a, tilde
                  #("auml", 228),       // small a, dieresis or umlaut mark
                  #("aring", 229),      // small a, ring
                  #("aelig", 230),      // small ae diphthong (ligature)
                  #("ccedil", 231),     // small c, cedilla
                  #("egrave", 232),     // small e, grave accent
                  #("eacute", 233),     // small e, acute accent
                  #("ecirc", 234),      // small e, circumflex accent
                  #("euml", 235),       // small e, dieresis or umlaut mark
                  #("igrave", 236),     // small i, grave accent
                  #("iacute", 237),     // small i, acute accent
                  #("icirc", 238),      // small i, circumflex accent
                  #("iuml", 239),       // small i, dieresis or umlaut mark
                  #("eth", 240),        // small Icelandic eth
                  #("ntilde", 241),     // small n, tilde
                  #("ograve", 242),     // small o, grave accent
                  #("oacute", 243),     // small o, acute accent
                  #("ocirc", 244),      // small o, circumflex accent
                  #("otilde", 245),     // small o, tilde
                  #("ouml", 246),       // small o, dieresis or umlaut mark
                  #("divide", 247),     // divide sign
                  #("oslash", 248),     // small o, slash
                  #("ugrave", 249),     // small u, grave accent
                  #("uacute", 250),     // small u, acute accent
                  #("ucirc", 251),      // small u, circumflex accent
                  #("uuml", 252),       // small u, dieresis or umlaut mark
                  #("yacute", 253),     // small y, acute accent
                  #("thorn", 254),      // small thorn, Icelandic
                  #("yuml", 255)])
    let name  = head(entry);
    let codes = tail(entry);
    //--- KMP's code asserted this: '(apply #'= (mapcar #'code codes))'
    *entity-names->character*[name] := as(<character>, head(codes));
    *character->entity-names*[as(<integer>, head(codes))] := name;
  end
end function initialize-html-entity-names;
 
initialize-html-entity-names();
