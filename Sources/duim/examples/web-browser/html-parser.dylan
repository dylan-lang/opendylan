Module:       web-browser
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HTML parsing

define class <html-page> (<web-page>)
  slot page-tokens = #f, init-keyword: tokens:;
end class;

define class <html-token> (<object>)
end class;

define class <html-tag> (<html-token>)
  slot token-name = #f, init-keyword: name:;
  slot token-options = #f, init-keyword: options:;
end class;

define class <html-text> (<html-token>)
  slot html-text = #f, init-keyword: text:;
end class;

define method blank-string? (string :: <string>, #key start, end: _end)
  block (return)
    for (position from start | 0 below _end | size(string))
      unless (string[position] = ' ')
        return(#f)
      end
      finally #t
    end
  end
end method;

define method make-text-token (string :: <string>, #key start, end: _end)
  unless (blank-string?(string, start: start, end: _end))
    make(<html-text>, 
         text: copy-sequence(string,
                             start: start | 0,
                             end: _end | size(string)))
  end
end method;

define method make-tag (string :: <string>, #key start, end: _end)
  let string 
    = copy-sequence(string, start: start | 0, end: _end | size(string));
  let options-pos = find-key(string, curry(\=, ' '));
  if (options-pos)
    make(<html-tag>, 
         name: as-lowercase!(copy-sequence(string, end: options-pos)),
         options: copy-sequence(string, start: options-pos + 1));
  else
    make(<html-tag>, name: as-lowercase(string))
  end
end method;

define method tokenize-line (line :: <string>, #key tokens)
  let tokens = tokens | make(<stretchy-vector>);
  let in-string? = #f;
  let start = 0;
  let pos = 0;
  let class = <html-text>;
  for (char in line)
    select (char)
      '"' =>
        in-string? := ~in-string?;
        pos := pos + 1;
      '<' =>
        if (class = <html-text>)
          if (pos > start)
            let token = make-text-token(line, start: start, end: pos);
            if (token)
              tokens := add!(tokens, token)
            end
          end;
          start := pos;
          class := <html-tag>;
        end;
        pos := pos + 1;
      '>' =>
        if (class = <html-tag>)
          tokens := add!(tokens,
                         make-tag(line, start: start + 1, end: pos));
          start := pos + 1;
          class := <html-text>;
        end;
        pos := pos + 1;
      otherwise =>
        pos := pos + 1;
    end
  end;
  values(tokens, if (pos > 0) copy-sequence(line, start: start) end)
end method;

define method find-web-page-title (tokens :: <sequence>)
  let title-position = find-key(tokens,
                                method (token)
                                  instance?(token, <html-tag>)
                                    & token-name(token) = "title"
                                end);
  if (title-position & title-position < size(tokens) - 1)
    let title-token = tokens[title-position + 1];
    if (instance?(title-token, <html-text>))
      html-text(title-token)
    end
  end
end method;

//---*** This is terrible... rewrite it to work with multiple options
//---*** at some point.

define method html-tag-link (tag :: <html-tag>)
  if (token-name(tag) = "a")
    html-link-from-options(token-options(tag))
  end
end method;

define method html-link-from-options (options :: <string>)
  copy-sequence(options, start: 6, end: size(options) - 1)
end method;

define method find-html-links (tokens :: <html-page>)
  let tokens = page-tokens(tokens);
  let link-tokens = choose(method (token)
			     instance?(token, <html-tag>)
			       & token-name(token) = "a"
			   end,
			   tokens);
  map-as(<vector>, html-tag-link, link-tokens)
end method;


/// stream reading

define method tokenize-web-page (stream :: <stream>) => (tokens)
  let tokens = make(<stretchy-vector>);
  let remainder = #f;
  block (return)
    until (stream-at-end?(stream))
      let line = read-line(stream, on-end-of-stream: #f);
      if (line)
        if (remainder)
          line := concatenate(remainder, " ", line)
        end;
        let (new-tokens, new-remainder) = tokenize-line(line, tokens: tokens);
        tokens := new-tokens;
        remainder := new-remainder
      else
        return()
      end
    end
  end;
  tokens
end method;

define method tokenize-web-page (file :: <string>) => (tokens)
  if (subsequence-position(file, "http://") = 0)
    let stream = make(<http-stream>, location: file);
    block ()
      tokenize-web-page(stream)
    cleanup
      close(stream)
    end
  else
    with-open-file (stream = file, direction: #"input")
      tokenize-web-page(stream);
    end
  end
end method;

define method read-web-page (page :: <html-page>) => (page :: <html-page)
  let tokens = tokenize-web-page(page-location(page));
  page-tokens(page) := tokens;
  page-title(page)  := find-web-page-title(tokens);
  page
end method;

define method read-web-page (file :: <string>) => (page :: <web-page)
  block ()
    let page = make(<html-page>, location: file);
    read-web-page(page);
    page
  end
end method;

define variable *default-html-text-width* = 60;

define method print-html-as-type
    (page :: <html-page>, stream :: <stream>, type == #"text",
     #key text-width = *default-html-text-width*)
  let separator = make(<string>, size: text-width - 4, fill: '-');
  let in-header? = #f;
  for (token in page-tokens(page))
    select (token by instance?)
      <html-tag> =>
        select (token-name(token) by \=)
          "p", "/h1", "/h2", "ul", "/ul", "br", "tr" =>
            format(stream, "\n");
          "li" =>
            format(stream, "\n - ");
          "hr" =>
            format(stream, "\n\n  %s\n\n", separator);
          "img" =>
            format(stream, "[Image] ");
          "head" => 
            in-header? := #t;
          "/head", "body" => 
            in-header? := #f;
          "/td" =>
            format(stream, " ");
          "/p", "a", "/a", "/body", "html", "/html", 
          "head", "/head", "/title", "!--", "!doctype", 
          "font", "/font", "table", "/table", "td", "/tr" =>
            ;
          "h1" =>
            format(stream, "\n     ");
          "h2" =>
            format(stream, "\n  ");
          otherwise =>
            unless (in-header?)
              format(stream, "<%s> ", token-name(token))
            end
        end;
      <html-text> =>
        unless (in-header?)
          format(stream, "%s ", html-text(token))
        end
    end
  end
end method;

define method print-html-as-type
    (page :: <html-page>, stream :: <stream>, type == #"html",
     #key text-width = *default-html-text-width*)
  for (token in page-tokens(page))
    select (token by instance?)
      <html-tag> =>
        let name = token-name(token);
        select (name by \=)
          "head", "body", "h1", "hr", "ul", "/ul", "/body", "/html" =>
            format(stream, "\n");
          "li" =>
            format(stream, "\n  ");
          otherwise => ;
        end;
        format(stream, "<%s", name);
        let options = token-options(token);
        if (options)
          format(stream, " %s", options)
        end;
        format(stream, "> ", name);
        select (name by \=)
          "html", "head", "/head", "body", "/body", "/h1", "/title",
          "br", "/ul" =>
            format(stream, "\n");
          "hr" =>
            format(stream, "\n\n");
          otherwise => ;
        end;
      <html-text> =>
        format(stream, "%s ", html-text(token));
    end
  end
end method;

define method print-html
    (page :: <html-page>, stream :: <stream>, 
     #key text-width = *default-html-text-width*, type = #"text")
  print-html-as-type(page, stream, type)
end method;

define method print-html-as-string 
    (page :: <html-page>,
     #key text-width = *default-html-text-width*,
          type = #"text")
  let stream = make(<string-stream>, direction: #"output");
  print-html(page, stream, text-width: text-width, type: type);
  as(<string>, stream-contents(stream))
end method;
