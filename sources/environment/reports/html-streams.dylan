Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  A wrapper stream for outputting HTML
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HTML wrapper stream

define class <html-wrapper-stream> (<wrapper-stream>)
end class <html-wrapper-stream>;

define method read-element
    (stream :: <html-wrapper-stream>, #key on-end-of-stream)
 => (character)
  read-element(stream.inner-stream, on-end-of-stream: on-end-of-stream)
end method read-element;

/*---*** Don't seem to need this as of yet...
define method raw-write
    (stream :: <html-wrapper-stream>, string :: <string>)
 => ()
  write(stream.inner-stream, string)
end method raw-write;
*/

define sealed method write
    (stream :: <html-wrapper-stream>, string :: <string>,
     #key start: start-index = 0, end: end-index)
 => ()
  for (index :: <integer> from start-index | 0 below end-index | string.size)
    write-element(stream, string[index])
  end
end method write;

define sealed method write
    (stream :: <html-wrapper-stream>, string :: <byte-string>,
     #key start: start-index = 0, end: end-index)
 => ()
  for (index :: <integer> from start-index | 0 below end-index | string.size)
    write-element(stream, string[index])
  end
end method write;

define method write-element
    (stream :: <html-wrapper-stream>, character :: <character>)
 => ()
  let inner-stream = stream.inner-stream;
  select (character)
    '<'       => write(inner-stream, "&lt;");
    '>'       => write(inner-stream, "&gt;");
    '&'       => write(inner-stream, "&amp;");
    '\n'      => new-line(inner-stream);
    otherwise => write-element(inner-stream, character);
  end
end method write-element;

define method write-element
    (stream :: <html-wrapper-stream>, string :: <string>)
 => ()
  write(stream, string)
end method write-element;

define method write-element
    (stream :: <html-wrapper-stream>, token :: <symbol>)
 => ()
  let inner-stream = stream.inner-stream;
  with-stream-locked (stream)
    write-element(inner-stream, '<');
    write(inner-stream, as-lowercase(as(<string>, token)));
    write-element(inner-stream, '>')
  end
end method write-element;

define method write-element
    (stream :: <html-wrapper-stream>, token :: <integer>)
 => ()
  write(stream.inner-stream, integer-to-string(token))
end method write-element;

define method write-html
    (stream :: <html-wrapper-stream>, #rest sequence)
 => ()
  do(curry(write-element, stream), sequence)
end method write-html;


/// Some special tokens

define class <html-anchor> (<object>)
  sealed constant slot anchor-name :: <string>,
    required-init-keyword: name:;
end class <html-anchor>;

define method write-element
    (stream :: <html-wrapper-stream>, anchor :: <html-anchor>)
 => ()
  let inner-stream = stream.inner-stream;
  with-stream-locked (stream)
    write(inner-stream, "<a name=");
    write(stream, anchor.anchor-name);
    write(inner-stream, ">")
  end
end method write-element;

define class <html-reference> (<object>)
  sealed constant slot reference-name :: <string>,
    required-init-keyword: name:;
end class <html-reference>;

define method write-element
    (stream :: <html-wrapper-stream>, reference :: <html-reference>)
 => ()
  let inner-stream = stream.inner-stream;
  with-stream-locked (stream)
    write(inner-stream, "<a href=");
    write(stream, reference.reference-name);
    write(inner-stream, ">")
  end
end method write-element;


/// Useful macro

define macro with-html-output
  { with-html-output (?streamn:name = ?streamv:expression, ?title:expression)
      ?body:body
    end }
 => { invoke-with-html-output(?streamv, ?title, method (?streamn) ?body end method)
       }
  { with-html-output (?stream:name, ?title:expression)
      ?body:body
    end }
 => { invoke-with-html-output(?stream, ?title, method (?stream) ?body end method)
       }
end macro with-html-output;

define method invoke-with-html-output 
    (inner :: <html-wrapper-stream>, title :: <string>, body :: <function>)
 => ()
  invoke-with-html-rubric(inner, title, body);
end method;

define method invoke-with-html-output 
    (inner :: <stream>, title :: <string>, body :: <function>)
 => ()
  invoke-with-html-rubric(make(<html-wrapper-stream>, inner-stream: inner),
			  title,
			  body)

end method;

define macro with-html-rubric
  { with-html-rubric (?streamn:name = ?streamv:expression, ?title:expression)
      ?body:body
    end }
 => { invoke-with-html-rubric (?streamv, ?title, method (?streamn) ?body end method)
       }
  { with-html-rubric (?stream:name, ?title:expression)
      ?body:body
    end }
 => { invoke-with-html-rubic (?stream, ?title, method (?stream) ?body end method)
       }
end macro with-html-rubric;

define method invoke-with-html-rubric 
    (stream :: <stream>, title :: <string>, body :: <function>)
 => ()
  write-html-header(stream, title);
  body(stream);
  write-html-footer(stream)
end method;

define function write-html-header
    (stream :: <html-wrapper-stream>, title :: <string>) => ()
  write-html(stream,
	     #"html", '\n', '\n',
	     #"head", '\n',
	     #"title", title, #"/title", '\n',
	     #"/head", '\n', '\n',
	     #"body", '\n', '\n')
end function write-html-header;

define function write-html-footer
    (stream :: <html-wrapper-stream>) => ()
  write-html(stream,
	     '\n',
	     #"/body", '\n',
	     #"/html", '\n')
end function write-html-footer;
