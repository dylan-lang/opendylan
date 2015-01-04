Module:       system-internals
Synopsis:     Implementation of concrete file streams
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways sealed method open-file-stream
    (locator :: <file-locator>, #rest keywords, #key, #all-keys)
 => (stream :: <stream>)
  apply(make, <file-stream>, locator: locator, keywords)
end method open-file-stream;

define sideways sealed method open-file-stream
    (string :: <string>, #rest keywords, #key, #all-keys)
 => (stream :: <file-stream>)
  apply(open-file-stream, as(<file-locator>, string), keywords)
end method open-file-stream;

define macro with-open-file
  { with-open-file (?stream:variable = ?locator:expression,
                    #rest ?keys:expression)
      ?body:body
    end }
  => { begin
         let _stream = #f;
         block ()
           _stream := open-file-stream(?locator, ?keys);
           let ?stream :: <file-stream> = _stream;
           ?body
         cleanup
           if (_stream & stream-open?(_stream)) close(_stream) end;
         end
       end }
end macro with-open-file;
