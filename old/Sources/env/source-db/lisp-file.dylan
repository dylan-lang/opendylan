Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <lisp-reader> (<section-reader>)
end;

define method read-section (r :: <lisp-reader>)
  block(return)
    let b = r.reader-stream.file-position;
    let last-code = emulator/$eof;
    while(#t)
      let code = emulator/read(stream: r.reader-stream);
      if(id?(code,emulator/$eof))
	return(last-code);
      end;
      last-code := code;
    end;
  exception(err :: emulator/<end-of-file>)
    emulator/$eof;
  end;
end;

define method section-id (r :: <lisp-reader>)
  "lisp-section";
end;
