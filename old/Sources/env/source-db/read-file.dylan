Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *debug-file-reader* = #f;

define class <db-file-error> (<simple-error>) end;

define method db-file-error (string :: <string>, #rest args)
  error (make (<db-file-error>, format-string: string, format-arguments: args));
end;

define class <section-reader> (<object>)
  slot reader-stream, required-init-keyword: stream:;
  slot file-name, required-init-keyword: file-name:;
  slot section-length, init-value: 0;
  slot section-start, init-value: 0;
  slot section-code, init-value: #f;
end;

define generic read-section(r :: <section-reader>);

define generic find-next-section(r :: <section-reader>);

define generic section-id(r ::  <section-reader>);

define method find-next-section(r :: <section-reader>)
  block(return)
    let start-section = r.reader-stream.file-position;
    let end-section = 0;
    if(*debug-file-reader*)
      format(*standard-output*, "Reading section:\n");
    end;
    let code = read-section(r);
    end-section := r.reader-stream.file-position;
    if(*debug-file-reader*)
      format(*standard-output*, "%=\n", code);
      format(*standard-output*, "b= %= e= %=\n", start-section, end-section);
    end;
    if(id?(code,emulator/$eof))
      if(*debug-file-reader*)
	format(*standard-output*, "$Eof.\n");
      end;
      return (#f);
    end;
    r.section-code := code;
    r.section-start := start-section;
    r.section-length := end-section - start-section;
  exception(emulator/<end-of-file>)
    if(*debug-file-reader*)
      format(*standard-output*, "<Eof>.\n");
    end;
    return (#f);
  end;
end;

define method get-signature(l :: <object>)
  let args-id = "";
  if(instance?(l, <list>))
    for(arg in l, until ~instance?(arg,<list>))
      let type = arg[1];
      let type-name = if(instance?(type, <list>)) 
			concatenate(as(<string>, type[0]), 
				    format-to-string("%=", type[1]))
		      else
			as(<string>, type)
		      end;
      args-id := concatenate(args-id, type-name);
    end
  end;
  args-id;
end;


