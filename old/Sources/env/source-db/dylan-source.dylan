Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method update-dylan-library (sys :: <db-source-system>)
  let lid-name = concatenate(sys.name, ".lid");
  let stream = #f;
  block(return)
    stream := emulator/open(lid-name, direction: input:);
    let headers = maybe-read-file-header(stream);
    let filenames = element(headers, files:);
    let libname = element(headers, library:);
    format(*standard-output*, "%= : %s\n", libname, filenames);
    let seq = make(<deque>);
    for( f in filenames )
      update-or-boot-file(sys, f);
      seq := push-last(seq, find-file(sys, f));
    end;
    sys.meta-sequence := seq;
    sys;
  cleanup
    if(stream) emulator/close(stream) end;
  end block;
end;

define method update-dylan-library-file (sys :: <db-source-system>,
					 file :: <locator>)
  update-or-boot-file(sys, file);
end;

define method update-source-file-sequence(sys :: <db-source-system>,
					  filenames :: <list>)
  
  let seq = make(<deque>);
  for( f in filenames )
    seq := push-last(seq, find-file(sys, f));
  end;
  sys.meta-sequence := seq;
  sys;
end;

define method boot-library-source (name :: <string>)
  let sys = make(<db-source-system>, name: name);
  update-dylan-library(sys);
end;

 
define method file-code(sys :: <db-source-system>,
			file :: <string>, #key dbg-print = #f)
  let (meta, branch) = find-file(sys, file);
  let sections = all-sections(meta, branch);
  if(dbg-print)
    for(s in sections)
      format(*standard-output*, "section: %=\n%=\n", s.id, s.section-code);
    end;
  end;
  let source-module = sections[0].section-code;
  let code = #();
  for(s in sections.tail)
    code := add(code, s.section-code);
  end;
  code := reverse(code);
  if(dbg-print)
    format(*standard-output*, "Module: %=\n%=\n", source-module, code);
  end;
  values(source-module, code);
end;
