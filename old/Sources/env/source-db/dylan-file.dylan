Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *dylan-suffixes* = #(".dylan", ".dyl", "");

define method dylan-filename (filename :: <string>)
	block (return)
		for ( suffix in *dylan-suffixes*)
			let full-filename = concatenate(filename, suffix);
			when (emulator/probe-file (full-filename))
				return (full-filename);
			end;
		end for;
	exception (<error>)
		return (#f);
	end block
end;

define method read-dylan-file-header(s :: emulator/<stream>)
  block(return)
    let table = maybe-read-file-header(s);
    if (~table)
      values(#"dylan-user", #"infix-dylan");
    else
      let module = element(table, module:);
      unless (module)
	return(values(#f, #f));
      end;
      let language = element(table, language:);
      unless (language)
	language := list(#"infix-dylan");
      end;
      values(as(<symbol>, first(module)),
	     as(<symbol>, first(language)));
    end;
//  exception(<error>)
//    db-file-error("Cannot read file headers");
  end;
end;
      
define class <dylan-reader> (<section-reader>)
  slot module :: <symbol>, required-init-keyword: module:;
end;

define class <dylan-infix-reader> (<dylan-reader>)
end;

define method read-section (r :: <dylan-infix-reader>)
  let code = read-infix(stream: r.reader-stream);
  if(code[0] == #"infix-define-macro")
    access(front-end,process-macro-definition);
  end;
  code;
end;

define method convert-to-string(code :: union(<symbol>, <list>))
  local method convert(s :: <string>, obj)
	  if(instance?(obj, <list>))
	    format(*standard-output*, "Reducing: %=\n", obj);
	    concatenate(s, reduce(convert, "", obj));
	  else
	    format(*standard-output*, "Concatenating: %=\n", obj);
	    concatenate(s, as(<string>, obj));
	  end;
	end;

  reduce(convert, "", code);
end;

define method as(class == <string>, val :: <integer>)
  format-to-string("%s", val);
end;

define method as(class == <string>, val :: singleton(#t))
  "true"
end;

define method as(class == <string>, val :: singleton(#f))
  "false"
end;


define method section-id (r :: <dylan-infix-reader>)
  let code = r.section-code;
  let el = if(code.size > 2) 2 else code.size - 1 end;
  let putative-name = code[el];
  let name
    = if (instance?(putative-name, <list>)) 
	convert-to-string(putative-name) 
      else 
	putative-name 
      end;
  let id = concatenate(as(<string>,code[0]), "/", as(<string>, name));
  select (code[0])
    #"infix-define-variable", #"infix-define-class" => id;
    #"infix-define-method", #"infix-define-generic" => 
      concatenate(id, get-signature(code[3]));
    #"import-cl-functions", #"import-cl-values" => id;
    #"infix-define-macro" => id;
    otherwise => id
  end;
end;

define class <dylan-prefix-reader> (<dylan-reader>)
end;

define method read-section (r :: <dylan-prefix-reader>)
  emulator/read(stream: r.reader-stream);
end;

define method section-id (r :: <dylan-prefix-reader>)
  let code = r.section-code;
  let id = as(<string>,code[0]);
  select (code[0])
    #"define-class" => concatenate(id, as(<string>, code[1]));
    #"define", #"set!", #"begin", #"define-variable" => 
      if(instance?(code[1], <list>))
	concatenate(id, as(<string>, code[1][0]));
      else
	concatenate(id, as(<string>, code[1]));
      end;

    #"define-generic",  #"define-method" => 
      concatenate(id, as(<string>, code[1]), get-signature(code[3]));
    #"import-cl-functions", #"import-cl-values", #"import-cl-classes" => 
      if(instance?(code[1][0], <list>))
	concatenate(id, as(<string>, code[1][0][1]));
      else
	concatenate(id, as(<string>, code[1][0]));
      end;
    otherwise => concatenate(id, as(<string>, code[1]));
  end;
end;


define method create-file-reader(filename :: <string>)
  let full-filename = dylan-filename(filename);
  let parser-stream = #f;
  unless(full-filename)
    db-file-error("Cannot find file: %=\n", filename);
  end;
  format(*standard-output*, "Reading file: %=\n", full-filename);
  block (return)
    parser-stream := emulator/open(full-filename, direction: input:);
    let (module, language) = read-dylan-file-header(parser-stream);
    unless(module & language)
      if(subsequence-position(full-filename, ".lisp"))
	language := #"lisp";
      else
	emulator/close(parser-stream);
	return (#f);
      end;
    end;
    format(*standard-output*, "module: %= language: %=\n", module, language);
    let reader = case 
		   language == #"infix-dylan" => make(<dylan-infix-reader>, 
						      stream: parser-stream,
						      file-name: full-filename,
						      module: module);
		   language == #"prefix-dylan" => make(<dylan-prefix-reader>, 
						       stream: parser-stream,
						       file-name: full-filename,
						       module: module);
		   language == #"lisp" => make(<lisp-reader>, 
					       stream: parser-stream,
					       file-name: full-filename);
		   otherwise => db-file-error("Unknown file format");
		 end case;
    reader;
  end;
end;

define method read-file-sections( filename :: <string>, #key keep-code = #t )
  let reader = #f;
  let source-stream = #f;

  block (return)
    reader := create-file-reader(filename);
    unless(reader)
      db-file-error("Cannot create reader for file: %=\n", filename);
    end;
    source-stream := read-stream-over(as(<pathname>, reader.file-name));
    let parser-stream = reader.reader-stream;
    let sections = #();
    let last-section = #f;

    let header-len = parser-stream.file-position;
    last-section := read-n(source-stream, header-len);
    if(instance?(reader, <dylan-reader>))
      sections := pair(list(#"header", 
			    0, header-len,
			    last-section, 
			    reader.module), sections);
    else
      sections := pair(list(#"header", 
			    0, header-len,
			    last-section, 
			    #"not-a-dylan-file"), sections);
    end;

    while(#t)
      let length = find-next-section(reader);
      if(length)
	last-section := read-n(source-stream, length);
	let the-section = if(keep-code) 
			    list(as(<symbol>, section-id(reader)),
				 reader.section-start, reader.section-length,
				 last-section,
				 reader.section-code) 
			  else 
			    list(as(<symbol>, section-id(reader)),
				 reader.section-start, reader.section-length,
				 last-section)
			  end;

	sections := pair(the-section, sections);
      else
	return(reverse!(sections));
      end if;
    end while;
  cleanup
    if(reader) emulator/close(reader.reader-stream) end;
    if(source-stream) stream-close(source-stream) end;
//  exception (<error>)
//    return (#f);
  end block;
end;

