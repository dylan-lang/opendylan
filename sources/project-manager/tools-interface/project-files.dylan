Module: 	tools-interface
Author:		1997-07-17: Seth LaForge
Synopsis:	Tools for reading and writing project files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $current-project-file-version :: <integer> = 2;


// Information about a project.  The intention is that all useful information
// about the project is contained in slots other than
// project-information-remaining-keys, in order to support future non-keyword
// formats (i.e. binary project files).

define class <project-information> (<object>)
  slot project-information-files :: <sequence> /* of: <locator> */ = #(), 
	  init-keyword: files:;
  slot project-information-subprojects :: <sequence> /* of: <locator> */ = #(), 
	  init-keyword: subprojects:;
  slot project-information-library :: <string>, init-keyword: library:;
  slot project-information-target-type :: one-of(#"executable", #"dll") 
	  = #"dll", init-keyword: target-type:;
  slot project-information-executable :: false-or(<string>) = #f,
	  init-keyword: executable:;
  slot project-information-compilation-mode :: one-of(#"tight", #"loose") 
	  = #"loose", init-keyword: compilation-mode:;
  slot project-information-major-version :: <integer> = 0,
	  init-keyword: major-version:;
  slot project-information-minor-version :: <integer> = 0,
	  init-keyword: minor-version:;
  slot project-information-library-pack :: <integer> = 0,
	  init-keyword: library-pack:;
  slot project-information-base-address :: false-or(<machine-word>) = #f,
	  init-keyword: base-address:;
  slot project-information-remaining-keys :: false-or(<table>) = #f,
	  init-keyword: remaining-keys:;
end class <project-information>;


// A function to generate a handler which can be used to add file information
// to a <tool-warning-condition>:

define function tool-warning-add-file-handler 
	(file :: type-union(<string>, <locator>)) => (r :: <function>)
  method (c :: <tool-warning-condition>, next :: <function>)
    c.tool-warning-file := file;
    next()
  end method;
end function tool-warning-add-file-handler;


// Write a project to a file.

define function write-project-file 
	(file :: type-union(<string>, <locator>), 
	 project :: <project-information>, 
	 #key version :: <integer> = $current-project-file-version) => ()
  with-open-file (stream = file, direction: #"output")
    write-project-to-stream(stream, project, version: version);
  end;
end function write-project-file;


// Write a project to a stream.  version specifies the format version to write -
// currently it has little effect (since the 1.0 format is basically the same).

define function write-project-to-stream 
	(s :: <stream>, p :: <project-information>, 
	 #key version :: <integer> = $current-project-file-version) => ()
  let t :: <table> = 
	  if (p.project-information-remaining-keys)
	    shallow-copy(p.project-information-remaining-keys)
	  else
	    make(<table>)
	  end if;
  remove-key!(t, comment:);
  remove-key!(t, format-version:);
  t[files:] := map(
  	method (locator :: <file-locator>)
	  as(<string>, make(<file-locator>,
			    directory: locator-directory(locator),
			    base: locator-base(locator)))
	end method,
	p.project-information-files);
  t[subprojects:] := 
	  map(curry(as, <string>), p.project-information-subprojects);
  t[library:] := p.project-information-library;
  t[target-type:] := as(<string>, p.project-information-target-type);
  if (p.project-information-executable)
    t[executable:] := p.project-information-executable;
  else remove-key!(t, executable:); end if;
  t[compilation-mode:] := as(<string>, p.project-information-compilation-mode);
  t[major-version:] := integer-to-string(p.project-information-major-version);
  t[minor-version:] := integer-to-string(p.project-information-minor-version);
  t[library-pack:] := integer-to-string(p.project-information-library-pack);
  if (p.project-information-base-address)
    t[base-address:] := machine-word-to-string(
			    p.project-information-base-address, prefix: "0x");
  else remove-key!(t, base-address:); end if;

  if (version ~= 1 & version ~= 2)
    error("attempt to write unknown project version: %d", version);
  end if;
  format(s, "comment:\tThis file is generated, please don't edit\n");
  format(s, "format-version:\t%d\n", version);
  write-keyword-pair-stream(s, t);
end function write-project-to-stream;


// Read a project file.

define function read-project-file 
	(file :: type-union(<string>, <locator>)) 
     => (project :: <project-information>, version :: <integer>)
  let handler <tool-warning-condition> = tool-warning-add-file-handler(file);
  with-open-file (stream = file, direction: #"input")
    read-project-from-stream(stream)
  end
end function read-project-file;


// Read a project file from a stream.

define function read-project-from-stream 
	(s :: <stream>) => (p :: <project-information>, version :: <integer>)
  let t :: <table> = read-keyword-pair-stream(s, 1);
  let error? :: <boolean> = #f;

  let keyval = keyword-file-element-value;
  let keyline = keyword-file-element-line;
  local method err (form :: <string>, args :: <sequence>, #rest keys) => ()
    apply(tool-warning, form, format-arguments: args, serious?: #t, keys);
    error? := #t;
  end method err;
  local method key (k :: <symbol>) => (r :: <sequence>)
    let r = element(t, k, default: #()); remove-key!(t, k); r
  end method key;
  local method required-key (s :: <sequence>, k :: <symbol>) 
  			 => (r :: <sequence>)
    if (s.size = 0) 
      err("required keyword \"%s\" not found", list(as(<string>, k)));
    end if; s
  end method required-key;
  local method single-key (s :: <sequence>, k :: <symbol>) 
  			=> (r :: false-or(<string>), l :: <integer>)
    if (s.size > 1)
      err("keyword \"%s\" does not accept multiple values",
	  list(as(<string>, k)), line: s.second.keyword-file-element-line);
    end if;
    if (s.size > 0)
      values(s.first.keyval, s.first.keyline)
    else
      values(#f, 0)
    end if;
  end method single-key;

  let version-string = single-key((format-version:).key, format-version:);
  let version :: <integer> = 
    if (~version-string)
      1 // 1.0 file format.
    elseif (string-to-integer(version-string, default: #f) ~= 
	    $current-project-file-version)
      tool-error("unknown project file format", serious?: #t);
    else
      2
    end if;

  let files = map(
  	method (s) 
	  let locator = as(<file-locator>, s.keyval);
	  if (~locator.locator-extension
		| empty?(locator.locator-extension))
	    make(<file-locator>,
		 directory: locator-directory(locator),
		 base: locator-base(locator),
		 extension: "dylan")
	  else
	    locator
	  end
	end method, key(files:));
  let subprojects = 
	  map(compose(curry(as, <file-locator>), keyval), (subprojects:).key);
  let executable = single-key((executable:).key, executable:);
  let p :: <project-information> = 
	  make(<project-information>, files: files, subprojects: subprojects,
	       executable: executable, remaining-keys: t);

  let library = single-key(required-key((library:).key, library:), library:);
  if (library) p.project-information-library := library; end if;

  let (val, line) = single-key((target-type:).key, target-type:);
  if (val)
    let sym = as(<symbol>, val);
    select (sym)
      #"executable", #"dll" => p.project-information-target-type := sym;
      otherwise => err("invalid target-type \"%s\"", 
		       list(as(<string>, sym)), line: line);
    end select;
  end if;

  let (val, line) = single-key((compilation-mode:).key, compilation-mode:);
  if (val)
    let sym = as(<symbol>, val);
    select (sym)
      #"tight", #"loose" => p.project-information-compilation-mode := sym;
      otherwise => err("invalid compilation-mode \"%s\"", 
		       list(as(<string>, sym)), line: line);
    end select;
  end if;

  let (major-version, line) = single-key((major-version:).key, major-version:);
  if (major-version)
    let i = string-to-integer(major-version, default: #f);
    if (i) p.project-information-major-version := i;
    else err("major-version \"%s\" is not a number", list(major-version), 
	     line: line);
    end if;
  end if;
  let (minor-version, line) = single-key((minor-version:).key, minor-version:);
  if (minor-version)
    let i = string-to-integer(minor-version, default: #f);
    if (i) p.project-information-minor-version := i;
    else err("minor-version \"%s\" is not a number", list(minor-version), 
	     line: line);
    end if;
  end if;

  let (library-pack, line) = single-key((library-pack:).key, library-pack:);
  if (library-pack)
    let i = string-to-integer(library-pack, default: #f);
    if (i) p.project-information-library-pack := i;
    else err("library-pack \"%s\" is not a number", list(library-pack), 
	     line: line);
    end if;
  end if;

  let (base-address, line) = single-key((base-address:).key, base-address:);
  if (base-address)
    let i = string-to-machine-word(base-address, default: #f);
    if (i) p.project-information-base-address := i;
    else err("base-address \"%s\" is not a hex number", list(base-address), 
	     line: line);
    end if;
  end if;

  if (error?)
    tool-error("an error occurred reading project file, halting", serious?: #t);
  end if;

  values(p, version)
end function read-project-from-stream;


define class <keyword-file-element> (<object>)
  slot keyword-file-element-value :: <string>, init-keyword: value:;
  slot keyword-file-element-line :: false-or(<integer>) = #f,
	  init-keyword: line:;
end class <keyword-file-element>;


define function write-keyword-pair-file 
	(file :: type-union(<string>, <locator>), keys :: <table>) => ()
  with-open-file (stream = file, direction: #"output")
    write-keyword-pair-stream(stream, keys);
  end;
end function write-keyword-pair-file;


define function write-keyword-pair-stream (s :: <stream>, keys :: <table>) => ()
  for (vals keyed-by key in keys)
    let prefix :: <string> = concatenate(as(<string>, key), ":\t");
    local method write-key (val) => ()
      write(s, prefix);
      write(s, if (instance?(val, <string>)) val 
	       else val.keyword-file-element-value end if);
      new-line(s);
      prefix := "\t";
    end method write-key;
    if (instance?(vals, <sequence>) & ~ instance?(vals, <string>))
      for (val in vals)
	write-key(val);
      end for;
    else
      write-key(vals);
    end if;
  end for;
end function write-keyword-pair-stream;


define function read-keyword-pair-file 
	(file :: type-union(<string>, <locator>)) => (keys :: <table>)
  let handler <tool-warning-condition> = tool-warning-add-file-handler(file);
  with-open-file (stream = file, direction: #"input")
    read-keyword-pair-stream(stream, 1)
  end
end function read-keyword-pair-file;


define function read-keyword-pair-stream (s :: <stream>, 
					  first-line-no :: <integer>) 
				      => (keys :: <table>,
					  last-line-no :: <integer>)
  let keys = make(<table>);
  local method loop (line-no :: <integer>) => (keys, last-line-no :: <integer>)
    let (key, strings, eoh?, line-no) = read-file-header-component(s, line-no);
    if (key)
      let old-strings = element(keys, key, default: #());
      keys[key] := concatenate!(old-strings, strings);
    end if;
    if (eoh?)
      values(keys, line-no)
    else
      loop(line-no)
    end if
  end method loop;
  loop(first-line-no)
end function read-keyword-pair-stream;


define function read-file-header-component (s :: <stream>, line-no :: <integer>)
					=> (key, strings, eoh?, new-line-no)
  let (key-line, nl?) = read-line(s, on-end-of-stream: "");
  if (header-end-marker-line?(key-line))
    values(#f, #f, #t, line-no)
  else
    local method loop (text-strings :: <list>)
      line-no := line-no + 1;
      let char = read-element(s, on-end-of-stream: #f);
      if (char == ' ' | char == '\t')
	let (continuation-line, nl?) = read-line(s, on-end-of-stream: "");
	if (header-end-marker-line?(continuation-line))
	  values(text-strings, #t, line-no)
	else
	  let text = parse-header-continuation-line(continuation-line, line-no);
	  loop(pair(text, text-strings))
	end;
      else
	if (char) unread-element(s, char) end;
	values(text-strings, #f, line-no)
      end;
    end method;
    let (key, text) = parse-header-keyword-line(key-line, line-no);
    let (text-strings, past-eoh?) = loop(list(text));
    values(key, reverse!(text-strings), past-eoh?, line-no)
  end;
end function read-file-header-component;


define function parse-header-keyword-line (line :: <string>, 
					   line-no :: <integer>)
  let colon = position(line, ':');
  if (~colon)
    tool-error("syntax error", line: line-no);
  end;
  values(as(<symbol>, copy-sequence(line, end: colon)),
	 make(<keyword-file-element>, 
	      value: trim-whitespace(line, colon + 1), line: line-no))
end function parse-header-keyword-line;

define function parse-header-continuation-line (line :: <string>, 
						line-no :: <integer>)
  make(<keyword-file-element>, 
       value: trim-whitespace(line, 0), line: line-no)
end function parse-header-continuation-line;

define function header-end-marker-line? (line :: <string>)
  every?(method (c) c == ' ' | c == '\t' end, line)
end function;

define function trim-whitespace (line :: <string>, start)
  local method bwd (line, start, len)
	  let last = len - 1;
	  let c = line[last];
	  if (c == ' ' | c == '\t') bwd(line, start, last)
	  else copy-sequence(line, start: start, end: len) end;
	end method;
  local method fwd (line, start, len)
	  if (start == len) ""
	  else
	    let c = line[start];
	    if (c == ' ' | c == '\t') fwd(line, start + 1, len)
	    else bwd(line, start, len) end;
	  end;
	end method;
  fwd(line, start, size(line))
end function;
