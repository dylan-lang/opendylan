Module: file-source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <flat-file-source-record> (<file-source-record>)
  slot cached-source-record-module-name :: false-or(<symbol>) = #f;
  slot cached-source-record-start-line :: false-or(<integer>) = #f;
  slot cached-source-record-contents :: false-or(<byte-vector>) = #f;
  slot source-record-removed :: <boolean> = #f;
  slot source-record-modified :: <boolean> = #f;
  constant slot source-record-project :: <object> = #f,
    init-keyword: project:;
  constant slot source-record-unique-id,
    required-init-keyword: unique-id:;
  constant slot source-record-location-slot :: <locator>,
    required-init-keyword: location:;
end;

*default-file-source-record-class* := <flat-file-source-record>;

define class <flat-file-source-record-proxy> (<object>)
  constant slot source-record-proxy-id :: <string>,
    required-init-keyword: id:;
  constant slot source-record-proxy-module-name :: false-or(<symbol>),
    required-init-keyword: module:;
  constant slot source-record-proxy-start-line :: false-or(<integer>),
    required-init-keyword: start-line:;
end;  

// There should be an adjective in define class that does this...
define sealed domain make(singleton(<flat-file-source-record-proxy>));
define sealed domain initialize(<flat-file-source-record-proxy>);

define method as (c == <string>, proxy :: <flat-file-source-record-proxy>)
 => (str :: <string>)
  proxy.source-record-proxy-id;
end method;

define function unique-file-id-iso(location :: <locator>)
 => (id :: <string>)
  let date :: <date> = file-property(location, #"write-date");
  as-iso8601-string(date)
end;

define constant unique-file-id = unique-file-id-iso;

// cannot be undone
define method source-record-removed?(sr :: <flat-file-source-record>)
 => (yes :: <boolean>);
  sr.source-record-removed
    |
    begin
      let removed? = ~file-exists?(sr.source-record-location-slot);
       if(removed?)
	 sr.source-record-removed := #t
       end;
      removed?
    end;
end;

// cannot be undone
define method source-record-modified?(sr :: <flat-file-source-record>)
 => (yes :: <boolean>);
  sr.source-record-modified
    |
    block()
      let modified? = 
	(sr.source-record-unique-id ~= 
	   unique-file-id(sr.source-record-location-slot));
      if(modified?)
	sr.source-record-modified := #t
      end;
      modified?
    exception (error :: <file-system-error>)
      if(source-record-removed?(sr))
	sr.source-record-modified := #t
      else
	signal(error)
      end;
    end;
end;

define method source-record-location(sr :: <flat-file-source-record>,
				     #key check-if-exists? = #f)
 => (loc :: <locator>);
  
  let location = sr.source-record-location-slot;
  if(~check-if-exists? | ~source-record-removed?(sr))
    location
  else
    signal(make(<source-record-missing>,
		source-record: sr,
		format-string: "File %s has been removed.",
		format-arguments: list(location)))
  end
end;

define method source-record-relative-name (sr :: <flat-file-source-record>,
					   directory :: <locator>)
 => (name :: <string>)
  let location = relative-locator(sr.source-record-location-slot, directory);
  as(<string>, make(<file-locator>,
		    directory: locator-directory(location),
		    base: locator-base(location)))
end method;

define function flat-file-id (location :: <locator>,
			      directory :: <locator>,
			      unique-id :: <string>) => id :: <string>;
  let location = relative-locator(location, directory);
  // pair(location, date)
  // For now, the compiler wants the id to be a one-line string...
  let namestring = as(<string>, location);
//  assert(~member?('\t', namestring));
  concatenate(namestring, "\t", unique-id)
end;

define method id-as-source-record (c == <flat-file-source-record>,
				   project :: <object>,
				   directory :: <locator>,
				   id :: <string>)
 => sr :: <flat-file-source-record>;
  let pos = position(id, '\t');
  debug-assert(pos, "Invalid sr id %=", id);
  let namestring = copy-sequence(id, end: pos);
  let location = merge-locators(as(<file-locator>, namestring), directory);
  let unique-id = copy-sequence(id, start: pos + 1);
  make(c, project: project, location: location, unique-id: unique-id);
end method;

define method id-as-source-record (c == <flat-file-source-record>,
				   project :: <object>,
				   directory :: <locator>,
				   id :: <flat-file-source-record-proxy>)
 => sr :: <flat-file-source-record>;
  let sr
    = id-as-source-record(c, project, directory, id.source-record-proxy-id);
  sr.cached-source-record-module-name :=
    id.source-record-proxy-module-name;
  sr.cached-source-record-start-line :=
    id.source-record-proxy-start-line;
  sr
end method;

define method source-record-as-id (sr :: <flat-file-source-record>,
				   directory :: <locator>)
 => (id :: <flat-file-source-record-proxy>)
  make(<flat-file-source-record-proxy>,
       // TODO: Don't need to cons this string, just store the date
       // and abbrev. locator in the proxy.
       id: flat-file-id(sr.source-record-location, directory,
			sr.source-record-unique-id),
       module: sr.cached-source-record-module-name,
       start-line: sr.cached-source-record-start-line)
end method;

define method file-source-record-ids (c  == <flat-file-source-record>,
				      directory :: <locator>,
				      location :: <locator>)
 => sr-id* :: <list>;
  block()
    list(flat-file-id(location, directory, unique-file-id(location)))
  exception (error :: <file-system-error>)
    signal(make(<source-record-missing>,
		source-record: location,
		format-string: "File %s does not exist or cannot be opened.",
		format-arguments: list(location)));
  end;

end method;

define method call-with-source-record-input-stream
    (fn :: <function>, sr :: <flat-file-source-record>,
     #key check-date? = *check-source-record-date?*) => (#rest fn-values);
  let location = 
    // if we are not checking the date we have to check if file exists
    source-record-location(sr, check-if-exists?: ~check-date?);
  unless (~check-date? | ~source-record-modified?(sr))
    debug-message("Source record modified: original %s on disk %s",
		  sr.source-record-unique-id, 
		  if(source-record-removed?(sr))
		    "missing"
		  else
		    unique-file-id(sr.source-record-location-slot)
		  end);
    signal(make(<source-record-missing>,
		source-record: sr,
		format-string: "%s was unexpectedly modified during compilation.",
		format-arguments: list(sr)));
  end;
  with-open-source-file (stream = location)
    stream-skip-lines (stream, sr.source-record-start-line);
    fn(stream);
  end;
end method;

define method source-record-module-name (sr :: <flat-file-source-record>)
 => module-name :: <symbol>;
  sr.cached-source-record-module-name | begin
					  cache-file-header-data(sr);
					  sr.cached-source-record-module-name
					end
end method;

define method source-record-start-line 
    (sr :: <flat-file-source-record>) => (line :: <integer>)
  sr.cached-source-record-start-line | begin
					 cache-file-header-data(sr);
					 sr.cached-source-record-start-line
				       end
end method;

define function cache-file-header-data (sr :: <flat-file-source-record>)
  let location = source-record-location(sr, check-if-exists?: #t);
  let (headers, lines) = read-file-header(location);
  sr.cached-source-record-start-line := lines;
  let module-strings = element(headers, #"module", default: #f);
  unless(module-strings)
    signal(make(<badly-formed-file-header>,
		format-string: "Source file %s does not contain "
		  "the entry for module, which is mandatory",
		format-arguments: vector(as(<string>, location))))
  end;
  let module-name = first(module-strings);
  if(empty?(module-name))
    signal(make(<badly-formed-file-header>,
		format-string: "Source file %s does not specify "
		  "module name, which is mandatory",
		format-arguments: vector(as(<string>, location))))
  end;
    
  sr.cached-source-record-module-name := as(<symbol>, module-name);
end function;

define method source-record-name (sr :: <flat-file-source-record>)
 => name :: <string>;
  locator-base(sr.source-record-location-slot)
end method;

// TODO: PERFORMANCE: When do we uncache this? Perhaps keep it around
// only while doing the initial read, and then just rescan the file 
// for generating warnings, moving "extract-string" here from the
// reader as an abstraction.

define method source-record-contents 
   (sr :: <flat-file-source-record>) => (bytes :: <byte-vector>)
  cached-source-record-contents(sr)
    | (cached-source-record-contents(sr)
         := with-input-from-source-record (stream = sr)
              read-to-end(stream);
            end)
end method;

define method print-object (sr :: <flat-file-source-record>, stream :: <stream>) => ()
  format(stream, "{flat file %=}", sr.source-record-name);
end method;

define method source-char-offset (sr :: <file-source-record>) => (offset);
  // Don't bother caching for now.
  let (headers, lines, chars) = 
    read-file-header(source-record-location(sr, check-if-exists?: #t));
  chars;
end method;
