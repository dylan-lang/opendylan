module: locator-internals
author: Tim McNerney
revised: 13-Feb-95 mf
revised: 25-Sep-96 Tim
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $url-separator  = '/';
define constant $host-separator = ':';

define abstract class <url-locator>
  (<physical-locator>)
end class;

define method locator-case-sensitive?
    (locator :: <url-locator>) => (sensitive? :: <boolean>)
  #t
end method locator-case-sensitive?;

define abstract class <locator-remote-host-login-slots> 
    (<locator-host-slot>,
     <locator-port-slot>,
     <locator-user-id-slot>,
     <locator-password-slot>)
end class;

define method \= 
    (loc1 :: <locator-remote-host-login-slots>,
     loc2 :: <locator-remote-host-login-slots>)
 => (equal? :: <boolean>)
  next-method ()
  & locator-host (loc1) = locator-host (loc2)
  & locator-port (loc1) = locator-port (loc2)
  & locator-user-id (loc1) = locator-user-id (loc2)
  & locator-password (loc1) = locator-password (loc2)
end method;

define method looking-at-url-prefix? 
    (locator-string :: <string>)
 => (at-url-prefix? :: <boolean>)
  let scheme = locator-scheme (locator-string);
  ~ (scheme == #f | scheme == #"abstract")
end method;

define method locator-scheme 
    (locator-string :: <string>) => (scheme :: <locator-scheme>)
  case
    looking-at?("abstract://", locator-string)  => #"abstract";
    looking-at?("http://", locator-string)      => #"http";
    looking-at?("ftp://", locator-string)       => #"ftp";
    looking-at?("mailto:", locator-string)      => #"mailto";
    looking-at?("gopher://", locator-string)    => #"gopher";
    looking-at?("wais://", locator-string)      => #"wais";
    looking-at?("prospero://", locator-string)  => #"prospero";
    looking-at?("telnet://", locator-string)    => #"telnet";
    looking-at?("file://", locator-string)      => #"file";
    looking-at?("news://", locator-string)      => #"news";
    looking-at?("nntp://", locator-string)      => #"nntp";
    otherwise                                   => if ($os-name = #"win32") #"windows" end if;
  end case;
end method;

/*
define method scheme->prefix (scheme :: false-or(<symbol>))
  select (scheme)
    #"abstract" => "abstract:";
    #"http"     => "http:";
    #"ftp"      => "ftp:";
    #"mailto"   => "mailto:";
    #"gopher"   => "gopher:";
    #"wais"     => "wais:";
    #"prospero" => "prospero:";
    #"telnet"   => "telnet:";
    #"file"     => "file:";
    #"news"     => "news:";
    #"nntp"     => "nntp:";
    otherwise   => "";
  end select;
end method;
*/

define method as
    (class == <url-locator>, locator-string :: <string>)
 => (locator :: <url-locator>)
  as (as-locator-class (locator-scheme (locator-string),
	                default: <http-locator>), //Later? <partial-url-locator>
      locator-string);
end method;

define method as-locator-class
    (scheme :: false-or(<symbol>), #key default)
 => (class :: subclass(<locator>))
  select (scheme)
    // URLs
    #"http"     => <http-locator>;
    #"ftp"      => <ftp-locator>;
    #"mailto"   => <mailto-locator>;
    //
    #"abstract" => <abstract-locator>;
    #"windows"  => <microsoft-locator>;
    otherwise   =>
      (~scheme & default)
	| locator-error(<locator-parse-error>,
			"AS-LOCATOR-CLASS: Unimplemented scheme");
  end select;
end method;

define method make
    (class == <url-locator>, #rest init-args, #key scheme, prefix)
 => (locator :: <url-locator>)
  // let directory = as(<abstract-directory-path>, directory);  
  // (push into individual make's)
  if (prefix & scheme)
    error ("MAKE <LOCATOR>: Can't supply both scheme and prefix")
  elseif (prefix)
    apply (make, as-locator-class (locator-scheme (prefix)),
                 init-args);
  else
    apply (make, as-locator-class (scheme, default: <http-locator>), 
                 init-args);
  end if;
end method;

// HTTP:

define class <http-locator> (<locator-host-slot>,
			     <locator-port-slot>,
			     <locator-directory-slot>,
			     <locator-derived-base-slot>,
			     <locator-derived-type-slot>,
			     <locator-derived-name-slot>,
			     <locator-search-keys-slot>,
			     <url-locator>)
end class;

define method locator-scheme 
    (locator :: <http-locator>) => (scheme :: <symbol>)
  #"http"
end method;

define method \= 
    (loc1 :: <http-locator>, loc2 :: <http-locator>)
 => (equal? :: <boolean>)
  next-method ()
  & locator-host (loc1) = locator-host (loc2)
  & locator-port (loc1) = locator-port (loc2)
  & locator-directory (loc1) = locator-directory (loc2)
  & locator-base (loc1) = locator-base (loc2)
  & locator-type (loc1) = locator-type (loc2)
  & locator-search-keys (loc1) = locator-search-keys (loc2)
end method;

define method make
    (class == <http-locator>,
     #key host, port, directory-path, directory = directory-path,
          base, type, search-keys, prefix, name, extension)
 => (locator :: <http-locator>)
  verify-locator-arguments
    (host, port, #f, directory, base, type, prefix, name, extension);
  if (prefix)
    let prefix-locator = as (<http-locator>, prefix); // inefficient!
    host := prefix-locator.locator-host;
    port := prefix-locator.locator-port;
    directory := prefix-locator.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    port := f-or-as (<integer>, port); // is this right?
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := #"derived";
    type := #"derived";
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>,
			     parse-posix-extension);
    name := #"derived";
  else
    type := f-or-as (<posix-type-path>, type);
    name := #"derived";
  end if;
  let subclass = if ((base | type) & name ~= "")
		   <http-file-locator>
		 else
		   <http-directory-locator>
		 end;
  make (subclass, host: host, port: port, directory: directory, name: name,
	base: base, type: type, search-keys: search-keys)
end method;

define class <http-directory-locator> (<directory-locator>,
				       <http-locator>)
end class;

define class <http-file-locator> (<file-locator>,
				  <http-locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <http-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  make (<http-directory-locator>,
	host: locator-host (locator),
	port: locator-port (locator),
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)),
	search-keys: locator-search-keys (locator)) //nuke or preserve?
end method;

define method as
    (class == <file-locator>, locator :: <http-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<http-file-locator>,
	host: locator-host (locator),
	port: locator-port (locator),
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))),
	search-keys: locator-search-keys (locator)) //nuke or preserve?
end method;

// Merging

define method merge-locators 
    (loc1 :: <http-locator>, loc2 :: <locator>)
 => (result :: <http-locator>)
  make (<http-locator>,
	host: merge-components (locator-host (loc1), locator-host (loc2)),
	port: merge-components (locator-port (loc1), locator-port (loc2)),
	directory: merge-components (locator-directory (loc1),
				     locator-directory (loc2)),
	base: merge-components (locator-base (loc1), locator-base (loc2)),
	type: merge-components (locator-type (loc1), locator-type (loc2)),
	search-keys: merge-components (locator-search-keys(loc1),
				       locator-search-keys(loc2)))
end method;

define method abbreviate-locator
    (locator :: <http-locator>, default :: <locator>)
 => (result :: <http-locator>)
  make (<http-locator>,
	host: abbreviate-component (locator-host (locator), locator-host (default)),
	port: abbreviate-component (locator-port (locator), locator-port (default)),
	directory: abbreviate-component (locator-directory (locator),
					 locator-directory (default)),
	base: locator-base (locator), // always keep?
	type: abbreviate-component (locator-type (locator), locator-type (default)),
	search-keys: abbreviate-component (locator-search-keys (locator),
					   locator-search-keys (default)))
end method;

define method override-locator
    (locator :: <http-locator>,
     #key host = unsupplied (),
	  port = unsupplied (),
	  directory-path = unsupplied(),
	  directory = directory-path,
	  base = unsupplied (),
	  type = unsupplied (),
	  search-keys = unsupplied (),
	  prefix = unsupplied (),
	  name = unsupplied (),
	  extension = unsupplied (),
     #all-keys)
 => (result :: <http-locator>)
  ensure-no-redundancy ((supplied? (host) | supplied? (port) | supplied? (directory)),
			supplied? (base), supplied? (type), supplied? (prefix),
			supplied? (name), supplied? (extension));
  search-keys := override-component (locator-search-keys (locator), search-keys, #f);
  if (unsupplied? (name))
    if (supplied? (extension))
      type := extension & parse-extension (extension, <posix-type-path>,
					   parse-posix-extension);
    end;
    base := override-component (locator-base (locator), base, #f);
    type := override-component (locator-type (locator), type, <posix-type-path>);
  end;
  if (supplied? (prefix))
    if (supplied? (name))
      make (<http-locator>, prefix: prefix, name: name, search-keys: search-keys)
    else
      make (<http-locator>, prefix: prefix, base: base, type: type,
	    search-keys: search-keys)
    end
  else
    host := override-component (locator-host (locator), host, <host-path>);
    port := override-component (locator-port (locator), port, #f);
    directory := override-component (locator-directory (locator), directory,
				     <abstract-directory-path>);
    if (supplied? (name))
      make (<http-locator>, host: host, port: port, directory: directory,
	    name: name, search-keys: search-keys)
    else
      make (<http-locator>, host: host, port: port, directory: directory,
	    base: base, type: type, search-keys: search-keys)
    end
  end
end method;

define method default-locator 
    (locator :: <http-locator>,
     #key host, port, directory-path, directory = directory-path,
          base, type, search-keys, prefix, name, extension,
     #all-keys)
 => (result :: <http-locator>)
  ensure-no-redundancy ((host | port | directory), base, type, prefix, name, extension);
  let defaults = (prefix | name)
    & as (<http-locator>, concatenate (prefix | "", name | "")); // inefficient!
  if (prefix)
    host := defaults.locator-host;
    port := defaults.locator-port;
    directory := defaults.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := defaults.locator-base;
    type := defaults.locator-type
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-posix-extension);
  else
    type := f-or-as (<posix-type-path>, type);
  end if;
  make (<http-locator>,
	host: default-component (locator-host (locator), host),
	port: default-component (locator-port (locator), port),
	directory: default-component (locator-directory (locator), directory),
	base: default-component (locator-base (locator), base),
	type: default-component (locator-type (locator), type),
	search-keys: default-component (locator-search-keys (locator), search-keys))
end method;							        

//

define method locator-prefix 
    (locator :: <http-locator>) => (prefix :: <string>)
  let host = as(<string>, locator-host(locator) | "");
  let port = locator-port (locator);
  let port-name
    = if (port)
	concatenate-as(<string>, ":", as(<string>, port))
      else
	""
      end;
  let dir = as(<string>, locator-directory(locator) | "/");
  concatenate-as(<string>, "http://", host, port-name, dir)
end method;

define method locator-suffix 
    (locator :: <http-locator>) => (suffix :: <string>)
  let search-keys = locator-search-keys(locator);
  if (search-keys)
    concatenate-as(<string>, "?", search-keys)
  else
    ""
  end if
end method;

define method grok-host-port 
    (locator-string :: <string>, #key question-ok? :: <boolean>)
 => (host, port, dir-start, host-only? :: <boolean>)
  let prefix? = looking-at-url-prefix?(locator-string);
  let prefix-size = if (prefix?) position($host-separator, locator-string) + 1 else 0 end if;
  let first-slash = position($url-separator, locator-string);
  let second-slash = first-slash & position($url-separator, locator-string, start: first-slash + 1);
  let third-slash = (second-slash & position($url-separator, locator-string, 
					     start: second-slash + 1))
                    | (question-ok? & position('?', locator-string));
  let colon = position($host-separator, locator-string, start: prefix-size);
  if (first-slash & second-slash & second-slash == first-slash + 1)
    if (third-slash)
      if (colon & colon < third-slash)
	values(parse-host(locator-string, 
			  start: second-slash + 1,
			  end: colon),
	       string-to-integer(locator-string,
				 start: colon + 1,
				 end: third-slash),
	       third-slash,
	       #f)
      else
	values(parse-host(locator-string, 
			  start: second-slash + 1,
			  end: third-slash),
	       #f,
	       third-slash,
	       #f)
      end if
    else // host only case
      if (colon)
	values(parse-host(locator-string, 
			  start: second-slash + 1,
			  end: colon),
	       string-to-integer(locator-string, start: colon + 1),
	       #f,
	       #t)
      else
	values(parse-host(locator-string, 
			  start: second-slash + 1),
	       #f,
	       #f,
	       #t)
      end if
    end if
  else // no host case
    values(#f, 
	   #f,
	   prefix-size,
	   #f)
  end if
end method;

define method as
    (class == <http-locator>, locator-string :: <string>)
 => (locator :: <http-locator>)
  // Parse as http locator:  http://host[:port]/dir.../name.ext[?search-keys]
  let locator-string = trim-whitespace(locator-string);
  let (host, port, dir-start, host-only?) = grok-host-port(locator-string,
							   question-ok?: #t);
  if (host-only?)
    make(<http-directory-locator>, host: host)
  else
    let dir-end = position-from-end($url-separator, locator-string);
    let (directory, base-start)
      = if (~dir-end | dir-end < dir-start) // ever happen?
	  values(#f, dir-start)
	elseif (dir-end = dir-start) // same slash
	  //#f
	  values(make(<abstract-directory-path>, elements: #(), relative-path?: #f),
		 dir-end + 1)
	else
	  values(parse-directory(locator-string, 
				 <abstract-directory-path>, $url-separator,
				 start: dir-start,
				 end: dir-end),
		 dir-end + 1)
	end if;
    if (dir-end & dir-end + 1 = size(locator-string))
      make(<http-directory-locator>, host: host,
	                             port: port,
                                     directory: directory)
    else
      //let base-start = dir-end + 1;
      //let ext-start = position ('.', locator-string, start: base-start + 1);
      let query-start = position ('?', locator-string, start: base-start);
      /* // don't derive name now
      let (base, type) =
	if (ext-start = base-start)  // eg .login
	  values
	    (parse-base(copy-sequence(locator-string, 
					   start: base-start,
					   end: query-start
					     | size(locator-string))),
	     #f)
	else
	  values
	    (parse-base(copy-sequence(locator-string, 
					   start: base-start,
					   end: ext-start
					     | query-start
					     | size(locator-string))),
	     if (ext-start == #f)      
	       unspecified-or-empty-path (<posix-type-path>)
	     else
	       parse-extension(locator-string, 
			       <posix-type-path>,
			       parse-posix-extension,
			       start: ext-start + 1,
			       end: query-start | size(locator-string))
	     end if)
	end if;
      */
      let search-keys = query-start & copy-sequence (locator-string,
						     start: query-start + 1);
      let name =  copy-sequence (locator-string, start: base-start,
				 end: query-start | size (locator-string));
      make(<http-locator>, host: host, port: port, // will choose dir or file
	                   directory: directory,
	                   base: #"derived", type: #"derived",
	                   name: name,
	                   search-keys: search-keys);
    end if;
  end if;
end method;


// FTP:

define abstract class <ftp-locator> (<locator-remote-host-login-slots>,
				     <locator-directory-slot>,
				     <locator-derived-base-slot>,
				     <locator-derived-type-slot>,
				     <locator-derived-name-slot>,
				     <locator-transfer-type-slot>,
				     <url-locator>)
end class;

define method locator-scheme 
    (locator :: <ftp-locator>) => (scheme :: <symbol>)
  #"ftp"
end method;

define method \= 
    (loc1 :: <ftp-locator>, loc2 :: <ftp-locator>)
 => (equal? :: <boolean>)
  next-method() 
  & locator-directory (loc1) = locator-directory (loc2)
  & locator-base (loc1) = locator-base (loc2)
  & locator-type (loc1) = locator-type (loc2)
  & locator-transfer-type (loc1) = locator-transfer-type (loc2)
end method;

define method make
    (class == <ftp-locator>,
     #key host, port, user-id, password,
          directory-path, directory = directory-path, base, type, 
          transfer-type, prefix, name, extension)
 => (locator :: <ftp-locator>)
  verify-locator-arguments
    (host, port, #f, directory, base, type, prefix, name, extension);
  if (prefix)
    let prefix-locator = as (<ftp-locator>, prefix); // inefficient!
    host := prefix-locator.locator-host;
    port := prefix-locator.locator-port;
    user-id := prefix-locator.locator-user-id;
    password := prefix-locator.locator-password;
    directory := prefix-locator.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    port := f-or-as (<integer>, port); // is this right?
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := #"derived";
    type := #"derived";
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>,
			     parse-posix-extension);
    name := #"derived";
  else
    type := f-or-as (<posix-type-path>, type);
    name := #"derived";
  end if;
  let subclass = if ((base | type) & name ~= "")
		   <ftp-file-locator>
		 else
		   <ftp-directory-locator>
		 end;
  make (subclass, host: host, port: port, user-id: user-id, password: password,
	directory: directory, name: name, base: base, type: type,
	transfer-type: f-or-as (<ftp-transfer-type>, transfer-type))
end method;

define class <ftp-directory-locator> (<directory-locator>,
				      <ftp-locator>)
end class;

define class <ftp-file-locator> (<file-locator>,
				 <ftp-locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <ftp-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  make (<ftp-directory-locator>,
	host: locator-host (locator),
	port: locator-port (locator),
	user-id: locator-user-id (locator),
	password: locator-password (locator),
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)),
	transfer-type: locator-transfer-type (locator))
end method;

define method as
    (class == <file-locator>, locator :: <ftp-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<ftp-file-locator>,
	host: locator-host (locator),
	port: locator-port (locator),
	user-id: locator-user-id (locator),
	password: locator-password (locator),
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))),
	transfer-type: locator-transfer-type (locator))
end method;

// merge

define method merge-locators 
    (loc1 :: <ftp-locator>, loc2 :: <locator>)
 => (result :: <ftp-locator>)
  make (<ftp-locator>,
	user-id: merge-components (locator-user-id (loc1), locator-user-id (loc2)),
	password: merge-components (locator-password (loc1), locator-password (loc2)),
	host: merge-components (locator-host (loc1), locator-host (loc2)),
	port: merge-components (locator-port (loc1), locator-port (loc2)),
	directory: merge-components (locator-directory (loc1),
				     locator-directory (loc2)),
	base: merge-components (locator-base (loc1), locator-base (loc2)),
	type: merge-components (locator-type (loc1), locator-type (loc2)),
	transfer-type: merge-components (locator-transfer-type (loc1),
					 locator-transfer-type (loc2)))
end method;

define method abbreviate-locator
    (locator :: <ftp-locator>, default :: <locator>)
 => (result :: <ftp-locator>)
  make (<ftp-locator>,
	user-id: abbreviate-component (locator-user-id (locator),
				       locator-user-id (default)),
	password: abbreviate-component (locator-password (locator),
					locator-password (default)),
	host: abbreviate-component (locator-host (locator), locator-host (default)),
	port: abbreviate-component (locator-port (locator), locator-port (default)),
	directory: abbreviate-component (locator-directory (locator),
					 locator-directory (default)),
	base: locator-base (locator), // always keep?
	type: abbreviate-component (locator-type (locator), locator-type (default)),
	transfer-type: abbreviate-component (locator-transfer-type (locator),
					     locator-transfer-type (default)))
end method;

define method override-locator
    (locator :: <ftp-locator>,
     #key host = unsupplied (),
	  port = unsupplied (),
	  user-id = unsupplied (),
	  password = unsupplied (),
	  directory-path = unsupplied (),
	  directory = directory-path,
	  base = unsupplied (),
	  type = unsupplied (),
	  transfer-type = unsupplied (),
	  prefix = unsupplied (),
	  name = unsupplied (),
	  extension = unsupplied (),
     #all-keys)
 => (result :: <ftp-locator>)
  ensure-no-redundancy ((supplied? (host) | supplied? (port)
			   | supplied? (user-id) | supplied? (password)
			   | supplied? (directory)),
			supplied? (base), supplied? (type), supplied? (prefix),
			supplied? (name), supplied? (extension));
  transfer-type := override-component (locator-transfer-type (locator),
				       transfer-type, <ftp-transfer-type>);
  if (unsupplied? (name))
    if (supplied? (extension))
      type := extension & parse-extension (extension, <posix-type-path>,
					   parse-posix-extension);
    end;
    base := override-component (locator-base (locator), base, #f);
    type := override-component (locator-type (locator), type, <posix-type-path>);
  end;
  if (supplied? (prefix))
    if (supplied? (name))
      make (<ftp-locator>, prefix: prefix, name: name, transfer-type: transfer-type)
    else
      make (<ftp-locator>, prefix: prefix, base: base, type: type,
	    transfer-type: transfer-type)
    end
  else
    host := override-component (locator-host (locator), host, <host-path>);
    port := override-component (locator-port (locator), port, #f);
    user-id := override-component (locator-user-id (locator), user-id, #f);
    password := override-component (locator-password (locator), password, #f);
    directory := override-component (locator-directory (locator), directory,
				     <abstract-directory-path>);
    if (supplied? (name))
      make (<ftp-locator>, host: host, port: port, user-id: user-id,
	    password: password, directory: directory,
	    name: name, transfer-type: transfer-type)
    else
      make (<ftp-locator>, host: host, port: port, user-id: user-id,
	    password: password, directory: directory,
	    base: base, type: type, transfer-type: transfer-type)
    end
  end
end method;

define method default-locator
    (locator :: <ftp-locator>,
     #key host, port, user-id, password, directory-path, directory = directory-path,
          base, type, transfer-type, prefix, name, extension,
     #all-keys)
 => (result :: <ftp-locator>)
  ensure-no-redundancy ((host | port | user-id | password | directory),
			base, type, prefix, name, extension);
  transfer-type := f-or-as (<ftp-transfer-type>, transfer-type);
  let defaults = (prefix | name)
    & as (<ftp-locator>, concatenate (prefix | "", name | "")); // inefficient!
  if (prefix)
    host := defaults.locator-host;
    port := defaults.locator-port;
    user-id := defaults.locator-user-id;
    password := defaults.locator-password;
    directory := defaults.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := defaults.locator-base;
    type := defaults.locator-type
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-posix-extension);
  else
    type := f-or-as (<posix-type-path>, type);
  end if;
  make (<ftp-locator>,
	host: default-component (locator-host (locator), host),
	port: default-component (locator-port (locator), port),
	user-id: default-component (locator-user-id (locator), user-id),
	password: default-component (locator-password (locator), password),
	directory: default-component (locator-directory (locator), directory),
	base: default-component (locator-base (locator), base),
	type: default-component (locator-type (locator), type),
	transfer-type: default-component (locator-transfer-type (locator), transfer-type))
end method;							        

//

define method locator-prefix 
    (locator :: <ftp-locator>) => (prefix :: <string>)
  let password = if (locator-password(locator))
		   concatenate(":", locator-password(locator))
		 else "" end;
  let user+pwd = if (locator-user-id(locator))
		   concatenate(locator-user-id(locator), password, "@")
		 else "" end;
  let host = as(<string>, locator-host(locator));
  let port = if (locator-port (locator))
	       concatenate(":", as(<string>, locator-port(locator)))
	     else "" end;
  let dir = as(<string>, locator-directory(locator) | "");
  concatenate("ftp://", user+pwd, host, port, dir)
end method;

define method locator-suffix 
    (locator :: <ftp-locator>) => (suffix :: <string>)
  // the ";" will be returned by "as"
  as(<string>, locator-transfer-type(locator) | "")
end method;

define method grok-login 
    (locator-string :: <string>)
 => (user-id, password, host, port, dir-start, host-only? :: <boolean>);
  // Disclaimer:  This is pretty ugly.  It should be rewritten using a real
  // parser generator!
  local method reduce-to-host-port (locator-string)
	  let (host, port, dir-start, host-only?) = grok-host-port(locator-string);
	  values (#f, #f, host, port, dir-start, host-only?);
	end method;
  let atsign = position('@', locator-string);
  let prefix? = looking-at-url-prefix?(locator-string);
  let prefix-size = if (prefix?) position($host-separator, locator-string) + 1 else 0 end if;
  let first-slash = position($url-separator, locator-string);
  let second-slash = first-slash & position($url-separator, locator-string, start: first-slash + 1);
  let third-slash = second-slash & position($url-separator, locator-string, start: second-slash + 1);
  let first-colon = position($host-separator, locator-string, start: prefix-size);
  let second-colon = first-colon & position($host-separator, locator-string, 
					    start: first-colon + 1);
  if (first-slash & second-slash & second-slash == first-slash + 1)
    // "//..."
    if (third-slash)
      // "//.../"
      if (~atsign | atsign > third-slash)
	// "//host[:port]/" no '@'
	reduce-to-host-port(locator-string);
      // found an '@'
      elseif (first-colon & first-colon < third-slash)
	// "//...:.../" & '@' somewhere
	if (first-colon > atsign)
	  // "//user-id@host:port/"
	    values(copy-sequence(locator-string,      // user-id
				 start: second-slash + 1,
				 end: atsign),
		   #f, // password
		   parse-host(locator-string,         // host
			      start: atsign + 1,
			      end: first-colon),
		   string-to-integer(locator-string,
				     start: first-colon + 1,
				     end: third-slash),
		   third-slash, // dir-start
		   #f) // host-only?
	else
	  // "//user-id:password@.../"
	  if (second-colon & second-colon < third-slash)
	    // "//user-id:password@host:port/"
	    values(copy-sequence(locator-string,      // user-id
				 start: second-slash + 1,
				 end: first-colon),
		   copy-sequence(locator-string,      // password
				 start: first-colon + 1,
				 end: atsign),
		   parse-host(locator-string,         // host
			      start: atsign + 1,
			      end: second-colon),
		   string-to-integer(locator-string,
				     start: second-colon + 1,
				     end: third-slash),
		   third-slash, // dir-start
		   #f) // host-only?
	  else
	    // "//user-id:password@host/"
	    values(copy-sequence(locator-string,      // user-id
				 start: second-slash + 1,
				 end: first-colon),
		   copy-sequence(locator-string,      // password
				 start: first-colon + 1,
				 end: atsign),
		   parse-host(locator-string,         // host
			      start: atsign + 1,
			      end: third-slash),
		   #f, // port
		   third-slash, // dir-start
		   #f) // host-only?
	  end if; // (second-colon & second-colon < third-slash)
	end if; // (first-colon > atsign)
      else
	// "//user-id@host/"
	values(copy-sequence(locator-string,      // user-id
			     start: second-slash + 1,
			     end: atsign),
	       #f, // password
	       parse-host(locator-string,         // host
			  start: atsign + 1,
			  end: third-slash),
	       #f, // port
	       third-slash, // dir-start
	       #f) // host-only?
      end if; // (~atsign | atsign > third-slash)
              // elseif (first-colon & first-colon < third-slash)
    else
      // "//..." no third slash
      reduce-to-host-port(locator-string);
    end if;
  else
    // "..." no "//"
    reduce-to-host-port(locator-string);
  end if;
end method;

define method as
    (class == <ftp-locator>, locator-string :: <string>)
 => (locator :: <ftp-locator>)
  // Parse as ftp locator:
  // ftp://[user-id[:password]@]host[:port]/dir.../name.ext
  // [;type={AEIL}{NTC|nn}]
  let locator-string = trim-whitespace(locator-string);
  let (user-id, password, host, port, dir-start, host-only?)
    = grok-login(locator-string);
  if (host-only?) // error?
    make(<ftp-directory-locator>, user-id: user-id,
	                          password: password,
				  host: host,
	                          port: port)
  else
    let dir-end = position-from-end($url-separator, locator-string);
    let (directory, base-start)
      = if (~dir-end | dir-end < dir-start) // ever happen?
	  values(#f, dir-start)
	elseif (dir-end = dir-start) // same slash
	  //#f
	  values(make(<abstract-directory-path>, elements: #(), relative-path?: #f),
		 dir-end + 1)
	else
	  values(parse-directory(locator-string, 
				 <abstract-directory-path>, $url-separator,
				 start: dir-start,
				 end: dir-end),
		 dir-end + 1)
	end if;
    if (dir-end & dir-end + 1 = size(locator-string))
      make(<ftp-directory-locator>, user-id: user-id,
	                            password: password,
				    host: host,
	                            port: port,
	                            directory: directory)
    else
      //let base-start = dir-end + 1;
      //let ext-start = position('.', locator-string, start: base-start);
      let type-start = position(';', locator-string, 
				start: base-start);
      /* // Don't derive name now
      let (base, type) =
	if (ext-start = base-start)  // eg .login
	  values
	    (parse-base(copy-sequence(locator-string, 
					   start: base-start,
					   end: type-start
					     | size(locator-string))),
	     #f)
	else
	  values
	    (parse-base(copy-sequence(locator-string, 
					   start: base-start,
					   end: ext-start
					     | type-start
					     | size(locator-string))),
	     if (ext-start == #f)      
	       unspecified-or-empty-path (<posix-type-path>)
	     else
	       parse-extension(locator-string, 
			       <posix-type-path>,
			       parse-posix-extension,
			       start: ext-start + 1,
			       end: type-start | size(locator-string))
	     end if)
	end if;
      */
      let transfer-type = type-start & as (<ftp-transfer-type>,
					   copy-sequence (locator-string, 
							  start: type-start + 1));
      let name =  copy-sequence (locator-string, start: base-start,
				 end: type-start | size (locator-string));
      make(<ftp-file-locator>, user-id: user-id,
	                       password: password,
	                       host: host, port: port, directory: directory,
	                       base: #"derived", type: #"derived",
	                       name: name,
	                       transfer-type: transfer-type);
    end if;
  end if;
end method;

define class <ftp-transfer-type> (<object>)
  constant slot type-code :: false-or(<character>) =  #f,
    init-keyword: type-code:;
  constant slot form-code :: false-or(<character>) = #f,
    init-keyword: form-code:;
  constant slot byte-size :: false-or(<integer>) = #f,
    init-keyword: byte-size:;
end class;

define method \= 
    (t1 :: <ftp-transfer-type>, t2 :: <ftp-transfer-type>)
 => (equal? :: <boolean>)
  type-code(t1) == type-code(t2)
  & form-code(t1) == form-code(t2)
  & byte-size(t1) == byte-size(t2)
end method;

/* FTP representations:

 type A[form] - ASCII
      E[form] - EBCDIC
      I - Image
      Lnn - Local (nn=Byte Size)

 form N - Non-Print
      T - Telnet format effectors
      C - Carriage Control (ASA)
*/

define method as
    (class == <ftp-transfer-type>, string :: <string>)
 => (type :: <ftp-transfer-type>)
  let size = size(string);
  let equal-pos = position('=', string);
  if (~equal-pos | equal-pos + 1 >= size)
    locator-error(<locator-parse-error>, "Missing FTP type")
  else
    let type-code = as-uppercase(string[equal-pos + 1]);
    select (type-code)
      'L' =>
	make(<ftp-transfer-type>,
	     type-code: 'L',
	     byte-size: string-to-integer(string, start: equal-pos + 2));
      'I' =>
	make(<ftp-transfer-type>, type-code: 'I');
      'A', 'E' =>
	if (equal-pos + 2 < size)
	  let form-code = as-uppercase(string[equal-pos + 2]);
	  if (~member?(form-code, "NTC"))
	    locator-error(<locator-parse-error>,
			  "Invalid FTP form code: %c", form-code)
	  else
	    make(<ftp-transfer-type>,
		 type-code: type-code,
		 form-code: form-code)
	  end if;
	else
	    make(<ftp-transfer-type>,
		 type-code: type-code,
		 form-code: 'N') // default: Non-Print
	end if;
      otherwise =>
	locator-error(<locator-parse-error>, "Invalid FTP type code: %c", type-code);
    end select;
  end if;
end method;

define method as
    (class == <string>, mode :: <ftp-transfer-type>)
 => (string :: <string>)
  let string = "";
  let type-code = type-code(mode);
  let form-code = form-code(mode);
  if (type-code)
    if (type-code == 'L')
      //format-to-string(";type=L%d", byte-size(mode))
      string := concatenate(";type=L", as(<string>, byte-size(mode)));
    elseif (~form-code)
      //format-to-string(";type=%c", type-code)
      string := copy-sequence(";type=?");
      string[6] := type-code;
    else
      //format-to-string(";type=%c%c", type-code, form-code)
      string := copy-sequence(";type=??");
      string[6] := type-code;
      string[7] := form-code;
    end if;
  end if;
  string
end method;

// MAILTO:

define class <mailto-locator> (<locator-user-id-slot>,
			       <locator-host-slot>,
			       <url-locator>)
end class;

define method locator-scheme 
    (locator :: <mailto-locator>) => (scheme :: <symbol>)
  #"mailto"
end method;

define method make
    (class == <mailto-locator>, #key user-id, host)
 => (locator :: <mailto-locator>)
  let host = as(<host-path>, host);
  let user-id = as(<string>, user-id);
  next-method(class, user-id: user-id, host: host);
end method;

define method as
    (class == <string>, locator :: <mailto-locator>)
 => (string :: <string>)
  concatenate-as
    (<string>,
     "mailto:",
     locator-user-id (locator), 
     "@",
     as(<string>, locator-host (locator)))
end method;

define method as
    (class == <mailto-locator>, locator-string :: <string>)
 => (locator :: <mailto-locator>)
  let atsign = position ('@', locator-string);
  if (atsign == #f)
    locator-error(<locator-parse-error>, "AS <MAILTO-LOCATOR>: No @")
  elseif (~looking-at?("mailto:", locator-string))
    locator-error(<locator-parse-error>, "AS <MAILTO-LOCATOR>: No prefix")
  else
    make(<mailto-locator>, user-id: copy-sequence(locator-string,
						  start: size("mailto:"),
						  end: atsign),
	                   host: parse-host(locator-string, 
					    start: atsign + 1))
  end if
end method;

