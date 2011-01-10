Module:       locators-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $web-separator           = '/';
define constant $web-extension-separator = '.';
define constant $web-protocol-separator  = ':';
define constant $web-host-prefix         = "//";
define constant $web-port-separator      = ':';
define constant $web-username-separator  = '?';
define constant $web-password-separator  = ':';
define constant $web-cgi-separator       = '?';
define constant $web-index-separator     = '#';

define constant $http-protocol   = "http";
define constant $https-protocol  = "https";
define constant $ftp-protocol    = "ftp";
define constant $file-protocol   = "file";
define constant $mailto-protocol = "mailto";


/// Web locators

define sealed abstract class <web-locator> (<locator>)
  virtual constant slot locator-protocol :: <string>;
end class <web-locator>;

define constant $web-protocol-classes = make(<string-table>);
define constant $web-protocol-names   = make(<object-table>);

define function register-web-protocol-class
    (protocol :: <string>, class :: subclass(<web-locator>)) => ()
  $web-protocol-classes[protocol] := class;
  $web-protocol-names[class]    := protocol
end function register-web-protocol-class;

define function web-protocol-class
    (protocol :: <string>) => (class :: subclass(<web-locator>))
  $web-protocol-classes[protocol]
end function web-protocol-class;

define function locator-protocol
    (locator :: <web-locator>) => (protocol :: <string>)
  $web-protocol-names[object-class(locator)]
end function locator-protocol;

define sealed method string-as-locator
    (class == <web-locator>, string :: <string>)
 => (locator :: <web-locator>)
  let pos = find-delimiter(string, $web-protocol-separator);
  let protocol = pos & copy-sequence(string, end: pos);
  select (protocol by \=)
    $mailto-protocol => as(<mail-to-locator>, string);
    otherwise        => as(<url>, string);
  end
end method string-as-locator;


/// URLs

define sealed abstract class <url> (<web-locator>, <physical-locator>)
end class <url>;

define sealed method string-as-url
    (string :: <string>, #key protocol :: false-or(<string>))
 => (url :: <url>)
  let (class, host, port, username, password, path, relative?,
       base, extension, cgi-string, index)
    = parse-url(string, protocol: protocol);
  let server
    = if (host)
	make(class,
	     host:     host,
	     port:     port,
	     username: username,
	     password: password)
      end;
  let directory
    = if (path)
	make(<directory-url>,
	     server:    server,
	     path:      path,
	     relative?: relative?)
      end;
  if (base | extension)
    let file
      = make(<file-url>,
	     directory: directory,
	     base:      base,
	     extension: extension);
    case
      cgi-string =>
	make(<cgi-url>,
	     file: file,
	     cgi-string: cgi-string);
      index =>
	make(<file-index-url>,
	     file: file,
	     index: index);
      otherwise =>
	file;
    end
  else
    directory
      | locator-error("Invalid URL '%s': no file or directory component",
		      if (protocol)
			concatenate
			  (protocol, 
			   delimiter-to-string($web-protocol-separator))
		      else
			string
		      end)
  end
end method string-as-url;

define sealed method string-as-locator
    (class == <url>, string :: <string>)
 => (locator :: <url>)
  string-as-url(string)
end method string-as-locator;


/// Servers

define sealed abstract class <server-url> (<server-locator>, <url>)
  sealed constant slot locator-host :: <string>,
    required-init-keyword: host:;
  sealed constant slot %port :: false-or(<integer>) = #f,
    init-keyword: port:;
  sealed constant slot locator-username :: false-or(<string>) = #f,
    init-keyword: username:;
  sealed constant slot locator-password :: false-or(<string>) = #f,
    init-keyword: password:;
end class <server-url>;

define generic locator-default-port
    (locator :: <server-url>) => (port :: false-or(<integer>));


define sealed method locator-port
    (server :: <server-url>) => (port :: false-or(<integer>))
  server.%port | locator-default-port(server)
end method locator-port;

define sealed method locator-default-port
    (server :: <server-url>) => (port :: singleton(#f))
  #f
end method locator-default-port;

define sealed method string-as-locator
    (class :: subclass(<server-url>), string :: <string>)
 => (locator :: <server-url>)
  let (class, host, port, username, password, path, relative?, 
       base, extension, cgi-string, index)
    = parse-url(string);
  if (path | relative? | base | extension | cgi-string | index)
    locator-error("Invalid server URL '%s'", string)
  end;
  unless (host)
    locator-error("Missing hostname in server URL '%s'", string)
  end;
  make(class,
       host:     host,
       port:     port,
       username: username,
       password: password)
end method string-as-locator;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <server-url>)
 => (string :: <string>)
  let port     = locator.%port;
  let username = locator.locator-username;
  let password = locator.locator-password;
  let name-without-port
    = concatenate-as(class,
		     locator.locator-protocol,
		     delimiter-to-string($web-protocol-separator),
		     $web-host-prefix,
		     locator.locator-host);
  if (port | username | password)
    concatenate-as
      (class,
       name-without-port,
       if (port)
	 concatenate-as(class,
			delimiter-to-string($web-port-separator),
			integer-to-string(port))
       else
	 #[]
       end,
       if (username)
	 concatenate-as(class,
			delimiter-to-string($web-username-separator),
			username)
       else
	 #[]
       end,
       if (password)
	 concatenate-as(class,
			delimiter-to-string($web-password-separator),
			password)
       else
	 #[]
       end)
  else
    name-without-port
  end
end method locator-as-string;

define sealed method \=
    (locator1 :: <server-url>,
     locator2 :: <server-url>)
 => (equal? :: <boolean>)
  locator1.locator-host = locator2.locator-host
    & locator1.locator-port = locator2.locator-port
    & locator1.locator-username = locator2.locator-username
    & locator1.locator-password = locator2.locator-password
end method \=;


define sealed class <http-server> (<server-url>)
end class <http-server>;

register-web-protocol-class($http-protocol, <http-server>);

define sealed method locator-default-port
    (server :: <http-server>) => (port :: <integer>)
  80
end method locator-default-port;

define method http-parser
    (text :: <byte-string>) => (locator :: <url>)
  string-as-url(text, protocol: $http-protocol)
end method http-parser;


define sealed class <https-server> (<server-url>)
end class <https-server>;

register-web-protocol-class($https-protocol, <https-server>);

define sealed method locator-default-port
    (server :: <https-server>) => (port :: <integer>)
  80
end method locator-default-port;

define method https-parser
    (text :: <byte-string>) => (locator :: <url>)
  string-as-url(text, protocol: $https-protocol)
end method https-parser;


define sealed class <ftp-server> (<server-url>)
end class <ftp-server>;

register-web-protocol-class($ftp-protocol, <ftp-server>);

define method ftp-parser
    (text :: <byte-string>) => (locator :: <url>)
  string-as-url(text, protocol: $ftp-protocol)
end method ftp-parser;


define sealed class <file-server> (<server-url>)
end class <file-server>;

register-web-protocol-class($file-protocol, <file-server>);

define sealed method locator-default-port
    (server :: <file-server>) => (port :: <integer>)
  80
end method locator-default-port;

define method file-parser
    (text :: <byte-string>) => (locator :: <url>)
  string-as-url(text, protocol: $file-protocol)
end method file-parser;


/// URL directories

define sealed class <directory-url> 
    (<directory-locator>, <url>)
  sealed constant slot locator-server :: false-or(<server-url>) = #f,
    init-keyword: server:;
  sealed constant slot locator-relative? :: <boolean> = #f,
    init-keyword: relative?:;
  sealed constant slot locator-path :: <simple-object-vector>,
    required-init-keyword: path:;
end class <directory-url>;

define sealed method make
    (class == <directory-url>,
     #key server :: false-or(<server-url>) = #f,
          path :: false-or(<sequence>) = #f,
          relative? :: <boolean> = #f,
          directory :: false-or(<directory-url>) = #f,
          name :: false-or(<string>))
 => (locator :: <directory-url>)
  let path
    = if (name | directory)
	concatenate(if (directory) directory.locator-path else #[] end,
		    if (name) vector(name) else #[] end)
      else
	path
      end;
  next-method(class,
	      server:    server,
	      path:      canonicalize-path(path),
	      relative?: relative?)
end method make;

define sealed method locator-name
    (locator :: <directory-url>)
 => (name :: false-or(<string>))
  let path = locator.locator-path;
  unless (empty?(path))
    path[size(path) - 1]
  end
end method locator-name;

define sealed method \=
    (locator1 :: <directory-url>,
     locator2 :: <directory-url>)
 => (equal? :: <boolean>)
  locator1.locator-relative? = locator2.locator-relative?
    & locator1.locator-server = locator2.locator-server
    & locator1.locator-path.size = locator2.locator-path.size
    & every?(\=, locator1.locator-path, locator2.locator-path)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <directory-url>)
 => (string :: <string>)
  let server = locator.locator-server;
  let directory-string
    = path-to-string(locator.locator-path,
		     class:     class,
		     separator: $web-separator,
		     relative?: locator.locator-relative?);
  if (server)
    concatenate-as(class,
		   as(class, server),
		   directory-string)
  else
    directory-string
  end
end method locator-as-string;

define sealed method locator-test
    (locator :: <directory-url>) => (test :: <function>)
  \=
end method locator-test;


define sealed class <file-url> (<file-locator>, <url>)
  sealed constant slot locator-directory :: false-or(<directory-url>) = #f,
    init-keyword: directory:;
  sealed constant slot locator-base :: false-or(<string>) = #f,
    init-keyword: base:;
  sealed constant slot locator-extension :: false-or(<string>) = #f,
    init-keyword: extension:;
end class <file-url>;

define sealed method make
    (class == <file-url>,
     #key directory :: false-or(<directory-url>),
          base :: false-or(<string>),
          extension :: false-or(<string>),
          name :: false-or(<string>))
 => (locator :: <file-url>)
  let directory
    = unless (directory & current-directory-locator?(directory))
	directory
      end;
  let pos = name & find-delimiter-from-end(name, $web-extension-separator);
  let base = base | if (pos) copy-sequence(name, end: pos) else name end;
  let extension = extension | if (pos) copy-sequence(name, start: pos + 1) end;
  if (~base)
    locator-error("Attemped to create a file locator without a base")
  end;
  next-method(class,
	      directory: directory,
	      base: base,
	      extension: extension)
end method make;

define sealed method locator-server
    (locator :: <file-url>)
 => (server :: false-or(<server-url>))
  let directory = locator.locator-directory;
  directory & directory.locator-server
end method locator-server;

define sealed method locator-name
    (locator :: <file-url>)
 => (name :: false-or(<string>))
  let base = locator.locator-base;
  let extension = locator.locator-extension;
  if (extension)
    concatenate(base | "",
		delimiter-to-string($web-extension-separator),
		extension)
  else
    base
  end
end method locator-name;

define sealed method \=
    (locator1 :: <file-url>,
     locator2 :: <file-url>)
 => (equal? :: <boolean>)
  locator1.locator-directory = locator2.locator-directory
    & locator1.locator-base = locator2.locator-base
    & locator1.locator-extension = locator2.locator-extension
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <file-url>)
 => (string :: <string>)
  let directory = locator.locator-directory;
  let name = locator.locator-name;
  if (directory)
    concatenate-as(class, as(<string>, directory), name)
  else
    name
  end
end method locator-as-string;


/// File index locators

// These might be better called <fragment-locator> to follow the terminology in
// RFC 3986.

define sealed class <file-index-url> (<url>)
  sealed constant slot locator-file :: <file-url>,
    required-init-keyword: file:;
  sealed constant slot locator-index :: <string>,
    required-init-keyword: index:;
end class <file-index-url>;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <file-index-url>)
 => (string :: <string>)
  concatenate-as(class,
		 as(class, locator.locator-file),
		 delimiter-to-string($web-index-separator),
		 locator.locator-index)
end method locator-as-string;

define sealed method locator-server
    (locator :: <file-index-url>)
 => (server :: false-or(<server-url>))
  let file = locator.locator-file;
  let directory = file & file.locator-directory;
  directory & directory.locator-server
end method locator-server;


/// CGI locators

// These might be better called <query-locator> to follow the terminology in
// RFC 3986.

define sealed class <cgi-url> (<url>)
  sealed constant slot locator-file :: <file-url>,
    required-init-keyword: file:;
  sealed constant slot locator-cgi-string :: <string>,
    required-init-keyword: cgi-string:;
end class <cgi-url>;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <cgi-url>)
 => (string :: <string>)
  concatenate-as(class,
		 as(class, locator.locator-file),
		 delimiter-to-string($web-cgi-separator),
		 locator.locator-cgi-string)
end method locator-as-string;


/// Mail-to locators

define sealed class <mail-to-locator> (<url>)
  sealed constant slot locator-address :: <string>,
    required-init-keyword: address:;
end class <mail-to-locator>;

register-web-protocol-class($mailto-protocol, <mail-to-locator>);

define sealed method locator-name
    (locator :: <mail-to-locator>) => (name :: <string>)
  locator.locator-address
end method locator-name;

define sealed method string-as-locator
    (class == <mail-to-locator>, string :: <string>)
 => (locator :: <mail-to-locator>)
  let pos = find-delimiter(string, $web-protocol-separator);
  let protocol = pos & copy-sequence(string, end: pos);
  unless (protocol = $mailto-protocol)
    locator-error("Cannot convert %= into <mail-to-locator>",
		  string)
  end;
  let address-pos = pos + 1;
  unless (string.size > address-pos + 1)
    locator-error("Mail to locator is missing an address")
  end;
  make(<mail-to-locator>,
       address: copy-sequence(string, start: address-pos))
end method string-as-locator;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <mail-to-locator>)
 => (string :: <string>)
  concatenate-as(class,
		 $mailto-protocol,
		 delimiter-to-string($web-separator),
		 locator.locator-address)
end method locator-as-string;

define method mailto-parser
    (text :: <byte-string>) => (locator :: <mail-to-locator>)
  make(<mail-to-locator>, address: text)
end method mailto-parser;


/// Utilities

define sealed method parse-url
    (string :: <string>, #key protocol :: false-or(<string>) = #f)
 => (class :: subclass(<server-url>),
     host :: false-or(<string>),
     port :: false-or(<integer>),
     username :: false-or(<string>),
     password :: false-or(<string>),
     path :: false-or(<sequence>),
     relative? :: <boolean>,
     base :: false-or(<string>),
     extension :: false-or(<string>),
     cgi-string :: false-or(<string>),
     index :: false-or(<string>))
  let stop :: <integer> = string.size;
  local 
    method parse-protocol
	() => (class :: subclass(<server-url>), next-index :: <integer>)
      let (protocol, pos)
        = if (protocol)
	    values(protocol, 0)
	  else
	    let pos = find-delimiter(string, $web-protocol-separator);
	    unless (pos)
	      locator-error("Missing protocol in URL '%s'", string)
	    end;
	    values(copy-sequence(string, end: pos), pos + 1)
	  end;
      let class = web-protocol-class(protocol);
      unless (class)
	locator-error("Unrecognised URL protocol '%s' in '%s'", 
		      protocol, string)
      end;
      values(class, pos)
    end method parse-protocol,

    method parse-host
	(start :: <integer>)
     => (host :: false-or(<string>), next-index :: <integer>)
      let prefix-end = start + $web-host-prefix.size;
      let prefix
	= prefix-end < stop
	    & copy-sequence(string, start: start, end: prefix-end);
      if (prefix = $web-host-prefix)
	let next-index
	  = find-delimiters(string,
			    vector($web-port-separator,
				   $web-username-separator,
				   $web-separator),
			    start: prefix-end)
	      | stop;
	let host
	  = copy-sequence(string,
			  start: prefix-end,
			  end: next-index);
	values(host, next-index)
      else
	values(#f, start)
      end
    end method parse-host,

    method parse-port
	(start :: <integer>)
     => (port :: false-or(<integer>), next-index :: <integer>)
      if (start < stop & string[start] == $web-port-separator)
	let next-index
	  = find-delimiters(string,
			    vector($web-username-separator,
				   $web-separator),
			    start: start + 1)
	      | stop;
	let port
	  = string-to-integer
	      (string, start: start + 1, end: next-index, default: -1);
	if (port == -1)
	  locator-error("Invalid port supplied for locator '%s'", string)
	else
	  values(port, next-index)
	end
      else
        values(#f, start)
      end
    end method parse-port,

    method parse-username
	(start :: <integer>)
     => (username :: false-or(<string>), password :: false-or(<string>),
	 next-index :: <integer>)
      if (start < stop & string[start] == $web-username-separator)
	let start = start + 1;
	let password-start
	  = find-delimiter(string, $web-password-separator, start: start);
	let password-stop
	  = find-delimiter(string, $web-separator,
			   start: password-start | start)
	      | stop;
	values(copy-sequence(string,
			     start: start, 
			     end: password-start | password-stop),
	       if (password-start)
		 copy-sequence(string,
			       start: password-start + 1,
			       end:   password-stop)
	       end,
	       password-stop)
      else
	values(#f, #f, start)
      end
    end method parse-username,

    method parse-directory
	(start :: <integer>, stop :: <integer>)
     => (path :: false-or(<sequence>), relative? :: <boolean>,
	 next-index :: <integer>)
      if (start < stop)
	let directory-end
	  = find-delimiter-from-end(string, $web-separator,
				    start: start,
				    end:   stop);
	let next-index
	  = if (directory-end)
	      directory-end + 1
	    else
	      stop
	    end;
	let (path, relative?)
	  = parse-path(string, 
		       start: start,
		       end:   next-index,
		       test:  rcurry(\==, $web-separator));
	values(path, relative?, next-index)
      else
	values(#f, #f, start)
      end
    end method parse-directory,

    method parse-base-and-extension
	(start :: <integer>, stop :: <integer>)
     => (base :: false-or(<string>), extension :: false-or(<string>),
	 next-index :: <integer>)
      if (start < stop)
	let pos
	  = find-delimiter(string, $web-extension-separator,
			   start: start,
			   end:   stop);
	values(copy-sequence(string,
			     start: start,
			     end: pos | stop),
	       if (pos)
		 copy-sequence(string,
			       start: pos + 1,
			       end: stop)
	       end,
	       stop)
      else
	values(#f, #f, stop)
      end
    end method parse-base-and-extension,

    method parse-cgi-string
	(start :: <integer>)
     => (cgi-string :: false-or(<string>), next-index :: <integer>)
      if (start < stop & string[start] == $web-cgi-separator)
	values(copy-sequence(string, start: start + 1),
	       stop)
      else
	values(#f, start)
      end
    end method parse-cgi-string,

    method parse-index
	(start :: <integer>)
     => (index :: false-or(<string>), next-index :: <integer>)
      if (start < stop & string[start] == $web-index-separator)
	values(copy-sequence(string, start: start + 1),
	       stop)
      else
	values(#f, start)
      end
    end method parse-index;

  let (class, protocol-end) = parse-protocol();
  let (host, host-end) = parse-host(protocol-end);
  let (port, port-end) = parse-port(host-end);
  let (username, password, username-end) = parse-username(port-end);
  let file-end
    = find-delimiter(string, $web-cgi-separator, start: username-end)
        | find-delimiter-from-end(string, $web-index-separator,
				  start: username-end)
        | stop;
  let (path, relative?, path-end) = parse-directory(username-end, file-end);
  let (base, extension, name-end)
    = parse-base-and-extension(path-end, file-end);
  let (cgi-string, cgi-string-end) = parse-cgi-string(file-end);
  let (index, index-end) = parse-index(cgi-string-end);
  unless (index-end == stop)
    locator-error("Unexpected suffix for URL '%s': '%s'",
		  string, copy-sequence(string, start: index-end))
  end;
  values(class, host, port, username, password, path, relative?, base, 
	 extension, cgi-string, index)
end method parse-url;

//---*** It is a pity we need this method for efficiency...
define sealed copy-down-method parse-url
    (string :: <byte-string>, #key protocol :: false-or(<string>) = #f)
 => (class :: subclass(<server-url>),
     host :: false-or(<string>),
     port :: false-or(<integer>),
     username :: false-or(<string>),
     password :: false-or(<string>),
     path :: false-or(<sequence>),
     relative? :: <boolean>,
     base :: false-or(<string>),
     extension :: false-or(<string>),
     cgi-string :: false-or(<string>),
     index :: false-or(<string>));
