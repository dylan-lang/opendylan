Module: url-internals
Author: James Casey
Synopsis: URLs as defined in draft-fielding-url-syntax-03
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is a superclass for all URL classes which comply to the common scheme
// syntax, Section 4.3
define  class <generic-url> (<url>)
  virtual slot url-server :: false-or(<byte-string>), init-value: #f;

  slot url-path :: false-or(<byte-string>), init-value: #f;
  slot url-query :: false-or(<byte-string>), init-value: #f;
  slot url-fragment :: false-or(<byte-string>), init-value: #f;

  // If compute-server-slot? is true, we need to recompute url-server
  // before they returning it
  slot server-cache-valid? :: <boolean>, init-value: #t;
  slot %url-server-cache :: false-or(<byte-string>), init-value: #f;
  // These are the bits of url-server. Stored unquoted. The master copy
  // of this data is stored here
  //
  // We define virtual slots for them, and a slot cache for each of them
  slot %url-host :: false-or(<byte-string>), init-value: #f;
  slot %url-port :: false-or(<integer>), init-value: #f;
  slot %url-user :: false-or(<byte-string>), init-value: #f;
  slot %url-password :: false-or(<byte-string>), init-value: #f;
  
  virtual slot url-host :: false-or(<byte-string>);
  virtual slot url-port :: false-or(<integer>);
  virtual slot url-user :: false-or(<byte-string>);
  virtual slot url-password :: false-or(<byte-string>);
end class <generic-url>; 


define method parse-url-scheme-specific(this :: <generic-url>, 
					spec :: <byte-string>)
 =>()
  let pos = 0;
  let url :: <byte-string> = copy-sequence(spec); 
  // Do not destroy spec
  
  // Extract fragment
  if(pos := subsequence-position(url,"#"))
    this.url-fragment := copy-sequence(url, start: pos + 1);
    if(pos = 0)
      url := "";
    else
      url := copy-sequence(url, end: pos );
    end;
  end;
  
  if (url.size > 2 & 
	element(url, 0) = '/' & element(url, 1) = '/') // has server
    url := copy-sequence(url, start: 2);
    if(pos := subsequence-position(url,"/"))
      this.url-server := copy-sequence(url, end: pos);
      url := copy-sequence(url, start: pos);
    else
      this.url-server := copy-sequence(url);
      url := "";
    end;
  end;

  if (pos := subsequence-position(url,"?")) // We have a query 
    this.url-query := copy-sequence(url, start: pos + 1);
    if (pos = 0)
      url := "";
    else
      url := copy-sequence(url, end: pos);
    end;
  end;
  this.url-path := copy-sequence(url);
end method parse-url-scheme-specific;

// slot access method for components of server segment
// If server is not defined, all of these are #f
//
// For each slot foo, we store a cached version of url-foo in %url-foo. 
//
define method url-server(this :: <generic-url>)
 =>(retval :: false-or(<byte-string>))
  if (this.server-cache-valid?)
    %url-server-cache(this);
  else
    %url-server-cache(this) := url-server-from-components(this);  
  end
end method url-server;

define method url-server-setter(v , // :: false-or(<byte-string>), 
				this :: <generic-url>)
 => (retval :: false-or(<byte-string>))
  if(v)
    update-server-components(this, v);
  end;
  v
end method url-host-setter;

define method url-host(this :: <generic-url>)
 =>(retval :: false-or(<byte-string>))
  %url-host(this)
end method url-host;

define method url-host-setter(v, //  :: false-or(<byte-string>), 
			      this :: <generic-url>)
 => (retval ::false-or(<byte-string>))
  %url-host-setter(v,this);
  this.server-cache-valid? := #f;
  v
end method url-host-setter;

define method url-user(this :: <generic-url>)
 =>(retval :: false-or(<byte-string>))
  %url-user(this)
end method url-user;

define method url-user-setter(v, // :: false-or(<integer>), 
			      this :: <generic-url>)
 => (retval :: false-or(<integer>))
  %url-user-setter(v,this);
  this.server-cache-valid? := #f;
  v
end method url-user-setter;

define method url-password(this :: <generic-url>)
 =>(retval :: false-or(<byte-string>))
  %url-password(this)
end method url-password;

define method url-password-setter(v, // :: false-or(<integer>), 
				  this :: <generic-url>)
 => (retval :: false-or(<integer>))
  %url-password-setter(v,this);
  this.server-cache-valid? := #f;
  v
end method url-password-setter;

define method url-port(this :: <generic-url>)
 =>(retval :: false-or(<integer>))
  if (%url-port(this))
    %url-port(this)
  else
    url-default-port(this)
  end
end method url-port;

define method url-port-setter(v, //  :: false-or(<integer>), 
			      this :: <generic-url>)
 => (retval :: false-or(<integer>))
  %url-port-setter(v,this);
  this.server-cache-valid? := #f;
  v
end method url-port-setter;

//
// Returns an escaped string representation from the components of the 
// server segment
define function url-server-from-components(this :: <generic-url>)
 =>(retval :: false-or(<byte-string>))
  
  block()
    if (~ this.%url-user & ~ this.%url-password 
	  & ~ this.%url-host & ~ this.%url-port)
      #f;
    else
      let server :: <byte-string> = "";
      if(this.%url-user)
	server := concatenate(server, this.%url-user);
	if(this.%url-password)
	  server := concatenate(server, ":", this.%url-password);
	end;
	server := concatenate(server, "@");
      end;
      
      if (this.%url-host)
	server := concatenate(server, this.%url-host);
	if(this.%url-port)
	  server := concatenate(server,":", format-to-string("%d",this.%url-port));
	end if;
      end if;
      server;
    end
      afterwards
    this.server-cache-valid? := #t;
  end block; 
end function url-server-from-components;

//
// This calculates the values for the server slots
//
define method update-server-components(this :: <generic-url>, 
				       value :: false-or(<byte-string>))
  =>()
  if(value)
    let server = copy-sequence(value);
    let pos = #f;

    if(pos := subsequence-position(server,"@"))
      let userpass = copy-sequence(server, end: pos);
      server := copy-sequence(server, start: pos + 1);
      if(pos := subsequence-position(userpass,":"))
	this.%url-user := copy-sequence(userpass,end: pos);
	this.%url-password := copy-sequence(userpass, start: pos + 1);
      else
	this.%url-user := copy-sequence(userpass);
      end
    end;
    // server now contains hostport
    if(pos := subsequence-position(server,":"))
      this.%url-host := copy-sequence(server, end: pos);
      let portstr = copy-sequence(server, start: pos + 1);
      if((this.%url-port := url-port-str-to-int(portstr)) = #f)
	// Bad number
	signal(make(<malformed-url-error>,
		    format-string: "Bad port %s",
		    format-agruments: portstr));
      end
    else
      this.%url-host := copy-sequence(server);
    end
  end;
  this.server-cache-valid? := #f;
end method update-server-components;

define constant <char>  = limited(<integer>, min: 0, max: 255);
define constant char-classes = make(<vector>, size: 256, fill: #f);

for (i from as(<char>, '0') below (as(<char>, '9') + 1))
  char-classes[i] := #"digit";
end;
//
// Modified from parse-integer in print module
//
define function url-port-str-to-int(input :: <byte-string>)
 =>( port :: false-or(<integer>))
  let result :: <integer> = 0;
  for (i :: <char> = 0 then (i + 1),
       len :: <integer> = input.size then len,
       ascii-zero :: <char> = as(<char>, '0') then ascii-zero,
       until: ((i = len) |
		 (~ (char-classes[as(<char>, input[i])] == #"digit"))))
    result := ((result * 10) + (as(<char>, input[i]) - ascii-zero));
  finally
    if (result = 0 | i ~= len)
      #f;
    else
      result
    end;
  end;
end function url-port-str-to-int;

// Uses algorithm in sec 7.5, num 7
define method url-string(this :: <generic-url>)
 =>(retval :: <byte-string>)
  
  let str :: <byte-string> = "";
  if(this.url-scheme)
    str := concatenate(str, this.url-scheme, ":");
  end;
  if(this.url-server)
    str := concatenate(str, "//", this.url-server);
  end;
  str := concatenate(str,this.url-path);
  
  if(this.url-query)
    str := concatenate(str,"?",this.url-query);
  end;

  if(this.url-fragment)
    str := concatenate(str,"#",this.url-fragment);
  end;
  str
end method url-string;

define method as(class == <byte-string>, 
		 this :: <generic-url>)
 =>(ret :: <byte-string>)
  url-string(this)
end;

