Module: inet-address-internals
Author: James Casey
Synopsis: Dylan FFI Definitions for inet-address
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// <inet-address>
define constant $AF-UNSPEC = 0;
define constant $AF-UNIX = 1;
define constant $AF-INET = 2;

//
define constant *MAXHOSTNAMELEN*
  = select ($os-name)
      #"Solaris2", #"win32" =>  256;
      #"SunOS4", #"OSF3"    =>  64;
    end;

// $INADDR-ANY = $#x00000000
define constant $ADDR-ANY = make(<byte-vector>, fill: 0, size: 4);

define function export-in-addr(value :: <byte-vector>) 
 => (result :: <sys-in-addr>)
  as(<sys-in-addr>, value);
end;

define function import-in-addr(v :: <sys-in-addr>)
 => (result :: <byte-vector>)  
  as(<byte-vector>, v);
end;

define C-mapped-subtype <sys-in-addr> (<C-unsigned-char*>)
  export-map type-union(<byte-vector>,<sys-in-addr>),
    export-function: export-in-addr;
  import-map <byte-vector>,
    import-function: import-in-addr;
  pointer-type <sys-in-addr*>;
end;

// Here we assume that size-of(this) == 4 ,which is valid for in_addr structs
define method as(class == <byte-vector>, this :: <sys-in-addr>) 
 => (retval :: <byte-vector>)
  let foo = make(<byte-vector>, size: 4, fill: 0);
  for (i from 0 to 3)
    foo[i] := pointer-value(this, index: i);
  end;
  foo
end;

define method as(class == <sys-in-addr>, this :: <byte-vector>) 
 => (retval :: <sys-in-addr>)
  let foo = make(<sys-in-addr>, element-count: 4);
  for (i from 0 to 3)
    pointer-value-setter(this[i], foo, index: i);
  end;
  foo
end;

define method as(class == <sys-in-addr>, this :: <sys-in-addr>)
 => (retval :: <sys-in-addr>)
  this
end;

define method as(class == <byte-string>, this :: <sys-in-addr>)
 => (str :: <byte-string>)
  let v = as(<byte-vector>, this);
  format-to-string("%d.%d.%d.%d", v[0], v[1], v[2], v[3])
end method;

define C-function c-local-host-name
  parameter local-name :: <C-char*>;
  parameter buffer-size :: <c-int>;
  c-name: "c_local_host_name";
end C-function c-local-host-name;

define function local-host-name()
 =>(ret :: <byte-string>)

  let bufsz :: <integer> = 64;
  let tmp :: <C-string> = make(<C-string>, element-count: bufsz);
  c-local-host-name(tmp, bufsz); 
  as(<byte-string>,tmp);
end function local-host-name;

define C-function c-get-host-addr-by-name
  input parameter hostname :: <C-string>;
  input parameter addr :: <sys-in-addr>;
  result retval :: <C-int>;
  c-name: "c_gethostaddrbyname";
end C-function c-get-host-addr-by-name;

define method get-host-addr-by-name(name :: <byte-string>)
  =>(ret-addr :: <byte-vector>);

  let tmp :: <sys-in-addr> = make(<sys-in-addr>, element-count: 4);
  let (res :: <integer>) = 
    c-get-host-addr-by-name(name, tmp);
  if(res == -1) 
    signal(make(<unknown-host-error>, host-name: name,
	format-string: "unknown host", format-arguments: #f));
  else 
    as(<byte-vector>,tmp); // XXX - Shouldn't need this
  end;
end method get-host-addr-by-name;

define C-function c-get-host-name-by-addr
  input parameter addr :: <sys-in-addr>;
  input parameter hostname :: <C-string>;
  input parameter bufsz :: <C-int>;
  result retval :: <C-int>;
  c-name: "c_gethostnamebyaddr";
end C-function c-get-host-name-by-addr;

define method get-host-name-by-addr(addr :: <byte-vector>)
  =>( ret-addr :: <byte-string>);

  let tmp = make(<C-string>, size: *MAXHOSTNAMELEN*);
  let (res :: <integer>) = c-get-host-name-by-addr(addr, tmp, *MAXHOSTNAMELEN*);
  if(res == -1) 
    signal(make(<unknown-host-error>,
		format-string: "Can't resolve to hostname; %s", 
		format-arguments: as(<byte-string>, addr)));
  else 
    as(<byte-string>,tmp);
  end;
end method get-host-name-by-addr;

/*
define C-function c-host-error-string
  result retval :: <C-char*>;
  c-name: "host_err_str";
end C-function c-host-error-string;

define function host-error-string()
  =>(ret-addr :: <byte-string>)

// XXX -- c-host-error-string is not mapping to <byte-string> automatically
  let res :: <byte-string> = as(<byte-string>, c-host-error-string());
  res;
end method host-error-string;
*/