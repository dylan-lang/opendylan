Module:       system-internals
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $os-variant = $os-name;
define constant $os-version = "Unknown";

define constant $command-line-option-prefix = '-';

define function command-line-option-prefix
    () => (prefix :: <character>)
  $command-line-option-prefix
end function command-line-option-prefix;

define function login-name () => (name :: false-or(<string>))
  let value = primitive-wrap-machine-word
                (primitive-cast-pointer-as-raw
		   (%call-c-function ("getlogin") () => (name :: <raw-byte-string>) () end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
					integer-as-raw(0)))
    primitive-raw-as-string
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(value)))
  else
    #f
  end
end function login-name;

define function login-group () => (group :: false-or(<string>))
  let group = primitive-wrap-machine-word
                (%call-c-function ("getgid") () => (gid :: <raw-c-unsigned-int>) () end);
  let value = primitive-wrap-machine-word
                (primitive-cast-pointer-as-raw
		   (%call-c-function ("getgrgid")
			(gid :: <raw-c-unsigned-int>) => (group :: <raw-c-pointer>)
		      (primitive-unwrap-machine-word(group))
		    end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
					integer-as-raw(0)))
    group-name(value)
  else
    #f
  end
end function login-group;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-name () => (name :: false-or(<string>))
  #f
end function owner-name;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-organization () => (organization :: false-or(<string>))
  #f
end function owner-organization;

define constant $environment-variable-delimiter = ':';

define function environment-variable
    (name :: <byte-string>) => (value :: false-or(<byte-string>))
  let value = primitive-wrap-machine-word
		(primitive-cast-pointer-as-raw
		   (%call-c-function ("getenv")
		        (name :: <raw-byte-string>) => (value :: <raw-byte-string>)
		      (primitive-string-as-raw(name))
		    end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
					integer-as-raw(0)))
    let value :: <byte-string>
      = primitive-raw-as-string
          (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(value)));
    value.size > 0
      & value
  else
    #f
  end
end function environment-variable;

define function environment-variable-setter
    (new-value :: false-or(<byte-string>), name :: <byte-string>)
 => (new-value :: false-or(<byte-string>))
  let thing = concatenate-as(<byte-string>, name, "=", new-value | "");
  //--- NOTE: The string passed to putenv must be statically allocated
  //--- as it will remain in use after this function returns to its caller.
  let static-thing = primitive-wrap-machine-word
		       (primitive-cast-pointer-as-raw
			  (%call-c-function ("GC_malloc")
			       (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
			     (integer-as-raw(size(thing) + 1))
			   end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(static-thing),
					integer-as-raw(0)))
    //--- NOTE: We can't use primitive-replace-bytes! as our 
    //--- first argument isn't a Dylan object.  (Sigh)
    %call-c-function ("memcpy")
        (dst :: <raw-c-pointer>, src :: <raw-c-pointer>, n-bytes :: <raw-c-unsigned-long>)
     => (dst :: <raw-c-pointer>)
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(static-thing)),
       primitive-string-as-raw(thing),
       integer-as-raw(size(thing)))
    end;
    //---*** Should we signal something if this call fails?
    %call-c-function ("putenv")
      (new-value :: <raw-byte-string>) => (result :: <raw-c-signed-int>)
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(static-thing)))
    end
  end;
  new-value
end function environment-variable-setter;

///---*** NOTE: Should change this to use exec so we can capture I/O, etc ...
define function run-application
    (command :: <string>, #key, #all-keys) => (status :: <integer>)
  raw-as-integer(primitive-run-application(primitive-string-as-raw(command)))
end function run-application;


///---*** NOTE: The following functions need real implementations!

define function create-application-event
    (event :: <string>) => (event-object :: <machine-word>)
  as(<machine-word>, 0)
end function create-application-event;

define constant $INFINITE_TIMEOUT = -1;

define function wait-for-application-event
    (event-object :: <machine-word>, #key timeout :: <integer> = $INFINITE_TIMEOUT)
 => (success? :: <boolean>)
  #t
end function wait-for-application-event;

define function signal-application-event
    (event :: <string>) => (success? :: <boolean>)
  #t
end function signal-application-event;

define function load-library
    (name :: <string>) => (module)
  #f
end function load-library;
