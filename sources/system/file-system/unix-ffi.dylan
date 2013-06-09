Module:	      system-internals
Author:       Gary Palter
Synopsis:     UNIX implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// From <sys/stat.h> ...
// define constant $S_ISUID = #o0004000;	// set user id on execution
// define constant $S_ISGID = #o0002000;	// set group id on execution
define constant    $S_IRWXU = #o0000700;	// read,write,execute perm: owner
define constant    $S_IRUSR = #o0000400;	// read permission: owner
define constant    $S_IWUSR = #o0000200;	// write permission: owner
define constant    $S_IXUSR = #o0000100;	// execute/search permission: owner
define constant    $S_IRWXG = #o0000070;	// read,write,execute perm: group
define constant    $S_IRGRP = #o0000040;	// read permission: group
define constant    $S_IWGRP = #o0000020;	// write permission: group
define constant    $S_IXGRP = #o0000010;	// execute/search permission: group
define constant    $S_IRWXO = #o0000007;	// read,write,execute perm: other
define constant    $S_IROTH = #o0000004;	// read permission: other
define constant    $S_IWOTH = #o0000002;	// write permission: other
define constant    $S_IXOTH = #o0000001;	// execute/search permission: other
define constant    $S_IFMT  = #o0170000;	// type of file mask
define constant    $S_IFDIR = #o0040000;	// directory
define constant    $S_IFLNK = #o0120000;	// symbolic link

/// From <unistd.h> ...
// define constant $F_OK = #o0;
define constant    $X_OK = #o1;
define constant    $W_OK = #o2;
define constant    $R_OK = #o4;


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

define macro with-stack-stat
  { with-stack-stat (?st:name, ?file:expression) ?:body end }
  => { with-storage (?st, $stat-size) ?body end }
end macro with-stack-stat;

define inline-only function st-mode (st :: <machine-word>) => (mode :: <integer>)
  raw-as-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw(0),
			       integer-as-raw($st-mode-offset)))
end function st-mode;

define inline-only function st-uid (st :: <machine-word>) => (uid :: <integer>)
  raw-as-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw(0),
			       integer-as-raw($st-uid-offset)))
end function st-uid;

define inline-only function st-gid (st :: <machine-word>) => (gid :: <integer>)
  raw-as-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw(0),
			       integer-as-raw($st-gid-offset)))
end function st-gid;
ignore(st-gid);

define inline-only function st-size (st :: <machine-word>) => (size :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-long-at(primitive-unwrap-machine-word(st),
			       integer-as-raw(0),
			      integer-as-raw($st-size-offset)))
end function st-size;

define inline-only function st-atime (st :: <machine-word>) => (atime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-long-at(primitive-unwrap-machine-word(st),
                              integer-as-raw(0),
	                      integer-as-raw($st-atime-offset)))
end function st-atime;

define inline-only function st-mtime (st :: <machine-word>) => (mtime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-long-at(primitive-unwrap-machine-word(st),
	                      integer-as-raw(0),
	                      integer-as-raw($st-mtime-offset)))
end function st-mtime;

define inline-only function st-ctime (st :: <machine-word>) => (ctime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-long-at(primitive-unwrap-machine-word(st),
	                      integer-as-raw(0),
	                      integer-as-raw($st-ctime-offset)))
end function st-ctime;


define inline-only function passwd-name (passwd :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw(0),
			    integer-as-raw($pw-name-offset)))
end function passwd-name;

define inline-only function passwd-dir (passwd :: <machine-word>) => (dir :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw(0),
			    integer-as-raw($pw-dir-offset)))
end function passwd-dir;


define inline-only function group-name (group :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(group),
			    integer-as-raw($gr-name-offset),
			    integer-as-raw(0)))
end function group-name;


define inline-only function dirent-name (dirent :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-cast-raw-as-pointer
       (primitive-machine-word-add(primitive-unwrap-machine-word(dirent),
                                  integer-as-raw($d-name-offset))))
end function dirent-name;


/// Error handling

define function unix-errno () => (errno :: <integer>)
  raw-as-integer
    (%call-c-function("io_errno") ()=>(result :: <raw-c-signed-int>)() end)
end function;

define function unix-last-error-message () => (message :: <string>)
  let message :: <byte-string>
    = primitive-raw-as-string
       (%call-c-function ("strerror")
	    (errno :: <raw-c-signed-int>) => (message :: <raw-byte-string>)
	  (integer-as-raw(unix-errno()))
	end);
  // Make a copy to avoid it being overwritten ...
  copy-sequence(message)
end function unix-last-error-message;

define function unix-file-error
    (operation :: <string>, additional-information, #rest additional-information-args)
 => (will-never-return :: <bottom>)
  let status-message = unix-last-error-message();
  if (additional-information)
    error(make(<file-system-error>,
	       format-string: concatenate("%s: Can't %s ", additional-information),
	       format-arguments: concatenate(list(status-message),
					     list(operation),
					     map(method (x)
						   if (instance?(x, <locator>))
						     as(<string>, x)
						   else
						     x
						   end
						 end method,
						 additional-information-args))))
  else
    error(make(<file-system-error>,
	       format-string: "%s: Can't %s",
	       format-arguments: list(status-message, operation)))
  end;
end function unix-file-error;
