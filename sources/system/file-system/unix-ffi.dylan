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

/// From <errno.h>
define constant $ENOENT  =  2;
define constant $EINTR   =  4;
define constant $EACCESS = 13;
define constant $EINVAL  = 22;
define constant $ETXTBSY = 26;
define constant $EROFS   = 30;


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <sys/stat.h> ...

define system-offset stat-size (x86-linux 88, ppc-linux 88, x86-freebsd 96, amd64-freebsd 120, x86-darwin 96, ppc-darwin 96, x86_64-linux 144) 72;
define system-offset st-mode (x86-linux 16, ppc-linux 16, x86-freebsd 8, amd64-freebsd 8, x86-darwin 8, ppc-darwin 8, x86_64-linux 24) 8;
//XXX: st-uid is 12 on FreeBSD-amd64, so it should be 1.5 here...
define system-offset st-uid (x86-linux 24, ppc-linux 24, x86-freebsd 12, amd64-freebsd 12, x86-darwin 12, ppc-darwin 12, x86_64-linux 28) 16;
define system-offset st-gid (x86-linux 28, ppc-linux 28, x86-freebsd 16, amd64-freebsd 16, x86-darwin 16, ppc-darwin 16, x86_64-linux 32) 20;
define system-offset st-size (x86-linux 44, ppc-linux 44, x86-freebsd 48, amd64-freebsd 72, x86-darwin 48, ppc-darwin 48, x86_64-linux 48) 28;
define system-offset st-atime (x86-linux 56, ppc-linux 56, x86-freebsd 24, amd64-freebsd 24, x86-darwin 24, ppc-darwin 24, x86_64-linux 72) 32;
define system-offset st-mtime (x86-linux 64, ppc-linux 64, x86-freebsd 32, amd64-freebsd 40, x86-darwin 32, ppc-darwin 32, x86_64-linux 88) 40;
define system-offset st-ctime (x86-linux 72, ppc-linux 72, x86-freebsd 40, amd64-freebsd 56, x86-darwin 40, ppc-darwin 40, x86_64-linux 104) 48;

define constant $STAT_SIZE = 
  $stat-size-offset;

define macro with-stack-stat
  { with-stack-stat (?st:name, ?file:expression) ?:body end }
  => { with-storage (?st, $STAT_SIZE) ?body end }
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
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
		         integer-as-raw(0),
			     integer-as-raw($st-atime-offset)))
end function st-atime;

define inline-only function st-mtime (st :: <machine-word>) => (mtime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
			     integer-as-raw(0),
			     integer-as-raw($st-mtime-offset)))
end function st-mtime;

define inline-only function st-ctime (st :: <machine-word>) => (ctime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
			     integer-as-raw(0),
			     integer-as-raw($st-ctime-offset)))
end function st-ctime;


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <pwd.h> ...

define system-offset passwd-name () 0;
define system-offset passwd-dir (alpha-linux 32, x86-freebsd 28, x86-darwin 28, ppc-darwin 28, x86_64-linux 32) 20;

define inline-only function passwd-name (passwd :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw(0),
			    integer-as-raw($passwd-name-offset)))
end function passwd-name;

define inline-only function passwd-dir (passwd :: <machine-word>) => (dir :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw(0),
			    integer-as-raw($passwd-dir-offset)))
end function passwd-dir;


/// From <grp.h> ...

define system-offset group-name () 0;

define inline-only function group-name (group :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(group),
			    integer-as-raw($group-name-offset),
			    integer-as-raw(0)))
end function group-name;


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <dirent.h> ...

define system-offset dirent-name (x86-linux 11, ppc-linux 11, x86-freebsd 8, x86-darwin 8, ppc-darwin 8, x86_64-linux 19) 8;

define inline-only function dirent-name (dirent :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-cast-raw-as-pointer
       (primitive-machine-word-add(primitive-unwrap-machine-word(dirent),
                                  integer-as-raw($dirent-name-offset))))
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
