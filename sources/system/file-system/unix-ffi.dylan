Module:	      system-internals
Author:       Gary Palter
Synopsis:     UNIX implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <sys/stat.h> ...

define system-offset stat-size (alpha-linux 10, x86-linux 22, ppc-linux 22, x86-freebsd 24) 18;
define system-offset st-mode (x86-linux 4, ppc-linux 4, x86-freebsd 2) 2;
define system-offset st-uid (x86-linux 6, ppc-linux 6, x86-freebsd 3) 4;
define system-offset st-gid (x86-linux 7, ppc-linux 7, x86-freebsd 4) 5;
define system-offset st-size (alpha-linux 4, x86-linux 11, ppc-linux 11, x86-freebsd 12) 7;
define system-offset st-atime (alpha-linux 10, x86-linux 14, ppc-linux 14, x86-freebsd 6) 8;
define system-offset st-mtime (alpha-linux 12, x86-linux 16, ppc-linux 16, x86-freebsd 8) 10;
define system-offset st-ctime (alpha-linux 14, x86-linux 18, ppc-linux 18, x86-freebsd 10) 12;

define constant $STAT_SIZE = 
  $stat-size-offset * raw-as-integer(primitive-word-size());

define macro with-stack-stat
  { with-stack-stat (?st:name, ?file:expression) ?:body end }
  => { begin
         let ?st = primitive-wrap-machine-word(integer-as-raw(0));
         block ()
	   ?st := primitive-wrap-machine-word
                    (primitive-cast-pointer-as-raw
		       (%call-c-function ("GC_malloc")
                            (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
                          (integer-as-raw($STAT_SIZE))
                        end));
	   if (primitive-machine-word-equal?(primitive-unwrap-machine-word(?st),
					     integer-as-raw(0)))
	     unix-file-error("get space for STAT structure for", "%s", ?file)
	   end;
           ?body
         cleanup
	   if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(?st),
						 integer-as-raw(0)))
	     %call-c-function ("GC_free") (p :: <raw-c-pointer>) => (void :: <raw-c-void>)
	       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?st)))
	     end;
	     #f
	   end
         end
       end }
end macro with-stack-stat;

define inline-only function st-mode (st :: <machine-word>) => (mode :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw($st-mode-offset),
			       integer-as-raw(0)))
end function st-mode;

define inline-only function st-uid (st :: <machine-word>) => (uid :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw($st-uid-offset),
			       integer-as-raw(0)))
end function st-uid;

define inline-only function st-gid (st :: <machine-word>) => (gid :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-unsigned-int-at(primitive-unwrap-machine-word(st),
			       integer-as-raw($st-gid-offset),
			       integer-as-raw(0)))
end function st-gid;
ignore(st-gid);

define inline-only function st-size (st :: <machine-word>) => (size :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-long-at(primitive-unwrap-machine-word(st),
			      integer-as-raw($st-size-offset),
			      integer-as-raw(0)))
end function st-size;

define inline-only function st-atime (st :: <machine-word>) => (atime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
			     integer-as-raw($st-atime-offset),
			     integer-as-raw(0)))
end function st-atime;

define inline-only function st-mtime (st :: <machine-word>) => (mtime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
			     integer-as-raw($st-mtime-offset),
			     integer-as-raw(0)))
end function st-mtime;

define inline-only function st-ctime (st :: <machine-word>) => (ctime :: <abstract-integer>)
  raw-as-abstract-integer
  (primitive-c-signed-int-at(primitive-unwrap-machine-word(st),
			     integer-as-raw($st-ctime-offset),
			     integer-as-raw(0)))
end function st-ctime;


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <pwd.h> ...

define system-offset passwd-name () 0;
define system-offset passwd-dir (alpha-linux 4, x86-freebsd 28) 5;

define inline-only function passwd-name (passwd :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw($passwd-name-offset),
			    integer-as-raw(0)))
end function passwd-name;

define inline-only function passwd-dir (passwd :: <machine-word>) => (dir :: <byte-string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(passwd),
			    integer-as-raw($passwd-dir-offset),
			    integer-as-raw(0)))
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

define system-offset dirent-name (x86-linux 11, ppc-linux 11, x86-freebsd 8) 8;

define inline-only function dirent-name (dirent :: <machine-word>) => (name :: <byte-string>)
  primitive-raw-as-string
    (primitive-cast-raw-as-pointer
       (primitive-machine-word-add(primitive-unwrap-machine-word(dirent),
				   integer-as-raw($dirent-name-offset))))
end function dirent-name;


/// Error handling

define function unix-last-error () => (errno :: <integer>)
  raw-as-integer(unix-errno())
end function unix-last-error;

define function unix-last-error-setter (errno :: <integer>) => (errno :: <integer>)
  unix-errno() := integer-as-raw(errno);
  errno
end function unix-last-error-setter;

define function unix-last-error-message () => (message :: <string>)
  let message :: <byte-string>
    = primitive-raw-as-string
       (%call-c-function ("strerror")
	    (errno :: <raw-c-signed-int>) => (message :: <raw-byte-string>)
	  (integer-as-raw(unix-last-error()))
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


/*

  File attributes on x86-Linux

zab.functionalobjects.com:/u/ldisk/nosa/libc/include/sys/stat.h

struct stat {
	dev_t		st_dev;

#ifdef __SVR4_I386_ABI_L1__
	long st_pad1[3];
#else
	unsigned short __pad1;
#endif

	ino_t		st_ino;
	umode_t		st_mode;
	nlink_t		st_nlink;
	uid_t		st_uid;
	gid_t		st_gid;
	dev_t		st_rdev;

#ifdef __SVR4_I386_ABI_L1__
	long st_pad2[2];
#else
	unsigned short __pad2;
#endif

	off_t		st_size;

#ifdef __SVR4_I386_ABI_L1__
	timestruc_t	st_atim;
	timestruc_t	st_mtim;
	timestruc_t	st_ctim;
    	long		st_blksize;
    	long		st_blocks;

#define	FSTYPSZ		16

        char            st_fstype[FSTYPSZ];
        long		st_pad4[8];

#define st_atime	st_atim.tv_sec
#define st_mtime	st_mtim.tv_sec
#define st_ctime	st_ctim.tv_sec

#else /*! __SVR4_I386_ABI_L1__*/
	unsigned long	st_blksize;
	unsigned long	st_blocks;
	time_t		st_atime;
	unsigned long	__unused1;
	time_t		st_mtime;
	unsigned long	__unused2;
	time_t		st_ctime;
	unsigned long	__unused3;
	unsigned long	__unused4;
	unsigned long	__unused5;
#endif /*! __SVR4_I386_ABI_L1__*/
};

(gdb) p *(struct stat *)0x8065e10
$3 = {st_dev = 2054, __pad1 = 0, st_ino = 139286, st_mode = 33277, 
  st_nlink = 1, st_uid = 681, st_gid = 100, st_rdev = 0, __pad2 = 0, 
  st_size = 11276, st_blksize = 4096, st_blocks = 24, st_atime = 930182170, 
  __unused1 = 0, st_mtime = 930182170, __unused2 = 0, st_ctime = 930182170, 
  __unused3 = 0, __unused4 = 0, __unused5 = 0}

(gdb) x /32 0x8065e10
0x8065e10:	0x00000806	0x00000000	0x00000000	0x00022016
0x8065e20:	0x000081fd	0x00000001	0x000002a9	0x00000064
0x8065e30:	0x00000000	0x00000000	0x00000000	0x00002c0c
0x8065e40:	0x00001000	0x00000018	0x3771741a	0x00000000
0x8065e50:	0x3771741a	0x00000000	0x3771741a	0x00000000
0x8065e60:	0x00000000	0x00000000	0x4040e250	0x403b7cb4
0x8065e70:	0x40386614	0x40386614	0x40386614	0x403669f8
0x8065e80:	0x40322374	0x402e2844	0x402e2844	0x402e2844
(gdb)


*/

