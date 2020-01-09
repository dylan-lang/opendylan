module: unix-sockets

define inline-only C-function errno
  result val :: <C-int>;
  c-name: "io_errno";
end C-function;

define inline-only C-function h-errno
  result val :: <C-int>;
  c-name: "network_h_errno";
end C-function;

// The following definitions are (essentially) common between FreeBSD,
// DragonFly BSD, NetBSD, OpenBSD, and Darwin

define constant $EPERM           = 1;  /* Operation not permitted */
define constant $ENOENT          = 2;  /* No such file or directory */
define constant $ESRCH           = 3;  /* No such process */
define constant $EINTR           = 4;  /* Interrupted system call */
define constant $EIO             = 5;  /* Input/output error */
define constant $ENXIO           = 6;  /* Device not configured */
define constant $E2BIG           = 7;  /* Argument list too long */
define constant $ENOEXEC         = 8;  /* Exec format error */
define constant $EBADF           = 9;  /* Bad file descriptor */
define constant $ECHILD          = 10;  /* No child processes */
define constant $EDEADLK         = 11;  /* Resource deadlock avoided */
define constant $ENOMEM          = 12;  /* Cannot allocate memory */
define constant $EACCES          = 13;  /* Permission denied */
define constant $EFAULT          = 14;  /* Bad address */
define constant $ENOTBLK         = 15;  /* Block device required */
define constant $EBUSY           = 16;  /* Device busy */
define constant $EEXIST          = 17;  /* File exists */
define constant $EXDEV           = 18;  /* Cross-device link */
define constant $ENODEV          = 19;  /* Operation not supported by device */
define constant $ENOTDIR         = 20;  /* Not a directory */
define constant $EISDIR          = 21;  /* Is a directory */
define constant $EINVAL          = 22;  /* Invalid argument */
define constant $ENFILE          = 23;  /* Too many open files in system */
define constant $EMFILE          = 24;  /* Too many open files */
define constant $ENOTTY          = 25;  /* Inappropriate ioctl for device */
define constant $EFBIG           = 27;  /* File too large */
define constant $ENOSPC          = 28;  /* No space left on device */
define constant $ESPIPE          = 29;  /* Illegal seek */
define constant $EROFS           = 30;  /* Read-only filesystem */
define constant $EMLINK          = 31;  /* Too many links */
define constant $EPIPE           = 32;  /* Broken pipe */
define constant $EDOM            = 33;  /* Numerical argument out of domain */
define constant $ERANGE          = 34;  /* Result too large */
define constant $EAGAIN          = 35;  /* Resource temporarily unavailable */
define constant $EWOULDBLOCK	 = $EAGAIN; /* Operation would block */
define constant $EINPROGRESS     = 36;  /* Operation now in progress */
define constant $EALREADY        = 37;  /* Operation already in progress */
define constant $ENOTSOCK        = 38;  /* Socket operation on non-socket */
define constant $EDESTADDRREQ    = 39;  /* Destination address required */
define constant $EMSGSIZE        = 40;  /* Message too long */
define constant $EPROTOTYPE      = 41;  /* Protocol wrong type for socket */
define constant $ENOPROTOOPT     = 42;  /* Protocol not available */
define constant $EPROTONOSUPPORT = 43;  /* Protocol not supported */
define constant $ESOCKTNOSUPPORT = 44;  /* Socket type not supported */
define constant $EOPNOTSUPP      = 45;  /* Operation not supported */
define constant $ENOTSUP         = $EOPNOTSUPP;	/* Operation not supported */
define constant $EPFNOSUPPORT    = 46;  /* Protocol family not supported */
define constant $EAFNOSUPPORT    = 47;  /* Address family not supported by protocol family */
define constant $EADDRINUSE      = 48;  /* Address already in use */
define constant $EADDRNOTAVAIL   = 49;  /* Can't assign requested address */
define constant $ENETDOWN        = 50;  /* Network is down */
define constant $ENETUNREACH     = 51;  /* Network is unreachable */
define constant $ENETRESET       = 52;  /* Network dropped connection on reset */
define constant $ECONNABORTED    = 53;  /* Software caused connection abort */
define constant $ECONNRESET      = 54;  /* Connection reset by peer */
define constant $ENOBUFS         = 55;  /* No buffer space available */
define constant $EISCONN         = 56;  /* Socket is already connected */
define constant $ENOTCONN        = 57;  /* Socket is not connected */
define constant $ESHUTDOWN       = 58;  /* Can't send after socket shutdown */
define constant $ETOOMANYREFS    = 59;  /* Too many references: can't splice */
define constant $ETIMEDOUT       = 60;  /* Operation timed out */
define constant $ECONNREFUSED    = 61;  /* Connection refused */
define constant $ELOOP           = 62;  /* Too many levels of symbolic links */
define constant $ENAMETOOLONG    = 63;  /* File name too long */
define constant $EHOSTDOWN       = 64;  /* Host is down */
define constant $EHOSTUNREACH    = 65;  /* No route to host */
define constant $ENOTEMPTY       = 66;  /* Directory not empty */
define constant $EPROCLIM        = 67;  /* Too many processes */
define constant $EUSERS          = 68;  /* Too many users */
define constant $EDQUOT          = 69;  /* Disc quota exceeded */
define constant $ESTALE          = 70;  /* Stale NFS file handle */
define constant $EREMOTE         = 71;  /* Too many levels of remote in path */
define constant $EBADRPC         = 72;  /* RPC struct is bad */
define constant $ERPCMISMATCH    = 73;  /* RPC version wrong */
define constant $EPROGUNAVAIL    = 74;  /* RPC prog. not avail */
define constant $EPROGMISMATCH   = 75;  /* Program version wrong */
define constant $EPROCUNAVAIL    = 76;  /* Bad procedure for program */
define constant $ENOLCK          = 77;  /* No locks available */
define constant $ENOSYS          = 78;  /* Function not implemented */
define constant $EFTYPE          = 79;  /* Inappropriate file type or format */
define constant $EAUTH           = 80;  /* Authentication error */
define constant $ENEEDAUTH       = 81;  /* Need authenticator */

// Definitions after this point differ between BSDs
