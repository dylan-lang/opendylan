Module:       dylan-user
Synopsis:     UNIX version of the Functional Objects Network library
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library network
  use functional-dylan;
  use C-FFI;
  use IO;
  use unix-portability;
  export unix-sockets,
         sockets;
end;


define module unix-sockets
  use functional-dylan,
    exclude: { close };
  use C-FFI;
  use unix-portability;

  // Misc
  export
    <C-char**>;

  // From sys/socket.h
  export
    $AF-UNSPEC, $AF-UNIX, $AF-INET, $AF-PACKET;
  export
    $PF-UNSPEC, $PF-LOCAL, $PF-UNIX, $PF-INET, $PF-PACKET;
  export
    $SOCK-STREAM, $SOCK-DGRAM, $SOCK-SEQPACKET, $SOCK-RAW;
  export
    $SHUT-RD, $SHUT-WR, $SHUT-RDWR;
  export 
    $SOL-SOCKET;
  export
    $ETH-P-ALL;
  export
    $SO-ACCEPTCONN, $SO-BROADCAST, $SO-DEBUG, $SO-DONTROUTE, $SO-ERROR,
    $SO-KEEPALIVE, $SO-LINGER, $SO-OOBINLINE, $SO-RCVBUF, $SO-RCVLOWAT,
    $SO-RCVTIMEO, $SO-REUSEADDR, $SO-SNDBUF, $SO-SNDLOWAT, $SO-SNDTIMEO,
    $SO-TYPE;
  export
    <socklen-t>, <socklen-t*>, <sa-family-t>, <sa-family-t*>;
  export
    <sockaddr>, <sockaddr*>,
      sa-family-value, sa-family-value-setter,
      sa-data-value, sa-data-array, sa-data-array-setter;
  export
    <linger>, <linger*>,
      l-onoff-value, l-onoff-value-setter,
      l-linger-value, l-linger-value-setter;
  export
    <msghdr>, <msghdr*>,
      msg-name-value, msg-name-value-setter,
      msg-namelen-value, msg-namelen-value-setter,
      msg-iov-value, msg-iov-value-setter,
      msg-iovlen-value, msg-iovlen-value-setter,
      msg-control-value, msg-control-value-setter,
      msg-controllen-value, msg-controllen-value-setter,
      msg-flags-value, msg-flags-value-setter;
  export
    <cmsghdr>, <cmsghdr*>,
      cmsg-len-value, cmsg-len-value-setter,
      cmsg-level-value, cmsg-level-value-setter,
      cmsg-type-value, cmsg-type-value-setter;
  export
    accept, bind, close, connect, getpeername, getsockname, getsockopt, listen,
    recv, recvfrom, recvmsg, send, sendmsg, sendto, setsockopt, shutdown,
    socket, socketpair, ioctl;

  // --
  export
    unix-recv-buffer, unix-send-buffer, 
    unix-recv-buffer-from, unix-send-buffer-to;

  // --
  // From netinet/in.h
  export
    <in-port-t>, <in-port-t*>,
    <in-addr-t>, <in-addr-t*>;
  export
    $INADDR-ANY, $INADDR-BROADCAST, $INADDR-NONE;
  export
    <in-addr>, <in-addr*>, <in-addr**>,
      s-addr-value, s-addr-value-setter;
  export
    <sockaddr-in>, <sockaddr-in*>,
      sin-family-value, sin-family-value-setter,
      sin-port-value, sin-port-value-setter,
      sin-addr-value, sin-addr-value-setter;
  export
    <sockaddr-ll>, <sockaddr-ll*>,
      sll-family, sll-family-setter,
      sll-protocol, sll-protocol-setter,
      sll-ifindex, sll-ifindex-setter,
      sll-hatype, sll-hatype-setter,
      sll-pkttype, sll-pkttype-setter,
      sll-halen, sll-halen-setter,
      sll-addr, sll-addr-setter;

  // From arpa/inet.h
  export
    ntohl, ntohs, htonl, htons;
  export
    inet-addr, inet-network, inet-makeaddr, inet-lnaof, inet-netof, inet-ntoa;

  // From netdb.h
  export
    <hostent>, <hostent*>,
      h-name-value, h-name-value-setter,
      h-aliases-value, h-aliases-value-setter,
      h-addrtype-value, h-addrtype-value-setter,
      h-length-value, h-length-value-setter,
      h-addr-list-value, h-addr-list-value-setter;
  export
    <netent>, <netent*>,
      n-name-value, n-name-value-setter,
      n-aliases-value, n-aliases-value-setter,
      n-addrtype-value, n-addrtype-value-setter,
      n-net-value, n-net-value-setter;
  export
    <protoent>, <protoent*>,
      p-name-value, p-name-value-setter,
      p-aliases-value, p-aliases-value-setter,
      p-proto-value, p-proto-value-setter;
  export
    <servent>, <servent*>,
      s-name-value, s-name-value-setter,
      s-aliases-value, s-aliases-value-setter,
      s-port-value, s-port-value-setter,
      s-proto-value, s-proto-value-setter;
  export
    <ifreq>, <ifreq*>,
      ifr-name, ifr-name-setter,
      ifr-ifindex, ifr-ifindex-setter,
      ifr-flags, ifr-flags-setter,
      $IF-NAMESIZE, $SIOCGIFINDEX,
      $SIOCGIFFLAGS, $SIOCSIFFLAGS,
      $SIOCGIFNAME,
      $IFF-UP,
      $IFF-BROADCAST,
      $IFF-DEBUG,
      $IFF-LOOPBACK,
      $IFF-POINTOPOINT,
      $IFF-NOTRAILERS,
      $IFF-RUNNING,
      $IFF-NOARP,   
      $IFF-PROMISC,
      $IFF-ALLMULTI,
      $IFF-MASTER,
      $IFF-SLAVE,
      $IFF-MULTICAST,
      $IFF-PORTSEL,
      $IFF-AUTOMEDIA;

  export
    endhostent, gethostbyaddr, gethostbyname, gethostent, sethostent,
    endnetent, getnetbyaddr, getnetbyname, getnetent, setnetent,
    endprotoent, getprotobynumber, getprotobyname, getprotoent, setprotoent,
    endservent, getservbyport, getservbyname, getservent, setservent;
  export
    gethostname;
  export
    $EPERM, $ENOENT, $ESRCH, $EINTR, $EIO, $ENXIO, $E2BIG, $ENOEXEC,
    $EBADF, $ECHILD, $EAGAIN, $EWOULDBLOCK, $ENOMEM, $EACCES, $EFAULT,
    $ENOTBLK, $EBUSY, $EEXIST, $EXDEV, $ENODEV, $ENOTDIR, $EISDIR,
    $EINVAL, $ENFILE, $EMFILE, $ENOTTY, $ETXTBSY, $EFBIG, $ENOSPC,
    $ESPIPE, $EROFS, $EMLINK, $EPIPE, $EDOM, $ERANGE, $EDEADLK,
    $EDEADLOCK, $ENAMETOOLONG, $ENOLCK, $ENOSYS, $ENOTEMPTY, $ELOOP,
    $ENOMSG, $EIDRM, $ECHRNG, $EL2NSYNC, $EL3HLT, $EL3RST, $ELNRNG,
    $EUNATCH, $ENOCSI, $EL2HLT, $EBADE, $EBADR, $EXFULL, $ENOANO,
    $EBADRQC, $EBADSLT, $EBFONT, $ENOSTR, $ENODATA, $ETIME, $ENOSR,
    $ENONET, $ENOPKG, $EREMOTE, $ENOLINK, $EADV, $ESRMNT, $ECOMM,
    $EPROTO, $EMULTIHOP, $EDOTDOT, $EBADMSG, $EOVERFLOW, $ENOTUNIQ,
    $EBADFD, $EREMCHG, $ELIBACC, $ELIBBAD, $ELIBSCN, $ELIBMAX,
    $ELIBEXEC, $EILSEQ, $ERESTART, $ESTRPIPE, $EUSERS, $ENOTSOCK,
    $EDESTADDRREQ, $EMSGSIZE, $EPROTOTYPE, $ENOPROTOOPT, 
    $EPROTONOSUPPORT, $ESOCKTNOSUPPORT, $EOPNOTSUPP, $ENOTSUP, 
    $EPFNOSUPPORT, $EAFNOSUPPORT, $EADDRINUSE, $EADDRNOTAVAIL,
    $ENETDOWN, $ENETUNREACH, $ENETRESET, $ECONNABORTED, $ECONNRESET,
    $ENOBUFS, $EISCONN, $ENOTCONN, $ESHUTDOWN, $ETOOMANYREFS,
    $ETIMEDOUT, $ECONNREFUSED, $EHOSTDOWN, $EHOSTUNREACH, $EALREADY,
    $EINPROGRESS, $ESTALE, $EUCLEAN, $ENOTNAM, $ENAVAIL, $EISNAM,
    $EREMOTEIO, $EDQUOT, $ENOMEDIUM, $EMEDIUMTYPE, $ECANCELED;

  export
    poll,
    <pollfd>, <pollfd*>,
    pollfd-fd, pollfd-events, pollfd-revents,
    pollfd-fd-setter, pollfd-events-setter, pollfd-revents-setter,
    $POLLIN, $POLLPRI, $POLLOUT, $POLLERR, $POLLHUP, $POLLNVAL;
end module unix-sockets;


define module sockets
  create start-sockets;
  create
    <abstract-socket>,
       local-host, local-port, socket-descriptor, close-socket, socket-open?,
       <server-socket>,
         server-class-for-protocol, with-server-socket, start-server,
         accept, client-class-for-server,
         <TCP-server-socket>,
         <UDP-server-socket>,
      <socket>, // client socket
        client-class-for-protocol, remote-host, remote-port, with-socket,
        <buffered-socket>,
          <TCP-socket>,
          <UDP-socket>;
  create
    \interruptible-system-call;
  create
    \with-socket-thread,
    invoke-with-socket-thread,
    register-socket-thread,
    unregister-socket-thread;				
  create
    <internet-address>,
      <ipv4-address>,
        host-name, host-address, numeric-host-address, all-addresses,
        aliases, $loopback-address, $local-host;
  create
    <numeric-address>,
      network-order, host-order,
      <ipv4-numeric-address>,
        <ipv4-network-order-address>, <ipv4-host-order-address>;
  create
    <socket-condition>,
      <socket-warning>,
      <socket-error>,
        <internal-socket-error>, <sockets-not-initialized>,
      <recoverable-socket-condition>,
        <blocking-call-interrupted>, <out-of-resources>,
        <network-not-responding>, <invalid-address>, <host-not-found>,
        <server-not-responding>, <socket-closed>, <connection-failed>,
        <connection-closed>, <address-in-use>, <host-unreachable>,
        <service-not-found>,
      <socket-accessor-error>,
        explanation, calling-function;
end module sockets;

define module sockets-internals
  use functional-dylan;
  use dylan-extensions;
  use machine-words;
  use C-FFI;
  use threads;
  use dylan-direct-c-ffi;
  use streams-internals;
  use format;
  use format-out;
  use byte-vector;
  use unix-portability;
  use unix-sockets,
    rename: {socket => unix-socket,
             connect => unix-connect,
	     bind => unix-bind,
             listen => unix-listen,
	     accept => unix-accept,
	     htonl => unix-htonl, ntohl => unix-ntohl,
	     htons => unix-htons, ntohs => unix-ntohs,
	     gethostbyname => unix-gethostbyname,
	     getservbyname => unix-getservbyname,
	     gethostbyaddr => unix-gethostbyaddr,
	     getsockname => unix-getsockname,
	     getpeername => unix-getpeername,
	     gethostname => unix-gethostname,
	     <c-socket> => <unix-socket-descriptor>,
	     close => unix-closesocket },
    exclude: {<socket>, // use <accessor-socket-descriptor>
	      send,  //  use unix-send-buffer instead
	      recv};  //  use unix-recv-buffer instead

  use sockets, export: all;
  create
    <general-TCP-socket>, <byte-char-TCP-socket>, <byte-TCP-socket>;

  create
    <general-UDP-socket>, <byte-char-UDP-socket>, <byte-UDP-socket>;
end module sockets-internals;
