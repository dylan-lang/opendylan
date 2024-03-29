module:    dylan-user
author:    Hannes Mehnert
copyright: Original Code is Copyright (c) 2010 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library ssl-network
  use common-dylan;
  use C-FFI;
  use IO;
  use system, import: { file-system };
  use network;
  export openssl-wrapper,
    ssl-sockets;
end;

define module openssl-wrapper
  use dylan;
  use c-ffi;

  use unix-sockets, import: { <C-buffer-offset> };

  export SSL-library-init, SSL-load-error-strings,
    ERR-load-BIO-strings, RAND-load-file;

  export SSL-new, SSL-read, SSL-write, SSL-shutdown,
    SSL-set-fd, SSL-get-fd, SSL-connect, SSL-accept,
    SSL-get-error;

  export ERR-get-error, ERR-error-string;

  export TLS-method, TLS-server-method, TLS-client-method,
    TLSv1-method, TLSv1-server-method, TLSv1-client-method,
    TLSv1-1-method, TLSv1-1-server-method, TLSv1-1-client-method,
    TLSv1-2-method, TLSv1-2-server-method, TLSv1-2-client-method;

  export <SSL-CTX>, SSL-context-new, SSL-context-free, SSL-free,
    SSL-context-use-certificate-file, SSL-context-use-private-key-file;

  export <x509>, <x509**>, X509-new;

  export $SSL-MODE-AUTO-RETRY, $SSL-FILETYPE-PEM;

  export $SSL-ERROR-NONE, $SSL-ERROR-SSL, $SSL-ERROR-WANT-READ,
    $SSL-ERROR-WANT-WRITE, $SSL-ERROR-WANT-X509-LOOKUP, $SSL-ERROR-SYSCALL,
    $SSL-ERROR-ZERO-RETURN, $SSL-ERROR-WANT-CONNECT, $SSL-ERROR-WANT-ACCEPT;

  export SSL-set-mode, PEM-read-X509,
    SSL-context-add-extra-chain-certificate, SSL-set-tlsext-host-name;
end;

define module ssl-sockets
  use common-dylan;
  use c-ffi;
  use streams-internals;
  use file-system, import: { file-exists?, file-property, <pathname> };
  use threads;
  use sockets;
  use unix-sockets, import: { errno };
  use openssl-wrapper;

  export <ssl-socket>, start-tls;

  export <ssl-failure>, <pem-file-failure>, <pem-file-not-available>,
    <pem-file-not-readable>, <error-reading-pem-file>, <x509-failure>,
    <ssl-error>, <err-error>;
end;

