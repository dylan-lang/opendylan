module: dylan-user

define library ssl-network
  use functional-dylan;
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

  export SSLv2-method, SSLv2-server-method, SSLv2-client-method,
    SSLv3-method, SSLv3-server-method, SSLv3-client-method,
    SSLv23-method, SSLv23-server-method, SSLv23-client-method,
    TLSv1-method, TLSv1-server-method, TLSv1-client-method;

  export <SSL-CTX>, SSL-context-new, SSL-context-free, SSL-free,
    SSL-context-use-certificate-file, SSL-context-use-private-key-file;

  export <x509>, <x509**>, X509-new;

  export $SSL-MODE-AUTO-RETRY, $SSL-FILETYPE-PEM;

  export $SSL-ERROR-NONE, $SSL-ERROR-SSL, $SSL-ERROR-WANT-READ,
    $SSL-ERROR-WANT-WRITE, $SSL-ERROR-WANT-X509-LOOKUP, $SSL-ERROR-SYSCALL,
    $SSL-ERROR-ZERO-RETURN, $SSL-ERROR-WANT-CONNECT, $SSL-ERROR-WANT-ACCEPT;

  export SSL-set-mode, PEM-read-X509, SSL-context-add-extra-chain-certificate;
end;

define module ssl-sockets
  use functional-dylan;
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

