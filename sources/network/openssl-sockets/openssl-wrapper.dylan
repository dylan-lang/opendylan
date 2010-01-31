module: sockets-internals
synopsis: ssl support for sockets
author: Hannes Mehnert, hannes@mehnert.org
copyright: BSD, GNU LGPL

/*
This code is experimental. It still misses some features:
 * Certificate verification (revocation lists, etc.)
 * error messages/error reporting
 * currently only supports tcp-sockets

Proper usage for ssl sockets are shown in '../ssl-echo-server|client'
as well as STARTTLS extension is shown in '../examples/ssl-smtp-server|client'
Please be aware that ssl-smtp-server is only a test for ssl-smtp-client

This code was tested with openssl 0.9.7l on MacOSX 10.5.8
*/

//define function init-ssl ()
begin
  SSL-library-init();
  SSL-load-error-strings();
  ERR-load-BIO-strings();
  //OpenSSL-add-all-algorithms();
  RAND-load-file("/dev/urandom", 2048);
end;

define constant $nullp = null-pointer(<C-void*>);

define function read-pem (filename :: <string>) => (result :: <x509>)
  //if filename exists and is readable!
  let x = X509-new(); //need to manually free the X509 struct?
  if (null-pointer?(x))
    //error in allocation, this is bad.
  else
    PEM-read-X509(filename, C-pointer-at(<x509**>, x), $nullp, $nullp);
  end;
end;

define abstract class <ssl-socket> (<TCP-socket>)
  slot underlying-socket :: <socket>, init-keyword: lower:;
  slot ssl-context :: <SSL-CTX>;
end;

define method make (class == <ssl-socket>, #rest initargs,
                    #key element-type = <byte-character>)
 => (stream :: <ssl-socket>)
  apply(make, client-class-for-element-type(class, element-type), initargs)
end;

define method local-port (s :: <ssl-socket>) => (port :: <integer>)
  s.underlying-socket.local-port
end;

define method local-host (s :: <ssl-socket>) => (host :: <internet-address>)
  s.underlying-socket.local-host
end;

define method remote-host (s :: <ssl-socket>) => (host :: <internet-address>)
  s.underlying-socket.remote-host
end;

define method remote-port (s :: <ssl-socket>) => (port :: <integer>)
  s.underlying-socket.remote-port
end;

define method client-class-for-element-type
    (class == <ssl-socket>, element-type == <byte>) => (class == <byte-ssl-socket>)
  <byte-ssl-socket>
end;

define method client-class-for-element-type
    (class == <ssl-socket>, element-type == <byte-character>) => (class == <byte-char-ssl-socket>)
  <byte-char-ssl-socket>
end;

define method client-class-for-element-type
    (class == <ssl-socket>, element-type :: <type>) => (class == <general-ssl-socket>)
  <general-ssl-socket>
end;

define class <general-ssl-socket> (<ssl-socket>, <general-typed-stream>)
  inherited slot stream-element-type = <character>;
end;

define class <byte-char-ssl-socket> (<ssl-socket>, <general-typed-stream>)
  inherited slot stream-element-type = <byte-character>;
end;

define class <byte-ssl-socket> (<ssl-socket>, <general-typed-stream>)
  inherited slot stream-element-type = <byte>;
end;

define method initialize (sock :: <ssl-socket>, #rest rest, #key lower, requested-buffer-size, acc?, ssl-method = #"SSLv23", #all-keys)
 => ()
  let keys = list(#"port", lower.remote-port,
                  #"host", lower.remote-host,
                  #"direction", lower.stream-direction);
  apply(next-method, sock, keys);
  unless (acc?) //not a server socket via accept
    //already setup a connection! do SSL handshake over this connection
    let con = select (ssl-method)
                #"SSLv23" => SSLv23-client-method;
                #"SSLv2" => SSLv2-client-method;
		#"TLSv1" => TLSv1-client-method;
	      end;
    sock.ssl-context := SSL-context-new(con());
    let ssl = SSL-new(sock.ssl-context);
    sock.accessor.socket-descriptor := ssl;
    SSL-set-fd(ssl, lower.accessor.socket-descriptor);
    let ret = SSL-connect(sock.accessor.socket-descriptor); //does the handshake
  end;
end;

define class <ssl-server-socket> (<TCP-server-socket>)
  constant slot underlying-socket :: <server-socket>, init-keyword: lower:;
  slot ssl-context :: <SSL-CTX>;
  constant slot starttls? :: <boolean> = #f, init-keyword: starttls?:;
end;

define class <unix-ssl-socket-accessor> (<unix-socket-accessor>)
end;

define method initialize (s :: <ssl-server-socket>, #rest rest,
			  #key ssl-method = #"SSLv23", certificate, key, certificate-chain, #all-keys)
 => ()
  let con = //if (s.starttls?)
	    //  TLSv1-server-method;
	    //else 
	      select (ssl-method)
		#"SSLv23" => SSLv23-server-method;
		#"SSLv2" => SSLv2-server-method;
		#"TLSv1" => TLSv1-server-method;
	      end;
	    //end;
  s.ssl-context := SSL-context-new(con());
  //next-method();
  SSL-context-use-certificate-file(s.ssl-context, certificate, $SSL-FILETYPE-PEM);
  SSL-context-use-private-key-file(s.ssl-context, key, $SSL-FILETYPE-PEM);
  if (certificate-chain)
    let cas = if (instance?(certificate-chain, <string>))
		list(certificate-chain)
	      else
		certificate-chain
	      end;
    map(curry(SSL-context-add-extra-chain-certificate, s.ssl-context),
        map(read-pem, cas));
  end;
  s.socket-descriptor := s.underlying-socket.socket-descriptor;
end;

define method accept 
    (server-socket :: <ssl-server-socket>, #rest args, #key element-type = #f, #all-keys)
 => (connected-socket :: <socket>);
  let manager = current-socket-manager();
  let lower-socket = server-socket.underlying-socket;
  let descriptor = accessor-accept(lower-socket);
  with-lock (socket-manager-lock(manager))
    let lower = apply(make,
		      client-class-for-server(lower-socket), 
		      descriptor: descriptor,
		      element-type: element-type | lower-socket.default-element-type,
		      args);
    if (server-socket.starttls?)
      lower
    else
      start-tls(server-socket, lower)
    end;
  end with-lock;
end method;

define method start-tls (server-socket :: <ssl-server-socket>, client :: <tcp-socket>)
 => (ssl-socket :: <ssl-socket>)
  let ssl = SSL-new(server-socket.ssl-context);
  SSL-set-fd(ssl, client.socket-descriptor);
  let s = SSL-accept(ssl);
//  if (s < 1)
//    let err = SSL-get-error(ssl, s);
//    let eerr = ERR-get-error();
//    let c-s = make(<C-string>, size: 200);
//    let str = ERR-error-string(eerr, c-s);
//    error("couldn't accept (%d) %d k %d s %s", s, err, eerr, as(<byte-string>, c-s))
//  end;
  let acc = make(<unix-ssl-socket-accessor>);
  acc.socket-descriptor := ssl;
  make(client-class-for-server(server-socket),
       lower: client,
       accessor: acc,
       descriptor: ssl,
       element-type: server-socket.underlying-socket.default-element-type,
       acc?: #t)
end;

define method close
    (the-socket :: <ssl-server-socket>,
     #rest keys, 
     #key abort? = #f, wait? = #t, synchronize? = #f,
     already-unregistered? = #f) => ()
  SSL-context-free(the-socket.ssl-context);
  close(the-socket.underlying-socket);
  the-socket.socket-descriptor := #f;
end;

define method accessor-read-into!
    (accessor :: <unix-ssl-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  let the-buffer = buffer | stream-input-buffer(stream);
  interruptible-system-call
    (SSL-read(accessor.socket-descriptor,
	      buffer-offset(the-buffer, offset),
	      count));
end;

define method accessor-write-from
    (accessor :: <unix-ssl-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer?) => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer = buffer | stream-output-buffer(stream);
  let nwritten = interruptible-system-call
    (SSL-write(accessor.socket-descriptor, buffer-offset(buffer, offset), count));
  values(nwritten, buffer)
end;

define method accessor-close
    (accessor :: <unix-ssl-socket-accessor>, #key abort?, wait?) 
 => (closed? :: <boolean>)
  let ssl* = accessor.socket-descriptor;
  SSL-shutdown(ssl*); //if retval == 0 and we want to be sure, call it again (wait for shutdown from other side)
  let real-fd = SSL-get-fd(ssl*);
  SSL-free(ssl*);
  accessor-close-socket(real-fd);
  accessor.socket-descriptor := #f;
end;

define method accessor-open
    (accessor :: <unix-ssl-socket-accessor>, locator, #rest rest,
     #key remote-host, remote-port, descriptor, no-delay?, direction, if-exists, if-does-not-exist,
     #all-keys) => ()
  if (descriptor)
    //this is a connected socket returned by accept
    accessor.socket-descriptor := descriptor;
  end;
end;

define method client-class-for-server (server :: <ssl-server-socket>) => (res == <ssl-socket>)
  <ssl-socket>
end;

define method type-for-socket (s :: <ssl-socket>) => (res == #"SSL")
  #"SSL"
end;

define sideways method platform-accessor-class
    (type == #"SSL", locator)
 => (class == <unix-ssl-socket-accessor>)
  ignore(locator);
  <unix-ssl-socket-accessor>
end method platform-accessor-class;
