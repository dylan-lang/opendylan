module:    ssl-sockets
synopsis:  ssl support for sockets
author:    Hannes Mehnert
copyright: Original Code is Copyright (c) 2010 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

/*
This code is experimental. It still misses some features:
 * Certificate verification (revocation lists, etc.)
 * certificates shielded with a user password, provide callback method for password
 * currently only supports tcp sockets

Proper usage for ssl sockets are shown in '../ssl-echo-server|client'
as well as STARTTLS extension is shown in '../examples/ssl-smtp-server|client'
Please be aware that ssl-smtp-server is only a test for ssl-smtp-client

CAUTION: ssl-echo-server and ssl-smtp-server depend on a "certificate.pem" and
 "key.pem" in the current working directory. In order to try them, go to
 'examples/ssl-echo-server' and run '~/Open-Dylan/bin/ssl-echo|smtp-server

This code was tested with openssl 0.9.7l on MacOSX 10.5.8
*/

//define function init-ssl ()
begin
  SSL-library-init();
  SSL-load-error-strings();
  ERR-load-BIO-strings();
  //OpenSSL-add-all-algorithms();
  //XXX: hardcoded random device. this is probably bad
  RAND-load-file("/dev/urandom", 2048);
end;

define constant $nullp = null-pointer(<C-void*>);

define class <ssl-failure> (<socket-error>)
end;

define class <pem-file-failure> (<ssl-failure>)
end;

define class <pem-file-not-available> (<pem-file-failure>)
end;

define class <pem-file-not-readable> (<pem-file-failure>)
end;

define class <error-reading-pem-file> (<pem-file-failure>)
end;

define class <x509-failure> (<ssl-failure>)
end;

define function read-pem (filename :: <string>) => (result :: <x509>)
  unless (file-exists?(filename))
    signal(make(<pem-file-not-available>))
  end;
  unless (file-property(as(<pathname>, filename), #"readable?"))
    signal(make(<pem-file-not-readable>))
  end;
  let x = X509-new(); //need to manually free the X509 struct?
  if (null-pointer?(x))
    let e = ERR-error();
    signal(make(<x509-failure>, format-string: "%s", format-arguments: e))
  else
    let ret = PEM-read-X509(filename, C-pointer-at(<x509**>, x), $nullp, $nullp);
    if (null-pointer?(ret))
      signal(make(<x509-failure>))
    else
      ret
    end
  end
end;

define abstract class <ssl-socket> (<TCP-socket>)
  constant slot underlying-socket :: <socket>, init-keyword: lower:;
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

define class <ssl-error> (<ssl-failure>)
end;

define class <err-error> (<ssl-failure>)
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
    let ctx = SSL-context-new(con());
    if (null-pointer?(ctx))
      ERR-error();
    end;
    sock.ssl-context := ctx;
    let ssl = SSL-new(sock.ssl-context);
    if (null-pointer?(ssl))
      ERR-error();
    end;
    sock.accessor.socket-descriptor := ssl;
    let r = SSL-set-fd(ssl, lower.accessor.socket-descriptor);
    if (r ~= 1)
      ssl-error(ssl, r);
    end;
    SSL-set-mode(ssl, $SSL-MODE-AUTO-RETRY);
    let ret = SSL-connect(sock.accessor.socket-descriptor); //does the handshake
    if (ret == 0)
      close(lower);
      ssl-error(ssl, ret);
    elseif (ret < 0)
      close(lower);
      ssl-error(ssl, ret);
    end
  end
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
  let con = select (ssl-method)
              #"SSLv23" => SSLv23-server-method;
              #"SSLv2" => SSLv2-server-method;
              #"TLSv1" => TLSv1-server-method;
	    end;
  let ctx = SSL-context-new(con());
  if (null-pointer?(ctx))
    ERR-error();
  end;
  s.ssl-context := ctx;
  unless (file-exists?(certificate))
    signal(make(<pem-file-not-available>))
  end;
  unless (file-property(as(<pathname>, certificate), #"readable?"))
    signal(make(<pem-file-not-readable>))
  end;
  let r = SSL-context-use-certificate-file(s.ssl-context, certificate, $SSL-FILETYPE-PEM);
  if (r ~= 1)
    ERR-error();
  end;
  unless (file-exists?(key))
    signal(make(<pem-file-not-available>))
  end;
  unless (file-property(as(<pathname>, key), #"readable?"))
    signal(make(<pem-file-not-readable>))
  end;
  let r = SSL-context-use-private-key-file(s.ssl-context, key, $SSL-FILETYPE-PEM);
  if (r ~= 1)
    ERR-error();
  end;
  if (certificate-chain)
    let cas = if (instance?(certificate-chain, <string>))
		list(certificate-chain)
	      else
		certificate-chain
	      end;
    let rs = map(curry(SSL-context-add-extra-chain-certificate, s.ssl-context),
		 map(read-pem, cas));
    if (any?(curry(\~=, 1), rs))
      ERR-error();
    end
  end;
  s.socket-descriptor := s.underlying-socket.socket-descriptor;
end;

define method accept 
    (server-socket :: <ssl-server-socket>, #rest args, #key element-type = #f, #all-keys)
 => (connected-socket :: <socket>);
  let manager = current-socket-manager();
  let lower-socket = server-socket.underlying-socket;
  let descriptor = accessor-accept(lower-socket);
  let result =
    with-lock (socket-manager-lock(manager))
      let lower = apply(make,
			client-class-for-server(lower-socket), 
			descriptor: descriptor,
			element-type: element-type | lower-socket.default-element-type,
			args);
      if (server-socket.starttls?)
	lower
      else
	block()
	  start-tls(server-socket, lower)
	exception (s :: type-union(<err-error>, <ssl-error>))
	  close(lower);
	  #f;
	end;
      end;
    end with-lock;
  result | apply(accept, server-socket, args)
end method;

define method start-tls (server-socket :: <ssl-server-socket>, client :: <tcp-socket>)
 => (ssl-socket :: false-or(<ssl-socket>))
  let ssl = SSL-new(server-socket.ssl-context);
  if (null-pointer?(ssl))
    ERR-error();
  end;
  let r = SSL-set-fd(ssl, client.socket-descriptor);
  if (r ~= 1)
    SSL-error(ssl, r);
  end;
  SSL-set-mode(ssl, $SSL-MODE-AUTO-RETRY);
  let s = SSL-accept(ssl);
  if (s == 0)
    SSL-error(ssl, s);
  elseif (s < 0)
    SSL-error(ssl, s);
  end;
  let acc = make(<unix-ssl-socket-accessor>);
  acc.socket-descriptor := ssl;
  make(client-class-for-server(server-socket),
       lower: client,
       accessor: acc,
       descriptor: ssl,
       element-type: server-socket.underlying-socket.default-element-type,
       acc?: #t)
end;

define function ERR-error (#key prefix = "") => ()
  let eerr = ERR-get-error();
  let mess = copy-sequence(as(<byte-string>, ERR-error-string(eerr, $nullp)));
  signal(make(<err-error>, format-string: "%s %s", format-arguments: list(prefix, mess)));
end;

define function ssl-error (ssl, r) => ()
  let err = SSL-get-error(ssl, r);
  if (err == $SSL-ERROR-SSL)
    ERR-error(prefix: "received ssl error");
  elseif (err == $SSL-ERROR-SYSCALL)
    signal(make(<ssl-error>, format-string: "received syscall error %d while calling openssl", format-arguments: errno()));
  end;
  signal(make(<ssl-error>, format-string: "%d %s",
	      format-arguments: list(err,
	      select (err)
		$SSL-ERROR-NONE => "no error";
		$SSL-ERROR-SSL => "SSL error";
		$SSL-ERROR-WANT-READ => "WANT READ";
		$SSL-ERROR-WANT-WRITE => "WANT WRITE";
		$SSL-ERROR-WANT-X509-LOOKUP => "WANT X509 LOOKUP";
		$SSL-ERROR-SYSCALL => "SYSCALL error";
		$SSL-ERROR-ZERO-RETURN => "ZERO RETURN";
		$SSL-ERROR-WANT-CONNECT => "WANT CONNECT";
		$SSL-ERROR-WANT-ACCEPT => "WANT ACCEPT";
		otherwise => "unknown error";
	      end)));
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
  let r = interruptible-system-call
    (SSL-read(accessor.socket-descriptor,
	      buffer-offset(the-buffer, offset),
	      count));
  if (r < 0)
    SSL-error(accessor.socket-descriptor, r);
  else
    r
  end
end;

define method accessor-write-from
    (accessor :: <unix-ssl-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer?) => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer = buffer | stream-output-buffer(stream);
  let nwritten = interruptible-system-call
    (SSL-write(accessor.socket-descriptor, buffer-offset(buffer, offset), count));
  if (nwritten < 0)
    SSL-error(accessor.socket-descriptor, nwritten);
  else
    values(nwritten, buffer)
  end
end;

define method accessor-close
    (accessor :: <unix-ssl-socket-accessor>, #key abort?, wait?) 
 => (closed? :: <boolean>)
  let ssl* = accessor.socket-descriptor;
  let s = SSL-shutdown(ssl*);
  /* according to documentation, shutdown only half-done,
     and SSL-shutdown should be called again.
     but, calling ssl-shutdown again sends data; and when
     the other side has quit (eg openssl s_client with ctrl+c)
     this results in bus errors... */
//  if (s == 0) 
//    let s = SSL-shutdown(ssl*);
//    if (s ~= 1)
//      SSL-error(ssl*, s);
//    end
//  elseif (s < 0)
  if (s < 0)
    SSL-error(ssl*, s);
  end;
  let real-fd = SSL-get-fd(ssl*);
  if (real-fd == -1)
    SSL-error(ssl*, real-fd);
  end;
  SSL-free(ssl*);
  accessor.socket-descriptor := #f;
  accessor-close-socket(real-fd);
  #t
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

define sideways method ssl-socket-class (class == <TCP-socket>)
 => (ssl-class == <ssl-socket>)
  <ssl-socket>
end;

define sideways method ssl-server-socket-class (class == <TCP-server-socket>)
 => (ssl-server-class == <ssl-server-socket>)
  <ssl-server-socket>
end;
