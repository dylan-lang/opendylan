module:    openssl-wrapper
synopsis:  ssl support for sockets
author:    Hannes Mehnert
copyright: Original Code is Copyright (c) 2010 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define C-function SSL-library-init
  result success :: <C-int>;
  c-name: "my_SSL_library_init"
end;

define C-function SSL-load-error-strings
  result res :: <C-void*>;
  c-name: "my_SSL_load_error_strings"
end;

define C-function ERR-load-BIO-strings
  result res :: <C-void*>;
  c-name: "ERR_load_BIO_strings"
end;

define C-function RAND-load-file
  input parameter filename :: <C-string>;
  input parameter maximal-bytes :: <C-int>;
  result read-bytes :: <C-int>;
  c-name: "RAND_load_file"
end;

define C-function SSL-new
  input parameter context :: <SSL-CTX>;
  result ssl :: <ssl*>;
  c-name: "SSL_new"
end;

define C-function SSL-read
  input parameter ssl :: <ssl*>;
  parameter data :: <C-buffer-offset>;
  input parameter length :: <C-int>;
  result read-bytes :: <C-int>;
  c-name: "SSL_read"
end;

define C-function SSL-write
  input parameter ssl :: <ssl*>;
  input parameter data :: <C-buffer-offset>;
  input parameter length :: <C-int>;
  result written-bytes :: <C-int>;
  c-name: "SSL_write"
end;

define C-function SSL-shutdown
  input parameter ssl :: <ssl*>;
  result res :: <C-int>;
  c-name: "SSL_shutdown"
end;

define C-function SSL-set-fd
  input parameter ssl :: <ssl*>;
  input parameter socket :: <C-int>;
  result res :: <C-int>;
  c-name: "SSL_set_fd"
end;

define C-function SSL-get-fd
  input parameter ssl :: <ssl*>;
  result res :: <C-int>;
  c-name: "SSL_get_fd"
end;

define C-function SSL-connect
  input parameter ssl :: <ssl*>;
  result res :: <C-int>;
  c-name: "SSL_connect"
end;

define C-function SSL-accept
  input parameter ssl :: <ssl*>;
  result res :: <C-int>;
  c-name: "SSL_accept"
end;

define C-function SSL-get-error
  input parameter ssl :: <ssl*>;
  input parameter ret :: <C-int>;
  result res :: <C-int>;
  c-name: "SSL_get_error"
end;

define C-function ERR-get-error
  result errcode :: <C-unsigned-long>;
  c-name: "ERR_get_error"
end;

define C-function ERR-error-string
  input parameter errcode :: <C-unsigned-long>;
  input parameter buffer :: <C-void*>;
  result res :: <C-string>;
  c-name: "ERR_error_string"
end;

//hope that I can treat this as opaque
define constant <SSL-METHOD> = <C-void*>;

define C-function TLS-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLS_method"
end;

define C-function TLS-server-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLS_server_method"
end;

define C-function TLS-client-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLS_client_method"
end;

define C-function TLSv1-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_method"
end;

define C-function TLSv1-server-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_server_method"
end;

define C-function TLSv1-client-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_client_method"
end;

define C-function TLSv1-1-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_1_method"
end;

define C-function TLSv1-1-server-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_1_server_method"
end;

define C-function TLSv1-1-client-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_1_client_method"
end;

define C-function TLSv1-2-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_2_method"
end;

define C-function TLSv1-2-server-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_2_server_method"
end;

define C-function TLSv1-2-client-method
  result ssl-method :: <SSL-METHOD>;
  c-name: "TLSv1_2_client_method"
end;

//opaque!?
define constant <SSL-CTX> = <C-void*>;

define C-function SSL-context-new
  input parameter ssl-method :: <SSL-METHOD>;
  result ssl-context :: <SSL-CTX>;
  c-name: "SSL_CTX_new"
end;

define C-function SSL-context-free
  input parameter ssl-context :: <SSL-CTX>;
  result res :: <C-void*>;
  c-name: "SSL_CTX_free"
end;

define C-function SSL-free
  input parameter ssl :: <ssl*>;
  result res :: <C-void*>;
  c-name: "SSL_free"
end;

define constant <ssl*> = <C-void*>;

define C-function SSL-context-use-certificate-file
  input parameter context :: <SSL-CTX>;
  input parameter filename :: <C-string>;
  input parameter type :: <C-int>;
  result res :: <C-int>;
  c-name: "SSL_CTX_use_certificate_file"
end;

define C-function SSL-context-use-private-key-file
  input parameter context :: <SSL-CTX>;
  input parameter filename :: <C-string>;
  input parameter type :: <C-int>;
  result res :: <C-int>;
  c-name: "SSL_CTX_use_PrivateKey_file"
end;

define C-function X509-new
  result x509 :: <x509>;
  c-name: "X509_new"
end;

//some constants
define constant $SSL-MODE-AUTO-RETRY = 4;
define constant $SSL-FILETYPE-PEM = 1;

define constant $SSL-ERROR-NONE = 0;
define constant $SSL-ERROR-SSL = 1;
define constant $SSL-ERROR-WANT-READ = 2;
define constant $SSL-ERROR-WANT-WRITE = 3;
define constant $SSL-ERROR-WANT-X509-LOOKUP = 4;
define constant $SSL-ERROR-SYSCALL = 5; /* look at error stack/return value/errno */
define constant $SSL-ERROR-ZERO-RETURN = 6;
define constant $SSL-ERROR-WANT-CONNECT = 7;
define constant $SSL-ERROR-WANT-ACCEPT = 8;

//these are macros or other stuff defined in support.c
define C-function SSL-set-mode
  input parameter ssl :: <ssl*>;
  input parameter operation :: <C-long>;
  result res :: <C-long>;
  c-name: "my_SSL_set_mode"
end;

define constant <x509> = <C-void*>;

define C-pointer-type <x509**> => <x509>;

define C-function PEM-read-X509
  input parameter file :: <C-string>;
  input parameter x :: <x509**>;
  input parameter password-callback :: <C-void*>; //actually pem_password_cb*
  input parameter u :: <C-void*>;
  result x509 :: <x509>;
  c-name: "my_PEM_read_X509"
end;

define C-function SSL-context-add-extra-chain-certificate
  input parameter context :: <SSL-CTX>;
  input parameter x509 :: <x509>;
  result res :: <C-long>;
  c-name: "my_SSL_CTX_add_extra_chain_cert"
end;

define C-function SSL-set-tlsext-host-name
  input parameter context :: <SSL-CTX>;
  input parameter name :: <C-string>;
  result res :: <C-int>;
  c-name: "my_SSL_set_tlsext_host_name"
end;
