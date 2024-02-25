Module: unix-sockets
Synopsis: FreeBSD specific constants

define constant <sa-family-t> = <C-unsigned-char>;
define constant <sa-family-t*> = <C-unsigned-char*>;

define C-struct <sockaddr>
  slot sa-len :: <C-unsigned-char>;
  slot sa-family-value :: <sa-family-t>;
  array slot sa-data-array :: <C-char>, length: 14,
    address-getter: sa-data-value;
  pointer-type-name: <sockaddr*>;
end C-struct;

ignore(sa-len, sa-len-setter);

define constant $SOL-SOCKET    = #xFFFF;

define constant $SO-ACCEPTCONN = #x2;
define constant $SO-BROADCAST  = #x20;
define constant $SO-DEBUG      = #x1;
define constant $SO-DONTROUTE  = #x10;
define constant $SO-KEEPALIVE  = #x8;
define constant $SO-LINGER     = #x80;
define constant $SO-OOBINLINE  = #x100;
define constant $SO-REUSEADDR  = #x4;

define constant $SO-RCVBUF     = #x1002;
define constant $SO-RCVLOWAT   = #x1004;
define constant $SO-RCVTIMEO   = #x1006;
define constant $SO-SNDBUF     = #x1001;
define constant $SO-SNDLOWAT   = #x1003;
define constant $SO-SNDTIMEO   = #x1005;
define constant $SO-TYPE       = #x1008;
define constant $SO-ERROR      = #x1007;

define constant $TCP-NODELAY   =  1;
define constant $IPPROTO-TCP   =  6;
