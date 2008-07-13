Module: unix-sockets
Synopsis: Linux specific constants

//linux only
define constant $PF-PACKET = 17;
define constant $AF-PACKET =    $PF-PACKET;

define constant $ETH-P-ALL = 3;

define constant $SOL-SOCKET = 1;

define constant $SO-ACCEPTCONN = ash(1, 16);
define constant $SO-BROADCAST = 6;
define constant $SO-DEBUG =	1;
define constant $SO-DONTROUTE =	5;
define constant $SO-ERROR =	4;
define constant $SO-KEEPALIVE =	9;
define constant $SO-LINGER =	13;
define constant $SO-OOBINLINE =	10;
define constant $SO-RCVBUF =	8;
define constant $SO-RCVLOWAT =	18;
define constant $SO-RCVTIMEO =	20;
define constant $SO-REUSEADDR =	2;
define constant $SO-SNDBUF =	7;
define constant $SO-SNDLOWAT =	19;
define constant $SO-SNDTIMEO =	21;
define constant $SO-TYPE =	3;

define constant $TCP-NODELAY =  1;
define constant $IPPROTO-TCP =  6;
