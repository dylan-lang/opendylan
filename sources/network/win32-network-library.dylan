Module:       dylan-user
Synopsis:     Win32 version of the Functional Objects Network library
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library network
  use functional-dylan;
  use C-FFI;
  use IO;
  export WinSock2,
         sockets;
end;


define module WinSock2
  use functional-dylan;
  use C-FFI, export: { <C-both-unsigned-long*>, <C-char*>,
		       <C-int*>, <C-unsigned-short*>, <C-string> };
  use win32-core, export: { <HANDLE>, <HWND> };

  // Special case because no setter
  export Buffer-value;

  // from "winsock2.h":
  export $WINSOCK-VERSION;
  export <u-char>, <u-short>, <u-int>, <u-long>, $FD-SETSIZE,
	fd-count-value, fd-count-value-setter, fd-array-array,
	fd-array-array-setter, fd-array-value, <fd-set>, <LPfd-set>,
	FD-ISSET;
  export tv-sec-value, tv-sec-value-setter, tv-usec-value,
	tv-usec-value-setter, <timeval>, <LPTIMEVAL>, timerclear,
	$IOCPARM-MASK, $IOC-VOID, $IOC-OUT, $IOC-IN, $IOC-INOUT, $FIONREAD,
	$FIONBIO, $FIOASYNC, $SIOCSHIWAT, $SIOCGHIWAT, $SIOCSLOWAT,
	$SIOCGLOWAT, $SIOCATMARK, h-name-value, h-name-value-setter,
	h-aliases-value, h-aliases-value-setter, h-addrtype-value,
	h-addrtype-value-setter, h-length-value, h-length-value-setter,
	h-addr-list-value, h-addr-list-value-setter, <hostent>, <LPHOSTENT>,
	n-name-value, n-name-value-setter, n-aliases-value,
	n-aliases-value-setter, n-addrtype-value, n-addrtype-value-setter,
	n-net-value, n-net-value-setter, <netent>, <LPNETENT>, s-name-value,
	s-name-value-setter, s-aliases-value, s-aliases-value-setter,
	s-port-value, s-port-value-setter, s-proto-value,
	s-proto-value-setter, <servent>, <LPSERVENT>, p-name-value,
	p-name-value-setter, p-aliases-value, p-aliases-value-setter,
	p-proto-value, p-proto-value-setter, <protoent>, <LPPROTOENT>;
  export $IPPROTO-IP, $IPPROTO-ICMP, $IPPROTO-IGMP, $IPPROTO-GGP,
	$IPPROTO-TCP, $IPPROTO-PUP, $IPPROTO-UDP, $IPPROTO-IDP, $IPPROTO-ND,
	$IPPROTO-RAW, $IPPROTO-MAX;
  export $IPPORT-ECHO, $IPPORT-DISCARD, $IPPORT-SYSTAT,
	$IPPORT-DAYTIME, $IPPORT-NETSTAT, $IPPORT-FTP, $IPPORT-TELNET,
	$IPPORT-SMTP, $IPPORT-TIMESERVER, $IPPORT-NAMESERVER, $IPPORT-WHOIS,
	$IPPORT-MTP;
  export $IPPORT-TFTP, $IPPORT-RJE, $IPPORT-FINGER, $IPPORT-TTYLINK,
	$IPPORT-SUPDUP;
  export $IPPORT-EXECSERVER, $IPPORT-LOGINSERVER, $IPPORT-CMDSERVER,
	$IPPORT-EFSSERVER;
  export $IPPORT-BIFFUDP, $IPPORT-WHOSERVER, $IPPORT-ROUTESERVER,
	$IPPORT-RESERVED;
  export $IMPLINK-IP, $IMPLINK-LOWEXPER, $IMPLINK-HIGHEXPER;
  export <in-addr>, IN-CLASSA, $IN-CLASSA-NET, $IN-CLASSA-NSHIFT,
	$IN-CLASSA-HOST, $IN-CLASSA-MAX, IN-CLASSB, $IN-CLASSB-NET,
	$IN-CLASSB-NSHIFT, $IN-CLASSB-HOST, $IN-CLASSB-MAX, IN-CLASSC,
	$IN-CLASSC-NET, $IN-CLASSC-NSHIFT, $IN-CLASSC-HOST, IN-CLASSD,
	$IN-CLASSD-NET, $IN-CLASSD-NSHIFT, $IN-CLASSD-HOST, IN-MULTICAST,
	$INADDR-ANY, $INADDR-LOOPBACK, $INADDR-BROADCAST, $INADDR-NONE,
	$ADDR-ANY;
  export sin-family-value, sin-family-value-setter, sin-port-value,
	sin-port-value-setter, sin-addr-value, sin-addr-value-setter,
	sin-zero-array, sin-zero-array-setter, sin-zero-value, <sockaddr-in>,
	<LPSOCKADDR-IN>, $WSADESCRIPTION-LEN, $WSASYS-STATUS-LEN,
	wVersion-value, wVersion-value-setter, wHighVersion-value,
	wHighVersion-value-setter, szDescription-array,
	szDescription-array-setter, szDescription-value,
	szSystemStatus-array, szSystemStatus-array-setter,
	szSystemStatus-value, iMaxSockets-value, iMaxSockets-value-setter,
	iMaxUdpDg-value, iMaxUdpDg-value-setter, lpVendorInfo-value,
	lpVendorInfo-value-setter, <WSADATA>, <LPWSADATA>, $INVALID-SOCKET,
	$SOCKET-ERROR, $FROM-PROTOCOL-INFO;
  export $SOCK-STREAM, $SOCK-DGRAM, $SOCK-RAW, $SOCK-RDM,
	$SOCK-SEQPACKET;
  export $SO-DEBUG, $SO-ACCEPTCONN, $SO-REUSEADDR, $SO-KEEPALIVE,
	$SO-DONTROUTE, $SO-BROADCAST, $SO-USELOOPBACK, $SO-LINGER,
	$SO-OOBINLINE, $SO-DONTLINGER;
  export $SO-SNDBUF, $SO-RCVBUF, $SO-SNDLOWAT, $SO-RCVLOWAT,
	$SO-SNDTIMEO, $SO-RCVTIMEO, $SO-ERROR, $SO-TYPE;
  export $SO-GROUP-ID, $SO-GROUP-PRIORITY, $SO-MAX-MSG-SIZE,
	$SO-PROTOCOL-INFOA, $SO-PROTOCOL-INFOW, $PVD-CONFIG;
  export $TCP-NODELAY;
  export $AF-UNSPEC, $AF-UNIX, $AF-INET, $AF-IMPLINK, $AF-PUP,
	$AF-CHAOS, $AF-NS, $AF-IPX, $AF-ISO, $AF-OSI, $AF-ECMA, $AF-DATAKIT,
	$AF-CCITT, $AF-SNA, $AF-DECnet, $AF-DLI, $AF-LAT, $AF-HYLINK,
	$AF-APPLETALK, $AF-NETBIOS, $AF-VOICEVIEW, $AF-FIREFOX, $AF-UNKNOWN1,
	$AF-BAN, $AF-ATM, $AF-INET6, $AF-CLUSTER, $AF-12844, $AF-MAX,
	sa-family-value, sa-family-value-setter, sa-data-array,
	sa-data-array-setter, sa-data-value, <sockaddr>, <LPSOCKADDR>,
	sp-family-value, sp-family-value-setter, sp-protocol-value,
	sp-protocol-value-setter, <sockproto>, <LPSOCKPROTO>;
  export $PF-UNSPEC, $PF-UNIX, $PF-INET, $PF-IMPLINK, $PF-PUP,
	$PF-CHAOS, $PF-NS, $PF-IPX, $PF-ISO, $PF-OSI, $PF-ECMA, $PF-DATAKIT,
	$PF-CCITT, $PF-SNA, $PF-DECnet, $PF-DLI, $PF-LAT, $PF-HYLINK,
	$PF-APPLETALK, $PF-VOICEVIEW, $PF-FIREFOX, $PF-UNKNOWN1, $PF-BAN,
	$PF-ATM, $PF-INET6, $PF-MAX;
  export l-onoff-value, l-onoff-value-setter, l-linger-value,
	l-linger-value-setter, <linger>, <LPLINGER>;
  export $SOL-SOCKET;
  export $SOMAXCONN, $MSG-OOB, $MSG-PEEK, $MSG-DONTROUTE,
	$MSG-PARTIAL, $MSG-INTERRUPT, $MSG-MAXIOVLEN;
  export $MAXGETHOSTSTRUCT;
  export $FD-READ-BIT, $FD-READ, $FD-WRITE-BIT, $FD-WRITE,
	$FD-OOB-BIT, $FD-OOB, $FD-ACCEPT-BIT, $FD-ACCEPT, $FD-CONNECT-BIT,
	$FD-CONNECT, $FD-CLOSE-BIT, $FD-CLOSE, $FD-QOS-BIT, $FD-QOS,
	$FD-GROUP-QOS-BIT, $FD-GROUP-QOS, $FD-ROUTING-INTERFACE-CHANGE-BIT,
	$FD-ROUTING-INTERFACE-CHANGE, $FD-ADDRESS-LIST-CHANGE-BIT,
	$FD-ADDRESS-LIST-CHANGE, $FD-MAX-EVENTS, $WSABASEERR;
  export $WSAEINTR, $WSAEBADF, $WSAEACCES, $WSAEFAULT, $WSAEINVAL,
	$WSAEMFILE;
  export $WSAEWOULDBLOCK, $WSAEINPROGRESS, $WSAEALREADY, $WSAENOTSOCK,
	$WSAEDESTADDRREQ, $WSAEMSGSIZE, $WSAEPROTOTYPE, $WSAENOPROTOOPT,
	$WSAEPROTONOSUPPORT, $WSAESOCKTNOSUPPORT, $WSAEOPNOTSUPP,
	$WSAEPFNOSUPPORT, $WSAEAFNOSUPPORT, $WSAEADDRINUSE,
	$WSAEADDRNOTAVAIL, $WSAENETDOWN, $WSAENETUNREACH, $WSAENETRESET,
	$WSAECONNABORTED, $WSAECONNRESET, $WSAENOBUFS, $WSAEISCONN,
	$WSAENOTCONN, $WSAESHUTDOWN, $WSAETOOMANYREFS, $WSAETIMEDOUT,
	$WSAECONNREFUSED, $WSAELOOP, $WSAENAMETOOLONG, $WSAEHOSTDOWN,
	$WSAEHOSTUNREACH, $WSAENOTEMPTY, $WSAEPROCLIM, $WSAEUSERS,
	$WSAEDQUOT, $WSAESTALE, $WSAEREMOTE;
  export $WSASYSNOTREADY, $WSAVERNOTSUPPORTED, $WSANOTINITIALISED,
	$WSAEDISCON, $WSAENOMORE, $WSAECANCELLED, $WSAEINVALIDPROCTABLE,
	$WSAEINVALIDPROVIDER, $WSAEPROVIDERFAILEDINIT, $WSASYSCALLFAILURE,
	$WSASERVICE-NOT-FOUND, $WSATYPE-NOT-FOUND, $WSA-E-NO-MORE,
	$WSA-E-CANCELLED, $WSAEREFUSED, $WSAHOST-NOT-FOUND, $HOST-NOT-FOUND,
	$WSATRY-AGAIN, $TRY-AGAIN, $WSANO-RECOVERY, $NO-RECOVERY,
	$WSANO-DATA, $NO-DATA, $WSANO-ADDRESS, $NO-ADDRESS,
	$WSA-QOS-RECEIVERS, $WSA-QOS-SENDERS, $WSA-QOS-NO-SENDERS,
	$WSA-QOS-NO-RECEIVERS, $WSA-QOS-REQUEST-CONFIRMED,
	$WSA-QOS-ADMISSION-FAILURE, $WSA-QOS-POLICY-FAILURE,
	$WSA-QOS-BAD-STYLE, $WSA-QOS-BAD-OBJECT, $WSA-QOS-TRAFFIC-CTRL-ERROR,
	$WSA-QOS-GENERIC-ERROR;
  export <WSAEVENT>, <LPWSAEVENT>, <WSAOVERLAPPED>, <LPWSAOVERLAPPED>,
	$WSA-IO-PENDING, $WSA-IO-INCOMPLETE, $WSA-INVALID-HANDLE,
	$WSA-INVALID-PARAMETER, $WSA-NOT-ENOUGH-MEMORY,
	$WSA-OPERATION-ABORTED, $WSA-INVALID-EVENT, $WSA-MAXIMUM-WAIT-EVENTS,
	$WSA-WAIT-FAILED, $WSA-WAIT-EVENT-0, $WSA-WAIT-IO-COMPLETION,
	$WSA-WAIT-TIMEOUT, $WSA-INFINITE;
  export len-value, len-value-setter, buf-value, buf-value-setter,
	<WSABUF>, <LPWSABUF>, SendingFlowspec-value,
	SendingFlowspec-value-setter, ReceivingFlowspec-value,
	ReceivingFlowspec-value-setter, ProviderSpecific-value,
	ProviderSpecific-value-setter, <QOS>, <LPQOS>;
  export $CF-ACCEPT, $CF-REJECT, $CF-DEFER;
  export $SD-RECEIVE, $SD-SEND, $SD-BOTH;
  export <GROUP>, $SG-UNCONSTRAINED-GROUP, $SG-CONSTRAINED-GROUP;
  export lNetworkEvents-value, lNetworkEvents-value-setter,
	iErrorCode-array, iErrorCode-array-setter, iErrorCode-value,
	<WSANETWORKEVENTS>, <LPWSANETWORKEVENTS>, Data1-value,
	Data1-value-setter, Data2-value, Data2-value-setter, Data3-value,
	Data3-value-setter, Data4-array, Data4-array-setter, Data4-value,
	<GUID>, <LPGUID>, $MAX-PROTOCOL-CHAIN, $BASE-PROTOCOL,
	$LAYERED-PROTOCOL, ChainLen-value, ChainLen-value-setter,
	ChainEntries-array, ChainEntries-array-setter, ChainEntries-value,
	<WSAPROTOCOLCHAIN>, <LPWSAPROTOCOLCHAIN>, $WSAPROTOCOL-LEN,
	dwServiceFlags1-value, dwServiceFlags1-value-setter,
	dwServiceFlags2-value, dwServiceFlags2-value-setter,
	dwServiceFlags3-value, dwServiceFlags3-value-setter,
	dwServiceFlags4-value, dwServiceFlags4-value-setter,
	dwProviderFlags-value, dwProviderFlags-value-setter,
	ProviderId-value, ProviderId-value-setter, dwCatalogEntryId-value,
	dwCatalogEntryId-value-setter, ProtocolChain-value,
	ProtocolChain-value-setter, iVersion-value, iVersion-value-setter,
	iAddressFamily-value, iAddressFamily-value-setter,
	iMaxSockAddr-value, iMaxSockAddr-value-setter, iMinSockAddr-value,
	iMinSockAddr-value-setter, iSocketType-value,
	iSocketType-value-setter, iProtocol-value, iProtocol-value-setter,
	iProtocolMaxOffset-value, iProtocolMaxOffset-value-setter,
	iNetworkByteOrder-value, iNetworkByteOrder-value-setter,
	iSecurityScheme-value, iSecurityScheme-value-setter,
	dwMessageSize-value, dwMessageSize-value-setter, szProtocol-array,
	szProtocol-array-setter, szProtocol-value, <WSAPROTOCOL-INFOA>,
	<LPWSAPROTOCOL-INFOA>, dwServiceFlags1-value,
	dwServiceFlags1-value-setter, dwServiceFlags2-value,
	dwServiceFlags2-value-setter, dwServiceFlags3-value,
	dwServiceFlags3-value-setter, dwServiceFlags4-value,
	dwServiceFlags4-value-setter, dwProviderFlags-value,
	dwProviderFlags-value-setter, ProviderId-value,
	ProviderId-value-setter, dwCatalogEntryId-value,
	dwCatalogEntryId-value-setter, ProtocolChain-value,
	ProtocolChain-value-setter, iVersion-value, iVersion-value-setter,
	iAddressFamily-value, iAddressFamily-value-setter,
	iMaxSockAddr-value, iMaxSockAddr-value-setter, iMinSockAddr-value,
	iMinSockAddr-value-setter, iSocketType-value,
	iSocketType-value-setter, iProtocol-value, iProtocol-value-setter,
	iProtocolMaxOffset-value, iProtocolMaxOffset-value-setter,
	iNetworkByteOrder-value, iNetworkByteOrder-value-setter,
	iSecurityScheme-value, iSecurityScheme-value-setter,
	dwMessageSize-value, dwMessageSize-value-setter, szProtocol-array,
	szProtocol-array-setter, szProtocol-value, <WSAPROTOCOL-INFOW>,
	<LPWSAPROTOCOL-INFOW>, <WSAPROTOCOL-INFO>, <LPWSAPROTOCOL-INFO>,
	$PFL-MULTIPLE-PROTO-ENTRIES, $PFL-RECOMMENDED-PROTO-ENTRY,
	$PFL-HIDDEN, $PFL-MATCHES-PROTOCOL-ZERO, $XP1-CONNECTIONLESS,
	$XP1-GUARANTEED-DELIVERY, $XP1-GUARANTEED-ORDER,
	$XP1-MESSAGE-ORIENTED, $XP1-PSEUDO-STREAM, $XP1-GRACEFUL-CLOSE,
	$XP1-EXPEDITED-DATA, $XP1-CONNECT-DATA, $XP1-DISCONNECT-DATA,
	$XP1-SUPPORT-BROADCAST, $XP1-SUPPORT-MULTIPOINT,
	$XP1-MULTIPOINT-CONTROL-PLANE, $XP1-MULTIPOINT-DATA-PLANE,
	$XP1-QOS-SUPPORTED, $XP1-INTERRUPT, $XP1-UNI-SEND, $XP1-UNI-RECV,
	$XP1-IFS-HANDLES, $XP1-PARTIAL-MESSAGE, $BIGENDIAN, $LITTLEENDIAN,
	$SECURITY-PROTOCOL-NONE;
  export $JL-SENDER-ONLY, $JL-RECEIVER-ONLY, $JL-BOTH;
  export $WSA-FLAG-OVERLAPPED, $WSA-FLAG-MULTIPOINT-C-ROOT,
	$WSA-FLAG-MULTIPOINT-C-LEAF, $WSA-FLAG-MULTIPOINT-D-ROOT,
	$WSA-FLAG-MULTIPOINT-D-LEAF;
  export $IOC-UNIX, $IOC-WS2, $IOC-PROTOCOL, $IOC-VENDOR,
	$SIO-ASSOCIATE-HANDLE, $SIO-ENABLE-CIRCULAR-QUEUEING,
	$SIO-FIND-ROUTE, $SIO-FLUSH, $SIO-GET-BROADCAST-ADDRESS,
	$SIO-GET-EXTENSION-FUNCTION-POINTER, $SIO-GET-QOS,
	$SIO-GET-GROUP-QOS, $SIO-MULTIPOINT-LOOPBACK, $SIO-MULTICAST-SCOPE,
	$SIO-SET-QOS, $SIO-SET-GROUP-QOS, $SIO-TRANSLATE-HANDLE,
	$SIO-ROUTING-INTERFACE-QUERY, $SIO-ROUTING-INTERFACE-CHANGE,
	$SIO-ADDRESS-LIST-QUERY, $SIO-ADDRESS-LIST-CHANGE;
  export $TH-NETDEV, $TH-TAPI, cbSize-value, cbSize-value-setter,
	pBlobData-value, pBlobData-value-setter, <BLOB>, <LPBLOB>;
  export $SERVICE-MULTIPLE;
  export $NS-ALL, $NS-SAP, $NS-NDS, $NS-PEER-BROWSE, $NS-TCPIP-LOCAL,
	$NS-TCPIP-HOSTS, $NS-DNS, $NS-NETBT, $NS-WINS, $NS-NBP, $NS-MS,
	$NS-STDA, $NS-NTDS, $NS-X500, $NS-NIS, $NS-NISPLUS, $NS-WRQ,
	$RES-UNUSED-1, $RES-FLUSH-CACHE, $RES-SERVICE;
  export $SERVICE-TYPE-VALUE-IPXPORT, $SERVICE-TYPE-VALUE-SAPID,
	$SERVICE-TYPE-VALUE-TCPPORT, $SERVICE-TYPE-VALUE-UDPPORT,
	$SERVICE-TYPE-VALUE-OBJECTID;
  export lpSockaddr-value, lpSockaddr-value-setter,
	iSockaddrLength-value, iSockaddrLength-value-setter,
	<SOCKET-ADDRESS>, <LPSOCKET-ADDRESS>, <PSOCKET-ADDRESS>;
  export LocalAddr-value, LocalAddr-value-setter, RemoteAddr-value,
	RemoteAddr-value-setter, iSocketType-value, iSocketType-value-setter,
	iProtocol-value, iProtocol-value-setter, <CSADDR-INFO>,
	<LPCSADDR-INFO>, <PCSADDR-INFO>;
  export iAddressCount-value, iAddressCount-value-setter,
	Address-array, Address-array-setter, Address-value,
	<SOCKET-ADDRESS-LIST>, <LPSOCKET-ADDRESS-LIST>;
  export iAddressFamily-value, iAddressFamily-value-setter,
	iProtocol-value, iProtocol-value-setter, <AFPROTOCOLS>,
	<LPAFPROTOCOLS>, <PAFPROTOCOLS>;
  export $COMP-EQUAL, $COMP-NOTLESS;
  export dwVersion-value, dwVersion-value-setter, ecHow-value,
	ecHow-value-setter, <WSAVERSION>, <LPWSAVERSION>, <PWSAVERSION>;
  export dwSize-value, dwSize-value-setter,
	lpszServiceInstanceName-value, lpszServiceInstanceName-value-setter,
	lpServiceClassId-value, lpServiceClassId-value-setter,
	lpVersion-value, lpVersion-value-setter, lpszComment-value,
	lpszComment-value-setter, dwNameSpace-value,
	dwNameSpace-value-setter, lpNSProviderId-value,
	lpNSProviderId-value-setter, lpszContext-value,
	lpszContext-value-setter, dwNumberOfProtocols-value,
	dwNumberOfProtocols-value-setter, lpafpProtocols-value,
	lpafpProtocols-value-setter, lpszQueryString-value,
	lpszQueryString-value-setter, dwNumberOfCsAddrs-value,
	dwNumberOfCsAddrs-value-setter, lpcsaBuffer-value,
	lpcsaBuffer-value-setter, dwOutputFlags-value,
	dwOutputFlags-value-setter, lpBlob-value, lpBlob-value-setter,
	<WSAQUERYSETA>, <LPWSAQUERYSETA>, <PWSAQUERYSETA>, dwSize-value,
	dwSize-value-setter, lpszServiceInstanceName-value,
	lpszServiceInstanceName-value-setter, lpServiceClassId-value,
	lpServiceClassId-value-setter, lpVersion-value,
	lpVersion-value-setter, lpszComment-value, lpszComment-value-setter,
	dwNameSpace-value, dwNameSpace-value-setter, lpNSProviderId-value,
	lpNSProviderId-value-setter, lpszContext-value,
	lpszContext-value-setter, dwNumberOfProtocols-value,
	dwNumberOfProtocols-value-setter, lpafpProtocols-value,
	lpafpProtocols-value-setter, lpszQueryString-value,
	lpszQueryString-value-setter, dwNumberOfCsAddrs-value,
	dwNumberOfCsAddrs-value-setter, lpcsaBuffer-value,
	lpcsaBuffer-value-setter, dwOutputFlags-value,
	dwOutputFlags-value-setter, lpBlob-value, lpBlob-value-setter,
	<WSAQUERYSETW>, <LPWSAQUERYSETW>, <PWSAQUERYSETW>, <WSAQUERYSET>,
	<PWSAQUERYSET>, <LPWSAQUERYSET>, $LUP-DEEP, $LUP-CONTAINERS,
	$LUP-NOCONTAINERS, $LUP-NEAREST, $LUP-RETURN-NAME, $LUP-RETURN-TYPE,
	$LUP-RETURN-VERSION, $LUP-RETURN-COMMENT, $LUP-RETURN-ADDR,
	$LUP-RETURN-BLOB, $LUP-RETURN-ALIASES, $LUP-RETURN-QUERY-STRING,
	$LUP-RETURN-ALL, $LUP-RES-SERVICE, $LUP-FLUSHCACHE,
	$LUP-FLUSHPREVIOUS;
  export $RESULT-IS-ALIAS;
  export $RNRSERVICE-REGISTER, $RNRSERVICE-DEREGISTER,
	$RNRSERVICE-DELETE;
  export lpszName-value, lpszName-value-setter, dwNameSpace-value,
	dwNameSpace-value-setter, dwValueType-value,
	dwValueType-value-setter, dwValueSize-value,
	dwValueSize-value-setter, lpValue-value, lpValue-value-setter,
	<WSANSCLASSINFOA>, <LPWSANSCLASSINFOA>, <PWSANSCLASSINFOA>,
	lpszName-value, lpszName-value-setter, dwNameSpace-value,
	dwNameSpace-value-setter, dwValueType-value,
	dwValueType-value-setter, dwValueSize-value,
	dwValueSize-value-setter, lpValue-value, lpValue-value-setter,
	<WSANSCLASSINFOW>, <LPWSANSCLASSINFOW>, <PWSANSCLASSINFOW>,
	<WSANSCLASSINFO>, <PWSANSCLASSINFO>, <LPWSANSCLASSINFO>,
	lpServiceClassId-value, lpServiceClassId-value-setter,
	lpszServiceClassName-value, lpszServiceClassName-value-setter,
	dwCount-value, dwCount-value-setter, lpClassInfos-value,
	lpClassInfos-value-setter, <WSASERVICECLASSINFOA>,
	<LPWSASERVICECLASSINFOA>, <PWSASERVICECLASSINFOA>,
	lpServiceClassId-value, lpServiceClassId-value-setter,
	lpszServiceClassName-value, lpszServiceClassName-value-setter,
	dwCount-value, dwCount-value-setter, lpClassInfos-value,
	lpClassInfos-value-setter, <WSASERVICECLASSINFOW>,
	<LPWSASERVICECLASSINFOW>, <PWSASERVICECLASSINFOW>,
	<WSASERVICECLASSINFO>, <PWSASERVICECLASSINFO>,
	<LPWSASERVICECLASSINFO>, NSProviderId-value,
	NSProviderId-value-setter, dwNameSpace-value,
	dwNameSpace-value-setter, fActive-value, fActive-value-setter,
	dwVersion-value, dwVersion-value-setter, lpszIdentifier-value,
	lpszIdentifier-value-setter, <WSANAMESPACE-INFOA>,
	<LPWSANAMESPACE-INFOA>, <PWSANAMESPACE-INFOA>, NSProviderId-value,
	NSProviderId-value-setter, dwNameSpace-value,
	dwNameSpace-value-setter, fActive-value, fActive-value-setter,
	dwVersion-value, dwVersion-value-setter, lpszIdentifier-value,
	lpszIdentifier-value-setter, <WSANAMESPACE-INFOW>,
	<LPWSANAMESPACE-INFOW>, <PWSANAMESPACE-INFOW>, <WSANAMESPACE-INFO>,
	<PWSANAMESPACE-INFO>, <LPWSANAMESPACE-INFO>, accept, bind,
	closesocket, connect, ioctlsocket, getpeername, getsockname,
	getsockopt, htonl, htons, inet-addr, inet-ntoa, listen, ntohl, ntohs,
	recv, recvfrom, winsock-select, send, sendto, setsockopt, shutdown,
	socket, gethostbyaddr, gethostbyname, gethostname, getservbyport,
	getservbyname, getprotobynumber, getprotobyname, WSAStartup,
	WSACleanup, WSASetLastError, WSAGetLastError, WSAAsyncGetServByName,
	WSAAsyncGetServByPort, WSAAsyncGetProtoByName,
	WSAAsyncGetProtoByNumber, WSAAsyncGetHostByName,
	WSAAsyncGetHostByAddr, WSACancelAsyncRequest, WSAAsyncSelect;
  export <LPCONDITIONPROC>;
  export <LPWSAOVERLAPPED-COMPLETION-ROUTINE>, WSAAccept,
	WSACloseEvent, WSAConnect, WSACreateEvent, WSADuplicateSocket,
	WSAEnumNetworkEvents, WSAEnumProtocols, WSAEventSelect,
	WSAGetOverlappedResult, WSAGetQOSByName, WSAHtonl, WSAHtons,
	WSAIoctl, WSAJoinLeaf, WSANtohl, WSANtohs, WSARecv,
	WSARecvDisconnect, WSARecvFrom, WSAResetEvent, WSASend,
	WSASendDisconnect, WSASendTo, WSASetEvent, WSASocket,
	WSAWaitForMultipleEvents, WSAAddressToString, WSAStringToAddress;
  export WSALookupServiceBegin, WSALookupServiceNext,
	WSALookupServiceEnd, WSAInstallServiceClass, WSARemoveServiceClass,
	WSAGetServiceClassInfo, WSAEnumNameSpaceProviders,
	WSAGetServiceClassNameByClassId, WSASetService,
	WSAProviderConfigChange;
  export WSAMAKEASYNCREPLY, WSAMAKESELECTREPLY, WSAGETASYNCBUFLEN,
	WSAGETASYNCERROR, WSAGETSELECTEVENT, WSAGETSELECTERROR;

  // from "qos.h":
  export <SERVICETYPE>, $SERVICETYPE-NOTRAFFIC,
	$SERVICETYPE-BESTEFFORT, $SERVICETYPE-CONTROLLEDLOAD,
	$SERVICETYPE-GUARANTEED, $SERVICETYPE-NETWORK-UNAVAILABLE,
	$SERVICETYPE-GENERAL-INFORMATION, $SERVICETYPE-NOCHANGE,
	$SERVICE-IMMEDIATE-TRAFFIC-CONTROL, $SERVICE-NO-QOS-SIGNALING;
  export TokenRate-value, TokenRate-value-setter,
	TokenBucketSize-value, TokenBucketSize-value-setter,
	PeakBandwidth-value, PeakBandwidth-value-setter, Latency-value,
	Latency-value-setter, DelayVariation-value,
	DelayVariation-value-setter, ServiceType-value,
	ServiceType-value-setter, MaxSduSize-value, MaxSduSize-value-setter,
	MinimumPolicedSize-value, MinimumPolicedSize-value-setter,
	<FLOWSPEC>, <LPFLOWSPEC>, <PFLOWSPEC>;
  export ObjectType-value, ObjectType-value-setter,
	ObjectLength-value, ObjectLength-value-setter, <QOS-OBJECT-HDR>,
	<LPQOS-OBJECT-HDR>, $RSVP-OBJECT-ID-BASE, $QOS-GENERAL-ID-BASE,
	$QOS-NOT-SPECIFIED, $POSITIVE-INFINITY-RATE;
  export Addr-value, Addr-value-setter, AddrBytes-array,
	AddrBytes-array-setter, AddrBytes-value, <IN-ADDR-IPV4>,
	<LPIN-ADDR-IPV4>;
  export Addr-array, Addr-array-setter, Addr-value, <IN-ADDR-IPV6>,
	<LPIN-ADDR-IPV6>;
  export <LPCIN-ADDR-IPV6>;
  export Address-value, Address-value-setter, Unused-value,
	Unused-value-setter, Port-value, Port-value-setter,
	<RSVP-FILTERSPEC-V4>, <LPRSVP-FILTERSPEC-V4>;
  export Address-value, Address-value-setter, UnUsed-value,
	UnUsed-value-setter, Port-value, Port-value-setter,
	<RSVP-FILTERSPEC-V6>, <LPRSVP-FILTERSPEC-V6>;
  export Address-value, Address-value-setter, UnUsed-value,
	UnUsed-value-setter, FlowLabel-array, FlowLabel-array-setter,
	FlowLabel-value, <RSVP-FILTERSPEC-V6-FLOW>,
	<LPRSVP-FILTERSPEC-V6-FLOW>;
  export Address-value, Address-value-setter, GeneralPortId-value,
	GeneralPortId-value-setter, <RSVP-FILTERSPEC-V4-GPI>,
	<LPRSVP-FILTERSPEC-V4-GPI>;
  export Address-value, Address-value-setter, GeneralPortId-value,
	GeneralPortId-value-setter, <RSVP-FILTERSPEC-V6-GPI>,
	<LPRSVP-FILTERSPEC-V6-GPI>;
  export $FILTERSPECV4, $FILTERSPECV6, $FILTERSPECV6-FLOW,
	$FILTERSPECV4-GPI, $FILTERSPECV6-GPI, $FILTERSPEC-END;
  export Type-value, Type-value-setter, FilterSpecV4-value,
	FilterSpecV4-value-setter, FilterSpecV6-value,
	FilterSpecV6-value-setter, FilterSpecV6Flow-value,
	FilterSpecV6Flow-value-setter, FilterSpecV4Gpi-value,
	FilterSpecV4Gpi-value-setter, FilterSpecV6Gpi-value,
	FilterSpecV6Gpi-value-setter, u-value, u-value-setter,
	<RSVP-FILTERSPEC>, <LPRSVP-FILTERSPEC>, FlowSpec-value,
	FlowSpec-value-setter, NumFilters-value, NumFilters-value-setter,
	FilterList-value, FilterList-value-setter, <FLOWDESCRIPTOR>,
	<LPFLOWDESCRIPTOR>, Type-value, Type-value-setter, <RSVP-POLICY>,
	<LPRSVP-POLICY>;
  export <LPCRSVP-POLICY>;
  export ObjectHdr-value, ObjectHdr-value-setter, Style-value,
	Style-value-setter, ConfirmRequest-value,
	ConfirmRequest-value-setter, Policy-value, Policy-value-setter,
	NumFlowDesc-value, NumFlowDesc-value-setter, FlowDescList-value,
	FlowDescList-value-setter, <RSVP-RESERVE-INFO>,
	<LPRSVP-RESERVE-INFO>;
  export <LPCRSVP-RESERVE-INFO>;
  export $RSVP-WILDCARD-STYLE, $RSVP-FIXED-FILTER-STYLE,
	$RSVP-SHARED-EXPLICIT-STYLE;
  export ObjectHdr-value, ObjectHdr-value-setter, StatusCode-value,
	StatusCode-value-setter, ExtendedStatus1-value,
	ExtendedStatus1-value-setter, ExtendedStatus2-value,
	ExtendedStatus2-value-setter, <RSVP-STATUS-INFO>,
	<LPRSVP-STATUS-INFO>;
  export <LPCRSVP-STATUS-INFO>, ObjectHdr-value,
	ObjectHdr-value-setter, SendPriority-value,
	SendPriority-value-setter, SendFlags-value, SendFlags-value-setter,
	ReceivePriority-value, ReceivePriority-value-setter, Unused-value,
	Unused-value-setter, <QOS-PRIORITY>, <LPQOS-PRIORITY>;
  export ObjectHdr-value, ObjectHdr-value-setter,
	ShapeDiscardMode-value, ShapeDiscardMode-value-setter, <QOS-SD-MODE>,
	<LPQOS-SD-MODE>, $TC-NONCONF-BORROW, $TC-NONCONF-SHAPE,
	$TC-NONCONF-DISCARD;
  export ObjectHdr-value, ObjectHdr-value-setter, TrafficClass-value,
	TrafficClass-value-setter, <QOS-TRAFFIC-CLASS>,
	<LPQOS-TRAFFIC-CLASS>, IntServAwareHopCount-value,
	IntServAwareHopCount-value-setter, PathBandwidthEstimate-value,
	PathBandwidthEstimate-value-setter, MinimumLatency-value,
	MinimumLatency-value-setter, PathMTU-value, PathMTU-value-setter,
	Flags-value, Flags-value-setter, <AD-GENERAL-PARAMS>,
	<LPAD-GENERAL-PARAMS>;
  export $AD-FLAG-BREAK-BIT;
  export CTotal-value, CTotal-value-setter, DTotal-value,
	DTotal-value-setter, CSum-value, CSum-value-setter, DSum-value,
	DSum-value-setter, <AD-GUARANTEED>, <LPAD-GUARANTEED>,
	ParameterId-value, ParameterId-value-setter, Length-value,
	Length-value-setter, Buffer-array, Buffer-array-setter,
	<PARAM-BUFFER>, <LPPARAM-BUFFER>;
  export Length-value, Length-value-setter, Service-value,
	Service-value-setter, Overrides-value, Overrides-value-setter,
	Guaranteed-value, Guaranteed-value-setter, ParamBuffer-array,
	ParamBuffer-array-setter, ParamBuffer-value, u-value, u-value-setter,
	<CONTROL-SERVICE>, <LPCONTROL-SERVICE>, ObjectHdr-value,
	ObjectHdr-value-setter, GeneralParams-value,
	GeneralParams-value-setter, NumberOfServices-value,
	NumberOfServices-value-setter, Services-array, Services-array-setter,
	Services-value, <RSVP-ADSPEC>, <LPRSVP-ADSPEC>;

  // from "mswsock.h":
  export $SO-CONNDATA, $SO-CONNOPT, $SO-DISCDATA, $SO-DISCOPT,
	$SO-CONNDATALEN, $SO-CONNOPTLEN, $SO-DISCDATALEN, $SO-DISCOPTLEN;
  export $SO-OPENTYPE, $SO-SYNCHRONOUS-ALERT,
	$SO-SYNCHRONOUS-NONALERT;
  export $SO-MAXDG, $SO-MAXPATHDG, $SO-UPDATE-ACCEPT-CONTEXT,
	$SO-CONNECT-TIME;
  export $TCP-BSDURGENT;
  export WSARecvEx;
  export Head-value, Head-value-setter, HeadLength-value,
	HeadLength-value-setter, Tail-value, Tail-value-setter,
	TailLength-value, TailLength-value-setter, <TRANSMIT-FILE-BUFFERS>,
	<LPTRANSMIT-FILE-BUFFERS>, <PTRANSMIT-FILE-BUFFERS>, $TF-DISCONNECT,
	$TF-REUSE-SOCKET, $TF-WRITE-BEHIND, TransmitFile, AcceptEx,
	GetAcceptExSockaddrs;
  export <LPFN-TRANSMITFILE>, <LPFN-ACCEPTEX>,
	<LPFN-GETACCEPTEXSOCKADDRS>;

  // defined in "hand.dylan":
  export <C-SOCKET>, <SOCKET>;
  export FD-CLR, FD-SET, FD-ZERO, timerisset, timercmp;
  export <in-addr**>, <in-addr*>, win32-send-buffer, win32-recv-buffer,
    win32-send-buffer-to, win32-recv-buffer-from,
    <C-buffer-offset>, <C-char**>, <sockaddr**>;
end module WinSock2;


define module sockets
  create start-sockets;
  create
    <abstract-socket>,
       local-host, local-port, socket-descriptor, close-socket, socket-open?,
       <server-socket>,
         server-class-for-protocol, with-server-socket, start-server,
         accept, client-class-for-server, type-for-socket,
         <TCP-server-socket>,
         <UDP-server-socket>,
      <socket>, // client socket
        client-class-for-protocol, remote-host, remote-port, with-socket,
        <buffered-socket>,
          <TCP-socket>,
          <UDP-socket>;
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
  create ssl-socket-class, ssl-server-socket-class;
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
  use WinSock2,                          // from "winsock2.h":
    rename: {socket => win32-socket,
             connect => win32-connect, 
	     bind => win32-bind,
             listen => win32-listen,
	     accept => win32-accept,
	     htonl => win32-htonl, ntohl => win32-ntohl,
	     htons => win32-htons, ntohs => win32-ntohs,
	     gethostbyname => win32-gethostbyname, 
	     getservbyname => win32-getservbyname, 
	     gethostbyaddr => win32-gethostbyaddr,
	     getsockname => win32-getsockname,
	     getpeername => win32-getpeername,
	     gethostname => win32-gethostname, 
	     <c-socket> => <win32-socket-descriptor>,
	     closesocket => win32-closesocket},
    exclude: {<socket>,                  // use <accessor-socket-descriptor>
	      send,                      //  use win32-send-buffer instead
	      recv};                     //  use win32-recv-buffer instead

  use sockets, export: all;
  create 
    <general-TCP-socket>, <byte-char-TCP-socket>, <byte-TCP-socket>;

  create
    <general-UDP-socket>, <byte-char-UDP-socket>, <byte-UDP-socket>;
end module sockets-internals;
