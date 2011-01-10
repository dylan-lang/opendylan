Library:      network
Author:       Andy Armstrong
Synopsis:     Win32 version of the Functional Objects Network library
Executable:   DxNETWRK
Base-Address: 0x65140000
Library-Pack: Network
RC-Files:     version.rc
Target-Type:  dll
Major-Version: 2
Minor-Version: 1
Files:  win32-network-library
        winsock2/first
	winsock2/qos
	winsock2/winsock2
	winsock2/mswsock
	winsock2/hand
	sockets/WSA-error-codes
	sockets/socket-conditions
	sockets/win32-socket-accessor
	sockets/internet-address
	sockets/abstract-sockets
	sockets/client-sockets
	sockets/TCP-sockets
	sockets/server-sockets
	sockets/win32-TCP-socket-accessor
	sockets/UDP-sockets
	sockets/win32-UDP-sockets
C-Libraries:  WS2_32.LIB
              mswsock.lib
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

