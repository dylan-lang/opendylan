module:        remote-access-path
synopsis:      FFI declarations to allow access-path to call the debugger
               nub on demand
author:        Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <RNUBHANDLE>            = Rtmgr/RemoteNub/<RNUBHANDLE>;
define constant <RNUBLIBRARY>           = <RNUBHANDLE>;
define constant <RTARGET-ADDRESS>       = Rtmgr/RemoteNub/<RTARGET-ADDRESS>;
define constant <RTARGET-ADDRESS-SEQ>   = Rtmgr/RemoteNub/<RTARGET-ADDRESS-SEQ>;
define constant <STRING-SEQ>            = Rtmgr/RemoteNub/<STRING-SEQ>;
