module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

define library netscape-plugin
  use dylan;
  use c-ffi;
  use idvm-application;
  use win32-common;
  use win32-user;
  export netscape-plugin;
  export netscape-plugin-interface;
  export netscape-plugin-shell-interface;
  export netscape-plugin-implementation;

end;

define module netscape-plugin-interface
  // these are the things provided by netscape
  /*
  create
    NPN-DestroyStream,
    NPN-GetJavaEnv,
    NPN-GetJavaPeer,
    NPN-GetUrl,
    NPN-MemAlloc,
    NPN-MemFree,
    NPN-NewStream,
    NPN-Post-Url,
    NPN-RequestRead,
    NPN-Status,
    NPN-UserAgent,
    NPN-Version,
    NPN-Write,
    $NP-EMBED,
    $NP-FULL,
    $NP-NORMAL,
    $NP-SEEK,
    $NP-ASFILE,
    $NP-ASFILEONLY,
    $NPERR-NO-ERROR,
    $NPERR-GENERIC-ERROR,
    $NPERR-INVALID-INSTANCE-ERROR,
    $NPERR-INVALID-FUNCTABLE-ERROR,
    $NPERR-MODULE-LOAD-FAILED-ERROR,
    $NPERR-OUT-OF-MEMORY-ERROR,
    $NPERR-INVALID-PLUGIN-ERROR,
    $NPERR-INVALID-PLUGIN-DIR-ERROR,
    $NPERR-INCOMPATIBLE-VERSION-ERROR,
    $NPERR-INVALID-PARAM,
    $NPERR-INVALID-URL,
    $NPERR-FILE-NOT-FOUND,
    $NPERR-NO-DATA,
    $NPERR-STREAM-NOT-SEEKABLE,
    $NPRES-NETWORK-ERR,
    $NPRES-USER-BREAK,
    $NPRES-DONE,
    <plugin-instance>,
    <NPMimeType*>,
    <NPWindow>,
    <NPWindow*>,
    window-handle,
    npwin-x,
    npwin-y,
    npwin-width,
    npwin-height,
    npwin-cliprect,
    <NPSavedData>,
    <NPSavedData*>,
    SavedData-len,
    SavedData-buf,
    SavedData-len-setter,
    SavedData-buf-setter,
    <NPStream>,
    <NPStream*>,
    NPStream-pdata,
    NPStream-ndata,
    NPStream-url,
    NPStream-end,
    NPStream-lastmodified,
    <NPP>,
    <NPPrint>,
    <NPPrint*>,
    npprint-mode,
    npprint-print,
    <C-void*>,
    plugin-window
    
    ;
  */
end module netscape-plugin-interface;

define module netscape-plugin-shell-interface
  // these are the things the plugin shell code must provide
  /*
  create
    NPP-New,
    NPP-Destroy,
    NPP-SetWindow,
    NPP-NewStream,
    NPP-WriteReady,
    NPP-Write,
    NPP-DestroyStream,
    NPP-StreamAsFile,
    NPP-Print
    ;
  */
end;


  

// this is where all the code for the infrastucture lives
define module netscape-plugin-implementation
  use c-ffi;
  use dylan;
  use idvm-application;
  use win32-common;
  use win32-user;

  export
    NPN-DestroyStream,
    NPN-GetJavaEnv,
    NPN-GetJavaPeer,
    NPN-GetUrl,
    NPN-MemAlloc,
    NPN-MemFree,
    NPN-NewStream,
    NPN-Post-Url,
    NPN-RequestRead,
    NPN-Status,
    NPN-UserAgent,
    NPN-Version,
    NPN-Write,
    $NP-EMBED,
    $NP-FULL,
    $NP-NORMAL,
    $NP-SEEK,
    $NP-ASFILE,
    $NP-ASFILEONLY,
    $NPERR-NO-ERROR,
    $NPERR-GENERIC-ERROR,
    $NPERR-INVALID-INSTANCE-ERROR,
    $NPERR-INVALID-FUNCTABLE-ERROR,
    $NPERR-MODULE-LOAD-FAILED-ERROR,
    $NPERR-OUT-OF-MEMORY-ERROR,
    $NPERR-INVALID-PLUGIN-ERROR,
    $NPERR-INVALID-PLUGIN-DIR-ERROR,
    $NPERR-INCOMPATIBLE-VERSION-ERROR,
    $NPERR-INVALID-PARAM,
    $NPERR-INVALID-URL,
    $NPERR-FILE-NOT-FOUND,
    $NPERR-NO-DATA,
    $NPERR-STREAM-NOT-SEEKABLE,
    $NPRES-NETWORK-ERR,
    $NPRES-USER-BREAK,
    $NPRES-DONE,
    <plugin-instance>,
    <NPMimeType*>,
    <NPWindow>,
    <NPWindow*>,
    window-handle,
    npwin-x,
    npwin-y,
    npwin-width,
    npwin-height,
    npwin-cliprect,
    <NPSavedData>,
    <NPSavedData*>,
    SavedData-len,
    SavedData-buf,
    SavedData-len-setter,
    SavedData-buf-setter,
    <NPStream>,
    <NPStream*>,
    NPStream-pdata,
    NPStream-ndata,
    NPStream-url,
    NPStream-end,
    NPStream-lastmodified,
    <NPP>,
    <NPPrint>,
    <NPPrint*>,
    npprint-mode,
    npprint-print,
    <C-void*>,
    plugin-window,
    plugin-window-handler;
  export
    NPP-New,
    NPP-Destroy,
    NPP-SetWindow,
    NPP-NewStream,
    NPP-WriteReady,
    NPP-Write,
    NPP-DestroyStream,
    NPP-StreamAsFile,
    NPP-Print
    ;
end module netscape-plugin-implementation;

define module netscape-plugin
  use netscape-plugin-implementation, export: all;
end module netscape-plugin;
