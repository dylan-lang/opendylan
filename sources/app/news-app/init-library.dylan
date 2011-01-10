language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// excluded source
// define interface
//   #include "Memory.h",
//      define: {"SystemSevenOrLater" => 1},
// /*   import: {"NewPtr", "NewPtrClear", "DisposePtr", // don't use old name DisposPtr!
//               "NewHandle", "NewHandleClear", "DisposeHandle",
//               "HLock", "HUnlock",
//               "BlockMove",  
//               "MemError", "SetHandleSize",
//               "MoveHHi", "StripAddress", "RecoverHandle"
//              };  */
//      exclude: {"GetHandleSize",  // rename InlineGetHandleSize instead
//                "GetPtrSize", "GetPhysical", "InitZone", "PurgeSpace",  // no access patch
//                "CallGrowZoneProc", "CallPurgeProc",  // cannot translate to Dylan
//                
//                // unneeded pointer type
//                "THz"
//                },
//      
//      rename: {"InlineGetHandleSize" => GetHandleSize};
//   
// end interface;

// excluded source
// define interface
//   #include "QuickDraw.h",
//      define: {"SystemSevenOrLater" => 1},
//      
//      import: {
//               "SetCursor"
//              };
// end interface;

// excluded source
// define interface
//   #include "ToolUtils.h",
//      define: {"SystemSevenOrLater" => 1},
//      
//      import: {
//               "watchCursor"
//              };
// end interface;

define-framework-library("news-app")
