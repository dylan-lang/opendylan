Module:       Dylan-User
Synopsis:     DUIM postscript backend
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module postscript-duim
  use dylan;

  use duim-imports;
  use duim-internals;
  use streams;
  use format;
  use date;

  export <apple-laser-writer-legal>,
         <apple-laser-writer>,
         <postscript-medium>,
         <postscript-port>,
         <postscript-sheet>,
         //--- <postscript-stream>,
         new-page,
         postscript-epilogue, postscript-device-epilogue,
         postscript-prologue, postscript-device-prologue,
         //--- \with-output-to-postscript-stream, do-with-output-to-postscript-stream,
         \with-output-to-postscript-sheet, do-with-output-to-postscript-sheet;
end module postscript-duim;
