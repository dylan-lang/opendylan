Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sheet debugging

define generic print-sheet-layout (sheet, #key, #all-keys) => ();

define method print-sheet-layout
    (sheet :: <sheet>, #key indentation = "") => ()
  let (left, top, right, bottom) = sheet-edges(sheet);
  format-out("\n%s%=: %dx%d at %d,%d %s",
             indentation, sheet, right - left, bottom - top, left, top,
             case
               sheet-withdrawn?(sheet) => "[withdrawn]";
               sheet-mapped?(sheet)    => "[mapped]";
               otherwise               => "[unmapped]";
             end);
  do(rcurry(print-sheet-layout, indentation: concatenate(indentation, "  ")),
     sheet-children(sheet))
end method print-sheet-layout;

define method print-sheet-layout
    (frame :: <frame>, #rest args, #key) => ()
  dynamic-extent(args);
  apply(print-sheet-layout, top-level-sheet(frame), args)
end method print-sheet-layout;
