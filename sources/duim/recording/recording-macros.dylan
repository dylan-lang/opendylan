Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-end-of-line-action
  { with-end-of-line-action (?sheet:variable, ?action:expression)
      ?:body
    end }
    => { begin
	   let with-end-of-line-action-body = method () ?body end;
           do-with-end-of-line-action(?sheet, with-end-of-line-action-body, ?action)
         end }
end macro with-end-of-line-action;

define macro with-end-of-page-action
  { with-end-of-page-action (?sheet:variable, ?action:expression)
      ?:body
    end }
    => { begin
	   let with-end-of-page-action-body = method () ?body end;
           do-with-end-of-page-action(?sheet, with-end-of-page-action-body, ?action)
         end }
end macro with-end-of-page-action;


// Options can be DRAW?: and RECORD?:
define macro with-output-recording-options
  { with-output-recording-options (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-output-recording-options-body = method () ?body end;
           do-with-output-recording-options
             (?sheet, with-output-recording-options-body, ?options)
         end }
end macro with-output-recording-options;

// Options are output record initargs, plus RECORD-CLASS:
define macro with-new-output-record
  { with-new-output-record (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
	   let with-new-output-record-body
	     = method (?record) ?body end;
           do-with-new-output-record
             (?sheet, with-new-output-record-body, ?options)
         end }
  { with-new-output-record (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-new-output-record-body
	     = method (_record) ignore(_record); ?body end;
           do-with-new-output-record
             (?sheet, with-new-output-record-body, ?options)
         end }
end macro with-new-output-record;

// Options are output record initargs, plus RECORD-CLASS:
define macro with-output-to-output-record
  { with-output-to-output-record (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-output-to-output-record-body
             = method (?record)
                 with-caret-position-saved (?sheet) ?body end
               end;
           do-with-output-to-output-record
             (?sheet, with-output-to-output-record-body, ?options)
         end }
  { with-output-to-output-record (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-output-to-output-record-body
             = method (_record)
		 ignore(_record);
                 with-caret-position-saved (?sheet) ?body end
               end;
           do-with-output-to-output-record
             (?sheet, with-output-to-output-record-body, ?options)
         end }
end macro with-output-to-output-record;


/// Graphics

define macro with-local-coordinates
  { with-local-coordinates (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-local-coordinates-body = method () ?body end;
           do-with-local-coordinates
             (?sheet, with-local-coordinates-body, ?options)
         end }
end macro with-local-coordinates;

define macro with-first-quadrant-coordinates
  { with-first-quadrant-coordinates (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-first-quadrant-coordinates-body = method () ?body end;
           do-with-first-quadrant-coordinates
             (?sheet, with-first-quadrant-coordinates-body, ?options)
         end }
end macro with-first-quadrant-coordinates;

// Options are output record initargs, plus RECORD-CLASS:, X:, Y:
define macro with-room-for-graphics
  { with-room-for-graphics (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let with-room-for-graphics-body = method () ?body end;
           do-with-room-for-graphics
             (?sheet, with-room-for-graphics-body, ?options)
         end }
end macro with-room-for-graphics;
