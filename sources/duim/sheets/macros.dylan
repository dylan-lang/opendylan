Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macros that are best defined early on

/// Medium binding macros

// Allocates a medium, and attaches it to the sheet
define macro with-sheet-medium
  { with-sheet-medium (?medium:name = ?sheet:expression) ?:body end }
    => { begin
           let with-sheet-medium-body = method (?medium) ?body end;
           do-with-sheet-medium(?sheet, with-sheet-medium-body)
         end }
end macro with-sheet-medium;

// Allocates a temporary medium, and attaches it to the sheet
define macro with-temporary-medium
  { with-temporary-medium (?medium:name = ?sheet:expression) ?:body end }
    => { begin
           let _sheet = ?sheet;
           let ?medium = allocate-medium(port(_sheet), _sheet);
           block ()
             ?body
           cleanup
             deallocate-medium(port(_sheet), ?medium)
           end
         end }
end macro with-temporary-medium;


/// Drawing state macros

define macro with-drawing-options
  { with-drawing-options (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-drawing-options-body = method () ?body end;
	   do-with-drawing-options(?medium, with-drawing-options-body, ?options)
	 end }
end macro with-drawing-options;

define macro with-pen
  { with-pen (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-pen-body = method () ?body end;
	   let _pen = make(<pen>, ?options);
	   do-with-drawing-options(?medium, with-pen-body, pen: _pen)
	 end }
end macro with-pen;

define macro with-brush
  { with-brush (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-brush-body = method () ?body end;
	   let _brush = make(<brush>, ?options);
	   do-with-drawing-options(?medium, with-brush-body, brush: _brush)
	 end }
end macro with-brush;

define macro with-text-style
  { with-text-style (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-text-style-body = method () ?body end;
	   let _text-style = make(<text-style>, ?options);
	   do-with-text-style(?medium, with-text-style-body, _text-style)
	 end }
end macro with-text-style;


define macro with-atomic-redisplay
  { with-atomic-redisplay (?sheet:name) ?:body end }
    => { begin
           let with-atomic-redisplay-body = method (?sheet) ?body end;
           do-with-atomic-redisplay(?sheet, with-atomic-redisplay-body)
         end }
end macro with-atomic-redisplay;


/// Medium transform hacking

define macro with-transform
  { with-transform (?medium:name, ?transform:expression) ?:body end }
    => { begin
           let with-transform-body = method () ?body end;
           do-with-transform(?medium, with-transform-body, ?transform)
         end }
end macro with-transform;

define macro with-translation
  { with-translation (?medium:name, ?dx:expression, ?dy:expression)
      ?:body
    end }
    => { with-transform (?medium, make-translation-transform(?dx, ?dy))
           ?body
         end }
end macro with-translation;

define macro with-rotation
  { with-rotation (?medium:name, ?angle:expression, #key ?x:expression, ?y:expression)
      ?:body
    end }
    => { with-transform (?medium, make-rotation-transform(?angle, origin-x: ?x, origin-y: ?y))
           ?body
         end }
  { with-rotation (?medium:name, ?angle:expression)
      ?:body
    end }
    => { with-transform (?medium, make-rotation-transform(?angle))
           ?body
         end }
end macro with-rotation;

define macro with-scaling
  { with-scaling (?medium:name, ?sx:expression, ?sy:expression)
      ?:body
    end }
    => { with-transform (?medium, make-scaling-transform(?sx, ?sy))
           ?body
         end }
end macro with-scaling;

define macro with-identity-transform
  { with-identity-transform (?medium:name) ?:body end }
    => { dynamic-bind (medium-transform(?medium) = $identity-transform)
           ?body
         end }
end macro with-identity-transform;


define macro with-clipping-region
  { with-clipping-region (?medium:name, ?region:expression) ?:body end }
    => { dynamic-bind (medium-clipping-region(?medium) = ?region)
           ?body
         end }
end macro with-clipping-region;


/// Frame managers

define macro with-frame-manager
  { with-frame-manager (?framem:expression) ?:body end }
    => { dynamic-bind (*current-frame-manager* = ?framem)
	   ?body
	 end }
end macro with-frame-manager;
