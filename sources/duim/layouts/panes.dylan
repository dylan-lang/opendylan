Module:       duim-layouts-internals
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Concrete pane classes

/// Useful classes for building concrete panes and gadgets

// Subclass this if you want to create a basic leaf pane, that is, a sheet that
// lives at the leaf of the sheet tree that obeys the layout protocols.
// If you want to do output to it, mix in one of the <sheet-with-medium-mixin> classes.
// If you want to do input from it, min in one of the <sheet-with-event-queue> classes.
// If you want to do repaint it, mix in of the <sheet-with-repainting-mixin> classes.
define open abstract class <leaf-pane>
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <space-requirement-mixin>,
     <leaf-layout-mixin>,
     <basic-sheet>)
end class <leaf-pane>;


// Subclass this if you want to create a basic composite pane.
// If you want to do input from it, mix in one of the <sheet-with-event-queue> classes.
// If you want to do repaint it, mix in one of the <sheet-with-repainting-mixin> classes.
define open abstract class <basic-composite-pane>
    (<cached-space-requirement-mixin>,
     <composite-layout-mixin>,
     <basic-sheet>)
end class <basic-composite-pane>;


// Subclass this one if you want to create a composite pane with one child
define open abstract class <single-child-composite-pane>
    (<single-child-mixin>,
     <basic-composite-pane>)
end class <single-child-composite-pane>;

// Subclass this one if you want to create a composite pane with multiple children
define open abstract class <multiple-child-composite-pane>
    (<multiple-child-mixin>,
     <basic-composite-pane>)
end class <multiple-child-composite-pane>;


/// Base class for concrete panes

define open abstract class <single-child-wrapping-pane>
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <wrapping-layout-mixin>,
     <single-child-mixin>,
     <basic-sheet>)
end class <single-child-wrapping-pane>;

define open abstract class <multiple-child-wrapping-pane>
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <wrapping-layout-mixin>,
     <multiple-child-mixin>,
     <basic-sheet>)
end class <multiple-child-wrapping-pane>;


/// Support for user-defined "embedded" panes

define open abstract class <basic-user-pane>
    (<single-child-wrapping-pane>)
  sealed slot %pane-framem :: false-or(<frame-manager>) = #f;
  sealed slot %pane-layout :: false-or(<abstract-sheet>) = #f;
end class <basic-user-pane>;

define method initialize (pane :: <basic-user-pane>, #key frame-manager: framem)
  next-method();
  pane.%pane-framem := framem;
  unless (sheet-child(pane))
    let new-child = pane-layout(pane);
    if (new-child)
      sheet-child(pane) := new-child
    end
  end
end method initialize;

define open generic pane-layout
    (pane :: <abstract-sheet>) => (sheet :: false-or(<abstract-sheet>));

define method pane-layout
    (pane :: <basic-user-pane>) => (sheet :: false-or(<abstract-sheet>))
  #f
end method pane-layout;

// The current pane in this thread
define thread variable *current-pane* = #f;

define inline function current-pane () *current-pane* end;


define macro pane-definer
  { define ?modifiers:* pane ?:name (?superclasses:*) ?slots:* end }
    => { define ?modifiers pane-class ?name (?superclasses) ?slots end;
         define pane-generators ?name (?superclasses) ?slots end;
         define pane-layout ?name (?superclasses) ?slots end; }
end macro pane-definer;

define macro pane-class-definer
  { define ?modifiers:* pane-class ?:name () ?slots:* end }
    => { define ?modifiers class ?name (<basic-user-pane>) ?slots end }
  { define ?modifiers:* pane-class ?:name (?superclasses:*) ?slots:* end }
    => { define ?modifiers class ?name (?superclasses, <basic-user-pane>) ?slots end }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot ... }
 slot:
  { ?modifiers:* pane ?:name (?pane:variable) ?:body }
    => { ?modifiers slot ?name ## "-pane" :: false-or(<abstract-sheet>) = #f; }
  { layout (?pane:variable) ?:body } => { }		// uses %pane-layout slot
  // Catch 'slot', 'keyword', and so forth
  { ?other:* } => { ?other; }
end macro pane-class-definer;

define macro pane-generators-definer
  { define pane-generators ?class:name (?superclasses:*) end }
    => { }
  { define pane-generators ?class:name (?superclasses:*) 
      pane ?:name (?pane:variable) ?:body; ?more-slots:*
    end }
    => { define method ?name (?pane :: ?class)
	   let framem = ?pane.%pane-framem;
	   ?pane.?name ## "-pane"
	   | (?pane.?name ## "-pane"
                := with-frame-manager (framem)
                     dynamic-bind (*current-pane* = ?pane)
                       ?body
                     end
                   end)
         end; 
         define pane-generators ?class (?superclasses) ?more-slots end; }
  { define pane-generators ?class:name (?superclasses:*) 
      ?non-pane-slot:*; ?more-slots:*
    end }
    => { define pane-generators ?class (?superclasses) ?more-slots end; }
end macro pane-generators-definer;

define macro pane-layout-definer
  { define pane-layout ?class:name (?superclasses:*) end }
    => { }
  { define pane-layout ?class:name (?superclasses:*) 
      layout (?pane:variable) ?:body; ?more-slots:*
    end }
    => { define method pane-layout (?pane :: ?class) => (sheet :: false-or(<abstract-sheet>))
	   let framem = ?pane.%pane-framem;
	   ?pane.%pane-layout
	   | (?pane.%pane-layout
                := with-frame-manager (framem)
                     dynamic-bind (*current-pane* = ?pane)
                       ?body
                     end
                   end)
         end }
  { define pane-layout ?class:name (?superclasses:*) 
      ?non-layout-slot:*; ?more-slots:*
    end }
    => { define pane-layout ?class (?superclasses) ?more-slots end; }
end macro pane-layout-definer;


/// Top level sheets

//--- Maybe we should define <top-level-sheet> in Duim-Sheets, and
//--- this should be called <top-level-layout>?
define open abstract class <top-level-sheet>
    (<single-child-wrapping-pane>)
  sealed slot display :: false-or(<display>) = #f,
    init-keyword: display:,
    setter: %display-setter;
  sealed slot sheet-frame :: false-or(<frame>) = #f,
    init-keyword: frame:,
    setter: %frame-setter;
  sealed slot frame-manager :: false-or(<frame-manager>) = #f,
    init-keyword: frame-manager:,
    setter: %frame-manager-setter;
  // For use in embedded frames, e.g., OLE and Netscape.
  // Note that the container is a native window system object.
  sealed slot sheet-container = #f,
    init-keyword: container:;
  sealed slot sheet-container-region = #f,
    init-keyword: container-region:;
end class <top-level-sheet>;

define method top-level-sheet 
    (sheet :: <top-level-sheet>) => (sheet :: <top-level-sheet>)
  sheet
end method top-level-sheet;

define method display-setter
    (_display :: false-or(<display>), sheet :: <top-level-sheet>)
 => (display :: false-or(<display>))
  sheet.%display := _display
end method display-setter;

define method sheet-frame-setter
    (frame :: false-or(<frame>), sheet :: <top-level-sheet>)
 => (frame :: false-or(<frame>))
  sheet.%frame := frame
end method sheet-frame-setter;

define method frame-manager-setter
    (framem :: false-or(<frame-manager>), sheet :: <top-level-sheet>)
 => (framem :: false-or(<frame-manager>))
  sheet.%frame-manager := framem
end method frame-manager-setter;

// When a sheet changes size, this can be used to notify its parent so
// that the new layout gets propagated up the sheet tree.  Note that this
// starts the relayout process at the sheet itself, not its parent (as the
// name of the function would seem to imply).
// "Sideways" because 'relayout-parent' is a forward reference from DUIM-Sheets.
//--- We should maybe do something to protect users from calling this
//--- before the sheets are mirrored, since 'compose-space' will blow out
define sideways method relayout-parent 
    (sheet :: <sheet>, #key width, height) => (did-layout? :: <boolean>)
  when (sheet-attached?(sheet))		// be forgiving
    reset-space-requirement(sheet);	// force 'compose-space' to run anew...
    let (old-width, old-height) = box-size(sheet);
    let space-req = compose-space(sheet,
				  width:  width  | old-width,
				  height: height | old-height);
    let (w, w-, w+, h, h-, h+) = space-requirement-components(sheet, space-req);
    ignore(w-, w+, h-, h+);
    let new-width  :: <integer> = w;
    let new-height :: <integer> = h;
    unless (sheet-layed-out-to-size?(sheet, new-width, new-height))
      let parent = sheet-parent(sheet);
      sheet-layed-out?(sheet) := #f;
      when (~parent | display?(parent) | ~relayout-parent(parent))
	set-sheet-size(sheet, new-width, new-height);
	#t
      end
    end
  end
end method relayout-parent;


/// Simple user panes

define open generic pane-display-function
    (pane :: <abstract-sheet>) => (function :: false-or(<function>));

define open abstract class <pane-display-function-mixin> (<abstract-sheet>)
  slot pane-display-function :: false-or(<function>) = #f,
    init-keyword: display-function:;
end class <pane-display-function-mixin>;

define method handle-repaint
    (pane :: <pane-display-function-mixin>, medium :: <medium>, region :: <region>) => ()
  let function = pane-display-function(pane);
  when (function)
    function(pane, medium, region)
  end
end method handle-repaint;


/// Simple panes

// A pane that provides event handling, but no drawing surface.  Repainting
// should get done on the medium of some parent.
//--- When unmirrored sheets like this get moved, they need to arrange for
//--- their parent to be repainted.  Where should this be done?
define open abstract class <simple-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <pane-display-function-mixin>,
     <multiple-child-wrapping-pane>)
end class <simple-pane>;

define method handle-repaint
    (sheet :: <simple-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(medium, region);
  if (pane-display-function(sheet))
    next-method()
  else
    error("The pane %= has no display function and no 'handle-repaint' method", sheet)
  end
end method handle-repaint;

define method sheet-handles-keyboard?
    (sheet :: <simple-pane>) => (true? :: <boolean>)
  sheet-accepts-focus?(sheet)
end method sheet-handles-keyboard?;

define sealed class <concrete-simple-pane> (<simple-pane>)
end class <concrete-simple-pane>;

define sealed domain make (singleton(<concrete-simple-pane>));
define sealed domain initialize (<concrete-simple-pane>);

define sealed inline method make
    (class == <simple-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-simple-pane>)
  apply(make, <concrete-simple-pane>, initargs)
end method make;


/// Drawing panes

// A pane that provides event handling and a drawing surface.  Note that a
// drawing pane can be wrapped around a layout pane to provide a medium for
// all the children of the layout pane.
// Note well! Back ends must supply a 'port-handles-repaint?' method for this!
define open abstract class <drawing-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,		// drawing panes always want a medium
     <mirrored-sheet-mixin>,		// mirroring them gives better behavior, too
     <sheet-with-caret-mixin>,
     <pane-display-function-mixin>,
     <multiple-child-wrapping-pane>)
end class <drawing-pane>;

define method sheet-handles-keyboard?
    (sheet :: <drawing-pane>) => (true? :: <boolean>)
  sheet-accepts-focus?(sheet)
end method sheet-handles-keyboard?;

define sealed class <concrete-drawing-pane> (<drawing-pane>)
end class <concrete-drawing-pane>;

define sealed domain make (singleton(<concrete-drawing-pane>));
define sealed domain initialize (<concrete-drawing-pane>);

define sealed inline method make
    (class == <drawing-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-drawing-pane>)
  apply(make, <concrete-drawing-pane>, initargs)
end method make;


/// Null panes

// This acts as a filler, and nothing more
define open abstract class <null-pane>
    (<leaf-pane>)
end class <null-pane>;

define method default-space-requirement
    (pane :: <null-pane>,
     #key width, min-width, max-width, height, min-height, max-height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 1,                 height: height | 1,
       min-width: min-width | 0,         min-height: min-height | 0,
       max-width: max-width | width | 1, max-height: max-height | height | 1)
end method default-space-requirement;

define sealed class <concrete-null-pane> (<null-pane>)
  keyword accepts-focus?: = #f;
end class <concrete-null-pane>;

define sealed domain make (singleton(<concrete-null-pane>));
define sealed domain initialize (<concrete-null-pane>);

define sealed inline method make
    (class == <null-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-null-pane>)
  apply(make, <concrete-null-pane>, initargs)
end method make;
