Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Borders

// Outline panes draw a dark (foreground) border around a pane
define open abstract class <border> 
    // This stuff acts like <layout-pane>, but manages only a single child
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <wrapping-layout-mixin>,
     <single-child-mixin>,
     <bordered-gadget-mixin>,
     <basic-gadget>)
  sealed slot border-thickness :: <integer> = 1,
    init-keyword: thickness:;
end class <border>;

define method initialize
    (gadget :: <border>, #key type: borders = #f)
  next-method();
  border-type(gadget) := borders
end method initialize;

// Options can be any of the pane sizing options, plus THICKNESS: and TYPE:
define macro with-border
  { with-border (#rest ?options:expression)
      ?child:body
    end }
    => { let _child = ?child;		// child is a single expression
	 make(<border>, child: _child, ?options) }
end macro with-border;

define open generic draw-border
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>, type :: <border-type>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ();

define method draw-border
    (sheet :: <sheet>, medium :: <medium>, type :: <border-type>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  with-drawing-options (medium, brush: default-foreground(sheet))
    draw-rectangle(medium, left, top, right, bottom, filled?: #f)
  end
end method draw-border;


/// Spacing

// Spacing panes leaves whitespace (background) around a pane
define open abstract class <spacing> 
    //--- Like <layout-pane>, but manages only a single child
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <wrapping-layout-mixin>,
     <single-child-mixin>,
     <basic-gadget>)
  sealed slot border-thickness :: <integer> = 1,
    init-keyword: thickness:;
end class <spacing>;

define method border-type
    (pane :: <spacing>) => (type :: singleton(#f))
  #f
end method border-type;

define method initialize (sheet :: <spacing>, #key spacing) => ()
  next-method();
  when (spacing) 
    border-thickness(sheet) := spacing
  end
end method initialize;

// Options can be any of the pane sizing options, plus THICKNESS:
define macro with-spacing
  { with-spacing (#rest ?options:expression)
      ?child:body
    end }
    => { let _child = ?child;		// child is a single expression
	 make(<spacing>, child: _child, ?options) }
end macro with-spacing;


/// Group boxes, aka labelled borders

define open abstract class <group-box>
    (<labelled-gadget-mixin>,
     <border>)
  sealed slot group-box-label-position :: <vertical-position> = #"top",
    init-keyword: label-position:;
end class <group-box>;

define macro grouping
  { grouping (?label:expression)
      ?child:body
    end }
    => { let _child = ?child;		// child is a single expression
	 make(<group-box>, child: _child, label: ?label) }
  { grouping (?label:expression, #rest ?options:expression)
      ?child:body
    end }
    => { let _child = ?child;		// child is a single expression
	 make(<group-box>, ?options, child: _child, label: ?label) }
end macro grouping;
