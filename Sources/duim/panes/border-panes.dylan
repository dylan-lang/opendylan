Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of borders

define open abstract class <border-pane-mixin>
    (<standard-repainting-mixin>)
  sealed slot %pen   :: false-or(<standard-pen>) = #f;
  sealed slot %brush :: false-or(type-union(<standard-brush>, <ink>)) = #f;
end class <border-pane-mixin>;

define method initialize (sheet :: <border-pane-mixin>, #key thickness) => ()
  next-method();
  when (~thickness
	& member?(border-type(sheet),
		  #[#"sunken", #"raised", #"ridge", #"groove", #"input", #"output"]))
    border-thickness(sheet) := 3
  end
end method initialize;

define open generic border-characteristics
    (pane)
 => (thickness :: <integer>,
     pen :: <standard-pen>, brush :: type-union(<standard-brush>, <ink>));

define method do-compose-space
    (pane :: <border-pane-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let thickness*2 = border-thickness(pane) * 2;
  space-requirement+(pane,
		     next-method(pane,
				 width:  width  & width  - thickness*2,
				 height: height & height - thickness*2),
		     width: thickness*2, height: thickness*2)
end method do-compose-space;

define method do-allocate-space
    (pane :: <border-pane-mixin>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let thickness = border-thickness(pane);
  when (child)
    set-sheet-edges(child,
                    thickness, thickness,
                    width - thickness, height - thickness)
  end
end method do-allocate-space;

define method handle-repaint
    (pane :: <border-pane-mixin>, medium :: <medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-border(pane, medium, border-type(pane), left, top, right, bottom)
end method handle-repaint;

define method draw-border
    (pane :: <border-pane-mixin>, medium :: <medium>, type :: <border-type>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let (thickness, pen, brush) = border-characteristics(pane);
  with-drawing-options (medium, pen: pen, brush: brush)
    let thickness/2 = ceiling/(thickness, 2);
    draw-rectangle(medium, 
                   left  + thickness/2, top + thickness/2, 
                   right - thickness/2 - 1, bottom - thickness/2 - 1,
                   filled?: #f)
  end
end method draw-border;


/// Border panes

define sealed class <border-pane>
    (<border-pane-mixin>, <border>, <basic-sheet>)
end class <border-pane>;

define method border-characteristics
    (pane :: <border-pane>)
 => (thickness :: <integer>,
     pen :: <standard-pen>, brush :: type-union(<standard-brush>, <ink>))
  let thickness = border-thickness(pane);
  unless (pane.%pen)
    pane.%pen := make(<pen>, width: thickness)
  end;
  unless (pane.%brush)
    pane.%brush := get-default-foreground(port(pane), pane)
  end;
  values(thickness, pane.%pen, pane.%brush)
end method border-characteristics;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <group-box-pane> else <border-pane> end;
  values(border-class, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<border-pane>));
define sealed domain initialize (<border-pane>);


/// Spacing panes

define sealed class <spacing-pane>
    (<border-pane-mixin>, <spacing>, <basic-sheet>)
end class <spacing-pane>;

define method border-characteristics
    (pane :: <spacing-pane>)
 => (thickness :: <integer>,
     pen :: <standard-pen>, brush :: type-union(<standard-brush>, <ink>))
  let thickness = border-thickness(pane);
  unless (pane.%pen)
    pane.%pen := make(<pen>, width: thickness)
  end;
  unless (pane.%brush)
    pane.%brush := get-default-background(port(pane), pane)
  end;
  values(thickness, pane.%pen, pane.%brush)
end method border-characteristics;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <spacing>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<spacing-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<spacing-pane>));
define sealed domain initialize (<spacing-pane>);


/// Group box panes, aka labelled border panes

define sealed class <group-box-pane>
    (<border-pane-mixin>, <group-box>, <basic-sheet>)
  sealed slot %border-coords = #f;
end class <group-box-pane>;

define method border-characteristics
    (pane :: <group-box-pane>)
 => (thickness :: <integer>,
     pen :: <standard-pen>, brush :: type-union(<standard-brush>, <ink>))
  let thickness = border-thickness(pane);
  unless (pane.%pen)
    pane.%pen := make(<pen>, width: thickness)
  end;
  unless (pane.%brush)
    pane.%brush := get-default-foreground(port(pane), pane)
  end;
  values(thickness, pane.%pen, pane.%brush)
end method border-characteristics;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <group-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<group-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<group-box-pane>));
define sealed domain initialize (<group-box-pane>);

define constant $group-box-internal-border :: <integer> = 7;

define method do-compose-space
    (pane :: <group-box-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  let child = sheet-child(pane);
  let border = (border-thickness(pane) + $group-box-internal-border) * 2;
  let (label-width, label-height) = gadget-label-size(pane);
  ignore(label-width);
  if (child)
    space-requirement+(pane,
		       compose-space(child,
				     width:  width  & width  - border,
				     height: height & height - border),
		       width:  border, height: border + label-height)
  else
    default-space-requirement(pane, width: width, height: height)
  end
end method do-compose-space;

define method do-allocate-space
    (pane :: <group-box-pane>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let border = border-thickness(pane) + $group-box-internal-border;
  let (label-width, label-height) = gadget-label-size(pane);
  ignore(label-width);
  when (child)
    select (group-box-label-position(pane))
      #"top" =>
	set-sheet-edges(child,
                        border, label-height + border,
                        width - border, height - border);
      #"bottom" =>
	set-sheet-edges(child,
                        border, border,
                        width - border, height - label-height - border);
    end
  end;
  let (thickness, pen, brush) = border-characteristics(pane);
  ignore(pen, brush);
  let (left, top, right, bottom) = box-edges(pane);
  let thickness/2 = ceiling/(thickness, 2);
  inc!(left, thickness/2);
  inc!(top,  thickness/2);
  dec!(right,  thickness/2 + 1);
  dec!(bottom, thickness/2 + 1);
  let (label-width, label-height) = gadget-label-size(pane);
  inc!(top, truncate/(label-height, 2));
  pane.%border-coords
    := select (group-box-label-position(pane))
	 #"top" =>
	   vector(left + floor/(right - left - label-width, 2), top,
		  left, top,
		  left, bottom,
		  right, bottom,
		  right, top,
		  right - floor/(right - left - label-width, 2), top);
	 #"bottom" =>
	   vector(left + floor/(right - left - label-width, 2), bottom,
		  left, bottom,
		  left, top,
		  right, top,
		  right, bottom,
		  right - floor/(right - left - label-width, 2), bottom);
       end
end method do-allocate-space;

define method handle-repaint
    (pane :: <group-box-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  let (thickness, pen, brush) = border-characteristics(pane);
  with-drawing-options (medium, pen: pen, brush: brush)
    let (left, top, right, bottom) = box-edges(pane);
    ignore(top);
    let thickness/2 = ceiling/(thickness, 2);
    inc!(left, thickness/2);
    inc!(top,  thickness/2);
    dec!(right,  thickness/2 - 1);
    dec!(bottom, thickness/2 - 1);
    select (group-box-label-position(pane))
      #"top" =>
	draw-polygon(medium, pane.%border-coords, closed?: #f, filled?: #f);
	draw-gadget-label
	  (pane, medium,
	   floor/(right + left, 2), pane.%border-coords[1],	// what can I say?
	   align-x: #"center", align-y: #"center");
      #"bottom" =>
	draw-polygon(medium, pane.%border-coords, closed?: #f, filled?: #f);
	draw-gadget-label
	  (pane, medium,
	   floor/(right - left, 2), pane.%border-coords[1],	// I'm embarrassed
	   align-x: #"center", align-y: #"center");
    end
  end
end method handle-repaint;
