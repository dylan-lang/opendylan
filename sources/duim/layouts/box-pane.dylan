Module:       duim-layouts-internals
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Box Panes

// In Motif, this would be a row-column widget...
define open abstract class <box-layout-pane> 
    (<layout-border-mixin>,
     <layout-pane>)
  sealed slot %max-major-size :: <integer> = 0;
  sealed slot %max-minor-size :: <integer> = 0;
end class <box-layout-pane>;

// Bits 12..13 of 'sheet-flags' are reserved for box layouts
define constant %equalize_widths  :: <integer> = #o10000;
define constant %equalize_heights :: <integer> = #o20000;

define method initialize
    (box :: <box-layout-pane>, #key equalize-widths? = #f, equalize-heights? = #f)
  next-method();
  let bits = logior(if (equalize-widths?)  %equalize_widths  else 0 end,
		    if (equalize-heights?) %equalize_heights else 0 end);
  sheet-flags(box) := logior(sheet-flags(box), bits)
end method initialize;

// Don't change the order of the sheets, because that will break the layout
// Note that 'raise-sheet' will arrange to raise mirrors, which is OK
define method do-raise-sheet
    (parent :: <box-layout-pane>, sheet :: <sheet>, #key activate? = #t) => ()
  ignore(activate?);
  #f
end method do-raise-sheet;

define sealed inline method layout-equalize-widths?
    (box :: <box-layout-pane>) => (equalize? :: <boolean>)
  logand(sheet-flags(box), %equalize_widths) = %equalize_widths
end method layout-equalize-widths?;

define sealed inline method layout-equalize-heights?
    (box :: <box-layout-pane>) => (equalize? :: <boolean>)
  logand(sheet-flags(box), %equalize_heights) = %equalize_heights
end method layout-equalize-heights?;

define function box-pane-compose-space
    (box-pane :: <box-layout-pane>,
     requested-major :: false-or(<integer>),
     fn-major :: <function>, fn-major- :: <function>, fn-major+ :: <function>,
     requested-minor :: false-or(<integer>),
     fn-minor :: <function>, fn-minor- :: <function>, fn-minor+ :: <function>,
     space-composer :: <function>, space-req-creator :: <function>,
     #key equalize-major-size?, equalize-minor-size?)
 => (space-req :: <space-requirement>)
  ignore(equalize-minor-size?);
  let children = sheet-children(box-pane);
  let n-children :: <integer> = size(children) - count(sheet-withdrawn?, children);
  let major  :: <integer> = 0;
  let major+ :: <integer> = 0;
  let major- :: <integer> = 0;
  let minor  :: <integer> = 0;
  let minor+ :: <integer> = 0;
  let minor- :: <integer> = 0;
  let minor-min :: <integer> = 0;
  let minor-max :: <integer> = 0;
  let max-major-size :: <integer> = 0;
  let max-minor-size :: <integer> = 0;
  local method update-major-space (space-req) => ()
	  let m  :: <integer> = fn-major (box-pane, space-req);
	  let m+ :: <integer> = fn-major+(box-pane, space-req);
	  let m- :: <integer> = fn-major-(box-pane, space-req);
	  max!(max-major-size, m);
	  inc!(major,  m);
	  inc!(major+, m+);
	  inc!(major-, m-)
	end method,
        method update-minor-space (space-req) => ()
	  let m  :: <integer> = fn-minor (box-pane, space-req);
	  let m+ :: <integer> = fn-minor+(box-pane, space-req);
	  let m- :: <integer> = fn-minor-(box-pane, space-req);
	  max!(max-minor-size, m);
	  max!(minor-max, m+);
	  max!(minor-min, m-);
	  minor := min(max(minor, m, minor-min), minor-max)
	end method;
  for (child in children)
    when (child)
      let child :: <basic-sheet> = child;	// force tighter type...
      unless (sheet-withdrawn?(child))
	let space-req = space-composer(child);
	update-major-space(space-req);
	update-minor-space(space-req)
      end
    end
  end;
  let border*2 = layout-border(box-pane) * 2;
  local method cleanup-major-space () => ()
	  let size-for-spacing :: <integer>
	    = (n-children - 1) * box-pane-major-spacing(box-pane) + border*2;
	  if (equalize-major-size?)
	    major := n-children * max-major-size + size-for-spacing;
	    when (major- < $fill) major- := major end;
	    when (major+ < $fill) major+ := major end
	  else
	    inc!(major,  size-for-spacing);
	    inc!(major-, size-for-spacing);
	    inc!(major+, size-for-spacing)
	  end;
	  // If there's a requested size, use it, but ensure that it
	  // falls between the min and max sizes
	  when (requested-major)
	    major := max(major-, min(major+, requested-major))
	  end
	end method,
        method cleanup-minor-space () => ()
	  minor- := minor-min + border*2;
	  minor+ := minor-max + border*2;
	  minor  := minor + border*2;
	  when (requested-minor)
	    minor := max(minor-, min(minor+, requested-minor))
	  end
	end method;
  cleanup-major-space();
  cleanup-minor-space();
  box-pane.%max-major-size := max-major-size;
  box-pane.%max-minor-size := max-minor-size;
  space-req-creator(major, major-, major+, minor, minor-, minor+)
end function box-pane-compose-space;

define function box-pane-allocate-space 
    (box-pane :: <box-layout-pane>,
     major-sizes :: <vector>, box-major-size :: <integer>, box-minor-size :: <integer>,
     compose :: <function>, alignment-function :: <function>, set-child-edges :: <function>,
     #key major-size-override, minor-size-override) => ()
  let border = layout-border(box-pane);
  let major-spacing  :: <integer> = box-pane-major-spacing(box-pane);
  let major-position :: <integer> = border;
  for (sheet in sheet-children(box-pane),
       suggested-major-size :: <integer> in major-sizes)
    when (sheet)
      let sheet :: <basic-sheet> = sheet;	// force tighter type...
      unless (sheet-withdrawn?(sheet))
	let (major-size :: <integer>, minor-size :: <integer>)
	  = compose(sheet, 
		    minor: box-minor-size,
		    major: suggested-major-size);
	let minor-position :: <integer>
	  = alignment-function(box-pane, sheet, 0, box-minor-size - minor-size) + border;
	let major-size :: <integer> = major-size-override | major-size;
	let minor-size :: <integer> = minor-size-override | minor-size;
	set-child-edges(sheet,
			major-position, minor-position, 
			major-position + major-size, minor-position + minor-size);
	inc!(major-position, major-size + major-spacing)
      end
    end
  end
end function box-pane-allocate-space;


/// Row panes, formerly known as hboxes

define open abstract class <row-layout>
    (<horizontal-layout-mixin>, <box-layout-pane>)
end class <row-layout>;

define method box-pane-major-spacing
    (box :: <row-layout>) => (spacing :: <integer>)
  layout-x-spacing(box)
end method box-pane-major-spacing;

define method do-compose-space
    (box-pane :: <row-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  local method space-composer
	    (child) => (sr :: <space-requirement>)
	  compose-space(child, height: height)
	end method,
        method space-req-creator
	    (width, min-width, max-width, height, min-height, max-height)
	 => (sr :: <space-requirement>)
	  make(<space-requirement>,
	       width:  width,  min-width:  min-width,  max-width:  max-width,
	       height: height, min-height: min-height, max-height: max-height)
	end method;
  dynamic-extent(space-composer, space-req-creator);
  if (empty?(sheet-children(box-pane)))
    default-space-requirement(box-pane, width: width, height: height)
  else
    box-pane-compose-space
      (box-pane,
       width,
       space-requirement-width,  space-requirement-min-width,  space-requirement-max-width,
       height,
       space-requirement-height, space-requirement-min-height, space-requirement-max-height,
       space-composer, space-req-creator,
       equalize-major-size?: layout-equalize-widths?(box-pane),
       equalize-minor-size?: layout-equalize-heights?(box-pane))
  end
end method do-compose-space;

define method do-allocate-space
    (box-pane :: <row-layout>, width :: <integer>, height :: <integer>) => ()
  let space-requirement = compose-space(box-pane, width: width, height: height);
  let children = sheet-children(box-pane);
  let n-children :: <integer> = size(children) - count(sheet-withdrawn?, children);
  let total-spacing = (n-children - 1) * box-pane-major-spacing(box-pane);
  let sizes
    = compose-space-for-items
        (box-pane,
	 width - total-spacing, space-requirement, children,
         space-requirement-width, space-requirement-min-width, space-requirement-max-width,
         method (child)
           compose-space(child, height: height)
         end,
         ratios: layout-x-ratios(box-pane));
  box-pane-allocate-space
    (box-pane, sizes, width, height,
     method (child, #key major, minor)
       let space-req = compose-space(child, width: major, height: minor);
       let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
       values(constrain-size(major | w, w-, w+),
              constrain-size(minor | h, h-, h+))
     end method,
     layout-align-sheet-y, set-sheet-edges,
     major-size-override: layout-equalize-widths?(box-pane)  & box-pane.%max-major-size,
     minor-size-override: layout-equalize-heights?(box-pane) & box-pane.%max-minor-size)
end method do-allocate-space;


// Options can be any of the pane sizing options, plus SPACING: and X-RATIOS:, etc
define macro horizontally
  { horizontally (#rest ?options:expression)
      ?contents:*
    end }
    => { make(<row-layout>, children: vector(?contents), ?options) }
 contents:
  { } => { }
  { ?pane-spec:*; ... } => { ?pane-spec, ... }
 pane-spec:
  //--- It would be nice to have syntax for ratios...
  { ?pane:expression } => { ?pane }
end macro horizontally;


/// Default implementation

define sealed class <row-layout-pane> (<row-layout>)
  keyword accepts-focus?: = #f;
end class <row-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <row-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<row-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<row-layout-pane>));
define sealed domain initialize (<row-layout-pane>);


/// Column panes, formerly known as vboxes

define open abstract class <column-layout>
    (<vertical-layout-mixin>, <box-layout-pane>)
end class <column-layout>;

define method box-pane-major-spacing
    (box :: <column-layout>) => (spacing :: <integer>)
  layout-y-spacing(box)
end method box-pane-major-spacing;

define method do-compose-space
    (box-pane :: <column-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  local method space-composer
	    (child) => (space-req :: <space-requirement>)
	  compose-space(child, width: width)
	end method,
        method space-req-creator
	    (height, min-height, max-height, width, min-width, max-width)
	 => (space-req :: <space-requirement>)
	  make(<space-requirement>,
	       width:  width,  min-width:  min-width,  max-width:  max-width,
	       height: height, min-height: min-height, max-height: max-height)
	end method;
  dynamic-extent(space-composer, space-req-creator);
  if (empty?(sheet-children(box-pane)))
    default-space-requirement(box-pane, width: width, height: height)
  else
    box-pane-compose-space
      (box-pane,
       height,
       space-requirement-height, space-requirement-min-height, space-requirement-max-height,
       width,
       space-requirement-width,  space-requirement-min-width,  space-requirement-max-width,
       space-composer, space-req-creator,
       equalize-major-size?: layout-equalize-heights?(box-pane),
       equalize-minor-size?: layout-equalize-widths?(box-pane))
  end
end method do-compose-space;

define method do-allocate-space
    (box-pane :: <column-layout>, width :: <integer>, height :: <integer>) => ()
  let space-requirement = compose-space(box-pane, width: width, height: height);
  let children = sheet-children(box-pane);
  let n-children :: <integer> = size(children) - count(sheet-withdrawn?, children);
  let total-spacing = (n-children - 1) * box-pane-major-spacing(box-pane);
  let sizes
    = compose-space-for-items
        (box-pane,
	 height - total-spacing, space-requirement, children,
         space-requirement-height, space-requirement-min-height, space-requirement-max-height, 
         method (sheet)
           compose-space(sheet, width: width)
         end,
         ratios: layout-y-ratios(box-pane));
  box-pane-allocate-space
    (box-pane, sizes, height, width,
     method (child, #key major, minor)
       let space-req = compose-space(child, width: major, height: minor);
       let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
       values(constrain-size(major | h, h-, h+),
	      constrain-size(minor | w, w-, w+))
     end method,
     layout-align-sheet-x,
     method (sheet :: <basic-sheet>,
	     top :: <integer>, left :: <integer>, bottom :: <integer>, right :: <integer>)
       set-sheet-edges(sheet, left, top, right, bottom)
     end,
     major-size-override: layout-equalize-heights?(box-pane) & box-pane.%max-major-size,
     minor-size-override: layout-equalize-widths?(box-pane)  & box-pane.%max-minor-size)
end method do-allocate-space;


// Options can be any of the pane sizing options, plus SPACING:
define macro vertically
  { vertically (#rest ?options:expression)
      ?contents:*
    end }
    => { make(<column-layout>, children: vector(?contents), ?options) }
 contents:
  { } => { }
  { ?pane-spec:*; ... } => { ?pane-spec, ... }
 pane-spec:
  //--- It would be nice to have syntax for ratios...
  { ?pane:expression } => { ?pane }
end macro vertically;


/// Default implementation

define sealed class <column-layout-pane> (<column-layout>)
  keyword accepts-focus?: = #f;
end class <column-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <column-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<column-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<column-layout-pane>));
define sealed domain initialize (<column-layout-pane>);
