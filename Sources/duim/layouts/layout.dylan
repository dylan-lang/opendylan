Module:       duim-layouts-internals
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Layout Protocol

// The superclass of all panes that do layout on their children,
// for example, <row-layout>, <column-layout>, and <table-layout>.
// Panes that merely implement 'do-compose-space' (such as all the
// gadgets) aren't subclasses of <layout>.
define protocol-class layout (<abstract-sheet>)
end protocol-class layout;

define constant <vertical-position>
    = one-of(#"top", #"bottom");
define constant <horizontal-position>
    = one-of(#"left", #"right");
define constant <y-alignment>
    = one-of(#"top", #"bottom", #"center", #"centre", #"baseline");
define constant <x-alignment>
    = one-of(#"left", #"right", #"center", #"centre");

define protocol <<layout-protocol>> ()
  function compose-space 
    (pane :: <abstract-sheet>, #key width, height)
 => (space-req :: <space-requirement>);
  function do-compose-space
    (pane :: <abstract-sheet>, #key width, height)
 => (space-req :: <space-requirement>);
  function allocate-space
    (pane :: <abstract-sheet>, width :: <integer>, height :: <integer>) => ();
  function do-allocate-space
    (pane :: <abstract-sheet>, width :: <integer>, height :: <integer>) => ();
  function default-space-requirement
    (sheet :: <abstract-sheet>,
     #key width, min-width, max-width, height, min-height, max-height, #all-keys)
 => (space-req :: <space-requirement>);
  function invalidate-space-requirements (sheet :: <abstract-sheet>) => ();
end protocol <<layout-protocol>>;


/// Default methods for layout protocol

define method compose-space
    (pane :: <sheet>, #key width, height) => (space-req :: <space-requirement>)
  let space-req = do-compose-space(pane, width: width, height: height);
  //--- Maybe remove this if the efficiency loss is too huge...
  let (w, w-, w+, h, h-, h+) = space-requirement-components(pane, space-req);
  assert("Space requirement components are all integers",
	   instance?(w, <integer>) & instance?(w-, <integer>) & instance?(w+, <integer>)
	 & instance?(h, <integer>) & instance?(h-, <integer>) & instance?(h+, <integer>));
  space-req
end method compose-space;

define method allocate-space
    (pane :: <sheet>, width :: <integer>, height :: <integer>) => ()
  do-allocate-space(pane, width, height);
  sheet-layed-out?(pane) := #t
end method allocate-space;

//--- This seems dubious...
define method do-allocate-space
    (pane :: <sheet>, width :: <integer>, height :: <integer>) => ()
  ignore(width, height);
  #f
end method do-allocate-space;


/// Layout Mixin

define open abstract class <layout-mixin> (<layout>)
end class <layout-mixin>;

define open abstract class <leaf-layout-mixin> (<layout-mixin>)
end class <leaf-layout-mixin>;

define open abstract class <composite-layout-mixin> (<layout-mixin>)
end class <composite-layout-mixin>;

// When the region of a sheet gets changed during layout, we want to
// re-layout its children to conform to the new geometry
define method relayout-children
    (pane :: <composite-layout-mixin>) => ()
  let (width, height) = box-size(pane);
  allocate-space(pane, width, height)
end method relayout-children;

define method sheet-layed-out-to-size?
    (pane :: <composite-layout-mixin>, width :: <integer>, height :: <integer>)
 => (layed-out? :: <boolean>)
  let (old-width, old-height) = box-size(pane);
  sheet-layed-out?(pane)
  & width = old-width & height = old-height
end method sheet-layed-out-to-size?;


/// Space Requirement Mixin

// This class manages a space requirement object for panes that don't
// have a 'compose-space' method.  It's most useful with <layout-mixin>.
define open abstract class <space-requirement-mixin> (<abstract-sheet>)
  sealed slot pane-space-requirement :: false-or(<space-requirement>) = #f,
    init-keyword: space-requirement:;
end class <space-requirement-mixin>;

define method pane-space-requirement (sheet :: <sheet>) => (space-req)
  #f
end method pane-space-requirement;

define method initialize
    (pane :: <space-requirement-mixin>, #rest initargs,
     #key space-requirement,
	  width, min-width, max-width, height, min-height, max-height)
  dynamic-extent(initargs);
  ignore(width, min-width, max-width, height, min-height, max-height);
  next-method();
  let space-req = space-requirement | apply(default-space-requirement, pane, initargs);
  pane-space-requirement(pane) := space-req
end method initialize;

define method validate-sheet-size
    (pane :: <space-requirement-mixin>, width :: <integer>, height :: <integer>) => ()
  let space-req = pane-space-requirement(pane);
  when (space-req)
    let (w, w-, w+, h, h-, h+) = space-requirement-components(pane, space-req);
    ignore(w, w+, h, h+);
    when (width < w- | height < h-)
      warn("Resizing sheet %= to be too small -- %dX%d, not %dX%d",
	   pane, width, height, w-, h-)
    end
  end
end method validate-sheet-size;

define method do-compose-space
    (pane :: <space-requirement-mixin>, #rest keys, #key width, height)
 => (space-req :: <space-requirement>)
  dynamic-extent(keys);
  ignore(width, height);
  pane-space-requirement(pane) 
  | apply(default-space-requirement, pane, keys)
end method do-compose-space;

define method default-space-requirement
    (sheet :: <sheet>,
     #key width, min-width, max-width, height, min-height, max-height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | $default-sheet-size, height: height | $default-sheet-size,
       min-width: min-width | 0,           min-height: min-height | 0,
       max-width: max-width | $fill,       max-height: max-height | $fill)
end method default-space-requirement;

// Resets and decaches the space requirement for a single pane
define method reset-space-requirement
    (pane :: <sheet>,
     #key space-requirement :: false-or(<space-requirement>) = #f) => ()
  ignore(space-requirement);
  #f
end method reset-space-requirement;

define method reset-space-requirement
    (pane :: <space-requirement-mixin>,
     #key space-requirement :: false-or(<space-requirement>) = #f) => ()
  pane-space-requirement(pane) := space-requirement
end method reset-space-requirement;


/// Cached Space Requirements

// If both <cached-space-requirement-mixin> and <space-requirement-mixin>
// appear in the same CPL, then <cached-space-requirement-mixin> must
// precede <space-requirement-mixin>
define open abstract class <cached-space-requirement-mixin> (<abstract-sheet>)
  // Contains a triple of [space-req, width, height]
  sealed slot %space-requirement-cache = #f;
end class <cached-space-requirement-mixin>;

define method validate-sheet-size
    (pane :: <cached-space-requirement-mixin>, width :: <integer>, height :: <integer>) => ()
  let cache = pane.%space-requirement-cache;
  let space-req = (cache & cache[0]) | pane-space-requirement(pane);
  when (space-req)
    let (w, w-, w+, h, h-, h+) = space-requirement-components(pane, space-req);
    ignore(w, w+, h, h+);
    when (width < w- | height < h-)
      warn("Resizing sheet %= to be too small -- %dX%d, not %dX%d",
	   pane, width, height, w-, h-)
    end
  end
end method validate-sheet-size;

// Yes, this really is a wrapper on 'compose-space', not on 'DO-compose-space'.
// It's here so we can try using the cache before doing 'do-compose-space'
define method compose-space
    (pane :: <cached-space-requirement-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let cache = pane.%space-requirement-cache;
  without-bounds-checks
    if (cache & (~width | width = cache[1]) & (~height | height = cache[2]))
      cache[0]
    else
      let space-req = next-method();
      unless (cache)
	cache := vector(#f, #f, #f);
	pane.%space-requirement-cache := cache
      end;
      cache[0] := space-req;
      cache[1] := width;
      cache[2] := height;
      space-req
    end
  end
end method compose-space;

define method sheet-layed-out?-setter
    (layed-out? == #f, pane :: <cached-space-requirement-mixin>)
 => (layed-out? :: <boolean>)
  pane.%space-requirement-cache := #f;
  next-method()
end method sheet-layed-out?-setter;

// Reset an entire sheet hierarchy to an un-layed-out state
define method invalidate-space-requirements (sheet :: <sheet>) => ()
  local method invalidate (sheet :: <sheet>) => ()
	  sheet-layed-out?(sheet) := #f
	end method;
  do-sheet-tree(invalidate, sheet)
end method invalidate-space-requirements;

define method reset-space-requirement
    (pane :: <cached-space-requirement-mixin>,
     #key space-requirement :: false-or(<space-requirement>) = #f)
  ignore(space-requirement);
  next-method();
  pane.%space-requirement-cache := #f
end method reset-space-requirement;


/// Client Overridability

// Bits 15..16  of 'sheet-flags' are reserved for fixed space requirements
define constant %fixed_width  :: <integer> = #o100000;
define constant %fixed_height :: <integer> = #o200000;

// The idea here is that a user can specify an "overriding" space req for
// a pane.  Any non-#f component in the space req is used to override the
// space requirement that would normally be used for the pane.
// Note that this needs to precede some class, such as <layout-mixin>, that
// implements a method for 'compose-space'
define open abstract class <client-overridability-mixin> (<abstract-sheet>)
  sealed slot %override-space-requirement :: false-or(<space-requirement>) = #f;
end class <client-overridability-mixin>;

define method initialize
    (pane :: <client-overridability-mixin>,
     #key space-requirement,
	  width, min-width, max-width, height, min-height, max-height,
          resizable? = #t, 
          fixed-width? = ~resizable?, fixed-height? = ~resizable?)
  next-method();
  let bits = logior(if (fixed-width?)  %fixed_width  else 0 end,
		    if (fixed-height?) %fixed_height else 0 end);
  sheet-flags(pane) := logior(sheet-flags(pane), bits);
  when (space-requirement
	| width | min-width | max-width | height | min-height | max-height)
    pane.%override-space-requirement
      := space-requirement
         | make(<space-requirement>,
		width:  width,  min-width:  min-width,  max-width:  max-width,
		height: height, min-height: min-height, max-height: max-height)
  end
end method initialize;

define sealed inline method sheet-force-fixed-width?
    (pane :: <client-overridability-mixin>) => (fixed-width? :: <boolean>)
  logand(sheet-flags(pane), %fixed_width) = %fixed_width
end method sheet-force-fixed-width?;

define sealed inline method sheet-force-fixed-height?
    (pane :: <client-overridability-mixin>) => (fixed-height? :: <boolean>)
  logand(sheet-flags(pane), %fixed_height) = %fixed_height
end method sheet-force-fixed-height?;

define inline function constrain-size
    (preferred-size :: <integer>, min-size :: <integer>, max-size :: <integer>)
 => (preferred-size :: <integer>)
  max(min-size, min(max-size, preferred-size))
end function constrain-size;

// Yes, this really is a wrapper on 'compose-space', not on 'DO-compose-space'
// Note that 'compose-space' constrains the width and height to be within the
// newly defined bounds.
define method compose-space
    (pane :: <client-overridability-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let fixed-width?  = sheet-force-fixed-width?(pane);
  let fixed-height? = sheet-force-fixed-height?(pane);
  let space-req
    = next-method(pane,
		  // Don't take top-down advice for fixed width or height
		  width:  ~fixed-width?  & width,
		  height: ~fixed-height? & height);
  let override-space-req = pane.%override-space-requirement;
  if (override-space-req | fixed-width? | fixed-height?)
    let (w, w-, w+, h, h-, h+)
      = space-requirement-components(pane, space-req);
    let (ow, ow-, ow+, oh, oh-, oh+)
      = space-requirement-components(pane, override-space-req | space-req);
    let nmin-width  = ow- | w-;
    let nmax-width  = ow+ | w+;
    let nmin-height = oh- | h-;
    let nmax-height = oh+ | h+;
    let nwidth      = constrain-size(ow | w, nmin-width,  nmax-width);
    let nheight     = constrain-size(oh | h, nmin-height, nmax-height);
    let (best-width, min-width, max-width)
      = if (fixed-width?)
	  values(nwidth, nwidth, nwidth)
	else
	  values(nwidth, nmin-width, nmax-width)
	end;
    let (best-height, min-height, max-height)
      = if (fixed-height?)
	  values(nheight, nheight, nheight)
	else
	  values(nheight, nmin-height, nmax-height)
	end;
    make(<space-requirement>,
	 width:  best-width,  min-width:  min-width,  max-width:  max-width,
	 height: best-height, min-height: min-height, max-height: max-height)
  else
    space-req
  end
end method compose-space;


/// Wrapping Layout Mixin

// This class gets used when the pane in question uses exactly the same
// space requirements as the "sum" of its children's requirements.  Viewport
// panes are good examples of this.
define open abstract class <wrapping-layout-mixin> (<composite-layout-mixin>)
end class <wrapping-layout-mixin>;

define method do-compose-space
    (pane :: <wrapping-layout-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let children = sheet-children(pane);
  case
    empty?(children) =>
      default-space-requirement(pane, width: width, height: height);
    size(children) = 1 =>	// optimize a very common case...
      compose-space(children[0], width: width, height: height);
    otherwise =>
      let (w :: <integer>, w- :: <integer>, w+ :: <integer>,
	   h :: <integer>, h- :: <integer>, h+ :: <integer>)
	= space-requirement-components(pane, compose-space(children[0]));
      for (child :: <basic-sheet> in children)
	unless (sheet-withdrawn?(child))
	  let (srw :: <integer>, srw- :: <integer>, srw+ :: <integer>,
	       srh :: <integer>, srh- :: <integer>, srh+ :: <integer>)
	    = space-requirement-components(child, compose-space(child));
	  let (x, y) = values(0, 0);	//--- sheet-position(child)...
	  max!(w,  srw  + x);
	  max!(h,  srh  + y);
	  max!(w-, srw- + x);
	  max!(h-, srh- + y);
	  max!(w+, srw+ + x);
	  max!(h+, srh+ + y)
	end
      end;
      let w = constrain-size(width  | w, w-, w+);
      let h = constrain-size(height | h, h-, h+);
      make(<space-requirement>,
           width:  w, min-width:  w-, max-width:  w+,
           height: h, min-height: h-, max-height: h+);
  end
end method do-compose-space;

define method do-allocate-space
    (pane :: <wrapping-layout-mixin>, width :: <integer>, height :: <integer>) => ()
  let children = sheet-children(pane);
  if (size(children) = 1)
    //--- Do we also want to set the position to (0,0)?
    let child :: <basic-sheet> = children[0];
    set-sheet-size(child, width, height)
  else
    for (child :: <basic-sheet> in children)
      unless (sheet-withdrawn?(child))
	let space-req = compose-space(child);
	let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
	ignore(w-, w+, h-, h+);
	set-sheet-size(child, w, h)
      end
    end
  end
end method do-allocate-space;


/// Generally useful layout function
/// Used all over to satisfy constraints


// This supa dupa version works by calculating the sizes required
// by the ratios and then constrains these sizes by the max and min
// sizes. Depending on whether the result is larger or smaller than
// the overall required size, the items that cannot be adjusted
// in the right direction to help the fit are then fixed at their
// limiting size. The algorithm then loops back trying to fit the
// remaining items into the remaining space.
//
// SHEET is the sheet on whose behalf the composition is being done.
// DESIRED-SIZE is the desired size for all of the items, and SPACE-REQ
// is a space requirement that describes the items' parent.  The three
// size/min/max functions pull apart space requirements.  ITEMS is a
// sequence of items (sheets or space requirements), and ITEM-COMPOSER
// generates a space requirement from an item.  (For example, when ITEMS
// is a set of sheets, this function is 'compose-space'; when ITEMS is a
// set of space reqs, this function is 'identity'.)  
define function compose-space-for-items
    (sheet :: <sheet>,
     desired-size :: <integer>, space-req :: <space-requirement>, items :: <sequence>, 
     size-function :: <function>, min-function :: <function>, max-function :: <function>,
     item-composer :: <function>, #key ratios)
 => (sizes :: <simple-object-vector>)
  let n-items :: <integer> = size(items);
  let sizes :: <simple-object-vector> = make(<simple-vector>, size: n-items);
  let desired-ratios :: <simple-object-vector> = make(<simple-vector>, size: n-items);
  let sized? :: limited(<vector>, of: <boolean>, size: n-items) = make(limited(<vector>, of: <boolean>, size: n-items));
  let constrained-size :: <integer> = 0;
  let ratio-denominator :: <integer> = 0;
  // Calculate ratios and note withdrawn items
  for (item in items,
       index :: <integer> from 0)
    let child = sheet?(item) & item;
    if (child & sheet-withdrawn?(child))
      sizes[index] := 0;
      sized?[index] := #t;
    else
      let ratio = (ratios & index < size(ratios) & ratios[index]) | 1;
      desired-ratios[index] := ratio;
      ratio-denominator := ratio-denominator + ratio
    end
  end;
  let done? :: <boolean> = #f;
  // Loop until constraints satisfied
  let size-left :: <integer> = desired-size;
  until (done?)
    // Calculate desired sizes and note violators
    let constrained-size :: <integer> = 0;
    let constraining-mins :: <list> = #();
    let constraining-maxs :: <list> = #();
    for (item in items,
         item-sized? in sized?,
         ratio in desired-ratios,
         index :: <integer> from 0)
      unless (item-sized?)
        let child = sheet?(item) & item;
        let item-sr = item-composer(item);
        let desired-item-size :: <integer> = truncate/(size-left * ratio, ratio-denominator);
        let item-max = max-function(child | sheet, item-sr);
        let item-min = min-function(child | sheet, item-sr);
        let constrained-item-size :: <integer> = desired-item-size;
        case
          (desired-item-size < item-min) =>
            constraining-mins := pair(index, constraining-mins);
            constrained-item-size := item-min;
          (desired-item-size > item-max) =>
            constraining-maxs := pair(index, constraining-maxs);
            constrained-item-size := item-max;
          otherwise => #f;
        end;
        sizes[index] := constrained-item-size;
        constrained-size := constrained-size + constrained-item-size;
      end
    end;
    // Nail down the ones that can't resize in the right direction to help fit
    case
      ((constrained-size < size-left) & ~empty?(constraining-maxs)) =>
        for (index in constraining-maxs)
          ratio-denominator := ratio-denominator - desired-ratios[index];
          size-left := size-left - sizes[index];
          sized?[index] := #t;
        end;
      ((constrained-size > size-left) & ~empty?(constraining-mins)) => 
        for (index in constraining-mins)
          ratio-denominator := ratio-denominator - desired-ratios[index];
          size-left := size-left - sizes[index];
          sized?[index] := #t;
        end;
      otherwise =>
        done? := #t;
    end
  end;
  sizes
end function compose-space-for-items;


/// The basic layout pane

// The internal layout panes (boxes, tables) are all built on this.
define open abstract class <layout-pane>
    (<cached-space-requirement-mixin>,
     <client-overridability-mixin>,
     <composite-layout-mixin>,
     <multiple-child-mixin>,
     <basic-sheet>)
end class <layout-pane>;


/// Horizontal and vertical layout mixins

define open abstract class <horizontal-layout-mixin> (<layout>)
  sealed slot layout-x-spacing :: <integer> = 0,
    init-keyword: x-spacing:;
  // The sequence here allows different alignments for each row in a table
  sealed slot layout-y-alignment :: type-union(<sequence>, <y-alignment>) = #"top",
    init-keyword: y-alignment:;
  sealed slot layout-x-ratios :: false-or(<sequence>) = #f,
    init-keyword: x-ratios:;
end class <horizontal-layout-mixin>;

define method initialize
    (pane :: <horizontal-layout-mixin>,
     #key spacing = $unsupplied, ratios = $unsupplied)
  next-method();
  when (supplied?(spacing))
    layout-x-spacing(pane) := spacing
  end;
  when (supplied?(ratios))
    layout-x-ratios(pane) := ratios
  end;
end method initialize;


define open abstract class <vertical-layout-mixin> (<layout>)
  sealed slot layout-y-spacing :: <integer> = 0,
    init-keyword: y-spacing:;
  // The sequence here allows different alignments for each row in a table
  sealed slot layout-x-alignment :: type-union(<sequence>, <x-alignment>) = #"left",
    init-keyword: x-alignment:;
  sealed slot layout-y-ratios :: false-or(<sequence>) = #f,
    init-keyword: y-ratios:;
end class <vertical-layout-mixin>;

define method initialize
    (pane :: <vertical-layout-mixin>,
     #key spacing = $unsupplied, ratios = $unsupplied)
  next-method();
  when (supplied?(spacing))
    layout-y-spacing(pane) := spacing
  end;
  when (supplied?(ratios))
    layout-y-ratios(pane) := ratios
  end;
end method initialize;


/// Layout borders

define open abstract class <layout-border-mixin> (<layout>)
  sealed slot layout-border :: <integer> = 0,
    init-keyword: border:;
end class <layout-border-mixin>;


/// Alignment hacking

define method sheet-interpret-alignment
    (sheet :: <sheet>, alignment :: <symbol>, key)
  alignment
end method sheet-interpret-alignment;

define method sheet-interpret-alignment
    (sheet :: <sheet>, alignment :: <collection>, key)
  alignment[key]
end method sheet-interpret-alignment;

define method sheet-x-alignment
    (sheet :: <sheet>, alignment, left :: <integer>, right :: <integer>, #key key)
 => (x-position :: <integer>)
  select (sheet-interpret-alignment(sheet, alignment, key))
    #"left"  => left;
    #"right" => right;
    #"center", #"centre" => floor/(left + right, 2);
  end
end method sheet-x-alignment;

define method sheet-y-alignment
    (sheet :: <sheet>, alignment, top :: <integer>, bottom :: <integer>, #key key)
 => (y-position :: <integer>)
  select (sheet-interpret-alignment(sheet, alignment, key))
    #"top"    => top;
    #"bottom" => bottom;
    #"center", #"centre" => floor/(top + bottom, 2);
  end
end method sheet-y-alignment;

// Returns the aligned Y value for a sheet
define method layout-align-sheet-x
    (layout :: <vertical-layout-mixin>, sheet :: <sheet>,
     left :: <integer>, right :: <integer>, #key key)
  sheet-x-alignment(sheet, layout-x-alignment(layout), left, right, key: key)
end method layout-align-sheet-x;

// Returns the aligned Y value for a sheet
define method layout-align-sheet-y
    (layout :: <horizontal-layout-mixin>, sheet :: <sheet>,
     top :: <integer>, bottom :: <integer>, #key key)
  sheet-y-alignment(sheet, layout-y-alignment(layout), top, bottom, key: key)
end method layout-align-sheet-y;


/// Fixed layouts

// Fixed layouts can have any number of children, but there's no layout
// policy at all -- the kids worry about their own geometry
define open abstract class <fixed-layout> (<layout-pane>)
end class <fixed-layout>;

define method do-compose-space 
    (pane :: <fixed-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  default-space-requirement(pane, width: width, height: height)
end method do-compose-space;

define method do-allocate-space
    (pane :: <fixed-layout>, width :: <integer>, height :: <integer>) => ()
  ignore(width, height);
  #f
end method do-allocate-space;


/// Default implementation

define sealed class <fixed-layout-pane> (<fixed-layout>)
  keyword accepts-focus?: = #f;
end class <fixed-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <fixed-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<fixed-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<fixed-layout-pane>));
define sealed domain initialize (<fixed-layout-pane>);


/// Pinboards

// Pinboards are like fixed layouts, except that they enforce
// the space constraints of their children.
define open abstract class <pinboard-layout> (<layout-pane>)
end class <pinboard-layout>;

define method do-compose-space 
    (pane :: <pinboard-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  default-space-requirement(pane, width: width, height: height)
end method do-compose-space;

define method do-allocate-space
    (pane :: <pinboard-layout>, width :: <integer>, height :: <integer>) => ()
  ignore(width, height);
  for (child :: <basic-sheet> in sheet-children(pane))
    unless (sheet-withdrawn?(child))
      let space-req = compose-space(child);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
      ignore(w-, w+, h-, h+);
      set-sheet-size(child, w, h)
    end
  end
end method do-allocate-space;


/// Default implementation

define sealed class <pinboard-layout-pane> (<pinboard-layout>)
  keyword accepts-focus?: = #f;
end class <pinboard-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <pinboard-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<pinboard-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<pinboard-layout-pane>));
define sealed domain initialize (<pinboard-layout-pane>);


/// Stacks

// Stack layouts position all of their children at the top-left
// one on top of the other. They are primarily useful for creating
// tab-controls or wizards where only one child is visible at a time.
define open abstract class <stack-layout> 
    (<layout-border-mixin>,
     <layout-pane>)
end class <stack-layout>;

define protocol <<stack-layout>> ()
  getter stack-layout-mapped-page
    (stack :: <stack-layout>)
 => (page :: false-or(<abstract-sheet>));
  setter stack-layout-mapped-page-setter
    (page :: false-or(<abstract-sheet>), stack :: <stack-layout>)
 => (page :: false-or(<abstract-sheet>));
end protocol <<stack-layout>>;

define method initialize
    (pane :: <stack-layout>, #key mapped-page :: false-or(<sheet>)) => ()
  next-method();
  let mapped-page
    = mapped-page
        | begin
	    let children = sheet-children(pane);
	    ~empty?(children) & children[0]
	  end;
  stack-layout-mapped-page(pane) := mapped-page
end method initialize;

//--- Children get added withdrawn, and need to be mapped using
//--- stack-layout-mapped-page-setter.
define method note-child-added
    (sheet :: <stack-layout>, child :: <sheet>) => ()
  next-method();
  sheet-withdrawn?(child) := #t
end method note-child-added;

define method do-compose-space 
    (pane :: <stack-layout>,
     #key width: requested-width, height: requested-height)
 => (space-req :: <space-requirement>)
  let children = sheet-children(pane);
  if (empty?(children))
    default-space-requirement(pane, width: requested-width, height: requested-height)
  else
    let border*2 = layout-border(pane) * 2;
    let extra-width  :: <integer> = border*2;
    let extra-height :: <integer> = border*2;
    let child-width  = requested-width  & (requested-width  - extra-width);
    let child-height = requested-height & (requested-height - extra-height);
    let width      :: <integer> = 0;
    let height     :: <integer> = 0;
    let min-width  :: <integer> = 0;
    let min-height :: <integer> = 0;
    let max-width  :: <integer> = 0;
    let max-height :: <integer> = 0;
    for (child in children)
      let space-req
	= compose-space(child, width: child-width, height: child-height);
      let (w, w-, w+, h, h-, h+)
	= space-requirement-components(child, space-req);
      max!(width,      w);
      max!(min-width,  w-);
      max!(max-width,  w+);
      max!(height,     h);
      max!(min-height, h-);
      max!(max-height, h+)
    end;
    inc!(width,      extra-width);
    inc!(min-width,  extra-width);
    inc!(max-width,  extra-width);
    inc!(height,     extra-height);
    inc!(min-height, extra-height);
    inc!(max-height, extra-height);
    let width  = requested-width  | width;
    let height = requested-height | height;
    let best-width  = constrain-size(width,  min-width,  max-width);
    let best-height = constrain-size(height, min-height, max-height);
    make(<space-requirement>,
	 width:  best-width,  min-width:  min-width,  max-width:  max-width,
	 height: best-height, min-height: min-height, max-height: max-height)
  end
end method do-compose-space;

define method do-allocate-space
    (pane :: <stack-layout>, width :: <integer>, height :: <integer>) => ()
  let border   = layout-border(pane);
  let border*2 = border * 2;
  let width  = width  - border*2;
  let height = height - border*2;
  for (child in sheet-children(pane))
    let space-req = compose-space(child, width: width, height: height);
    let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
    ignore(w-, w+, h-, h+);
    set-sheet-edges(child, border, border, border + w, border + h)
  end
end method do-allocate-space;


/// Default implementation

define sealed class <stack-layout-pane> (<stack-layout>)
  keyword accepts-focus?: = #f;
end class <stack-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <stack-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<stack-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<stack-layout-pane>));
define sealed domain initialize (<stack-layout-pane>);

define sealed method stack-layout-mapped-page
    (stack :: <stack-layout>)
 => (page :: false-or(<sheet>))
  block (return)
    for (child :: <basic-sheet> in sheet-children(stack))
      unless (sheet-withdrawn?(child))
	return(child)
      end
    end
  end
end method stack-layout-mapped-page;

define sealed method stack-layout-mapped-page-setter
    (page :: false-or(<sheet>), stack :: <stack-layout>)
 => (page :: false-or(<sheet>))
  let damaged-width  :: <integer> = 0;
  let damaged-height :: <integer> = 0;
  let old-page = #f;
  // Be conservative and ensure that we start with everything withdrawn
  for (child :: <basic-sheet> in sheet-children(stack))
    if (~sheet-withdrawn?(child))
      old-page := child;
      if (child ~= page)
	sheet-withdrawn?(child, do-repaint?: #f) := #t;
	let (child-width, child-height) = sheet-size(child);
	max!(damaged-width, child-width);
	max!(damaged-height, child-height);
      end
    end
  end;
  when (page & page ~= old-page)
    // Un-withdraw the new child so that we can do layout, if necessary
    sheet-withdrawn?(page) := #f;
    when (sheet-attached?(stack))
      let (width, height) = sheet-size(stack);
      // Note that this apparent re-layout will be quite inexpensive
      // because we've already layed out all the pages in the stack
      let space-req
	= compose-space(page, width: width, height: height);
      let (w, w-, w+, h, h-, h+)
	= space-requirement-components(page, space-req);
      ignore(w-, w+, h-, h+);
      // We repaint if the child isn't mirrored, or if the old damaged
      // region extends beyond the new child's own region.
      if (~sheet-direct-mirror(page) | damaged-width > w | damaged-height > h)
	clear-box(stack, 0, 0, damaged-width, damaged-height)
      end;
      set-sheet-edges(page, 0, 0, w, h);
      //---*** Can this be removed if we fix 'set-sheet-edges'?
      update-all-mirror-positions(page);
      // Now we can finally display the new child!
      sheet-mapped?(page, clear?: #f) := sheet-mapped?(stack);
    end
  end;
  page
end method stack-layout-mapped-page-setter;
