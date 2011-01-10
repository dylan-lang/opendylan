Module:    environment-project-wizard
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Choice panes

define method choice-label
    (choice :: <choice>)
 => (label :: <string>)
  choice-object-label(choice-object(choice))
end method choice-label;

// This GF must be extended to cope with every kind of object which
// is to be displayed in a <choice-pane>
define generic choice-object-label
    (object) => (label :: <string>);

define sealed method choice-object-label
    (string :: <string>) => (label :: <string>)
  string
end method choice-object-label;

define sealed method choice-object-label
    (symbol :: <symbol>) => (label :: <string>)
  as(<string>, symbol)
end method choice-object-label;

define method choice-object-label
    (object :: <repository-object>) => (label :: <string>)
  repository-object-label(object)
end method choice-object-label;


define method choice-documentation
    (choice :: <choice>) => (documentation :: <string>)
  choice-object-documentation(choice-object(choice))
end method choice-documentation;

// This GF must be extended to cope with every kind of object which
// is to be displayed in a <choice-pane>
define generic choice-object-documentation
    (object) => (documentation :: <string>);

define sealed method choice-object-documentation
    (string :: <string>) => (documentation :: <string>)
  string
end method choice-object-documentation;

define sealed method choice-object-documentation
    (symbol :: <symbol>) => (documentation :: <string>)
  as(<string>, symbol)
end method choice-object-documentation;

define method choice-object-documentation
    (object :: <repository-object>) => (documentation :: <string>)
  repository-object-documentation(object)
end method choice-object-documentation;


// Access to choices

/*--- Not presently used
define method choice-pane-choices
    (pane :: <choice-pane>)
 => (choices :: <sequence>)
  gadget-items(choice-list-pane(pane))
end method choice-pane-choices;
*/

define method choice-pane-choices-setter
    (choices :: <sequence>, pane :: <choice-pane>)
 => (choices :: <sequence>)
  gadget-items(choice-list-pane(pane)) := choices;
  choices
end method choice-pane-choices-setter;


// The <choice-pane> class

define variable $check-mark-icon   = #f;
define variable $uncheck-mark-icon = #f;

define pane <choice-pane> ()
  constant slot choice-type-label :: <string>,
    required-init-keyword: choice-type-label:;
  slot next-pane :: false-or(<choice-pane>) = #f,
    init-keyword: next-pane:;
  slot previous-pane :: false-or(<choice-pane>) = #f,
    init-keyword: previous-pane:;

/*
  // This should be initialized to a collection of all items which might
  // ever be displayed here.  It will be replaced with the cached value of
  // the width of the largest of them.
  slot %choice-list-all-items :: false-or(<collection>),
    required-init-keyword: all-items:;
  slot %choice-column-width :: false-or(<integer>) = #f;
  slot %choice-label-height :: false-or(<integer>) = #f;
*/

  pane choice-list-pane (pane)
    make(<table-control>,
	 selection-mode: #"multiple",
	 always-show-selection?: #t,
	 headings:   vector(choice-type-label(pane)),
	 generators: vector(choice-label),
	 icon-function:
	   method (choice)
	     if (choice-included?(choice))
	       values($check-mark-icon,   $check-mark-icon)
	     else
	       values($uncheck-mark-icon, $uncheck-mark-icon)
	     end
	   end method,
	 //---*** andrewa: hack to make the controls the size of the headers!
	 width: 134, fixed-width?: #t,
	 widths: #[130],
	 value-changed-callback:	// Update gadgets below
	   method (gadget)
	     // Update contents and enabling of next pane, if any
	     update-next-choice-pane-list(pane)
	   end method,
	 activate-callback:		// Toggle "included" state
	   method (gadget)
	     let choice    = ~empty?(gadget-value(gadget)) & gadget-value(gadget)[0];
	     let included? = choice-included?(choice);
	     choice-included?(choice) := ~included?;
	     // Update contents and enabling of panes, if any
	     update-choice-pane-lists(pane)
	   end method);
  layout (pane)
    vertically (spacing: 4)
      pane.choice-list-pane;
    end;
  keyword choices:, type: false-or(<vector>), init-value: #f;
end pane <choice-pane>;

ignore(next-pane, next-pane-setter, previous-pane, previous-pane-setter);

define method choice-pane-enabled?-setter
    (enabled? :: <boolean>, pane :: <choice-pane>)
 => (enabled? :: <boolean>)
  gadget-enabled?(choice-list-pane(pane)) := enabled?;
  enabled?
end method choice-pane-enabled?-setter;

define method initialize
    (pane :: <choice-pane>, #key choices)
  next-method();
  choice-pane-choices(pane)  := choices | #[];
  choice-pane-enabled?(pane) := choices & #t;
end method initialize;


// Updating list panes within the choice pane

define method update-choice-pane-lists
    (pane :: <choice-pane>) => ()
  update-gadget(choice-list-pane(pane));
  update-choice-pane-check-marks(pane);
  update-next-choice-pane-list(pane);
end method update-choice-pane-lists;

define method update-choice-pane-check-marks
    (pane :: <choice-pane>) => ()
  let list = choice-list-pane(pane);
  for (choice in gadget-items(list))
    let item = find-item(list, choice);
    item-icon(item)
      := if (choice-included?(choice)) $check-mark-icon else $uncheck-mark-icon end
  end
end method update-choice-pane-check-marks;

define method update-next-choice-pane-list
    (pane :: <choice-pane>) => ()
  local method only-one-choice? (choices :: <sequence>)
          //--- Hack to work around bug in multi-select table controls
          //--- Should just be 'size(choices) == 1'
	  when (~empty?(choices))
	    block (return)
	      let choice1 = choices[0];
	      for (choice in choices)
	        unless (choice == choice1) return(#f) end
	      end;
	      #t
	    end
	  end
	end method;
  let next = next-pane(pane);
  when (next)
    let choices = gadget-value(choice-list-pane(pane));
    let (documentation, items, enabled?)
      = if (only-one-choice?(choices))
	  // Exactly one item selected, so this pane can show documentation
	  // and next pane can show children
	  let choice = choices[0];
	  let children = choice & choice-children(choice);
	  let included? = choice & choice-included?(choice);
	  values(choice-documentation(choice),
		 children | #[], children & included?)
	else
	  values("", #[], #f)
	end;
    choice-pane-choices(next)  := items;
    choice-pane-enabled?(next) := enabled?;
    update-gadget(choice-list-pane(next));
    update-choice-pane-check-marks(next);
    // And propagate to "next again"
    update-next-choice-pane-list(next);
  end
end method update-next-choice-pane-list;
