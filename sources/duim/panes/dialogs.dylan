Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of dialogs

define constant $ok-label     = "OK";
define constant $cancel-label = "Cancel";
define constant $help-label   = "Help";
define constant $apply-label  = "Apply";
define constant $back-label   = "< Back";
define constant $next-label   = "Next >";
define constant $finish-label = "Finish";

define protocol <<default-dialog-protocol>> ()
  function default-dialog-border
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>);
  function default-dialog-extra-size
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (width :: <integer>, height :: <integer>);
  function default-dialog-spacing
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (spacing :: <integer>);
  function default-dialog-button-spacing
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (spacing :: <integer>);
  function default-dialog-button-x-alignment
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (alignment :: <x-alignment>);
  function default-dialog-button-y-alignment
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (alignment :: <y-alignment>);
  function dialog-needs-title-pane?
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (needs-title-pane? :: <boolean>);
  function dialog-needs-separator?
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (needs-separator? :: <boolean>);
  function make-top-level-drawing-pane
    (framem :: <frame-manager>, children :: <sequence>)
 => (sheet :: <sheet>);
  function make-exit-box
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (layout :: false-or(<layout-pane>));
  function make-exit-buttons
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (buttons :: <sequence>);
  function make-exit-button
    (framem :: <frame-manager>, dialog :: <dialog-frame>,
     callback :: false-or(<callback-type>), label :: <string>,
     #rest initargs, #key, #all-keys)
 => (button :: false-or(<push-button>));
end protocol <<default-dialog-protocol>>;

define pane <dialog-top-level-layout> ()
  slot dialog-user-layout :: false-or(<sheet>),
    init-keyword: user-layout:;
end pane <dialog-top-level-layout>;

define method default-dialog-extra-size
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (width :: <integer>, height :: <integer>)
  values(0, 0)
end method default-dialog-extra-size;

define method do-compose-space
    (layout :: <dialog-top-level-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  let frame = sheet-frame(layout);
  let (extra-width, extra-height)
    = default-dialog-extra-size(frame-manager(frame), frame);
  let child-width  = width  & (width  - extra-width);
  let child-height = height & (height - extra-height);
  let space-req
    = next-method(layout, width: child-width, height: child-height);
  if (extra-width = 0 & extra-height = 0)
    space-req
  else
    space-requirement+(layout, space-req,
		       width: extra-width, height: extra-height)
  end
end method do-compose-space;

define method do-allocate-space
    (layout :: <dialog-top-level-layout>, width :: <integer>, height :: <integer>) => ()
  let frame = sheet-frame(layout);
  let (extra-width, extra-height)
    = default-dialog-extra-size(frame-manager(frame), frame);
  next-method(layout, width - extra-width, height - extra-height)
end method do-allocate-space;

define constant $dialog-text-style
    = make(<text-style>, weight: #"bold", size: 14);

define method default-dialog-frame-wrapper
    (framem :: <frame-manager>, dialog :: <dialog-frame>, layout :: false-or(<sheet>))
 => (sheet :: false-or(<sheet>))
  with-frame-manager (framem)
    let children :: <stretchy-object-vector> = make(<stretchy-vector>);
    let drawing-children :: <stretchy-object-vector> = make(<stretchy-vector>);
    let title = frame-title(dialog);
    let exit-box = make-exit-box(framem, dialog);
    let spacing = default-dialog-spacing(framem, dialog);
    when (title & dialog-needs-title-pane?(framem, dialog))
      add!(drawing-children,
           make(<label-pane>,
                label: title,
                text-style: $dialog-text-style))
    end;
    when (layout | exit-box)
      let position = dialog-exit-buttons-position(dialog);
      let button-orientation
        = select (position)
            #"top", #"bottom" => #"vertical";
	    #"left", #"right" => #"horizontal";
	    otherwise         => #"vertical";
          end;
      let layout-children :: <stretchy-object-vector> = make(<stretchy-vector>);
      when (layout) add!(layout-children, layout) end;
      when (exit-box & dialog-needs-separator?(framem, dialog))
	let separator-orientation
	  = select (button-orientation)
	      #"horizontal" => #"vertical";
	      #"vertical"   => #"horizontal";
	    end;
	add!(layout-children, 
             make(<separator>, orientation: separator-orientation))
      end;
      when (exit-box) add!(layout-children, exit-box) end;
      when (position = #"left" | position = #"top") reverse!(layout-children) end;
      add!(drawing-children,
           select (button-orientation)
             #"horizontal" =>
               make(<row-layout>,
                    spacing: spacing,
                    y-alignment: #"center",
                    max-height: $fill,
                    children: layout-children);
             #"vertical" =>
               make(<column-layout>,
                    spacing: spacing,
                    x-alignment: #"center",
                    max-width: $fill,
                    children: layout-children);
           end)
    end;
    unless (empty?(drawing-children))
      add!(children,
           make(<spacing>,
                child: make-top-level-drawing-pane(framem, drawing-children),
                spacing: default-dialog-border(framem, dialog)))
    end;
    make(<dialog-top-level-layout>,
         user-layout: layout,
         child: unless (empty?(children))
                  make(<column-layout>,
                       spacing: spacing,
                       children: children)
                end)
  end
end method default-dialog-frame-wrapper;

define method default-dialog-border
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  8
end method default-dialog-border;

define method default-dialog-spacing
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  4
end method default-dialog-spacing;

define method default-dialog-button-spacing
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  4
end method default-dialog-button-spacing;

define method default-dialog-button-x-alignment
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (x-alignment :: <x-alignment>)
  #"right"
end method default-dialog-button-x-alignment;

define method default-dialog-button-y-alignment
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (y-alignment :: <y-alignment>)
  #"top"
end method default-dialog-button-y-alignment;

define method dialog-needs-title-pane?
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (needs-title-pane? :: <boolean>)
  #f
end method dialog-needs-title-pane?;

define method dialog-needs-separator?
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (needs-separator? :: <boolean>)
  #f
end method dialog-needs-separator?;

define method make-top-level-drawing-pane
    (framem :: <frame-manager>, children :: <sequence>)
 => (sheet :: <sheet>)
  make(<column-layout>,
       y-spacing: 6,
       x-alignment: #"center",
       children: children)
end method make-top-level-drawing-pane;

define method update-default-dialog-layout
    (framem :: <frame-manager>, frame :: <dialog-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-layout = sheet-child(top-sheet);
    let old-layout = dialog-user-layout(top-layout);
    if (old-layout)
      let parent = sheet-parent(old-layout);
      let new-layout = frame-layout(frame);
      replace-child(parent, old-layout, new-layout);
      dialog-user-layout(top-layout) := new-layout;
      relayout-parent(new-layout);
      sheet-mapped?(new-layout) := frame-mapped?(frame)
    else
      //---*** What do you do if there wasn't a layout before?
      #f
    end
  end
end method update-default-dialog-layout;

// Makes a layout containing all of the exit buttons
define method make-exit-box 
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (layout :: false-or(<layout-pane>))
  when (dialog-exit-buttons-position(dialog))	// if no position, no exit box...
    with-frame-manager (framem)
      let children = make-exit-buttons(framem, dialog);
      let help-callback = dialog-help-callback(dialog);
      let help-button 
	= dialog-help-button(dialog)
	  | when (help-callback)
	      make-exit-button(framem, dialog, help-callback, $help-label)
	    end;
      when (help-button)
	add!(children, help-button);
	dialog-help-button(dialog) := help-button
      end;
      let spacing = default-dialog-button-spacing(framem, dialog);
      unless (empty?(children))
	select (dialog-exit-buttons-position(dialog))      
	  #"top", #"bottom" =>
	    let button-layout
	      = make(<row-layout>,
		     spacing: spacing,
		     equalize-widths?: #t, equalize-heights?: #t,
		     children: children);
	    make(<column-layout>,
		 children: vector(button-layout),
		 x-alignment: default-dialog-button-x-alignment(framem, dialog),
		 max-width: $fill);
	  #"left", #"right" =>
	    let button-layout
	      = make(<column-layout>,
		     spacing: spacing,
		     equalize-widths?: #t, equalize-heights?: #t,
		     children: children);
	    make(<row-layout>,
		 children: vector(button-layout),
		 y-alignment: default-dialog-button-y-alignment(framem, dialog),
		 max-height: $fill);
	end
      end
    end
  end
end method make-exit-box;

// Returns a sequence of all the exit buttons
define method make-exit-buttons
    (framem :: <frame-manager>, dialog :: <dialog-frame>)
 => (buttons :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  let exit-enabled? = dialog-exit-enabled?(dialog);
  let exit-button 
    = dialog-exit-button(dialog)
      | (dialog-exit-callback(dialog)
	 & make-exit-button(framem, dialog, dialog-exit-callback(dialog), $ok-label, 
			    enabled?: exit-enabled?));
  when (exit-button)
    add!(children, exit-button);
    dialog-exit-button(dialog) := exit-button;
    when (~frame-default-button(dialog) & exit-enabled?)
      frame-default-button(dialog) := exit-button
    end
  end;
  let cancel-button 
    = dialog-cancel-button(dialog)
      | (dialog-cancel-callback(dialog)
	 & make-exit-button(framem, dialog, dialog-cancel-callback(dialog), $cancel-label));
  when (cancel-button)
    add!(children, cancel-button);
    dialog-cancel-button(dialog) := cancel-button
  end;
  children
end method make-exit-buttons;

// Returns a single exit button
define method make-exit-button
    (framem :: <frame-manager>, dialog :: <dialog-frame>,
     callback :: false-or(<callback-type>), label :: <string>,
     #rest initargs, 
     #key enabled? = (callback ~= #f), #all-keys)
 => (button :: false-or(<push-button>))
  when (callback)
    with-frame-manager (framem)
      apply(make, <push-button>,
	    activate-callback: method (button)
				 let dialog = sheet-frame(button);
				 execute-callback(dialog, callback, dialog)
			       end,
	    label: label,
	    enabled?: enabled?,
	    initargs)
    end
  end
end method make-exit-button;


/// Property frames

define method make-exit-buttons
    (framem :: <frame-manager>, dialog :: <property-frame>)
 => (buttons :: <sequence>)
  let children = next-method();
  let apply-callback = dialog-apply-callback(dialog);
  let apply-button 
    = make-exit-button(framem, dialog, apply-callback, $apply-label);
  when (apply-button)
    add!(children, apply-button);
    dialog-apply-button(dialog) := apply-button
  end;
  children  
end method make-exit-buttons;

define method property-frame-tab-control
    (dialog :: <property-frame>)
 => (tab-control :: false-or(<tab-control>))
  frame-layout(dialog)
end method property-frame-tab-control;

define sideways method dialog-current-page
    (dialog :: <property-frame>) => (page :: false-or(<sheet>))
  tab-control-current-page(property-frame-tab-control(dialog))
end method dialog-current-page;

define sideways method dialog-current-page-setter
    (page :: false-or(<sheet>), dialog :: <property-frame>)
 => (page :: false-or(<sheet>))
  tab-control-current-page(property-frame-tab-control(dialog)) := page
end method dialog-current-page-setter;


/// Wizards

define method dialog-needs-separator?
    (framem :: <frame-manager>, dialog :: <wizard-frame>)
 => (needs-separator? :: <boolean>)
  #t
end method dialog-needs-separator?;

define method make-exit-buttons
    (framem :: <frame-manager>, dialog :: <wizard-frame>)
 => (buttons :: <sequence>)
  let next-callback = dialog-next-callback(dialog);
  let back-callback = dialog-back-callback(dialog);
  let pages = dialog-pages(dialog);
  let multiple-pages? = (size(pages) > 1);
  let exit-enabled?   = dialog-exit-enabled?(dialog);
  let cancel-callback = dialog-cancel-callback(dialog) | cancel-dialog;
  let exit-callback   = dialog-exit-callback(dialog)   | exit-dialog;
  let cancel-button
    = make-exit-button(framem, dialog, cancel-callback, $cancel-label);
  let back-button
    = when (multiple-pages?)
	make-exit-button(framem, dialog, back-callback, $back-label,
                         enabled?: #f)
      end;
  let next-button
    = when (multiple-pages?)
	make-exit-button(framem, dialog, next-callback, $next-label)
      end;
  let exit-button
    = make-exit-button(framem, dialog, exit-callback, $finish-label,
		       //---*** How do we handle enabling of this button?
		       // enabled?: exit-enabled?,
		       withdrawn?: multiple-pages?);
  let buttons
    = if (multiple-pages?)
        vector(back-button, next-button, exit-button, cancel-button)
      else
        vector(exit-button, cancel-button)
      end;
  dialog-cancel-button(dialog) := cancel-button;
  dialog-exit-button(dialog) := exit-button;
  dialog-back-button(dialog) := back-button;
  dialog-next-button(dialog) := next-button;
  buttons
end method make-exit-buttons;

define sideways method dialog-current-page
    (dialog :: <wizard-frame>) => (page :: false-or(<sheet>))
  let pages = dialog-pages(dialog);
  block (return)
    for (page in pages)
      unless (sheet-withdrawn?(page))
        return(page)
      end
    end
  end
end method dialog-current-page;

define sideways method dialog-current-page-setter
    (new-page :: false-or(<sheet>), dialog :: <wizard-frame>)
 => (new-page :: false-or(<sheet>))
  for (page in dialog-pages(dialog))
    unless (page == new-page)
      sheet-withdrawn?(page) := #t
    end
  end;
  when (new-page)
    let parent = sheet-parent(new-page);
    sheet-withdrawn?(new-page) := #f;
    let mapped? = parent & sheet-mapped?(parent);
    when (mapped?)
      relayout-parent(new-page);
      sheet-mapped?(new-page) := #t
    end;
    let frame = sheet-frame(new-page);
    when (frame & page-initial-focus(new-page))
      frame-input-focus(frame) := page-initial-focus(new-page)
    end
  end;
  debug-assert(dialog-current-page(dialog) == new-page,
               "Internal error: failed to set the current page for %=",
               dialog);
  new-page
end method dialog-current-page-setter;

//---*** This is disgusting, there must be a way to avoid using sideways
define sideways method initialize
    (dialog :: <wizard-frame>, #key page, frame-manager: framem) => ()
  unless (frame-layout(dialog))
    //---*** There must be an easier way than this...
    let framem
      = framem
          | frame-manager(dialog)
          | port-default-frame-manager(default-port());
    with-frame-manager (framem)
      let pages = dialog-pages(dialog);
      let image = dialog-image(dialog);
      let stack-layout = make(<stack-layout>, children: pages);
      let layout
	= if (image)
	    //--- This spacing is just a guess!
	    let spacing = 2 * default-dialog-button-spacing(framem, dialog);
	    let label = make(<label>, label: image);
	    let border
	      = with-border (type: #"sunken")
	          label
	        end;
	    make(<row-layout>,
		 children: vector(border, stack-layout),
		 y-alignment: #"center",
		 spacing: spacing)
	  else
	    stack-layout
	  end;
      frame-layout(dialog) := layout;
      for (page in pages)
	sheet-withdrawn?(page) := #t
      end
    end
  end;
  next-method();
end method initialize;
