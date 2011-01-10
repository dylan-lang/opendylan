Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Dialog protocols

define protocol <<dialog-protocol>> ()
  getter dialog-image
    (dialog :: <frame>) => (image :: false-or(<image>));
  getter dialog-image-setter
    (image :: false-or(<image>), dialog :: <frame>)
 => (image :: false-or(<image>));
  getter dialog-exit-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-exit-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-cancel-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-cancel-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-apply-callback
    (dialog :: <frame>) => (callback :: false-or(<callback-type>));
  getter dialog-apply-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-apply-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-back-callback
    (dialog :: <frame>) => (callback :: false-or(<callback-type>));
  getter dialog-back-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-back-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-next-callback
    (dialog :: <frame>) => (callback :: false-or(<callback-type>));
  getter dialog-next-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-next-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-help-callback
    (dialog :: <frame>) => (callback :: false-or(<callback-type>));
  getter dialog-help-button
    (dialog :: <frame>) => (button :: false-or(<button>));
  setter dialog-help-button-setter
    (button :: false-or(<button>), dialog :: <frame>)
 => (button :: false-or(<button>));
  getter dialog-current-page
    (frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  setter dialog-current-page-setter
    (page :: false-or(<abstract-sheet>), frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  getter dialog-next-page
    (frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  setter dialog-next-page-setter
    (page :: false-or(<abstract-sheet>), frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  getter dialog-previous-page
    (frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  setter dialog-previous-page-setter
    (page :: false-or(<abstract-sheet>), frame :: <frame>) => (page :: false-or(<abstract-sheet>));
  getter dialog-pages
    (frame :: <frame>) => (pages :: <sequence>);
  setter dialog-pages-setter
    (pages :: <sequence>, frame :: <frame>) => (pages :: <sequence>);
  getter dialog-page-complete?
    (frame :: <frame>) => (complete? :: <boolean>);
  setter dialog-page-complete?-setter
    (complete? :: <boolean>, frame :: <frame>) => (complete? :: <boolean>);
end protocol <<dialog-protocol>>;

define protocol <<wizard-protocol>> (<<dialog-protocol>>)
  function compute-next-page
    (dialog :: <wizard-frame>) => (next-page :: false-or(<sheet>));
  function compute-previous-page
    (dialog :: <wizard-frame>) => (prev-page :: false-or(<sheet>));
end protocol <<wizard-protocol>>;


/// Dialogs

define open abstract primary class <dialog-frame> (<simple-frame>)
  // Exit button
  sealed slot dialog-exit-callback :: false-or(<function>) = exit-dialog,
    init-keyword: exit-callback:;
  sealed slot dialog-exit-button :: false-or(<button>) = #f,
    init-keyword: exit-button:;
  sealed slot dialog-exit-enabled? :: <boolean> = #t,
    init-keyword: exit-enabled?:,
    setter: %exit-enabled?-setter;
  // Cancel button
  sealed slot dialog-cancel-callback :: false-or(<function>) = cancel-dialog,
    init-keyword: cancel-callback:;
  sealed slot dialog-cancel-button :: false-or(<button>) = #f,
    init-keyword: cancel-button:;
  // Help button
  sealed slot dialog-help-callback :: false-or(<function>) = #f,
    init-keyword: help-callback:;
  sealed slot dialog-help-button :: false-or(<button>) = #f,
    init-keyword: help-button:;
  // Position, or #f meaning "don't lay out any exit buttons"
  sealed slot dialog-exit-buttons-position :: false-or(<symbol>) = #"bottom",
    init-keyword: exit-buttons-position:;
  // By default, dialogs are modal
  keyword mode: = #"modal";
end class <dialog-frame>;

define sealed class <concrete-dialog-frame> (<dialog-frame>)
end class <concrete-dialog-frame>;

define sealed domain make (singleton(<concrete-dialog-frame>));
define sealed domain initialize (<concrete-dialog-frame>);

define sealed inline method make
    (class == <dialog-frame>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-dialog-frame>)
  apply(make, <concrete-dialog-frame>, initargs)
end method make;

define method initialize
    (dialog :: <dialog-frame>,
     #key exit-buttons? = #t, save-under? = #f,
	  minimize-box? = #f, maximize-box? = #f) => ()
  next-method();
  frame-flags(dialog)
    := logior(logand(frame-flags(dialog),
		     lognot(logior(%frame_minimize_box, %frame_maximize_box))),
	      if (minimize-box?) %frame_minimize_box else 0 end,
	      if (maximize-box?) %frame_maximize_box else 0 end);
  unless (exit-buttons?)
    // If the user asked for no exit buttons, just claim their position is #f.
    // We do this because the user might want to lay out this own exit buttons,
    // but still need 'dialog-exit-button' and 'dialog-cancel-button' to be set up.
    dialog-exit-buttons-position(dialog) := #f
  end
end method initialize;


/// Starting dialogs

define method start-dialog
    (dialog :: <dialog-frame>)
 => (#rest values :: <object>)
  start-frame(dialog)
end method start-dialog;

define method start-frame
    (dialog :: <dialog-frame>)
 => (status-code :: false-or(<integer>))
  let owner  = frame-owner(dialog);
  let modal? = (frame-mode(dialog) == #"modal");
  if (owner & modal?)
    // If this is a modal dialog, the owning frame needs to be disabled
    // for the duration of the dialog
    let old-state = frame-enabled?(owner);
    block ()
      frame-enabled?(owner) := #f;
      next-method();
    cleanup
      frame-enabled?(owner) := old-state
    end
  else
    next-method()
  end
end method start-frame;


/// Exiting dialogs

define method exit-dialog
    (dialog :: <dialog-frame>, #key destroy? = #t) => ()
  let _port = port(dialog);
  when (_port)
    distribute-event(_port, make(<frame-exit-event>,
				 frame: dialog,
				 destroy-frame?: destroy?));
    let top-sheet = top-level-sheet(dialog);
    when (top-sheet)
      generate-trigger-event(_port, top-sheet)
    end
  end
end method exit-dialog;

define method exit-dialog
    (sheet :: <sheet>, #key destroy? = #t) => ()
  let dialog = sheet-frame(sheet);
  exit-dialog(dialog, destroy?: destroy?)
end method exit-dialog;

define open generic do-exit-dialog 
    (framem :: <abstract-frame-manager>, dialog :: <dialog-frame>, #key destroy?) => ();

define method handle-event
    (dialog :: <dialog-frame>, event :: <frame-exit-event>) => ()
  when (dialog-exit-enabled?(dialog))
    do-exit-dialog(frame-manager(dialog), dialog, destroy?: event-destroy-frame?(event))
  end
end method handle-event;

define method handle-event
    (dialog :: <dialog-frame>, event :: <dialog-exit-event>) => ()
  do-exit-frame(frame-manager(dialog), dialog,
		destroy?: event-destroy-frame?(event), status-code: 0)
end method handle-event;

define method dialog-exit-enabled?-setter
    (enabled? :: <boolean>, dialog :: <dialog-frame>) => (true? :: <boolean>)
  let exit-button = dialog-exit-button(dialog);
  when (exit-button)
    let default-button = frame-default-button(dialog);
    case
      enabled? & ~default-button =>
        frame-default-button(dialog) := exit-button;
      ~enabled? & default-button = exit-button =>
        frame-default-button(dialog) := #f;
      otherwise =>
        #f
    end;
    gadget-enabled?(exit-button) := enabled?
  end;
  dialog.%exit-enabled? := enabled?
end method dialog-exit-enabled?-setter;


/// Canceling dialogs

define method cancel-dialog
    (dialog :: <dialog-frame>, #key destroy? = #t) => ()
  do-cancel-dialog(frame-manager(dialog), dialog, destroy?: destroy?)
end method cancel-dialog;

define method cancel-dialog
    (sheet :: <sheet>, #key destroy? = #t) => ()
  let dialog = sheet-frame(sheet);
  cancel-dialog(dialog, destroy?: destroy?)
end method cancel-dialog;

define open generic do-cancel-dialog 
    (framem :: <abstract-frame-manager>, dialog :: <dialog-frame>, #key destroy?) => ();

define method handle-event
    (dialog :: <dialog-frame>, event :: <dialog-cancel-event>) => ()
  do-exit-frame(frame-manager(dialog), dialog,
		destroy?: event-destroy-frame?(event))
end method handle-event;


/// Simple chooser

define sealed class <simple-chooser-dialog> (<dialog-frame>)
end class <simple-chooser-dialog>;

define sealed domain make (singleton(<simple-chooser-dialog>));
define sealed domain initialize (<simple-chooser-dialog>);

// Default implementation of dialog chooser when handed a sequence of items.
// This is "sideways" because it is a forward reference from DUIM-Sheets.
define sideways method do-choose-from-dialog
    (framem :: <frame-manager>, owner :: <sheet>, items :: <sequence>,
     #key title, value,
          selection-mode = #"single",
	  gadget-class = <list-box>, gadget-options = #[],
          label-key = collection-gadget-default-label-key,
          value-key = collection-gadget-default-value-key,
          width, height, foreground, background, text-style,
     #all-keys)
 => (value, success? :: <boolean>,
     width :: false-or(<integer>), height :: false-or(<integer>))
  let value
    = if (selection-mode == #"single") value | (~empty?(items) & items[0]) else value end;
  with-frame-manager (framem)
    let gadget
      = apply(make-pane, gadget-class,
	      items: items,
	      selection-mode: selection-mode,
	      title: title,
	      value: value,
	      label-key: label-key,
	      value-key: value-key,
	      activate-callback: exit-dialog,
	      foreground: foreground,
	      background: background,
	      text-style: text-style,
	      gadget-options);
    let dialog
      = make(<simple-chooser-dialog>,
	     mode: #"modal",
	     owner: sheet-frame(owner),
	     title: title,
	     layout: gadget,		// the gadget is the layout
	     input-focus: gadget,	// ensure the gadget has the focus
	     width:  if (width  = $fill) #f else width  | 300 end,
	     height: if (height = $fill) #f else height | 200 end);
    let status-code     = start-dialog(dialog);
    let (width, height) = frame-size(dialog);
    values(status-code & gadget-value(gadget),
	   status-code & #t,
	   width, height)
  end
end method do-choose-from-dialog;


/// Multi-page dialog

define open abstract primary class <multi-page-dialog-frame> (<dialog-frame>)
  // #f means the pages have yet to be initialized, so that
  // the 'pages' section of 'define frame' knows when it is done
  slot %pages :: false-or(<sequence>) = #f,
    init-keyword: pages:;
  slot dialog-page-changed-callback :: false-or(<callback-type>) = #f,
    init-keyword: page-changed-callback:;
end class <multi-page-dialog-frame>;
  
define open generic update-dialog-buttons 
    (dialog :: <multi-page-dialog-frame>) => ();

define open generic note-dialog-page-changed
    (dialog :: <multi-page-dialog-frame>) => ();

define method initialize
    (dialog :: <multi-page-dialog-frame>, #key page) => ()
  next-method();
  let pages = dialog-pages(dialog);
  when (~dialog-current-page(dialog) & ~empty?(pages))
    let new-page = page | pages[0];
    dialog-current-page(dialog) := new-page
  end;
  note-dialog-page-changed(dialog)
end method initialize;

define method dialog-pages
    (dialog :: <multi-page-dialog-frame>) => (pages :: <sequence>)
  dialog.%pages | #[]
end method dialog-pages;

define method dialog-pages-setter
    (pages :: <sequence>, dialog :: <multi-page-dialog-frame>)
 => (pages :: <sequence>)
  unless (pages = dialog-pages(dialog))
    dialog.%pages := pages
  end;
  pages
end method dialog-pages-setter;

define method note-dialog-page-changed
    (dialog :: <multi-page-dialog-frame>) => ()
  #f
end method note-dialog-page-changed;

define method dialog-next-button-visible?
    (dialog :: <multi-page-dialog-frame>) => (visible? :: <boolean>)
  let next-button = dialog-next-button(dialog);
  next-button & ~sheet-withdrawn?(next-button)
end method dialog-next-button-visible?;

define method dialog-exit-button-visible?
    (dialog :: <multi-page-dialog-frame>) => (visible? :: <boolean>)
  let exit-button = dialog-exit-button(dialog);
  exit-button & ~sheet-withdrawn?(exit-button)
end method dialog-exit-button-visible?;

define method dialog-page-complete?
    (dialog :: <multi-page-dialog-frame>) => (complete? :: <boolean>)
  case
    dialog-next-button-visible?(dialog) => dialog-next-enabled?(dialog);
    dialog-exit-button-visible?(dialog) => dialog-exit-enabled?(dialog);
    otherwise                           => #f;
  end
end method dialog-page-complete?;

define method dialog-page-complete?-setter
    (complete? :: <boolean>, dialog :: <multi-page-dialog-frame>)
 => (complete? :: <boolean>)
  let next-button-visible? = sheet-mapped?(dialog-next-button(dialog));
  let exit-button-visible? = sheet-mapped?(dialog-exit-button(dialog));
  when (dialog-next-button-visible?(dialog))
    dialog-next-enabled?(dialog) := complete?
  end;
  when (dialog-exit-button-visible?(dialog))
    dialog-exit-enabled?(dialog) := complete?
  end;
  complete?
end method dialog-page-complete?-setter;


/// Page changed callback

define sealed class <page-changed-dialog-event> (<frame-event>)
end class <page-changed-dialog-event>;

define sealed domain make (singleton(<page-changed-dialog-event>));
define sealed domain initialize (<page-changed-dialog-event>);

define sealed method handle-event
    (dialog :: <multi-page-dialog-frame>, event :: <page-changed-dialog-event>) => ()
  execute-page-changed-callback(dialog)
end method handle-event;

define function execute-page-changed-callback
    (dialog :: <dialog-frame>) => ()
  let callback = dialog-page-changed-callback(dialog);
  callback & execute-callback(dialog, callback, dialog)
end function execute-page-changed-callback;

define function distribute-page-changed-callback
    (dialog :: <multi-page-dialog-frame>) => ()
  distribute-event(port(dialog), 
		   make(<page-changed-dialog-event>, frame: dialog))
end function distribute-page-changed-callback;


/// Property frames

define open primary class <property-frame> 
    (<multi-page-dialog-frame>)
  sealed slot dialog-apply-callback :: false-or(<callback-type>) = #f,
    init-keyword: apply-callback:;
  sealed slot dialog-apply-button :: false-or(<button>) = #f,
    init-keyword: apply-button:;
  // By default, property sheets are modeless
  keyword mode: = #"modeless";
end class <property-frame>;

define method initialize
    (frame :: <property-frame>, #key frame-manager: framem) => ()
  let tab-control
    = make(<tab-control>,
	   pages: dialog-pages(frame),
	   value-changed-callback: note-property-frame-current-page-changed,
	   frame-manager: framem);
  frame-layout(frame) := tab-control;
  next-method()
end method initialize; 

define method note-property-frame-current-page-changed
    (gadget :: <tab-control>) => ()
  let dialog = sheet-frame(gadget);
  execute-page-changed-callback(dialog)
end method note-property-frame-current-page-changed;


/// Property pages

define open abstract class <property-page> (<basic-page>)
end class <property-page>;

define sealed class <property-page-pane>
    (<property-page>, <single-child-wrapping-pane>)
end class <property-page-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <property-page>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<property-page-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<property-page-pane>));
define sealed domain initialize (<property-page-pane>);


/// Wizard frames

define open primary class <wizard-frame> 
    (<multi-page-dialog-frame>)
  slot dialog-image :: false-or(<image>) = #f,
    init-keyword: image:;
  slot dialog-back-button :: false-or(<button>) = #f,
    init-keyword: back-button:;
  slot dialog-next-button :: false-or(<button>) = #f,
    init-keyword: next-button:;
  slot dialog-next-enabled? :: <boolean> = #f,
    init-keyword: next-enabled?:,
    setter: %next-enabled?-setter;
  constant slot dialog-next-callback :: false-or(<callback-type>) = move-to-next-page,
    init-keyword: next-callback:;
  constant slot dialog-back-callback :: false-or(<callback-type>) = move-to-previous-page,
    init-keyword: back-callback:;
  slot dialog-next-page :: false-or(<sheet>) = #f;
  slot dialog-previous-page :: false-or(<sheet>) = #f;
  // By default, wizards are modal
  keyword mode: = #"modal";
  keyword exit-enabled?: = #f;
end class <wizard-frame>;

define method dialog-next-enabled?-setter
    (enabled? :: <boolean>, dialog :: <wizard-frame>) => (true? :: <boolean>)
  let next-button = dialog-next-button(dialog);
  when (next-button)
    let default-button = frame-default-button(dialog);
    let back-button = dialog-back-button(dialog);
    case
      enabled? & ~default-button =>
        frame-default-button(dialog) := next-button;
      ~enabled? & default-button = next-button =>
        frame-default-button(dialog) := back-button;
      otherwise =>
        #f
    end;
    gadget-enabled?(next-button) := enabled?
  end;
  dialog.%next-enabled? := enabled?
end method dialog-next-enabled?-setter;

define method dialog-current-page-number
    (dialog :: <wizard-frame>)
 => (page-number :: false-or(<integer>))
  let page = dialog-current-page(dialog);
  when (page)
    let pages = dialog-pages(dialog);
    position(pages, page)
    | error("Wizard page %= not found within pages of %=", page, dialog)
  end
end method dialog-current-page-number;

define method update-dialog-buttons
    (dialog :: <wizard-frame>) => ()
  let back-button = dialog-back-button(dialog);
  let next-button = dialog-next-button(dialog);
  let exit-button = dialog-exit-button(dialog);
  let back-available? = (dialog-previous-page(dialog) ~= #f);
  let next-available? = (dialog-next-page(dialog) ~= #f);
  let exit-enabled? = (~next-available? | dialog-exit-enabled?(dialog));
  let exit-box = sheet-parent(back-button | next-button | exit-button);
  when (back-button)
    gadget-enabled?(back-button) := back-available?
  end;
  // We go through this rigamarole for the Next and Exit buttons
  // because they are stacked on top of each other, that is, you
  // never see them both at the same time
  when (next-button)
    if (next-available?)
      sheet-withdrawn?(next-button) := #f;
      sheet-mapped?(next-button)    := #t
    else
      sheet-withdrawn?(next-button) := #t
    end
  end;
  when (exit-button)
    if (exit-enabled?)
      sheet-withdrawn?(exit-button) := #f;
      sheet-mapped?(exit-button)    := #t
    else
      sheet-withdrawn?(exit-button) := #t
    end
  end;
  frame-default-button(dialog)
    := (next-available? & next-button) | (exit-enabled? & exit-button);
  // Re-layout the exit buttons, because if some were withdrawn before
  // we did any layout, they will be in the wrong place
  invalidate-space-requirements(exit-box);
  relayout-children(exit-box)
end method update-dialog-buttons;

define method note-dialog-page-changed
    (dialog :: <wizard-frame>) => ()
  let next-page = compute-next-page(dialog);
  dialog-next-page(dialog)     := next-page;
  dialog-previous-page(dialog) := compute-previous-page(dialog);
  dialog-exit-enabled?(dialog) := (next-page == #f)
end method note-dialog-page-changed;

define method compute-next-page
    (dialog :: <wizard-frame>) => (next-page :: false-or(<sheet>))
  let current-page-number = dialog-current-page-number(dialog);
  when (current-page-number)
    let next-page-number = current-page-number + 1;
    let pages = dialog-pages(dialog);
    when (next-page-number < size(pages))
      pages[next-page-number]
    end
  end
end method compute-next-page;

define method compute-previous-page
    (dialog :: <wizard-frame>) => (previous-page :: false-or(<sheet>))
  let current-page-number = dialog-current-page-number(dialog);
  when (current-page-number)
    let previous-page-number = current-page-number - 1;
    let pages = dialog-pages(dialog);
    when (previous-page-number >= 0)
      pages[previous-page-number]
    end
  end
end method compute-previous-page;

define method move-to-next-page
    (dialog :: <wizard-frame>) => ()
  let new-page = dialog-next-page(dialog);
  dialog-current-page(dialog) := new-page;
  note-dialog-page-changed(dialog);
  update-dialog-buttons(dialog);
  distribute-page-changed-callback(dialog)
end method move-to-next-page;

define method move-to-previous-page
    (dialog :: <wizard-frame>) => ()
  let new-page = dialog-previous-page(dialog);
  dialog-current-page(dialog) := new-page;
  note-dialog-page-changed(dialog);
  update-dialog-buttons(dialog);
  distribute-page-changed-callback(dialog)
end method move-to-previous-page;


/// Wizard pages

define open abstract class <wizard-page> (<basic-page>)
end class <wizard-page>;

define sealed class <wizard-page-pane>
    (<wizard-page>, <single-child-wrapping-pane>)
end class <wizard-page-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <wizard-page>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<wizard-page-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<wizard-page-pane>));
define sealed domain initialize (<wizard-page-pane>);
