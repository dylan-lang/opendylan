Module:    environment-debugger
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <DEBUGGER-SETTINGS>

define settings <debugger-settings> (<open-dylan-user-settings>)
  key-name "Debugger";
  slot one-debugger-per-thread    :: <boolean> = #f;
  slot notifier-dialog            :: <boolean> = #t;
  slot expand-backtrace-initially :: <boolean> = #t;
  slot expand-first-frame         :: <boolean> = #t;
  slot refresh-all-on-debug       :: <boolean> = #f;
  slot open-debugger-on-pause     :: <boolean> = #t;
  slot stack-include              :: <string>  = "";
  slot stack-exclude              :: <string>  = "";
  slot stack-modules              :: <symbol>  = #"all";
  slot stack-show-dylan-calls     :: <boolean> = #t;
  slot stack-show-internal-calls  :: <boolean> = #f;
  slot stack-show-foreign-calls   :: <boolean> = #f;
  slot stack-show-cleanup-frames  :: <boolean> = #f;
  slot stack-show-unknown-frames  :: <boolean> = #f;
  slot bug-report-template        :: <string>
    = release-bug-report-template-location();

  // These should really be in the window settings
  slot stack-pane-ratio      :: <integer> = 1;
  slot source-pane-ratio     :: <integer> = 1;
  slot stack-source-ratio    :: <integer> = 4;
  slot interactor-pane-ratio :: <integer> = 3;
end settings <debugger-settings>;

define constant $debugger-settings = make(<debugger-settings>);

define function stack-show-frame-types
    (settings :: <settings>) => (types :: <sequence>)
  remove
    (vector
       (if (settings.stack-show-dylan-calls)    #"dylan-call"    end,
	if (settings.stack-show-internal-calls) #"internal-call" end,
	if (settings.stack-show-foreign-calls)  #"foreign-call"  end,
	if (settings.stack-show-cleanup-frames) #"cleanup"       end,
	if (settings.stack-show-unknown-frames) #"unknown"       end),
     #f)
end function stack-show-frame-types;

/// <OPTIONS-MISC-PAGE> (internal)

define sealed pane <options-misc-page> ()
  pane options-one-debugger-per-thread-button (dialog)
    make(<check-button>,
	 label: "Use a new debugger for each thread.",
	 value: $debugger-settings.one-debugger-per-thread,
	 documentation: "Does not allow debuggers to be recycled.");
  pane options-notifier-dialog-button (dialog)
    make(<check-button>,
	 label: "Confirm before entering the debugger after an error.",
	 value: $debugger-settings.notifier-dialog,
	 documentation: "Displays a dialog to confirm entering the debugger.");
  pane options-expand-stack-initially-button (dialog)
    make(<check-button>,
	 label: "Expand stack backtrace when first opening debugger",
	 value: $debugger-settings.expand-backtrace-initially,
	 documentation: "Expands stack backtrace when debugger is first opened.");
  pane options-expand-first-frame-button (dialog)
    make(<check-button>,
	 label: "Expand first stack frame when refreshing debugger",
	 value: $debugger-settings.expand-first-frame,
	 documentation: "Expands first stack frame when debugger is refreshed.");
  pane options-refresh-all-on-debug-button (dialog)
    make(<check-button>,
	 label: "Refresh all open debuggers when entering debugger",
	 value: $debugger-settings.refresh-all-on-debug,
	 documentation: "Refreshes contents of all open debuggers instead of simply enabling them.");
  pane options-debug-on-user-requested-pause-button (dialog)
    make(<check-button>,
	 label: "Open debugger window on pause button",
	 value: $debugger-settings.open-debugger-on-pause,
	 documentation: "Opens debugger window for main thread when pause is requested.");
  layout (page)
    vertically () // hughg, 1998/03/10: Without this, the '$fill' doesn't work
      grouping ("Options", max-width: $fill)
	vertically (spacing: $vertical-spacing)
	  page.options-one-debugger-per-thread-button;
	  page.options-notifier-dialog-button;
	  page.options-expand-stack-initially-button;
	  page.options-expand-first-frame-button;
	  page.options-refresh-all-on-debug-button;
	  page.options-debug-on-user-requested-pause-button;
	end
      end
    end;
end pane;


/// OPTIONS-PAGE-NAME (internal)

define sealed method options-page-name 
    (page :: <options-misc-page>)
 => (name :: <string>)
  "Misc"
end method;


/// UPDATE-FROM-PAGE (internal)

define sealed method update-from-page 
    (debugger :: <debugger>, page :: <options-misc-page>)
 => ()
  $debugger-settings.one-debugger-per-thread
    := page.options-one-debugger-per-thread-button.gadget-value;
  $debugger-settings.notifier-dialog
    := page.options-notifier-dialog-button.gadget-value;
  $debugger-settings.expand-backtrace-initially
    := page.options-expand-stack-initially-button.gadget-value;
  $debugger-settings.expand-first-frame
    := page.options-expand-first-frame-button.gadget-value;
  $debugger-settings.refresh-all-on-debug
    := page.options-refresh-all-on-debug-button.gadget-value;
  $debugger-settings.open-debugger-on-pause
    := page.options-debug-on-user-requested-pause-button.gadget-value;
end method;


/// INITIALIZE-PAGE (internal)

define sealed method initialize-page 
    (debugger :: <debugger>, page :: <options-misc-page>)
 => ()
  //---*** andrewa: why do we initialize just this one gadget?
  page.options-notifier-dialog-button.gadget-value
    := $debugger-settings.notifier-dialog;
end method;


/// <OPTIONS-DIALOG> (internal)

define frame <options-dialog> (<dialog-frame>)
  keyword title: = "Debugger Options";
  pane options-misc-page (dialog)
    make(<options-misc-page>);
  pane options-stack-page (dialog)
    make(<options-stack-page>);
  pane options-stop-reason-page (dialog)
    make(<options-stop-reason-page>);
  layout (dialog)
    make(<tab-control>,
	 pages: map(method (page)
		      make(<property-page>,
			   child: page,
			   label: options-page-name(page))
		    end method,
		    vector(dialog.options-stack-page,
			   dialog.options-stop-reason-page,
			   dialog.options-misc-page)));
end frame;


/// INITIALIZE (dylan)
///
/// Introduce new initialize protocol so that stack-page
/// can initialize itself from the debugger.

define sealed method initialize (dialog :: <options-dialog>, #key)
 => ()
  next-method();
  let debugger :: <debugger> = dialog.frame-owner;
  for (page :: <property-page> in dialog.frame-layout.tab-control-pages)
    let inner-page = page.sheet-children[0];
    initialize-page(debugger, inner-page);
  end for;
end method;

define sealed domain make (singleton(<options-dialog>));
define sealed domain initialize (<options-dialog>);


/// UPDATE-FROM-DIALOG (internal)

define function update-from-dialog (debugger :: <debugger>, dialog :: <options-dialog>)
 => ()
  for (page :: <property-page> in dialog.frame-layout.tab-control-pages)
    let inner-page = page.sheet-children[0];
    update-from-page(debugger, inner-page);
  end for;
end function;


/// UPDATE-FROM-PAGE (internal)

define sealed method update-from-page (object :: <object>, page :: <object>)
  => ()
end method;


/// INITIALIZE-PAGE (internal)

define sealed method initialize-page (object :: <object>, page :: <object>)
  => ()
end method;
