Module:    win32-environment
Synopsis:  Win32 Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Icon and bitmap initialization

define variable *bitmaps-initialized?* :: <boolean> = #f;

define macro initialize-bitmap
  { initialize-bitmap(?bitmap:name, ?resource-id:expression) }
    => { let _id     = as(<byte-string>, ?resource-id);
	 let _bitmap = read-image-as(<win32-bitmap>, _id, #"bitmap");
	 when (_bitmap)
	   ?bitmap := _bitmap
	 end }
end macro initialize-bitmap;

define macro initialize-icon
  { initialize-icon(?size:expression, ?icon:name, ?resource-id:expression) }
    => { let _id   = as(<byte-string>, ?resource-id);
	 let _icon
	   = select (?size)
	       #"small" => read-image-as(<win32-icon>, _id, #"small-icon");
	       #"large" => read-image-as(<win32-icon>, _id, #"large-icon");
	       #"16x16" => read-image-as(<win32-icon>, _id, #"icon",
					 width: 16, height: 16);
	       #"32x32" => read-image-as(<win32-icon>, _id, #"icon",
					 width: 32, height: 32);
	     end;
	 when (_icon)
	   ?icon := _icon
	 end }
end macro initialize-icon;

define function initialize-bitmaps ()
  unless (*bitmaps-initialized?*)
    // Initialize the check mark bitmap
    //---*** hughg, 1998/11/02: This one really belongs in DUIM, but andrewa
    //---*** agrees this'll do for now (for the playground dialog).
    $check-bitmap := read-image-as(<win32-bitmap>, $OBM-CHECK, #"bitmap",
				   resource-context: #"system");

    // Initialize the splash screen
    initialize-bitmap($splash-screen-bitmap, "SPLASHSCREEN");
    initialize-bitmap($about-box-bitmap,     "ABOUT");

    // Initialize the 16x16 tool-bar icons for Environment-Framework
    initialize-icon(#"16x16", $page-setup-bitmap,            "PAGESETUP");
    initialize-icon(#"16x16", $print-bitmap,                 "PRINT");
    initialize-icon(#"16x16", $undo-bitmap,                  "UNDO");
    initialize-icon(#"16x16", $redo-bitmap,                  "REDO");
    initialize-icon(#"16x16", $cut-bitmap,                   "CUT");
    initialize-icon(#"16x16", $copy-bitmap,                  "COPY");
    initialize-icon(#"16x16", $paste-bitmap,                 "PASTE");
    initialize-icon(#"16x16", $find-bitmap,                  "FIND");
    initialize-icon(#"16x16", $replace-and-find-next-bitmap, "REPLACE");
    initialize-icon(#"16x16", $find-next-bitmap,             "FINDNEXT");
    initialize-icon(#"16x16", $find-previous-bitmap,         "FINDPREVIOUS");
    initialize-icon(#"16x16", $back-bitmap,                  "BACK");
    initialize-icon(#"16x16", $forward-bitmap,               "FORWARD");
    initialize-icon(#"16x16", $home-bitmap,                  "HOME");
    initialize-icon(#"16x16", $help-bitmap,                  "HELP");
    initialize-icon(#"16x16", $interact-bitmap,              "INTERACT");
    initialize-icon(#"16x16", $debug-bitmap,                 "DEBUG");
    initialize-icon(#"16x16", $new-bitmap,                   "NEW");
    initialize-icon(#"16x16", $open-bitmap,                  "OPEN");
    initialize-icon(#"16x16", $save-bitmap,                  "SAVE");
    initialize-icon(#"16x16", $save-all-bitmap,              "SAVEALL");

    // Initialize the 32x32 icons for the initial dialog
    initialize-icon(#"32x32", $product-large-bitmap,      "AAA");
    initialize-icon(#"32x32", $tutorial-large-bitmap,     "TUTORIAL");
    initialize-icon(#"32x32", $examples-large-bitmap,     "EXAMPLES");
    initialize-icon(#"32x32", $playground-large-bitmap,   "PLAYGROUND");
    initialize-icon(#"32x32", $project-file-large-bitmap, "AAC");
    initialize-icon(#"32x32", $dylan-file-large-bitmap,   "DYLANFILE");
    initialize-icon(#"32x32", $text-file-large-bitmap,    "NEWTEXT");
    initialize-icon(#"32x32", $open-large-bitmap,         "OPEN");

    // Initialize the 16x16 tool-bar icons for Environment-Tools
    initialize-icon(#"16x16", $build-bitmap,            "BUILD");
    initialize-icon(#"16x16", $clone-bitmap,            "CLONE");
    initialize-icon(#"16x16", $compile-bitmap,          "COMPILECHANGES");
    initialize-icon(#"16x16", $edit-source-bitmap,      "EDITSOURCE");
    initialize-icon(#"16x16", $examples-bitmap,         "EXAMPLES");
    initialize-icon(#"16x16", $tutorial-bitmap,         "TUTORIAL");
    initialize-icon(#"16x16", $load-bitmap,             "LOAD");
    initialize-icon(#"16x16", $new-project-file-bitmap, "AAC");
    initialize-icon(#"16x16", $new-text-file-bitmap,    "NEWTEXT");
    initialize-icon(#"16x16", $pause-bitmap,            "PAUSE");
    initialize-icon(#"16x16", $playground-bitmap,       "PLAYGROUND");
    initialize-icon(#"16x16", $profile-bitmap,          "PROFILE");
    initialize-icon(#"16x16", $run-bitmap,              "PLAY");
    initialize-icon(#"16x16", $stop-bitmap,             "STOP");

    // Initialize the 16x16 tree control bitmaps
    
    initialize-icon(#"16x16", $application-bitmap,    "APPLICATION");
    initialize-icon(#"16x16", $class-bitmap,      "CLASS");
    initialize-icon(#"16x16", $constant-bitmap,   "CONSTANT");
    initialize-icon(#"16x16", $default-bitmap,    "DEFAULT");
    initialize-icon(#"16x16", $definition-bitmap, "DEFINITION");
    initialize-icon(#"16x16", $dylan-file-bitmap, "DYLANFILE");
    initialize-icon(#"16x16", $canonical-source-bitmap, "CANONICAL");
    initialize-icon(#"16x16", $current-source-bitmap,   "CURRENT");
    initialize-icon(#"16x16", $error-bitmap,      "ERROR");
    initialize-icon(#"16x16", $foreign-bitmap,    "FOREIGN");
    initialize-icon(#"16x16", $function-bitmap,   "FUNCTION");
    initialize-icon(#"16x16", $generic-bitmap,    "GENERIC");
    initialize-icon(#"16x16", $library-bitmap,    "LIBRARY");
    initialize-icon(#"16x16", $macro-bitmap,      "MACRODEF");
    initialize-icon(#"16x16", $method-bitmap,     "METHOD");
    initialize-icon(#"16x16", $module-bitmap,     "MODULE");
    initialize-icon(#"16x16", $object-bitmap,     "OBJECT");
    initialize-icon(#"16x16", $project-bitmap,    "PROJECT");
    initialize-icon(#"16x16", $restart-bitmap,    "RESTART");
    initialize-icon(#"16x16", $serious-warning-bitmap, "SERIOUSWARNING");
    initialize-icon(#"16x16", $slot-bitmap,       "SLOT");
    initialize-icon(#"16x16", $stack-frame-bitmap,    "STACKFRAME");
    initialize-icon(#"16x16", $text-file-bitmap,  "TEXTFILE");
    initialize-icon(#"16x16", $threads-bitmap,    "THREADS");
    initialize-icon(#"16x16", $unbound-bitmap,    "UNBOUND");
    initialize-icon(#"16x16", $variable-bitmap,   "VARIABLE");
    initialize-icon(#"16x16", $warning-bitmap,    "WARNING");
    initialize-icon(#"16x16", $clients-folder-bitmap, "CLIENTS");
    initialize-icon(#"16x16", $uses-folder-bitmap,    "USES");

    // Initialize the 16x16 tool-bar icons for Environment-Debugger
    initialize-icon(#"16x16", $step-over-bitmap,       "STEPOVER");
    initialize-icon(#"16x16", $step-into-bitmap,       "STEPINTO");
    initialize-icon(#"16x16", $step-out-bitmap,        "STEPOUT");
    initialize-icon(#"16x16", $top-of-stack-bitmap,    "TOPOFSTACK");
    initialize-icon(#"16x16", $bottom-of-stack-bitmap, "BOTTOMOFSTACK");
    initialize-icon(#"16x16", $up-stack-bitmap,        "UPSTACK");
    initialize-icon(#"16x16", $down-stack-bitmap,      "DOWNSTACK");

    // Initialize the 16x16 bitmaps for Deuce
    initialize-icon(#"16x16", $potential-breakpoint-image, "POTENTIALBREAKPOINT");
    initialize-icon(#"16x16", $enabled-breakpoint-image,   "ENABLEDBREAKPOINT");
    initialize-icon(#"16x16", $disabled-breakpoint-image,  "DISABLEDBREAKPOINT");
    initialize-icon(#"16x16", $step-breakpoint-image,      "STEPBREAKPOINT");
    initialize-icon(#"16x16", $test-breakpoint-image,      "TESTBREAKPOINT");
    initialize-icon(#"16x16", $enabled-tracepoint-image,   "ENABLEDTRACEPOINT");
    initialize-icon(#"16x16", $disabled-tracepoint-image,  "DISABLEDTRACEPOINT");
    initialize-icon(#"16x16", $profile-point-image,        "PROFILEPOINT");
    initialize-icon(#"16x16", $current-location-image,     "CURRENTLOCATION");
    initialize-icon(#"16x16", $prompt-image,               "PROMPT");
    initialize-icon(#"16x16", $values-image,               "VALUES");
    initialize-icon(#"16x16", $warning-image,              "WARNING");
    initialize-icon(#"16x16", $serious-warning-image,      "SERIOUSWARNING");

    // Initialize the window icons.
    // ---*** Change debugger and browser icons when new ones are checked in.
    initialize-icon(#"small", $main-window-small-icon,     "AAA");
    initialize-icon(#"small", $project-window-small-icon,  "AAC");
    initialize-icon(#"small", $browser-window-small-icon,  "BROWSER");
    initialize-icon(#"small", $editor-window-small-icon,   "AAB");
    initialize-icon(#"small", $debugger-window-small-icon, "DEBUGGER");
    initialize-icon(#"small", $describer-window-small-icon, "DEFINITION");
    initialize-icon(#"small", $find-window-small-icon,     "FIND");

    // Internal error box
    initialize-icon(#"32x32", $internal-error-bitmap, "IERROR");

    // Check mark icon
    initialize-icon(#"16x16", $check-mark-icon,   "CHECKMARK");
    initialize-icon(#"16x16", $uncheck-mark-icon, "UNCHECKMARK");

    initialize-standard-images();
    // All done
    *bitmaps-initialized?* := #t
  end
end function initialize-bitmaps;


/// Deuce initialization

define function initialize-deuce ()
  local method make-deuce-color (color) => (deuce-color)
	  let (r, g, b) = color-rgb(color);
	  deuce/make-color(floor(r * 255.0), floor(g * 255.0), floor(b * 255.0))
	end method;
  $region-marking-color        := make-deuce-color($default-face-color);
  $dylan-definition-line-color := make-deuce-color($default-shadow-color)
end function initialize-deuce;


/// Editor initialization

define function initialize-editors ()
  //--- It would be nice to register Emacs and some DDE-based editor, too
  register-editor-class(<deuce-editor>);
  // Initial default editor for the Win32 environment is Deuce
  current-editor()
    := find-editor-of-class(<deuce-editor>)
end function initialize-editors;


/// Source control initialization

define function initialize-source-control ()
  #f
end function initialize-source-control;


/// Icon-for-file-type

//---*** Should we cache this somehow, or does Windows do that for us?
define function icon-for-file
    (locator :: <file-locator>, #key icon-size :: one-of(#"small", #"large"))
 => (icon :: false-or(<win32-icon>))
  let filename = as(<string>, locator);
  with-stack-structure (file-info :: <LPSHFILEINFOA>)
    let options
      = %logior($SHGFI-ICON,
		select (icon-size)
		  #"small" => $SHGFI-SMALLICON;
		  #"large" => $SHGFI-LARGEICON;
		end);
    with-c-string (c-string = filename)
      SHGetFileInfo(c-string, 0, file-info, size-of(<SHFILEINFO>), options);
    end;
    let (width, height)
      = select (icon-size)
          #"small" => values($SM-CXSMICON, $SM-CYSMICON);
          #"large" => values($SM-CXICON,   $SM-CYICON);
        end;
    let handle = file-info.hIcon-value;
    unless (null-pointer?(handle))
      make(<win32-icon>,
	   resource-id: "none",
	   handle: file-info.hIcon-value,
	   width: width, height: height)
    end
  end
end function icon-for-file;

define sideways method environment-object-small-icon
    (project :: <project-object>, locator :: <file-locator>)
  icon-for-file(locator, icon-size: #"small")
    | next-method()
end method environment-object-small-icon;

define sideways method environment-object-large-icon
    (project :: <project-object>, locator :: <file-locator>)
  icon-for-file(locator, icon-size: #"large")
    | next-method()
end method environment-object-large-icon;

define constant $open-action  = "open";
define constant $print-action = "print";

define method frame-shell-execute
    (frame :: <frame>, action :: <symbol>, locator :: <locator>,
     #key show-command = $SW-SHOWNORMAL)
 => ()
  let action-name
    = select (action)
	#"open"  => $open-action;
	#"print" => $print-action;
      end;
  debug-message("Action: %sing %s", action-name, locator);
  let sheet = top-level-sheet(frame);
  if (sheet)
    debug-message("Really %sing %s", action-name, locator);
    let handle = window-handle(sheet);
    with-c-string (action-name = action-name)
      with-c-string (filename = as(<string>, locator))
	with-c-string (path = "")
	  check-result
	    ("ShellExecute",
	     ShellExecute(handle, action-name, filename,
			  $NULL-string, path, show-command))
	end
      end
    end
  end
end method frame-shell-execute;

define sideways method frame-open-object
    (frame  :: <frame>, locator :: <url>) => ()
  frame-shell-execute(frame, #"open", locator, show-command: 0)
end method frame-open-object;

define sideways method frame-open-object
    (frame  :: <frame>, locator :: <file-locator>) => ()
  frame-shell-execute(frame, #"open", locator, show-command: $SW-SHOWNORMAL)
end method frame-open-object;

define sideways method frame-hardcopy-object
    (frame  :: <frame>, locator :: <file-locator>) => ()
  frame-shell-execute(frame, #"print", locator)
end method frame-hardcopy-object;

define sideways method frame-cascade-offset
    (framem :: <win32-frame-manager>, frame :: <frame-cascading-window-mixin>)
 => (x :: <integer>, y :: <integer>)
  let title-bar-height = GetSystemMetrics($SM-CYCAPTION) + 2;
/*---*** andrewa: This seems a bit much
  let resizable? = frame-resizable?(frame);
  let (extra-width, extra-height)
     = values(GetSystemMetrics
		(if (resizable?) $SM-CXSIZEFRAME else $SM-CXFIXEDFRAME end),
	      GetSystemMetrics
		(if (resizable?) $SM-CYSIZEFRAME else $SM-CYFIXEDFRAME end));
*/
  let (extra-width, extra-height) = values(1, 1);
  values(title-bar-height + extra-width, title-bar-height + extra-height)
end method frame-cascade-offset;

//---*** This isn't yet available in win32-shell, unfortunately.
define constant $SHARD-PATHA = 2;
define constant $SHARD-PATH  = $SHARD-PATHA;

//---*** This isn't yet available in win32-shell, unfortunately.
define C-function SHAddToRecentDocs
  parameter uFlags :: <UINT>;
  parameter pv :: <LPCVOID>;
  c-name: "SHAddToRecentDocs", c-modifiers: "__stdcall";
end;

define sideways method register-opened-file
    (filename :: <file-locator>) => ()
  with-c-string (filename = filename)
    SHAddToRecentDocs($SHARD-PATH, filename)
  end
end method register-opened-file;
