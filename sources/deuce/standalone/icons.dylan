Module:       standalone-deuce-internals
Synopsis:     Standalone wrapper for DUIM-Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Icon initialization for Standalone Deuce

define variable *icons-initialized?* :: <boolean> = #f;

define macro initialize-icon
  { initialize-icon(?icon:name, ?resource-id:expression) }
    => { let _id   = as(<byte-string>, ?resource-id);
         let _icon = read-image-as(<win32-icon>, _id, #"small-icon");
         when (_icon)
           ?icon := _icon
         end }
end macro initialize-icon;

define function initialize-icons ()
  unless (*icons-initialized?*)
    // Initialize the Deuce icons
    $deuce-small-icon
      := read-image-as(<win32-icon>, "DEUCE", #"icon", width: 16, height: 16);
    $deuce-large-icon
      := read-image-as(<win32-icon>, "DEUCE", #"icon", width: 32, height: 32);
    // Initialize the 16x16 tool-bar icons
    initialize-icon($undo-bitmap,          "UNDO");
    initialize-icon($redo-bitmap,          "REDO");
    initialize-icon($cut-bitmap,           "CUT");
    initialize-icon($copy-bitmap,          "COPY");
    initialize-icon($paste-bitmap,         "PASTE");
    initialize-icon($find-bitmap,          "FIND");
    initialize-icon($replace-bitmap,       "REPLACE");
    initialize-icon($find-next-bitmap,     "FINDNEXT");
    initialize-icon($find-previous-bitmap, "FINDPREVIOUS");
    initialize-icon($new-bitmap,           "NEW");
    initialize-icon($open-bitmap,          "OPEN");
    initialize-icon($save-bitmap,          "SAVE");
    // Initialize the 16x16 breakpoint icons
    initialize-icon($potential-breakpoint-image, "POTENTIALBREAKPOINT");
    initialize-icon($enabled-breakpoint-image,   "ENABLEDBREAKPOINT");
    initialize-icon($disabled-breakpoint-image,  "DISABLEDBREAKPOINT");
    initialize-icon($step-breakpoint-image,      "STEPBREAKPOINT");
    initialize-icon($test-breakpoint-image,      "TESTBREAKPOINT");
    initialize-icon($enabled-tracepoint-image,   "ENABLEDTRACEPOINT");
    initialize-icon($disabled-tracepoint-image,  "DISABLEDTRACEPOINT");
    initialize-icon($profile-point-image,        "PROFILEPOINT");
    initialize-icon($current-location-image,     "CURRENTLOCATION");
    initialize-icon($prompt-image,               "PROMPT");
    initialize-icon($values-image,               "VALUES");
    initialize-icon($warning-image,              "WARNING");
    initialize-icon($serious-warning-image,      "SERIOUSWARNING");
    // All done
    *icons-initialized?* := #t
  end
end function initialize-icons;

// Read in the icons, then reinitialize the standard images
begin
  initialize-icons();
  initialize-standard-images()
end;
