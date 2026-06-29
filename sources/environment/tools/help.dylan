Module:    environment-tools
Synopsis:  Environment Tools
Author:    Andy Armstrong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constants

define constant $license-font
  = make(<text-style>,
         family: #"fix");

define constant $about-box-copyright-font
  = make(<text-style>,
         family: #"sans-serif",
         size:   #"small");


/// License agreement

define constant $license-text-width = 72;

define frame <license-agreement-box> (<dialog-frame>)
  pane splash-screen-pane (frame)
    make(<label>, label: $splash-screen-bitmap);
  pane license-agreement-copyright-pane (frame)
    make(<text-editor>,
         text: license-agreement-text(),
         text-style: $license-font,
         lines: 30, columns: $license-text-width + 5,
         read-only?: #t, tab-stop?: #f);
  layout (frame)
    frame.license-agreement-copyright-pane;
  keyword title: = "License Agreement";
  keyword cancel-callback: = #f;
  keyword center?: = #t;
end frame <license-agreement-box>;

define function license-agreement-text
    () => (text :: <string>)
  with-open-file (file-stream = release-license-agreement-location())
    read-to-end(file-stream)
  end;
end function license-agreement-text;


/// About Box

define function about-box-info-text
    () => (text :: <sequence>)
  vector(concatenate("Version", release-version()))
end function about-box-info-text;

define frame <about-box> (<dialog-frame>)
  pane splash-screen-pane (frame)
    make(<label>, label: $splash-screen-bitmap);
  pane user-info (frame)
    make-labels-layout
      (about-box-info-text(),
       text-style: $about-box-copyright-font);
  pane license-agreement-button (frame)
    make(<button>,
         label: "&License Terms",
         activate-callback:
           method (button)
             let dialog
               = make(<license-agreement-box>,
                      owner: sheet-frame(button));
             start-dialog(dialog)
           end);
  pane ok-button (frame)
    make(<button>,
         label: "OK",
         activate-callback: exit-dialog);
  pane exit-buttons (frame)
    horizontally (x-spacing: 8, equalize-widths?: #t)
      frame.license-agreement-button;
      frame.ok-button
    end;
  layout (frame)
    vertically (spacing: 4, x-alignment: #"center")
      with-border (type: #"raised")
        frame.splash-screen-pane
      end;
      horizontally (x-spacing: 8, max-width: $fill)
        frame.user-info;
        make(<null-pane>, max-width: $fill, height: 1, max-height: $fill);
        horizontally (y-alignment: #"bottom", max-height: $fill)
          frame.exit-buttons
        end
      end
    end;
  keyword title: = format-to-string("About %s %s",
                                    release-product-name(),
                                    release-version());
  keyword exit-buttons?: = #f;
  keyword center?: = #t;
  //--- This would be a good idea if DUIM didn't screw it up!
  // keyword fixed-width?:  = #t;
  // keyword fixed-height?: = #t;
end frame <about-box>;

define method initialize
    (frame :: <about-box>, #key) => ()
  next-method();
  frame-default-button(frame) := frame.ok-button
end method initialize;

define method frame-show-about-box
    (frame :: <environment-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let about-box = make(<about-box>, owner: frame);
    start-dialog(about-box);
  end
end method frame-show-about-box;

define method do-execute-command
    (frame :: <environment-frame>, command :: <help-on-version>) => ()
  frame-show-about-box(frame)
end method do-execute-command;

define help-source open-dylan
  release-help-web-address()
end help-source;

define method frame-help-source
    (frame :: <environment-frame>, command :: <help-command>)
 => (source :: <symbol>)
  #"open-dylan"
end method frame-help-source;


/// HELP-CREDITS

/*---*** andrewa: this isn't currently used
define constant $functional-dylan-quotes
  = vector("\"This is not the end. It is not even the beginning of the end. "
           "But it is, perhaps, the end of the beginning.\"\n"
           " -- Winston Churchill, 1942",

           "\"What we call the beginning is often the end, "
           "And to make an end is to make a beginning. "
           "The end is where we start from.\"\n"
           " -- T. S. Eliot, \"Little Gidding\", 5.",

           "\"A hard beginning maketh a good ending.\"\n"
           " -- John Heywood, \"Proverbes\". Part i. Chap. iv."
           );

define variable *quote-index* :: <integer> = 0;

define frame <help-quote> (<dialog-frame>)
  keyword title: = format-to-string("%s Beginnings", release-product-name());
  keyword cancel-callback: = #f;
  pane help-credits-text-pane (help-credits)
    make(<text-editor>,
         read-only?: #t, tab-stop?: #f,
         value: format-to-string($functional-dylan-quotes[*quote-index*]),
         scroll-bars: #"none",
         lines: 4, columns: 40);
  layout (help-credits)
    with-border (type: #"raised")
      with-spacing (spacing: 16)
        help-credits.help-credits-text-pane
      end
    end;
  keyword center?: = #t;
end frame;

define method help-credits (frame :: <about-box>)
  => ()
  with-frame-manager (frame-manager(frame))
    let frame = make(<help-quote>, owner: frame, width: 400, height: 200);
    start-dialog(frame)
  end;
  *quote-index* := floor(modulo(*quote-index* + 1, size($functional-dylan-quotes)));
end method help-credits;
*/


/// Web site command tables

define function frame-open-dylan-web-page
    (frame :: <frame>, #key page = release-web-address()) => ()
  let location = as(<url>, page);
  frame-open-object(frame, location)
end function frame-open-dylan-web-page;


/// Help command table

define sideways method frame-help-contents-and-index
    (frame :: <frame>) => ()
  let location = as(<url>, release-help-web-address());
  frame-open-object(frame, location);
end method frame-help-contents-and-index;

define sideways method frame-help-on-keyword
    (frame :: <frame>, keyword) => ()
  let location = as(<url>, release-help-web-address());
  frame-open-object(frame, make(<cgi-url>, file: location, cgi-string: keyword));
end method frame-help-on-keyword;

// "Service" function, mainly for use by environment-manager library.
define sideways method show-documentation
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<definition-object>))
 => (success? :: <boolean>)
  let frame = environment-primary-frame();
  frame-help-on-keyword(frame, name)
end method show-documentation;


// define constant $tutorial-title = "Tutorial"; // ---*** not used yet

//---*** The release-info backend isn't available during initialization... :-(
//define constant $help-about-title = format-to-string("About %s", release-product-name());
define constant $help-about-title = "About Open Dylan";

//---*** Not yet implemented
/*
define command-table *environment-tutorial-command-table* (*global-command-table*)
  menu-item $tutorial-title = frame-start-tutorial,
    documentation: "Starts the Dylan tutorial.";
end command-table *environment-tutorial-command-table*;
*/

// Nope, not for the open-source version --tc
/*
define command-table *environment-web-links-command-table* (*global-command-table*)
end command-table *environment-web-links-command-table*;

add-command-table-menu-item
  (*environment-web-links-command-table*, "", <push-box>, #f,
   update-callback: method (menu-box :: <menu-box>)
                      let items
                        = vector(format-to-string("Register %s",
                                                  release-product-name()));
                      gadget-items(menu-box) := items
                    end,
   label-key: identity,
   callback: method (menu-box :: <menu-box>)
               let frame = sheet-frame(menu-box);
               frame-register-developer(frame)
             end);
*/

define command-table *environment-help-about-command-table* (*global-command-table*)
  menu-item $help-about-title = <help-on-version>,
    documentation: "Displays program information, version, and copyright.";
end command-table *environment-help-about-command-table*;

define command-table *environment-specific-help-command-table* (*global-command-table*)
  //---*** Not yet implemented!
  // include *environment-tutorial-command-table*;
  // We don't have any Web links at the moment. --tc
  // include *environment-web-links-command-table*;
  include *environment-help-about-command-table*;
end command-table *environment-specific-help-command-table*;

define command-table *environment-help-command-table* (*global-command-table*)
  include *help-command-table*;
  include *environment-specific-help-command-table*;
end command-table *environment-help-command-table*;
