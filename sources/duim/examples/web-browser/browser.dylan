Module:       web-browser
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Web Browser

define frame <web-browser> (<simple-frame>)
  slot %web-page :: <web-page> = #f;
  slot web-pages :: <collection> = make(<stretchy-vector>);
  slot browser-view = #"page";
  pane back-button (frame)
    make(<push-button>, label: "Back",
         activate-callback: web-browser-back);
  pane link-button (frame)
    make(<push-button>, label: "Follow Link...",
         activate-callback: web-browser-follow-link);
  pane reload-button (frame)
    make(<push-button>, label: "Reload",
         activate-callback: web-browser-reload-page);
  pane location-title (frame)
    make(<label>, label: "Location:");
  pane location-pane (frame)
    make(<text-field>,
	 value-changed-callback: method (tf)
				   open-web-location(sheet-frame(tf), gadget-value(tf))
				 end);
  pane web-pane (frame)
    make(<web-pane>, width: 500, height: 500);
  pane text-web-pane (frame)
    make(<text-web-pane>, width: 500, height: 500);
  pane status (frame)
    make(<status-bar>);
  pane tool-bar (frame)
    make(<tool-bar>,
         child: horizontally (spacing: 2)
                  frame.back-button;
                  frame.reload-button;
                  frame.link-button;
                end);
  pane main-layout (frame)
    vertically (spacing: 2)
      horizontally (spacing: 2, y-alignment: #"centre")
        frame.location-title;
        frame.location-pane;
      end;
      frame.web-pane;
    end;
  pane file-menu (frame)
    make(<menu>, label: "File",
         children: vector(make(<menu-button>, label: "Open Location..."),
                          make(<menu-button>, 
                               label: "Open File...",
                               activate-callback: open-web-file),
                          make(<menu-button>, label: "Save as...")));
  pane view-menu (frame)
    make(<menu>, label: "View",
         children: vector(make(<radio-menu-box>,
                               items: #(#("View Page", #"page"),
                                        #("View as HTML", #"html"),
                                        #("View as Text", #"text")),
                               label-key: first,
                               value-key: second,
                               value-changed-callback:
				 method (rb)
				   set-web-browser-view(rb, gadget-value(rb))
				 end)));
  pane help-menu (frame)
    make(<menu>, label: "Help",
         children: vector(make(<menu-button>,
                               label: "About Function Developer Web Browser...",
                               activate-callback: about-web-browser)));
  layout (frame) frame.main-layout;
  status-bar (frame) frame.status;
  tool-bar (frame) frame.tool-bar;
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(frame.file-menu, 
                          frame.view-menu, 
                          frame.help-menu));
end frame <web-browser>;

define method web-page (browser :: <web-browser>)
  browser.%web-page
end method;

define method web-page-setter (page :: <web-page>, browser :: <web-browser>)
  block ()
    browser.%web-page := page;
  afterwards
    let pages = web-pages(browser);
    web-page(web-pane(browser)) := page;
    unless (~empty?(pages) & page = last(pages))
      web-pages(browser) := add!(pages, page)
    end;
    frame-title(browser) := format-to-string("Function Developer Web Browser - [%s]",
                                             page-title(page));
    gadget-value(location-pane(browser)) := page-location(page)
  end
end method;

define method initialize
    (browser :: <web-browser>, #key home-page :: <string>)
  next-method();
  if (home-page)
    open-web-location(browser, home-page)
  end
end method;

define method open-web-file (sheet :: <sheet>)
  let browser = sheet-frame(sheet);
  open-web-location(browser, "/usr/users/andrewa/test-html/movies.html")
end method;

define method open-web-location (browser :: <frame>, location :: <string>)
  let page = web-page(browser);
  let location = if (page)
                   web-relative-location(location, page)
                 else
                   location
                 end;
  block ()
    web-page(browser) := read-web-page(location)
  exception (<error>)
    notify-user(format-to-string("Failed to open location %s", location),
                frame: browser)
  end
end method;

define method web-browser-back (sheet :: <sheet>)
  let browser = sheet-frame(sheet);
  let pages = web-pages(browser);
  if (size(pages) > 1)
    size(pages) := size(pages) - 1;
    web-page(browser) := last(pages);
  else
    with-sheet-medium (medium = sheet)
      beep(medium)
    end
  end
end method;

define method web-browser-follow-link (sheet :: <sheet>)
  let frame = sheet-frame(sheet);
  let link = choose-from-dialog(find-html-links(web-page(frame)),
                                frame: frame);
  if (link)
    open-web-location(frame, link)
  end
end method;

define method web-browser-reload-page (sheet :: <sheet>)
  let browser = sheet-frame(sheet);
  let page = web-page(browser);
  block ()
    read-web-page(page);
    web-page(web-pane(browser)) := page
  exception (<error>)
    notify-user(format-to-string("Failed to reload page %s",
                                 page-location(page)),
                frame: browser)
  end
end method;

define method set-web-browser-view (sheet :: <sheet>, view)
  let browser = sheet-frame(sheet);
  browser-view(browser) := view;
  let pane = web-pane(browser);
  pane-text-type(pane) := select (view)
                            #"page", #"text" => #"text";
                            #"html" => #"html";
                          end;
  web-page(pane) := web-page(pane);
end method;

define variable *browser-title* = "Function Developer Web Browser";
define variable *version-number* = 1;
define variable *copyright-message* = "(c) Functional Objects, Inc. 1995";
 
define method about-web-browser (sheet :: <sheet>)
  let (major-version, minor-version) = floor/(*version-number*, 100);
  let message = format-to-string("%s %d.%d %s",
                                 *browser-title*,
                                 major-version,
                                 minor-version,
                                 *copyright-message*);
  notify-user(message, frame: sheet-frame(sheet))
end method;


/// browse-web

define variable *home-page* = "http://www.functionalobjects.com/";

define method browse-web (#key home-page)
  make(<web-browser>,
       home-page: home-page | *home-page*,
       mapped?: #t)
end method;
