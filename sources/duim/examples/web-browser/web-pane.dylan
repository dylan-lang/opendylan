Module:       web-browser
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Web pane

define abstract class <web-pane> (<object>)
  slot %web-page :: <web-page> = #f, init-keyword: page:;
end class;

define method make (class == <web-pane>, #rest args)
  apply(make, <text-web-pane>, args)
end method make;

define method web-page (pane :: <web-pane>)
  pane.%web-page
end method;

define method web-page-setter (page :: <web-page>, pane :: <web-pane>)
  block ()
    pane.%web-page := page;
  afterwards
    note-web-page-changed(pane, page)
  end
end method;


/// text web pane

define class <text-web-pane> (<web-pane>, <single-child-wrapping-pane>)
  slot pane-text-type = #"text", init-keyword: text-type:;
end class;

define method initialize (pane :: <text-web-pane>, #key frame-manager)
  next-method();
  with-frame-manager (frame-manager)
    add-child(pane, make(<text-editor>))
  end
end method;

define method note-web-page-changed 
    (pane :: <text-web-pane>, page :: <web-page>)
  let child = sheet-child(pane);
  if (child)
    let new-text = print-html-as-string(page, type: pane-text-type(pane));
    gadget-value(child) := new-text
  end
end method;


/// Useful sheets

define class <web-sheet-mixin> (<standard-input-mixin>)
  slot web-link = #f, 
    init-keyword: link:;
end class;

define method handle-event 
    (sheet :: <web-sheet-mixin>, event :: <button-release-event>)
  let link = web-link(sheet);
  if (link)
    web-sheet-follow-link(sheet, link: link)
  end
end method handle-event;

define method web-sheet-follow-link 
    (sheet :: <web-sheet-mixin>, #key link = web-link(sheet))
  if (link)
    notify-user(format-to-string("Follow link to %=", link),
                owner: sheet)
  end
end method;
  
define class <web-text-pane> (<web-sheet-mixin>, <label-pane>)
end class;

define class <web-image-pane> (<web-sheet-mixin>, <single-child-wrapping-pane>)
end class;

define method initialize (pane :: <web-image-pane>, #key)
  next-method();
  sheet-child(pane)
    := make(<push-button>,
            label: "[Image]",
            activate-callback: method (button)
                                 web-sheet-follow-link(pane)
                               end)
end method initialize;

define method make-default-image ()
  make-pattern(#(#(1, 1, 1, 1, 1, 1, 1, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 0, 0, 0, 0, 0, 0, 1),
                 #(1, 1, 1, 1, 1, 1, 1, 1)),
               vector($red, $black))
end method;

define class <web-bullet-pane> (<simple-pane>)
end class;

define variable *bullet-diameter* = 14;

define method handle-repaint 
    (sheet :: <web-bullet-pane>, medium :: <medium>, region :: <region>)
  let (width, height) = box-size(sheet);
  let radius = floor/(min(width, height) - 1, 2);
  with-sheet-medium (medium = sheet)
    draw-circle(medium, radius, radius, radius, filled?: #t)
  end
end method;

define method do-compose-space 
    (pane :: <web-bullet-pane>, #key width, height)
  make(<space-requirement>,
       width: *bullet-diameter*, height: *bullet-diameter*)
end method;

                      
/// graphic web pane

define class <graphic-web-pane> (<web-pane>, <single-child-wrapping-pane>)
end class;

define method do-allocate-space (pane :: <graphic-web-pane>, width, height)
  let page = web-page(pane);
  // layout-web-page(pane, page, width, height);
  next-method()  
end method;

define method layout-web-page 
    (pane :: <graphic-web-pane>, page :: <web-page>, width, height)
  let framem 
    = frame-manager(pane) | port-default-frame-manager(default-port());
  with-frame-manager (framem)
    let tokens = page-tokens(page);
    let spacing = make(<spacing-pane>,
                            parent: pane,
                            thickness: 8);
    let column = make(<column-layout>, parent: spacing, max-width: $fill);
    let parent = column;
    let link = #f;
    let in-list? = #f;
    for (token in tokens)
      select (token by instance?)
        <html-tag> =>
          select (token-name(token) by \=)
            "a" =>
              link := html-tag-link(token);
            "/a" =>
              link := #f;
            "img" =>
              make(<web-image-pane>,
                        parent: parent,
                        link: link);
            "ul" =>
              let spacing-pane = make(<spacing-pane>,
                                           parent: parent,
                                           thickness: 10);
              parent := make(<column-layout>,
                                  parent: spacing-pane,
                                  spacing: 2);
            "/ul" =>
              if (in-list?)
                parent := sheet-parent(parent)
              end;
              parent := sheet-parent(sheet-parent(parent));
              in-list? := #f;
            "li" =>
              if (in-list?)
                parent := sheet-parent(parent)
              end;
              parent := make(<row-pane>, 
                                  parent: parent,
                                  spacing: 8);
              make(<null-pane>,
                        parent: parent,
                        min-width: 20, max-width: 20,
                        min-height: 1, max-height: 1);
              make(<web-bullet-pane>, parent: parent);
              in-list? := #t;
            "hr" =>
              make(<separator>, parent: parent);
            otherwise =>
	      ;
	  end;
	<html-text> =>
	  make(<web-text-pane>,
		    parent: parent,
		    label: html-text(token),
		    link: link,
		    foreground: if (link) $red end)
      end
    end
  end
end method;

define method display-web-page (page :: <web-page>)
  let pane = make(<graphic-web-pane>, page: page);
  layout-web-page(pane, page, 400, 600);
  contain(scrolling (scroll-bars: #"vertical",
                     viewport-width: 500,
                     viewport-height: 600)
            pane
          end,
          width: 500,
          height: 700)
end method display-web-page;

define method display-web-page (location :: <string>)
  let page = read-web-page(location);
  display-web-page(page)
end method;
