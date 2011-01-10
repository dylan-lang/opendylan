Module:    windows-gadgets-example
Synopsis:  An example demonstrating the use of Win32 gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Create one of every gadget

define variable *gadgets*
  = #[#[#"push-button",  "Push Button"],
      #[#"radio-button", "Radio Button"],
      #[#"check-button", "Check Button"],
      #[#"option-box",   "Option Box"],
      #[#"list-box",     "List Box"],
      #[#"label",        "Label"],
      #[#"text-field",   "Text Field"],
      #[#"scroll-bar",   "Scroll Bar"]];

define method make-every-gadget (window :: <HWND>)
  let hInstance :: <HINSTANCE> = application-instance-handle();
  for (gadget in *gadgets*,
       id from 100,
       y from 0 by 30)
    make-gadget(gadget[0], window, id,
		application: hInstance,
		title: gadget[1],
		x: 0, y: y);
  end;
end method make-every-gadget;

define method make-gadget
    (style, parent :: <HWND>, id :: <integer>,
     #key application = application-instance-handle(), title = "Gadget",
          x = 0, y = 0, width = 100, height = 20)
 => (ok :: <boolean>)
  let gadget :: <HWND>
    = CreateWindow
        (select (style)
	   #"push-button", #"radio-button", #"check-button" =>
	     "BUTTON";
	   #"list-box" =>
	     "LISTBOX";
	   #"option-box" =>
	     "COMBOBOX";
	   #"text-field" =>
	     "EDIT";
	   #"label" =>
	     "STATIC";
	   #"scroll-bar" =>
	     "SCROLLBAR";
	 end,
	 title,
	 %logior(%logior($WS-CHILD, $WS-VISIBLE),
		 select (style)
		   #"push-button"  => $BS-PUSHBUTTON;
 		   #"radio-button" => $BS-RADIOBUTTON;
		   #"check-button" => $BS-CHECKBOX;
		   #"text-field"   => $WS-BORDER;
		   #"option-box" =>
		     logior($CBS-HASSTRINGS, $CBS-DROPDOWN);
		   otherwise => 0;
		 end),
	 x, y, width, height,
	 parent,
	 as(<HMENU>, id),
	 application,
	 $NULL-VOID);
  ~null-handle?(gadget)
end method make-gadget;
