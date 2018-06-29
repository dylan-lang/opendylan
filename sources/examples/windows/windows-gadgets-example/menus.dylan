Module:    windows-gadgets-example
Synopsis:  An example demonstrating the use of Win32 gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *sub-menu*
  = vector(vector("&Color...",    #t, #f, $CHOOSE-COLOR-MENU-ITEM),
	   vector("&Font...",     #t, #f, $CHOOSE-FONT-MENU-ITEM));

define variable *menus*
  = vector
      (vector
	 ("&File",
	  vector(vector("&New",          #f, #f, $NEW-MENU-ITEM),
		 vector("&Open",         #t, #f, $OPEN-MENU-ITEM),
		 vector("&Save",         #f, #f, $SAVE-MENU-ITEM),
		 vector("Save &As...",   #t, #f, $SAVE-AS-MENU-ITEM),
		 #f,
		 vector("Page Setup...", #t, #f, $PAGE-SETUP-MENU-ITEM),
		 vector("Print...",      #t, #f, $PRINT-MENU-ITEM),
		 #f,
		 vector("E&xit",         #t, #f, $EXIT-MENU-ITEM))),
       vector
	 ("&Edit",
	  vector(vector("&Undo",         #f, #f, $UNDO-MENU-ITEM),
		 #f,
		 vector("Cu&t",          #f, #f, $CUT-MENU-ITEM),
		 vector("&Copy",         #t, #f, $COPY-MENU-ITEM),
		 vector("&Paste",        #f, #f, $PASTE-MENU-ITEM),
		 #f,
		 vector("Find...",       #t, #f, $FIND-MENU-ITEM),
		 vector("Replace...",    #t, #f, $REPLACE-MENU-ITEM))),
       vector
	 ("&View",
	  vector(vector("&1. One",       #t, #f, $VIEW-ONE-MENU-ITEM),
		 vector("&2. Two",       #t, #t, $VIEW-TWO-MENU-ITEM),
		 vector("&3. Three",     #t, #f, $VIEW-THREE-MENU-ITEM),
		 #f,
		 vector(vector("&Options", *sub-menu*)))),
       vector
	 ("&Help",
	  vector(vector("&Contents",     #t, #f, $HELP-CONTENTS-MENU-ITEM),
		 #f,
		 vector("&About...",     #t, #f, $HELP-ABOUT-MENU-ITEM))));

define method make-menu-bar
    (parent :: <HWND>)
 => (ok? :: <boolean>)
  let menu :: <HMENU> = CreateMenu();
  if (~null-pointer?(menu))
    for (menu-info in *menus*)
      make-menu(menu, menu-info[1], title: menu-info[0]);
    end;
    SetMenu(parent, menu);
  end
end method make-menu-bar;

define method make-menu
    (menu :: <HMENU>, items :: <sequence>, #key title = "Menu")
 => (ok? :: <boolean>)
  let sub-menu :: <HMENU> = CreateMenu();
  if (~null-pointer?(sub-menu))
    make-menu-items(sub-menu, items);
    AppendMenu(menu, 
	       $MF-POPUP + $MF-STRING,
	       pointer-address(sub-menu),
	       title)
  end
end method make-menu;

define method make-menu-items
    (menu :: <HMENU>, items :: <sequence>)
 => (ok? :: <boolean>)
  for (item-info in items)
    if (item-info)
      let name = item-info[0];
      if (instance?(name, <string>))
	let enabled? = item-info[1];
	let selected? = item-info[2];
	let id = item-info[3];
	AppendMenu(menu,
		   $MF-STRING
		     + (if (enabled?) $MF-ENABLED else $MF-GRAYED end)
		     + (if (selected?) $MF-CHECKED else 0 end),
		   id,
		   name)
      else
	let title = name[0];
	let items = name[1];
	make-menu(menu, items, title: title)
      end
    else
      AppendMenu(menu,
		 $MF-SEPARATOR,
		 0,
		 "")
    end
  end
end method make-menu-items;
