Module:    win32-duim
Synopsis:  Win32 dialog implementation
Author:    Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Notify user

//---*** Hack until these are really imported
// define constant $IDOK     = 1;
// define constant $IDCANCEL = 2;
// define constant $IDABORT  = 3;
define constant $IDRETRY  = 4;
define constant $IDIGNORE = 5;
// define constant $IDYES    = 6;
define constant $IDNO     = 7;
define constant $IDCLOSE  = 8;
define constant $IDHELP   = 9;


define method do-notify-user
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key title, documentation, name,
     #all-keys)
 => (ok? :: <boolean>)
  ignore(foreground, background, text-style);
  let handle = owner.%window-handle;
  let title = title | "DUIM application";
  let style-flag
    = select (style)
	#"serious-error" => $MB-ICONERROR;
	#"error"         => $MB-ICONERROR;
	#"question"      => $MB-ICONQUESTION;
	#"information"   => $MB-ICONINFORMATION;
	#"warning"       => $MB-ICONWARNING;
      end;
  let button-flag
    = select (style)
	#"question"      => $MB-YESNO;
	otherwise        => $MB-OK;
      end;
  let modality
    = select (style)
	#"serious-error" => $MB-SYSTEMMODAL;
	otherwise        => $MB-APPLMODAL;
      end;
  let flags
    = logior(logior($MB-APPLMODAL, $MB-OK),
	     logior(style-flag, button-flag));
  let result = MessageBox(handle, message, title, flags);
  select (result)
    $IDNO     => #f;
    $IDOK     => #t;
    $IDYES    => #t;
    otherwise => error("Unexpected return code %= from MessageBox", result);
  end;
end method do-notify-user;


/// Choose file

define constant $max-file-name-length = 1000;

define method do-choose-file
    (framem :: <win32-frame-manager>, owner :: <sheet>, 
     direction == #"input",
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (locator :: false-or(<string>))
  let mirror = sheet-mirror(owner);
  let handle = mirror.%window-handle;
  let buffer = make(<C-string>, size: $max-file-name-length);
  let file :: <LPOPENFILENAME> 
    = make-open-file-name(handle, buffer, $max-file-name-length);
  if (check-result("GetOpenFileName", GetOpenFileName(file)))
    as(<string>, buffer)
  else
    //---*** We should really look at CommDlgExtendedError!
    #f
  end
end method do-choose-file;

define method do-choose-file
    (framem :: <win32-frame-manager>, owner :: <sheet>, 
     direction == #"output",
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (locator :: false-or(<string>))
  let mirror = sheet-mirror(owner);
  let handle = mirror.%window-handle;
  let buffer = make(<C-string>, size: $max-file-name-length);
  let file :: <LPOPENFILENAME>
    = make-open-file-name(handle, buffer, $max-file-name-length);
  if (check-result("GetSaveFileName", GetSaveFileName(file)))
    as(<string>, buffer)
  else
    //---*** We should really look at CommDlgExtendedError!
    #f
  end
end method do-choose-file;

//---*** Fix this to take a default at some time
define method make-open-file-name
    (window :: <HWND>, buffer :: <C-string>, size :: <integer>)
 => (file :: <LPOPENFILENAME>)
  let file :: <LPOPENFILENAME> = make(<LPOPENFILENAME>);

  lStructSize-value(file) := size-of(<OPENFILENAME>);
  hwndOwner-value(file) := window;
  hInstance-value(file) := application-instance-handle();

  lpstrFilter-value(file) := $NULL-string;
  lpstrCustomFilter-value(file) := $NULL-string;
  nMaxCustFilter-value(file) := 0;
  nFilterIndex-value(file) := 0;
  lpstrFile-value(file) := buffer;
  lpstrFile-value(file)[0] := as(<character>, 0);
  nMaxFile-value(file) := size;
  lpstrFileTitle-value(file) := $NULL-string;
  nMaxFileTitle-value(file) := 0;
  lpstrInitialDir-value(file) := $NULL-string;
  lpstrTitle-value(file) := $NULL-string;
  Flags-value(file) := $OFN-SHOWHELP;
  nFileOffset-value(file) := 0;
  nFileExtension-value(file) := 0;
  lpstrDefExt-value(file) := "bat";
  // slot lCustData-value :: <LPARAM>;
  // slot lpfnHook-value :: <LPOFNHOOKPROC>;
  // slot lpTemplateName-value :: <LPCSTR>;

  file
end method make-open-file-name;

 
/// Directory chooser

define method do-choose-directory
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (locator)
  //---*** Implement this!
  not-yet-implemented("do-choose-directory")
end method do-choose-directory;

 
/// Directory chooser

define method do-choose-color
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (color :: false-or(<color>));
  //---*** Implement this!
  not-yet-implemented("do-choose-color")
end method do-choose-directory;


/// Choose from menu

define method do-choose-from-menu
    (framem :: <win32-frame-manager>, owner :: <sheet>, menu :: <menu>,
     #key title, default-item,
          name-key, value-key,
          foreground, background, text-style,
     #all-keys)
 => (value, success? :: <boolean>)
  //---*** Implement this!
  not-yet-implemented("do-choose-from-menu")
end method do-choose-from-menu;


///---*** Some not yet implemented dialogs
/*

/// Color dialog

/*
define variable *custom-colors* = make(<LPCOLORREF>, element-count: 16);

define method display-color-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  with-stack-structure (lpcc :: <LPCHOOSECOLOR>)
    lpcc.lStructSize-value := size-of(<CHOOSECOLOR>);
    lpcc.hwndOwner-value := window;
    lpcc.Flags-value := $CC-SHOWHELP;
    lpcc.lpCustColors-value := make(<LPCOLORREF>, element-count: 16);
    ChooseColor(lpcc)
  end with-stack-structure;
end method display-color-dialog;
*/

define method display-color-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  display-message(window, "Color dialog not yet implemented!")
end method display-color-dialog;


/// Font dialog

define method make-font-choice
    (window :: <HWND>) => (font :: <LPCHOOSEFONT>)
  let cf :: <LPCHOOSEFONT> = make(<LPCHOOSEFONT>);
  let font :: <LPLOGFONT> = make(<LPLOGFONT>);

  lStructSize-value(cf) := size-of(<CHOOSEFONT>);
  hwndOwner-value(cf) := window;
  hInstance-value(cf) := application-instance-handle();
  lpLogFont-value(cf) := font;
  Flags-value(cf) := $CF-SHOWHELP + $CF-SCREENFONTS + $CF-EFFECTS;
  lpszStyle-value(cf) := make(<string>, size: 100);

  cf
end method make-font-choice;

define method display-font-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let font-choice :: <LPCHOOSEFONT> = make-font-choice(window);
  ChooseFont(font-choice);
end method display-font-dialog;


/// Find and replace dialogs

define method make-find-options
    (window :: <HWND>, #key find, replace) => (options :: <LPFINDREPLACE>)
  let options :: <LPFINDREPLACE> = make(<LPFINDREPLACE>);

  lStructSize-value(options) := size-of(<FINDREPLACE>);
  hwndOwner-value(options) := window;
  hInstance-value(options) := application-instance-handle();
  Flags-value(options) := $FR-NOMATCHCASE + $FR-NOWHOLEWORD;
  lpstrFindWhat-value(options) := find;
  lpstrReplaceWith-value(options) := replace | $NULL-string;
  wFindWhatLen-value(options) := size(find);
  wReplaceWithLen-value(options) := if (replace) size(replace) else 0 end;

  options
end method make-find-options;

define method display-find-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let options :: <LPFINDREPLACE> = make-find-options(window, find: "Hello");
  FindText(options)
end method display-find-dialog;

define method display-replace-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let options :: <LPFINDREPLACE>
    = make-find-options(window, find: "Hello", replace: "Goodbye");
  ReplaceText(options)
end method display-replace-dialog;


/// Printer dialogs

define method display-page-setup-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  display-message(window, "Page setup dialog not yet implemented!")
end method display-page-setup-dialog;

define method make-print-options
    (window :: <HWND>) => (font :: <LPPRINTDLG>)
  let options :: <LPPRINTDLG> = make(<LPPRINTDLG>);

  lStructSize-value(options) := size-of(<PRINTDLG>);
  hwndOwner-value(options) := window;
  hDevMode-value(options) := null-pointer(<HGLOBAL>);
  hDevNames-value(options) := null-pointer(<HGLOBAL>);
  Flags-value(options)
    := $PD-SHOWHELP + $PD-PRINTSETUP + $PD-COLLATE + $PD-USEDEVMODECOPIES;
  nFromPage-value(options) := 0;
  nToPage-value(options) := 0;
  nMinPage-value(options) := 1;
  nMaxPage-value(options) := 10;
  nCopies-value(options) := 1;
  hInstance-value(options) := application-instance-handle();

  options
end method make-print-options;

define method display-print-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let print-options :: <LPPRINTDLG> = make-print-options(window);
  PrintDlg(print-options);
end method display-print-dialog;
*/
