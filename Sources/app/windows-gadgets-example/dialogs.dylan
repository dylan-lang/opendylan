Module:    windows-gadgets-example
Synopsis:  An example demonstrating the use of Win32 gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple message dialogs
define method display-message
    (window :: <HWND>, message :: <string>) => (ok? :: <boolean>)
  let result
    = MessageBox(window,
	         message,
	         "This Application",
	         logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
  (result ~= 0)
end method display-message;


/// File open/save dialogs

define method make-open-file-name
    (window :: <HWND>) => (file :: <LPOPENFILENAME>)
  let file :: <LPOPENFILENAME> = make(<LPOPENFILENAME>);

  lStructSize-value(file) := size-of(<OPENFILENAME>);
  hwndOwner-value(file) := window;
  hInstance-value(file) := application-instance-handle();

  lpstrFilter-value(file) := $NULL-string;
  lpstrCustomFilter-value(file) := $NULL-string;
  nMaxCustFilter-value(file) := 0;
  nFilterIndex-value(file) := 0;
  lpstrFile-value(file) := make(<string>, size: 256);
  lpstrFile-value(file)[0] := as(<character>, 0);
  nMaxFile-value(file) := 256;
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

define method display-open-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let file :: <LPOPENFILENAME> = make-open-file-name(window);
  GetOpenFileName(file)
end method display-open-dialog;

define method display-save-as-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let file :: <LPOPENFILENAME> = make-open-file-name(window);
  GetSaveFileName(file)
end method display-save-as-dialog;


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
  null-pointer?(FindText(options))
end method display-find-dialog;

define method display-replace-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let options :: <LPFINDREPLACE>
    = make-find-options(window, find: "Hello", replace: "Goodbye");
  null-pointer?(ReplaceText(options))
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



