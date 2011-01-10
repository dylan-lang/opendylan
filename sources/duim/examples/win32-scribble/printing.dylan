Module:       win32-scribble
Author:       Andy Armstrong
Synopsis:     Printer support for scribble
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-printing
  { with-printing (?dc:name, ?name:expression)
      ?body:body
    end }
 => { do-with-printing(?dc, ?name, method (?dc) ?body end) }
end macro with-printing;

define method scribble-page-setup
    (frame :: <scribble-frame>) => ()
  notify-user("Not yet implemented!",
	      style: #"warning", exit-style: #"ok")
end method scribble-page-setup;

define method scribble-print
    (frame :: <scribble-frame>) => ()
  let dc = choose-printer-options(frame);
  if (dc)
    with-printing (dc, "scribble")
      let sheet = frame.surface;
      let (x-scale, y-scale) = sheet-printer-scaling(sheet, dc);
      repaint-in-dc-recursive(sheet, dc, x-scale, y-scale, 0, 0)
    end
  end
end method scribble-print;


/// Win32 code

define function ensure-no-dialog-error (name :: <string>) => ()
  let error = CommDlgExtendedError();
  unless (error = $NO-ERROR)
    report-error(name, error: error)
  end
end function ensure-no-dialog-error;

define method choose-printer-options
    (frame :: <scribble-frame>)
 => (dc :: false-or(<HDC>))
  let sheet = top-level-sheet(frame);
  let handle = window-handle(sheet);
  with-stack-structure (print :: <LPPRINTDLG>)
    print.lStructSize-value  := size-of(<PRINTDLG>);
    print.hwndOwner-value    := handle;
    print.hInstance-value    := application-instance-handle();
    print.hDevMode-value     := null-pointer(<LPDEVMODE>);
    print.hDevNames-value    := null-pointer(<LPDEVNAMES>);
    print.Flags-value        := %logior($PD-USEDEVMODECOPIESANDCOLLATE,
					$PD-RETURNDC);
    print.nCopies-value      := 1;
    print.nFromPage-value    := 1;
    print.nToPage-value      := 1;
    print.nMinPage-value     := 1;
    print.nMaxPage-value     := 1;
    if (PrintDlg(print))
      print.hDC-value;
    else
      ensure-no-dialog-error("PrintDlg");
      #f
    end
  end
end method choose-printer-options;

define method do-with-printing
    (dc :: <HDC>, name :: <string>, function :: <function>) => ()
  let started? :: <boolean> = #f;
  block ()
    with-stack-structure (docinfo :: <LPDOCINFO>)
      with-c-string (c-string = name)
	docinfo.cbSize-value       := size-of(<DOCINFO>);
	docinfo.lpszDocName-value  := c-string;
	docinfo.lpszOutput-value   := $NULL-string;
	docinfo.lpszDatatype-value := $NULL-string;
	docinfo.fwType-value       := 0;
	StartDoc(dc, docinfo);
	started? := #t;
      end
    end;
    function(dc)
  cleanup
    if (started?)
      EndDoc(dc)
    end;
    DeleteDc(dc)
  end
end method do-with-printing;

define method sheet-printer-scaling
    (sheet :: <sheet>, printer-dc :: <HDC>)
 => (x-scale :: <single-float>, y-scale :: <single-float>)
  let mirror = sheet-mirror(sheet);
  let sheet-dc = get-dc(mirror);
  local method scale
	    (value1 :: <integer>, value2 :: <integer>)
	 => (scale :: <single-float>)
	  as(<single-float>, value1) / as(<single-float>, value2)
	end method scale;
  values(scale(GetDeviceCaps(printer-dc, $LOGPIXELSX),
	       GetDeviceCaps(sheet-dc,   $LOGPIXELSX)),
	 scale(GetDeviceCaps(printer-dc, $LOGPIXELSY),
	       GetDeviceCaps(sheet-dc,   $LOGPIXELSY)))
end method sheet-printer-scaling;
