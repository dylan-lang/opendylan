Module:    SillyBalls
Author:    Gary Palter
Synopsis:  Tranlsation of classic Macintosh SillyBalls example to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library SillyBalls
  use common-dylan;
  use c-ffi;
  //---*** NOTE: We're going to split this library up later ...
  use carbon-interface;
end library SillyBalls;

define module SillyBalls
  use common-dylan;
  use c-ffi;
  use carbon-interface;
end module SillyBalls;

define constant $Ball-Width  = 20;
define constant $Ball-Height = 20;
define constant $Gary-Size   = 8;

define function Setup (qd :: <QDGlobals*>, windRect :: <Rect*>) => ()
  with-stack-structure (theWorld :: <SysEnvRec*>)
    SysEnvirons(1, theWorld);
    if (~c-type-cast(<boolean>, theWorld.hasColorQD-value))
      SysBeep(50);
      ExitToShell()
    end
  end;
  InitGraf(qd.thePort-address);
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(null-pointer(<C-void*>));
  InitCursor();
  GetDateTime(qd.randSeed-address);
  //---*** How does one copy structures using our FFI?
  SetRect(windRect, qd.screenBits-value.bounds-value.left-value,
	            qd.screenBits-value.bounds-value.top-value,
	            qd.screenBits-value.bounds-value.right-value,
	            qd.screenBits-value.bounds-value.bottom-value);
  InsetRect(windRect, 50, 50);
  with-pascal-string (title = "Gary's Land")
    let main = NewCWindow(null-pointer(<C-void*>), windRect, title, $true, $documentProc,
			  $kFirstWindowOfClass, $false, 0);
    SetPort(main);
  end;
  TextSize($Gary-Size)
end function Setup;

define function NewBall (windRect :: <Rect*>) => ()
  with-stack-structure (ballColor :: <RGBColor*>)
    with-stack-structure (ballRect :: <Rect*>)
      ballColor.red-value   := Random();
      ballColor.green-value := Random();
      ballColor.blue-value  := Random();
      RGBForeColor(ballColor);
      let newTop  = truncate/((Random() + 32767) * windRect.bottom-value, 65536);
      let newLeft = truncate/((Random() + 32767) * windRect.right-value, 65536);
      SetRect(ballRect, newLeft, newTop, newLeft + $Ball-Width, newTop + $Ball-Height);
      MoveTo(newLeft, newTop);
      PaintOval(ballRect);
      MoveTo(ballRect.left-value + truncate/($Ball-Width, 2) - $Gary-Size,
	     ballRect.top-value + truncate/($Ball-Height, 2) + truncate/($Gary-Size, 2) - 1);
      InvertColor(ballColor);
      RGBForeColor(ballColor);
      with-pascal-string (text = "Gary")
	DrawString(text)
      end
    end
  end
end function NewBall;

define function main () => ()
  with-stack-structure (qd :: <QDGlobals*>)
    with-stack-structure (windRect :: <Rect*>)
      Setup(qd, windRect);
      while (~c-type-cast(<boolean>, Button()))
	NewBall(windRect)
      end
    end
  end
end function main;

main();
