Module:    environment-splash-screen
Synopsis:  environment splash screen
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $splash-screen-duration = 60;
define variable $splash-screen-bitmap   = #f;
define variable $splash-screen-resource = "SPLASHSCREEN";


/// Bitmaps

define variable *bitmaps-initialized?* :: <boolean> = #f;

define function default-splash-screen-image
    () => (image :: false-or(<image>))
  unless (*bitmaps-initialized?*)
    //---*** Hack in the size for the moment, as DUIM doesn't
    //---*** calculate it correctly.
    let bitmap
      = read-image-as(<win32-bitmap>, $splash-screen-resource, #"bitmap",
		      width: 500, height: 375);
    when (bitmap)
      $splash-screen-bitmap := bitmap
    end;
    *bitmaps-initialized?* := #t
  end;
  $splash-screen-bitmap
end function default-splash-screen-image;


/// Splash screen

define frame <splash-screen> (<dialog-frame>)
  slot splash-screen-image :: <image> = default-splash-screen-image(),
    init-keyword: image:;
  pane image (frame)
    make(<label>, label: frame.splash-screen-image);
  layout (frame)
    frame.image;
  keyword exit-callback: = #f;
  keyword cancel-callback: = #f;
  //--- It seems that modal is better (note: not system modal)
  // keyword mode: = #"modeless";
end frame <splash-screen>;

define function display-splash-screen
    (image :: <image>, #key duration :: <integer> = $splash-screen-duration)
 => ()
  let splash-screen = make(<splash-screen>, image: image);
  make(<thread>,
       name: "splash screen",
       function: curry(start-dialog, splash-screen));
  sleep(duration);
  exit-frame(splash-screen)
end function display-splash-screen;

define function fork-splash-screen
    (#key duration :: <integer> = $splash-screen-duration) => ()
  let image = default-splash-screen-image();
  when (image)
    make(<thread>,
	 name: "splash screen timer",
	 function: method ()
		     display-splash-screen(image, duration: duration)
		   end)
  end
end function fork-splash-screen;
