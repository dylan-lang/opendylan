Module:    flying-squares
Synopsis:  DUIM OpenGL panes
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <gl-pane> (<drawing-pane>) 
  slot pane-gl-initialized? :: <boolean> = #f;
  slot pane-gl-context = #f;
end class;

define open generic handle-initialize-scene 
    (pane :: <gl-pane>, medium :: <medium>) => ();
define open generic handle-repaint-scene 
    (pane :: <gl-pane>, medium :: <medium>) => ();

define method handle-initialize-scene 
    (pane :: <gl-pane>, medium :: <medium>) => () 
end method;

define method handle-repaint-scene 
    (pane :: <gl-pane>, medium :: <medium>) => () 
end method;

define method handle-repaint
    (pane :: <gl-pane>, medium :: <medium>, region :: <region>) => ()
  next-method();
  let mirror = sheet-mirror(pane);
  let dc = get-DC(mirror);
  if (~pane-gl-initialized?(pane))
    setup-pixel-format(dc);
    // TODO: Destroy this context when this pane is destroyed.
    let ghRC = wglCreateContext(dc);
    pane-gl-context(pane) := ghRC;
    wglMakeCurrent(dc, ghRC);
    handle-initialize-scene(pane, medium);
    pane-gl-initialized?(pane) := #t; 
  end;
  wglMakeCurrent(dc, pane-gl-context(pane));
  handle-repaint-scene(pane, medium);
  SwapBuffers(dc);
end method;

// TODO: These should come from elsewhere...

define constant $PFD-TYPE-RGBA = 0;
// define constant $PFD-TYPE-COLORINDEX = 1;

define constant $PFD-MAIN-PLANE = 0;

define constant $PFD-DOUBLEBUFFER = #x0001;
define constant $PFD-DRAW-TO-WINDOW = #x0004;
define constant $PFD-SUPPORT-OPENGL = #x0020;

define method setup-pixel-format (hdc :: <HDC>)
  with-stack-structure (ppfd :: <PPIXELFORMATDESCRIPTOR>)
    ppfd.nSize-value := size-of(<PIXELFORMATDESCRIPTOR>);
    ppfd.nVersion-value := 1;
    ppfd.dwFlags-value := logior($PFD-DRAW-TO-WINDOW, $PFD-SUPPORT-OPENGL,
                                 $PFD-DOUBLEBUFFER);
    ppfd.dwLayerMask-value := $PFD-MAIN-PLANE; 
    // TODO: This stuff needs to be parameterisable when making GL panes...
    ppfd.iPixelType-value := $PFD-TYPE-RGBA;
    // ppfd.iPixelType-value := $PFD-TYPE-COLORINDEX; 
    ppfd.cColorBits-value := 8; 
    ppfd.cDepthBits-value := 16; 
    ppfd.cAccumBits-value := 0; 
    ppfd.cStencilBits-value := 0; 
    let pixelformat = ChoosePixelFormat(hdc, ppfd); 
    SetPixelFormat(hdc, pixelformat, ppfd);
  end;
end method;
