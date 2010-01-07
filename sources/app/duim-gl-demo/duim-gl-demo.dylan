Module:    duim-gl-demo
Synopsis:  DUIM OpenGL demo
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Snatched almost verbatim with a minimum of understanding from one
// of the MSDN Library samples.

define constant $black-index = 0;
define constant $blue-index = 16;
define constant $red-index = 13;
define constant $green-index = 14;

define constant $globe = 1;
define constant $cone = 2;
define constant $cylinder = 3;

define class <demo-gl-pane> (<gl-pane>) 
  slot pane-latitude = 6.0d0;
  slot pane-longitude = 3.0d0;
  slot pane-last-x = #f;
  slot pane-last-y = #f;
end class;

define method handle-initialize-scene 
    (pane :: <demo-gl-pane>, medium :: <medium>) => ()
  next-method();
  glClearIndex(as(<single-float>, $black-index));
  glClearDepth(1.0d0);

  glEnable($GL-DEPTH-TEST);

  glMatrixMode($GL-PROJECTION);
  let aspect = 350.0d0 / 300.0d0;
  gluPerspective(45.0d0, aspect, 3.0d0, 7.0d0);
  glMatrixMode($GL-MODELVIEW);

  // Create objects...

  glNewList($globe, $GL-COMPILE);
    let quadObj = gluNewQuadric();
    gluQuadricDrawStyle(quadObj, $GLU-LINE); 
    gluSphere(quadObj, 1.5d0, 16, 16); 
  glEndList(); 
  glNewList($CONE, $GL-COMPILE); 
    let quadObj = gluNewQuadric(); 
    gluQuadricDrawStyle (quadObj, $GLU-FILL); 
    gluQuadricNormals (quadObj, $GLU-SMOOTH); 
    gluCylinder(quadObj, 0.3d0, 0.0d0, 0.6d0, 15, 10); 
  glEndList(); 
 
  glNewList($CYLINDER, $GL-COMPILE); 
    glPushMatrix (); 
    glRotatef (90.0e0, 1.0e0, 0.0e0, 0.0e0); 
    glTranslatef (0.0e0, 0.0e0, -1.0e0); 
    let quadObj = gluNewQuadric (); 
    gluQuadricDrawStyle (quadObj, $GLU-FILL); 
    gluQuadricNormals (quadObj, $GLU-SMOOTH); 
    gluCylinder (quadObj, 0.3d0, 0.3d0, 0.6d0, 12, 2); 
    glPopMatrix (); 
  glEndList(); 

end method;

define method handle-repaint-scene 
    (pane :: <demo-gl-pane>, medium :: <medium>) => ()
  // Draw scene...
  let near-plane = 3.0d0;
  let far-plane = 7.0d0;
  let maxObjectSize = 3.0d0;
  let radius = near-plane + maxObjectSize / 2.0d0;

  let latitude = pane-latitude(pane);
  let longitude = pane-longitude(pane);
  let latinc = 6.0d0;
  let longinc = 2.5d0;
  glClear(logior($GL-COLOR-BUFFER-BIT, $GL-DEPTH-BUFFER-BIT));
  glPushMatrix();
    polar-view(radius, 2d0, latitude, longitude);
    glIndexi($blue-index);
    glCallList($globe);
    glIndexi($red-index);
    glCallList($cone);
    glIndexi($green-index);
    glCallList($cylinder);
  glPopMatrix();
end method;

define method polar-view (radius, twist, latitude, longitude)
  glTranslated(0.0d0, 0.0d0, - radius);
  glRotated(- twist, 0.0d0, 0.0d0, 1.0d0);
  glRotated(- latitude, 1.0d0, 0.0d0, 0.0d0);
  glRotated(longitude, 0.0d0, 0.0d0, 1.0d0);
end method;

define method handle-event (pane :: <demo-gl-pane>, event :: <pointer-drag-event>) => ()
  next-method();
  let x = event-x(event);
  let y = event-y(event);
  let last-x = pane-last-x(pane);
  let last-y = pane-last-y(pane);
  if (last-x)
    let dx = x - last-x;
    pane-longitude(pane) := pane-longitude(pane) - as(<double-float>, dx);
  end;
  if (last-y)
    let dy = y - last-y;
    pane-latitude(pane) := pane-latitude(pane) - as(<double-float>, dy);
  end;
  pane-last-x(pane) := x;
  pane-last-y(pane) := y;
  repaint-sheet(pane, $everywhere);
end method;

define method handle-event 
    (pane :: <demo-gl-pane>, event :: <button-release-event>) => ()
  next-method();
  pane-last-x(pane) := #f;
  pane-last-y(pane) := #f;
end method;

define method handle-event 
    (pane :: <demo-gl-pane>, event :: <pointer-exit-event>) => ()
  next-method();
  pane-last-x(pane) := #f;
  pane-last-y(pane) := #f;
end method;

define frame <demo-frame> (<simple-frame>)
  pane file-menu (frame)
    make(<menu>,
         label: "&File",
         children:
           vector(make(<menu-button>,
                       label: "Close",
                       activate-callback:
                         method (sheet)
                           exit-frame(sheet-frame(sheet))
                         end)));
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(file-menu(frame)));
  layout (frame)
    vertically (spacing: 2)
      with-border (style: #"inset")
        make(<demo-gl-pane>, width: 350, height: 300)
      end;
      with-border (style: #"inset")
        make(<demo-gl-pane>, width: 350, height: 300)
      end;
    end;
  status-bar (frame)
    make(<status-bar>);
  keyword title: = "DUIM GL Demo";
end frame;

define method main () => ()
  start-frame(make(<demo-frame>));
end method main;

begin
  main();
end;
