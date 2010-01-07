Module:    flying-squares
Synopsis:  Harlequin Logo Flying Squares OpenGL DUIM demo
Authors:   Jonathan Bachrach, Gary Palter, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// PHYSICS
///

/// Global confinement spring constant -- larger values confine tile closer to origin
define constant $kg :: <single-float> = 2000.0;

/// Local spring constant between tiles -- larger values keep tile separation closer to r0
define constant $kl :: <single-float> = 10000.0;

/// Tile mass
define constant $m :: <single-float> = 1000.0;

/// Z penalty
define constant $kz :: <single-float> = 1.0;

/// Time constant
define constant $dt :: <single-float> = 0.001;

define constant $updates-per-redisplay :: <integer> = 10;

define function rnd (min :: <single-float>, max :: <single-float>)
  min + as(<single-float>, random(1000))  / 1000.0 * (max - min)
end function rnd;

/*
define class <v3> (<object>)
  slot x :: <single-float>, init-value: 0.0, init-keyword: x:;
  slot y :: <single-float>, init-value: 0.0, init-keyword: y:;
  slot z :: <single-float>, init-value: 0.0, init-keyword: z:;
end class;

define inline function make-v3(x :: <single-float>, y :: <single-float>, z :: <single-float>)
 => (v3 :: <v3>)
  make(<v3>, x: x, y: y, z: z)
end function make-v3;
*/

define constant <v3> = limited(<simple-vector>, of: <single-float>);

define inline-only function x (v3 :: <v3>) => (x :: <single-float>)
  v3[0]
end function x;
define inline-only function x-setter (new-x :: <single-float>, v3 :: <v3>)
 => (new-x :: <single-float>)
  v3[0] := new-x
end function x-setter;

define inline-only function y (v3 :: <v3>) => (y :: <single-float>)
  v3[1]
end function y;
define inline-only function y-setter (new-y :: <single-float>, v3 :: <v3>)
 => (new-y :: <single-float>)
  v3[1] := new-y
end function y-setter;

define inline-only function z (v3 :: <v3>) => (z :: <single-float>)
  v3[2]
end function z;
define inline-only function z-setter (new-z :: <single-float>, v3 :: <v3>)
 => (new-z :: <single-float>)
  v3[2] := new-z
end function z-setter;

define inline function make-v3(x :: <single-float>, y :: <single-float>, z :: <single-float>)
 => (v3 :: <v3>)
  let v3 = make(<v3>, size: 3, fill: 0.0);
  v3.x := x;
  v3.y := y;
  v3.z := z;
  v3
end function make-v3;

define function vadd (a :: <v3>, b :: <v3>)
  a.x := a.x + b.x;
  a.y := a.y + b.y;
  a.z := a.z + b.z;
  a
end function vadd;

define function vreplace (a :: <v3>, b :: <v3>)
  a.x := b.x;
  a.y := b.y;
  a.z := b.z;
  a
end function vreplace;

define function vmul-s (v :: <v3>, k :: <single-float>)
  v.x := k * v.x;
  v.y := k * v.y;
  v.z := k * v.z;
  v
end function vmul-s;

define function distance*
    (ax :: <single-float>, ay :: <single-float>, az :: <single-float>,
     bx :: <single-float>, by :: <single-float>, bz :: <single-float>)
  let dx :: <single-float> = ax - bx;
  let dy :: <single-float> = ay - by;
  let dz :: <single-float> = az - bz;
  sqrt(dx * dx + dy * dy + dz * dz)
end function distance*;

define function unit-vector-and-distance (u :: <v3>, a :: <v3>, b :: <v3>)
  let dx :: <single-float> = a.x - b.x;
  let dy :: <single-float> = a.y - b.y;
  let dz :: <single-float> = a.z - b.z;
  let d  :: <single-float> = sqrt(dx * dx + dy * dy + dz * dz);
  u.x := dx / d;
  u.y := dy / d;
  u.z := dz / d;
  d
end function unit-vector-and-distance;


///
/// SQUARES
///

define constant <v4> = <GLfloat*>;

define inline function make-v4 (a :: <single-float>, b :: <single-float>,
                                c :: <single-float>, d :: <single-float>)
 => (v4 :: <v4>)
  let v4 = make(<v4>, element-count: 4);
  v4[0] := a;
  v4[1] := b;
  v4[2] := c;
  v4[3] := d;
  v4
end function make-v4;

define inline function blue-square? (i :: <integer>, j :: <integer>) => (blue? :: <boolean>)
  let ib = logand(i, 1);
  let jb = logand(j, 1);
  (ib = 1 & jb = 1) | (ib = 0 & jb = 0)
end function blue-square?;

define constant $white-specular  = make-v4(1.0, 1.0, 1.0, 1.0);
define constant $red-specular    = make-v4(1.0, 0.0, 0.0, 1.0);
define constant $green-specular  = make-v4(0.0, 1.0, 0.0, 1.0);
define constant $blue-specular   = make-v4(0.0, 0.0, 1.0, 1.0);
define constant $purple-specular = make-v4(1.0, 0.0, 1.0, 1.0);
define constant $orange-specular = make-v4(0.0, 1.0, 1.0, 1.0);
define constant $yellow-specular = make-v4(1.0, 1.0, 0.0, 1.0);
define constant $gray-specular   = make-v4(0.2, 0.2, 0.2, 0.5);
// define constant $white-ambient   = make-v4(0.3, 0.3, 0.3, 1.0);
// define constant $red-ambient     = make-v4(0.3, 0.1, 0.1, 1.0);
// define constant $green-ambient   = make-v4(0.1, 0.3, 0.1, 1.0);
// define constant $blue-ambient    = make-v4(0.1, 0.1, 0.3, 1.0);
// define constant $white-diffuse   = make-v4(1.0, 1.0, 1.0, 1.0);
// define constant $red-diffuse     = make-v4(1.0, 0.0, 0.0, 1.0);
// define constant $green-diffuse   = make-v4(0.0, 1.0, 0.0, 1.0);
// define constant $blue-diffuse    = make-v4(0.0, 0.0, 1.0, 1.0);

define constant *colors*
  = vector($red-specular, $green-specular, $purple-specular, 
           $orange-specular, $yellow-specular);

define variable *angle* :: <single-float> = 90.0;

define constant $number-rows :: <integer> = 7;
define constant $number-columns :: <integer> = $number-rows;

define variable $sw :: <single-float> = 2.0 / $number-rows;
define variable $sh :: <single-float> = 2.0 / $number-columns;

define variable *squares* :: <stretchy-object-vector> = make(<stretchy-vector>);

define sealed class <square> (<object>)
  constant slot r  :: <v3>, init-function: method () make-v3(0.0, 0.0, 0.0) end;
  constant slot p  :: <v3>, init-function: method () make-v3(0.0, 0.0, 0.0) end;
  constant slot r0 :: <simple-object-vector>, 
    init-function: 
      method () 
        make(<simple-object-vector>, size: $number-rows * $number-columns) 
      end;
  slot c;
end class;
   
define method initialize (s :: <square>, #key i, j, #all-keys) 
  s.r.x := (i - $number-columns / 2.0) * $sw + rnd(-$sw / 4.0, $sw / 4.0);
  s.r.y := (j - $number-rows / 2.0) * $sh + rnd(-$sh / 4.0, $sh / 4.0);
  s.r.z := rnd(0.25, 0.50);
  s.p.x := 0.0;
  s.p.y := 100.0;
  s.p.z := 0.0;
end method initialize;

define sealed domain make (singleton(<square>));
define sealed domain initialize (<square>);

define function draw (s :: <square>) => ()
  glPushAttrib($GL-LIGHTING-BIT);
    // The square ...
    glPushMatrix();
      glMaterialfv($GL-FRONT-AND-BACK, $GL-SPECULAR, s.c);
      glTranslatef(s.r.x, s.r.y, s.r.z);
      glRectf(0.0, 0.0, $sw, $sh);
    glPopMatrix();
    // Its "shadow" ...
    glPushMatrix();
      glMaterialfv($GL-FRONT-AND-BACK, $GL-SPECULAR, $gray-specular);
      glTranslatef(s.r.x, s.r.y, 0.01);
      glRectf(0.0, 0.0, $sw, $sh);
    glPopMatrix();
  glPopAttrib();
end function draw;

define variable *grid*
  = make(<array>, dimensions: list($number-rows, $number-columns));

define function initialize-grid () => ()
  let color-count :: <integer> = 0;
  for (i :: <integer> from 0 below $number-columns)
    for (j :: <integer> from 0 below $number-rows)
      if (blue-square?(i, j))
        if (rnd(0.0, 1.0) > 0.80)
          let s = make(<square>, i: i, j: j);
          add!(*squares*, s);
          s.c := *colors*[color-count];
          color-count := modulo(color-count + 1, 5);
          *grid*[i, j] := #t;
        else
          *grid*[i, j] := #f;
        end if;
      end if;
    end for;
  end for;
  for (sj :: <square> in *squares*,
       j :: <integer> from 0)
    for (si :: <square> in *squares*)
      if (si ~== sj)
        si.r0[j] := distance*(si.r.x * $sw, si.r.y * $sh, 0.0, 
                              sj.r.x * $sw, sj.r.y * $sh, 0.0);
      end if;
    end for;
  end for;
end function initialize-grid;

define variable dp :: <v3> = make-v3(0.0, 0.0, 0.0);
define variable rh :: <v3> = make-v3(0.0, 0.0, 0.0);
define variable dr :: <v3> = make-v3(0.0, 0.0, 0.0);
define variable tt :: <v3> = make-v3(0.0, 0.0, 0.0);

define function move-squares () => ()
  for (n :: <integer> from 0 below $updates-per-redisplay)
    for (si :: <square> in *squares*)
      vreplace(dp, si.r);
      vmul-s(dp, -$kg);
      tt.x := 0.0;
      tt.y := 0.0;
      let zz :: <single-float> = si.r.z;
      tt.z := $kz / (zz * zz * zz * zz * zz * zz);
      vadd(dp, tt);
      for (sj :: <square> in *squares*,
           r0ij in si.r0)
        if (sj ~== si)
          let dj :: <single-float> 
            = unit-vector-and-distance(rh, si.r, sj.r);
          vadd(dp, vmul-s(rh, -$kl * (dj - r0ij)));
        end if;
        vadd(si.p, vmul-s(dp, $dt));
      end for;
    end for;
    for (si :: <square> in *squares*)
      vreplace(dr, si.p);
      vmul-s(dr, 1.0 / $m);
      vadd(si.r, vmul-s(dr, $dt));
    end for;
  end for;
end function move-squares;


///
/// OpenGL PANE, DUIM FRAME
///

define constant $near-plane :: <double-float> = 0.5d0;
define constant $far-plane :: <double-float>  = 4.0d0;
define constant $view-angle :: <double-float> = 120.0d0;

define variable *latitude* :: <double-float>      = 60.0d0;
define variable *latitude-bump* :: <double-float> =  2.5d0;
define variable *longitude* :: <double-float>     = 30.0d0;
define variable *radius* :: <double-float>        = $near-plane + 1.5d0;
        
define constant $logo-plane     = 1;
define constant $solid-cylinder = 2;

define class <flying-squares-pane> (<gl-pane>) 
end class;

define function build-logo-plane () => ()
  glNewList($logo-plane, $GL-COMPILE);
    glPushAttrib($GL-LIGHTING-BIT);
      glMaterialfv($GL-FRONT-AND-BACK, $GL-SPECULAR, $blue-specular);
      for (i from 0 below $number-columns)
        for (j from 0 below $number-rows)
          if (blue-square?(i, j) & ~ *grid*[i, j])
            glPushMatrix();
              glTranslatef((i - $number-columns / 2.0) * $sw,
                           (j - $number-rows / 2.0) * $sh, 0.0);
              glRectf(0.0, 0.0, $sw, $sh);
            glPopMatrix();
          end if;
        end for;
      end for;
    glPopAttrib();
    glPushAttrib($GL-LIGHTING-BIT);
      glMaterialfv($GL-FRONT-AND-BACK, $GL-SPECULAR, $white-specular);
      for (i from 0 below $number-columns)
        for (j from 0 below $number-rows)
          if (~(blue-square?(i, j) & ~ *grid*[i, j]))
            glPushMatrix();
              glTranslatef((i - $number-columns / 2.0) * $sw,
                           (j - $number-rows / 2.0) * $sh, 0.0);
              glRectf(0.0, 0.0, $sw, $sh);
            glPopMatrix();
          end if;
        end for;
      end for;
    glPopAttrib();
  glEndList();
end function build-logo-plane;

define function build-solid-cylinder (radius :: <double-float>, height :: <double-float>)
 => ()
  glNewList($solid-cylinder, $GL-COMPILE);
    glPushMatrix();
      glRotatef(90.0, 1.0, 0.0, 0.0);
      glTranslatef(0.0, 0.0, -1.0);
      let quadObj = gluNewQuadric();
      gluQuadricDrawStyle(quadObj, $GLU-FILL);
      gluQuadricNormals(quadObj, $GLU-SMOOTH);
      gluCylinder(quadObj, radius, radius, height, 12, 2);
    glPopMatrix();
  glEndList()
end function build-solid-cylinder;

define function resize (width :: <integer>, height :: <integer>) => ()
  glViewport(0, 0, width, height);
  let fwidth = as(<double-float>, width);
  let fheight = as(<double-float>, height);
  let faspect = fwidth / fheight;
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  gluPerspective($view-angle, faspect, $near-plane, $far-plane);
  glMatrixMode($GL-MODELVIEW);
  glLoadIdentity();
end function resize;

define function update-flying-squares (pane :: <flying-squares-pane>) => ()
  repaint-sheet(pane, $everywhere);
  *latitude* := *latitude* + *latitude-bump*;
  case
    *latitude* > 75.0d0 =>
      *latitude* := 75.0d0;
      *latitude-bump* := - *latitude-bump*;
    *latitude* < -75.0d0 =>
      *latitude* := -75.0d0;
      *latitude-bump* := - *latitude-bump*;
  end;
  *longitude* := *longitude* - 3.75d0;
  *angle* := *angle* + 6.0;
  move-squares();
  let frame = sheet-frame(pane);
  with-lock (frame.lock)
    release(frame.update-notification)
  end
end function update-flying-squares;

define method handle-initialize-scene 
    (pane :: <flying-squares-pane>, medium :: <medium>) => ()
  next-method();
  let ambient-properties = make-v4(0.7, 0.7, 0.7, 1.0);
  let diffuse-properties = make-v4(0.8, 0.8, 0.8, 1.0);
  let specular-properties = make-v4(1.0, 1.0, 1.0, 1.0);
  *squares*.size := 0;
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClearIndex(0.0);
  glClearDepth(1.0d0);
  glEnable($GL-DEPTH-TEST);
  glEnable($GL-LIGHTING);
  glLightfv($GL-LIGHT0, $GL-AMBIENT, ambient-properties);
  glLightfv($GL-LIGHT0, $GL-DIFFUSE, diffuse-properties);
  glLightfv($GL-LIGHT0, $GL-SPECULAR, specular-properties);
  glLightModelf($GL-LIGHT-MODEL-TWO-SIDE, 1.0);
  glEnable($GL-LIGHT0);
  let (width, height) = sheet-size(pane);
  resize(width, height);
  initialize-grid();
  build-logo-plane();
  build-solid-cylinder(0.05d0, 0.3d0);
  make(<thread>,
       function: method ()
                   let frame = sheet-frame(pane);
                   while(#t)
                     sleep(1.0 / 60.0);                     
                     with-lock (frame.lock)
                       call-in-frame(frame, update-flying-squares, pane);
                       wait-for(frame.update-notification)
                     end
                   end
                 end)
end method handle-initialize-scene;

define method do-allocate-space
    (pane :: <flying-squares-pane>, width :: <integer>, height :: <integer>) => ()
  resize(width, height)
end method do-allocate-space;

define function polar-view (radius :: <double-float>, twist :: <double-float>,
                            latitude :: <double-float>, longitude :: <double-float>) => ()
  glTranslated(0.0d0, 0.0d0, -radius);
  glRotated( -twist, 0.0d0, 0.0d0, 1.0d0);
  glRotated( -latitude, 1.0d0, 0.0d0, 0.0d0);
  glRotated( longitude, 0.0d0, 0.0d0, 1.0d0);    
end function polar-view;

// define constant $light0-position = make-v4(0.0, 0.0, -1.0, 1.0);

define function draw-light () => ()
/*---*** Using this code to move the light position makes it really ugly ...
  glPushMatrix();
    glRotatef(*angle*, 1.0, 0.0, 1.0);
    glTranslatef(0.0, 1.5, 0.0);
    glLightfv($GL-LIGHT0, $GL-POSITION, $light0-position);
    glPushAttrib($GL-LIGHTING-BIT);
      glDisable($GL-LIGHTING);
      glColor3f(1.0, 1.0, 1.0);
      glCallList($solid-cylinder);
    glPopAttrib();
  glPopMatrix();
*/
end function draw-light;

define method handle-repaint-scene 
    (pane :: <flying-squares-pane>, medium :: <medium>) => ()
  // Draw scene...
  glClear(logior($GL-COLOR-BUFFER-BIT, $GL-DEPTH-BUFFER-BIT));
  glPushMatrix();
    polar-view(*radius*, 0.0d0, *latitude*, *longitude*);
    draw-light();
    glCallList($logo-plane);
    for (s in *squares*)
      draw(s)
    end for;
  glPopMatrix();
end method handle-repaint-scene;

define frame <flying-squares-frame> (<simple-frame>)
  constant slot lock :: <lock>, init-value: make(<lock>);
  slot update-notification :: false-or(<notification>), init-value: #f;
  pane file-menu (frame)
    make(<menu>,
         label: "&File",
         children:
           vector(make(<menu-button>,
                       label: "New",
                       activate-callback:
                         method (sheet)
                           *squares*.size := 0;
                           initialize-grid();
                           build-logo-plane()
                         end),
                  make(<menu-button>,
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
        make(<flying-squares-pane>,
	     width:  400, min-width:  100,
             height: 600, min-height: 100)
      end;
    end;
  status-bar (frame)
    make(<status-bar>);
  keyword title: = "Harlequin Flying Squares";
end frame;

define method initialize (frame :: <flying-squares-frame>, #key)
  next-method();
  frame.update-notification := make(<notification>, lock: frame.lock)
end method initialize;

define function main () => ()
  start-frame(make(<flying-squares-frame>));
end function main;

begin
  main();
end;
