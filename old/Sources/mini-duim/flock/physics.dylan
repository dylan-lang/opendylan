module:    flock
Synopsis:  Flying tiles
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $number-rows :: <small-integer> = 7;
define constant $number-columns :: <small-integer> = $number-rows;
define constant $max-squares :: <small-integer> = $number-rows * $number-columns;

define constant $dx :: <single-float> = 0.2;
define constant $dy :: <single-float> = $dx;
define constant $dz :: <single-float> = $dy;
define constant $dr :: <single-float> = 30.0;

define constant $sw :: <single-float> = 2.0 / $number-rows;
define constant $sh :: <single-float> = 2.0 / $number-columns;

define variable *grid*
  = make(<array>, dimensions: list($number-rows, $number-columns));

define class <square> (<object>)
  slot r  :: <v3>, init-function: method () make (<v3>) end;
  slot p  :: <v3>, init-function: method () make (<v3>) end;
  slot a  :: <v3>, init-function: method () make (<v3>) end;
  slot la :: <v3>, init-function: method () make (<v3>) end;
  slot r0 :: <simple-object-vector>, 
    init-function: 
      method () 
        make(<simple-object-vector>, size: $number-rows * $number-columns) 
      end;
  slot c;
end class;

define variable *squares* :: <stretchy-vector> = make(<stretchy-vector>);

// global confinement spring constant.	kg > 0, larger values 
// confine tile closer to origin

define constant $kg :: <single-float> = 2000.0;

// local spring constant between tiles.	kg > 0, larger values keep
// tile separation closer to r0

define constant $kl :: <single-float> = 10000.0;

// tile mass

define constant $m :: <single-float> = 1000.0;

// z penalty

define constant $kz :: <single-float> = 1.0;

// time constant

define constant $dt :: <single-float> = 0.01;

// rotational constants

define constant $ia :: <single-float> = 1.0;
define constant $kla :: <single-float> = 1.0;

// magnitude error correction

define constant $max-momentum-magitude :: <single-float> = 5000.0;

define method rnd (min :: <single-float>, max :: <single-float>)
  random-uniform(from: min, to: max);
end method rnd;

define class <v3> (<object>)
  slot x :: <single-float>, init-value: 0.0, init-keyword: x:;
  slot y :: <single-float>, init-value: 0.0, init-keyword: y:;
  slot z :: <single-float>, init-value: 0.0, init-keyword: z:;
end class;

define method add (a :: <v3>, b :: <v3>)
  a.x := a.x + b.x;
  a.y := a.y + b.y;
  a.z := a.z + b.z;
  a
end method;

define method sub (a :: <v3>, b :: <v3>)
  a.x := a.x - b.x;
  a.y := a.y - b.y;
  a.z := a.z - b.z;
  a
end method;

define method replace (a :: <v3>, b :: <v3>)
  a.x := b.x;
  a.y := b.y;
  a.z := b.z;
  a
end method;

define method mul-s (v :: <v3>, k :: <single-float>)
  v.x := k * v.x;
  v.y := k * v.y;
  v.z := k * v.z;
  v
end method;

define method mul (a :: <v3>, b :: <v3>)
  a.x * b.x + a.y * b.y + a.z * b.z
end method;

define method distance*
    (ax :: <single-float>, ay :: <single-float>, az :: <single-float>,
     bx :: <single-float>, by :: <single-float>, bz :: <single-float>)
  let dx :: <single-float> = ax - bx;
  let dy :: <single-float> = ay - by;
  let dz :: <single-float> = az - bz;
  sqrt(dx * dx + dy * dy + dz * dz)
end method;

define method distance (a :: <v3>, b :: <v3>)
  distance*(a.x, a.y, a.z, b.x, b.y, b.z)
end method;
   
define method magnitude (v :: <v3>)
  sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
end method;
   
define method unit-vector-and-distance (u :: <v3>, a :: <v3>, b :: <v3>)
  let dx :: <single-float> = a.x - b.x;
  let dy :: <single-float> = a.y - b.y;
  let dz :: <single-float> = a.z - b.z;
  let d  :: <single-float> = sqrt(dx * dx + dy * dy + dz * dz);
  u.x := dx / d;
  u.y := dy / d;
  u.z := dz / d;
  d
end method;
   
define method initialize (s :: <square>, #key i, j, #all-keys) 
  s.r.x := (i - $number-columns / 2.0) 
    * $sw + rnd(-$sw / 4.0, $sw / 4.0) + 4.0 * $sw;
  s.r.y := (j - $number-rows / 2.0) 
    * $sw + rnd(-$sh / 4.0, $sh / 4.0);
  s.r.z := rnd(0.25, 0.50);
  s.p.x := 0.0;
  s.p.y := 100.0;
  s.p.z := 0.0;
  s.a.x := rnd(-20.0, 20.0);
  s.a.y := rnd(-20.0, 20.0);
  s.a.z := rnd(-20.0, 20.0);
  s.la.x := 10.0;
  s.la.y := 0.0;
  s.la.z := 0.0;
end method;

define method blue-square?
    (i :: <small-integer>, j :: <small-integer>) => (_ :: <boolean>)
  let ib = logand(i, 1);
  let jb = logand(j, 1);
  (ib = 1 & jb = 1) | (ib = 0 & jb = 0)
end method;

define method initialize-grid ()
  let color-count = 0;
  for (i from 0 below $number-columns)
    for (j from 0 below $number-rows)
      if (blue-square?(i, j))
	if (rnd(0.0, 1.0) > 0.80)
	  let s = make(<square>, i: i, j: j);
	  add!(*squares*, s);
	  // s.c := *colors*[color-count];
	  color-count := modulo(color-count + 1, 5);
	  *grid*[i, j] := #t;
	else
	  *grid*[i, j] := #f;
	end if;
      end if;
    end for;
  end for;

  for (sj in *squares*, j from 0)
    for (si in *squares*)
      if (si ~== sj)
	si.r0[j] := distance*(si.r.x * $sw, si.r.y * $sh, 0.0, 
                              sj.r.x * $sw, sj.r.y * $sh, 0.0);
      end if;
    end for;
  end for;
end method;

define variable dp :: <v3> = make(<v3>);
define variable rh :: <v3> = make(<v3>);
define variable dr :: <v3> = make(<v3>);
define variable tt :: <v3> = make(<v3>);
define variable dla :: <v3> = make(<v3>);
define variable da :: <v3> = make(<v3>);

define method move-squares ()
    for (si in *squares*)
      replace(dp, si.r);
      mul-s(dp, -$kg);
      tt.x := 0.0;
      tt.y := 0.0;
      let zz :: <single-float> = si.r.z;
      tt.z := $kz / (zz * zz * zz * zz * zz * zz);
      add(dp, tt);
      
      replace(dla, si.a);
      mul-s(dla, as(<single-float>, 1 - *squares*.size));

      for (sj in *squares*, r0ij in si.r0)
	if (sj ~== si)
	  let dj :: <single-float> 
	    = unit-vector-and-distance(rh, si.r, sj.r);
	  add(dp, mul-s(rh, -$kl * (dj - r0ij)));
          add(dla, sj.a);
	end if;
	add(si.p, mul-s(dp, $dt));
	add(si.la, mul-s(dla, $dt * $kla));
      end for;
    end for;
    
    for (si in *squares*)
      replace(dr, si.p);
      mul-s(dr, 1.0 / $m);
      let m = magnitude(si.p);
      if (m > $max-momentum-magitude)
	mul-s(si.p, 0.5);
      end if;
      add(si.r, mul-s(dr, $dt));

      replace(da, si.la);
      add(si.a, mul-s(da, $dt / $ia));
    end for;
end method;


