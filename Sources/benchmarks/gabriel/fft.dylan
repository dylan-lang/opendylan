Module: gabriel-benchmarks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <single-float-vector> = limited(<vector>, of: <single-float>);

define constant **fft-re** :: <single-float-vector>
  = make(<single-float-vector>, size: 1025, fill: 0.0);

define constant **fft-im** :: <single-float-vector>
  = make(<single-float-vector>, size: 1025, fill: 0.0);

define function fft
    (areal :: <single-float-vector>, aimag :: <single-float-vector>)
  let ar :: <single-float-vector> = areal;
  let ai :: <single-float-vector> = aimag;
  let i :: <integer> = 1;
  let j :: <integer> = 0;
  let k :: <integer> = 0;
  let m :: <integer> = 0;
  let n :: <integer> = size(ar) - 1;
  let nv2 :: <integer> = floor/(n, 2);
  let le :: <integer> = 0;
  let le1 :: <integer> = 0;
  let ip :: <integer> = 0;
  let ur :: <single-float> = 0.0;
  let ui :: <single-float> = 0.0;
  let wr :: <single-float> = 0.0;
  let wi :: <single-float> = 0.0;
  let tr :: <single-float> = 0.0;
  let ti :: <single-float> = 0.0;
  while (i < n)
    m := m + 1;  i := i + i
  end;
  if (n ~== (2 ^ m))
    format-out("Error: array size must be a power of two.");
  else
    j := 1;     // interchange elements in bit-reversed order
    i := 1;
    local method l3 ()
            if (i < j)
	      tr := ar[j];
	      ti := ai[j];
	      ar[j] := ar[i];
	      ai[j] := ai[i];
	      ar[i] := tr;
	      ai[i] := ti;
	    end if;
	    k := nv2;
	    while (k < j)
	      j := j - k;
	      k := truncate/(k, 2);  // (the fixnum (/ k 2))
	    end;
	    j := j + k;
	    i := i + 1;
            if (i < n) l3() end;
          end method l3;
    l3();
    for (l :: <integer> from 1 to m)
      le := 2 ^ l;
      le1 := floor/(le, 2);
      ur := 1.0;
      ui := 0.0;
      wr := cos($single-pi / le1);
      wi := sin($single-pi / le1);
      for (j :: <integer> from 1 to le1)
        for (i :: <integer> from j to n by le)  // do a butterfly
          ip := i + le1;
          tr := ar[ip] * ur - ai[ip] * ui;
          ti := ar[ip] * ui + ai[ip] * ur;
          ar[ip] := ar[i] - tr;
          ai[ip] := ai[i] - ti;
        end for;
      end for;
      tr := ur * wr - ui * wi;
      ti := ur * wi + ui * wr;
      ur := tr;
      ui := ti;
    end for;
    #t
  end if
end function fft;

define function fft-bench ()
  for (i from 1 to 10)
    fft(**fft-re**, **fft-im**);
  end for;
end function fft-bench;

define constant testfft = fft-bench;

///
/// The following are for verifying that the implementation gives the
/// correct result.
///
define constant <double-float-vector> = limited(<vector>, of: <double-float>);

define constant **fft-re-d** :: <double-float-vector>
  = make(<double-float-vector>, size: 1025, fill: 0.0d0);

define variable **fft-im-d** :: <double-float-vector>
  = make(<double-float-vector>, size: 1025, fill: 0.0d0);


define function clear-fft ()
  for (i from 0 below 1025)
    **fft-re-d**[i] := 0.0d0;
    **fft-im-d**[i] := 0.0d0;
  end for;
end function clear-fft;

// ---*** Note that in the Common Lisp version this function used "pi" rather
//        than s-pi.  The equivalent in Dylan would seem to be to use $double-pi
//        here, but that doesn't type correctly.
define function setup-fft-component (theta, #key phase = 0.0)
  let f = 2 * $double-pi * theta;
  let c = cos(0.5 * $double-pi * phase);
  let s = sin(0.5 * $double-pi * phase);
  for (i from 0 below 1025)
    let x = sin(f * (i / 1024.0));
    **fft-re-d**[i] := **fft-re-d**[i] + c * x;
    **fft-im-d**[i] := **fft-re-d**[i] + s * x;
  end for;
end function setup-fft-component;

define variable fft-delta :: <double-float> = 0.0001d0;

define function print-fft ()
  for (i from 0 below 1025)
    let re = **fft-re-d**[i];
    let im = **fft-im-d**[i];
    unless ((abs(re) < fft-delta) & (abs(im) < fft-delta))
      format-out("%d  %= %=\n", i, re, im);
    end;
  end for;
end function print-fft;

define benchmark fft = testfft;

ignorable(clear-fft, setup-fft-component, print-fft);

