Module: fft-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <float-vector>
  = limited(<simple-object-vector>, of: <single-float>);

define constant $ar = make(<float-vector>, size: 1025, fill: 0.0);
define constant $ai = make(<float-vector>, size: 1025, fill: 0.0);

define function timed-fft-test ()
  let count = 0;
  let mulcount = 0;
  let (elapsed-seconds, elapsed-microseconds)
    = timing ()
        let (c, m) = fft-test();
        count := c;
        mulcount := m;
      end;
  format-out("fft(%d) in %d.%s seconds with %d multiplies\n", 
	     count, elapsed-seconds, 
	     integer-to-string(elapsed-microseconds, size: 6),
	     mulcount)
end function timed-fft-test;

define function fft-test () => (count :: <integer>, mulcount :: <integer>)
  let ar :: <float-vector> = $ar;
  let ai :: <float-vector> = $ai;
  let count :: <integer> = 100;
  let tr :: <single-float> = 0.0;
  let ti :: <single-float> = 0.0;
  let mulcount :: <integer> = 0;
  let ip :: <integer> = 0;
  let n :: <integer> = 1024;
  let nv2 :: <integer> = floor/(n,2);
  let nm1 :: <integer> = n - 1;
  for (counter :: <integer> from 0 below count)
    let m :: <integer> = 0;
    let i :: <integer> = 1;
    while (i < n)
      m := m + 1;
      i := i + i;
    end;
    let j :: <integer> = 1;
    for (i :: <integer> from 1 below n)
      if (i < j)
	tr := ar[j];
	ti := ai[j];
	ar[j] := ar[i];
	ai[j] := ai[i];
	ar[i] := tr;
	ai[i] := ti;
      end;
      let k :: <integer> = nv2;
      while (k < j)
	j := j - k;
	k := floor/(k,2);
      end;
      j := j + k;
    end;
    for (l :: <integer> from 1 to m)
      let le :: <integer> = 2 ^ l;
      let le1 :: <integer> = floor/(le, 2);
      let ur :: <single-float> = 1.0;
      let ui :: <single-float> = 0.0;
      let wr :: <single-float> = cos(floor/($single-pi, le1));
      let wi :: <single-float> = sin(floor/($single-pi, le1));
      for (j :: <integer> from 1 to le1)
	for (i :: <integer> from j to n by le)
	  ip := i + le1;
	  let ar-ip :: <single-float> = ar[ip];
	  let ai-ip :: <single-float> = ai[ip];
	  tr := ar-ip * ur - ai-ip * ui;
	  ti := ar-ip * ur + ai-ip * ui;
	  mulcount := mulcount + 4;
	  
	  let ar-i :: <single-float> = ar[i];
	  let ai-i :: <single-float> = ai[i];
	  ar[ip] := ar-i - tr;
	  ai[ip] := ai-i - ti;
	  
	  let ar-i :: <single-float> = ar[i];
	  let ai-i :: <single-float> = ai[i];
	  ar[i] := ar-i + tr;
	  ai[i] := ai-i + ti;
	end;
      end;
      tr := ur * wr - ui * wi;
      ti := ur * wi + ui * wr;
      mulcount := mulcount + 4;
      ur := tr;
      ui := ti;
    end;
  end;
  values(count, mulcount)
end function fft-test;

timed-fft-test();
