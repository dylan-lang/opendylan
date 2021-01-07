Module: common-dylan-test-suite
Synopsis: Tests for simple-random:common-dylan


/*---*** andrewa: not used yet...
define method chi-square
    (N :: <integer>, range :: <integer>) => (chi-square :: <integer>)
  let f = make(<simple-object-vector>, size: range, fill: 0);
  for (i from 0 below N)
    let rand = random(range);
    f[rand] := f[rand] + 1;
  end;
  let t = 0;
  for (i from 0 below range) t := t + f[i] * f[i] end;
  floor/(range * t, N) - N
end method chi-square;
*/

define test test-random ()
  // We should use chi-square somehow, but we don't want it to be slow.
  // Also, what value should it be returning?
  //---*** Fill this in...
end test;

define test test-<random> ()
  //---*** Fill this in...
end test;

define suite simple-random-test-suite ()
  test test-random;
  test test-<random>;
end suite;
