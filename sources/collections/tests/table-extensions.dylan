Module: collections-test-suite

define test test-tabling-macro-without-class ()
  assert-no-errors(tabling(), "tabling() with no args");

  let t = tabling(1 => 2, 3 => 4);
  assert-instance?(<table>, t);
  assert-equal(2, t.size);
  assert-equal(2, t[1]);
  assert-equal(4, t[3]);
end test;

define test test-tabling-macro-with-class ()
  assert-no-errors(tabling(<string-table>), "tabling() with class but no keyvals");

  let t = tabling(<string-table>, "a" => 1, "b" => 2);
  assert-instance?(<string-table>, t);
  assert-equal(2, t.size);
  assert-equal(1, t["a"]);
  assert-equal(2, t["b"]);
end test;
