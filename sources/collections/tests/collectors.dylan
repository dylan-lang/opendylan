Module: collections-test-suite


define test test-collect ()
  assert-instance?(<list>, collecting() end, "default collection type is <list>?");
  assert-equal(#(1, 2, 3),
               collecting ()
                 collect(1);
                 collect(2);
                 collect(3);
               end,
               "items are added at the end by default for lists?");
  assert-equal(#(2, 1, 3),
               collecting ()
                 collect(1);
                 collect-first(2);
                 collect-last(3);
               end,
               "collect-first adds to the beginning of the collection?");
  let c = collecting ()
            collect(1);
            collect(2);
            collect-first(3);
            assert-equal(#(3, 1, 2), collected(),
                         "collected() works for unnamed collections?");
            collect(5);
          end;
  assert-equal(#(3, 1, 2, 5), c, "collecting returns the collection?");
end test;

// Note that for named collections the collection isn't automatically returned from the
// body of `collecting`, unlike for unnamed collections. See comment in the `collecting`
// macro.
define test test-collect-into ()
  assert-equal(#(1, 2, 3),
               collecting (c)
                 collect-into(c, 1);
                 collect-into(c, 2);
                 collect-into(c, 3);
                 collected(c)
               end,
               "items are added at the end by default?");
  assert-equal(#(2, 1, 3),
               collecting (c)
                 collect-into(c, 1);
                 collect-first-into(c, 2);
                 collect-last-into(c, 3);
                 collected(c)
               end,
               "collect-first-into adds to the beginning of the collection?");
  let cc = collecting (c)
             collect-first-into(c, 1); // first/last should not matter here
             assert-equal(#(1), collected(c));
             collect-last-into(c, 2);
             assert-equal(#(1, 2), collected(c));
             collect-first-into(c, 3);
             assert-equal(#(3, 1, 2), collected(c),
                          "collected(c) works for named collections?");
             collect-last-into(c, 5);
             collected(c)
           end;
  assert-equal(#(3, 1, 2, 5), cc, "collecting returns the collection?");
end test;

define test test-collecting-as ()
  let c = collecting (as <vector>)
            collect(1);
            collect(2);
          end;
  assert-instance?(<vector>, c);
  assert-equal(#[1, 2], c);

  let c = collecting (as <integer>)
            collect(1);
            collect(2);
          end;
  assert-equal(c, 3);
end test;

