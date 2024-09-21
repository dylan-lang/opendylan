Module: common-dylan-test-suite


define test test-microsecond-counter ()
  let t1 = microsecond-counter();
  sleep(0.1);
  let t2 = microsecond-counter();
  let diff = t2 - t1;
  // I need slop to be at least 5200 on my macOS M3 Pro. Be more generous than
  // that since the main point is simply to exercise the function at all. --cgay
  let slop = 50_000;
  assert-true(diff >= (100_000 - slop) & diff < (100_000 + slop),
              diff);
end test;
