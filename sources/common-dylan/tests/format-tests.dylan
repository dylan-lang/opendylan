Module: common-dylan-test-suite

define test test-character-to-integer ()
  for (char in "0123456789",
       code from 0)
    assert-equal(code, character-to-integer(char),
                 format-to-string("code for %c is %d", char, code));
  end;
  for (char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
       code from 10)
    assert-equal(code, character-to-integer(char),
                 format-to-string("code for %c is %d", char, code));
    assert-equal(code, character-to-integer(as-lowercase(char)),
                 format-to-string("code for %c is %d", as-lowercase(char), code));
  end;
end test;

define test test-string-to-machine-word ()
  local method mword (int)
          as(<machine-word>, int)
        end;
  assert-signals(<error>, string-to-machine-word(""));
  assert-signals(<error>, string-to-machine-word("G"));

  let (value, epos) = string-to-machine-word("123G");
  assert-equal(mword(#x123), value);
  assert-equal(3, epos);

  assert-equal(mword(#x123), string-to-machine-word("G0123G", start: 1));
  assert-equal(mword(#x12), string-to-machine-word("G0123G", start: 1, end: 4));
  assert-equal(mword(#x9abcdef), string-to-machine-word("9abcdef"));
end test;
