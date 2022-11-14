Module: file-source-records-test-suite

define test test-read-header-from-stream/empty-header ()
  let header = "";
  let stream = make(<string-stream>, contents: header, direction: #"input");
  let (keys, nlines, nchars) = read-header-from-stream(stream);
  assert-equal(0, keys.size);
  assert-equal(0, nlines);
  assert-equal(0, nchars);
end test;

define test test-read-header-from-stream/no-trailing-newline ()
  for (header in #("Module: m\nSynopsis:   s",          // no newline at end
                   "Module: m\nSynopsis:s   "))         // no space after colon
    let stream = make(<string-stream>, contents: header, direction: #"input");
    let (keys, nlines, nchars) = read-header-from-stream(stream);
    assert-equal(2, keys.size);
    assert-equal(1, nlines);
    assert-equal(23, nchars);
    assert-equal(#("m"), keys[#"module"]);
    assert-equal(#("s"), keys[#"synopsis"]);
  end;
end test;

define test test-read-header-from-stream/no-continuation-lines ()
  for (header in #("Module: m\nSynopsis:   s\n\n123\n", // stops at empty line?
                   "Module: m  \nSynopsis: s\n\n123\n")) // space after 'm' stripped?
    let stream = make(<string-stream>, contents: header, direction: #"input");
    let (keys, nlines, nchars) = read-header-from-stream(stream);
    assert-equal(2, keys.size);
    assert-equal(3, nlines);
    assert-equal(25, nchars);
    assert-equal(#("m"), keys[#"module"]);
    assert-equal(#("s"), keys[#"synopsis"]);
  end;
end test;

define test test-read-header-from-stream/continuation-lines ()
  let header = "Module: m\nSynopsis: a \n b\n c\nXYZ: xyz\n\n123\n";
  let stream = make(<string-stream>, contents: header, direction: #"input");
  let (keys, nlines, nchars) = read-header-from-stream(stream);
  assert-equal(3, keys.size);
  assert-equal(6, nlines);
  assert-equal(39, nchars);
  assert-equal(#("m"), keys[#"module"]);
  assert-equal(#("a", "b", "c"), keys[#"synopsis"]);
  assert-equal(#("xyz"), keys[#"xyz"]);
end test;
