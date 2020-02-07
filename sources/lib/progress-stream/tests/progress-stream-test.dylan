Module: progress-stream-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2020 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Tests for module progress-stream

define interface-specification-suite progress-stream-specification-suite ()
  abstract class <progress-stream> (<wrapper-stream>);
  function stream-supports-show-progress? (<stream>) => (<boolean>);
  function show-progress (<progress-stream>, <integer>, <integer>, #"key", #"label") => ();
end progress-stream-specification-suite;

define test test-<progress-stream> ()
  let output
    = with-output-to-string (s)
        let progress = make(<progress-stream>, inner-stream: s);
        format(progress, "line 1\n");
      end;
  assert-equal("line 1\n", output);
end test;

define test test-stream-supports-show-progress? ()
  let output
    = with-output-to-string (s)
        let progress-unforced = make(<progress-stream>, inner-stream: s);
        assert-false(stream-supports-show-progress?(progress-unforced));
        let progress-forced
          = make(<progress-stream>, inner-stream: s, force?: #t);
        assert-true(stream-supports-show-progress?(progress-forced));
      end;
end test;

define test test-show-progress-simple ()
  let output
    = with-output-to-string (s)
        let progress
          = make(<progress-stream>, inner-stream: s, force?: #t,
                 bar-width: 40);
        show-progress(progress, 17, 43);
        show-progress(progress, 22, 43, label: "Snerb");
        show-progress(progress, 22, 43, label: "Snerb");
        show-progress(progress, 22, 43, label: "Sner");
      end;
  assert-equal("\r[================                        ]"
               "\r[====================                    ] Snerb"
               "\r[====================                    ] Sner ",
               output);
end test;

define test test-show-progress-mixed ()
  let output
    = with-output-to-string (s)
        let progress
          = make(<progress-stream>, inner-stream: s, force?: #t,
                 bar-width: 40);
        format(progress, "line 1\n");
        show-progress(progress, 17, 43);
        format(progress, "line 2\n");
      end;
  assert-equal("line 1\n"
               "\r[================                        ]"
               "\r                                          \r"
               "line 2\n",
               output);
end test;

define test test-show-progress-truncate ()
  let output
    = with-output-to-string (s)
        let progress
          = make(<progress-stream>, inner-stream: s, force?: #t,
                 bar-width: 20, line-width: 36);
        show-progress(progress, 43, 43);
        show-progress(progress, 43, 43, label: "thalassocracy");
        show-progress(progress, 43, 43, label: "thalassography");
        show-progress(progress, 43, 43, label: "thalassocrat");
      end;
  assert-equal("\r[====================]"
               "\r[====================] thalassocracy"
               "\r[====================] thalassogr..."
               "\r[====================] thalassocrat ",
               output);
end test;

define test test-show-progress-zero-range ()
  let output
    = with-output-to-string (s)
        let progress
          = make(<progress-stream>, inner-stream: s, force?: #t,
                 bar-width: 20, line-width: 36);
        show-progress(progress, 0, 0);
      end;
  assert-equal("\r[====================]",
               output);
end test;

define suite progress-stream-module-test-suite ()
  test test-<progress-stream>;
  test test-stream-supports-show-progress?;
  test test-show-progress-simple;
  test test-show-progress-mixed;
  test test-show-progress-truncate;
  test test-show-progress-zero-range;
end suite;

define suite progress-stream-test-suite ()
  suite progress-stream-specification-suite;
  suite progress-stream-module-test-suite;
end suite;
