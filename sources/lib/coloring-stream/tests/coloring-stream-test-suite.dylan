Module:    coloring-stream-test-suite
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define module-spec coloring-stream ()
  abstract instantiable class <coloring-stream> (<wrapper-stream>);

  function colorize-stream
      (<stream>, #"key", #"force-ansi?")
   => (<coloring-stream>);

  open generic-function stream-supports-color? (<stream>) => (<boolean>);

  open generic-function stream-supports-ansi-color? (<stream>) => (<boolean>);

  constant $normal-intensity :: <object>;
  constant $bright-intensity :: <object>;
  constant $dim-intensity :: <object>;

  constant $color-black :: <text-color>;
  constant $color-blue :: <text-color>;
  constant $color-cyan :: <text-color>;
  constant $color-default :: <text-color>;
  constant $color-green :: <text-color>;
  constant $color-magenta :: <text-color>;
  constant $color-red :: <text-color>;
  constant $color-white :: <text-color>;
  constant $color-yellow :: <text-color>;

  constant $reset-attributes :: <object>;

  instantiable class <text-attributes> (<object>);

  function text-attributes
      (#"key", #"foreground", #"background", #"intensity")
   => (<text-attributes>);
end module-spec coloring-stream;

define sideways method make-test-instance
    (class == <coloring-stream>)
 => (object)
  make(<coloring-stream>, inner-stream: *standard-output*);
end method make-test-instance;

define coloring-stream class-test <coloring-stream> ()
  //---*** Fill this in...
end class-test <coloring-stream>;

define coloring-stream function-test stream-supports-color? ()
  let string-stream = make(<string-stream>, direction: #"output");
  check-false("String streams are not colorizable.",
              stream-supports-color?(string-stream));
end function-test stream-supports-color?;

define coloring-stream function-test colorize-stream ()
  let c = colorize-stream(*standard-output*);
  check-instance?("colorize-stream(file stream) returns a <coloring-stream>",
                  <coloring-stream>, c);

  let string-stream = make(<byte-string-stream>, direction: #"output");
  let c = colorize-stream(string-stream);
  check-instance?("colorize-stream(string stream) returns a <null-coloring-stream>",
                  <null-coloring-stream>, c);
  let f = colorize-stream(c, force-ansi?: #t);
  check-instance?("colorize-stream can be forced to rewrap as ansi stream",
                  <ansi-coloring-stream>, f);
  check-equal("colorize-stream on coloring stream returns same",
              c, colorize-stream(c));

  let c = colorize-stream(string-stream, force-ansi?: #t);
  check-instance?("colorize-stream can be forced to return an ansi stream",
                  <ansi-coloring-stream>, c);
  check-equal("colorize-stream on ansi coloring stream returns same",
              c, colorize-stream(c));
end function-test colorize-stream;

define function encode-to-ansi (attributes :: false-or(<text-attributes>))
  with-output-to-string (s)
    let ansi-stream = colorize-stream(s, force-ansi?: #t);
    print-object(attributes, ansi-stream);
  end with-output-to-string
end function encode-to-ansi;

define coloring-stream constant-test $normal-intensity ()
  let attr = text-attributes(intensity: $normal-intensity);
  assert-equal(encode-to-ansi(attr), "\<1b>[0m");
end constant-test $normal-intensity;

define coloring-stream constant-test $bright-intensity ()
  let attr = text-attributes(intensity: $bright-intensity);
  assert-equal(encode-to-ansi(attr), "\<1b>[1m");
end constant-test $bright-intensity;

define coloring-stream constant-test $dim-intensity ()
  let attr = text-attributes(intensity: $dim-intensity);
  assert-equal(encode-to-ansi(attr), "\<1b>[2m");
end constant-test $dim-intensity;

define coloring-stream constant-test $color-black ()
  let attr = text-attributes(foreground: $color-black);
  assert-equal(encode-to-ansi(attr), "\<1b>[30m");
end constant-test $color-black;

define coloring-stream constant-test $color-blue ()
  let attr = text-attributes(foreground: $color-blue);
  assert-equal(encode-to-ansi(attr), "\<1b>[34m");
end constant-test $color-blue;

define coloring-stream constant-test $color-cyan ()
  let attr = text-attributes(foreground: $color-cyan);
  assert-equal(encode-to-ansi(attr), "\<1b>[36m");
end constant-test $color-cyan;

define coloring-stream constant-test $color-default ()
  let attr = text-attributes(foreground: $color-default);
  assert-equal(encode-to-ansi(attr), "\<1b>[39m");
end constant-test $color-default;

define coloring-stream constant-test $color-green ()
  let attr = text-attributes(foreground: $color-green);
  assert-equal(encode-to-ansi(attr), "\<1b>[32m");
end constant-test $color-green;

define coloring-stream constant-test $color-magenta ()
  let attr = text-attributes(foreground: $color-magenta);
  assert-equal(encode-to-ansi(attr), "\<1b>[35m");
end constant-test $color-magenta;

define coloring-stream constant-test $color-red ()
  let attr = text-attributes(foreground: $color-red);
  assert-equal(encode-to-ansi(attr), "\<1b>[31m");
end constant-test $color-red;

define coloring-stream constant-test $color-white ()
  let attr = text-attributes(foreground: $color-white);
  assert-equal(encode-to-ansi(attr), "\<1b>[37m");
end constant-test $color-white;

define coloring-stream constant-test $color-yellow ()
  let attr = text-attributes(foreground: $color-yellow);
  assert-equal(encode-to-ansi(attr), "\<1b>[33m");
end constant-test $color-yellow;

define coloring-stream constant-test $reset-attributes ()
  let attr = $reset-attributes;
  assert-equal(encode-to-ansi(attr), "\<1b>[0m");
end constant-test $reset-attributes;

define coloring-stream function-test text-attributes ()
  let attr = text-attributes(foreground: $color-red,
                             background: $color-green,
                             intensity: $bright-intensity);
  check-equal("complex attributes work",
              encode-to-ansi(attr), "\<1b>[31;42;1m");
end function-test text-attributes;

define coloring-stream class-test <text-attributes> ()
  // Tested via the other tests.
end class-test <text-attributes>;

define test format-support-test ()
  check-equal("Using format works with color",
              with-output-to-string (s)
                let ansi-stream = colorize-stream(s, force-ansi?: #t);
                let error-attributes = text-attributes(foreground: $color-red);
                format(ansi-stream, "%=Hello%=!", error-attributes, $reset-attributes);
              end with-output-to-string,
              "\<1b>[31mHello\<1b>[0m!");
end test format-support-test;

define test print-support-test ()
  // We intentionally use print-object and print with the attributes to
  // test that they both work.
  check-equal("Using print works with color",
              with-output-to-string (s)
                let ansi-stream = colorize-stream(s, force-ansi?: #t);
                let error-attributes = text-attributes(foreground: $color-red);
                print-object(error-attributes, ansi-stream);
                print("World", ansi-stream, escape?: #f);
                print($reset-attributes, ansi-stream);
                print("!", ansi-stream, escape?: #f);
              end with-output-to-string,
              "\<1b>[31mWorld\<1b>[0m!");
end test print-support-test;

define test indenting-support-test ()
  check-equal("Color can poke through indentation",
              with-output-to-string (s)
                let is = make(<indenting-stream>, inner-stream: s);
                let ansi-stream = colorize-stream(is, force-ansi?: #t);
                indent(is, 2);
                let error-attributes = text-attributes(foreground: $color-red);
                format(ansi-stream, "%=Hello%=!",
                       error-attributes, $reset-attributes);
              end with-output-to-string,
              "  \<1b>[31mHello\<1b>[0m!");
end test indenting-support-test;

define library-spec coloring-stream ()
  module coloring-stream;
  test format-support-test;
  test print-support-test;
  test indenting-support-test;
end library-spec coloring-stream;
