module: dfmc-typist-tests
synopsis: Tests for subtypes
author: Hannes Mehnert
copyright: 2008, all rights reversed

define function subtype-tests()
  let s1 = subtype?(<list>, <collection>);
  format-out("sub1 (DRM #t) ok %=\n", s1); //#t
  let s2 = subtype?(limited(<list>, of: <object>),
                    limited(<list>, of: <object>));
  format-out("sub2 (DRM #t) ok %=\n", s2); //#t
  let s3 = subtype?(limited(<list>, of: <object>),
                    limited(<collection>, of: <object>));
  format-out("sub3 (DRM #t) fail %=\n", s3); //#f
  let s4 = subtype?(limited(<list>, of: <integer>),
                    limited(<list>, of: <integer>));
  format-out("sub4 (DRM #t) ok %=\n", s4); //#t
  let s5 = subtype?(limited(<list>, of: <integer>),
                    limited(<collection>, of: <integer>));
  format-out("sub5 (DRM #t) fail %=\n", s5); //#f
  let s6 = subtype?(limited(<list>, of: <integer>),
                    limited(<list>, of: <string>));
  format-out("sub6 (DRM #f) fail %=\n", s6); //#t
  let s7 = subtype?(limited(<list>, of: <integer>),
                    limited(<collection>, of: <string>));
  format-out("sub7 (DRM #f) ok %=\n", s7); //#f
  let s8 = subtype?(limited(<list>, of: <integer>),
                    <collection>);
  format-out("sub8 (DRM #t) ok %=\n", s8); //#t
end;

//subtype-tests();
