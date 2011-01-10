Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Higher level harp operations

define open generic op--store-multiple-values-count(back-end :: <harp-back-end>, register) => ();

define method ins--load(back-end :: <harp-back-end>, dregister, s1, s2) => ()
  ins--ld(back-end, dregister, s1, s2);
end method ins--load;

define method ins--store(back-end :: <harp-back-end>, val, s1, s2) => ()
  ins--st(back-end, val, s1, s2);
end method ins--store;

/*
define method ins--load-byte(back-end :: <harp-back-end>, dregister, s1, s2) => ()
  ins--ldb(back-end, dregister, s1, s2);
end method ins--load-byte;

define method ins--store-byte(back-end :: <harp-back-end>, val, s1, s2) => ()
  ins--stb(back-end, val, s1, s2);
end method ins--store-byte;
*/

define method op--tag(back-end :: <harp-back-end>, label :: <integer>) => (tag :: <tag>)
  element(back-end.cg-variables.tags, label, default: #f)
  | (back-end.cg-variables.tags[label] := make-tag(back-end));
end method op--tag;

define method op--start-bind-exit
(back-end :: <harp-back-end>, c :: <bind-exit>, tag :: <tag>) => (bind-exit-frame)
  let be-ref = $primitive-build-bind-exit-frame.runtime-reference;

  back-end.bind-exit-frame? := #t;
  ins--rem(back-end, "start bind exit");
  let bind-exit-frame = emit-object(back-end, *harp-outputter*, c.entry-state);
  ins--force-d(back-end, bind-exit-frame);
  ins--load-nlx-address(back-end, back-end.registers.reg-arg0, tag);
  ins--call(back-end, be-ref, 1);
  ins--move(back-end, bind-exit-frame, back-end.registers.reg-arg0);
  bind-exit-frame
end method op--start-bind-exit;

define method op--start-unwind-protect
(back-end :: <harp-back-end>, c :: <unwind-protect>, cleanup-tag :: <tag>) => ()
  let up-ref = $primitive-build-unwind-protect-frame.runtime-reference;

  ins--rem(back-end, "start unwind protect");
  ins--load-nlx-address(back-end, back-end.registers.reg-arg0, cleanup-tag);
  ins--call(back-end, up-ref, 1);
  ins--move(back-end, emit-object(back-end, *harp-outputter*, c.entry-state), back-end.registers.reg-arg0);
end method op--start-unwind-protect;

define method op--unwind-protect-cleanup
(back-end :: <harp-back-end>, c :: <unwind-protect>, cleanup-tag :: <tag>, continue-tag :: <tag>) => ()
  let up-ref = $primitive-unwind-protect-cleanup.runtime-reference;

  ins--call(back-end, up-ref, 0);
  ins--control-flow-link(back-end, cleanup-tag);
  ins--bra(back-end, continue-tag);
end method op--unwind-protect-cleanup;

/*
define method op--continue-unwind
(back-end :: <harp-back-end>, c :: <unwind-protect>, continue-tag :: <tag>, #rest arguments) => ()

  ins--end-cleanup(back-end, continue-tag);

end method op--continue-unwind;

define method bits%(back-end :: <harp-back-end>, word :: <integer>) => (bits :: <integer>)
  32 * word;
end method bits%;
*/

define method bytes%(back-end :: <harp-back-end>, word :: <integer>) => (bytes :: <integer>)
  4 * word;
end method bytes%;

define method bytes%(back-end :: <harp-back-end>, word :: <virtual-register>) => (bytes :: <register>)
  let new-register = make-register(back-end);
  ins--asl(back-end, new-register, word, 2);
  new-register;
end method bytes%;

define method half%(back-end :: <harp-back-end>, word :: <integer>) => (bytes :: <integer>)
  2 * word;
end method half%;

define method half%(back-end :: <harp-back-end>, word :: <virtual-register>) => (bytes :: <register>)
  let new-register = make-register(back-end);
  ins--asl(back-end, new-register, word, 1);
  new-register;
end method half%;

define method double%(back-end :: <harp-back-end>, word :: <integer>) => (double :: <integer>)
  8 * word;
end method double%;

define method double%(back-end :: <harp-back-end>, word :: <virtual-register>) => (double :: <register>)
  let new-register = make-register(back-end);
  ins--asl(back-end, new-register, word, 3);
  new-register;
end method double%;

define method op--raw(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-n-register(back-end);

  ins--asr(back-end, result, object, 2);

  result;

end method op--raw;

define method op--raw(back-end :: <harp-back-end>, result, object :: <integer>) => (result :: <integer>)
  let value = as(<integer>, truncate/(object, 4));

  if (result)
    ins--move(back-end, result, value);
  end if;
  value
end method op--raw;

define method op--raw(back-end :: <harp-back-end>, result, object :: <byte-character>) => (result :: <integer>)
  let value = as(<integer>, object);

  if (result)
    ins--move(back-end, result, value);
  end if;
  value
end method op--raw;


define method op--character(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-g-register(back-end);

  ins--asl(back-end, result, object, 2);
  ins--or(back-end, result, result, 2);

  result;

end method op--character;

define method op--unicode-character(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-g-register(back-end);

  ins--asl(back-end, result, object, 2);
  ins--or(back-end, result, result, 3);

  result;

end method op--unicode-character;


define method op--integer(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-g-register(back-end);

  ins--asl(back-end, result, object, 2);
  ins--or(back-end, result, result, 1);

  result;

end method op--integer;

define method op--integer(back-end :: <harp-back-end>, result, object :: <integer>) => (result :: <integer>)

  4 * object + 1

end method op--integer;


define method op--as-raw(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-n-register(back-end);

  ins--load(back-end, result, object, bytes%(back-end, 1));

  result;

end method op--as-raw;


define method op--sf-as-raw(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-sf-register(back-end);

  ins--fld(back-end, result, object, bytes%(back-end, 1));

  result;

end method op--sf-as-raw;


define method op--df-as-raw(back-end :: <harp-back-end>, result, object) => (result)

  let result = result | make-df-register(back-end);

  ins--dld(back-end, result, object, bytes%(back-end, 1));

  result;

end method op--df-as-raw;


define method op--add(back-end :: <harp-back-end>, result, x, y) => (result)

  let result = result | make-n-register(back-end);

  ins--add(back-end, result, x, y);

  result;

end method op--add;


define method op--add(back-end :: <harp-back-end>, result, x :: <integer>, y :: <integer>) => (sum :: <integer>)

  x + y

end method op--add;


define method op--slots(back-end :: <harp-back-end>, x) => (result)

  let result = make-n-register(back-end);

  ins--add(back-end, result, x, bytes%(back-end, 1));

  result;

end method op--slots;


define method op--ld-mv-count
    (back-end :: <harp-back-end>) => (multiple-values-count :: <Nreg>)
  let count = make-n-register(back-end);
  ins--ld-teb(back-end, count, back-end.teb-mv-count-offset);
  count
end method;


define method op--st-mv-count
    (back-end :: <harp-back-end>, data) => ()
  ins--st-teb(back-end, data, back-end.teb-mv-count-offset);
end method;


define method op--ld-mv-area-address
    (back-end :: <harp-back-end>) => (multiple-values-area :: <Nreg>)
  let $multiple-values-area = make-n-register(back-end);

  ins--ld-teb-address(back-end, $multiple-values-area, back-end.teb-mv-area-offset);

  $multiple-values-area;
end method;
