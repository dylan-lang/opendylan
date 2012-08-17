module: dfmc-harp-x86-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// ACCESSORS


define sideways method op--load-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--ld-index(back-end, result, base, scaled-index, unset-tag-bit(offset));
  else
    ins--ld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-index;

define sideways method op--load-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--ld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-index;


define sideways method op--store-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--st-index(back-end, value, base, scaled-index, unset-tag-bit(offset));
  else
    ins--st-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--move(back-end, result, value);

end method op--store-index;

define sideways method op--store-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--st-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-index;


define sideways method op--load-byte-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldb-index(back-end, result, base, scaled-index, offset);

end method op--load-byte-index;

define sideways method op--load-byte-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset) => ()

  ins--ldb-index(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-byte-index;


define sideways method op--store-byte-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset :: <integer>) => ()

  ins--stb-index(back-end, value, base, scaled-index, offset);
  ins--move(back-end, result, value);

end method op--store-byte-index;

define sideways method op--store-byte-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset) => ()

  ins--stb-index(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-byte-index;


define sideways method op--store-bit-index
    (back-end :: <harp-x86-back-end>, result, value, base, index, offset, bit) => ()

  select (value)
    0 => 
      ins--bitc-mem(back-end, base, index, offset, bit);
    1 => 
      ins--bits-mem(back-end, base, index, offset, bit);
    otherwise =>
      let set-bit-tag = make-tag(back-end);
      let done-tag = make-tag(back-end);
      ins--bne(back-end, set-bit-tag, value, 0);
      ins--bitc-mem(back-end, base, index, offset, bit);
      ins--bra(back-end, done-tag);      
      ins--tag(back-end, set-bit-tag);
      ins--bits-mem(back-end, base, index, offset, bit);
      ins--tag(back-end, done-tag);
  end select;

  ins--move(back-end, result, value);

end method op--store-bit-index;

define sideways method op--load-signed-byte-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldb-index-signed(back-end, result, base, scaled-index, offset);

end method op--load-signed-byte-index;

define sideways method op--load-signed-byte-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset) => ()

  ins--ldb-index-signed(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-signed-byte-index;


define sideways method op--store-signed-byte-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-byte-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-byte-index;


define sideways method op--load-half-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldh-index-scaled(back-end, result, base, scaled-index, offset);

end method op--load-half-index;

define sideways method op--load-half-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset) => ()

  ins--ldh-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-half-index;


define sideways method op--store-half-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset :: <integer>) => ()

  ins--sth-index-scaled(back-end, value, base, scaled-index, offset);
  ins--move(back-end, result, value);

end method op--store-half-index;

define sideways method op--store-half-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset) => ()

  ins--sth-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-half-index;

define sideways method op--load-signed-half-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldh-index-scaled-signed(back-end, result, base, scaled-index, offset);

end method op--load-signed-half-index;

define sideways method op--load-signed-half-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset) => ()

  ins--ldh-index-scaled-signed(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-signed-half-index;


define sideways method op--store-signed-half-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-half-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-half-index;


define sideways method op--load-float-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--fld-index(back-end, result, base, scaled-index, unset-tag-bit(offset));
  else
    ins--fld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-float-index;

define sideways method op--load-float-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--fld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-float-index;


define sideways method op--store-float-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--fst-index(back-end, value, base, scaled-index, unset-tag-bit(offset));
  else
    ins--fst-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--fmove(back-end, result, value);

end method op--store-float-index;

define sideways method op--store-float-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--fst-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--fmove(back-end, result, value);

end method op--store-float-index;


define sideways method op--load-dfloat-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--dld-index-scale-2(back-end, result, base, scaled-index, offset - 2);
  else
    ins--dld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-dfloat-index;

define sideways method op--load-dfloat-index
    (back-end :: <harp-x86-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--dld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-dfloat-index;


define sideways method op--store-dfloat-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--dst-index-scale-2(back-end, value, base, scaled-index, offset - 2);
  else
    ins--dst-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--dmove(back-end, result, value);

end method op--store-dfloat-index;

define sideways method op--store-dfloat-index
    (back-end :: <harp-x86-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--dst-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--dmove(back-end, result, value);

end method op--store-dfloat-index;

