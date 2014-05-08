Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// MEMBER?
//

define sealed method member?
    (ch :: <byte-character>, big :: <byte-string>,
     #key test :: <function> = \==)
 => (found? :: <boolean>)
  let sz = big.size;
  if (test == \==)
    for (key :: <integer> from 0 below sz,
         until: (big[key] == ch))
    finally
      if (key < sz) #t end if;
    end for;
  else
    for (key :: <integer> from 0 below sz,
         until: test(big[key], ch))
    finally
      if (key < sz) #t end if;
    end for;
  end if;
end method member?;

//
// SUBSEQUENCE-POSITION
//

define sealed method subsequence-position
    (big :: <byte-string>, pat :: <byte-string>,
     #key test :: <function> = \==, count :: <integer> = 1)
 => (index :: false-or(<integer>));
  let sz :: <integer> = size(big);
  let pat-sz :: <integer> = size(pat);

  select (pat-sz)
    0 =>
      count - 1;
    1 =>
      let ch = pat[0];
      for (key :: <integer> from 0 below sz,
           until: test(big[key], ch) & (count := count - 1) <= 0)
      finally
        if (key < sz) key end if;
      end for;
    2 =>
      let ch1 = pat[0];
      let ch2 = pat[1];
      for (key :: <integer> from 0 below sz - 1,
           until: test(big[key], ch1) & test(big[key + 1], ch2)
             & (count := count - 1) <= 0)
      finally
        if (key < (sz - 1)) key end if;
      end for;
    otherwise =>
      if (test ~= \==)
        local method search(index :: <integer>,
                            big-key :: <integer>,
                            pat-key :: <integer>,
                            count :: <integer>)
                case
                  pat-key >= pat-sz =>
                    if (count = 1) index
                    else search(index + 1, index + 1, 0, count - 1);
                    end if;
                  big-key = sz =>
                    #f;
                  test(big[big-key], pat[pat-key]) =>
                    search(index, big-key + 1, pat-key + 1, count);
                  otherwise =>
                    search(index + 1, index + 1, 0, count);
                end case;
              end method search;
        search(0, 0, 0, count);
      else
        // It's worth doing something Boyer-Moore-ish....
        let pat-last :: <integer> = pat-sz - 1;
        let last-char :: <byte-character> = pat[pat-last];
        let skip :: <simple-object-vector>
          = make(<vector>, size: 256, fill: pat-sz);
        for (i :: <integer> from 0 below pat-last)
          skip[as(<integer>, pat[i])] := pat-last - i;
        end for;
        local method do-skip(index :: <integer>, count :: <integer>)
                if (index >= sz)
                  #f;
                else
                  let char :: <byte-character> = big[index];
                  if (char == last-char)
                    search(index - pat-last, index, pat-last, count);
                  else
                    do-skip(index + skip[as(<integer>, char)], count);
                  end if;
                end if;
              end method,
              method search(index :: <integer>,
                            big-key :: <integer>,
                            pat-key :: <integer>,
                            count :: <integer>)
                case
                  pat-key < 0 =>
                    if (count = 1) index
                    else do-skip(index + pat-sz, count - 1)
                    end if;
                  big[big-key] == pat[pat-key] =>
                    search(index, big-key - 1, pat-key - 1, count);
                  otherwise =>
                    do-skip(index + pat-sz, count);
                end case;
              end method search;
        do-skip(pat-last, count);
      end if;
  end select;
end method subsequence-position;

//
// COPY-SEQUENCE
//

define sealed method copy-sequence
    (source :: <byte-string>,
     #key start: first :: <integer> = 0, end: last = source.size)
 => (result-sequence :: <byte-string>);
  let last :: <integer> = check-start-compute-end(source, first, last);
  let sz :: <integer> = last - first;
  let target :: <byte-string> = make(<byte-string>, size: sz);
  primitive-replace-bytes!
    (target, primitive-repeated-slot-offset(target), integer-as-raw(0),
     source, primitive-repeated-slot-offset(source), integer-as-raw(first),
     integer-as-raw(sz));
  target
end method copy-sequence;

//
// CONCATENATE-AS
//

define sealed method concatenate-as
    (class == <byte-string>, vector :: <byte-string>,
     #rest more-vectors) => (result :: <byte-string>)
  block (return)
    let total-sz :: <integer> = vector.size;
    let num-non-empty :: <integer> = if (total-sz = 0) 0 else 1 end;

    for (v in more-vectors)
      if (~instance?(v, <byte-string>)) return(next-method()) end;
      let sz :: <integer> = v.size;
      if (sz ~= 0)
        total-sz := total-sz + sz;
        num-non-empty := num-non-empty + 1;
      end;
    end for;

    select (num-non-empty)
      0 => make(<byte-string>);
      1 => if (vector.size > 0) vector
           else
             for (i :: <integer> from 0 below more-vectors.size,
                  while: more-vectors[i].size = 0) finally more-vectors[i] end
           end;
      otherwise =>
        let result = make(<byte-string>, size: total-sz);
        let index = 0;
        let sz :: <raw-integer> = integer-as-raw(vector.size);
        primitive-replace-bytes!
          (result, primitive-repeated-slot-offset(result), integer-as-raw(0),
           vector, primitive-repeated-slot-offset(vector), integer-as-raw(0), sz);
        let result-index :: <raw-integer> = sz;
        for (v :: <byte-string> in more-vectors)
          let vsz :: <raw-integer> = integer-as-raw(v.size);
          primitive-replace-bytes!
            (result, primitive-repeated-slot-offset(result), result-index,
             v,      primitive-repeated-slot-offset(v),      integer-as-raw(0), vsz);
          result-index := primitive-machine-word-add(result-index, vsz);
        end;
        result
    end select;
  end block
end method concatenate-as;

