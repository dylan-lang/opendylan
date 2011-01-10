language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant char= =
     method(x :: <character>, y :: <character>)
       as-uppercase(x) == as-uppercase(y);
     end;

define method append-char! (x :: <string>, ch :: <character>)
  let new-string = make(<string>, size: x.size + 1, fill: ch);
  replace-subsequence!(new-string, x, end: x.size)
end;

/*
define method append-char! (x :: <stretchy-byte-string>, ch :: <character>)
  add!(x, ch);
end;

define method append-string!(target-str :: <stretchy-byte-string>, new-string)
  let old-size = size(target-str);
  size(target-str) := old-size + size(new-string);
  replace-subsequence!(target-str, new-string, start: old-size);
end;
*/

define method string-trim (str :: <byte-string>)
  let sz = size(str);
  let start = for (i :: <small-integer> from 0 below sz,
                   until (str[i] ~= ' '))
              finally i;
              end;
  let stop = for (i :: <small-integer> from sz - 1 to 0 by -1,
                  until (str[i] ~= ' '))
             finally i;
             end;
  if ((start = 0) & (stop = sz - 1))
    str;
  else
    copy-sequence(str, start: start, end: stop + 1);
  end;
end;

/*
define method string-trim (str :: <stretchy-byte-string>)
  let sz = size(str);
  let start = for (i :: <small-integer> from 0 below sz,
                   until (str[i] ~= ' '))
              finally i;
              end;
  let stop = for (i :: <small-integer> from (sz - 1) to 0 by -1,
                  until (str[i] ~= ' '))
             finally i;
             end;
  if (start = 0)
    if (stop = sz - 1)
      str;
    else
      size(str) := stop + 1;
      str;
    end;
  else
    copy-sequence(str, start: start, end: stop + 1);
  end;
end;
*/

define method find-char(str :: <string>, ch :: <character>, 
                        #key start = 0, stop = size(str), skip = 0)
  => (index-or-false)
  for (i :: <small-integer> from start below stop,
       hits = 0 then if (str[i] = ch)
                       hits + 1;
                     else
                       hits;
                     end,
       until (hits > skip))
  finally if (hits > skip) i - 1 end;
  end;
end;

define method nth-word (str :: <string>, n :: <integer>,
                        #key start: start-1 = 0,
                             stop: stop-1  = size(str))
  => (str, position);
  let start = if (n = 0)
                start-1;
              else
                find-char(str, ' ', start: start-1, stop: stop-1, skip: n - 1) + 1;
              end;
  let stop = find-char(str, ' ', start: start-1 + 1, stop: stop-1, skip: n)
                | stop-1;
  values(string-trim(copy-sequence(str, start: start, end: stop)),
         stop);
end;
