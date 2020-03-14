Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: BROWSE - Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  BROWSE -- Benchmark to create and browse through an AI-like data base of units.
//  n is # of symbols
//  m is maximum amount of stuff on the plist
//  npats is the number of basic patterns on the unit
//  ipats is the instantiated copies of the patterns

define variable *browse-rand* :: <integer> = 21;

define inline function char1 (s :: <symbol>)
  let sstr = as(<string>, s);
  sstr[0]
end function;

define method init-browse (n :: <integer>, m :: <integer>, npats :: <integer>,
                           ipats)
  *browse-rand* := 21;
  let ipats = copy-tree(ipats);
  for (p = ipats then tail(p), until: empty?(tail(p)))
  finally
    tail(p) := ipats;
  end for;
  let a = #();
  for (n :: <integer> = n then n - 1,
       i :: <integer> = m then if (zero?(i)) m else i - 1 end,
       name = generate-symbol() then generate-symbol(),
       until: n = 0)
    push!(a, name);
    for (i = i then i - 1, until: i = 0)
      get(name, generate-symbol()) := #();
    end for;
    get(name, #"pattern")
      := for (i :: <integer> = npats then i - 1,
              ipats = ipats then tail(ipats),
              a = #() then pair(head(ipats), a),
              until: i = 0)
         finally
           a
         end for;
    for (j :: <integer> = m - i then j - 1, until: j = 0)
      get(name, generate-symbol()) := #();
    end for;
  finally
    a;
  end for;
end method init-browse;

define method browse-random ()
  *browse-rand* := modulo(*browse-rand* * 17, 251)
end method browse-random;

define function browse-randomize (l)
  let a = #();
  until (empty?(l))
    let n :: <integer> = modulo(browse-random(), size(l));
    if (n = 0)
      push!(a, head(l));
      l := tail(l);
    else
      for (n :: <integer> = n then n - 1,
          x = l then tail(x),
          until: n = 1)
      finally
        push!(a, second(x));
        tail(x) := tail(tail(x));
      end for;
    end if;
  end until;
  a
end function;

define method match (pat, dat, alist)
  if (empty?(pat))
    empty?(dat)
  elseif (empty?(dat))
    #f
  elseif (head(pat) == #"?" | head(pat) == head(dat))
    match(tail(pat), tail(dat), alist)
  elseif (head(pat) == #"*")
    match(tail(pat), dat, alist)
      | match(tail(pat), tail(dat), alist)
      | match(pat, tail(dat), alist)
  else
    if (instance?(head(pat), <symbol>))
      if (char1(head(pat)) == '?')
        let val = assoc(head(pat), alist);
        if (~empty?(val))
          match(pair(tail(val), tail(pat)), dat, alist)
        else
          match(tail(pat), tail(dat),
                pair(pair(head(pat), head(dat)), alist))
        end if
      elseif (char1(head(pat)) == '*')
        let val = assoc(head(pat), alist);
        if (~empty?(val))
          match(concatenate(tail(val), tail(pat)), dat, alist)
        else
          block (return)
            for (l = #() then nconc(l, pair(head(d), #())),
                 e = pair(#(), dat) then tail(e),
                 d = dat then tail(d),
                 until: empty?(e))
              if (match(tail(pat), d, pair(pair(head(pat), l), alist)))
                return(#t);
              end if;
            finally
              #f
            end for;
          end block;
        end if;
      end if;
    else
      instance?(head(dat), <pair>)
        & match(head(pat), head(dat), alist)
        & match(tail(pat), tail(dat), alist)
    end if
  end if
end method match;

define method browse ()
  investigate(browse-randomize(init-browse(100, 10, 4,
                                           #(#(#"a", #"a", #"a", #"b", #"b", #"b",
                                               #"b", #"a", #"a", #"a", #"a", #"a",
                                               #"b", #"b", #"a", #"a", #"a"),
                                             #(#"a", #"a", #"b", #"b", #"b", #"b", #"a", #"a",
                                               #(#"a", #"a"), #(#"b", #"b")),
                                             #(#"a", #"a", #"a", #"b", #(#"b", #"a"),
                                               #"b", #"a", #"b", #"a")))),
              #(#(#"*a", #"?b", #"*b", #"?b", #"a", #"*a", #"a", #"*b", #"*a"),
                #(#"*a", #"*b", #"*b", #"*a", #(#"*a"), #(#"*b")),
                #(#"?", #"?", #"*", #(#"b", #"a"), #"*", #"?", #"?")));
end method browse;

define method investigate (units, pats)
  for (units = units then tail(units), until: empty?(units))
    for (pats = pats then tail(pats), until: empty?(pats))
      for (p = get(head(units), #"pattern") then tail(p), until: empty?(p))
        match(head(pats), head(p), #());
      end for;
    end for;
  end for;
end method investigate;

define benchmark browse-benchmark ()
  benchmark-repeat (iterations: 10)
    browse();
  end;
end benchmark;
