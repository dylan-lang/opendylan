Module:   gabriel-benchmarks
Synopsis: Gabriel benchmarks in Dylan
Author:   Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  FRPOLY -- Benchmark from Berkeley based on polynomial arithmetic.
//  Originally writen in Franz Lisp by Richard Fateman.
define variable *frpoly-v* = #f;

define variable *x* = #();

//define variable *alpha* = #f;

//define variable *a* = #f;

//define variable *chk = #f;

//define variable *l = #f;

//define variable *p = #f;

//define variable q* = #f;

define variable u* = #f;

//define variable *var = #f;

//define variable *y* = #f;

define method pcoefadd (e, c, x)
  if (instance?(c, <real>) & zero?(c))
    x
  else
    pair(e, pair(c, x))
  end if;
end method pcoefadd;

define method pcplus (c, p)
  if (~instance?(p, <list>))
    p + c;
  else
    psimp(head(p), pcplus1(c, tail(p)));
  end if;
end method pcplus;

define method pcplus1 (c, x)
  if (null?(x))
    if (instance?(c, <number>) & zero?(c))
      #()
    else
      pair(0, pair(c, #()));
    end if;
  elseif (instance?(head(x), <number>) & zero?(head(x)))
    pcoefadd(0, pplus(c, second(x)), #());
  else
    pair(head(x), pair(second(x), pcplus1(c, tail(tail(x)))));
  end if;
end method pcplus1;

define method pctimes (c, p)
  if (~(instance?(p, <list>)))
    c * p;
  else
    psimp(head(p), pctimes1(c, tail(p)));
  end if;
end method pctimes;

define method pctimes1 (c, x)
  if (null?(x))
    #();
  else
    pcoefadd(head(x), ptimes(c, second(x)), pctimes1(c, tail(tail(x))));
  end if;
end method pctimes1;

define method pplus (x, y)
  if (~(instance?(x, <list>)))
    pcplus(x, y);
  elseif (~(instance?(y, <list>)))
    pcplus(y, x);
  elseif (head(x) == head(y))
    psimp(head(x), pplus1(tail(y), tail(x)));
  elseif (get(head(x), #"order") > get(head(y), #"order"))
    psimp(head(x), pcplus1(y, tail(x)));
  else
    psimp(head(y), pcplus1(x, tail(y)));
  end if;
end method pplus;

define method pplus1 (x, y)
  if (null?(x))
    y;
  elseif (null?(y))
    x;
  elseif (head(x) = head(y))
    pcoefadd(head(x), pplus(second(x), second(y)),
             pplus1(tail(tail(x)), tail(tail(y))));
  elseif (head(x) > head(y))
    pair(head(x), pair(second(x), pplus1(tail(tail(x)), y)));
  else
    pair(head(y), pair(second(y), pplus1(x, tail(tail(y)))));
  end if;
end method pplus1;

define method psimp (var, x)
  if (null?(x))
    0;
  elseif (~(instance?(x, <list>)))
    x;
  elseif (zero?(head(x)))
    second(x);
  else
    pair(var, x);
  end if;
end method psimp;

define method ptimes (x, y)
  if ((instance?(x, <number>) & zero?(x))
        | (instance?(y, <number>) & zero?(y)))
    0;
  elseif (~(instance?(x, <list>)))
    pctimes(x, y);
  elseif (~(instance?(y, <list>)))
    pctimes(y, x);
  elseif (head(x) == head(y))
    psimp(head(x), ptimes1(tail(x), tail(y)));
  elseif (get(head(x), #"order") > get(head(y), #"order"))
    psimp(head(x), pctimes1(y, tail(x)));
  else
    psimp(head(y), pctimes1(x, tail(y)));
  end if;
end method ptimes;

define method ptimes1 (x, y)
  block (return)
    begin
      let u* = #f;
      dynamic-bind (*x* = x, *frpoly-v* = #f)
        local method go-a ()
                *x* := tail(tail(*x*));
                if (null?(*x*))
                  return(u*);
                end if;
                ptimes3(y);
                go-a();
              end method go-a;
        *frpoly-v* := (u* := ptimes2(y));
        go-a();
      end dynamic-bind;
    end;
  end block;
end method ptimes1;

define method ptimes2 (y)
  if (null?(y))
    #();
  else
    pcoefadd(head(*x*) + head(y), ptimes(second(*x*), second(y)),
             ptimes2(tail(tail(y))));
  end if;
end method ptimes2;

define method ptimes3 (y)
  block (return)
    begin
      let e = #f;
      let u = #f;
      let c = #f;
      local method go-c ()
              if (~null?(tail(u)) & second(u) > e)
                u := tail(tail(u));
                go-c();
              end if;
              go-b();
            end method go-c,
            method go-d ()
              y := tail(tail(y));
              if (null?(y))
                return(#());
              end if;
              e := head(*x*) + head(y);
              c := ptimes(second(y), second(*x*));
              go-c();
            end method go-d,
            method go-e ()
              u := tail(tail(u));
              go-d();
            end method go-e,
            method go-b ()
              if (null?(tail(u)) | second(u) < e)
                tail(u) := pair(e, pair(c, tail(u)));
              else
                go-e();
              end if;
              if (instance?(c := pplus(third(u), c), <number>)
                    & zero?(c := pplus(third(u), c)))
                tail(u) := tail(tail(tail(u)));
                go-d();
              else
                head(tail(tail(u))) := c;
              end if;
              go-e();
            end method go-b,
            method go-a ()
              if (~null?(tail(tail(*frpoly-v*))) & third(*frpoly-v*) > e)
                *frpoly-v* := tail(tail(*frpoly-v*));
                go-a();
              end if;
              u := tail(*frpoly-v*);
              go-b();
            end method go-a,
            method go-a1 ()
              if (null?(y))
                return(#());
              end if;
              begin
                e := head(*x*) + head(y);
                c := ptimes(second(y), second(*x*));
              end;
              if (instance?(c, <number>) & zero?(c))
                y := tail(tail(y));
                go-a1();
              elseif (null?(*frpoly-v*) | e > head(*frpoly-v*))
                u* := (*frpoly-v* := pplus1(u*, list(e, c)));
                y := tail(tail(y));
                go-a1();
              elseif (e = head(*frpoly-v*))
                c := pplus(c, second(*frpoly-v*));
                if (instance?(c, <number>) & zero?(c))
                  u* := (*frpoly-v* := pair(u*, list(head(*frpoly-v*),
                                                     second(*frpoly-v*))));
                else
                  head(tail(*frpoly-v*)) := c;
                end if;
                y := tail(tail(y));
                go-a1();
              end if;
              go-a();
            end method go-a1;
      go-a1();
    end;
  end block;
end method ptimes3;

define method pexptsq (p, n)
  let s = if (odd?(n)) p else 1 end;
  for (n = floor/(n, 2) then floor/(n, 2), until: zero?(n))
    p := ptimes(p, p);
    if (odd?(n))
      s := ptimes(s, p);
    end if;
  end for;
  s
end method pexptsq;

begin
  get(#"x", #"order") := 1;
  get(#"y", #"order") := 2;
  get(#"z", #"order") := 3;
end;

//  r = x+y+z+1
//  r2 = 100000 * r
//  r3 = r with floating point coefficients
define variable *frpoly-r*
  = pplus(#(#"x", 1, 1, 0, 1), pplus(#(#"y", 1, 1), #(#"z", 1, 1)));

// define variable *frpoly-r2* = ptimes(*frpoly-r*, 100000);

define variable *frpoly-r3* = ptimes(*frpoly-r*, 1.0s0);

define method run-frpoly/fixnum ()
  for (exp in #(2, 5, 10, 15))
    pexptsq(*frpoly-r*, exp)
  end for;
end method run-frpoly/fixnum;

// define method run-frpoly/bignum ()
//   for (exp in #(2, 5, 10, 15))
//     pexptsq(*frpoly-r2*, exp);
//   end for;
// end method run-frpoly/bignum;

define method run-frpoly/float ()
  for (exp in #(2, 5, 10, 15))
    pexptsq(*frpoly-r3*, exp);
  end for;
end method run-frpoly/float;

define benchmark frpoly/fixnum-benchmark ()
  benchmark-repeat (iterations: 100)
    run-frpoly/fixnum();
  end;
end benchmark;

// define benchmark frpoly/bignum-benchmark ()
//   benchmark-repeat (iterations: 30)
//   end;
// end benchmark;

define benchmark frpoly/float-benchmark ()
  benchmark-repeat (iterations: 100)
    run-frpoly/float();
  end;
end benchmark;
