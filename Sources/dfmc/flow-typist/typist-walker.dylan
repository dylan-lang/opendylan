module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method typist-walk-computations
    (walk :: <function>, c :: <computation>, css :: <call-site-summary>, last) 
  iterate loop (c = c)
    unless (c == last)
      typist-walk-computation(walk, c, css, last);
      let cc = c.next-computation;
      when (cc)
        loop(cc);
      end when;
    end unless;
  end iterate;
end method;

define method typist-walk-computations
    (walk :: <function>, c :: <computation>, css :: <call-site-summary>, pred :: <function>) 
  iterate loop (c = c)
    unless (pred(c))
      typist-walk-computation(walk, c, css, pred);
      let cc = c.next-computation;
      when (cc)
        loop(cc);
      end when;
    end unless;
  end iterate;
end method;

define method typist-walk-computation (walk :: <function>, c :: <computation>, css, comp) 
  walk(c);
end method;

/*
define method typist-walk-computation (walk :: <function>, c :: <if>, css :: <call-site-summary>, end-c) 
  walk(c);
  let branch-state = element(css.branch-taken, c, default: both:);
  select (branch-state)
    both: =>
      typist-walk-computations(walk, c.consequent, css, c.completion-computation);
      typist-walk-computations(walk, c.alternative, css, c.completion-computation);
    true: =>
      typist-walk-computations(walk, c.consequent, css, c.completion-computation);
    false: =>
      typist-walk-computations(walk, c.alternative, css, c.completion-computation);
  end;
end method;
*/

define method typist-walk-computation (walk :: <function>, c :: <if>, css :: <call-site-summary>, end-c) 
  walk(c);
  let bt = element(css.branch-taken, c, default: both:);
  if (bt == true:  | bt == both:) typist-walk-computations(walk, c.consequent, css, c.next-computation) end;
  if (bt == false: | bt == both:) typist-walk-computations(walk, c.alternative, css, c.next-computation) end;
end method;

define method typist-walk-computation (walk :: <function>, c :: <if>, css :: <call-site-summary>, pred :: <function>) 
  walk(c);
  local method end? (comp)
          pred(c) | (comp == c.next-computation)
        end;
  let bt = element(css.branch-taken, c, default: both:);
  if (bt == true:  | bt == both:) typist-walk-computations(walk, c.consequent, css, end?) end;
  if (bt == false: | bt == both:) typist-walk-computations(walk, c.alternative, css, end?) end;
end method;

define method typist-walk-computation (walk :: <function>, c :: <bind-exit>, css, end-c)
  walk(c);
  typist-walk-computations(walk, c.body, css, c.next-computation);
end method;

define method typist-walk-computation (walk :: <function>, c :: <bind-exit>, css :: <call-site-summary>, pred :: <function>) 
  walk(c);
  local method end? (comp)
          pred(c) | (comp == c.next-computation)
        end;
  typist-walk-computations(walk, c.body, css, end?);
end method;

define method typist-walk-computation (walk :: <function>, c :: <unwind-protect>, css :: <call-site-summary>, end-c)
  walk(c);
  typist-walk-computations(walk, c.body, css, c.next-computation);
  typist-walk-computations(walk, c.cleanups, css, c.next-computation);
end method;

define method typist-walk-computation (walk :: <function>, c :: <unwind-protect>, css :: <call-site-summary>, pred :: <function>)
  walk(c);
  local method end? (comp)
          pred(c) | (comp == c.next-computation)
        end;
  typist-walk-computations(walk, c.body, css, end?);
  typist-walk-computations(walk, c.cleanups, css, end?);
end method;

define method typist-walk-computation (walk :: <function>, c :: <loop>, css :: <call-site-summary>, end-c)
  walk(c);
  typist-walk-computations(walk, c.loop-body, css, c.next-computation);
end method;

define method typist-walk-computation (walk :: <function>, c :: <loop>, css :: <call-site-summary>, pred :: <function>)
  walk(c);
  local method end? (comp)
          pred(c) | (comp == c.next-computation)
        end;
  typist-walk-computations(walk, c.loop-body, css, end?);
end method;

// eof
