Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function re-type-users (tmp :: <temporary>) => ()
  let css = current-css();
  let wa = make(<work-agenda>);
  for (user in tmp.users)
    re-type-computation(user, css, wa);
  end;
end;

define method re-type-computation(comp :: <computation>, css, wa) => ()
  let tmp = comp.temporary;
  when (tmp)
    let old-type = lookup-type(tmp, css, comp);
    let new-type = infer-node-type(comp, css, wa);
    when (^subtype?(new-type, old-type) & ~^subtype?(old-type, new-type))
      css.pool[tmp] := new-type; //!!!
      for (user in tmp.users)
        re-type-computation(user, css, wa);
      end;
    end;
  end;
end;

define method re-type-computation(comp :: <call>, css, wa) => ()
  let tmp = comp.temporary;
  let old-type = tmp & lookup-type(tmp, css, comp);
  let new-type = infer-node-type(comp, css, wa);
  when (tmp & ^subtype?(new-type, old-type) & ~^subtype?(old-type, new-type))
    css.pool[tmp] := new-type; //!!!
    for (user in tmp.users)
      re-type-computation(user, css, wa);
    end;
    re-optimize(comp);
  end;
end;

define method re-type-computation(comp :: <check-type-computation>, css, wa) => ()
  let tmp = comp.temporary;
  let old-type = tmp & lookup-type(tmp, css, comp);
  let new-type = infer-node-type(comp, css, wa);
  when (tmp & ^subtype?(new-type, old-type) & ~^subtype?(old-type, new-type))
    css.pool[tmp] := new-type; //!!!
    for (user in tmp.users)
      re-type-computation(user, css, wa);
    end;
    re-optimize(comp);
  end;
end;

define method re-type-computation(comp :: <set-cell-value!>, css, wa) => ()
end;

define method re-type-computation(comp :: <assignment>, css, wa) => ()
end;

