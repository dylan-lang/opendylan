Module: dfmc-flow-graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <renaming> (<object>)
  slot renamed-value, required-init-keyword: value: ;
  slot renaming-computation :: <computation>,
    required-init-keyword: comp: ;
  slot call-site-summary :: <call-site-summary>,
   required-init-keyword: css: ;
end;

define class <true-renaming> (<renaming>) end;

define method print-object (r :: <true-renaming>, s :: <stream>) => ()
  format(s, "{true renaming of %s}", r.renamed-value);
end;

define class <false-renaming> (<renaming>) end;

define method print-object (r :: <false-renaming>, s :: <stream>) => ()
  format(s, "{false renaming of %s}", r.renamed-value);
end;

define class <recovered-renaming> (<renaming>) end;

define method print-object (r :: <recovered-renaming>, s :: <stream>) => ()
  format(s, "{recovered renaming of %s}", r.renamed-value);
end;

define method get-renaming (x, css :: <call-site-summary>, c)
  if (element(css.renamed-temporaries, x, default: #f))
    let comp-renamed-uses = element(css.renaming-map, c, default: #f);
    if (comp-renamed-uses)
      block (return)
        for (entry in comp-renamed-uses)
          if (entry.head == x)
            return(entry.tail);
          end;
        end;
        x;
      end;
    else
      x;
    end;
  else
    x;
  end;
end;
    
define inline function get-renaming-at
  (value, renaming-class :: subclass(<renaming>), css :: <call-site-summary>, comp)
  => (renaming :: false-or(<renaming>));
  let renamings = element(css.renamed-temporaries, value, default: #());
  block (return)
    for (entry in renamings)
      if ((instance?(entry, renaming-class) & (entry.renaming-computation == comp)))
        return(entry);
      end;
    end;
  end;
end;

define inline function get-true-renaming-at
  (value, css :: <call-site-summary>, c)
  => (x :: false-or(<true-renaming>));
  get-renaming-at(value, <true-renaming>, css, c);
end;

define inline function get-false-renaming-at
  (value, css :: <call-site-summary>, c)
  => (x :: false-or(<false-renaming>));
  get-renaming-at(value, <false-renaming>, css, c);
end;

define inline function get-recovered-renaming-at
  (value, css :: <call-site-summary>, c)
  => (x :: false-or(<recovered-renaming>));
  get-renaming-at(value, <recovered-renaming>, css, c);
end;

define inline function delete-renamings
  (value, css :: <call-site-summary>, if-c)
  let renamings = css.renamed-temporaries[value];
  collecting (new-renamings)
    for (renaming in renamings)
      unless (renaming.renaming-computation == if-c)
        collect-into(new-renamings, renaming);
      end;
    end;
    css.renamed-temporaries[value] := as(<list>, collected(new-renamings));
  end;
  for (user in value.users)
    let renamed-uses = element(css.renaming-map, user, default: #f);
    when (renamed-uses)
      collecting (uses)
        for (entry in renamed-uses)
          unless (entry.head == value)
            collect-into(uses, entry);
          end;
        end;
        css.renaming-map[user] := as(<list>, collected(uses));
      end;
    end;
  end;
  css.renaming-computations[if-c] := #f;
end;

define function record-renaming-in-computations
  (start-c :: <computation>, end-c, 
   temp , renaming :: <renaming>, css :: <call-site-summary>)
  => (b :: <boolean>);
  let found-use? = #f;
  local method record-use(c :: <computation>)
          if (member?(c, temp.users))
            found-use? := #t;
            let mapping = element(css.renaming-map, c, default: #f);
            if (mapping)
              local method assoc ()
                      block (return)
                        for (x in mapping)
                          if (x.head == temp)
                            return(x);
                          end;
                        end;
                      end;
                    end;
              let entry = assoc();
              if (entry)
                entry.tail := renaming;
              else
                css.renaming-map[c] := add(mapping, pair(temp, renaming));
              end;
            else
              css.renaming-map[c] := list(pair(temp, renaming));
            end;
          end;
        end;
  if (instance?(end-c, <function>))
    walk-computations-until(record-use, start-c, end-c);
  else
    walk-computations(record-use, start-c, end-c);
  end;
  found-use?;
end;

define function schedule-renaming-computations
  (temp, css :: <call-site-summary>, wa :: <work-agenda>);
  let renamings = element(css.renamed-temporaries, temp, default: #f);
  if (renamings)
    for (renaming in renamings)
      schedule-for-retyping(wa, renaming.renaming-computation, renaming.call-site-summary);
    end;
  end;
end;


// There is probably more abstraction to be done with renamings. See typist-if and type-calls.
