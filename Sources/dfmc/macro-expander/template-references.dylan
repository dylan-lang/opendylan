Module:    dfmc-macro-expander
Synopsis:  Computing the reference set of a template
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method compute-template-references 
    (f* :: <list>, collect-reference :: <function>) => ()
  // Maintain a two token history.
  let cursor-2 = #f; // Read as "cursor minus two".
  let cursor-1 = #f;
  for (cursor in f*, next = tail(f*) then tail(next))
    compute-template-element-references
      (cursor-2, cursor-1, cursor, next, collect-reference);
    cursor-2 := cursor-1;
    cursor-1 := cursor;
  end;
end method;

//// Infrastructure cases.

define method compute-template-element-references
    (cursor-2, cursor-1, cursor :: <object>,
       after :: <list>, collect-reference) 
 => ()
end method;

define method compute-template-element-references
    (cursor-2, cursor-1, cursor :: <nested-fragment>,
       after :: <list>, collect-reference) 
 => ()
  compute-template-references
    (fragment-nested-fragments(cursor), collect-reference);
end method;

//// Reference spotting cases.

// o variable-name
// o DEFINE definition-head-OPT definer-name

define method compute-template-element-references
    (cursor-2, cursor-1, cursor :: <name-fragment>, 
       after :: <list>, collect-reference) 
 => ()
  let f = #f;
  case
    // This is yet more conservative than the DRM-prescribed method since
    // it doesn't only consider DEFINE words. This is so we can run this
    // pass any time we wish to, including at the definition point of the
    // macro before all possible DEFINE words are known.
    fragment-name(cursor) == #"define"
      => block (stop)
           for (f in after)
             if (instance?(f, <name-fragment>))
               collect-reference
                 (splice-name-hygienically(f, "", "-definer"));
             elseif (~instance?(f, <substitution>))
               stop();
             end;
           end;
         end;
    otherwise
      => collect-reference(cursor);
  end;
end method;

// o . getter-name :=

define method compute-template-element-references
    (cursor-2 :: <dot-fragment>, cursor-1 :: <name-fragment>,
       cursor :: <binary-operator-fragment>, 
       after :: <list>, collect-reference) 
 => ()
  next-method(); // Collect the individual name.
  if (fragment-name(cursor) == #":=")
    collect-reference
      (splice-name-hygienically(cursor-1, "", "-setter"));    
  end;
end method;

define method compute-template-element-references
    (cursor-2 :: <object>, cursor-1 :: <name-fragment>,
       cursor :: <parens-fragment>, 
       after :: <list>, collect-reference) 
 => ()
  next-method(); // Collect the component names.
  if (fragment-name(cursor-1) == #":=")
    let args = fragment-nested-fragments(cursor);
    let arg1 = split-at-comma(args);
    let dot-name = last(arg1, default: #f);
    if (instance?(dot-name, <name-fragment>))
      collect-reference
        (splice-name-hygienically(dot-name, "", "-setter"));    
    end;
  end;
end method;

// eof

