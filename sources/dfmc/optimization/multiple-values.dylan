Synopsis: multiple values processing and optimization
Author:   Paul Haahr
Module:   dfmc-optimization
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// single value propagation

// define compilation-pass single-value-propagation,
//   visit: computations,
//   trigger: analyze-calls,
//   before: analyze-environments;

define method single-value-propagation (c :: <computation>)
  #f
end method single-value-propagation;

define method single-value-propagation (c :: <extract-single-value>)
  let mv-t = c.computation-value;
  let t = extract-single-value(mv-t, mv-t.generator, c.index);
//  format-out("single-val-prop: c=%=, mv-t=%=, mv-t-gen=%=, t=%=.\n", c, mv-t, mv-t.generator, t);
  if (t)
    replace-temporary-in-users!(c.temporary, t);
    t
  else
    #f
  end if;
end method single-value-propagation;

define method single-value-propagation (c :: <extract-rest-value>)
  let mv-t = c.computation-value;
  let t = extract-rest-value(mv-t, mv-t.generator, c.index);
  if (t)
    replace-temporary-in-users!(c.temporary, t);
    t
  else
    #f
  end if;
end method single-value-propagation;

/// simple values

define method extract-single-value
    (t :: <temporary>, c :: <computation>, index :: <integer>)
  if (t == c.temporary)
    #f
  else
    // This case helps push the extract as early as possible.
    // It should be defined only for linear computations, because
    // we do an insert-after.
    // assert(c.next-computations.size < 2);
    let (extract-c, new-t)
      = make-with-temporary(c.environment, <extract-single-value>,
                            value: c.temporary, index: index);
    insert-computation-after!(c, extract-c);
    new-t
  end if
end method extract-single-value;

define method extract-single-value
    (t :: <temporary>, c :: <values>, index :: <integer>)
  if (c.fixed-values.size > index)
    c.fixed-values[index]
  elseif (c.rest-value)
    // TODO: generate a call to element here?
    #f
  else
    make-object-reference(&false)
  end if
end method extract-single-value;

/*
define method extract-single-value
    (t :: <temporary>, c :: <adjust-multiple-values-computation>,
     index :: <integer>)
// WAS: extract-single-value(t, c.computation-value.generator, index);
/* this is wrong:  it takes
     *t1 := h(...)
     *t2 := ADJ-MV(*t2, 2)
     t3 := *t2[2]
   to:
     *t1 := h(...)
     *t2 := ADJ-MV(*t2, 2)
     t3 := *t1[3]
   and then *t2 is later eliminated.
   This is wrong if h stuffs 4 (or more) values into the MV area.
   The ADJ-MV will #f-out the values after [0] and [1], and the
   *t2[2] will return #f; whereas the *t1[3] after the ADJ-MV is
   eliminated will return whatever h put there.   (gts, 8/97)
*/
end method extract-single-value;
*/
/*
define method extract-single-value
    (t :: <temporary>, c :: <merge>, index :: <integer>)
  /*
  let new-sources
    = map(method (source)
            extract-single-value(t, source.generator, index);
          end, c.sources);
  if (member?(#f, new-sources))
    #f
  else
    let (merge-c, new-t)
      = make-with-temporary(c.environment, <merge>, sources: new-sources);
    insert-computation-after!(c, merge-c);
    new-t
  end if
  */
  #f
end method extract-single-value;
*/

/*
///
/// REASON THIS DOESN'T WORK IS BECAUSE WE MUST SPLIT MERGES INTO ONE
/// PER VALUE AND SINCE IF/LOOPS ARE ONE-TO-ONE WITH MERGES WE'RE SCREWED
///

define method extract-single-value
    (t :: <temporary>, c :: <if-merge>, index :: <integer>)
  let old-left-value = merge-left-value(c);
  let new-left-value
    = extract-single-value(t, generator(old-left-value), index);
  if (new-left-value)
    let old-right-value = merge-right-value(c);
    let new-right-value
      = extract-single-value(t, generator(old-right-value), index);
    if (new-right-value)
      let (merge-c, new-t)
        = make-with-temporary(c.environment, object-class(c),
                              left-value: new-left-value,
                              right-value: new-right-value,
                              ...);
      insert-computation-after!(c, merge-c);
      new-t
    end if
  end if
end method extract-single-value;
*/

define method extract-single-value (t :: <temporary>, c == #f, i :: <integer>)
  #f
end method;

define method extract-single-value
    (t :: <temporary>, c :: <if-merge>, index :: <integer>)
  let merge-users = users(temporary(c));
  // HACK: VERY RETRICTED AT PRESENT BECAUSE WE CAN TOLERATE ONLY
  //       ONE MERGE PER IF
  if (size(merge-users) = 1
        // & instance?(merge-users[0], <extract-single-value>))
        )
    let old-left-value = merge-left-value(c);
    let new-left-value
      = extract-single-value(t, generator(old-left-value), index);
    if (new-left-value)
      let old-right-value = merge-right-value(c);
      let new-right-value
        = extract-single-value(t, generator(old-right-value), index);
      if (new-right-value)
        merge-replace-right-value!(c, old-right-value, new-right-value);
        merge-replace-left-value!(c, old-left-value, new-left-value);
        temporary(c).generator := #f;
        let single-temporary =
          make(<temporary>, generator: c, environment: environment(c));
        temporary(c) := single-temporary;
        re-optimize(c);
        single-temporary
      elseif (new-left-value ~== old-left-value)
        let extract-c = generator(new-left-value);
        when (instance?(extract-c, <extract-single-value>) & useless?(extract-c))
          // make extract go away but be careful not to infinitely reoptimize
          delete-computation!(extract-c);
        end when;
        #f
      end if
    end if
  else
//    format-out("extract-single-value, can't extract: c=%=, merge-users=%=.\n", c, merge-users);
//    format-out("\ttemporary(c)=%=, t=%=.\n", temporary(c), t);
    #f;
  end if
end method extract-single-value;

define method extract-single-value
    (t :: <temporary>, c :: <bind-exit>, index :: <integer>)
  #f // no sensible next-computation
     // eventually, we should push the <extract-single-value> code
     // into the body of the escape procedure
end method extract-single-value;

/// rest vectors (TODO)

define method extract-rest-value
    (t :: <temporary>, c :: <computation>, offset :: <integer>)
  #f
end method extract-rest-value;

define method extract-rest-value
    (t :: <temporary>, c :: <adjust-multiple-values-computation>,
     index :: <integer>)
  extract-rest-value(t, c.computation-value.generator, index)
end method extract-rest-value;
