Module:       collections-test-suite
Author:       Keith Dennison
Synopsis:     Test suite for bit-sets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *finite-members*  = #[0, 10, 27, 30, 55, 60, 69];
define variable *infinite-members* = #[2, 5, 7, 23, 24, 45, 76];

define variable *empty-set* :: <bit-set>
  = make(<bit-set>);

define variable *universal-set* :: <bit-set>
  = make(<bit-set>, all-members-from: 0);

define variable *finite-set* :: <bit-set>
  = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);

define variable *infinite-set* :: <bit-set>
  = make(<bit-set>, all-members-from: 100, members: *infinite-members*);


/*
equals
*/
define test bit-set-equals ()
  let empty-set = make(<bit-set>);
  let universal-set = make(<bit-set>, all-members-from: 0);
  let finite-set
    = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);
  let finite-set1
    = make(<bit-set>, upper-bound-hint: 20, members: #[7, 10, 13, 14, 15, 19]);
  let infinite-set
    = make(<bit-set>, all-members-from: 100, members: *infinite-members*);
  let infinite-set1
    = make(<bit-set>, all-members-from: 20, members: #[0, 1, 2, 3, 7, 8, 11]);


  check-equal("empty-set equals itself", empty-set = empty-set, #t);
  check-equal("universal-set equals itself",
    universal-set = universal-set, #t);
  check-equal("finite-set equals itself", finite-set = finite-set, #t);
  check-equal("infinite-set equals itself", infinite-set = infinite-set, #t);

  check-equal("empty-set equals another", empty-set = *empty-set*, #t);
  check-equal("universal-set equals another",
    universal-set = *universal-set*, #t);
  check-equal("finite-set equals another", finite-set = *finite-set*, #t);
  check-equal("infinite-set equals another",
    infinite-set = *infinite-set*, #t);

  check-equal("empty-set not equal to universal-set",
    empty-set = universal-set, #f);
  check-equal("empty-set not equal to finite-set",
    empty-set = finite-set, #f);
  check-equal("empty-set not equal to infinite-set",
    empty-set = infinite-set, #f);

  check-equal("universal-set not equal to finite-set",
    universal-set = finite-set, #f);
  check-equal("universal-set not equal to infinite-set",
    universal-set = infinite-set, #f);

  check-equal("finite-set not equal to infinite-set",
    finite-set = infinite-set, #f);

  check-equal("finite-set not equal to finite-set1",
    finite-set = finite-set1, #f);

  check-equal("infinite-set not equal to infinite-set1",
    infinite-set = infinite-set1, #f);
end test;


/*
member?
*/
define test bit-set-member ()
  check-equal("0 not in empty set", member?(0, *empty-set*), #f);
  check-equal("100000 not in empty set", member?(100000, *empty-set*), #f);

  check-equal("0 in universal set", member?(0, *universal-set*), #t);
  check-equal("100000 in universal set", member?(100000, *universal-set*), #t);

  for (i in *finite-members*)
    check-equal(format-to-string("%d not in empty set", i),
                member?(i, *empty-set*), #f);
    check-equal(format-to-string("%d in universal set", i),
                member?(i, *universal-set*), #t);
    check-equal(format-to-string("%d in finite set", i),
                member?(i, *finite-set*), #t);
    check-equal(format-to-string("%d not in infinite set", i),
                member?(i, *infinite-set*), #f);
  end for;

  for (i in *infinite-members*)
    check-equal(format-to-string("%d not in empty set", i),
                member?(i, *empty-set*), #f);
    check-equal(format-to-string("%d in universal set", i),
                member?(i, *universal-set*), #t);
    check-equal(format-to-string("%d not in finite set", i),
                member?(i, *finite-set*), #f);
    check-equal(format-to-string("%d in infinite set", i),
                member?(i, *infinite-set*), #t);
  end for;
end test;


/*
add, add!, remove, remove!
  1. new element
  2. element already in the set
  3. element beyond initial upper-bound hint
  4. Do for finite & infinite sets
*/
define function check-bit-set-add
    (prefix :: <string>, set :: <bit-set>, i :: <integer>)
 => (new-set :: <bit-set>)
  let old-infinite? = infinite?(set);
  let old-size = size(set);
  let already-present? = member?(i, set);
  let new-set = set-add(set, i);

  if (old-size)
    if (already-present?)
      check-equal(concatenate(prefix, "Doesn't increase set size"),
        size(new-set), old-size);
    else
      check-equal(concatenate(prefix, "Increases set size by 1"),
        size(new-set), old-size + 1);
    end if;
  else
    check-equal(concatenate(prefix, "Set size still #f"), size(new-set), #f);
  end if;
  check-equal(concatenate(prefix, "Member of new set"),
    member?(i, new-set), #t);
  check-equal(concatenate(prefix, "New set not empty"), empty?(new-set), #f);
  check-equal(concatenate(prefix, "No change in (in)finiteness of set"),
    infinite?(new-set), old-infinite?);
  new-set;
end function;

define test bit-set-add ()
  check-bit-set-add("Add 0 to empty set: ", *empty-set*, 0);
  check-bit-set-add("Add 100000 to empty set: ", *empty-set*, 100000);

  check-bit-set-add("Add 0 to universal set: ", *universal-set*, 0);
  check-bit-set-add("Add 100000 to universal set: ", *universal-set*, 100000);

  let new-set = check-bit-set-add("Add 56 to finite-set: ", *finite-set*, 56);
  let new-set = check-bit-set-add("Add 209 to finite-set: ", new-set, 209);
  check-bit-set-add("Add 10 to finite-set: ", new-set, 10);

  let new-set = check-bit-set-add("Add 6 to infinite-set: ",
    *infinite-set*, 6);
  let new-set = check-bit-set-add("Add 378 to infinite-set: ", new-set, 378);
  check-bit-set-add("Add 76 to infinite-set: ", new-set, 76);
end test;


define function check-bit-set-add!
    (prefix :: <string>, set :: <bit-set>, i :: <integer>)
 => (new-set :: <bit-set>)
  let old-infinite? = infinite?(set);
  let old-size = size(set);
  let already-present? = member?(i, set);
  let new-set = set-add!(set, i);

  check(concatenate(prefix, "Modifies set"), \==, set, new-set);
  if (old-size)
    if (already-present?)
      check-equal(concatenate(prefix, "Doesn't increase set size"),
        size(new-set), old-size);
    else
      check-equal(concatenate(prefix, "Increases set size by 1"),
        size(new-set), old-size + 1);
    end if;
  else
    check-equal(concatenate(prefix, "Set size still #f"), size(new-set), #f);
  end if;
  check-equal(concatenate(prefix, "Member of new set"),
    member?(i, new-set), #t);
  check-equal(concatenate(prefix, "New set not empty"), empty?(new-set), #f);
  check-equal(concatenate(prefix, "No change in (in)finiteness of set"),
    infinite?(new-set), old-infinite?);
  new-set;
end function;

define test bit-set-add! ()
  let empty-set = make(<bit-set>);
  let universal-set = make(<bit-set>, all-members-from: 0);
  let finite-set = make(<bit-set>, upper-bound-hint: 70,
    members: *finite-members*);
  let infinite-set = make(<bit-set>, all-members-from: 100,
    members: *infinite-members*);

  check-bit-set-add!("Add! 0 to empty set: ", empty-set, 0);
  check-bit-set-add!("Add! 100000 to empty set: ", empty-set, 100000);

  check-bit-set-add!("Add! 0 to universal set: ", universal-set, 0);
  check-bit-set-add!("Add! 100000 to universal set: ", universal-set, 100000);

  let new-set = check-bit-set-add!("Add! 56 to finite-set: ", finite-set, 56);
  let new-set = check-bit-set-add!("Add! 209 to finite-set: ", new-set, 209);
  check-bit-set-add!("Add! 10 to finite-set: ", new-set, 10);

  let new-set = check-bit-set-add!("Add! 6 to infinite-set: ",
    infinite-set, 6);
  let new-set = check-bit-set-add!("Add! 378 to infinite-set: ", new-set, 378);
  check-bit-set-add!("Add! 76 to infinite-set: ", new-set, 76);
end test;


define function check-bit-set-remove
    (prefix :: <string>, set :: <bit-set>, i :: <integer>)
 => (new-set :: <bit-set>)
  let old-infinite? = infinite?(set);
  let old-size = size(set);
  let present? = member?(i, set);
  let new-set = set-remove(set, i);

  if (old-size)
    if (present?)
      check-equal(concatenate(prefix, "Decreases set size by 1"),
        size(new-set), old-size - 1);
    else
      check-equal(concatenate(prefix, "Doesn't decrease set size"),
        size(new-set), old-size);
    end if;
  else
    check-equal(concatenate(prefix, "Set size still #f"), size(new-set), #f);
  end if;
  check-equal(concatenate(prefix, "Not member of new set"),
    member?(i, new-set), #f);
  if (old-size = 1 & present?)
    check-equal(concatenate(prefix, "New set empty"), empty?(new-set), #t);
  end if;
  check-equal(concatenate(prefix, "No change in (in)finiteness of set"),
    infinite?(new-set), old-infinite?);
  new-set;
end function;

define test bit-set-remove ()
  check-bit-set-remove("Remove 0 from empty set: ", *empty-set*, 0);
  check-bit-set-remove("Remove 100000 from empty set: ", *empty-set*, 100000);

  check-bit-set-remove("Remove 0 from universal set: ", *universal-set*, 0);
  check-bit-set-remove("Remove 100000 from universal set: ", *universal-set*,
    100000);

  let new-set = check-bit-set-remove("Remove 30 from finite-set: ",
    *finite-set*, 30);
  let new-set = check-bit-set-remove("Remove 209 from finite-set: ",
    new-set, 209);
  check-bit-set-remove("Remove 5 from finite-set: ", new-set, 5);

  let new-set = check-bit-set-remove("Remove 2 from infinite-set: ",
    *infinite-set*, 2);
  let new-set = check-bit-set-remove("Remove 378 from infinite-set: ",
    new-set, 378);
  check-bit-set-remove("Remove 75 from infinite-set: ", new-set, 75);
end test;


define function check-bit-set-remove!
    (prefix :: <string>, set :: <bit-set>, i :: <integer>)
 => (new-set :: <bit-set>)
  let old-infinite? = infinite?(set);
  let old-size = size(set);
  let present? = member?(i, set);
  let new-set = set-remove!(set, i);

  check(concatenate(prefix, "Modifies set"), \==, set, new-set);
  if (old-size)
    if (present?)
      check-equal(concatenate(prefix, "Decreases set size by 1"),
        size(new-set), old-size - 1);
    else
      check-equal(concatenate(prefix, "Doesn't decrease set size"),
        size(new-set), old-size);
    end if;
  else
    check-equal(concatenate(prefix, "Set size still #f"), size(new-set), #f);
  end if;
  check-equal(concatenate(prefix, "Not member of new set"),
    member?(i, new-set), #f);
  if (old-size = 1 & present?)
    check-equal(concatenate(prefix, "New set empty"), empty?(new-set), #t);
  end if;
  check-equal(concatenate(prefix, "No change in (in)finiteness of set"),
    infinite?(new-set), old-infinite?);
  new-set;
end function;

define test bit-set-remove! ()
  let empty-set = make(<bit-set>);
  let universal-set = make(<bit-set>, all-members-from: 0);
  let finite-set = make(<bit-set>, upper-bound-hint: 70,
    members: *finite-members*);
  let infinite-set = make(<bit-set>, all-members-from: 100,
    members: *infinite-members*);

  check-bit-set-remove!("Remove! 0 from empty set: ", empty-set, 0);
  check-bit-set-remove!("Remove! 100000 from empty set: ", empty-set, 100000);

  check-bit-set-remove!("Remove! 0 from universal set: ", universal-set, 0);
  check-bit-set-remove!("Remove! 100000 from universal set: ", universal-set,
    100000);

  let new-set = check-bit-set-remove!("Remove! 30 from finite-set: ",
    finite-set, 30);
  let new-set = check-bit-set-remove!("Remove! 209 from finite-set: ",
    new-set, 209);
  check-bit-set-remove!("Remove! 5 from finite-set: ", new-set, 5);

  let new-set = check-bit-set-remove!("Remove! 2 from infinite-set: ",
    infinite-set, 2);
  let new-set = check-bit-set-remove!("Remove! 378 from infinite-set: ",
    new-set, 378);
  check-bit-set-remove!("Remove! 75 from infinite-set: ", new-set, 75);
end test;


/*
infinite?
*/
define test bit-set-infinite ()
  check-equal("Empty bit-set is finite", infinite?(*empty-set*), #f);
  check-equal("Universal bit-set is infinite", infinite?(*universal-set*), #t);
  check-equal("Finite bit-set is finite", infinite?(*finite-set*), #f);
  check-equal("Infinite bit-set is infinite", infinite?(*infinite-set*), #t);
end test;


/*
empty?
*/
define test bit-set-empty ()
  check-equal("Empty bit-set is empty", empty?(*empty-set*), #t);
  check-equal("Universal bit-set is not empty", empty?(*universal-set*), #f);
  check-equal("Finite bit-set is not empty", empty?(*finite-set*), #f);
  check-equal("Infinite bit-set is not empty", empty?(*infinite-set*), #f);
end test;


/*
size
  1. finite set
  2. infinite set
*/
define test bit-set-size ()
  check-equal("Empty bit-set size is zero", size(*empty-set*), 0);
  check-equal("Universal bit-set size is #f", size(*universal-set*), #f);
  check-equal("Finite bit-set size is 7", size(*finite-set*), 7);
  check-equal("Infinite bit-set size is #f", size(*infinite-set*), #f);
end test;


/*
set-union, set-union!, set-intersecion, set-intersection!,
set-complement, set-complement!, set-difference, set-difference!
  1. two finite sets
  2. one infinite & one finite set
  3. two infinite sets
  4. empty set
  5. universal set
*/
define test bit-set-union ()
  let finite-infinite-union = make(<bit-set>);
  copy-bit-set!(finite-infinite-union, *infinite-set*);
  for (i in *finite-members*)
    set-add!(finite-infinite-union, i);
  end for;

  check-equal("Union empty-set with itself",
    set-union(*empty-set*, *empty-set*) = *empty-set*, #t);
  check-equal("Union empty-set with universal-set",
    set-union(*empty-set*, *universal-set*) = *universal-set*, #t);
  check-equal("Union empty-set with finite-set",
    set-union(*empty-set*, *finite-set*) = *finite-set*, #t);
  check-equal("Union empty-set with infinite-set",
    set-union(*empty-set*, *infinite-set*) = *infinite-set*, #t);

  check-equal("Union universal-set with itself",
    set-union(*universal-set*, *universal-set*) = *universal-set*, #t);
  check-equal("Union universal-set with finite-set",
    set-union(*universal-set*, *finite-set*) = *universal-set*, #t);
  check-equal("Union universal-set with infinite-set",
    set-union(*universal-set*, *infinite-set*) = *universal-set*, #t);

  check-equal("Union finite-set with itself",
    set-union(*finite-set*, *finite-set*) = *finite-set*, #t);
  check-equal("Union finite-set with infinite-set",
    set-union(*finite-set*, *infinite-set*) = finite-infinite-union, #t);

  check-equal("Union infinite-set with itself",
    set-union(*infinite-set*, *infinite-set*) = *infinite-set*, #t);
end test;

define test bit-set-union! ()
  let empty-set :: <bit-set> = make(<bit-set>);
  let universal-set :: <bit-set> = make(<bit-set>, all-members-from: 0);
  let finite-set :: <bit-set>
    = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);
  let infinite-set :: <bit-set>
    = make(<bit-set>, all-members-from: 100, members: *infinite-members*);
  let finite-infinite-union = make(<bit-set>);
  copy-bit-set!(finite-infinite-union, *infinite-set*);
  for (i in *finite-members*)
    set-add!(finite-infinite-union, i);
  end for;

  let set = set-union!(empty-set, *empty-set*);
  check("Union! empty-set with itself: =", \=, set, *empty-set*);
  check("Union! empty-set with itself: ==", \==, set, empty-set);
  let set = set-union!(empty-set, *finite-set*);
  check("Union! empty-set with finite-set: =", \=, set, *finite-set*);
  check("Union! empty-set with finite-set: ==", \==, set, empty-set);
  let set = set-union!(empty-set, *infinite-set*);
  check("Union! empty-set with infinite-set: =",
    \=, set, finite-infinite-union);
  check("Union! empty-set with infinite-set: ==", \==, set, empty-set);
  let set = set-union!(empty-set, *universal-set*);
  check("Union! empty-set with universal-set: =", \=, set, *universal-set*);
  check("Union! empty-set with universal-set: ==", \==, set, empty-set);

  let set = set-union!(universal-set, *universal-set*);
  check("Union! universal-set with itself: =", \=, set, *universal-set*);
  check("Union! universal-set with itself: ==", \==, set, universal-set);
  let set = set-union!(universal-set, *finite-set*);
  check("Union! universal-set with finite-set: =", \=, set, *universal-set*);
  check("Union! universal-set with finite-set: ==", \==, set, universal-set);
  let set = set-union!(universal-set, *infinite-set*);
  check("Union! universal-set with infinite-set: =", \=, set, *universal-set*);
  check("Union! universal-set with infinite-set: ==", \==, set, universal-set);

  let set = set-union!(finite-set, *finite-set*);
  check("Union! finite-set with itself: =", \=, set, *finite-set*);
  check("Union! finite-set with itself: ==", \==, set, finite-set);

  let set = set-union!(infinite-set, *infinite-set*);
  check("Union! infinite-set with itself: =", \=, set, *infinite-set*);
  check("Union! infinite-set with itself: ==", \==, set, infinite-set);
end test;

define test bit-set-intersection ()
  check-equal("Intersect empty-set with itself",
    set-intersection(*empty-set*, *empty-set*) = *empty-set*, #t);
  check-equal("Intersect empty-set with universal-set",
    set-intersection(*empty-set*, *universal-set*) = *empty-set*, #t);
  check-equal("Intersect empty-set with finite-set",
    set-intersection(*empty-set*, *finite-set*) = *empty-set*, #t);
  check-equal("Intersect empty-set with infinite-set",
    set-intersection(*empty-set*, *infinite-set*) = *empty-set*, #t);

  check-equal("Intersect universal-set with itself",
    set-intersection(*universal-set*, *universal-set*) = *universal-set*, #t);
  check-equal("Intersect universal-set with finite-set",
    set-intersection(*universal-set*, *finite-set*) = *finite-set*, #t);
  check-equal("Intersect universal-set with infinite-set",
    set-intersection(*universal-set*, *infinite-set*) = *infinite-set*, #t);

  check-equal("Intersect finite-set with itself",
    set-intersection(*finite-set*, *finite-set*) = *finite-set*, #t);
  check-equal("Intersect finite-set with infinite-set",
    set-intersection(*finite-set*, *infinite-set*) = *empty-set*, #t);

  check-equal("Intersect infinite-set with itself",
    set-intersection(*infinite-set*, *infinite-set*) = *infinite-set*, #t);
end test;

define test bit-set-intersection! ()
  let empty-set :: <bit-set> = make(<bit-set>);
  let universal-set :: <bit-set> = make(<bit-set>, all-members-from: 0);
  let finite-set :: <bit-set>
    = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);
  let infinite-set :: <bit-set>
    = make(<bit-set>, all-members-from: 100, members: *infinite-members*);

  let set = set-intersection!(empty-set, *empty-set*);
  check("Intersect! empty-set with itself: =", \=, set, *empty-set*);
  check("Intersect! empty-set with itself: ==", \==, set, empty-set);
  let set = set-intersection!(empty-set, *finite-set*);
  check("Intersect! empty-set with finite-set: =", \=, set, *empty-set*);
  check("Intersect! empty-set with finite-set: ==", \==, set, empty-set);
  let set = set-intersection!(empty-set, *infinite-set*);
  check("Intersect! empty-set with infinite-set: =", \=, set, *empty-set*);
  check("Intersect! empty-set with infinite-set: ==", \==, set, empty-set);
  let set = set-intersection!(empty-set, *universal-set*);
  check("Intersect! empty-set with universal-set: =", \=, set, *empty-set*);
  check("Intersect! empty-set with universal-set: ==", \==, set, empty-set);

  let set = set-intersection!(universal-set, *universal-set*);
  check("Intersect! universal-set with itself: =", \=, set, *universal-set*);
  check("Intersect! universal-set with itself: ==", \==, set, universal-set);
  let set = set-intersection!(universal-set, *finite-set*);
  check("Intersect! universal-set with finite-set: =", \=, set, *finite-set*);
  check("Intersect! universal-set with finite-set: ==",
    \==, set, universal-set);
  let set = set-intersection!(universal-set, *infinite-set*);
  check("Intersect! universal-set with infinite-set: =", \=, set, *empty-set*);
  check("Intersect! universal-set with infinite-set: ==",
    \==, set, universal-set);

  let set = set-intersection!(finite-set, *finite-set*);
  check("Intersect! finite-set with itself: =", \=, set, *finite-set*);
  check("Intersect! finite-set with itself: ==", \==, set, finite-set);

  let set = set-intersection!(infinite-set, *infinite-set*);
  check("Intersect! infinite-set with itself: =", \=, set, *infinite-set*);
  check("Intersect! infinite-set with itself: ==", \==, set, infinite-set);
end test;

define test bit-set-difference ()
  check-equal("Difference empty-set with itself",
    set-difference(*empty-set*, *empty-set*) = *empty-set*, #t);
  check-equal("Difference empty-set with universal-set",
    set-difference(*empty-set*, *universal-set*) = *empty-set*, #t);
  check-equal("Difference empty-set with finite-set",
    set-difference(*empty-set*, *finite-set*) = *empty-set*, #t);
  check-equal("Difference empty-set with infinite-set",
    set-difference(*empty-set*, *infinite-set*) = *empty-set*, #t);

  check-equal("Difference universal-set with itself",
    set-difference(*universal-set*, *universal-set*) = *empty-set*, #t);
  check-equal("Difference universal-set with finite-set",
    set-difference(*universal-set*, *finite-set*)
      = set-complement(*finite-set*), #t);
  check-equal("Difference universal-set with infinite-set",
    set-difference(*universal-set*, *infinite-set*)
      = set-complement(*infinite-set*), #t);

  check-equal("Difference finite-set with itself",
    set-difference(*finite-set*, *finite-set*) = *empty-set*, #t);
  check-equal("Difference finite-set with infinite-set",
    set-difference(*finite-set*, *infinite-set*) = *finite-set*, #t);

  check-equal("Difference infinite-set with itself",
    set-difference(*infinite-set*, *infinite-set*) = *empty-set*, #t);
end test;

define test bit-set-difference! ()
  let empty-set :: <bit-set> = make(<bit-set>);
  let universal-set :: <bit-set> = make(<bit-set>, all-members-from: 0);
  let finite-set :: <bit-set>
    = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);
  let infinite-set :: <bit-set>
    = make(<bit-set>, all-members-from: 100, members: *infinite-members*);

  let universal-finite-difference = make(<bit-set>, all-members-from: 0);
  do(method (i) remove!(universal-finite-difference, i) end, *finite-members*);

  let universal-finite-infinite-difference
    = set-intersection(universal-finite-difference,
                       set-complement(infinite-set));

  let set = set-difference!(empty-set, *empty-set*);
  check("Difference! empty-set with itself: =", \=, set, *empty-set*);
  check("Difference! empty-set with itself: ==", \==, set, empty-set);
  let set = set-difference!(empty-set, *finite-set*);
  check("Difference! empty-set with finite-set: =", \=, set, *empty-set*);
  check("Difference! empty-set with finite-set: ==", \==, set, empty-set);
  let set = set-difference!(empty-set, *infinite-set*);
  check("Difference! empty-set with infinite-set: =", \=, set, *empty-set*);
  check("Difference! empty-set with infinite-set: ==", \==, set, empty-set);
  let set = set-difference!(empty-set, *universal-set*);
  check("Difference! empty-set with universal-set: =", \=, set, *empty-set*);
  check("Difference! empty-set with universal-set: ==", \==, set, empty-set);


  let set = set-difference!(universal-set, *finite-set*);
  check("Difference! universal-set with finite-set: =",
    \=, set, universal-finite-difference);
  check("Difference! universal-set with finite-set: ==",
    \==, set, universal-set);
  let set = set-difference!(universal-set, *infinite-set*);
  check("Difference! universal-set with infinite-set: =",
    \=, set, universal-finite-infinite-difference);
  check("Difference! universal-set with infinite-set: ==",
    \==, set, universal-set);
  let set = set-difference!(universal-set, *universal-set*);
  check("Difference! universal-set with itself: =", \=, set, *empty-set*);
  check("Difference! universal-set with itself: ==", \==, set, universal-set);

  let set = set-difference!(finite-set, *finite-set*);
  check("Difference! finite-set with itself: =", \=, set, *empty-set*);
  check("Difference! finite-set with itself: ==", \==, set, finite-set);

  let set = set-difference!(infinite-set, *infinite-set*);
  check("Difference! infinite-set with itself: =", \=, set, *empty-set*);
  check("Difference! infinite-set with itself: ==", \==, set, infinite-set);
end test;

define test bit-set-complement ()
  let finite-set-complement = make(<bit-set>, all-members-from: 0);
  do(method (i) remove!(finite-set-complement, i) end, *finite-members*);

  let infinite-set-complement = make(<bit-set>);
  for (i from 0 below 100)
    if (~member?(i, *infinite-members*))
      set-add!(infinite-set-complement, i);
    end if;
  end for;

  check-equal("Complement of empty-set is the universal set",
    set-complement(*empty-set*) = *universal-set*, #t);
  check-equal("Complement of universal-set is the empty set",
    set-complement(*universal-set*) = *empty-set*, #t);

  let finite-set1 = set-complement(*finite-set*);
  check-equal("Complement of finite-set is correct",
    finite-set1 = finite-set-complement, #t);
  let finite-set2 = set-complement(finite-set1);
  check-equal("Complement of complement of finite-set is finite-set",
    finite-set2 = *finite-set*, #t);

  let infinite-set1 = set-complement(*infinite-set*);
  check-equal("Complement of infinite-set is correct",
    infinite-set1 = infinite-set-complement, #t);
  let infinite-set2 = set-complement(infinite-set1);
  check-equal("Complement of complement of infinite-set is infinite-set",
    infinite-set2 = *infinite-set*, #t);
end test;

define test bit-set-complement! ()
  let empty-set :: <bit-set> = make(<bit-set>);
  let universal-set :: <bit-set> = make(<bit-set>, all-members-from: 0);
  let finite-set :: <bit-set>
    = make(<bit-set>, upper-bound-hint: 70, members: *finite-members*);
  let infinite-set :: <bit-set>
    = make(<bit-set>, all-members-from: 100, members: *infinite-members*);

  let finite-set-complement = make(<bit-set>, all-members-from: 0);
  do(method (i) remove!(finite-set-complement, i) end, *finite-members*);

  let infinite-set-complement = make(<bit-set>);
  for (i from 0 below 100)
    if (~member?(i, *infinite-members*))
      set-add!(infinite-set-complement, i);
    end if;
  end for;

  let set = set-complement!(empty-set);
  check("Complement! of empty-set: =", \=, set, *universal-set*);
  check("Complement! of empty-set: ==", \==, set, empty-set);

  let set = set-complement!(universal-set);
  check("Complement of universal-set: =", \=, set, *empty-set*);
  check("Complement of universal-set: ==", \==, set, universal-set);

  let set = set-complement!(finite-set);
  check("Complement of finite-set: =", \=, set, finite-set-complement);
  check("Complement of finite-set: ==", \==, set, finite-set);
  let set = set-complement!(finite-set);
  check("Complement of complement of finite-set: =", \=, set, *finite-set*);
  check("Complement of complement of finite-set: ==", \==, set, finite-set);

  let set = set-complement!(infinite-set);
  check("Complement of infinite-set is: =", \=, set, infinite-set-complement);
  check("Complement of infinite-set is: ==", \==, set, infinite-set);
  let set = set-complement!(infinite-set);
  check("Complement of complement of infinite-set: =",
    \=, set, *infinite-set*);
  check("Complement of complement of infinite-set: ==",
    \==, set, infinite-set);
end test;


/*
copy-bit-set!
*/
define test bit-set-copy ()
  let set = make(<bit-set>);
  copy-bit-set!(set, *empty-set*);
  check-equal("empty-set copied into set", set = *empty-set*, #t);

  copy-bit-set!(set, *universal-set*);
  check-equal("universal-set copied into set", set = *universal-set*, #t);

  copy-bit-set!(set, *finite-set*);
  check-equal("finite-set copied into set", set = *finite-set*, #t);

  copy-bit-set!(set, *infinite-set*);
  check-equal("infinite-set copied into set", set = *infinite-set*, #t);
end test;

/*
empty-bit-set!
*/
define test bit-set-force-empty ()
  let empty-set = make(<bit-set>);
  let universal-set = make(<bit-set>, all-members-from: 0);
  let finite-set = make(<bit-set>, upper-bound-hint: 70,
    members: *finite-members*);
  let infinite-set = make(<bit-set>, all-members-from: 100,
    members: *infinite-members*);

  empty-bit-set!(empty-set);
  check-equal("empty-set is now empty", empty-set = *empty-set*, #t);
  empty-bit-set!(universal-set);
  check-equal("universal-set is now empty", universal-set = *empty-set*, #t);
  empty-bit-set!(finite-set);
  check-equal("finite-set is now empty", finite-set = *empty-set*, #t);
  empty-bit-set!(infinite-set);
  check-equal("infinite-set is now empty", infinite-set = *empty-set*, #t);
end test;


/*
universal-bit-set!
*/
define test bit-set-force-universal ()
  let empty-set = make(<bit-set>);
  let universal-set = make(<bit-set>, all-members-from: 0);
  let finite-set = make(<bit-set>, upper-bound-hint: 70,
    members: *finite-members*);
  let infinite-set = make(<bit-set>, all-members-from: 100,
    members: *infinite-members*);

  universal-bit-set!(empty-set);
  check-equal("empty-set is now universal", empty-set = *universal-set*, #t);
  universal-bit-set!(universal-set);
  check-equal("universal-set is now universal",
    universal-set = *universal-set*, #t);
  universal-bit-set!(finite-set);
  check-equal("finite-set is now universal", finite-set = *universal-set*, #t);
  universal-bit-set!(infinite-set);
  check-equal("infinite-set is now universal",
    infinite-set = *universal-set*, #t);
end test;


/*
forward-iteration-protocol
  1. empty set
  2. universal set
  3. finite set
  4. infinite set
*/
define test bit-set-iteration ()

  for (count from 0, i in *empty-set*)
  finally check-equal("Forward iteration over the empty set", count, 0);
  end for;

  let result = #t;
  for (i in *universal-set*, count from 0, while: i < 50)
    result := result & (i = count);
  end for;
  check-equal("Forward iteration over the universal set", result, #t);

  let result = #t;
  for (count from 0, i in *finite-set*)
    result := result & (i = *finite-members*[count]);
  end for;
  check-equal("Forward iteration over a finite set", result, #t);

  let result = #t;
  let expect = 100;
  for (count from 0, i in *infinite-set*, while: i < 120)
    if (count < size(*infinite-members*))
      result := result & (i = *infinite-members*[count]);
    else
      result := result & (i = expect);
      expect := expect + 1;
    end if;
  end for;
  check-equal("Forward iteration over an infinite set", result, #t);

  for (count from 0,
       i in *empty-set* using backward-iteration-protocol)
  finally check-equal("Backward iteration over the empty set", count, 0);
  end for;

  let result = #t;
  let set-size = size(*finite-set*);
  for (count from 1,
       i in *finite-set* using backward-iteration-protocol)
    result := result & (i = *finite-members*[set-size - count]);
  end for;
  check-equal("Backward iteration over a finite set", result, #t);
end test;


define method bit-set-laws-one(set :: <bit-set>) => ()
  // Empty set laws
  check-equal("Empty set law 1",
    set-intersection(set, *empty-set*), *empty-set*);
  check-equal("Empty set law 2",
    set-union(set, *empty-set*), set);

  // Idempotency laws
  check-equal("Idempotency law 1",
    set-intersection(set, set), set);
  check-equal("Idempotency law 2",
    set-union(set, set), set);
end method;

define method bit-set-laws-two(set1 :: <bit-set>, set2 :: <bit-set>) => ()
  // Commutative laws
  check-equal("Commutative law 1",
    set-intersection(set1, set2), set-intersection(set2, set1));
  check-equal("Commutative law 2",
    set-union(set1, set2), set-union(set2, set1));

  // Absorption laws
  check-equal("Absorption law 1",
    set-intersection(set1, set-union(set1, set2)), set1);
  check-equal("Absorption law 2",
    set-union(set1, set-intersection(set1, set2)), set1);
end method;

define method bit-set-laws-three
    (set1 :: <bit-set>, set2 :: <bit-set>, set3 :: <bit-set>) => ()
  // Associative laws
  check-equal("Associative law 1",
    set-intersection(set1, set-intersection(set2, set3)),
    set-intersection(set-intersection(set1, set2), set3));
  check-equal("Associative law 2",
    set-union(set1, set-union(set2, set3)),
    set-union(set-union(set1, set2), set3));

  // Distributive laws
  check-equal("Distributive law 1",
    set-intersection(set1, set-union(set2, set3)),
    set-union(set-intersection(set1, set2), set-intersection(set1, set3)));
  check-equal("Distributive law 2",
    set-union(set1, set-intersection(set2, set3)),
    set-intersection(set-union(set1, set2), set-union(set1, set3)));

  // DeMorgan's laws
  check-equal("DeMorgan's law 1",
    set-difference(set1, set-intersection(set2, set3)),
    set-union(set-difference(set1, set2), set-difference(set1, set3)));
  check-equal("DeMorgan's law 2",
    set-difference(set1, set-union(set2, set3)),
    set-intersection(set-difference(set1, set2), set-difference(set1, set3)));
end method;

define test bit-set-laws ()
  let set1 = make(<bit-set>, all-members-from: 109,
    members: #[0, 5, 13, 34, 35, 47, 55, 60, 78, 83, 87, 90, 92, 95, 99, 107]);
  let set2 = make(<bit-set>,
    members: #[2, 5, 17, 22, 27, 30, 35, 42, 50, 52, 57, 63, 65, 70, 73, 77]);
  let set3 = make(<bit-set>,
     members: #[0, 5, 17, 34, 35, 47, 50, 60, 63, 78, 110, 120, 125, 130]);

  bit-set-laws-one(set1);
  bit-set-laws-one(set2);
  bit-set-laws-one(set3);

  bit-set-laws-two(set1, set2);
  bit-set-laws-two(set1, set3);
  bit-set-laws-two(set2, set3);

  bit-set-laws-three(set1, set2, set3);
  bit-set-laws-three(set1, set3, set2);
  bit-set-laws-three(set2, set1, set3);
  bit-set-laws-three(set2, set3, set1);
  bit-set-laws-three(set3, set1, set2);
  bit-set-laws-three(set3, set2, set1);
end test;
