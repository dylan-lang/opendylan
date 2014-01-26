module:   dfmc-flow-graph
author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
synopsis: graph manipulations on the program flow graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// find the final <computation> by following control flow

// Uses of this function should be considered with strict scutiny.
// The converters should be made to return the first and last computations
// they build in a sequence, which would obviate most uses.  Use of
// this function could lead to O(n^2) behavior where O(n) is expected.

define method final-computation (c :: <computation>)
  for (this = c then next,
       next = c.next-computation then this.next-computation,
       while: next)
  finally
    this
  end for
end method final-computation;


//// connecting two computations

define function join-2x2!
    (xf :: false-or(<computation>), xl :: false-or(<computation>),
     yf :: false-or(<computation>), yl :: false-or(<computation>))
 => (zf :: false-or(<computation>), zl :: false-or(<computation>))
  if (xl)
    if (yf)
      assert(xl.next-computation == #f,
             "first computation in join! has non-empty next-computations");
      assert(yf.previous-computation == #f,
             "second computation in join! has non-empty previous-computation");
      xl.next-computation     := yf;
      yf.previous-computation := xl;
      values(xf, yl)
    else
      assert(xl.next-computation == #f,
             "first computation in join! has non-empty next-computations");
      values(xf, xl)
    end if
  else
    if (yf)
      assert(yf.previous-computation == #f,
             "second computation in join! has non-empty previous-computation");
      values(yf, yl)
    else
      values(yf, yl)
    end if
  end if
end function;

define inline function join-2x2-t!
    (xf :: false-or(<computation>), xl :: false-or(<computation>),
     yf :: false-or(<computation>), yl :: false-or(<computation>),
     t :: false-or(<value-reference>))
 => (zf :: false-or(<computation>), zl :: false-or(<computation>), t :: false-or(<value-reference>))
  let (zf, zl) = join-2x2!(xf, xl, yf, yl);
  values(zf, zl, t)
end function;

define inline function join-1x1!
    (c1 :: false-or(<computation>), c2 :: false-or(<computation>))
 => (c1 :: false-or(<computation>), c2 :: false-or(<computation>))
  join-2x2!(c1, c1, c2, c2)
end function;

define inline function join-1x1-t!
    (c1 :: false-or(<computation>), c2 :: false-or(<computation>),
     t :: false-or(<value-reference>))
 => (c1 :: false-or(<computation>), c2 :: false-or(<computation>),
     t :: false-or(<value-reference>))
  join-2x2-t!(c1, c1, c2, c2, t)
end function;


define inline function join-2x1!
    (xf :: false-or(<computation>), xl :: false-or(<computation>),
     y :: false-or(<computation>))
 => (zf :: false-or(<computation>), zl :: false-or(<computation>))
  join-2x2!(xf, xl, y, y)
end function;

define inline function join-2x1-t!
    (xf :: false-or(<computation>), xl :: false-or(<computation>),
     y :: false-or(<computation>), t :: false-or(<value-reference>))
 => (zf :: <computation>, zl :: <computation>,
     t :: false-or(<value-reference>))
  let (zf, zl) = join-2x1!(xf, xl, y);
  values(zf, zl, t)
end function;

define method join! (c1 :: <computation>, c2 :: <computation>) => ()
  assert(c1.next-computation == #f,
         "first computation in join! has non-empty next-computations");
  assert(c2.previous-computation == #f,
         "second computation in join! has non-empty previous-computation");
  c1.next-computation     := c2;
  c2.previous-computation := c1;
end method;


//// computation rewiring

// recalibrate next-computation slots
define method redirect-previous-computation!
    (prev-c, old-c :: <computation>, new-c :: <computation>) => ()
end method;

define method redirect-previous-computation!
    (prev-c :: <computation>,
     old-c :: <computation>, new-c :: <computation>) => ()
  next-computation(prev-c) := new-c
end method;

define method redirect-previous-computation!
    (prev-c :: <if>, old-c :: <computation>, new-c :: <computation>) => ()
  // if (next-computation(prev-c) == old-c)
  //   assert(instance?(old-c, <if-merge>),
  //          "next-compution of if must be merge");
  //   if (instance?(new-c, <if-merge>))
  //     next-computation(prev-c) := new-c;
  //   elseif (temporary(new-c) == merge-left-value(old-c))
  //     consequent(prev-c) := new-c;
  //   elseif (temporary(new-c) == merge-right-value(old-c))
  //     alternative(prev-c) := new-c;
  //   else
  //     error("Ambiguous insertion of computation into empty if");
  //   end if
  if (consequent(prev-c) == old-c)
    consequent(prev-c) := new-c
  elseif (alternative(prev-c) == old-c)
    alternative(prev-c) := new-c
  else // if (next-computation(prev-c) == old-c)
    next-computation(prev-c) := new-c;
  // else
  //   error("Ambiguous redirection of previous if links");
  end if;
end method;

define method redirect-previous-computation!
    (prev-c :: <block>, old-c :: <computation>, new-c :: <computation>) => ()
  if (body(prev-c) == old-c)
    body(prev-c) := new-c
  else // (next-computation(prev-c) == old-c)
    next-computation(prev-c) := new-c
  end if;
end method;

define method redirect-previous-computation!
    (prev-c :: <loop>, old-c :: <computation>, new-c :: <computation>) => ()
  if (loop-body(prev-c) == old-c)
    loop-body(prev-c) := new-c
  else // (next-computation(prev-c) == old-c)
    next-computation(prev-c) := new-c
  end if;
end method;

define method redirect-previous-computation!
    (prev-c :: <unwind-protect>,
     old-c :: <computation>, new-c :: <computation>) => ()
  if (body(prev-c) == old-c)
    body(prev-c) := new-c
  elseif (cleanups(prev-c) == old-c)
    cleanups(prev-c) := new-c
  else // (next-computation(prev-c) == old-c)
    next-computation(prev-c) := new-c
  end if;
end method;

define method redirect-previous-computations!
    (old-c :: <computation>, new-c :: <computation>) => ()
  redirect-previous-computation!(previous-computation(old-c), old-c, new-c)
end method;


// recalibrate previous-computation slots

define method redirect-next-computation!
    (next-c, old-c :: <computation>, new-c :: false-or(<computation>)) => ()
end method;

define method redirect-next-computation!
    (next-c :: <computation>,
     old-c :: <computation>, new-c :: false-or(<computation>)) => ()
  previous-computation(next-c) := new-c
end method;

define method redirect-next-computation!
    (next-c :: <binary-merge>,
     old-c :: <computation>, new-c :: false-or(<computation>)) => ()
  if (merge-left-previous-computation(next-c) == old-c)
    merge-left-previous-computation(next-c) := new-c
  elseif (merge-right-previous-computation(next-c) == old-c)
    merge-right-previous-computation(next-c) := new-c
  else // (previous-computation(next-c) == old-c)
    // if (instance?(next-c, <if-merge>))
    //  break("oops, %=,%=,%=", next-c, old-c, new-c);
    // end if;
    previous-computation(next-c) := new-c
  end if;
end method;

/*
define method redirect-next-computation!
    (next-c :: <if-merge>,
     old-c :: <computation>, new-c :: false-or(<computation>)) => ()
  if (previous-computation(next-c) == old-c)
    assert(instance?(old-c, <if>), "previous-compution of merge must be if");
    if (instance?(new-c, <if>))
      previous-computation(next-c) := new-c;
    elseif (temporary(new-c) == merge-left-value(next-c))
      merge-left-previous-computation(next-c) := new-c;
    elseif (temporary(new-c) == merge-right-value(next-c))
      merge-right-previous-computation(next-c) := new-c;
    else
      error("Ambiguous insertion of computation into empty if");
    end if
  else
    next-method();
  end if;
end method;
*/

define method redirect-next-computations!
    (old-c :: <computation>, new-c :: false-or(<computation>)) => ()
  redirect-next-computation!(next-computation(old-c), old-c, new-c);
end method;


//// single computation interfaces

define method insert-computation-after!
    (old-c :: <computation>, new-c :: <computation>) => ();
  insert-computations-after!(old-c, new-c, new-c);
end method insert-computation-after!;

define method insert-computation-before!
    (old-c :: <computation>, new-c :: <computation>) => ();
  insert-computations-before!(old-c, new-c, new-c);
end method insert-computation-before!;

define method insert-computations-before-reference!
    (old-c :: <computation>,
     new-first-c :: <computation>, new-last-c :: <computation>,
     ref :: <value-reference>)
 => ()
  insert-computations-before!(old-c, new-first-c, new-last-c);
end method;

define method insert-computation-before-reference!
    (old-c :: <computation>, new-c :: <computation>, ref :: <value-reference>)
 => ()
  insert-computations-before-reference!(old-c, new-c, new-c, ref);
end method;

define method insert-computations-before-reference!
    (old-c :: <if-merge>,
     new-first-c :: <computation>, new-last-c :: <computation>,
     ref :: <value-reference>)
 => ()
  let if-c = previous-computation(old-c);
  case
    merge-left-value(old-c) == ref
      => let prev-c = merge-left-previous-computation(old-c);
	 merge-left-previous-computation(old-c) := new-last-c;
	 next-computation(new-last-c) := old-c;
	 if (prev-c == if-c) // EMPTY BRANCH
	   consequent(if-c) := new-first-c;
	 else
	   next-computation(prev-c) := new-first-c;
	 end if;
	 previous-computation(new-first-c) := prev-c;
    merge-right-value(old-c) == ref
      => let prev-c = merge-right-previous-computation(old-c);
	 merge-right-previous-computation(old-c) := new-last-c;
	 next-computation(new-last-c) := old-c;
	 if (prev-c == if-c) // EMPTY BRANCH
	   alternative(if-c) := new-first-c;
	 else
	   next-computation(prev-c) := new-first-c;
	 end if;
	 previous-computation(new-first-c) := prev-c;
    otherwise
      => insert-computations-before!(old-c, new-first-c, new-last-c);
  end case;
end method;

define method insert-computations-before-reference!
    (old-c :: <loop-merge>,
     new-first-c :: <computation>, new-last-c :: <computation>,
     ref :: <value-reference>)
 => ()
  if (merge-left-value(old-c) == ref)
    insert-computations-before!
      (merge-left-previous-computation(old-c), new-first-c, new-last-c);
  elseif (merge-right-value(old-c) == ref)
    insert-computations-before!
      (merge-right-previous-computation(old-c), new-first-c, new-last-c);
  else
    insert-computations-before!(old-c, new-first-c, new-last-c);
  end if;
end method;

/// RE-OPTIMIZE COMPUTATIONS WHICH MIGHT CHANGE IF NOW IN TAIL POSITION

define method re-optimize-tail-computations (c) => ()
end method;

define method re-optimize-tail-computations (c :: <computation>) => ()
  re-optimize(c);
end method;

define method re-optimize-tail-computations
    (c :: <temporary-transfer-computation>) => ()
  re-optimize-tail-computations(previous-computation(c));
end method;

define method re-optimize-tail-computations (c :: <binary-merge>) => ()
  re-optimize-tail-computations(merge-left-previous-computation(c));
  re-optimize-tail-computations(merge-right-previous-computation(c));
end method;

/// DELETE-COMPUTATION!

define method delete-computation! (c :: <computation>) => ();
  let completion-c = c.next-computation;
  if (completion-c)
    if (instance?(completion-c, <return>))
      re-optimize-tail-computations(c.previous-computation); // revisit tail?
    end if;
    if (instance?(completion-c, <end-block>)   // gts,98feb24
        & entry-state(completion-c))           //  -- added reopt of block
      re-optimize(me-block(entry-state(completion-c)));  // maybe empty now
    end if;
    redirect-previous-computations!(c, completion-c);
    redirect-next-computations!(c, c.previous-computation);
  end if;
  remove-computation-references!(c);
end method;

define method delete-computation! (c :: <if>) => ();
  let completion-c  = c.next-computation;
  let consequent-c  = c.consequent;
  let alternative-c = c.alternative;
  // A BIT HEAVY WEIGHT FOR WHAT IT'S USED FOR
  let keep-c =
    if (instance?(consequent-c, <if-merge>))
      alternative-c
    elseif (instance?(alternative-c, <if-merge>))
      consequent-c
    else
      error ("Can't delete <if> with live consequent and alternative - %=", c)
    end;
  let if-prev = previous-computation(c);
  if (completion-c == keep-c)
    previous-computation(completion-c) := if-prev;
  else
    previous-computation(completion-c) := keep-c;
    previous-computation(keep-c) := if-prev;
  end;
  redirect-previous-computations!(c, keep-c);
  remove-computation-references!(c);
end method;

define method delete-computation! (c :: <bind-exit>) => ();
  // gts-debug("exits", "deleting bind-exit %=.\n", c);
  let merge-c = next-computation(c);
  if (instance?(merge-c, <bind-exit-merge>))
    let body-first-c = body(c);
    let body-last-c  = merge-right-previous-computation(merge-c);
    let end-c        = next-computation(body-last-c);
    debug-assert(instance?(end-c, <end-exit-block>));
    let next-c = next-computation(merge-c);
    let prev-c = previous-computation(c);

    redirect-previous-computations!(c, body-first-c);  // fixup next-computation values
    redirect-next-computations!(merge-c, body-last-c); // fixup previous-computation's
    replace-temporary-in-users!(temporary(merge-c),
				bind-exit-merge-body-temporary(merge-c));
    next-computation(body-last-c) := next-c;
    previous-computation(body-first-c) := prev-c;
    remove-computation-references!(merge-c);
    remove-computation-references!(end-c);
    remove-computation-references!(c);
  else
    next-method();
  end if;
end method;

define method delete-computation! (c :: <exit>) => ();
  // gts-debug("exits", "deleting exit %=.\n", c);
  // remove pointers from entry-state to this exit node
  exits(entry-state(c)) := remove!(exits(entry-state(c)), c);
  if (me-block(entry-state(c)))
    re-optimize(me-block(entry-state(c)));
  end if;
  next-method();
end method;

define method delete-computation! (c :: <unwind-protect>) => ();
  // gts-debug("cleanups", "deleting unwind-protect %=.\n", c);

  let prev-c = previous-computation(c);
  let end-c = protected-end(c);
  let next-c = next-computation(c);
  let end-cleanup = c.cleanups & c.cleanups-end;

  // At most one of the body and the cleanup can be
  // non-empty (otherwise, we shouldn't be deleting this node)
  debug-assert(~(has-cleanups?(c) & has-body?(c)));

  let (first-c, last-c, using-cleanup?)
    = if (has-cleanups?(c))   // body must be empty
        values(c.cleanups,
               previous-computation(c.cleanups-end),
               #t);
      elseif (has-body?(c))   // cleanup #f or empty
        values(c.body,
               previous-computation(c.protected-end),
               #f);
      else                    // neither body nor cleanup
        values(next-c, prev-c, #f)
      end if;

  redirect-previous-computations!(c, first-c);  // fixup next-computation values
  redirect-next-computations!(c, last-c);   // fixup previous-computation's
  next-computation(last-c) := next-c;
  previous-computation(first-c) := prev-c;
  remove-computation-references!(end-cleanup);
  remove-computation-references!(end-c);
  remove-computation-references!(c);
end method;

//// multiple computation interfaces

define method insert-computations-after!
    (old-c :: <if>, first :: <computation>, last :: <computation>) => ()
  error("can't insert computations after branching computations");
  // If that needs to be changed, follow the completion computation
  // forward and insert before that.  But it shouldn't be necessary,
  // given that we can always insert near the merge node.
end method;

define method insert-computations-after!
    (old-c :: <computation>, first :: <computation>, last :: <computation>)
 => ();
  // HACK: DEFEATED ASSERTION FOR NOW
  // assert(~first.previous-computation
  //         /* & empty?(last.next-computations) */);
  redirect-next-computations!(old-c, last);
  last.next-computation := old-c.next-computation;
  first.previous-computation := old-c;
  old-c.next-computation := first;
end method insert-computations-after!;

define method insert-computations-after!
    (old-c :: <computation>, first == #f, last == #f)
 => ()
end method insert-computations-after!;

define method insert-computations-before!
    (old-c :: <computation>, first :: <computation>, last :: <computation>)
 => ();
  assert(~first.previous-computation & ~last.next-computation);
  redirect-previous-computations!(old-c, first);
  first.previous-computation := old-c.previous-computation;
  last.next-computation := old-c;
  old-c.previous-computation := last;
end method insert-computations-before!;

define method insert-computations-before!
    (old-c :: <computation>, first == #f, last == #f) => ()
end method insert-computations-before!;


define function delete-computation-block!
    (start :: <computation>, before, #key on-deletion = identity) => ()
  let queue :: <list> = #();
  walk-computations(method (c)
		      queue := pair(c, queue);
		    end,
		    start, before);
  // everything is now in reverse order, so data dependencies should
  // go away automatically
  for (c in queue)
    let t = c.temporary;
    if (~t | ~used?(t))
      on-deletion(c);
      delete-computation!(c);
    elseif (instance?(c, <make-closure>)
              | every?(rcurry(instance?, <loop-merge>), users(t)))
      replace-temporary-in-users!(t, #f);
      on-deletion(c);
      delete-computation!(c);
    else
      error("Computation still used - %=", c);
    end;
  end;
end;

define function remove-computation-block-references!
    (start :: <computation>, before, #key on-deletion = identity) => ()
  let queue :: <list> = #();
  walk-computations(method (c)
		      queue := pair(c, queue);
		    end,
		    start, before);
  // everything is now in reverse order, so data dependencies should
  // go away automatically
  for (c in queue)
    let t = c.temporary;
    if (~t | ~used?(t))
      on-deletion(c);
      remove-computation-references!(c);
    elseif (instance?(c, <make-closure>)
              | every?(rcurry(instance?, <loop-merge>), users(t)))
      replace-temporary-in-users!(t, #f);
      on-deletion(c);
      remove-computation-references!(c);
    else
      // gts-debug("refs", "remove-computation-block-references!, queue = %=.\n", queue);
      error("Computation still used - %=", c);
    end;
  end;
end;

define method remove-computation-references! (c :: <computation>)
  // gts-debug("refs", "remove-computation-references, c=%=, c.used-temp-accessors=%=.\n",
  //           c, c.used-temporary-accessors);
  mark-as-dead(c);
  let t = temporary(c);
  if (t)
    let env :: <lambda-lexical-environment> = environment(c);
    remove-temporary!(env, t);
  end;
  do-used-temporaries(method (t) remove-user!(t, c) end, c);
end method remove-computation-references!;

define method remove-computation-references! (c :: <make-closure>)
  next-method();
  let lambda = computation-closure-method(c);
  remove-user!(lambda, c);
  re-optimize-users(iep(function(lambda)));
end method remove-computation-references!;

define function maybe-delete-function-body (function :: <&lambda>)
 => (deleted? :: <boolean>)
  let function-body = body(function);
  if (function-body)
    let outside = lambda-environment(outer(environment(function)));
    let visited = make(<set>);
    local method outside-user-c? (user :: <computation>)
	    let lambda-env = lambda-environment(environment(user));
	    lambda-env == outside |
	      used-from-outside?(lambda(lambda-env))
	  end,
          method outside-user? (ref)
            if (instance?(ref, <make-closure>))
              outside-user-c?(ref)
            else
              any?(outside-user-c?, users(ref))
            end if
	  end,
          method used-from-outside? (f :: <&lambda>)
	    if (member?(f, visited))
	      #f
	    else
	      add!(visited, f);
	      any?(outside-user?, users(f)) |
		any?(outside-user?, users(iep(f)))
	    end
	  end;
      unless (used-from-outside?(function))
	// clear the body for recursive references
	body(function) := #f;
	delete-computation-block!(function-body, #f);
	#t
      end;
  end;
end;

define sideways method add-user! (code :: <&code>, user) => ()
  unless (lambda-top-level?(function(code)))
    next-method();
  end;
end;

define sideways method add-user! (function :: <&lambda>, user) => ()
  unless (lambda-top-level?(function))
    next-method();
  end;
end;

define sideways method remove-user! (code :: <&code>, user) => ()
  unless (lambda-top-level?(function(code)))
    next-method();
    maybe-delete-function-body(function(code));
    maybe-re-optimize-used-function(code.environment, code);
  end;
end;

define sideways method remove-user! (function :: <&lambda>, user) => ()
  unless (lambda-top-level?(function))
    next-method();
    maybe-delete-function-body(function);
  end;
end;

//// <computation> replacement

define function replace-computation!
    (old-c :: <computation>, new-first :: false-or(<computation>),
     new-last :: false-or(<computation>),
     new-ref :: false-or(<value-reference>)) => ()
  if (new-first)
    // Put the new code in.  (Must happen before temporary replacement.)
    insert-computations-after!(old-c, new-first, new-last);
  end if;
  // Redirect data flow
  replace-temporary-in-users!(old-c.temporary, new-ref);
  if (new-ref)
    mvt-transfer-values!(old-c.temporary, new-ref);
  end if;
  // Take the old code out.
  delete-computation!(old-c);
end function replace-computation!;

define method replace-computation-with-temporary!
    (c :: <computation>, ref :: <value-reference>)
  let c-t = c.temporary;
  if (c-t)
    replace-temporary-in-users!(c-t, ref);
  end;
  // usually, the computation is removed above,
  // but it might not be side-effect-free
  delete-computation!(c);
  // closure hack, since closure sets aren't register users
  if (c-t & instance?(c, <make-closure>))
    remove-closure-references(c-t);
  end;
end method replace-computation-with-temporary!;

define method remove-closure-references (t :: <temporary>) => ()
  let def-env = t.environment;
  for-all-lambdas (f in def-env.lambda)
    let f-env = f.environment;
    let f-clos = f-env.closure;
    if (member?(t, f-clos))
      f-env.closure := remove!(f-clos, t);
    end;
  end;
end method;

//// <temporary> replacement

// Replace all references to the temporary result t in the computation c
// with the temporary result new-t.

define method replace-temporary-references!
    (c :: <computation>, t :: <value-reference>,
       new-t :: false-or(<value-reference>))
 => (renamed? :: <boolean>)
  debug-assert
    (t ~== new-t,
     "replace-temporary-references! - temporaries should differ but "
     "%= == %=", t, new-t);
  let renamed? :: <boolean> = #f;
  for (accessors :: <temporary-accessors> in c.used-temporary-accessors)
    let getter = temporary-getter(accessors);
    let setter = temporary-zetter(accessors);
    let ref = getter(c);
    if (instance?(ref, <sequence>))
      for (ref-t in ref, index from 0)
        if (ref-t == t)
          ref[index] := new-t;
          if (new-t) add-user!(new-t, c) end;
          remove-user!(t, c);
          renamed? := #t;
	end if;
      end for;
    else
      if (ref == t)
        setter(new-t, c);
        if (new-t) add-user!(new-t, c) end;
        remove-user!(t, c);
        renamed? := #t;
      end if;
    end if;
  end for;
  renamed?
end method replace-temporary-references!;

define method rename-temporary!
    (old-t :: <named-temporary-mixin>, new-t :: <named-temporary-mixin>)
 => (renamed? :: <boolean>)
  unless (named?(new-t))
    name(new-t) := name(old-t);
    #t
  end unless;
end method;

define method rename-temporary!
    (old-t :: <value-reference>, new-t :: <value-reference>)
 => (renamed? :: <boolean>)
  // when (named?(old-t) & instance?(new-t, <temporary>))
  //   break("UNABLE TO RENAME %= TO %=\n", new-t, old-t);
  // end when;
  #f
end method;

define function replace-temporary-in-users!
    (old-t, new-t, #key exclude = method (user) #f end) => ()
  for (old-t-user in old-t.users)
    unless (exclude(old-t-user))
      replace-temporary-references!(old-t-user, old-t, new-t);
      re-optimize(old-t-user);
    end unless;
  end for;
  if (new-t)
    rename-temporary!(old-t, new-t);
    if (~used?(new-t) & generator(new-t))
      re-optimize(generator(new-t));
    end if;
  end if;
end;

define method rename-temporary-references!
    (c :: <computation>, t :: <value-reference>,
     new-t :: <value-reference>) => (renamed? :: <boolean>)
  replace-temporary-references!(c, t, new-t)
end method;
