Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// gts,98may27:  I can't stand it -- I copied what I got from Kevin to 
//  typist-if.orig, and I'm attacking this code.  I'm getting rid 
//  of all the specialized generator and temporary calculation, and 
//  I'll see if I run into the reasons they got stuck in here.
//  It could be that we want a 'real-generator' function which tracks back 
//  through the generator chain to find the nearest non-trivial generator. 
//  That I could understand.  If only there were a few comments here...
//  Never mind:
//  Look for gts, suspect: ... gts, replace: ...
//  for suggested replacements/simplifications of code.

/* gts:
  let gen = get-generator(if-c.test);
  let value = gen & gen.temporary;
  let fn = gen & get-if-inferencer(gen);
  if (fn)
    do-if-inference(fn, if-c, gen, css);
  else
    default-initial-if-inference(gen, value, if-c, css);
  end;
*/

define method initialize-node-type 
  (if-c :: <if>, css, work-agenda) => ();
  let test-value = if-c.test;
  let gen = get-generator(test-value);
  let value = if (gen)
                gen.temporary;
              else
                //uck : hard code in knowledge about preceding primitive
                test-value.generator.arguments[0];
              end;
  let fn = gen & get-if-inferencer(gen);
  if (fn)
    do-if-inference(fn, if-c, gen, css);
  else
    default-initial-if-inference(gen, value, if-c, css);
  end;
end;

define method default-initial-if-inference
  (gen, temp, if-c, css :: <call-site-summary>) 
  => ()
  let (true-type, false-type, temp-to-rename) = split-types(temp, gen, if-c, css);
  set-branch(true-type, false-type, if-c, css);
  unless (~true-type
          | instance?(true-type, <values-type>)
          | ^type-equivalent?(true-type, false-type))
    let (true-renaming, false-renaming) = 
      install-renamings(true-type, false-type, gen, if-c, temp-to-rename, css);
    if (true-renaming)
      css.pool[true-renaming] := true-type;
    end;
    if (false-renaming)
      css.pool[false-renaming] := false-type;
    end;
  end;
end;

define function retype-loop-merges
  (from :: <computation>, to :: <computation>, css , wa) => ();
  // I'm not convinced the current approach to loops is the best approach.  At the moment
  // the loop-merge node forces itself to be retyped.  So if it depends on a temporary that's
  // assigned in a branch of a conditional then we might get the following:
  //   1) loop-merge typed
  //   2) typing of assignment to temporary it depends on gets skipped because it is in a dead
  //      branch of a conditional
  //   3) loop-merge gets retyped 
  //   4) later the dead branch becomes live and the assignment typed.  However, this initial
  //      typing won't force the loop-merge to be retyped.  
  //  For now let's just look out for this case, but it's a pretty gross hack.  

  typist-walk-computations(
    method (c :: <computation>)
      let t = c.temporary;
      if (t & t.users)
        for (u in t.users) 
	  if (instance?(u, <loop-merge>)) schedule-for-retyping(wa, u, css) end
	end;
      end
    end,
    from, css, to);
end;

define method refine-initial-node-type
    (if-c :: <if>, css , wa) => ();
  let test-value = if-c.test;
  let gen = get-generator(test-value);
  let value = if (gen)
                gen.temporary;
	      else
                let test-gen = test-value.generator;
                if (test-gen)
                  //uck : hard code in knowledge about preceding primitive
                  test-gen.arguments[0];
                end
              end;
  let fn = gen & get-if-inferencer(gen);
  if (fn)
    do-if-refiner(fn, if-c, gen, css, wa);
  elseif (value)
    // gts,98may27: it looks like get-if-inferencer is only usefully defined 
    //      on <function-call>, so this else case is for non-function-call 
    //      generators
    let temp = select (value.generator by instance?)
                 <get-cell-value>, <set-cell-value!> => 
                    value.generator.computation-cell;
                 otherwise => value;
               end;
    let (true-type, false-type, temp-to-rename) = split-types(temp, gen, if-c, css);
    let old-branch = element(css.branch-taken, if-c, default: #f);
    let new-branch = set-branch(true-type, false-type, if-c, css);
    unless (old-branch == new-branch)
//      format-out("Upgrading if: old: %=, new: %=\n", old-branch, new-branch);
      if (old-branch == true:)
        type-computations(if-c.alternative, if-c.next-computation, css);
        retype-loop-merges(if-c.alternative, if-c.next-computation, css, wa);
      else
        type-computations(if-c.consequent, if-c.next-computation, css);
        retype-loop-merges(if-c.consequent, if-c.next-computation, css, wa);
      end;
      schedule-for-retyping(wa, if-c.next-computation, css)
    end;

    let true-renaming = get-true-renaming-at(temp-to-rename, css, if-c);
    if (true-type)
      if (true-renaming)
        update-renaming-type(true-type, true-renaming, if-c, css, wa);
      else
        install-renamings(true-type, false-type, gen, if-c, temp-to-rename, css);
        schedule-users(wa, temp-to-rename.users, css, except: list(if-c));
      end;
    elseif (true-renaming)
        delete-renamings(temp-to-rename, css, if-c);
        schedule-users(wa, temp-to-rename.users, css, except: list(if-c));
    end;
  end;
end;

define method get-generator(x :: <temporary>)
  ensure-real-generator(x.generator);
end;

/* gts -- <multiple-value-tempoary> is a subclass of <temporary>, 
          so this is unnecessary! */
define method get-generator(x :: <multiple-value-temporary>)
  ensure-real-generator(x.generator);
end;

define method get-generator(x :: <object-reference>)
  #f;
end;

define method get-generator(x)
  break("get-generator called on %s", x);
  #f;
end;

define method ensure-real-generator(c :: <extract-single-value>)
  get-generator(c.computation-value);
end;


define method ensure-real-generator(c :: <primitive-call>)
  if(c.primitive == dylan-value(#"primitive-boolean-as-raw"))
    get-generator(c.arguments[0]);
  else
    c;
  end;
end;

// gts, new:
// define method ensure-real-generator(c :: <computation>)
//   c;
// end;

/* gts -- replace following plethora with above default for <computation>
   - the default of #f seems fragile and is certainly undocumented */

define method ensure-real-generator(c :: <check-type>)
  c;
end;

define method ensure-real-generator(c :: <function-call>)
  c;
end;

define method ensure-real-generator(c :: <get-cell-value>)
  c;
end;

define method ensure-real-generator(c :: <set-cell-value!>)
  c;
end;

define method ensure-real-generator(c :: <if-merge>)
  c;
end;

define method ensure-real-generator(c)
  #f;
end;
  
define method split-types
  (value, gen, if-c, css :: <call-site-summary>)
  => (c :: false-or(<&type>), a :: false-or(<&type>));
  let value-type = lookup-type(value, css, if-c);
  let (true-type, false-type, temp) = get-consequent-type(value-type, if-c);
  if (true-type)
    values(true-type, false-type, temp);
  else
    values(true-type, false-type, value);
  end;
end;

define method split-types
  (value, gen :: <get-cell-value>, if-c, css :: <call-site-summary>)
  => (c :: false-or(<&type>), a :: false-or(<&type>));
//  let value-type = lookup-type(value, css, if-c);
  let value-type = lookup-type(gen.temporary, css, gen);
  let (true-type, false-type) = get-consequent-type(value-type, if-c);
  if (true-type)
    values(true-type, false-type, gen.computation-cell);
  else
    values(true-type, false-type, value);
  end;
end;

define method split-types
  (value, gen :: <set-cell-value!>, if-c, css :: <call-site-summary>)
  => (c :: false-or(<&type>), a :: false-or(<&type>));
//  let value-type = lookup-type(value, css, if-c);
  let value-type = lookup-type(gen.temporary, css, gen);
  let (true-type, false-type) = get-consequent-type(value-type, if-c);
  if (true-type)
    values(true-type, false-type, gen.computation-cell);
  else
    values(true-type, false-type, value);
  end;
end;

// gts,98may27 -- what're get-consequent-type, and its user, split-types, doing?
// return-sig: => (true-type, false-type, temp)
// invariant: true-type <= type (true-type is smaller-than-or-eq type)
// true-type is the type that the temp is guaranteed to have if execution 
//   proceeds to the true branch of the conditional.  The simple case is that 
//   if the temp is typed false-or(<foo>), we can be assured that within the 
//   true branch, the temp has type <foo>.  If the conditional is guaranteed to 
//   take the false branch, true-type will be typist-<bottom-type>.
// similarly for false-type.

define function get-consequent-type (type, if-c :: <if>)
  let gen = generator(if-c.test);
  // gts -- <if>'s used to take raw booleans which were set via call-by-reference 
  //        to some primitive.  This is no longer the case, so keep temp as the
  //        <if>'s temp.
  // TO DO: handle side-effected temp's cleanly throughout
  let temp = if-c.test;
  //gts, orig: let temp = if (instance?(gen, <call>)) gen.arguments[0] else gen.temporary end;

  // gts -- handle <boolean>!!!
  if (type == dylan-value(#"<boolean>"))
    format-out("yes!\n");
    values(typist-<true-type>(), typist-<false-type>(), temp);
  elseif (instance?(type, <&union>))
    let old-members = type.^union-members;
    let new-members = remove(old-members, typist-<false-type>(), test: ^type-equivalent?);
    let members-size = new-members.size;
    if (members-size = 1)
      values(new-members[0], typist-<false-type>(), temp);
    elseif (members-size < old-members.size)
      values(make(<&union>, members: new-members), typist-<false-type>(), temp);
    end;
  elseif (^subtype?(type, typist-<false-type>()))
    values(typist-<bottom-type>(), type, temp);
  elseif (^subtype?(type, typist-<true-type>()))
    values(type, typist-<bottom-type>(), temp);
  elseif (instance?(type, <&class>))
    values(type, typist-<false-type>(), temp);
  elseif (instance?(type, <&singleton>))
    let obj = type.^singleton-object;
    if (instance?(obj, <&raw-boolean>))
      if (obj.^raw-object-value)
        values(type, typist-<bottom-type>(), temp);
      else
        values(typist-<bottom-type>(), type, temp);
      end
    else
      values(type, typist-<bottom-type>(), temp);
    end;
  elseif (instance?(type, <values-type>))
    local method find-extractor (c)
            if (instance?(c, <extract-single-value>))
              c;
            elseif (instance?(c, <check-type>))
              find-extractor(c.computation-value.generator);
            end;
          end;
    let xsv = find-extractor(temp.generator);
    when (xsv)
      let i = xsv.index;
      let fixed-t = type.fixed-types;
      let x-type = if (i < fixed-t.size)
                     fixed-t[i];
                   elseif (type.values-rest-type)
                     // can't know what type it might be
                     cached-type-union(typist-<false-type>(), type.values-rest-type);
                   else
                     // there aren't any `rest values'
                     typist-<false-type>();
                   end;
      get-consequent-type(x-type, if-c);
    end;
  else
    values(#f, #f, #f);
  end;
end;

// TODO: need to fix up &type hierarchy. Raws should be &types!
define method maybe-introduce-if-renamings 
  (true-type , false-type , true-start, false-start, end-c,
   if-c :: <if>, temp, css :: <call-site-summary>)
  let true-renaming = make(<true-renaming>, value: temp, comp: if-c, css: css);
  let false-renaming = make(<false-renaming>, value: temp, comp: if-c, css: css);
  let renamings = #f;
  let used-in-true? = 
    record-renaming-in-computations(true-start, end-c, temp, true-renaming, css);
  let used-in-false? =
    record-renaming-in-computations(false-start, end-c, temp, false-renaming, css);
  let new = element(css.renamed-temporaries, temp, default: #f);
  when (used-in-true?)
    renamings := list(pair(temp, true-renaming));
    new := list(true-renaming);
  end;
  when (used-in-false?)
    renamings := if(renamings)
                   pair(pair(temp, false-renaming), renamings);
                 else
                   list(pair(temp, false-renaming));
                 end;
    new := if (new)
             pair(false-renaming, new);
           else
             list(false-renaming);
           end;
  end;
  when (renamings)
    css.renaming-computations[if-c] := renamings;
  end;
  when (new)
    css.renamed-temporaries[temp] := new;
  end;

//gts  if (used-in-true? | used-in-false?)
//    access(dfmc-back-end, print-method-out)(if-c.environment.lambda); 
//    break("installing renaming(s): %=, %=, %=, %=, %=, %=, %=, %=, %=",
//      used-in-true?, true-renaming, used-in-false?, false-renaming,
//      true-type, false-type, if-c, temp, css);
//  end if;

  values(used-in-true? & true-renaming, used-in-false? & false-renaming);
end;

define method maybe-introduce-if-renamings 
  (true-type == #f, false-type, true-start, false-start, end-c,
   if-c :: <if>, temp, css :: <call-site-summary>)
  let false-renaming = make(<false-renaming>, value: temp, comp: if-c, css: css);
  let found-use? = record-renaming-in-computations(false-start, end-c, temp, false-renaming, css);
  if (found-use?)
    css.renaming-computations[if-c] := list(pair(temp, false-renaming));
    let new = (element(css.renamed-temporaries, temp, default: #f) | make(<empty-list>));
    new := add(new, false-renaming);
    css.renamed-temporaries[temp] := new;
    values(#f, found-use? & false-renaming);
  else
    values(#f, #f);
  end;
end;

define method maybe-introduce-if-renamings 
  (true-type, false-type == #f, true-start, false-start, end-c,
   if-c :: <if>, temp, css :: <call-site-summary>)
  let true-renaming = make(<true-renaming>, value: temp, comp: if-c, css: css);
  let found-use? = record-renaming-in-computations(true-start, end-c, temp, true-renaming, css);
  if (found-use?)
    css.renaming-computations[if-c] := list(pair(temp, true-renaming));
    let new = (element(css.renamed-temporaries, temp, default: #f) | make(<empty-list>));
    new := add(new, true-renaming);
    css.renamed-temporaries[temp] := new;
    values(true-renaming, #f);
  else
    values(#f, #f);
  end;
end;

define method maybe-introduce-if-renamings 
  (true-type == #f, false-type == #f, true-start, false-start, end-c, if-c, temp, css)
  values(#f, #f);
end;

define constant $if-inferencers = make(<table>);

define constant $if-refiners = make(<table>);

define macro if-inferencer-definer
  { define if-inferencer ?:name ?etc:* end }
    => { define function ?name ## "-if-inferencer" ?etc end;
         do-define-if-typist(?#"name", ?name ## "-if-inferencer", $if-inferencers); }
end macro;


define function do-define-if-typist
  (s :: <symbol>, fn :: <function>, table :: <table>) 
  => ()
  /* TODO: figure out how to get this booted 
  with-library-context (lookup-library-description (#"dylan"))
    table[dylan-value(s)] := fn;
  end
  */
end;

define method get-function (c :: <computation>)
  c.computation-value;
end;

define method get-function (c :: <call>)
  #f
end;

define method get-function (c == #f)
  #f;
end;

// loose some precision here possibly. We`d need to pass the css to get it though
define method get-function (c :: <extract-single-value>)
  #f;
end;

define method get-function (c :: <merge>)
  #f;
end;

define method get-function (c :: <make-closure>)
  #f; // TODO: check if we can do any better here.
end;

// gts, new 
//define method get-if-inferencer (gen :: <computation>)
//  #f
// end method;

define method get-if-inferencer (gen :: <function-call>)
  let fn = gen.function;
  let fn = if (instance?(fn, <object-reference>))
             fn.reference-value
           else 
             get-function(fn.generator)
           end;
  let key = if (instance?(fn, <&code>))
              fn.function;
            else
              fn;
            end;
  // element($if-inferencers, key, default: #f);
  select(key)
    dylan-value(#"instance?")
      => instance?-if-inferencer; 
/* These are broken at the moment...
    dylan-value(#"=")
      => eq-if-inferencer;
    dylan-value(#"~")
      => not-if-inferencer;
*/
    otherwise
      => #f;
  end;
end;

define method get-if-inferencer (gen :: <primitive-call>)
  #f;
end;

define method get-if-inferencer (gen :: <check-type>)
  #f;
end;

define method get-if-inferencer (gen :: <get-cell-value>)
  #f;
end;

define method get-if-inferencer (gen :: <set-cell-value!>)
  #f;
end;

define method get-if-inferencer (gen :: <if-merge>)
  #f;
end;

define function do-if-inference (fn, if-c, gen, css) => ();
  let (true-type, false-type, temp) = fn(gen, if-c, css);
  set-branch(true-type, false-type, if-c, css);
  when (true-type)
    if (~false-type)
      install-renamings(true-type, false-type, gen, if-c, temp, css);
    else 
      unless (instance?(true-type, <&type>)
              & instance?(false-type, <&type>)
              & ^type-equivalent?(true-type, false-type))
        install-renamings(true-type, false-type, gen, if-c, temp, css);
      end;
    end;
  end;
end;

define inline function set-branch 
  (true, false, if-c, css :: <call-site-summary>)
  let true-bottom? = instance?(true, <&type>) & ^subtype?(true, typist-<bottom-type>());
  let false-bottom? = instance?(false, <&type>) & ^subtype?(false, typist-<bottom-type>());
  unless (true-bottom? & false-bottom?)
    css.branch-taken[if-c] := if (true & ~true-bottom? & false-bottom?)
                                true:;
                              elseif (true-bottom? & false & ~false-bottom?)
                                false:;
                              else
                                both:;
                              end;
  end;
end;

define method install-renamings 
  (true-type, false-type, gen, if-c, temp, css :: <call-site-summary>)
  let true = if-c.consequent;
  let false = if-c.alternative;
  let end-c = if-c.next-computation.next-computation;
  let (true-renaming, false-renaming) = 
    maybe-introduce-if-renamings
     (true-type, false-type, true, false, end-c, if-c, temp, css);
  if (true-renaming)
    css.pool[true-renaming] := true-type;
  end;
  if (false-renaming)
    css.pool[false-renaming] := false-type;
  end;
end;

define function end-box-if-renaming?
  (c :: <computation>, box-temp, end-c, css :: <call-site-summary>)
  => (result :: <boolean>)
  (c == end-c) // In fact with a little more work we could allow renamings in the
               // the <merge> sources, but ...
  | (instance?(c, <set-cell-value!>) & (box-temp == c.computation-cell))
  | (instance?(c, <function-call>) &
     begin
       let fn = lookup-type(c.function, css, c);
       when (instance?(fn, <&method>) & ~lambda-top-level?(fn)) 
         let env1 = fn.environment;
         block (return)
           for (user in box-temp.users)
             when (instance?(user, <set-cell-value!>))
               let env2 = user.environment;
               iterate loop (env = env2)
                 when (env)
                   if (env == env1)
                     return(#t);
                   else
                     loop(env.outer);
                   end;
                 end;
               end;
             end;
           end;
         end;
       end;
     end)
end;       

define method install-renamings 
  (true-type, false-type, gen :: <get-cell-value>, if-c, temp, css :: <call-site-summary>)
  install-renamings-box-if-internal
    (true-type, false-type, gen, if-c, temp, css);
end;

define method install-renamings 
  (true-type, false-type, gen :: <set-cell-value!>, if-c, temp, css :: <call-site-summary>)
  install-renamings-box-if-internal
    (true-type, false-type, gen, if-c, temp, css);
end;

define inline function install-renamings-box-if-internal
  (true-type, false-type, gen, if-c, temp, css :: <call-site-summary>)
  let true = if-c.consequent;
  let false = if-c.alternative;
  let end-c = if-c.next-computation;
  let end? = rcurry(end-box-if-renaming?, temp, end-c, css);
  let (true-renaming, false-renaming) = 
    maybe-introduce-if-renamings
     (true-type, false-type, true, false, end?, if-c, temp, css);
  if (true-renaming)
    css.pool[true-renaming] := true-type;
  end;
  if (false-renaming)
    css.pool[false-renaming] := false-type;
  end;
end;
             
define function do-if-refiner (fn, if-c, gen, css, wa) => ();
  let (true-type, false-type, temp) = fn(gen, if-c, css);
  let true-renaming = get-true-renaming-at(temp, css, if-c);
  let false-renaming = get-false-renaming-at(temp, css, if-c);
  if (true-type & false-type & (true-renaming | false-renaming))
    if (^type-equivalent?(true-type, false-type))
      delete-renamings(temp, css, if-c);
      schedule-users(wa, temp.users, css, except: list(if-c)); 
    else
      update-renaming-types(true-type, true-renaming, false-type, false-renaming, 
                            temp, if-c, css, wa);  
    end;
  elseif (~true-renaming & ~false-renaming & true-type & false-type &
          ~^type-equivalent?(true-type, false-type))
    install-renamings(true-type, false-type, gen, if-c, temp, css);
    schedule-users(wa, temp.users, css, except: list(if-c)); 
  elseif (~true-type & ~false-type & true-renaming & false-renaming)
    //delete renamings
    delete-renamings(temp, css, if-c);
    schedule-users(wa, temp.users, css, except: list(if-c)); 
  elseif ((true-type & true-renaming & ~false-type & ~false-renaming)
          |(~true-type & ~true-renaming & false-type & false-renaming))
      update-renaming-types(true-type, true-renaming, false-type, false-renaming, 
                            temp, if-c, css, wa);  
  end;
  let old-branch = element(css.branch-taken, if-c, default: #f);
  let new-branch = set-branch(true-type, false-type, if-c, css);
  unless (old-branch == new-branch)
//    format-out("Upgrading if: old: %=, new: %=\n", old-branch, new-branch);
    if (old-branch == true:)
      type-computations(if-c.alternative, if-c.next-computation, css);
    else
      type-computations(if-c.consequent, if-c.next-computation, css);
    end;
    schedule-for-retyping(wa, if-c.next-computation, css)
  end;
end;

define method update-renaming-types 
  (true-type, true-renaming, false-type, false-renaming, 
   temp, if-c, css :: <call-site-summary>, wa :: <work-agenda>)
  if (true-renaming)
    update-renaming-type(true-type, true-renaming, if-c, css, wa);
  end;
  if (false-renaming)
    update-renaming-type(false-type, false-renaming, if-c, css, wa);
  end;
end;

define function update-renaming-type
  (type, renaming, if-c, css :: <call-site-summary>, wa :: <work-agenda>)
  let old-type = css.pool[renaming];
  if (~^subtype?(type, old-type))
    css.pool[renaming] := maybe-make-inferred-union(if-c, type, old-type);
    schedule-users(wa, renaming.renamed-value.users, css, except: list(if-c));
  end;
end;  

//define if-inferencer \~ 
define function not-if-inferencer
  (not :: <function-call>, if-c :: <if>, css :: <call-site-summary>)
  let value = not.arguments[0];
  let gen = get-generator(value);
  let fn = gen & get-if-inferencer(gen);
// gts, suspect:
  let (true-type, false-type, temp) = 
    if (fn)
      fn(gen, if-c, css);
    else
      split-types(value, gen, if-c, css);
    end;
  if (true-type == false-type) // cheap !!
    values(#f, #f, #f);
  elseif (instance?(true-type, <&type>) & ^subtype?(true-type, typist-<false-type>())
          & instance?(false-type, <&type>) & ^subtype?(false-type, typist-<false-type>()))
    values(false-type, typist-<bottom-type>(), temp)
  elseif (instance?(true-type, <&type>) & ^subtype?(true-type, typist-<true-type>())
          & instance?(false-type, <&type>) & ^subtype?(false-type, typist-<true-type>()))
    values(typist-<bottom-type>(), true-type, temp)
  else   
    values(false-type, true-type, temp);
  end;
end;
  
define if-inferencer instance?
  (call :: <function-call>, if-c :: <if>, css :: <call-site-summary>)
  let class-arg = call.arguments[1];
  let c-type = lookup-type(class-arg, css, call);
  let arg0-type = lookup-type(call.arguments[0], css, call);
  let class-type = if (instance?(c-type, <&singleton>) & 
                        instance?(c-type.^singleton-object, <&class>))
                     c-type.^singleton-object;
                   else
                     c-type;
                   end;
  if (c-type == class-type & ~instance?(c-type, <&union>))
    values(#f, #f, call.arguments[0]);
  elseif (^subtype?(arg0-type, class-type))
    values(arg0-type, typist-<bottom-type>(), call.arguments[0]);
  elseif (^subtype?(class-type, arg0-type))
    values(class-type, arg0-type, call.arguments[0]);
  elseif (guaranteed-disjoint?(arg0-type, class-type))
    values(typist-<bottom-type>(), arg0-type, call.arguments[0])
  else
    values(#f, #f, #f);
  end;
end;

define if-inferencer eq
  (call :: <function-call>, if-c :: <if>, css :: <call-site-summary>)
  let arg0 = call.arguments[0];
  let arg1 = call.arguments[1];
  let arg0-type = lookup-type(arg0, css, call);
  let arg1-type = lookup-type(arg1, css, call);
  if (^type-equivalent?(arg0-type, arg1-type))
    values(#f, #f, #f);
  elseif (instance?(arg0-type, <&singleton>) & instance?(arg1-type, <&singleton>))
    values(#f, #f, #f);
  elseif (instance?(arg0-type, <&singleton>) & 
           eq-sealed-domain?(arg0-type.^singleton-object.^object-class))
    values(arg0-type, #f, arg0);
  elseif (instance?(arg1-type, <&singleton>) &
           eq-sealed-domain?(arg1-type.^singleton-object.^object-class))
    values(arg1-type, #f, arg1);
  elseif (^subtype?(arg0-type,typist-<bottom-type>()) 
          | ^subtype?(arg1-type, typist-<bottom-type>()))
    values(typist-<bottom-type>(), #f, arg0);
  elseif (eq-sealed-domain?(arg0-type) & ^subtype?(arg0-type, arg1-type))
    values(arg0-type, #f, arg0);
  elseif (eq-sealed-domain?(arg0-type) & ^subtype?(arg1-type, arg0-type))
    values(arg1-type, #f, arg1);
  else
    values(#f, #f, #f);
  end;
end;

/*
define macro singleton-eq-if-inference-definer
  { define singleton-eq-if-inference end } 
   => { define method do-singleton-eq-if-inference
	  (type, object, temp)
	  values(#f, #f, #f);
	end; }
        
  { define singleton-eq-if-inference 
      ?object:name :: ?object-class:name ;
      ?more:* 
    end } 
   => { define method do-singleton-eq-if-inference
	  (type, ?object :: ?object-class, temp)
	  values(type, #f, temp);
	end;
        define singleton-eq-if-inference 
          ?more
        end } 
end;

// \= is sealed for these types so we know what we can infer when given a singleton 
// containing an instance of one of them.
define singleton-eq-if-inference 
  sym  :: <symbol>;
  ch   :: <character>;
  bool :: <boolean>;
  n    :: <number>;
  list :: <list>;
  r    :: <range>;
end;

*/

define variable $eq-sealed-domains = #f;

define function make-eq-sealed-domains()
//  with-library-context (lookup-library-description (#"dylan"))
    vector(dylan-value(#"<symbol>"),
           dylan-value(#"<character>"),
           dylan-value(#"<boolean>"),
           dylan-value(#"<number>"),
           dylan-value(#"<integer>"),
           dylan-value(#"<list>"));
//  end;
end;

define function eq-sealed-domain? (x)
  local method in-domain? (domain)
          ^subtype?(x, domain.^domain-types[0])
          //TODO: this seems dubious
          | ^subtype?(x, domain.^domain-types[1]);
        end;
  any?(in-domain?, ^generic-function-domains(dylan-value(#"=")));
end;

/*
define function set-up-instance?-methods-for-if-inference ()
  with-library-context (lookup-library-description (#"dylan"))
    for (m in  dylan-value(#"instance?").%generic-function-methods-known)
       $if-inferencers[m] := instance?-if-inferencer;
    end;
  end;
end;
*/
define function set-up-instance?-methods-for-if-inference ()
  for (m in  dylan-value(#"instance?").^generic-function-methods-known)
     $if-inferencers[m] := instance?-if-inferencer;
  end;
end;

//set-up-instance?-methods-for-if-inference();
/*
define function set-up-eq-methods-for-if-inference ()
  with-library-context (lookup-library-description (#"dylan"))
    unless ($eq-sealed-domains)
      $eq-sealed-domains := make-eq-sealed-domains();
    end;
    for (m in  dylan-value(#"=").^generic-function-methods-known)
      if (any?(curry(^subtype?, m.^function-signature.^signature-required[0]), 
               $eq-sealed-domains))
         $if-inferencers[m] := eq-if-inferencer;
      end;
    end;
  end;
end;
*/
define function set-up-eq-methods-for-if-inference ()
  for (m in  dylan-value(#"=").^generic-function-methods)
    if (eq-sealed-domain?(m.^function-signature.^signature-required[0]))
       $if-inferencers[m] := eq-if-inferencer;
    end;
  end;
end;

//set-up-eq-methods-for-if-inference();
