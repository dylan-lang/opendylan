Module: dfmc-flow-graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////////////////////////////////////////////////////////////////////////
///
/// Classes to represent thing the typist must do and when to do them
/// and also functions to manipulte them.
///
///////////////////////////////////////////////////////////////////////


/// Records a computation which needs to be typed and a context
/// i.e. a call site summary in which to do it. These are used
/// to schedule work. 
/// They are queueable and indicate whether they are in a queue (they can 
/// only be in one queue at a time.

define primary abstract class <typist-work-item> (<object>)
  slot node :: <computation>,
    required-init-keyword: comp:;
  slot call-site-summary :: <call-site-summary>,
    required-init-keyword: summary:;
  slot next-item :: false-or(<typist-work-item>) = #f,
    init-keyword: next:;
end;

define method initialize(twi :: <typist-work-item>, #key)
  next-method();
  if (twi.call-site-summary.compressed?)
//    break("recon")
  end
end;

define function work-item-= 
  (i1 :: <typist-work-item>, i2 :: <typist-work-item>)
  => (result :: <boolean>)
  (i1.node == i2.node) & 
  (i1.call-site-summary == i2.call-site-summary)
end;

define class <work-item-queue> (<object>)
  slot queue-front :: false-or(<typist-work-item>) = #f,
    init-keyword: front:;
  slot queue-back :: false-or(<typist-work-item>) = #f,
    init-keyword: back:;
end;

define function queue-empty? (q :: <work-item-queue>)
  ~q.queue-front;
end;

define class <work-agenda> (<object>)
  slot initial-work :: <work-item-queue> = make(<work-item-queue>),
    init-keyword: initial-work:;
  slot current-re-type-work :: <work-item-queue> = make(<work-item-queue>);
  slot re-type-work :: <work-item-queue> = make(<work-item-queue>);
  slot %resourced-work-items :: false-or(<work-item>) = #f;
  slot used-call-site-summaries :: <table> = make(<table>);
end;

define function resourced-work-item (agenda :: <work-agenda>)
  => (work-item :: false-or(<work-item>));
  let item = %resourced-work-items(agenda);
  if (item)
    %resourced-work-items(agenda) := item.next-item;
  end;
  item;
end;

define function resourced-work-item-setter (work-item :: <work-item>, agenda :: <work-agenda>)
  => ();
  work-item.next-item := %resourced-work-items(agenda);
  %resourced-work-items(agenda) := work-item;
end;

/// These only ever appear in an agenda (i.e aren`t used to record dependencies). They can be 
/// resourced by the agenda.
define class <work-item> (<typist-work-item>) end;

define sealed method make (c == <work-item>, 
                           #key comp :: <computation>, 
                                summary :: <call-site-summary>,
                                agenda  :: false-or(<work-agenda>),
                                next :: false-or(<typist-work-item>) = #f)
  => (work-item :: <work-item>);
  /* if (comp.environment.lambda ~= summary.css-lambda)
    error("Invalid work item: %s & %s", comp, summary);
  else */
  if (agenda)
    let item :: false-or(<work-item>) = resourced-work-item(agenda);
    if (item)
      item.node := comp;
      item.call-site-summary := summary;
      item.next-item := next;
      item;
    else
      next-method();
    end;
  else
    next-method();
  end;
end;

define method schedule-for-initial-typing
  (agenda :: <work-agenda>, comp :: <computation>, css :: <call-site-summary>)
  => ();
  let where = agenda.initial-work;
  let first = where.queue-front;
  if (first)
    block (exit)
      iterate repeat (item :: <work-item> = first)
        if (comp == item.node & css == item.call-site-summary)
          exit();
        elseif (item.next-item)
          repeat(item.next-item);
        else
          let new-item = make(<work-item>, comp: comp,
                                                 summary: css);
          item.next-item := (where.queue-back := new-item);
        end;
      end;
    end;
  else
    let new-item = make(<work-item>, comp: comp,
                                     summary: css);
    where.queue-front := (where.queue-back := new-item);
  end;
end;

/// In this case we don`t check to see whether an equivalent work item is in the agenda because
/// This almost certainly costs more than the redundant processing of the node.
define method schedule-for-retyping
  (agenda :: <work-agenda>, comp :: <computation>, css :: <call-site-summary>)
  => ();
  let item = make(<work-item>, comp: comp, 
                               summary: css,
                               agenda: agenda);
  if (agenda.current-re-type-work.queue-back)
    agenda.current-re-type-work.queue-back.next-item := item;
    agenda.current-re-type-work.queue-back := item;
  else
    agenda.current-re-type-work.queue-front := (agenda.current-re-type-work.queue-back := item);
  end;
end;



/// These implicitly record NON-LOCAL dependencies 
/// between computations (e.g. actual call sites depend on the return
/// computation of the function called) or computations and 'bindings'
/// with indefinite extent (e.g. module variables and 'boxes' (better
/// known as cells - but we wont go into that again).

define class <dependency-record> (<typist-work-item>) 
  slot home-queue :: <work-item-queue>,
    required-init-keyword: home:;
end;

define class <module-binding-dependency-record> (<object>) 
  slot node :: <computation>,
    required-init-keyword: comp:;
  slot call-site-summary :: <call-site-summary>,
    required-init-keyword: summary:;
end;

define constant <dependency-records> = <work-item-queue>;

define class <module-binding-dependency-records> (<table>)
end class;

define sealed method make (c == <dependency-record>, 
                           #key comp :: <computation>, 
                                summary :: <call-site-summary>,
                                home :: false-or(<work-item-queue>),
                                next :: false-or(<typist-work-item>) = #f)
  => (work-item :: <work-item>);
//  if (comp.environment.lambda ~= summary.css-lambda)
//    format-out("Invalid dependency record?: %s & %s", 
//               comp.environment.lambda, summary);
//    break("inv");
//  end;
  next-method();
end;

define sideways method make-dependency-records() => (dependency-records)
  make(<dependency-records>);
end;

/// I don't actually believe we need to check to the presence of this dependency already
/// because the dependencies are set up during initialize-node-type which only visits each node
/// once. But for safety`s sake we`ll do a version which checks. We can try a non-checking 
/// version later.


define function record-dependency-new (dependee, comp, summary :: <call-site-summary>)
  => ();
  let (dependee-compilation-record-library, record?) = 
    if (instance?(dependee, <call-site-summary>))
      values(
        dependee.css-lambda.model-creator.form-compilation-record.compilation-record-library,
        ~dependee.compressed?)
    else
      values(
        dependee.model-creator.form-compilation-record.compilation-record-library,
        #t)
    end;
  let summary-creator = summary.model-creator;
  let summary-cr = 
    if (instance?(summary-creator, <compilation-record>))
      summary-creator
    else
      summary-creator.form-compilation-record
    end;
  let summary-compilation-record-library = summary-cr.compilation-record-library;

  if (dependee-compilation-record-library == summary-compilation-record-library & record?)
    summary.introduced-dependencies := 
      add-new!(summary.introduced-dependencies, dependee);
    let where :: <work-item-queue> = dependee.type-dependencies;
    let first :: false-or(<dependency-record>) = where.queue-front;
    if (first)
      block (exit)
        iterate repeat (item :: <dependency-record> = first)
          if (comp == item.node & summary == item.call-site-summary)
            exit();
          elseif (item.next-item)
            repeat(item.next-item);
          else
            let new-dependency = make(<dependency-record>, comp: comp,
                                                           summary: summary,
                                                           home: where);
            item.next-item := (where.queue-back := new-dependency);
          end;
        end;
      end;
    else
      let new-dependency = make(<dependency-record>, comp: comp,
                                                     summary: summary,
                                                     home: where);
      where.queue-front := (where.queue-back := new-dependency);
    end;
  end;
end;


define method record-dependency (dependee, comp, summary :: <call-site-summary>)
  => ();
  let (dependee-compilation-record-library, record?) = 
    if (instance?(dependee, <call-site-summary>))
      values(
        dependee.css-lambda.model-creator.form-compilation-record.compilation-record-library,
        ~dependee.compressed?)
    else
      values(
        dependee.model-creator.form-compilation-record.compilation-record-library,
        #t)
    end;
  let summary-creator = summary.model-creator;
  let summary-cr = 
    if (instance?(summary-creator, <compilation-record>))
      summary-creator
    else
      summary-creator.form-compilation-record
    end;
  let summary-compilation-record-library = summary-cr.compilation-record-library;

  if (dependee-compilation-record-library == summary-compilation-record-library & record?)
    summary.introduced-dependencies := 
      add-new!(summary.introduced-dependencies, dependee);
    let where :: <work-item-queue> = dependee.type-dependencies;
    let new-dependency = make(<dependency-record>, comp: comp, summary: summary, home: where);

    if (where.queue-front)
      where.queue-back := (where.queue-back.next-item := new-dependency);  
    else
      where.queue-front := (where.queue-back := new-dependency);
    end;
  end;
end;

define method record-dependency (dependee :: <module-binding>, comp, summary :: <call-site-summary>)
  => ();
  let dependee-compilation-record-library = 
    dependee.model-creator.form-compilation-record.compilation-record-library;
  let summary-compilation-record-library = 
    summary.model-creator.form-compilation-record.compilation-record-library;

  if (dependee-compilation-record-library == summary-compilation-record-library)
    summary.introduced-dependencies := add-new!(summary.introduced-dependencies, dependee);
    let table = dependee.type-dependencies;
    let l :: <list> = element(table, summary, default: #());
    if (empty?(l))
      table[summary] := list(comp)
    else
      unless (member?(comp, l))
        l.tail := pair(comp, l.tail);
      end
    end
  end
end;

define function remove-introduced-dependencies (l :: <&lambda>)
  for-all-summaries (css in l.call-site-summaries)
    for (dependee in css.introduced-dependencies)
      unless (instance?(dependee, <call-site-summary>) & dependee.compressed?)
        remove-dependency(dependee, css);
      end;
    end;
  end;
end;

define method remove-dependency (dependee, dependent :: <call-site-summary>)
  let from :: <work-item-queue> = dependee.type-dependencies;
  let first :: false-or(<dependency-record>) = from.queue-front;
  when (first)
    let new-first = #f;
    iterate repeat (prev :: <dependency-record> = first) 
      unless (new-first | prev.call-site-summary == dependent)
        new-first := prev;
      end;
      let current = prev.next-item;
      when (current)
        when (current.call-site-summary == dependent)
          prev.next-item := current.next-item;
        end;
        repeat(current);
      end;
    end;
    from.queue-front := new-first;
    unless (new-first)
      from.queue-back := #f;
    end;
  end;
end;

define method remove-dependency (dependee :: <module-binding>, dependent :: <call-site-summary>)
  let table = dependee.type-dependencies;
  remove-key!(table, dependent);
end;

/*
define function schedule-dependents (agenda :: <work-agenda>, deps :: <work-item-queue>)
  => ();
  let first-dep :: false-or(<dependency-record>) = deps.queue-front;
  when (first-dep)
    if (agenda.current-re-type-work.queue-back)
      agenda.current-re-type-work.queue-back.next-item := first-dep;
     else
      agenda.current-re-type-work.queue-front := first-dep;
    end;
   agenda.current-re-type-work.queue-back := deps.queue-back;
   deps.queue-front := (deps.queue-back := #f);
  end;
end;
*/


define method schedule-dependents (agenda :: <work-agenda>, dependents :: <work-item-queue>)
  => ();
  iterate loop (item :: false-or(<typist-work-item>) = dependents.queue-front)
    when (item)
      schedule-for-retyping(agenda, item.node, item.call-site-summary);
      loop(item.next-item);
    end;
  end;
end;


define method schedule-filtered-dependents (agenda :: <work-agenda>, dependents :: <work-item-queue>, css :: <call-site-summary>)
  => ();
  let lambda = css.css-lambda;
  iterate loop (item :: false-or(<typist-work-item>) = dependents.queue-front)
    when (item)
      let item-css = item.call-site-summary;
      if (item-css == css | item-css.css-lambda ~== lambda)
        schedule-for-retyping(agenda, item.node, item-css);
      end;
      loop(item.next-item);
    end;
  end;
end;


define method schedule-dependents (agenda :: <work-agenda>, dependents :: <table>)
  => ();
  for (l keyed-by summary in dependents)
    for (node in l)
      schedule-for-retyping(agenda, node, summary);
    end
  end;
end;      


define method schedule-filtered-dependents (agenda :: <work-agenda>, dependents :: <table>, css :: <call-site-summary>)
  => ();
  let lambda = css.css-lambda;
  for (l keyed-by summary in dependents)
    if (summary = css | summary.css-lambda ~== lambda)
      for (node in l)
        schedule-for-retyping(agenda, node, summary);
      end
    end
  end;
end;

// gts,98may27 added except: keyword
define function schedule-users
  (agenda :: <work-agenda>, nodes :: <list>, summary, #key except :: <list> = #f)
  for (node in nodes)
    if (except & ~ member?(node, except))
      if (node.environment.lambda == summary.css-lambda)
        schedule-for-retyping(agenda, node, summary);
      else
        for-all-summaries (summary in node.environment.lambda.call-site-summaries)
          schedule-for-retyping(agenda, node, summary);
        end;
      end if;
    end if;
  end for;
end;


define method push-current-work (w-a :: <work-agenda>)
  let crtw = w-a.current-re-type-work;
  let b = crtw.queue-back;
  when (b)
    let rtw = w-a.re-type-work;
    b.next-item := rtw.queue-front;
    unless (rtw.queue-back)
      rtw.queue-back := b;
    end;
    rtw.queue-front := crtw.queue-front;
    crtw.queue-front := (crtw.queue-back := #f) ;
  end; 
end;

define function pop-work-item (agenda :: <work-agenda>, queue :: <work-item-queue>)
  => (item :: false-or(<typist-work-item>));
  let item = queue.queue-front;
  if (item)
    let new-front = item.next-item;
    queue.queue-front := new-front;    
    unless (new-front)
      queue.queue-back := new-front;
    end;
    maybe-save-item(agenda, item);
  end;
  item;
end;

define method maybe-save-item (agenda :: <work-agenda>, item :: <work-item>)
  => ()
  resourced-work-item(agenda) := item;
end;

define method maybe-save-item (agenda :: <work-agenda>, rec :: <dependency-record>)
  => ()
  rec.next-item := #f;
  let where =   rec.home-queue;
  let first = where.queue-front;
  if (first)
    block (exit)
      iterate repeat (item :: <dependency-record> = first)
        if (rec.node == item.node & rec.call-site-summary == item.call-site-summary)
          exit();
        elseif (item.next-item)
          repeat(item.next-item);
        else
          item.next-item := (where.queue-back := rec);
        end;
      end;
    end;
  else
    where.queue-front := (where.queue-back := rec);
  end;
end;

