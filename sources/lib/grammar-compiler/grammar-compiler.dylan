Module: grammar-compiler
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function find (obj, list :: <list>,
			     #key test = \==, key = identity)
  // any?(method (x) test(obj, key(x)) & x end, seq)
  iterate loop (list :: <list> = list)
    if (empty?(list))
      #f
    elseif (test(obj, key(list.head)))
      list.head
    else
      loop(list.tail)
    end;
  end;
end;

define function assoc (item, list :: <list>) => (a :: false-or(<pair>))
  find(item, list, key: head)
end;

define constant $state-item-shift = 14;
define constant $max-item-index = ash(1, $state-item-shift) - 1;

define constant <index> = limited(<integer>, min: 0, max: $max-item-index);

define constant <state> = limited(<integer>, min: 0);

define constant <item-set> = limited(<list>, of: <item>);

define abstract class <sealed-constructor> (<object>) end;
define sealed domain make(subclass(<sealed-constructor>));
define sealed domain initialize(<sealed-constructor>);


define class <rule> (<sealed-constructor>)
  constant slot rule-index :: <index>, required-init-keyword: index:;
  constant slot rule-lhs :: <non-terminal>, required-init-keyword: lhs:;
  constant slot rule-user-rule :: <sequence>, required-init-keyword: user-rule:;
  slot rule-item :: <item>;
end;

define class <term> (<sealed-constructor>)
  constant slot term-token, required-init-keyword: token:;
  slot term-index :: <index>, required-init-keyword: index:;
  slot term-first-set :: false-or(<list>) = #f;
end;

define class <terminal> (<term>) end;

define method initialize (t :: <terminal>, #rest keys, #key)
  t.term-first-set := list(t);
end;

define class <non-terminal> (<term>)
  slot nont-rules :: <list> = #();
  slot nont-derivs :: <list> = #();
  slot nont-epsilon-item :: false-or(<item>) = #f;
  slot nont-error-rule-index :: false-or(<index>) = #f;
  // TODO: this doesn't seem different from term-first-set.
  slot nont-deriv-first-set :: <list> = #();
end;

// Note that this will be distinct from any user token of the same name.
// We could make it non-distinct by interning it during setup-symbols...
define constant $eoi-term = make(<terminal>,
				 token: #"eoi", index: $max-item-index);

define inline function term-index-code (index :: <index>)
  if (index == $max-item-index)
    #"eoi"
  else
    index
  end;
end;

define inline function term-code (term :: <term>)
  term-index-code(term.term-index)
end;

define abstract class <item> (<sealed-constructor>)
  constant slot item-index :: <index>, required-init-keyword: index:;
  constant slot item-rule :: <rule>, required-init-keyword: rule:;
  slot item-lr1-closure :: <list> = #();
end;

define class <item-set-table> (<table>, <sealed-constructor>) end;

define sealed method table-protocol 
    (table :: <item-set-table>) => (test :: <function>, hash :: <function>)
  values(\=, item-set-hash)
end method table-protocol;

define function item-set-hash (items :: <list>, hash-state)
 => (id :: <integer>, hash-state)
  for (item :: <item> in items,
       id = 23 then merge-hash-ids(id, item.item-index, ordered: #t))
  finally values(id, hash-state)
  end for;
end;

define class <empty-item> (<item>) end;

define inline sealed method empty? (x :: <item>) => (empty? :: <boolean>)
  ~instance?(x, <non-empty-item>);
end;

// TODO: really want <terminal-left-item> and <non-terminal-left-item>..
define class <non-empty-item> (<item>)
  constant slot item-first-term :: <term>, required-init-keyword: first-term:;
  slot item-next-item :: <item>, required-init-keyword: next-item:;
  slot item-first-set :: false-or(<list>) = #f;
end;

define inline method item-non-terminal-left? (item :: <item>)
  ~empty?(item) & instance?(item.item-first-term, <non-terminal>);
end;

define inline method item-terminal-left? (item :: <item>)
  ~empty?(item) & instance?(item.item-first-term, <terminal>);
end;


define /* EXPORTED */ class <grammar> (<sealed-constructor>)
  constant slot grammar-rules :: <simple-object-vector>,
    required-init-keyword: rules:;
  constant slot grammar-error-rules :: <simple-object-vector>,
    required-init-keyword: error-rules:;
  constant slot grammar-terminals :: <simple-object-vector>,
    required-init-keyword: terminals:;
  constant slot grammar-goto-table :: <simple-object-vector>,
    required-init-keyword: goto-table:;
  constant slot grammar-action-table :: <simple-object-vector>,
    required-init-keyword: action-table:;
  constant slot grammar-rule-reduction-table :: <simple-object-vector>,
    required-init-keyword: rule-reduction-table:;
end;

define class <gvars> (<sealed-constructor>)
  // The rules in order given.
  constant slot %grammar-user-rules :: <simple-object-vector>,
    required-init-keyword: user-rules:;
  constant slot %grammar-user-error-rules :: <simple-object-vector>,
    required-init-keyword: user-error-rules:;
  // All terminal tokens in the grammar.
  slot %grammar-terminal-tokens :: <simple-object-vector> = #[];
  // Vector of <rules>, same order as user rules
  slot %grammar-rules :: <simple-object-vector> = #[];
  // Vector of <non-terminal>'s.
 slot %grammar-non-terminals :: <simple-object-vector> = #[];
  // Vector of <item>'s
  slot %grammar-items :: <simple-object-vector> = #[];
  // state => item sets
  constant slot %grammar-item-sets :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  // state => error-action
  constant slot %grammar-error-action-table :: <object-table> = make(<object-table>);
  // state, item -> terminals
  constant slot %grammar-lookaheads :: <object-table> = make(<object-table>);
  // state, term -> next-state
  constant slot %grammar-gotos :: <object-table> = make(<object-table>);
  // state, term -> action
  constant slot %grammar-actions :: <object-table> = make(<object-table>);
  // lookahead propagation queue.
  slot %grammar-propagation-queue = #f;
end;

define inline function %grammar-initial-item (gv :: <gvars>) => (item :: <item>)
  gv.%grammar-items[0]
end;

/*
define inline function %grammar-number-items (gv :: <gvars>) => (n :: <integer>)
  gv.%grammar-items.size
end;
*/

define inline function %grammar-number-states (gv :: <gvars>) => (n :: <integer>)
  gv.%grammar-item-sets.size
end;

define inline method state-key (state :: <state>, item :: <item>)
 => (key :: <integer>)
  ash(state, $state-item-shift) + item.item-index
end;

define inline method state-key (state :: <state>, term :: <term>)
 => (key :: <integer>)
  ash(state, $state-item-shift) + term.term-index
end;

define inline function key-state (key :: <integer>) => (state :: <state>)
  ash(key, - $state-item-shift)
end;

define inline function key-value (key :: <integer>) => (index :: <index>)
  logand(key, $max-item-index)
end;

define inline function state-item-lookaheads
    (gv :: <gvars>, state :: <state>, item :: <item>) => (lookaheads :: <list>)
  gv.%grammar-lookaheads[state-key(state, item)]
end;

define inline function state-item-lookaheads-setter
    (lookaheads :: <list>, gv :: <gvars>, state :: <state>, item :: <item>)
  gv.%grammar-lookaheads[state-key(state, item)] := lookaheads
end;

define inline function state-term-goto-state
    (gv :: <gvars>, state :: <state>, term :: <term>) => (state :: <state>)
  gv.%grammar-gotos[state-key(state, term)]
end;


define inline function state-item-set (gv :: <gvars>, state :: <state>)
 => (items :: <item-set>)
  gv.%grammar-item-sets[state];
end;

define inline function item-start-item (item :: <item>)
 => (start-item :: <item>)
  item.item-rule.rule-item
end;

define inline function item-rule-index (item :: <item>)
 => (rule-index :: <index>)
  item.item-rule.rule-index
end;


define /* EXPORTED */ function compile-grammar-rules
    (rules :: <sequence>, #key error-rules :: <sequence> = #[])
 => (grammar :: <grammar>)
  let gv = make(<gvars>,
		user-rules: as(<simple-object-vector>, rules),
		user-error-rules: as(<simple-object-vector>, error-rules));
  setup-symbols(gv);
  setup-first-sets(gv);
  setup-derivs(gv);
  create-parse-table(gv);
  define-actions(gv);
  make-compiled-grammar(gv)
end;

define function make-compiled-grammar (gv :: <gvars>)
  make(<grammar>,
       rules: gv.%grammar-user-rules,
       error-rules: gv.%grammar-user-error-rules,
       terminals: gv.%grammar-terminal-tokens,
       rule-reduction-table: map(method (rule :: <rule>) rule.rule-lhs.term-code end,
				 gv.%grammar-rules),
       goto-table: externalize-goto-table(gv),
       action-table: externalize-action-table(gv))
end;

define constant $all-same-code = 65535;

define function externalize-action-table (gv :: <gvars>) => (v :: <simple-object-vector>)
  let nstates =  gv.%grammar-number-states;
  let v = make(<simple-object-vector>, size: nstates, fill: #());
  for (entry :: <action-entry> keyed-by state-term-key in gv.%grammar-actions)
    let state :: <state> = key-state(state-term-key);
    let term-code = term-index-code(key-value(state-term-key));
    v[state] := pair(pair(term-code,
			  if (entry.aentry-shift?)
			    lognot(gv.%grammar-gotos[state-term-key])
			  else
			    let rule-index = entry.aentry-item.item-rule-index;
			    if (zero?(rule-index) & term-code == #"eoi")
			      #"accept"
			    else
			      rule-index
			    end
			  end),
		     v[state]);
  end;
  for (state from 0 below nstates)
    let actions :: <list> = v[state];
    let err = element(gv.%grammar-error-action-table, state, default: #f);
    when (err)
      actions := concatenate!(actions, list(pair(#"error", err)));
    end;
    let r = ~empty?(actions) & begin
				 let r = actions.first.tail;
				 instance?(r, <integer>) & r >= 0
				  & every?(method (p) p.tail == r end, actions)
				  & r
			       end;
    when (r)
      actions := concatenate!(list(pair($all-same-code, r)), actions);
    end;
    let plist = make(<simple-object-vector>, size: 2 * actions.size);
    for (index from 0 below plist.size by 2, pair ::<pair> in actions)
      plist[index] := pair.head;
      plist[index + 1] := pair.tail;
    end for;
    v[state] := plist;
  end for;
  v
end;

define function externalize-goto-table (gv :: <gvars>) => (v :: <simple-object-vector>)
  let nstates =  gv.%grammar-number-states;
  let v = make(<simple-object-vector>, size: nstates, fill: #());
  for (goto keyed-by state-term-index in gv.%grammar-gotos)
    let state :: <state> = key-state(state-term-index);
    let term-index = key-value(state-term-index);
    v[state] := pair(goto, pair(term-index-code(term-index), v[state]));
  end;
  for (state from 0 below nstates)
    v[state] := as(<simple-object-vector>, reverse!(v[state]));
  end;
  v
end;

define function setup-symbols (gv :: <gvars>)
  let user-rules = gv.%grammar-user-rules;
  let nrules = user-rules.size;
  let nitems = reduce(method (n, r) n + r.second.size end, nrules, user-rules);
  assert(nitems <= $max-item-index, "Too many productions in grammar");

  let terms = make(<object-table>);
  let nont-seq = make(<stretchy-vector>);
  for (urule in user-rules)
    let tkn = urule.first;
    element(terms, tkn, default: #f) |
      add!(nont-seq, terms[tkn] := make(<non-terminal>, token: tkn, index: 0));
  end;
  gv.%grammar-non-terminals := as(<simple-object-vector>, nont-seq);
  let error-rules = gv.%grammar-user-error-rules;
  for (index from 0 below error-rules.size)
    let tkn = error-rules[index].first;
    let nt = element(terms, tkn, default: #f);
    assert(nt, "Error rule for terminal %s", tkn);
    assert(~nt.nont-error-rule-index, "Multiple error rules for %s", tkn);
    nt.nont-error-rule-index := index;
  end;

  let tkn-seq = make(<stretchy-vector>);
  local method as-term (tkn)
	  element(terms, tkn, default: #f)
	    | begin
		add!(tkn-seq, tkn);
		terms[tkn] := make(<terminal>,
				   token: tkn,
				   index: tkn-seq.size - 1)
	      end;
	end;
  let rules = make(<simple-object-vector>, size: nrules);
  let items = make(<simple-object-vector>, size: nitems);
  let item-index :: <integer> = 0;
  for (rule-index from 0 below nrules)
    let urule = user-rules[rule-index];
    let nt :: <non-terminal> = terms[urule.first];
    let seq = urule.second;
    let nseq :: <integer> = seq.size;
    let rule = make(<rule>, user-rule: urule, index: rule-index, lhs: nt);
    nt.nont-rules := add(nt.nont-rules, rule);

    rules[rule-index] := rule;

    let empty = make(<empty-item>, rule: rule, index: item-index + nseq);
    if (nseq == 0)
      assert(~nt.nont-epsilon-item,
	     "Multiple empty rules for %s", nt.term-token);
      nt.nont-epsilon-item := empty;
      rule.rule-item := empty;
    else
      let previous :: <item> = empty;
      for (tkn in seq)
	let new = make(<non-empty-item>, rule: rule, index: item-index,
		       first-term: as-term(tkn), next-item: empty);
	items[item-index] := new;
	item-index := item-index + 1;
	if (previous == empty)
	  rule.rule-item := new;
	else
	  previous.item-next-item := new;
	end;
	previous := new;
      end for;
    end;
    items[item-index] := empty;
    item-index := item-index + 1;
  end;
  gv.%grammar-items := items;
  gv.%grammar-rules := rules;
  gv.%grammar-terminal-tokens := as(<simple-object-vector>, tkn-seq);
  for (index from tkn-seq.size by 1, nt in gv.%grammar-non-terminals)
    nt.term-index := index;
  end;
end;

define function setup-first-sets (gv :: <gvars>)
  for (nt :: <non-terminal> in gv.%grammar-non-terminals)
    unless (nt.term-first-set)
      nt.term-first-set := compute-first-set(gv, nt, #());
    end;
  end;
end;

define function compute-first-set (gv :: <gvars>,
				   nt :: <non-terminal>,
				   visited :: <list>)
 => (s :: <list>)
  let set :: <list> = #();
  let complete? = #t;
  local method do-item (item :: <item>)
	  if (empty?(item))
	    set := add-new(set, #f)
	  elseif (~member?(item, visited))
	    let t :: <term> = item.item-first-term;
	    let initial-first-set
	      = t.term-first-set
	           | compute-first-set(gv, t, add(visited, item));
	    if (~t.term-first-set) complete? := #f end;
	    for (t in initial-first-set)
              when (t) set := add-new(set, t) end
            end;
	    when (member?(#f, initial-first-set))
	      do-item(item.item-next-item)
	    end;
	  else
	    complete? := #f;
	  end if;
	end;
  for (rule :: <rule> in nt.nont-rules)
    do-item(rule.rule-item)
  end;
  if (complete?) nt.term-first-set := set end;
  set
end;

/// Structure definitions for items and rules

define class <deriv-item> (<sealed-constructor>)
  constant slot deriv-item-initial :: <non-terminal>,
    required-init-keyword: initial:;
  constant slot deriv-item-result :: <non-terminal>,
    required-init-keyword: result:;
  constant slot deriv-item-terminal :: false-or(<terminal>),
    required-init-keyword: terminal:;
end;

define class <deriv-rule> (<sealed-constructor>)
  constant slot deriv-rule-to :: <non-terminal>,
    required-init-keyword: to:;
  constant slot deriv-rule-terminals :: <list>,
    required-init-keyword: terminals:;
end;

define function setup-derivs (gv :: <gvars>)
  let rule-set = make-deriv-rule-set(gv);
  let items = make-initial-item-list(gv);
  for (item in items) add-deriv(item) end;
  iterate loop (items :: <list> = items)
    unless (empty?(items))
      let item = items.head;
      let undone-items :: <list> = items.tail;
      let rules = element(rule-set, item.deriv-item-initial, default: #f);
      when (rules)
	for (new-item in apply-rules(rules, item))
	  when (add-deriv(new-item))
	    undone-items := pair(new-item, undone-items)
	  end;
	end;
      end;
      loop(undone-items)
    end;
  end;
end;

define function make-deriv-rule-set (gv :: <gvars>)
 => (set :: <table>) // first-nt => list of <deriv-rule>'s
  let rule-set = make(<table>);
  for (rule :: <rule> in gv.%grammar-rules)
    let item = rule.rule-item;
    if (item-non-terminal-left?(item))
      let nt = item.item-first-term;
      let dr = make(<deriv-rule>,
		    to: rule.rule-lhs,
		    terminals: item-next-first-set(item));
      rule-set[nt] := pair(dr, element(rule-set, nt, default: #()));
    end;
  end;
  rule-set
end;

/// apply-rules (rule-set item) applies all the rules in rule-set to
/// the item, returning a list of the new items generated by the
/// rules.  The returned list may contain duplicates.
define function apply-rules (rules :: <list>, item :: <deriv-item>)
 => (new-items :: <list>)
  let result = item.deriv-item-result;
  let terminal = item.deriv-item-terminal;
  if (~terminal)
    reduce(method (so-far :: <list>, rule :: <deriv-rule>)
	     reduce(method (so-far :: <list>, term)
		      add(so-far,
			  make(<deriv-item>,
			       initial: rule.deriv-rule-to,
			       result: result,
			       terminal: term))
		    end,
		    so-far,
		    rule.deriv-rule-terminals)
	   end,
	   #(),
	   rules)
  else
    map(method (rule :: <deriv-rule>)
	  make(<deriv-item>,
	       initial: rule.deriv-rule-to,
	       result: result,
	       terminal: terminal)
	end,
	rules);
  end;
end;


define function make-initial-item-list (gv :: <gvars>)
 => (deriv-items :: <list>)
  map-as(<list>,
	 method (nt :: <non-terminal>)
	   make(<deriv-item>, initial: nt, result: nt, terminal: #f)
	 end,
	 gv.%grammar-non-terminals);
end;

define function add-deriv (item :: <deriv-item>) => (new?)
  let initial = item.deriv-item-initial;
  let result = item.deriv-item-result;
  let terminal = item.deriv-item-terminal;
  let alist = initial.nont-derivs;
  let data = assoc(result, alist);
  if (~data)
    initial.nont-derivs := pair(pair(result, list(terminal)), alist);
  elseif (~member?(terminal, data.tail))
    data.tail := pair(terminal, data.tail);
  else
    #f
  end;
end;

define function create-parse-table (gv :: <gvars>)
  /// The start symbol's rule is always the first of the grammar rules
  compute-kernel-sets(gv);
  initialize-lookahead-table(gv);
  determine-lookaheads(gv);
  propagate-the-lookaheads(gv);
end;

define function compute-kernel-sets (gv :: <gvars>)
  let indices :: <item-set-table> = make(<item-set-table>);
  let item-sets = gv.%grammar-item-sets;
  local method intern-item-set (items :: <list>) => (state :: <state>)
	  let items = make-canonical(items);
	  element(indices, items, default: #f)
	    | begin
		let index = item-sets.size;
		add!(item-sets, items);
		indices[items] := index // return index
	      end;
	end;
  let reductions :: <object-table> = make(<object-table>);
  for (nt in gv.%grammar-non-terminals)
    reductions[nt] := map(head, nt.nont-derivs)
  end;
  local method state-transitions (state)
	  let item-set = state-item-set(gv, state);
	  let goto-sets = make-goto-table(reductions, item-set);
	  let result = #();
	  for (new-item-set keyed-by sym in goto-sets)
	    let goto-state = intern-item-set(new-item-set);
	    add-to-goto-table(gv, state, sym, goto-state);
	    result := pair(goto-state, result);
	  end;
	  result
	end;
  make-set-closure(state-transitions, intern-item-set(list(gv.%grammar-initial-item)));
end;

define function initialize-lookahead-table (gv :: <gvars>)
  for (item-set keyed-by state in gv.%grammar-item-sets)
    for (item :: <item> in item-set)
      state-item-lookaheads(gv, state, item) := #();
    end;
  end;
end;

define constant $initial-state = 0;

define function determine-lookaheads (gv :: <gvars>)
  for (item :: <item> in gv.%grammar-items)
    item.item-lr1-closure := compute-lr1-closure(item);
  end;
  /// By definition the end of input token :eoi is spontaneously
  /// generated for the initial rule
  add-to-lookahead-table(gv,
			 $initial-state,
			 gv.%grammar-initial-item,
			 $eoi-term);
  for (state from 0 below gv.%grammar-number-states)
    determine-state-lookaheads(gv, state);
  end;
end;

///
/// Propagate the lookaheads 
///

define function propagate-the-lookaheads (gv :: <gvars>)
  iterate loop ()
    let more? = #f;
    for (pe = gv.%grammar-propagation-queue then pe.pentry-next, while: pe)
      more? := propagate-a-lookahead(gv, pe) | more?;
    end;
    when (more?) // try again.
      loop()
    end;
  end;
end;  

define function define-actions (gv :: <gvars>)
  let nstates = gv.%grammar-number-states;
  for (nt :: <non-terminal> in gv.%grammar-non-terminals)
    nt.nont-deriv-first-set := compute-nont-deriv-first-set(nt);
  end;
  for (state from 0 below nstates)
    make-actions(gv, state);
  end;
end;

// TODO is this different from term-first-set??
define function compute-nont-deriv-first-set (nt :: <non-terminal>)
 => (fs :: <list>)
  let seen :: <list> = #();
  for (d in nt.nont-derivs)
    for (rule in d.head.nont-rules)
      let item = rule.rule-item;
      when (item-terminal-left?(item))
	seen := add-new(seen, item.item-first-term);
      end;
    end;
  end;
  seen
end;

define function make-actions (gv :: <gvars>, state :: <state>)
  let item-set = state-item-set(gv, state);
  for (item in item-set) make-action(gv, state, item) end;
end;

define function item-next-first-set (item :: <non-empty-item>)
 => (s :: <list>)
  let item = item.item-next-item;
  if (empty?(item))
    #(#f)
  elseif (item-terminal-left?(item) | empty?(item.item-next-item))
    term-first-set(item.item-first-term)
  else
    item.item-first-set | (item.item-first-set := compute-item-first-set(item))
  end;
end;

define function compute-item-first-set (item :: <non-empty-item>)
 => (s :: <list>)
  let first-set = term-first-set(item.item-first-term);
  if (member?(#f, first-set))
    let rest-of-first-set = item-next-first-set(item);
    combine-first-sets(first-set, rest-of-first-set);
  else
    first-set
  end;
end;


define function add-to-goto-table (gv :: <gvars>,
				   state :: <state>,
				   term :: <term>,
				   newstate :: <state>)
  gv.%grammar-gotos[state-key(state, term)] := newstate;
end;


define function compute-lr1-closure (item :: <item>)
 => (l :: <list>)
  let result :: <list> = list(pair(item, #(#f)));
  if (~item-non-terminal-left?(item))
    result
  else
    let beta-first-set = item.item-next-first-set;
    for (deriv :: <pair> in item.item-first-term.nont-derivs)
      let nont = deriv.head;
      let zeta = deriv.tail;
      let zeta-beta-first-set = combine-first-sets(zeta, beta-first-set);
      for (rule in nont.nont-rules)
	result := add!(result, pair(rule.rule-item, zeta-beta-first-set));
      end;
    end;
    result
  end;
end;

define function combine-first-sets (alpha :: <list>, beta :: <list>)
 => (s :: <list>)
  if (~member?(#f, alpha))
    alpha
  else
    let result = beta;
    for (la :: false-or(<term>) in alpha)
      when (la & ~member?(la, beta))
	result := pair(la, result);
      end;
    end;
    result
  end;
end;

define function determine-state-lookaheads (gv :: <gvars>, state :: <state>)
  let kernel-set = state-item-set(gv, state);
  for (kernel-item :: <item> in kernel-set)
    for (lr1-item :: <pair> in kernel-item.item-lr1-closure)
      let item :: <item> = lr1-item.head;
      let lookaheads :: <list> = lr1-item.tail;
      for (la :: false-or(<term>) in lookaheads)
	if (~la)
	  add-to-propagation-table(gv, kernel-item, state, item);
	else
	  generate-spontaneous-lookahead(gv, state, item, la);
	end;
      end;
    end;
  end;
end;


define class <propagation-entry> (<sealed-constructor>)
  constant slot pentry-from-key, required-init-keyword: from-key:;
  constant slot pentry-goto-key, required-init-keyword: goto-key:;
  constant slot pentry-next :: false-or(<propagation-entry>),
    required-init-keyword: next:;
end;

define inline function add-to-propagation-table (gv :: <gvars>,
					  from-item :: <item>,
					  in-state :: <state>,
					  to-item :: <item>)
  unless (empty?(to-item))
    let to-item :: <non-empty-item> = to-item;
    gv.%grammar-propagation-queue
      := make(<propagation-entry>,
	      from-key: state-key(in-state, from-item),
	      goto-key: state-key(state-term-goto-state(gv, in-state,
                                                        to-item.item-first-term),
                                  to-item.item-next-item),
	      next: gv.%grammar-propagation-queue);
  end;
end;


define inline function generate-spontaneous-lookahead
    (gv :: <gvars>, state :: <state>, to-item :: <item>, lookahead :: <term>)
  unless (empty?(to-item))
    let to-item :: <non-empty-item> = to-item;
    let goto-term = to-item.item-first-term;
    let goto-state = state-term-goto-state(gv, state, goto-term);
    let goto-item = to-item.item-next-item;
    add-to-lookahead-table(gv, goto-state, goto-item, lookahead)
  end;
end;


define function propagate-a-lookahead (gv :: <gvars>,
				       entry :: <propagation-entry>)
 => (changed? :: <boolean>)
  // entry is "state-item1 -> state-item2"
  // add lookaheads for state-item1 to those of state-item2, if not
  // already there.
  let from-las = gv.%grammar-lookaheads[entry.pentry-from-key];
  let goto-key = entry.pentry-goto-key;
  let goto-las = gv.%grammar-lookaheads[goto-key];
  let changed? = #f;
  for (t in from-las,
       las = goto-las then if (member?(t, goto-las)) las else pair(t, las) end)
  finally
    unless (las == goto-las)
      gv.%grammar-lookaheads[goto-key] := las;
      #t
    end;
  end;
end;

define function add-to-lookahead-table
    (gv :: <gvars>, state :: <state>, item :: <item>, la :: <term>)
  let key = state-key(state, item);
  let las :: <list> = gv.%grammar-lookaheads[key];
  unless (member?(la, las))
    gv.%grammar-lookaheads[key] := pair(la, las);
  end;
end;

define inline function make-set-closure (f :: <function>, top-state :: <state>)
  let seen-states = make(<object-set>);
  add!(seen-states, top-state);
  iterate loop (states :: <list> = f(top-state))
    unless(empty?(states))
      let top-state = states.head;
      if (member?(top-state, seen-states))
	loop(states.tail);
      else
	add!(seen-states, top-state);
	loop(concatenate(f(top-state), states.tail))
      end;
    end;
  end iterate;
end function;


define function make-derived-actions (gv :: <gvars>, state :: <state>, item :: <item>, itemb :: <non-terminal>)
  generate-shifts(gv, state, item, itemb);
  generate-errors(gv, state, item, itemb);
  generate-reductions(gv, state, item, itemb);
end;

define function generate-shifts (gv :: <gvars>, state :: <state>, item :: <item>, itemb :: <non-terminal>)
  for (term in itemb.nont-deriv-first-set)
    set-shift-action(gv, state, item, term);
  end;
end;

define function generate-errors (gv :: <gvars>, state :: <state>, item :: <item>, itemb :: <non-terminal>)
  for (d in itemb.nont-derivs)
    maybe-set-error-action(gv, state, d.head);
  end;
end;

define function generate-reductions (gv :: <gvars>, state :: <state>, item :: <item>, nt :: <non-terminal>)
  let derivs = nt.nont-derivs;
  let possible-reductors =
    choose(method (d) d.head.nont-epsilon-item end, derivs);
  when (~empty?(possible-reductors))
    let lookaheads = state-item-lookaheads(gv, state, item);
    let beta-lookaheads = item-next-first-set(item);
    for (deriv :: <pair> in possible-reductors)
      let las = deriv.tail;
      let reducing-input = las;
      when (member?(#f, las))
	reducing-input := concatenate(beta-lookaheads, reducing-input);
	when (member?(#f, beta-lookaheads))
	  reducing-input := concatenate(lookaheads, reducing-input);
	end;
      end;
      for (term in remove-duplicates(reducing-input))
	when (term)
	  set-reduce-action(gv, state, deriv.head.nont-epsilon-item, term)
	end;
      end;
    end;
  end;
end;

define function make-goto-table (reductions :: <object-table>,
				 item-set :: <list>)
  // Returns a hash table containing the goto sets for the given states.
  // That is, make-goto-table(state)[symbol] is the set of items in the
  // set GOTO(state, symbol).
  let newstates = make(<object-table>); // rehash-size: 2.0
  local method add-goto (sym, item :: <non-empty-item>)
	  let next-item = item.item-next-item;
	  let old = element(newstates, sym, default: #());
	  unless (member?(next-item, old))
	    newstates[sym] := pair(next-item, old);
	  end;
	end;
  for (item :: <item> in item-set)
    unless (empty?(item))
      let next-term = item.item-first-term;
      add-goto(next-term, item);
      when (item-non-terminal-left?(item))
	for (nont in element(reductions, next-term, default: #()))
	  for (rule in nont.nont-rules)
	    let start-item = rule.rule-item;
	    unless (empty?(start-item))
	      add-goto(start-item.item-first-term, start-item)
	    end;
	  end;
	end;
      end;
    end;
  end;
  newstates
end;

define function make-canonical (item-set :: <list>)
  if (item-set == #() | item-set.tail == #())
    item-set
  else
    sort(item-set, stable: #t, test: method (i :: <item>, j :: <item>)
				       i.item-index < j.item-index
				     end)
  end;
end;

define function make-action (gv :: <gvars>, state :: <state>, item :: <item>)
  if (empty?(item))
    make-simple-reductions(gv, state, item);
  elseif (item-terminal-left?(item))
    set-shift-action(gv, state, item, item.item-first-term);
  else
    make-derived-actions(gv, state, item, item.item-first-term)
  end;
end;

define function make-simple-reductions (gv :: <gvars>, state :: <state>, item :: <item>)
   let lookaheads = state-item-lookaheads(gv, state, item);
   for (lookahead in lookaheads)
     set-reduce-action(gv, state, item, lookahead)
   end;
end;

define class <action-entry> (<sealed-constructor>)
  slot aentry-shift? :: <boolean>, required-init-keyword: shift?:;
  slot aentry-item :: <item>, required-init-keyword: item:;
end;

define function maybe-set-error-action (gv :: <gvars>, state :: <state>, nt :: <non-terminal>)
  let index = nt.nont-error-rule-index;
  when (index)
    let old :: false-or(<index>) = element(gv.%grammar-error-action-table, state, default: #f);
    if (~old | old < index)
      gv.%grammar-error-action-table[state] := index;
    end
  end;
end;

define /* EXPORTED */ class <grammar-conflict> (<warning>, <sealed-constructor>)
  constant slot grammar-conflict-terminal,
    required-init-keyword: terminal:;
  constant slot grammar-conflict-action-1 :: one-of(#"shift", #"reduce"),
    required-init-keyword: action-1:;
  constant slot grammar-conflict-rule-1 :: <sequence>,
    required-init-keyword: rule-1:;
  constant slot grammar-conflict-position-1 :: <integer>,
    required-init-keyword: position-1:;
  constant slot grammar-conflict-action-2 :: one-of(#"shift", #"reduce"),
    required-init-keyword: action-2:;
  constant slot grammar-conflict-rule-2 :: <sequence>,
    required-init-keyword: rule-2:;
  constant slot grammar-conflict-position-2 :: <integer>,
    required-init-keyword: position-2:;
end;

define function set-reduce-action (gv, state, item, terminal)
  set-action(gv, state, item, terminal, #f);
end;

define function set-shift-action (gv :: <gvars>, state :: <state>, item :: <item>, terminal :: <term>)
  set-action(gv, state, item, terminal, #t)
end;

define function note-conflict (terminal :: <terminal>,
                               shift-1? :: <boolean>,
                               item-1 :: <item>,
                               shift-2? :: <boolean>,
                               item-2 :: <item>)
  local method item-position (item :: <item>)
      iterate loop (start = item.item-start-item, n = 0)
        if (item == start) n else loop(start.item-next-item, n + 1) end
      end;
  end;
  signal(make(<grammar-conflict>, terminal: terminal.term-token,
              action-1: if (shift-1?) #"shift" else #"reduce" end,
              rule-1: item-1.item-rule.rule-user-rule,
              position-1: item-position(item-1),
              action-2: if (shift-2?) #"shift" else #"reduce" end,
              rule-2: item-2.item-rule.rule-user-rule,
              position-2: item-position(item-2)))
end;
define function set-action (gv :: <gvars>,
			    state :: <state>,
			    item :: <item>,
			    terminal :: <terminal>,
			    shift? :: <boolean>)
  let key = state-key(state, terminal);
  let old :: false-or(<action-entry>)
	 = element(gv.%grammar-actions, key, default: #f);
  if (~old)
    gv.%grammar-actions[key] :=  make(<action-entry>,
				      shift?: shift?,
				      item: item);
  elseif (shift? & old.aentry-shift?)
    // Remember earliest item in case of conflict...
    when (item.item-rule-index  < old.aentry-item.item-rule-index)
      old.aentry-item := item;
    end;
  else
    let old-item = old.aentry-item;
    let old-shift? = old.aentry-shift?;
    let old-rule = old-item.item-rule-index;
    let rule = item.item-rule-index;
    unless (~shift? & ~old-shift? & rule == old-rule)
      // There is a conflict
      if (old-rule < rule | (old-rule = rule & shift?))
	note-conflict(terminal, old-shift?, old-item, shift?, item)
      else
	note-conflict(terminal, shift?, item, old-shift?, old-item);
	old.aentry-shift? := shift?;
	old.aentry-item := item;
      end;
    end unless;
  end if;
end;

// eof

