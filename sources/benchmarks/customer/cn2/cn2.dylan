Author:   Bruce Tobin.
Synopsis: A simple version of the CN2 rule induction algorithm.
Module:   cn2
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parser utilities

define inline function whitespace?
    (c :: <byte-character>) => (whitespace? :: <boolean>)
  c == ' ' | c == '\t' | c == '\n'
end function whitespace?;

define inline function skip-whitespace
    (string :: <byte-string>, _start :: <integer>, _end :: <integer>)
 => (_start :: <integer>)
  while (_start < _end & whitespace?(string[_start]))
    _start := _start + 1
  end;
  _start
end function skip-whitespace;

define function tokenize
    (line :: <byte-string>)
 => (tokens :: <stretchy-object-vector>)
  let tokens :: <stretchy-object-vector> = make(<stretchy-vector>);
  let _start :: <integer> = 0;
  let _end   :: <integer> = size(line);
  let token  :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method next-token () => (token :: false-or(<byte-string>))
	  _start := skip-whitespace(line, _start, _end);
	  if (_start < _end)
	    let quoted? :: false-or(<character>) = #f;
            let done?   :: <boolean> = #f;
            token.size := 0;
            while (_start < _end & ~done?)
              let c :: <byte-character> = line[_start];
	      case
		quoted? & whitespace?(c) =>
		  add!(token, c);
		quoted? == c =>
		  quoted? := #f;
                c == '"' | c == '\'' =>
                  quoted? := c;
                whitespace?(c) =>
                  done? := #t;
                otherwise =>
                  add!(token, c);
              end;
              _start := _start + 1
            end;
            concatenate-as(<byte-string>, token)
          else
            #f
          end
        end method next-token;
  while (_start < _end)
    let token = next-token();
    when (token)
      add!(tokens, token)
    end
  end;
  tokens
end function tokenize;


/// Simple representation abstractions

define constant <class-name> = <symbol>;
define constant $no-class    = #"none";

define constant <cn2-float> = <single-float>;

define inline function cn2-float (r :: <real>) => (float :: <cn2-float>)
  as(<cn2-float>, r)
end function;


/// Variables

define variable *classes*    :: <simple-object-vector> = #[];
define variable *attributes* :: <list> = #();
define variable *domains*    :: <list> = #();

define variable *heap*       :: <list> = #();

define variable *examples*   :: <list> = #();

define variable *best-node*                 :: false-or(<node>) = #f;
define variable *initial-majority-class*    :: <class-name>     = $no-class;
define variable *global-distribution*       :: <list>           = #();
define variable *global-number-of-examples* :: <integer>        = 0;
define variable *threshold*                 :: <cn2-float>      = cn2-float(0);

define variable *heap-size* :: <integer> = 5;
define variable *star-size* :: <integer> = 5;


/// Classes

define made-inline sealed class <node> (<object>)
  slot node-rank :: <cn2-float> = cn2-float(-1.0);
  slot node-qqs :: <list> = #(), // actually, 'limited(<list>, of: <list>)'
    init-keyword: qqs:;
  constant slot node-cover :: <simple-object-vector>
    = make(<vector>, size: *domains*.size, fill: #f),
    init-keyword: cover:;
end class <node>;

define sealed domain make (singleton(<node>));
define sealed domain initialize (<node>);

define class <rule> (<object>)
  constant slot rule-cover :: <simple-object-vector>,
    required-init-keyword: cover:;
  slot rule-class :: <class-name> = $no-class,
    init-keyword: class:;
  slot rule-distribution :: <list> = #(),
    init-keyword: distribution:;
end class <rule>;

define sealed domain make (singleton(<rule>));
define sealed domain initialize (<rule>);

define function load-cn2
    (attribute-file, example-file) => ()
  with-open-file (fin = attribute-file)
    for (line = read-line(fin) then read-line(fin),
         until: stream-at-end?(fin))
      let tokens :: <list> = as(<list>, tokenize(line));
      when (~empty?(tokens))
        let first-token :: <byte-string> = first(tokens);
	let first-token-end = first-token.size - 1;
	if (first-token[first-token-end] == ':')
          let first-token 
            = as(<symbol>, copy-sequence(first-token, end: first-token-end));
          *attributes* := add!(*attributes*, first-token);
          *domains*    := add!(*domains*, 
                               map(string-to-integer, copy-sequence(tokens, start: 1)));
        end
      end
    end;
    *attributes* := reverse!(*attributes*);
    *domains*    := reverse!(*domains*);
  end;
  with-open-file (fin = example-file)
    let class-table :: <object-table> = make(<object-table>);
    for (line = read-line(fin) then read-line(fin),
         until: stream-at-end?(fin))
      let tokens :: <list> = as(<list>, tokenize(line));
      when (~empty?(tokens))
        let last-token :: <byte-string> = last(tokens);
	let last-token-end = last-token.size - 1;
	if (last-token[last-token-end] == ';')
          let last-token 
            = as(<symbol>, copy-sequence(last-token, end: last-token-end));
          class-table[last-token] := 1;
          *examples* := add!(*examples*,
			     pair(last-token, 
                                  map(string-to-integer,
                                      copy-sequence(tokens, end: tokens.size - 1))));
        end
      end
    end;
    *classes* := as(<simple-object-vector>, class-table.key-sequence);
  end
end function load-cn2;

define function run-cn2
    (examples :: <list>) => ()
  let rules = #();
  let (elapsed-seconds, elapsed-microseconds)
    = timing ()
        rules := cn2(examples);
      end;
  print-rules(rules);
  format-out("%d rules found (in %d.%s seconds)\n",
	     rules.size,
	     elapsed-seconds, integer-to-string(elapsed-microseconds, size: 6));
end function run-cn2;

define function cn2
    (examples :: <list>) => (rules :: <list>)
  *heap* := #();
  let rules :: <list> = #();
  for (qqs :: <list> = classify(examples) then filter(qqs),
       until: qqs-empty?(qqs))
    *best-node* := make(<node>, qqs: qqs);
    record-global-dist(qqs);
    *initial-majority-class* := majority-class(qqs);
    rules := add!(rules, get-best-rule(initial-star(qqs)));
  end;
  reverse!(rules)
end function cn2;

define function classify
    (examples :: <list>) => (classified :: <list>)
  let tmp :: <list> = #();
  format-out("there are %d classes\n",  *classes*.size);
  format-out("there are %d examples\n", *examples*.size);
  for (class in *classes*)
    tmp := add!(tmp, map(method (list) as(<vector>, tail(list)) end,
			 choose(method (list) first(list) == class end,
			        examples)))
  end;
  *global-number-of-examples* := reduce(\+, 0, map(size, tmp));
  reverse!(tmp)
end function classify;

define function filter
    (qqs :: <list>) => (filtered-qqs :: <list>)
  let tmp = #();
  let best-cover = *best-node*.node-cover;
  for (qq :: <list> in qqs)
    tmp := add!(tmp, choose(method (example) ~covers?(best-cover, example) end, qq))
  end;
  reverse!(tmp)
end function filter;

define inline function qqs-empty?
    (qqs :: <list>) => (is-empty :: <boolean>)
  ~find-key(qqs, complement(empty?))
end function qqs-empty?;

define function covers?
    (cover :: <simple-object-vector>, example :: <simple-object-vector>)
 => (covered :: <boolean>)
  block (return)
    for (i :: <integer> from 0 below cover.size)
      let x = cover[i];
      when (x & (x ~= example[i]))
        return(#f)
      end
    end;
    #t
  end
end function covers?;

define function record-global-dist
    (qqs :: <list>) => ()
  *global-distribution* := map(size, qqs);
  *global-number-of-examples* :=  reduce(\+, 0, *global-distribution*);
end function record-global-dist;

define function majority-class
    (qqs :: <list>)
 => (class :: <class-name>, index :: <integer>, largest :: <integer>, total :: <integer>)
  let largest :: <integer> = 0;
  let counter :: <integer> = 0;
  let index   :: <integer> = 0;
  let total   :: <integer> = 0;
  for (qq :: <list> in qqs)
    let len :: <integer> = qq.size;
    when (len > largest)
      largest := len;
      index   := counter;
    end;
    total   := total + len;
    counter := counter + 1;
   end;
  values(*classes*[index], index, largest, total)
end function majority-class;

define function initial-star
    (qqs :: <list>) => (star :: <list>)
  let node = make(<node>, qqs: qqs);
  evaluate-rank(node);
  list(node)
end function initial-star;

define function get-best-rule
    (star :: <list> ) => (rule :: <rule>)
  while(~empty?(star))
    for (e in star)
      specialise(e);
    end;
    star := heap-to-star();
  end;
  let best-node :: <node> = *best-node*;
  let rule :: <rule> = make(<rule>, cover: best-node.node-cover);
  if (best-node.node-rank > -1)
    rule.rule-class        := majority-class(best-node.node-qqs);
    rule.rule-distribution := map(size, best-node.node-qqs);
  else
    rule.rule-class        := *initial-majority-class*;
    rule.rule-distribution := *global-distribution*;
  end;
  rule
end function get-best-rule;

define function heap-to-star
    () => (star :: <list>)
  let heap :: <list> = sort!(*heap*, test: worse?);
  if (heap.size > *star-size*)
    *heap* := copy-sequence(heap, start: *star-size*);
    copy-sequence(heap, end: *star-size*);
  else
    *heap* := #();
    heap;
  end
end function heap-to-star;

define function specialise
    (node :: <node>) => ()
  for (index :: <integer> from 0,
       domain :: <list> in *domains*)
    unless (node.node-cover[index])
      specialise-domain(domain, node, index);
    end
  end
end function specialise;

define inline function specialise-domain
    (domain :: <list>, node :: <node>, index :: <integer>) => ()
  for (val in domain)
    let lower-node :: <node>
      = make(<node>, cover: copy-sequence(node.node-cover));
    lower-node.node-cover[index] := val;
    when (filter-examples(lower-node, node, index, val))
      assess(lower-node)
    end
  end
end function specialise-domain;

define inline function filter-examples
    (lower-node :: <node>, node :: <node>, index :: <integer>, val)
 => (passed :: <boolean>)
  let val :: <integer> = val;
  let tmp :: <list> = #();
  let new-count :: <integer> = 0;
  let old-count :: <integer> = 0;
  for (qq :: <list> in node.node-qqs)
    let tmp1 :: <list> = #();
    for (ex :: <simple-object-vector> in qq)
      old-count := old-count + 1;
      without-bounds-checks
        when (ex[index] == val)
          tmp1 := pair(ex, tmp1);
          new-count := new-count + 1;
        end
      end
    end;
    tmp := pair(reverse!(tmp1), tmp);
  end;
  lower-node.node-qqs := reverse!(tmp);
  (new-count > 0) & (old-count > new-count);
end function filter-examples;

define inline function assess
    (node :: <node>) => ()
  let node-status :: <symbol> = evaluate-rank(node);
  unless (node-status == #"useless")
    if (better?(node, *best-node*)
	  & significant?(node)
	  & node-status == #"better-than-default")
      *best-node* := node;
    else
      let best-node :: <node> = *best-node*;
      if (potential-rank(node) > best-node.node-rank)
	toss-onto-heap(node)
      end
    end
  end
end function assess;

define function evaluate-rank
    (node :: <node>) => (status :: <symbol>)
  let (class, index, length, total) = majority-class(node.node-qqs);
  if (total > 0)
    node.node-rank 
      := cn2-float(1 + length) / cn2-float(total + *classes*.size);
    let global-distribution :: <integer> = *global-distribution*[index];
    let glob-dist :: <cn2-float> = cn2-float(global-distribution);
    if (cn2-float(length) / cn2-float(total)
	  > (glob-dist / cn2-float(*global-number-of-examples*)))
      #"better-than-default"
    else
      #"worse-than-default"
    end
  else
    #"useless"
  end
end function evaluate-rank;

define function potential-rank
    (node :: <node>) => (rank :: <cn2-float>)
  let (class, index, length) = majority-class(node.node-qqs);
  cn2-float(length + 1) / cn2-float(length + *classes*.size)
end function potential-rank;

define function significant?
    (node :: <node>) => (significant :: <boolean>)
  let qqs = node.node-qqs;
  let examples-at-node :: <integer> = reduce(\+, 0, map(size, qqs));
  let ratio :: <cn2-float> 
    = examples-at-node / cn2-float(*global-number-of-examples*);
  let distance = reduce(\+, 0, map(method (x :: <list>, y :: <integer>)
				     let quot :: <integer> = x.size;
				     if (quot > 0 & y > 0)
				       quot * log(cn2-float(quot) / (y * ratio))
				     else
				       0
				     end
				   end method,
				   qqs, *global-distribution*));
  let significance = 2 * distance;
  significance > *threshold*
end function significant?;

define inline function better?
    (node1 :: <node>, node2 :: <node>) => (better? :: <boolean>)
  node1.node-rank > node2.node-rank
end function better?;

define inline function worse?
    (node1 :: <node>, node2 :: <node>) => (worse? :: <boolean>)
  node1.node-rank < node2.node-rank
end function worse?;

/* 
define function count-qqs
    (node :: <node>) => (count :: <integer>)
  reduce(\+, 0, map(size, node.node-qqs))
end function count-qqs;
*/

define function toss-onto-heap
    (item :: <node>) => ()
    unless (find-key(*heap*, method (node :: <node>)
  			       node.node-rank = item.node-rank
  			         & node.node-cover = item.node-cover
  			     end method))
      if (*heap*.size < *heap-size*)
        *heap* := add!(*heap*,item)
      else
        *heap* := sort!(*heap*, test: worse?);
        if (better?(item, *heap*.first))
          *heap*.first := item
        end
      end
    end
end function toss-onto-heap;

define inline function nontrivial?
    (rule :: <rule>) => (nontrivial? :: <object>)
  find-key(rule.rule-cover, identity)
end function nontrivial?;

define function print-rule (rule :: <rule>) => ()
  if (nontrivial?(rule))
    let clauses = choose(complement(curry(\=, "")),
			 map(method (x, y)
			       if(x)
				 format-to-string("%s = %s", y, x);
			       else
				 ""
			       end if
			     end method,
			     rule.rule-cover, *attributes*));
    let condition = reduce1(method (x, y)
			      concatenate(x, " and ", y)
			    end method, clauses);
    format-out("if %s then item is %s\n", condition, rule.rule-class);
  else
    format-out("<DEFAULT> item is %s\n", rule.rule-class);
  end
end function print-rule;

define function print-rules
    (rules :: <list>) => ()
  format-out("Printing rules..\n");
  for (rule :: <rule> in rules)
    print-rule(rule);
    when (nontrivial?(rule))
      format-out("else\n")
    end
  end
end function print-rules;

define function node-print
    (node :: <node>) => ()
  format-out("Rank: %=\n", node.node-rank);
  cover-print(node.node-cover);
  for (qq :: <list> in node.node-qqs)
    format-out("%d ", qq.size)
  end;
  format-out("\n");
  let (class,index,length,total) = majority-class(node.node-qqs);
  format-out("class= %s, index = %d, length = %d, total = %d\n",
	     class, index, length, total)
end function node-print;

define function cover-print
    (cover :: <simple-object-vector>) => ()
  format-out("Cover: ");
  for (elt in cover)
    format-out("%= ", elt)
  end;
  format-out("\n")
end function cover-print;

// Suppress warnings for these unused definitions.
ignore(cover-print, node-print, rule-distribution);

load-cn2("soya.att", "soya.exs");

run-cn2(*examples*);
