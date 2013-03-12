Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $max-lexer-code :: <integer> = 255;

define class <tokenizer-state> (<grammar-object>)
  constant slot tokenizer-result :: false-or(<function>),
    required-init-keyword: result:;
  constant slot tokenizer-transitions :: false-or(<simple-object-vector>),
    required-init-keyword: transitions:;
end class <tokenizer-state>;


define method add-transition
    (table :: <simple-object-vector>, on :: <integer>, new-state :: <symbol>)
  debug-assert(~table[on],
               "input %= transitions to both %= and %=",
               as(<character>, on), table[on], new-state);
  table[on] := new-state;
end;

define inline method add-transition
    (table :: <simple-object-vector>, on :: <character>, new-state :: <symbol>)
  add-transition(table, as(<integer>, on), new-state);
end method add-transition;

define method add-transition
    (table :: <simple-object-vector>, on :: <string>, new-state :: <symbol>)
  let last = #f;
  let range = #f;
  for (char :: <character> in on)
    if (range)
      if (last)
        for (i :: <integer> from as(<integer>, last) + 1 to as(<integer>, char))
          add-transition(table, i, new-state);
        end for;
        last := #f;
      else
        add-transition(table, '-', new-state);
        add-transition(table, char, new-state);
        last := char;
      end if;
      range := #f;
    elseif (char == '-')
      range := #t;
    else
      add-transition(table, char, new-state);
      last := char;
    end if;
  end for;
end method add-transition;

define method make-tokenizer-state
    (name :: <symbol>, result :: false-or(<function>), #rest transitions)
  let table = size(transitions) > 0
                & make(<vector>, size: $max-lexer-code + 1, fill: #f);
  for (transition :: <pair> in transitions)
    add-transition(table, head(transition), tail(transition));
  end for;
  pair(name, make(<tokenizer-state>, result: result, transitions: table));
end make-tokenizer-state;


define inline-only function compile-state-machine (#rest states)
 => (state :: <tokenizer-state>)
  // make a hash table mapping state names to states.
  let state-table = make(<table>);
  for (state-pair :: <pair> in states)
    let name = state-pair.head;
    debug-assert(~element(state-table, name, default: #f),
                 "State %= multiply defined.", name);
    state-table[name] := state-pair.tail;
  end for;
  // Now that we have a table mapping state names to states, change the
  // entries in the transition tables to refer to the new state
  // object themselves instead of just to the new state name.
  for (state-pair :: <pair> in states)
    let state :: <tokenizer-state> = state-pair.tail;
    let table = state.tokenizer-transitions;
    if (table)
      for (i from 0 to $max-lexer-code)
        let new-state = table[i];
        if (new-state)
          table[i] := state-table[new-state];
        end if;
      end for;
    end if;
  end for;
  element(state-table, #"start");
end compile-state-machine;


define macro state-machine-definer
  { define state-machine ?variable-name:name ?states end }
    => { define constant ?variable-name :: <tokenizer-state>
          = compile-state-machine(?states) }
states:
  { } => { }
  { state ?state-name:token ?action:expression, ?actions ; ... }
    => { make-tokenizer-state(?state-name, ?action, ?actions), ... }
actions:
  { } => { }
  // TODO: since we never use an actual expression for chars,
  //   we could do #(?chars, ?next-state), save load-time consing...
  //   Do this after verify that \<> escapes work.
  { ?next-state:token ?chars:expression, ... }
    => { pair(?chars, ?next-state), ... }
end macro;


define function hex-shift (value :: false-or(<integer>), h :: <integer>)
 => (new-value :: false-or(<integer>))
  when (value)
    let n = if (48 <= h & h <= 57) h - 48
            elseif (65 <= h & h <= 70) h - 55
            elseif (97 <= h & h <= 102) h - 87
            else #f end;
    when (n)
      value * 16 + n
    end;
  end;
end;

// TODO: THIS NEEDS TO HANDLE UNICODE ESCAPES
define function extract-string
    (contents :: <byte-vector>, start-pos :: <integer>, end-pos :: <integer>)
  let bytes = end-pos - start-pos;
  let string :: <byte-string> = make(<byte-string>, size: bytes);
  copy-bytes(string, 0, contents, start-pos, bytes);
  string
end;

define method get-token-from-contents (initial-state :: <tokenizer-state>,
                                       contents :: <byte-vector>,
                                       initial-position :: <integer>,
                                       context)
 => (token-or-eoi, new-position :: <integer>)
  let length :: <integer> = contents.size;

  let even? = #t;
  local method readch (posn :: <integer>) => (ch :: <integer>, posn :: <integer>)
          let ch = contents[posn];
          if (ch ~== as(<integer>, '\\'))
            even? := #t;
            values(ch, posn + 1)
          elseif (even? &
                    posn + 1 < length &
                    contents[posn + 1] == as(<integer>, 'u'))
            iterate unicode (pos :: <integer> = posn + 2)
              if (pos < length & contents[pos] == as(<integer>, 'u'))
                unicode(pos + 1);
              else
                let val = pos + 3 < length &
                  hex-shift(hex-shift(hex-shift(hex-shift(0,contents[pos]),
                                                contents[pos + 1]),
                                      contents[pos + 2]),
                            contents[pos + 3]);
                if (val)
                  values(val, pos + 3)
                else
                  parse-error(context, "Invalid escape: %s",
                              extract-string(contents, posn, posn + 3));
                  even? := #f;
                  values(ch, posn + 1);
                end;
              end;
            end iterate;
          else
            even? := ~even?;
            values(ch, posn + 1)
          end;
        end method;

  let result-function :: false-or(<function>) = #f;
  let result-start :: <integer> = initial-position;
  let result-end :: false-or(<integer>) = #f;

  iterate loop (state :: <tokenizer-state> = initial-state,
                posn :: <integer> = initial-position)
    when (state == initial-state)
      result-function := #f;
      result-start := posn;
      result-end := #f;
    end;
    when (state.tokenizer-result)
      // It is an accepting state, so record the result and where it ended.
      result-function := state.tokenizer-result;
      result-end := posn;
    end;
    // Try advancing the state machine once more if possible.
    let table = state.tokenizer-transitions;
    let (new-state, next-pos)
      = when (posn < length)
          let (ch, next-pos) = readch(posn);
          values(table & ch <= $max-lexer-code & table[ch], next-pos)
        end;
    if (new-state)
      loop(new-state, next-pos);
    elseif (result-function)
      let string = extract-string(contents, result-start, result-end);
      values(result-function(context, string), result-end)
    elseif (result-start == length)
      values(#f, length) // EOI
    else
      let end-pos = next-pos | length;
      parse-error(context, "Invalid token: %s",
                  extract-string(contents, result-start, result-end));
      get-token-from-contents(initial-state, contents, end-pos, context);
    end;
  end iterate;
end method get-token-from-contents;

define method parse-error (context :: <object>, #rest format-args)
  apply(error, format-args)
end;
