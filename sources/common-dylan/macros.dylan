Module:       common-dylan-internals
Author:       Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Table definition macro

define macro table-definer
  { define table ?table-name:name = { ?entries } }
    => { define constant ?table-name :: <table> = make(<table>);
         begin let the-table = ?table-name; ?entries end; }
  { define table ?table-name:name :: ?table-type:name = { ?entries } }
    => { define constant ?table-name :: ?table-type = make(?table-type);
         begin let the-table = ?table-name; ?entries end; }
 entries:
  { } => { }
  { ?key:expression => ?value:expression, ... }
    => { the-table[ ?key ] := ?value; ... }
end macro table-definer;


/// Generic profiling macro

// Syntax: 
// profiling
//    (cpu-time-seconds,
//     cpu-time-microseconds,
//     allocation)
//   ...
// results
//   [bind cpu-time-seconds to CPU time in seconds]
//   [bind cpu-time-microseconds to CPU time in microseconds]
//   [bind allocation to total allocation]
//   ...
// end

/// Profiling macro

define macro profiling
  { profiling
        (?options:*)
      ?body:body
    results
      ?result-body:body
    end }
 => { do-with-profiling(method () ?body end,
                        profiling-keywords (?options) end,
                        profiling-results (?options) ?result-body end) }
end macro profiling;

define macro profiling-keywords
  { profiling-keywords
      (?options:*)
    end }
 => { vector(?options) }
 options:
   { } => { }
   { ?keyword:name, ... }
     => { ?#"keyword", ... }
   { ?keyword:name = ?args:expression, ... }
     => { ?#"keyword", ... }
end macro profiling-keywords;

define macro profiling-results
  { profiling-results (?options:*)
      ?body:body
    end } 
 => { method (state)
        ?options;
        ?body
      end }
 options:
   { } => { }
   { ?keyword:name, ... }
     => { let ?keyword = profiling-type-result(state, ?#"keyword"); ...}
   { ?keyword:name = ?args:expression, ... }
     => { let ?keyword = apply(profiling-type-result, state, ?#"keyword", ?args); ...}
end macro profiling-results;

/// Profiling protocols

define constant <profiling-state> = <object-table>;

define open generic start-profiling-type
    (state :: <profiling-state>, keyword :: <symbol>) => ();

define open generic stop-profiling-type
    (state :: <profiling-state>, keyword :: <symbol>) => ();

define open generic profiling-type-result
    (state :: <profiling-state>, keyword :: <symbol>, #key, #all-keys)
 => (value);

define function start-profiling
    (keywords :: <sequence>) => (state :: <profiling-state>)
  let state = make(<profiling-state>);
  do(curry(start-profiling-type, state), keywords);
  state
end function start-profiling;

define function stop-profiling
    (state :: <profiling-state>, keywords :: <sequence>) => ()
  do(curry(stop-profiling-type, state), keywords)
end function stop-profiling;

define function do-with-profiling
    (body :: <function>, keywords :: <sequence>, result-function :: <function>)
 => (#rest results)
  let state = start-profiling(keywords);
  block ()
    body()
  afterwards
    stop-profiling(state, keywords);
    result-function(state)
  end
end function do-with-profiling;


/// CPU time profiling

define constant <cpu-profiling-type> 
  = one-of(#"cpu-time-seconds", #"cpu-time-microseconds");

define method profiling-type-result
    (state :: <profiling-state>, keyword :: <cpu-profiling-type>, #key)
 => (seconds :: <integer>)
  state[keyword]
end method profiling-type-result;

define method start-profiling-type
    (state :: <profiling-state>, keyword :: <cpu-profiling-type>) => ()
  unless (element(state, #"cpu-profiling", default: #f))
    primitive-start-timer();
    state[#"cpu-profiling"] := #t
  end;
end method start-profiling-type;

define method stop-profiling-type
    (state :: <profiling-state>, keyword :: <cpu-profiling-type>) => ()
  when (element(state, #"cpu-profiling", default: #f))
    let elapsed-time = primitive-stop-timer();
    state[#"cpu-time-seconds"]      := elapsed-time[0];
    state[#"cpu-time-microseconds"] := elapsed-time[1];
    state[#"cpu-profiling"]         := #f
  end
end method stop-profiling-type;


/// Allocation profiling

define method start-profiling-type
    (state :: <profiling-state>, keyword == #"allocation") => ()
  // To avoid possible overflow of allocation-count
  primitive-initialize-allocation-count();
end method start-profiling-type;

define method stop-profiling-type
    (state :: <profiling-state>, keyword == #"allocation") => ()
  #f
end method stop-profiling-type;

define method profiling-type-result
    (state :: <profiling-state>, keyword == #"allocation", #key)
 => (allocation :: <integer>)
  raw-as-integer(primitive-allocation-count());
end method profiling-type-result;


/// Allocation profiling statistics

define constant $buffer-max = 8192;
define thread variable dylan-string-buffer :: <byte-string> = "";

define method start-profiling-type
    (state :: <profiling-state>, keyword == #"allocation-stats") => ()
  primitive-begin-heap-alloc-stats();
end method start-profiling-type;

define method stop-profiling-type
    (state :: <profiling-state>, keyword == #"allocation-stats") => ()
  #f
end method stop-profiling-type;

define method profiling-type-result
    (state :: <profiling-state>, keyword == #"allocation-stats",
     #key description :: <string> = "")
 => (allocation-stats)
  if (dylan-string-buffer.empty?)
    dylan-string-buffer := make(<byte-string>, size: $buffer-max, fill: '\0');
  end if;
  let actual-buffer-size :: <integer> =
    raw-as-integer(primitive-end-heap-alloc-stats(primitive-string-as-raw(dylan-string-buffer)));
  format-out("\nProfiling Results: Heap Allocation Statistics: %s\n", description);
  write-console(dylan-string-buffer, end: actual-buffer-size);
  #f
end method profiling-type-result;
