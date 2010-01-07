Module:       collections-internals
Synopsis:     The collector protocol
Author:       Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic collector-protocol (class, #key)
  => (new-collector      :: <object>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>);

//// Default.

define inline sealed method collector-protocol
     (class == <object>, #rest options, #key)
  => (new-collector      :: <object>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  apply(collector-protocol, <list>, options)
end method;

//// Collection methods.

// Naff default method on sequences that just uses lists and converts
// at the end.

define constant <sequence-type>
  = type-union(subclass(<sequence>), <limited-sequence-type>);

define inline method collector-protocol (class :: <sequence-type>, #key)
  => (new-collector      :: <pair>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  let (new, add-first, add-last, add-seq-first, add-seq-last, collection)
    = collector-protocol(<list>);
  values(new,
         add-first,
         add-last,
         add-seq-first,
         add-seq-last,
         method (collector)
           as(class, collection(collector))
         end)
end method;

// Do the tconc thing for list collection.

define inline sealed method collector-protocol
     (class :: subclass(<list>), #key)
  => (new-collector      :: <pair>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  values(begin
           let head-pair = pair(#f, #());
	   head(head-pair) := head-pair;
         end,
         method (collector :: <pair>, value)
           let new-pair = pair(value, collector.tail);
           collector.tail := new-pair;
           value;
         end,
         method (collector :: <pair>, value)
	   let new-pair = pair(value, #());
           let c-head :: <pair> = head(collector);
	   tail(c-head) := new-pair;
	   head(collector) := new-pair;
         end,
         sequence-collection-not-yet-implemented,
         sequence-collection-not-yet-implemented,
         method (collector :: <pair>)
           collector.tail
         end)
end method;

// Just add in place for stretchy things.

define constant <stretchy-sequence-type>
//  = type-union(subclass(<stretchy-sequence>), <limited-stretchy-sequence-type>);
  = subclass(<stretchy-sequence>);

define inline method collector-protocol 
    (class :: <stretchy-sequence-type>, #key)
  => (new-collector      :: <stretchy-sequence>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  values(make(class, size: 0),
         method (collector, value)
           collector.size := collector.size + 1;
           for (i from collector.size - 2 to 0 by - 1)
             collector[i + 1] := collector[i];
           end;
           collector[0] := value
         end,
         method (collector, value)
           collector.size := collector.size + 1;
           collector[collector.size - 1] := value
         end,
         sequence-collection-not-yet-implemented,
         sequence-collection-not-yet-implemented,
         method (collector)
           collector
         end)
end method;

// Use the specific methods for <deque>.

define inline sealed method collector-protocol
     (class :: subclass(<deque>), #key)
  => (new-collector      :: <deque>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  values(make(class, size: 0),
         method (collector, value)
           push(collector, value);
         end,
         method (collector, value)
           push-last(collector, value);
         end,
         sequence-collection-not-yet-implemented,
         sequence-collection-not-yet-implemented,
         method (collector)
           collector
         end)
end method;

//// Arithmetic methods.

// Dumb. The macro should side-effect the state variable.

define class <box> (<object>)
  slot object, required-init-keyword: object:;
end class;

define method box (object) make(<box>, object: object) end;

define inline method collector-protocol 
    (class :: subclass(<number>), #key from = 0, by = \+)
  => (new-collector      :: <box>,
      add-first          :: <function>,
      add-last           :: <function>,
      add-sequence-first :: <function>,
      add-sequence-last  :: <function>,
      collection         :: <function>)
  values(box(from),
         method (collector, value) 
           collector.object := by(value, collector.object);
         end,
         method (collector, value) 
           collector.object := by(collector.object, value);
         end,
         sequence-collection-not-yet-implemented,
         sequence-collection-not-yet-implemented,
         method (collector)
           collector.object
         end)
end method;

define method sequence-collection-not-yet-implemented (collector, seq)
  error("Sorry, sequence collection not yet implemented.");
end method;
