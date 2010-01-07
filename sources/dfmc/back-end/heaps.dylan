Module:   dfmc-back-end
Synopsis: Static heap modeling.
Author:   Keith Playford and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *merge-literals?* = #t;

define variable *literal-merging-stats* = #f; // make(<table>);

// Notes:
//
//   Claiming is the act of taking an object as being the responsibility
//   of the current heap. When an object is claimed, it is recorded
//   appropriately if special in some way (bindings, code, etc.) and
//   placed on the pending queue. Objects on the pending queue are 
//   internal elements of the heap that have yet to have their references
//   walked.
//
//   The bindings belonging to a particular compilation record are
//   static and evident - no descent or tracing is required to 
//   work out the set, they can just be accessed directly from
//   the record's definition objects.
//
//   Tracing is done on the root objects to determine their local
//   contents vs. references to objects created in other compilation
//   records - to tell a vector used to represent a sequence of
//   superclasses from the superclasses the vector contains for
//   example.

// Any bindings are queried in order to claim their value, so a form
// doesn't have to explicitly return any object that is reached 
// directly via that route.

// If the init method has ended up doing no computation, we don't emit it.

define function maybe-init-method 
    (init-method :: false-or(<&lambda>)) 
 => (maybe-method :: false-or(<&lambda>))
  if (init-method)
    case
      code(^iep(init-method)) 
        => init-method; // Previously emitted code?
      ~body(init-method) 
        // The no body & no code case means the method is being re-emitted,
        // except that it wasn't emitted the first time because it was 
        // empty.
        => #f;
      ~empty-method?(init-method)
        => init-method; 
      otherwise 
        => #f;
    end
  end
end function;

define class <model-heap> (<object>)

  // The known starting points of the heap.
  constant slot heap-root-init-code :: <stretchy-vector>
    = make(<stretchy-vector>);
  constant slot heap-root-system-init-code :: <stretchy-vector>
    = make(<stretchy-vector>);

  // The information derived about the heap, including its contents
  // and references to elements belonging to other compilation
  // records.

  slot heap-back-pointers :: <object-table> = make(<table>);

  slot heap-defined-bindings :: <ordered-object-set>
    = make(<ordered-object-set>);
  slot heap-referenced-bindings :: <ordered-object-set>
    = make(<ordered-object-set>);
  slot heap-defined-objects :: <ordered-object-set>
    = make(<ordered-object-set>);
  slot heap-referenced-objects :: <ordered-object-set>
    = make(<ordered-object-set>);
  slot heap-defined-repeated-object-sizes :: <object-table>
    = make(<object-table>);
  slot heap-referenced-repeated-object-sizes :: <object-table>
    = make(<object-table>);
  slot heap-load-bound-references :: <ordered-object-table>
    = make(<ordered-object-table>);
  slot heap-symbols :: <object-table> = make(<object-table>);
  slot %heap-next-id :: <integer> = 0;
  slot heap-size :: <integer> = 0;

  constant slot heap-deferred-model-references :: <object-table>
    = make(<object-table>);
  constant slot heap-record-repeated-object-sizes? :: <boolean> 
    = back-end-record-repeated-object-sizes?(current-back-end());
end class;

define function heap-defined-object-sequence (heap :: <model-heap>)
  key-sequence(heap-defined-objects(heap))
end;

define class <compilation-record-model-heap> (<model-heap>)
  constant slot heap-compilation-record :: <compilation-record>,
    required-init-keyword: compilation-record:;
  // The incoming references from other heaps within this library.
  constant slot heap-incoming-references :: <ordered-object-table>
    = make(<ordered-object-table>);
  // slot heap-original-defined-objects :: false-or(<ordered-object-set>) = #f;
end;

define class <library-model-heap> (<model-heap>)
  constant slot heap-library :: <library-description>,
    required-init-keyword: library:;
end;

define method heap-approximate-size
    (heap :: <model-heap>) => (res :: <integer>)
  size(heap-defined-bindings(heap))
    + size(heap-referenced-bindings(heap))
    + size(heap-defined-objects(heap))
    + size(heap-referenced-objects(heap))
end method;

define method print-object (o :: <library-model-heap>, stream :: <stream>) => ()
  format(stream, "{model-heap of %s}", o.heap-library);
end method;

define method heap-library
    (heap :: <compilation-record-model-heap>) => (ld :: <library-description>)
  compilation-record-library(heap-compilation-record(heap))
end;

define method heap-compilation-record
    (heap :: <library-model-heap>) => (cr :: <library-compilation-record>)
  heap.heap-library.library-description-combined-record
end;

define method heap-next-id (heap :: <model-heap>) => (number :: <integer>)
  let next-id = heap.%heap-next-id;
  heap.%heap-next-id := next-id + 1;
  next-id
end method;

define variable *heap-record-back-pointers?* = #f;

define sideways method retract-compilation-record-heap 
    (cr :: <compilation-record>) => ()
  unless (*heap-record-back-pointers?*)
    let heap = compilation-record-model-heap(cr);
    if (heap)
      // Get rid of all pointers to the heap.
      // Nowadays, there aren't any...
      compilation-record-model-heap(cr) := #f;
    end;
  end unless;
  cr.compilation-record-heap-referenced-objects := #f;
  cr.compilation-record-needs-linking? := #t;
end method;

/*
define variable *dbg?* = #f;

define method dbg? (name :: <symbol>)
  *dbg?* == #t | name == *dbg?*
end;

define method dbg? (model)
  *dbg?* &
  select (model by instance?)
    <byte-string> => dbg?(as(<symbol>, model));
    <pair> => dbg?(model.head) | dbg?(model.tail);
    <vector> => any?(dbg?, model);
    <variable-name-fragment> => dbg?(model.fragment-identifier);
    <module-binding> => dbg?(model.name);
    <top-level-form> => dbg?(model.form-variable-name);
    <&iep> => dbg?(model.function);
    <&mm-wrapper> => dbg?(model.^mm-wrapper-implementation-class.^iclass-class);
    otherwise => model & ~direct-object?(model)
	           & dbg?(model.model-definition | model.^debug-name);
  end;
end method;
*/

define thread variable *precomputing-heap?* :: <boolean> = #f;

// Used by loosely-link-library-heaps, and by tightly-link-library-heaps when
// combining, and also for interactive layer, and maybe-recompute-library-heaps
// (which is used by ensure-library-stripped)
define method compute-and-install-compilation-record-heap
    (cr :: <compilation-record>, #rest flags) => ()
  install-compilation-record-heap(cr);
  apply(compute-compilation-record-heap, cr, flags);
end method;

// Used by tightly-link-library-heaps, when not combining, to compute
// heap-incoming-references.
define method precompute-library-heaps (ld :: <project-library-description>)
  dynamic-bind (*precomputing-heap?* = #t)
    let cr* = ld.compilation-context-records;
    do(install-compilation-record-heap, cr*);
    for (cr in cr*)
      compute-compilation-record-heap(cr, skip-emit?: #t);
    end;
  end;
end;

// Used by tightly-link-library-heaps when not combining.
define method compute-library-reachable-heap (ld :: <project-library-description>)
  let cr* = ld.compilation-context-records;
  do(compute-compilation-record-reachable-heap, cr*);
  do(process-heap-deferred-models, cr*);
end;

// in loose link, or preheaping, or combining.
define method compute-compilation-record-heap
    (cr :: <compilation-record>, #rest flags, #key skip-emit?, #all-keys)
 => ()
  with-dependent ($compilation of cr)
    trace-heap-from-roots(compilation-record-model-heap(cr));
  end;
  process-heap-deferred-models(cr);

  unless (skip-emit?)
    apply(emit-compilation-record-heap, cr, flags);
  end;
  let heap = compilation-record-model-heap(cr);
  compilation-record-approximate-model-heap-size(cr) 
    := heap-approximate-size(heap);
  compilation-record-data-size(cr) 
    := heap-size(heap) * word-size();
  when (compilation-record-interactive?(cr)
	  | library-forms-dynamic?(compilation-record-original-library(cr)))
    compilation-record-heap-referenced-objects(cr)
      := as(<vector>, heap-referenced-objects(heap));
  end when;
  // CHECK-HEAP(cr);
  when (*precomputing-heap?*)
    // Reset the walking machinery, just wanted to the incoming references.
    // heap.heap-original-defined-objects := heap.heap-defined-objects;
    heap.heap-back-pointers := make(<table>);
    heap.heap-defined-bindings := make(<ordered-object-set>);
    heap.heap-referenced-bindings := make(<ordered-object-set>);
    heap.heap-defined-objects := make(<ordered-object-set>);
    heap.heap-referenced-objects := make(<ordered-object-set>);
    heap.heap-defined-repeated-object-sizes := make(<object-table>);
    heap.heap-referenced-repeated-object-sizes := make(<object-table>);
    heap.heap-load-bound-references := make(<ordered-object-table>);
    heap.heap-symbols := make(<object-table>);
    heap.%heap-next-id := 0;
    heap.heap-size := 0;
  end;
end method;

// tight link non-combining real heaping pass
define method compute-compilation-record-reachable-heap
    (cr :: <compilation-record>) => ()
  with-dependent ($compilation of cr)
    trace-heap-from-roots(compilation-record-model-heap(cr),
			  incoming?: #t);
  end with-dependent;
  // CHECK-HEAP(cr);
end method;


define method emit-compilation-record-heap
    (cr :: <compilation-record>, #rest flags)
  with-dependent ($compilation of cr)
    apply(emit-all, current-back-end(), cr, flags);
  end with-dependent;
end;
  

/*
define function CHECK-HEAP (cr :: <compilation-record>)
  let heap = compilation-record-model-heap(cr);
  let objects = heap.heap-referenced-objects;
  let object-names = make(<table>);
  for (object in objects)
    if (instance?(object, <&class>))
      let binding = model-variable-binding(object);
      if (binding)
	let info = element(object-names, binding, default: #());
	object-names[binding] := pair(object, info);
	unless (info == #())
	  error("Duplicate objects for %s:%s", binding, object-names[binding]);
	end;
      end;
    end;
  end;
end function;
*/

define method install-compilation-record-heap 
    (cr :: <compilation-record>) => ()
  // Make sure any old one is properly retracted and that old object file
  // is marked obsolete (i.e. cr-needs-linking? is set)
  with-dependent ($compilation of cr)
    retract-compilation-record-heap(cr);
    cr.compilation-record-preceeding-line-count := 0;
    compilation-record-model-heap(cr)
      := make(<compilation-record-model-heap>, compilation-record: cr)
  end with-dependent;
end;

define method install-compilation-record-heap
    (combined-cr :: <library-compilation-record>) => ()
  let ld = combined-cr.compilation-record-library;
  local method cr-lines (cr :: <compilation-record>)
	  let lines = cr.compilation-record-source-line-count | 0;
	  // For some reason, this count seems to be consistently off by 2.
	  let fudged-lines = if (lines >= 2) lines - 2 else lines end;
	  fudged-lines
	    + cr.compilation-record-source-record.source-record-start-line
	end;
  for (cr in ld.library-description-compilation-records,
       lines = 0 then lines + cr-lines(cr))
    cr.compilation-record-preceeding-line-count := lines
  end;
  compilation-record-model-heap(combined-cr)
    := make(<library-model-heap>, library: ld);
end method;


define method form-created-bindings (form :: <top-level-form>)
  form-defined-bindings(form)
end method;

define method form-created-bindings (form :: <modifying-form>)
  #()
end method;


define function claim-init-method (heap :: <model-heap>,
				   code :: false-or(<&lambda>),
				   system? :: <boolean>)
  when (code)
    mark-emitted-name(heap, code);
    maybe-claim-computations-references(heap, code, #f);
  end;
end;

define function claim-init-form (heap :: <model-heap>, form :: <top-level-form>)
 => (claimed? :: singleton(#t))
  let (init, sys-init) = form-init-code(form);
  claim-init-method(heap, init, #f);
  claim-init-method(heap, sys-init, #t);
  #t
end claim-init-form;
  
define function claim-compilation-record-roots
    (heap :: <model-heap>, cr :: <compilation-record>)
  for (form :: <top-level-form> in compilation-record-top-level-forms(cr))
    claim-form-roots(heap, form)
  end;
end claim-compilation-record-roots;

define inline function claim-form-roots
    (heap :: <model-heap>, form :: <top-level-form>)
 => (object);
  let bindings = form-created-bindings(form);
  unless (empty?(bindings) & form-ignored?(form))
    for (binding :: <module-binding> in bindings)
      unless (binding-previously-defined?(binding))
	if (*precomputing-heap?* | model-externally-visible?(binding))
	  maybe-claim-heap-element(heap, #f, binding, #t);
	end;
      end;
    end;
    if (*precomputing-heap?*)
      claim-init-form(heap, form);
      let model = instance?(form, <modifying-form>) & form-model(form);
      // When pre-heaping, need to note external references from the
      // method in this heap in case end up claiming the method via
      // generic-function-methods from a generic in another heap
      when (model) maybe-claim-heap-element(heap, #f, model, #t) end;
    else
      let (init, system-init) = form-init-code(form);
      when (init | system-init)
	unless (process-pending-init-form(heap, form))
	  make-init-form-pending(heap, form);
	end;
      end;
    end;
  end;
end claim-form-roots;

define method process-pending-init-form (heap :: <model-heap>,
					 form :: <top-level-form>)
 => (processed? :: <boolean>)
  claim-init-form(heap, form);
end process-pending-init-form;

define method process-pending-init-form (heap :: <model-heap>,
					 form :: <modifying-form>)
 => (processed? :: <boolean>)
  // For a modifying form (method or domain) any init code just installs
  // the object with varying degrees of runtime checking.  If we can decide
  // that the object itself will not be created, we can ignore the
  // init form.
  let model = form-model(form);
  when (~model |
	  model-externally-visible?(model) |
	  model-externally-accessible?(heap, model))
    // TODO: Why do we do this?  Should we only export if the generic
    // is exported?  How else could somebody get access to it?
    when (model) maybe-export(heap, model, #t) end;
    claim-init-form(heap, form);
  end;
end process-pending-init-form;

  
// If true, assume constant/variable init forms are side-effect-free, and
// hence can be ignored when the variable they initialize is ignored.
define variable *assume-side-effect-free-init-forms?* = #f;

define method process-pending-init-form (heap :: <model-heap>,
					 form :: <binding-defining-form>)
 => (processed? :: <boolean>)
  // The init code in define constant/variable can be ignored if
  // (1) none of the bindings defined by the form are actually created
  // and (2) the init code is side-effect-free except for setting the
  // bindings it defines...
  // Note that for thread variables, the init code actually allocates
  // the storage, not just sets it...  However, if the binding is
  // never actually referenced, we should be able to remove it.
  // And if the binding is ever referenced, it should get claimed.
  // so how come dylan-string-buffer seems unclaimed (init gets zappd)
  // but the functions referencing it seem to be around???
  // (display-class-breakpoints).
  when (~*assume-side-effect-free-init-forms?* |
	  any?(method (binding :: <module-binding>)
		 debug-assert(internal-binding?(heap, binding),
			      "Local form with non-local binding?");
		 heap-element-claimed?(heap, binding)
	       end,
	       form-defined-bindings(form)))
    claim-init-form(heap, form)
  end;
end process-pending-init-form;

define method process-pending-init-form (heap :: <library-model-heap>,
					 form :: <generic-definition>)
 => (processed? :: <boolean>)
  // A generic definition is just like a define constant...
  let binding = form-variable-binding(form);
  debug-assert(internal-binding?(heap, binding), "Local form with non-local binding?");
  when (heap-element-claimed?(heap, binding))
    claim-init-form(heap, form)
  end;
end process-pending-init-form;

define method process-pending-init-form (heap :: <model-heap>,
					 form :: <class-definition>)
 => (processed? :: <boolean>)
  // The init code in a define class can be ignored if the class
  // itself is not created, since it just installs the class (e.g. in
  // subclass lists of external superclasses).
  let binding = form-variable-binding(form);
  debug-assert(internal-binding?(heap, binding), "Local form with non-local binding?");  
  when (heap-element-claimed?(heap, binding) |
	  begin
	    let class = binding-model-object(binding);
	    debug-assert(~class | internal-object?(heap, class),
			 "Local class with non-local model?");
	    ~class | heap-element-claimed?(heap, class)
	  end)
    claim-init-form(heap, form)
  end;
end process-pending-init-form;

define inline method claim-heap-roots (heap :: <compilation-record-model-heap>)
  let cr = heap-compilation-record(heap);
  when (first-compilation-record?(cr))
    let ld = compilation-record-library(cr);
    claim-init-method(heap, library-description-system-class-init-code(ld), #f);
    claim-init-method(heap, library-description-system-gf-init-code(ld), #f);
  end;
  claim-compilation-record-roots(heap, cr);
end claim-heap-roots;

define inline method claim-heap-roots (heap :: <library-model-heap>)
  let ld = heap.heap-library;

  claim-init-method(heap, library-description-system-class-init-code(ld), #f);
  claim-init-method(heap, library-description-system-gf-init-code(ld), #f);

  for (cr in ld.library-description-compilation-records)
    claim-compilation-record-roots(heap, cr);
  end for;
end claim-heap-roots;


define function make-init-form-pending (heap :: <model-heap>, form)
  *heap-pending*.heap-pending-init-forms
    := add(*heap-pending*.heap-pending-init-forms, form);
end;

define inline-only function first-compilation-record? (cr :: <compilation-record>)
  cr == first(library-description-compilation-records(compilation-record-library(cr)))
end function;
  

define method add-heap-init-code
    (heap :: <compilation-record-model-heap>,
     code :: <stretchy-vector>,
     system-code :: <stretchy-vector>,
     exceptions :: <object-set>)
  let cr = heap.heap-compilation-record;
  when (first-compilation-record?(cr))
    add-whole-library-init-code(compilation-record-library(cr),
				code, system-code);
  end;
  add-compilation-record-init-code(cr, code, system-code, exceptions);
end add-heap-init-code;

define method add-heap-init-code
    (heap :: <library-model-heap>,
     code :: <stretchy-vector>,
     system-code :: <stretchy-vector>,
     exceptions :: <object-set>)
  let ld = heap.heap-library;
  add-whole-library-init-code(ld, code, system-code);
  for (cr in library-description-compilation-records(ld))
    add-compilation-record-init-code(cr, code, system-code, exceptions);
  end;
end add-heap-init-code;

define inline function add-whole-library-init-code
    (ld :: <library-description>, code, system-code)
  let class-init = library-description-system-class-init-code(ld);
  when (class-init) add!(code, class-init) end;
  let gf-init = library-description-system-gf-init-code(ld);
  when (gf-init) add!(code, gf-init) end;
end add-whole-library-init-code;

define inline function add-compilation-record-init-code
    (cr :: <compilation-record>, code, system-code, exceptions)
  for (form in compilation-record-top-level-forms(cr))
    unless (form-ignored?(form) | member?(form, exceptions))
      let (init, system-init) = form-init-code(form);
      when (init) add!(code, init) end;
      when (system-init) add!(system-code, system-init) end;
    end;
  end;
end add-compilation-record-init-code;


define function form-init-code
    (form :: <top-level-form>) => (init, system-init)
  let init = maybe-init-method(form.form-init-method);
  let sys-init = maybe-init-method(form.form-system-init-method);
  debug-assert(~(init | sys-init) | ~form-compile-stage-only?(form),
	       "Compile-stage form %s with non-empty inits!", form);
  values(init, sys-init)
end;


define class <heap-pending> (<object>)
  constant slot heap-pending-heap :: <model-heap>,
    required-init-keyword: heap:;
  constant slot heap-pending-elements :: <deque> = make(<deque>);
  constant slot heap-compile-time-references :: <object-set> = make(<object-set>);
  constant slot heap-compile-time-elements :: <object-set> = make(<object-set>);
  slot heap-pending-init-forms :: <list> = #();
  slot heap-pending-generics :: <pending-generic-info> = #f;
  constant slot heap-merged-literals :: false-or(<literal-table>)
    = *merge-literals?* & ~*precomputing-heap?* & make(<literal-table>);
end;

define thread variable *heap-pending* :: false-or(<heap-pending>) = #f;

// when combining:	incoming? = #f, *precomputing-heap?* = #f
// when not combining (tight mode):
//    pre-heaping:	incoming? = #f, *precomputing-heap?* = #t
//    heaping:          incoming? = #t, *precomputing-heap?* = #f
// When loose-mode/interactive
//    heaping:          incoming? = #f, *precomputing-heap?* = #f
define method trace-heap-from-roots (heap :: <model-heap>, #key incoming? = #f)
 => ()
  debug-out(#"heap", "Trace %= from roots preheaping?=%s incoming?=%s\n",
	    heap.heap-compilation-record, *precomputing-heap?*, incoming?);
  dynamic-bind (*heap-pending* = make(<heap-pending>, heap: heap))
    when (~incoming? & *literal-merging-stats*)
      *literal-merging-stats* := make(<table>);
    end;
    // Seed the tracing process with the known roots.
    claim-heap-roots(heap);

    when (incoming?)
      for (ct-ref? keyed-by element in heap-incoming-references(heap))
	// Even though we can't go back and change the original pointers in
	// other CR's, these objects must participate in literal merging
	// to avoid the case where non-eq incoming and internal versions
	// of an object get defined, causing link-time conflicts.  Since at
	// link-time, incoming references are by name only, it's not important
	// to change the original pointers in other CR's.
	let element = maybe-merge-literal(element);
	maybe-claim-heap-element-derived(heap, #f, element, ct-ref?);
      end;
    end;

    // All compilation units need these because the back-end inserts 
    // references to them that may not be evident in the code.
    maybe-claim-heap-element(heap, #f, dylan-value(#"<class>"), #f);
    maybe-claim-heap-element(heap, #f, dylan-value(#"<object>"), #f);
    maybe-claim-heap-element(heap, #f, &unbound, #f);
    maybe-claim-heap-element(heap, #f, #t, #f);
    maybe-claim-heap-element(heap, #f, #f, #f);

    drain-pending-elements(heap);

    // store init code for the linker.
    unless (*precomputing-heap?*)
      let code = heap.heap-root-init-code;
      let system-code = heap.heap-root-system-init-code;
      let exceptions = make(<object-set>);
      for (form in *heap-pending*.heap-pending-init-forms)
	add!(exceptions, form);
      end;
      add-heap-init-code(heap, code, system-code, exceptions);
    end;

/*
     unless (empty?(*heap-pending*.heap-pending-init-forms))
       for (form in *heap-pending*.heap-pending-init-forms)
	 format-out(">>>>>> Zapped init form: %=\n", form);
       end;
     end;
*/

  end dynamic-bind;
  // Record the repeated sizes of heap members.
  // Note: This is now done as a post-pass since it requires the class
  // of the objects to be available and initialized, an assumption that
  // causes false circularities on classes in the Dylan library now that
  // heap roots are determined as models are computed. Because this
  // is only a problem in the Dylan library, which has no external
  // references, external reference repeated sizes are still recorded
  // as we go along.
  for (object in heap-defined-object-sequence(heap))
    // HACK: THIS SHOULD BE A SUBCLASS OF A CATEGORY THAT IS NOT A MODEL
    unless (instance?(object, <&iep>))
      record-repeated-size(heap, object);
    end unless;
  end;
end method;

define method element-compile-stage-only? (e :: <module-binding>)
  let form = untracked-binding-definition(e, default: #f);
  form & form-compile-stage-only?(form)
end;

define method element-compile-stage-only? (e)
  ~direct-object?(e) & model-compile-stage-only?(e)
end;

define method element-compile-stage-only? (e :: <&iep>)
  element-compile-stage-only?(e.function)
end;

define method element-compile-stage-only? (e :: <&mm-wrapper>)
  element-compile-stage-only?(e.^mm-wrapper-implementation-class.^iclass-class)
end;

// TODO: this is a workaround for form-compile-stage-only? being false for
// domains on compile-time-only methods/generics.  Should fix form-compile-
// stage-only(<method-definition>) to be on <modifying-form> instead.
define method element-compile-stage-only? (e :: <&domain>)
  let binding = model-variable-binding(e);
  element-compile-stage-only?(binding)
end;

// TODO: This is a workaround for raw slot accessors - form-compile-stage-only?
// is true for them, but they are referenced from the class (via the slot
// descriptor) so they must exist at run-time if the class exists at run-time
define method element-compile-stage-only? (e :: <&generic-function>)
  let form = model-definition(e);
  if (form & form-compile-stage-only?(form))
    let parent = form-parent-form(form);
    ~instance?(parent, <slot-definition>)
      | form-compile-stage-only?(form-parent-form(parent))
  end;
end;

define inline method remove-if! (list :: <list>, remove? :: <function>)
  iterate loop (list :: <list> = list, last :: false-or(<list>) = #f, this :: <list> = list)
    if (this == #())
      list
    else
      let object = this.head;
      let next = this.tail;
      if (~remove?(object))
	loop(list, this, next)
      elseif (last == #f)
	loop(next, last, next)
      else
	last.tail := next;
	loop(list, last, next)
      end;
    end if;
  end iterate;
end remove-if!;

define function drain-pending-elements (heap :: <model-heap>)
  while (~empty?(*heap-pending*.heap-pending-elements))
    let e = pop(*heap-pending*.heap-pending-elements);
    let ct? = member?(e, *heap-pending*.heap-compile-time-elements);
    maybe-claim-heap-element-references(heap, e, ct?);
  end;
  process-pending-init-forms(heap);
  process-pending-generic-models(heap);
  if (~empty?(*heap-pending*.heap-pending-elements))
    drain-pending-elements(heap)
  else
    unless (*precomputing-heap?*)
      // this should only add <pair>'s to the pending elements,
      // everything else should be already claimed.
      remove-unclaimed-pending-models(heap);
      if (~empty?(*heap-pending*.heap-pending-elements))
	drain-pending-elements(heap)
      end;
    end;
  end;
end function drain-pending-elements;


define method maybe-claim-generic-function-modifying-models
    (heap :: <model-heap>, gf :: <&generic-function>)
  let methods = ^generic-function-methods(gf);
  let domains = if (instance?(gf, <&incremental-generic-function>))
		  ^generic-function-domains(gf)
		else // else domains not referenced at runtime...
		  #()
		end;
  if (*precomputing-heap?*)
    do-claim-generic-function-modifying-models(heap, gf);
  else
    let methods = choose(method (m)
			   ~process-generic-function-model(heap, gf, m)
			 end,
			 methods);
    let domains = choose(method (d)
			   ~process-generic-function-model(heap, gf, d)
			 end,
			 domains);
    if (methods == #() & domains == #())
      do-claim-generic-function-modifying-models(heap, gf);
    else
      *heap-pending*.heap-pending-generics
	:= make(<non-empty-pending-generic-info>,
		function: gf,
		methods: methods,
		domains: domains,
		next: *heap-pending*.heap-pending-generics)
    end;
  end;
end maybe-claim-generic-function-modifying-models;

define method do-claim-generic-function-modifying-models
    (heap :: <model-heap>, gf :: <&generic-function>)
  maybe-claim-heap-element(heap, gf, gf.^generic-function-methods, #f);
end do-claim-generic-function-modifying-models;

define method do-claim-generic-function-modifying-models
    (heap :: <model-heap>, gf :: <&incremental-generic-function>)
  maybe-claim-heap-element(heap, gf, gf.^generic-function-methods, #f);  
  // TODO: The way generic function domains are stored, only the last one is
  // actually referenced directly from the gf.
  for (domain in gf.^generic-function-domains)
    maybe-claim-heap-element(heap, gf, domain, #f);
  end;
end do-claim-generic-function-modifying-models;



define method claim-generic-model
    (heap :: <model-heap>, gf :: <&generic-function>, model)
  when (model-externally-visible?(gf))
    maybe-export(heap, model, #f);
  end;
  maybe-claim-heap-element(heap, gf, model, #f);
  debug-assert(begin
		 let form = model.model-definition;
		 ~form | begin
			   let (init, sys-init) = form-init-code(form);
			   ~init & ~sys-init
			 end
	       end,
	       "Init form on non-ct generic model %s",
	       format-to-string("%s", model.model-definition));
end;

define constant <pending-generic-info> = false-or(<non-empty-pending-generic-info>);

define class <non-empty-pending-generic-info> (<object>)
  constant slot pending-generic-function, required-init-keyword: function:;
  slot pending-generic-unclaimed-methods :: <list>, required-init-keyword: methods:;
  slot pending-generic-unclaimed-domains :: <list>, required-init-keyword: domains:;
  slot pending-generic-next :: <pending-generic-info>, required-init-keyword: next:;
end;
define sealed-constructor <non-empty-pending-generic-info>;

define inline method remove-if! (pgfs :: <pending-generic-info>, remove? :: <function>)
  iterate loop (all :: <pending-generic-info> = pgfs,
		last :: <pending-generic-info> = #f,
		this :: <pending-generic-info> = pgfs)
    if (this == #f)
      all
    else
      let next = this.pending-generic-next;
      if (~remove?(this))
	loop(all, this, next)
      elseif (last == #f)
	loop(next, last, next)
      else
	last.pending-generic-next := next;
	loop(all, last, next)
      end;
    end if;
  end iterate;
end method remove-if!;
				 
  
define method process-generic-function-model
    (heap :: <model-heap>, gf :: <&generic-function>, model)
 => (processed? :: <boolean>)
  when (model-externally-visible?(model) |
	  model-externally-accessible?(heap, model))
    // Claim it and remove from list.
    claim-generic-model(heap, gf, model);
    #t
  end when;
end process-generic-function-model;

define function process-pending-generic-models (heap :: <model-heap>)
  let pgfs = *heap-pending*.heap-pending-generics;
  *heap-pending*.heap-pending-generics := #f;
  let rest
    = remove-if!(pgfs,
		 method (pgf :: <non-empty-pending-generic-info>)
		   let gf = pgf.pending-generic-function;
		   pgf.pending-generic-unclaimed-methods
		     := remove-if!(pgf.pending-generic-unclaimed-methods,
				   curry(process-generic-function-model,
					 heap, gf));
		   pgf.pending-generic-unclaimed-domains
		     := remove-if!(pgf.pending-generic-unclaimed-domains,
				   curry(process-generic-function-model,
					 heap, gf));
		   when (pgf.pending-generic-unclaimed-methods == #() &
			 pgf.pending-generic-unclaimed-domains == #())
		     // Claim them and remove from list
		     do-claim-generic-function-modifying-models(heap, gf);
		     #t
		   end;
		 end method);
  // Concatenate
  iterate loop (last = #f, this = *heap-pending*.heap-pending-generics)
    if (this)
      loop(this, this.pending-generic-next)
    elseif (last)
      last.pending-generic-next := rest
    else
      *heap-pending*.heap-pending-generics := rest
    end
  end;
end process-pending-generic-models;

define function remove-unclaimed-pending-models (heap :: <model-heap>)
  while (*heap-pending*.heap-pending-generics)
    let pgf = *heap-pending*.heap-pending-generics;
    *heap-pending*.heap-pending-generics := pgf.pending-generic-next;
    remove-unclaimed-pending-models-1
      (heap, pgf.pending-generic-function,
             pgf.pending-generic-unclaimed-methods,
             pgf.pending-generic-unclaimed-domains);
  end;
end remove-unclaimed-pending-models;

define method remove-unclaimed-pending-models-1
    (heap :: <model-heap>, gf :: <&generic-function>, methods, domains)
  debug-assert(gf.%generic-function-methods-initialized?, "Uninitialized gf?");
  gf.%generic-function-methods
    := choose(method (m) ~member?(m, methods) end,
	      gf.%generic-function-methods);
  do-claim-generic-function-modifying-models(heap, gf);
end remove-unclaimed-pending-models-1;

define method remove-unclaimed-pending-models-1
    (heap :: <model-heap>, gf :: <&incremental-generic-function>, methods, domains)
  debug-assert(gf.%generic-function-domains-initialized?, "Uninitialized gf?");
  gf.%generic-function-domains
    := choose(method (d) ~member?(d, domains) end,
	      gf.%generic-function-domains);
  for (d :: <&domain> in gf.%generic-function-domains,
       a = #f then begin ^domain-next(d) := a; d end)
  finally ^incremental-gf-domain-info(gf) := a
  end for;
  next-method();
end remove-unclaimed-pending-models-1;


define function process-pending-init-forms (heap :: <model-heap>)
  // I don't think currently init forms can get added during claiming, but
  // for the sake of generality, allow for that.
  let forms :: <list> = *heap-pending*.heap-pending-init-forms;
  *heap-pending*.heap-pending-init-forms := #();
  let rest = remove-if!(forms, curry(process-pending-init-form, heap));
  *heap-pending*.heap-pending-init-forms
    := concatenate!(*heap-pending*.heap-pending-init-forms, rest);
end process-pending-init-forms;

define method model-externally-accessible? (heap :: <model-heap>, model :: <&domain>)
  // TODO: maybe can skip domain auto-generated for sealed methods if the
  // method is skipped.
  let types = ^domain-types(model);
  ~types | block (return)
	     for (type in types,
		  count from 0 below model.^domain-number-required)
	       unless (type-can-have-instances?(heap, type))
		 return(#f)
	       end;
	     end;
	     #t
	   end;
end;

define method model-externally-accessible? (heap :: <model-heap>, model :: <&method>)
  // A method is externally accessible only if all of its specializers
  // can have instances.  If any of the specializers can't have instances,
  // then the method can never be applicable, and  so it might as well
  // not exist.
  let sig = ^function-signature(model);
  ~sig | block (return)
	   for (type in sig.^signature-required,
		count from 0 below sig.^signature-number-required)
	     unless (type-can-have-instances?(heap, type))
	       // If a specializer can't have instances, the method can't possibly
	       // be applicable, so it might as well not exist.
	       return(#f);
	     end;
	   end;
	   #t
	 end;
end;

define method type-can-have-instances? (heap :: <model-heap>, type)
  #t
end;

define method type-can-have-instances? (heap :: <model-heap>, class :: <&class>)
  ~internal-object?(heap, class) | heap-element-claimed?(heap, class)
end;

define method type-can-have-instances? (heap :: <model-heap>, type :: <&singleton>)
  let object = ^singleton-object(type);
  ~instance?(object, <&class>)
    | ~internal-object?(heap, object) | heap-element-claimed?(heap, object)
end;

define method type-can-have-instances? (heap :: <model-heap>, type :: <&subclass>)
  let class = ^subclass-class(type);
  ~internal-object?(heap, class) | heap-element-claimed?(heap, class)
end;

define method type-can-have-instances? (heap :: <model-heap>, type :: <&union>)
  type-can-have-instances?(heap, type.^union-type1)
    | type-can-have-instances?(heap, type.^union-type2)
end;

////  Deferred models

// *** DEBUGGING
define method maybe-claim-heap-element (heap ::<model-heap>,
					parent,
					e :: <heap-deferred-model>,
					ct-ref?)
  error("Who is claiming %= from %=?", e, parent)
end;

define method record-deferred-model-reference
    (heap :: <model-heap>, ref, model :: <heap-deferred-model>)
  let table = heap.heap-deferred-model-references;
  table[model] := add-new!(element(table, model, default: #()), ref, test: \=);
end;

define method install-deferred-model-reference
    (heap :: <model-heap>, ref :: <pair>, value)
  element(ref.head, ref.tail) := value
end;

define method install-deferred-model-reference
    (heap :: <model-heap>, ref :: <object-reference>, value)
  ref.reference-value := value
end;

define method process-heap-deferred-models (cr :: <compilation-record>)
  with-dependent ($compilation of cr)
    let heap = cr.compilation-record-model-heap;
    for (refs keyed-by model in heap.heap-deferred-model-references)
      let value = compute-heap-deferred-model(heap, model);
      for (ref in refs)
	install-deferred-model-reference(heap, ref, value)
      end;
    end;
  end;
end;

define method compute-heap-deferred-model
    (heap :: <model-heap>, model :: <heap-deferred-all-classes-model>)
 => (all-classes :: <simple-object-vector>)
  let ld = heap-library(heap);
  let all-classes = make(<stretchy-object-vector>);
  for (obj in ld.library-externally-visible-models)
    when (instance?(obj, <&class>) & ~instance?(obj, <virtual-object>)
	    & ~heap-imported-object?(heap, obj))
      all-classes := add!(all-classes, obj);
    end;
  end;
  let all-classes = as(<simple-object-vector>, all-classes);
  // Nothing really cares about the order, but force an order so can
  // have reproducible heaping.
  sort!(all-classes, test: method (c1 :: <&class>, c2 :: <&class>)
			     defined-after?(c1.model-definition, c2.model-definition)
			   end);

  // Assign class dispatch keys in the dylan library.  Dispatch keys are integers, so
  // they don't need to get claimed, so it's ok to assign them this late.
  when (compiling-dylan-library?())
    for (obj in all-classes, index :: <integer> from 0 by 1)
      let key :: <integer> = ^iclass-number-to-key(index);
      ^iclass-dispatch-key(^class-implementation-class(obj)) := key;
    end;
  end;
  // Update direct-subclasses slots
  local method mark-carefully (heap, parent, obj)
	  if (heap-imported-object?(heap, obj))
	    record-external-heap-element-reference(heap, parent, obj, #f);
	  elseif (~internal-object?(heap, obj))
	    debug-assert(heap-element-referenced?(heap, obj, #f),
			"Introducing new reference in all-class computation?");
	  else
	    mark-heap-element(heap, parent, obj);
	    let &class = &object-class(obj);
	    debug-assert(heap-imported-object?(heap,  &class)
			   | model-externally-visible?(&class),
			 "Introducing new object in all-class computation?");
	    maybe-claim-heap-element(heap, obj, &class, #f);
	  end;
	end method;
  for (obj in all-classes)
    let ic :: <&implementation-class> = obj.^class-implementation-class;
    // debug-assert(%direct-subclasses-initialized?(ic));
    let subclasses :: <list> = ic.^direct-subclasses;
    let claimed-subclasses
      = if (every?(model-externally-visible?, subclasses))
	  subclasses // don't copy...
	else
	  mapped-model(choose(model-externally-visible?, subclasses));
	end;
    ic.^direct-subclasses := claimed-subclasses;
    let ic-heap = if (internal-object?(heap, ic)) // includes combined heap case
		    heap
		  else
		    record-external-heap-element-reference(heap, #f, obj, #f);
		    ic.model-compilation-record.compilation-record-model-heap;
		  end;
    for (pair = claimed-subclasses then pair.tail, until: pair == #())
      mark-carefully(ic-heap, ic, pair);
      let subc = pair.head;
      if (internal-object?(ic-heap, subc))
	debug-assert(heap-element-claimed?(ic-heap, subc));
      else
	record-external-heap-element-reference(ic-heap, ic, subc, #f);
      end;
    finally
      mark-carefully(ic-heap, ic, #());
    end;
  end for;
  let classvec = immutable-model(all-classes);
  record-repeated-size(heap, classvec);
  mark-carefully(heap, #f, classvec);
  classvec
end method;
  
define method record-repeated-size-explicitly 
    (heap :: <model-heap>, class, size) => ()
  when (heap-record-repeated-object-sizes?(heap))
    let sizes :: <function>
      = if (internal-object?(heap, class))
	  heap-defined-repeated-object-sizes
	else
	  heap-referenced-repeated-object-sizes
	end if;
    heap.sizes[class] 
      := add-new!(element(heap.sizes, class, default: #()), size);
  end when;
end method;

define method record-repeated-size (heap :: <model-heap>, object) => ()
  when (heap-record-repeated-object-sizes?(heap))
    let class = object.&object-class;
    let rslotd = class.^repeated-slot-descriptor;
    if (rslotd)
      let value = ^slot-value(object, rslotd.^size-slot-descriptor);
      record-repeated-size-explicitly(heap, class, value);
    end if;
  end when;
end method;

define method record-heap-load-bound-reference 
    (heap :: <model-heap>, object, ref) => ()
  let ref-table = heap-load-bound-references(heap);
  let refs = element(ref-table, object, default: #());
  ref-table[object] := pair(ref, refs);
end method;

/// INTERNAL ALREADY TRACED OBJECT DETERMINATION

define inline method internal-object? 
    (heap :: <compilation-record-model-heap>, object)
  // TODO: SHOULD BE DEBUG-ASSERT
  // if (instance?(object, <&implementation-class>)
  // 	& object.model-compilation-record ~== heap.heap-compilation-record)
  //   break("About to assert implementation-class %= external.", ^iclass-class(object))
  // end if;
  object.model-compilation-record == heap.heap-compilation-record
end;


define inline method internal-binding?
    (heap :: <compilation-record-model-heap>, object :: <module-binding>)
  object.binding-compilation-record == heap.heap-compilation-record
end;

/// IMPORTED FROM ANOTHER LIBRARY DETERMINATION

define inline function library-imported-object? (ld :: <library-description>, object)
  model-library(object) ~== ld
end function;

define inline function library-imported-binding? 
    (ld :: <library-description>, object :: <module-binding>)
  debug-assert(valid-binding-home-library-in?(ld, object));
  binding-imported-into-library?(object);
end function;

define inline function heap-imported-object? (heap :: <model-heap>, object)
  library-imported-object?(heap-library(heap), object)
end function;

define inline function heap-imported-binding? 
    (heap :: <model-heap>, object :: <module-binding>)
  library-imported-binding?(heap-library(heap), object)
end function;


define inline method internal-object? 
    (heap :: <library-model-heap>, object)
  // TODO: SHOULD BE DEBUG-ASSERT
  // if (instance?(object, <&implementation-class>)
  // 	& heap-imported-object?(heap, object))
  //   break("About to assert implementation-class %= external.", ^iclass-class(object))
  // end if;
  ~heap-imported-object?(heap, object)
end;

define inline method internal-binding?
    (heap :: <library-model-heap>, object :: <module-binding>)
  ~heap-imported-binding?(heap, object)
end;

define macro with-merged-literal
  { with-merged-literal (?:variable = ?lvalue:expression)
     ?:body
    end }
    => { let (?variable, changed?) = maybe-merge-literal(?lvalue);
	 if (changed?) ?lvalue := ?variable end;
	 ?body }
end macro;

// external visibility

define method do-export (heap :: <model-heap>, e)
  let e = standard-model-object(e);
  unless (model-externally-visible?(e))
    model-externally-visible?(e) := #t;
    // Eagerly force a run-time claim if necessary.
    maybe-claim-heap-element(heap, #f, e, element-compile-stage-only?(e));
    make-binding-externally-visible(heap, e);
    when (heap-element-seen?(heap, e))
      // might have already been processed but without the exports
      // have to walk again to get all the exports marked
      let ct? = member?(e, *heap-pending*.heap-compile-time-elements);
      export-references(heap, e, ct?);
    end;
  end;
end;

define method do-export (heap :: <model-heap>, e :: <symbol>)
  when (e == #()) next-method() end; // TODO: Emulator-specific hack.
end;

define method do-export (heap :: <model-heap>, e :: <uninterned-symbol>)
end;

define method do-export (heap :: <model-heap>, e :: <byte-string>)
  when (empty?(e)) next-method() end; // other strings never exported
end;

define method do-export (heap :: <model-heap>, e :: <&primitive>)
end;

define inline method maybe-export-derived (heap :: <model-heap>, e)
  // Doesn't require a definition
  unless (direct-object?(e) | heap-imported-object?(heap, e))
    do-export(heap, e)
  end;
end;

define method maybe-export (heap :: <model-heap>, e, ct-ref?)
  unless (direct-object?(e) | heap-imported-object?(heap, e)
	    | ~model-has-definition?(e))
    do-export(heap, e);
  end;
end method;

define method maybe-export (heap :: <model-heap>, e :: <&iep>, ct-ref?)
  unless (direct-object?(e) | heap-imported-object?(heap, e)
	    | ~model-has-definition?(e.function))
    do-export(heap, e);
  end;
end method;

define method maybe-export (heap :: <model-heap>, e :: <&mm-wrapper>, ct-ref?)
  let class = e.^mm-wrapper-implementation-class.^iclass-class;
  unless (direct-object?(e) | heap-imported-object?(heap, e)
	    | ~model-has-definition?(class))
    do-export(heap, e);
  end;
end method;



define method maybe-export (heap :: <model-heap>, e :: <module-binding>, ct-ref?)
  unless (heap-imported-binding?(heap, e))
    do-export(heap, e);
    // The binding might have been already processed and skipped due to not
    // being exported, so now have to force it.
    maybe-claim-heap-element(heap, #f, e, ct-ref?);
  end;
end method;


define function maybe-export-sequence (heap :: <model-heap>, s :: <sequence>, ct-ref?)
  for (e in s) maybe-export(heap, e, ct-ref?) end;
end;

define method export-references (heap :: <model-heap>,
				 e :: <module-binding>,
				 ct?)
  let form = untracked-binding-definition(e, default: #f);
  if (instance?(form, <shared-symbols-definition>))
    for (symbol in form-shared-symbols(form))
      unless (model-externally-visible?(symbol))
	model-externally-visible?(symbol) := #t;
	maybe-claim-heap-element(heap, e, symbol, ct?);
      end;
    end;
  else
    let value = merged-binding-value(e);
    when (value) maybe-export(heap, value, ct?) end;
    let type = merged-binding-type(e);
    when (type) maybe-export(heap, type, ct?) end;
  end;
end;
  
define function merged-binding-value (binding :: <module-binding>)
  let value = binding-model-or-hollow-object(binding);
  let (value, changed?) = maybe-merge-literal(value);
  // assume models that could be hollow (classes, gf's) never actually change
  if (changed?) binding-cached-model-object(binding) := value end;
  value
end;

define function merged-binding-type (binding :: <module-binding>)
  let value = binding-type-model-object(binding);
  let (value, changed?) = maybe-merge-literal(value);
  if (changed?) binding-cached-type-model-object(binding) := value end;
  value
end;  

define method export-references
    (heap :: <model-heap>, e :: <&library>, ct?) => ()
  with-merged-literal (value = ^used-libraries(e))
    maybe-export-sequence(heap, value, ct?)
  end;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&used-library>, ct?) => ()
  maybe-export(heap, ^used-library(e), ct?)
end method;

define method export-references
    (heap :: <model-heap>, e :: <&module>, ct?) => ()
  maybe-export(heap, ^home-library(e), ct?)
end method;

define method export-references
    (heap :: <model-heap>, e :: <&domain>, ct?) => ()
  maybe-export(heap, ^domain-library(e), ct?);
  maybe-export(heap, ^domain-next(e), ct?);
//  // maybe-export(heap, the generic function ??? ) ;
//  for (i :: <integer> from 0 below ^domain-number-required(e))
//    with-merged-literal (type = ^domain-type(e, i))
//      maybe-export(walk, type)
//    end;
//  end for;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&class>, ct?) => ()
  if (~(^class-sealed?(e)))
    // Used directly by subclasses.
    maybe-export-sequence(heap, ^direct-superclasses(e), ct?);
    maybe-export-sequence(heap, ^slot-descriptors(e), ct?);
    for (d in ^direct-inherited-slot-descriptors(e))
      maybe-export-init-data(heap, d, ct?);
    end;
    for (d in ^direct-initialization-argument-descriptors(e))
      maybe-export-init-data(heap, d, ct?);
    end;
  end;
  // Used by type checks.
  let wrapper = ^class-mm-wrapper(e);
  // format-out(">>> Walking wrapper for %=: %=\n", e, wrapper);
  wrapper & maybe-export-derived(heap, wrapper);
  // Direct call to the constructor may be generated implicitly.
  maybe-export(heap, ^class-constructor(e), ct?);
end method;

define method export-references
    (heap :: <model-heap>, e :: <&virtual-class>, ct?) => ()
end;

define method export-references
    (heap :: <model-heap>, e :: <&slot-initial-value-descriptor>, ct?)
 => ()
  maybe-export-init-data(heap, e, ct?);
end method;

define method maybe-export-init-data
    (heap :: <model-heap>, e :: <&slot-initial-value-descriptor>, ct?)
 => ()
  with-merged-literal (data = ^init-data-slot(e))
    maybe-export(heap, data, ct?);
    if (instance?(data, <&method>) & ~model-has-definition?(data))
      // An anonymous function that may get inlined in make methods.
      export-body-references(heap, data, ct?);
    end;
  end;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&slot-descriptor>, ct?)
 => ()
  next-method();
  with-merged-literal (type = ^slot-type(e))
    maybe-export(heap, type, ct?);
  end;
end method;

define function maybe-export-name
    (heap :: <model-heap>, name, ct-ref?) => ()
  if (name & instance?(name, <name-fragment>))
    let binding = untracked-lookup-binding(name, default: #f);
    if (binding & untracked-binding-definition(binding, default: #f))
      maybe-export(heap, binding, ct-ref?);
    end;
  end;
end function;

define method export-references
    (heap :: <model-heap>, e :: <&designator-class>, ct?) => ()
  next-method();
  maybe-export-name(heap, concrete-class-name(e), ct?);
  maybe-export-name(heap, pointer-type-name(e), ct?);
  maybe-export-name(heap, ^export-function(e), ct?);
  maybe-export-name(heap, ^import-function(e), ct?);
  maybe-export(heap, ^referenced-type(e), ct?);
  maybe-export(heap, ^concrete-class(e), ct?);
end method;

define method export-references
    (heap :: <model-heap>, e :: <&generic-function>, ct?) => ()
  maybe-export-sequence(heap, ^generic-function-methods(e), ct?);
  export-signature-references(heap, ^function-signature(e), ct?);
end method;

/*
define method export-references
    (heap :: <model-heap>, e :: <&incremental-generic-function>, ct?) => ()
  with-merged-literal (info = ^incremental-gf-domain-info(e))
    maybe-export(heap, info, ct?);
  end;
end method;
*/

define method export-references
    (heap :: <model-heap>, e :: <&method>, ct?) => ()
end method;

define method export-references
    (heap :: <model-heap>, e :: <&lambda>, ct?) => ()
  maybe-export-derived(heap, ^iep(e));
  // And the body if inlineable...
  if (method-inlineable?(e))
    export-body-references(heap, e, ct?);
    export-signature-references(heap, ^function-signature(e), ct?);
  end;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&signature>, ct?) => ()
  export-signature-references(heap, e, ct?)
end method;

define method export-signature-references
    (heap :: <model-heap>, sig :: false-or(<&signature>), ct?) => ()
  if (sig)
    let reqs = ^signature-required(sig);
    for (count from 0 below ^signature-number-required(sig))
      with-merged-literal (type = reqs[count])
	maybe-export(heap, type, ct?);
      end;
    end;
    let vals = ^signature-values(sig);
    for (count from 0 below ^signature-number-values(sig))
      with-merged-literal (type = vals[count])
	maybe-export(heap, type, ct?);
      end;
    end;
  end;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&keyword-method>, ct?) => ()
  next-method();
  let keys = keyword-specifiers(e);
  for (i from 1 below size(keys) by 2)
    with-merged-literal (default = keys[i])
      maybe-export(heap, default, ct?);
    end;
  end;
end method;

define method embedded-inline-only-function? 
    (object) => (well? :: <boolean>)
  #f
end method;

define method embedded-inline-only-function? 
    (m :: <&method>) => (well? :: <boolean>)
  ~model-has-definition?(m) & lambda-top-level?(m)
end method;

define method embedded-inline-only-function? 
    (gf :: <&generic-function>) => (well? :: <boolean>)
  if (~model-has-definition?(gf))
    // signal("*** Embedded inline-only generic %= encountered.", gf);
    #t
  end;
end method;

define method embedded-inline-only-function?
    (iep :: <&iep>) => (well? :: <boolean>)
  embedded-inline-only-function?(iep.function)
end;

define method export-body-references
    (heap :: <model-heap>, e :: <&lambda>, ct?) => ()
//  debug-out(#"heap", "*** EXPORT-BODY-REFERENCES %=, ct?=%s\n", e, ct?);
  let refs = lambda-heap-for-sure(e);
  let refs-data = code-references-vector(refs);
  for (i from 0 below code-vector-sizes-offset(refs))
    let e = refs-data[i];
    if (instance?(e, <heap-deferred-model>))
      #f
    elseif (embedded-inline-only-function?(e))
      // format-out("Embedded inline-only: %=\n", e);
      export-references(heap, e, ct?);
    else
      maybe-export(heap, e, ct?);
    end;
  end for;
end method;


define method export-references
    (heap :: <model-heap>, e :: <&macro>, ct?) => ()
  let object = expander-macro-object(e);
  let def = model-has-definition?(e);
  // The following isn't true for procedural macros...
  if (instance?(object, <macro-descriptor>))
    let referenced-names = macro-referenced-names(object);
    for (name in referenced-names)
      maybe-export-name(heap, name, #t);
    end;
  end;
end method;

define method export-references
    (heap :: <model-heap>, e :: <&type>, ct?) => ()
  // And the references?
end method;

define method export-references
    (heap :: <model-heap>, e :: <&expander>, ct?) => ()
  // Not much we can do here...
end method;

define method export-references
    (heap :: <model-heap>, e :: <&iep>, ct?) => ()
end method;

define method export-references
    (heap :: <model-heap>, e :: <&primitive>, ct?) => ()
end method;

define method export-references
    (heap :: <model-heap>, e :: <&engine-node>, ct?) => ()
end method;

define method export-references
    (heap :: <model-heap>, e :: <&mm-wrapper>, ct?) => ()
end method;

define method export-references
    (heap :: <model-heap>, e :: <list>, ct?) => ()
  // What about what it contains?
end method;

define method export-references
    (heap :: <model-heap>, e :: <vector>, ct?) => ()
  // What about what it contains?
end method;

define method export-references
    (heap :: <model-heap>, e :: <object>, ct?) => ()
end method;


// Claim world.

define function heap-element-seen? 
    (heap :: <model-heap>, object) => (well?)
  heap-element-claimed?(heap, object)
    | member?(object, *heap-pending*.heap-compile-time-elements)
end;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element, ct-ref?) => ()
  if (direct-object?(element))
    #f
  elseif (~internal-object?(heap, element))
    record-external-heap-element-reference(heap, parent, element, ct-ref?);
  else
    claim-heap-element(heap, parent, element, ct-ref?);
  end;
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, binding :: <module-binding>, ct-ref?) => ()
  if (~internal-binding?(heap, binding) | binding-previously-defined?(binding))
    record-external-heap-element-reference(heap, parent, binding, ct-ref?);
  else
    claim-heap-element(heap, parent, binding, ct-ref?);
  end;
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <interactor-binding>, ct-ref?) => ()
  #f
end method;

// Exceptions.

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, object :: <&class>, ct-ref?)
  // TO DO: should avoid doing all this work if already claimed.
  object.^direct-subclasses; // force subclasses to be computed
  maybe-export(heap, object, ct-ref?);
  next-method();
  ^ensure-slots-initialized(object); // forces wrapper to be computed
  let wrapper = ^class-mm-wrapper(object);
  maybe-claim-heap-element-derived(heap, object, wrapper, ct-ref?);
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, e :: <&virtual-class>, ct-ref?)
end;


// TODO: For debugging only
// @@@@GSB - check this check
define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&mm-wrapper>, ct-ref?)
  debug-assert(model-creator(element) 
		 == model-creator(element.^mm-wrapper-implementation-class.^iclass-class),
	       "Class wrapper created in wrong context");
  next-method();
end method;


define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <virtual-object>, ct-ref?)
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&runtime-object>, ct-ref?)
  // ALWAYS EXTERNAL
  do-record-external-heap-element-reference(heap, element, ct-ref?);
end method;

// TODO: This makes local copies of external strings which don't have a
// definition.  Have to make local copies because don't have a way to
// reference the external string.  I'm not sure why we would have such
// references at all, perhaps because of inlining.
// [Now have references due to model merging]
define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <string>, ct-ref?) => ()
  if (~internal-object?(heap, element) & model-has-definition?(element))
    record-external-heap-element-reference(heap, parent, element, ct-ref?);
  elseif (~heap-element-seen?(heap, element))
    if (~internal-object?(heap, element))
      new-mapped-model(element);
    end;
    claim-heap-element(heap, parent, element, ct-ref?);
  end;
end method;

// Ditto here.
define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <simple-object-vector>, ct-ref?) => ()
  if (~internal-object?(heap, element) & model-has-definition?(element))
    record-external-heap-element-reference(heap, parent, element, ct-ref?);
  elseif (~heap-element-seen?(heap, element))
    if (~internal-object?(heap, element))
      new-mapped-model(element)
    end;
    claim-heap-element(heap, parent, element, ct-ref?);
  end;
end method;

define method heap-element-referenced? (heap, object, ct-ref?)
  member?(object, heap-referenced-objects(heap))
    | (ct-ref? & member?(object, *heap-pending*.heap-compile-time-references))
end;

define method mark-heap-element-referenced (heap, object, ct-ref?)
  if (ct-ref?)
    add!(*heap-pending*.heap-compile-time-references, object);
  else
    add!(heap-referenced-objects(heap), object);
  end;
end;

define method heap-element-referenced? (heap, object :: <module-binding>, ct-ref?)
  member?(object, heap-referenced-bindings(heap))
    | (ct-ref? & member?(object, *heap-pending*.heap-compile-time-references))
end;

define method mark-heap-element-referenced (heap, object :: <module-binding>, ct-ref?)
  if (ct-ref?)
    add!(*heap-pending*.heap-compile-time-references, object);
  else
    add!(heap-referenced-bindings(heap), object);
  end;
end;

define method do-record-external-heap-element-reference
    (heap :: <model-heap>, object, ct-ref?) => ()
  mark-heap-element-referenced(heap, object, ct-ref?);
  if (~heap-imported-object?(heap, object))
    let element-heap 
      = compilation-record-model-heap(model-compilation-record(object));
    if (element-heap & element-heap ~== heap)
      let refs = heap-incoming-references(element-heap);
      refs[object] := ct-ref? & element(refs, object, default: #t);
      unless (model-externally-visible?(object))
	model-externally-visible?(object) := #t;
	make-binding-externally-visible(heap, object);
      end;
      // TODO: Is there a nicer way of handling this? This canonicalises
      // an iep, getting rid of any deferred iep's to that we don't get
      // multiply claimed aliases.
      if (instance?(object, <&iep>))
	model-externally-visible?(object.function.iep) := #t;
      end;
    end;
  end;
end method;

define method do-record-external-heap-element-reference
    (heap :: <model-heap>, binding :: <module-binding>, ct-ref?) => ()
  mark-heap-element-referenced(heap, binding, ct-ref?);
  if (~heap-imported-binding?(heap, binding))
    let element-heap
      = compilation-record-model-heap(binding-compilation-record(binding));
    if (element-heap)
      let refs = heap-incoming-references(element-heap);
      refs[binding] := ct-ref? & element(refs, binding, default: #t);
      model-externally-visible?(binding) := #t;
    end;
  end;
end method;

define method record-external-heap-element-reference
    (heap :: <model-heap>, parent, element, ct-ref?) => ()
  unless (heap-element-referenced?(heap, element, ct-ref?))
    unless (ct-ref?)
      record-repeated-size(heap, element);
    end;
    do-record-external-heap-element-reference(heap, element, ct-ref?);
    let class = element.^object-class;
    maybe-claim-heap-element(heap, parent, class, ct-ref?);
  end unless;
end method;

define method record-external-heap-ct-element-reference
    (heap :: <model-heap>, parent, element, ct-ref?) => ()
  unless (heap-element-referenced?(heap, element, ct-ref?))
    do-record-external-heap-element-reference(heap, element, ct-ref?);
  end;
end method;

define method record-external-heap-element-reference
    (heap :: <model-heap>, parent, element :: <module-binding>, ct-ref?) => ()
  record-external-heap-ct-element-reference(heap, parent, element, ct-ref?)
end method;

define method record-external-heap-element-reference
    (heap :: <model-heap>, parent, element :: <&iep>, ct-ref?) => ()
  record-external-heap-ct-element-reference(heap, parent, element, ct-ref?)
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <float>, ct-ref?)
end method;

// 
// Contiguous static list dumping
// 
// This is a temporary hacky requirement for dynamic linking
// of generic-function methods from the generic's lists, to
// enable a single runtime indirection to be performed in fixing
// up the methods at DLL load-time
// 
// TODO: make static collection subset of generic-function methods
//       a simple-vector to remove this hack
//
// Nosa  Feb 24, 1999



define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <pair>, ct-ref?)
  next-method();
  with-merged-literal (element-tail = element.tail)
    if (instance?(element-tail, <list>))
      maybe-claim-heap-element(heap, element, element-tail, ct-ref?);
    end;
  end;
end method;


define function claim-heap-element 
    (heap :: <model-heap>, parent, e, ct-ref?) => (new?)
  unless (heap-element-claimed?(heap, e) |
	    (ct-ref? & member?(e, *heap-pending*.heap-compile-time-elements)))
    if (ct-ref? & element-compile-stage-only?(e))
      add!(*heap-pending*.heap-compile-time-elements, e);
    else
      mark-heap-element(heap, parent, e);
      if (~ct-ref? & member?(e, *heap-pending*.heap-compile-time-elements))
	remove!(*heap-pending*.heap-compile-time-elements, e);
	// May have already been processed, but as a compile-time.  Need
	// to re-process as run-time.
	maybe-claim-heap-element-references(heap, e, #f);
      end;

    end;
    make-heap-element-pending(heap, e);
    #t
  end;
end;


// Explicitly mark model-objects as runtime;
// Shared library back-ends require this knowledge to implement
// dynamic linking of derived model objects
// 
// Nosa  Feb 24, 1999

define method mark-run-time-element(heap :: <model-heap>, e) => ()
end method;

define method mark-run-time-element(heap :: <model-heap>, e :: <&lambda>) => ()
  e.lambda-runtime-function? := #t;
end method;


// Whenever certain models are exported across a shared dynamic library
// boundary, need to also define and export the binding for it;
// dynamic linking of the model or database resolution may indirect
// through the binding to retrieve the runtime value for the model
// 
// Nosa  Feb 24, 1999

define method make-binding-externally-visible
    (heap :: <model-heap>, o) => ()
end;

// Do nothing for class constructors as they are derived from
// their classes

define method make-binding-externally-visible
    (heap :: <model-heap>, o :: <&initializer-method>) => ()
end;

// For the relevant models...

define constant <binding-anchored-object> = type-union(<&function>, <&class>);

// ...define and export their bindings 

define method make-binding-externally-visible
    (heap :: <model-heap>, o :: <binding-anchored-object>) => ()

  let binding? = model-variable-binding(o);
  if (binding?)
    let b :: <module-binding> = binding?;

    // Why doesn't this do do-export?

    unless (model-externally-visible?(b))
      model-externally-visible?(b) := #t;

      if (internal-binding?(heap, b) & ~binding-previously-defined?(b))
	claim-heap-element(heap, #f, b, element-compile-stage-only?(b));
      end;
    end;
  end;
end;

define function maybe-claim-heap-element-references
    (heap :: <model-heap>, element, compile-time?)
  when (model-externally-visible?(element))
    export-references(heap, element, compile-time?);
  end;
  if (~compile-time?)
    maybe-claim-heap-element-references-internal(heap, element, #f);
  end;
end;

define function maybe-merge-literal (object)
 => (new-object, changed?)
  if (instance?(object, <&iep>))
    // TODO: Could we just check for <&deferred-iep>'s?
    // TODO: Is there a nicer way of handling this?
    // This really has nothing to do with merging literals, it just
    // canonicalizes an iep, getting rid of an deferred iep's so that we
    // don't get multiple claimed aliases.
    let std-object = object.function.iep;
    if (std-object == object)
      values(object, #f)
    else
      debug-assert(instance?(std-object, <&iep>), "Bogus deferred IEP!");
      values(std-object, #t)
    end;
  elseif (*heap-pending* & *heap-pending*.heap-merged-literals &
	    literal-mergable?(object) &
	    internal-object?(*heap-pending*.heap-pending-heap, object))
    let tab = *heap-pending*.heap-merged-literals;
    let std-object = element(tab, object, default: #f);
    if (std-object)
      let changed? = (std-object ~== object);
      when (changed?)
	// TODO: what a horrible kludge!!! Really should maintain all the
	// refs to the object, and if need to select a new standard object,
	// because found one with a definition, should back-patch all refs
	// to point to the new one....
	when (object.model-has-definition?)
	  if (std-object.model-has-definition?)
	    debug-message("######### Fudging MODEL DEF %s into %s\n",
			  format-to-string("%s", object.model-definition),
			  format-to-string("%s", std-object.model-definition));
	    // Patch so external references to object get the correct name
	    object.model-definition := std-object.model-definition;
	  else
	    debug-message("######### Fudging MODEL DEF %s into anonymous\n",
			  format-to-string("%s", object.model-definition));
	    std-object.model-definition := object.model-definition;
	  end;
	end;
	when (*literal-merging-stats*)
	  *literal-merging-stats*[std-object]
	    := add!(element(*literal-merging-stats*, std-object, default: #()),
		    object);
	end;
      end;
      values(std-object, changed?);
    else
      tab[object] := object;
      values(object, #f)
    end;
  else
    values(object, #f)
  end;
end;

define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, element, ct?) => ()
  maybe-claim-heap-element(heap, element, &object-class(element), ct?);
  for-layout-fixed-slot-value (val described-by slotd in element)
    claim-instance-slot-value(heap, element, slotd, val, ct?);
  end;
  for-layout-repeated-slot-value 
      (val described-by slotd keyed-by index in element)
    let (val, changed?) = maybe-merge-literal(val);
    if (changed?) ^repeated-slot-value(element, slotd, index) := val end;
    if (load-bound-object?(val))
      record-heap-load-bound-reference
        (heap, val,
         make(<load-bound-repeated-slot-reference>,
              referenced-object:      val,
              referencing-object:     element,
              referencing-slot:       slotd,
              referencing-slot-index: index));
    end;
    maybe-claim-heap-element(heap, element, val, ct?);
  end;
end method;

define method maybe-claim-heap-element-references-internal 
    (heap :: <model-heap>, element :: <&generic-function>, ct?) => ()
  if (ct?)
    with-merged-literal (sig = ^function-signature(element))
      maybe-claim-heap-element(heap, element, sig, ct?);
    end;
    // Assume for now that we don't literal-merge methods..
    for (meth in ^generic-function-methods(element))
      maybe-claim-heap-element(heap, element, meth, ct?);
    end;
  else
    // Special handing of methods/domains.
    maybe-claim-heap-element(heap, element, &object-class(element), ct?);
    maybe-claim-generic-function-modifying-models(heap, element);
    for-layout-fixed-slot-value (val described-by slotd in element)
      let getter = slotd.model-object-getter;
      unless (slotd.model-object-getter == ^generic-function-methods |
	      slotd.model-object-setter == ^incremental-gf-domain-info-setter)
	claim-instance-slot-value(heap, element, slotd, val, ct?);
      end;
    end;
  end;
end;

// TODO: avoid merging ^class-slot-storage, which is mutable.  Should
// be replaced by a more general mechanism to allow not merging slots.
define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, element :: <&implementation-class>, ct?) => ()
  maybe-claim-heap-element(heap, element, &object-class(element), ct?);
  for-layout-fixed-slot-value (val described-by slotd in element)
    // Don't claim ^direct-subclasses, handled by deferred processing
    unless (slotd.model-object-setter == ^direct-subclasses-setter)
      claim-instance-slot-value(heap, element, slotd, val, ct?,
				merge?: slotd.model-object-setter
				          ~== ^class-slot-storage-setter);
    end;
  end;
end;


define function claim-instance-slot-value
    (heap :: <model-heap>, element, slotd :: <&slot-descriptor>, val, ct?,
     #key merge? = #t)
  let val = if (merge? & slotd.model-object-setter)
	      let (val, changed?) = maybe-merge-literal(val);
	      if (changed?) ^slot-value(element, slotd) := val end;
	      val
	    else
	      val
	    end;
  if (load-bound-object?(val))
    record-heap-load-bound-reference
      (heap, val,
       make(<load-bound-instance-slot-reference>,
	    referenced-object:  val,
	    referencing-object: element,
	    referencing-slot:   slotd));
  end;
  maybe-claim-heap-element(heap, element, val, ct?);
end claim-instance-slot-value;


// An optimization for byte-strings.

define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, element :: <byte-string>, ct?) => ()
  // Everything else is immediate.
  maybe-claim-heap-element(heap, element, &object-class(element), ct?);
end method;

define method maybe-claim-heap-element-references-internal 
    (heap :: <model-heap>, element :: <module-binding>, ct?) => ()
  let value = merged-binding-value(element);
  unless (instance?(value, <unknown>))
    if (load-bound-object?(value))
      record-heap-load-bound-reference
        (heap, value,
         make(<load-bound-binding-reference>,
              referenced-object: value,
              referencing-binding: element));
    end;
    maybe-claim-heap-element(heap, element, value, ct?);
  end unless;
  let binding-type = static-module-binding-type(element);
  when (binding-type)
    maybe-claim-heap-element(heap, element, binding-type, ct?);
  end when;
end method;

define method static-module-binding-type
      (binding :: <module-binding>) => (binding-type)
  let form = untracked-binding-definition(binding, default: #f);
  unless (form & constant?(form))
    let binding-type = binding.merged-binding-type;
    let declared? = binding-type ~== dylan-value(#"<object>");
    if (declared?)
      binding-type
    end if;
  end unless;
end method;

define inline method mark-emitted-name (heap :: <model-heap>, object)
  object.emitted-name := heap-next-id(heap)
end method;

define method mark-heap-element (heap :: <model-heap>, parent, object)
  debug-assert(~direct-object?(object));
  // unless (object.model-has-definition?)
    mark-emitted-name(heap, object);
  // end unless;
  // debug-assert(internal-object?(heap, object));
  let model-object = standard-model-object(object);
  heap-size(heap) := heap-size(heap) + heap-instance-size(model-object);
  unless (*precomputing-heap?*)
    if (*heap-record-back-pointers?*)
      unless (member?(model-object, heap-defined-objects(heap)))
	// record parents
	heap-back-pointers(heap)[object]
	  := parent;
	     // pair(parent, 
	     //      element(heap-back-pointers(heap), object, default: #()));
      end unless;
    end if;
    // explicitly mark runtime model objects at definition-time
    // during "Heaping"
    mark-run-time-element(heap, model-object);
  end;
  add!(heap-defined-objects(heap), model-object);
end method;

define method heap-element-claimed? (heap :: <model-heap>, object)
  let object = standard-model-object(object);
  direct-object?(object) | member?(object, heap-defined-objects(heap))
end;

define method mark-heap-element 
    (heap :: <model-heap>, parent, binding :: <module-binding>)
  debug-assert(internal-binding?(heap, binding));
  let defined = heap-defined-bindings(heap);
  unless (member?(binding, defined))
    add!(defined, binding);
  end;
end method;

define method heap-element-claimed? (heap :: <model-heap>, binding :: <module-binding>)
  member?(binding, heap-defined-bindings(heap))
end;

define method make-heap-element-pending (heap :: <model-heap>, element)
  // Breadth first: 
  // push-last(*heap-pending*.heap-pending-elements, element);
  // Depth first:
  push(*heap-pending*.heap-pending-elements, element);
end method;

//// Symbol hacks.

// Symbols are replaced by a local copy in each compilation record.
// This is achieved in rather an ugly way at the moment. The canonical
// symbol is also left in the heap to prevent multiple copies being made,
// and special methods are defined in the emit code to do nothing for
// symbols. Only the copies, the uninterned symbols, are dumped.

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, symbol :: <symbol>, ct-ref?) => ()
  // TODO: This is an emulator-specific hack to catch #(), which is 
  // classified as a symbol there.
  if (symbol == #())
    next-method();
  elseif (~internal-object?(heap, symbol) & model-has-definition?(symbol))
    record-external-heap-element-reference(heap, parent, symbol, ct-ref?);
  else
    if (~element(heap-symbols(heap), symbol, default: #f))
      element(heap-symbols(heap), symbol) := symbol;
      let copy = mapped-model(deep-copy-symbol(symbol));
      model-definition(copy) := model-definition(symbol);
      when (model-externally-visible?(symbol)
	      // TODO: Can't find copy later if export symbol later...
	      | model-has-definition?(copy))
	model-externally-visible?(copy) := #t;
      end;
      claim-heap-element(heap, parent, copy, ct-ref?);
    end;
  end;
end method;

// Special case hacks to ensure symbols and such get dumped in each
// compilation unit.

define method load-bound-object? 
    (element :: <symbol>) => (boolean)
  element ~== #() // TODO: Remove this emulator-specific hack.
    & ~model-has-definition?(element)
end method;


/// ffi support


  
define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, element :: <&raw-aggregate-type>, ct?) => ()
  do-record-external-heap-element-reference(heap, element, ct?);
  // only trace any raw aggregate types embedded in this one.
  for (member in element.raw-aggregate-members)
    if (instance?(member.member-raw-type, <&raw-aggregate-type>))
      maybe-claim-heap-element-references(heap, member.member-raw-type, ct?);
    end;
  end;
end;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&c-function>, ct-ref?)
  unless (element.binding-name)
    mark-emitted-name(heap, element);
  end unless;
  do-record-external-heap-element-reference(heap, element, ct-ref?);
  maybe-claim-c-signature-elements(heap, element.c-signature);
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&c-callable-function>, ct-ref?)
  next-method();
  maybe-claim-c-signature-elements(heap, element.c-signature);
end method;  

define method maybe-claim-c-signature-elements
    (heap :: <model-heap>, sig :: <&signature>)
  for (type in ^signature-required(sig),
       count from 0 below ^signature-number-required(sig))
    if (instance?(type, <&raw-aggregate-type>))
      maybe-claim-heap-element-references(heap, type, #f);
    end;
  end;
  for (type in ^signature-values(sig),
       count from 0 below ^signature-number-values(sig))
    if (instance?(type, <&raw-aggregate-type>))
      maybe-claim-heap-element-references(heap, type, #f);
    end;
  end;
end method;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&c-variable>, ct-ref?)
  do-record-external-heap-element-reference(heap, element, ct-ref?);
end method;

////
//// CODE REFERENCES
////

define class <code-references> (<object>)
  constant slot code-references :: <ordered-object-set>
    = make(<ordered-object-set>);
  constant slot code-value-references :: <ordered-object-set>
    = make(<ordered-object-set>);
  constant slot code-vector-sizes :: <ordered-object-set>
    = make(<ordered-object-set>);
end class;

define class <compressed-code-references> (<object>)
  constant slot code-value-references-offset :: <integer>,
    required-init-keyword: value-references-offset:;
  constant slot code-vector-sizes-offset :: <integer>,
    required-init-keyword: vector-sizes-offset:;
  constant slot code-references-vector :: <simple-object-vector>,
    required-init-keyword: references:;
end class;

define method compress-set-into
    (x :: <ordered-object-set>,
     y :: <simple-object-vector>,
     offset :: <integer>)
 => (res :: <simple-object-vector>)
  for (e in x.key-sequence, i :: <integer> from offset)
    y[i] := e;
  end for;
  y
end method;

define method compress-code-references
    (x :: <code-references>) => (z :: <compressed-code-references>)
  let refs-size          = size(code-references(x));
  let value-refs-size    = size(code-value-references(x));
  let vector-sizes-size  = size(code-vector-sizes(x));
  let total-size         = refs-size + value-refs-size + vector-sizes-size;
  let refs               = make(<simple-object-vector>, size: total-size);
  compress-set-into
    (code-references(x),       refs, 0);
  compress-set-into
    (code-value-references(x), refs, refs-size);
  compress-set-into
    (code-vector-sizes(x),     refs, refs-size + value-refs-size);
  let y :: <compressed-code-references>
    = make(<compressed-code-references>,
	   references:              refs,
	   value-references-offset: refs-size,
	   vector-sizes-offset:     refs-size + value-refs-size);
  y
end method;

define method maybe-claim-code-reference (refs :: <code-references>, x) => ()
  add!(code-references(refs), x);
end method;

define method maybe-claim-value-reference (refs :: <code-references>, x) => ()
  add!(code-value-references(refs), x);
end method;

define method maybe-claim-stack-vector-reference
    (refs :: <code-references>, size) => ()
  add!(code-vector-sizes(refs), size);
end method;

//// MAYBE-CLAIM-VALUE-REFERENCES

define method maybe-claim-value-references (refs :: <code-references>, o) => ()
end method;

define method maybe-claim-value-references
    (refs :: <code-references>, o :: <defined-constant-reference>) => ()
  maybe-claim-value-reference(refs, o.referenced-binding);
end method;

define method maybe-claim-value-references
    (refs :: <code-references>, ref :: <object-reference>) => ()
  maybe-claim-value-reference(refs, ref);
end method;

//// MAYBE-CLAIM-COMPUTATION-REFERENCES

// Hack!!! This should be done in a more uniform way:

define method maybe-claim-computation-references
    (refs :: <code-references>, c) => ()
end method;

/// TODO: SHOULD SHARE WITH C-EMIT-COMPUTATION

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <variable-reference>) => ()
  maybe-claim-code-reference(refs, c.referenced-binding);
end method;

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <make-closure>) => ()
  maybe-claim-code-reference(refs, function(c.computation-closure-method));
end method;

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <assignment>) => ()
  maybe-claim-code-reference(refs, c.assigned-binding);
end method;

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <redefinition>) => ()
  maybe-claim-code-reference(refs, c.assigned-binding);
end method;

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <stack-vector>) => ()
  let empty-vector = dylan-value(#"%empty-vector");
  let size = c.temporary.number-values;
  if (size = 0)
    maybe-claim-code-reference(refs, empty-vector);
  else
    let class = &object-class(empty-vector);
    maybe-claim-code-reference(refs, class);
    maybe-claim-stack-vector-reference(refs, size);
  end if;
end method;

// !@#$ seems like a wart

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <primitive-call>) => ()
  maybe-claim-code-reference(refs, c.primitive);
end method;

// !@#$ seems like a wart

define method maybe-claim-computation-references
    (refs :: <code-references>, c :: <c-variable-pointer-call>) => ()
  maybe-claim-code-reference(refs, c.c-variable); // !@#$ external
end method;

define function lambda-heap-for-sure
    (m :: <&lambda>) => (z :: <compressed-code-references>)
  lambda-heap(m)
  | (lambda-heap(m) 
       := begin
	    let refs = make(<code-references>);
	    for-computations (c in m)
	      if (instance?(c, <simple-call>)
		    // object-class(c) == <simple-call>
		    & call-iep?(c)
		    // Check for an empty environment too?
		    & instance?(c.function, <object-reference>))
		maybe-claim-value-reference
		  (refs, c.function.reference-value.^iep);
		for (arg in c.arguments)
		  maybe-claim-value-references(refs, arg);
		end;
		// This is so ugly.
		if (instance?(c, <method-call>))
		  maybe-claim-value-references(refs, c.next-methods);
		end;
	      else
		do-used-value-references
		  (curry(maybe-claim-value-references, refs), c);
		maybe-claim-computation-references(refs, c);
	      end;
	    end for-computations;
            compress-code-references(refs)
          end);
end function;

define method maybe-claim-computations-references
    (heap :: <model-heap>, m :: <&lambda>, ct?) => ()
  let refs = lambda-heap-for-sure(m);
  let refs-data = code-references-vector(refs);
  for (i from 0 below code-value-references-offset(refs))
    let e = refs-data[i];
    maybe-claim-heap-element(heap, m, e, ct?);
  finally
    for (j from i below code-vector-sizes-offset(refs))
      let e = refs-data[j];
      let e = if (~instance?(e, <object-reference>))
		e
	      elseif (*precomputing-heap?*)
		e.reference-value
	      else // update both the reference itself and the lambda heap
		with-merged-literal (value = e.reference-value)
		  refs-data[j] := value;
		  when (instance?(value, <heap-deferred-model>))
		    record-deferred-model-reference(heap, e, value);
		    record-deferred-model-reference(heap, pair(refs-data, j), value);
		  end;
		  value
		end;
	      end;
      if (load-bound-reference?(m, e))
	record-heap-load-bound-reference(heap, e,
					 make(<load-bound-code-reference>,
					      referenced-object: e));
      end;
      if (~instance?(e, <heap-deferred-model>))
	maybe-claim-heap-element(heap, m, e, ct?);
      end;
    finally
      let class = &object-class(dylan-value(#"%empty-vector"));
      for (k from j below size(refs-data))
	record-repeated-size-explicitly(heap, class, refs-data[k]);
      end for;
    end for;
  end for;
end method;


/// LAMBDA

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&lambda>, ct-ref?) => ()
  next-method();
  // Eagerly claim the iep so that it doesn't get moved away from 
  // the function.
  // TODO: maybe this should be a method on claim-heap-element, since only
  // care about the internal case.
  if (internal-object?(heap, element))
    let ct? = member?(element, *heap-pending*.heap-compile-time-elements);
    maybe-claim-heap-element-derived(heap, element, element.^iep, ct?);
  end;
end method;

define function maybe-claim-heap-element-derived
    (heap :: <model-heap>, parent, element, ct-ref?)
  when (~parent | model-externally-visible?(parent))
    maybe-export(heap, element, ct-ref?)
  end;
  maybe-claim-heap-element(heap, parent, element, ct-ref?)
end;

// TODO: PERFORMANCE: This code is pretty ugly and slow - perhaps there
// should be C-callable and C-callable-wrapper ieps instead of functions?

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&iep>, ct-ref?) => ()
  let fn = element.function;
  debug-assert(model-creator(element) == model-creator(fn));
  next-method();
  if (instance?(fn, <&c-callable-function>))
    maybe-claim-c-signature-elements(heap, fn.c-signature);
  end;
end method;

define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, i :: <&iep>, ct?) => ()
  // next-method();
  let l = i.function;
  if (~instance?(l, <&generic-function>) & (l.body | l.lambda-heap))
    // This just in case the function itself doesn't get claimed 
    // at some point.
    if (~model-has-definition?(l))
      mark-emitted-name(heap, l); 
    end;
    maybe-claim-computations-references(heap, l, ct?);
  end;
end method;

define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, e :: <&expander>, ct?)
end;

define method maybe-claim-heap-element-references-internal
    (heap :: <model-heap>, e :: <virtual-object>, ct?)
  debug-assert(ct?, "Non-compile-time virtual object %=?", e);
end;

define method maybe-claim-heap-element 
    (heap :: <model-heap>, parent, element :: <&any-kernel-ep>, ct-ref?) => ();
end method;

/*
define method maybe-claim-heap-element 
    (heap :: <library-model-heap>, parent, element :: <&deferred-iep>, ct-ref?) => ()
  if (compiling-dylan-library?())
    maybe-claim-heap-element(heap, parent, element.function.iep);
    next-method()
  else
    next-method()
  end;
end;
*/
