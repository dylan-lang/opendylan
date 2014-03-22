module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Useful timing macros.

define macro timing-compilation-phase
  { timing-compilation-phase (?phase:expression of ?ld:expression, ?keys:*)
      ?:body
    end }
    => { do-timing-compilation-phase(?ld, ?phase, method () ?body end, ?keys) }
end macro;

// TODO: All this because floating point printing is dodgy right now.

define function integer-amount-to-string (amount :: <integer>, microamount :: <integer>)
 => (float :: <string>)
  format-to-string("%d.%s", amount, integer-to-string(microamount, size: 6))
end function;

define function float-amount-to-string (amount :: <float>)
 => (float :: <string>)
  let whole-amount :: <integer> = floor(amount);
  let microamount :: <integer> = round(1000000.0 * (amount - max(whole-amount, 0)));
  integer-amount-to-string(whole-amount, microamount);
end function;

define class <profile-snap> (<object>)
  slot snap-time :: <float>,
    required-init-keyword: time:;
  slot snap-time-string :: <string>,
    required-init-keyword: time-string:;
  slot snap-space :: <float>,
    required-init-keyword: space:;
  slot snap-space-string :: <string>,
    required-init-keyword: space-string:;
  constant slot snap-phase  :: <string>,
    required-init-keyword: phase:;
end class;

define variable *dfmc-profile-allocation?* = #f;

define function do-timing-compilation-phase
    (ld :: <compilation-context>, phase, body :: <function>,
     #key progress? = #f, accumulate?, #all-keys)
  progress? & progress-line("%s of %s timing begun.", phase, ld);
  let (elapsed-seconds, elapsed-microseconds, allocated-space) = values(#f, #f, #f);
  profiling(cpu-time-seconds, cpu-time-microseconds, allocation)
    if (*dfmc-profile-allocation?*)
      profiling(allocation-stats =
                  list(description: format-to-string
                         ("%s of %s", phase, library-description-emit-name(ld))))
        body();
      results end;
    else
      body();
    end if;
  results
    elapsed-seconds := cpu-time-seconds;
    elapsed-microseconds := cpu-time-microseconds;
    allocated-space := allocation / 1000000.0;
  end;
  let elapsed-time = elapsed-seconds + elapsed-microseconds / 1000000.0;
  let snap =
    accumulate? &
    compilation-timing-property?(ld,
                                 method(snap :: <profile-snap>)
                                     snap.snap-phase = phase;
                                 end method);
  if (snap)
    let elapsed-time = snap.snap-time + elapsed-time;
    let allocated-space = snap.snap-space + allocated-space;
    snap.snap-time := elapsed-time;
    snap.snap-time-string := float-amount-to-string(elapsed-time);
    snap.snap-space := allocated-space;
    snap.snap-space-string := float-amount-to-string(allocated-space);
  else
    snap :=
        make(<profile-snap>,
             time:  elapsed-time,
             time-string: integer-amount-to-string
               (elapsed-seconds, elapsed-microseconds),
             space: allocated-space,
             space-string: float-amount-to-string(allocated-space),
             phase:  phase);
    record-compilation-timing-property(ld, snap);
  end if;

  progress? &
  if (accumulate?)
    progress-line
      ("%s of %s took %s seconds.",
       phase, ld,
       integer-amount-to-string
         (elapsed-seconds, elapsed-microseconds));
  else
    progress-line
      ("%s of %s took %s seconds.", phase, ld, snap-time-string(snap));
  end if;
  values();
end function;

define function uniquify-timing-properties
    (props :: <sequence>) => (new-props :: <sequence>)
  let seen = make(<object-table>);
  let rprops = reverse(props);
  collecting ()
    for (prop in rprops)
      let name = prop.snap-phase;
      let symbol = as(<symbol>, name);
      if (~element(seen, symbol, default: #f))
        collect-first(prop);
        element(seen, symbol) := #t;
      end;
    end;
  end;
end function;

define function compilation-source-line-count
    (ld :: <compilation-context>) => (count :: <integer>)
  collecting (as <integer>)
    for (cr in compilation-context-records(ld))
      let count = compilation-record-source-line-count(cr);
      // A false count means the source record was skipped entirely,
      // typically because its module: could not be found.
      if (count) collect(count) end;
    end;
  end;
end function;

/*
// This only makes sense when comparing emulated with emulated or
// native with native. In units of floops per second, where a floop
// is the inner for loop.

define constant *lum-floop-mark* = 0.225;

define variable *platform-floop-mark* = #f;

define function platform-floop-mark ()
  *platform-floop-mark*
    | begin
        progress-line("Computing platform floop mark...");
        let (secs, msecs)
          = timing ()
              for (i from 0 below 50000)
                make(<vector>, size: 1024);
              end;
            end;
        let time = secs + msecs / 1000000.0;
        let rating = 1 / time;
        progress-line("Floop mark is: %=", rating);
        *platform-floop-mark* := rating
      end;
end function;
*/

define function dump-timings-for (ld :: <compilation-context>) => ()
  let props = uniquify-timing-properties(compilation-timing-properties(ld));
  progress-line("Time taken per phase:");
  progress-line
  ("                                 Elapsed Time (secs)     Amount Allocated (Mb)");
  progress-line("");
  collecting (total-time :: <float>, total-space :: <float>)
    for (prop in props)
      collect-into(total-time, snap-time(prop));
      collect-into(total-space, snap-space(prop));
    end;
    let total-time = collected(total-time);
    let total-space = collected(total-space);
    local method table-line (name, percent-time, time, percent-space, space)
      progress-line("  %-25s -   %3d%% %14s      %3d%% %14s", name, percent-time, time, percent-space, space);
    end method;
    for (prop in props)
      let name = snap-phase(prop);
      let time = snap-time(prop);
      let time-string = snap-time-string(prop);
      let percentage-time
        = block ()
            round((time / total-time) * 100);
          exception (<arithmetic-overflow-error>)
            0
          end;
      let space = snap-space(prop);
      let space-string = snap-space-string(prop);
      let percentage-space
        = block ()
            round((space / total-space) * 100);
          exception (<arithmetic-overflow-error>)
            0
          end;
      table-line(name, percentage-time, time-string, percentage-space, space-string);
    end;
    progress-line("  --");
    table-line("Sum",
               100, float-amount-to-string(total-time),
               100, float-amount-to-string(total-space));
    let lines = compilation-source-line-count(ld);
    progress-line("  --");
    let lpm = (lines / total-time) * 60.0;
    let bps = float-amount-to-string(total-space / total-time);
    progress-line("  Compiled %d lines at a rate of %d lines per minute, %s Mb per second.",
                  lines, lpm, bps);
    let dood      = instance?(ld, <project-library-description>) & library-description-dood(ld);
    let dood-size = if (dood) dood-size(dood) else 0 end;
    when (dood-size)
      progress-line("  --");
      progress-line("  Database %d bytes.", dood-size);
    end when;
    /*
    let floop = platform-floop-mark();
    let floop% = round((floop / *lum-floop-mark*) * 100.0);
    progress-line("  Normalized rate is %= lines per minute "
                  "(machine rated at %=%% of lum).",
                  lpm * *lum-floop-mark* / floop, floop%);
    */
  end;
  retract-compilation-timings(ld);
end function;

// pretty print integers, right justified in supplied field length
// and decorated with commas -- this should really live in a shared
// IO library

define function display-integer(number :: <integer>, field :: <integer>)
 => (mangled-number :: <byte-string>)
  let number-characters :: <byte-string> = "0123456789";
  let result :: <byte-string> = make(<byte-string>, size: field);

  iterate process-integer (num :: <integer> = number, exponent :: <integer> = 1)
    let (quotient :: <integer>, remainder :: <integer>) = truncate/(num, 10);
    let digit :: <byte-character> = number-characters[remainder];

    if (quotient = 0)
      let index = field - (exponent + truncate/(exponent - 1, 3));
      if (index < 0)
        error("field %d not big enough for integer %d", field, number);
      end;
      result[index] := digit;
      index + 1
    else
      let index = process-integer(quotient, exponent + 1);
      if (modulo(exponent, 3) = 0)
        result[index] := ',';
        result[index + 1] := digit;
        index + 2
      else
        result[index] := digit;
        index + 1
      end;
    end if;

  end iterate;
  result;
end function;

define function print-gc-statistics (ld :: <compilation-context>) => ()
  progress-line("");
  progress-line("GC statistics for compilation of %s:",
                library-description-emit-name(ld));
  progress-line
  ("        Live                    Condemned                     Not Condemned");
  progress-line("");
  let stats :: <stretchy-object-vector> = garbage-collection-stats();

  for (i :: <integer> from 0 below stats.size by 3)
    progress-line("%s            %s                %s",
                  display-integer(stats[i], 15),
                  display-integer(stats[i + 1], 15),
                  display-integer(stats[i + 2], 15));
  end for;

end function;

//// Top level parsing.

define constant $empty-compilation-record-vector
  = as(<compilation-record-vector>, #[]);

// TODO: want to switch over to limited compilation-record vectors
//   but they must be consistently treated that way throughout compiler

define function compute-source-compilation-records
    (ld :: <library-description>, sr* :: <source-record-sequence>)
      => (new-cr* :: <sequence>,      // TODO: <compilation-record-vector>
          removed-cr* :: <sequence>)  // TODO: <compilation-record-vector>
  let cr* = ld.library-description-compilation-records;
  if (empty?(sr*)) // force-parse case...
    values($empty-compilation-record-vector, cr*)
  elseif ((sr*.size == cr*.size) &
           every?(method(sr, cr) sr == cr.compilation-record-source-record end,
                  sr*, cr*))
    values(cr*, $empty-compilation-record-vector)
  else
    let table = ld.library-description-record-table;
    block ()
      // Temporarily use sequence numbers as "removed" flags.
      for (cr in table)
        cr.compilation-record-sequence-number := #t;
      end;
      local method find-or-make-cr (sr)
              let cr = element(table, sr, default: #f);
              if (cr)
                if (cr.compilation-record-sequence-number == #f)
                  error("Duplicate use of %s", sr);
                end;
                cr.compilation-record-sequence-number := #f;
                cr
              else
                table[sr] := make(<compilation-record>,
                                  library: ld, source-record: sr);
              end if;
            end method;
      let new-cr* = map-as(<compilation-record-vector>, find-or-make-cr, sr*);
      let removed-cr* = #();
      for (cr in table)
        if (cr.compilation-record-sequence-number)
          removed-cr* := pair(cr, removed-cr*);
        end;
      end for;
      values(// If nothing changed, reuse the original...
             if ((size(new-cr*) == size(cr*)) & every?(\==, new-cr*, cr*))
               cr*
             else
               new-cr*
             end,
             removed-cr*);
    cleanup
      for (index from 0 below size(cr*))
        cr*[index].compilation-record-sequence-number := index;
      end;
    end block;
  end if;
end function;

// This function must be called within a with-library-context if there is any
// possibility of retracting.
define sideways method install-library-description-sources
    (ld :: <project-library-description>, sr* :: <source-record-sequence>)
  debug-out(#"Driver", "Computing changes in %s", ld.library-description-project);
  let (new-cr*, removed-cr*) = compute-source-compilation-records(ld, sr*);
  debug-out(#"Driver",
            if (new-cr* == ld.compilation-context-records)
              " -> No changes!\n"
            else
              " -> New-cr*: %s, old-cr*: %s, Removed-cr*: %s\n"
            end,
            new-cr*, ld.compilation-context-records, removed-cr*);
  unless (new-cr* == ld.compilation-context-records)
    detach-interactive-namespaces(ld);
    block (continue)
      block (retract)
        if (ld.library-description-stripped?) retract() end;
        if (empty?(sr*)) retract() end;
        // KLUDGE: if library doesn't have a definition, retract in order
        // to force reparsing of previously ignored records in case we
        // get a definition.  (This would happen automatically if we handled
        // library redefinition, but we don't).
        if (~ld.library-description-defined?) retract() end;
        with-dependent-retraction
          dynamic-bind (*cross-module-access-abort* = retract)
            progress-line(" Retracting changed sources");
            // Retract records we'll be removing
            do(retract-compilation-record, removed-cr*);
            // Retract any dependencies on changed sequence positions
            let n = size(new-cr*);
            for (i from 0 below n)
              let cr1 = new-cr*[i];
              for (j from i + 1 below n,
                   while: cr1.compilation-record-top-level-forms)
                let cr2 = new-cr*[j];
                if (cr2.compilation-record-top-level-forms &
                      (cr2.compilation-record-sequence-number
                         < cr1.compilation-record-sequence-number))
                  retract-compilation-record-order(cr1, cr2);
                end;
              end for;
            end for;
            // Might be adding new source records, so defs no longer complete.
            ld.compiled-to-definitions? := #f;
          end dynamic-bind;
        end with-dependent-retraction;
        continue();
      end block /* retract */;
      progress-line("Retracting library parsing");
      retract-library-parsing(ld);
    end block /* continue */;
    ld.compilation-context-records := new-cr*;
    for (index from 0 below size(new-cr*))
      new-cr*[index].compilation-record-sequence-number := index;
    end;
  end;
  // If compilation records computation and installation gets interrupted,
  // we may end up with some extra cr's in the compilation records table.
  // This is harmless and gets cleaned up at the next call.  But that
  // is why we remove removed-cr*'s even if nothing has changed in the
  // sources..
  let table = ld.library-description-record-table;
  for (cr in removed-cr*)
    remove-key!(table, cr.compilation-record-source-record);
  end;
end method;

define function ensure-library-definitions-installed (ld :: <compilation-context>, #key library-only? = #f)
  debug-assert(~ld.compilation-definitions-inconsistent?,
               "Inconsistent definitions should have already been handled!");
  unless (ld.compiled-to-definitions? |
            (library-only? & ld.library-description-defined?))
    debug-out(#"driver", if (library-only?)
                           "Parsing library definition for %s\n"
                         else
                           "Parsing unparsed definitions in %s\n"
                         end,
              ld.library-description-project);
    debug-assert(~instance?(ld, <project-library-description>) |
                   ld.interactive-namespaces-detached?,
                 "Retracted defs with attached interactive namespaces!");
    // TODO: This is using compiled-to-definitions? as a temp flag... Should
    // instead have a sequence  associated with with-dependent-retraction,
    // so that stuff that gets retracted gets directly remembered for
    // reprocessing...
    block ()
      until (ld.compiled-to-definitions? |
               (library-only? & ld.library-description-defined?))
        ld.compiled-to-definitions? := #t;
        for (cr in ld.compilation-context-records,
             until: library-only? & ld.library-description-defined?)
          update-compilation-record-definitions(cr,
                                                library-only?: library-only?);
        end;
      end until;
    cleanup
      ld.compiled-to-definitions? := #f;
    end;
    unless (library-only? & ld.library-description-defined?)
      mark-definitions-installed(ld);
      note-definitions-updated(ld);
      debug-out(#"Driver", "Top-level %s parsing complete.\n", ld);
      // Some caches are not incremental, have to clear them on every change.
      retract-non-incremental-caches(ld);
      // Because we want to be able to compile lazily from definitions,
      // we force the installation of any models that can be reached
      // directly, rather than via name lookup, now so that they get
      // claimed appropriately.
      if (compiling-dylan-library?() & ld.library-description-defined?)
        install-dylan-boot-constants(ld)
      end if;
    end;
  end unless;
end;

// This is called at the end of parsing and installing definitions.
define method mark-definitions-installed (ld :: <compilation-context>)
  if (*interactive-compilation-layer*)
    debug-assert(ld == *interactive-compilation-layer*);
    ld.compiled-to-definitions? := #t;
  else
    mark-project-definitions-installed(ld)
  end if;
end method;

define variable *compute-interfaces* = #f;

define function mark-project-definitions-installed
    (ld :: <project-library-description>)
  ld.library-description-change-count := next-library-change-count(ld);
  if (*compute-interfaces*)
    let new-spec = library-interface-spec(ld.language-definition);
    unless (ld.library-description-interface-spec = new-spec)
      ld.library-description-interface-spec := new-spec;
      ld.library-description-interface-version
        := ld.library-description-change-count;
    end;
  end;
  // Reset since this is always relative to change-count anyway, and this
  // way we lessen the chance of overflow.
  ld.library-description-models-change-count := 0;
  debug-assert(every?(compilation-record-definitions-installed?,
                      ld.library-description-compilation-records),
               "cr %s not installed!",
               choose(complement(compilation-record-definitions-installed?),
                      ld.library-description-compilation-records));
  ld.compiled-to-definitions? := #t;
end function;

// Must agree with same in boot.dylan
define constant $library-build-count-wildcard = -1;
define constant $library-build-count-wildcard-body-double = 17;
define constant $library-build-count-only-wildcard = -2;
define constant $library-build-count-only-wildcard-body-double = 31;

define function next-library-change-count (ld :: <project-library-description>)
 => (count :: <integer>)
  let (msecs, days) = current-timestamp();
  let next-count = logxor(msecs, days);
  // avoid stumbling on wildcard
  if (next-count = $library-build-count-wildcard)
    $library-build-count-wildcard-body-double
  elseif (next-count = $library-build-count-only-wildcard)
    $library-build-count-only-wildcard-body-double
  else
    next-count
  end if;
end function;

// Some caches are not incremental, so have to clear them on every change..
define method retract-non-incremental-caches (ld :: <compilation-context>) end;

define method retract-non-incremental-caches (ld :: <library-description>)
  remove-all-keys!(ld.library-type-cache);
  remove-all-keys!(ld.library-type-estimate-disjoint?-cache);
  remove-all-keys!(ld.library-type-estimate-cons-cache);
  remove-all-keys!(ld.library-type-estimate-dispatch-cache);
  retract-library-copiers(ld);
  // Clear out cache slots in imported bindings
  retract-library-imported-bindings(ld);
  next-method();
end method;

define method retract-non-incremental-caches (ld :: <dylan-library-description>)
  remove-all-keys!(ld.library-description-dylan-value-cache);
  next-method();
end method;

define function update-library-version (ld :: <library-description>, mj, mn)
  debug-out(#"driver", "Checking major/minor version changes");
  if (ld.library-description-major-version ~== mj |
        ld.library-description-minor-version ~== mn)
    // force database update with new version info.
    ld.compiled-to-definitions? := #f;
    ld.library-description-major-version := mj;
    ld.library-description-minor-version := mn;
  end;
  debug-out(#"driver", if (ld.library-description-major-version ~== mj |
                             ld.library-description-minor-version ~== mn)
                         " -> Changed, was: (%s, %s), is (%s, %s)\n"
                       else
                         " -> No change.\n"
                       end,
            ld.library-description-major-version,
            ld.library-description-minor-version,
            mj, mn);
end function;

define method compute-library-definitions (ld :: <compilation-context>)
  with-dependent-retraction
    ensure-library-definitions-installed(ld, library-only?: #f);
  end with-dependent-retraction;
end;

// External entry point
define method install-project-sources (ld :: <project-library-description>,
                                       sr* :: <source-record-sequence>,
                                       major-version, minor-version)
  with-program-conditions
    debug-out(#"driver", "Install sources %s for %s\n", sr*, ld.library-description-project);
    with-library-context (ld)
     with-stage-progress ("Updating sources for", $installing-stage)
      timing-compilation-phase ("Updating sources" of ld)
        ensure-definitions-consistent(ld, verify?: #f);
        update-library-version(ld, major-version, minor-version);
        install-library-description-sources(ld, sr*);
        if (ld.library-description-defined?)
          // Retract library definition if invalidated.
          verify-library-definition(ld);
        end;
        ensure-library-defined(ld);
      end timing-compilation-phase;
     end with-stage-progress;
    end with-library-context;
  end with-program-conditions;
end method;

// External entry point
// It is the responsibility of the caller to make sure all used libraries
// have been parsed.
define method parse-project-sources (ld :: <project-library-description>)
  let parsed? = #f;
  with-program-conditions
    debug-out(#"driver", "parse-project-sources for %s\n", ld);
    with-library-context (ld)
      ensure-definitions-consistent(ld);
      ensure-library-defined(ld);
      verify-used-libraries(ld);
      unless (ld.compiled-to-definitions?)
        with-stage-progress ("Updating definitions for", $parsing-stage)
          timing-compilation-phase ("Updating definitions" of ld)
            parsed? := #t;
            compute-library-definitions(ld);
          end;
        end;
      end;
      if (library-references-retracted-models?(ld))
        progress-line("Retracting obsoleted models");
        retract-library-compilation(ld)
      end;
    end;
    debug-out(#"driver", "DONE parse-project-sources for %s\n", ld);
  end with-program-conditions;
  parsed?
end method;

define function ensure-definitions-consistent (ld :: <project-library-description>,
                                               #key verify? = #t)
  if (compilation-definitions-inconsistent?(ld))
    progress-line("Retracting aborted parse");
    retract-library-parsing(ld);
  elseif (verify?)
    verify-library-definition(ld);
  end;
end function;

define thread variable *demand-load-library-only?* = #t;

define function ensure-library-defined (ld :: <project-library-description>)
  unless (ld.library-description-defined?)
    with-dependent-retraction
      debug-out(#"driver", "No library-definition in %s, parsing for it\n",
                ld.library-description-project);
      ensure-library-definitions-installed
        (ld, library-only?: *demand-load-library-only?*);
      debug-assert(ld.library-description-defined? |
                     // maybe completely compiled, and still no def...
                     ld.compiled-to-definitions?);
      debug-out(#"driver", "Library definition %s parsed\n", ld);
    end;
  end;
end function;


// HACK: need better solution like having a shared-symbols
//   table that can be consulted to lazily load mapped model properties
define sideways method install-dylan-shared-symbols (ld :: <dylan-library-description>)
  with-library-context (ld)
    local method touch-symbol-properties (name)
            let defn    = dylan-definition(name);
            let symbols = form-shared-symbols(defn);
            symbols
          end method;
    list(touch-symbol-properties(#"%shared-dylan-symbols"),
         touch-symbol-properties(#"%shared-streams-symbols"));
  end with-library-context;
end method;

define sideways method record-all-booted-model-properties
    (ld :: <dylan-library-description>)
  for (constant in booted-constant-definitions())
    with-dependent ($compilation of constant)
      let binding = form-variable-binding(constant);
      debug-assert(binding, "NO BINDING FOR BOOTED FORM %s", constant);
      let (model-object, computed?) =
        untracked-binding-model-object-if-computed(binding);
      when (computed?)
        let properties
          = lookup-owned-model-properties-in(ld, model-object);
        record-booted-model-properties(ld, model-object, properties);
      end when;
    end with-dependent;
  end for;
end method;

define sideways method install-dylan-boot-constants
    (ld :: <dylan-library-description>, #key force?)
  debug-assert(~*interactive-compilation-layer*);
  if (compiled-to-definitions?(ld))
    with-library-context (ld)
      let started? = ld.compilation-from-definitions-started?;
      for (constant in booted-constant-definitions())
        if (force?)
          form-models-installed?(constant) := #f;
        end if;
        with-dependent ($compilation of constant)
          maybe-compute-and-install-form-model-objects(constant);
        end;
      end;
      install-dylan-shared-symbols(ld);
      // By special dispensation, boot constants installation is not
      // considered as having started compilation... Otherwise we get an
      // infinite loop on retractions.
      ld.compilation-from-definitions-started? := started?;
    end;
  end;
end;


//// Top-level form generation.

define function update-compilation-record-definitions
    (cr :: <compilation-record>, #key library-only? = #f)
  let name = cr.compilation-record-source-record.source-record-name;
  source-record-progress-text("Parsing %s.dylan", name);
  debug-assert(~instance?(cr, <interactive-compilation-record>) |
                 (*interactive-compilation-layer* &
                    ~cr.compilation-record-definitions-installed? &
                    ~cr.compilation-record-top-level-forms),
               "Bad setup for interactive cr update");
  if (cr.compilation-record-definitions-installed?)
    debug-assert(if (cr.compilation-record-module)
                   module-defined?(cr.compilation-record-module)
                 else
                   empty?(cr.compilation-record-top-level-forms)
                 end,
                 "Undefined module in installed cr?");
    debug-out(#"internal", "  %s unchanged.\n", cr);
    source-record-progress-report();
  else
    if (cr.compilation-record-top-level-forms)
      let module = cr.compilation-record-module;
      unless (module & module-defined?(module))
        // The module undefined case means module got deleted.
        // The module = #f case means module was undefined last time parsed
        // this record, so just keep trying - retracting it is practically
        // a noop (just remove the warning from last time), and all we really
        // do is lookup the module again.
        retract-compilation-record(cr);
      end;
    end;
    with-form-creation
      let sr = cr.compilation-record-source-record;
      if (cr.compilation-record-top-level-forms)
        debug-out(#"driver", "  Reinstalling some forms in: %s.\n", sr);
      else
        let name = cr.compilation-record-source-record.source-record-name;
        progress-line("  Reading and installing: %s.dylan", name);
        compute-source-record-top-level-forms(cr);
        source-record-progress-report();
      end;
      unless (library-only? & current-library-defined?())
        let install-finished? = #f;
        block ()
          // TODO: Set this first so form processing can clear it to request
          // reprocessing.  Find some other way to do this...
          cr.compilation-record-definitions-installed? := #t;
          // This may add derived forms.
          install-top-level-forms(cr.compilation-record-top-level-forms);
          install-finished? := #t;
        cleanup
          unless (install-finished?)
            cr.compilation-record-definitions-installed? := #f;
          end;
        end block;
        // TODO: put derived forms in a separate slot so can retract them
        // (when explicit def is found later) without copying the whole
        // top-level-forms vector. See retract-form-top-level-processing.
        cr.compilation-record-top-level-forms
          := as(<vector>, cr.compilation-record-top-level-forms);
      end;
    end with-form-creation;
  end;
end function update-compilation-record-definitions;

define method compute-source-record-top-level-forms (cr :: <compilation-record>)
    with-dependent ($top-level-processing of cr)
      let module
        = lookup-compilation-record-module(cr, warn?: #f);
      compilation-record-module(cr) := module;
      if (~module)
        // Default to dylan-user while we look for namespace definitions
        compilation-record-module(cr) := dylan-user-module();
      elseif (compiling-dylan-library?() &
                instance?(module, <dylan-user-module>))
        // Kludge: the dylan-user module in the dylan library is useless,
        // since it doesn't use 'dylan'.  Normally, the dylan library has no
        // files in the dylan-user module, so it doesn't matter.  This kludge
        // is to support whole-program compilation wherein we artificially
        // introduce random source records into the dylan library.
        let internal = lookup-module(#"internal", default: #f);
        when (internal) compilation-record-module(cr) := internal end;
      end;
      // Demand load everything if we may need the modules in order to parse
      // this file.
      dynamic-bind (*demand-load-library-only?* = if (module) #t else #f end)
        let sr = compilation-record-source-record(cr);
        let cr-name = source-record-module-name(sr);
        with-input-from-source-record (stream = sr)
          local method read (state, record-forms)
            let (fragment, new-state)
              = read-top-level-fragment(stream, cr, state);
            let fragment-forms
              = if (fragment)
                  top-level-convert-forms(cr, fragment)
                else
                  #()
                end;
            let record-forms = concatenate!(record-forms, fragment-forms);
            // If we've come across a namespace definition and we're in
            // an undefined module context, switch to the module if it's
            // now become defined.
            if (~module
                   & ~empty?(fragment-forms)
                   & instance?
                       (first(fragment-forms), <namespace-defining-form>))
              let new-module = lookup-module(cr-name, default: #f);
              if (new-module)
                compilation-record-module(cr) := new-module;
                module := new-module;
              end;
              // Continue reading.
              read(new-state, record-forms);
            elseif (~module)
              // The first things we see in an undefined module context
              // must all be namespace defining forms. Empty the source
              // record, and return.
              compilation-record-module(cr) := #f;
              // Force a warning.
              lookup-compilation-record-module(cr);
              values(#f, #());
            elseif (fragment)
              // Continue reading.
              read(new-state, record-forms);
            else
              // Stop.
              values(new-state, record-forms);
            end;
          end method;
          // TODO: The db is inconsistent here for two reasons: the main
          // is that library/macro definitions get installed as they are
          // parsed, before they get recorded in cr-top-level-forms, so
          // there is no way to retract them!  Second is that parsing
          // introduces
          // dependencies of the cr on syntax of bindings, but we assume
          // that cr's with no cr-top-level-forms have no dependencies, so
          // if parsing is aborted those dependencies never get retracted.
          // TRT is to (1) update cr-top-level-forms as parse them, before
          // install and (2) add a cr-inconsistent flag and retract cr if
          // aborted, instead of invalidating the whole library.
          with-inconsistent-definitions (current-library-description())
            let (final-state, record-forms) = read(#f, #());
            compilation-record-top-level-forms(cr) := record-forms;
            compilation-record-source-line-count(cr)
              := (final-state & source-lines-read(final-state)) | 0;
          end with-inconsistent-definitions;
        end with-input-from-source-record;
      end dynamic-bind;
    end with-dependent;
end method;

define sideways method install-top-level-forms
    (forms :: <top-level-form-sequence>) => ()
  for (form in forms)
    unless (form-top-level-installed?(form))
      // TODO: make this finer-grained.
      with-inconsistent-definitions (current-library-description())
        install-top-level-form(form);
      end with-inconsistent-definitions;
    end;
  end;
end method;


