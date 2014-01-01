Synopsis: Library and module declarations for program conditions.
Author:   haahr, jonathan, keith, swm
Module:   dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-conditions
  use dylan;
  use dfmc-common;
  export dfmc-conditions;
end library dfmc-conditions;


define module dfmc-conditions
  use dylan;
  use dfmc-common;
  use dfmc-imports;

  // debugging macros
  create \with-simple-abort-retry-restart;
  
  // basic macros
  create \program-condition-definer,
         \program-condition-aux-definer,
         \program-condition-definer-definer,
         \condition-make-filter-definer,
         \convert-slots-to-ppml,
         \condition-block,
         \with-program-conditions, do-with-program-conditions,
         \accumulate-subnotes-during,
         \maybe-note,
         \note-during;

  // the class hierarchy and defining macros
  create <program-condition>,
           <program-error>,             \program-error-definer,
           <program-warning>,           \program-warning-definer,
             <serious-program-warning>, \serious-program-warning-definer,
             <run-time-error-warning>,  \run-time-error-warning-definer,
             <style-warning>,           \style-warning-definer,
           <program-note>,              \program-note-definer,
             <performance-note>,        \performance-note-definer,
             <portability-note>,        \portability-note-definer,
           <program-restart>,           \program-restart-definer;

  // the signalling protocol
  create note,
         raise,
         restart,
         simple-note,
         simple-raise;

  // slots in basic program conditions
  create condition-source-location,
         condition-program-note-creator,
         condition-compilation-stage;


  // notes about code
  create condition-context-id,
         subnotes;

  // properties of conditions
  create serious-note?,
         interesting-note?,
         obsolete-condition?,
         <ignore-serious-note>;

  // formatting and reporting
  create <detail-level>,
         report-condition,
         format-condition,
         present-program-error,
         present-program-note;

  // recording
  create library-conditions-table,
         add-program-condition,
         remove-program-conditions-from!,
         convert-condition-slots-to-ppml,
         program-note-filter,
         program-note-filter-setter,
         make-program-note-filter,
         <program-note-filter>,
         $record-program-note,
         $signal-program-error,
         $signal-program-note,
         program-note-in,
         program-note-location-between,
         program-note-class-=,
         program-note-file-name-=;


  // global data structures
  create *subnotes-queue*;

  // ugly user interface to picking restarts
  create dfmc-restart, dfmc-continue;

  // policies that should move to the policy object
  create *error-recovery-model*,
         *detail-level*;

  // workarounds for implicit macro exports not working yet
  create \condition-block-aux,
         \condition-make-method-maybe-definer,
         ;

end module dfmc-conditions;

define module dfmc-conditions-implementation
  // things we use in the implementation
  use dylan;
  use dfmc-common;
  use dfmc-imports;
  // the interface we define
  use dfmc-conditions;
end module dfmc-conditions-implementation;
