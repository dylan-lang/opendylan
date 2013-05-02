Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <mutable-sequence> ... end;


////////////
// INTERFACE
////////////

// Functions on <mutable-sequence>

define sealed generic first-setter
  (new-value, sequence :: <mutable-sequence>) => (new-value);

define sealed generic second-setter
  (new-value, sequence :: <mutable-sequence>) => (new-value);

define sealed generic third-setter
  (new-value, sequence :: <mutable-sequence>) => (new-value);


// Open generics on <mutable-sequence>

define open generic last-setter
  (new-value, sequence :: <mutable-sequence>) => (new-value);



/////////////////
// IMPLEMENTATION
/////////////////


//
// FIRST-SETTER
///////////////

define sealed inline method first-setter (new-value, sequence :: <mutable-sequence>)
 => (new-value);
  sequence[0] := new-value
end method;


//
// SECOND-SETTER
////////////////

define sealed inline method second-setter (new-value, sequence :: <mutable-sequence>)
 => (new-value);
  sequence[1] := new-value
end method;


//
// THIRD-SETTER
///////////////

define sealed inline method third-setter (new-value, sequence :: <mutable-sequence>)
 => (new-value);
  sequence[2] := new-value
end method;


//
// LAST-SETTER
//////////////

define method last-setter (new-value, sequence :: <mutable-sequence>)
 => (new-value);
  sequence[sequence.size - 1] := new-value
end method last-setter;


//
// Specialized inherited generic methods
//


//
// REPLACE-SUBSEQUENCE!
//

define method replace-subsequence!
    (target :: <mutable-sequence>, insert :: <sequence>,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <sequence>);
  let target-size :: <integer> = target.size;
  let insert-size :: <integer> = insert.size;
  let last :: <integer> = check-start-compute-end(target, start, last);
  let delete-size :: <integer> = last - start;

  if (delete-size = insert-size)
    // Can modify in place
    with-fip-of target
      for (state = initial-state then next-state(target, state),
           index from 0 below start)
      finally
        for (state = state then next-state(target, state),
             e in insert)
          current-element(target, state) := e;
        end for
      end for
    end with-fip-of;
    target
  else // The fun starts here
    // Don't know if target is stretchy so we have to create a new one
    let new-size :: <integer> = target-size - delete-size + insert-size;
    let new-target = make(type-for-copy(target), size: new-size);
    with-fip-of new-target
      if (start = 0 & last = target-size)
        // Don't need to copy from target
        for (state = initial-state then next-state(new-target, state),
             e in insert) /* Use with-setter? */
          current-element(new-target, state) := e;
        end
      else
        // Have to copy stuff from target
        with-fip-of target with prefix o-
          for (state = initial-state then next-state(new-target, state),
               o-state = o-initial-state then o-next-state(target, o-state),
               index from 0 below start)
            current-element(new-target, state) :=
              o-current-element(target, o-state);
          finally // Copy insert to new target
            for (state = state then next-state(new-target, state),
                 e in insert)
              current-element(new-target, state) := e;
            finally // Copy rest of target if necessary
              if (last = target-size)
                new-target
              else
                for (o-state = o-state then next-state(target, o-state),
                     index from start below last) // Skip replaced segment
                finally
                  for (state = state then next-state(new-target, state),
                       o-state = o-state
                         then o-next-state(target, o-state),
                       index from last below target-size)
                    current-element(new-target, state) :=
                      o-current-element(target, o-state);
                  end for
                end for
              end if
            end for
          end for
        end with-fip-of
      end if
    end with-fip-of;
    new-target
  end if
end method replace-subsequence!;


//
// FILL!
//

define method fill!
    (target :: <mutable-sequence>, value,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (target :: <mutable-sequence>)
  if (start = 0 & last.unsupplied?) next-method()
  else
    with-fip-of target
      for (state = initial-state then next-state(target, state),
           index from 0,
           until: start == index | finished-state?(target, state, limit))
      finally
        case
          index ~== start => invalid-sequence-start-error(target, start);
          unsupplied?(last) =>
            // Modify elements to the end of the sequence
            for (state = state then next-state(target, state),
                 until: finished-state?(target, state, limit))
              current-element(target, state) := value
            end for;
          last >= index =>
            // Modify elements from start to end or size of sequence,
            // and then check that end was valid.
            let last :: <integer> = last;
            for (state = state then next-state(target, state),
                 index from index,
                 until: index == last | finished-state?(target, state, limit))
              current-element(target, state) := value
            finally
              unless (index == last)
                invalid-sequence-end-error(target, last)
              end
            end for;
          otherwise => invalid-sequence-bounds-error(target, start, last);
        end
      end
    end;
    target
  end if
end method fill!;


//
// ITERATION PROTOCOL
//

define inline method forward-iteration-protocol (sequence :: <mutable-sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(0,
         sequence.size,
         sequence-next-state,
         sequence-finished-state?,
         sequence-current-key,
         element,
         element-setter,
         identity-copy-state)
end method forward-iteration-protocol;


define inline method backward-iteration-protocol (sequence :: <mutable-sequence>)
 => (final-state :: <integer>, limit :: <integer>,
     previous-state :: <function>,   finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,  current-element-setter :: <function>,
     copy-state :: <function>);
  values(sequence.size - 1,
         -1,
         sequence-previous-state,
         sequence-finished-state?,
         sequence-current-key,
         element,
         element-setter,
         identity-copy-state)
end method backward-iteration-protocol;
