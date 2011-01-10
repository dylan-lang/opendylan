Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
/// Implementation of output-records that contain gadgets

define sealed class <gadget-record> (<basic-composite-record>)
  sealed slot output-record-gadget = #f;
  sealed slot %optional-values = #[];
  sealed slot %cache-value = #f, init-keyword: cache-value:;
end class <gadget-record>;

define sealed domain make (singleton(<gadget-record>));
define sealed domain initialize (<gadget-record>);

define method initialize (record :: <gadget-record>, #key cache-test)
  // Accept 'cache-test:' as an initarg
  ignore(cache-test);
  next-method()
end method initialize;

define method do-recompute-region
	(record :: <gadget-record>) => (left, top, right, bottom)
  box-edges(record)
end method do-recompute-region;

define method note-transform-changed
    (record :: <gadget-record>) => ()
  next-method();
  update-gadget-position(record)
end method note-transform-changed;

define method update-gadget-position (record :: <gadget-record>) => ()
  let gadget = output-record-gadget(record);
  when (gadget & output-record-sheet(record))
    let (x, y) = sheet-position(record);
    set-sheet-position(gadget, x, y)
  end
end method update-gadget-position;

define method handle-repaint
    (record :: <gadget-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region, medium);
  let gadget = output-record-gadget(record);
  when (sheet-mapped?(gadget))
    //---*** Does this need to reset the medium state?
    //---*** Does it need to look at 'port-handles-repaint?'?
    // Use '$everywhere' to ensure the gadget really gets drawn
    repaint-sheet(gadget, $everywhere)
  end
end method handle-repaint;

// Options can be X:, Y:, MOVE-CARET?:, and gadget initargs
define macro with-output-as-gadget
  { with-output-as-gadget (?sheet:variable, #rest ?options:expression) ?:body end }
    => { begin
           let with-output-as-gadget-body
             = method (_framem)
                 with-frame-manager (_framem)
                   ?body
                 end
               end method;
           do-with-output-as-gadget(?sheet, with-output-as-gadget-body, ?options)
         end }
end macro with-output-as-gadget;

define method do-with-output-as-gadget
    (sheet, continuation :: <function>, #rest initargs,
     #key x, y, move-caret? = #t, cache-test, cache-value, update-gadget)
 => (gadget :: <sheet>, record :: <output-record>)
  dynamic-extent(initargs);
  ignore(cache-test, cache-value);
  with-keywords-removed (initargs = initargs, #[update-gadget:, move-caret?:])
    let framem = frame-manager(sheet);
    unless (x & y)
      let (_x, _y) = sheet-caret-position(sheet);
      x := _x;
      y := _y
    end;
    let new = #f;
    let gadget = #f;
    let record
      = with-output-recording-options (sheet, draw?: #f, record?: #t)
          local method with-new-output-record-body (record) => ()
		  unless (gadget := output-record-gadget(record))
		    let (#rest vals) = continuation(framem);
		    gadget := head(vals);
		    record.%optional-values := tail(vals);
		    new := #t
		  end
		end method;
          dynamic-extent(with-new-output-record-body);
          apply(do-with-new-output-record, sheet, with-new-output-record-body,
		record-class: <gadget-record>,
		constructor: <gadget-record>-constructor, initargs)
        end;
    case
      new =>
        attach-gadget-record(record, gadget, sheet, x, y);
      update-gadget =>
        apply(update-gadget, record, gadget, record.%optional-values)
    end;
    repaint-sheet(record, $everywhere);
    when (move-caret?)
      move-caret-beyond-output-record(sheet, record)
    end;
    values(gadget, record)
  end
end method do-with-output-as-gadget;

//---*** Finish this
define method attach-gadget-record
    (record :: <gadget-record>, gadget :: <sheet>, sheet :: <sheet>, x, y)
  // Just in case
  sheet-mapped?(gadget) := #f;
  add-child(sheet, gadget);
  // In order to be able to determine the space the gadget has to be
  // added to the parent and, hopefully, grafted
  assert(port(sheet),
	 "The gadget %= is not attached", gadget);
  output-record-gadget(record) := gadget;
  let space-req = compose-space(gadget);
  begin
    let (abs-x, abs-y) = point-position(sheet-output-record-position(sheet));
    dec!(x, abs-x);
    dec!(y, abs-y);
    let (cx, cy) = stream-caret-position(sheet);
    record.%start-x := cx - abs-x;
    record.%start-y := cy - abs-y;
    let (w, w-, w+, h, h-, h+) = space-requirement-components(gadget, space-req);
    ignore(w-, w+, h-, h+);
    sheet-region(record)
      := set-box-edges(sheet-region(record), x, y, x + w, y + h);
    //--- We could probably just use 'recompute-extent-for-changed-child'
    when (sheet-parent(record))
      recompute-region(record)
    end
  end;
  when (output-record-sheet(record))
    update-gadget-position(record);
    sheet-mapped?(gadget) := #t
  end
end method attach-gadget-record;

//---*** Is this just 'note-child-added'?  Or is is 'note-sheet-grafted'?
define method note-output-record-attached (record :: <gadget-record>, sheet)
  ignore(sheet);
  next-method();
  when (output-record-gadget(record) & port(output-record-gadget(record)))
    update-gadget-position(record);
    update-gadget-state(record, #t)
  end
end method note-output-record-attached;

//---*** Is this just 'note-child-removed'?  Or is is 'note-sheet-degrafted'?
define method note-output-record-detached (record :: <gadget-record>)
  next-method();
  update-gadget-state(record, #f)
end method note-output-record-detached;

define thread variable *deferred-gadget-updates* = #f;

define macro with-deferred-gadget-updates
  { with-deferred-gadget-updates ?:body end }
    => { begin
           let with-deferred-gadget-updates-body
             = method (_framem) ?body end;
           do-with-deferred-gadget-updates(with-deferred-gadget-updates-body)
         end }
end macro with-deferred-gadget-updates;

define method do-with-deferred-gadget-updates (continuation :: <function>) => (#rest values)
  if (*deferred-gadget-updates*)
    continuation()
  else
    dynamic-bind (*deferred-gadget-updates* = make(<stretchy-vector>))
      block ()
        continuation()
      cleanup
        let _list
          = remove-duplicates(*deferred-gadget-updates*,
                              test: method (_x, _y) head(_x) == head(_y) end);
        do(method (x) sheet-mapped?(first(x)) := second(x) end,
           _list);
        _list
      end
    end
  end
end method do-with-deferred-gadget-updates;

define method update-gadget-state (record :: <gadget-record>, state)
  if (*deferred-gadget-updates*)
    add!(*deferred-gadget-updates*, list(output-record-gadget(record), state))
  else
    sheet-mapped?(output-record-gadget(record)) := state
  end
end method update-gadget-state;
*/
