module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <sequence-view> (<grid-view>)
  slot sequence :: <sequence>,
     // getter: sequence,
     setter: %sequence-setter,
     init-keyword: sequence:;
  
  slot draw-function,
     init-value: #f,
     init-keyword: draw-function:;
  
  slot click-function,
     init-value: #f,
     init-keyword: click-function:;
  end class;

define method initialize (view :: <sequence-view>, #key sequence: the-sequence = #())
  next-method();
  
  view.sequence := the-sequence;
end method;

define method behavior-event (behavior :: <grid-select-behavior>,
                              next :: <list>,
                              view :: <sequence-view>,
                              event :: <mouse-down-event>,
                              id :: <symbol>)
  ignore(behavior, next, id);
  
  next-method();
  
  let function = view.click-function;
  if (function)
    let (row, column) = point-to-cell(view, event.local-mouse);
    if (row < size(view.sequence)) // hack until framework fixed.
      function(view, row, column, double-click?: double-click?(event));
    end;
  end if;
end method;

define method draw-cell (view :: <sequence-view>,
                         row :: <integer>,
                         column :: <integer>)
  let function = view.draw-function;
  if (function)
    let horiz = column-to-offset(view, column);
    let vert = row-to-offset(view, row);
    function(view, row, column, 
             rect(vert, horiz,
                  vert + row-height(view, row), horiz + column-width(view, column)));
  end if;
end method;

define method sequence-setter (new-sequence :: <sequence>, view :: <sequence-view>,
                               #key invalidate? = #t)
  view.%sequence := new-sequence;
  view.columns := 1;
  view.rows := size(new-sequence);
  view.extent := point(column-to-offset(view, view.columns - 1) +
                          column-width(view, view.columns - 1),
                          row-to-offset(view, view.rows - 1) +
                          row-height(view, view.rows - 1));
  // view.selected-cells := #();
  if (invalidate?) invalidate-all(view); end;
end;

define constant test-draw-method =
     method(view, row, column, rect)
       ignore(view);
       inset-region(rect, 1, 1);
       let str = concatenate(int-to-string(row), ", ", int-to-string(column));
       draw-just-string(str, rect, #"left");
     end;

define constant test-click-method =
     method(view, row, column, #key double-click?)
       ignore(view, row, column, double-click?);
       beep();
     end;

/* patches for framework: */

// excluded source
// define method view-size-setter (size :: <point>, view :: <grid-view>)
//   next-method(view, point(min(size.h, column-to-offset(view, view.columns)),
//                           min(size.v, row-to-offset(view, view.rows))));
// end method;

/* - end patches. */
