Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Splitters

define protocol <<splitter-protocol>> ()
  getter gadget-ratios
    (gadget :: <abstract-gadget>) => (ratios :: false-or(<sequence>));
  setter gadget-ratios-setter
    (ratios :: false-or(<sequence>), gadget :: <abstract-gadget>)
 => (ratios :: false-or(<sequence>));
end protocol <<splitter-protocol>>;

define open abstract class <splitter> (<layout>, <basic-gadget>)
  // Initial set of layout ratios for the splitter
  slot gadget-ratios :: false-or(<sequence>) = #f,
    init-keyword: ratios:;
  // This callback gets called when the split box is clicked on;
  // the callback is responsible for doing the actual splitting
  sealed slot splitter-split-box-callback :: <callback-type> = #f,
    init-keyword: split-box-callback:;
  // This callback gets called after a draggable border has been
  // moved and after the relayout has been done
  sealed slot splitter-split-bar-moved-callback :: <callback-type> = #f,
    init-keyword: split-bar-moved-callback:;
  sealed slot %horizontal-split-box? = #t,
    init-keyword: horizontal-split-box?:;
  sealed slot %vertical-split-box?   = #t,
    init-keyword: vertical-split-box?:;
end class <splitter>;

define method initialize
    (pane :: <splitter>, #key split-box? = $unsupplied) => ()
  next-method();
  when (supplied?(split-box?))
    pane.%horizontal-split-box? := split-box?;
    pane.%vertical-split-box?   := split-box?
  end
end method initialize;


// Arranges kids in a row, allows a vertical split box
define open abstract class <row-splitter> (<splitter>)
  keyword horizontal-split-box?: = #f;
  keyword vertical-split-box?:   = #t;
end class <row-splitter>;

// Arranges kids in a column, allows a horizontal split box
define open abstract class <column-splitter> (<splitter>)
  keyword horizontal-split-box?: = #t;
  keyword vertical-split-box?:   = #f;
end class <column-splitter>;


define method execute-split-box-callback
    (gadget :: <splitter>, client, id, box) => ()
  ignore(client, id);
  let callback = splitter-split-box-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget, box)
  else
    do-execute-split-box-callback(gadget, client, id, box)
  end
end method execute-split-box-callback;

define method do-execute-split-box-callback
    (gadget :: <splitter>, client, id, box) => ()
  ignore(client, id, box);
  #f
end method do-execute-split-box-callback;

define method execute-split-bar-moved-callback
    (gadget :: <splitter>, client, id, pane1, pane2) => ()
  ignore(client, id);
  let callback = splitter-split-bar-moved-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget, pane1, pane2)
  else
    do-execute-split-bar-moved-callback(gadget, client, id, pane1, pane2)
  end
end method execute-split-bar-moved-callback;

define method do-execute-split-bar-moved-callback
    (gadget :: <splitter>, client, id, pane1, pane2) => ()
  ignore(client, id);
  #f
end method do-execute-split-bar-moved-callback;
