Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-framework
  use functional-dylan;
  export dylan-framework;
end library;

define module dylan-framework
  use functional-dylan;
  export
    <point>, point,

    <region>, as-rect, 
    bounds, top, bottom, left, right,

    <view>, add-idler, remove-idler, do-setup-menus, 
    \with-focused-view, call-with-focused-view,
    idle,
    extent, v, h,

    <tracker>, track-feedback, tracker-view,

    <event-handler>,
    <event>, do-event,
    <mouse-down-event>, local-mouse,
    <menu-event>,

    <behavior>, add-behavior, behavior-setup-menus, behavior-event,

    install-menu, get-resource-menu, enable-item, about-item, mark-item,
    menu-item, title, title-setter, mark, mark-setter, do-marking,

    draw, 

    $blackColor, $cyanColor,
    $patBic, $patCopy,
    Line, LineTo, ForeColor, MoveTo, FrameRect, SetRect, PenMode,
    <QDRect>, SetRect, FrameRect,

    tell-dialog,

    open, close, make-scrolling-window,

    invalidate-all, track-mouse,

    define-framework-library, set-library-init-function,

    random, \with-stack-structure;

end module;

// Drawing: Line, LineTo, ForeColor, MoveTo, FrameRect, SetRect,
// Other: random, with-stack-structure, string-to-int, penmode,

// eof
