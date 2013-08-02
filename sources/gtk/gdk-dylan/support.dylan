module: gdk
synopsis: Support functions for the Gdk library.
copyright: See LICENSE file in this distribution.

define method g-value-to-dylan-helper
    (type == #"GdkEvent", address)
 => (dylan-instance :: <C-void*>)
  let event = make(<GdkEventAny>, address: address);
  make(select(event.gdk-event-any-type)
         $gdk-nothing           => <GdkEventAny>;
         $gdk-delete            => <GdkEventAny>;
         $gdk-destroy           => <GdkEventAny>;
         $gdk-expose            => <GdkEventExpose>;
         $gdk-motion-notify     => <GdkEventMotion>;
         $gdk-button-press      => <GdkEventButton>;
         $gdk-2button-press     => <GdkEventButton>;
         $gdk-3button-press     => <GdkEventButton>;
         $gdk-button-release    => <GdkEventButton>;
         $gdk-key-press         => <GdkEventKey>;
         $gdk-key-release       => <GdkEventKey>;
         $gdk-enter-notify      => <GdkEventCrossing>;
         $gdk-leave-notify      => <GdkEventCrossing>;
         $gdk-focus-change      => <GdkEventFocus>;
         $gdk-configure         => <GdkEventConfigure>;
         $gdk-map               => <GdkEventAny>;
         $gdk-unmap             => <GdkEventAny>;
         $gdk-property-notify   => <GdkEventProperty>;
         $gdk-selection-clear   => <GdkEventSelection>;
         $gdk-selection-request => <GdkEventSelection>;
         $gdk-selection-notify  => <GdkEventSelection>;
         $gdk-proximity-in      => <GdkEventProximity>;
         $gdk-proximity-out     => <GdkEventProximity>;
         $gdk-drag-enter        => <GdkEventDND>;
         $gdk-drag-leave        => <GdkEventDND>;
         $gdk-drag-motion       => <GdkEventDND>;
         $gdk-drag-status       => <GdkEventDND>;
         $gdk-drop-start        => <GdkEventDND>;
         $gdk-drop-finished     => <GdkEventDND>;
         $gdk-visibility-notify => <GdkEventAny>;
         $gdk-scroll            => <GdkEventScroll>;
         $gdk-window-state      => <GdkEventWindowState>;
         $gdk-setting           => <GdkEventSetting>;
         $Gdk-owner-change      => <GdkEventOwnerChange>;
         $gdk-grab-broken       => <GdkEventGrabBroken>;
         otherwise => <GdkEventAny>;
       end, address: address);
end method g-value-to-dylan-helper;
