Module:       gtk-duim
Synopsis:     GTK gadget implementation
Author:       Hannes Mehnert, Andreas Bogk
Copyright:    (c) 2007 Dylan Hackers
              All rights reserved.
License:      GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define sealed class <gtk-column-layout>
    (<gtk-gadget-mixin>,
     <column-layout>,
     <sealed-constructor-mixin>)
end class <gtk-separator>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <column-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-column-layout>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-column-layout>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = gtk-vbox-new(0, 1); // fix spacing!
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
  end
end method make-gtk-mirror;
*/
