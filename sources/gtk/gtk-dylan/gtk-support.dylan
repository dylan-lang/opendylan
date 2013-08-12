module: gtk
synopsis: constants defined in the Gtk library
copyright: See LICENSE file in this distribution.

define C-function gtk-set-button-time
  input parameter event :: <GdkEventButton>;
  c-name: "gtk_set_button_time";
end;

define C-function popup-gtk-menu
  input parameter menu :: <GtkMenu>;
  input parameter button :: <C-unsigned-int>;
  c-name: "popup_gtk_menu";
end;
