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

define C-function %gtk-file-chooser-dialog-new
  input parameter title_ :: <C-string>;
  input parameter parent_ :: <GtkWindow>;
  input parameter action_ :: <GtkFileChooserAction>;
  input parameter first-button-text_ :: <C-string>;
  result res :: <GtkWidget>;
  c-name: "gtk_file_chooser_dialog_new";
end;

define function gtk-file-chooser-dialog-new
    (title :: <string>, parent :: <GtkWindow>, action :: <integer>)
 => (dialog :: <GtkFileChooserDialog>)
  %gtk-file-chooser-dialog-new(title, parent, action, null-pointer(<C-string>));
end;
