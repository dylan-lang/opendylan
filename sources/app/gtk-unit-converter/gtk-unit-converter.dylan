module: gtk-unit-converter
synopsis: A simple unit converter that uses the Gtk+ toolkit
author: Francesco Ceccon
License: See License.txt in this distribution for details.

define constant $fahrenheit2celsius = #"f2c";
define constant $celsius2fahrenheit = #"c2f";

define table $available-conversions = {
  // Temperature
  $fahrenheit2celsius => "Fahrenheit to Celsius",
  $celsius2fahrenheit => "Celsius to Fahrenheit"
};

define method convert
    (type, value)
 => (result)
    "ERROR"
end;

define method convert
    (type == $fahrenheit2celsius, value :: <integer>)
 => (degrees-celsius :: <float>)
    (value - 32.0) * 5.0 / 9.0
end method convert;

define method convert
    (type == $celsius2fahrenheit, value :: <integer>)
 => (degrees-fahrenheit :: <float>)
    value * 9.0 / 5.0 + 32.0
end method convert;

define method conversion-values-changed
    (combo :: <GtkComboBox>, entry :: <GtkEntry>, text :: <GtkLabel>)
 => ()
  let active-id = gtk-combo-box-get-active-id(combo);
  let result = if (gtk-entry-get-text-length(entry) > 0)
                 convert(as(<symbol>, active-id),
                         string-to-integer(gtk-entry-get-text(entry), default: 0));
               else
                 "0";
               end if;
  text.@label := format-to-string("%s", result);
end method conversion-values-changed;

define function display-about-dialog () => ()
  let about = gtk-about-dialog-new();
  about.@license-type := $gtk-license-mit-x11;
  about.@program-name := "Gtk+ Unit Converter";
  about.@website := "http://opendylan.org";
  about.@comments := "Simple example demonstrating how to use Gtk+ with Dylan";
  gtk-dialog-run(about);
  gtk-widget-destroy(about);
end function display-about-dialog;

define function create-window () => (window :: <GtkWindow>)
  let window = gtk-window-new($gtk-window-toplevel);
  gtk-window-set-title(window, "Gtk+ Unit Converter");
  gtk-window-set-position(window, $gtk-win-pos-center);
  gtk-window-set-default-size(window, 300, 200);

  // Setup UI
  let box = gtk-box-new($gtk-orientation-vertical, 5);
  let conversions-combo = gtk-combo-box-text-new();
  for (value keyed-by key in $available-conversions)
    gtk-combo-box-text-append(conversions-combo, as(<string>, key), value);
  end for;
  gtk-combo-box-set-active(conversions-combo, 0);

  let text-entry = gtk-entry-new();
  let result-label = gtk-label-new("0");

  // Menu Bar
  let menubar-box = gtk-vbox-new(#f, 0);
  let menubar = gtk-menu-bar-new();

  // File menu
  let file-menu = gtk-menu-new();
  let file-menu-item = gtk-menu-item-new-with-label("File");
  let quit-menu-item = gtk-menu-item-new-with-label("Quit");
  g-signal-connect(quit-menu-item, "activate", method(#rest args) gtk-main-quit() end);

  gtk-menu-item-set-submenu(file-menu-item, file-menu);
  gtk-menu-shell-append(file-menu, quit-menu-item);
  gtk-menu-shell-append(menubar, file-menu-item);

  // Help menu
  let help-menu = gtk-menu-new();
  let help-menu-item = gtk-menu-item-new-with-label("Help");
  let about-menu-item = gtk-menu-item-new-with-label("About");
  g-signal-connect(about-menu-item, "activate", method(#rest args) display-about-dialog() end);

  gtk-menu-item-set-submenu(help-menu-item, help-menu);
  gtk-menu-shell-append(help-menu, about-menu-item);
  gtk-menu-shell-append(menubar, help-menu-item);

  gtk-box-pack-start(menubar-box, menubar, #f, #f, 3);

  gtk-box-pack-start(box, menubar-box, #t, #t, 0);
  gtk-box-pack-start(box, conversions-combo, #t, #t, 0);
  gtk-box-pack-start(box, text-entry, #t, #t, 0);
  gtk-box-pack-start(box, result-label, #t, #t, 0);



  gtk-container-add(window, box);

  // Connect signals
  g-signal-connect(window, "destroy", method(#rest args) gtk-main-quit() end);
  g-signal-connect(conversions-combo,
                   "changed",
                   method(#rest args)
                     conversion-values-changed(conversions-combo, text-entry, result-label);
                   end);
  g-signal-connect(text-entry,
                   "key-release-event",
                   method(#rest args)
                     conversion-values-changed(conversions-combo, text-entry, result-label);
                   end);

  window
end function create-window;

define function main (name :: <string>, arguments :: <vector>)
  gtk-init(0, "");

  let w = create-window();

  gtk-widget-show-all(w);
  gtk-main();
  exit-application(0);
end function main;

main(application-name(), application-arguments());
