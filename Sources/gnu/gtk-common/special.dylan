Module:    gtk-common
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Type coercion functions

define macro gtk-type-cast-function-definer
  { define gtk-type-cast-function ?name:name => ?type:name }
 => { define inline function ?name
	  (pointer :: <C-pointer>) => (object :: ?type)
	pointer-cast(?type, pointer)
      end }
end macro gtk-type-cast-function-definer;

define gtk-type-cast-function GTK-OBJECT        => <GtkObject*>;
define gtk-type-cast-function GTK-CONTAINER     => <GtkContainer*>;
define gtk-type-cast-function GTK-BOX           => <GtkBox*>;
define gtk-type-cast-function GTK-WIDGET        => <GtkWidget*>;
define gtk-type-cast-function GTK-ADJUSTMENT    => <GtkAdjustment*>;
define gtk-type-cast-function GTK-FIXED         => <GtkFixed*>;
define gtk-type-cast-function GTK-WINDOW        => <GtkWindow*>;
define gtk-type-cast-function GTK-DRAWING-AREA  => <GtkDrawingArea*>;


/// Useful functions

define function initialize-gtk
    () => ()
  let name = application-name();
  with-c-string (string = name)
    let string* = make(<C-string*>, element-count: 1);
    string*[0] := string;
    let string** = make(<C-string**>);
    string**[0] := string*;
    let int* = make(<C-int*>);
    int*[0] := 1;
    gtk-init(int*, string**);
    destroy(string*);
    destroy(string**);
    destroy(int*)
  end
end function initialize-gtk;

define method gtk-signal-connect*
    (object :: <GtkObject*>, name :: <byte-string>, func :: <GtkSignalFunc>,
     func_data :: <gpointer>)
 => (value)
  with-c-string (string = name)
    gtk-signal-connect(object, string, func, func_data)
  end
end method gtk-signal-connect*;

define method gtk-signal-connect-object*
    (object :: <GtkObject*>, name :: <byte-string>, func :: <GtkSignalFunc>,
     slot_object :: <GtkObject*>)
 => (value)
  with-c-string (string = name)
    gtk-signal-connect-object(object, string, func, slot_object)
  end
end method gtk-signal-connect-object*;
