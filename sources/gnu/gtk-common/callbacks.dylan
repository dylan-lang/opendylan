Module:    gtk-common
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// GTK callbacks via `define callback'

define macro delete-event-callback-definer
  { define delete-event-callback ?new:name = ?old:name }
 => { define C-callable-wrapper ?new of ?old
	parameter widget :: <GtkWidget*>;
	parameter event  :: <GdkEvent*>;
	parameter data   :: <gpointer>;
	result    value :: <guint>;
      end C-callable-wrapper }
end macro delete-event-callback-definer;

define macro gtk-callback-definer
  { define gtk-callback (?new:name, ?data-type:name) = ?old:name }
 => { define C-callable-wrapper ?new of ?old
	parameter widget :: <GtkWidget*>;
	parameter data  :: ?data-type;
      end C-callable-wrapper }
end macro gtk-callback-definer;

define macro widget-callback-definer
  { define widget-callback ?new:name = ?old:name }
 => { define gtk-callback (?new, <gpointer>) = ?old }
end macro widget-callback-definer;

define macro configure-callback-definer
  { define configure-callback ?new:name = ?old:name }
 => { define gtk-callback (?new, <GdkEventConfigure*>) = ?old }
end macro configure-callback-definer;

define macro motion-callback-definer
  { define motion-callback ?new:name = ?old:name }
 => { define gtk-callback (?new, <GdkEventMotion*>) = ?old }
end macro motion-callback-definer;

define macro expose-callback-definer
  { define expose-callback ?new:name = ?old:name }
 => { define gtk-callback (?new, <GdkEventExpose*>) = ?old }
end macro expose-callback-definer;
