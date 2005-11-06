Module:        GObject-2
Synopsis:      Manually coded additions to the automatic translation.
Copyright:     Copyright (C) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-license:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define opaque-structure <_GParamSpecPool>;
define opaque-structure <_GTypeCValue>;
define opaque-structure <_GTypePlugin>;
define opaque-structure <_GValue>;

define method g-signal-connect
    (object :: <gpointer>, detailed-signal :: <byte-string>,
     function :: <GCallback>, data :: <gpointer>)
 => (value)
  with-c-string (string = detailed-signal)
    g-signal-connect-data(object, string, function, data,
                          null-pointer(<GClosureNotify>), 0)
  end with-c-string
end method g-signal-connect;
