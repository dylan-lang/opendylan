module:    threads-primitives
Synopsis:  The implementation of low-level classes for use with threads
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define generic handle1 
    (obj :: <object>) => (h :: <raw-pointer>);

define generic handle1-setter
    (newval :: <raw-pointer>, obj :: <object>) => (h :: <raw-pointer>);

define generic handle2 
    (obj :: <object>) => (h :: <object>);

define generic handle2-setter
    (newval :: <object>, obj :: <object>) => (h :: <object>);



/// Portable container objects have raw slots which are managed at the primitive level


define abstract primary class <portable-container> (<object>)

  slot handle1 :: <raw-pointer>;

end class;



/// This one has 2 raw slots

define abstract primary class <portable-double-container> (<portable-container>)

  slot handle2 :: <object>;

end class;



/// <optional-name> is not exactly related, but gets defined here anyway

define constant <optional-name> = type-union(<byte-string>, singleton(#f));
