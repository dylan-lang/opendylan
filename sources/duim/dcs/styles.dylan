Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Style descriptors

define open generic default-foreground
    (object) => (ink :: false-or(<ink>));
define open generic default-foreground-setter
    (ink :: false-or(<ink>), object) => (ink :: false-or(<ink>));

define open generic default-background
    (object) => (ink :: false-or(<ink>));
define open generic default-background-setter
    (ink :: false-or(<ink>), object) => (ink :: false-or(<ink>));

define open generic default-text-style
    (object) => (text-style :: false-or(<text-style>));
define open generic default-text-style-setter
    (text-style :: false-or(<text-style>), object) => (text-style :: false-or(<text-style>));


define sealed class <style-descriptor> (<object>)
  // Default these to #f so that the port can fill them in appropriately
  sealed slot default-foreground :: false-or(<ink>) = #f,
    init-keyword: foreground:;
  sealed slot default-background :: false-or(<ink>) = #f,
    init-keyword: background:;
  sealed slot default-text-style :: false-or(<text-style>) = #f,
    init-keyword: text-style:;
end class <style-descriptor>;

define sealed domain make (singleton(<style-descriptor>));
define sealed domain initialize (<style-descriptor>);
