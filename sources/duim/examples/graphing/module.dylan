Module:       dylan-user
Author:       Scott McKay
Synopsis:     Simple bar charts for DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-graphing
  create <bar-chart>,
	 <bar-chart-pane>;

  create <bar>,
	 <bar-pane>,
	 bar-scaling, bar-scaling-setter,
	 bar-minor-size, bar-minor-size-setter,
	 bar-spacing, bar-spacing-setter;
end module duim-graphing;

define module duim-graphing-internals
  use functional-dylan;
  use duim-internals;
  use duim-graphing;
end module duim-graphing-internals;
