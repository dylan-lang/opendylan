module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library oww-othello-names

  use dylan;
  use idvm-namespace;

  //use random;
  // use transcendentals;

  use mini-duim;
  // use scribble;
  use othello;
  use plugin-othello;
  use netscape-plugin;

  export oww-names;

end library oww-names;



define module mini-duim-names

  use dylan;
  use idvm-namespace;
  use mini-duim;

end module mini-duim-names;


define module othello-names

  use dylan;
  use idvm-namespace;
  use othello;

end module othello-names;

/*
define module scribble-names

  use dylan;
  use idvm-namespace;
  use scribble;

end module scribble-names;

define module random-names

  use dylan;
  use idvm-namespace;
  use random;

end module random-names;

define module transcendentals-names

  use dylan;
  use idvm-namespace;
  use transcendentals;

end module transcendentals-names;

*/


define module netscape-plugin-names

  use dylan;
  use idvm-namespace;
  use netscape-plugin-interface;
  use netscape-plugin-shell-interface;
  use netscape-plugin-implementation;
  use netscape-plugin;

end module netscape-plugin-names;

define module oww-names

  use dylan;
  use idvm-namespace;

end module oww-names;
