Module:    Dylan-User
Synopsis:  Environment code viewer
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-code-viewer
  use environment-imports;
  use environment-protocols;

  use duim-internals;

  // Code-viewer
  export <code-viewer>,
         code-viewer-definition, code-viewer-definition-setter,
         code-viewer-current-location, code-viewer-current-location-setter;

  // Breakpoint button
  export <breakpoint-button>,
         breakpoint-button-definition, breakpoint-button-definition-setter,
         breakpoint-button-code-location, breakpoint-button-code-location-setter,
         breakpoint-button-type, breakpoint-button-type-setter,
         breakpoint-button-enabled?, breakpoint-button-enabled?-setter;

  //---*** Should we export this, or should we provide this information through
  //       some other means?
  export $breakpoint-button-column-width;

  // Breakpoint popup menu
  //---*** It isn't yet clear how this menu should be handled. Currently it is
  //       special in that its contents are static. Other popup menus are
  //       automatically constructed by filling in all applicable commands.
  export make-breakpoint-popup-menu,
         breakpoint-popup-menu-type, breakpoint-popup-menu-type-setter,
         breakpoint-popup-menu-enabled?, breakpoint-popup-menu-enabled?-setter;

  // Current location indicator
  export <current-location-indicator>;

end module environment-code-viewer;
