Module:    glib
Synopsis:  Additional declarations to be loaded before the automatically
	   converted files.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The following slot accessor functions need to be declared as open generics
// because they have methods defined in more than one library. 

define macro open-accessor-definer
  { define open-accessor ?name:name }
 => { define open inline-only generic ?name (struct) => (slot-value);
      define open inline-only generic ?name ## "-setter" 
          (new, struct) => (new); }
end macro open-accessor-definer;

define open-accessor area-value;
define open-accessor background-value;
define open-accessor bg-gc-value;
define open-accessor bin-value;
define open-accessor button-value;
define open-accessor children-value;
define open-accessor colormap-value;
define open-accessor configure-event-value;
define open-accessor container-value;
define open-accessor cursor-value;
define open-accessor data-value;
define open-accessor depth-value;
define open-accessor destroy-value;
define open-accessor index-value;
define open-accessor fg-gc-value;
define open-accessor fill-value;
define open-accessor flags-value;
define open-accessor focus-value;
define open-accessor foreground-value;
define open-accessor font-value;
define open-accessor func-value;
define open-accessor height-value;
define open-accessor key-value;
define open-accessor length-value;
define open-accessor max-width-value;
define open-accessor min-width-value;
define open-accessor motion-value;
define open-accessor name-value;
define open-accessor parent-value;
define open-accessor parent-class-value;
define open-accessor position-value;
define open-accessor property-value;
define open-accessor ref-count-value;
define open-accessor selection-value;
define open-accessor seq-id-value;
define open-accessor spacing-value;
define open-accessor state-value;
define open-accessor style-value;
define open-accessor target-value;
define open-accessor text-value;
define open-accessor text-end-value;
define open-accessor title-value;
define open-accessor type-value;
define open-accessor user-data-value;
define open-accessor value-value;
define open-accessor widget-value;
define open-accessor width-value;
define open-accessor window-value;
define open-accessor wmclass-class-value;
define open-accessor wmclass-name-value;
define open-accessor x-value;
define open-accessor y-value;
