Module:    carbon-interface
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
