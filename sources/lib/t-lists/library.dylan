Module: dylan-user
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library t-lists
  use functional-dylan;
  use io;
  export t-lists;
end library;

// Interface module
define module t-lists
  create 
    <t-list>;
  use functional-dylan,
    export: 
    {find-method, forward-iteration-protocol, as, element, element-setter, 
     push, pop, push-last, pop-last, empty?, size, last, last-setter, 
     concatenate!};     
end module;

// Main implementation module
define module t-lists-internal
  use functional-dylan, exclude: {close};
  use streams;
  use print;
  use format;
  use format-out;	//  for debugging	
  use t-lists, export: all; // re-export stuff from 't-lists' module
end module;
