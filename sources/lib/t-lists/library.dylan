Module: dylan-user
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library t-lists
  use common-dylan;
  use io;
  export t-lists;
end library;

// Interface module
define module t-lists
  create 
    <t-list>;
  use common-dylan,
    export: 
    {find-method, forward-iteration-protocol, as, element, element-setter, 
     push, pop, push-last, pop-last, empty?, size, last, last-setter, 
     concatenate!};     
end module;

// Main implementation module
define module t-lists-internal
  use common-dylan, exclude: {close, format-to-string};
  use streams;
  use print;
  use format;
  use format-out;	//  for debugging	
  use t-lists, export: all; // re-export stuff from 't-lists' module
end module;
