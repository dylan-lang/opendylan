module:    native-main-harp
Synopsis:  <harp-native-back-end> indexes into the Thread Environment Block
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Indexes into the TEB


define method teb-dynamic-environment-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 0
end method;


define method teb-thread-local-variables-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 4
end method;


define method teb-current-thread-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 8
end method;


define method teb-current-thread-handle-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 12
end method;


define method teb-current-handler-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 16
end method;


define method teb-runtime-state-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 20
end method;



// Reserve a few words in the middle


define method teb-mv-count-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 32
end method;


define open generic teb-mv-area-offset
      (be :: <harp-native-back-end>) => (i :: <integer>);

define method teb-mv-area-offset
      (be :: <harp-native-back-end>) => (i :: <integer>)
 36
end method;


define open generic teb-mv-area-size
      (be :: <harp-native-back-end>) => (i :: <integer>);

define method teb-mv-area-size
      (be :: <harp-native-back-end>) => (i :: <integer>)
 64
end method;


define open generic teb-total-size
      (be :: <harp-native-back-end>) => (i :: <integer>);

define method teb-total-size
      (be :: <harp-native-back-end>) => (i :: <integer>)
 be.teb-mv-area-offset + (4 * be.teb-mv-area-size)
end method;



    
    
    
    
    
    
