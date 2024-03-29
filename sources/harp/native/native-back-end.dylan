module:    native-back-end
Synopsis:  The definition of the <harp-native-back-end> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// The abstract class from which all native HARP backends are derived.
// Native backends have stack operations (unlike the IDVM).

define abstract open primary class <harp-native-back-end> (<harp-back-end>)
end;


define abstract open class <harp-native-windows-back-end> (<harp-native-back-end>)
end;

define abstract open class <harp-native-unix-back-end> (<harp-native-back-end>)
end;

// All Linux back-ends should multiply inherit this class.

define abstract open class <harp-native-linux-back-end> (<harp-native-unix-back-end>)
end;

define abstract open class <harp-native-freebsd-back-end> (<harp-native-unix-back-end>)
end;

define abstract open class <harp-native-darwin-back-end> (<harp-native-unix-back-end>)
end;
