module:    harp-registers
Synopsis:  Class definitions for the HARP spill types.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Spill structure definitions

define abstract primary open class <spill> (<object>)
  slot spill-offset :: <integer>, init-keyword: offset:;
end;

define abstract primary open class <ispill> (<spill>)
end;

define primary open class <gspill> (<ispill>)
end;

define primary open class <nspill> (<ispill>)
end;

define abstract primary open class <fspill> (<spill>)
end;

define primary open class <sfspill> (<fspill>)
end;

define primary open class <dfspill> (<fspill>)
end;
