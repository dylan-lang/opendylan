module: dood
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <dood-table> = <table>;

define class <dood-warning> (<warning>)
end class;

define macro with-unbound-caught
  { with-unbound-caught ?:body unbound ?unbound:body end }
    => { block () ?body exception (<error>) ?unbound end }
end macro;

define macro without-bounds-checks
  { without-bounds-checks ?:body end }
    => { ?body }
end macro;

