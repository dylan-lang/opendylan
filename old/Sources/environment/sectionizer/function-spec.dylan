Module:    sectionizer
Synopsis:  Dylan sectionizer
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dylan section -> function spec

define method section-name
    (section :: <section>) => (name, define-word, adjectives)
  let defining-line = section-defining-line(section);
  //---*** How should we do this?
end method section-name;
