Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Lisp interaction support

define method lisp-false? (value)
  instance?(value, <list>) & empty?(value)
end method lisp-false?;

define method lisp-true? (value)
  ~lisp-false?(value)
end method lisp-true?;
