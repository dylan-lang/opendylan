Module:    tetris
Synopsis:  DUIM implementation of the game Tetris
Author:    Richard Tucker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $application-name :: <byte-string> = "Tetris";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "0";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;

