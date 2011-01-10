Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Fundamental mode

define open class <fundamental-mode> (<major-mode>)
end class <fundamental-mode>;

begin
  gethash(*keyword->major-mode*,   #"fundamental") := <fundamental-mode>
end;

define method mode-name
    (mode :: <fundamental-mode>) => (name :: <byte-string>)
  "Fundamental"
end method mode-name;


/// Text mode

define open class <text-mode> (<fundamental-mode>)
end class <text-mode>;

begin
  gethash(*keyword->major-mode*,   #"text") := <text-mode>;
  gethash(*file-type->major-mode*, #"text") := <text-mode>
end;

define method mode-name
    (mode :: <text-mode>) => (name :: <byte-string>)
  "Text"
end method mode-name;

define method source-file-type
    (mode :: <text-mode>) => (file-type)
  #"text"
end method source-file-type;
