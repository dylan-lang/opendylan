Module:       Win32-shell
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//define constant <C-HRESULT> = <C-raw-signed-long>;

define C-pointer-type <LPHICON> => <HICON>;

define constant <HKEY> = <HANDLE>;


define sealed inline-only method hIcon-value ( this :: <SHELLEXECUTEINFOA> )
  => ( value :: <HANDLE> )
  this.u-value.hIcon-value
end;

define sealed inline-only method hIcon-value-setter
    ( value :: <HANDLE>, this :: <SHELLEXECUTEINFOA> ) => ( value :: <HANDLE> )
  this.u-value.hIcon-value := value
end;

define sealed inline-only method hMonitor-value ( this :: <SHELLEXECUTEINFOA> )
  => ( value :: <HANDLE> )
  this.u-value.hMonitor-value
end;

define sealed inline-only method hMonitor-value-setter
    ( value :: <HANDLE>, this :: <SHELLEXECUTEINFOA> ) => ( value :: <HANDLE> )
  this.u-value.hMonitor-value := value
end;
