Module:       WinSock2
Synopsis:     Hand-generated definitions to supplement the automatic translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline constant <C-SOCKET> = <C-raw-unsigned-long>; // in C
define inline constant <SOCKET> = <machine-word>; // Dylan representation

define C-pointer-type <C-char**> => <C-char*>;

// From "winnt.h":
define constant $MAXIMUM-WAIT-OBJECTS = 64;

// From "winbase.h":
define constant $WAIT-OBJECT-0 = 0;

// hand-translated macros:

define function FD-CLR(fd :: <SOCKET>, set :: <LPfd-set>) => (); 
  let i :: <integer> = 0;
  block (quit)
    while ( i < set.fd-count-value )
      if ( fd-array-array(set,i) = fd ) 
	while( i < (set.fd-count-value - 1) )
	  fd-array-array(set,i) := fd-array-array(set,i + 1); 
	  i := i + 1;	  
	end while; 	  
	let new-count :: <integer> = set.fd-count-value - 1;
	set.fd-count-value := new-count;
	quit(); 
      end if; 
    end while; 
  end block;
end FD-CLR;

define function FD-SET(fd :: <SOCKET>, set :: <LPfd-set>) => (); 
  let i :: <integer> = 0;
  block (quit)
    while ( i < set.fd-count-value ) 
      if ( fd-array-array(set,i) = fd )
	quit(); 
      end if; 
    end while; 
  end block;
  if ( i = set.fd-count-value )  
    if ( set.fd-count-value < $FD-SETSIZE )  
      fd-array-array(set,i) := fd; 
      let new-count :: <integer> = set.fd-count-value + 1;
      set.fd-count-value := new-count;
    end if; 
  end if; 
end FD-SET;

define inline-only function FD-ZERO (set :: <LPfd-set>) => ();
  set.fd-count-value := 0;
end FD-ZERO;

define inline-only function timerisset(tvp :: <LPTIMEVAL>)
 => set? :: <boolean>; 
  (~ zero?(tvp.tv-sec-value)) | (~ zero?(tvp.tv-usec-value))
end timerisset;

define inline-only function timercmp(tvp :: <LPTIMEVAL>, uvp :: <LPTIMEVAL>,
				     cmp :: <function>) => value :: <boolean>; 
  cmp(tvp.tv-sec-value, uvp.tv-sec-value) |
    (tvp.tv-sec-value = uvp.tv-sec-value &
       cmp(tvp.tv-usec-value, uvp.tv-usec-value))
end timercmp;



// utility functions for convenience

define C-pointer-type <in-addr*> => <in-addr>;
define C-pointer-type <in-addr**> => <in-addr*>;

define simple-C-mapped-subtype <C-buffer-offset> (<C-char*>)
  export-map <machine-word>, export-function: identity;
end;

define inline-only C-function win32-recv-buffer
  parameter s          :: <C-SOCKET>;
  parameter buf        :: <C-buffer-offset>;
  parameter len        :: <C-int>;
  parameter flags      :: <C-int>;
  result value :: <C-int>;
  c-name: "recv", c-modifiers: "__stdcall";
end;

define inline-only C-function win32-send-buffer
  parameter s          :: <C-SOCKET>;
  parameter buf        :: <C-buffer-offset>;
  parameter len        :: <C-int>;
  parameter flags      :: <C-int>;
  result value :: <C-int>;
  c-name: "send", c-modifiers: "__stdcall";
end;

define inline-only C-function win32-recv-buffer-from
  parameter s          :: <C-SOCKET>;
  parameter buf        :: <C-buffer-offset>;
  parameter len        :: <C-int>;
  parameter flags      :: <C-int>;
  parameter from       :: <LPSOCKADDR>;
  parameter fromlen    :: <C-int*>;
  result value :: <C-int>;
  c-name: "recvfrom", c-modifiers: "__stdcall";
end;

define inline-only C-function win32-send-buffer-to
  parameter s          :: <C-SOCKET>;
  parameter buf        :: <C-buffer-offset>;
  parameter len        :: <C-int>;
  parameter flags      :: <C-int>;
  parameter to         ::  /* const */ <LPSOCKADDR>;
  parameter tolen      :: <C-int>;
  result value :: <C-int>;
  c-name: "sendto", c-modifiers: "__stdcall";
end;

// new things for mswsock.h

define C-pointer-type <sockaddr**> => <LPSOCKADDR>;
