Module:   flat-sequence
Author:   Eliot Miranda
Synopsis: An abstract class for order(1) access sequences
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <flat-sequence> (<sequence>) end class;
define class <mutable-flat-sequence> (<flat-sequence>,<mutable-sequence>) end class;


define constant $flat-sequence-next-state
  = method (fs :: <flat-sequence>, n :: <integer>) => n :: <integer>;
       n + 1
    end;

define constant $flat-sequence-previous-state
  = method (fs :: <flat-sequence>, n :: <integer>) => n :: <integer>;
       n - 1
    end;

define constant $flat-sequence-forwards-finished-state?
  = method (fs :: <flat-sequence>, n :: <integer>, limit :: <integer>)
      => finished? :: <boolean>;
      n >= limit
    end;

define constant $flat-sequence-backwards-finished-state?
  = method (fs :: <flat-sequence>, n :: <integer>, ignored-limit :: <integer>)
      => finished? :: <boolean>;
      n < 0
    end;

define constant $flat-sequence-current-key
  = method (fs :: <flat-sequence>, n :: <integer>) => n :: <integer>;
       n
    end;

define method size (fs :: <flat-sequence>) => n :: <integer>;
  error("The method size is a subclass responsibility of <flat-sequence>.");
  0
end method;

define method forward-iteration-protocol (flat-sequence :: <flat-sequence>)
              => (initial-state          :: <integer>,
                  limit                  :: <integer>,
                  next-state             :: <function>,
                  finished-state?        :: <function>,
                  current-key            :: <function>,
                  current-element        :: <function>,
                  current-element-setter :: <function>,
                  copy-state             :: <function>);

  values(0,
         flat-sequence.size,
         $flat-sequence-next-state,
         $flat-sequence-forwards-finished-state?,
         $flat-sequence-current-key,
         element,
         element-setter,
         $flat-sequence-current-key)
end method;


define method backward-iteration-protocol (flat-sequence :: <flat-sequence>)
              => (final-state            :: <integer>,
                  limit                  :: <integer>,
                  previous-state         :: <function>,
                  finished-state?        :: <function>,
                  current-key            :: <function>,
                  current-element        :: <function>,
                  current-element-setter :: <function>,
                  copy-state             :: <function>);

  values(flat-sequence.size,
         0,
         $flat-sequence-previous-state,
         $flat-sequence-backwards-finished-state?,
         $flat-sequence-current-key,
         element,
         element-setter,
         $flat-sequence-current-key)
end method;
