Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Brushes

define protocol <<brush-protocol>> ()
  getter brush-foreground
    (brush :: <brush>) => (foreground :: <ink>);
  getter brush-background
    (brush :: <brush>) => (background :: <ink>);
  getter brush-mode
    (brush :: <brush>) => (mode :: <integer>);
  getter brush-fill-style
    (brush :: <brush>) => (fill-style);
  getter brush-fill-rule
    (brush :: <brush>) => (fill-rule);
  getter brush-tile
    (brush :: <brush>) => (image);
  getter brush-stipple
    (brush :: <brush>) => (stipple);
  getter brush-ts-x
    (brush :: <brush>) => (x :: false-or(<integer>));
  getter brush-ts-y
    (brush :: <brush>) => (y :: false-or(<integer>));
  getter brush-stretch-mode
    (brush :: <brush>) => (stretch-mode);
end protocol <<brush-protocol>>;


// Drawing "functions"...
define constant $boole-clr   :: <integer> = 0;
define constant $boole-set   :: <integer> = 1;
define constant $boole-1     :: <integer> = 2;
define constant $boole-2     :: <integer> = 3;
define constant $boole-c1    :: <integer> = 4;
define constant $boole-c2    :: <integer> = 5;
define constant $boole-and   :: <integer> = 6;
define constant $boole-ior   :: <integer> = 7;
define constant $boole-xor   :: <integer> = 8;
define constant $boole-eqv   :: <integer> = 9;
define constant $boole-nand  :: <integer> = 10;
define constant $boole-nor   :: <integer> = 11;
define constant $boole-andc1 :: <integer> = 12;
define constant $boole-andc2 :: <integer> = 13;
define constant $boole-orc1  :: <integer> = 14;
define constant $boole-orc2  :: <integer> = 15;


//--- What about other Windows arguments?
//--- What about X plane-mask, arc-mode, clip-x/y, clip-ordering?
define sealed class <standard-brush> (<brush>)
  sealed constant slot brush-foreground :: <ink> = $foreground,
    init-keyword: foreground:;
  sealed constant slot brush-background :: <ink> = $background,
    init-keyword: background:;
  sealed constant slot brush-mode :: <integer> = $boole-1,
    init-keyword: mode:;
  sealed constant slot brush-fill-style = #f,
    init-keyword: fill-style:;
  sealed constant slot brush-fill-rule = #f,
    init-keyword: fill-rule:;
  sealed constant slot brush-tile = #f,
    init-keyword: tile:;
  sealed constant slot brush-stipple = #f,
    init-keyword: stipple:;
  sealed constant slot brush-ts-x :: false-or(<integer>) = #f,
    init-keyword: ts-x:;
  sealed constant slot brush-ts-y :: false-or(<integer>) = #f,
    init-keyword: ts-y:;
  sealed constant slot brush-stretch-mode = #f,
    init-keyword: stretch-mode:;
end class <standard-brush>;

define sealed domain make (singleton(<standard-brush>));
define sealed domain initialize (<standard-brush>);

define sealed method \=
    (brush1 :: <standard-brush>, brush2 :: <standard-brush>) => (true? :: <boolean>)
  brush1 == brush2
  | begin
      brush-foreground(brush1) = brush-foreground(brush2)
      & brush-background(brush1) = brush-background(brush2)
      & brush-mode(brush1) == brush-mode(brush2)
      & brush-fill-style(brush1) == brush-fill-style(brush2)
      & brush-fill-rule(brush1) == brush-fill-rule(brush2)
      & brush-tile(brush1) = brush-tile(brush2)
      & brush-stipple(brush1) = brush-stipple(brush2)
      & brush-ts-x(brush1) == brush-ts-x(brush2)
      & brush-ts-y(brush1) == brush-ts-y(brush2)
      & brush-stretch-mode(brush1) == brush-stretch-mode(brush2)
    end
end method \=;

define sealed inline method make
    (class == <brush>,
     #key foreground = $foreground, background = $background,
          mode = $boole-1, fill-style, fill-rule, tile, stipple, ts-x, ts-y)
 => (brush :: <standard-brush>)
  make(<standard-brush>,
       foreground: foreground, background: background, mode: mode,
       fill-style: fill-style, fill-rule: fill-rule,
       tile: tile, stipple: stipple, ts-x: ts-x, ts-y: ts-y)
end method make;


// This one is pretty useful...
define constant $xor-brush :: <standard-brush>
    = make(<standard-brush>, mode: $boole-xor);

