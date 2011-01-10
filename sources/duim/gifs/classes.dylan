Module:    duim-gifs
Synopsis:  GIF images for DUIM
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// GIF image classes

define class <gif-image-descriptor> (<object>)
  slot gif-image-color-resolution :: <integer> = 0,
    init-keyword: color-resolution:;
  slot gif-image-background :: <integer> = 0, 
    init-keyword: background:;
  slot gif-image-aspect-ratio :: <integer> = 0,
    init-keyword: aspect-ratio:;
end class <gif-image-descriptor>;

define class <gif-color-table> (<object>)
  slot gif-color-table-sorted? :: <boolean> = #f,
    init-keyword: sorted?:;
  slot gif-color-table-colors :: <vector>,
    required-init-keyword: colors:;
end class <gif-color-table>;
  
define class <gif-image> (<image>)
  slot image-width,
    required-init-keyword: width:;
  slot image-height,
    required-init-keyword: height:;
  slot gif-image-version = "87a",
    init-keyword: version:;
  slot gif-image-color-table :: false-or(<gif-color-table>),
    init-keyword: color-table:;
  slot gif-image-descriptor = #f,
    init-keyword: descriptor:;
end class <gif-image>;
