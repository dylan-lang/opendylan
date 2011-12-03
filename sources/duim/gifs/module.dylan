Module:    Dylan-User
Synopsis:  GIF images for DUIM
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-gifs
  use common-dylan;
  use simple-io;
  use streams;
  use duim;

  export <gif-image>,
         read-gif-image;
end module duim-gifs;
