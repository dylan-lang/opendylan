Module:    duim-gifs
Synopsis:  GIF images for DUIM
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Read GIF image

define method read-gif-image (string :: <string>) => (image :: <gif-image>)
  with-open-file (stream = string)
    read-gif-image(stream)
  end
end method read-gif-image;

define method read-gif-image (stream :: <stream>) => (image :: <gif-image>)
  let (width, height, version, color-table, descriptor)
    = read-gif-header(stream);
  let image = make(<gif-image>,
                   version: version,
                   width: width, height: height,
                   color-table: color-table,
                   descriptor: descriptor);
  block (return)
    while (#t)
      let separator = read-byte(stream);
      format-out("\n Parsing section #x%x\n", separator);
      select (separator)
	#x3B      => return(image);
	otherwise => read-gif-image-section(image, separator, stream)
      end
    end
  end
end method read-gif-image;



/// Gif header parsing

define method read-gif-header 
    (stream :: <stream>) 
 => (width :: <width>, height :: <height>, version :: <string>,
     color-table :: <gif-color-table>, descriptor :: <gif-image-descriptor>)
  let header = read(stream, 3);
  assert(header = "GIF",
         "Attempting to read non-GIF image in READ-GIF-IMAGE");
  let version = read(stream, 3);
  let width = read-word(stream);
  let height = read-word(stream);
  let color-table-info = read-byte(stream);
  let background = read-byte(stream);
  let aspect-ratio = read-byte(stream);
  let (color-table-size, color-table-sorted?, resolution)
    = decode-color-table-information(color-table-info);
  let descriptor
    = make(<gif-image-descriptor>,
           color-resolution: resolution,
           background: background,
           aspect-ratio: aspect-ratio);
  let color-table
    = if (color-table-size > 0)
        read-gif-color-table(stream, color-table-size, color-table-sorted?)
      end;
  values(width, height, version, color-table, descriptor)
end method read-gif-header;
  
define method read-word (stream :: <stream>) => (word :: <integer>)
  read-byte(stream) + read-byte(stream) * 256
end method read-word;

define method read-byte (stream :: <stream>) => (word :: <integer>)
  let byte = read-element(stream);
  as(<integer>, byte)
end method read-byte;

define method decode-color-table-information
    (byte :: <integer>)
 => (color-table-size :: <integer>, color-table-sorted? :: <boolean>,
     color-resolution :: <integer>)
  let global-color-table? = logand(byte, #b10000000) ~= 0;
  values(if (global-color-table?)
           decode-color-table-size(logand(byte, #b00000111))
         else
           0
         end,
         logand(byte, #b00001000) ~= 0,
         ash(logand(byte, #b01110000), -4))
end method decode-color-table-information;


/// Color table support

define method decode-color-table-size (size :: <integer>)
  ash(1, size + 1)
end method decode-color-table-size;

define method read-gif-color-table 
    (stream :: <stream>, size :: <integer>, sorted? :: <boolean>)
 => (table :: <gif-color-table>)
  let table = make(<vector>, size: size);
  for (i from 0 below size)
    let red   = read-byte(stream);
    let green = read-byte(stream);
    let blue  = read-byte(stream);
    table[i] := make-rgb-color(red, green, blue);
  end;
  make(<gif-color-table>,
       colors: table,
       sorted?: sorted?)
end method read-gif-color-table;


/// Local image parser

define method read-gif-image-section
    (image :: <gif-image>, section, stream :: <stream>)
  error("Unrecognized GIF section separator #x%x", section)
end method read-gif-image-section;

define method read-gif-image-section
    (image :: <gif-image>, section == #x2c, stream :: <stream>)
  let left = read-word(stream);
  let top = read-word(stream);
  let width = read-word(stream);
  let height = read-word(stream);
  let color-table-info = read-byte(stream);
  format-out("\n Image %dx%d at %d,%d info=%d\n", 
             width, height, left, top, color-table-info);
  let (color-table-size, interlaced?, sorted?)
    = decode-local-color-table-information(color-table-info);
  let color-table
    = if (color-table-size > 0)
        read-gif-color-table(stream, color-table-size, sorted?)
      end;
  read-image-data(image, color-table, interlaced?, stream);
end method read-gif-image-section;

define method decode-local-color-table-information
    (byte :: <integer>)
 => (color-table-size :: <integer>, interlaced? :: <boolean>,
     sorted? :: <boolean>)
  let has-color-table? = logand(byte, #b00000001) ~= 0;
  values(if (has-color-table?)
           decode-color-table-size(logand(byte, #b11100000))
         else
           0
	 end,
         logand(byte, #b00000010) ~= 0,
         logand(byte, #b00000100) ~= 0)
end method decode-local-color-table-information;

define method read-image-data
    (image :: <gif-image>, color-table,
     interlaced? :: <boolean>, stream :: <stream>)
 => (image :: <gif-image>)
  let image-data = make(<vector>, size: 256);
  let block-size = 0;
  while (begin
           block-size := read-byte(stream);
           block-size ~= 0
         end)
    format-out("\nRead block...");
    // read-into!(stream, block-size, image-data)
    read(stream, block-size)
  end;
  image
end method read-image-data;


// Graphics Control Extension Block handling

define class <gif-graphics-control> (<object>)
end class <gif-graphics-control>;

define method read-gif-image-section
    (image :: <gif-image>, section == #x21, stream :: <stream>)
  let label = read-byte(stream);
  read-gif-extension(image, label, stream)
end method read-gif-image-section;

define method read-gif-extension
    (image :: <gif-image>, section, stream :: <stream>)
  error("Unrecognized GIF extension label #x%x", section)
end method read-gif-extension;

define method read-gif-extension
    (image :: <gif-image>, section == #xf9, stream :: <stream>)
  let block-size = read-byte(stream);
  let packed = read-byte(stream);
  let delay-time = read-word(stream);
  let color-index = read-byte(stream);
  let terminator = read-byte(stream);
  format-out("\n  Control block: %x %d %d %d %x\n",
             block-size, packed, delay-time, color-index, terminator);
  assert(block-size = #x04 & terminator = #x00,
         "Invalid GIF graphics control block");
  format-out("\nCurrently ignoring graphics control block");
end method read-gif-extension;

define method read-gif-extension
    (image :: <gif-image>, section == #xff, stream :: <stream>)
  let block-size = read-byte(stream);
  let identifier = read(stream, 8);
  let authentication-code = read(stream, 3);
  let application-data = read(stream, 2);
  let terminator = read-byte(stream);
  format-out("\n  Application block: %x %s %d %s %x\n",
             block-size, identifier, authentication-code,
             application-data, terminator);
  assert(block-size = #x0b & terminator = #x00,
         "Invalid GIF application block");
  format-out("\nCurrently ignoring application extension block");
end method read-gif-extension;
