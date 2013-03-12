Module: java-vm-code-generation
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// NOTE Emulator version uses <integer> for 32 bit ints - non-emulator
/// version uses <machine-word>

/* .zip/.jar file writer, takes a sequence of <zip-entry>'s and writes them to a file */

define constant <maschine-word> = <integer> ;
// define constant <maschine-word> = <machine-word> ;

//////// Firstly a wrapper stream to accumulate file contents, offsets and the CRC

define inline function make-chunk () => (chunk :: limited (<vector>, element-type: <byte-character>))
  make (limited (<vector>, element-type: <byte-character>), size: 512)
end;


define class <chunk-spool-stream> (<stream>)
  sealed slot  chunk-list :: <list> = list (make-chunk ()) ;
  sealed slot  chunk-pos  :: <integer> = 0 ;
  sealed slot  chunk-read-pos :: <integer> = 0 ;
  sealed slot  is-closed? :: <boolean> = #f ;
end;

define class <zip-crc-stream> (<stream>)
  sealed slot  the-stream :: <stream>, required-init-keyword: stream: ;
  sealed slot  the-crc    :: <maschine-word> = $crc-notzero ;
  sealed slot  the-offset :: <integer> = 0 ;
  sealed slot  is-closed? :: <boolean> = #f ;
end;

define inline function add-byte-to-chunk-spool-stream (stream :: <chunk-spool-stream>, byte :: <byte-character>) => ()
  if (stream.is-closed?)
    error ("attempt to write to closed zip-crc-stream")
  end;
  let  chunk = head (stream.chunk-list) ;
  let  len :: <integer> = chunk.size ;
  let  pos :: <integer> = stream.chunk-pos ;
  if (pos >= len)
    chunk := make-chunk () ;
    len   := chunk.size ;
    pos   := 0 ;
    stream.chunk-list := pair (chunk, stream.chunk-list) ;
    stream.chunk-pos  := pos ;
  end;
  stream.chunk-pos := pos + 1 ;
  chunk [pos] := byte ;
end;

define inline function add-byte-to-zip-crc-stream (z :: <zip-crc-stream>, byte :: <byte-character>) => ()
  if (z.is-closed?)
    error ("attempt to write to closed zip-crc-stream")
  end;
  write-element (z.the-stream, byte) ;
  z.the-offset := z.the-offset + 1 ;
  z.the-crc := update1-crc32 (z.the-crc, byte) ;
end;

define method initialize (z :: <zip-crc-stream>, #key) => ()
//  z.the-stream-direction := #"output" ;
end;

define method close (zstream :: <zip-crc-stream>, #key) => ()
  close (zstream.the-stream) ;
  zstream.is-closed? := #t ;
end;

define method initialize (stream :: <chunk-spool-stream>, #key) => ()
//  stream.stream-direction := #"output" ;
end;

define method close (stream :: <chunk-spool-stream>, #key) => ()
  stream.is-closed? := #t ;
end;

define method rewind (stream :: <chunk-spool-stream>) => ()
  stream.chunk-list := reverse! (stream.chunk-list) ;
  stream.chunk-read-pos := 0 ;
end;


define method available? (stream :: <chunk-spool-stream>) => (available? :: <boolean>)
  let list = stream.chunk-list ;
  block (return)
    unless (instance? (list, <pair>)) 
      return (#f)
    end;
    if (stream.chunk-read-pos < list.head.size) 
      return (#t) 
    end;      
    return (instance? (list.tail, <pair>)) ;
  end
end;

define method available? (stream :: <zip-crc-stream>) => (available? :: <boolean>)
  stream.the-stream.available?
end;

define method read-element (stream :: <chunk-spool-stream>, #key) => (res :: <byte-character>)
  let  list = stream.chunk-list ;
  let chunk = head (list) ;
  let pos = stream.chunk-read-pos ;
  if (pos >= chunk.size)
    list := tail (list) ;
    stream.chunk-list := list ;
    if (list == #())
      error ("end of file")
    end;
    chunk := head (list) ;
    pos   := 0 ;
  end;
  stream.chunk-read-pos := pos + 1 ;
  chunk [pos]
end;

//define method current-position-setter (pos :: <integer>, zstream :: <zip-crc-stream>) => (pos :: <integer>)
//  zstream.the-stream.current-position := pos ;
//end;

define sealed method get-crc (z :: <zip-crc-stream>) => (crc :: <maschine-word>)
  lognot (z.the-crc)
end;

define sealed method get-offset (z :: <zip-crc-stream>) => (offs :: <integer>)
  z.the-offset
end;

define method write (z :: <chunk-spool-stream>, str :: <byte-string>, #key start = #f, end: end-index = #f) => ()
  // this isn't general enough for the stop/start keywords, but this code doesn't use them
  for (ch :: <byte-character> in str)
    add-byte-to-chunk-spool-stream (z, ch)
  end
end;

define method write (z :: <zip-crc-stream>, str :: <byte-string>, #key start = #f, end: end-index = #f) => ()
  // this isn't general enough for the stop/start keywords, but this code doesn't use them
  for (ch :: <byte-character> in str)
    add-byte-to-zip-crc-stream (z, ch)
  end
end;

define method read-element (z :: <zip-crc-stream>, #key) => (char :: <byte-character>)
  read-element (z.the-stream)
end;

define method write-element (z :: <chunk-spool-stream>, char :: <byte-character>) => ()
  add-byte-to-chunk-spool-stream (z, char)
end;

define method write-element (z :: <zip-crc-stream>, char :: <byte-character>) => ()
  add-byte-to-zip-crc-stream (z, char)
end;

// for elements of byte vectors
define method write-element (z :: <chunk-spool-stream>, char :: <integer>) => ()
format-out ("add element to <chunk-spool-stream>, at %d\n", z.chunk-pos) ;
  add-byte-to-chunk-spool-stream (z, as (<byte-character>, char)) ;
end;

define method write-element (z :: <zip-crc-stream>, char :: <integer>) => ()
  add-byte-to-zip-crc-stream (z, as (<byte-character>, char)) ;
end;

define method reset-crc (z :: <zip-crc-stream>) => ()
  z.the-crc := $crc-notzero
end;


///////////////


define abstract class <zip-entry> (<object>)
  slot filename :: <string>, required-init-keyword: filename: ;
  slot filetime :: <maschine-word> = as (<maschine-word>, 0), init-keyword: filetime: ;
  virtual slot filesize :: <integer> ;
  virtual slot file-crc :: <maschine-word> ;
  virtual /* read-only */ slot zip-details-upfront? :: <boolean> ;
end;

define method print-object (ze :: <zip-entry>, str :: <stream>) => ()
  format (str, "{%s %s}", ze.object-class, ze.filename)
end;


define class <zip-dir-entry> (<zip-entry>)
end;

define generic directory? (ze :: <zip-entry>) => (directory? :: <boolean>) ;
define method directory? (ze :: <zip-entry>) => (directory? :: <boolean>)
  #f
end;

define method directory? (ze :: <zip-dir-entry>) => (directory? :: <boolean>)
  #t
end;

define method zip-details-upfront? (e :: <zip-dir-entry>) => (upfront? :: <boolean>)
  #t
end;

define method filesize (e :: <zip-dir-entry>) => (len :: <integer>)
  0
end;

define method file-crc (e :: <zip-dir-entry>) => (crc :: <maschine-word>)
  0
end;

define method writer (e :: <zip-dir-entry>) => (f :: <function>)
  method (s :: <stream>) => () end
end;




/* test class for using a string as an entry */

define class <zip-string-entry> (<zip-entry>)
  inherited slot filename = "aaa.java" ;
  inherited slot filetime = as (<maschine-word>, #x00ff1234) ;
  slot str :: <string>, required-init-keyword: str: ;
end;

define method zip-details-upfront? (e :: <zip-string-entry>) => (upfront? :: <boolean>)
  #t
end;

define method filesize (e :: <zip-string-entry>) => (len :: <integer>)
  e.str.size 
end;

define method file-crc (e :: <zip-string-entry>) => (crc :: <maschine-word>)
  compute-crc32 (e.str)
end;


define method writer (e :: <zip-string-entry>) => (f :: <function>)
  method (s :: <stream>) => ()
    for (ch in e.str)
      write-element (s, ch) ;
    end
  end
end;

/* end of test class */


// construct a DOS style timestamp value (32 bits - have to convert to machine words)
define function zip-timestamp (year ::  <integer>, 
			       month :: <integer>, 
			       date ::  <integer>,
			       hours :: <integer>,
			       mins ::  <integer>,
			       secs ::  <integer>) => (zip-date :: <maschine-word>)
  let  top-half = 
    logior (ash (year - 1980, 9),
	    logior (ash (month + 1, 5),
		    date)) ;
  let  bot-half = 
    logior (ash (hours, 11),
	    logior (ash (mins, 5), 
		    ash (secs, -1))) ;
  logior (ash (top-half, 16), bot-half) // for EMULATOR
end;

define function as-zip-date (d :: <date>) => (zip-date :: <maschine-word>)
  zip-timestamp (d.date-year,
		 d.date-month - 1,
		 d.date-day,
		 d.date-hours,
		 d.date-minutes,
		 d.date-seconds)
end;



// top level interface - take stem of jar file name, and a sequence of zip-entries
// and write the file, return total size in bytes
define function write-zip-file (file-name, entries :: <sequence>) => (size :: <integer>)
  let  local-offsets = make (<simple-object-vector>, size: entries.size) ;
  let  zip-stream = #f ;
  block ()
    let out-stream = open-output-stream (*java-back-end*, concatenate (file-name, ".jar")) ;
    zip-stream := make (<zip-crc-stream>, stream: out-stream) ;

    for (entry in entries, n :: <integer> from 0)
      local-offsets [n] := zip-stream.get-offset ;
      write-zip-loc (zip-stream, entry, entry.writer)
    end;
    let  cen-offset :: <integer> = zip-stream.get-offset ;
    for (entry in entries, n :: <integer> from 0)
      write-zip-cen (zip-stream, entry, local-offsets [n])
    end;
    write-zip-end (zip-stream, entries.size, cen-offset, zip-stream.get-offset - cen-offset, "Zip file") ;
    zip-stream.get-offset
  cleanup
    if (zip-stream)  close (zip-stream)  end
  end
end;


define function write-zip-loc (s      :: <zip-crc-stream>,
			       entry  :: <zip-entry>,
			       writer :: <function>) => ()
  write-zip-entry (s, entry, writer, 0)
end;

define function write-zip-cen (s      :: <zip-crc-stream>,
			       entry  :: <zip-entry>,
			       loc-offset :: <integer>) => ()
  write-zip-entry (s, entry, #f, loc-offset)
end;

define function write-zip-end (s      :: <zip-crc-stream>,
			       count  :: <integer>,
			       cen-start :: <integer>,
			       cen-size :: <integer>,
                               zip-comment :: <byte-string>) => ()
  write-int (s, #x06054b50) ;
  write16 (s, 0) ;  // disk number
  write16 (s, 0) ;  // dir start disk
  write16 (s, count) ;  // entries on disk
  write16 (s, count) ;  // total entries 
  write-int (s, cen-size) ;
  write-int (s, cen-start) ;
  write16 (s, zip-comment.size) ;
  write-ascii (s, zip-comment) ;
end;



// The ZIP file CRC32 algorithm, as documented in RFC 1952
// Again this should be turned into tight machine-word code,
// with appropriate limited (<vector>)

define constant $crc-magic-value :: <maschine-word> = #xedb88320 ;

define constant $crc-table = make (<simple-object-vector>, size: #x100) ;

for (n :: <integer> from 0 below #x100)
  let c :: <maschine-word> = as (<maschine-word>, n) ;
  for (k :: <integer> from 0 below 8)
    if (logand (c, 1) = 1)  // should be machine-word-logand?
      c := logxor ($crc-magic-value, ash (c, -1))
    else
      c := ash (c, -1)
    end
  end;
  $crc-table [n] := c
end;


define constant $crc-notzero :: <maschine-word> = #xffffffff ;

define function compute-crc32 (str :: <byte-string>) => (crc :: <maschine-word>)
  logxor ($crc-notzero, update-crc32 ($crc-notzero, str))
end;


define inline function update1-crc32 (crc :: <maschine-word>, char :: <byte-character>) => (new-crc :: <maschine-word>)
  let  code :: <integer> = logand (#xff, as (<integer>, char)) ;
  let  low :: <integer> = logxor (logand (#xff, crc), code) ;
  logxor ($crc-table [low], ash (crc, -8))
end;

define function update-crc32 (crc :: <maschine-word>, str :: <byte-string>) => (new-crc :: <maschine-word>)
  for (ch :: <byte-character> in str)
    crc := update1-crc32 (crc, ch)
  end;
  crc
end;



  
define function unix-mode (#key user = "rw", group = "rw", other = "r", directory? = #f) => (mode :: <integer>)
  local 
    method unix-mode-part-from-string (rwx-string :: <byte-string>) => (mode :: <integer>)
      let  mode :: <integer> = 0 ;
      let  bit :: <integer> = 1 ;
      for (char in "xwr")
        if (member? (char, rwx-string)) 
          mode := logior (mode, bit) 
        end;
        bit := bit * 2 ;
      end;
      mode
    end;
  logior (if (directory?) #x416D else #x8000 end,
          ash (unix-mode-part-from-string (user), 6),
          ash (unix-mode-part-from-string (group), 3),
          unix-mode-part-from-string (other))
end;



define function write-zip-entry (s      :: <zip-crc-stream>, 
				 entry  :: <zip-entry>,
				 writer :: false-or (<function>),
				 loc-offset :: <integer>) => ()
  let  name = entry.filename ;
  let  extra-string = "" ;   // no extra string
  let  comment = "" ;   // no comment
  let  timestamp :: <maschine-word>   = entry.filetime ;
  let  defer-file-info = ~ entry.zip-details-upfront? ;
  let  filesiz :: <integer> = -1 ;
  let  crc :: <maschine-word> = $crc-notzero ;

  write-int (s, if (writer) 
		#x04034b50  // local header
	      else
		#x02014b50  // central directory
	      end) ;
  unless (writer)
    write16 (s, #x0314) ; // creator version  (Unix, 2.0)
  end;
  write16 (s, #x0014) ;  // extractor version (2.0 +)
  if (writer & defer-file-info)
    write16 (s, #x0008)
  else
    write16 (s, #x0000)  // flags
  end;
  write16 (s, #x0000) ;  // compression method code (NONE)
  write32 (s, timestamp) ;  // DOS format timestamp
  if (writer & defer-file-info)
    write-int (s, 0) ;
    write-int (s, 0) ;
    write-int (s, 0) ;
  else
    filesiz := entry.filesize ;
    crc    := entry.file-crc ;
    write32 (s, crc) ;   // CRC 32
    write-int (s, filesiz) ;    // size in archive
    write-int (s, filesiz) ;    // expanded file size
  end;
  write16 (s, name.size) ;
  write16 (s, extra-string.size) ;      // extra string length
  unless (writer)
    write16 (s, comment.size) ;    
    write16 (s, #x0000) ;    // start disk number
    write16 (s, #x0000) ;    // internal file attrs (unused? - should be #x0001 for text files)
    if (entry.directory?)          // external file attrs  - put dir/mode here
      write16 (s, #x0010) ;
      write16 (s, unix-mode (directory?: #t)) ; // write16 (s, #x41FF) ;
      //write-int (s, #x41FF0010) ;  // mode drwxrwxrwx
    else
      write16 (s, #x0000) ;
      write16 (s, unix-mode (user: "rw", group: "rw", other: "r")) ; // write16 (s, #x81a4) ;  // #x81b6 for rw-rw-rw-
      //write-int (s, #x81a40000) ;  // mode -rw-r--r--
    end;
    write-int (s, loc-offset) ;  // offset of entry local header
  end;
  write-ascii (s, name) ;
  write-ascii (s, extra-string) ;
  unless (writer)
    write-ascii (s, comment) ; // no comment
  end;
  // end of header

  // body of file, if a writer provided
  if (writer)
    if (defer-file-info)
      let  old-off = s.get-offset ;
      reset-crc (s) ;
      writer (s) ;
      crc    := s.get-crc ;
      filesiz :=  s.get-offset - old-off ;
      write-int (s, #x08074b50) ;  // EXTra data desc sig.
      write32 (s, crc) ; 
      write-int (s, filesiz) ;    // size in archive
      write-int (s, filesiz) ;    // expanded file size
      entry.file-crc := crc ;
      entry.filesize := filesiz
    else
      let  should-be = s.get-offset + filesiz ;
      reset-crc (s) ;
      writer (s) ;
      let actual-crc = s.get-crc ;
      if (s.get-offset ~= should-be)
	error ("bad ZIP file offset")
      end;
      if (crc ~= 0 & 
          logand (#xffffffff, logxor (actual-crc, crc)) ~= 0)
	error ("bad ZIP checksum")
      end
    end
  end;
end;


define function write16 (s :: <stream>, short :: <integer>) => ()
  write-element (s, as (<byte-character>, logand (#xff, short))) ;
  write-element (s, as (<byte-character>, logand (#xff, ash (short, -8)))) ;
end;

define function write32 (s :: <stream>, long :: <maschine-word>) => ()
  let  mask :: <maschine-word> = as (<maschine-word>, #xffff) ;
  write16 (s, as (<integer>, logand (mask, long))) ;
  write16 (s, as (<integer>, logand (mask, ash (long, -16)))) ;
end;

define function write-int (s :: <stream>, int :: <integer>) => ()
  write32 (s, as (<maschine-word>, int))
end;

define function write-ascii (s :: <stream>, string :: <byte-string>) => ()
  write (s, string) ;
end;



define class <jar-file-rep> (<object>)
  slot  jar-name :: <byte-string>, required-init-keyword: jar-name: ;
  slot  jar-library              , required-init-keyword: jar-library: ;
  slot  jar-stream :: <stream>   , required-init-keyword: jar-stream: ;
  slot  zip-stream :: <zip-crc-stream> ;
  slot  zip-entries :: <stretchy-vector> = make (<stretchy-vector>) ;
  slot  local-offsets :: <stretchy-vector> = make (<stretchy-vector>) ;
  slot  open? :: <boolean> = #t ;
  slot  jar-comment :: <byte-string> = "", init-keyword: jar-comment: ;
end;

define method initialize (jar :: <jar-file-rep>, #key) => ()
  jar.zip-stream := make (<zip-crc-stream>, stream: jar.jar-stream) ;
end;

define method add-to-jar! (jar :: <jar-file-rep>, zip-ent :: <zip-entry>) => ()
  if (jar.open?)
    jar.zip-entries := add! (jar.zip-entries, zip-ent) ;
    jar.local-offsets := add! (jar.local-offsets, jar.zip-stream.get-offset) ;
    write-zip-loc (jar.zip-stream, zip-ent, zip-ent.writer)
  else
    error ("attempt to write to a closed jar archive")
  end
end;

define method jar-close (jar :: <jar-file-rep>) => (size :: <integer>)
  let  zstream = jar.zip-stream ;
  let  ents = jar.zip-entries ;
  let  cen-offset :: <integer> = zstream.get-offset ;
  for (zip-ent in ents, offs in jar.local-offsets)
    write-zip-cen (zstream, zip-ent, offs)
  end;
  write-zip-end (zstream, ents.size, cen-offset, zstream.get-offset - cen-offset, jar.jar-comment) ;
  zstream.close ;
  jar.open? := #f ;
  zstream.get-offset
end;



define variable *current-jar* :: false-or (<jar-file-rep>) = #f ;

define function current-jar () => (jar :: <jar-file-rep>)
  *current-jar* |
    error ("no current .jar file")
end;

define function current-jar-setter (jar :: <jar-file-rep>) => (jar :: <jar-file-rep>)
  *current-jar* := jar
end;
