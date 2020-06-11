Module: io-test-suite
Synopsis: streams benchmarks

// These first two benchmarks compare writing to a string stream and building a
// string with a simple <string-builder> class, modeled after the one in Go.
// As of Open Dylan 2020.1pre, the string builder is 1.5x faster with 10k
// iterations. The string builder version allocates significantly less.

// Use the same number of iterations for both benchmarks.
define constant $builder-iterations = 1000;

define benchmark benchmark-output-to-string ()
  benchmark-repeat (iterations: $builder-iterations)
    let s = "The quick brown fox.";
    let buf :: <string-stream> = make(<string-stream>, direction: #"output");
    let s2 :: <string> = "";
    for (j from 1 to 1000)
      write(buf, s);
      write(buf, "\n");
    end;
    s2 := stream-contents(buf, clear-contents?: #t);
    // Open Dylan doesn't seem to need the following (i.e., it doesn't optimize
    // out the unused s2) but I leave this here to be similar to the Go version,
    // which *does* optimize away s2 (and gives a compiler error).
    if (s2[0] == 'a')           // never true
      s := s2;
    end;
  end;
end benchmark;

// A minimal, unoptimized string builder implementation to compare with string
// output streams.

define constant $builder-initial-size :: <integer> = 127;
define constant $builder-growth-factor :: <double-float> = 2.0d0;

define sealed class <string-builder> (<object>)
  slot builder-buffer :: <string> = make(<string>, size: $builder-initial-size),
    init-keyword: buffer:;
  slot builder-index :: <integer> = 0;
end;

define method builder-write (b :: <string-builder>, s :: <string>)
  let idx = b.builder-index;
  let s-size = s.size;
  let new-size = idx + s-size;
  if (new-size >= b.builder-buffer.size)
    builder-grow(b, new-size);
  end;
  copy-bytes(b.builder-buffer, idx, s, 0, s-size);
  b.builder-index := new-size;
end;

// Grow the buffer, making sure it's not smaller than min-size.
define method builder-grow (b :: <string-builder>, min-size :: <integer>) => ()
  let buffer1 = b.builder-buffer;
  let size2 :: <double-float> = buffer1.size * $builder-growth-factor;
  while (size2 < min-size)
    size2 := size2 * $builder-growth-factor;
  end;
  let buffer2 :: <string> = make(<string>, size: truncate(size2));
  copy-bytes(buffer2, 0, buffer1, 0, b.builder-index);
  b.builder-buffer := buffer2;
end;

define method builder-string (b :: <string-builder>) => (s :: <string>)
  let s = copy-sequence(b.builder-buffer, end: b.builder-index);
  b.builder-index := 0;
  s
end;

define benchmark benchmark-string-builder ()
  benchmark-repeat (iterations: $builder-iterations)
    let s = "The quick brown fox.";
    let builder :: <string-builder> = make(<string-builder>);
    for (j from 1 to 1000)
      builder-write(builder, s);
      builder-write(builder, "\n");
    end;
    let s2 = builder-string(builder);
    // Open Dylan doesn't seem to need the following (i.e., it doesn't optimize
    // out the unused s2) but I leave this here to be similar to the Go version,
    // which *does* optimize away s2 (and gives a compiler error).
    if (s2[0] == 'a')           // never true
      s := s2;
    end;
  end;
end benchmark;
