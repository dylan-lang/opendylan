Module:    duim-gifs
Synopsis:  GIF images for DUIM
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// LZW decompression

define method remove-all-keys! (collection :: <table>)
  for (key in key-sequence(collection))
    remove-key!(collection, key)
  end
end method remove-all-keys!;

define class <lzw-dictionary> (<object>)
  slot dictionary-code-values = make(<vector>, size: 4096);
  slot dictionary-table = make(<hash-table>);
  slot dictionary-initial-size = 256,
    init-keyword: size:;
  slot dictionary-size = 256,
    init-keyword: size:;
end class <lzw-dictionary>;

define method initialize-dictionary (dictionary :: <lzw-dictionary>)
  let size = dictionary-initial-size(dictionary);
  let table = dictionary-table(dictionary);
  let code-values = dictionary-code-values(dictionary);
  dictionary-size(dictionary) := size;
  remove-all-keys(dictionary-table(dictionary));
  for (i from 0 below size)
    table[i] := i;
    code-values[i] := i
  end
end method initialize-dictionary;

define method decompress-lzw
    (input :: <sequence>, #key initial-database, buffer-size = 256)
 => (output :: <sequence>)
  let buffer = make(<vector>, size: buffer-size);
  decompress-lzw-into!(input, buffer, initial-database: initial-database)
end method decompress-lzw;

define method decompress-lzw-into!
    (input :: <sequence>, output :: <sequence>, #key initial-database)
 => (data :: <sequence>)
end method decompress-lzw-into!;
