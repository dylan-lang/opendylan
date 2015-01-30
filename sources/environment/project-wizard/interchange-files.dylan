Module:    environment-project-wizard
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ----------------------------------------------------------------------
/// PROCESSING INTERCHANGE FORMAT FILES

/// Single header pairs

// Writing

define inline function write-single-value-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <object>)
  format(stream, format-string, key, value);
end function;

define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <object>)
  when (value)
    write-single-value-header-pair(stream, format-string, key, value);
  end;
end method;

define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, values :: <sequence>)
  unless (empty?(values))
    format(stream, format-string, key, values[0]);
    for (index from 1 below size(values))
      format(stream, "\t%s\n", values[index]);
    end;
  end;
end method;

// This method is here to override the one on "values :: <sequence>".
define method write-header-pair
    (stream :: <stream>, format-string :: <string>,
     key :: <string>, value :: <string>)
  write-single-value-header-pair(stream, format-string, key, value);
end method;

define function write-headers
    (stream :: <stream>, headers :: <vector> /* of: <table> */,
     #key header-order :: false-or(<sequence> /* of: <symbol> */))
 => ()
  // Collect the key-value pairs from the header <table>s into one
  // <stretchy-vector>, obeying any supplied header-order.
  // The header-order, when non-#f, is a vector of keys, whose key-value
  // pairs must appear in that order, before any other keys.
  let headers-vector = make(<stretchy-vector>);
  if (header-order & ~empty?(header-order))
    // This sort algorithm keeps a separate "bin" for each key in the
    // header-order, plus one for all other keys, which are to go to
    // the end.  We just iterate over all the key-value pairs copying
    // them into the appropriate bin, and then concatenate all the bins
    // in order.  It's sort[;)] of a "two-space insertion sort".

    // First initialize the "bins" to empty <stretchy-vector>s.
    let order-bins = make(<table>, size: size(header-order) + 1);
    for (key in header-order)
      when (key)
        // #f is invalid, and reserved for the "others" bin below.
        order-bins[key] := make(<stretchy-vector>);
      end;
    end;
    order-bins[#f] := make(<stretchy-vector>);

    // Now copy the key-value pairs into the appropriate bins.
    for (header in headers)
      for (value keyed-by key in header)
        let bin = element(order-bins, key, default: order-bins[#f]);
        add!(bin, pair(key, value));
      end;
    end;

    // Now flatten all the bins into the single headers-vector.
    for (key in header-order)
      when (key)
        // #f is invalid, and reserved for the "others" bin.
        headers-vector := concatenate!(headers-vector, order-bins[key]);
      end;
    end;
    headers-vector := concatenate!(headers-vector, order-bins[#f]);
  else
    for (header in headers)
      for (value keyed-by key in header)
        add!(headers-vector, pair(key, value));
      end;
    end;
  end;

  // We measure the longest key for pretty-printing, and collect the key-value
  // pairs into a vector after stringifying and capitalizing the keys.
  let key-size = 0;
  for (header-pair in headers-vector)
    let string-key
      = concatenate(string-capitalize(as(<string>, head(header-pair))), ":");
    key-size := max(key-size, size(string-key));
    head(header-pair) := string-key;
  end;
  let format-string = format-to-string("%%-%ds %%s\n", key-size);
  for (header-pair in headers-vector)
    write-header-pair(stream, format-string,
                      head(header-pair), tail(header-pair));
  end;
  new-line(stream);
end function;


/// Whole files

// 'headers' will not be destructively modified by this operation.
define function process-interchange-file-headers
    (stream :: <stream>,
     #key direction,
          headers :: false-or(<vector> /* of: <table> */),
          header-order,
     #all-keys)
 => (header :: false-or(<table>))
  select (direction)
    #"output" =>
      when (headers)
        write-headers(stream, headers, header-order: header-order);
        #f
      end;
  end;
end function;

define macro with-open-interchange-file
  { with-open-interchange-file
        (?stream:variable = ?locator:expression,
         ?headers:variable = ?headers-val:expression,
         #rest ?keys:*, #key, #all-keys)
      ?:body
    end }
 => { begin
        with-open-file (?stream = ?locator, ?keys)
          let ?headers
            = process-interchange-file-headers
                (?stream, headers: ?headers-val, ?keys);
          ?body
        end
      end }
end macro;

define sealed method write-interchange-file
    (location :: <file-locator>,
     header-groups :: <vector> /* of: <table> */,
     lines :: false-or(<vector>) /* of: <string> */,
     #rest keys, #key, #all-keys)
 => ()
  // --- I'd like to use with-open-[interchange-]file here, but
  // I can't work out a way to pass in the "options".
  let _stream = #f;
  block ()
    _stream := apply(make, <file-stream>, locator: location,
                     direction: #"output", keys);
    let stream :: <file-stream> = _stream;
    let headers
      = apply(process-interchange-file-headers,
              stream, headers: header-groups, direction: #"output", keys);
    when (lines)
      for (line in lines)
        write-line(stream, line);
      end;
    end;
  cleanup
    if (_stream & stream-open?(_stream)) close(_stream) end;
  end;
end method;
