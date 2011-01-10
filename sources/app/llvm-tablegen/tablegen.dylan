Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  block ()
    let include-path = make(<stretchy-object-vector>);
    let input-file = #f;
    let backend = tablegen-print-records;

    // Process command-line arguments
    let arguments = application-arguments();
    iterate loop (index :: <integer> = 0)
      if (index < arguments.size)
        let argument = arguments[index];
        if (argument = "-I")
          // Include path
          add!(include-path, as(<directory-locator>, arguments[index + 1]));
          loop(index + 2)
        elseif (argument = "-gen-intrinsic")
          // Intrinsic generator backend
          backend := tablegen-gen-intrinsic;
          loop(index + 1);
        else
          // Input file argument
          input-file := argument;
          loop(index + 1);
        end if;
      end if;
    end iterate;

    // Parse the input file (or standard input)
    if (input-file)
      with-open-file (stream = input-file, direction: #"input")
        tablegen-parse(stream, include-path);
      end;
    else
      tablegen-parse(*standard-input*, include-path);
    end if;

    // Run the selected backend
    backend();

    exit-application(0);
  exception (e :: <simple-error>)
    format(*standard-error*, "%s\n", e);
    force-output(*standard-output*);
    exit-application(1);
  end;
end method main;

begin
  main();
end;
