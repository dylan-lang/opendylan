Module: write-100mb

define function main
    ()
  let string = make(<string>, size: 100, fill: 'x');
  with-open-file (stream = "/tmp/100mb.dylan.txt", direction: #"output")
    for (i from 1 to 1024 * 1024)
      write(stream, string);
    end;
  end;
end function;

main();
