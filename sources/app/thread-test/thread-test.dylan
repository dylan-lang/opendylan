module: thread-test
synopsis: stress-test thread creation
author: Andreas Bogk

begin  
  for(i from 0 below 100000)
    make(<thread>,
         function: method ()
                     with-stream-locked(*standard-output*)
                       format-out("Thread number %=\n", i);
                     end;
                   end);
  end for;
end