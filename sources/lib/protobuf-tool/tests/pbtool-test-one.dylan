Module: pbtool-test-one


define function main
    (name :: <string>, arguments :: <vector>)
  format-out("Hello, world! Here's a <message1>: %=\n",
             make(<message1>,
                  field1: "field1"));
  exit-application(0);
end function;

main(application-name(), application-arguments());
