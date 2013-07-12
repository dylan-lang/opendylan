Module: make-dylan-app

define constant $main-template-simple :: <string>
  = ("Module: %s\n"
     "Synopsis: \n"
     "Author: \n"
     "Copyright: \n"
     "\n"
     "define function main (name :: <string>, arguments :: <vector>)\n"
     "  format-out(\"Hello, world!\\n\");\n"
     "  exit-application(0);\n"
     "end function main;\n"
     "\n"
     "main(application-name(), application-arguments());\n");

define constant $library-template-simple :: <string>
  = ("Module: dylan-user\n"
     "\n"
     "define library %s\n"
     "  use common-dylan;\n"
     "  use io;\n"
     "end library;\n"
     "\n"
     "define module %s\n"
     "  use common-dylan, exclude: { format-to-string };\n"
     "  use format-out;\n"
     "end module;\n");

define constant $lid-template-simple :: <string>
  = ("Library: %s\n"
     "Files: %s\n"
     "       %s\n");

