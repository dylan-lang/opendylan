FILES =	defsys.lisp demand-compiler.lisp \
        demand-loader.lisp main-resources.lisp \
	ReadMe Install Conformance \
	Share/*.lisp TransTime/*.lisp RunTime/*.lisp \
	Lib/*.dyl ParserEngine/*.dyl Infix/*.dyl* \
  	Zimmerman/*.dyl Zimmerman/*.lisp \
	Syntax/*.dyl Syntax/*.lisp Env/*.lisp \
	GenericEnv/*.lisp Examples/*.dyl

tar:
	tar -cvhf dylan-translator-`date +"%d%h%y"`.tar ${FILES}

clean:
	rm */*.*fasl

