
! more rules to be used in addition to "cinterf.pat" for generating C code
! from "olectl.h".

DECLARE_INTERFACE_(<I>,<I>)\{*\/\/\ $2 methods\W\n<skip><matchparen>\}\G\;=\
	@{interface $1 \: public $2 \{ $5 \}}

skip:\/\/*\n=@end
skip:\n\W\n=@end
skip:\P\}=@end

members:\/\/*\n=
members:PURE=

members:STDMETHOD(<I>)\W<args>=\
	@write{${header-file};\
	  \nextern HRESULT __stdcall __RPC_FAR\nDW_$N_$1 (@wrap-args{$2})\;\n}\
	@append{vt;\,\sDW_$N_$1}\
      \nHRESULT C_$N_$1 (@wrap-args{$2})\ \
	 \{\n\ \ return This-\>lpVtbl-\>$1(@justparms{$2})\;\n\}\n

members:STDMETHOD_(<matchparen>,<I>)\W<args>=\
	@write{${header-file};\
	  \nextern $1 __stdcall __RPC_FAR\nDW_$N_$2 (@wrap-args{$3})\;\n}\
	@append{vt;\,\sDW_$N_$2}\
      \n$1 C_$N_$2 (@wrap-args{$3})\ \
	 \{\n\s\s@maybe-return{$1}\IThis-\>lpVtbl-\>$2(@justparms{$3})\;\n\}\n

args:(THIS)=\n\t\t$N __RPC_FAR \* This@end
args:(THIS_\W<matchparen>)=\I$N __RPC_FAR \* This, $1@end

stars:<s>FAR<s>\*=$0

wrap-args:,<s><G>=,@wrap{$1$2}
