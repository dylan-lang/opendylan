! Synopsis:  Generate callback wrapper for C function types.
! Author:    David N. Gray
! Copyright: 1997 Functional Objects, Inc.  All rights reserved.

typedef BOOL (CALLBACK\*\W<relevant-name>)\G(HWND, UINT, WPARAM, LPARAM)\;=\n\
    define macro @export{@type{$1}-callback-wrapper}\n\
    \ \ \{ @type{$1}-callback-wrapper(\?new\:name,\?old\:name) \} \=\>\n\
    \ \ \ \{ \<DLGPROC\>-callback-wrapper(\?new,\?old) \}\n\
    end\;\n

typedef\ <type>(\WCALLBACK\W<stars>\W<relevant-name>)\G(\W<args>)\;=\N\n\
  define C-subtype @export{\<$3\>} ( \<C-function-pointer\> ) end\;\n\
  define macro @export{\<@fnname{$3}\>-callback-wrapper}\n\
  \ \{ \<@fnname{$3}\>-callback-wrapper(\?new\:name,\?old\:name) \} =>\n\
  \ \{ define C-callable-wrapper \?new of \?old\n\
       $4\
       @result{$1}\
  \ \ c-modifiers: "__stdcall"\;\n\
  \ end C-callable-wrapper \}\n\
  end\;\n



