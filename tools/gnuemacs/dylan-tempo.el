;;;; Module: Dylan Source Templates
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.
;;;; 
;;;; The contents of this file are automatically generated from
;;;; some editor-independent templates.
;;;; 
;;;; See /usr/local/soft/emacs-19.30/run/hqn/sol2.3_sparc/share/emacs/19.30/lisp/tempo.el

(require 'tempo)

(defvar *dylan-tempo-tags* '() "An association list with tags and corresponding templates,
for Dylan mode.")

(tempo-define-template
 "begin"
 '(
   "begin" '> 'n
   "  " 'p "XbodyX;" '> 'n
   "end;" '> 'n
 )
 "begin"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "block"
 '(
   "block (" 'p "Xexit-functionX)" '> 'n
   "  XbodyX ;" '> 'n
   "afterwards" '> 'n
   "  XbodyX ;" '> 'n
   "cleanup" '> 'n
   "  XbodyX ;" '> 'n
   "exception" '> 'n
   "    ( XnameX :: <XtypeX>," '> 'n
   "      test: XexprX," '> 'n
   "      init-arguments: XexprX )" '> 'n
   "  XbodyX ;" '> 'n
   "end block;" '> 'n
 )
 "block"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "case"
 '(
   "case" '> 'n
   "  " 'p "XtestX =>" '> 'n
   "    XbodyX ;" '> 'n
   "  otherwise =>" '> 'n
   "    XbodyX ;" '> 'n
   "end case;" '> 'n
 )
 "case"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-class"
 '(
   "define XadjectiveX class " 'p "<Xclass-nameX> (<XsuperclassX>, )" '> 'n
   "  XadjectiveX slot XnameX :: <XtypeX> = XexprX," '> 'n
   "    setter: XnameXor#f," '> 'n
   "    init-keyword: XkeyX:," '> 'n
   "    required-init-keyword: XkeyX:," '> 'n
   "    init-value: XexprX," '> 'n
   "    init-function: XexprX," '> 'n
   "    type: XexprX" '> 'n
   ";" '> 'n
   "" '> 'n
   "  required" '> 'n
   "  keyword XkeyX: = XexprX," '> 'n
   "    init-value: XexprX," '> 'n
   "    init-function: XexprX," '> 'n
   "    type: XexprX" '> 'n
   ";" '> 'n
   "    " '> 'n
   "  inherited slot XnameX = XexprX," '> 'n
   "    init-value: XexprX," '> 'n
   "    init-function: XexprX" '> 'n
   ";" '> 'n
   "end class ;" '> 'n
 )
 "define class"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-constant"
 '(
   "define constant " 'p "$Xconstant-nameX" '> 'n
   "    :: <XtypeX>" '> 'n
   "  = XexprX;" '> 'n
 )
 "define constant"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-function"
 '(
   "define function " 'p "Xfunction-nameX" '> 'n
   "    (XnameX :: <XtypeX>," '> 'n
   "     XnameX == XexprX," '> 'n
   "     #next XnameX," '> 'n
   "     #rest XnameX," '> 'n
   "     #key XkeyX: XkeyX = XexprX," '> 'n
   "     #all-keys)" '> 'n
   " => (XnameX," '> 'n
   "     #rest XnameX)" '> 'n
   "  XbodyX;" '> 'n
   "end function ;" '> 'n
 )
 "define function"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-generic"
 '(
   "define XadjectiveX generic " 'p "Xgeneric-nameX" '> 'n
   "    (XnameX :: <XtypeX>," '> 'n
   "     XnameX == XexprX," '> 'n
   "     #next XnameX," '> 'n
   "     #rest XnameX," '> 'n
   "     #key XkeyX: XkeyX = XexprX," '> 'n
   "     #all-keys)" '> 'n
   " => (XnameX," '> 'n
   "     #rest XnameX)" '> 'n
   ";" '> 'n
 )
 "define generic"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-library"
 '(
   "define library " 'p "Xlibrary-nameX" '> 'n
   "  export XmoduleX, ;" '> 'n
   "" '> 'n
   "  use XlibraryX," '> 'n
   "    import: all / { XmoduleX, Xmodule1X => Xmodule2X, }," '> 'n
   "    exclude: { XmoduleX, }," '> 'n
   "    prefix: \"\"," '> 'n
   "    rename: { Xmodule1X => Xmodule2X, }," '> 'n
   "    export: all / { XmoduleX, }" '> 'n
   ";" '> 'n
   "end library ;" '> 'n
 )
 "define library"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-macro"
 '(
   "define macro " 'p "Xmacro-nameX-definer" '> 'n
   "// body style definition rule" '> 'n
   "  { define" '> 'n
   "        \`modifier\` / \`pattern-variable\` / ..." '> 'n
   "      XnameX" '> 'n
   "        \`pattern\`" '> 'n
   "    ; end }" '> 'n
   " => { \`template\` } ;" '> 'n
   "// list style definition rule" '> 'n
   "  { define" '> 'n
   "        \`modifier\` / \`pattern-variable\` / ..." '> 'n
   "      XnameX" '> 'n
   "        \`pattern\`" '> 'n
   "    }" '> 'n
   " => { \`template\` } ;" '> 'n
   "// statement rule" '> 'n
   "  { XnameX" '> 'n
   "      \`pattern\`" '> 'n
   "    ; end }" '> 'n
   " => { \`template\` } ;" '> 'n
   "// function rule" '> 'n
   "  { XnameX ( \`pattern\` ) }" '> 'n
   " => { \`template\` } ;" '> 'n
   "// aux rule sets" '> 'n
   "aux-rule-set:" '> 'n
   "  { \`pattern\` }" '> 'n
   " => { \`template\` } ;" '> 'n
   "/*" '> 'n
   "  In this compact grammar," '> 'n
   "    \"...\" means \"repeat the preceding element any number of times\";" '> 'n
   "    \"{\`element\`/ \`element\`}\" means \"use one of the elements\";" '> 'n
   "    \"[\`element\`]\" means \"optionally include the element\";" '> 'n
   "    if any of the above chracters has whitespace on both sides, it" '> 'n
   "      literally represents itself." '> 'n
   "  \`modifier\`" '> 'n
   "    UNRESERVED-NAME" '> 'n
   "  \`pattern-variable\`" '> 'n
   "    ?NAME[:WORD]" '> 'n
   "    ..." '> 'n
   "  \`pattern\`" '> 'n
   "    \`pattern-list\` ;..." '> 'n
   "  \`pattern-list\`" '> 'n
   "    \`simple-pattern\`... [, \`pattern-list\`]" '> 'n
   "    \`property-list-pattern\`" '> 'n
   "  \`simple-pattern\`" '> 'n
   "    NAME // not \"end\"" '> 'n
   "    =>" '> 'n
   "    ( [ { \`pattern\` } ] )" '> 'n
   "    \`pattern-variable\` [:: \`pattern-variable\`] [= \`pattern-variable\`]" '> 'n
   "  \`property-list-pattern\`" '> 'n
   "    #rest \`pattern-variable\` [, #key [\`pattern-keywords\`]]" '> 'n
   "    #key [\`pattern-keywords\`]" '> 'n
   "  \`pattern-keywords\`" '> 'n
   "    #all-keys" '> 'n
   "    [?]?NAME[:WORD] [= \`expression\`]" '> 'n
   "" '> 'n
   "  \`template\`" '> 'n
   "    \`template-element\`..." '> 'n
   "  \`template-element\`" '> 'n
   "    {NAME/ SYMBOL/ NUMBER/ CHARACTER-LITERAL/ STRING/ UNARY-OPERATOR}" '> 'n
   "    {\`separator\`/ #-word/ ./ ::/ =>}" '> 'n
   "    ( [ { #( #[ \`template\` ] ) } ] )" '> 'n
   "    \`substitution\`" '> 'n
   "  \`separator\`" '> 'n
   "    ;/ ,/ BINARY-OPERATOR" '> 'n
   "  \`substitution\`" '> 'n
   "    [STRING ##] {NAME/ STRING/ SYMBOL} [## STRING]" '> 'n
   "    [??NAME [\`separator\`]] ..." '> 'n
   "    ?=NAME" '> 'n
   "*/" '> 'n
   "end macro ;" '> 'n
 )
 "define macro"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-method"
 '(
   "define method XadjectiveX " 'p "Xmethod-nameX" '> 'n
   "    (XnameX :: <XtypeX>," '> 'n
   "     XnameX == XexprX," '> 'n
   "     #next XnameX," '> 'n
   "     #rest XnameX," '> 'n
   "     #key XkeyX: XkeyX = XexprX," '> 'n
   "     #all-keys)" '> 'n
   " => (XnameX," '> 'n
   "     #rest XnameX)" '> 'n
   "  XbodyX;" '> 'n
   "end method ;" '> 'n
 )
 "define method"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-module"
 '(
   "define module " 'p "Xmodule-nameX" '> 'n
   "  export XnameX, ;" '> 'n
   "" '> 'n
   "  create XnameX, ;" '> 'n
   "" '> 'n
   "  use XmoduleX," '> 'n
   "    import: all / { XnameX, Xname1X => Xname2X, }," '> 'n
   "    exclude: { XnameX, }," '> 'n
   "    prefix: \"\"," '> 'n
   "    rename: { Xname1X => Xname2X, }," '> 'n
   "    export: all / { XnameX, }" '> 'n
   ";" '> 'n
   "end module ;" '> 'n
 )
 "define module"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-sealed-domain"
 '(
   "define sealed domain " 'p "Xgeneric-nameX " '> 'n
   "    (<XtypeX>, );" '> 'n
 )
 "define sealed domain"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "define-variable"
 '(
   "define XadjectiveX variable " 'p "*Xvariable-nameX*" '> 'n
   "    :: <XtypeX>" '> 'n
   "  = XexprX;" '> 'n
 )
 "define variable"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "for"
 '(
   "for (" '> 'n
   "    " 'p "XvarX = XexprX then XexprX," '> 'n
   "    XvarX in XcollectionX," '> 'n
   "    XvarX from XexprX" '> 'n
   "      to / above / below XexprX" '> 'n
   "      by XexprX" '> 'n
   "    until: / while: XexprX" '> 'n
   "    )" '> 'n
   "  XbodyX ;" '> 'n
   "finally" '> 'n
   "  XbodyX ;" '> 'n
   "end for;" '> 'n
 )
 "for"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "if"
 '(
   "if ( " 'p "XtestX )" '> 'n
   "    XbodyX ;" '> 'n
   "  elseif ( XtestX )" '> 'n
   "    XbodyX ;" '> 'n
   "  else" '> 'n
   "    XbodyX ;" '> 'n
   "end if;" '> 'n
 )
 "if"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "let"
 '(
   "let ( " 'p "XnameX, )" '> 'n
   "  = XexprX ;" '> 'n
 )
 "let"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "let-handler"
 '(
   "let handler" '> 'n
   "    (" 'p "<XtypeX>," '> 'n
   "     test: XexprX," '> 'n
   "     init-arguments: XexprX)" '> 'n
   "  = XexprX ;" '> 'n
 )
 "let handler"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "local-method"
 '(
   "local" '> 'n
   "  method  " 'p "Xmethod-nameX" '> 'n
   "      (XnameX :: <XtypeX>," '> 'n
   "       XnameX == XexprX," '> 'n
   "       #next XnameX," '> 'n
   "       #rest XnameX," '> 'n
   "       #key XkeyX: XkeyX = XexprX," '> 'n
   "       #all-keys)" '> 'n
   "    => (XnameX," '> 'n
   "       #rest XnameX)" '> 'n
   "    XbodyX;" '> 'n
   "  end method;" '> 'n
 )
 "local"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "method"
 '(
   "method " 'p "Xmethod-nameX" '> 'n
   "    (XnameX :: <XtypeX>," '> 'n
   "     XnameX == XexprX," '> 'n
   "     #next XnameX," '> 'n
   "     #rest XnameX," '> 'n
   "     #key XkeyX: XkeyX = XexprX," '> 'n
   "     #all-keys)" '> 'n
   " => (XnameX," '> 'n
   "     #rest XnameX)" '> 'n
   "  XbodyX;" '> 'n
   "end method;" '> 'n
 )
 "method"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "select"
 '(
   "select ( " 'p "XtargetX by XtestX )" '> 'n
   "  XexprX, =>" '> 'n
   "    XbodyX ;" '> 'n
   "  otherwise =>" '> 'n
   "    XbodyX ;" '> 'n
   "end select;" '> 'n
 )
 "select"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "unless"
 '(
   "unless ( " 'p "XtestX )" '> 'n
   "  XbodyX ;" '> 'n
   "end unless;" '> 'n
 )
 "unless"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "until"
 '(
   "until ( " 'p "XtestX )" '> 'n
   "  XbodyX ;" '> 'n
   "end until;" '> 'n
 )
 "until"
 ""
 *dylan-tempo-tags*)

(tempo-define-template
 "while"
 '(
   "while ( " 'p "XtestX )" '> 'n
   "  XbodyX ;" '> 'n
   "end while;" '> 'n
 )
 "while"
 ""
 *dylan-tempo-tags*)

