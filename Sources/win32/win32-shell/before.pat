
! compensate for duplicate defintions in the .h file:
\L\#define\ SEE_MASK_\J<I> 0x<X>=\
    \Ndefine inline-only constant @export{\$SEE_MASK_$1} @tab{57}\= \#x$2\;\n\
    @define{\\L\\\#define\\\ SEE_MASK_$1\\S0x$2\\I\=}


