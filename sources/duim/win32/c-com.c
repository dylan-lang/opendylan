/* Copyright 1996 Functional Objects, Inc.  All rights reserved. */

#include <unknwn.h>

void C_IMalloc_Free ( 
	    IMalloc __RPC_FAR * This, 
	    /* [in] */ void __RPC_FAR *pv) {
  This->lpVtbl->Free(This,pv);
}
