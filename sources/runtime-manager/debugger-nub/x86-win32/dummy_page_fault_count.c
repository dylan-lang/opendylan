/* ********************************************************************** */
/* ** misc_utils.c                                                     ** */
/* ** Dummy implementation for the page fault count of a process       ** */
/* **                                                                  ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Keith Dennison                                           ** */
/* ** Copyright: Functional Objects, Inc. 1997, All Rights Reserved ** */
/* ********************************************************************** */

#include "nub-core.h"

void initialize_get_os_process_page_fault_count(void)
{
}


NUBINT nub_get_process_page_fault_count
  (NUB nub)
{
  return ((NUBINT)0);
}
