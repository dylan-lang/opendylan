#include "run-time.h"
#include "trace.h"

#include <string.h>

/* DYLAN CONSTANTS */

extern OBJECT KPfalseVKi;

/* NON-LOCAL EXITS */

#define COPY_WORDS(dst, src, size) memcpy((dst), (src), (size) * sizeof(D))

#ifdef VERIFY_NLX
/**
 * Check the NLX stack of the given TEB for invariants
 *
 * This will crash dylan if:
 *  - the UWP chain can reach NULL
 *  - an UWP from another thread is present
 *
 */
void verify_nlx_stack (TEB* teb) {
  int bug_found = 0;
  Unwind_protect_frame* ptr;
  /* iterate over uwp stack and verify it */
  for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
    /* top of uwp stack */
    if (ptr == &teb->top_uwp_frame) {
      break;
    }
    /* NULL current uwp */
    if (ptr == NULL) {
      bug_found = 1;
      break;
    }
    /* frame belongs to wrong TEB */
    if (ptr->verify_teb != teb) {
      bug_found = 1;
      break;
    }
    /* NULL previous uwp */
    if (ptr->previous_unwind_protect_frame == NULL) {
      bug_found = 1;
      break;
    }
  }
  /* act on results from verification run */
  if (bug_found) {
    fprintf(stderr, "BUG: invalid uwp stack:\n");
    /* run over stack again */
    for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
      /* top of uwp stack */
      if (ptr == &teb->top_uwp_frame) {
        fprintf(stderr, "  reached top\n");
        break;
      }
      /* NULL current uwp */
      if (ptr == NULL) {
        fprintf(stderr, "  current uwp is NULL\n");
        break;
      }
      fprintf(stderr, "  uwp<%p> previous uwp<%p>\n",
             ptr, ptr->previous_unwind_protect_frame);
      /* frame belongs to wrong TEB */
      if (ptr->verify_teb != teb) {
        fprintf(stderr, "  frame belongs to other thread teb<%p>\n",
                ptr->verify_teb);
      }
      /* NULL previous uwp */
      if (ptr->previous_unwind_protect_frame == NULL) {
        fprintf(stderr, "  previous uwp is NULL\n");
        break;
      }
    }
    fflush(stderr);
    abort();
  }
}
/**
 * Check a single BEF for invariants
 *
 * This will crash dylan if:
 *  - the BEF is owned by another thread
 *  - the UWP associated with the BEF is unreachable
 */
void verify_nlx_bef(TEB* teb, Bind_exit_frame* bef) {
  Unwind_protect_frame* dest_uwp = bef->present_unwind_protect_frame;
  int dest_reachable = 0;
  Unwind_protect_frame* ptr;
  /* check if BEF is of this TEB */
  if (bef->verify_teb != teb) {
    fprintf(stderr, "BUG: trying to nlx to bef<%p> of other thread teb<%p>\n",
                    bef, bef->verify_teb);
    fflush(stderr);
    abort();
  }
  /* verify that the associated UWP is reachable */
  for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
    /*  uwp found */
    if (ptr == dest_uwp) {
      dest_reachable = 1;
      break;
    }
    /* top of uwp stack */
    if (ptr == &teb->top_uwp_frame) {
      break;
    }
  }
  /* act on errors from verification run */
  if (!dest_reachable) {
    fprintf(stderr, "BUG: destination uwp<%p> of bef<%p> is unreachable\n",
           dest_uwp, bef);
    fflush(stderr);
    abort();
  }
}
#endif

static void nlx_step (Bind_exit_frame*) NORETURN_FUNCTION;

static void nlx_step (Bind_exit_frame* ultimate_destination) {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  verify_nlx_bef(teb, ultimate_destination);
#endif

  /* handled all unwind protect frames presently in force? */
  if (teb->uwp_frame == ultimate_destination->present_unwind_protect_frame) {
    trace_nlx("step to bef<%p> - reached ultimate uwp<%p>",
              ultimate_destination, teb->uwp_frame);

    /* invalidate current frame */
    teb->uwp_frame->ultimate_destination = NULL;

    /* jump to ultimate destination */
    nlx_longjmp(ultimate_destination->destination, 1);
  } else {
    Unwind_protect_frame* next_frame = teb->uwp_frame;

    trace_nlx("step to bef<%p> - to uwp<%p> previous uwp<%p> destination uwp<%p>",
              ultimate_destination,
              next_frame, next_frame->previous_unwind_protect_frame,
              ultimate_destination->present_unwind_protect_frame);

    /* pop current unwind protect frame */
    teb->uwp_frame = next_frame->previous_unwind_protect_frame;

    /* register ultimate destination of non-local exit in cupf */
    teb->uwp_frame->ultimate_destination = ultimate_destination;

    /* do cleanup step in next unwind protect frame */
    nlx_longjmp(next_frame->destination, 1);
  }
}

D FALL_THROUGH_UNWIND (D argument) {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
#endif

  trace_nlx("fallthrough uwp<%p>", teb->uwp_frame);

  /* return values */
  teb->uwp_frame->return_values.count = teb->return_values.count;
  teb->uwp_frame->return_values.value[0] = argument;
  if (teb->return_values.count > 1) {
    COPY_WORDS
      (&teb->uwp_frame->return_values.value[1],
       &teb->return_values.value[1], teb->return_values.count - 1);
  }

  /* invalidate current frame */
  teb->uwp_frame->ultimate_destination = NULL;

  return((D)0);
}

D CONTINUE_UNWIND () {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
#endif

  if (teb->uwp_frame->ultimate_destination) { /* nlx? */
    trace_nlx("continue unwind to bef<%p> from uwp<%p>",
              teb->uwp_frame->ultimate_destination,
              teb->uwp_frame);

    nlx_step(teb->uwp_frame->ultimate_destination);

    return(DFALSE);     /* Keeps some compilers happy */
  } else {
    trace_nlx("continue execution in uwp<%p> from uwp<%p>",
              teb->uwp_frame->previous_unwind_protect_frame,
              teb->uwp_frame);

    /* return values */
    int i;
    int n = teb->uwp_frame->return_values.count;
    teb->return_values.count = n;
    for (i = 0; i < n; i++) {
      teb->return_values.value[i]
        = teb->uwp_frame->return_values.value[i];
    }

    /* pop current unwind protect frame */
    teb->uwp_frame = teb->uwp_frame->previous_unwind_protect_frame;

    return n == 0 ? DFALSE : teb->return_values.value[0];
  }
}

D NLX (Bind_exit_frame* target, D argument) {
  TEB* teb = get_teb();
  trace_nlx("nlx to bef<%p> from uwp<%p>", target, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  verify_nlx_bef(teb, target);
#endif
  target->return_values.count = teb->return_values.count;
  target->return_values.value[0] = argument;
  if (teb->return_values.count > 1) {
    COPY_WORDS
      (&target->return_values.value[1],
       &teb->return_values.value[1], teb->return_values.count - 1);
  }
  nlx_step(target);
  return((D)0);                 /* Keeps some compilers happy -- Won't actually get here */
}

D SETUP_EXIT_FRAME (D frame) {
  TEB* teb = get_teb();
  Bind_exit_frame* be_frame = (Bind_exit_frame*)frame;
  trace_nlx("setup bef<%p> in uwp<%p>", be_frame, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  be_frame->verify_teb = teb;
#endif
  be_frame->present_unwind_protect_frame = teb->uwp_frame;
  return frame;
}

D SETUP_UNWIND_FRAME (D frame) {
  TEB* teb = get_teb();
  Unwind_protect_frame* uwp_frame = (Unwind_protect_frame*)frame;
  trace_nlx("setup uwp<%p> in uwp<%p>", uwp_frame, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  uwp_frame->verify_teb = teb;
#endif
  uwp_frame->previous_unwind_protect_frame = teb->uwp_frame;
  teb->uwp_frame = uwp_frame;
  uwp_frame->ultimate_destination = (Bind_exit_frame*)0;
  return frame;
}

D FRAME_DEST (D frame) {
  return((D)(((Bind_exit_frame*)frame)->destination));
}

D FRAME_RETVAL (D frame) {
  /* TODO: real multiple values */
  TEB* teb = get_teb();
  Bind_exit_frame *bef = ((Bind_exit_frame*) frame);
  /* Copy the multiple values into the result values MV */
  COPY_WORDS
      (&(teb->return_values.value[0]),
       &(bef->return_values.value[0]),
       bef->return_values.count);
  teb->return_values.count = bef->return_values.count;
  return((D)(bef->return_values.value[0]));
}
