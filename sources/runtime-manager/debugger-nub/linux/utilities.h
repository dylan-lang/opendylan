/* If cond is true, then call perror on MESSAGE and exit with code 1 */
#define check_error(COND, MESSAGE) if (COND) {perror(MESSAGE); exit(1);}

/* Evaluation EXPR.  If value is negative then exit with error message */
#define call_check_error(EXPR, MESSAGE) check_error((EXPR)<0, MESSAGE)
