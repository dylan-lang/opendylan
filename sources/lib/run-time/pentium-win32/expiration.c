/* Check for expiration of the Dylan runtime library */

#include <windows.h>

#define RUN_TIME_API __declspec( dllexport )

/* Expiration date supplied by the makefile must be in the form YYYYMMYY
   and represents the last day that the runtime may be used ... */

#ifdef EXPIRATION
long encodedExpiration = EXPIRATION;
#else
long encodedExpiration = -1;
#endif

#define expirationMessageTemplate \
  "This test version of the Functional Developer runtime expired on DD-MMM-YYYY."
#define expirationMessageGeneric  \
  "This test version of the Functional Developer runtime has expired."

static char* MonthNames[13] = {"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

static void insert_integer(long n, long nDigits, char *p) {
  long i, digit;
  for (i = 0; i < nDigits; i++) {
    digit = n % 10;
    n = n / 10;
    *p-- = digit + '0';
  }
}

static void insert_month(long m, char *p) {
  long i;
  for (i = 0; i < 3; i++)
    *p-- = MonthNames[m][2 - i];
}

RUN_TIME_API
void check_runtime_library_expiration_date() {
  SYSTEMTIME now;
  char *expirationMessage, *originalExpirationMessage;
  long expirationMessageSize, i;
  long encodedNow, expirationYear, expirationMonth, expirationDay;

  if (encodedExpiration > 0) {
    GetLocalTime(&now);
    encodedNow = (10000 * now.wYear) + (100 * now.wMonth) + now.wDay;
    if (encodedNow > encodedExpiration) {
      expirationMessageSize = sizeof(expirationMessageTemplate);
      expirationMessage = LocalAlloc(LPTR, expirationMessageSize + 1);
      if (expirationMessage != NULL) {
	originalExpirationMessage = expirationMessageTemplate;
	for (i = 0; i < expirationMessageSize; i++)
	  expirationMessage[i] = originalExpirationMessage[i];
	expirationYear = encodedExpiration / 10000;
	expirationMonth = (encodedExpiration % 10000) / 100;
	expirationDay = encodedExpiration % 100;
	insert_integer(expirationYear, 4, &expirationMessage[expirationMessageSize - 3]);
	insert_month(expirationMonth, &expirationMessage[expirationMessageSize - 8]);
	insert_integer(expirationDay, 2, &expirationMessage[expirationMessageSize - 12]);
	FatalAppExit(0, expirationMessage);
      } else 
	FatalAppExit(0, expirationMessageGeneric);
    }
  }

  return;
}
