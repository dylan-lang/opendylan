/* C wrapper on the inet address routines
 * $Id: c-inet-address.c,v 1.1 2004/03/12 00:41:31 cgay Exp $
 * $HopeName: D-lib-inet-address!inet-address.c(trunk.2) $
 *
 * Author: James Casey
 */

#include <errno.h>  /* For errno */

#if defined(_WIN32) /* Win32 */
#include <winsock.h>
#else /* Not Win32 */
#include <sys/types.h>

#include <sys/socket.h> /* For AF_INET */
#include <netinet/in.h> /* For in_addr */
#include <arpa/inet.h> /* For inet_addr(), htonl() */
#include <netdb.h> /* For hostent, h_errno, gethostbyname(), gethostbyaddr() */
#endif

int
c_gethostaddrbyname(char * hostname, struct in_addr *ret_addr){
   struct hostent *hp;
   unsigned long iaddr;

   if(isdigit(hostname[0])){
     iaddr = inet_addr(hostname);
     if ((int)iaddr== -1){ /* Unknown IP Address */
       return -1;
     }
     memcpy((char *)ret_addr,(char *)&iaddr,sizeof(struct in_addr));
   }else{
     if ((hp=gethostbyname(hostname)) != 0){
       memcpy((char *)ret_addr,(char *)hp->h_addr_list[0],hp->h_length);
     }else{ /* Unknown Host */
       return -1;
     }
   }
   return 0;
}

int
c_gethostnamebyaddr(struct in_addr addr,char * retval, int bufsz){
  struct hostent hent, *hp;
  char buf[1024];
  unsigned long iaddr;
  int end;

#if defined(__sun__) && !defined(__svr4__)
  int h_errno = 0;

  iaddr=htonl(addr.s_addr);
  hp=gethostbyaddr((char *)&iaddr, sizeof(iaddr), AF_INET,
		   &hent, buf, sizeof(buf),&h_errno);
#else
#if !defined(_WIN32)
  extern int h_errno;

  iaddr=htonl(addr.s_addr);
#endif
  hp=gethostbyaddr((char *)&iaddr, sizeof(iaddr), AF_INET);
#endif

  if (hp == 0){
      return -1;
  }

  end = strlen(hp->h_name)<bufsz?strlen(hp->h_name):bufsz;
  memcpy(retval,hp->h_name, end);
  *(retval + end) = 0;
  return 0;
}

void
c_local_host_name(char * hostname, int bufsz){
  int res;

  char localhost[] = "localhost";
#if (defined(__sun__) && defined(__svr4__)) || defined (_WIN32) /* Solaris or Win32 */
  res=gethostname(hostname, bufsz);
#else /* Solaris;Win32 / not Solaris */
  res=gethostname(hostname);
#endif /* Solaris */
  if (res == -1)
    strncpy(hostname, localhost, bufsz);
}


/* Stevens Pg 730 */

char *
host_err_str(){
  static char msgstr[200];

#if !defined(_WIN32)
  extern int h_errno;
#endif

#if defined(__sun__) && !defined(__svr4__) /* SunOS */
  extern int h_nerr;
  extern char *h_errlist[];

  if (h_errno != 0){
    if (h_errno > 0 && h_errno < h_nerr)
      sprintf(msgstr, "%s", h_errlist[h_errno]);
    else
      sprintf(msgstr, "h_errno = %d", h_errno);
  }else{
    msgstr[0]= '\0';
  }
#else /* SunOS/not SunOS */
  sprintf(msgstr, "h_errno = %d", h_errno);
#endif /* not SunOS */

  return msgstr;
}
