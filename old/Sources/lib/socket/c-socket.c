/* C wrapper on the Unix socket library to make things simpler
 * $Id: c-socket.c,v 1.1 2004/03/12 00:41:31 cgay Exp $
 * $HopeName: D-lib-socket!socket.c(trunk.2) $
 *
 * Author: James Casey
 */

#if defined(_WIN32) /* Win 32 */
#include <winsock.h>
#else /* not Win32 */
#include <string.h> /* For strerror() */
#include <errno.h> /* For errno */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif /* Win32 */

/* 
 * Socket routines 
 */

int 
c_socket_create(){  
  int descriptor;
  
  descriptor = socket(AF_INET, SOCK_STREAM,0);
  return descriptor;  
}

/* 
 * c_socket_connect -- Connect to the given remote InetAddress on the 
 * given port
 * default to a AF_INET address
 */
int
c_socket_connect(int descriptor, struct in_addr remote, 
		 unsigned short port, unsigned short * localportp){
  struct sockaddr_in remoteAddr;

  remoteAddr.sin_family = AF_INET;
  memcpy((char *)&remoteAddr.sin_addr,(char *)&remote,sizeof(remote));
  remoteAddr.sin_port=htons(port);

  if (connect(descriptor, 
	      (struct sockaddr *) &remoteAddr, 
	      sizeof(remoteAddr)) <0){
    return -1; 
  }

  if(*localportp == 0) {
    int length = sizeof(remoteAddr);
    if(getsockname(descriptor, (struct sockaddr *) &remoteAddr, &length)){
      return -1;
    }
    *localportp = ntohs(remoteAddr.sin_port);
  }
  return 0;
}

/* 
 * Close a given socket 
 */
int 
c_socket_close(int descriptor){
#if defined(_WIN32) /* Win32 */
  return closesocket(descriptor);
#else /* Not Win32 */
  return close(descriptor);
#endif /* Win32 */
}

/* 
 * adapted from stevens, pg 279
 */
int 
c_socket_recv(int descriptor, char *ptr, int offset, int nbytes, int flag){

  int nleft, nrecv;
  char *where;

  where = ptr + offset;
  nleft = nbytes;
  while(nleft >0){
    nrecv = recv(descriptor,where,nleft,flag);
    if(nrecv <0)
      return (nrecv); /* error, return < 0 */
    else if(nrecv == 0)
      break; /* EOF */
    nleft -= nrecv;
    where += nrecv;
  }
  return(nbytes - nleft); /* return >= 0 */
} 

int 
c_socket_send(int descriptor, char *ptr, int offset, int nbytes, int flag){

  int nleft, nsend;
  char *where;

  where = ptr + offset;
  nleft=nbytes;
  while(nleft >0){
    nsend = send(descriptor,where,nleft,flag);
    if(nsend <0)
      return (nsend); /* return < 0 on error */

    nleft -= nsend;
    ptr += nsend;
  }
  return(nbytes - nleft); /* return >= 0 */
} 


int 
c_socket_bind(int descriptor, int localport){
  return 0;
}

int 
c_socket_listen(int descriptor, int backlog){
  return 0;
}

int 
c_socket_accept(int descriptor, int *accepted){
  return 0;
}


int c_socket_ffi_initialize(void){
#if defined (_WIN32)
  WSADATA wsadata;

  if (WSAStartup(MAKEWORD(1,1), &wsadata) != 0) {
    return -1;
  }
#endif /* WIN32 */
  return 0;
}

int c_socket_ffi_finalize(void){
#if defined (_WIN32)
  WSADATA wsadata;

  if (WSACleanup() != 0) {
    return -1;
  }
#endif /* WIN32 */
  return 0;
}

/*
 * Utility routines for system error reporting protocol
 */
int 
c_socket_errno(){
  return errno;
}

char *
c_socket_strerror(int num){
#if defined(__sun__) && !defined(__svr4__) /* SunOS */
  extern char *sys_errlist[];
  char *rval = sys_errlist[num];
#else /* SunOS/ not SunOS */
  char *rval = strerror(num);
#endif /* not SunOS */

  return rval;
}







