
#include <windows.h>       /* required for all Windows applications */
#include <winsock.h>
#include <stdio.h>         /* for sprintf                           */
#include <string.h>        /* for strlen                            */
#include <memory.h>
#include <process.h>       /* for _beginthread                      */
#include "wsock.h"         /* specific to this program              */

HANDLE hInst;              /* current instance                      */

SOCKET sock;
SOCKET sock2;
SOCKET sock3;
u_short portno = 998;            /* Which tcp port are we going to use?   */

char szBuff[ 80 ];         /* Temp buffer - used to pass strings    */
                           /* to and from dialog boxes, etc         */

#define MAX_PENDING_CONNECTS 4  /* The backlog allowed for listen() */
#define NO_FLAGS_SET         0  /* Used with recv()/send()          */
#define MY_MSG_LENGTH       80  /* msg buffer sent back and forth   */
char dylan_stream[ MY_MSG_LENGTH ];
char dylan_command[ MY_MSG_LENGTH ];

int JustConnected = 1;

/* The following four variables are for use by the user's program via the
   Dylan `Win32-common' library.  */
HANDLE application_hInstance = NULL;
HANDLE application_hPrevInstance = NULL;
LPSTR  application_lpCmdLine = NULL;
int    application_nCmdShow = SW_SHOWNORMAL;

/* activate the IDVM engine  */
LRESULT APIENTRY idvm_engine(void)
{
  DoDylanInit();

  dylan_listener();

  return(0);
  
}

/****************************************************************************\
*
*    FUNCTION:  FillAddr(HWND, PSOCKADDR_IN, BOOL)
*
*    PURPOSE:  Retrieves the IP address and port number.
*
*    COMMENTS:
*        This function is called in two conditions.
*            1.) When a client is preparing to call connect(), or
*            2.) When a server host is going to call bind(), listen() and
*                accept().
*        In both situations, a SOCKADDR_IN structure is filled.
*        However, different fields are filled depending on the condition.
*
*   ASSUMPTION:
*      szBuff is a global variable that contains the remote host name or NULL
*      if local.
*      bClient determines if this is being called by a client ( will be
*         performing a connect ) or a server ( will be listening )
*
*
*\***************************************************************************/

BOOL FillAddr(
        HWND hWnd,
        PSOCKADDR_IN psin,
        BOOL bClient)
{
   DWORD dwSize;
   PHOSTENT phe;
   char szTemp[200];


   psin->sin_family = AF_INET;


   /*
   *   If we are setting up for a listen() call (bConnect = FALSE),
   *   fill servent with our address.
   */
   if (bClient) {
      phe = gethostbyname(szBuff);
      if (phe == NULL) {
         sprintf(szTemp, "%d is the error. Make sure '%s' is listed in the hosts file.", WSAGetLastError(), szBuff);

         MessageBox(hWnd, szTemp, "gethostbyname() failed.", MB_OK);
         return FALSE;
      }
      memcpy((char FAR *)&(psin->sin_addr), phe->h_addr,
         phe->h_length);

      }
   else { // server

      /*
      *   Retrieve my ip address.  Assuming the hosts file in
      *   in %systemroot%/system/drivers/etc/hosts contains my computer name.
      */

      dwSize = sizeof(szBuff);
      gethostname(szBuff, dwSize);

      psin->sin_addr.s_addr = INADDR_ANY;
      }


   /*
   *   Retrieve the Port number
   */
   psin->sin_port = htons(portno);        /* Convert to network ordering */

   return TRUE;
}

char *read_string(SOCKET sock)
{
 unsigned long totalamt = 0;
 int ct = 0;
 char ch = '*';

 recv(sock, &ch, 1, NO_FLAGS_SET );
 while (ch != ' ')
   {
     szBuff[ct] = ch;
     ct++;
     recv(sock, &ch, 1, NO_FLAGS_SET );
   }

 szBuff[ct] = '\0';

 return(szBuff);

}

/* Set up Communication with LispWorks */

void ConfirmConnection(void) 
{
   SOCKADDR_IN local_sin;  /* Local socket - internet style */
   SOCKADDR_IN acc_sin;    /* Accept socket address - internet style */
   int acc_sin_len;        /* Accept socket address length */
   static int Connected = 0;

   if (Connected == 0)
     {

         sock = socket( AF_INET, SOCK_STREAM, 0);
         if (sock == INVALID_SOCKET) {
            MessageBox(NULL, "socket() failed", "Error", MB_OK);
            closesocket(sock);
            return;
         }

         /*
         *   Retrieve the IP address and TCP Port number
         */

         if (!FillAddr(NULL, &local_sin, FALSE ))
            return;


         /*
         *   Associate an address with a socket. (bind)
         */
         if (bind( sock, (struct sockaddr FAR *) &local_sin, sizeof(local_sin)) == SOCKET_ERROR) {
            sprintf(szBuff, "%d is the error", WSAGetLastError());

            MessageBox(NULL, szBuff, "bind(sock) failed", MB_OK);
            return;
         }

         if (listen( sock, MAX_PENDING_CONNECTS ) < 0) {
            sprintf(szBuff, "%d is the error", WSAGetLastError());

            MessageBox(NULL, szBuff, "listen(sock) failed", MB_OK);
            return;
         }

         acc_sin_len = sizeof(acc_sin);


         sock2 = accept( sock,(struct sockaddr FAR *) &acc_sin,
            (int FAR *) &acc_sin_len );
         if (sock2 < 0) {
            sprintf(szBuff, "%d is the error", WSAGetLastError());

            MessageBox(NULL, szBuff, "accept(sock2) failed", MB_OK);
            return;
         }

         sock3 = accept( sock,(struct sockaddr FAR *) &acc_sin,
            (int FAR *) &acc_sin_len );
         if (sock3 < 0) {
            sprintf(szBuff, "%d is the error", WSAGetLastError());

            MessageBox(NULL, szBuff, "accept(sock3) failed", MB_OK);
            return;
         }
	 Connected = 1;
     };

  JustConnected = 1;

}

/* Register socket with the Dylan Server */

LONG APIENTRY ConnectionProtocol(void)                /* window handle                   */
{
   int status;             /* Status Code */


   {
      WSADATA WSAData;
      char szTemp[80];

      if ((status = WSAStartup(MAKEWORD(1,1), &WSAData)) != 0) {
         sprintf(szTemp, "%d is the err", status);
         MessageBox( NULL, szTemp, "Error", MB_OK);
      }
      {
         /*

         When a network client wants to connect to a server,
         it must have:
            1.) a TCP port number (gotten via getservbyname())
            and
            2.) an IP address of the remote host (gotten via gethostbyname()).

         The following summarizes the steps used to connect.
         Make a dialog box (HostName)
         Get the name of the remote host computer in which
          to connect from the user (store string in "szBuff" global var)
       * Check to see if the hosts file knows the computer (gethostbyname)
       * Get the host information (hostent structure filled)
       * Fill in the address of the remote host into the servent structure (memcpy)
       * Make a dialog box (TCPPORTNUM)
       * Get the NAME of the port to connect to on the remote host from the
         user.
       * Get the port number (getservbyname)
       * Fill in the port number of the servent structure
         Establish a connection (connect)

         The * prefixed steps are done in the FillAddr() procedure.


         */
         SOCKADDR_IN dest_sin;  /* DESTination Socket INternet */



         /* Get the name of the remote host. Store the string in szBuff. */

	 {
	   DWORD dwSize;

	   /*
	    *   Retrieve my ip address.  Assuming the hosts file in
	    *   in %systemroot%/system/drivers/etc/hosts contains my computer name.
	    */
	   
	   dwSize = sizeof(szBuff);
	   gethostname(szBuff, dwSize);

	 }
/*
         status = DialogBox(hInst,
            "HOSTNAME",
            NULL,
            GetHostName);

         if (!status)
            return(0);
*/
         sock = socket( AF_INET, SOCK_STREAM, 0);
         if (sock == INVALID_SOCKET) {
            MessageBox(NULL, "socket() failed", "Error", MB_OK);
            return(0);
         }

         /*
         *    Retrieve the IP address and TCP Port number
         *    Global variable szBuff contains the remote host name.
         */
         if (!FillAddr( NULL, &dest_sin, TRUE)) {
            closesocket( sock );
            return(0);
         }


         if (connect( sock, (PSOCKADDR) &dest_sin, sizeof( dest_sin)) < 0) {
            closesocket( sock );
            MessageBox(NULL, "connect() failed", "Error", MB_OK);
            return(0);
         }

	 send(sock, "IDVM ", strlen("IDVM "), NO_FLAGS_SET);

         portno = strtol(read_string(sock), NULL, 0);

	 closesocket(sock);

      }
   }

   /* idvm_engine(); */


 return(0);
}

/*
WINAPI WinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow
    )

{
  application_hInstance = hInstance;
  application_hPrevInstance = hPrevInstance;
  application_lpCmdLine = lpCmdLine;
  application_nCmdShow = nCmdShow;

  ConnectionProtocol();
  DoDylanInit();
  return(0);
}
*/

void send_char(char s)
{
  send(sock2, &s, strlen(&s), NO_FLAGS_SET);
}

char read_char(void)
{
 char ch;

 recv(sock2, &ch, 1, NO_FLAGS_SET );
 return(ch);
}

void flush_socket_stream(SOCKET sock)
{
 unsigned long totalamt;

 ioctlsocket(sock, FIONREAD, &totalamt);
 while (totalamt != 0)
   {
    recv(sock, dylan_stream, MY_MSG_LENGTH, NO_FLAGS_SET );
    ioctlsocket(sock, FIONREAD, &totalamt);
  };

 
}

void flush_socket(void)
{

 flush_socket_stream(sock2);
 
}

void flush_control_socket(void)
{
 char ch = '*';

 while (ch != ' ')
   {
     recv(sock3, &ch, 1, NO_FLAGS_SET );
   };
 
}

char *read_command(void)
{
 unsigned long totalamt = 0;
 int ct = 0;
 char ch = '*';

 recv(sock3, &ch, 1, NO_FLAGS_SET );
 if (JustConnected == 1)
   {
     flush_control_socket();  /* Flush control channel */
     flush_control_socket();  /* Flush control channel for start of data */
     flush_socket();          /* Flush data channel */
     JustConnected = 0;
     recv(sock3, &ch, 1, NO_FLAGS_SET );
   };
  
 while (ch != ' ')
   {
     dylan_command[ct] = ch;
     ct++;
     recv(sock3, &ch, 1, NO_FLAGS_SET );
   };

 dylan_command[ct] = '\0';

 return(dylan_command);

}
