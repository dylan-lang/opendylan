/* *********************************************************************** */
/* ** server_starter.c                                                  ** */
/* ** Simple stub program to start the connection server with a         ** */
/* ** minimized DOS box.                                                ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                      ** */
/* **             All Rights Reserved.                                  ** */
/* *********************************************************************** */

#include <windows.h>
#include "transport_protocols.h"
#include "nubserve.h"
#include "resource.h"

#define SERVER_EXECUTABLE_NAME "nubserve.exe"
#define SIZEOF_SERVER_EXECUTABLE_NAME 12
#define MESSAGE_TITLE "Functional Developer Remote Debugger"

#define SERVER_MINIMUM_PASSWORD_SIZE 6
#define SERVER_MAXIMUM_PASSWORD_SIZE 64

/* Allocation routines required by the RPC Runtime Library. */

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t x)
{
  return ((void __RPC_FAR*) malloc (x));
}

void __RPC_USER MIDL_user_free(void __RPC_FAR * x)
{
  free(x);
}

/* Global storage for the password, since more than one function needs to
   refer to it. */

static char password[SERVER_MAXIMUM_PASSWORD_SIZE + 2];

/* Examines the state of the password dialog box to determine whether the
   submitted password is within acceptable size boundaries. Returns
   TRUE if the password is acceptable, and FALSE otherwise. Note that this
   function also implicitly stores the password in the global storage. */

BOOL validate_password
  (HWND dialog)
{
  /* Pull the user's password text out of the dialog control. */

  int    sz = GetDlgItemText(dialog, IDC_PASSWORD_EDIT_BOX, password,
                             SERVER_MAXIMUM_PASSWORD_SIZE + 2);

  /* Ensure that the password is within acceptable size boundaries. */

  if (sz < SERVER_MINIMUM_PASSWORD_SIZE) {
    MessageBox(dialog,
               /* !MAINTENANCE! Shouldn't really be referring to the number "6"
                  directly in this string, but it isn't too serious. */
               "Password must be at least 6 characters long.",
               MESSAGE_TITLE,
               MB_OK | MB_ICONWARNING | MB_SETFOREGROUND);
    return(FALSE);
  }
  else if (sz > SERVER_MAXIMUM_PASSWORD_SIZE) {
    MessageBox(dialog,
               /* !MAINTENANCE! Shouldn't really be referring to the number "64"
                  directly in this string, but it isn't too serious. */
               "Password must be no more than 64 characters long.",
               MESSAGE_TITLE,
               MB_OK | MB_ICONWARNING | MB_SETFOREGROUND);
    return(FALSE);
  }
  else {
    return(TRUE);
  }
}

/* Dialog box housekeeping function. */

BOOL CALLBACK password_dialog_message_processor
  (HWND dialog, UINT msg, WPARAM wp, LPARAM lp)
{
  switch (msg) {
  case WM_INITDIALOG:
    SetFocus(GetDlgItem(dialog, IDC_PASSWORD_EDIT_BOX));
    return(FALSE);

  case WM_COMMAND:
    switch(LOWORD(wp)) {
    case IDC_PASSWORD_EDIT_BOX:
      return(FALSE);

    case ID_BUTTON_OK:
      if (validate_password(dialog)) {
        EndDialog(dialog, 1);
      }
      return(TRUE);

    case ID_BUTTON_CANCEL:
      EndDialog(dialog, 0);
      return(TRUE);

    }
    return(FALSE);

  default:
    return(FALSE);
  }
}

int WINAPI WinMain (HINSTANCE hInstance, 
                    HINSTANCE hPrevInstance,
                    LPSTR lpCommandLine,
                    int nCommandShow)
{
  STARTUPINFO           startup_info;
  PROCESS_INFORMATION   received_info;
  BOOL                  creation_status;
  BOOL                  server_already_running;
  RPC_STATUS            status;
  char                 *local_string_binding;
  RPC_IF_HANDLE         local_server_binding;
  DBG_TRANSPORT_INDEX   protocol = DBG_TRANSPORT_DEFAULT;
  NUBINT                hostname_length;
  int                   i = 0;
  int                   j = 0;
  char                  server_command_line
                          [SIZEOF_SERVER_EXECUTABLE_NAME + 
                           SERVER_MAXIMUM_PASSWORD_SIZE +
                           2];
  char                 *server_name = SERVER_EXECUTABLE_NAME;

  /* We only want one instance of the server to be running on any
     one machine. As a means of enforcing this, we will first attempt
     to connect to the server on the local machine, and call an
     arbitrary API. If this succeeds, we assume that the server is
     already running, and we won't start up a second instance. */

  /* Call the RPC library to generate a string encoding of the
     interface binding. Note that NULL is supplied as the network
     address, since we are only interested in establishing a connection
     to servers running on our own machine. */

  status = RpcStringBindingCompose
             (CONNECTION_SERVER_UUID,
              dbg_transport_protocols[protocol].ProtSeqEncoding,
              NULL,
              NULL,
              NULL,
              &local_string_binding);

  /* Call the RPC library to convert the string representation into
     an actual interface binding. */

  status = RpcBindingFromStringBinding(local_string_binding,
                                       &local_server_binding);

  /* With exception handling in place, call the server_get_hostname_length
     interface. If this succeeds, the server must already be running. */

  RpcTryExcept {
    hostname_length = svr_server_get_hostname_length(local_server_binding);
    server_already_running = TRUE;
  }
  RpcExcept(1) {
    server_already_running = FALSE;
  } 
  RpcEndExcept;

  /* Ping the user! */

  if (server_already_running) {
    return(
      MessageBox(NULL,
                 "The debug server program is already running.",
                 MESSAGE_TITLE,
                 MB_OK | MB_ICONINFORMATION | MB_SETFOREGROUND));
  }

  /* Otherwise, proceed with firing up the server. */

  /* First, obtain the password. */

  if (!DialogBox(hInstance,
                 MAKEINTRESOURCE( IDD_ACCEPT_PASSWORD ),
                 NULL,
                 (DLGPROC) password_dialog_message_processor)) {
    /* Take this path if the user choses "Cancel" in the password dialog.
       Don't start the server. */
    return(0);
  }

  /* If a valid password has been accepted, it will now be stored in the
     'password' variable, so we'll pass it on the command line when we
     call CreateProcess. */

  for (j = 0; j < SIZEOF_SERVER_EXECUTABLE_NAME; j++) {
    server_command_line[i] = server_name[j];
    i++;
  }
  server_command_line[i] = ' ';
  i++;
  for (j = 0; password[j] != '\0'; j++) {
    server_command_line[i] = password[j];
    i++;
  }
  server_command_line[i] = '\0';

  startup_info.cb                  = sizeof(STARTUPINFO);
  startup_info.lpReserved          = NULL;
  startup_info.lpDesktop           = NULL;
  startup_info.lpTitle             = NULL;
  startup_info.dwX                 = 0;
  startup_info.dwY                 = 0;
  startup_info.dwXSize             = 0;
  startup_info.dwYSize             = 0;
  startup_info.dwXCountChars       = 0;
  startup_info.dwYCountChars       = 0;
  startup_info.dwFillAttribute     = 0;
  startup_info.dwFlags             = STARTF_FORCEONFEEDBACK |
                                     STARTF_USESHOWWINDOW;

  /* The connection server is a console process, but we don't want it
     farting all over the screen, so we use SW_SHOWMINNOACTIVE so
     that its appearance causes no disturbances. */

  startup_info.wShowWindow         = SW_SHOWMINNOACTIVE;
  startup_info.cbReserved2         = 0;
  startup_info.lpReserved2         = NULL;

  creation_status = CreateProcess(NULL,
                                  server_command_line,
                                  NULL,
                                  NULL,
                                  FALSE,
                                  CREATE_NEW_CONSOLE,
                                  NULL,
                                  NULL,
                                  &startup_info,
                                  &received_info);

  /* Give some appropriate user-notification on the result. */

  if (creation_status) {
    return(
      MessageBox(NULL,
                 "The debug server program is running.",
                 MESSAGE_TITLE,
                 MB_OK | MB_ICONINFORMATION | MB_SETFOREGROUND));
  }
  else {
    return(
      MessageBox(NULL,
                 "The debug server program could not be started.",
                 MESSAGE_TITLE,
                 MB_OK | MB_ICONWARNING | MB_SETFOREGROUND));
  }
}
