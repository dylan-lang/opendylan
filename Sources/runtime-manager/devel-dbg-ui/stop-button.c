/* *************************************************************************** */
/* ** stop-button.c                                                         ** */
/* ** Functions implementing the stop button in the debugger                ** */
/* ** --------------------------------------------------------------------- ** */
/* ** Author: Paul Howard    Copyright: (c) 1996, 1997 Functional Objects, Inc.  ** */
/* **                                   All Rights Reserved.                ** */
/* *************************************************************************** */

#include <windows.h>
#include <stdio.h>

void terminate_stop_button ();

int primitive_command_line_length();

void primitive_fill_command_line(char *buf);

extern void c_signal_stop_button();

int primitive_command_line_length()
{
  char *ln = GetCommandLine();
  int   i = 0;
  while (ln[i] != '\0') i++;
  return(i);
}

void primitive_fill_command_line(char *buf)
{
  char *ln = GetCommandLine();
  int   i = 0;
  while (ln[i] != '\0') {
    buf[i] = ln[i];
    i++;
  }
}

BOOL APIENTRY dialog_proc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

HWND              StopButtonDlg;
BOOL              stop_requested = FALSE;

void create_and_process_stop_button ()
{
  StopButtonDlg = CreateDialog ((HINSTANCE) GetModuleHandle (NULL),
                                 MAKEINTRESOURCE(101),
                                 NULL,
                                 dialog_proc);

  ShowWindow(StopButtonDlg, SW_SHOWNA);

  stop_requested = FALSE;

  while (StopButtonDlg != NULL) {
    MSG    message;

    if (GetMessage (&message, NULL, 0, 0))
      DispatchMessage (&message);
    else
      Sleep (200);
  }
  return;
}

void terminate_stop_button ()
{
  CloseHandle (StopButtonDlg);
  StopButtonDlg = (HWND) NULL;
}

BOOL APIENTRY dialog_proc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg) {

  case WM_INITDIALOG:
    return (TRUE);
    break;

  case WM_COMMAND:
    stop_requested = TRUE;
    c_signal_stop_button();
    return (TRUE);
    break;

  default:
    return (FALSE);
    break;
  }
}
