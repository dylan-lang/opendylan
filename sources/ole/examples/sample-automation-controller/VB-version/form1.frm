VERSION 4.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Automation"
   ClientHeight    =   1935
   ClientLeft      =   1140
   ClientTop       =   1515
   ClientWidth     =   2655
   Height          =   2340
   Icon            =   "Form1.frx":0000
   Left            =   1080
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1935
   ScaleWidth      =   2655
   Top             =   1170
   Width           =   2775
   Begin VB.CommandButton Quit 
      Caption         =   "Quit"
      Height          =   495
      Left            =   1440
      TabIndex        =   4
      Top             =   1320
      Width           =   1095
   End
   Begin VB.CommandButton SetColor 
      Caption         =   "Set Color..."
      Height          =   495
      Left            =   1440
      TabIndex        =   3
      Top             =   120
      Width           =   1095
   End
   Begin VB.CommandButton Square 
      Caption         =   "Draw Square"
      Height          =   495
      Left            =   120
      TabIndex        =   2
      Top             =   1320
      Width           =   1215
   End
   Begin VB.CommandButton Circle 
      Caption         =   "Draw Circle"
      Height          =   495
      Left            =   120
      TabIndex        =   1
      Top             =   720
      Width           =   1215
   End
   Begin VB.CommandButton Clear 
      Caption         =   "Clear"
      Height          =   495
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog CommonDialog 
      Left            =   1440
      Top             =   720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   327680
   End
End
Attribute VB_Name = "Form1"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Dim P As Object


Private Sub Circle_Click()
    P.cirkle    ' Note that because "Circle" is a reserved word
                ' in VB, we can't use it.  According to the docs,
                ' P.[circle] should work, but it doesn't.
End Sub

Private Sub Clear_Click()
    P.Clear
End Sub
Private Sub Form_Load()
    Set P = CreateObject("HQNexamples.DPaintApp.1")
End Sub

Private Sub Quit_Click()
    Unload Form1
End Sub

Private Sub SetColor_Click()
    CommonDialog.ShowColor
    P.Color = CommonDialog.Color
End Sub

Private Sub Square_Click()
    P.Square
End Sub

