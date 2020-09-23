VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H80000007&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "2P Falldown"
   ClientHeight    =   4245
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5280
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4245
   ScaleWidth      =   5280
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer timReady 
      Enabled         =   0   'False
      Interval        =   2000
      Left            =   4485
      Top             =   1770
   End
   Begin VB.Frame Frame1 
      Caption         =   "2P Falldown"
      Height          =   1950
      Left            =   1995
      TabIndex        =   0
      Top             =   1380
      Width           =   1680
      Begin VB.TextBox txtSpeed 
         Height          =   255
         Left            =   510
         TabIndex        =   11
         Text            =   "Custom"
         Top             =   1185
         Width           =   720
      End
      Begin VB.OptionButton Option4 
         Height          =   225
         Left            =   270
         TabIndex        =   10
         Top             =   1215
         Width           =   840
      End
      Begin VB.OptionButton Option3 
         Caption         =   "Fast"
         Height          =   225
         Left            =   270
         TabIndex        =   4
         Top             =   960
         Width           =   825
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Medium"
         Height          =   270
         Left            =   270
         TabIndex        =   3
         Top             =   690
         Value           =   -1  'True
         Width           =   930
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Slow"
         Height          =   225
         Left            =   270
         TabIndex        =   2
         Top             =   465
         Width           =   1080
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Start!"
         Default         =   -1  'True
         Height          =   285
         Left            =   390
         TabIndex        =   1
         Top             =   1575
         Width           =   930
      End
      Begin VB.Label Label1 
         Caption         =   "Speed:"
         Height          =   210
         Left            =   165
         TabIndex        =   5
         Top             =   210
         Width           =   660
      End
   End
   Begin VB.Image imgCheat 
      Height          =   135
      Left            =   5160
      Top             =   4125
      Width           =   135
   End
   Begin VB.Label breakLbl 
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H8000000E&
      Height          =   255
      Left            =   360
      TabIndex        =   9
      Top             =   75
      Visible         =   0   'False
      Width           =   525
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   ":"
      ForeColor       =   &H8000000E&
      Height          =   195
      Left            =   285
      TabIndex        =   8
      Top             =   60
      Visible         =   0   'False
      Width           =   150
   End
   Begin VB.Shape Shape1 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   225
      Left            =   105
      Top             =   45
      Visible         =   0   'False
      Width           =   165
   End
   Begin VB.Shape break 
      BackColor       =   &H00FF8080&
      BorderColor     =   &H00FF0000&
      FillColor       =   &H00FFFF80&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   570
      Shape           =   4  'Rounded Rectangle
      Top             =   2475
      Visible         =   0   'False
      Width           =   150
   End
   Begin VB.Label ready 
      BackColor       =   &H80000012&
      BackStyle       =   0  'Transparent
      Caption         =   "GET READY!"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   510
      Left            =   1395
      TabIndex        =   6
      Top             =   1785
      Visible         =   0   'False
      Width           =   2970
   End
   Begin VB.Shape ball 
      BackColor       =   &H000000FF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00000000&
      FillColor       =   &H000000FF&
      Height          =   315
      Left            =   2685
      Shape           =   3  'Circle
      Top             =   500
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick7 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   3930
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick6 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   3345
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick5 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   2775
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick4 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   2175
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick3 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   1545
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick2 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   975
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   10
      Left            =   4800
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   9
      Left            =   4320
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   8
      Left            =   3840
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   7
      Left            =   3360
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   6
      Left            =   2880
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   5
      Left            =   2400
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   4
      Left            =   1920
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   3
      Left            =   1440
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   2
      Left            =   960
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   1
      Left            =   480
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Shape brick 
      BackColor       =   &H0000FFFF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FF8080&
      FillColor       =   &H00C0C0C0&
      Height          =   195
      Index           =   0
      Left            =   0
      Top             =   360
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label Label2 
      BackStyle       =   0  'Transparent
      Caption         =   "Score:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   285
      Left            =   3840
      TabIndex        =   7
      Top             =   45
      Visible         =   0   'False
      Width           =   1395
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim game As Boolean
Dim speed As Integer
Dim vis As Integer
Dim ballMove As Boolean
Dim score As Double
Dim highscore As Integer
Dim breakTF As Boolean
Dim breakL As Integer
Dim breakV As Boolean
Dim breakVar As Integer
Dim Pos As PointAPI
Dim mouseXY As Long
Dim cheat As Boolean
Dim keyBreak As Boolean


'MAIN LOOP
Private Sub main()
Do While game = True 'LOOP IF GAME IS TRUE
    
'KEY PRESS DETECT
    If Keypressed(vbKeyRight) Or Keypressed(vbRightButton) Then
        If (ball.Left + ball.Width) < (Form1.Width - 100) Then
              ball.Left = ball.Left + speed
        End If
    End If
    If Keypressed(vbKeyLeft) Or Keypressed(vbLeftButton) Then
        If ball.Left > 10 Then
            ball.Left = ball.Left - speed
        End If
    End If
    
    If cheat = True Then
        mouseXY = GetCursorPos(Pos)
        ball.Left = Pos.X * 10
    End If
    
    

    If Keypressed(vbKeySpace) Or Keypressed(vbMiddleButton) Then
        If keyBreak = True Then
            If breakVar > 0 Then
                breakVar = breakVar - 1
                keyBreak = False
                For h = 0 To 10
                brick(h).Visible = False
                Next h
                For m = 0 To 10
                brick2(m).Visible = False
                Next m
                For n = 0 To 10
                brick3(n).Visible = False
                Next n
                For o = 0 To 10
                brick4(o).Visible = False
                Next o
                For p = 0 To 10
                brick5(p).Visible = False
                Next p
                For q = 0 To 10
                brick6(q).Visible = False
                Next q
                For t = 0 To 10
                brick7(t).Visible = False
                Next t
                breakLbl.Caption = breakVar
            End If
        End If
        Else
        keyBreak = True
    End If


'MOVEMENT
    
    'row 1
    For a = 0 To 10
        brick(a).Top = brick(a).Top - speed
    Next a

    'row 2
    For b = 0 To 10
        brick2(b).Top = brick2(b).Top - speed
    Next b

    'row 3
    For c = 0 To 10
        brick3(c).Top = brick3(c).Top - speed
    Next c
    
    'row 4
    For d = 0 To 10
        brick4(d).Top = brick4(d).Top - speed
    Next d
    
    'row 5
    For e = 0 To 10
        brick5(e).Top = brick5(e).Top - speed
    Next e
    
    'row 6
    For f = 0 To 10
        brick6(f).Top = brick6(f).Top - speed
    Next f
    
    'row 7
    For g = 0 To 10
        brick7(g).Top = brick7(g).Top - speed
    Next g
    
'BALL FALL

    If ballMove = True Then
        If ball.Top < 3800 Then
                ball.Top = ball.Top + (speed * 5)
        End If
    End If
    
'COLLISION DETECTION
    
    'WHEN BRICK ROW REACHES TOP, MAKE BRICKS APPEAR AT top: 4245, and randomly make them visible
    If brick(0).Top < 0 Then
        For a = 0 To 10
            brick(a).Top = 4245
            vis = (1 * Rnd) 'EITHER 1 or 0
            If vis = 1 Then
                brick(a).Visible = True
            Else 'IF vis = 0
                brick(a).Visible = False
            End If
        Next a
    End If
    
    If brick2(0).Top < 0 Then
        For b = 0 To 10
            brick2(b).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick2(b).Visible = True
            Else
                brick2(b).Visible = False
            End If
        Next b
    End If

    If brick3(0).Top < 0 Then
        For c = 0 To 10
            brick3(c).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick3(c).Visible = True
            Else
                brick3(c).Visible = False
            End If
        Next c
    End If

    If brick4(0).Top < 0 Then
        For d = 0 To 10
            brick4(d).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick4(d).Visible = True
            Else
                brick4(d).Visible = False
            End If
        Next d
    End If
    
    If brick5(0).Top < 0 Then
        For e = 0 To 10
            brick5(e).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick5(e).Visible = True
            Else
                brick5(e).Visible = False
            End If
        Next e
    End If
    
    If brick6(0).Top < 0 Then
        For f = 0 To 10
            brick6(f).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick6(f).Visible = True
            Else
                brick6(f).Visible = False
            End If
        Next f
    End If
    
    If brick7(0).Top < 0 Then
        For g = 0 To 10
            brick7(g).Top = 4245
            vis = (1 * Rnd)
            If vis = 1 Then
                brick7(g).Visible = True
            Else
                brick7(g).Visible = False
            End If
        Next g
        breakTF = True
        break.Visible = False
    End If
    
    
'BALL COLLISION DETECT
'row 1
    For h = 0 To 10
    If ball.Left < brick(h).Left + (brick(h).Width - 100) And ball.Left + ball.Width > (brick(h).Left + 100) And ball.Top < brick(h).Top + (brick(h).Height - 100) And ball.Top + ball.Height > brick(h).Top Then
        If brick(h).Visible = True Then
            ballMove = False
            ball.Top = brick(h).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If
        Else
    ballMove = True
    End If
    
Next h

'row2
For m = 0 To 10
    If ball.Left < brick2(m).Left + (brick2(m).Width - 100) And ball.Left + ball.Width > (brick2(m).Left + 100) And ball.Top < brick2(m).Top + (brick2(m).Height - 100) And ball.Top + ball.Height > brick2(m).Top Then
        If brick2(m).Visible = True Then
            ballMove = False
            ball.Top = brick2(m).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If
    End If
Next m

'row3
For n = 0 To 10
    If ball.Left < brick3(n).Left + (brick3(n).Width - 100) And ball.Left + ball.Width > (brick3(n).Left + 100) And ball.Top < brick3(n).Top + (brick3(n).Height - 100) And ball.Top + ball.Height > brick3(n).Top Then
        If brick3(n).Visible = True Then
            ballMove = False
            ball.Top = brick3(n).Top - ball.Height 'MOVE BALL UP WITH BRICK
            
        End If
    End If
Next n

'row4
For o = 0 To 10
    If ball.Left < brick4(o).Left + (brick4(o).Width - 100) And ball.Left + ball.Width > (brick4(o).Left + 100) And ball.Top < brick4(o).Top + (brick4(o).Height - 100) And ball.Top + ball.Height > brick4(o).Top Then
        If brick4(o).Visible = True Then
            ballMove = False
            ball.Top = brick4(o).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If

    End If
Next o

'row5
For p = 0 To 10
    If ball.Left < brick5(p).Left + (brick5(p).Width - 100) And ball.Left + ball.Width > (brick5(p).Left + 100) And ball.Top < brick5(p).Top + (brick5(p).Height - 100) And ball.Top + ball.Height > brick5(p).Top Then
        If brick5(p).Visible = True Then
            ballMove = False
            ball.Top = brick5(p).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If
    End If
Next p

'row6
For q = 0 To 10
    If ball.Left < brick6(q).Left + (brick6(q).Width - 100) And ball.Left + ball.Width > (brick6(q).Left + 100) And ball.Top < brick6(q).Top + (brick6(q).Height - 100) And ball.Top + ball.Height > brick6(q).Top Then
        If brick6(q).Visible = True Then
            ballMove = False
            ball.Top = brick6(q).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If
    End If
Next q

'row7
For t = 0 To 10
    If ball.Left < brick7(t).Left + (brick7(t).Width - 100) And ball.Left + ball.Width > (brick7(t).Left + 100) And ball.Top < brick7(t).Top + (brick7(t).Height - 100) And ball.Top + ball.Height > brick7(t).Top Then
        If brick7(t).Visible = True Then
            ballMove = False
            ball.Top = brick7(t).Top - ball.Height 'MOVE BALL UP WITH BRICK
        End If
    End If
    
    
Next t

If ball.Left < break.Left + break.Width And ball.Left + ball.Width > break.Left And ball.Top < break.Top + break.Height And ball.Top + ball.Height > break.Top Then
    If break.Visible = True Then
        break.Visible = False
        breakVar = breakVar + 1
        breakLbl.Caption = breakVar
    End If
End If


'IF BALL REACHES THE TOP
If ball.Top < 10 Then
Call gameEnd
End If
    
'SCORE

score = score + (speed / 100)
Label2.Caption = "Score: " & Int(score)
If highscore < score Then
Label2.ForeColor = vbRed
End If

'BREAK
If breakTF = True Then
    breakL = (10 * Rnd)
    Do While breakV = False
        If brick7(breakL).Visible = False Then
            breakL = (10 * Rnd)
            breakV = False
        ElseIf brick7(breakL).Visible = True Then
            breakV = True
        End If
    Loop
    break.Left = brick7(breakL).Left + 100
    breakTF = False
    break.Visible = True
    breakV = False
End If
break.Top = brick7(0).Top - break.Height


Loop
End Sub

Private Sub gameEnd()
game = False
'CHECK SCORE
If highscore < score Then
Call WriteToINI("Data.ini", "Score", Int(score), App.Path & "\data.ini")
MsgBox "New High Score!: " & Int(score), vbExclamation, "2P Falldown"
Else
MsgBox "You Lost", vbInformation, "2P Falldown"
End If
Label2.Visible = False

'POSITION BALL
ball.Top = 500
ball.Left = Form1.Width / 2

'MAKE ALL BRICKS INVISIBLE
For h = 0 To 10
brick(h).Visible = False
Next h
For m = 0 To 10
brick2(m).Visible = False
Next m
For n = 0 To 10
brick3(n).Visible = False
Next n
For o = 0 To 10
brick4(o).Visible = False
Next o
For p = 0 To 10
brick5(p).Visible = False
Next p
For q = 0 To 10
brick6(q).Visible = False
Next q
For t = 0 To 10
brick7(t).Visible = False
Next t

Frame1.Visible = True
ball.Visible = False
break.Visible = False
Shape1.Visible = False
Label3.Visible = False
breakLbl.Visible = False
imgCheat.Visible = True
End Sub


Private Sub Command1_Click()
If Option1.Value = True Then
speed = 10
End If
If Option2.Value = True Then
speed = 15
End If
If Option3.Value = True Then
speed = 25
End If
If Option4.Value = True Then
On Error GoTo speedErr
    If Int(txtSpeed.Text) < 71 And Int(txtSpeed.Text) > 0 Then
       speed = txtSpeed
    Else
        MsgBox "input a value between 1 and 70", vbCritical, "Error"
        Exit Sub
    End If

End If


score = 0
game = True
ballMove = True
Frame1.Visible = False
ready.Visible = True
timReady.Enabled = True
ball.Visible = True
Label2.Caption = "Score:"
Label2.Visible = True
Label2.ForeColor = vbWhite
breakVar = 0
breakLbl.Caption = breakVar
breakLbl.Visible = True
Label3.Visible = True
Shape1.Visible = True
imgCheat.Visible = False
'CHECK HIGH SCORE
highscore = GetFromINI("Data.ini", "Score", App.Path & "\data.ini")
Exit Sub
speedErr:
MsgBox "Error with speed", vbCritical, "Error"
End Sub


Private Sub imgCheat_Click()
cheat = True
End Sub

Private Sub timReady_Timer()
'GET READY
ready.Visible = False
Call main
timReady.Enabled = False
End Sub
