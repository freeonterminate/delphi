object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Code to HTML'
  ClientHeight = 730
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    603
    730)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSource: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Code (&S)'
    FocusControl = memoSource
  end
  object lblDest: TLabel
    Left = 8
    Top = 326
    Width = 44
    Height = 13
    Caption = 'HTML (&H)'
    FocusControl = memoSource
  end
  object Bevel1: TBevel
    Left = -7
    Top = 315
    Width = 644
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = -17
    Top = 687
    Width = 644
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
    ExplicitTop = 724
  end
  object memoSource: TMemo
    Left = 8
    Top = 27
    Width = 587
    Height = 246
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object memoDest: TMemo
    Left = 8
    Top = 345
    Width = 587
    Height = 301
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitHeight = 338
  end
  object btnGo: TButton
    Left = 435
    Top = 652
    Width = 160
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #22793#25563'(&G)'
    Default = True
    TabOrder = 5
    OnClick = btnGoClick
    ExplicitTop = 689
  end
  object btnEnd: TButton
    Left = 435
    Top = 697
    Width = 160
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #32066#20102'(&X)'
    TabOrder = 7
    OnClick = btnEndClick
    ExplicitTop = 734
  end
  object btnCopy: TButton
    Left = 184
    Top = 652
    Width = 160
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #12467#12500#12540'(&C)'
    TabOrder = 4
    OnClick = btnCopyClick
    ExplicitTop = 689
  end
  object btnClear: TButton
    Left = 435
    Top = 279
    Width = 160
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #12463#12522#12450'(&D)'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnAdd: TButton
    Left = 8
    Top = 652
    Width = 160
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #24375#35519#35486#12398#36861#21152'(&A)'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnOpenIni: TButton
    Left = 8
    Top = 697
    Width = 160
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'IniFile '#12434#32232#38598#12377#12427'(&E)'
    TabOrder = 6
    OnClick = btnOpenIniClick
  end
end
