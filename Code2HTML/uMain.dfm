object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Code to HTML'
  ClientHeight = 730
  ClientWidth = 603
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 580
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
    Width = 315
    Height = 13
    Caption = 'Code (&S)   '#12477#12540#12473#12467#12540#12489#12434#8595#12371#12371#12395#12506#12540#12473#12488#12375#12390#12289#22793#25563#12508#12479#12531#12434#25276#12375#12414#12377
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
    Width = 590
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
    Width = 590
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
    TabOrder = 3
  end
  object btnGo: TButton
    Left = 438
    Top = 279
    Width = 160
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #22793#25563'(&G)'
    Default = True
    TabOrder = 2
    OnClick = btnGoClick
  end
  object btnEnd: TButton
    Left = 438
    Top = 697
    Width = 160
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #32066#20102'(&X)'
    TabOrder = 7
    OnClick = btnEndClick
  end
  object btnCopy: TButton
    Left = 438
    Top = 656
    Width = 160
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #12467#12500#12540'(&C)'
    TabOrder = 4
    OnClick = btnCopyClick
  end
  object btnClear: TButton
    Left = 8
    Top = 279
    Width = 160
    Height = 25
    Caption = #12463#12522#12450'(&D)'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnAdd: TButton
    Left = 8
    Top = 697
    Width = 160
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #19968#26178#24375#35519#35486#12398#36861#21152'(&A)...'
    TabOrder = 5
    OnClick = btnAddClick
  end
  object btnOpenIni: TButton
    Left = 174
    Top = 697
    Width = 160
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'IniFile '#12434#32232#38598#12377#12427'(&E)'
    TabOrder = 6
    OnClick = btnOpenIniClick
  end
  object chbxLineNo: TCheckBox
    Left = 314
    Top = 283
    Width = 121
    Height = 17
    Caption = #34892#30058#21495#12434#20184#12369#12427'(&L)'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
end
