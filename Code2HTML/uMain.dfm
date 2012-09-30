object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Code to HTML'
  ClientHeight = 777
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblSource: TLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 13
    Caption = '&Soruce'
    FocusControl = memoSource
  end
  object lblDest: TLabel
    Left = 8
    Top = 366
    Width = 22
    Height = 13
    Caption = '&Dest'
    FocusControl = memoSource
  end
  object bvSep2: TBevel
    Left = -7
    Top = 723
    Width = 644
    Height = 9
    Shape = bsTopLine
  end
  object Bevel1: TBevel
    Left = -7
    Top = 355
    Width = 644
    Height = 9
    Shape = bsTopLine
  end
  object memoSource: TMemo
    Left = 8
    Top = 27
    Width = 593
    Height = 278
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
    Top = 385
    Width = 593
    Height = 323
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btnGo: TButton
    Left = 8
    Top = 738
    Width = 160
    Height = 25
    Caption = #22793#25563'(&G)'
    Default = True
    TabOrder = 3
    OnClick = btnGoClick
  end
  object btnEnd: TButton
    Left = 441
    Top = 738
    Width = 160
    Height = 25
    Cancel = True
    Caption = #32066#20102'(&X)'
    TabOrder = 5
    OnClick = btnEndClick
  end
  object btnCopy: TButton
    Left = 184
    Top = 738
    Width = 160
    Height = 25
    Caption = #12467#12500#12540'(&C)'
    TabOrder = 4
    OnClick = btnCopyClick
  end
  object btnClear: TButton
    Left = 8
    Top = 320
    Width = 160
    Height = 25
    Caption = #12463#12522#12450'(&D)'
    TabOrder = 1
    OnClick = btnClearClick
  end
end
