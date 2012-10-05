object frmExtra: TfrmExtra
  Left = 0
  Top = 0
  Caption = #24375#35519#35486#12398#36861#21152
  ClientHeight = 281
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    495
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object lblExplain: TLabel
    Left = 8
    Top = 8
    Width = 457
    Height = 13
    Caption = #19968#26178#30340#12395#24375#35519#38917#30446#12434#36861#21152#12375#12414#12377#12290#24120#26178#24375#35519#12375#12383#12356#22580#21512#12399' ini file '#12398' ExtraWord '#12395#36861#21152#12375#12390#12367#12384#12373#12356
  end
  object btnAddName: TButton
    Left = 8
    Top = 250
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #36861#21152'(&A)'
    TabOrder = 1
    OnClick = btnAddNameClick
  end
  object btnClearItem: TButton
    Left = 114
    Top = 250
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #21066#38500'(&D)'
    TabOrder = 2
    OnClick = btnClearItemClick
  end
  object btnApply: TButton
    Left = 283
    Top = 250
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #19968#26178#36969#29992#65288'&L'#65289
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnApplyClick
  end
  object lstbxWords: TListBox
    Left = 8
    Top = 27
    Width = 481
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 389
    Top = 250
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #12461#12515#12531#12475#12523#65288'&C'#65289
    ModalResult = 2
    TabOrder = 4
  end
end
