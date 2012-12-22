object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 39
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 89
    Top = 8
    Width = 272
    Height = 321
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object ListBox1: TListBox
    Left = 367
    Top = 8
    Width = 260
    Height = 321
    ItemHeight = 13
    TabOrder = 3
  end
end
