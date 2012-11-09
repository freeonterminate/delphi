object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 161
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 241
    Height = 25
    Caption = '1 : Show Find Dialog'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 81
    Width = 241
    Height = 72
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Lines.Strings = (
      'Please push "Show Find Dialog" first. '
      'Next, please push "Change Style". '
      'Please push "Show Find Dialog" once again. '
      ''
      'The Dialog are never displayed. ')
    ReadOnly = True
    TabOrder = 1
    StyleElements = [seBorder]
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 241
    Height = 25
    Caption = '2 : Change Style'
    TabOrder = 2
    OnClick = Button2Click
  end
  object FindDialog1: TFindDialog
    Left = 224
  end
end
