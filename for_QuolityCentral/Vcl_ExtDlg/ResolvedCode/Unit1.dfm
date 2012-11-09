object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 136
  ClientWidth = 228
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
    Width = 209
    Height = 25
    Caption = 'Show Dialog'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 57
    Width = 209
    Height = 72
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Lines.Strings = (
      '[] is set to StyleElements of Control which '
      'rides on a dialog using Extension RTTI. '
      ''
      'When control is originally generated, you '
      'should set [] to StyleElements. ')
    ReadOnly = True
    TabOrder = 1
    StyleElements = [seBorder]
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 160
  end
end
