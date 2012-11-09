object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 141
  ClientWidth = 230
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 137
    Height = 14
    Caption = 'Apply THotKeyStyleHook'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object HotKey1: THotKey
    Left = 8
    Top = 36
    Width = 209
    Height = 19
    HotKey = 32833
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 70
    Width = 209
    Height = 63
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Lines.Strings = (
      'However, "(none)" is not localized. '
      ''
      'Please replace to get "shortcut text" using '
      '"IAccessible::get_accKeyboardShortcut".')
    ReadOnly = True
    TabOrder = 1
    StyleElements = [seBorder]
  end
end
