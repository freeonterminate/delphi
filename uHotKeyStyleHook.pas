unit uHotKeyStyleHook;

interface

uses
  Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;

type
  THotKeyStyleHook = class(TEditStyleHook)
  protected
    procedure Paint(Canvas: TCanvas); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

implementation

uses
  Winapi.Windows, System.Classes, Vcl.Themes, Vcl.ComCtrls, Vcl.Menus;

{ THotKeyStyleHook }

constructor THotKeyStyleHook.Create(AControl: TWinControl);
begin
  inherited;

  OverridePaint := True;
end;

procedure THotKeyStyleHook.Paint(Canvas: TCanvas);
var
  Text: String;
  Shift: TShiftState;
  Key: Word;
begin
  ShortCutToKey(THotKey(Control).HotKey, Key, Shift);

  if (ssAlt in Shift) then
    Text := Text + 'Alt + ';

  if (ssCtrl in Shift) then
    Text := Text + 'Ctrl + ';

  if (ssShift in Shift) then
    Text := Text + 'Shift + ';

  Text := Text + ShortCutToText(Key);

  SetBkMode(Canvas.Handle, TRANSPARENT);

  Canvas.Font.Color := StyleServices.GetStyleFontColor(sfEditBoxTextNormal);
  Canvas.TextOut(0, 0, Text);
end;

initialization
begin
  TCustomStyleEngine.RegisterStyleHook(THotKey, THotKeyStyleHook);
end;

end.
