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

  function CheckPressed(const iVK: Integer): Boolean;
  begin
    Result := ((GetKeyState(iVK) and $8000) > 0);
  end;

begin
  {
    The following codes are provisional codes.
    Please replace to get "shortcut text" using "IAccessible::get_accKeyboardShortcut".
  }

  ShortCutToKey(THotKey(Control).HotKey, Key, Shift);

  if (ssAlt in Shift) and (CheckPressed(VK_MENU)) then
    Text := Text + 'Alt + ';

  if (ssCtrl in Shift) and (CheckPressed(VK_CONTROL)) then
    Text := Text + 'Ctrl + ';

  if (ssShift in Shift) or (CheckPressed(VK_SHIFT)) then
    Text := Text + 'Shift + ';

  Text := Text + ShortCutToText(Key);

  if (Text = '') then
    Text := '(none)';

  SetBkMode(Canvas.Handle, TRANSPARENT);

  Canvas.Font.Color := StyleServices.GetStyleFontColor(sfEditBoxTextNormal);
  Canvas.TextOut(0, 0, Text);
end;

initialization
begin
  TCustomStyleEngine.RegisterStyleHook(THotKey, THotKeyStyleHook);
end;

end.
