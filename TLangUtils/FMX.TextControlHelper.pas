unit FMX.TextControlHelper;

interface

uses
  FMX.Controls
  , FMX.Forms
  ;

type
  TTextControlHelper = class helper for TTextControl
  public
    class procedure UpdateTextAll(const iForm: TCommonCustomForm);
    procedure UpdateText;
  end;

implementation

uses
  FMX.Types
  ;

{ TTextControlHelper }

procedure TTextControlHelper.UpdateText;
begin
  if AutoTranslate and (Text <> '') then
    Text := Translate(Text);
end;

class procedure TTextControlHelper.UpdateTextAll(
  const iForm: TCommonCustomForm);
var
  i: Integer;

  procedure UpdateTextAllSub(const iControl: TFmxObject);
  var
    i: Integer;
  begin
    if (iControl is TTextControl) then
      TTextControl(iControl).UpdateText;

    if (iControl is TControl) then
      for i := 0 to TControl(iControl).ControlsCount - 1 do
        UpdateTextAllSub(TControl(iControl).Controls[i]);
  end;

begin
  for i := 0 to iForm.ChildrenCount - 1 do
    UpdateTextAllSub(iForm.Children[i]);
end;

end.
