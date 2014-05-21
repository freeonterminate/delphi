unit uDynStringArray;

interface

type
  TDynStringArray = TArray<String>;

  TDynStringArrayUtils = class
  public
    class function Exists(
      const iArray: TDynStringArray;
      const iStr: String): Boolean;
  end;

implementation

{ TDynStringArrayUtils }

class function TDynStringArrayUtils.Exists(
  const iArray: TDynStringArray;
  const iStr: String): Boolean;
var
  Str: String;
begin
  Result := False;

  for Str in iArray do
    if (Str = iStr) then begin
      Result := True;
      Break;
    end;
end;

end.
