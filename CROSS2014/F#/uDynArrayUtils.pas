unit uDynArrayUtils;

interface

type
  TDynArrayUtils = class
  public
    class procedure Clear<T>(var iArray: TArray<T>);
    class procedure Add<T>(var iArray: TArray<T>; const iFact: T);
    class function Length<T>(const iArray: TArray<T>): Integer;
  end;

implementation


{ TDynArrayUtils }

class procedure TDynArrayUtils.Add<T>(var iArray: TArray<T>; const iFact: T);
var
  Len: Integer;
begin
  Len := System.Length(iArray);
  SetLength(iArray, Len + 1);

  iArray[Len] := iFact;
end;

class procedure TDynArrayUtils.Clear<T>(var iArray: TArray<T>);
begin
  SetLength(iArray, 0);
end;

class function TDynArrayUtils.Length<T>(const iArray: TArray<T>): Integer;
begin
  Result := System.Length(iArray);
end;

end.
