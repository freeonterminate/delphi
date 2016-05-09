unit uGetEnvironmentVariables;

interface

uses
  System.SysUtils;

type
  TGetEnvironmentVariablesProc =
    reference to procedure(const iKey, iValue: String);

procedure GetEnvironmentVariables(const iProc: TGetEnvironmentVariablesProc);

implementation

uses
  Winapi.Windows;

procedure GetEnvironmentVariables(const iProc: TGetEnvironmentVariablesProc);
var
  P: PChar;
  Key, Value: String;
  Index: Integer;
begin
  if (not Assigned(iProc)) then
    Exit;

  P := GetEnvironmentStrings;
  try
    repeat
      Key := P;

      Index := Key.IndexOf('=');
      if (Index > -1) then
      begin
        Value := Key.Substring(Index + 1);
        Key := Key.Substring(0, Index);
        iProc(Key, Value);
      end;

      P := StrEnd(P);
      Inc(P);
    until P^ = #0;
  finally
    FreeEnvironmentStrings(P);
  end;
end;


end.
