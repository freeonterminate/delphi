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
  PIndex, PStart: PChar;
  Key, Value: String;
  Index: Integer;
begin
  if (not Assigned(iProc)) then
    Exit;

  PStart := GetEnvironmentStrings;
  PIndex := PStart;
  try
    repeat
      Key := PIndex;

      Index := Key.IndexOf('=');
      if (Index > -1) then
      begin
        Value := Key.Substring(Index + 1);
        Key := Key.Substring(0, Index);
        iProc(Key, Value);
      end;

      PIndex := StrEnd(PIndex);
      Inc(PIndex);
    until PIndex^ = #0;
  finally
    FreeEnvironmentStrings(PStart);
  end;
end;


end.
