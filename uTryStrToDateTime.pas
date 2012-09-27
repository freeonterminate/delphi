unit uTryStrToDateTime;

interface

function TryStrToDateTimeEx(
  const iStr: String;
  out oDateTime: TDateTime): Boolean;

implementation

uses
  System.SysUtils;

function TryStrToDateTimeEx(
  const iStr: String;
  out oDateTime: TDateTime): Boolean;
const
  DateSeps: array [0.. 1] of Char = ('/', '-');
  TimeSeps: array [0.. 1] of Char = (':', '.');
var
  FS: TFormatSettings;
  i, j: Integer;
begin
  FS := TFormatSettings.Create;

  for i := Low(DateSeps) to High(DateSeps) do begin
    FS.DateSeparator := DateSeps[i];

    for j := Low(TimeSeps) to High(TimeSeps) do begin
      FS.TimeSeparator := TimeSeps[j];

      Result := TryStrToDateTime(iStr, oDateTime, FS);
      if (Result) then
        Break;
    end;
  end;
end;

end.
