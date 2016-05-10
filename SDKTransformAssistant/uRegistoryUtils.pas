unit uRegistoryUtils;

interface

uses
  System.SysUtils,
  Winapi.Windows;

function GetRegValue(const iReservedKey: HKEY; const iAddress: String): String;
function GetRegValue2(
  const iReservedKey: HKEY;
  const iAddress: String): String;
function GetRegDWORDValue(
  const iReservedKey: HKEY;
  const iAddress: String): DWORD;

procedure SetRegValue(iReservedKey: HKEY; iAddress, iValue: String);
procedure SetRegValue2(iReservedKey: HKEY; iAddress, iValue: String);

procedure DeleteReg(const iReservedKey: HKEY; const iAddress: String);

implementation

function DeleteDelimiter(
  const iStr: String;
  const iDelimiter: String = '\'): String;
var
  Len: Cardinal;
begin
  Result := iStr;

  Len := Length(Result);
  if (Result[Len] = iDelimiter) then
    Result := Copy(Result, 1, Len - 1);
end;

procedure RegValueGetPathValue(
  const iAddress: String;
  var oPath, oValue: String);
begin
  oPath := DeleteDelimiter(ExtractFilePath(iAddress));
  oValue := ExtractFileName(iAddress);

  if (oPath <> '') and (oPath[1] = '\') then
    Delete(oPath, 1, 1);
end;

function GetRegValueFull(
  const iReservedKey: DWORD;
  const iAddress, iPath, iValue: String;
  const vRegType: DWORD;
  var vBuffSize: DWORD;
  const vBuff: Pointer): Boolean;
var
  hReg: HKEY;
  RegType: DWORD;
begin
  hReg := 0;

  RegType := vRegType;

  RegOpenKeyEx(
    iReservedKey,
    PChar(iPath),
    0,
    KEY_QUERY_VALUE,
    hReg);
  try
    Result :=
      (
        RegQueryValueEx(
          hReg,
          PChar(iValue),
          nil,
          @RegType,
          vBuff,
          @vBuffSize
        )
        = ERROR_SUCCESS
      );
  finally
    RegCloseKey(hReg);
  end;
end;

function GetRegStringValue(
  const iReservedKey: DWORD;
  const iAddress, iPath, iValue: String): String;
var
  VarSize: DWORD;
begin
  SetLength(Result, $ff);
  VarSize := Length(Result);

  if
    (
      GetRegValueFull(
        iReservedKey,
        iAddress,
        iPath,
        iValue,
        REG_SZ,
        VarSize,
        PChar(Result)
      )
    )
  then
    Result := StrPas(PChar(Result))
  else
    Result := '';
end;

function GetRegValue(const iReservedKey: HKEY; const iAddress: String): String;
begin
  Result :=
    GetRegStringValue(iReservedKey, iAddress, DeleteDelimiter(iAddress), '');
end;

function GetRegValue2(
  const iReservedKey: HKEY;
  const iAddress: String): String;
var
  Path, Value: String;
begin
  RegValueGetPathValue(iAddress, Path, Value);

  Result :=
    GetRegStringValue(iReservedKey, iAddress, Path, Value);
end;

function GetRegDWORDValue(
  const iReservedKey: HKEY;
  const iAddress: String): DWORD;
var
  Path, Value: String;
  VarSize: DWORD;
begin
  RegValueGetPathValue(iAddress, Path, Value);

  VarSize := SizeOf(Result);

  if
    (
      not GetRegValueFull(
        iReservedKey,
        iAddress,
        Path,
        Value,
        REG_SZ,
        VarSize,
        @Result
      )
    )
  then
    Result := 0;
end;

procedure SetRegValueFull(
  const iReservedKey: DWORD;
  const iAddress, iPath, iValue: String);
var
  hReg: HKEY;
  Disposition: DWORD;
begin
  hReg := 0;

  RegCreateKeyEx(
    iReservedKey,
    PChar(iAddress),
    0,
    nil,
    REG_OPTION_NON_VOLATILE,
    KEY_ALL_ACCESS,
    nil,
    hReg,
    @Disposition);
  try
    RegSetValueEx(
      hReg, PChar(iPath), 0, REG_SZ, PChar(iValue), Length(iValue));
  finally
    RegCloseKey(hReg);
  end;
end;

procedure SetRegValue(iReservedKey: HKEY; iAddress, iValue: String);
begin
  SetRegValueFull(iReservedKey, iAddress, '', iValue);
end;

procedure SetRegValue2(iReservedKey: HKEY; iAddress, iValue: String);
var
  Path, Value: String;
begin
  RegValueGetPathValue(iAddress, Path, Value);
  SetRegValueFull(iReservedKey, Path, Value, iValue);
end;

procedure DeleteReg(const iReservedKey: HKEY; const iAddress: String);
var
  Len: Integer;
  Name: String;
  NameLen: Cardinal;
  FT: TFileTime;
  tmpAddr: String;

  function DeleteRegSub(iAddress: String): Boolean;
  var
    i: Integer;
    RetVal: DWORD;
    hReg: HKEY;
  begin
    Result := False;

    Len := Length(iAddress);

    if (Len < 1) then
      Exit;

    if (iAddress[Len] <> '\') then
      iAddress := iAddress + '\';

    RetVal :=
      RegOpenKeyEx(
        iReservedKey,
        PChar(DeleteDelimiter(iAddress)),
        0,
        KEY_ALL_ACCESS,
        hReg);
    try
      if (RetVal = ERROR_SUCCESS) then begin
        i := 0;

        repeat
          SetLength(Name, MAX_PATH);
          NameLen := Length(Name);

          RetVal :=
            RegEnumKeyEx(hReg, i, PChar(Name), NameLen, nil, nil, nil, @FT);

          if (RetVal = ERROR_SUCCESS) then begin
            SetLength(Name, NameLen);

            if (DeleteRegSub(iAddress + Name)) then
              Dec(i);
          end
          else begin
            tmpAddr := iAddress;

            if
              (Win32Platform <> VER_PLATFORM_WIN32_NT) and
              (tmpAddr[Len] = '\')
            then
              tmpAddr := DeleteDelimiter(tmpAddr);

            Result :=
              (RegDeleteKey(iReservedKey, PChar(tmpAddr)) = ERROR_SUCCESS);
          end;

          Inc(i);
        until (RetVal <> ERROR_SUCCESS);
      end;
    finally
      RegCloseKey(hReg);
    end;
  end;

begin
  DeleteRegSub(iAddress);
end;

end.
