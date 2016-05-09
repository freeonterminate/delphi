unit uExecCMD;

interface

uses
  System.SysUtils;

type
  TExecCMDLineProc = reference to procedure(const iLine: String);

procedure ExecCMD(const iCommand: String; const iProc: TExecCMDLineProc);

implementation

uses
  Winapi.Windows;

procedure ExecCMD(const iCommand: String; const iProc: TExecCMDLineProc);
var
  ReadHandle, WriteHandle: THandle;
  SA: TSecurityAttributes;
  SI: TStartUpInfo;
  PI: TProcessInformation;
  Buffer: RawByteString;
  Len: Cardinal;

  procedure ReadResult;
  var
    Count: DWORD;
    ReadableByte: DWORD;
    Data: RawByteString;
  begin
    ZeroMemory(PRawByteString(Buffer), Len);
    PeekNamedPipe(ReadHandle, PRawByteString(Buffer), Len, nil, nil, nil);
    ReadableByte := Length(Trim(String(Buffer)));

    if (ReadableByte > 0) then begin
      while
        (ReadFile(ReadHandle, PRawByteString(Buffer)^, Len, Count, nil))
      do begin
        Data := Data + RawByteString(Copy(Buffer, 1, Count));

        if (Count >= ReadableByte) then
          Break;
      end;

      if (Assigned(iProc)) then
        iProc(String(Data));
    end;
  end;

begin
  ZeroMemory(@SA, SizeOf(SA));
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;

  CreatePipe(ReadHandle, WriteHandle, @SA, 0);
  try
    ZeroMemory(@SI, SizeOf(SI));
    with SI do
    begin
      cb := SizeOf(SI);
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := SW_HIDE;
      hStdOutput := WriteHandle;
      hStdError := WriteHandle;
    end;

    if
      (
        not CreateProcess(
          nil,
          PChar(iCommand),
          nil,
          nil,
          True,
          0,
          nil,
          nil,
          SI,
          PI)
      )
    then
      Exit;

    SetLength(Buffer, 4096);
    Len := Length(Buffer);

    with PI do
    begin
      while (WaitForSingleObject(hProcess, 100) = WAIT_TIMEOUT) do
      begin
        ReadResult;
      end;

      ReadResult;

      CloseHandle(hProcess);
      CloseHandle(hThread);
    end;
  finally
    CloseHandle(WriteHandle);
    CloseHandle(ReadHandle);
  end;
end;

end.
