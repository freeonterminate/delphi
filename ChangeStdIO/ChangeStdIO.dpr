program ChangeStdIO;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, Winapi.Windows;

function Exec(const iCommand, iParam: String): String;
var
  ReadHandle, WriteHandle: THandle;
  SA: TSecurityAttributes;
  SI: TStartUpInfo;
  PI: TProcessInformation;
  Buffer: RawByteString;
  Len: Cardinal;

  // パイプから値を読み出す
  procedure ReadResult;
  var
    Count: DWORD;
    ReadableByte: DWORD;
    Data: RawByteString;
  begin
    // 読み出しバッファをクリア
    ZeroMemory(PRawByteString(Buffer), Len);

    // パイプに読み出せるバイト数がいくつあるのか調べる
    PeekNamedPipe(ReadHandle, PRawByteString(Buffer), Len, nil, nil, nil);
    ReadableByte := Length(Trim(String(Buffer)));

    // 読み込める文字列があるなら
    if (ReadableByte > 0) then begin
      while
        (ReadFile(ReadHandle, PRawByteString(Buffer)^, Len, Count, nil))
      do begin
        Data := Data + RawByteString(Copy(Buffer, 1, Count));

        if (Count >= ReadableByte) then
          Break;
      end;

      Result := Result + Data;
    end;
  end;

begin
  Result := '';

  ZeroMemory(@SA, SizeOf(SA));
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;

  // パイプを作る
  CreatePipe(ReadHandle, WriteHandle, @SA, 0);
  try
    // StartInfo を初期化
    ZeroMemory(@SI, SizeOf(SI));
    with SI do begin
      cb := SizeOf(SI);
      dwFlags := STARTF_USESTDHANDLES; // 標準入出力ハンドルを使います！宣言
      hStdOutput := WriteHandle;       // 標準出力を出力パイプに変更
      hStdError := WriteHandle;        // 標準エラー出力を出力パイプに変更
    end;

    // プロセスを作成
    if (not CreateProcess(
      PChar(iCommand),
      PChar(iParam),
      nil,
      nil,
      True,
      0,
      nil,
      nil,
      SI,
      PI))
    then
      Exit;

    // 読み出しバッファを 4096[byte] 確保
    SetLength(Buffer, 4096);
    Len := Length(Buffer);

    with PI do begin
      // プロセスが終了するまで、パイプを読み出す
      while (WaitForSingleObject(hProcess, 100) = WAIT_TIMEOUT) do
        ReadResult;

      ReadResult;

      // プロセスを閉じる
      CloseHandle(hProcess);
      CloseHandle(hThread);
    end;
  finally
    // パイプを閉じる
    CloseHandle(WriteHandle);
    CloseHandle(ReadHandle);
  end;
end;

begin
  // dir の結果を出力
  Writeln(Exec('C:\Windows\System32\CMD.exe', '/C dir'));
  Readln;
end.
