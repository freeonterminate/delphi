program AdminCMD;

// 自分を表示したくないので指定しない
//{$APPTYPE CONSOLE}

uses
  Winapi.Windows, Winapi.ShellApi, System.SysUtils;

// 管理者権限で実行する
function RunAsAdmin(const iExeName, iParam: String): Boolean;
var
  SEI: TShellExecuteInfo;
begin
  Result := False;

  // runas は、Vista 以降のみ動作する
  if (CheckWin32Version(6)) then begin
    ZeroMemory(@SEI, SizeOf(SEI));

    with SEI do begin
      cbSize := SizeOf(SEI);
      Wnd := 0;
      fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
      lpVerb := 'runas';
      lpFile := PChar(iExeName);
      lpParameters := PChar(iParam);
      nShow := SW_SHOW;
    end;

    Result := ShellExecuteEx(@SEI);
  end;
end;

var
  CmdPath: String;
begin
  // 環境変数から CMD.exe のパスを取得する
  CmdPath := StringOfChar(#0, MAX_PATH);
  ExpandEnvironmentStrings(
    PChar('%ComSpec%'),
    PChar(CmdPath),
    Length(CmdPath));

  CmdPath := Trim(CmdPath);

  // 管理者権限で実行
  RunAsAdmin(CmdPath, '');
end.
