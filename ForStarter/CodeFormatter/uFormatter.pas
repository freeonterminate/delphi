unit uFormatter;

interface

implementation

uses
  ToolsAPI,
  Winapi.Windows,
  System.Classes, System.SysUtils, System.IOUtils,
  Vcl.Menus, Vcl.Dialogs;

type
  TFormatter = class
  private
    function DOSExe(const iCommand: String): String;
    procedure FormatterClick(Sender: TObject);
  end;

var
  Initialized: Boolean = False;
  NTAServices: INTAServices = nil;
  MenuFormatter: TMenuItem = nil;
  Formatter: TFormatter = nil;

{ TFormatter }

function TFormatter.DOSExe(const iCommand: String): String;
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

    if (ReadableByte > 0) then
    begin
      while (ReadFile(ReadHandle, PRawByteString(Buffer)^, Len, Count, nil)) do
      begin
        Data := Data + RawByteString(Copy(Buffer, 1, Count));

        if (Count >= ReadableByte) then
          Break;
      end;

      Result := Result + UTF8ToString(Data);
    end;
  end;

begin
  Result := '';

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
    then
      Exit;

    SetLength(Buffer, 4096);
    Len := Length(Buffer);

    with PI do
    begin
      while (WaitForSingleObject(hProcess, 100) = WAIT_TIMEOUT) do
        ReadResult;

      ReadResult;

      CloseHandle(hProcess);
      CloseHandle(hThread);
    end;
  finally
    CloseHandle(WriteHandle);
    CloseHandle(ReadHandle);
  end;
end;

procedure TFormatter.FormatterClick(Sender: TObject);
const
  EXT_PAS = '.PAS';
  ERR_MSG_INTERFACE = 'Interface の取得に失敗しました';
  ERR_MSG_FORMATTER_PATH = 'Formatter.exe が見つかりません (%S)';
  ERR_MSG_EXECUTE = '実行に失敗しました';
  ERR_MSG_FILE_NOT_FOUND = 'ソースファイルがありません (%S)';
  ERR_MSG_CREATE_TEMPFILE = '一時ファイルの作成に失敗しました';
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  MessageServices: IOTAMessageServices;
  CurrentEditor: IOTAEditor;
  Editor: IOTASourceEditor;
  EditView: IOTAEditView;
  EditPosition: IOTAEditPosition;
  EditBlock: IOTAEditBlock;
  EditWriter: IOTAEditWriter;
  Target: String;
  Formatter: String;
  Res: String;
  Contents: String;
  CurrentLine: Integer;

  function AddQuote(const iName: String): String;
  begin
    Result := AnsiQuotedStr(iName, '"');
  end;

  procedure ShowMsg(const iMsg: String);
  begin
    MessageServices.AddTitleMessage('[Formatter] ' + iMsg);
  end;

begin
  // Message 取得
  if not Supports(BorlandIDEServices, IOTAMessageServices, MessageServices) then
    Exit;

  // Source Editor 取得
  if
    Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) and
    Supports(ModuleServices.CurrentModule, IOTAModule, Module)
  then
  begin
    CurrentEditor := Module.CurrentEditor;

    if
      (CurrentEditor <> nil) and (TFile.Exists(CurrentEditor.FileName)) and
      (ExtractFileExt(CurrentEditor.FileName).ToUpper = EXT_PAS)
    then
      CurrentEditor.QueryInterface(IOTASourceEditor, Editor);
  end;

  if (Editor = nil) or (Editor.EditViewCount < 1) then
  begin
    ShowMsg(ERR_MSG_INTERFACE);
    Exit;
  end;

  EditView := Editor.EditViews[0];
  EditPosition := EditView.Position;
  EditBlock := EditView.Block;
  EditWriter := Editor.CreateUndoableWriter;

  if (EditPosition = nil) or (EditBlock = nil) or (EditWriter = nil) then
  begin
    ShowMsg(ERR_MSG_INTERFACE);
    Exit;
  end;

  // Formatter パス取得
  Formatter := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
    'Formatter.exe');

  if not TFile.Exists(Formatter) then
  begin
    ShowMsg(Format(ERR_MSG_FORMATTER_PATH, [Formatter]));
    Exit;
  end;

  // ソースファイル
  if not TFile.Exists(Editor.FileName) then
  begin
    ShowMsg(Format(ERR_MSG_FILE_NOT_FOUND, [Target]));
    Exit;
  end;

  // 変更格納先テンポラリファイル作成
  Target := TPath.GetTempFileName;
  try
    TFile.Copy(Editor.FileName, Target, True);
  except on E: Exception do
    begin
      ShowMsg(E.ToString);
      Exit;
    end;
  end;

  if not TFile.Exists(Target) then
  begin
    ShowMsg(ERR_MSG_CREATE_TEMPFILE);
    Exit;
  end;

  // 実行
  Res := DOSExe(AddQuote(Formatter) + ' -delphi ' + AddQuote(Target));

  // 変更内容読み込み
  with TStringList.Create do
    try
      LoadFromFile(Target);
      Contents := Text;
    finally
      DisposeOf;
    end;

  // テンポラリ削除
  try
    TFile.Delete(Target);
  except on E: Exception do
    begin
      ShowMsg(E.ToString);
      Exit;
    end;
  end;

  if (Res.IsEmpty) then
  begin
    ShowMsg(ERR_MSG_EXECUTE);
    Exit;
  end;

  MessageServices.AddTitleMessage('[Formatter] ' + Res);

  // リプレース
  CurrentLine := EditPosition.Row;
  EditPosition.GotoLine(1);
  EditWriter.DeleteTo(MaxInt);
  EditWriter.Insert(PAnsiChar(AnsiToUtf8(Contents)));
  EditPosition.GotoLine(CurrentLine);
end;

procedure Initialize;
begin
  if Supports(BorlandIDEServices, INTAServices, NTAServices) then
  begin
    Formatter := TFormatter.Create;

    MenuFormatter :=
      NewItem(
        'ソースの整形',
        TextToShortCut('Ctrl+D'),
        False,
        True,
        Formatter.FormatterClick,
        0,
        'menuCodeFormatter');

    NTAServices.AddActionMenu('ToolsMenu', nil, MenuFormatter, True, True);

    Initialized := True;
  end;
end;

procedure Finalize;
begin
  if Initialized then
  begin
    Formatter.DisposeOf;
    MenuFormatter.DisposeOf;
  end;
end;

initialization
Initialize;

finalization
Finalize;

end.

