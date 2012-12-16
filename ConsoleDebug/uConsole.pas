unit uConsole;

interface

type
  // コンソールイベント
  TConsoleEventType = (
    ceC,       // CTRL + C が押された
    ceBreak,   // CTRL + BREAK が押された
    ceClose,   // コンソールウィンドウが閉じられた
    ceLogOff,  // ログオフされた
    ceShutdown // シャットダウンされた
  );

  // コンソールイベント型
  TConsoleEvent = procedure(const iType: TConsoleEventType) of object;

// コンソールイベントを受け取るリスナーを追加・解除
procedure AddConsoleEventListener(const iListener: TConsoleEvent);
procedure RemoveConsoleEventListener(const iListener: TConsoleEvent);

// コンソールから文字列を読み取る
function ReadConsole(
  const iPrompt: String = '';
  const iToLower: Boolean = False): String;

implementation

uses
  Winapi.Windows,  Generics.Collections, System.SysUtils;

var
  // コンソールウィンドウのハンドル
  GWnd: HWND;
  // イベントリスナを管理するリスト
  GListeners: TList<TConsoleEvent>;

// コンソールイベントリスナを追加
procedure AddConsoleEventListener(const iListener: TConsoleEvent);
begin
  if (GListeners.IndexOf(iListener) < 0) then
    GListeners.Add(iListener);
end;

// コンソールイベントリスナを削除
procedure RemoveConsoleEventListener(const iListener: TConsoleEvent);
begin
  if (GListeners.IndexOf(iListener) > -1) then
    GListeners.Remove(iListener);
end;

// コンソールから文字列を読み取る
// iPrompt  読み取り前に表示する文字列（ex. 'Please input your name: '）
// iToLower 読み取った文字列を小文字にするなら True
function ReadConsole(
  const iPrompt: String = '';
  const iToLower: Boolean = False): String;
begin
  // プロンプトの表示
  if (iPrompt <> '') then
    Write(iPrompt);

  // コンソールに入力フォーカスを与える
  ShowWindow(GWnd, SW_SHOW);
  SetForegroundWindow(GWnd);

  // 読み込む
  Readln(Result);

  // 小文字化
  if (iToLower) then
    Result := LowerCase(Result);
end;

// コンソールイベントが起きたときに呼ばれる関数
function HandlerRoutine(dwCtrlType: DWORD): BOOL; stdcall;
var
  Listener: TConsoleEvent;
begin
  Result := True; // False の場合、イベントは OS が適切に処理する
                  //（ex. CTRL + C が押されたらアプリケーションを終了させる）
                  // True の場合、OS は何もしない

  for Listener in GListeners do
    Listener(TConsoleEventType(dwCtrlType));
end;

// コンソールの初期設定
// コンソールの Window Handle の特定
// コンソールのタイトルの設定
procedure InitConsole;
var
  Cap: String;
begin
  // Window Caption に GUID を設定する
  Cap := TGUID.NewGuid.ToString;
  SetConsoleTitle(PWideChar(Cap));

  Sleep(40); // Caption が確実に設定されるために 40[msec] 待つ
             // http://support.microsoft.com/kb/124103/ja

  // GUID でウィンドウを探す
  GWnd := FindWindow(nil, PChar(Cap));

  if (GWnd <> 0) then
    // 見つけたらスタイルから System Menu を外す
    //（コンソールを勝手に閉じられないようにするため）
    SetWindowLong(
      GWnd,
      GWL_STYLE,
      GetWindowLong(GWnd, GWL_STYLE) and not WS_SYSMENU);

  // コンソールのタイトルをアプリケーションのパスにする
  SetConsoleTitle(PWideChar(ParamStr(0)));
end;

// 初期化
initialization
begin
  // イベントハンドラ管理用リストの生成
  GListeners := TList<TConsoleEvent>.Create;

  // アプリケーションにコンソールを割り当てる
  AllocConsole;

  // コンソールイベントのハンドラを設定する
  SetConsoleCtrlHandler(@HandlerRoutine, True);

  // コンソールの初期設定
  InitConsole;
end;

// 終了処理
finalization
begin
  // コンソールイベントのハンドラを解除
  SetConsoleCtrlHandler(@HandlerRoutine, False);

  // 割り当て済みのコンソールを解除
  FreeConsole;

  // イベントハンドラ管理用リストの破棄
  GListeners.Free;
end;

end.
