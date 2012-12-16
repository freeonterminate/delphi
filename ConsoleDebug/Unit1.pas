unit Unit1;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  uConsole;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure ConsoleEventListener(const iType: TConsoleEventType);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // 文字列の表示
  Writeln('Hello, World !');

  // 数字や Boolean、文字列などを混在して表示できる
  Writeln(123456789, ' ', True);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Str: String;
begin
  // 命令をコンソールから読み取る
  Str := ReadConsole('Input Command: ', True);

  // exit なら終了
  if (Str = 'exit') then
    Close
  // notepad なら「メモ帳」を起動
  else if (Str = 'notepad') then
    WinExec('notepad.exe', SW_SHOW)
  // それ以外なら不明と表示
  else
    Writeln('Unknown command:', Str);
end;

procedure TForm1.ConsoleEventListener(const iType: TConsoleEventType);
begin
  // コンソールイベントの種類をコンソールに表示
  case iType of
    ceC:
      Writeln('Ctrl + C が押されました');

    ceBreak:
      Writeln('Ctrl + BREAK が押されました');

    ceClose:
      Writeln('コンソールが閉じられました');

    ceLogOff:
      Writeln('ログオフされました');

    ceShutdown:
      Writeln('シャットダウンされました');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // コンソールイベントリスナを追加
  AddConsoleEventListener(ConsoleEventListener);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // コンソールイベントリスナを削除
  RemoveConsoleEventListener(ConsoleEventListener);
end;

end.
