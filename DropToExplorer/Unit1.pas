unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Winapi.ActiveX, DragDrop, DropSource, DragDropFile, DropTarget;

type
  TForm1 = class(TForm)
    DataFormatAdapter1: TDataFormatAdapter;
    DropEmptySource1: TDropEmptySource;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FIsLButtonDown: Boolean;
    FIsDragging: Boolean;
    FDragStartPos: TPoint;

    procedure DropExecute;
    procedure OnGetStream(
      Sender: TFileContentsStreamOnDemandClipboardFormat;
      Index: integer;
      out AStream: IStream);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
begin
  FIsLButtonDown := True;
  FIsDragging := False;
  FDragStartPos := Mouse.CursorPos;
end;

procedure TForm1.FormMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer);
var
  Pos: TPoint;
begin
  if (not FIsLButtonDown) or (FIsDragging) then
    Exit;

  Pos := Mouse.CursorPos;
  Pos := Pos - FDragStartPos;

  if
    (Abs(Pos.X) < Mouse.DragThreshold) and
    (Abs(Pos.Y) < Mouse.DragThreshold)
  then
    Exit;

  FIsDragging := True;

  DropExecute;
end;

procedure TForm1.FormMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
begin
  FIsLButtonDown := False;
  FIsDragging := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // イベントハンドラを設定する
  (DataFormatAdapter1.DataFormat as TVirtualFileStreamDataFormat).OnGetStream
    := OnGetStream;
end;

procedure TForm1.OnGetStream(
  Sender: TFileContentsStreamOnDemandClipboardFormat;
  Index: integer;
  out AStream: IStream);
var
  Data: TVirtualFileStreamDataFormat;
begin
  // ここは非同期で別スレッドから呼ばれるので、
  // ここでファイルのダウンロードなどをし、ファイルを一旦生成する
  // 生成した一時ファイルは TDropEmptySource.OnAfterDrop イベントなどの
  // タイミングで適宜削除する

  // 追加しておいたファイルを取り出す
  Data := TVirtualFileStreamDataFormat(DataFormatAdapter1.DataFormat);

  // 追加してある何番目のファイルかが Index に入っている
  if (Index < Data.FileNames.Count) then
    AStream :=
      // TStreamAdapter を使うと IStream の実装が得られる
      TStreamAdapter.Create(
        TFileStream.Create(
          Data.FileNames[Index],
          fmOpenRead or fmShareDenyNone),
        soOwned
      );
end;

procedure TForm1.DropExecute;
var
  Data: TVirtualFileStreamDataFormat;
begin
  // OnMouseMove から呼ばれる（煩雑になるため機能を分けた）

  // TVirtualFileStreamDataFormat.FileNames にコピーするファイル名を追加する
  Data := TVirtualFileStreamDataFormat(DataFormatAdapter1.DataFormat);
  Data.FileNames.Clear;

  // 今回は自分をコピーしてみる
  Data.FileNames.Add(Application.ExeName);

  if (Data.FileNames.Count > 0) then
    DropEmptySource1.Execute(True); // True を渡すと非同期で実行する
end;

end.
