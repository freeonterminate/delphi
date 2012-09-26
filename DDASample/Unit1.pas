unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormDestroy(Sender: TObject);
  private
    FBmp: TBitmap;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  uDDASample;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  BmpData: TBitmapData;
begin
  FBmp := TBitmap.Create(320, 240);

  // 描画開始
  FBmp.Canvas.BeginScene;

  // DrawLine による描画  左上→右下　＼
  FBmp.Canvas.Stroke.Color := $ff0000ff;
  FBmp.Canvas.DrawLine(
    TPointF.Create(0, 0),                    // 始点の X, Y（左上）
    TPointF.Create(FBmp.Width, FBmp.Height), // 終点の X, Y（右下）
    1);

  // DDA による描画       左下→右上　／
  if (FBmp.Map(TMapAccess.maWrite, BmpData)) then
    try
      DDA(
        FBmp.Width - 1, 0,   // 始点の X, Y（左下）
        0, FBmp.Height - 1,  // 終点の X, Y（右上）
        procedure (const iX, iY: Integer; const iData: Pointer)
        begin
          PAlphaColorArray(iData)[iY * FBmp.Width + iX] := $ffff0000;
        end,
        BmpData.Data);
    finally
      FBmp.Unmap(BmpData);
    end;

  // 描画終了
  FBmp.Canvas.EndScene;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
end;

procedure TForm1.FormPaint(
  Sender: TObject;
  Canvas: TCanvas;
  const ARect: TRectF);
var
  W, H: Integer;
begin
  W := FBmp.Width;
  H := FBmp.Height;

  Canvas.DrawBitmap(
    FBmp,
    TRectF.Create(0, 0, W, H),
    TRectF.Create(0, 0, W, H),
    1);
end;

end.
