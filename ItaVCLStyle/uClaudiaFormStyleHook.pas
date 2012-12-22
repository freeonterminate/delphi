unit uClaudiaFormStyleHook;

interface

uses
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, Vcl.Forms, Vcl.Themes,
  Vcl.Imaging.pngimage;

type
  // クラウディアたんを右下に表示する Style Hook
  TClaudiaFormStyleHook = class(TFormStyleHook)
  private
    FClaudia: TPngImage;
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
  public
    constructor Create(iControl: TWinControl); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes, System.SysUtils;

{ TClaudiaFormStyleHook }

constructor TClaudiaFormStyleHook.Create(iControl: TWinControl);
begin
  inherited;

  // PNG Image を生成
  FClaudia := TPngImage.Create;

  // クラウディアの画像を読み出す
  FClaudia.LoadFromFile(ExtractFilePath(Application.ExeName) + '\Claudia.png');
end;

destructor TClaudiaFormStyleHook.Destroy;
begin
  // PNG Image を破棄
  FClaudia.Free;

  inherited;
end;

// WM_ERASEBKGND が来たときに呼ばれるメソッド
procedure TClaudiaFormStyleHook.PaintBackground(Canvas: TCanvas);
var
  FormCanvas: TCanvas;
  Back: TBitmap;
begin
  // まず親の PaintBackground を呼び、このフォームに乗っている
  // 子コントロール分の背景を描画する
  inherited;

  // 背景用 Bitmap を作る
  Back := TBitmap.Create;
  try
    with Back, Canvas do begin
      // 大きさは、フォームのクライアントエリアと同じ
      SetSize(Control.Width, Control.Height);

      // 背景色で塗りつぶす
      Brush.Color := StyleServices.GetStyleColor(scWindow);
      FillRect(Rect(0, 0, Width, Height));

      // クラウディアを右下に描画する
      Draw(Width - FClaudia.Width, Height - FClaudia.Height, FClaudia);
    end;

    // フォームに描画するための Canvas を作る
    // 引数の Canvas は、TBitmap の Canvas でしかないため
    // Canvas に Draw しても描画されない
    FormCanvas := TCanvas.Create;
    try
      // デバイスコンテキストを取得
      FormCanvas.Handle := GetDC(Control.Handle);
      try
        // 背景用 Bitmap を描画する
        FormCanvas.Draw(0, 0, Back);
      finally
        ReleaseDC(Control.Handle, FormCanvas.Handle);
      end;
    finally
      FormCanvas.Free;
    end;
  finally
    Back.Free;
  end;
end;

initialization
begin
  // まず TFormStyleHook を TCustomForm から外さないと
  // TClaudiaFormStyleHook は呼ばれない
  TCustomStyleEngine.UnregisterStyleHook(TCustomForm, TFormStyleHook);

  // TClaudiaFormStyleHook を TCustomForm の StyleHook として設定
  TCustomStyleEngine.RegisterStyleHook(TCustomForm, TClaudiaFormStyleHook);
end;

finalization
begin
  // TCustomForm はアプリケーションが終わり破棄されるとき TFormStyleHook を
  // UnregisterStyleHook しようとする。
  // そのため、ここで StyleHook に TFormStyleHook を登録してやる。
  // 登録しないと、例外が発生する
  TCustomStyleEngine.RegisterStyleHook(TCustomForm, TFormStyleHook);
end;

end.
