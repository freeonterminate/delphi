unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, uJudge;

type
  TForm1 = class(TForm)
    imgGoo: TImage;
    imgChoki: TImage;
    imgPar: TImage;
    lblUser: TLabel;
    StyleBook1: TStyleBook;
    effectGlowChoki: TGlowEffect;
    effectGlowGoo: TGlowEffect;
    effectGlowPar: TGlowEffect;
    imgPCGoo: TImage;
    effectGlowPCGoo: TGlowEffect;
    imgPCChoki: TImage;
    effectGlowPCChoki: TGlowEffect;
    imgPCPar: TImage;
    effectGlowPCPar: TGlowEffect;
    lblPC: TLabel;
    lblWin: TLabel;
    effectGlowWin: TGlowEffect;
    lblLose: TLabel;
    effectGlowLose: TGlowEffect;
    pnlDisabler: TPanel;
    btnRetry: TButton;
    lblDraw: TLabel;
    effectGlowDraw: TGlowEffect;
    Timer1: TTimer;
    procedure imageEnter(Sender: TObject);
    procedure imageLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imageClick(Sender: TObject);
    procedure btnRetryClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private const
    SCALE_BY = 1.2;
    DISABLE_OPACITY = 0.2;
    RESULT_ROTATION = 30;
    RESULT_DURATION = 0.3;
  private
    FNormalPos: array [TJanken] of TPointF;
    FScaledPos: array [TJanken] of TPointF;
    FPC: Integer;
    FUser: Integer;
    FClickedImage: TImage;
    procedure InitView;
    procedure InitResultView;
    procedure ResetImage;
    procedure ShowResult(const iResult: TJankenResult);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnRetryClick(Sender: TObject);
begin
  InitView;
end;

procedure TForm1.FormCreate(Sender: TObject);

  procedure SetImagePos(const iImage: TImage);
  var
    Pos: TPointF;
  begin
    Pos := TPointF.Create(iImage.Position.X, iImage.Position.Y);

    FNormalPos[IntToJanken(iImage.Tag)] := Pos;

    Pos.X := Pos.X - iImage.Width * (SCALE_BY - 1) / 2;
    Pos.Y := Pos.Y - iImage.Height * (SCALE_BY - 1) / 2;

    FScaledPos[IntToJanken(iImage.Tag)] := Pos;
  end;

begin
  Randomize;

  SetImagePos(imgGoo);
  SetImagePos(imgChoki);
  SetImagePos(imgPar);

  InitView;
end;

procedure TForm1.imageEnter(Sender: TObject);
begin
  if not (Sender is TImage) then
    Exit;

  FClickedImage := TImage(Sender);

  FClickedImage.Scale.X := SCALE_BY;
  FClickedImage.Scale.Y := SCALE_BY;
  FClickedImage.Position.X := FScaledPos[IntToJanken(FClickedImage.Tag)].X;
  FClickedImage.Position.Y := FScaledPos[IntToJanken(FClickedImage.Tag)].Y;
end;

procedure TForm1.imageLeave(Sender: TObject);
begin
  if not (Sender is TImage) then
    Exit;

  ResetImage;
end;

procedure TForm1.InitResultView;
begin
  lblLose.Visible := False;
  lblDraw.Visible := False;
  lblWin.Visible := False;
end;

procedure TForm1.InitView;

  procedure SetImageProp(const iImage: TImage);
  begin
    iImage.Opacity := 1;
  end;

  procedure SetImagePCProp(const iImage: TImage);
  begin
    iImage.Opacity := DISABLE_OPACITY;
    iImage.Scale.X := 1;
    iImage.Scale.Y := 1;
  end;

begin
  pnlDisabler.Visible := False;
  btnRetry.Visible := False;

  SetImageProp(imgGoo);
  SetImageProp(imgChoki);
  SetImageProp(imgPar);

  SetImagePCProp(imgPCGoo);
  SetImagePCProp(imgPCChoki);
  SetImagePCProp(imgPCPar);

  InitResultView;
end;

procedure TForm1.ResetImage;
begin
  if (FClickedImage = nil) then
    Exit;

  FClickedImage.Scale.X := 1;
  FClickedImage.Scale.Y := 1;
  FClickedImage.Position.X := FNormalPos[IntToJanken(FClickedImage.Tag)].X;
  FClickedImage.Position.Y := FNormalPos[IntToJanken(FClickedImage.Tag)].Y;

  FClickedImage := nil;
end;

procedure TForm1.ShowResult(const iResult: TJankenResult);
var
  LabelResult: TLabel;
begin
  InitResultView;
  ResetImage;

  case iResult of
    jrLose:
      LabelResult := lblLose;

    jrDraw:
      LabelResult := lblDraw;

    else
      LabelResult := lblWin;
  end;

  pnlDisabler.Visible := True;
  btnRetry.Visible := True;

  LabelResult.Visible := True;
  LabelResult.RotationAngle := RESULT_ROTATION;
  LabelResult.AnimateFloat(
    'RotationAngle', 360 - LabelResult.RotationAngle, RESULT_DURATION);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  ShowResult(Judge(FUser, FPC));
end;

procedure TForm1.imageClick(Sender: TObject);
var
  ImagePC: TImage;

  procedure SetAnimation(const iPropName: String);
  begin
    ImagePC.AnimateFloat(
      iPropName, 1, 0.5, TAnimationType.atIn, TInterpolationType.itBounce);
  end;

begin
  imgGoo.Opacity := DISABLE_OPACITY;
  imgChoki.Opacity := imgGoo.Opacity;
  imgPar.Opacity := imgGoo.Opacity;

  TImage(Sender).Opacity := 1;
  FUser := TImage(Sender).Tag;

  case Random(3) of
    0:
      ImagePC := imgPCGoo;
    1:
      ImagePC := imgPCChoki;
    2:
      ImagePC := imgPCPar;
  end;

  ImagePC.Opacity := 0;

  FPC := ImagePC.Tag;
  ImagePC.RotationAngle := 0;
  ImagePC.Scale.X := 1;
  ImagePC.Scale.Y := 1;
  SetAnimation('Opacity');

  Timer1.Enabled := True;
end;

end.
