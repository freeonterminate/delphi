unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, uJudge, FMX.Controls.Presentation, FMX.Ani;

type
  TfrmMain = class(TForm)
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
    animPC: TFloatAnimation;
    animResult: TFloatAnimation;
    procedure imageEnter(Sender: TObject);
    procedure imageLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imageClick(Sender: TObject);
    procedure btnRetryClick(Sender: TObject);
    procedure animPCFinish(Sender: TObject);
  private const
    SCALE_BY = 1.2;
    DISABLE_OPACITY = 0.2;
  private var
    FNormalPos: array [TJanken] of TPointF;
    FScaledPos: array [TJanken] of TPointF;
    FPC: Integer;
    FUser: Integer;
    FClickedImage: TImage;
  private
    procedure InitView;
    procedure InitResultView;
    procedure ResetImage;
    procedure ShowResult(const iResult: TJankenResult);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.animPCFinish(Sender: TObject);
begin
  ShowResult(Judge(FUser, FPC));
end;

procedure TfrmMain.btnRetryClick(Sender: TObject);
begin
  InitView;
end;

procedure TfrmMain.FormCreate(Sender: TObject);

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

procedure TfrmMain.imageEnter(Sender: TObject);
begin
  if not (Sender is TImage) then
    Exit;

  FClickedImage := TImage(Sender);

  FClickedImage.Scale.X := SCALE_BY;
  FClickedImage.Scale.Y := SCALE_BY;
  FClickedImage.Position.X := FScaledPos[IntToJanken(FClickedImage.Tag)].X;
  FClickedImage.Position.Y := FScaledPos[IntToJanken(FClickedImage.Tag)].Y;
end;

procedure TfrmMain.imageLeave(Sender: TObject);
begin
  if not (Sender is TImage) then
    Exit;

  ResetImage;
end;

procedure TfrmMain.InitResultView;
begin
  lblLose.Visible := False;
  lblDraw.Visible := False;
  lblWin.Visible := False;
end;

procedure TfrmMain.InitView;

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

procedure TfrmMain.ResetImage;
begin
  if (FClickedImage = nil) then
    Exit;

  FClickedImage.Scale.X := 1;
  FClickedImage.Scale.Y := 1;
  FClickedImage.Position.X := FNormalPos[IntToJanken(FClickedImage.Tag)].X;
  FClickedImage.Position.Y := FNormalPos[IntToJanken(FClickedImage.Tag)].Y;

  FClickedImage := nil;
end;

procedure TfrmMain.ShowResult(const iResult: TJankenResult);
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
  LabelResult.AddObject(animResult);
  animResult.Start;
end;

procedure TfrmMain.imageClick(Sender: TObject);
var
  ImagePC: TImage;

  procedure SetOpacity(const iImage: TImage);
  begin
    if iImage = Sender then
      iImage.Opacity := 1
    else
      iIMage.Opacity := DISABLE_OPACITY;
  end;

begin
  SetOpacity(imgGoo);
  SetOpacity(imgChoki);
  SetOpacity(imgPar);

  FUser := TImage(Sender).Tag;

  ImagePC := imgPCGoo; // åxçêÇÊÇØ
  case Random(3) of
    1: ImagePC := imgPCChoki;
    2: ImagePC := imgPCPar;
  end;

  FPC := ImagePC.Tag;

  ImagePC.Opacity := 0;
  ImagePC.RotationAngle := 0;
  ImagePC.Scale.X := 1;
  ImagePC.Scale.Y := 1;
  ImagePC.AddObject(animPC);
  animPC.Start;
end;

end.
