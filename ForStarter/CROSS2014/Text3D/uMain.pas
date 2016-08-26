unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls3D, FMX.Layers3D, FMX.Viewport3D, FMX.Edit, FMX.Layouts,
  System.Math.Vectors, FMX.Ani, FMX.Controls.Presentation;

type
  TfrmMain = class(TForm)
    viewport3D: TViewport3D;
    pnlTrackbarBase: TPanel;
    trackbarRotation: TTrackBar;
    btnExec: TButton;
    layer3D: TLayer3D;
    edtSample: TEdit;
    layoutButtonBase: TLayout;
    animFont: TFloatAnimation;
    animOpacity: TFloatAnimation;
    animLayer3D: TFloatAnimation;
    StyleBook1: TStyleBook;
    procedure trackbarRotationTracking(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.btnExecClick(Sender: TObject);

  procedure ChangeEditProp(const iFontSize, iOpacity: Single);
  begin
    animFont.StartValue := animFont.StopValue;
    animFont.StopValue := iFontSize;
    animFont.Start;

    animOpacity.StartValue := animOpacity.StopValue;
    animOpacity.StopValue := iOpacity;
    animOpacity.Start;
  end;

begin
  if (edtSample.Opacity = 1) then
    ChangeEditProp(0, 0)
  else
    ChangeEditProp(48, 1);

  animLayer3D.Start;
  trackbarRotation.Value := 0;
end;

procedure TfrmMain.trackbarRotationTracking(Sender: TObject);
begin
  Layer3D.RotationAngle.Y := trackbarRotation.Value;
end;

end.
