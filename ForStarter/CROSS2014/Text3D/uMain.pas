unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls3D, FMX.Layers3D, FMX.Viewport3D, FMX.Edit, FMX.Layouts,
  System.Math.Vectors, FMX.Ani, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    viewport3D: TViewport3D;
    pnlTrackbarBase: TPanel;
    trackbarRotation: TTrackBar;
    btnExec: TButton;
    layer3D: TLayer3D;
    edtSample: TEdit;
    StyleBook1: TStyleBook;
    layoutButtonBase: TLayout;
    animFont: TFloatAnimation;
    animOpacity: TFloatAnimation;
    animLayer3D: TFloatAnimation;
    procedure FormResize(Sender: TObject);
    procedure trackbarRotationTracking(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  TOpenLayer3D = class(TLayer3D);

procedure TForm1.btnExecClick(Sender: TObject);

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

procedure TForm1.FormResize(Sender: TObject);
begin
  TOpenLayer3D(Layer3D).Resize3D;
end;

procedure TForm1.trackbarRotationTracking(Sender: TObject);
begin
  Layer3D.RotationAngle.Y := trackbarRotation.Value;
end;

end.
