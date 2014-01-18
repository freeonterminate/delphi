unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls3D, FMX.Layers3D, FMX.Viewport3D, FMX.Edit, FMX.Layouts;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    Button1: TButton;
    Layer3D1: TLayer3D;
    Edit1: TEdit;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    procedure TrackBar1Tracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private êÈåæ }
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);

  procedure ChangeEditProp(const iFontSize, iOpacity: Single);
  begin
    Edit1.AnimateFloat('Font.Size', iFontSize, 1);
    Edit1.AnimateFloat('Opacity', iOpacity, 1);
  end;

begin
  if (Edit1.Opacity = 1) then
    ChangeEditProp(0, 0)
  else
    ChangeEditProp(48, 1);

  Layer3D1.AnimateFloat('RotationAngle.Y', 360, 1);
  TrackBar1.Value := 0;
end;

type
  TOpenLayer3D = class(TLayer3D);

procedure TForm1.FormResize(Sender: TObject);
begin
  TOpenLayer3D(Layer3D1).Resize3D;
end;

procedure TForm1.TrackBar1Tracking(Sender: TObject);
begin
  Layer3D1.RotationAngle.Y := TrackBar1.Value;
end;

end.
