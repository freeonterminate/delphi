unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, System.Math;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    FStartPos: TPointF;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Macintosh.fmx MACOS}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetBounds(0, 0, Screen.Width, Screen.Height);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkEscape) then
    Close;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FStartPos := TPointF.Create(X, Y);

  Rectangle1.SetBounds(X, Y, 1, 1);
  Rectangle1.Visible := True;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  MinX, MinY, MaxX, MaxY: Single;

  procedure GetMinMax(var oMin, oMax: Single; const iValues: array of Single);
  begin
    oMin := MinValue(iValues);
    oMax := MaxValue(iValues);
  end;

begin
  if (not Rectangle1.Visible) then
    Exit;

  GetMinMax(MinX, MaxX, [FStartPos.X, X]);
  GetMinMax(MinY, MaxY, [FStartPos.Y, Y]);

  Rectangle1.SetBounds(MinX, MinY, MaxX - MinX, MaxY - MinY);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Rectangle1.Visible := False;
end;

end.
