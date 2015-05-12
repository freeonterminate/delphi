unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FStartPos: TPointF;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math;

{$R *.fmx}

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if (Key = vkEscape) then
    Close;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FStartPos := TPointF.Create(X, Y);

  Rectangle1.SetBounds(X, Y, 1, 1);
  Rectangle1.Visible := True;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
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

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Rectangle1.Visible := False;
end;

end.
