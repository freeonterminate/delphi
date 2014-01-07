unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private éŒ¾ }
  public
    { public éŒ¾ }
  end;

var
  Form1: TForm1;

implementation

uses
  FMX.FormHelper;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DisableAltEnter;
  SetMinSize(300, 200);
end;

end.
