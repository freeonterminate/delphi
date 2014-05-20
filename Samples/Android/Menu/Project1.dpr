program Project1;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FMX.PopupHelper in 'FMX.PopupHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
