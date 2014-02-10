program Project1;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uJudge in 'uJudge.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
