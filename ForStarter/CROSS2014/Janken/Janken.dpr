program Janken;

uses
  FMX.Forms,
  uMain in 'uMain.pas' {frmMain},
  uJudge in 'uJudge.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
