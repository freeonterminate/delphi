program SDKTransformAssitant;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {frmMain},
  uExecCMD in 'uExecCMD.pas',
  uGetEnvironmentVariables in 'uGetEnvironmentVariables.pas',
  uCautionForm in 'uCautionForm.pas' {frmCaution},
  uRegistoryUtils in 'uRegistoryUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
