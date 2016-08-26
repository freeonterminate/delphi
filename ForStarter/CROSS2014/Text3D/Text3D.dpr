program Text3D;

uses
  FMX.Forms,
  uMain in 'uMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
