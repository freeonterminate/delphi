program SUOIVEX;

uses
  FMX.Forms,
  frmuSUOIVEX in 'frmuSUOIVEX.pas' {frmSUOIVEX};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSUOIVEX, frmSUOIVEX);
  Application.Run;
end.
