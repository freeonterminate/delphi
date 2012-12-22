program Project1;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Unit1 in 'Unit1.pas' {Form1},
  uClaudiaFormStyleHook;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Auric');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
