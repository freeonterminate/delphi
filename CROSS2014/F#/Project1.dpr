program Project1;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uOrderList in 'uOrderList.pas',
  uDynStringArray in 'uDynStringArray.pas',
  uDynArrayUtils in 'uDynArrayUtils.pas',
  uOrderListAverage in 'uOrderListAverage.pas',
  uOrderListToppingCount in 'uOrderListToppingCount.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
