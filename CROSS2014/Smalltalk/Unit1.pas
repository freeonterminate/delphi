unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    Button1: TButton;
    CalloutPanel1: TCalloutPanel;
    Image1: TImage;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure CountUp;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CountUp;
end;

procedure TForm1.CountUp;
var
  i: Integer;
  Count: Integer;

  procedure CountUpSub(const iObj: TObject);
  var
    i: Integer;
  begin
    Inc(Count);

    Memo1.Lines.Add(iObj.ClassName);

    if (iObj is TFmxObject) then
      for i := 0 to TFmxObject(iObj).ChildrenCount - 1 do
        CountUpSub(TFmxObject(iObj).Children[i]);
  end;

begin
  Count := 0;
  Memo1.Lines.Clear;

  for i := 0 to Screen.FormCount - 1 do
    CountUpSub(Screen.Forms[i]);

  Memo1.Lines.Insert(0, '--------------------');
  Memo1.Lines.Insert(0, Count.ToString + 'ŒÂ');
end;

end.
