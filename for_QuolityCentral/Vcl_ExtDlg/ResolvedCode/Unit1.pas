unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Themes, uStyleUtils;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenTextFileDialog1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClearStyles(OpenTextFileDialog1);

  Memo1.Font.Color := StyleServices.GetStyleFontColor(sfWindowTextNormal);
  Memo1.Color := StyleServices.GetStyleColor(scWindow);
end;

end.
