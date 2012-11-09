unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Button1: TButton;
    Image1: TImage;
    Memo1: TMemo;
    Button2: TButton;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  RichEdit1.Lines.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RichEdit1.Color := StyleServices.GetStyleColor(scWindow);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Font.Color := StyleServices.GetStyleFontColor(sfWindowTextNormal);
  Memo1.Color := StyleServices.GetStyleColor(scWindow);
end;

end.
