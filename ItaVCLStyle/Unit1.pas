unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
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
  TStyleManager.LoadFromFile('C:\Source\Delphi\for_blog\2012-12-22-ItaStyle\Claudia.vsf');
  TStyleManager.TrySetStyle('Claudia');
end;

end.
