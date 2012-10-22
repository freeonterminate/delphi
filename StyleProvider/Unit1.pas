unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uStyleProvider, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    StyleProvider: TStyleProvider;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Name: String;
begin
  StyleProvider :=
    TStyleProvider.Create(
      ExtractFilePath(Application.ExeName) + '\Redist\styles');

  for Name in StyleProvider do
    ListBox1.Items.Add(Name);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBox1.ItemIndex;

  if (Index > -1) and (Index < ListBox1.Items.Count) then
    StyleProvider.ApplyByName(ListBox1.Items[Index]);
end;

end.
