unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uFindDialogEx;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFindDialogEx: TFindDialogEx;
    procedure ChangeMemoColor;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FFindDialogEx.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (StyleServices.Name = 'Auric') then
    TStyleManager.TrySetStyle('Windows')
  else
    TStyleManager.TrySetStyle('Auric');

  ChangeMemoColor;
end;

procedure TForm1.ChangeMemoColor;
begin
  Memo1.Font.Color := StyleServices.GetStyleFontColor(sfWindowTextNormal);
  Memo1.Color := StyleServices.GetStyleColor(scWindow);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFindDialogEx := TFindDialogEx.Create(Self);

  ChangeMemoColor;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFindDialogEx.Free;
end;

end.
