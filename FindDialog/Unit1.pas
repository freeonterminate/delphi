unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uFindDialogEx;

type
  TForm1 = class(TForm)
    FindDialogEx1: TFindDialogEx;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FCheckBox: TCheckBox;
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
  FindDialogEx1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (StyleServices.Name = 'Auric') then
    TStyleManager.TrySetStyle('Windows')
  else
    TStyleManager.TrySetStyle('Auric');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCheckBox := TCheckBox.Create(Self);
  FCheckBox.Caption := 'Hello, CommonDialog !';
  FCheckBox.SetBounds(
    7,
    0,
    Canvas.TextWidth(FCheckBox.Caption) * 3 div 2,
    FCheckBox.Height);

  FindDialogEx1.AddControl(FCheckBox);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCheckBox.Free;
end;

end.
