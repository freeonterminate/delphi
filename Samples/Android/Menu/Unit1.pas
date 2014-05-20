unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.StdCtrls, FMX.Effects, FMX.Menus;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Popup1: TPopup;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ShadowEffect1: TShadowEffect;
    StyleBook1: TStyleBook;
    Button1: TButton;
    procedure ListBoxItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  FMX.LateExecuter, FMX.Log, FMX.PopupHelper;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Popup 時に ListBox の選択状態を解除
  ListBox1.ItemIndex := -1;

  // 疑似メニューを表示
  Popup1.Toggle(Button1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 起動時にスタイルをセットしておかないと、Popup した時に乱れる
  ListBox1.ApplyStyleLookup;
end;

procedure TForm1.ListBoxItemClick(Sender: TObject);
begin
  // ListBox の選択色を見せたいので、100msec 後に閉じる
  LateExec(
    procedure
    begin
      Popup1.IsOpen := False;
    end,
    100
  );

  // Popup が閉じた後にダイアログを表示しないと、Popup があるまま表示される
  LateExec(
    procedure
    begin
      ShowMessage((Sender as TControl).Name + ' cliecked !');
    end,
    200
  );
end;

end.
