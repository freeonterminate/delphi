unit uAddWord;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TfrmExtra = class(TForm)
    btnAddName: TButton;
    btnClearItem: TButton;
    btnApply: TButton;
    lstbxWords: TListBox;
    lblExplain: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnClearItemClick(Sender: TObject);
    procedure btnAddNameClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private

  public
  end;

procedure ShowAddWords(const iList: TStrings);

implementation

uses
  System.UITypes;

{$R *.dfm}

var
  GList: TStrings;

procedure ShowAddWords(const iList: TStrings);
begin
  GList := iList;

  with TfrmExtra.Create(nil) do
    try
      ShowModal;
    finally
      Release;
    end;
end;

procedure TfrmExtra.btnAddNameClick(Sender: TObject);
var
  Word: String;
begin
  if (InputQuery('í«â¡', 'í«â¡ÇµÇΩÇ¢íPåÍÇì¸óÕÇµÇƒÇ≠ÇæÇ≥Ç¢', Word)) then
    lstbxWords.Items.Add(Word);
end;

procedure TfrmExtra.btnApplyClick(Sender: TObject);
begin
  GList.Clear;
  GList.Assign(lstbxWords.Items);
end;

procedure TfrmExtra.btnClearItemClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := lstbxWords.ItemIndex;
  if (Index < 0) or (Index >= lstbxWords.Items.Count) then
    Exit;

  if
    (
      MessageDlg(
        lstbxWords.Items[Index] + 'ÇçÌèúÇµÇ‹Ç∑Ç©ÅH',
        mtConfirmation,
        [mbYes, mbNo],
        0) =
      mrYes
    )
  then
    lstbxWords.Items.Delete(Index);
end;

procedure TfrmExtra.FormCreate(Sender: TObject);
begin
  lstbxWords.Items.Assign(GList);
end;

end.
