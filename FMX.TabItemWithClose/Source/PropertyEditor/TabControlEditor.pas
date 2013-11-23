unit TabControlEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.TabControl;

type
  TfrmTabControlPropEditor = class(TForm)
    lstItems: TListBox;
    cmbbxItemClasses: TComboBox;
    btnAdd: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
  private type
    TTabItemClass = class of TTabItem;
  private var
    FTabControl: TTabControl;
  private
    function GetCurrentTabItem: TTabItem;
    procedure ChangeIndex(const iDelta: Integer);
  public
    function ShowModal(const iTabControl: TTabControl): TModalResult; overload;
    class procedure RegisterTabItemClasses(
      const iClasses: array of TTabItemClass);
  end;

var
  frmTabControlPropEditor: TfrmTabControlPropEditor;

implementation

uses
  System.Generics.Collections,
  System.Math,
  FMX.TabItemWithClose;

{$R *.fmx}

type
  TOpenTabControl = class(TTabControl);
  TTabItemClasses = TList<TfrmTabControlPropEditor.TTabItemClass>;

var
  GTabItemClasses: TTabItemClasses;

{ TfrmTabControlPropEditor }

procedure TfrmTabControlPropEditor.btnAddClick(Sender: TObject);
var
  Index: Integer;
  ItemClass: TTabItemClass;
  Item: TTabItem;
  NameIndex: Integer;
  Prefix: String;
begin
  Index := cmbbxItemClasses.ItemIndex;
  if
    (not InRange(Index, 0, GTabItemClasses.Count - 1))
    or (FTabControl = nil)
  then
    Exit;

  ItemClass := GTabItemClasses[Index];

  Item := ItemClass.Create(FTabControl.Root.GetObject);
  Item.Parent := FTabControl;

  if (String.IsNullOrEmpty(Item.Name)) then begin
    Prefix := ItemClass.ClassName;
    if (Prefix.StartsWith('T')) then
      Prefix := Prefix.Remove(0, 1);

    NameIndex := 1;
    while (True) do begin
      try
        Item.Name := Prefix + NameIndex.ToString;
      except
        Inc(NameIndex);
        Continue;
      end;

      Break;
    end;
  end;

  lstItems.Items.AddObject(Item.Name, Item);
end;

procedure TfrmTabControlPropEditor.btnDeleteClick(Sender: TObject);
var
  Item: TTabItem;
begin
  Item := GetCurrentTabItem;

  if (Item <> nil) then begin
    Item.Release;
    lstItems.Items.Delete(Index);
  end;
end;

procedure TfrmTabControlPropEditor.btnDownClick(Sender: TObject);
begin
  ChangeIndex(+1);
end;

procedure TfrmTabControlPropEditor.btnUpClick(Sender: TObject);
begin
  ChangeIndex(-1);
end;

procedure TfrmTabControlPropEditor.ChangeIndex(const iDelta: Integer);
var
  Item: TTabItem;
  ActiveTab: TTabItem;
begin
  Item := GetCurrentTabItem;

  if (Item <> nil) then begin
    if
      ((iDelta < 0) and (Item.Index > 0))
      or ((iDelta > 0) and (Item.Index < FTabControl.TabCount))
    then begin
      ActiveTab := FTabControl.ActiveTab;

      lstItems.Items.Delete(Item.Index);
      Item.Index := Item.Index + iDelta;

      lstItems.Items.InsertObject(Item.Index, Item.Name, Item);
      lstItems.ItemIndex := Item.Index;

      TOpenTabControl(FTabControl).RealignTabs;
      FTabControl.ActiveTab := ActiveTab;
    end;
  end;
end;

procedure TfrmTabControlPropEditor.FormCreate(Sender: TObject);
var
  TabItem: TTabItemClass;
begin
  RegisterTabItemClasses([TTabItem, TTabItemWithClose]);

  for TabItem in GTabItemClasses do
    cmbbxItemClasses.Items.Add(TabItem.ClassName);

  cmbbxItemClasses.ItemIndex := 0;
end;

function TfrmTabControlPropEditor.GetCurrentTabItem: TTabItem;
var
  Index: Integer;
begin
  Index := lstItems.ItemIndex;
  if (not InRange(Index, 0, lstItems.Items.Count - 1)) then
    Exit(nil);

  Result := TTabItem(lstItems.Items.Objects[Index]);
end;

class procedure TfrmTabControlPropEditor.RegisterTabItemClasses(
  const iClasses: array of TTabItemClass);
var
  TabItem: TTabItemClass;
begin
  for TabItem in iClasses do begin
    if (not GTabItemClasses.Contains(TabItem)) then
      GTabItemClasses.Add(TabItem);
  end;
end;

function TfrmTabControlPropEditor.ShowModal(
  const iTabControl: TTabControl): TModalResult;
var
  Item: TTabItem;
  i: Integer;
begin
  FTabControl := iTabControl;

  lstItems.Items.Clear;
  for i := 0 to FTabControl.TabCount - 1 do begin
    Item := FTabControl.Tabs[i];
    lstItems.Items.AddObject(Item.Name, Item);
  end;

  Result := ShowModal;
end;

initialization
begin
  GTabItemClasses := TTabItemClasses.Create;
end;

finalization
begin
  GTabItemClasses.DisposeOf;
end;

end.
