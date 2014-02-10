unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid, FMX.Memo, FMX.Layouts, FMX.ListBox, FMX.Objects,
  System.Generics.Collections, uOrderList;

type
  TForm1 = class(TForm)
    gridOrders: TStringGrid;
    memoOrders: TMemo;
    Label1: TLabel;
    columnOrderId: TStringColumn;
    columnCurryMenu: TStringColumn;
    columnCategory: TStringColumn;
    columnRiceWeight: TStringColumn;
    columnHotFlavor: TStringColumn;
    columnToppings: TStringColumn;
    btnSet: TButton;
    btnQ1: TButton;
    btnQ2: TButton;
    btnQ3: TButton;
    StyleBook1: TStyleBook;
    gridResults: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    memoResults: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure btnSetClick(Sender: TObject);
    procedure btnQ1Click(Sender: TObject);
    procedure btnQ2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnQ3Click(Sender: TObject);
  private var
    FOrders: TOrderArray;
    procedure ShowResultToMemo(const iIsMemo: Boolean);
    procedure ClearResults;
    procedure OrderToGrid(const iOrders: TOrderArray; const iGrid: TStringGrid);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math, System.Generics.Defaults,
  uDynStringArray, uOrderListAverage, uOrderListToppingCount;

{$R *.fmx}

procedure TForm1.btnQ1Click(Sender: TObject);
begin
  ShowResultToMemo(False);
  OrderToGrid(FindByHotFlavoer(FOrders, 2), gridResults);
end;

procedure TForm1.btnQ2Click(Sender: TObject);
var
  Average: TAverage;
  Averages: TAverages;
begin
  ClearResults;
  ShowResultToMemo(True);

  Averages := TAverages.Create;
  try
    GetAvarageByCategory(FOrders, Averages);

    for Average in Averages do
      memoResults.Lines.Add(
        Format('%s, %0.4f', [Average.Category, Average.Average]));
  finally
    Averages.Free;
  end;
end;

procedure TForm1.btnQ3Click(Sender: TObject);
var
  ToppingDic: TToppingDic;
  Pair: TToppingPair;
begin
  ClearResults;
  ShowResultToMemo(True);

  ToppingDic := TToppingDic.Create;
  try
    GetToppingCounts(FOrders, 'ロースカツ', ToppingDic);

    for Pair in ToppingDic do
      memoResults.Lines.Add(
        Format('%s, %d', [Pair.Key, Pair.Value]));
  finally
    ToppingDic.Free;
  end;
end;

procedure TForm1.btnSetClick(Sender: TObject);
begin
  FOrders := getOrderHistory(memoOrders.Lines);
  OrderToGrid(FOrders, gridOrders);
end;

procedure TForm1.ClearResults;
begin
  gridResults.RowCount := 0;
  memoResults.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowResultToMemo(False);
end;

procedure TForm1.OrderToGrid(
  const iOrders: TOrderArray;
  const iGrid: TStringGrid);
var
  X, Y: Integer;
begin
  iGrid.RowCount := Length(iOrders);

  for Y := 0 to iGrid.RowCount - 1 do begin
    for X := 0 to iOrders[Y].Count - 1 do
      iGrid.Cells[X, Y] := iOrders[Y][X];
  end;
end;

procedure TForm1.ShowResultToMemo(const iIsMemo: Boolean);
begin
  memoResults.Visible := iIsMemo;
  gridResults.Visible := not iIsMemo;
end;

end.
