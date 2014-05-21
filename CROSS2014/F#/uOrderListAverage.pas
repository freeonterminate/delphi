unit uOrderListAverage;

interface

uses
  System.Math, System.Generics.Defaults, System.Generics.Collections,
  uDynStringArray, uOrderList;

type
  TAverage = record
    Category: String;
    Average: Single;
  end;
  TAverages = TList<TAverage>;

procedure GetAvarageByCategory(
  const iOrders: TOrderArray;
  const iResults: TAverages);

implementation

type
  TAverageComparer= class(TComparer<TAverage>)
  public
    function Compare(const Left, Right: TAverage): Integer; override;
  end;

{ TAverageComparer }

function TAverageComparer.Compare(const Left, Right: TAverage): Integer;
begin
  if (SameValue(Left.Average, Right.Average)) then
    Result := 0
  else if (Left.Average > Right.Average) then
    Result := -1
  else
    Result := +1;
end;

procedure GetAvarageByCategory(
  const iOrders: TOrderArray;
  const iResults: TAverages);
var
  CategorieOrders: TOrderArray;
  Categories: TDynStringArray;
  Category: String;
  RiceAverage: Integer;
  Average: TAverage;
  Order: TOrder;
  i: Integer;
begin
  Categories := GetCategories(iOrders);

  for i := Low(Categories) to High(Categories) do begin
    Category := Categories[i];
    CategorieOrders := FindByCategory(iOrders, Category);

    RiceAverage := 0;
    for Order in CategorieOrders do
      Inc(RiceAverage, Order.RiceWeight);

    Average.Category := Category;
    Average.Average := RiceAverage / Length(CategorieOrders);

    iResults.Add(Average);
  end;

  iResults.Sort(TAverageComparer.Create);
end;

end.
