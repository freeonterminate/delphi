unit uOrderList;

interface

uses
  System.Classes,
  System.Generics.Collections,
  uDynStringArray;

type
  TOrder = record
  private
    FId: Integer;
    FCurryMenu: String;
    FCategory: String;
    FRiceWeight: Integer;
    FHotFlavor: Integer;
    FToppings: TDynStringArray;
    FFactors: TDynStringArray;
    function GetFactor(const iIndex: Integer): String;
    function GetFactorInt(const iIndex: Integer): Integer;
    function GetFactorCount: Integer;
  public
    constructor Create(const iLine: String);
    function ToString: String;
    property Count: Integer read GetFactorCount;
    property Factors[const iIndex: Integer]: String read GetFactor; default;
    property FactorsInt[const iIndex: Integer]: Integer read GetFactorInt;
    property Id: Integer read FId;
    property CurryMenu: String read FCurryMenu;
    property Category: String read FCategory;
    property RiceWeight: Integer read FRiceWeight;
    property HotFlavor: Integer read FHotFlavor;
    property Toppings: TDynStringArray read FToppings;
  end;

  TOrderArray = TArray<TOrder>;

function getOrderHistory(const iStrings: TStrings): TOrderArray;

function GetCategories(const iOrders: TOrderArray): TDynStringArray;

type
  TFindFunc = reference to function(const iOrder: TOrder): Boolean;

function FindBy(const iSrc: TOrderArray; const iFunc: TFindFunc): TOrderArray;

function FindByHotFlavoer(
  const iSrc: TOrderArray;
  const iMinHotFlavor: Integer): TOrderArray;

function FindByCategory(
  const iSrc: TOrderArray;
  const iCategory: String): TOrderArray;

function FindByCurryMenu(
  const iSrc: TOrderArray;
  const iCurryMenu: String): TOrderArray;

implementation

uses
  System.SysUtils, uDynArrayUtils;

function getOrderHistory(const iStrings: TStrings): TOrderArray;
var
  i: Integer;
  Len: Integer;
begin
  Len := iStrings.Count;

  SetLength(Result, Len);

  for i := 0 to Len - 1 do
    Result[i] := TOrder.Create(iStrings[i]);end;

function GetCategories(const iOrders: TOrderArray): TDynStringArray;
var
  Order: TOrder;
begin
  for Order in iOrders do begin
    if (TDynStringArrayUtils.Exists(Result, Order.FCategory)) then
      Continue;

    TDynArrayUtils.Add<String>(Result, Order.FCategory);
  end;
end;

function FindBy(const iSrc: TOrderArray; const iFunc: TFindFunc): TOrderArray;
var
  Order: TOrder;
begin
  TDynArrayUtils.Clear<TOrder>(Result);

  for Order in iSrc do
    if (iFunc(Order)) then
      TDynArrayUtils.Add<TOrder>(Result, Order);
end;

function FindByHotFlavoer(
  const iSrc: TOrderArray;
  const iMinHotFlavor: Integer): TOrderArray;
begin
  Result :=
    FindBy(
      iSrc,
      function(const iOrder: TOrder): Boolean
      begin
        Result := iOrder.FHotFlavor >= iMinHotFlavor;
      end
    );
end;

function FindByCategory(
  const iSrc: TOrderArray;
  const iCategory: String): TOrderArray;
begin
  Result :=
    FindBy(
      iSrc,
      function(const iOrder: TOrder): Boolean
      begin
        Result := iOrder.FCategory = iCategory;
      end
    );
end;

function FindByCurryMenu(
  const iSrc: TOrderArray;
  const iCurryMenu: String): TOrderArray;
begin
  Result :=
    FindBy(
      iSrc,
      function(const iOrder: TOrder): Boolean
      begin
        Result := iOrder.FCurryMenu = iCurryMenu;
      end
    );
end;

{ TOrder }

constructor TOrder.Create(const iLine: String);
begin
  FFactors := iLine.Split([';']);

  FId := GetFactorInt(0);
  FCurryMenu := GetFactor(1);
  FCategory := GetFactor(2);
  FRiceWeight := GetFactorInt(3);
  FHotFlavor := GetFactorInt(4);
  FToppings := GetFactor(5).Split([',']);
end;

function TOrder.GetFactor(const iIndex: Integer): String;
begin
  if (iIndex < Length(FFactors)) then
    Result := FFactors[iIndex]
  else
    Result := '';
end;

function TOrder.GetFactorCount: Integer;
begin
  Result := Length(FFactors);
end;

function TOrder.GetFactorInt(const iIndex: Integer): Integer;
begin
  Result := StrToIntDef(GetFactor(iIndex), 0);
end;

function TOrder.ToString: String;
begin

end;

end.
