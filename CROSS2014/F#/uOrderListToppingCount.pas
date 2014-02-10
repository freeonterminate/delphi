unit uOrderListToppingCount;

interface

uses
  System.Generics.Collections, uOrderList, uDynArrayUtils;

type
  TToppingDic = TDictionary<String, Integer>;
  TToppingPair = TPair<String, Integer>;

procedure GetToppingCounts(
  const iOrder: TOrderArray;
  const iToppingName: String;
  const iResult: TToppingDic);

implementation

procedure GetToppingCounts(
  const iOrder: TOrderArray;
  const iToppingName: String;
  const iResult: TToppingDic);
var
  Orders: TOrderArray;
  Order: TOrder;
  Topping: String;
begin
  Orders := FindByCurryMenu(IOrder, iToppingName);

  for Order in Orders do begin
    for Topping in Order.Toppings do
      if (iResult.ContainsKey(Topping)) then
        iResult[Topping] := iResult[Topping] + 1
      else
        iResult.Add(Topping, 1);
  end;
end;

end.
