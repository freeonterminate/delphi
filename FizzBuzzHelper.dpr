program FizzBuzzHelper;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

type
  TFizzBuzzHelper = record helper for Integer
    function ToFizzBuzz: String;
  end;

function TFizzBuzzHelper.ToFizzBuzz: String;
begin
  Result := '';

  if (Self mod 3 = 0) then
    Result := 'Fizz';

  if (Self mod 5 = 0) then
    Result := Result + 'Buzz';

  if (Result = '') then
    Result := IntToStr(Self);
end;

var
  i: Integer;
begin
  for i := 1 to 100 do
    Writeln(i.ToFizzBuzz);
end.
