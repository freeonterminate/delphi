unit uJudge;

interface

type
  TJanken = (
    jkGoo =   0, // 00
    jkChoki = 1, // 01
    jkPar =   2  // 10
  );

  TJankenResult = (jrLose, jrDraw, jrWin);

function IntToJanken(const iInt: Integer): TJanken; inline;

function Judge(const iPlayer1, iPlayer2: Integer): TJankenResult; overload;
function Judge(const iPlayer1, iPlayer2: TJanken): TJankenResult; overload;

implementation

function IntToJanken(const iInt: Integer): TJanken;
begin
  Result := TJanken(iInt);
end;

function Judge(const iPlayer1, iPlayer2: Integer): TJankenResult;
begin
  Result := Judge(IntToJanken(iPlayer1), IntToJanken(iPlayer2));
end;

function Judge(const iPlayer1, iPlayer2: TJanken): TJankenResult;
begin
  case (Ord(iPlayer2) - Ord(iPlayer1)) of
    0:
      Result := jrDraw;
    1, -2:
      Result := jrWin;
    else
      Result := jrLose;
  end;
end;

end.
