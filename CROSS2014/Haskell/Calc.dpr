program Calc;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Generics.Collections, System.Math;

const
  OP_ADD =   '+';
  OP_SUB =   '-';
  OP_MUL =   '*';
  OP_DIV =   '/';
  B_OPEN =   '(';
  B_CLOSE =  ')';
  END_MARK = #0;

type
  EParseException = Exception;

var
  Exp: String;
  OldCh: Char;

function Eval(
  const iStr: String;
  const iEndMark: Char;
  var iIndex: Integer): Single;
var
  i: Integer;
  C: Char;
  Num: Single;
  NumStr: String;
  Nums: TList<Single>;
  Ops: TList<Char>;
  OK: Boolean;
  Len: Integer;
  Part: String;

  function FloatToStr(const iFloat: Single): String; inline;
  begin
    Result := Format('%.4f', [iFloat]);
  end;

  procedure PushNum;
  begin
    if (NumStr <> '') then begin
      if (NumStr[NumStr.Length - 1] = '.') then
        NumStr := NumStr + '0';

      try
        Nums.Add(NumStr.ToSingle);
      except 
      end;
    end;

    NumStr := '';
  end;

  function CalcSub(var iIndex: integer; const iNum2: Single): Single;
  var
    Op: Char;
    Old: Single;
  begin
    Result := iNum2;
  
    while (iIndex > -1) do begin
      Old := Result;
      Num := Nums[iIndex];
      Dec(iIndex);

      Op := Ops.Last;
      Ops.Delete(Ops.Count - 1);

      case Op of
        OP_ADD, OP_SUB: begin
          Num := CalcSub(iIndex, Num);

          case Op of
            OP_ADD:  
              Result := Num + Result;
            OP_SUB:
              Result := Num - Result
          end;
        end;

        OP_MUL: begin
          Result := Num * Result;
        end;

        OP_DIV: begin
          Result := Num / Result;
        end;
      end;

      Writeln(FloatToStr(Num), Op, FloatToStr(Old), '=', FloatToStr(Result));
    end;
  end;

  procedure RaiseOpTooMuch;
  var
    Op: Char;
    OpsStr: String;
  begin
    OpsStr := '';
    for Op in Ops do
      OpsStr := OpsStr + ',' + Op;

    if (OpsStr <> '') then
      OpsStr := OpsStr.Substring(1);
      
    raise EParseException.Create(
      'Operator too much. [ ' + OpsStr + ' ]')
  end;

  procedure AddPart; overload;
  begin
    Part := Part + C;
  end;

  procedure AddPart(const iNum: Single); overload; 
  begin
    Part := Part + FloatToStr(iNum);
  end;

  function OldIsOp: Boolean;
  begin
    Result := CharInSet(OldCh, [OP_ADD, OP_SUB, OP_MUL, OP_DIV]);
  end;

begin
  Result := 0;
  OK := False;

  Nums := nil;
  Ops := nil;
  try
    Part := '';
    NumStr := '';
    
    Nums := TList<Single>.Create;
    Ops := TList<Char>.Create;

    // ãtÉ|Å[ÉâÉìÉhãLñ@âª
    Len := iStr.Length;
    while (iIndex < Len) do begin
      C := iStr.Chars[iIndex];
      Inc(iIndex);

      if (C = iEndMark) then begin
        PushNum;
        OK := True;
        Break;
      end;

      case C of
        '0'.. '9', '.': begin
          NumStr := NumStr + C;
          if (NumStr = '.') then
            NumStr := '0.';

          AddPart;
        end;

        OP_ADD, OP_SUB: begin
          if (NumStr = '') and (OldIsOp) then
            NumStr := C
          else begin
            PushNum;
            Ops.Add(C);
          end;
          
          AddPart;
        end;

        OP_MUL, OP_DIV: begin
          PushNum;
          Ops.Add(C);
          AddPart;
        end;

        B_OPEN: begin
          if (not OldIsOp) then
            Ops.Add(OP_MUL);

          Num := Eval(iStr, B_CLOSE, iIndex);
          Nums.Add(Num);
          AddPart(Num);
        end;

        else
          Continue;
      end;
      
      OldCh := C;
    end;

    if (not OK) then
      raise EParseException.Create('EndMark Not found [ ' + iEndMark + ' ]');

    // ìríÜåoâﬂ
    Writeln;
    Writeln('STACKS:--------------------');
    for Num in Nums do
      Write(FloatToStr(Num), ' ');
    Writeln;

    for C in Ops do
      Write(C, ' ');
    Writeln;
    Writeln;

    // ââéZ
    i := Nums.Count - 1;
    if (i > -1) then begin
      Result := Nums[i];
      Dec(i);
    end;
    
    Result := CalcSub(i, Result);

    if (Ops.Count > 1) then
      RaiseOpTooMuch 
    else if (Ops.Count > 0) then
      case Ops.Last of
        OP_ADD:
          ;
        
        OP_SUB:
          Result := -Result;

        else
          RaiseOpTooMuch;
      end;
  finally
    Ops.Free;
    Nums.Free;
  end;

  Writeln;
  Writeln('Calc: ' + Part + ' = ', FloatToStr(Result));
  Writeln('---------------------------');
end;

procedure CallEval(const iExp: String) ;
var
  Dummy: Integer;
  Ans: Single;
begin
  try
    Writeln(iExp);
      
    Dummy := 0;
    OldCh := OP_ADD;

    Ans := Eval(iExp + END_MARK, END_MARK, Dummy);

    Writeln('ANSWER: ', FloatToStr(Ans));
  except on E: EParseException do
    Writeln('ERROR: ' + E.Message);
  end;
end;

begin
  if (ParamCount > 0) then begin
    CallEval(ParamStr(1));
    Readln;
  end
  else begin
    while (True) do begin
      Writeln;
      Write('Input expression: ');
      Readln(Exp);
      if (Exp = '') then
        Break;

      CallEval(Exp);  
    end;
  end;
end.
