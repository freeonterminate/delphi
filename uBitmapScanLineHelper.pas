unit uBitmapScanLineHelper;

interface

uses
  FMX.Types;

type
  TBitmapScanLineHelper = class helper for TBitmap
  public
    function BeginScanLine(const Row: Integer): Pointer;
    procedure EndScanLine;
  end;

implementation

uses
  Generics.Collections, FMX.PixelFormats;

type
  TBmpDataDic = TDictionary<TBitmap, TBitmapData>;

var
  GBmpDataDic: TBmpDataDic = nil;

function TBitmapScanLineHelper.BeginScanLine(const Row: Integer): Pointer;
var
  BmpData: TBitmapData;
begin
  if (Map(TMapAccess.maReadWrite, BmpData)) then begin
    GBmpDataDic.Add(Self, BmpData);
    Result := BmpData.Data;
    Inc(PByte(Result), Row * Width * GetPixelFormatBytes(PixelFormat));
  end
  else
    Result := nil;
end;

procedure TBitmapScanLineHelper.EndScanLine;
var
  BmpData: TBitmapData;
begin
  if (GBmpDataDic.TryGetValue(Self, BmpData)) then begin
    Unmap(BmpData);
    GBmpDataDic.Remove(Self);
  end;
end;

initialization
begin
  GBmpDataDic := TBmpDataDic.Create;
end;

finalization
begin
  GBmpDataDic.Free;
end;

end.
