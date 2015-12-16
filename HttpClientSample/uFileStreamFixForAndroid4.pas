unit uFileStreamFixForAndroid4;

interface

uses
  System.Classes
  , System.SysUtils
  ;

type
  TFileStream = class(System.Classes.TFileStream)
  public
    function Write(
      const Buffer: TBytes;
      Offset, Count: Integer): Longint; override;
  end;

implementation

{ TFileStream }

function TFileStream.Write(
  const Buffer: TBytes;
  Offset, Count: Integer): Longint;
begin
  Result := FileWrite(Handle, Buffer[Offset], Count);
end;

end.
