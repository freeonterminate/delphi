(*
 * Cross-platform IniFile
 *
 * PLATFORMS
 *   Windows / OS X / iOS / Android
 *
 * HOW TO USE
 *   uses FMX.IniFile;
 *   var
 *     IniFile: TXplatIniFile;
 *   begin
 *     IniFile := CreateIniFile('YOUR_COMPANY_NAME');
 *     IniFile.WriteString('SECTION', 'KEY', 'VALUE');
 *     IniFile.ReadString('SECTION', 'KEY', 'DEFAULT_VALUE');
 *   end;
 *
 * 2014/05/21 Version 1.0
 * Programmed by HOSOKAWA Jun (@pik)
 *)

unit FMX.IniFile;

interface

{$IF defined(MACOS) or defined(IOS)}
  {$DEFINE APPLE}
{$ENDIF}

uses
  System.Classes, System.IniFiles;

type
  TXplatIniFile = class(TCustomIniFile);

  TXplatIniFileImpl = class(TXplatIniFIle)
  protected
    procedure Init; virtual;
  public
    constructor Create(const iFileName: String); reintroduce; virtual;
  public
    class function GetAppName: String;
  end;

function CreateIniFile(const iCompany: String): TXplatIniFile;

{$IFDEF MSWINDOWS}
resourceString
  rsErrorCompanyNameIsMust = 'A Company name is a must on Windows.';
{$ENDIF}

implementation
uses
  System.SysUtils

  {$IFDEF APPLE}
    // OSX, iOS
    , FMX.IniFile.Apple
  {$ELSE}
    {$IFDEF ANDROID}
      // Android
      , FMX.IniFile.Android
    {$ELSE}
      // Windows
      , System.IOUtils
    {$ENDIF}
  {$ENDIF}
  ;

function CreateIniFile(const iCompany: String): TXplatIniFile;
{$IFDEF MSWINDOWS}
var
  FileName: String;
  AppName: String;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    if (iCompany = '') then
      raise EArgumentNilException.Create(rsErrorCompanyNameIsMust);

    AppName := TXplatIniFileImpl.GetAppName;
    FileName := String.Join(PathDelim, [TPath.GetHomePath, iCompany, AppName]);
    if (not FileName.EndsWith(PathDelim)) then
      FileName := FileName + PathDelim;

    if (not TDirectory.Exists(FileName)) then
      TDirectory.CreateDirectory(FileName);

    if (TDirectory.Exists(FileName)) then
      FileName := FileName + AppName + '.ini';

    Result := TXplatIniFile(TIniFile.Create(FIleName));
  {$ELSE}
    {$IFDEF APPLE}
      Result := TIniFileApple.Create('');
    {$ELSE}
      {$IFDEF ANDROID}
        Result := TIniFIleAndroid.Create('');
      {$ELSE}
        Result := TIniFile.Create('');
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

{ TXplatIniFileImpl }

constructor TXplatIniFileImpl.Create(const iFileName: String);
begin
  inherited Create(iFileName);
  Init;
end;

class function TXplatIniFileImpl.GetAppName: String;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

procedure TXplatIniFileImpl.Init;
begin
  // Nothing to do
end;

end.
