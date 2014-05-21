(*
 * Log Utility
 *
 * PLATFORMS
 *   Windows / OS X / iOS / Android
 *
 * METHODS
 *   v  Verbose
 *   d  Debug
 *   i  Information
 *   w  Warning
 *   e  Error
 *   f  Fatal Error
 *
 * USAGE
 *   Log.d("Message"); or Log.d([Value1, Value2...]);
 *
 * Programmed by HOSOKAWA Jun (@pik)
 *
 *)

unit FMX.Log;

interface

uses
  System.Rtti, FMX.Platform;

type
  Log = class
  public type
    TLogEvent =
      procedure(const iPriority: Integer; const iText: String) of object;
    TLogLevel = (
      UNKNOWN,
      DEFAULT,
      VERBOSE,
      DEBUG,
      INFO,
      WARN,
      ERROR,
      FATAL,
      SILENT);
  public const
    LOG_LEVEL_TEXT: array [TLogLevel] of String = (
      'UNKNOWN',
      'DEFAULT',
      'VERBOSE',
      'DEBUG',
      'INFO',
      'WARN',
      'ERROR',
      'FATAL',
      'SILENT'
    );
  private
    class var FTag: String;
    class var FOnLog: TLogEvent;
    {$IFDEF MACOS or iOS}
      class var FLogger: IFMXLoggingService;
    {$ENDIF}
  public
    class function Join(const iTexts: array of String): String;
    class function JoinV(const iValues: array of TValue): String;
    class procedure v(const Text: String); overload;
    class procedure v(const iValues: array of TValue); overload;
    class procedure d(const Text: String); overload;
    class procedure d(const iValues: array of TValue); overload;
    class procedure i(const Text: String); overload;
    class procedure i(const iValues: array of TValue); overload;
    class procedure w(const Text: String); overload;
    class procedure w(const iValues: array of TValue); overload;
    class procedure e(const Text: String); overload;
    class procedure e(const iValues: array of TValue); overload;
    class procedure f(const Text: String); overload;
    class procedure f(const iValues: array of TValue); overload;
    class procedure output(const iPriority: Integer; const iText: String);
    class property TAG: String read FTag write FTag;
    class property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  SysUtils, System.TypInfo
  {$IFDEF ANDROID}
  , Androidapi.Log, Androidapi.JNI.JavaTypes, FMX.Helpers.Android
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF}
  ;

{ Log }
class procedure Log.d(const Text: String);
begin
  output(Ord(TLogLevel.DEBUG), Text);
end;

class procedure Log.d(const iValues: array of TValue);
begin
  d(JoinV(iValues));
end;

class procedure Log.e(const Text: String);
begin
  output(Ord(TLogLevel.ERROR), Text);
end;

class procedure Log.e(const iValues: array of TValue);
begin
  e(JoinV(iValues));
end;

class procedure Log.f(const Text: String);
begin
  output(Ord(TLogLevel.FATAL), Text);
end;

class procedure Log.f(const iValues: array of TValue);
begin
  f(JoinV(iValues));
end;

class procedure Log.i(const Text: String);
begin
  output(Ord(TLogLevel.INFO), Text);
end;

class procedure Log.i(const iValues: array of TValue);
begin
  i(JoinV(iValues));
end;

class procedure Log.v(const Text: String);
begin
  output(Ord(TLogLevel.VERBOSE), Text);
end;

class procedure Log.v(const iValues: array of TValue);
begin
  v(JoinV(iValues));
end;

class procedure Log.w(const Text: String);
begin
  output(Ord(TLogLevel.WARN), Text);
end;

class procedure Log.w(const iValues: array of TValue);
begin
  w(JoinV(iValues));
end;

class procedure Log.output(const iPriority: Integer; const iText: String);
var
  AppService: IFMXApplicationService;
{$IFDEF ANDROID}
  M: TMarshaller;
{$ELSE}

  function GetFormatedText: String;
  begin
    Result :=
      Format('%s (%s) %s', [FTag, LOG_LEVEL_TEXT[TLogLevel(iPriority)], iText]);
  end;
{$ENDIF}
begin
  if
    (FTag = '') and
    (
      TPlatformServices.Current
      .SupportsPlatformService(IFMXApplicationService, IInterface(AppService))
    )
  then
    Log.FTag := AppService.GetTitle;

  {$IFDEF ANDROID}
  __android_log_write(
    android_LogPriority(iPriority),
    M.AsUTF8(FTag).ToPointer,
    M.AsUtf8(iText).ToPointer);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      AllocConsole;
      WriteLn(GetFormatedText);
    {$ELSE}
      if (FLogger = nil) then
        TPlatformServices.Current
          .SupportsPlatformService(IFMXLoggingService, IInterface(FLogger));

      if (FLogger <> nil) then
        FLogger.Log(GetFormatedText, []);
    {$ENDIF}
  {$ENDIF}

  if (Assigned(FOnLog)) then
    FOnLog(iPriority, iText);
end;

class function Log.Join(const iTexts: array of String): String;
var
  SB: TStringBuilder;
  Text: String;
begin
  Result := '';

  SB := TStringBuilder.Create;
  try
    for Text in iTexts do begin
      SB.Append(Text);
      SB.Append(' ');
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function Log.JoinV(const iValues: array of TValue): String;
var
  Texts: array of String;
  i: Integer;
  Value: TValue;
begin
  Result := '';

  SetLength(Texts, Length(iValues));

  for i := Low(Texts) to High(Texts) do begin
    Value := iValues[i];
    case (Value.Kind) of
      TTypeKind.tkInteger:
        Texts[i] := IntToStr(Value.AsInteger);

      TTypeKind.tkInt64:
        Texts[i] := IntToStr(Value.AsInt64);

      TTypeKind.tkString,
      TTypeKind.tkLString,
      TTypeKind.tkWString,
      TTypeKind.tkUString,
      TTypeKind.tkChar,
      TTypeKind.tkWChar:
        Texts[i] := Value.AsString;

      TTypeKind.tkClass:
        Texts[i] := Value.AsObject.ClassName;

      TTypeKind.tkClassRef:
        Texts[i] := Value.AsClass.ClassName;

      TTypeKind.tkFloat:
        Texts[i] := Format('%f', [Value.AsType<Double>]);

      TTypeKind.tkPointer:
        Texts[i] := IntToStr(Integer(Value.AsType<Pointer>));

      else
        Texts[i] := '?';
    end;
  end;

  Result := Log.Join(Texts);
end;

end.