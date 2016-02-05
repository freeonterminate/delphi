(*
 * Log Utility
 *
 * 2014-05-20  Version 1.0  Release
 * 2014-08-05  Version 1.1  IFMXLoggingService -> IOS: NSLog, OSX: WriteLn
 * 2014-08-28  Version 1.2  New: LogToDelphi;
 *                          if True, Log is displayed on Delphi's Event Log.
 * 2014-09-05  Version 1.3  New: UnknownType, use TryAsString
 *                          Bug: tkEnumeration, AsInteger -> AsOrdinal
 * 2014-10-28  Version 1.4  Bug: XE5, XE7 can not compile.
 * 2016-02-05  Version 1.5  New: Rect/PointF/SizeF to String methods
 *                               Enabled property
 *                               NumberOfSignifiantFigures property
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
 * HOW TO USE
 *   1. uses FMX.Log;
 *   2. Log.d('Message');           // Type only String
 *      Log.d([Value1, Value2...]); // multiple arguments, Any type ok.
 *
 * Programmed by HOSOKAWA Jun / Delphi MVP / twitter: @pik
 *)

unit FMX.Log;

interface

uses
  System.Types, System.SysUtils, System.Rtti, FMX.Platform
  {$IFDEF MACOS}
    {$IFDEF IOS}
    , iOSapi.Foundation
    {$ELSE}
    , Macapi.Foundation
    {$ENDIF}
  {$ENDIF}
  ;

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
  private const
    DEFAULT_NUMBER_OF_SIGNIFIANT_FIGURES = 4;
  private class var
    FTag: String;
    FEnabled: Boolean;
    FLogToDelphi: Boolean;
    FNumberOfSignifiantFigures: Integer;
    FOnLog: TLogEvent;
    {$IFDEF IOS}
      class var FLogText: NSString;
    {$ENDIF}
  protected // constructor
    constructor Create; reintroduce;
  protected // methods
    class function GetFloatFormat: String; inline;
    class procedure output(const iPriority: Integer; const iText: String);
  public // methods
    class function Join(const iTexts: array of String): String;
    class function JoinV(const iValues: array of TValue): String;
    class function RectFToString(const iRect: TRectF): String;
    class function PointFToString(const iPoint: TPointF): String;
    class function SizeFToString(const iSize: TSizeF): String;
  public // log methods
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
  public // properties
    class property TAG: String read FTag write FTag;
    class property Enabled: Boolean read FEnabled write FEnabled;
    class property LogToDelphi: Boolean
      read FLogToDelphi
      write FLogToDelphi
      default True;
    class property NumberOfSignifiantFigures: Integer
      read FNumberOfSignifiantFigures
      write FNumberOfSignifiantFigures
      default DEFAULT_NUMBER_OF_SIGNIFIANT_FIGURES;
  public // events
    class property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  System.Classes
  {$IF RTLVersion < 28}
  , System.TypInfo
  {$ENDIF}
  {$IFDEF ANDROID}
  , Androidapi.Log, Androidapi.JNI.JavaTypes, FMX.Helpers.Android
  {$ENDIF}
  {$IFDEF MACOS}
  , Macapi.ObjectiveC, Macapi.Helpers
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

constructor Log.Create;
begin
  raise
    Exception.Create(
      'DO NOT CREATE LOG CLASS. how to use -> Log.x(''message''); ');
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

class function Log.GetFloatFormat: String;
begin
  Result := '%.' + FNumberOfSignifiantFigures.ToString + 'f';
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
{$IFDEF MSWINDOWS}
  LogBody: String;
{$ENDIF}
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
  if (not FEnabled) then
    Exit;

  if
    (FTag = '') and
    (
      TPlatformServices.Current
      .SupportsPlatformService(IFMXApplicationService, IInterface(AppService))
    )
  then
  begin
    Log.FTag := AppService.GetTitle;

    {$IF RTLVersion > 26}
    if (Log.FTag = '') then
      Log.FTag := AppService.GetDefaultTitle;
    {$ENDIF}
  end;

  {$IFDEF ANDROID}
  __android_log_write(
    android_LogPriority(iPriority),
    M.AsUTF8(FTag).ToPointer,
    M.AsUtf8(iText).ToPointer);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      LogBody := GetFormatedText;
      {$WARNINGS OFF 2039}
      if (DebugHook <> 0) and (FLogToDelphi) then
        OutputDebugString(PWideChar(LogBody));
      {$WARNINGS ON 2039}

      AllocConsole;
      WriteLn(LogBody);
    {$ENDIF}
    {$IFDEF MACOS}
      {$IFDEF IOS}
      FLogText := StrToNSStr(GetFormatedText);
      NSLog((FLogText as ILocalObject).GetObjectID);
      {$ELSE}
      WriteLn(GetFormatedText);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  if (Assigned(FOnLog)) then
    FOnLog(iPriority, iText);
end;

class function Log.PointFToString(const iPoint: TPointF): String;
var
  Fmt: String;
begin
  Fmt := GetFloatFormat;
  Result := Format('PointF(' + Fmt + ', ' + Fmt + ')', [iPoint.X, iPoint.Y]);
end;

class function Log.RectFToString(const iRect: TRectF): String;
var
  Fmt: String;
begin
  Fmt := GetFloatFormat;

  Result :=
    Format(
      'RectF(' + Fmt + ', ' + Fmt + ', ' + Fmt + ', ' + Fmt + ')',
      [iRect.Left, iRect.Top, iRect.Width, iRect.Height]);
end;

class function Log.SizeFToString(const iSize: TSizeF): String;
var
  Fmt: String;
begin
  Fmt := GetFloatFormat;
  Result :=
    Format('SizeF(' + Fmt + ', ' + Fmt + ')', [iSize.Width, iSize.Width]);
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
const
  BOOL_STR: array [Boolean] of String = ('False', 'True');
var
  Texts: array of String;
  i: Integer;
  Value: TValue;
  B: Boolean;
  S: String;
  Obj: TObject;
begin
  Result := '';

  SetLength(Texts, Length(iValues));

  for i := Low(Texts) to High(Texts) do
  begin
    Value := iValues[i];
    case (Value.Kind) of
      TTypeKind.tkInteger:
        Texts[i] := Value.AsInteger.ToString;

      TTypeKind.tkInt64:
        Texts[i] := Value.AsInt64.ToString;

      TTypeKind.tkString,
      TTypeKind.tkLString,
      TTypeKind.tkWString,
      TTypeKind.tkUString,
      TTypeKind.tkChar,
      TTypeKind.tkWChar:
        Texts[i] := Value.AsString;

      TTypeKind.tkClass:
      begin
        Obj := Value.AsObject;
        if (Obj = nil) then
          Texts[i] := 'nil'
        else
          Texts[i] := Format('%s($%p)', [Obj.ClassName, Pointer(Obj)]);
      end;

      TTypeKind.tkClassRef:
        Texts[i] := Value.AsClass.ClassName;

      TTypeKind.tkFloat:
        Texts[i] := Format(GetFloatFormat, [Value.AsType<Double>]);

      TTypeKind.tkPointer:
        Texts[i] := Format('$%p', [Value.AsType<Pointer>]);

      TTypeKind.tkEnumeration:
        if (Value.TryAsType<Boolean>(B)) then
          Texts[i] := BOOL_STR[B]
        else
          Texts[i] := Value.AsOrdinal.ToString;

      else
      begin
        if (Value.TryAsType<String>(S)) then
          Texts[i] := S
        else
          Texts[i] := '?';
      end;
    end;
  end;

  Result := Log.Join(Texts);
end;

initialization
  Log.FLogToDelphi := True;
  Log.FEnabled := True;
  Log.FNumberOfSignifiantFigures := Log.DEFAULT_NUMBER_OF_SIGNIFIANT_FIGURES;

end.