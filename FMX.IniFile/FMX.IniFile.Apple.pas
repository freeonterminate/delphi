(*
 * IniFile for OS X / iOS.
 *
 * The File is used from FMX.IniFile.
 *
 * [COPYRIGHT]
 * Embarcadero Technologies holds the copyright of "Apple.Inifiles".
 * Therefore, Embarcadero Technologies holds the copyright of "FMX.IniFile.Apple".
 *
 * 2014/05/19 version 1.0
 * coding by HOSOKAWA Jun (@pik)
 *)

// If you use Apple.Utils.pas then comment-out below directive.
//{$DEFINE USE_APPLEUTILS}

unit FMX.IniFile.Apple;

interface

{$IF defined(MACOS) or defined(IOS)}

uses
  System.Inifiles, System.Classes
  , FMX.IniFile
  , Macapi.Helpers
  {$IFDEF MACOS}
    {$IFDEF IOS}
    , iOSapi.Foundation, iOSapi.CocoaTypes
    {$ELSE}
    , MacApi.Foundation, Macapi.CocoaTypes
    {$ENDIF IOS}
  {$ENDIF MACOS}
  ;

type
  TIniFileApple = class(TXplatIniFileImpl)
  private
    FDefaults: NSUserDefaults;
    function GetDictionary(const Section: String): NSMutableDictionary;
    function ReadPointer(const Section, Ident: String): Pointer;
    procedure WritePointer(const Section, Ident: String; Value: Pointer);
  protected
    procedure Init; override;
  public
    // Boolean
    function ReadBool(
      const Section, Ident: String;
      Default: Boolean): Boolean; override;
    procedure WriteBool(
      const Section, Ident:
      String; Value: Boolean); override;

    // String
    function ReadString(
      const Section, Ident, Default: String): String; override;
    procedure WriteString(
      const Section, Ident, Value: String); override;

    // Integer
    function ReadInteger(
      const Section, Ident: String;
      Default: Integer): Integer; override;
    procedure WriteInteger(
      const Section, Ident: String;
      Value: Integer); override;

    // Date
    function ReadDate(
      const Section, Ident: String;
      Default: TDateTime): TDateTime; override;
    procedure WriteDate(
      const Section, Ident: String;
      Value: TDateTime); override;

    // DateTime
    function ReadDateTime(
      const Section, Ident: String;
      Default: TDateTime): TDateTime; override;
    procedure WriteDateTime(
      const Section, Ident: String;
      Value: TDateTime); override;

    // Float
    function ReadFloat(
      const Section, Ident: String;
      Default: Double): Double; override;
    procedure WriteFloat(
      const Section, Ident: String;
      Value: Double); override;

    // Time
    function ReadTime(
      const Section, Ident: String;
      Default: TDateTime): TDateTime; override;
    procedure WriteTime(
      const Section, Ident: String;
      Value: TDateTime); override;

    // Sections
    procedure ReadSection(const Section: String; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(
      const Section: String; Strings: TStrings); override;

    // Erase / Delete
    procedure EraseSection(const Section: String); override;
    procedure DeleteKey(const Section, Ident: String); override;

    // Update
    procedure UpdateFile; override;
  end;


resourceString
  rsErrorSynchronizing = 'Error synchronizing user defaults';

implementation

uses
  System.SysUtils, System.DateUtils,
  FMX.Platform,
  Macapi.ObjectiveC,  Macapi.ObjCRuntime
  {$IFDEF USE_APPLEUTILS}
  , Apple.Utils
  {$ENDIF}
  ;

{$IFNDEF USE_APPLEUTILS}
function PtrForObject(AObject: NSObject): Pointer; inline;
begin
  Result := (AObject as ILocalObject).GetObjectID;
end;

function ObjcClassName(APtr: Pointer): string; inline;
begin
  Result := String(class_getname(object_getclass(APtr)));
end;

function NSStrPtr(AString: String): Pointer; inline;
begin
  {$IFDEF IOS}
  Result :=
    TNSString.OCClass.stringWithUTF8String(
      MarshaledAString(UTF8Encode(AString)));
  {$ELSE}
  Result :=
    TNSString.OCClass.stringWithUTF8String(
      PAnsiChar(UTF8Encode(AString)));
  {$ENDIF}
end;

function NSNumberToBool(APtr: Pointer): Boolean; inline;
begin
  Result := TNSNumber.Wrap(APtr).boolValue;
end;

function NSDateToDateTime(APtr: Pointer): TDateTime; inline;
var
  Date: NSDate;
  Calendar: NSCalendar;
  Comps: NSDateComponents;
  Units: Cardinal;
begin
  Date := TNSDate.Wrap(APtr);

  Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);

  Units :=
    NSYearCalendarUnit
    or NSMonthCalendarUnit
    or NSDayCalendarUnit
    or NSHourCalendarUnit
    or NSMinuteCalendarUnit
    or NSSecondCalendarUnit;

  Comps := Calendar.components(Units, Date);

  Result :=
    EncodeDate(Comps.year, Comps.month, Comps.day)
    + EncodeTime(Comps.hour, Comps.minute, Comps.second, 0);
end;

function NSNumberToDouble(APtr: Pointer): Double; inline;
begin
  Result := TNSNumber.Wrap(APtr).doubleValue;
end;

function NSNumberToInt(APtr: Pointer): Integer; inline;
begin
  Result := TNSNumber.Wrap(APtr).integerValue;
end;

function NSStringToString(APtr: Pointer): string; inline;
begin
  Result := NSStrToStr(TNSString.Wrap(APtr));
end;

function NSObjectToString(APtr: Pointer): string;
var
  Clazz: String;
begin
  Clazz := ObjcClassName(APtr);

  if (Clazz.EndsWith('Date')) then
    Result := DateTimeToStr(NSDateToDateTime(APtr))
  else
    Result := NSStrToStr(TNSString.Wrap(APtr).description);
end;

function NSNumberPtr(ANumber: Boolean): Pointer; overload; inline;
begin
  Result := TNSNumber.OCClass.numberWithBool(ANumber);
end;

function NSNumberPtr(ANumber: Integer): Pointer; overload; inline;
begin
  Result := TNSNumber.OCClass.numberWithInt(ANumber);
end;

function NSNumberPtr(ANumber: Double): Pointer; overload; inline;
begin
  Result := TNSNumber.OCClass.numberWithDouble(ANumber);
end;

function DateTimeToNSDate(ADateTime: TDateTime): NSDate;
const
  cnDateTemplate = 'YYYY-MM-dd HH:mm:ss';
  cnDateFmt = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d';
var
  Day, Month, Year, Hour, Min, Sec, MSec: Word;
  DateStr: string;
  Tmp: NSDate;
  Formatter: NSDateFormatter;
begin
  DecodeDate(ADateTime, Year, Month, Day);
  DecodeTime(ADateTime, Hour, Min, Sec, MSec);
  DateStr := Format(cnDateFmt, [Year, Month, Day, Hour, Min, Sec, 0]);

  Formatter := TNSDateFormatter.Create;
  try
    Formatter.setDateFormat(StrToNSStr(cnDateTemplate));
    Tmp := formatter.dateFromString(StrToNSStr(DateStr));
    Result := TNSDate.Wrap(Tmp.addTimeInterval(MSec / 1000));
    Result.retain;
  finally
    Formatter.release;
  end;
end;
{$ENDIF}

{ TIniFileApple }

procedure TIniFileApple.DeleteKey(const Section, Ident: String);
var
  Dict: NSMutableDictionary;
begin
  Dict := GetDictionary(Section);

  if (Assigned(Dict)) then begin
    Dict.removeObjectForKey(NSStrPtr(Ident));
    FDefaults.setObject(PtrForObject(Dict), StrToNSStr(Section));
    UpdateFile;
  end;
end;

procedure TIniFileApple.EraseSection(const Section: String);
var
  Name: NSString;
begin
  Name := StrToNSStr(Section);

  if (Assigned(FDefaults.dictionaryForKey(Name))) then begin
    FDefaults.removeObjectForKey(Name);
    UpdateFile;
  end;
end;

function TIniFileApple.GetDictionary(
  const Section: String): NSMutableDictionary;
var
  Dict: NSDictionary;
begin
  Dict := FDefaults.dictionaryForKey(StrToNSStr(Section));

  if (Assigned(Dict)) then
    Result :=
      TNSMutableDictionary.Wrap(
        TNSMutableDictionary.OCClass.dictionaryWithDictionary(Dict))
  else begin
    Result := TNSMutableDictionary.Create;
    FDefaults.setObject(PtrForObject(Result), StrToNSStr(Section));
  end;
end;

procedure TIniFileApple.Init;
begin
  FDefaults :=
    TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
end;

function TIniFileApple.ReadBool(
  const Section, Ident: String;
  Default: Boolean): Boolean;
var
  Ptr: Pointer;
begin
  Ptr := ReadPointer(Section, Ident);

  if (Assigned(Ptr)) then
    Result := NSNumberToBool(Ptr)
  else
    Result := Default;
end;

function TIniFileApple.ReadDate(
  const Section, Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := DateOf(ReadDateTime(Section, Ident, Default));
end;

function TIniFileApple.ReadDateTime(
  const Section, Ident: String;
  Default: TDateTime): TDateTime;
var
  Ptr: Pointer;
begin
  Ptr := ReadPointer(Section, Ident);

  if Assigned(Ptr) then
    Result := NSDateToDateTime(Ptr)
  else
    Result := Default;
end;

function TIniFileApple.ReadFloat(
  const Section, Ident: String;
  Default: Double): Double;
var
  Ptr: Pointer;
begin
  Ptr := ReadPointer(Section, Ident);

  if Assigned(Ptr) then
    Result := NSNumberToDouble(Ptr)
  else
    Result := Default;
end;

function TIniFileApple.ReadInteger(
  const Section, Ident: String;
  Default: Integer): Integer;
var
  Ptr: Pointer;
begin
  Ptr := ReadPointer(Section, Ident);

  if (Assigned(Ptr)) then
    Result := NSNumberToInt(Ptr)
  else
    Result := Default;
end;

function TIniFileApple.ReadPointer(const Section, Ident: String): Pointer;
var
  Dict: NSMutableDictionary;
begin
  if (Section = '') then
    Result := FDefaults.objectForKey(StrToNSStr(Ident))
  else begin
    Dict := GetDictionary(Section);
    Result := Dict.valueForKey(StrToNSStr(Ident));
  end;
end;

procedure TIniFileApple.ReadSection(const Section: String; Strings: TStrings);
var
  Dict: NSMutableDictionary;
  Keys: NSArray;
  i: Cardinal;
  Count: Cardinal;
  Key: Pointer;
  Obj: Pointer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    Dict := GetDictionary(Section);
    Keys := Dict.allKeys;
    Count := Keys.count;

    if (Count > 0) then
      for i := 0 to Count - 1 do begin
        Key := Keys.objectAtIndex(i);
        Strings.Add(NSStringToString(Key));
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TIniFileApple.ReadSections(Strings: TStrings);
var
  Keys: NSArray;
  Cur: NSString;
  Str: String;
  Count: Cardinal;
  i: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Keys := FDefaults.dictionaryRepresentation.allKeys;
    Count := Keys.count;

    if (Count > 0) then
      for i := 0 to Count - 1 do begin
        Cur := TNSString.Wrap(Keys.objectAtIndex(i));
        Str := NSStrToStr(Cur);

        if
          (Assigned(FDefaults.dictionaryForKey(Cur))) and
          (Strings.IndexOf(Str) < 0)
        then
          Strings.Add(Str);
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TIniFileApple.ReadSectionValues(
  const Section: String;
  Strings: TStrings);
var
  Dict: NSMutableDictionary;
  Keys: NSArray;
  i: Cardinal;
  Count: Cardinal;
  Key: Pointer;
  Obj: Pointer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    Dict := GetDictionary(Section);
    Keys := Dict.allKeys;
    Count := Keys.count;

    if (Count > 0) then
      for i := 0 to Count - 1 do begin
        Key := Keys.objectAtIndex(i);
        Obj := Dict.objectForKey(Key);

        Strings.Add(
          Format('%s=%s', [NSStringToString(Key), NSObjectToString(Obj)]));
      end;
  finally
    Strings.EndUpdate;
  end;
end;

function TIniFileApple.ReadString(
  const Section, Ident,
  Default: String): String;
var
  Ptr: Pointer;
begin
  Ptr := ReadPointer(Section, Ident);

  if (Assigned(Ptr)) then
    Result := NSStringToString(Ptr)
  else
    Result := Default;
end;

function TIniFileApple.ReadTime(
  const Section, Ident: String;
  Default: TDateTime): TDateTime;
begin
 Result := TimeOf(ReadDateTime(Section, Ident, Default));
end;

procedure TIniFileApple.UpdateFile;
begin
  if (not FDefaults.synchronize) then
    raise Exception.Create(rsErrorSynchronizing);
end;

procedure TIniFileApple.WriteBool(const Section, Ident: String; Value: Boolean);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TIniFileApple.WriteDate(
  const Section, Ident: String;
  Value: TDateTime);
begin
  WriteDateTime(Section, Ident, DateOf(Value));
end;

procedure TIniFileApple.WriteDateTime(const Section, Ident: String;
  Value: TDateTime);
var
  Date: NSDate;
begin
  Date := DateTimeToNSDate(Value);

  try
    WritePointer(Section, Ident, PtrForObject(Date));
  finally
    Date.release;
  end;
end;

procedure TIniFileApple.WriteFloat(const Section, Ident: String; Value: Double);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TIniFileApple.WriteInteger(
  const Section, Ident: String;
  Value: Integer);
begin
  WritePointer(Section, Ident, NSNumberPtr(Value));
end;

procedure TIniFileApple.WritePointer(
  const Section, Ident: String;
  Value: Pointer);
var
  Dict: NSMutableDictionary;
  Ptr: Pointer;
  Key: NSString;
begin
  if (Section = '') then begin
    Ptr := Value;
    Key := StrToNSStr(Ident);
  end
  else begin
    Dict := GetDictionary(Section);
    Dict.setValue(Value, StrToNSStr(Ident));
    Ptr := PtrForObject(Dict);
    Key := StrToNSStr(Section);
  end;

  FDefaults.setObject(Ptr, Key);

  UpdateFile;
end;

procedure TIniFileApple.WriteString(const Section, Ident, Value: String);
begin
  WritePointer(Section, Ident, NSStrPtr(Value));
end;

procedure TIniFileApple.WriteTime(
  const Section, Ident: String;
  Value: TDateTime);
begin
  WriteDateTime(Section, Ident, 2 + TimeOf(Value));
end;

{$ELSE}
implementation
{$ENDIF}

end.

