(*
 * IniFile for Android.
 *
 * The File is used from FMX.IniFile.
 *
 * 2014/05/20 version 1.0
 * Programmed by HOSOKAWA Jun (@pik)
 *)

unit FMX.IniFile.Android;

interface

{$IFDEF ANDROID}
uses
  System.Classes, System.IniFiles
  , FMX.IniFile
  , FMX.Helpers.Android
  , Androidapi.Helpers
  , Androidapi.NativeActivity
  , Androidapi.JNI
  , Androidapi.JNI.App
  , Androidapi.JNI.GraphicsContentViewText
  , Androidapi.JNI.JavaTypes
  ;

type
  TIniFileAndroid = class(TXplatIniFileImpl)
  private const
    SECTION_DELIMITER: Char = ']';
    EQUAL: Char = '=';
  private type
    TWriteFunc =
      reference to procedure (const iEdit: JSharedPreferences_Editor);
    TReadSecIdValFunc =
      reference to procedure (const iSec, iId, iVal: String);
  private
    FPrefs : JSharedPreferences;
    function GetKey(const iSection, iIdent: String): JString;
    procedure ReadAll(const iSection: String; const iFunc: TReadSecIdValFunc);
    procedure WriteTo(const iFunc: TWriteFunc);
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

    // Secions
    procedure ReadSection(const Section: String; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(
      const Section: String;
      Strings: TStrings); override;

    // Erase / Delete
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: string); override;

    // Update
    procedure UpdateFile; override;
  end;

implementation

uses
  System.SysUtils;

{ TIniFileAndroid }

procedure TIniFileAndroid.DeleteKey(const Section, Ident: String);
begin
  WriteTo(
    procedure (const iEdit: JSharedPreferences_Editor)
    begin
      iEdit.remove(GetKey(Section, Ident));
    end
  );
end;

procedure TIniFileAndroid.EraseSection(const Section: string);
begin
  if (Section = '') then
    Exit;

  ReadAll(
    Section,
    procedure (const iSec, iId, iVal: String)
    begin
      WriteTo(
        procedure (const iEdit: JSharedPreferences_Editor)
        begin
          iEdit.remove(GetKey(Section, iId));
        end
      );
    end
  );
end;

function TIniFileAndroid.GetKey(const iSection, iIdent: String): JString;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;

  SB.Append(iSection);
  SB.Append(SECTION_DELIMITER);
  SB.Append(iIdent);

  Result := StringToJString(SB.ToString);
end;

procedure TIniFileAndroid.Init;
begin
  if (FPrefs = nil) then
    FPrefs :=
      SharedActivityContext.getSharedPreferences(
        StringToJString(GetAppName),
        TJActivity.JavaClass.MODE_PRIVATE
      );
end;

procedure TIniFileAndroid.ReadAll(
  const iSection: String;
  const iFunc: TReadSecIdValFunc);
var
  Map: JMap;
  Entries: JSet;
  Iter: JIterator;
  Obj: JObject;
  Strs: TArray<String>;
  Sec: String;
begin
  Map := FPrefs.getAll;
  if (Map = nil) then
    Exit;

  Entries := Map.entrySet;
  if (Entries = nil) then
    Exit;

  Iter := Entries.iterator;

  while (Iter.hasNext) do begin
    Obj := Iter.next;

    Strs := JStringToString(Obj.toString).Split([SECTION_DELIMITER], 2);
    if (Length(Strs) < 2) then
      Continue;

    Sec := Strs[0];

    Strs := Strs[1].Split([EQUAL], 2);
    if (Length(Strs) < 2) then
      Continue;

    if (iSection = '') or (iSection = Sec) then
      iFunc(Sec, Strs[0], Strs[1]);
  end;
end;

function TIniFileAndroid.ReadBool(
  const Section, Ident: String;
  Default: Boolean): Boolean;
begin
  Result := FPrefs.getBoolean(GetKey(Section, Ident), Default);
end;

function TIniFileAndroid.ReadInteger(
  const Section, Ident: String;
  Default: Integer): Integer;
begin
  Result := FPrefs.getInt(GetKey(Section, Ident), Default);
end;

procedure TIniFileAndroid.ReadSection(const Section: String; Strings: TStrings);
begin
  ReadAll(
    Section,
    procedure (const iSec, iId, iVal: String)
    begin
      Strings.Add(iId);
    end
  );
end;

procedure TIniFileAndroid.ReadSections(Strings: TStrings);
begin
  ReadAll(
    '',
    procedure (const iSec, iId, iVal: String)
    begin
      if (Strings.IndexOf(iSec) < 0) then
        Strings.Add(iSec);
    end
  );
end;

procedure TIniFileAndroid.ReadSectionValues(
  const Section: String;
  Strings: TStrings);
begin
  ReadAll(
    Section,
    procedure (const iSec, iId, iVal: String)
    begin
      Strings.Add(String.Join(EQUAL, [iId, iVal]));
    end
  );
end;

function TIniFileAndroid.ReadString(
  const Section, Ident, Default: String): String;
begin
  Result :=
    JStringToString(
      FPrefs.getString(GetKey(Section, Ident),
      StringToJString(Default))
    );
end;

procedure TIniFileAndroid.UpdateFile;
begin
  // Nothing to do
end;

procedure TIniFileAndroid.WriteBool(
  const Section, Ident: String;
  Value: Boolean);
begin
  WriteTo(
    procedure (const iEdit: JSharedPreferences_Editor)
    begin
      iEdit.putBoolean(GetKey(Section, Ident), Value);
    end
  );
end;

procedure TIniFileAndroid.WriteInteger(
  const Section, Ident: String;
  Value: Integer);
begin
  WriteTo(
    procedure (const iEdit: JSharedPreferences_Editor)
    begin
      iEdit.putInt(GetKey(Section, Ident), Value);
    end
  );
end;

procedure TIniFileAndroid.WriteString(const Section, Ident, Value: String);
begin
  WriteTo(
    procedure (const iEdit: JSharedPreferences_Editor)
    begin
      iEdit.putString(GetKey(Section, Ident), StringToJString(Value));
    end
  );
end;

procedure TIniFileAndroid.WriteTo(const iFunc: TWriteFunc);
var
  Edit: JSharedPreferences_Editor;
begin
  Edit := FPrefs.edit;
  iFunc(Edit);
  Edit.apply;
end;

{$ELSE}
implementation
{$ENDIF}

end.
