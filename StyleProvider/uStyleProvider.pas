unit uStyleProvider;

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  TStyleProvider = class(TObject)
  strict private
    type
      TStyleData = class
      private
        FName: String;
        FPath: String;
        FRegistered: Boolean;
        FProvider: TStyleProvider;
        constructor Create(
          const iName, iPath: String;
          const iProvider: TStyleProvider);
      public
        function Apply: Boolean;
      end;

      TStyleEnumerator = class(TObject)
      private
        FProvider: TStyleProvider;
        FIndex: Integer;
      public
        constructor Create(const iProvider: TStyleProvider);
        function GetCurrent: String;
        function MoveNext: Boolean;
        procedure Reset;
        property Current: String read GetCurrent;
      end;

      TStyleSorter = class(TInterfacedObject, IComparer<TStyleData>)
        function Compare(const Left, Right: TStyleData): Integer;
      end;

    var
      FStyles: TList<TStyleData>;
      function GetCount: Integer;
      function GetStyles(const iIndex: Integer): String;
      function GetStyleData(
        const iIndex: Integer;
        out oStyleData: TStyleData): Boolean;
  protected
    procedure LoadStyle(const iStyleDirectory: String); overload;
    procedure LoadStyle(const iStyleDirectories: array of String); overload;
  public
    constructor Create(const iStyleDirectory: String); overload;
    constructor Create(const iStyleDirectories: array of String); overload;
    destructor Destroy; override;
    function Apply(const iIndex: Integer): Boolean;
    function ApplyByName(const iName: String): Boolean;
    function Exists(const iName: String): Boolean;
    function GetEnumerator: TStyleEnumerator;
    property Count: Integer read GetCount;
    property Styles[const iIndex: Integer]: String read GetStyles; default;
  end;

implementation

uses
  System.Types, System.SysUtils, System.IOUtils, Vcl.Themes, Vcl.Styles;

{ TStyleProvider.TStyleData }

constructor TStyleProvider.TStyleData.Create(
  const iName, iPath: String;
  const iProvider: TStyleProvider);
begin
  inherited Create;

  FName := iName;
  FPath := iPath;
  FProvider := iProvider;
  FRegistered := (FPath = '');
end;

function TStyleProvider.TStyleData.Apply: Boolean;
begin
  if (not FRegistered) then begin
    try
      TStyleManager.LoadFromFile(FPath);
    except
    end;

    FRegistered := True;
  end;

  try
    Result := TStyleManager.TrySetStyle(FName);
  except
    Result := False;
  end;
end;

{ TStyleProvider.TStyleEnumerator }

constructor TStyleProvider.TStyleEnumerator.Create(
  const iProvider: TStyleProvider);
begin
  inherited Create;

  FProvider := iProvider;
  FIndex := -1;
end;

function TStyleProvider.TStyleEnumerator.GetCurrent: String;
begin
  Result := FProvider[FIndex];
end;

function TStyleProvider.TStyleEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FProvider.Count);
end;

procedure TStyleProvider.TStyleEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TStyleProvider.TStyleSorter }

function TStyleProvider.TStyleSorter.Compare(
  const Left, Right: TStyleData): Integer;
begin
  Result := CompareStr(Left.FName, Right.FName);
end;

{ TStyleProvider }

function TStyleProvider.Apply(const iIndex: Integer): Boolean;
var
  StyleData: TStyleData;
begin
  Result := GetStyleData(iIndex, StyleData);

  if (Result) then
    Result := StyleData.Apply;
end;

function TStyleProvider.ApplyByName(const iName: String): Boolean;
var
  StyleData: TStyleData;
begin
  Result := False;

  for StyleData in FStyles do
    if (StyleData.FName = iName) then begin
      Result := StyleData.Apply;
      Break;
    end;
end;

constructor TStyleProvider.Create(const iStyleDirectory: String);
begin
  Create([iStyleDirectory]);
end;

constructor TStyleProvider.Create(const iStyleDirectories: array of String);
begin
  inherited Create;

  FStyles := TList<TStyleData>.Create;

  LoadStyle(iStyleDirectories);
end;

destructor TStyleProvider.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStyles.Count - 1 do
    FStyles[i].Free;

  FStyles.Free;

  inherited;
end;

function TStyleProvider.Exists(const iName: String): Boolean;
var
  StyleData: TStyleData;
begin
  Result := False;

  for StyleData in FStyles do
    if (StyleData.FName = iName) then begin
      Result := True;
      Break;
    end;
end;

function TStyleProvider.GetCount: Integer;
begin
  Result := FStyles.Count;
end;

function TStyleProvider.GetEnumerator: TStyleEnumerator;
begin
  Result := TStyleEnumerator.Create(Self);
end;

function TStyleProvider.GetStyleData(
  const iIndex: Integer;
  out oStyleData: TStyleData): Boolean;
begin
  Result := (iIndex > -1) and (iIndex < FStyles.Count);

  if (Result) then
    oStyleData := FStyles[iIndex];
end;

function TStyleProvider.GetStyles(const iIndex: Integer): String;
var
  StyleData: TStyleData;
begin
  if (GetStyleData(iIndex, StyleData)) then
    Result := StyleData.FName
  else
    Result := '';
end;

procedure TStyleProvider.LoadStyle(const iStyleDirectory: String);
begin
  LoadStyle([iStyleDirectory]);
end;

procedure TStyleProvider.LoadStyle(const iStyleDirectories: array of String);
var
  Dir: String;
  Files: TStringDynArray;
  Path: String;
  StyleInfo: TStyleInfo;
  StyleName: String;
  StyleSorter: TStyleSorter;
begin
  FStyles.Clear;

  // Exe にインクルード済みスタイル
  for StyleName in TStyleManager.StyleNames do
    FStyles.Add(TStyleData.Create(StyleName, '', Self));

  // ディレクトリにあるスタイル
  for Dir in iStyleDirectories do begin
    if (not DirectoryExists(Dir)) then
      Continue;

    Files := TDirectory.GetFiles(Dir);

    for Path in Files do
      if
        (FileExists(Path)) and
        (TStyleManager.IsValidStyle(Path, StyleInfo))
      then begin
        if (not Exists(StyleInfo.Name)) then
          FStyles.Add(TStyleData.Create(StyleInfo.Name, Path, Self));
      end;
  end;

  // Sort
  StyleSorter := TStyleSorter.Create;
  try
    FStyles.Sort(StyleSorter);
  finally
    StyleSorter.Free;
  end;
end;

end.
