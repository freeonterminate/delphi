unit FMX.TabItemWithClose;

interface

uses
  System.Classes
  , FMX.Controls
  , FMX.TabControl
  , FMX.StdCtrls
  ;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TTabItemWithClose = class(TTabItem)
  public const
    STYLE_CLOSE_BUTTON_NAME = 'closebutton';
  private var
    FCloseBtn: TCustomButton;
    FTabControl2: TTabControl;
    function GetText: string;
  protected
    procedure SetText(const Value: String); override;
    function GetDefaultStyleLookupName: string; override;
    procedure ChangeParent; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoCloseBtnClick(Sender: TObject);
    function DoSetWidth(
      var ioValue: Single;
      iNewValue: single;
      var ioLastValue: Single): boolean; override;
  public
    property TabControl: TTabControl read FTabControl2;
  published
    property Text: String read GetText write SetText;
  end;

implementation

uses
  System.Rtti
  , FMX.Types;

{ TTabItemWithClose }

procedure TTabItemWithClose.ApplyStyle;
var
  CloseBtn: TFmxObject;
begin
  inherited;

  CloseBtn := FindStyleResource(STYLE_CLOSE_BUTTON_NAME);
  if (CloseBtn <> nil) and (CloseBtn is TCustomButton) then begin
    FCloseBtn := TCustomButton(CloseBtn);
    FCloseBtn.OnClick := DoCloseBtnClick;
  end;
end;

procedure TTabItemWithClose.ChangeParent;
var
  Rtti: TRttiContext;
  Field: TRttiField;
  Obj: TObject;
begin
  inherited;

  FTabControl2 := nil;

  Rtti := TRttiContext.Create;
  try
    Field := Rtti.GetType(ClassType).GetField('FTabControl');
    if (Field <> nil) then begin
      Obj := Field.GetValue(Self).AsObject;

      if (Obj is TTabControl) then
        FTabControl2 := TTabControl(Obj);
    end;
  finally
    Rtti.Free;
  end;
end;

procedure TTabItemWithClose.DoCloseBtnClick(Sender: TObject);
var
  TC: TTabControl;
  i: Integer;
  ActiveTab: TTabItem;
begin
  ActiveTab := nil;
  TC := FTabControl2;

  if (TC <> nil) then
    ActiveTab := TC.ActiveTab;

  i := Index;

  Parent := nil;
  Release;

  if (ActiveTab  = Self) then begin
    if (i < TC.TabCount) then begin
      TC.TabIndex := -1;
      TC.TabIndex := i;
    end
    else begin
      Dec(i);

      if (i > -1) and (i < TC.TabCount) then
        TC.TabIndex := i;
    end;
  end;
end;

function TTabItemWithClose.DoSetWidth(var ioValue: Single; iNewValue: single;
  var ioLastValue: Single): boolean;
begin
  if (FCloseBtn <> nil) then
    iNewValue := iNewValue + FCloseBtn.Width * 1.5;

  Result := inherited;
end;

procedure TTabItemWithClose.FreeStyle;
begin
  inherited;
  FCloseBtn := nil;
end;

function TTabItemWithClose.GetDefaultStyleLookupName: string;
begin
  Result := inherited GetDefaultStyleLookupName;
  //if (FindStyleResource(Result) = nil) then
    //Result := 'TabItemStyle';
end;

function TTabItemWithClose.GetText: String;
begin
  Result := inherited Text;
  if (Result = '') then
    Result := ' ';
end;

procedure TTabItemWithClose.SetText(const Value: String);
begin
  inherited SetText(Value);
end;

initialization
begin
  RegisterFmxClasses([TTabItemWithClose], [TTabControl]);
end;

end.
