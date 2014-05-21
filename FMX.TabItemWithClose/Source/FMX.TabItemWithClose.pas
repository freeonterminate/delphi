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
  public type
    TCloseEvent = procedure (Sender: TObject; var ioDoClose: Boolean) of object;
  private const
    STYLE_COLOR_BUTTON = 'closebutton';
    STYLE_TEXT = 'text';
    STYLE_TABITEM = 'tabitemstyle';
  private var
    FCloseBtn: TCustomButton;
    FTabControl2: TTabControl;
    FOnClose: TCloseEvent;
  protected
    procedure ChangeParent; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoCloseBtnClick(Sender: TObject);
    function DoSetWidth(
      var ioValue: Single;
      iNewValue: single;
      var ioLastValue: Single): boolean; override;
  public
    class function Make(const iParent: TTabControl): TTabItemWithClose;
    property TabControl: TTabControl read FTabControl2;
  published
    property OnClose: TCloseEvent read FOnClose write FOnClose;
  end;

implementation

uses
  System.Rtti
  , FMX.Types
  , FMX.Ani
  , FMX.Objects
  ;

{ TTabItemWithClose }

procedure TTabItemWithClose.ApplyStyle;
var
  CloseBtn: TFmxObject;

  procedure AssignColors;
  var
    Src, Dest: TFmxObject;
    i, j: Integer;
    SrcChild, DestChild: TFmxObject;
    TabItem: TTabItem;
  begin
    TabItem := TTabItem.Create(nil);
    try
      TabItem.Visible := False;
      TabItem.Parent := Parent;
      TabItem.StyleLookup := STYLE_TABITEM;
      TabItem.ApplyStyleLookup;

      Src := TabItem.FindStyleResource(STYLE_TEXT);
      Dest := FindStyleResource(STYLE_TEXT);

      if (Src is TText) and (Dest is TText) then begin
        TText(Dest).Color := TText(Src).Color;

        Src := TText(Src).Parent;
        Dest := TText(Dest).Parent;
        if (Dest <> nil) then
          Dest := Dest.Parent;

        if (Src is TRectangle) and (Dest is TRectangle) then begin
          TRectangle(Dest).Corners := TRectangle(Src).Corners;
          TRectangle(Dest).CornerType := TRectangle(Src).CornerType;
          TRectangle(Dest).Sides := TRectangle(Src).Sides;
          TRectangle(Dest).Fill.Assign(TRectangle(Src).Fill);
          TRectangle(Dest).Stroke.Assign(TRectangle(Src).Stroke);
          TRectangle(Dest).XRadius := TRectangle(Src).XRadius;
          TRectangle(Dest).YRadius := TRectangle(Src).YRadius;

          j := -1;
          for i := 0 to TRectangle(Dest).Children.Count - 1 do
            if (TRectangle(Dest).Children[i] is TColorAnimation) then begin
              j := i;
              Break;
            end;

          if (j > -1) then
            for i := 0 to TRectangle(Src).Children.Count - 1 do begin
              SrcChild := TRectangle(Src).Children[i];
              DestChild := nil;

              if (SrcChild is TColorAnimation) then begin
                while (j < TRectangle(Dest).ChildrenCount) do begin
                  DestChild := TRectangle(Dest).Children[j];
                  Inc(j);

                  if (DestChild is TColorAnimation) then
                    Break;
                end;

                if (DestChild is TColorAnimation) then begin
                  TColorAnimation(DestChild).StartValue :=
                    TColorAnimation(SrcChild).StartValue;

                  TColorAnimation(DestChild).StopValue :=
                    TColorAnimation(SrcChild).StopValue;
                end;
              end;
            end;
          end;
      end;
    finally
      TabItem.DisposeOf;
    end;

    if (FTabControl2 <> nil) then begin
      FTabControl2.TabIndex := -1;
      FTabControl2.TabIndex := 0;
    end;
  end;

begin
  inherited;

  CloseBtn := FindStyleResource(STYLE_COLOR_BUTTON);
  if (CloseBtn <> nil) and (CloseBtn is TCustomButton) then begin
    AssignColors;

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
  DoClose: Boolean;
begin
  ActiveTab := nil;
  TC := FTabControl2;
  i := Index;

  if (TC <> nil) then
    ActiveTab := TC.ActiveTab;

  if (Assigned(FOnClose)) then begin
    DoClose := True;
    FOnClose(Self, DoClose);

    if (not DoClose) then
      Exit;
  end;

  if (Parent <> nil) then
    Parent := nil;

  if (TC <> nil) then begin
    if (ActiveTab = Self) then begin
      if (i < TC.TabCount) then begin
        TC.TabIndex := -1;
        TC.TabIndex := i;
      end
      else begin
        Dec(i);

        if (i > -1) and (i < TC.TabCount) then
          TC.TabIndex := i;
      end;
    end
    else if (ActiveTab <> nil) then begin
      TC.TabIndex := -1;
      TC.TabIndex := ActiveTab.Index;
    end;
  end;

  Release;
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

class function TTabItemWithClose.Make(
  const iParent: TTabControl): TTabItemWithClose;
begin
  Result := TTabItemWithClose.Create(iParent.Root.GetObject);
  Result.Parent := iParent;
  if (Result.Text = '') then
    Result.Text := ' ';
end;

initialization
begin
  RegisterFmxClasses([TTabItemWithClose], [TTabControl]);
end;

end.
