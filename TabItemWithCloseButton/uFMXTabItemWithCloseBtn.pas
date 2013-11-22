unit uFMXTabItemWithCloseBtn;

interface

uses
  FMX.TabControl;

type
  TTabControlHelper = class helper for TTabControl
  private
    procedure CloseButtonClick(Sender: TObject);
    procedure TimerOnTimer(Sender: TObject);
  public
    procedure CheckCloseBtn(const iBtnResourceName: String = 'closebutton');
  end;

implementation

uses
  System.UITypes
  , System.Classes
  , FMX.Types
  , FMX.StdCtrls
  ;

{ TTabControlHelper }

var
  GRemoveTimer: TTimer;
  GIndex: Integer;
  GRemoveTab: TTabItem;
  GTabControlHelperChecked: Boolean = False;

procedure TTabControlHelper.CheckCloseBtn;
var
  B: TFmxObject;
  TabItem: TTabItem;
  i: Integer;
begin
  if (GTabControlHelperChecked) then
    Exit;

  for i := 0 to TabCount - 1 do begin
    TabItem := Tabs[i];

    B := TabItem.FindStyleResource(iBtnResourceName);

    if (B <> nil) and (B is TCustomButton) then begin
      B.TagObject := TabItem;

      TabItem.AutoSize := False;
      TabItem.Width := TabItem.Width + TCustomButton(B).Width * 1.5;

      TCustomButton(B).OnClick := CloseButtonClick;

      GTabControlHelperChecked := True;
    end;
  end;
end;

procedure TTabControlHelper.CloseButtonClick(Sender: TObject);
var
  TabItem: TTabItem;
  i: Integer;
  Len: Integer;
begin
  if (Sender is TCustomButton) then begin
    GRemoveTab := TTabItem(TCustomButton(Sender).TagObject);

    GIndex := -1;
    Len := TabCount - 1;

    for i := 0 to Len do begin
      TabItem := Tabs[i];

      if (TabItem = GRemoveTab) then begin
        if (i = Len) then begin
          if (Len > 0) then
            GIndex := i - 1;

          Break;
        end
        else begin
          GIndex := i;
          Break;
        end;
      end;
    end;

    GRemoveTab.Release;

    TabIndex := -1;

    if (GIndex <> -1) then begin
      GRemoveTimer := TTimer.Create(nil);
      GRemoveTimer.OnTimer := TimerOnTimer;
      GRemoveTimer.Interval := 20;
      GRemoveTimer.Enabled := True;
    end;
  end;
end;

procedure TTabControlHelper.TimerOnTimer(Sender: TObject);
var
  Ist: Boolean;
  i: Integer;
begin
  Ist := False;

  for i := 0 to TabCount - 1 do
    if (Tabs[i] = GRemoveTab) then begin
      Ist := True;
      Break;
    end;

  if (Ist) then
    Exit;

  GRemoveTimer.DisposeOf;
  GRemoveTImer := nil;
  GRemoveTab := nil;

  TabIndex := GIndex;
end;

end.
