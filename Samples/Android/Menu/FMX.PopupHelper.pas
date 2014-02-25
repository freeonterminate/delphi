(*
 * TPopup をトグルで表示できるようにする
 *（ボタンを押す度、表示・非表示を繰り返す）
 *
 * Platforms: Windows, Android
 *
 *)

unit FMX.PopupHelper;

interface

uses
  System.Classes, System.UITypes, FMX.Controls;

type
  TPopupHelper = class helper for TPopup
  private
    procedure AfterClose;
    procedure CloseProcEx(Sender: TObject; var Action: TCloseAction);
  protected
    procedure PopupEx(const iControl: TControl);
    procedure Close;
  public
    procedure Toggle(const iControl: TControl);
  end;

implementation

uses
  System.Rtti, System.Types,
  FMX.Forms, FMX.Types, FMX.Platform, FMX.Log, FMX.LateExecuter
  {$IFDEF MSWINDOWS}
    , Winapi.Windows, FMX.Platform.Win
  {$ENDIF}
  {$IFDEF ANDROID}
    , FMX.Platform.Android
  {$ENDIF}
  ;

{ TPopupHelper }

var
  [Weak] GMouse: IFMXMouseService;
  [Weak] GControl: TControl = nil;
  GCloseProc: TCloseEvent;
  GPopuped: Boolean = False;

procedure TPopupHelper.AfterClose;
var
  Pos: TPointF;
  Outside: Boolean;
  {$IFDEF MSWINDOWS}
    CurPos: TPoint;
  {$ENDIF}
begin
  Outside := True;

  if (GControl <> nil) and (GMouse <> nil) then begin
    Pos := GMouse.GetMousePos;

    {$IFDEF MSWINDOWS}
      CurPos := Pos.Truncate;
      ScreenToClient(
        WindowHandleToPlatform(Screen.ActiveForm.Handle).Wnd,
        CurPos);
      Pos := TPointF.Create(CurPos.X, CurPos.Y);
    {$ENDIF}
    {$IFDEF ANDROID or IOS}
      Pos.Offset(0, -Screen.ActiveForm.Top);
    {$ENDIF}

    if (GControl.AbsoluteRect.Contains(Pos)) then
      Outside := False;
  end;

  if (Outside) then
    GPopuped := False;
end;

procedure TPopupHelper.Close;
begin
  IsOpen := False;
end;

procedure TPopupHelper.CloseProcEx(Sender: TObject; var Action: TCloseAction);
begin
  GCloseProc(Sender, Action);
  LateExec(procedure begin AfterClose; end);
end;

procedure TPopupHelper.PopupEx(const iControl: TControl);
var
  Form: TCustomPopupForm;
  Context: TRttiContext;
  Field: TRttiField;
begin
  GControl := iControl;
  Popup;

  if (GMouse = nil) then
    TPlatformServices.Current.SupportsPlatformService(
      IFMXMouseService,
      IInterface(GMouse));

  if (HasPopupForm) then begin
    Context := TRttiContext.Create;
    try
      Field := Context.GetType(ClassType).GetField('FPopupForm');
      Form := TCustomPopupForm(Field.GetValue(Self).AsObject);

      GCloseProc := Form.OnClose;
      Form.OnClose := CloseProcEx;
    finally
      Context.Free;
    end;
  end;
end;

procedure TPopupHelper.Toggle(const iControl: TControl);
begin
  GPopuped := not GPopuped;

  if (HasPopupForm) then begin
    Visible := GPopuped;
  end
  else begin
    if (GPopuped) then
      PopupEx(iControl)
    else
      Close;
  end;
end;

end.
