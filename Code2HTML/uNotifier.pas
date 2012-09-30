unit uNotifier;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ComCtrls;

type
  TNotifyForm = class(TForm)
  protected
    procedure CreateParams(var vParams: TCreateParams); override;
  end;

  TNotifier = class(TComponent)
  private
    // Variables
    FShowing: Boolean;
    FShowRect: TRect;
    MsgLabelWidth: Integer;
    MsgLabelHeight: Integer;
    // Objects
    MsgLabel: TLabel;
    btnCancel: TButton;
    // Property Implementation of Variables
    FCaption: TCaption;
    FMsg: String;
    FProgressBar: TProgressBar;
    FNotifyForm: TNotifyForm;
    FCanCancel: Boolean;
    FIsCancel: Boolean;
    FShowProgressBar: Boolean;
    FAdditionalCreateParams: Integer;
    FOnCancel: TNotifyEvent;
    FBorderStyle: TFormBorderStyle;
    FAdjustWidth: Boolean;
    // Methods
    procedure CalcBounds;
    procedure ReadClassData(vStream: TStream);
    procedure WriteClassData(vStream: TStream);
    // Event Handlers
    procedure CancelClick(Sender: TObject);
    procedure SetMsg(const Value: String);
    procedure SetFormBorderStyle(const Value: TFormBorderStyle);
  protected
    procedure DefineProperties(vFiler: TFiler); override;
  public
    // Constructor & Destructor
    constructor Create(vOwner: TComponent); override;
    destructor Destroy; override;
    // Methods
    procedure Show; overload;
    procedure Show(vForm: TForm); overload;
    procedure Show(vControl: TControl); overload;
    procedure Show(vRect: TRect); overload;
    procedure Hide;
    procedure ClearProgressInfo;
    procedure SetParent(const vControl: TWinControl);
  published
    // Properties
    property Caption: TCaption read FCaption write FCaption;
    property Msg: String read FMsg write SetMsg;
    property ProgressBar: TProgressBar read FProgressBar;
    property CanCancel: Boolean read FCanCancel write FCanCancel;
    property Showing: Boolean read FShowing;
    property ShowProgressBar: Boolean
      read FShowProgressBar write FShowProgressBar;
    property IsCancel: Boolean read FIsCancel;
    property AdditionalCreateParams: Integer
     read FAdditionalCreateParams write FAdditionalCreateParams default 0;
    property BorderStyle: TFormBorderStyle
      read FBorderStyle write SetFormBorderStyle default bsDialog;
    property AdjustWidth: Boolean
      read FAdjustWidth write FAdjustWidth default False;
    // Events
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

procedure ShowNotify(const iCaption, iMsg: String; const iControl: TControl);
procedure HideNotify(const iRecoveryFocus: Boolean = True);

procedure Register;

implementation

var
  GNotifier: TNotifier = nil;
  GFocusedForm: TCustomForm = nil;
  GActiveControl: TWinControl = nil;

procedure Register;
begin
  RegisterComponents('delphi maniacs', [TNotifier]);
end;

procedure ShowNotify(const iCaption, iMsg: String; const iControl: TControl);
var
  Form: TCustomForm;
begin
  HideNotify;

  if (csDestroying in Application.ComponentState) then
    Exit;

  Form := GetParentForm(iControl);

  GFocusedForm := Screen.FocusedForm;
  GActiveControl := Screen.ActiveControl;

  GNotifier := TNotifier.Create(Form);
  with GNotifier do begin
    Caption := iCaption;
    Msg := iMsg;

    Show(iControl);
  end;
end;

procedure HideNotify(const iRecoveryFocus: Boolean = True);
begin
  if (GNotifier <> nil) then begin
    GNotifier.Hide;
    GNotifier.Free;
    GNotifier := nil;
  end;

  if (iRecoveryFocus) then begin
    Application.ProcessMessages;

    if (GFocusedForm <> nil) then
      SetFocus(GFocusedForm.Handle);

    if (GActiveControl <> nil) then
      SetFocus(GActiveControl.Handle);
  end;

  GFocusedForm := nil;
  GActiveControl := nil;
end;

{ TNotifyForm }

procedure TNotifyForm.CreateParams(var vParams: TCreateParams);
begin
  inherited;

  vParams.Style :=
    vParams.Style or DWORD(TNotifier(Owner).FAdditionalCreateParams);
end;

//***** TNotifier ****************************************************************

constructor TNotifier.Create(vOwner: TComponent);
(*
 * äTóv
 * à¯êî
 *)
begin
  inherited Create(vOwner);

  FBorderStyle := bsDialog;

  FNotifyForm := TNotifyForm.CreateNew(Self);
  FNotifyForm.FormStyle := fsStayOnTop;
  FNotifyForm.BorderStyle := FBorderStyle;
  FNotifyForm.BorderIcons := [];
  FNotifyForm.DefaultMonitor := dmDesktop;

  MsgLabel := TLabel.Create(Self);
  MsgLabel.Parent := FNotifyForm;
  MsgLabel.Transparent := True;

  btnCancel := TButton.Create(Self);
  btnCancel.Visible := False;
  btnCancel.Height := 20;
  btnCancel.Parent := FNotifyForm;
  btnCancel.ModalResult := mrCancel;
  btnCancel.Caption := '∑¨›æŸ(&C)';
  btnCancel.Cancel := True;
  btnCancel.OnClick := CancelClick;

  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Visible := False;
  FProgressBar.Parent := FNotifyForm;

  FCaption := Application.Title;
end;

destructor TNotifier.Destroy;
(*
 * äTóv
 * à¯êî
 *)
begin
  if (FShowing) then
    Hide;

  MsgLabel.Free;
  FNotifyForm.Release;

  inherited Destroy;
end;

procedure TNotifier.Show;
(*
 * äTóv
 *)
begin
  Show(Rect(0, 0, Screen.Width, Screen.Height));
end;

procedure TNotifier.Show(vForm: TForm);
(*
 * äTóv
 * à¯êî
 *)
begin
  if (vForm = nil) then
    Show
  else
    Show(vForm.BoundsRect);
end;

procedure TNotifier.Show(vControl: TControl);
var
  tmpRect: TRect;
begin
  if (vControl = nil) then
    Show
  else begin
    tmpRect := vControl.ClientRect;
    OffsetRect(tmpRect, vControl.ClientOrigin.X, vControl.ClientOrigin.Y);

    Show(tmpRect);
  end;
end;

procedure TNotifier.CalcBounds;
var
  tmpInt: Integer;
  tmpInt2: Integer;
begin
  FNotifyForm.Caption := FCaption;
  MsgLabel.Caption := FMsg;

  tmpInt := MsgLabel.Height + 30;
  if (FCanCancel) then
    Inc(tmpInt, btnCancel.Height);

  if (FShowProgressBar) then
    Inc(tmpInt, ProgressBar.Height);

  FNotifyForm.ClientHeight := tmpInt;

  tmpInt := MsgLabel.Width;
  tmpInt2 := FNotifyForm.Canvas.TextWidth(FCaption);
  if (tmpInt2 > tmpInt) then
    tmpInt := tmpInt2;
  if (FCanCancel) and (btnCancel.Width > tmpInt) then
    tmpInt := btnCancel.Width;

  FNotifyForm.ClientWidth := Round(tmpInt * 1.2);

  tmpInt := (FNotifyForm.ClientHeight - MsgLabel.Height) div 2;
  if (FCanCancel) or (FShowProgressBar) then
    tmpInt := 8;

  MsgLabelHeight := MsgLabel.Height;
  MsgLabelWidth := MsgLabel.Width;

  MsgLabel.SetBounds(
    //(FNotifyForm.ClientWidth - MsgLabel.Width) div 2,
    9,
    tmpInt,
    MsgLabelWidth,
    MsgLabelHeight);

  tmpInt := FNotifyForm.ClientWidth - 16;
  FProgressBar.Visible := FShowProgressBar;
  if (FProgressBar.Visible) then
    FProgressBar.SetBounds(
      9,
      MsgLabel.BoundsRect.Bottom + 12,
      tmpInt,
      FProgressBar.Height);

  btnCancel.Visible := FCanCancel;
  if (btnCancel.Visible) then begin
    if (FShowProgressBar) then
      tmpInt := ProgressBar.BoundsRect.Bottom + 4
    else
      tmpInt := MsgLabel.BoundsRect.Bottom + 10;

    btnCancel.SetBounds(
      (FNotifyForm.ClientWidth - btnCancel.Width) div 2,
      tmpInt,
      btnCancel.Width,
      btnCancel.Height);
  end;


  with FNotifyForm do
    SetBounds(
      FShowRect.Left + ((FShowRect.Right - FShowRect.Left) - Width) div 2,
      FShowRect.Top + ((FShowRect.Bottom - FShowRect.Top) - Height) div 2,
      Width,
      Height);
end;

procedure TNotifier.Show(vRect: TRect);
(*
 * äTóv
 * à¯êî
 *)
begin
  FIsCancel := False;

  FShowRect := vRect;

  CalcBounds;

  FShowing := True;
  FNotifyForm.Show;

  if (Owner is TWinControl) then
    TWinControl(Owner).Enabled := False;

  MsgLabel.Update;
  FNotifyForm.Update;

  //if (FCanCancel) then
    //Application.ProcessMessages;
  //Application.HandleMessage;
end;

procedure TNotifier.Hide;
(*
 * äTóv
 *)
begin
  if (Owner is TWinControl) then
    with TWinControl(Owner) do begin
      Enabled := True;
      SetForegroundWindow(Handle);
    end;

  FNotifyForm.Hide;

  FShowing := False;
end;

procedure TNotifier.SetParent(const vControl: TWinControl);
begin
  Windows.SetParent(FNotifyForm.Handle, vControl.Handle);

  with FNotifyForm do begin
    SetBounds(
      (vControl.ClientWidth - Width) div 2,
      (vControl.ClientHeight - Height) div 2,
      Width,
      Height);

    Invalidate;
  end;
end;

procedure TNotifier.CancelClick(Sender: TObject);
begin
  FIsCancel := True;

  if (Assigned(FOnCancel)) then
    FOnCancel(Self);

  Hide;
end;

procedure TNotifier.SetMsg(const Value: String);
begin
  FMsg := Value;

  MsgLabel.Caption := FMsg;

  if
    (MsgLabel.Height <> MsgLabelHeight) or
    ((FAdjustWidth) and (MsgLabel.Width <> MsgLabelWidth))
  then
    CalcBounds;

  MsgLabelWidth := MsgLabel.Width;

  MsgLabel.Update;
  FNotifyForm.Update;

  //Application.HandleMessage;
end;

procedure TNotifier.ClearProgressInfo;
begin
  SetMsg('');
  FProgressBar.Position := 0;
end;

procedure TNotifier.SetFormBorderStyle(const Value: TFormBorderStyle);
begin
  if (FBorderStyle = Value) then
    Exit;

  FBorderStyle := Value;

  FNotifyForm.BorderStyle := FBorderStyle;
end;

procedure TNotifier.DefineProperties(vFiler: TFiler);
begin
  inherited;

  vFiler.DefineBinaryProperty('ClassData', ReadClassData, WriteClassData, True);
end;

procedure TNotifier.ReadClassData(vStream: TStream);
begin
  vStream.ReadComponent(FProgressBar);
end;

procedure TNotifier.WriteClassData(vStream: TStream);
begin
  vStream.WriteComponent(FProgressBar);
end;

//***** Initialization & Finalization ******************************************

initialization
begin
end;

finalization
begin
  HideNotify;
end;

end.
