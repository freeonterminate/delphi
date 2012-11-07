unit uFindDialogEx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  TFindDialogEx = class(TFindDialog)
  private
    type
      TListupCallback = reference to procedure (const iControl: TControl);
    var
      FPanel: TPanel;
      FControl: TControl;
    procedure ListupControl(
      const iControl: TControl;
      const iCallback: TListupCallback);
    procedure WMEraseBkGnd(var ioMsg: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    procedure DoShow; override;
  public
    constructor Create(iOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddControl(const iControl: TControl);
    function Execute(ParentWnd: HWND): Boolean; override;
  end;

procedure Register;

implementation

uses
  System.Rtti, Vcl.Graphics;

procedure Register;
begin
  RegisterComponents('Dialogs', [TFindDialogEx]);
end;

{ TFindDialogEx }

procedure TFindDialogEx.AddControl(const iControl: TControl);
begin
  if (FControl <> nil) then
    FControl.Parent := nil;

  FControl := iControl;

  if (FControl <> nil) then begin
    FControl.Parent := FPanel;

    ListupControl(
      FControl,
      procedure (const iControl: TControl)
      begin
        iControl.StyleElements := [];
      end
    );
  end;
end;

constructor TFindDialogEx.Create(iOwner: TComponent);
begin
  inherited;

  FPanel := TPanel.Create(Self);
  with FPanel do begin
    BevelInner := bvNone;
    BevelOuter := bvNone;
    StyleElements := [];
    FullRepaint := False;
    TabOrder := 0;
    TabStop := True;
    Color := clBtnFace;
  end;
end;

destructor TFindDialogEx.Destroy;
begin
  FPanel.Free;

  inherited;
end;

procedure TFindDialogEx.DoShow;
var
  WndRect: TRect;
  tmpRect: TRect;
begin
  inherited;

  GetWindowRect(Handle, WndRect); // Screen 座標の位置。NoClient エリアも含む
  GetClientRect(Handle, tmpRect); // Window 座標の位置。Client エリアのみ

  if (FControl <> nil) then begin
    FPanel.ParentWindow := Handle; // Dialog を親に設定

    // パネルのサイズを計算
    FPanel.ClientHeight := FControl.Height;
    FPanel.SetBounds(
      0,
      tmpRect.Height - 10, // 10 pixel 位上だと見栄えが良い
      tmpRect.Width,
      FControl.Height);

    // パネルの高さ分、ダイアログのウィンドウを伸ばす
    MoveWindow(
      Handle,
      WndRect.Left,
      WndRect.Top,
      WndRect.Width,
      WndRect.Height + FPanel.Height, // FPanel の高さ分伸ばす
      True);
  end;end;

function TFindDialogEx.Execute(ParentWnd: HWND): Boolean;
var
  TId: DWORD;
  Context: TRttiContext;
  Field: TRttiField;
  Obj: TObject;
begin
  // 無効なウィンドウハンドルでは 0 が返る
  TId := GetWindowThreadProcessId(Handle);
  if (TId = 0) then begin
    Context := TRttiContext.Create;
    try
      try
        Field := Context.GetType(Self.ClassType).GetField('FFindHandle');
        if (Field <> nil) then
          Field.SetValue(Self, 0); // FFindHandle を 0 に

        Field := Context.GetType(Self.ClassType).GetField('FRedirector');
        if (Field <> nil) then begin
          Obj := Field.GetValue(Self).AsObject;
          if (Obj <> nil) then
            Obj.Free; // FRedirector を解放
        end;
      except
      end;
    finally
      Context.Free;
    end;
  end;

  Result := inherited;
end;

procedure TFindDialogEx.ListupControl(
  const iControl: TControl;
  const iCallback: TListupCallback);
var
  i: Integer;
begin
  if (iControl <> nil) then begin
    if (iControl is TWinControl) then
      with TWinControl(iControl) do
        for i := 0 to ControlCount - 1 do
          ListupControl(Controls[i], iCallback);

    iCallback(iControl);
  end;
end;

procedure TFindDialogEx.WMEraseBkGnd(var ioMsg: TWMEraseBkGnd);
begin
  inherited;

  ListupControl(
    FControl,
    procedure (const iControl: TControl)
    begin
      iControl.Repaint; // 再描画させる
    end
  );
end;

end.
