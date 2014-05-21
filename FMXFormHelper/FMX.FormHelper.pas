unit FMX.FormHelper;

interface

uses
  FMX.Forms;

type
  TFormHelper = class helper for TCustomForm
  protected
    procedure DisableAltEnter;
  public
    procedure SetMinSize(const iWidth, iHeight: Integer);
  end;

implementation

uses
  System.SysUtils, System.DateUtils
  , FMX.Controls
  {$IFDEF MSWINDOWS}
  , Winapi.Windows, Winapi.Messages, Winapi.DXGI
  , System.Generics.Collections
  , FMX.Platform.Win, FMX.Context.DX10, FMX.Graphics
  {$ENDIF}
  {$IFDEF MACOS}
  , Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit
  , FMX.Platform.Mac
  {$ENDIF}
  ;

{ TFormHelper }

procedure TFormHelper.DisableAltEnter;
begin
  {$IFDEF MSWINDOWS}
  if
    (TCanvasManager.DefaultCanvas.ClassName <> 'TCanvasD2D')
    or (TCustomDX10Context.DXGIFactory = nil)
  then
    Exit;

  Canvas.BeginScene;
  try
    TCustomDX10Context.DXGIFactory.MakeWindowAssociation(
      WindowHandleToPlatform(Handle).Wnd
      , DXGI_MWA_NO_ALT_ENTER or DXGI_MWA_NO_WINDOW_CHANGES);
  finally
    Canvas.EndScene;
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
type
  TMinMaxData = record
    WndProc: Pointer;
    Width, Height: Integer;
    constructor Create(iWndProc: Pointer; iWidth, iHeight: Integer);
  end;
  TMinMaxDataDic = TDictionary<HWND, TMinMaxData>;

var
  GMinMaxDataDic: TMinMaxDataDic = nil;

constructor TMinMaxData.Create(iWndProc: Pointer; iWidth, iHeight: Integer);
begin
  WndProc := iWndProc;
  Width := iWidth;
  Height := iHeight;
end;

function WndProc(
  iWnd: HWND;
  iMsg: DWORD;
  iwParam: WPARAM;
  ilParam: LPARAM): LRESULT; stdcall;
var
  Data: TMinMaxData;
begin
  if (GMinMaxDataDic.ContainsKey(iWnd)) then begin
    Data := GMinMaxDataDic[iWnd];

    case iMsg of
      WM_GETMINMAXINFO: begin
        with PMinMaxInfo(ilParam)^ do begin
          ptMinTrackSize.X := Data.Width;
          ptMinTrackSize.Y := Data.Height;
        end;
      end;
    end;

    Result := CallWindowProc(Data.WndProc, iWnd, iMsg, iwParam, ilParam);
  end
  else
    Result := DefWindowProc(iWnd, iMsg, iwParam, ilParam);
end;

procedure TFormHelper.SetMinSize(const iWidth, iHeight: Integer);
var
  Wnd: HWND;
  tmpWndProc: Pointer;
begin
  Wnd := WindowHandleToPlatform(Self.Handle).Wnd;
  if (Wnd = 0) then
    Exit;

  tmpWndProc := Pointer(SetWindowLong(Wnd, GWL_WNDPROC, Integer(@WndProc)));
  if (tmpWndProc = nil) then
    Exit;

  if (GMinMaxDataDic = nil) then
    GMinMaxDataDic := TMinMaxDataDic.Create;

  GMinMaxDataDic.Add(Wnd, TMinMaxData.Create(tmpWndProc, iWidth, iHeight));
end;
{$ENDIF}

{$IFDEF MACOS}
procedure TFormHelper.SetMinSize(const iWidth, iHeight: Integer);
var
  Wnd: NSWindow;
  Size: NSSize;
begin
  Size.width := iWidth;
  Size.height := iHeight;

  Wnd := WindowHandleToPlatform(Self.Handle).Wnd;
  Wnd.setContentMinSize(Size);
end;
{$ENDIF}

initialization
begin
end;

finalization
begin
{$IFDEF MSWINDOWS}
  GMinMaxDataDic.DisposeOf;
{$ENDIF}
end;

end.