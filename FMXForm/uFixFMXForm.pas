unit uFixFMXForm;

interface

uses
  FMX.Forms;

procedure FixForm(const iForm: TCommonCustomForm);

implementation

uses
  System.SysUtils,
  Generics.Collections,
  FMX.Platform.Win,
  Winapi.Messages,
  Winapi.Windows;

type
  TFormStruct = record
    Proc: TFnWndProc;
    Form: TCommonCustomForm;
  end;

var
  GOldWndProc: TDictionary<HWND, TFormStruct>;

function WndProc(
  hwnd: HWND;
  uMsg: UINT;
  wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  FormStruct: TFormStruct;
begin
  if
    (GOldWndProc = nil) or
    (not GOldWndProc.TryGetValue(hwnd, FormStruct)) or
    (FormStruct.Proc = nil)
  then
    Exit(0);

  Result := CallWindowProc(FormStruct.Proc, hwnd, uMsg, wParam, lParam);

  case uMsg of
    WM_SYSCOMMAND: begin
      case wParam of
        SC_CLOSE: begin
          if (wParam = SC_CLOSE) then begin
            if (FormStruct.Form = nil) then
              Application.Terminate
            else
              FormStruct.Form.Close;
          end;
        end;
      end;
    end;
  end;
end;

procedure HookWndProc(const iWnd: HWND; const iForm: TCommonCustomForm);
var
  FormStruct: TFormStruct;
  Proc: TFnWndProc;
begin
  Proc := Pointer(GetWindowLong(iWnd, GWL_WNDPROC));

  if (SetWindowLong(iWnd, GWL_WNDPROC, Integer(@WndProc)) <> 0) then begin
    FormStruct.Proc := Proc;
    FormStruct.Form := iForm;

    GOldWndProc.Add(iWnd, FormStruct);
  end;
end;

procedure FixApplication(const iWnd: HWND);
var
  Wnd: HWND;
begin
  Wnd := GetWindow(iWnd, GW_OWNER);
  HookWndProc(Wnd, nil);

  ShowWindow(Wnd, SW_HIDE);
end;

procedure FixForm(const iForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  Wnd := WindowHandleToPlatform(iForm.Handle).Wnd;

  if (GOldWndProc = nil) then begin
    GOldWndProc := TDictionary<HWND, TFormStruct>.Create;
    FixApplication(Wnd);
  end;

  HookWndProc(Wnd, iForm);

  SetWindowLong(
    Wnd,
    GWL_EXSTYLE,
    GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
end;

initialization
begin
end;

finalization
begin
  GOldWndProc.Free;
end;

end.
