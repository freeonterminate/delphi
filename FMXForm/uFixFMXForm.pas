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
  GAppWnd: HWND;
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

procedure FixApplication(const iWnd: HWND);
var
  Proc: TFnWndProc;
  FormStruct: TFormStruct;
begin
  GAppWnd := GetWindow(iWnd, GW_OWNER);
  Proc := Pointer(GetWindowLong(GAppWnd, GWL_WNDPROC));

  if (SetWindowLong(GAppWnd, GWL_WNDPROC, Integer(@WndProc)) <> 0) then begin
    FormStruct.Proc := Proc;
    FormStruct.Form := nil;

    GOldWndProc.Add(GAppWnd, FormStruct);
  end;
end;

procedure FixForm(const iForm: TCommonCustomForm);
var
  Wnd: HWND;
  Proc: TFnWndProc;
  FormStruct: TFormStruct;
begin
  Wnd := WindowHandleToPlatform(iForm.Handle).Wnd;
  Proc := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));

  if (GOldWndProc = nil) then begin
    GOldWndProc := TDictionary<HWND, TFormStruct>.Create;

    FixApplication(Wnd);
  end;

  if (SetWindowLong(Wnd, GWL_WNDPROC, Integer(@WndProc)) <> 0) then begin
    FormStruct.Proc := Proc;
    FormStruct.Form := iForm;

    //SetParent(Wnd, GAppWnd);

    GOldWndProc.Add(Wnd, FormStruct);
  end;
end;

end.
