(*
 * FIX for FireMonkey-Form Minimize & WM_SYSCOMMAND-SC_CLOSE
 *                                  Version 1.0 / 2013-05-24
 * programed by HOSOKAWA Jun     (freeonterminate@gmail.com)
 *
 * [USAGE]
 *
 * uses
 *   uFixFMXForm; // Declare only once in Unit or Project.dpr
 *
 * [EXAMPLE]
 *
 * uses
 *   uFixFMXForm;
 *
 * procedure TForm1.Button1Click(Sender: TObject);
 * begin
 *   // doing
 * end;
 *
 *)

unit uFixFMXForm;

interface

implementation

uses
  System.SysUtils,
  uUtils,
  Winapi.Messages, Winapi.Windows;

var
  GHookHandle: HHOOK;
  GAppWnd: HWND = 0;

function CallWndProc(
  iNCode: Integer;
  iWParam: WPARAM;
  iLParam: LPARAM): LRESULT; stdcall;
var
  ActiveThreadID: DWORD;
  TargetID: DWORD;
begin
  Result := CallNextHookEx(GHookHandle, iNCode, iWParam, iLParam);

  if (iNCode < 0) then
    Exit;

  with PCWPStruct(iLParam)^ do begin
    case message of
      WM_CREATE: begin
        with PCREATESTRUCT(lParam)^ do begin
          if (GAppWnd = 0) and (StrComp(lpszClass, 'TFMAppClass') = 0) then
            GAppWnd := hwnd
          else
            if (GetWindow(hwnd, GW_OWNER) = GAppWnd) then
              SetWindowLong(
                hwnd,
                GWL_EXSTYLE,
                GetWindowLong(hwnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
        end;
      end;

      WM_SHOWWINDOW: begin
        if (GetWindow(hwnd, GW_OWNER) = GAppWnd) then begin
          ActiveThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
          TargetID := GetWindowThreadProcessId(hwnd, nil);

          AttachThreadInput(TargetID, ActiveThreadID, True);
          try
            SetForegroundWindow(hwnd);
            SetActiveWindow(hwnd);
          finally
            AttachThreadInput(TargetID, ActiveThreadID, False);
          end;
        end;
      end;
    end;
  end;
end;

initialization
begin
  GHookHandle :=
    SetWindowsHookEx(WH_CALLWNDPROC, CallWndProc, 0, GetCurrentThreadID);
end;

finalization
begin
  UnhookWIndowsHookEx(GHookHandle);
end;

end.
