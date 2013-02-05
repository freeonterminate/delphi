unit uIMEStartEnd;

interface

type
  TIMEStartEndNotifyEvent = procedure(const iStart: Boolean) of object;

procedure AddIMEEventListener(const iEvent: TIMEStartEndNotifyEvent);
procedure RemoveIMEEventListener(const iEvent: TIMEStartEndNotifyEvent);

implementation

uses
  Winapi.Windows, Winapi.Messages, Vcl.Controls, System.Generics.Collections;

var
  GHookHandle: HHOOK;    // Keyboard Hook のハンドル
  GHandlers: TList<TIMEStartEndNotifyEvent>;

procedure AddIMEEventListener(const iEvent: TIMEStartEndNotifyEvent);
begin
  if (GHandlers.IndexOf(iEvent) < 0) then
    GHandlers.Add(iEvent);
end;

procedure RemoveIMEEventListener(const iEvent: TIMEStartEndNotifyEvent);
begin
  if (GHandlers.IndexOf(iEvent) > -1) then
    GHandlers.Remove(iEvent);
end;

procedure CallEventHandlers(const iStart: Boolean);
var
  Handler: TIMEStartEndNotifyEvent;
begin
  for Handler in GHandlers do
    Handler(iStart);
end;

function CallWndProc(
  iNCode: Integer;
  iWParam: WPARAM;
  iLParam: LPARAM): LRESULT; stdcall;
begin
  Result := CallNextHookEx(GHookHandle, iNCode, iWParam, iLParam);

  if (iNCode < 0) then
    Exit;

  with PCWPStruct(iLParam)^ do begin
    case message of
      WM_IME_COMPOSITION: begin
        CallEventHandlers(True);
      end;

      WM_IME_ENDCOMPOSITION: begin
        CallEventHandlers(False);
      end;
    end;
  end;
end;

initialization
begin
  GHandlers := TList<TIMEStartEndNotifyEvent>.Create;
  GHookHandle :=
    SetWindowsHookEx(WH_CALLWNDPROC, CallWndProc, 0, GetCurrentThreadID);
end;

finalization
begin
  UnhookWIndowsHookEx(GHookHandle);
  GHandlers.Free;
end;

end.
