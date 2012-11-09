unit uFindDialogEx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  TFindDialogEx = class(TFindDialog)
  public
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

end.
