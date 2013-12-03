unit TabControlPropEditor;

interface

uses
  System.Classes
  , DesignEditors
  , DesignIntf
  ;

type
  TTabControlPropEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  System.SysUtils
  , FMX.TabControl
  , FMX.TabItemWithClose
  , TabControlEditor
  ;

procedure Register;
begin
  RegisterComponentEditor(TTabControl, TTabControlPropEditor);
end;

{ TTabControlPropEditor }

procedure TTabControlPropEditor.ExecuteVerb(Index: Integer);
var
  Editor: TfrmTabControlPropEditor;
begin
  Editor := TfrmTabControlPropEditor.Create(nil);
  try
    Editor.Editor := Self;
    Editor.ShowModal(Component as TTabControl);
  finally
    Editor.Release;
  end;
end;

function TTabControlPropEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0:
      Result := '&Show TabItem Editor';
  else
    raise ENotImplemented.Create(
      'TClockLabelEditor has only one verb (index = 0) supported.');
  end;
end;

function TTabControlPropEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
