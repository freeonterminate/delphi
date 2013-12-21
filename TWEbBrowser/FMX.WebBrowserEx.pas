unit FMX.WebBrowserEx;

interface

uses
  System.Classes
  ,FMX.Types, FMX.WebBrowser
  ;

type
  [
    ComponentPlatformsAttribute(
      pidWin32
      or pidWin64
      or pidOSX32
      or pidiOSSimulator
      or pidiOSDevice
      or pidAndroid
    )
  ]
  TWebBrowserEx = class(TWebBrowser)
  protected
    procedure SetParent(const Value: TFmxObject); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Rtti
  {$IFDEF MSWINDOWS}
    , FMX.WebBrowser.Win
  {$ENDIF}
  {$IFDEF MACOS}
    , FMX.WebBrowser.Mac
  {$ENDIF}
  ;

{ TWebBrowserEx }
constructor TWebBrowserEx.Create(Owner: TComponent);
begin
  inherited;

  HitTest := False;
end;

destructor TWebBrowserEx.Destroy;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  [Weak] Web: ICustomBrowser;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(TWebBrowser);
    Web :=
      ICustomBrowser(RttiType.GetField('FWeb').GetValue(Self).AsInterface);
  finally
    Context.Free;
  end;

  inherited;

  if (Web <> nil)  then
    Web.Hide;
end;

procedure TWebBrowserEx.SetParent(const Value: TFmxObject);
begin
  inherited;

  if (Parent <> nil) then
    Show;
end;

initialization
  RegisterFmxClasses([TWebBrowserEx]);

end.
