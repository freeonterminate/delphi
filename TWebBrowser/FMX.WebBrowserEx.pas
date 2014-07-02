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
  private
    procedure ParentResize(Sender: TObject);
  protected
    procedure SetParent(const Value: TFmxObject); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CheckBounds;
    [Weak] function GetWeb: ICustomBrowser;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure CallJS(const iFunction: String; const iParams: array of String);
    function GetTagValue(const iTagName, iValueName: String): String;
  end;

implementation

uses
  System.Rtti, System.SysUtils
  , FMX.Controls
  {$IFDEF MSWINDOWS}
    , FMX.WebBrowser.Win
  {$ENDIF}
  {$IFDEF MACOS}
    , FMX.WebBrowser.Mac
  {$ENDIF}
  ;

{ TWebBrowserEx }

procedure TWebBrowserEx.CallJS(
  const iFunction: String;
  const iParams: array of String);
{$IF defined(IOS) or defined(ANDROID)}
var
  Params: TStringBuilder;
  Param: String;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    FMX.WebBrowser.Win.CallJS(Self, iFunction, iParams);
  {$ENDIF}
  {$IFDEF MACOS}
    FMX.WebBrowser.Mac.CallJS(Self, iFunction, iParams);
    CheckBounds;
  {$ENDIF}
  {$IF defined(IOS) or defined(ANDROID)}
  Params := TStringBuilder.Create;
  try
    for Param in iParams do begin
      Params.Append(',');
      Params.Append(Param);
    end;

    Params.Remove(0, 1);

    EvaluateJavaScript(Format('%s(%s);', [iFunction, Params.ToString]));
  finally
    Params.Free;
  end;
  {$ENDIF}
end;

function TWebBrowserEx.GetTagValue(const iTagName, iValueName: String): String;
begin
  {$IFDEF MSWINDOWS}
    Result := FMX.WebBrowser.Win.GetTagValue(Self, iTagName, iValueName);
  {$ENDIF}
  {$IFDEF MACOS}
    Result := FMX.WebBrowser.Mac.GetTagValue(Self, iTagName, iValueName);
    CheckBounds;
  {$ENDIF}
  {$IF defined(IOS) or defined(ANDROID)}
    raise
      ENotSupportedException.Create(
        'GetTagValue Method is not supported on iOS/Android.');
  {$ENDIF}
end;

procedure TWebBrowserEx.CheckBounds;
var
  [Weak] Web: ICustomBrowser;
begin
  Web := GetWeb;

  if (Web <> nil) then
    Web.UpdateContentFromControl;
end;

constructor TWebBrowserEx.Create(Owner: TComponent);
begin
  inherited;

  HitTest := False;
  TabStop := False;
end;

destructor TWebBrowserEx.Destroy;
var
  [Weak] Web: ICustomBrowser;
begin
  Web := GetWeb;

  inherited;

  if (Web <> nil)  then
    Web.Hide;
end;

procedure TWebBrowserEx.EvaluateJavaScript(const JavaScript: string);
var
  [Weak] Web: ICustomBrowser;
begin
  Web := GetWeb;

  if (Web <> nil) then
    Web.EvaluateJavaScript(JavaScript);
end;

[Weak] function TWebBrowserEx.GetWeb: ICustomBrowser;
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

  Result := Web;
end;

procedure TWebBrowserEx.ParentResize(Sender: TObject);
begin
  CheckBounds;
end;

procedure TWebBrowserEx.SetParent(const Value: TFmxObject);
var
  [Weak] Web: ICustomBrowser;
begin
  if (Parent <> nil) and (Parent is TControl) then
    TControl(Parent).OnResize := nil;

  inherited;

  if (Parent <> nil) then begin
    if (Parent is TControl) then
      TControl(Parent).OnResize := ParentResize;

    Web := GetWeb;
    Web.SetWebBrowserControl(Self);

    Show;
  end;
end;

initialization
  RegisterFmxClasses([TWebBrowser]);

end.
