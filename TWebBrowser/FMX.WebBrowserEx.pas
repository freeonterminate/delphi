unit FMX.WebBrowserEx;

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE MACOS_ONLY}
{$ENDIF}

interface

uses
  System.Classes
  ,FMX.Types, FMX.WebBrowser
  ;

type
  IWebBrowserEx = interface
  ['{66AB09D6-6B38-49DA-B831-B00F43EAF471}']
    function GetTagValue(const iTagName, iValueName: String): String;
  end;

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
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CheckBounds;
    [Weak] function GetWeb: ICustomBrowser;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure CallJS(
      const iFunction: String;
      const iParams: array of String); overload;
    procedure CallJS(const iFunction: String); overload;
    function GetTagValue(const iTagName, iValueName: String): String;
  end;

implementation

uses
  System.Rtti, System.SysUtils
  , FMX.Controls
  {$IFDEF MSWINDOWS}
    , FMX.WebBrowser.Win
  {$ENDIF}
  {$IF defined(MACOS) and not defined(IOS)}
    , FMX.WebBrowser.Mac
  {$ENDIF}
  ;

{ TWebBrowserEx }

procedure TWebBrowserEx.CallJS(const iFunction: String);
begin
  CallJS(iFunction, []);
end;

procedure TWebBrowserEx.CallJS(
  const iFunction: String;
  const iParams: array of String);
var
  Params: TStringBuilder;
  Param: String;
begin
  Params := TStringBuilder.Create;
  try
    for Param in iParams do begin
      Params.Append(',');
      Params.Append(Param);
    end;

    if (Params.Length > 0) then
      Params.Remove(0, 1);

    EvaluateJavaScript(Format('%s(%s);', [iFunction, Params.ToString]));
  finally
    Params.Free;
  end;
end;

function TWebBrowserEx.GetTagValue(const iTagName, iValueName: String): String;
var
  [Weak] Web: ICustomBrowser;
  WebEx: IWebBrowserEx;
begin
  Web := GetWeb;

  if (Web = nil) then
    Exit;

  if (Supports(Web, IWebBrowserEx, WebEx)) then
    Result := WebEx.GetTagValue(iTagname, iValueName)
  else
    raise
      ENotSupportedException.Create(
        'GetTagValue method is not supported on this platform.')
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
  RttiType: TRttiType;
  [Weak] Web: ICustomBrowser;
begin
  RttiType := SharedContext.GetType(TWebBrowser);
  Web := ICustomBrowser(RttiType.GetField('FWeb').GetValue(Self).AsInterface);

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

procedure TWebBrowserEx.SetVisible(const Value: Boolean);
var
  [Weak] Web: ICustomBrowser;
begin
  inherited;

  Web := GetWeb;

  if (Web <> nil) then begin
    if (Value) then
      Web.Show
    else
      Web.Hide;
  end;
end;

initialization
  RegisterFmxClasses([TWebBrowser]);

end.
