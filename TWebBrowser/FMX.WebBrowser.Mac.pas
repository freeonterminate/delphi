unit FMX.WebBrowser.Mac;

interface

uses
  FMX.WebBrowserEx;

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

procedure CallJS(
  const iWebBrowser: TWebBrowserEx;
  const iFunction: String;
  const iParams: array of String);

function GetTagValue(
  const iWebBrowser: TWebBrowserEx;
  const iTagName, iValueName: String): String;

implementation

uses
  System.Classes, System.SysUtils, System.Types, System.Math
  , Macapi.WebView, Macapi.Foundation, Macapi.AppKit, Macapi.CocoaTypes
  , FMX.Types, FMX.WebBrowser, FMX.Platform, FMX.Platform.Mac, FMX.Forms
  ;

type
  TMacWebBrowserService = class(TInterfacedObject, ICustomBrowser)
  private const
    WEBKIT_FRAMEWORK: String =
      '/System/Library/Frameworks/WebKit.framework/WebKit';
  private var
    FURL: String;
    FWebView: WebView;
    FWebControl: TCustomWebBrowser;
    FModule: HMODULE;
    FForm: TCommonCustomForm;
  private
    function GetBounds: TRectF;
    function GetNSBounds: NSRect;
  protected
    { IFMXWebBrowserService }
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    function GetParent : TFmxObject;
    function GetVisible : Boolean;
    procedure UpdateContentFromControl;
    procedure Navigate;
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure Show;
    procedure Hide;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMacWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

var
  GWBService: TMacWBService;

{ TMacWBService }

function TMacWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TMacWebBrowserService.Create;
end;

procedure RegisterWebBrowserService;
begin
  GWBService := TMacWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, GWBService);
end;

procedure UnRegisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

{ TMacWebBrowserService }

constructor TMacWebBrowserService.Create;
begin
  inherited;

  FModule := LoadLibrary(PWideChar(WEBKIT_FRAMEWORK));

  FWebView :=
    TWebView.Wrap(
      TWebView.Alloc.initWithFrame(MakeNSRect(0, 0, 100, 100), nil, nil));
end;

destructor TMacWebBrowserService.Destroy;
begin
  FWebView.release;

  FreeLibrary(FModule);

  inherited;
end;

function TMacWebBrowserService.GetBounds: TRectF;
var
  Pt: TPointF;
begin
  Result.Empty;

  if (FWebControl <> nil) and (FForm <> nil) then begin
    Pt := FWebControl.LocalToAbsolute(FWebControl.Position.Point);
    Result :=
      TRectF.Create(
        Pt.X,
        FForm.ClientHeight - Pt.Y - FWebControl.Height,
        FWebControl.Width,
        FWebControl.Height);
  end;
end;

function TMacWebBrowserService.GetCanGoBack: Boolean;
begin
  Result := FWebView.canGoBack;
end;

function TMacWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := FWebView.canGoForward;
end;

function TMacWebBrowserService.GetNSBounds: NSRect;
var
  Bounds: TRectF;
begin
  Bounds := GetBounds;
  Result := MakeNSRect(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
end;

function TMacWebBrowserService.GetParent: TFmxObject;
begin
  if (FWebControl <> nil) then
    Result := FWebControl.Parent
  else
    Result := nil;
end;

function TMacWebBrowserService.GetURL: string;
begin
  Result := FURL;
end;

function TMacWebBrowserService.GetVisible: Boolean;
begin
  if (FWebControl <> nil) then
    Result := FWebControl.Visible
  else
    Result := False;
end;

procedure TMacWebBrowserService.GoBack;
begin
  FWebView.goBack;
end;

procedure TMacWebBrowserService.GoForward;
begin
  FWebView.goForward;
end;

procedure TMacWebBrowserService.GoHome;
begin

end;

procedure TMacWebBrowserService.Hide;
begin
  if (FWebView <> nil) and (not FWebView.isHidden) then
    FWebView.setHidden(True);
end;

procedure TMacWebBrowserService.Navigate;
var
  Url: NSURL;
  Req: NSURLRequest;
begin
  Url := TNSURL.Wrap(TNSURL.Alloc.initWithString(NSSTR(FURL)));
  Req := TNSURLRequest.Create;
  Req.initWithURL(Url);

  FWebView.mainFrame.loadRequest(Req);
end;

procedure TMacWebBrowserService.SetURL(const AValue: string);
begin
  FURL := AValue;
end;

procedure TMacWebBrowserService.SetWebBrowserControl(
  const AValue: TCustomWebBrowser);
begin
  FWebControl := AValue;

  if
    (FWebControl <> nil)
    and (FWebControl.Root <> nil)
    and (FWebControl.Root.GetObject is TCommonCustomForm)
  then
    FForm := TCommonCustomForm(FWebControl.Root.GetObject);

  UpdateContentFromControl;
end;

procedure TMacWebBrowserService.Show;
begin
  if (FWebView <> nil) and (FWebView.isHidden) then
    FWebView.setHidden(False);
end;

procedure TMacWebBrowserService.UpdateContentFromControl;
var
  View: NSView;
  Bounds: TRectF;
begin
  if (FWebView <> nil) then begin
    if
      (FWebControl <> nil)
      and not (csDesigning in FWebControl.ComponentState)
      and (FForm <> nil)
    then begin
      Bounds := GetBounds;

      View := WindowHandleToPlatform(FForm.Handle).View;
      View.addSubview(FWebView);

      if (SameValue(Bounds.Width, 0)) or (SameValue(Bounds.Height, 0)) then
        FWebView.setHidden(True)
      else begin
        FWebView.setFrame(GetNSBounds);
        FWebView.setHidden(not FWebControl.ParentedVisible);
      end;
    end
    else
      FWebView.setHidden(True);
  end;
end;


procedure CallJS(
  const iWebBrowser: TWebBrowserEx;
  const iFunction: String;
  const iParams: array of String);
var
  Params: TStringBuilder;
  Param: String;
  Service: TMacWebBrowserService;
begin
  Params := TStringBuilder.Create;
  try
    for Param in iParams do begin
      Params.Append(',');
      Params.Append(Param);
    end;

    Params.Remove(0, 1);

    Service := (iWebBrowser.GetWeb as TMacWebBrowserService);
    if (Service <> nil) then begin
      Service.FWebView.stringByEvaluatingJavaScriptFromString(
        NSSTR(Format('%s(%s);', [iFunction, Params.ToString]))
      );
    end;
  finally
    Params.Free;
  end;
end;

function GetTagValue(
  const iWebBrowser: TWebBrowserEx;
  const iTagName, iValueName: String): String;
var
  WebService: TMacWebBrowserService;
  Res: NSString;
begin
  if (iWebBrowser.GetWeb = nil) then
    Exit;

  WebService := (iWebBrowser.GetWeb as TMacWebBrowserService);
  if (WebService = nil) then
    Exit;

  Res :=
    WebService.FWebView.stringByEvaluatingJavaScriptFromString(
      NSSTR(
        'document.getElementById("' + iTagName + '").' + iValueName
      )
    );

  Result := String(Res.UTF8String);
end;

initialization
  RegisterWebBrowserService;

end.
