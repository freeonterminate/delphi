(*
 * TWebBrowserEx Classes
 *   WebBrowser Componet for FireMonkey.
 *
 * Copyright (c) 2013, 2014 HOSOKAWA Jun.
 *
 * Last Update 2014/12/01
 *
 * Platform:
 *   Windows, OS X, iOS, Android
 *   Delphi / C++Builder XE5, XE6, XE7 and Appmethod 1.14, 1.15
 *
 * Contact:
 *   Twitter @pik or freeonterminate@gmail.com
 *
 * Original Source:
 *   https://github.com/freeonterminate/delphi/tree/master/TWebBrowser
 *
 * How to Use:
 *   1. Add FMX.WebBroserEx to "uses" block.
 *
 *   2. Create WebBroserEx and Set parent.
 *      procedure TForm1.FormCreate(Sender: TObject);
 *      begin
 *        FWebBrowser := TWebBrowserEx.Create(Self);
 *        FWebBrowser.Parent := Panel1;
 *        FWebBrowser.Align := TAlignLayout.Client;
 *      end;
 *
 *   3. Set URL property.
 *      procedure TForm1.Button1Click(Sender: TObject);
 *      begin
 *        FWebBrowser.URL := 'https://twitter.com/pik';
 *      end;
 *
 * LICENSE:
 *   本ソフトウェアは「現状のまま」で、明示であるか暗黙であるかを問わず、
 *   何らの保証もなく提供されます。
 *   本ソフトウェアの使用によって生じるいかなる損害についても、
 *   作者は一切の責任を負わないものとします。
 *
 *   以下の制限に従う限り、商用アプリケーションを含めて、本ソフトウェアを
 *   任意の目的に使用し、自由に改変して再頒布することをすべての人に許可します。
 *
 *   1. 本ソフトウェアの出自について虚偽の表示をしてはなりません。
 *      あなたがオリジナルのソフトウェアを作成したと主張してはなりません。
 *      あなたが本ソフトウェアを製品内で使用する場合、製品の文書に謝辞を入れて
 *      いただければ幸いですが、必須ではありません。
 *
 *   2. ソースを変更した場合は、そのことを明示しなければなりません。
 *      オリジナルのソフトウェアであるという虚偽の表示をしてはなりません。
 *
 *   3. ソースの頒布物から、この表示を削除したり、表示の内容を変更したりしては
 *      なりません。
 *
 *   This software is provided 'as-is', without any express or implied warranty.
 *   In no event will the authors be held liable for any damages arising from
 *   the use of this software.
 *
 *   Permission is granted to anyone to use this software for any purpose,
 *   including commercial applications, and to alter it and redistribute
 *   it freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented;
 *      you must not claim that you wrote the original software.
 *      If you use this software in a product, an acknowledgment in the product
 *      documentation would be appreciated but is not required.
 *
 *   2. Altered source versions must be plainly marked as such,
 *      and must not be misrepresented as being the original software.
 *
 *   3. This notice may not be removed or altered from any source distribution.
 *)

unit FMX.WebBrowser.Win;

interface

uses
  FMX.WebBrowserEx;

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.DateUtils
  , System.Generics.Collections, System.IOUtils
  , Winapi.Windows, Winapi.Messages
  , FMX.Types, FMX.WebBrowser, FMX.Platform, FMX.Platform.Win, FMX.Forms
  , FMX.Controls, FMX.Graphics
  , Vcl.Controls, Vcl.OleCtrls, Vcl.Forms, Vcl.Graphics
  , SHDocVw, Winapi.ActiveX, MSHTML, idoc
  ;

const
  SID_STravelLogCursor: TGUID = '{7EBFDD80-AD18-11d3-A4C5-00C04F72D6B8}';
  IID_ITravelLogStg: TGUID = '{7EBFDD80-AD18-11d3-A4C5-00C04F72D6B8}';

  TLEF_RELATIVE_BACK = $00000010;
  TLEF_RELATIVE_FORE = $00000020;

type
  ITravelLogEntry = interface(IUnknown)
    ['{7EBFDD87-AD18-11d3-A4C5-00C04F72D6B8}']
    function GetTitle(var ppszTitle: POleStr): HRESULT; stdcall;
    function GetUrl(var ppszURL: POleStr): HRESULT; stdcall;
  end;

  IEnumTravelLogEntry = interface(IUnknown)
    ['{7EBFDD85-AD18-11d3-A4C5-00C04F72D6B8}']
    function Next(cElt: ULONG; out rgElt: ITravelLogEntry;
      out pcEltFetched: ULONG): HRESULT; stdcall;
    function Skip(cElt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumTravelLogEntry): HRESULT; stdcall;
  end;

  ITravelLogStg = interface(IUnknown)
    ['{7EBFDD80-AD18-11d3-A4C5-00C04F72D6B8}']
    function CreateEntry(
      pszURL, pszTitle: POleStr;
      ptleRelativeTo: ITravelLogEntry;
      fPrepend: BOOL;
      out pptle: ITravelLogEntry): HRESULT; stdcall;
    function TravelTo(ptle: ITravelLogEntry): HRESULT; stdcall;
    function EnumEntries(
      Flags: DWORD;
      out ppenum: IEnumTravellogEntry): HRESULT; stdcall;
    function FindEntries(
      Flags: DWORD;
      pszURL: POleStr;
      out ppenum: IEnumTravelLogEntry): HRESULT; stdcall;
    function GetCount(Flags: DWORD; out pcEntries: DWORD): HRESULT; stdcall;
    function RemoveEntry(ptle: ITravelLogEntry): HRESULT; stdcall;
    function GetRelativeEntry(
      iOffset: Integer;
      out ptle: ITravelLogEntry): HRESULT; stdcall;
  end;

  TWinWebBrowserService = class;

  TWebBrowser = class(SHDocVw.TWebBrowser, IOleCommandTarget, IDocHostUIHandler)
  public type
    TCanNavigate =
      procedure (
        Sender: TObject;
        var ioURL: String;
        out iCanNavigate: Boolean) of object;
  private const
    IID_DocHostCommandHandler: TGUID = '{F38BC242-B950-11D1-8918-00C04FC2C836}';
  private var
    FRootFormWnd: HWND;
    FOldWndProc: Pointer;
    FIEWnd: HWND;
    FOnShouldStartLoadWithRequest: TWebBrowserShouldStartLoadWithRequest;
  protected
    function CheckIEWnd: Boolean;
    {IOleCommandTarget interface}
    function QueryStatus(
      CmdGroup: PGUID;
      cCmds: Cardinal;
      prgCmds: POleCmd;
      CmdText: POleCmdText): HResult; stdcall;
    function Exec(
      CmdGroup: PGUID;
      nCmdID, nCmdexecopt: DWORD;
      const vaIn: OleVariant;
      var vaOut: OleVariant): HResult; stdcall;
    {IDocHostUIHandler}
    function ShowContextMenu(
      dwID: UINT;
      ppt: PtagPOINT;
      const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HResult; stdcall;
    function GetHostInfo(var pInfo: _DOCHOSTUIINFO): HResult; stdcall;
    function ShowUI(
      dwID: UINT;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget;
      const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
    function HideUI: HResult; stdcall;
    function UpdateUI: HResult; stdcall;
    function EnableModeless(fEnable: Integer): HResult; stdcall;
    function OnDocWindowActivate(fActivate: Integer): HResult; stdcall;
    function OnFrameWindowActivate(fActivate: Integer): HResult; stdcall;
    function ResizeBorder(
      var prcBorder: tagRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      fRameWindow: Integer): HResult; stdcall;
    function TranslateAccelerator(
      var lpmsg: tagMSG;
      var pguidCmdGroup: TGUID;
      nCmdID: UINT): HResult; stdcall;
    function GetOptionKeyPath(
      out pchKey: PWideChar;
      dw: UINT): HResult; stdcall;
    function GetDropTarget(
      const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
    function TranslateUrl(
      dwTranslate: UINT;
      pchURLIn: PWideChar;
      out ppchURLOut: PWideChar): HResult; stdcall;
    function FilterDataObject(
      const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
  public
    property OnShouldStartLoadWithRequest: TWebBrowserShouldStartLoadWithRequest
      read FOnShouldStartLoadWithRequest
      write FOnShouldStartLoadWithRequest;
  end;

  TWinWebBrowserService =
    class(TInterfacedObject, ICustomBrowser, IWebBrowserEx)
  private const
    MAX_WAIT_TIME = 1000 * 10;
  private var
    FURL: String;
    FWebView: TWebBrowser;
    FWebControl: TCustomWebBrowser;
    FTravelLog: ITravelLogStg;
    FLoading: Boolean;
    FEnableCaching: Boolean;
  private
    procedure WebViewBeforeNavigate2(
      ASender: TObject;
      const pDisp: IDispatch;
      const URL: OleVariant;
      const Flags: OleVariant;
      const TargetFrameName: OleVariant;
      const PostData: OleVariant;
      const Headers: OleVariant;
      var Cancel: WordBool);
    procedure WebViewDocumentComplete(
      ASender: TObject;
      const pDisp: IDispatch;
      const URL: OleVariant);
    procedure WebViewNavigateEror(
      ASender: TObject;
      const pDisp: IDispatch;
      const URL: OleVariant;
      const Frame: OleVariant;
      const StatusCode: OleVariant;
      var Cancel: WordBool);
    procedure WebViewShouldStartLoadWithRequest(
      ASender: TObject;
      const URL: String);
  protected
    procedure GetTravelLog;
    function CanGo(const iFlag: DWORD): Boolean;
    { ICustomBrowser }
    function GetURL: string;
    function CaptureBitmap: FMX.Graphics.TBitmap;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value : Boolean);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    function GetParent: TFmxObject;
    function GetVisible : Boolean;
    procedure UpdateContentFromControl;
    procedure Navigate;
    procedure Reload;
    procedure Stop;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure LoadFromStrings(const Content: string; const BaseUrl: string);
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure Show;
    procedure Hide;
    property URL: string read GetURL write SetURL;
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    { IWebBrowserEx }
    function GetTagValue(const iTagName, iValueName: String): String;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWinWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

  TWebBrowserList = TList<TWebBrowser>;

var
  GWBService: TWinWBService;
  GWebViews: TWebBrowserList = nil;

{ TWinWBService }

function TWinWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TWinWebBrowserService.Create as ICustomBrowser;
end;

procedure RegisterWebBrowserService;
begin
  GWBService := TWinWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, GWBService);
end;

procedure UnRegisterWebBrowserService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

{ TWebBrowser }

type
  PEnumClassName = ^TEnumClassName;
  TEnumClassName = record
    WndHandle: HWND;
    ClassName: String;
  end;

function EnumChildProc(iWnd: HWND; iLParam: LPARAM): BOOL; stdcall;
var
  tmpName: String;
begin
  Result := True;

  SetLength(tmpName, $100);
  SetLength(
    tmpName,
    GetClassName(iWnd, PChar(tmpName), Length(tmpName) - 1));
  tmpName := tmpName.Trim;

  with PEnumClassName(iLParam)^ do begin
    if ((ClassName = '') or (ClassName = tmpName)) then begin
      Result := False;

      WndHandle := iWnd;
    end;
  end;
end;

function GetChildWindow(const iTopLevel: HWND; const iClassName: String): HWND;
var
  CN: TEnumClassName;
begin
  with CN do begin
    WndHandle := 0;
    ClassName := iClassName;
  end;

  EnumChildWindows(iTopLevel, @EnumChildProc, LPARAM(@CN));

  Result := CN.WndHandle;
end;

function FormWndProc(
  iWnd: HWND;
  iMsg: DWORD;
  iwParam: WPARAM;
  ilParam: LPARAM): LRESULT; stdcall;
var
  WebView: TWebBrowser;
  Found: TWebBrowser;
begin
  Found := nil;
  for WebView in GWebViews do
    if (WebView.FRootFormWnd = iWnd) then begin
      Found := WebView;
      Break;
    end;

  case iMsg of
    WM_LBUTTONDOWN: begin
      if (Found <> nil) and (Found.CheckIEWnd) then
        Winapi.Windows.SetFocus(iWnd);
    end;
  end;

  if (Found = nil) then
    Result := 0
  else
    Result :=
      CallWindowProc(Found.FOldWndProc, iWnd, iMsg, iwParam, ilParam);
end;

function TWebBrowser.CheckIEWnd: Boolean;
begin
  if (csDestroying in ComponentState) or (IsWindow(FIEWnd)) then
    Exit(True);

  FIEWnd := GetChildWindow(Handle, 'Internet Explorer_Server');

  Result := IsWindow(FIEWnd);
end;

function TWebBrowser.EnableModeless(fEnable: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.Exec(
  CmdGroup: PGUID;
  nCmdID, nCmdexecopt: DWORD;
  const vaIn: OleVariant;
  var vaOut: OleVariant): HResult;
begin
  Result := OLECMDERR_E_NOTSUPPORTED;

  if (CmdGroup <> nil) then begin
    if (IsEqualGuid(cmdGroup^, IID_DocHostCommandHandler)) then
      case nCmdID of
        OLECMDID_SHOWSCRIPTERROR:
          begin
            vaOut := True;
            Result := S_OK;
          end;
      end;
  end;
end;

function TWebBrowser.FilterDataObject(
  const pDO: IDataObject;
  out ppDORet: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.GetDropTarget(
  const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.GetExternal(out ppDispatch: IDispatch): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.GetHostInfo(var pInfo: _DOCHOSTUIINFO): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.GetOptionKeyPath(out pchKey: PWideChar; dw: UINT): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.HideUI: HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.OnDocWindowActivate(fActivate: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.OnFrameWindowActivate(fActivate: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.QueryStatus(
  CmdGroup: PGUID;
  cCmds: Cardinal;
  prgCmds: POleCmd;
  CmdText: POleCmdText): HResult;
begin
  prgCmds.cmdf := OLECMDF_ENABLED;
  Result := S_OK;
end;

function TWebBrowser.ResizeBorder(var prcBorder: tagRECT;
  const pUIWindow: IOleInPlaceUIWindow;
  fRameWindow: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.ShowContextMenu(
  dwID: UINT;
  ppt: PtagPOINT;
  const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.ShowUI(
  dwID: UINT;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget;
  const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.TranslateAccelerator(
  var lpmsg: tagMSG;
  var pguidCmdGroup: TGUID;
  nCmdID: UINT): HResult;
begin
  Result := E_NOTIMPL;
end;

function TWebBrowser.TranslateUrl(
  dwTranslate: UINT;
  pchURLIn: PWideChar;
  out ppchURLOut: PWideChar): HResult;
begin
  if (Assigned(FOnShouldStartLoadWithRequest)) then
    FOnShouldStartLoadWithRequest(Self, String(pchURLIn));

  Result := E_NOTIMPL;
end;

function TWebBrowser.UpdateUI: HResult;
begin
  Result := E_NOTIMPL;
end;

{ TWinWebBrowserService }

function TWinWebBrowserService.CanGo(const iFlag: DWORD): Boolean;
var
  Count: DWORD;
begin
  Result := False;

  if (FTravelLog <> nil) and (Succeeded(FTravelLog.GetCount(iFlag, Count))) then
    Result := (Count > 0);
end;

function TWinWebBrowserService.CaptureBitmap: FMX.Graphics.TBitmap;
var
  HTMLDoc: IHTMLDocument2;
  Body: IHTMLElement;
  ResultBmp: TBitmap;
  Stream: TMemoryStream;
begin
  Result := FMX.Graphics.TBitmap.Create;

  if
    (FWebView = nil) or
    (FWebView.Document = nil) or
    (FWebView.Document.QueryInterface(IHTMLDocument2, HTMLDoc) <> S_OK)
  then
    Exit;

  Body := HTMLDoc.body;

  ResultBmp := TBitmap.Create;
  try
    ResultBmp.SetSize(FWebView.Width, FWebView.Height);

    OleDraw(
      FWebView.ControlInterface,
      DVASPECT_DOCPRINT,
      ResultBmp.Canvas.Handle,
      Rect(0, 0, ResultBmp.Width, ResultBmp.Height));

    Stream := TMemoryStream.Create;
    try
      ResultBmp.SaveToStream(Stream);
      Stream.Position := 0;
      Result.LoadFromStream(Stream);
    finally
      Stream.DisposeOf;
    end;
  finally
    ResultBmp.DisposeOf;
  end;
end;

constructor TWinWebBrowserService.Create;
begin
  inherited;

  if (GWebViews = nil) then
    GWebViews := TWebBrowserList.Create;

  FWebView := TWebBrowser.Create(nil);
  FWebView.Align := alClient;
  FWebView.Ctl3D := False;
  FWebView.OnBeforeNavigate2 := WebViewBeforeNavigate2;
  FWebView.OnDocumentComplete := WebViewDocumentComplete;
  FWebView.OnNavigateError := WebViewNavigateEror;
  FWebView.OnShouldStartLoadWithRequest := WebViewShouldStartLoadWithRequest;

  GWebViews.Add(FWebView);
end;

destructor TWinWebBrowserService.Destroy;
begin
  if (Vcl.Forms.Application <> nil) then begin
    GWebViews.Remove(FWebView);

    if (FWebView.FOldWndProc <> nil) then
      SetWindowLong(
        FWebView.FRootFormWnd,
        GWL_WNDPROC,
        Integer(FWebView.FOldWndProc));

    FWebView.DisposeOf;
  end;

  inherited;
end;

procedure TWinWebBrowserService.EvaluateJavaScript(const JavaScript: String);
var
  Doc: IHTMLDocument2;
  Win: IHTMLWindow2;
begin
  Doc := FWebView.Document as IHTMLDocument2;

  if (Doc <> nil) then begin
    Win := Doc.parentWindow;
    if (Win <> nil) then begin
      try
        Win.execScript(JavaScript, 'JavaScript');
      except
      end;
    end;
  end;
end;

function TWinWebBrowserService.GetCanGoBack: Boolean;
begin
  Result := CanGo(TLEF_RELATIVE_BACK);
end;

function TWinWebBrowserService.GetCanGoForward: Boolean;
begin
  Result := CanGo(TLEF_RELATIVE_FORE);
end;

function TWinWebBrowserService.GetEnableCaching: Boolean;
begin
  Result := FEnableCaching;
end;

function TWinWebBrowserService.GetParent: TFmxObject;
begin
  if (FWebControl = nil) then
    Result := nil
  else
    Result := FWebControl.Parent;
end;

function TWinWebBrowserService.GetTagValue(
  const iTagName, iValueName: String): String;
var
  Doc: IHTMLDocument3;
  Elem: IHTMLElement;
begin
  if
    (FWebView = nil)
    or (FWebView.Document = nil)
    or (FWebView.Document.QueryInterface(IHTMLDocument3, Doc) <> S_OK)
  then
    Exit;

  Elem := Doc.getElementById(iTagName);
  if (Elem = nil) then
    Exit;

  Result := Elem.getAttribute(iValueName, 0);
end;

procedure TWinWebBrowserService.GetTravelLog;
var
  SP: IServiceProvider;
begin
  if
    (FWebView.DefaultInterface = nil)
    or (Failed(FWebView.Application.QueryInterface(IServiceProvider, SP)))
  then
    Exit;

  SP.QueryInterface(ITravelLogEntry, FTravelLog);
end;

function TWinWebBrowserService.GetURL: string;
begin
  Result := FURL;
end;

function TWinWebBrowserService.GetVisible: Boolean;
begin
  if (FWebView = nil) then
    Result := False
  else
    Result := FWebView.Visible;
end;

procedure TWinWebBrowserService.GoBack;
begin
  if (FWebView <> nil) then
    FWebView.GoBack;
end;

procedure TWinWebBrowserService.GoForward;
begin
  if (FWebView <> nil) then
    FWebView.GoForward;
end;

procedure TWinWebBrowserService.GoHome;
begin
  if (FWebView <> nil) then
    FWebView.GoHome;
end;

procedure TWinWebBrowserService.Hide;
begin
  if (FWebView <> nil) then
    FWebView.Hide;
end;

procedure TWinWebBrowserService.LoadFromStrings(const Content, BaseUrl: String);
var
  StreamInit: IPersistStreamInit;
  ContentStream: TStringStream;
begin
  if (FWebView.Document = nil) then
    FWebView.Navigate('about:blank'); // DO NOT LOCALIZE

  if
    (FWebView.Document <> nil) and
    (FWebView.Document.QueryInterface(IPersistStreamInit, StreamInit) = S_OK)
  then begin
    ContentStream := TStringStream.Create(Content);
    try
      // The instance of TStreamAdapter is released automatically
      StreamInit.InitNew;
      StreamInit.Load(TStreamAdapter.Create(ContentStream));
    finally
      ContentStream.DisposeOf;
    end;
  end;
end;

procedure TWinWebBrowserService.Navigate;
begin
  if (FWebView <> nil) then begin
    FLoading := True;

    if (FEnableCaching) then
      FWebView.Navigate2(FURL, navNoReadFromCache)
    else
      FWebView.Navigate2(FURL);

    if (FURL.StartsWith('http')) or (FURL.StartsWith('file')) then
      TThread.CreateAnonymousThread(
        procedure
        var
          Start: TDateTime;
        begin
          try
            Start := Now;
            while
              (not Application.Terminated)
              and (FWebView.ReadyState < READYSTATE_COMPLETE)
              and (MilliSecondsBetween(Now, Start) < MAX_WAIT_TIME)
            do begin
              TThread.Sleep(100);
            end;
          except
          end;

          FLoading := False;
        end
      ).Start
    else
      FLoading := False;
  end;
end;

procedure TWinWebBrowserService.Reload;
begin
  if (FWebView <> nil) then
    FWebView.Refresh;
end;

procedure TWinWebBrowserService.SetEnableCaching(const Value: Boolean);
begin
  FEnableCaching := Value;
end;

procedure TWinWebBrowserService.SetURL(const AValue: string);
begin
  if (FLoading) then
    Exit;

  FURL := Avalue;
end;

procedure TWinWebBrowserService.SetWebBrowserControl(
  const AValue: TCustomWebBrowser);
var
  Form: TCommonCustomForm;
  RootForm: TCommonCustomForm;
  i: Integer;
begin
  FWebControl := AValue;
  RootForm := nil;

  if (FWebControl <> nil) and (FWebControl.Root <> nil) then
    for i := 0 to FMX.Forms.Screen.FormCount - 1 do begin
      Form := FMX.Forms.Screen.Forms[i];

      if (Form = FWebControl.Root.GetObject) then begin
        RootForm := Form;
        Break;
      end;
    end;

  if (RootForm <> nil) then begin
    FWebView.FRootFormWnd := FormToHWND(RootForm);
    FWebView.FOldWndProc :=
      Pointer(
        SetWindowLong(
          FWebView.FRootFormWnd,
          GWL_WNDPROC,
          Integer(@FormWndProc)
        )
      );
  end;

  UpdateContentFromControl;
end;

procedure TWinWebBrowserService.Show;
begin
  if (FWebView <> nil) then
    FWebView.Show;
end;

procedure TWinWebBrowserService.Stop;
begin
  if (FWebView <> nil) then
    FWebView.Stop;
end;

procedure TWinWebBrowserService.UpdateContentFromControl;
var
  Bounds: TRectF;
  RootForm: TCommonCustomForm;
  Wnd: HWND;
begin
  if (FWebView <> nil) then begin
    if (FWebControl <> nil)
      and not (csDesigning in FWebControl.ComponentState)
      and (FWebControl.Root <> nil)
      and (FWebControl.Root.GetObject is TCommonCustomForm)
    then begin
      RootForm := TCommonCustomForm(FWebControl.Root.GetObject);

      Bounds := TRectF.Create(0, 0, FWebControl.Width, FWebControl.Height);
      Bounds.Fit(FWebControl.AbsoluteRect);

      Wnd := WindowHandleToPlatform(RootForm.Handle).Wnd;

      if
        (Wnd = 0)
        or (SameValue(Bounds.Width, 0))
        or (SameValue(Bounds.Height, 0))
      then
        Hide
      else begin
        TWinControl(FWebView).ParentWindow := Wnd;

        FWebView.SetBounds(
          Trunc(Bounds.Left)
          , Trunc(Bounds.Top)
          , Trunc(Bounds.Width)
          , Trunc(Bounds.Height)
        );

        if (FWebControl.Visible) then
          Show;
      end;

      GetTravelLog;
    end
    else begin
      FWebView.ParentWindow := 0;
      Hide;
    end;
  end;
end;

procedure TWinWebBrowserService.WebViewBeforeNavigate2(
  ASender: TObject;
  const pDisp: IDispatch;
  const URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant;
  var Cancel: WordBool);
begin
  if (FWebControl <> nil) then
    FWebControl.StartLoading;
end;

procedure TWinWebBrowserService.WebViewDocumentComplete(
  ASender: TObject;
  const pDisp: IDispatch;
  const URL: OleVariant);
begin
  if (FWebControl <> nil) then
    FWebControl.FinishLoading;
end;

procedure TWinWebBrowserService.WebViewNavigateEror(
  ASender: TObject;
  const pDisp: IDispatch;
  const URL, Frame, StatusCode: OleVariant;
  var Cancel: WordBool);
begin
  if (FWebControl <> nil) then
    FWebControl.FailLoadingWithError;
end;

procedure TWinWebBrowserService.WebViewShouldStartLoadWithRequest(
  ASender: TObject; const URL: String);
begin
  if (FWebControl <> nil) then
    FWebControl.ShouldStartLoading(URL);
end;

procedure FreeWebBrowsers;
var
  Web: TWebBrowser;
begin
  if (GWebViews <> nil) then
    for Web in GWebViews do
      Web.DisposeOf;

  GWebViews.DisposeOf;
end;

initialization
begin
  CoInitialize(nil);
  RegisterWebBrowserService;
end;

finalization
begin
  FreeWebBrowsers;
  CoUninitialize;
end;

end.
