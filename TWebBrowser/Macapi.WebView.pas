(*
 * TWebBrowserEx Classes
 *   WebBrowser Componet for FireMonkey.
 *
 * Copyright (c) 2013, 2015 HOSOKAWA Jun.
 *
 * Last Update 2015/10/29
 *
 * Platform:
 *   Windows, OS X, iOS, Android
 *   Delphi / C++Builder XE5, XE6, XE7, XE8, 10 seattle
 *   Appmethod 1.14, 1.15, 1.16, 1.17
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

unit Macapi.WebView;

{$IF not defined(MACOS) or defined(IOS)}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

uses
  System.Classes
  , Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit
  , FMX.Types, FMX.Controls
  ;

const
  WEBKIT_FRAMEWORK: String =
    '/System/Library/Frameworks/WebKit.framework/WebKit';

type
  TWebKitError = record
  public const
    CannotShowMIMEType = 100;
    CannotShowURL = 101;
    FrameLoadInterruptedByPolicyChange = 102;
    CannotFindPlugIn = 200;
    CannotLoadPlugIn = 201;
    JavaUnavailable = 202;
    BlockedPlugInVersion = 203;
  end;

  TWebNavigationType = record
  public const
    LinkClicked = 0;
    FormSubmitted = 1;
    BackForward = 2;
    Reload = 3;
    FormResubmitted = 4;
    Other = 5;
  end;

  TWebMenuItem = record
  public const
    TagOpenLinkInNewWindow = 1;
    TagDownloadLinkToDisk = 2;
    TagCopyLinkToClipboard = 3;
    TagOpenImageInNewWindow = 4;
    TagDownloadImageToDisk = 5;
    TagCopyImageToClipboard = 6;
    TagOpenFrameInNewWindow = 7;
    TagCopy = 8;
    TagGoBack = 9;
    TagGoForward = 10;
    TagStop = 11;
    TagReload = 12;
    TagCut = 13;
    TagPaste = 14;
    TagSpellingGuess = 15;
    TagNoGuessesFound = 16;
    TagIgnoreSpelling = 17;
    TagLearnSpelling = 18;
    TagOther = 19;
    TagSearchInSpotlight = 20;
    TagSearchWeb = 21;
    TagLookUpInDictionary = 22;
    TagOpenWithDefaultApplication = 23;
    PDFActualSize = 24;
    PDFZoomIn = 25;
    PDFZoomOut = 26;
    PDFAutoSize = 27;
    PDFSinglePage = 28;
    PDFFacingPages = 29;
    PDFContinuous = 30;
    PDFNextPage = 31;
    PDFPreviousPage = 32;
  end;

  TWebDragDestinationAction = record
  public const
    None = 0;
    DHTML = 1;
    Edit = 2;
    Load = 4;
    Any = 429496729;
  end;

  TWebDragSourceAction = record
  public const
    None = 0;
    DHTML = 1;
    Image = 2;
    Link = 4;
    Selection = 8;
    Any = 429496729;
  end;

  TWebCacheModel = record
  public const
     DocumentViewer = 0;
     DocumentBrowser = 1;
     PrimaryWebBrowser = 2;
  end;

  TWebActionKeys = record
  public const
    NavigationTypeKey = 'WebActionNavigationTypeKey';
    ElementKey = 'WebActionElementKey';
    ButtonKey = 'WebActionButtonKey';
    ModifierFlagsKey = 'WebActionModifierFlagsKey';
    OriginalURLKey = 'WebActionOriginalURLKey';
  public
    class function GetNavigationTypeKey: NSString; static;
    class function GetElementKey: NSString; static;
    class function GetButtonKey: NSString; static;
    class function GetModifierFlagsKey: NSString; static;
    class function GetOriginalURLKey: NSString; static;

    class function GetNavigationTypeKeyObj: Pointer; static;
    class function GetElementKeyObj: Pointer; static;
    class function GetButtonKeyObj: Pointer; static;
    class function GetModifierFlagsKeyObj: Pointer; static;
    class function GetOriginalURLKeyObj: Pointer; static;
  end;

  TWebElement = record
  public const
    DOMNodeKey = 'WebElementDOMNodeKey';
    FrameKey = 'WebElementFrameKey';
    ImageAltStringKey = 'WebElementImageAltStringKey';
    ImageKey = 'WebElementImageKey';
    ImageRectKey = 'WebElementImageRectKey';
    ImageURLKey = 'WebElementImageURLKey';
    IsSelectedKey = 'WebElementIsSelectedKey';
    LinkURLKey = 'WebElementLinkURLKey';
    LinkTargetFrameKey = 'WebElementLinkTargetFrameKey';
    LinkTitleKey = 'WebElementLinkTitleKey';
    LinkLabelKey = 'WebElementLinkLabelKey';
  public
    class function GetDOMNodeKey: NSString; static;
    class function GetFrameKey: NSString; static;
    class function GetImageAltStringKey: NSString; static;
    class function GetImageKey: NSString; static;
    class function GetImageRectKey: NSString; static;
    class function GetImageURLKey: NSString; static;
    class function GetIsSelectedKey: NSString; static;
    class function GetLinkURLKey: NSString; static;
    class function GetLinkTargetFrameKey: NSString; static;
    class function GetLinkTitleKey: NSString; static;
    class function GetLinkLabelKey: NSString; static;

    class function GetDOMNodeKeyObj: Pointer; static;
    class function GetFrameKeyObj: Pointer; static;
    class function GetImageAltStringKeyObj: Pointer; static;
    class function GetImageKeyObj: Pointer; static;
    class function GetImageRectKeyObj: Pointer; static;
    class function GetImageURLKeyObj: Pointer; static;
    class function GetIsSelectedKeyObj: Pointer; static;
    class function GetLinkURLKeyObj: Pointer; static;
    class function GetLinkTargetFrameKeyObj: Pointer; static;
    class function GetLinkTitleKeyObj: Pointer; static;
    class function GetLinkLabelKeyObj: Pointer; static;
  end;

type
  WebFrame = interface;
  WebView = interface;
  WebFrameView = interface;
  WebDocumentView = interface;
  WebDataSource = interface;
  WebResource = interface;
  WebDocumentRepresentation = interface;
  WebArchive = interface;
  DOMDocument = interface;
  WebScriptObject = interface;
  WebBackForwardList = interface;
  WebHistoryItem = interface;
  WebPreferences = interface;

  WebCacheModel = NSUInteger;

  WebResource = interface(NSObject)
    ['{616664A6-28D7-49F4-82D4-67A9C9B0ABBE}']
    function data: NSData; cdecl;
    function frameName: NSString; cdecl;
    function initWithData(
      data: NSData;
      URL: NSURL;
      MIMEType: NSString;
      textEncodingName: NSString;
      frameName: NSString): Pointer; cdecl;
    function MIMEType: NSString; cdecl;
    function textEncodingName: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;

  WebDocumentRepresentation = interface(NSObject)
    ['{F1996B50-2F93-4DE3-84AB-42F588AFE74A}']
    function canProvideDocumentSource: Boolean; cdecl;
    function documentSource: NSString; cdecl;
    procedure finishedLoadingWithDataSource(dataSource: WebDataSource); cdecl;
    procedure receivedData(data: NSData; dataSource: WebDataSource); cdecl;
    procedure receivedError(error: NSError; dataSource: WebDataSource); cdecl;
    procedure setDataSource(dataSource: WebDataSource); cdecl;
    function title: NSString; cdecl;
  end;

  WebArchive = interface(NSObject)
    ['{182A3424-31CE-4E93-BD4E-736859206529}']
    function data: NSData; cdecl;
    function initWithData(data: NSData): Pointer; cdecl;
    function initWithMainResource(
      mainResource: WebResource;
      subresources: NSArray;
      subframeArchives: NSArray): Pointer; cdecl;
    function mainResource: WebResource; cdecl;
    function subframeArchives: NSArray; cdecl;
    function subresources: NSArray; cdecl;
  end;

  WebDataSource = interface(NSObject)
    ['{D138FA02-49A7-4F29-9AE0-EEC381C1AFED}']
    procedure addSubresource(subresource: WebResource); cdecl;
    function data: NSData; cdecl;
    function initialRequest: NSURLRequest; cdecl;
    function initWithRequest(request: NSURLRequest): Pointer; cdecl;
    function isLoading: Boolean; cdecl;
    function mainResource: WebResource; cdecl;
    function pageTitle: NSString; cdecl;
    function representation: WebDocumentRepresentation; cdecl;
    function request: NSMutableURLRequest; cdecl;
    function response: NSURLResponse; cdecl;
    function subresourceForURL(URL: NSURL): WebResource; cdecl;
    function subSOurces: NSArray; cdecl;
    function textEncodingName: NSString; cdecl;
    function unreachableURL: NSURL; cdecl;
    function webArchive: WebArchive; cdecl;
    function webFrame: WebFrame; cdecl;
  end;

  WebDocumentView = interface(NSObject)
    ['{1175C5ED-14A6-4115-A3C3-547650316EAC}']
    procedure dataSourceUpdate(dataSource: WebDataSource); cdecl;
    procedure layout; cdecl;
    procedure setDataSource(dataSource: WebDataSource); cdecl;
    procedure setNeedsLayout(flag: Boolean); cdecl;
    procedure viewDidMoveToHostWindow; cdecl;
    procedure viewWillMoveToHostWindow(hostWindow: NSWindow); cdecl;
  end;

  WebFrameView = interface(NSView)
    ['{348DEC2C-1594-4C5F-B625-4FA382090A49}']
    function allowScrolling: Boolean; cdecl;
    function canPrintHeadersAndFooters: Boolean; cdecl;
    function documentView: WebDocumentView; cdecl;
    function documentViewShouldHandlePrint: Boolean; cdecl;
    procedure printDocumentView; cdecl;
    function printOperationWithPrintInfo(
      printInfo: NSPrintInfo): NSPrintOperation; cdecl;
    procedure setAllowsScrolling(flag: Boolean); cdecl;
    function webFrame: WebFrame; cdecl;
  end;


  WebFrameClass = interface(NSObjectClass)
    ['{7BE750C8-DFEC-4870-851A-12DBCB0B78F6}']
  end;

  WebFrame = interface(NSObject)
    ['{BCFA04BE-41AB-4B78-89C0-3330F12C7695}']
    function childDrames: NSArray; cdecl;
    function dataSource: WebDataSource; cdecl;
    function DOMDocument: DOMDocument; cdecl;
    function findFrameNamed(name: NSString): WebFrame; cdecl;
    // function frameElement: DOMHtmlElement; cdecl;
    function frameView: WebFrameView; cdecl;
    // function globalContext: JSGlobalContextRef; cdecl;
    function initWithFrame(
      frameName: NSString;
      view: WebFrameView;
      webView: WebView): Pointer; cdecl;
    procedure loadAlternateHTMLString(
      string_: NSString;
      URL: NSURL;
      unreachableURL: NSURL); cdecl;
    procedure loadArchive(archive: WebArchive); cdecl;
    procedure loadData(
      data: NSData;
      MIMEType: NSString;
      encodingName: NSString;
      URL: NSURL); cdecl;
    procedure loadHTMLString(string_: NSString; baseURL: NSURL); cdecl;
    procedure loadRequest(request: NSURLRequest); cdecl;
    function name: NSString; cdecl;
    function parentFrame: WebFrame; cdecl;
    function provisionalDataSource: WebDataSource; cdecl;
    procedure reload; cdecl;
    procedure reloadFromOrigin; cdecl;
    procedure stopLoading; cdecl;
    function webView: WebView; cdecl;
    function windowObject: WebScriptObject; cdecl;
  end;

  DOMDocument = interface(NSObject)
    function URLWithAttributeString(string_: NSString): NSURL; cdecl;
    function webFrame: WebFrame; cdecl;
  end;

  WebScriptObjectClass = interface(NSObjectClass)
    {class} function throwException(exceptionMessage: NSString): Boolean; cdecl;
  end;

  WebScriptObject = interface(NSObject)
    function callWebScriptMethod(name: NSString; args: NSArray): Pointer; cdecl;
    function evaluateWebScript(script: NSString): Pointer; cdecl;
    // function JSObject: JSObjectRec; cdecl;
    procedure removeWebScriptKey(name: NSString); cdecl;
    procedure setException(description: NSString); cdecl;
    procedure setWebScriptValueAtIndex(index: UInt32; value: Pointer); cdecl;
    function stringRepresentation: NSString; cdecl;
    function webScriptValueAtIndex(index: UInt32): Pointer; cdecl;
  end;

  WebPolicyDelegate = interface(IObjectiveC)
    ['{0BE9E848-E8DF-4736-B10B-63B565D3EA6D}']
    [MethodName('webView:decidePolicyForNavigationAction:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForNavigationActionRequestFrameDecisionListener(
      WebView: WebView;
      decidePolicyForNavigationAction: NSDictionary;
      request: NSURLRequest;
      frame: WebFrame;
      decisionListener: Pointer); cdecl;
    [MethodName('webView:decidePolicyForNewWindowAction:request:newFrameName:decisionListener:')]
    procedure webViewDecidePolicyForNewWindowActionRequestNewFrameNameDecisionListener(
      WebView: WebView;
      decidePolicyForNewWindowAction: NSDictionary;
      request: NSURLRequest;
      newFrameName: NSString;
      decisionListener: Pointer); cdecl;
    [MethodName('webView:decidePolicyForMIMEType:request:frame:decisionListener:')]
    procedure webViewDecidePolicyForMIMETypeRequestFrameDecisionListener(
      WebView: WebView;
      decidePolicyForMIMEType: NSString;
      request: NSURLRequest;
      frame: WebFrame;
      decisionListener: Pointer); cdecl;
    [MethodName('webView:unableToImplementPolicyWithError:frame:')]
    procedure webViewUnableToImplementPolicyWithErrorFrame(
      WebView: WebView;
      unableToImplementPolicyWithError: NSError;
      frame: WebFrame); cdecl;
  end;

  WebPolicyDecisionListener = interface(IObjectiveCInstance)
    ['{3D5D132B-B8C7-4227-A414-F590E93477F2}']
    procedure use; cdecl;
    procedure download; cdecl;
    procedure ignore; cdecl;
  end;

  WebViewClass = interface(NSViewClass)
    ['{0D9F44B7-09FD-4E35-B96E-8DB71B9A2537}']
    {class} function canShowMIMEType(MIMEType: NSString): Boolean; cdecl;
    {class} function canShowMIMETypeAsHTML(MIMEType: NSString): Boolean; cdecl;
    {class} function MIMETypesShownAsHTML: NSArray; cdecl;
    {class} procedure registerURLSchemeAsLocal(scheme: NSString); cdecl;
    {class} procedure registerViewClass(
      viewClass: Pointer;
      representationClass: Pointer;
      MIMEType: NSString); cdecl;
    {class} procedure setMIMETypesShownAsHTML(MIMETypes: NSArray); cdecl;
    {class} function URLFromPasteboard(pasteboard: NSPasteboard): NSURL; cdecl;
    {class} function URLTitleFromPasteboard(
      pasteboard: NSPasteBoard): NSString; cdecl;
  end;

  WebView = interface(NSView)
    ['{C36D8016-2FCB-49F0-BA1C-C9913A37F9AC}']
    procedure alignCenter(sender: Pointer); cdecl;
    procedure alignJustified(sender: Pointer); cdecl;
    procedure alignLeft(sender: Pointer); cdecl;
    procedure alignRight(sender: Pointer); cdecl;
    function applicationNameForUserAgent: NSString; cdecl;
    //procedure applyStyle(style: DOMCSSStyleDeclaration ); cdecl;
    function backForwardList: WebBackForwardList; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function canMakeTextLarger: Boolean; cdecl;
    function canMakeTextSmaller: Boolean; cdecl;
    function canMakeTextStandardSize: Boolean; cdecl;
    procedure changeAttributes(sender: Pointer); cdecl;
    procedure changeColor(sender: Pointer); cdecl;
    procedure changeDocumentBackgroundColor(sender: Pointer); cdecl;
    procedure changeFont(sender: Pointer); cdecl;
    procedure checkSpelling(sender: Pointer); cdecl;
    procedure close; cdecl;
    //function computedStyleForElement(
      //element: DOMElement;
      //pseudoElement: NSString): DOMCSSStyleDeclaration; cdecl;
    procedure copy(sender: Pointer); cdecl;
    procedure copyFont(sender: Pointer); cdecl;
    function customTextEncodingName: NSString; cdecl;
    function customUserAgent: NSString; cdecl;
    procedure cut(sender: Pointer); cdecl;
    procedure delete(sender: Pointer); cdecl;
    procedure deleteSelection; cdecl;
    function downloadDelegate: Pointer; cdecl;
    function drawsBackground: Boolean; cdecl;
    //function editableDOMRangeForPoint(point: NSPoint): DOMRange; cdecl;
    function editingDelegate: Pointer; cdecl;
    function elementAtPoint(point: NSPoint): NSDictionary; cdecl;
    function estimatedProgress: Double; cdecl;
    function frameLoadDelegate: Pointer; cdecl;
    function goBack: Boolean; cdecl; overload;
    procedure goBack(sender: Pointer); cdecl; overload;
    function goForward: Boolean; cdecl; overload;
    procedure goForward(sender: Pointer); cdecl; overload;
    function goToBackForwardItem(item: WebHistoryItem): Boolean; cdecl;
    function groupName: NSString; cdecl;
    function hostWindow: NSWindow; cdecl;
    function initWithFrame(
      frame: NSRect;
      frameName: NSString;
      groupName: NSString): Pointer; cdecl;
    function isContinuousSpellCheckingEnabled: Boolean; cdecl;
    function isEditable: Boolean; cdecl;
    function isLoading: Boolean; cdecl;
    function mainFrame: WebFrame; cdecl;
    function mainFrameDocument: DOMDocument; cdecl;
    function mainFrameIcon: NSImage; cdecl;
    function mainFrameTitle: NSString; cdecl;
    function mainFrameURL: NSString; cdecl;
    function maintainsInactiveSelection: Boolean; cdecl;
    procedure makeTextLarger(sender: Pointer); cdecl;
    procedure makeTextSmaller(sender: Pointer); cdecl;
    procedure makeTextStandardSize(sender: Pointer); cdecl;
    function mediaStyle: NSString; cdecl;
    procedure moveDragCaretToPoint(point: NSPoint); cdecl;
    procedure moveToBeginningOfSentence(sender: Pointer); cdecl;
    procedure moveToBeginningOfSentenceAndModifySelection(
      sender: Pointer); cdecl;
    procedure moveToEndOfSentence(sender: Pointer); cdecl;
    procedure moveToEndOfSentenceAndModifySelection(sender: Pointer); cdecl;
    procedure paste(sender: Pointer); cdecl;
    procedure pasteAsPlainText(sender: Pointer); cdecl;
    procedure pasteAsRichText(sender: Pointer); cdecl;
    function pasteboardTypesForElement(element: NSDictionary): NSArray; cdecl;
    function pasteboardTypesForSelection: NSArray; cdecl;
    procedure pasteFont(sender: Pointer);  cdecl;
    procedure performFindPanelAction(sender: Pointer); cdecl;
    function policyDelegate: Pointer; cdecl;
    function preferences: WebPreferences; cdecl;
    function preferencesIdentifier: NSString; cdecl;
    procedure reload(sender: Pointer); cdecl;
    procedure reloadFromOrigin(sender: Pointer); cdecl;
    procedure removeDragCaret; cdecl;
    procedure replaceSelectionWithArchive(archive: WebArchive); cdecl;
    procedure replaceSelectionWithMarkupString(markupString: NSString); cdecl;
    //procedure replaceSelectionWithNode(node: DOMNode); cdecl;
    procedure replaceSelectionWithText(text: NSString); cdecl;
    function resourceLoadDelegate: Pointer; cdecl;
    function searchFor(
      string_: NSString;
      forward_: Boolean;
      caseFlag: Boolean;
      wrapFlag: Boolean): Boolean; cdecl;
    //function selectedDOMRange: DOMRange; cdecl;
    function selectedFrame: WebFrame; cdecl;
    function selectionAffinity: NSSelectionAffinity; cdecl;
    procedure selectSentence(sender: Pointer); cdecl;
    procedure setApplicationNameForUserAgent(applicationName: NSString); cdecl;
    procedure setContinuousSpellCheckingEnabled(flag: Boolean); cdecl;
    procedure setCustomTextEncodingName(encodingName: NSString); cdecl;
    procedure setCustomUserAgent(userAgent: NSString); cdecl;
    procedure setDownloadDelegate(delegate: Pointer); cdecl;
    procedure setDrawsBackground(drawsBackgound: Boolean); cdecl;
    procedure setEditable(flag: Boolean); cdecl;
    procedure setEditingDelegate(delegate: Pointer); cdecl;
    procedure setFrameLoadDelegate(delegate: Pointer); cdecl;
    procedure setGroupName(groupName: NSString); cdecl;
    procedure setHostWindow(hostWindow: NSWindow); cdecl;
    procedure setMainFrameURL(URLString: NSString); cdecl;
    procedure setMaintainsBackForwardList(flag: Boolean); cdecl;
    procedure setMediaStyle(mediaStyle: NSString); cdecl;
    procedure setPolicyDelegate(delegate: Pointer); cdecl;
    procedure setPreferences(preferences: WebPreferences); cdecl;
    procedure setPreferencesIdentifier(anIdentifier: NSString); cdecl;
    procedure setResourceLoadDelegate(delegate: Pointer); cdecl;
    //procedure setSelectedDOMRange(
      //range: DOMRange;
      //selectionAffinity: NSSelectionAffinity); cdecl;
    procedure setShouldCloseWithWindow(close: Boolean); cdecl;
    procedure setShouldUpdateWhileOffscreen(
      updateWhileOffscreen: Boolean); cdecl;
    procedure setSmartInsertDeleteEnabled(flag: Boolean); cdecl;
    procedure setTextSizeMultiplier(multiplier: Single); cdecl;
    //procedure setTypingStyle(style: DOMCSSStyleDeclaratoin); cdecl;
    procedure setUIDelegate(delegate: Pointer); cdecl;
    function shouldCloseWithWindow: Boolean; cdecl;
    function shouldUpdateWhileOffscreen: Boolean; cdecl;
    procedure showGuessPanel(sender: Pointer); cdecl;
    function smartInsertDeleteEnabled: Boolean; cdecl;
    function spellCheckerDocumentTag: NSInteger; cdecl;
    procedure startSpeaking(sender: Pointer); cdecl;
    procedure stopLoading(sender: Pointer); cdecl;
    procedure stopSpeaking(sender: Pointer); cdecl;
    function stringByEvaluatingJavaScriptFromString(
      script: NSString): NSString; cdecl;
    //function styleDeclarationWithText(
      //text: NSString): DOMCSSStyleDeclaration; cdecl;
    function supportsTextEncoding: Boolean; cdecl;
    procedure takeStringURLFrom(sender: Pointer); cdecl;
    function textSizeMultiplier: Single; cdecl;
    procedure toggleContinuousSpellChecking(sender: Pointer);cdecl;
    procedure toggleSmartInsertDelete(sender: Pointer); cdecl;
    //function typingStyle: DOMCSSStyleDeclaration; cdecl;
    function UIDelegate: Pointer; cdecl;
    function undoManager: NSUndoManager; cdecl;
    function userAgentForURL(URL: NSURL): NSString; cdecl;
    function windowScriptObject: WebScriptObject; cdecl;
    procedure writeElement(
      element: NSDictionary;
      types: NSArray;
      pasteboard: NSPasteBoard); cdecl;
    procedure writeSelectionWithPasteboardTypes(
      types: NSArray;
      pasteboard: NSPasteBoard); cdecl;
  end;

  WebBackForwardList = interface(NSObject)
    ['{FAC642D3-A5AC-490F-A1B6-5ACBEB2D6C20}']
    procedure addItem(item: WebHistoryItem); cdecl;
    function backItem: WebHistoryItem; cdecl;
    function backListCount: Int32; cdecl;
    function backListWithLimit(limit: Int32): NSArray; cdecl;
    function capacity: Int32; cdecl;
    function containsItem(item: WebHistoryItem): Boolean; cdecl;
    function currentItem: WebHistoryItem; cdecl;
    function forwardItem: WebHistoryItem; cdecl;
    function forwardListCount: Int32; cdecl;
    function forwardListWithLimit(limit: Int32): NSArray; cdecl;
    procedure goBack; cdecl;
    procedure goForward; cdecl;
    procedure goToItem(item: WebHistoryItem); cdecl;
    function itemAtIndex(index: Int32): WebHistoryItem; cdecl;
    procedure setCapacity(size: Int32); cdecl;
  end;

  WebHistoryItem = interface(NSObject)
    ['{E0642BE4-04F1-40A6-91AD-ABEE665A7717}']
    function alternateTitle: NSString; cdecl;
    function icon: NSImage; cdecl;
    function initWithURLString(
      URLString: NSString;
      title: NSString;
      time: NSTimeInterval): Pointer; cdecl;
    function lastVisitedTimeInterval: NSTImeInterval; cdecl;
    function originalURLString: NSString; cdecl;
    procedure setAlternateTitle(alternateTitle: NSString); cdecl;
    function title: NSString; cdecl;
    function URLString: NSString; cdecl;
  end;

  WebPreferencesClass = interface(NSObjectClass)
    ['{1830695E-5C7A-424A-AABB-1600B7F2DE61}']
    {class} function standardPreferences: WebPreferences; cdecl;
  end;

  WebPreferences = interface(NSObject)
    ['{65D8916F-6503-4EE2-8A8B-3A3CFFF6B133}']
    function allowsAnimatedImageLooping: Boolean; cdecl;
    function allowsAnimatedImages: Boolean; cdecl;
    function arePlugInsEnabled: Boolean; cdecl;
    function autosaves: Boolean; cdecl;
    function cacheModel: WebCacheModel; cdecl;
    function cursiveFontFamily: NSString; cdecl;
    function defaultFixedFontSize: Int32; cdecl;
    function defaultFontSize: Int32; cdecl;
    function defaultTextEncodingName: NSString; cdecl;
    function fantasyFontFamily: NSString; cdecl;
    function fixedFontFamily: NSString; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(anIdentifier: NSString): Pointer; cdecl;
    function isJavaEnabled: Boolean; cdecl;
    function isJavaScriptEnabled: Boolean; cdecl;
    function javaScriptCanOpenWindowsAutomatically: Boolean; cdecl;
    function loadsImagesAutomatically: Boolean; cdecl;
    function minimumFontSize: Int32; cdecl;
    function minimumLogicalFontSize: Int32; cdecl;
    function privateBrowsingEnabled: Boolean; cdecl;
    function sansSerifFontFamily: NSString; cdecl;
    function serifFontFamily: NSString; cdecl;
    procedure setAllowsAnimatedImageLooping(flag: Boolean); cdecl;
    procedure setAllowsAnimatedImages(flag: Boolean); cdecl;
    procedure setAutosaves(flag: Boolean); cdecl;
    procedure setCacheModel(cacheMode: WebCacheModel); cdecl;
    procedure setCursiveFontFamily(family: NSString); cdecl;
    procedure setDefaultFixedFontSize(size: Int32); cdecl;
    procedure setDefaultFontSize(size: Int32); cdecl;
    procedure setDefaultTextEncodingName(encoding: NSString); cdecl;
    procedure setFantasyFontFamily(family: NSString); cdecl;
    procedure setFixedFontFamily(family: NSString); cdecl;
    procedure setJavaEnabled(flag: Boolean); cdecl;
    procedure setJavaScriptCanOpenWindowsAutomatically(flag: Boolean); cdecl;
    procedure setJavaScriptEnabled(flag: Boolean); cdecl;
    procedure setLoadsImagesAutomatically(flag: Boolean); cdecl;
    procedure setMinimumFontSize(size: Int32); cdecl;
    procedure setMinimumLogicalFontSize(size: Int32); cdecl;
    procedure setPlugInsEnabled(flag: Boolean); cdecl;
    procedure setPrivateBrowsingEnabled(flag: Boolean); cdecl;
    procedure setSansSerifFontFamily(family: NSString); cdecl;
    procedure setSerifFontFamily(family: NSString); cdecl;
    procedure setShouldPrintBackgrounds(flag: Boolean); cdecl;
    procedure setStandardFontFamily(family: NSString); cdecl;
    procedure setSuppressesIncrementalRendering(
      suppressesIncrementalRendering: Boolean); cdecl;
    procedure setTabsToLinks(flag: Boolean); cdecl;
    procedure setUserStyleSheetEnabled(flag: Boolean); cdecl;
    procedure setUserStyleSheetLocation(URL: NSURL); cdecl;
    procedure setUsesPageCache(usesPageCache: Boolean); cdecl;
    function shouldPrintBackgrounds: Boolean; cdecl;
    function standardFontFamily: NSString; cdecl;
    function suppressesIncrementalRendering: Boolean; cdecl;
    function tabsToLinks: Boolean; cdecl;
    function userStyleSheetEnabled: Boolean; cdecl;
    function userStyleSheetLocation: NSURL; cdecl;
    function usesPageCache: Boolean; cdecl;
  end;

  // Overload needs TypeName from which a name is different.
  WebFrame2 = type WebFrame;
  WebFrame3 = type WebFrame;
  WebFrame4 = type WebFrame;
  WebFrame5 = type WebFrame;
  WebFrame6 = type WebFrame;
  WebFrame7 = type WebFrame;
  WebFrame8 = type WebFrame;

  WebFrameLoadDelegate = interface(IObjectiveC)
    ['{00FFAF7F-51EC-4F8D-84C2-B0FE4F24270A}']
    procedure webView(
      sender: WebView;
      didStartProvisionalLoadForFrame: WebFrame); overload; cdecl;
    procedure webView(
      sender: WebView;
      didReceiveServerRedirectForProvisionalLoadForFrame: WebFrame2);
      overload; cdecl;
    procedure webView(
      sender: WebView;
      didFailProvisionalLoadWithError: NSError;
      frame: WebFrame); overload; cdecl;
    procedure webView(
      sender: WebView;
      didCommitLoadForFrame: WebFrame3); overload; cdecl;
    procedure webView(
      sender: WebView;
      didReceiveTitle: NSString;
      frame: WebFrame); overload; cdecl;
    procedure webView(
      sender: WebView;
      didReceiveIcon: NSImage;
      frame: WebFrame); overload; cdecl;
    procedure webView(
      sender: WebView;
      didFinishLoadForFrame: WebFrame4); overload; cdecl;
    procedure webView(
      sender: WebView;
      didFailLoadWithError: NSError;
      frame: WebFrame5); overload; cdecl;
    procedure webView(
      sender: WebView;
      didChangeLocationWithinPageForFrame: WebFrame6); overload; cdecl;
    procedure webView(
      sender: WebView;
      willPerformClientRedirectToURL: NSURL;
      seconds: NSTimeInterval;
      date: NSDate;
      frame: WebFrame); overload; cdecl;
    procedure webView(
      sender: WebView;
      didCancelClientRedirectForFrame: WebFrame7); overload; cdecl;
    procedure webView(
      sender: WebView;
      willCloseFrame: WebFrame8); overload; cdecl;
    procedure webView(
      sender: WebView;
      didClearWindowObject: WebScriptObject;
      frame: WebFrame); overload; cdecl;
    //procedure webView(
      //sender: WebView;
      //windowScriptObjectAvailable: WebScriptObject); overload; cdecl;
    //procedure webView(
      //sender: WebView;
      //didCreateJavaScriptContext: JSContext;
      //frame: WebFrame); overload; cdecl;
  end;

  TWebFrame = class(TOCGenericImport<WebFrameClass, WebFrame>)
  end;

  TWebView = class(TOCGenericImport<WebViewClass, WebView>)
  end;

implementation

uses
  Macapi.Helpers;

function KeyToNSStr(const iKey: String): NSString; inline;
begin
  Result := CocoaNSStringConst(WEBKIT_FRAMEWORK, iKey);
end;

{ TWebActionKeys }

class function TWebActionKeys.GetButtonKey: NSString;
begin
  Result := KeyToNSStr(TWebActionKeys.ButtonKey);
end;

class function TWebActionKeys.GetButtonKeyObj: Pointer;
begin
  Result := (GetButtonKey as ILocalObject).GetObjectID;
end;

class function TWebActionKeys.GetElementKey: NSString;
begin
  Result := KeyToNSStr(TWebActionKeys.ElementKey);
end;

class function TWebActionKeys.GetElementKeyObj: Pointer;
begin
  Result := (GetElementKey as ILocalObject).GetObjectID;
end;

class function TWebActionKeys.GetModifierFlagsKey: NSString;
begin
  Result := KeyToNSStr(TWebActionKeys.ModifierFlagsKey);
end;

class function TWebActionKeys.GetModifierFlagsKeyObj: Pointer;
begin
  Result := (GetModifierFlagsKey as ILocalObject).GetObjectID;
end;

class function TWebActionKeys.GetNavigationTypeKey: NSString;
begin
  Result := KeyToNSStr(TWebActionKeys.NavigationTypeKey);
end;

class function TWebActionKeys.GetNavigationTypeKeyObj: Pointer;
begin
  Result := (GetNavigationTypeKey as ILocalObject).GetObjectID;
end;

class function TWebActionKeys.GetOriginalURLKey: NSString;
begin
  Result := KeyToNSStr(TWebActionKeys.OriginalURLKey);
end;

class function TWebActionKeys.GetOriginalURLKeyObj: Pointer;
begin
  Result := (GetOriginalURLKey as ILocalObject).GetObjectID;
end;

{ TWebElement }

class function TWebElement.GetDOMNodeKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.DOMNodeKey);
end;

class function TWebElement.GetDOMNodeKeyObj: Pointer;
begin
  Result := (GetDOMNodeKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetFrameKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.FrameKey);
end;

class function TWebElement.GetFrameKeyObj: Pointer;
begin
  Result := (GetFrameKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetImageAltStringKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.ImageAltStringKey);
end;

class function TWebElement.GetImageAltStringKeyObj: Pointer;
begin
  Result := (GetImageAltStringKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetImageKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.ImageKey);
end;

class function TWebElement.GetImageKeyObj: Pointer;
begin
  Result := (GetImageKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetImageRectKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.ImageRectKey);
end;

class function TWebElement.GetImageRectKeyObj: Pointer;
begin
  Result := (GetImageRectKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetImageURLKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.ImageURLKey);
end;

class function TWebElement.GetImageURLKeyObj: Pointer;
begin
  Result := (GetImageURLKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetIsSelectedKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.IsSelectedKey);
end;

class function TWebElement.GetIsSelectedKeyObj: Pointer;
begin
  Result := (GetIsSelectedKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetLinkLabelKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.LinkLabelKey);
end;

class function TWebElement.GetLinkLabelKeyObj: Pointer;
begin
  Result := (GetLinkLabelKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetLinkTargetFrameKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.LinkTargetFrameKey);
end;

class function TWebElement.GetLinkTargetFrameKeyObj: Pointer;
begin
  Result := (GetLinkTargetFrameKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetLinkTitleKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.LinkTitleKey);
end;

class function TWebElement.GetLinkTitleKeyObj: Pointer;
begin
  Result := (GetLinkTitleKey as ILocalObject).GetObjectID;
end;

class function TWebElement.GetLinkURLKey: NSString;
begin
  Result := KeyToNSStr(TWebElement.LinkURLKey);
end;

class function TWebElement.GetLinkURLKeyObj: Pointer;
begin
  Result := (GetLinkURLKey as ILocalObject).GetObjectID;
end;

end.
