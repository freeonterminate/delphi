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

  WebFrameView = interface(NSObject)
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

  TWebCacheModel = (
     WebCacheModelDocumentViewer = 0,
     WebCacheModelDocumentBrowser = 1,
     WebCacheModelPrimaryWebBrowser = 2
   );

  TWebFrame = class(TOCGenericImport<WebFrameClass, WebFrame>)
  end;

  TWebView = class(TOCGenericImport<WebViewClass, WebView>)
  end;

implementation

end.
