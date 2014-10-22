#TWebBrowserEx Class


THIS TEXT IS TRANSLATION BY A MACHINE. 
PLEASE CONTACT ME, WHEN THERE IS STRANGE WORDING. 

MY TWITTER ACCOUNT: @pik


This class provide WebBrowser for All-Platform FireMonkey Applications.  
This class use Default WebView.  

|Platform|Component       |
|--------|----------------|
|Windows |IWebBrowser(IE) |
|OS X    |WebView(Safari) |
|iOS     |WebView         |
|Android |WebView         |

##Environment Platform
Delphi / C++Builder / RAD Studio version XE6, XE7.  

If you use XE5, Please make the following correction.   

    Change "StrToNSSTR" to "NSSTR".
    Delete "Macapi.Helpers" unit.


##File

You download following files.

    FMX.WebBrowser.Mac.pas    WebBrowser class for OS X
    FMX.WebBrowser.Win.pas    WebBrowser class for Windows
    FMX.WebBrowserEx.pas      WebBrowser for MultiPlatform.
    Macapi.WebView.pas        WebView API Unit.

##How to use
As follows, it can use like TWebBrowser of iOS /Android. 

```pascal
uses
  FMX.WebBrowserEx;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FWebBrowser: TWebBrowserEx;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FWebBrowser := TWebBrowserEx.Create(Self);
  FWebBrowser.Parent := Panel1;
  FWebBrowser.Align := TAlignLayout.Client;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FWebBrowser.URL := 'http://www.embarcadero.com/';

  // As follows, WebBrowserEx can also read a HTML-String. 
  FWebBrowser.LoadFromStrings('<html><body>Hello WebBrowser!</body></html>', '');
end;

```

As follows, WebBrowserEx can use JavaScript

```pascal
procedure TForm1.Button2Click(Sender: TObject);
var
  Value: String;
begin
  // JavaScript function "foo" needs Two-Arguments.
  FWebBrowser.CallJS('foo', [Param1, Param2]);
  
  // TWebBrowserEx can call with TWebBrowser default style.
  FWebBrowser.EvaluteJavascript('alert("Delphi!")');
  
  // TWebBrowserEx take a Value of HTML Tags.
  // If following tag exists,
  // <input type="text" id="bar" value="" /> 
  // ↓ can taking id of bar's attribute value.
  Value := FWebBrowser.GetTagValue('bar', 'value'); 
end;
```

##Known specification like a bug
TWebBrowserEx can't move a focus by Tab key.  
When TWebBrowserEx has a focus, A focus is unmovable to other control by a Tab key.   

##Copyright
Copyright (C) 2013,2014 HOSOKAWA Jun (as @pik).
This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.  

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:  

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.  

2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.  

3. This notice may not be removed or altered from any source distribution.  
