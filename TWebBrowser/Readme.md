#TWebBrowserEx クラス

このクラスは、Windows / OS X の FireMonkey アプリケーションで WebBrowser を使用する方法を提供します。  
このクラスを使って作った Web Browser は下記のようにプラットフォームに搭載されているデフォルトのウェブブラウザコントロールを使います。

|Platform|Component       |
|--------|----------------|
|Windows |IWebBrowser(IE) |
|OS X    |WebView(Safari) |

##動作環境
Delphi / C++Builder / RAD Studio の XE5 以上

##ファイル

以下のファイルを全てダウンロードします。

    FMX.WebBrowser.Mac.pas    OS X 用 WebBrowser クラス
    FMX.WebBrowser.Win.pas    Windows 用 WebBrowser クラス
    FMX.WebBrowserEx.pas      Windows / OS X 用の WebBrowser を統一的に扱うクラス
    Macapi.WebView.pas        OS X の WebView の定義を Delphi に移植したユニット

##使用方法

下記のように、iOS/Android の TWebBrowser と同じように使えます。

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
  FWebBrowser.Navigate;
end;

```

また、下記のように、JavaScript も使えます。

```pascal
procedure TForm1.Button2Click(Sender: TObject);
var
  Value: String;
begin
  // HTML に定義されている JavaScript 関数 foo に引数を２つ渡して呼ぶ
  FWebBrowser.CallJS('foo', [Param1, Param2]);
  
  // HTML のタグの値を取得する
  // <input type="text" id="bar" value="" /> というタグがあった場合に
  // ↓これで bar という id の属性値 value を取得できる
  Value := FWebBrowser.GetTagValue('bar', 'value'); 
end;
```

##著作権
商用・非商用に関わらず自由に使用して構いません。  
なお、著作権は放棄していません。  
