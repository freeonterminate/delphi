#TWebBrowserEx クラス

このクラスは、Windows / OS X / iOS / Android の FireMonkey アプリケーションで WebBrowser を統一的に使用する方法を提供します。  
このクラスを使って作った Web Browser は下記のようにプラットフォームに搭載されているデフォルトのウェブブラウザコントロールを使います。

|Platform|Component       |
|--------|----------------|
|Windows |IWebBrowser(IE) |
|OS X    |WebView(Safari) |
|iOS     |WebView         |
|Android |WebView         |

##動作環境
Delphi / C++Builder / RAD Studio の XE6, XE7

XE5 以前で動かす場合は

    StrToNSSTR を NSSTR に変更
    Macapi.Helpers を削除

と変更してください。

##ファイル

以下のファイルを全てダウンロードします。

    FMX.WebBrowser.Mac.pas    OS X 用 WebBrowser クラス
    FMX.WebBrowser.Win.pas    Windows 用 WebBrowser クラス
    FMX.WebBrowserEx.pas      マルチプラットフォームの WebBrowser を統一的に扱うクラス
    Macapi.WebView.pas        OS X の WebView の定義を Delphi に移植したユニット

##使用方法

下記のように、iOS / Android の TWebBrowser と同じように使えます。

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

  // こんな風に String を読み込ませる事もできる
  FWebBrowser.LoadFromStrings('<html><body>Hello WebBrowser!</body></html>', '');
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
  
  // TWebBrowser の標準的な方法で呼ぶ
  FWebBrowser.EvaluteJavascript('alert("Delphi!")');
  
  // HTML のタグの値を取得する
  // <input type="text" id="bar" value="" /> というタグがあった場合に
  // ↓これで bar という id の属性値 value を取得できる
  Value := FWebBrowser.GetTagValue('bar', 'value'); 
end;
```

##既知のバグみたいな仕様
Tab キーでは TWebBrowserEx にフォーカスを移動できません。  
同様に TWebBrowserEx がフォーカスを持っている場合 Tab キーで他のコントロールにフォーカスを移動できません。  
IDocHostUIHandler の DocHostTranslateAccelerator を実装すれば TWebBrowser → FMX Control という方向にはフォーカスを移せると思います。  
TWebBrowserEx の SetFocus を override すれば FMX Control → TWebBrowser 方向にもフォーカスを移せるかもしれません。  

##著作権
Copyright (c) 2013, 2014 HOSOKAWA Jun(@pik).  
  
本ソフトウェアは「現状のまま」で、明示であるか暗黙であるかを問わず、何らの保証もなく提供されます。 本ソフトウェアの使用によって生じるいかなる損害についても、作者は一切の責任を負わないものとします。  
  
以下の制限に従う限り、商用アプリケーションを含めて、本ソフトウェアを任意の目的に使用し、自由に改変して再頒布することをすべての人に許可します。  
  
1. 本ソフトウェアの出自について虚偽の表示をしてはなりません。あなたがオリジナルのソフトウェアを作成したと主張してはなりません。あなたが本ソフトウェアを製品内で使用する場合、製品の文書に謝辞を入れていただければ幸いですが、必須ではありません。  

2. ソースを変更した場合は、そのことを明示しなければなりません。オリジナルのソフトウェアであるという虚偽の表示をしてはなりません。

3. ソースの頒布物から、この表示を削除したり、表示の内容を変更したりしてはなりません。  
