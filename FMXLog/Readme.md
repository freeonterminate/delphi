#Log ユーティリティー

このユーティリティーは、Windows / OS X / iOS / Android のアプリケーションで「ログ出力」を統一的に使用する方法を提供します。  
このユーティリティーを用いたログは下記の場所に出力されます。  

|Platform|Out to                                   |Implementation      |
|--------|-----------------------------------------|--------------------|
|Windows |Console                                  |WriteLn             |
|OS X    |Console (PAServer)                       |WriteLn             |
|iOS     |Console (Organizer -> Device -> Console) |NSLog               |
|Android |System Log                               |__android_log_write |

##動作環境
Delphi / C++Builder / RAD Studio の XE5, XE6  
Appmethod 1.14  

##ファイル

以下のファイルをダウンロードします。

    FMX.Log.pas

##使用方法

FMX.Log を uses すると Log クラスが使えるようになります。  
Log.d といったクラスメソッドを使うとログが出力されます。  
  
レベルによって下記の様に使うメソッドが変わります。  

|Level   |Method Name|
|--------|-----------|
|VERBOSE |Log.v      |
|DEBUG   |Log.d      |
|INFO    |Log.i      |
|WARN    |Log.w      |
|ERROR   |Log.e      |
|FATAL   |Log.f      |

また、複数の値を渡すこともできます。  

```pascal
  Log.d('文字列');              // １つの引数版は文字列のみ指定可能
  Log.d(['文字列', 123, True]); // 複数引数版は、様々な値を指定可能
```


##例
```pascal
uses
  FMX.Log;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FWebBrowser: TWebBrowserEx;
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Log.i('ボタン１が押されたよ'); // Info Level で出力
  Log.d(['文字列', 100]);        // Debug Level で出力
end;

```

##著作権
商用・非商用に関わらず自由に使用して構いません。  
改変もご自由にどうぞ。  
なお、著作権は放棄していませんので、「これ 僕が/弊社が 作ったんだー！」っていうのは無しです。  
