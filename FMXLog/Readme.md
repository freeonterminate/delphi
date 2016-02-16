#Log ユーティリティー

このユーティリティーは、Windows / OS X / iOS / Android のアプリケーションで「ログ出力」を統一的に使用する方法を提供します。  
このユーティリティーを用いたログは下記の場所に出力されます。  

|Platform|Out to                                   |Implementation      |
|--------|-----------------------------------------|--------------------|
|Windows |Console and Delphi's  Event Log          |WriteLn             |
|OS X    |Console (PAServer)                       |WriteLn             |
|iOS     |Device Log (Xcode -> Devices -> Log)     |NSLog               |
|Android |System Log (adb logcat)                  |__android_log_write |

##動作環境
Delphi / C++Builder / RAD Studio の XE5, XE6, XE7, XE8, 10 Seattle  
Appmethod 1.14, 1.15, 1.16, 1.17  

##最終更新日
2016/02/09

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

複数の値を渡すこともできます。  

```pascal
  Log.d('文字列');              // １つの引数版は文字列のみ指定可能  
  Log.d(['文字列', 123, True]); // 複数引数版は、様々な値を指定可能  
```

TRectF, TPointF, TSizeF, TRect, TPoint を文字列に変換するメソッドもあります。

```pascal
  Log.d(Log.PointFToString(TPointF.Create(100, 100)));    
  Log.d(Log.RectFToString(TRectF.Create(100, 100, 100, 100)));    
```

Enabled プロパティによって、出力を抑制できます。

```pascal
  {$IFDEF RELEASE}  
  Log.Enabled := False; // リリースビルドではログを出力しない  
  {$ENDIF}  
```

##例
```pascal
uses
  FMX.Log;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Log.i('ボタン１が押されたよ'); // Info Level で出力
  Log.d(['文字列', 100]);        // Debug Level で出力
end;

```

##著作権
本ソフトウェアは「現状のまま」で、明示であるか暗黙であるかを問わず、何らの保証もなく提供されます。
本ソフトウェアの使用によって生じるいかなる損害についても、作者は一切の責任を負わないものとします。

以下の制限に従う限り、商用アプリケーションを含めて、本ソフトウェアを任意の目的に使用し、自由に改変して再頒布することをすべての人に許可します。

1. 本ソフトウェアの出自について虚偽の表示をしてはなりません。
   あなたがオリジナルのソフトウェアを作成したと主張してはなりません。
   あなたが本ソフトウェアを製品内で使用する場合、製品の文書に謝辞を入れていただければ幸いですが、必須ではありません。

2. ソースを変更した場合は、そのことを明示しなければなりません。
   オリジナルのソフトウェアであるという虚偽の表示をしてはなりません。

3. ソースの頒布物から、この表示を削除したり、表示の内容を変更したりしてはなりません。

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented;
   you must not claim that you wrote the original software.
   If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
