#TXplatIniFile クラス

このクラスは、プラットフォームを気にせずに IniFile を使用する方法を提供します。  
このクラスを使って作った IniFile は下記のようにプラットフォームネイティブな形式になります。  

|Platform|Format          |
|--------|----------------|
|Windows |IniFile         |
|OS X    |plist           |
|iOS     |plist           |
|Android |SharedPreference|

##ファイル

以下のファイルを全てダウンロードします。

    FMX.IniFile.pas          プラットフォームフリーな IniFile クラス
    FMX.IniFile.Apple.pas    OS X / iOS 用の IniFile クラス
    FMX.IniFile.Android.pas  Android 用の IniFile クラス

##使用方法

下記のように、通常の TIniFile と同じように使えます。

```pascal
uses
  FMX.IniFile;

var
  IniFile: TXplatIniFile;
begin
  IniFile := CreateIniFile('会社名'); // 会社名は Windows では必須ですが他プラットフォームでは空文字で構いません
  IniFile.WriteString('セクション', 'キー名', '書き込む値');
  IniFile.ReadString('セクション', 'キー名', 'デフォルト値');
end;
```

##既知のバグ
OS X で ReadSections を使うと IniFile のセクション以外にシステムのパラメータも取得されます。

##著作権
FMX.IniFile.Apple.pas はエンバカデロ・テクノロジーズが提供する Apple.IniFile.pas を元に作られています。  
そのため、FMX.IniFile.Apple.pas の著作権もまたエンバカデロ・テクノロジーズが保有します。  
この結果、FMX.IniFile を OS X / iOS で使用するためには、正当な Delphi/C++Builder/RAD Studio/AppMethod の使用者で無ければなりません。  
それ以外のファイルについては、商用・非商用に関わらず自由に使用して構いません。  

つまり、Delphi/C++Builder/RAD Studio/AppMethod の使用者であれば普通に使えます。  

なお、著作権は放棄していません。  
