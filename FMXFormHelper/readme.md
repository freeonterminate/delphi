#FMX.FormHelper ヘルパークラス

FireMonkey の TForm に「Alt + Enter の無効化」「最小化サイズの設定」機能を追加するヘルパークラスです。

##使用方法

1.  
FMX.FormHelper を uses してください。

2.  
TForm.OnCreate などでヘルパーメソッドを呼んでください。

■例

    uses
      FMX.FormHelper;
    
    procedure TForm1.FormCreate(Sender: TObject);
    begin
      DisableAltEnter;
      SetMinSize(300, 200);
    end;

##参考
Alt+Enter 問題を修正するコードは↓こちらのコードを使わせていただきました。

> 全力わはー 
> ■[Delphi][FireMonkey]Alt+Enterによるフルスクリーン化を無効にする。 
> http://d.hatena.ne.jp/tales/20130221/1361380072
