#FMX.FormHelper ヘルパークラス

FireMonkey の TForm に「Alt + Enter の無効化」「最小化サイズの設定」機能を追加するヘルパークラスです。

##使用方法

1.  
FMX.FormHelper を uses してください。

2.  
TForm.OnCreate などでヘルパーメソッドを呼んでください。

例

```Unit1.pas
procedure TForm1.FormCreate(Sender: TObject);
begin
  DisableAltEnter;
  SetMinSize(300, 200);
end;
```

