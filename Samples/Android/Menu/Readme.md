#Android Menu サンプル

Android でオプションメニューを出すサンプルです。

![alt サンプル画像](https://raw.github.com/freeonterminate/delphi/master/Samples/Android/Menu/screenshot.png)

##解説
Delphi for Android には [TMainMenu](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Menus.TMainMenu) や [TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup)Menu はありません。  
Activity の OnCreateOptionMenus があるわけでもない（NativeActivity なので）ため、メニューは自前で実装する必要があります。  
[TMenuBar](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Menus.TMenuBar) の実装でも使われている [TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup) を使うと簡単に実装できますが、[TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup) の表示が遅いため、本来であれば、これを修正する必要があります（※）  

今回は [TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup) の上に [TListBox](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.ListBox.TListBox) を載せて、ListBoxItem をメニューアイテムに見立てました。  
[TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup) を使っていることから判るように、基本的には何でも載せられます。  
例えば、[TButton](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.StdCtrls.TButton) を載せても良いと思います。  

また、メニューボタンを１度押すと表示、表示中に押すと非表示にするために、TPopupHelper というヘルパクラスを作りました。  
これも、本来であれば継承して修正するべき所です（※）。  

###※
[TPopup](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Controls.TPopup) を継承した TAndroidMenu などといったクラスを作り、CreatePopupForm を Override します。  
そして、毎回作り直される [TCustomPopupForm](http://docwiki.embarcadero.com/Libraries/XE5/ja/FMX.Forms.TCustomPopupForm) を一度だけ作るように改めると、多分速くなります。  
今回、継承して作らなかった訳は、継承して作った場合、コンポーネント化して登録する必要があるためです（デザイナでデザインしたいならば）。  
そんなに難しくはないのですが、とりあえず、このような形にしました。  
