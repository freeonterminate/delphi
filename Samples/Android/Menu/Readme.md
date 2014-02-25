#Android Menu サンプル

Android でオプションメニューを出すサンプルです。

![alt サンプル画像](https://raw.github.com/freeonterminate/delphi/master/Samples/Android/Menu/screenshot.png)

##解説
Delphi for Android には TMenu や TPopupMenu はありません。
Activity の OnCreateOptionMenus があるわけでもない（NativeActivity なので）ため、メニューは自前で実装する必要があります。
TMenuBar の実装でも使われている TPopup を使うと簡単に実装できますが、TPopup の表示が遅いため、本来であれば、これを修正する必要があります（※）

今回は TPopup の上に ListBox を載せて、ListBoxItem をメニューアイテムに見立てました。
TPopup を使っていることから判るように、基本的には何でも載せられます。
例えば、TButton を載せても良いと思います。

また、メニューボタンを１度押すと表示、表示中に押すと非表示にするために、TPopupHelper というヘルパクラスを作りました。
これも、本来であれば継承して修正するべき所です（※）。

###※
TPopup を継承した TAndroidMenu などといったクラスを作り、CreatePopupForm を Override します。
そして、毎回作り直される TCustomPopupForm を一度だけ作るように改めると、多分速くなります。
今回、継承して作らなかった訳は、継承して作った場合、コンポーネント化して登録する必要があるためです（デザイナでデザインしたいならば）。
そんなに難しくはないのですが、とりあえず、このような形にしました。
