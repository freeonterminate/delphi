#TabItemWithClose コンポーネント

このコンポーネントは、FireMonkey の TabControl に「閉じるボタン」付きの Tab を表示します。

##ファイル

以下のファイルが含まれています。

    Readme.text  このファイル
    Style
      + TabItemWithCloseStyle.style  TabItemWidthCloseStyle 用のスタイル

    Source
      + FMX.TabItemWithClose.pas           コンポーネント本体
      + FMX.TabItemWithClosePackage.dpk    パッケージプロジェクトソース
      + FMX.TabItemWithClosePackage.dproj  パッケージプロジェクト
      + FMX.TabItemWithClosePackage.res    パッケージリソース
      + PropertyEditor
           + TabControlEditor.fmx          コンポーネントエディタのリソース
           + TabControlEditor.pas          コンポーネントエディタ
           + TabControlPropEditor.pas      プロパティエディタ

##使用方法

1.
  FMX.TabItemWithClosePackage.dproj を開き、コンポーネントをインストールします。
  これによって、TabControl を右クリックしたときに開くコンテキストメニューの最上部に「Show TabItem Editor」という項目が追加されます。

2.
  StyleBook のスタイル編集画面を開き「追加」ボタンをクリックして下さい（StyleBook が無ければ配置してください）。
  ダイアログから同梱の Style\TabItemWithCloseStyle.style を選択します。
  最後に「適用して閉じる」ボタンをクリックして、スタイルの編集を終了します。

3.
  Show TabItem Editor をクリックするとプロパティエディタが立ち上がります。
  DropDown から TTabItem を選ぶと普通のタブが、TTabItemWithClose を選ぶとボタン付きのタブが生成されます。

