#English version is below.

---

#Formatter 機能拡張

##概要

Delphi / C++Builder Starter Edition には「ソースの整形」が存在しません。  
しかし、コマンドライン版の Formatter.exe は存在するため、これを使ってコードの整形をする機能拡張です。  

##使い方
###コンパイル編
1. Formatter.dproj を開きます。
2. パッケージをインストールします。  
（プロジェクトマネージャの Formatter.bpl を右クリック、コンテキストメニューの「インストール」を選択）
3. インストールすると IDE の「ツール」メニューの最下段に「ソースの整形」が追加されます。
4. クリックするとソースが整形されます。

###パッケージのインストール編
1. Formatter.bpl ダウンロードします
2. IDE のメニュー「コンポーネント」→「パッケージのインストール」で Formatter.bpl を追加します。
3. IDE の「ツール」メニューの最下段に「ソースの整形」が追加されます。

##著作権
Copyright (C) 2016 HOSOKAWA Jun(@pik).
  
本ソフトウェアは「現状のまま」で、明示であるか暗黙であるかを問わず、何らの保証もなく提供されます。 本ソフトウェアの使用によって生じるいかなる損害についても、作者は一切の責任を負わないものとします。  
  
以下の制限に従う限り、商用アプリケーションを含めて、本ソフトウェアを任意の目的に使用し、自由に改変して再頒布することをすべての人に許可します。  
  
1. 本ソフトウェアの出自について虚偽の表示をしてはなりません。あなたがオリジナルのソフトウェアを作成したと主張してはなりません。

2. ソースを変更した場合は、そのことを明示しなければなりません。オリジナルのソフトウェアであるという虚偽の表示をしてはなりません。

3. ソースの頒布物から、この表示を削除したり、表示の内容を変更したりしてはなりません。  


---


#Formatter IDE Extension

##About

Delphi / C++Builder Starter Edition doesn't have "Code Formatter".
But, Command-Line Tool "Fomatter.exe" exists.
The Extension uses Fomatter.exe from IDE.

##Usage
###By Compile
1. Open "Formatter.dproj"
2. Build & Install package (at ProjectManager Window's Contextg Menu)
3. Formatter Menu is added to IDE MainMenu's [Tools].

###By Installe
1. Download "Formatter.bpl"
2. IDE MainMenu [Component] -> [Install Packages], Add "Formatter.bpl"
3. Formatter Menu is added to IDE MainMenu's [Tools].

##Copyright
Copyright (C) 2016 HOSOKAWA Jun (as @pik).  
This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.  

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:  

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. 

2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.  

3. This notice may not be removed or altered from any source distribution.  
