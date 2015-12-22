(*
 * TWebBrowserEx Helper Libraries
 *
 * Copyright (c) 2013, 2015 HOSOKAWA Jun.
 *
 * Platform:
 *   Windows, OS X, iOS, Android
 *   Delphi / C++Builder XE5, XE6, XE7, XE8, 10 seattle
 *   Appmethod 1.14, 1.15, 1.16, 1.17
 *
 * Contact:
 *   Twitter @pik or freeonterminate@gmail.com
 *
 * Original Source:
 *   https://github.com/freeonterminate/delphi/tree/master/TWebBrowser
 *
 * LICENSE:
 *   本ソフトウェアは「現状のまま」で、明示であるか暗黙であるかを問わず、
 *   何らの保証もなく提供されます。
 *   本ソフトウェアの使用によって生じるいかなる損害についても、
 *   作者は一切の責任を負わないものとします。
 *
 *   以下の制限に従う限り、商用アプリケーションを含めて、本ソフトウェアを
 *   任意の目的に使用し、自由に改変して再頒布することをすべての人に許可します。
 *
 *   1. 本ソフトウェアの出自について虚偽の表示をしてはなりません。
 *      あなたがオリジナルのソフトウェアを作成したと主張してはなりません。
 *      あなたが本ソフトウェアを製品内で使用する場合、製品の文書に謝辞を入れて
 *      いただければ幸いですが、必須ではありません。
 *
 *   2. ソースを変更した場合は、そのことを明示しなければなりません。
 *      オリジナルのソフトウェアであるという虚偽の表示をしてはなりません。
 *
 *   3. ソースの頒布物から、この表示を削除したり、表示の内容を変更したりしては
 *      なりません。
 *
 *   This software is provided 'as-is', without any express or implied warranty.
 *   In no event will the authors be held liable for any damages arising from
 *   the use of this software.
 *
 *   Permission is granted to anyone to use this software for any purpose,
 *   including commercial applications, and to alter it and redistribute
 *   it freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented;
 *      you must not claim that you wrote the original software.
 *      If you use this software in a product, an acknowledgment in the product
 *      documentation would be appreciated but is not required.
 *
 *   2. Altered source versions must be plainly marked as such,
 *      and must not be misrepresented as being the original software.
 *
 *   3. This notice may not be removed or altered from any source distribution.
 *)

unit FMX.WebBrowser.Helpers;

interface

procedure OpenBrowser(const iURL: String);

implementation

uses
  System.SysUtils

  {$IFDEF MSWINDOWS}
    , Winapi.Windows, Winapi.ShellAPI
  {$ENDIF}

  {$IFDEF ANDROID}
    , Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers
    , FMX.Helpers.Android
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
      , Macapi.Helpers
      , iOSapi.Foundation
      , FMX.Helpers.IOS
    {$ELSE}
      , Posix.Stdlib
    {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF MSWINDOWS}
procedure OpenBrowser(const iURL: String);
begin
  if (ShellExecute(0, 'open', PChar(iURL), nil, nil, SW_SHOW) < 32) then
    WinExec(PAnsiChar(AnsiString('explorer ' + iURL)), SW_SHOW)
end;
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
procedure OpenBrowser(const iURL: String);
begin
  _system(PAnsiChar(AnsiString('open ' + iURL)));
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure OpenBrowser(const iURL: String);
var
  Intent: JIntent;
begin
  Intent :=
    TJIntent.JavaClass.init(
      TJIntent.JavaClass.ACTION_VIEW,
      StrToJURI(iURL));

  SharedActivity.startActivity(Intent);
end;
{$ENDIF}

{$IFDEF IOS}
procedure OpenBrowser(const iURL: String);
begin
  SharedApplication.openURL(
    TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(iURL))));
end;
{$ENDIF}
end.
