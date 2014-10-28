(*
 * TWebBrowserEx Classes
 *   WebBrowser Componet for FireMonkey.
 *
 * Copyright (c) 2013, 2014 HOSOKAWA Jun.
 *
 * Platform:
 *   Windows, OS X, iOS, Android
 *   Delphi / C++Builder XE5, XE6, XE7 and Appmethod 1.14, 1.15
 *
 * Contact:
 *   Twitter @pik or freeonterminate@gmail.com
 *
 * Original Source:
 *   https://github.com/freeonterminate/delphi/tree/master/TWebBrowser
 *
 * How to Use:
 *   1. Add FMX.WebBroserEx to "uses" block.
 *
 *   2. Create WebBroserEx and Set parent.
 *      procedure TForm1.FormCreate(Sender: TObject);
 *      begin
 *        FWebBrowser := TWebBrowserEx.Create(Self);
 *        FWebBrowser.Parent := Panel1;
 *        FWebBrowser.Align := TAlignLayout.Client;
 *      end;
 *
 *   3. Set URL property.
 *      procedure TForm1.Button1Click(Sender: TObject);
 *      begin
 *        FWebBrowser.URL := 'https://twitter.com/pik';
 *      end;
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

unit FMX.WebBrowserEx;

interface

uses
  System.Classes
  ,FMX.Types, FMX.WebBrowser
  ;

type
  IWebBrowserEx = interface
  ['{66AB09D6-6B38-49DA-B831-B00F43EAF471}']
    function GetTagValue(const iTagName, iValueName: String): String;
  end;

  [
    ComponentPlatformsAttribute(
      pidWin32
      or pidWin64
      or pidOSX32
      or pidiOSSimulator
      or pidiOSDevice
      or pidAndroid
    )
  ]
  TWebBrowserEx = class(TWebBrowser)
  private
    procedure ParentResize(Sender: TObject);
  protected
    procedure SetParent(const Value: TFmxObject); override;
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CheckBounds;
    function GetWeb: ICustomBrowser;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure CallJS(
      const iFunction: String;
      const iParams: array of String); overload;
    procedure CallJS(const iFunction: String); overload;
    function GetTagValue(const iTagName, iValueName: String): String;
  end;

implementation

uses
  System.Rtti, System.SysUtils
  , FMX.Controls
  {$IFDEF MSWINDOWS}
    , FMX.WebBrowser.Win
  {$ENDIF}
  {$IF defined(MACOS) and not defined(IOS)}
    , FMX.WebBrowser.Mac
  {$ENDIF}
  ;

{ TWebBrowserEx }

procedure TWebBrowserEx.CallJS(const iFunction: String);
begin
  CallJS(iFunction, []);
end;

procedure TWebBrowserEx.CallJS(
  const iFunction: String;
  const iParams: array of String);
var
  Params: TStringBuilder;
  Param: String;
begin
  Params := TStringBuilder.Create;
  try
    for Param in iParams do begin
      Params.Append(',');
      Params.Append(Param);
    end;

    if (Params.Length > 0) then
      Params.Remove(0, 1);

    EvaluateJavaScript(Format('%s(%s);', [iFunction, Params.ToString]));
  finally
    Params.Free;
  end;
end;

function TWebBrowserEx.GetTagValue(const iTagName, iValueName: String): String;
var
  Web: ICustomBrowser;
  WebEx: IWebBrowserEx;
begin
  Web := GetWeb;

  if (Web = nil) then
    Exit;

  if (Supports(Web, IWebBrowserEx, WebEx)) then
    Result := WebEx.GetTagValue(iTagname, iValueName)
  else
    raise
      ENotSupportedException.Create(
        'GetTagValue method is not supported on this platform.')
end;

procedure TWebBrowserEx.CheckBounds;
var
  Web: ICustomBrowser;
begin
  Web := GetWeb;

  if (Web <> nil) then
    Web.UpdateContentFromControl;
end;

constructor TWebBrowserEx.Create(Owner: TComponent);
begin
  inherited;

  HitTest := False;
  TabStop := False;
end;

destructor TWebBrowserEx.Destroy;
var
  Web: ICustomBrowser;
begin
  Web := GetWeb;

  inherited;

  if (Web <> nil)  then
    Web.Hide;
end;

procedure TWebBrowserEx.EvaluateJavaScript(const JavaScript: string);
var
  Web: ICustomBrowser;
begin
  Web := GetWeb;

  if (Web <> nil) then
    Web.EvaluateJavaScript(JavaScript);
end;

function TWebBrowserEx.GetWeb: ICustomBrowser;
var
  RttiType: TRttiType;
  Web: ICustomBrowser;
begin
  RttiType := SharedContext.GetType(TWebBrowser);
  Web := ICustomBrowser(RttiType.GetField('FWeb').GetValue(Self).AsInterface);

  Result := Web;
end;

procedure TWebBrowserEx.ParentResize(Sender: TObject);
begin
  CheckBounds;
end;

procedure TWebBrowserEx.SetParent(const Value: TFmxObject);
var
  Web: ICustomBrowser;
begin
  if (Parent <> nil) and (Parent is TControl) then
    TControl(Parent).OnResize := nil;

  inherited;

  if (Parent <> nil) then begin
    if (Parent is TControl) then
      TControl(Parent).OnResize := ParentResize;

    Web := GetWeb;
    if (Web <> nil) then
      Web.SetWebBrowserControl(Self);

    Show;
  end;
end;

procedure TWebBrowserEx.SetVisible(const Value: Boolean);
var
  Web: ICustomBrowser;
begin
  inherited;

  Web := GetWeb;

  if (Web <> nil) then begin
    if (Value) then
      Web.Show
    else
      Web.Hide;
  end;
end;

initialization
  RegisterFmxClasses([TWebBrowser]);

end.
