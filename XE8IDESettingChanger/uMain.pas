(*
 * IDE setting changer
 *
 * Copyright (c) 2015 HOSOKAWA Jun.
 *
 * First Release 2015/04/09 XE8 対応
 * Last Update   2015/09/01 10 seattle 対応
 *
 * Contact:
 *   Twitter @pik or freeonterminate@gmail.com
 *
 * Original Source:
 *   https://github.com/freeonterminate/delphi/tree/master/XE8IDESettingChanger
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
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Colors, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, FMX.ListBox, FMX.SpinBox, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    lblPreviewTitle: TLabel;
    imgPreview: TImage;
    pnlPreviewBase: TPanel;
    lblPreviewFont: TLabel;
    rectPreviewColor: TRectangle;
    colorBaseColor: TComboColorBox;
    btnSave: TButton;
    cmbbxFontName: TComboBox;
    lblFontNameTitle: TLabel;
    lblFontSizeTitle: TLabel;
    lblBaseColorTitle: TLabel;
    Line1: TLine;
    spinFontSize: TSpinBox;
    btnReset: TButton;
    memoLog: TMemo;
    btnXE7Color: TButton;
    cmbbxTarget: TComboBox;
    lblTarget: TLabel;
    procedure cmbbxFontNameChange(Sender: TObject);
    procedure colorBaseColorChange(Sender: TObject);
    procedure spinFontSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnXE7ColorClick(Sender: TObject);
  private
    procedure Reset;
  end;

var
  Form1: TForm1;

implementation

uses
  Winapi.Windows;

const
  REG_KEY = 'Software\Embarcadero\BDS\%s\ModernTheme';

  VER_XE8 = '16.0';
  VER_SEATTLE = '17.0';

  REG_KEY_FONT_NAME = 'FontName';
  REG_KEY_FONT_SIZE = 'FontSize';
  REG_KEY_TOOLBAR_COLOR = 'MainToolBarColor';

  DEFAULT_FONTNAME = 'Segoe UI';
  DEFAULT_FONTNAME_JP = 'Meiryo UI';

  DEFAULT_FONT_SIZE = 10;

  DEFAULT_TOOLBAR_COLOR = $FFB9D1EA;
  DEFAULT_TOOLBAR_COLOR_XE7 = $FFF0F0F0;

{$R *.fmx}

procedure TForm1.btnResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  Index: Integer;
  Size: DWORD;

  procedure Log(const iMsg: String);
  begin
    memoLog.Lines.Add(iMsg);
    memoLog.ScrollBy(0, 100);
  end;

  procedure SetRegValue(
    const iKey: String;
    const iType: DWORD;
    const iValue: Pointer;
    const iSize: DWORD);
  var
    hReg: HKEY;
    Disposition: DWORD;
    Res: DWORD;
    Ver: String;
    RegKey: String;
  begin
    case cmbbxTarget.ItemIndex of
      0:
        Ver := VER_XE8;

      1:
      begin
        Ver := VER_SEATTLE;
      end;

      else
        Ver := '';
    end;

    if (Ver.IsEmpty) then
    begin
      Log('ERROR, Target IDE unknown');
      Exit;
    end;

    RegKey := Format(REG_KEY, [Ver]);

    if (iType = REG_SZ) then
      Log(Format('%s: String = %s', [iKey, String(PChar(iValue))]))
    else
      Log(Format('%s: DWORD = $%x', [iKey, Integer(iValue)]));

    hReg := 0;

    Res :=
      RegCreateKeyEx(
        HKEY_CURRENT_USER,
        PChar(RegKey),
        0,
        nil,
        REG_OPTION_NON_VOLATILE,
        KEY_ALL_ACCESS,
        nil,
        hReg,
        @Disposition);
    try
      if (Res = ERROR_SUCCESS) then
        Res :=
          RegSetValueEx(
            hReg,
            PChar(iKey),
            0,
            iType,
            iValue,
            iSize
          );

      if (Res <> ERROR_SUCCESS) then
        Log('ERROR !');
    finally
      RegCloseKey(hReg);
    end;
  end;

  procedure SetRegStrValue(const iKey, iValue: String);
  begin
    SetRegValue(iKey, REG_SZ, PChar(iValue), Length(iValue) * SizeOf(Char));
  end;

begin
  memoLog.Lines.Clear;
  Log(FormatDateTime('yyyy/mm/dd hh:nn:ss', Now) + ' Start.');

  Index := cmbbxFontName.ItemIndex;
  if (Index > -1) and (Index < cmbbxFontName.Count) then
    SetRegStrValue(REG_KEY_FONT_NAME, cmbbxFontName.Items[Index]);

  Size := Trunc(spinFontSize.Value);
  SetRegValue(REG_KEY_FONT_SIZE, REG_DWORD, @Size, SizeOf(DWORD));

  SetRegStrValue(
    REG_KEY_TOOLBAR_COLOR,
    '$' + IntToHex(colorBaseColor.Color, 8).SubString(2));

  Log('Done.');
end;

procedure TForm1.btnXE7ColorClick(Sender: TObject);
begin
  colorBaseColor.Color := DEFAULT_TOOLBAR_COLOR_XE7;
end;

procedure TForm1.cmbbxFontNameChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := cmbbxFontName.ItemIndex;

  if (Index > -1) and (Index < cmbbxFontName.Count) then
  begin
    lblPreviewFont.TextSettings.Font.Family := cmbbxFontName.Items[Index];
    lblPreviewFont.Repaint;
  end;
end;

procedure TForm1.colorBaseColorChange(Sender: TObject);
begin
  rectPreviewColor.Fill.Color := colorBaseColor.Color;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  DC: HDC;
  LFont: TLogFont;
  Fonts: TStringList;

  function EnumFontsProc(
    var LogFont: TLogFont;
    var TextMetric: TTextMetric;
    FontType: Integer;
    Data: Pointer): Integer; stdcall;
  var
    S: TStrings;
    Temp: String;
  begin
    Result := 1;

    S := TStrings(Data);
    Temp := LogFont.lfFaceName;

    if
      (not Temp.StartsWith('@')) and
      (
        (S.Count = 0) or
        (CompareText(S[S.Count - 1], Temp) <> 0)
      )
    then
      S.Add(Temp);
  end;

begin
  Fonts := TStringList.Create;
  try
    Fonts.Sorted := True;

    DC := GetDC(0);
    try
      FillChar(LFont, SizeOf(LFont), 0);
      LFont.lfCharset := DEFAULT_CHARSET;
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LPARAM(Fonts), 0);
    finally
      ReleaseDC(0, DC);
    end;

    cmbbxFontName.Items.Assign(Fonts);
  finally
    Fonts.DisposeOf;
  end;

  Reset;
end;

procedure TForm1.Reset;

  procedure GetDefault(const iFontName: String);
  var
    Index: Integer;
  begin
    Index := cmbbxFontName.Items.IndexOf(iFontName);
    if (Index > -1) then
      cmbbxFontName.ItemIndex := Index;
  end;

begin
  GetDefault(DEFAULT_FONTNAME);
  GetDefault(DEFAULT_FONTNAME_JP);
  spinFontSize.Value := DEFAULT_FONT_SIZE;
  colorBaseColor.Color := DEFAULT_TOOLBAR_COLOR;
end;

procedure TForm1.spinFontSizeChange(Sender: TObject);
begin
  lblPReviewFont.TextSettings.Font.Size := spinFontSize.Value + 2;
  lblPReviewFont.Repaint;
end;

end.
