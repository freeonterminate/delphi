unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Generics.Collections;

type
  TComment = record
    Start, Ende: String;
    class function Create(const iStart, iEnd: String): TComment; static;
    function ToString: String;
  end;

  TTag = record
    Tag: String;
    Attr: String;
    class function Create(const iValue: String): TTag; static;
    function Tagged(const iStr: String): String;
    function StartTag: String;
    function EndTag: String;
    function ToString: String;
  end;

  TfrmMain = class(TForm)
    memoSource: TMemo;
    lblSource: TLabel;
    memoDest: TMemo;
    lblDest: TLabel;
    btnGo: TButton;
    btnEnd: TButton;
    btnCopy: TButton;
    Bevel1: TBevel;
    btnClear: TButton;
    btnAdd: TButton;
    Bevel2: TBevel;
    btnOpenIni: TButton;
    chbxLineNo: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnEndClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnOpenIniClick(Sender: TObject);
  private
    FReserveds: TStringList;
    FCommands: TStringList;
    FExtras: TStringList;
    FTempExtras: TStringList;
    FCommentChars: TStringList;
    FComments: TList<TComment>;
    FStartTag: TTag;
    FNumberTag: TTag;
    FStringTag: TTag;
    FCommentTag: TTag;
    FReservedTag: TTag;
    FCommandTag: TTag;
    FExtraTag: TTag;
    FLineNoTag: TTag;
    FOddTag: TTag;
    FEvenTag: TTag;
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IniFiles, Vcl.Clipbrd, System.UITypes, uAddWord, uNotifier;

{$R *.dfm}

// Procedures
procedure ReadSectionLines(
  const iFileName, iSectionName: String;
  const iResults: TStrings);
var
  SL: TStringList;
  Str: String;
  Sec: String;
  Inner: Boolean;
begin
  iResults.Clear;

  if (not FileExists(iFileName)) then
    Exit;

  Sec := '[' + iSectionName + ']';

  SL := TStringList.Create;
  try
    SL.LoadFromFile(iFileName);

    Inner := False;
    for Str in SL do begin
      if (CompareText(Sec, Str) = 0) then begin
        Inner := True;
        Continue;
      end;

      if (Inner) then begin
        if (Copy(Str, 1, 1) = '[') then
          Break;

        if (Str <> '') then
          iResults.Add(Str);
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure Split(const iResults: TStrings; iSource: String; const iSep: String);
var
  Pos: Integer;
begin
  iResults.Clear;

  if (iSource <> '') then
    repeat
      Pos := AnsiPos(iSep, iSource);

      if (Pos < 1) then begin
        iResults.Add(iSource);
        iSource := '';
      end
      else begin
        iResults.Add(Copy(iSource, 1, Pos - 1));
        Delete(iSource, 1, Pos);

        if (iSource = '') then
          iResults.Add('');
      end;
    until (iSource = '');
end;

{ TComment }

class function TComment.Create(const iStart, iEnd: String): TComment;
begin
  Result.Start := iStart;
  Result.Ende := iEnd;
end;

function TComment.ToString: String;
begin
  Result := Start + ' exapmle ' + Ende;
end;

{ TTag }

class function TTag.Create(const iValue: String): TTag;
var
  Index: Integer;
begin
  Index := Pos(' ', iValue);

  if (Index > 0) then begin
    Result.Tag := Copy(iValue, 1, Index - 1);
    Result.Attr := Copy(iValue, Index + 1, Length(iValue));
  end
  else
    Result.Tag := iValue;
end;

function TTag.EndTag: String;
begin
  Result := '</' + Tag + '>';
end;

function TTag.StartTag: String;
var
  Start: String;
begin
  Start := Tag;
  if (Attr <> '') then
    Start := Tag + ' ' + Attr;

  Result := '<' + Start + '>';
end;

function TTag.Tagged(const iStr: String): String;
begin
  Result := StartTag + iStr + EndTag;
end;

function TTag.ToString: String;
begin
  Result := Tagged('example');
end;

{TForm1}

procedure TfrmMain.btnAddClick(Sender: TObject);
begin
  ShowAddWords(FTempExtras);
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  memoSource.Lines.Clear;
end;

procedure TfrmMain.btnCopyClick(Sender: TObject);
begin
  Clipboard.AsText := memoDest.Lines.Text;
end;

procedure TfrmMain.btnEndClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
var
  Source, Dest: String;
  Word: String;
  Ch: Char;
  Ch2: String;
  tmpCh: Char;
  InComment, InString, InSharpString, InNumber: Boolean;
  PrevIsOther: Boolean;
  i, j: Integer;
  Comment: TComment;
  ExecComment: TComment;
  LineWidth: Integer;
  Index: Integer;
  LineNoNeeded: Boolean;
  LineTag: TTag;
  Space: String;

  procedure Addef;
  var
    Str: String;
  begin
    Str := Ch;

    case Ch of
      #0:
        Str := '';

      '<':
        Str := '&lt;';

      '>':
        Str := '&gt;';

      '&':
        Str := '&amp;';
    end;

    if (Str <> '') then
      Word := Word + Str;

    Ch := #0;
  end;

  procedure AddRaw(const iSrc: String);
  begin
    Dest := Dest + iSrc;
  end;

  procedure AddStartTag(const iTag: String);
  begin
    Dest := Dest + iTag;
    Addef;
  end;

  procedure AddEndTag(const iTag: String);
  begin
    Addef;
    Dest := Dest + Word + iTag;
  end;

  procedure Flush;
  begin
    Dest := Dest + Word;
    Word := '';
  end;

  function CheckTag(const iEndTag: String; var ioFlag: Boolean): Boolean;
  begin
    Result := False;

    if (ioFlag) then begin
      ioFlag := False;

      AddRaw(Word);
      AddRaw(iEndTag);

      Word := '';
      Addef;
      Flush;

      Ch := #0;

      Result := True;
    end;
  end;

  function CheckNumber: Boolean;
  var
    Doted: Boolean;
  begin
    Result := False;

    if (InNumber) then begin
      Doted := (Word[Word.Length] = '.');

      if (Doted) then
        Word := Copy(Word, 1, Word.Length - 1);

      Result := CheckTag(FNumberTag.EndTag, InNumber);

      if (Doted) then
        Word := '.' + Word;
    end;
  end;

  function CheckSharpString: Boolean;
  begin
    Result := CheckTag(FStringTag.EndTag, InSharpString);
  end;

  function CheckReserved(const iList: TStringList; const iTag: TTag): Boolean;
  begin
    Result := (iList.IndexOf(Word) > -1);

    if (Result) then begin
      AddRaw(iTag.Tagged(Word));
      Word := Ch;
    end
  end;

  function CheckReserveds: Boolean;
  begin
    Result := CheckReserved(FTempExtras, FExtraTag);

    if (not Result) then
      Result := CheckReserved(FExtras, FExtraTag);

    if (not Result) then
      Result := CheckReserved(FReserveds, FReservedTag);

    if (not Result) then
      Result := CheckReserved(FCommands, FCommandTag);
  end;

  procedure EndComment;
  begin
    AddEndTag(FCommentTag.EndTag);
    InComment := False;
    Ch := #0;
    Word := '';
  end;

begin
  ShowNotify('Please Wait', 'Decorating...', Self);
  try
    InComment := False;

    with memoDest.Lines do begin
      Clear;

      i := memoSource.Lines.Count;
      j := 1;
      LineWidth := 0;
      while (j < i) do begin
        Inc(LineWidth);
        j := j * 10;
      end;

      if (LineWidth = 0) then
        LineWidth := 1;

      LineNoNeeded := chbxLineNo.Checked;

      Dest := FStartTag.StartTag + sLineBreak;

      Index := 1;
      LineTag := FOddTag;

      if (LineNoNeeded) then
        Space := '&nbsp'
      else
        Space := '';

      for Source in memoSource.Lines do begin
        // 行番号
        if (LineNoNeeded) then
          Dest :=
            Dest +
            FLineNoTag.Tagged(IntToStr(Index).PadLeft(LineWidth, '0'));

        // 色替え
        if (Odd(Index)) then
          LineTag := FOddTag
        else
          LineTag := FEvenTag;

        Dest := Dest + LineTag.StartTag + Space ;

        Inc(Index);

        // 初期化
        InString := False;
        InSharpString := False;
        InNumber := False;
        PrevIsOther := False;
        Word := '';
        Ch2 := '  ';

        // パース
        for i := 1 to Length(Source) do begin
          Ch := Source[i];
          Ch2 := Copy(Ch2 + Ch, 2, 2);

          if (InString) then begin
            case Ch of
              '''': begin
                InString := False;
                AddEndTag(FStringTag.EndTag);

                Word := '';
              end;

              else
                Addef;
            end;
          end
          else
            for Comment in FComments do begin
              if (Comment.Start = Ch) or (Comment.Start = Ch2) then begin
                InComment := True;
                ExecComment := Comment;

                AddRaw(FCommentTag.StartTag);
                Break;
              end;

              if (Comment.Ende = Ch) or (Comment.Ende = Ch2 )then begin
                EndComment;
                Continue;
              end;
            end;

            if (InComment) then begin
              Addef;
              Continue;
            end;

            case Ch of
              '''': begin
                PrevIsOther := False;
                CheckNumber;
                CheckSharpString;
                Flush;

                InString := True;
                AddStartTag(FStringTag.StartTag);
              end;

              '$', '0'.. '9': begin
                if (PrevIsOther) then
                  Flush;

                PrevIsOther := False;

                if (Word = '') then begin
                  if (InNumber) then begin
                    if (Ch = '$') then
                      CheckNumber;
                  end
                  else begin
                    InNumber := True;
                    AddRaw(FNumberTag.StartTag);
                  end;
                end;

                Addef;
              end;

              'A'.. 'Z', 'a'.. 'z': begin
                PrevIsOther := False;

                if
                  (InNumber) and
                  (Word[1] = '$') and
                  (CharInSet(Ch, ['A'.. 'F', 'a'..'f']))
                then
                  Addef
                else begin
                  CheckNumber;
                  CheckSharpString;

                  Addef;
                end;
              end;

              '#': begin
                PrevIsOther := False;

                CheckNumber;

                InSharpString := True;
                AddStartTag(FStringTag.StartTag);
              end;

              else begin
                if (Ch = '.') and (InNumber) then
                  Addef
                else begin
                  PrevIsOther := True;

                  if (CheckNumber) or (CheckSharpString) then
                    Word := '';

                  tmpCh := Ch;

                  if (not CheckReserveds) then
                    Addef;

                  if (FCommentChars.IndexOf(tmpCh) < 0) then begin
                    AddRaw(Word);
                    Word := '';
                  end;
                end;
              end;
            end;
        end;

        if (InString) then begin
          AddRaw('<br />');
          AddEndTag(FStringTag.EndTag);
        end;

        CheckNumber;
        CheckSharpString;

        if (InComment) and (ExecComment.Ende = '\n') then
          EndComment;

        if (not CheckReserveds) then
          Dest := Dest + Word;

        Dest := Dest + LineTag.EndTag + sLineBreak;
      end;

      Dest := Dest + FStartTag.EndTag;

      Add(Dest);
    end;
  finally
    HideNotify;
  end;
end;

procedure TfrmMain.btnOpenIniClick(Sender: TObject);
var
  Str: AnsiString;
begin
  Str := AnsiString('notepad ' + ChangeFileExt(Application.ExeName, '.ini'));
  WinExec(PAnsiChar(Str), SW_SHOW);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  SL: TStringList;
  Str: String;
  Comments: TStringList;
  Filename: String;
  CS, CE: String;

  procedure AddCommentChars(const iC: String);
  var
    Ch: Char;
  begin
    for Ch in iC do
      if (FCommentChars.IndexOf(Ch) < 0) then
        FCommentChars.Add(Ch);
  end;

begin
  FReserveds := TStringList.Create;
  FCommands := TStringList.Create;
  FExtras := TStringList.Create;
  FTempExtras := TStringList.Create;
  FCommentChars := TStringList.Create;
  FComments := TList<TComment>.Create;

  FileName := ChangeFileExt(Application.ExeName, '.ini');
  if (not FileExists(FileName)) then begin
    MessageDlg(
      'ini file が見つかりません！' + sLineBreak +
      'アプリケーションと同じフォルダに ini file を置いてください',
      mtWarning,
      [mbOk],
      0);

    Exit;
  end;

  with TIniFile.Create(FileName) do
    try
      // Tags
      FStartTag :=
        TTag.Create(
          ReadString(
            'Tag',
            'start',
            'pre style="' +
            'font-family: monospace; ' +
            'background: #000000; ' +
            'border: 1px solid gray; ' +
            'padding: 5px 10px 5px 10px; ' +
            'color: #ffffff;' +
            '"'));

      FNumberTag :=
        TTag.Create(ReadString(
          'Tag', 'number', 'span style="color: #00ff00;"'));

      FStringTag :=
        TTag.Create(ReadString(
          'Tag', 'string', 'span style="color: #ffff00;"'));

      FCommentTag :=
        TTag.Create(ReadString(
          'Tag', 'comment', 'span style="color: #008080;"'));

      FReservedTag :=
        TTag.Create(ReadString('Tag', 'reserved', 'b'));

      FCommandTag :=
        TTag.Create(ReadString('Tag', 'command', 'b'));

      FExtraTag :=
        TTag.Create(ReadString(
          'Tag', 'extra', 'span style="font-weight: bold; color: #ff9900;"'));

      FLineNoTag :=
        TTag.Create(ReadString(
          'Tag',
          'lineno',
          'span style="' +
          'color: #999999; ' +
          'background: #202040; ' +
          'padding: 0px 4px 0px 4px;' +
          '"'));

      FOddTag :=
        TTag.Create(ReadString(
          'Tag', 'oddline', 'span style="background: #000000;"'));

      FEvenTag :=
        TTag.Create(ReadString(
          'Tag', 'evenline', 'span style="background: #202020;"'));
    finally
      Free;
    end;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);

    // ReservedWord
    ReadSectionLines(FileName, 'ReservedWord', FReserveds);

    // Command Word
    ReadSectionLines(FileName, 'CommandWord', FCommands);

    // Extra Word
    ReadSectionLines(FileName, 'ExtraWord', FExtras);

    // Comments
    Comments := TStringList.Create;
    try
      ReadSectionLines(FileName, 'Comment', SL);

      for Str in SL do begin
        Comments.Clear;
        Split(Comments, Str, ',');

        if (Comments.Count > 1) then begin
          CS := Comments[0];
          CE := Comments[1];

          FComments.Add(TComment.Create(CS, CE));

          AddCommentChars(CS);
          AddCommentChars(CE);
        end;
      end;
    finally
      Comments.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FReserveds.Free;
  FCommands.Free;
  FExtras.Free;
  FTempExtras.Free;
  FCommentChars.Free;
  FComments.Free;
end;

end.
