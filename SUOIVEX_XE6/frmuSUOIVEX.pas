unit frmuSUOIVEX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Ani, FMX.Objects, FMX.Styles.Objects;

type
  TfrmSUOIVEX = class(TForm)
    ScaledLayout1: TScaledLayout;
    Panel1: TPanel;
    StyleBook1: TStyleBook;
    procedure RectAnimation_Finish(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private êÈåæ }
    FArea: byte;
    FStartMethod: TMethod;
    procedure CallStart;
    procedure Clack(const iObj: TObject);
  public
    { public êÈåæ }
  end;

var
  frmSUOIVEX: TfrmSUOIVEX;

implementation

uses
  FMX.Log;

{$R *.fmx}

procedure TfrmSUOIVEX.CallStart;
type
  TStartProc = procedure of object;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(
        TThread.CurrentThread,
        procedure
        begin
          TStartProc(FStartMethod)();
        end
      );
    end
  ).Start;
end;

procedure TfrmSUOIVEX.Clack(const iObj: TObject);
var
  Instance: TFmxObject;
  Typ: TRttiType;
  RttiSourceLink: TRttiField;
  RttiStyleProp: TRttiField;
  RttiInstance: TRttiField;
  RttiBmpSourceRect: TRttiProperty;
  RttiMethod: TRttiMethod;
  BmpLinks: TObject;
  BmpLink: TBitmapLink;

  function GetRttiType(const iName: String): Boolean;
  begin
    Instance := Panel1.FindStyleResource(iName);

    if (Instance = nil) then
      Typ := nil
    else
      Typ := SharedContext.GetType(Instance.ClassType);

    Result := (Typ <> nil);
  end;

begin
  if (not GetRttiType('styleobject1')) then
    Exit;

  RttiSourceLink := Typ.GetField('FSourceLink');
  BmpLinks := RttiSourceLink.GetValue(Instance).AsObject;
  if (BmpLinks = nil) then
    Exit;

  BmpLink := TBitmapLinks(BmpLinks).Links[0];
  if (BmpLink = nil) then
    Exit;

  Typ := SharedContext.GetType(BmpLink.ClassType);
  if (Typ = nil) then
    Exit;

  RttiBmpSourceRect := Typ.GetProperty('SourceRect');
  if (RttiBmpSourceRect = nil) then
    Exit;

  if (not GetRttiType('rectanimation1')) then
    Exit;

  RttiStyleProp := Typ.GetField('FRttiProperty');
  RttiInstance := Typ.GetField('FInstance');

  if (RttiStyleProp = nil) or (RttiInstance = nil) then
    Exit;

  RttiInstance.SetValue(Instance, BmpLink);
  RttiStyleProp.SetValue(Instance, RttiBmpSourceRect);

  Typ := SharedContext.GetType(TAnimation);

  if (Typ <> nil) then begin
    RttiMethod := Typ.GetMethod('Start');

    FStartMethod.Data := iObj;
    FStartMethod.Code := RttiMethod.CodeAddress;
  end;
end;

procedure TfrmSUOIVEX.FormShow(Sender: TObject);
var
  res: TFmxObject;
begin
  OnShow := nil;
  FArea := 0;
  Panel1.StyleLookup := 'SUOIVEX_STYLE';
  res := Panel1.FindStyleResource('rectanimation1');
  if res <> nil then
    begin
      Clack(res);
      (res as TRectAnimation).OnFinish := RectAnimation_Finish;
      CallStart;
    end;
end;

procedure TfrmSUOIVEX.RectAnimation_Finish(Sender: TObject);
var
  res: TFmxObject;
  LeftPos: Integer;
begin
  Inc(FArea);
  FArea := FArea mod 16;
  res := Panel1.FindStyleResource('rectanimation1');
  if res <> nil then
    begin
      case FArea of
        $0: LeftPos := 512;
        $1: LeftPos := 800;
        $2: LeftPos := 128;
        $3: LeftPos := 688;
        $4: LeftPos := 288;
        $5: LeftPos := 592;
        $6: LeftPos :=   0;
        $7: LeftPos := 768;
        $8: LeftPos := 464;
        $9: LeftPos :=  64;
        $A: LeftPos := 688;
        $B: LeftPos := 768;
        $C: LeftPos := 352;
        $D: LeftPos := 128;
        $E: LeftPos := 592;
        $F: LeftPos := 800;
      else
        LeftPos := 512;
      end;
      (res as TRectAnimation).StartValue.Left  := LeftPos;
      (res as TRectAnimation).StartValue.Right := LeftPos + 224;
      (res as TRectAnimation).StopValue.Left   := LeftPos;
      (res as TRectAnimation).StopValue.Right  := LeftPos + 224;

      CallStart;
    end;
end;

end.
