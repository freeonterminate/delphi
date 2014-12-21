unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers;

type
  // JString は Androidapi.JNI.JavaTypes に
  // JIntent は Androidapi.JNI.GraphicsContentViewText に
  // JStringToString は Androidapi.Helpers に
  // それぞれ定義されています
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure Received(const iAction: JString);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  uBraodcastReceiver;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  BroadcastReceiver.OnReceived := Received;

  // スクリーン ON / OFF を受け取るように設定
  BroadcastReceiver.AddAction(
    [
      TJIntent.JavaClass.ACTION_SCREEN_OFF,
      TJIntent.JavaClass.ACTION_SCREEN_ON
    ]
  );
end;

procedure TForm1.Received(const iAction: JString);
begin
  Log.d('Broadcast Received = ' + JStringToString(iAction));
end;

end.
