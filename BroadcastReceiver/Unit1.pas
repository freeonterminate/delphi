unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls,

  uBraodcastReceiver,
  //FMX.LifecycleManager,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers;

type
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
  FMX.Log;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  BroadcastReceiver.OnReceived := Received;
  BroadcastReceiver.AddAction(TJIntent.JavaClass.ACTION_SCREEN_OFF);
  BroadcastReceiver.AddAction(TJIntent.JavaClass.ACTION_SCREEN_ON);
end;

procedure TForm1.Received(const iAction: JString);
begin
  Log.d('Broadcast Received = ' + JStringToString(iAction));
end;

end.
