unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Layout1: TLayout;
    ProgressBar1: TProgressBar;
    Edit1: TEdit;
    Label1: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    procedure Button1Click(Sender: TObject);
  private
    procedure HttpReceiveData(
      const Sender: TObject;
      iContentLength, iReadCount: Int64;
      var ioAbort: Boolean);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils
  , System.Net.HttpClient
  //, uFileStreamFixForAndroid4
  ;

{$R *.fmx}

const
  URL = 'https://qiita-image-store.s3.amazonaws.com/0/12977/af4a05d5-dea6-979e-c367-d80241056406.png';

procedure TForm1.Button1Click(Sender: TObject);
begin
  ProgressBar1.Value := 0;

  TThread.CreateAnonymousThread(
    procedure
    var
      Http: THttpClient;
      FS: TFileSTream;
      FileName: String;
      Size: Integer;
    begin
      FileName := TPath.Combine(TPath.GetDocumentsPath, 'test.jpg');

      FS := TFileStream.Create(FileName, fmCreate, $1ff);
      try
        Http := THttpClient.Create;
        try
          Http.HandleRedirects := True;
          Http.OnReceiveData := HttpReceiveData;

          Http.Get(URL, FS);

          Size := FS.Size;
        finally
          Http.DisposeOf;
        end;
      finally
        FS.DisposeOf;
      end;

      TThread.Synchronize(
        TThread.Current,
        procedure
        begin
          Edit1.Text := Size.ToString;
          Image1.Bitmap.LoadFromFile(FileName);
        end
      );
    end
  ).Start;
end;

procedure TForm1.HttpReceiveData(
  const Sender: TObject;
  iContentLength, iReadCount: Int64;
  var ioAbort: Boolean);
begin
  TThread.Synchronize(
    TThread.Current,
    procedure
    begin
      ProgressBar1.Max := iContentLength;
      ProgressBar1.Value := iReadCount;
    end
  );
end;

end.
