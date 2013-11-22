unit Unit1;

interface

uses
  System.SysUtils
  , System.Types
  , System.Classes
  , System.Generics.Collections
  , FMX.Types
  , FMX.Controls
  , FMX.Graphics
  , FMX.Forms
  , FMX.StdCtrls
  , FMX.TabControl
  ;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  uFMXTabItemWithCloseBtn;

{$R *.fmx}

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  TabControl1.CheckCloseBtn;
end;

end.
