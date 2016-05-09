unit uCautionForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TfrmCaution = class(TForm)
    Image1: TImage;
    btnClose: TButton;
    Layout1: TLayout;
    procedure btnCloseClick(Sender: TObject);
  private
  public
    class procedure ShowSelf;
  end;

implementation

{$R *.fmx}

procedure TfrmCaution.btnCloseClick(Sender: TObject);
begin
  Close;
end;

class procedure TfrmCaution.ShowSelf;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(
        TThread.Current,
        procedure
        var
          Form: TfrmCaution;
        begin
          Application.CreateForm(TfrmCaution, Form);
          Form.ShowModal;
          Form.Release;
        end
      );
    end
  ).Start;
end;

end.
