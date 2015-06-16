unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FMX.Helpers.Android,
  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JProgressDialog,
  DPF.Android.JButton,
  DPF.Android.JAlertDialog, DPF.Android.JRelativeLayout;

type
  TFProgressDialog = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJProgressDialog1: TDPFJProgressDialog;
    DPFJButton1: TDPFJButton;
    Timer1: TTimer;
    DPFJAlertDialog1: TDPFJAlertDialog;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJProgressDialog1Cancel( sender: TObject );
    procedure Timer1Timer( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FProgressDialog: TFProgressDialog;

implementation

{$R *.fmx}

procedure TFProgressDialog.DPFJButton1Click( Sender: TObject );
begin
  DPFJProgressDialog1.Max      := 1000;
  DPFJProgressDialog1.Progress := 0;
  DPFJProgressDialog1.Title    := 'Progress Dialog Title';
  DPFJProgressDialog1.Message  := 'Progress Dialog Message';
  DPFJProgressDialog1.Show( TDPFProgressDialogStyle.dsHORIZONTAL );
  Timer1.Enabled := true;
end;

procedure TFProgressDialog.DPFJProgressDialog1Cancel( sender: TObject );
begin
  Timer1.Enabled := false;
  DPFJAlertDialog1.ShowMessageDialog( '', 'Progress Dialog Canceled!', ['Close!'] )
end;

procedure TFProgressDialog.Timer1Timer( Sender: TObject );
begin
  DPFJProgressDialog1.Progress := DPFJProgressDialog1.Progress + 10;
  if DPFJProgressDialog1.Progress > DPFJProgressDialog1.Max then
    DPFJProgressDialog1.Progress := 0;
end;

end.
