unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JCheckBox,
  DPF.Android.JChronometer,
  DPF.Android.JButton,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFChronometer = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextView1: TDPFJTextView;
    DPFJButton1: TDPFJButton;
    DPFJChronometer1: TDPFJChronometer;
    DPFJButton2: TDPFJButton;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJButton2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FChronometer: TFChronometer;

implementation

{$R *.fmx}

procedure TFChronometer.DPFJButton1Click( Sender: TObject );
begin
  DPFJChronometer1.Start
end;

procedure TFChronometer.DPFJButton2Click( Sender: TObject );
begin
  DPFJChronometer1.Stop;
end;

end.
