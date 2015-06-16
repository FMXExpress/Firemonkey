unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JButton,
  DPF.Android.JTextView,
  DPF.Android.JRelativeLayout;

type
  TFDatePicker = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextView1: TDPFJTextView;
    DPFJView2: TDPFJRelativeLayout;
    DPFJView3: TDPFJRelativeLayout;
    DPFJView4: TDPFJRelativeLayout;
    DPFJView5: TDPFJRelativeLayout;
    procedure DPFJButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FDatePicker: TFDatePicker;

implementation

{$R *.fmx}

procedure TFDatePicker.DPFJButton1Click( Sender: TObject );
begin
  //DPFJView2.Position.X := DPFJView2.Position.X + 100;
end;

end.
