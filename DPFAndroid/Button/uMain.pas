unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JButton,
  DPF.Android.JAlertDialog,
  DPF.Android.JEditText, DPF.Android.JRelativeLayout;

type
  TFButton = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJAlertDialog1: TDPFJAlertDialog;
    DPFJButton2: TDPFJButton;
    DPFJButton3: TDPFJButton;
    DPFJButton6: TDPFJButton;
    DPFJButton4: TDPFJButton;
    DPFJButton5: TDPFJButton;
    procedure DPFJButton1Click( Sender: TObject );
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  FButton: TFButton;

implementation

{$R *.fmx}

procedure TFButton.DPFJButton1Click( Sender: TObject );
begin
  DPFJAlertDialog1.ShowMessageDialog( 'Button 1 Demo Alert', 'Button Clicked!', ['Yes', 'No'] );
end;

end.
