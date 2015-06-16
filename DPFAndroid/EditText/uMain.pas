unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JEditText,
  FMX.Platform.Android,
  DPF.Android.JTextView,
  DPF.Android.JButton,
  DPF.Android.JRelativeLayout;

type
  TFEditText = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJEditText1: TDPFJEditText;
    DPFJEditText2: TDPFJEditText;
    DPFJTextView1: TDPFJTextView;
    DPFJTextView2: TDPFJTextView;
    procedure DPFJEditText2Changed( sender: TObject );
    procedure DPFJTextView2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FEditText: TFEditText;

implementation

{$R *.fmx}

procedure TFEditText.DPFJEditText2Changed( sender: TObject );
begin
  DPFJTextView1.Text.Text := DPFJEditText2.Text;
end;

procedure TFEditText.DPFJTextView2Click( Sender: TObject );
begin
  DPFJTextView2.Text.Clear;
end;

end.
