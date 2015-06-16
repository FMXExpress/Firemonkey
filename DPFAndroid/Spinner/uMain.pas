unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JSpinner,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFProgressBar = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJSpinner1: TDPFJSpinner;
    DPFJTextView1: TDPFJTextView;
    DPFJTextView2: TDPFJTextView;
    DPFJSpinner2: TDPFJSpinner;
    procedure DPFJSpinner1ItemSelected( sender: TObject; position: Integer; id: Int64 );
    procedure DPFJSpinner2ItemSelected( sender: TObject; position: Integer; id: Int64 );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FProgressBar: TFProgressBar;

implementation

{$R *.fmx}

procedure TFProgressBar.DPFJSpinner1ItemSelected( sender: TObject; position: Integer; id: Int64 );
begin
  DPFJTextView1.Text.Text := DPFJSpinner1.Items[position];
end;

procedure TFProgressBar.DPFJSpinner2ItemSelected( sender: TObject; position: Integer; id: Int64 );
begin
  DPFJTextView2.Text.Text := DPFJSpinner2.Items[position];
end;

end.
