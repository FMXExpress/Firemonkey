unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.Common,
  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JWebView,
  DPF.Android.JButton,
  DPF.Android.JTextView,
  DPF.Android.JEditText,
  DPF.Android.JRelativeLayout;

type
  TFWebview = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJWebView1: TDPFJWebView;
    DPFJView2: TDPFJRelativeLayout;
    DPFJButtonGo: TDPFJButton;
    DPFJTextView1: TDPFJTextView;
    DPFJEditText1: TDPFJEditText;
    procedure DPFJButtonGoClick( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FWebview: TFWebview;

implementation

{$R *.fmx}

procedure TFWebview.DPFJButtonGoClick( Sender: TObject );
begin
  DPFJWebView1.LoadURL( DPFJEditText1.Text, [MakeWebHeaders( 'ACCEPT', 'text/html' )] );
end;

end.
