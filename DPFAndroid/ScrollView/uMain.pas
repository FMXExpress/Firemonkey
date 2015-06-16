unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JTextView,
  DPF.Android.JView,
  DPF.Android.JRadioGroup,
  DPF.Android.JScrollView,
  DPF.Android.JButton,
  DPF.Android.JImageView,
  DPF.Android.JAlertDialog, DPF.Android.JRelativeLayout;

type
  TFScrollView = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJScrollView1: TDPFJScrollView;
    DPFJView2: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJButton2: TDPFJButton;
    DPFJAlertDialog1: TDPFJAlertDialog;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJButton2Click( Sender: TObject );
  private
    { Private declarations }
    procedure ButtonClick( Sender: TObject );
  public
    { Public declarations }
  end;

var
  FScrollView: TFScrollView;

implementation

{$R *.fmx}

procedure TFScrollView.ButtonClick( Sender: TObject );
begin
  DPFJAlertDialog1.ShowMessageDialog( 'ScrollView Demo', 'Button ' + IntToStr( TDPFJButton( Sender ).Tag ) + ' Clicked!', ['Close'] );
end;

procedure TFScrollView.DPFJButton1Click( Sender: TObject );
var
  b  : TDPFJButton;
  img: TDPFJImageView;
  I  : Integer;
begin
  for I := 0 to 20 do
  begin
    b         := TDPFJButton.Create( DPFJScrollView1 );
    b.Align   := TAlignLayout.alHorzCenter;
    b.Width   := 250;
    b.Text    := 'Button ' + i.ToString;
    b.Tag     := I;
    b.OnClick := ButtonClick;
    DPFJScrollView1.AddControl( b );

    img          := TDPFJImageView.Create( DPFJScrollView1 );
    img.Align    := TAlignLayout.alHorzCenter;
    img.FileName := 'DPF.png';
    img.Width    := 250;
    img.Height   := 200;
    DPFJScrollView1.AddControl( img );
  end;
end;

procedure TFScrollView.DPFJButton2Click( Sender: TObject );
begin
  DPFJScrollView1.RemoveAllControls;
end;

end.
