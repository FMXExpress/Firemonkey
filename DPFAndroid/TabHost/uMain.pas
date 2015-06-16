unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JTabHost,
  DPF.Android.JAnalogClock,
  DPF.Android.JImageView,
  DPF.Android.JRadioGroup,
  DPF.Android.JProgressBar,
  DPF.Android.JSpinner,
  DPF.Android.JSeekBar, DPF.Android.JRelativeLayout;

type
  TFTabHost = class( TForm )
    DPFJTabHost1: TDPFJTabHost;
    DPFAndroidTabBarItem1: TDPFAndroidTabBarItem;
    DPFAndroidTabBarItem2: TDPFAndroidTabBarItem;
    DPFAndroidTabBarItem3: TDPFAndroidTabBarItem;
    DPFJImageView1: TDPFJImageView;
    DPFJAnalogClock1: TDPFJAnalogClock;
    DPFJRadioGroup1: TDPFJRadioGroup;
    DPFJSeekBar1: TDPFJSeekBar;
    DPFJRelativeLayout1: TDPFJRelativeLayout;
    procedure FormCreate( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FTabHost: TFTabHost;

implementation

{$R *.fmx}

procedure TFTabHost.FormCreate( Sender: TObject );
begin
  Self.Width  := 100;
  Self.Height := 100;
end;

end.
