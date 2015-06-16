unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JVideoView,
  DPF.Android.JButton,
  DPF.Android.JRelativeLayout, DPF.Android.JLinearLayout;

type
  TFVideoView = class( TForm )
    DPFJVideoView1: TDPFJVideoView;
    DPFJButton1: TDPFJButton;
    DPFJLinearLayout1: TDPFJLinearLayout;
    procedure DPFJButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FVideoView: TFVideoView;

implementation

{$R *.fmx}

procedure TFVideoView.DPFJButton1Click( Sender: TObject );
begin
  //DPFJVideoView1.Play( 'http://download.wavetlan.com/SVV/Media/HTTP/H264/Talkinghead_Media/H264_test1_Talkinghead_mp4_480x360.mp4' );
  DPFJVideoView1.Play( 'http://download.wavetlan.com/SVV/Media/HTTP/3GP/HelixMobileProducer/HelixMobileProducer_test5_3GPv5_MPEG4SP_24bit_176x144_AR1.22_30fps_KFx_320kbps_AAC-LC_Mono_11025Hz_24kbps.3gp' );
end;

end.
