// --------------------------------------------------------------
//
// For Background Playing:
//
// 1) Goto Project->Options->Version Info:
// 2) Add this key in your version list:
// Key: DPFAudioKey1
// Value: </string><key>UIBackgroundModes</key><array><string>audio</string></array><key>DPFAudioKey2</key><string>
// --------------------------------------------------------------
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,

  FMX.Platform,
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,

  DPF.iOS.Common,
  DPF.iOS.UIView,
  DPF.iOS.UIToolbar,
  DPF.iOS.MPVolume,
  DPF.iOS.UILabel,
  DPF.iOS.AVPlayer,
  DPF.iOS.UISlider,
  DPF.iOS.UIProgressView,
  DPF.iOS.UIButton;

type

  TFAVAudioPlayer = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFMPVolume1: TDPFMPVolume;
    DPFLabel1: TDPFLabel;
    DPFAVPlayer1: TDPFAVPlayer;
    DPFButton3: TDPFButton;
    DPFButton1: TDPFButton;
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFAVPlayer1FinishPlaying( sender: TObject; var ContinuePlayList: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FAVAudioPlayer: TFAVAudioPlayer;

implementation

{$R *.fmx}

procedure TFAVAudioPlayer.DPFAVPlayer1FinishPlaying( sender: TObject; var ContinuePlayList: Boolean );
begin
  DPFAVPlayer1.Stop;
  DPFAVPlayer1.Play;
end;

procedure TFAVAudioPlayer.DPFButton1Click( Sender: TObject );
begin
  DPFAVPlayer1.Stop;
end;

procedure TFAVAudioPlayer.DPFButton3Click( Sender: TObject );
var
  S: string;
begin
  DPFAVPlayer1.PlayInBackground := true;
  DPFAVPlayer1.Open( '', '', '', '', GetAppFolder + 'Documents/music.mp3', true );
end;

procedure TFAVAudioPlayer.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
