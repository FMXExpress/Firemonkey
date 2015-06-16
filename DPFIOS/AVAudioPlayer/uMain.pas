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
  DPF.iOS.UIButton,
  DPF.iOS.ApplicationManager;

type

  TFAVAudioPlayer = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFMPVolume1: TDPFMPVolume;
    DPFLabel1: TDPFLabel;
    DPFAVPlayer1: TDPFAVPlayer;
    DPFProgress1: TDPFProgress;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFButton5: TDPFButton;
    DPFButton6: TDPFButton;
    DPFButton7: TDPFButton;
    procedure DPFToolbar1BarItems1Click( Sender: TObject );
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFAVPlayer1Playing( sender: TObject; currentTime: Int64; duration: int64 );
    procedure FormShow( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );
    procedure DPFButton5Click( Sender: TObject );
    procedure DPFButton6Click( Sender: TObject );
    procedure DPFButton7Click( Sender: TObject );
    procedure DPFAVPlayer1FinishPlaying( sender: TObject; var ContinuePlayList: Boolean );
    procedure DPFAVPlayer1OpenStatus( sender: TObject; const Status: Integer );
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
  DPFLabel1.Text := ' Finished Playing ';
end;

procedure TFAVAudioPlayer.DPFAVPlayer1OpenStatus( sender: TObject; const Status: Integer );
begin
  DPFLabel1.Text := 'Open File Status: ' + Status.ToString( );
end;

procedure TFAVAudioPlayer.DPFAVPlayer1Playing( sender: TObject; currentTime: Int64; duration: int64 );
var
  minutes1: int64;
  seconds1: int64;
  minutes2: int64;
  seconds2: int64;
begin
  minutes1 := floor( currentTime / 60 );
  seconds1 := floor( currentTime - minutes1 * 60 );

  minutes2 := 0;
  seconds2 := 0;
  if duration > 0 then
  begin
    minutes2              := floor( ( duration - currentTime ) / 60 );
    seconds2              := floor( duration - currentTime - minutes2 * 60 );
    DPFProgress1.Progress := currentTime / duration;
  end
  else
    DPFProgress1.Progress := 0;

  DPFLabel1.Text := Format( 'Time: %d:%d - Remain: %d:%d', [minutes1, seconds1, minutes2, seconds2] );
end;

procedure TFAVAudioPlayer.DPFButton1Click( Sender: TObject );
begin
  PlaySystemSound( );
end;

procedure TFAVAudioPlayer.DPFButton2Click( Sender: TObject );
begin
  PlaySystemSound( 1151 );
end;

procedure TFAVAudioPlayer.DPFButton3Click( Sender: TObject );
var
  isPlaying, isPaused, isStopped: Boolean;
begin
  DPFAVPlayer1.Open( '', '', '', '', GetAppFolder + 'Documents/music.mp3', true );
  isPlaying := DPFAVPlayer1.isPlaying;
  isPaused  := DPFAVPlayer1.isPaused;
  isStopped := DPFAVPlayer1.isStopped;
end;

procedure TFAVAudioPlayer.DPFButton4Click( Sender: TObject );
var
  isPlaying, isPaused, isStopped: Boolean;
begin
  DPFAVPlayer1.Pause;
  isPlaying := DPFAVPlayer1.isPlaying;
  isPaused  := DPFAVPlayer1.isPaused;
  isStopped := DPFAVPlayer1.isStopped;
end;

procedure TFAVAudioPlayer.DPFButton5Click( Sender: TObject );
var
  isPlaying, isPaused, isStopped: Boolean;
begin
  DPFAVPlayer1.Stop;
  isPlaying := DPFAVPlayer1.isPlaying;
  isPaused  := DPFAVPlayer1.isPaused;
  isStopped := DPFAVPlayer1.isStopped;
end;

procedure TFAVAudioPlayer.DPFButton6Click( Sender: TObject );
begin
  DPFAVPlayer1.Play;
end;

procedure TFAVAudioPlayer.DPFButton7Click( Sender: TObject );
begin
  DPFAVPlayer1.Close;
end;

procedure TFAVAudioPlayer.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFAVPlayer1.Open( '', '', '', '', GetAppFolder + 'Documents/music.mp3', true );
  DPFAVPlayer1.isPlaying;
end;

procedure TFAVAudioPlayer.DPFToolbar1BarItems1Click( Sender: TObject );
begin
  // DPFAVPlayer1.Open('', '', '', '',  'http://webradio.antennevorarlberg.at:80/classicrock', true );
  DPFAVPlayer1.Open( '', '', '', '', 'http://www.ifilmtv.ir//Media/20131104/1311040715557500_.mp3', true );
end;

procedure TFAVAudioPlayer.FormShow( Sender: TObject );
begin
  { }
end;

procedure TFAVAudioPlayer.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
