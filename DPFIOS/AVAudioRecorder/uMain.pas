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
  DPF.iOS.AVAudioRecorder,
  DPF.iOS.UISlider,
  DPF.iOS.UIProgressView,
  DPF.iOS.UIButton,
  DPF.iOS.ApplicationManager;

type

  TAVAudioRecorder = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFMPVolume1: TDPFMPVolume;
    DPFLabel1: TDPFLabel;
    DPFProgress1: TDPFProgress;
    DPFButtonStartRecord: TDPFButton;
    DPFButtonStopRecord: TDPFButton;
    DPFAVAudioRecorder1: TDPFAVAudioRecorder;
    DPFButtonPlay: TDPFButton;
    DPFAVPlayer1: TDPFAVPlayer;
    DPFButton1: TDPFButton;
    procedure DPFButtonStartRecordClick( Sender: TObject );
    procedure DPFButtonStopRecordClick( Sender: TObject );
    procedure DPFButtonPlayClick( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFAVAudioRecorder1Recording( sender: TObject; currentTime: Double );
    procedure DPFAVAudioRecorder1FinishRecording( sender: TObject; currentTime: Double );
    procedure DPFAVPlayer1Playing( sender: TObject; currentTime, duration: Int64 );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  AVAudioRecorder: TAVAudioRecorder;

implementation

{$R *.fmx}

procedure TAVAudioRecorder.DPFAVAudioRecorder1FinishRecording( sender: TObject; currentTime: Double );
begin
  DPFLabel1.Text := 'Record Finished Sec: ' + FloatToStr( currentTime );

end;

procedure TAVAudioRecorder.DPFAVAudioRecorder1Recording( sender: TObject; currentTime: Double );
begin
  DPFLabel1.Text := 'Record Sec: ' + FloatToStr( currentTime );

end;

procedure TAVAudioRecorder.DPFAVPlayer1Playing( sender: TObject; currentTime, duration: Int64 );
begin
  DPFLabel1.Text := 'Playing: ' + IntToStr( currentTime ) + '/' + IntToStr( duration );
end;

procedure TAVAudioRecorder.DPFButton1Click( Sender: TObject );
begin
  DPFAVPlayer1.Stop;
end;

procedure TAVAudioRecorder.DPFButtonPlayClick( Sender: TObject );
begin
  DPFAVPlayer1.Open( '', '', '', '', GetDocumentsFolder + 'test.caf', true );
end;

procedure TAVAudioRecorder.DPFButtonStartRecordClick( Sender: TObject );
begin
  DPFAVPlayer1.Stop;
  DPFAVPlayer1.Close;
  DPFAVAudioRecorder1.StopRecording;
  DPFAVAudioRecorder1.StartRecording( GetDocumentsFolder + 'test.caf', 10 );
end;

procedure TAVAudioRecorder.DPFButtonStopRecordClick( Sender: TObject );
begin
  DPFAVAudioRecorder1.StopRecording;
end;

procedure TAVAudioRecorder.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
