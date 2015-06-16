unit uMain;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.Dialogs,

{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.Media,
  DPF.iOS.UIToolbar,
  DPF.iOS.UIImageView,
  DPF.iOS.MPMoviePlayerViewController,
  DPF.iOS.UIProgressView,
  DPF.iOS.UISwitch,
  DPF.iOS.UILabel,
  DPF.iOS.NSTimer;

type
  TFiOSVideoCap = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFCamera1: TDPFCamera;
    DPFUIViewPreview: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFButtonStop: TDPFButton;
    DPFMPMoviePlayerViewController1: TDPFMPMoviePlayerViewController;
    DPFButtonPlay: TDPFButton;
    DPFSwitch1: TDPFSwitch;
    DPFLabel1: TDPFLabel;
    DPFLabelTime: TDPFLabel;
    DPFButton1: TDPFButton;
    DPFUIView2: TDPFUIView;
    DPFButtonStartPaue: TDPFButton;
    DPFImageView1: TDPFImageView;
    DPFProgress1: TDPFProgress;
    DPFNSTimer1: TDPFNSTimer;
    procedure DPFButtonStopClick( Sender: TObject );
    procedure DPFButtonPlayClick( Sender: TObject );
    procedure DPFCamera1ConvertComplete( Sender: TObject; Status: Integer );
    procedure DPFCamera1ConvertProgress( Sender: TObject; Progress: Single );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure FormShow( Sender: TObject );
    procedure DPFCamera1SampleBufferReady( Sender: TObject; const ATime: Int64 );
    procedure DPFButtonStartPaueClick( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFCamera1CaptureFinished( Sender: TObject; var SaveToAlbumLibrary: Boolean );
    procedure DPFNSTimer1Timer( Sender: TObject );
    procedure DPFUIViewPreviewClick( Sender: TObject );
  private
    { Private declarations }
    lastATime: Int64;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FiOSVideoCap: TFiOSVideoCap;

implementation

{$R *.fmx}

function SecToTime( Sec: Integer ): string;
var
  ZH, ZM, ZS: Integer;
begin
  ZH     := Sec div 3600;
  ZM     := Sec div 60 - ZH * 60;
  ZS     := Sec - ( ZH * 3600 + ZM * 60 );
  Result := AddLeadingZeroes( ZM, 2 ) + ':' + AddLeadingZeroes( ZS, 2 );
end;

procedure TFiOSVideoCap.DPFButtonStartPaueClick( Sender: TObject );
begin
  lastATime             := 0;
  DPFProgress1.Progress := 0;
  if DPFCamera1.RecordingState in [rsStop] then
  begin
    DeleteFile( DPFCamera1.OutputFileName );
    DPFCamera1.OutputFileName := GetTempDirectory + GetUUID + 'temp.mov';
    DPFCamera1.SetupDevice;
  end;

  if DPFCamera1.RecordingState in [rsPause, rsReady] then
  begin
    DPFButtonStartPaue.Selected := true;
    DPFCamera1.StartRecording;
    DPFNSTimer1.Enabled := true;
  end
  else
  begin
    DPFImageView1.Visible       := false;
    DPFButtonStartPaue.Selected := false;
    DPFCamera1.PauseRecording;
  end
end;

procedure TFiOSVideoCap.DPFButton1Click( Sender: TObject );
begin
  DPFCamera1.switchCameras;
end;

procedure TFiOSVideoCap.DPFButtonPlayClick( Sender: TObject );
begin
  if DPFCamera1.RecordingState in [rsStart, rsPause, rsReady] then
    exit;

  DPFMPMoviePlayerViewController1.MoviePath := DPFCamera1.OutputFileName;
  DPFMPMoviePlayerViewController1.Play;
end;

procedure TFiOSVideoCap.DPFButtonStopClick( Sender: TObject );
begin
  DPFNSTimer1.Enabled         := false;
  DPFToolbar1.TagBoolean      := true;
  DPFImageView1.Visible       := false;
  DPFButtonStartPaue.Selected := false;
  DPFCamera1.StopRecording;
end;

// ------------------------------------------------------------------------------
// Status:
// 0 = Unknown
// 1 = Waiting
// 2 = Exporting;
// 3 = Completed;
// 4 = Failed
// 5 = Cancelled
// ------------------------------------------------------------------------------
procedure TFiOSVideoCap.DPFCamera1CaptureFinished( Sender: TObject; var SaveToAlbumLibrary: Boolean );
begin
  if not DPFToolbar1.TagBoolean then
  begin
    DPFToolbar1.TagBoolean := true;
    DPFToolbar1.SetAnimationTransition( 0, 0, -DPFToolbar1.Height, 0, DPFToolbar1.Width, DPFToolbar1.Width, DPFToolbar1.Height, DPFToolbar1.Height );
  end;
end;

procedure TFiOSVideoCap.DPFCamera1ConvertComplete( Sender: TObject; Status: Integer );
begin
  if Status = 3 then
    ShowAlert( 'Convert Completed ' )
  else
    ShowAlert( 'Convert Stause: ' + Status.ToString );
end;

procedure TFiOSVideoCap.DPFCamera1ConvertProgress( Sender: TObject; Progress: Single );
begin
  DPFProgress1.Progress := Progress;
end;

procedure TFiOSVideoCap.DPFCamera1SampleBufferReady( Sender: TObject; const ATime: Int64 );
begin
  DPFLabelTime.Text := SecToTime( ATime );
  if ATime - lastATime > 0 then
  begin
    DPFImageView1.Visible := not DPFImageView1.Visible;
    lastATime             := ATime;
  end;
end;

procedure TFiOSVideoCap.DPFNSTimer1Timer( Sender: TObject );
begin
  DPFNSTimer1.Enabled := false;
  DPFToolbar1.SetAnimationTransition( 0, 0, 0, -DPFToolbar1.Height, DPFToolbar1.Width, DPFToolbar1.Width, DPFToolbar1.Height, DPFToolbar1.Height );
  DPFToolbar1.TagBoolean := false;
end;

procedure TFiOSVideoCap.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFCamera1.StopRecording;
  DPFCamera1.ConvertToMP4 := ISON;
end;

procedure TFiOSVideoCap.DPFUIViewPreviewClick( Sender: TObject );
begin
  if not DPFToolbar1.TagBoolean then
  begin
    DPFToolbar1.TagBoolean := false;
    DPFToolbar1.SetAnimationTransition( 0, 0, -DPFToolbar1.Height, 0, DPFToolbar1.Width, DPFToolbar1.Width, DPFToolbar1.Height, DPFToolbar1.Height );
    DPFNSTimer1.Enabled := true;
  end;
end;

procedure TFiOSVideoCap.FormShow( Sender: TObject );
begin
  DeleteFile( DPFCamera1.OutputFileName );
  DPFCamera1.OutputFileName := GetTempDirectory + GetUUID + 'temp.mov';
  DPFCamera1.SetupDevice;
end;

procedure TFiOSVideoCap.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
