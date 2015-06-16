unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.Common, DPF.iOS.BaseControl,
  DPF.iOS.UIButton, DPF.iOS.MPMoviePlayerViewController, DPF.iOS.UILabel,
  DPF.iOS.UIView, DPF.iOS.CheckBox;

type
  TFMoviePlayer = class( TForm )
    DPFMPMoviePlayerViewController1: TDPFMPMoviePlayerViewController;
    DPFButtonLocalPlay: TDPFButton;
    DPFButtonURLPlay: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFCheckBox1: TDPFCheckBox;
    procedure DPFButtonLocalPlayClick( Sender: TObject );
    procedure DPFButtonURLPlayClick( Sender: TObject );
    procedure DPFMPMoviePlayerViewController1Error( Sender: TObject; Error: string );
    procedure DPFMPMoviePlayerViewController1PlaybackEnded( Sender: TObject; var CanClose: Boolean );
    procedure DPFMPMoviePlayerViewController1UserExited( Sender: TObject );
    procedure DPFCheckBox1Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMoviePlayer: TFMoviePlayer;

implementation

{$R *.fmx}

procedure TFMoviePlayer.DPFButtonLocalPlayClick( Sender: TObject );
begin
  DPFMPMoviePlayerViewController1.MoviePathType := mptFileName;
  DPFMPMoviePlayerViewController1.MoviePath     := GetAppFolder + 'Test.mp4';
  DPFMPMoviePlayerViewController1.play;
end;

procedure TFMoviePlayer.DPFButtonURLPlayClick( Sender: TObject );
begin
  DPFMPMoviePlayerViewController1.MoviePathType   := mptURL;
  DPFMPMoviePlayerViewController1.MoviePath       := 'http://clips.vorwaerts-gmbh.de/VfE_html5.mp4';
  DPFMPMoviePlayerViewController1.play;
end;

procedure TFMoviePlayer.DPFCheckBox1Click( Sender: TObject );
begin
  if DPFCheckBox1.Checked then
    DPFMPMoviePlayerViewController1.ShowInView := DPFUIView2
  else
    DPFMPMoviePlayerViewController1.ShowInView := nil;
end;

procedure TFMoviePlayer.DPFMPMoviePlayerViewController1Error( Sender: TObject; Error: string );
begin
  DPFLabel1.Text := Error;
end;

procedure TFMoviePlayer.DPFMPMoviePlayerViewController1PlaybackEnded( Sender: TObject; var CanClose: Boolean );
begin
  DPFLabel1.Text := 'Playback Ended';
end;

procedure TFMoviePlayer.DPFMPMoviePlayerViewController1UserExited( Sender: TObject );
begin
  DPFLabel1.Text := 'User Closed Player !';
end;

procedure TFMoviePlayer.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
