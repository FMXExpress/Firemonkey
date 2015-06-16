
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MediaPlayerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts, FMX.Media,
  FMX.Filter, FMX.Filter.Effects, FMX.StdCtrls;

type
  TForm240 = class(TForm)
    OpenDialog1: TOpenDialog;
    OpenButton: TSpeedButton;
    Rectangle1: TRectangle;
    PauseButton: TSpeedButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    VolumeTrack: TTrackBar;
    MediaPlayer1: TMediaPlayer;
    MediaPlayerControl1: TMediaPlayerControl;
    ClearButton: TSpeedButton;
    procedure OpenButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure VolumeTrackChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form240: TForm240;

implementation

{$R *.fmx}

procedure TForm240.CheckBox1Change(Sender: TObject);
begin
  MediaPlayerControl1.Visible := CheckBox1.IsChecked;
end;

procedure TForm240.OpenButtonClick(Sender: TObject);
begin
  OpenDialog1.Filter := TMediaCodecManager.GetFilterString;
  if OpenDialog1.Execute then
  begin
    PauseButton.Enabled := True;
    ClearButton.Enabled := True;
    MediaPlayer1.FileName := OpenDialog1.FileName;

    if MediaPlayer1.Media <> nil then
    begin
      Label1.Text := IntToStr(MediaPlayer1.Media.VideoSize.Truncate.X) + 'x' + IntToStr(MediaPlayer1.Media.VideoSize.Truncate.Y) + 
        'px ' + IntToStr(MediaPlayer1.Media.Duration div MediaTimeScale) + 'ms';
      TrackBar1.Max := MediaPlayer1.Media.Duration;
      VolumeTrack.Value := 1 - MediaPlayer1.Media.Volume;

      MediaPlayer1.Play;
    end;
  end;
end;

procedure TForm240.PauseButtonClick(Sender: TObject);
begin
  if MediaPlayer1.State = TMediaState.Playing then
  begin 
    PauseButton.Text := 'Play';
    MediaPlayer1.Stop;
  end
  else
  begin
    PauseButton.Text := 'Pause';
    MediaPlayer1.Play;
  end;
end;

procedure TForm240.ClearButtonClick(Sender: TObject);
begin
  MediaPlayer1.Clear;
  PauseButton.Enabled := False;
  ClearButton.Enabled := False;
end;

procedure TForm240.Timer1Timer(Sender: TObject);
begin
  TrackBar1.Tag := 1;
  TrackBar1.Value := MediaPlayer1.CurrentTime;
  TrackBar1.Tag := 0;
end;

procedure TForm240.TrackBar1Change(Sender: TObject);
begin
  if TrackBar1.Tag = 0 then
    MediaPlayer1.CurrentTime := Round(TrackBar1.Value);
end;

procedure TForm240.VolumeTrackChange(Sender: TObject);
begin
  MediaPlayer1.Volume := 1 - VolumeTrack.Value; 
end;

end.
