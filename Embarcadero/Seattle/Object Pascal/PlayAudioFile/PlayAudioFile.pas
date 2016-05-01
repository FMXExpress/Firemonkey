unit PlayAudioFile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls;

type
  TAudioPlayBackForm = class(TForm)
    ToolBar1: TToolBar;
    btnPlay: TSpeedButton;
    btnStop: TSpeedButton;
    Title: TLabel;
    MediaPlayer1: TMediaPlayer;
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AudioPlayBackForm: TAudioPlayBackForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
System.iOUtils;

procedure TAudioPlayBackForm.btnPlayClick(Sender: TObject);
begin
  {Under Project-Deployment, we added the media file and set the remote path.
  When the program starts, everything in the directory that is set under remote path is
   copied over to the Documents folder on the device. The MediaPlayer loads the file from
   the Documents folder}
  MediaPlayer1.FileName := TPath.GetDocumentsPath + PathDelim + 'soundsample.mp3';
  MediaPlayer1.Play;
end;
procedure TAudioPlayBackForm.btnStopClick(Sender: TObject);
begin
  MediaPlayer1.Stop;
end;

end.
