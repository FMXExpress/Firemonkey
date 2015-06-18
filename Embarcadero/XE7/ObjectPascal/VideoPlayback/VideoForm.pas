unit VideoForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls;

type
  TVideoPlayBackForm = class(TForm)
    videoBtn: TButton;
    MediaPlayer1: TMediaPlayer;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure videoBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VideoPlayBackForm: TVideoPlayBackForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
  System.iOUtils;

type
  TOpenMedia = class(TMedia);

procedure TVideoPlayBackForm.videoBtnClick(Sender: TObject);
begin
  {Under Project-Deployment, we added the media file and set the remote path.
  When the program starts, everything in the directory that is set under remote path is
  copied over to the Documents folder on the device. The MediaPlayer loads the file from
  the Documents folder}
  MediaPlayer1.FileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + 'Ocean.mp4';
  MediaPlayer1.Play;
end;

end.
