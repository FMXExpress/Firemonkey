unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls,
  FMX.Objects, System.Actions, FMX.ActnList;

const
  AUDIO_FILENAME = 'test.caf';

type
  TAudioRecPlayForm = class(TForm)
    btnStartRec: TButton;
    btnStopRec: TButton;
    btnStartPlay: TButton;
    btnStopPlay: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    imgOff: TImage;
    imgOn: TImage;
    MediaPlayer: TMediaPlayer;
    ActionList: TActionList;
    actStartRecording: TAction;
    actStopRecording: TAction;
    actPlay: TAction;
    actStop: TAction;
    procedure actStartRecordingExecute(Sender: TObject);
    procedure actStopRecordingExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure imgOnClick(Sender: TObject);
    procedure imgOffClick(Sender: TObject);
  public
    FMicrophone: TAudioCaptureDevice;
    function HasMicrophone: Boolean;
    function IsMicrophoneRecording: Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

var
  AudioRecPlayForm: TAudioRecPlayForm;

implementation

uses
  IOUtils;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

{ GetAudioFileName resolves the audio file path for either platform. }

function GetAudioFileName(const AFileName: string): string;
begin
{$IFDEF ANDROID}
  Result := TPath.GetTempPath + '/' + AFileName;
{$ELSE}
  {$IFDEF IOS}
    Result := TPath.GetHomePath + '/Documents/' + AFileName;
  {$ELSE}
    Result := AFileName;
  {$ENDIF}
{$ENDIF}
end;

procedure TAudioRecPlayForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  { Provide feedback on capture on/off}

  imgOn.Visible := HasMicrophone and (FMicrophone.State = TCaptureDeviceState.Capturing);

  { ... and enable buttons accordingly }

  actStartRecording.Enabled := not IsMicrophoneRecording and HasMicrophone;
  actStopRecording.Enabled := IsMicrophoneRecording;
  actStop.Enabled := Assigned(MediaPlayer.Media) and (MediaPlayer.State = TMediaState.Playing);
  actPlay.Enabled := FileExists(GetAudioFileName(AUDIO_FILENAME)) and (MediaPlayer.State <> TMediaState.Playing);
end;

procedure TAudioRecPlayForm.actPlayExecute(Sender: TObject);
begin
  if IsMicrophoneRecording then
    actStopRecording.Execute;

  MediaPlayer.FileName := GetAudioFileName(AUDIO_FILENAME);
  MediaPlayer.Play;
end;

procedure TAudioRecPlayForm.actStartRecordingExecute(Sender: TObject);
begin
  actStop.Execute;

  { get the microphone device }
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
  if HasMicrophone then
  begin
    { and attempt to record to 'test.caf' file }
    FMicrophone.FileName := GetAudioFileName(AUDIO_FILENAME);
    try
      FMicrophone.StartCapture;
    except
      ShowMessage('StartCapture: Operation not supported by this device');
    end;
  end
  else
    ShowMessage('No microphone is available.');
end;

procedure TAudioRecPlayForm.actStopExecute(Sender: TObject);
begin
  MediaPlayer.Stop;
end;

procedure TAudioRecPlayForm.actStopRecordingExecute(Sender: TObject);
begin
  { stop capturing audio from the microphone }
  if IsMicrophoneRecording then
    try
      FMicrophone.StopCapture;
    except
      ShowMessage('Get state: Operation not supported by this device');
    end;
end;

procedure TAudioRecPlayForm.imgOffClick(Sender: TObject);
begin
  { we want the same functionality as clicking the recording button }
  actStartRecording.Execute;
end;

procedure TAudioRecPlayForm.imgOnClick(Sender: TObject);
begin
  actStopRecording.Execute;
end;

constructor TAudioRecPlayForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
end;

function TAudioRecPlayForm.HasMicrophone: Boolean;
begin
  Result := Assigned(FMicrophone);
end;

function TAudioRecPlayForm.IsMicrophoneRecording: Boolean;
begin
  Result := HasMicrophone and (FMicrophone.State = TCaptureDeviceState.Capturing);
end;

end.
