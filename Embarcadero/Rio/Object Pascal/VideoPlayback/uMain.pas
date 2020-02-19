//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Media, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox;

type
  TfrmMain = class(TForm)
    Rectangle1: TRectangle;
    MediaPlayerControl1: TMediaPlayerControl;
    MediaPlayer1: TMediaPlayer;
    Timer1: TTimer;
    tbVolume: TTrackBar;
    ScrollBox1: TScrollBox;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    bToTheStart: TButton;
    Panel1: TPanel;
    bPlay: TButton;
    bStop: TButton;
    bToTheEnd: TButton;
    b10SecBackward: TButton;
    b10SecForward: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    bMovingParentUp: TButton;
    bMovingParentDown: TButton;
    bChangeParentsVisible: TButton;
    tbProcess: TTrackBar;
    procedure bPlayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure bStopClick(Sender: TObject);
    procedure bToTheStartClick(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bToTheEndClick(Sender: TObject);
    procedure b10SecBackwardClick(Sender: TObject);
    procedure b10SecForwardClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure bMovingParentDownClick(Sender: TObject);
    procedure bMovingParentUpClick(Sender: TObject);
    procedure bChangeParentsVisibleClick(Sender: TObject);
  private
    { Private declarations }
    FLibraryPath: string;
    function GetPathWithVideo: string;
    procedure FillFilesList;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils;

const
  TenSecond = 100000000;
  StepForShifting = 10;

{$R *.fmx}

procedure TfrmMain.b10SecBackwardClick(Sender: TObject);
begin
  if MediaPlayer1.CurrentTime > TenSecond then
    MediaPlayer1.CurrentTime := MediaPlayer1.CurrentTime - TenSecond
  else
    MediaPlayer1.CurrentTime := 0;
end;

procedure TfrmMain.b10SecForwardClick(Sender: TObject);
begin
  if MediaPlayer1.CurrentTime < (MediaPlayer1.Duration - TenSecond) then
    MediaPlayer1.CurrentTime := MediaPlayer1.CurrentTime + TenSecond
  else
    MediaPlayer1.CurrentTime := MediaPlayer1.Duration;
end;

procedure TfrmMain.bChangeParentsVisibleClick(Sender: TObject);
begin
  Rectangle1.Visible := not Rectangle1.Visible;
end;

procedure TfrmMain.bMovingParentDownClick(Sender: TObject);
begin
  Rectangle1.Position.Y := Rectangle1.Position.Y + StepForShifting;
end;

procedure TfrmMain.bMovingParentUpClick(Sender: TObject);
begin
  Rectangle1.Position.Y := Rectangle1.Position.Y - StepForShifting;
end;

procedure TfrmMain.bPlayClick(Sender: TObject);
begin
  MediaPlayer1.Play;
end;

procedure TfrmMain.bStopClick(Sender: TObject);
begin
  MediaPlayer1.Stop;
end;

procedure TfrmMain.bToTheEndClick(Sender: TObject);
begin
  MediaPlayer1.CurrentTime := MediaPlayer1.Duration;
end;

procedure TfrmMain.bToTheStartClick(Sender: TObject);
begin
  MediaPlayer1.CurrentTime := 0;
end;

procedure TfrmMain.FillFilesList;
var
  F: TSearchRec;
  Path: string;
  Attr: Integer;
begin
  Path := TPath.Combine(FLibraryPath, '*.mp4');
  Attr := faReadOnly + faArchive;
  FindFirst(Path, Attr, F);
  if F.name <> '' then
  begin
    ListBox1.Items.Add(F.name);
    while FindNext(F) = 0 do
      ListBox1.Items.Add(F.name);
  end;
  FindClose(F);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLibraryPath := GetPathWithVideo;
  FillFilesList;
  tbVolume.Value := MediaPlayer1.Volume;
end;

function TfrmMain.GetPathWithVideo: string;
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows: Result := '..\..\MP4\';
    TOSVersion.TPlatform.pfMacOS: Result := TPath.GetFullPath( '../Resources/StartUp');
    TOSVersion.TPlatform.pfiOS,
    TOSVersion.TPlatform.pfAndroid: Result := TPath.GetDocumentsPath;
    TOSVersion.TPlatform.pfWinRT,
    TOSVersion.TPlatform.pfLinux: raise Exception.Create('Unexpected platform');
  end;
end;

procedure TfrmMain.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  MediaPlayer1.Stop;
  MediaPlayer1.FileName := TPath.Combine(FLibraryPath, Item.Text);
end;

procedure TfrmMain.Panel1Click(Sender: TObject);
begin
  FLibraryPath := GetPathWithVideo;
  FillFilesList;
  tbVolume.Value := MediaPlayer1.Volume;
end;

procedure TfrmMain.tbVolumeChange(Sender: TObject);
begin
  MediaPlayer1.Volume := tbVolume.Value;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if tbProcess.Max <> MediaPlayer1.Duration then
    tbProcess.Max := MediaPlayer1.Duration;
  if tbProcess.Value <> MediaPlayer1.CurrentTime then
    tbProcess.Value := MediaPlayer1.CurrentTime;
end;

end.
