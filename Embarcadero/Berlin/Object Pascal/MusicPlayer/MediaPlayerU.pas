//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit MediaPlayerU;

interface

uses
  MusicPlayer.Utils,
  {$IFDEF IOS}
  MusicPlayer.iOS,
  {$ENDIF}
  {$IFDEF ANDROID}
  MusicPlayer.Android,
  {$ENDIF}
  FMX.Types, System.SysUtils, System.UITypes, System.Classes,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.ListBox, FMX.Layouts, FMX.TabControl, System.Actions,
  FMX.ListView.Types, FMX.ListView, FMX.Dialogs, FMX.MobilePreview, FMX.MultiView, FMX.Controls.Presentation,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TFMXMusicPlayerFrm = class(TForm)
    tcUITabs: TTabControl;
    tiAlbums: TTabItem;
    tiSongs: TTabItem;
    tiNowPlaying: TTabItem;
    lvAlbums: TListView;
    lvSongs: TListView;
    tbNowPlaying: TToolBar;
    btnPlay: TButton;
    btnPrev: TButton;
    btnPause: TButton;
    btnNext: TButton;
    btnStop: TButton;
    lyState: TLayout;
    lblArtist: TLabel;
    lblTitle: TLabel;
    lblAlbum: TLabel;
    lblDuration: TLabel;
    lblArtistVal: TLabel;
    lblDurationVal: TLabel;
    lblTitleVal: TLabel;
    lblAlbumVal: TLabel;
    tbProgress: TTrackBar;
    SettingsList: TListBox;
    RepeatModes: TListBoxGroupHeader;
    All: TListBoxItem;
    One: TListBoxItem;
    None: TListBoxItem;
    Default: TListBoxItem;
    ShuffleMusic: TListBoxGroupHeader;
    ShufffleMode: TListBoxItem;
    swShuffleMode: TSwitch;
    VolumeHeader: TListBoxGroupHeader;
    VolumeListItem: TListBoxItem;
    VolumeTrackBar: TTrackBar;
    mvSettings: TMultiView;
    lyProgressSettings: TLayout;
    btnSettings: TButton;
    volTimer: TTimer;
    tbSettings: TToolBar;
    lblSettings: TLabel;
    btnCloseSettings: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lvAlbumsChange(Sender: TObject);
    procedure lvSongsChange(Sender: TObject);
    procedure tbProgressChange(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure RepeatItemsClick(Sender: TObject);
    procedure swShuffleModeSwitch(Sender: TObject);
    procedure VolumeTrackBarChange(Sender: TObject);
    procedure volTimerTimer(Sender: TObject);
    procedure btnCloseSettingsClick(Sender: TObject);
  private
    procedure DoUpdateUI(newPos: Single);
    procedure UpdateNowPlaying(newIndex: Integer);
    procedure UpdateSongs;
    procedure SongChanged(newIndex: Integer);
    procedure StateChanged(state: TMPPlaybackState);
  public
    { Public declarations }
  end;



var
  FMXMusicPlayerFrm: TFMXMusicPlayerFrm;

implementation

{$R *.fmx}


procedure TFMXMusicPlayerFrm.btnNextClick(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Next;
  StateChanged(TMusicPlayer.DefaultPlayer.PlaybackState);
end;

procedure TFMXMusicPlayerFrm.btnPauseClick(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Pause;
  StateChanged(TMPPlaybackState.Paused);
end;

procedure TFMXMusicPlayerFrm.btnPlayClick(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Play;
  StateChanged(TMPPlaybackState.Playing);
end;

procedure TFMXMusicPlayerFrm.btnPrevClick(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Previous;
  StateChanged(TMusicPlayer.DefaultPlayer.PlaybackState);
end;

procedure TFMXMusicPlayerFrm.btnStopClick(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Stop;
  StateChanged(TMPPlaybackState.Stopped);
end;

procedure TFMXMusicPlayerFrm.btnCloseSettingsClick(Sender: TObject);
begin
  mvSettings.HideMaster;
end;

procedure TFMXMusicPlayerFrm.DoUpdateUI(newPos: Single);
var
  handler: TNotifyEvent;
begin
  handler := tbProgress.OnChange;
  tbProgress.OnChange := nil;
  tbProgress.Value := newPos;
  tbProgress.OnChange := handler;
end;

procedure TFMXMusicPlayerFrm.FormCreate(Sender: TObject);
var
  Item: TListViewItem;
  album: TMPAlbum;
begin
  {$ifdef ANDROID}
  tcUITabs.TabPosition := TTabPosition.Top;
  {$endif}
  TMusicPlayer.DefaultPlayer.OnSongChange := SongChanged;
  TMusicPlayer.DefaultPlayer.OnProcessPlay := DoUpdateUI;
  TMusicPlayer.DefaultPlayer.GetAlbums;
  TMusicPlayer.DefaultPlayer.GetSongs;
  if Length(TMusicPlayer.DefaultPlayer.Albums) >= 2 then
  begin
    lvAlbums.BeginUpdate;
    for album in TMusicPlayer.DefaultPlayer.Albums do
    begin
      Item := lvAlbums.Items.Add;
      Item.Text := album.Name;
      Item.Detail := album.Artist;
      Item.Bitmap := album.Artwork
    end;
    lvAlbums.EndUpdate;
    UpdateSongs;
    RepeatItemsClick(All);
    StateChanged(TMPPlaybackState.Stopped);
  end
  else
    ShowMessage('There is no music on this device');
end;

procedure TFMXMusicPlayerFrm.lvAlbumsChange(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.GetSongsInAlbum(TMusicPlayer.DefaultPlayer.Albums[lvAlbums.ItemIndex].Name);
  UpdateSongs;
  tcUITabs.SetActiveTabWithTransition(tiSongs,TTabTransition.Slide);
end;

procedure TFMXMusicPlayerFrm.lvSongsChange(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.PlayByIndex(lvSongs.ItemIndex);
  UpdateNowPlaying(lvSongs.ItemIndex);
  tcUITabs.SetActiveTabWithTransition(tiNowPlaying,TTabTransition.Slide);
  StateChanged(TMPPlaybackState.Playing);
end;

procedure TFMXMusicPlayerFrm.RepeatItemsClick(Sender: TObject);
var
  Item : TListBoxItem;
  I : Integer;
begin
  if Sender is TListBoxItem then
  begin
    for I := 0 to SettingsList.Items.Count - 1 do
      SettingsList.ItemByIndex(i).ItemData.Accessory := TListBoxItemData.TAccessory.aNone;

    Item := Sender as TListBoxItem;
    if Item.Text = 'All' then
      TMusicPlayer.DefaultPlayer.RepeatMode := TMPRepeatMode.All;
    if Item.Text = 'One' then
      TMusicPlayer.DefaultPlayer.RepeatMode := TMPRepeatMode.One;
    if Item.Text = 'None' then
      TMusicPlayer.DefaultPlayer.RepeatMode := TMPRepeatMode.None;
    if Item.Text = 'Default' then
      TMusicPlayer.DefaultPlayer.RepeatMode := TMPRepeatMode.Default;

    Item.ItemData.Accessory := TListBoxItemData.TAccessory.aCheckmark;
  end;
end;

procedure TFMXMusicPlayerFrm.SongChanged(newIndex: Integer);
var
  handler: TNotifyEvent;
begin
  handler := lvSongs.OnChange;
  lvSongs.OnChange := nil;
  lvSongs.ItemIndex := newIndex;
  UpdateNowPlaying(newIndex);
  lvSongs.OnChange := handler;
  StateChanged(TMPPlaybackState.Playing);
end;

procedure TFMXMusicPlayerFrm.StateChanged(state: TMPPlaybackState);
begin
  btnPlay.Enabled := not (state in [TMPPlaybackState.Playing]);
  btnPause.Enabled := not (state in [TMPPlaybackState.Paused, TMPPlayBackState.Stopped]);
  btnStop.Enabled := not (state in [TMPPlaybackState.Stopped]);
  tbProgress.Enabled := not (state in [TMPPlaybackState.Stopped, TMPPlaybackState.Paused]);
  btnNext.Enabled := TMusicPlayer.DefaultPlayer.CanSkipForward;
  btnPrev.Enabled := TMusicPlayer.DefaultPlayer.CanSkipBack;
end;

procedure TFMXMusicPlayerFrm.swShuffleModeSwitch(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.ShuffleMode := swShuffleMode.IsChecked;
end;

procedure TFMXMusicPlayerFrm.tbProgressChange(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Time := (tbProgress.Value * TMusicPlayer.DefaultPlayer.Duration)/100;
end;

procedure TFMXMusicPlayerFrm.UpdateNowPlaying(newIndex: Integer);
begin
  if newIndex >= 0 then
  begin
    lblArtistVal.Text := TMusicPlayer.DefaultPlayer.Playlist[newIndex].Artist;
    lblTitleVal.Text := TMusicPlayer.DefaultPlayer.Playlist[newIndex].Title;
    lblAlbumVal.Text := TMusicPlayer.DefaultPlayer.Playlist[newIndex].Album;
    lblDurationVal.Text := TMusicPlayer.DefaultPlayer.Playlist[newIndex].Duration;
  end;
end;

procedure TFMXMusicPlayerFrm.UpdateSongs;
var
  song: TMPSong;
  Item: TListViewItem;
begin
  lvSongs.BeginUpdate;
  lvSongs.Items.Clear;

  for song in TMusicPlayer.DefaultPlayer.Playlist do
  begin
    Item := lvSongs.Items.Add;
    if (song.Artist <> 'Unknown') then
      Item.Text := Format('%s - %s',[song.Artist, song.Title])
    else
      Item.Text := song.Title;
  end;
  lvSongs.EndUpdate;
end;

procedure TFMXMusicPlayerFrm.volTimerTimer(Sender: TObject);
var
  LEvent: TNotifyEvent;
begin
  LEvent := VolumeTrackBar.OnChange;
  VolumeTrackBar.OnChange := nil;
  VolumeTrackBar.Value := TMusicPlayer.DefaultPlayer.Volume;
  VolumeTrackBar.OnChange := LEvent;
end;

procedure TFMXMusicPlayerFrm.VolumeTrackBarChange(Sender: TObject);
begin
  TMusicPlayer.DefaultPlayer.Volume := VolumeTrackBar.Value;
end;

end.
