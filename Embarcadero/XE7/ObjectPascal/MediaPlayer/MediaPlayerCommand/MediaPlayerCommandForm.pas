
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MediaPlayerCommandForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, System.Generics.Collections, FMX.ListBox, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, FMX.Memo;

type
  TForm3 = class(TForm)
    CommandManager: TTetheringManager;
    CommandApp: TTetheringAppProfile;
    Button1: TButton;
    Button2: TButton;
    lbPlayers: TListBox;
    VolumeTrack: TTrackBar;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure lbPlayersItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure VolumeTrackChange(Sender: TObject);
    procedure CommandManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
    procedure CommandManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
    procedure CommandManagerRemoteManagerShutdown(const Sender: TObject; const ManagerIdentifier: string);
  private
    { Private declarations }
    FInvariantFormatSettings: TFormatSettings;
    function CheckMediaPlayers: Boolean;
    procedure RefreshList;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm3.Button1Click(Sender: TObject);
begin
  if CheckMediaPlayers then
    CommandApp.RunRemoteAction(CommandManager.RemoteProfiles[LbPlayers.ItemIndex], 'acPlayPause');
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  for I := CommandManager.PairedManagers.Count - 1 downto 0 do
    CommandManager.UnPairManager(CommandManager.PairedManagers[I]);
  LbPlayers.Clear;
  CommandManager.DiscoverManagers;
end;

function TForm3.CheckMediaPlayers: Boolean;
begin
  if LbPlayers.ItemIndex >= 0 then
    Result := True
  else
  begin
    Result := False;
    ShowMessage('Select a MediaPlayer from the list to connect, please');
  end;
end;

procedure TForm3.RefreshList;
var
  I: Integer;
begin
  lbPlayers.Clear;
  for I := 0 to CommandManager.RemoteProfiles.Count - 1 do
    if CommandManager.RemoteProfiles[I].ProfileText = 'FMXMediaPlayer' then
      LbPlayers.Items.Add(CommandManager.RemoteProfiles[I].ProfileText);
  if LbPlayers.Count > 0 then
  begin
    LbPlayers.ItemIndex := 0;
    CommandApp.Connect(CommandManager.RemoteProfiles[0]);
  end;
  // Connect to the first one
end;

procedure TForm3.CommandManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  for I := 0 to RemoteManagers.Count - 1 do
    if RemoteManagers[I].ManagerText = 'FMXManager' then
      CommandManager.PairManager(RemoteManagers[I]);
end;

procedure TForm3.CommandManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
begin
  RefreshList;
end;

procedure TForm3.CommandManagerRemoteManagerShutdown(const Sender: TObject; const ManagerIdentifier: string);
begin
  RefreshList;
end;

procedure TForm3.CommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := '1234';
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FInvariantFormatSettings := TFormatSettings.Create;
  FInvariantFormatSettings.DecimalSeparator := '.';
  FInvariantFormatSettings.ThousandSeparator := ',';
end;

procedure TForm3.lbPlayersItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  CommandApp.Connect(CommandManager.RemoteProfiles[Item.Index]);
end;

procedure TForm3.VolumeTrackChange(Sender: TObject);
begin
  if CheckMediaPlayers then
    CommandApp.SendString(CommandManager.RemoteProfiles[LbPlayers.ItemIndex], 'VolumeTrack',
      VolumeTrack.Value.ToString(FInvariantFormatSettings));
end;

end.
