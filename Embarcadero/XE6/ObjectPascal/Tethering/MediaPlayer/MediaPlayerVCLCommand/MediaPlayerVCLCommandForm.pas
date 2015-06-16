unit MediaPlayerVCLCommandForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile,
  Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm9 = class(TForm)
    Label1: TLabel;
    LbPlayers: TListBox;
    Button1: TButton;
    Button2: TButton;
    VolumeTrack: TTrackBar;
    Label2: TLabel;
    VCLCommandManager: TTetheringManager;
    VCLCommandApp: TTetheringAppProfile;
    procedure VCLCommandManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
    procedure VCLCommandManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
    procedure FormCreate(Sender: TObject);
    procedure VCLCommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VolumeTrackChange(Sender: TObject);
  private
    { Private declarations }
    FInvariantFormatSettings: TFormatSettings;
    function CheckMediaPlayers: Boolean;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

function TForm9.CheckMediaPlayers: Boolean;
begin
  if LbPlayers.ItemIndex >= 0 then
    Result := True
  else
  begin
    Result := False;
    ShowMessage('Select a MediaPlayer from the list to connect, please');
  end;
end;

procedure TForm9.Button1Click(Sender: TObject);
begin
  if CheckMediaPlayers then
    VCLCommandApp.RunRemoteAction(VCLCommandManager.RemoteProfiles[LbPlayers.ItemIndex], 'acPlayPause');
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  LbPlayers.Clear;
  VCLCommandManager.DiscoverManagers;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  FInvariantFormatSettings := TFormatSettings.Create;
  FInvariantFormatSettings.DecimalSeparator := '.';
  FInvariantFormatSettings.ThousandSeparator := ',';
end;

procedure TForm9.VolumeTrackChange(Sender: TObject);
begin
  if CheckMediaPlayers then
    VCLCommandApp.SendString(VCLCommandManager.RemoteProfiles[LbPlayers.ItemIndex], 'VolumeTrack',
      (VolumeTrack.position / 100).ToString(FInvariantFormatSettings));
end;

procedure TForm9.VCLCommandManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  for I := 0 to RemoteManagers.Count - 1 do
    VCLCommandManager.PairManager(RemoteManagers[I]);
end;

procedure TForm9.VCLCommandManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
var
  I: Integer;
begin
  LbPlayers.Clear;
  for I := 0 to VCLCommandManager.RemoteProfiles.Count - 1 do
    if VCLCommandManager.RemoteProfiles[I].ProfileText = 'FMXMediaPlayer' then
      LbPlayers.Items.Add(VCLCommandManager.RemoteProfiles[I].ProfileText);

  if LbPlayers.Count > 0 then
  begin
    LbPlayers.ItemIndex := 0;
    VCLCommandApp.Connect(VCLCommandManager.RemoteProfiles[0]);  // Connect to the first one
  end;
end;

procedure TForm9.VCLCommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := '1234';
end;

end.
