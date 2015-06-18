unit uReceiveData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, System.SyncObjs;

type
  TfrmHRMDataReceiver = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    FooterLabel: TLabel;
    Image1: TImage;
    SpeedButton1: TSpeedButton;
    Memo1: TMemo;
    TetherManagerHRM: TTetheringManager;
    TetherProfileHRM: TTetheringAppProfile;
    Label1: TLabel;
    Memo2: TMemo;
    Label2: TLabel;
    tmCheckConnection: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TetherManagerHRMRequestManagerPassword(const Sender: TObject;
      const ARemoteIdentifier: string; var Password: string);
    procedure TetherManagerHRMEndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure TetherManagerHRMEndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure TetherProfileHRMResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure TetherManagerHRMPairedToRemote(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherManagerHRMPairedFromLocal(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherManagerHRMRemoteManagerShutdown(const Sender: TObject;
      const AManagerIdentifier: string);
    procedure TetherManagerHRMNewManager(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherManagerHRMUnPairManager(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetherProfileHRMResourceUpdated(const Sender: TObject;
      const AResource: TRemoteResource);

      procedure RefreshBPMList;
    procedure TetherProfileHRMDisconnect(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);

  private
    { Private declarations }
    //App Tethering Shared Resource
    FMyBPM: String;

    FLock: TCriticalSection;
    FRemoteProfiles: TTetheringProfileInfoList;
    procedure Lock;
    procedure Unlock;

  public
    { Public declarations }
  end;


var
 frmHRMDataReceiver: TfrmHRMDataReceiver;

implementation

{$R *.fmx}

procedure TfrmHRMDataReceiver.Lock;
begin
  FLock.Acquire;
end;

procedure TfrmHRMDataReceiver.Unlock;
begin
  FLock.Release;
end;


procedure TfrmHRMDataReceiver.FormCreate(Sender: TObject);
begin
  // Assign a default value to the "Username" Resource
  TetherProfileHRM.Resources.FindByName('MyBPM').Value := 'Waiting for BPM...';

  // Create the Remote Profile Lock
  FLock := TCriticalSection.Create;

// Create the Remote Profiles List
  FRemoteProfiles := TTetheringProfileInfoList.Create;

 // Log the unique LOCAL identifier so we can distinguish between instances
  // This is a unique GUID generated when our application is executed
  Memo1.Lines.Add('Local Identifier: ' + TetherManagerHRM.Identifier);
end;

procedure TfrmHRMDataReceiver.SpeedButton1Click(Sender: TObject);
begin
  FooterLabel.Text := 'Looking for Heart Rate Monitor...';
    // AT: Log the unique GUID HR Monitor identifier when app starts
  Memo1.Lines.Add('GUID Identifier: ' + TetherManagerHRM.Identifier);

// The Local Manager scans for Remote Managers.
  Memo1.Lines.Add('Scanning for Remote Managers to pair with...');
  TetherManagerHRM.DiscoverManagers(2000);
end;

procedure TfrmHRMDataReceiver.TetherManagerHRMEndManagersDiscovery(
  const Sender: TObject; const ARemoteManagers: TTetheringManagerInfoList);
    var
  I: Integer;
begin
// Output the number of Remote Managers into the Memo.
// Handle pairing with Remote Managers
Memo1.Lines.Add(Format('Manager Discovery Complete, found %d Remote Managers',
                         [ARemoteManagers.Count]));
// Iterate through all of the discovered Remote Managers,
// Check if its Text properties matches that of the Local Manager.
// If it matches, the Local Manager pairs with it; if not,
// we acknowledge its existence in the log and move on.
  for I := 0 to ARemoteManagers.Count - 1 do
  begin
    // Log information about the Remote Manager
    Memo1.Lines.Add(Format('Discovered Remote Manager %d - %s' + #13#10 +
                           #9 + 'Manager Name: %s' + #13#10 +
                           #9 + 'Manager Text: %s' + #13#10 +
                           #9 + 'Connection String: %s',
                           [I,
                            ARemoteManagers[I].ManagerIdentifier,
                            ARemoteManagers[I].ManagerName,
                            ARemoteManagers[I].ManagerText,
                            ARemoteManagers[I].ConnectionString]));

    // Check if the Remote Manager's "Text" matches the Local Manager's
    if (ARemoteManagers[I].ManagerText = TetherManagerHRM.Text) then
    begin
      // Log that we're attempting to pair with this Remote Manager
      Memo1.Lines.Add('Remote Manager matches Local Manger, attempting to pair...');
      FooterLabel.Text := 'Connected to HRM:  '+ TetherManagerHRM.Text;
      // Pair with the Remote Manager
      TetherManagerHRM.PairManager(ARemoteManagers[I]);
    end else
    begin
      // Log that this Remote Manager is of no interest
      Memo1.Lines.Add('Remote Manager does not match Local Manager, ignoring it...');
    end;
  end;
 // Once the Local Manager pairs with a Remote Manager,
 // the Profile Discovery process automatically takes place on both sides.

end;
procedure TfrmHRMDataReceiver.TetherManagerHRMEndProfilesDiscovery(
  const Sender: TObject; const ARemoteProfiles: TTetheringProfileInfoList);
  var
  LRemoteProfile: TTetheringProfileInfo;
begin
  Memo1.Lines.Add(Format('Profile Discovery Complete, Found %d Remote Profiles',
                         [ARemoteProfiles.Count]));
  // Lock the container
  Lock;
  try
    // Iterate all discovered profiles
    for LRemoteProfile in ARemoteProfiles do
    begin
      // If the profile isn't already in the list...
      if (not FRemoteProfiles.Contains(LRemoteProfile)) then
      begin
        // If we can connect to the Remote Profile
        if TetherProfileHRM.Connect(LRemoteProfile) then
        begin
        //We need to Subscribe to each Remote Profile‘s BPM resource.
        //This will enable the OnResourceUpdated event to update the BPM value.
        //Each time a Remote Profile changes the value of its BPM resource,
        //our Local Profile‘s OnResourceUpdated event is automatically fired,
        //and that refreshes the BPM value.
        TetherProfileHRM.SubscribeToRemoteItem(LRemoteProfile,
        TetherProfileHRM.GetRemoteResourceValue(LRemoteProfile, 'MyBPM'));
          // Add it into our lockable container
          FRemoteProfiles.Add(LRemoteProfile);
          // Log the Remote Profile
          Memo1.Lines.Add(Format('Added Remote Profile %s to the Lockable List',
                                  [LRemoteProfile.ProfileIdentifier]));
        end;
      end;
    end;
    // Re-sort the lockable container
    FRemoteProfiles.Sort;
  finally
    // Unlock the container
    Unlock;
  end;
end;

procedure TfrmHRMDataReceiver.TetherManagerHRMNewManager(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
begin
      // Log new Remote Manager discovered.
  Memo1.Lines.Add(Format('New Remote Manager %s' + #13#10 +
                         #9 + 'Manager Name: %s' + #13#10 +
                         #9 + 'Manager Text: %s' + #13#10 +
                         #9 + 'Connection String: %s',
                         [AManagerInfo.ManagerIdentifier,
                          AManagerInfo.ManagerName,
                          AManagerInfo.ManagerText,
                          AManagerInfo.ConnectionString]));

end;

procedure TfrmHRMDataReceiver.TetherManagerHRMPairedFromLocal(
  const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
  // Log we've paired to a Remote Manager
  Memo1.Lines.Add(Format('A Remote Manager %s has paired with us' + #13#10 +
                         #9 + 'Manager Name: %s' + #13#10 +
                         #9 + 'Manager Text: %s' + #13#10 +
                         #9 + 'Connection String: %s',
                         [AManagerInfo.ManagerIdentifier,
                          AManagerInfo.ManagerName,
                          AManagerInfo.ManagerText,
                          AManagerInfo.ConnectionString]));


end;

procedure TfrmHRMDataReceiver.TetherManagerHRMPairedToRemote(
  const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
     // Log pairing with a Remote Manager initiated from our Local Manager.
  Memo1.Lines.Add(Format('We have paired with a Remote Manager %s' + #13#10 +
                         #9 + 'Manager Name: %s' + #13#10 +
                         #9 + 'Manager Text: %s' + #13#10 +
                         #9 + 'Connection String: %s',
                         [AManagerInfo.ManagerIdentifier,
                          AManagerInfo.ManagerName,
                          AManagerInfo.ManagerText,
                          AManagerInfo.ConnectionString]));


end;

procedure TfrmHRMDataReceiver.TetherManagerHRMRemoteManagerShutdown(
  const Sender: TObject; const AManagerIdentifier: string);
begin
//  Logs the identity of a Remote Manager that has shut down.
    Memo1.Lines.Add(Format('Remote Manager %s Shutdown', [AManagerIdentifier]));
end;


procedure TfrmHRMDataReceiver.TetherManagerHRMRequestManagerPassword(
  const Sender: TObject; const ARemoteIdentifier: string; var Password: string);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure begin
      Memo1.Lines.Add(Format('Remote Manager %s needs the password...',
                             [ARemoteIdentifier]));
    end);
  Password := '1234';
end;


procedure TfrmHRMDataReceiver.TetherManagerHRMUnPairManager(
  const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
       // Log we have unpaired from a Remote Manager.
  Memo1.Lines.Add(Format('UnPaired with Remote Manager %s' + #13#10 +
                         #9 + 'Manager Name: %s' + #13#10 +
                         #9 + 'Manager Text: %s' + #13#10 +
                         #9 + 'Connection String: %s',
                         [AManagerInfo.ManagerIdentifier,
                          AManagerInfo.ManagerName,
                          AManagerInfo.ManagerText,
                          AManagerInfo.ConnectionString]));

end;

procedure TfrmHRMDataReceiver.TetherProfileHRMDisconnect(const Sender: TObject;
  const AProfileInfo: TTetheringProfileInfo);
var
  LIndex: Integer;
  LProfileInfo: TTetheringProFileInfo;
begin
  // Engage the Lock
  Lock;
  try
    // If the Profile is in the list...
    if FRemoteProfiles.BinarySearch(AProfileInfo, LIndex) then
    begin
      // ...remove it
      FRemoteProfiles.Delete(LIndex);
      // Log that we've removed it
      LProfileInfo := AProfileInfo;
      TThread.Synchronize(TThread.CurrentThread,
        procedure begin
          Memo2.Lines.Add(Format('Removed Remote Profile %s from the lockable container',
                                  [LProfileInfo.ProfileIdentifier]));
        end);
    end;
  finally
    // Disengage the Lock
    Unlock;
  end;
end;

procedure TfrmHRMDataReceiver.TetherProfileHRMResourceReceived(
  const Sender: TObject; const AResource: TRemoteResource);
begin
Memo1.Lines.Add(Format('Temporary Resource Received: %s', [AResource.Hint]));
// if it's a String resource...
  if (AResource.Hint = 'BPM') and
     (AResource.Value.DataType = TResourceType.String) then
  begin
    // Add the message to the display
    Memo2.Lines.Add(AResource.Value.AsString);
  end;
end;

procedure TfrmHRMDataReceiver.TetherProfileHRMResourceUpdated(
  const Sender: TObject; const AResource: TRemoteResource);
  begin
  // Log that a Remote Resource has been updated
  Memo1.Lines.Add(Format('Remote Resource Updated: %s', [AResource.Name]));
  // if this is the MyBPM resource...
  if AResource.Name = 'MyBPM' then
    RefreshBPMList; // ...Refresh the BPM list
end;


procedure TfrmHRMDataReceiver.RefreshBPMList;
var
  LRemoteProfile: TTetheringProfileInfo;
  LRemoteBPM: String;
begin
  // Lock the container
  Lock;
  try
    // Iterate all Connected Remote Profiles
    for LRemoteProfile in FRemoteProfiles do
    begin
      // Retreive the BPM
      LRemoteBPM := TetherProfileHRM.GetRemoteResourceValue(
                           LRemoteProfile, 'MyBPM').Value.AsString;
      // If the BPM has been specified
      if LRemoteBPM <> 'Waiting for BPM...' then
      begin
        // Add the BPM to the list
       // lbBPM.Items.Add(LRemoteBPM);
      end;
    end;
    // Unlock the container
  finally
    Unlock;
  end;
end;


end.


