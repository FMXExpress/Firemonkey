//---------------------------------------------------------------------------
// Copyright (c) 2017 Embarcadero Technologies, Inc. All rights reserved.  
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
unit ClientDataModule;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.Stan.StorageBin,
  REST.Backend.EMSServices, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  REST.Backend.EMSFireDAC, REST.Backend.EMSProvider, FireDAC.UI.Intf,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.StorageJSON,
  REST.Backend.ServiceTypes, System.JSON, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint,
  REST.Backend.MetaTypes, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, REST.Backend.Providers, REST.Backend.EMSApi,
  SetupData, System.Threading;

type
  TClientDM = class(TDataModule)
    EMSProvider: TEMSProvider;
    EMSFireDACClientItems: TEMSFireDACClient;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    BackendEndpointSettings: TBackendEndpoint;
    RESTResponse: TRESTResponse;
    memStores: TFDMemTable;
    FDTableAdapter: TFDTableAdapter;
    FDSchemaAdapter: TFDSchemaAdapter;
    memItems: TFDMemTable;
    BackendAuth: TBackendAuth;
    BackendUsers: TBackendUsers;
    memCurrentUser: TFDMemTable;
    memCurrentUserUserName: TStringField;
    memCurrentUserUserId: TStringField;
    memCurrentUserGroupName: TStringField;
    memCurrentUserHasWriteAccess: TBooleanField;
    memCurrentUserDisplayUser: TStringField;
    procedure memCurrentUserDisplayUserGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FSettionToken: string;
    FMaxId: Integer;
    FSetupDataTask: ITask;
    procedure OnGetCurrentUser(const AUser: TEMSClientAPI.TUser; const AObj: TJSONObject);
    function GetLoggedIn: Boolean;
  public
    { Public declarations }
    procedure LoadItems;
    procedure LoginEmployee;
    procedure SetTenant(const ATenantSecret: string);
    procedure SetUserCredentials(const AUserName, APassword: string);
    procedure Initialize(const AHost, APort: string);
    procedure Save;
    procedure AddItem;
    procedure DeleteCurrent;
    procedure Cancel;
    procedure SetHost(const AHost: string; APort: Integer);
    property LoggedIn: Boolean read GetLoggedIn;
  end;

var
  ClientDM: TClientDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Math;

{ TDataModule2 }

procedure TClientDM.AddItem;
begin
  memItems.Append;
  Inc(FMaxId);
  memItems.FieldByName('ITEMID').AsInteger := FMaxId;
  memItems.FieldByName('TENANTID').AsString := memStores.FieldByName('TENANTID').AsString;
end;

procedure TClientDM.Cancel;
begin
  memItems.CancelUpdates;
end;

procedure TClientDM.DataModuleDestroy(Sender: TObject);
begin
  if FSetupDataTask.Status = TTaskStatus.Running then
    FSetupDataTask.Cancel;
end;

procedure TClientDM.DeleteCurrent;
begin
  memItems.Delete;
end;

function TClientDM.GetLoggedIn: Boolean;
begin
  Result := BackendAuth.LoggedIn;
end;

procedure TClientDM.Initialize(const AHost, APort: string);
begin
  SetHost(AHost, StrToIntDef(APort, 8080));

  BackendEndpointSettings.ExecuteAsync(
    procedure
    var
      LStream: TMemoryStream;
      Writer: TBinaryWriter;
    begin
      LStream := TMemoryStream.Create;
      try
        Writer := TBinaryWriter.Create(LStream);
        try
          Writer.Write(RESTResponse.RawBytes);
          LStream.Position := 0;
          memStores.LoadFromStream(LStream, TFDStorageFormat.sfJSON);
        finally
          Writer.Free;
        end;
      finally
        LStream.Free;
      end;
    end
    );

  if Assigned(FSetupDataTask) and (FSetupDataTask.Status = TTaskStatus.Running) then
    FSetupDataTask.Cancel;

  if not Assigned(FSetupDataTask) or (FSetupDataTask.Status in [TTaskStatus.Canceled, TTaskStatus.Exception]) then
  begin
    FSetupDataTask := TTask.Create(procedure begin SetupTestData(AHost, APort); end);
    FSetupDataTask.Start;
  end;
  FMaxId := RandomRange(100, 1000);
end;

procedure TClientDM.LoadItems;
begin
  if memItems.Active then
  begin
    memItems.CancelUpdates;
    memItems.Active := False;
  end;
  EMSFireDACClientItems.GetData;  
end;

procedure TClientDM.LoginEmployee;
begin
  FSettionToken := '';
  BackendAuth.Login;
  if BackendAuth.LoggedIn then
    FSettionToken := BackendAuth.LoggedInToken;

  if FSettionToken = '' then
    BackendAuth.Authentication := TBackendAuthentication.Default
  else
    BackendAuth.Authentication := TBackendAuthentication.Session;

  (BackendUsers.ProviderService as IGetEMSApi).EMSAPI.RetrieveCurrentUser(OnGetCurrentUser);  
end;

procedure TClientDM.memCurrentUserDisplayUserGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin  
  if memStores.Active then
    Text := 
      'Store: ' + memStores.FieldByName('TENANTNAME').AsString + sLineBreak +
      'User: ' + memCurrentUserUserName.AsString + sLineBreak +
      'Group: ' + memCurrentUserGroupName.AsString;
end;

procedure TClientDM.Save;
begin
  if EMSFireDACClientItems.CanPostUpdates then
    EMSFireDACClientItems.PostUpdates;
end;

procedure TClientDM.SetHost(const AHost: string; APort: Integer);
begin
  EMSProvider.URLHost := AHost;
  EMSProvider.URLPort := APort;
end;

procedure TClientDM.SetTenant(const ATenantSecret: string);
begin
  EMSProvider.TenantSecret := ATenantSecret;
  EMSProvider.TenantId := memStores.FieldByName('TenantId').AsString;
end;

procedure TClientDM.SetUserCredentials(const AUserName, APassword: string);
begin
  BackendAuth.UserName := AUserName;
  BackendAuth.Password := APassword;
  BackendAuth.Authentication := TBackendAuthentication.Default;

  EMSProvider.UserName := AUserName;
  EMSProvider.Password := APassword;
end;

procedure TClientDM.OnGetCurrentUser(const AUser: TEMSClientAPI.TUser; const AObj: TJSONObject);
var
  LGroups: TArray<string>;
begin
  if AUser.UserID.IsEmpty then
    raise Exception.Create('Failed to identify user.');
    
  memCurrentUser.Edit;  
  
  memCurrentUserUserName.AsString := AUser.UserName;
  memCurrentUserUserId.AsString := AUser.UserID;

  //Getting groups
  LGroups := (BackendUsers.ProviderService as IGetEMSApi).EMSAPI.RetrieveUserGroups(AUser.UserID);
  memCurrentUserGroupName.Clear;
  memCurrentUserHasWriteAccess.AsBoolean := False;
  if Length(LGroups) > 0 then
  begin
    memCurrentUserGroupName.AsString := LGroups[0];
    memCurrentUserHasWriteAccess.AsBoolean := LGroups[0] = 'Managers';
  end;
  
  memCurrentUser.Post;
end;

end.
