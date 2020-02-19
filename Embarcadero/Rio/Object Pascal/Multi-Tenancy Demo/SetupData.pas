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
unit SetupData;

interface

uses
  REST.Backend.EMSApi, System.JSON, IPPeerClient, System.DateUtils, System.SysUtils;


procedure SetupTestData(const AHost, APort: string);

implementation

const
  _TENANT_ID_1 = 'AB715312-06FF-41AB-BBB5-07FF6F101B49';
  _TENANT_SECRET_1 = '1';
  _TENANT_ID_2 = '37B3DBCB-A27E-4F89-8947-3AE24317B924';
  _TENANT_SECRET_2 = '2';
  sTestUser1 = 'John';
  sTestUser2 = 'Sam';
  sTestUser3 = 'Michael';
  sTestUser4 = 'Tina';
  sPassword = '1';
  sTestGroup1 = 'Managers';
  sTestGroup2 = 'Cashiers';

var
  FClientAPI: TEMSClientApi;
  FBaseURL: string;

procedure SetupUsersAndGroups(AFirstTenant: Boolean);
var
  LUser1, LUser2: TEMSClientAPI.TUser;
  LGroup1, LGroup2: TEMSClientAPI.TGroup;
  LUpdatedAt: TEMSClientAPI.TUpdatedAt;
begin
  if AFirstTenant then
  begin
    FClientAPI.AddUser(sTestUser1, sPassword, nil, LUser1);
    FClientAPI.AddUser(sTestUser2, sPassword, nil, LUser2);
  end
  else
  begin
    FClientAPI.AddUser(sTestUser3, sPassword, nil, LUser1);
    FClientAPI.AddUser(sTestUser4, sPassword, nil, LUser2);
  end;
  FClientAPI.CreateGroup(sTestGroup1, nil, LGroup1);
  FClientAPI.CreateGroup(sTestGroup2, nil, LGroup2);
  FClientAPI.AddUsersToGroup(sTestGroup1, [LUser1.UserID], LUpdatedAt);
  FClientAPI.AddUsersToGroup(sTestGroup2, [LUser2.UserID], LUpdatedAt);
end;

procedure CheckConnection;
var
  LJSONArray: TJSONArray;
  LServerFound: Boolean;
  EndTime: TDateTime;
begin
  LServerFound := False;
  LJSONArray := TJSONArray.Create;
  try
    EndTime := IncSecond(Now, 20);
    while not LServerFound and (Now < EndTime) do
    begin
      try
        FClientAPI.AppHandshake(LJSONArray);
        LServerFound := True;
      except
      end;
    end;
  finally
    LJSONArray.Free;
  end;
end;

procedure SetupByTenant(const ATenantId, ATenantSecret: string);
var
  LConnectionInfo: TEMSClientAPI.TConnectionInfo;
  LUser, LUser2: TEMSClientAPI.TUser;
  LFirstTenant: Boolean;
begin
  FClientAPI := TEMSClientAPI.Create(nil);
  try
    LConnectionInfo.TenantId := ATenantId;
    LConnectionInfo.TenantSecret := ATenantSecret;
    LConnectionInfo.BaseURL := FBaseURL;
    FClientAPI.ConnectionInfo := LConnectionInfo;

    CheckConnection;

    LFirstTenant := ATenantId = _TENANT_ID_1;
    if LFirstTenant then
      FClientAPI.QueryUserName(sTestUser1, LUser)
    else
      FClientAPI.QueryUserName(sTestUser3, LUser);

    if LUser.UserID.IsEmpty then
      SetupUsersAndGroups(LFirstTenant);
  finally
    FreeAndNil(FClientAPI);
  end;
end;

procedure SetupTestData(const AHost, APort: string);
begin
  FBaseURL := Format('http://%s:%s/', [AHost, APort]);
  SetupByTenant(_TENANT_ID_1, _TENANT_SECRET_1);
  SetupByTenant(_TENANT_ID_2, _TENANT_SECRET_2);
end;

end.
