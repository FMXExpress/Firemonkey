// 
// Created by the DataSnap proxy generator.
// 03/03/2015 14:31:44
// 

unit Proxy;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.FireDACJSONReflect, Data.DBXJSONReflect;

type

  IDSRestCachedTFDJSONDataSets = interface;

  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
    FGetDepartmentNamesCommand: TDSRestCommand;
    FGetDepartmentNamesCommand_Cache: TDSRestCommand;
    FGetDepartmentEmployeesCommand: TDSRestCommand;
    FGetDepartmentEmployeesCommand_Cache: TDSRestCommand;
    FApplyChangtesDepartmentEmployessCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
    function GetDepartmentNames(const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetDepartmentNames_Cache(const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    function GetDepartmentEmployees(AID: string; const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetDepartmentEmployees_Cache(AID: string; const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    procedure ApplyChangtesDepartmentEmployess(ADeltaList: TFDJSONDeltas);
  end;

  IDSRestCachedTFDJSONDataSets = interface(IDSRestCachedObject<TFDJSONDataSets>)
  end;

  TDSRestCachedTFDJSONDataSets = class(TDSRestCachedObject<TFDJSONDataSets>, IDSRestCachedTFDJSONDataSets, IDSRestCachedCommand)
  end;

const
  TServerMethods1_EchoString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ReverseString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_GetDepartmentNames: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetDepartmentNames_Cache: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetDepartmentEmployees: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetDepartmentEmployees_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_ApplyChangtesDepartmentEmployess: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'ADeltaList'; Direction: 1; DBXType: 37; TypeName: 'TFDJSONDeltas')
  );

implementation

function TServerMethods1Client.EchoString(Value: string; const ARequestFilter: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FConnection.CreateCommand;
    FEchoStringCommand.RequestType := 'GET';
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare(TServerMethods1_EchoString);
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.Execute(ARequestFilter);
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string; const ARequestFilter: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FConnection.CreateCommand;
    FReverseStringCommand.RequestType := 'GET';
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare(TServerMethods1_ReverseString);
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.Execute(ARequestFilter);
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.GetDepartmentNames(const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetDepartmentNamesCommand = nil then
  begin
    FGetDepartmentNamesCommand := FConnection.CreateCommand;
    FGetDepartmentNamesCommand.RequestType := 'GET';
    FGetDepartmentNamesCommand.Text := 'TServerMethods1.GetDepartmentNames';
    FGetDepartmentNamesCommand.Prepare(TServerMethods1_GetDepartmentNames);
  end;
  FGetDepartmentNamesCommand.Execute(ARequestFilter);
  if not FGetDepartmentNamesCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetDepartmentNamesCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetDepartmentNamesCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetDepartmentNamesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetDepartmentNames_Cache(const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetDepartmentNamesCommand_Cache = nil then
  begin
    FGetDepartmentNamesCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentNamesCommand_Cache.RequestType := 'GET';
    FGetDepartmentNamesCommand_Cache.Text := 'TServerMethods1.GetDepartmentNames';
    FGetDepartmentNamesCommand_Cache.Prepare(TServerMethods1_GetDepartmentNames_Cache);
  end;
  FGetDepartmentNamesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetDepartmentNamesCommand_Cache.Parameters[0].Value.GetString);
end;

function TServerMethods1Client.GetDepartmentEmployees(AID: string; const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetDepartmentEmployeesCommand = nil then
  begin
    FGetDepartmentEmployeesCommand := FConnection.CreateCommand;
    FGetDepartmentEmployeesCommand.RequestType := 'GET';
    FGetDepartmentEmployeesCommand.Text := 'TServerMethods1.GetDepartmentEmployees';
    FGetDepartmentEmployeesCommand.Prepare(TServerMethods1_GetDepartmentEmployees);
  end;
  FGetDepartmentEmployeesCommand.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesCommand.Execute(ARequestFilter);
  if not FGetDepartmentEmployeesCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetDepartmentEmployeesCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetDepartmentEmployeesCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetDepartmentEmployeesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetDepartmentEmployees_Cache(AID: string; const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetDepartmentEmployeesCommand_Cache = nil then
  begin
    FGetDepartmentEmployeesCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentEmployeesCommand_Cache.RequestType := 'GET';
    FGetDepartmentEmployeesCommand_Cache.Text := 'TServerMethods1.GetDepartmentEmployees';
    FGetDepartmentEmployeesCommand_Cache.Prepare(TServerMethods1_GetDepartmentEmployees_Cache);
  end;
  FGetDepartmentEmployeesCommand_Cache.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetDepartmentEmployeesCommand_Cache.Parameters[1].Value.GetString);
end;

procedure TServerMethods1Client.ApplyChangtesDepartmentEmployess(ADeltaList: TFDJSONDeltas);
begin
  if FApplyChangtesDepartmentEmployessCommand = nil then
  begin
    FApplyChangtesDepartmentEmployessCommand := FConnection.CreateCommand;
    FApplyChangtesDepartmentEmployessCommand.RequestType := 'POST';
    FApplyChangtesDepartmentEmployessCommand.Text := 'TServerMethods1."ApplyChangtesDepartmentEmployess"';
    FApplyChangtesDepartmentEmployessCommand.Prepare(TServerMethods1_ApplyChangtesDepartmentEmployess);
  end;
  if not Assigned(ADeltaList) then
    FApplyChangtesDepartmentEmployessCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FApplyChangtesDepartmentEmployessCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FApplyChangtesDepartmentEmployessCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(ADeltaList), True);
      if FInstanceOwner then
        ADeltaList.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FApplyChangtesDepartmentEmployessCommand.Execute;
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TServerMethods1Client.Destroy;
begin
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  FGetDepartmentNamesCommand.DisposeOf;
  FGetDepartmentNamesCommand_Cache.DisposeOf;
  FGetDepartmentEmployeesCommand.DisposeOf;
  FGetDepartmentEmployeesCommand_Cache.DisposeOf;
  FApplyChangtesDepartmentEmployessCommand.DisposeOf;
  inherited;
end;

end.
