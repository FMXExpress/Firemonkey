unit ServerMethodsUnit1;

interface

uses
  System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, DataSnap.DSProviderDataModuleAdapter,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageJSON,
  FireDAC.Stan.StorageBin, FireDAC.Comp.UI, FireDAC.Phys.IBBase,

  Data.FireDACJSONReflect;

type
  TServerMethods1 = class(TDSServerModule)
    FDConnection1: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDQueryDepartmentNames: TFDQuery;
    FDQueryDepartment: TFDQuery;
    FDQueryDepartmentEmployees: TFDQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;

    function  GetDepartmentNames: TFDJSONDataSets;
    function  GetDepartmentEmployees(const AID: string): TFDJSONDataSets;
    procedure ApplyChangtesDepartmentEmployess(const ADeltaList: TFDJSONDeltas);
  end;

implementation


{$R *.dfm}


uses
  System.StrUtils;

const
  sDepartment = 'Department';
  sEmployees  = 'Employees';

procedure TServerMethods1.ApplyChangtesDepartmentEmployess(
  const ADeltaList: TFDJSONDeltas);
var
  LApply : IFDJSONDeltasApplyUpdates;
begin
  //ApplyUpdates
  LApply := TFDJSONDeltasApplyUpdates.Create(ADeltaList);
  LApply.ApplyUpdates(sDepartment, FDQueryDepartment.Command);

  if LApply.Errors.Count = 0 then
    LApply.ApplyUpdates(sEmployees, FDQueryDepartmentEmployees.Command);

  if LApply.Errors.Count > 0 then
    raise Exception.Create(LApply.Errors.Strings.Text);
end;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.GetDepartmentEmployees(
  const AID: string): TFDJSONDataSets;
begin
  FDQueryDepartmentEmployees.Active := False;
  FDQueryDepartment.Active          := False;

  FDQueryDepartmentEmployees.Params[0].Value := AID;
  FDQueryDepartment.Params[0].Value          := AID;

  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, sDepartment, FDQueryDepartment);
  TFDJSONDataSetsWriter.ListAdd(Result, sEmployees, FDQueryDepartmentEmployees);
end;

function TServerMethods1.GetDepartmentNames: TFDJSONDataSets;
begin
  FDQueryDepartmentNames.Active := False;

  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, FDQueryDepartmentNames);
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.

