//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ResourceModuleU;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON, EMS.Services, EMS.ResourceAPI,
  EMS.ResourceTypes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FireDAC.Stan.StorageJSON, FireDAC.Comp.DataSet, FireDAC.ConsoleUI.Wait,
  EMS.DataSetResource;

type
  [ResourceName('test')]  // Url segment.  For example, use http://localhost:8080/test
  TResourceModule = class(TDataModule)
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    qOrders: TFDQuery;
    qCustomers: TFDQuery;
    FDSchemaAdapter1: TFDSchemaAdapter;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDConnection1: TFDConnection;

    [EndPointRequestSummary('List', 'FireDACResource', 'List Of Records', 'Process GetRecords and return available records', 'application/vnd.embarcadero.firedac+json', '')]
    [EndPointRequestSummary('Post', 'FireDACResource', 'Post Updates', 'Process PostUpdates and save changed records', '', 'application/vnd.embarcadero.firedac+json')]
    [EndPointRequestSummary('Get', 'FireDACResource', 'Not Implemented', 'Endpoint is not enabled.', '', '')]
    [EndPointRequestSummary('Put', 'FireDACResource', 'Not Implemented', 'Endpoint is not enabled.', '', '')]
    [EndPointRequestSummary('Delete', 'FireDACResource', 'Not Implemented', 'Endpoint is not enabled.', '', '')]
    [EndPointResponseDetails(200, 'Ok', TAPIDoc.TPrimitiveType.spObject, TAPIDoc.TPrimitiveFormat.None, '', '')]
    [ResourceSuffix('/')]
    [EndpointName('GetRecords', 'List')] // Name to show in analytics
    [EndpointName('PostUpdates', 'Post')] // Name to show in  analytics
    EMSDataSetResource1: TEMSDataSetResource;
  end;

procedure Register;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure Register;
begin
  RegisterResource(TypeInfo(TResourceModule));
end;

end.


