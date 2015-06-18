unit InterBaseDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IBDef,
  FireDAC.Phys.IBBase, FireDAC.Phys.IB, Data.DB, FireDAC.Comp.Client,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.IBLiteDef;

type
  TdmInterBase = class(TDataModule)
    IBLiteDB: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    procedure IBLiteDBAfterConnect(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  strict private
    { Private declarations }
    FTables: TStringList;
    // Called automatically after connection
  public
    { Public declarations }
    // Fast Cached Table List
    /// <summary>
    /// Returns True if the TableName value exists in the local database.
    /// </summary>
    /// <remarks>
    /// Works from a local cache collected after connection. If you modify the database at runtime you should call CacheTableNames(True) to refresh the local cache before calling TableExists
    /// </remarks>
    function TableExists(TableName: string): Boolean;
    /// <summary>
    /// Fetches the tables in the current database to the local cache
    /// </summary>
    /// <remarks>
    /// Called automatically after connceting to the database. All table names are converted to Uppercase to support TableExists
    /// </remarks>
    procedure CacheTableNames(ForceReload : Boolean = False);
    property Tables : TStringList read FTables;
  end;

var
  dmInterBase: TdmInterBase;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TdmInterBase }

procedure TdmInterBase.DataModuleCreate(Sender: TObject);
begin
  FTables := TStringList.Create;
end;

procedure TdmInterBase.DataModuleDestroy(Sender: TObject);
begin
  FTables.Free;
end;

procedure TdmInterBase.IBLiteDBAfterConnect(Sender: TObject);
begin
  CacheTableNames(True);
end;

procedure TdmInterBase.CacheTableNames(ForceReload : Boolean);
begin
  // Clear Cache if needed
  if ForceReload then
    FTables.Clear;

  // If Not Cached, fetch table names
  if FTables.Count = 0 then begin
    IBLiteDB.GetTableNames('','','',FTables); // Get Tables
    FTables.Text := UpperCase(FTables.Text);
  end;
end;

function TdmInterBase.TableExists(TableName: string): Boolean;
begin
  Result := FTables.IndexOf(UpperCase(TableName)) > -1;
end;

end.
