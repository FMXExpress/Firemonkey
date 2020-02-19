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
unit CommonDataModule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.ConsoleUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.IBBase, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.Comp.Script;

type
  TCommonDM = class(TDataModule)
    IBConnection: TFDConnection;
    FDPhysIBDriverLink: TFDPhysIBDriverLink;
    QuerySetupDatabase: TFDQuery;
    FDTransaction: TFDTransaction;
    FDScript: TFDScript;
  private
    { Private declarations }
    procedure SetupDataBase;
    function IsTablesExist: Boolean;
    function NeedRecreate: Boolean;
    procedure RunScript(const AScriptText: string; AWithTransaction: Boolean = True);
    procedure DropExisting;
  public
    { Public declarations }
    procedure CheckConnected;
    procedure CheckDatabase;
  end;

function CommonDM: TCommonDM;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  SQLScripts;

const
  RECREATE_NEEDED = False;

var
  _CommonDM: TCommonDM;

function CommonDM: TCommonDM;
begin
  Result := _CommonDM;
end;

{ TCommonDM }

procedure TCommonDM.CheckConnected;
begin
  IBConnection.Connected := True;
end;

procedure TCommonDM.CheckDatabase;
begin
  if NeedRecreate then
  begin
    if IsTablesExist then
      DropExisting;
    SetupDataBase;
  end;
end;

procedure TCommonDM.DropExisting;
begin
  RunScript(SCRIPT_CLEAR_DATA);
end;

procedure TCommonDM.SetupDataBase;
begin
  RunScript(SCRIPT_CREATE_TABLE, False);
  RunScript(SCRIPT_INSERT_TENANTS);
  RunScript(SCRIPT_INSERT_ITEMS);
end;

function TCommonDM.IsTablesExist: Boolean;
const
  SQL_CHECK_TABLE_EXISTS = 'SELECT True AS EXT FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = %s';
begin
  if QuerySetupDatabase.Active then
    QuerySetupDatabase.Active := False;
  QuerySetupDatabase.SQL.Text := Format(SQL_CHECK_TABLE_EXISTS, [QuotedStr('STORE_ITEMS')]);
  QuerySetupDatabase.Open;
  Result := (QuerySetupDatabase.RecordCount > 0) and QuerySetupDatabase.Fields[0].AsBoolean;
end;

function TCommonDM.NeedRecreate: Boolean;
begin
  Result := RECREATE_NEEDED or not IsTablesExist;
end;

procedure TCommonDM.RunScript(const AScriptText: string; AWithTransaction: Boolean = True);
var
  Scripts: TFDSQLScripts;
begin
  AWithTransaction := False;
  Scripts := TFDSQLScripts.Create(nil);
  try
    Scripts.Add.SQL.Text := AScriptText;
    FDScript.SQLScripts := Scripts;
    if AWithTransaction then
      FDTransaction.StartTransaction;
    try
      FDScript.ScriptOptions.CommandSeparator := '\';
      FDScript.ValidateAll;
      FDScript.ExecuteAll;
      if AWithTransaction then
        FDTransaction.Commit;
    except
      if AWithTransaction then
        FDTransaction.Rollback;
      raise;
    end;
  finally
    Scripts.Free;
  end;
end;

procedure InitCommonDM;
begin
  if not Assigned(_CommonDM) then
  begin
    _CommonDM := TCommonDM.Create(nil);
    _CommonDM.CheckConnected;
    _CommonDM.CheckDatabase;
  end;
end;

initialization
  InitCommonDM;

finalization
  FreeAndNil(_CommonDM);

end.
